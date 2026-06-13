import gleam/option.{None, Some}
import gleam/string
import scherzo/control/defaults as control_defaults
import scherzo/control/file
import scherzo/path
import simplifile
import support/test_helpers

pub fn write_and_read_control_json_test() {
  let root = "test/tmp/control-file/write-read/workspaces"
  test_helpers.reset_dir("test/tmp/control-file")
  let path = file.path_for_workspace(root)
  let control =
    file.ControlFile(
      host: "127.0.0.1",
      port: 54_321,
      token: "secret-token",
      workspace_root: root,
      started_at_ms: 42,
      command_timeout_ms: 75_000,
    )

  let assert Ok(Nil) = file.write(path, control)
  let assert Ok(read_back) = file.read(path)

  assert read_back.host == "127.0.0.1"
  assert read_back.port == 54_321
  assert read_back.token == "secret-token"
  assert read_back.workspace_root == root
  assert read_back.started_at_ms == 42
  assert read_back.command_timeout_ms == 75_000
}

pub fn legacy_control_json_defaults_command_timeout_test() {
  let path = "test/tmp/control-file/legacy-default/control.json"
  test_helpers.reset_dir("test/tmp/control-file/legacy-default")
  let contents =
    "{\"version\":1,\"host\":\"127.0.0.1\",\"port\":54321,\"token\":\"secret-token\",\"workspace_root\":\"test/tmp/control-file/legacy-default/workspaces\",\"started_at_ms\":42}"
  let assert Ok(Nil) = simplifile.write(path, contents)

  let assert Ok(read_back) = file.read(path)
  assert read_back.command_timeout_ms
    == control_defaults.default_command_timeout_ms
}

pub fn env_discovery_uses_injected_environment_test() {
  let root = "test/tmp/control-file/env/workspaces"
  test_helpers.reset_dir("test/tmp/control-file/env")
  let path = file.path_for_workspace(root)
  let assert Ok(Nil) =
    file.write(
      path,
      file.ControlFile("127.0.0.1", 10_000, "token", root, 1, 60_000),
    )

  let assert Ok(discovered) =
    file.discover(None, fn(name) {
      case name == "SCHERZO_CONTROL_FILE" {
        True -> Some(path)
        False -> None
      }
    })

  assert discovered.token == "token"
  assert discovered.workspace_root == root
}

pub fn caller_cwd_resolves_relative_control_file_paths_test() {
  let base = "test/tmp/control-file/caller-cwd"
  let core_root = base <> "/core"
  let caller_root = base <> "/consumer"
  test_helpers.reset_dir(base)
  let assert Ok(core_abs) = path.absolute(core_root)
  let assert Ok(caller_abs) = path.absolute(caller_root)
  let control_rel = file.default_discovery_path
  let core_control = core_abs <> "/" <> control_rel
  let caller_control = caller_abs <> "/" <> control_rel
  let assert Ok(Nil) =
    file.write(
      core_control,
      file.ControlFile("127.0.0.1", 10_001, "core-token", core_abs, 1, 60_000),
    )
  let assert Ok(Nil) =
    file.write(
      caller_control,
      file.ControlFile(
        "127.0.0.1",
        10_002,
        "caller-token",
        caller_abs,
        1,
        60_000,
      ),
    )
  let env = fn(name) {
    case name {
      "SCHERZO_CALLER_CWD" -> Some(caller_abs)
      _ -> None
    }
  }

  let assert Ok(discovered_default) =
    file.discover_with_default(None, env, control_rel)
  let assert Ok(discovered_explicit) = file.discover(Some(control_rel), env)

  assert discovered_default.token == "caller-token"
  assert discovered_default.workspace_root == caller_abs
  assert discovered_explicit.token == "caller-token"
  assert discovered_explicit.workspace_root == caller_abs
}

pub fn caller_cwd_resolves_relative_scherzo_control_file_env_test() {
  let base = "test/tmp/control-file/caller-env"
  let caller_root = base <> "/consumer"
  test_helpers.reset_dir(base)
  let assert Ok(caller_abs) = path.absolute(caller_root)
  let control_rel = file.default_discovery_path
  let caller_control = caller_abs <> "/" <> control_rel
  let assert Ok(Nil) =
    file.write(
      caller_control,
      file.ControlFile("127.0.0.1", 10_003, "env-token", caller_abs, 1, 60_000),
    )

  let assert Ok(discovered) =
    file.discover(None, fn(name) {
      case name {
        "SCHERZO_CALLER_CWD" -> Some(caller_abs)
        "SCHERZO_CONTROL_FILE" -> Some(control_rel)
        _ -> None
      }
    })

  assert discovered.token == "env-token"
  assert discovered.workspace_root == caller_abs
}

pub fn default_discovery_fails_cleanly_when_no_file_exists_test() {
  let missing_path = "test/tmp/control-file/missing/control.json"
  let assert Error(file.ControlFileNotFound(path)) =
    file.discover_with_default(None, fn(_) { None }, missing_path)
  assert path == missing_path
}

pub fn write_reports_cleanup_failure_when_permission_cleanup_fails_test() {
  let base = "test/tmp/control-file/write-cleanup-failure"
  test_helpers.reset_dir(base)
  let control_path = base <> "/control.json"

  case simplifile.file_info("/dev/null"), simplifile.file_info(base) {
    Ok(dev_null_info), Ok(base_info)
      if dev_null_info.user_id != base_info.user_id
    -> {
      let assert Ok(Nil) = path.symlink("/dev/null", control_path)
      let assert Ok(Nil) =
        simplifile.set_permissions_octal(for_file_at: base, to: 0o500)
      let write_result =
        file.write(
          control_path,
          file.ControlFile("127.0.0.1", 10_004, "token", base, 1, 60_000),
        )
      let restore_result =
        simplifile.set_permissions_octal(for_file_at: base, to: 0o700)
      let assert Ok(Nil) = restore_result

      let assert Error(file.ControlFilePermissionFailed(error_path, message)) =
        write_result
      assert error_path == control_path
      assert string.contains(message, "cleanup failed")
      assert string.contains(message, control_path)
    }
    _, _ -> Nil
  }
}
