import gleam/option.{None, Some}
import scherzo/control/file
import scherzo/path
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
    )

  let assert Ok(Nil) = file.write(path, control)
  let assert Ok(read_back) = file.read(path)

  assert read_back.host == "127.0.0.1"
  assert read_back.port == 54_321
  assert read_back.token == "secret-token"
  assert read_back.workspace_root == root
  assert read_back.started_at_ms == 42
}

pub fn env_discovery_uses_injected_environment_test() {
  let root = "test/tmp/control-file/env/workspaces"
  test_helpers.reset_dir("test/tmp/control-file/env")
  let path = file.path_for_workspace(root)
  let assert Ok(Nil) =
    file.write(path, file.ControlFile("127.0.0.1", 10_000, "token", root, 1))

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
      file.ControlFile("127.0.0.1", 10_001, "core-token", core_abs, 1),
    )
  let assert Ok(Nil) =
    file.write(
      caller_control,
      file.ControlFile("127.0.0.1", 10_002, "caller-token", caller_abs, 1),
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
      file.ControlFile("127.0.0.1", 10_003, "env-token", caller_abs, 1),
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
