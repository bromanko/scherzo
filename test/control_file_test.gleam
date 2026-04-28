import gleam/option.{None, Some}
import scherzo/control/file
import simplifile

fn reset_dir(dir: String) -> Nil {
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  Nil
}

pub fn write_and_read_control_json_test() {
  let root = "test/tmp/control-file/write-read/workspaces"
  reset_dir("test/tmp/control-file")
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
  reset_dir("test/tmp/control-file/env")
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

pub fn default_discovery_fails_cleanly_when_no_file_exists_test() {
  let missing_path = "test/tmp/control-file/missing/control.json"
  let assert Error(file.ControlFileNotFound(path)) =
    file.discover_with_default(None, fn(_) { None }, missing_path)
  assert path == missing_path
}
