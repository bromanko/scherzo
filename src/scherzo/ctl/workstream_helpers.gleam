import gleam/option.{type Option, None, Some}
import scherzo/control/file
import scherzo/state/ledger
import scherzo/state/projection as state_projection

pub fn workspace_root(
  control_path: Option(String),
  explicit_root: Option(String),
) -> Result(String, #(String, String)) {
  case explicit_root {
    Some(root) -> Ok(file.resolve_cli_path(root, file.get_env))
    None -> {
      use control_file <- try_workstream(load_control_file(control_path))
      Ok(control_file.workspace_root)
    }
  }
}

pub fn load_schedule_projection(
  root: String,
) -> Result(state_projection.Projection, #(String, String)) {
  case ledger.path_for_workspace_root(root) {
    Error(_) -> Error(#("ledger_path_failed", "could not resolve ledger path"))
    Ok(ledger_path) ->
      case ledger.load_projection(ledger_path) {
        Ok(projected) -> Ok(projected)
        Error(_) ->
          Error(#("ledger_load_failed", "could not load local ledger"))
      }
  }
}

pub fn try_workstream(
  result: Result(a, #(String, String)),
  next: fn(a) -> Result(b, #(String, String)),
) -> Result(b, #(String, String)) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}

fn load_control_file(
  explicit_path: Option(String),
) -> Result(file.ControlFile, #(String, String)) {
  file.discover(explicit_path, file.get_env) |> map_file_error
}

fn map_file_error(
  result: Result(a, file.ControlFileError),
) -> Result(a, #(String, String)) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) -> Error(file_error(err))
  }
}

fn file_error(error: file.ControlFileError) -> #(String, String) {
  case error {
    file.ControlFileNotFound(path) -> #(
      "control_file_not_found",
      "control file not found: " <> path,
    )
    file.ControlFileReadFailed(_, message) -> #(
      "control_file_read_failed",
      message,
    )
    file.ControlFileWriteFailed(_, message) -> #(
      "control_file_write_failed",
      message,
    )
    file.ControlFileInvalid(_, message) -> #("control_file_invalid", message)
    file.ControlFilePermissionFailed(_, message) -> #(
      "control_file_permission_failed",
      message,
    )
    file.TokenGenerationFailed(message) -> #("token_generation_failed", message)
  }
}
