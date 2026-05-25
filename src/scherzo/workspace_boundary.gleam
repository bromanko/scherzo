import gleam/int
import scherzo/error
import scherzo/path

pub fn resolve_absolute_path(
  value: String,
  context: String,
) -> Result(String, error.WorkspaceError) {
  case path.absolute(value) {
    Ok(absolute) -> Ok(absolute)
    Error(Nil) -> Error(error.WorkspaceIo(context <> " path resolution failed"))
  }
}

pub fn workspace_error_summary(err: error.WorkspaceError) -> String {
  case err {
    error.UnsafeWorkspaceKey(value) -> "unsafe workspace key: " <> value
    error.WorkspaceOutsideRoot(value) -> "workspace outside root: " <> value
    error.WorkspaceCollision(value) -> "workspace collision: " <> value
    error.WorkspaceIo(message) -> "workspace io: " <> message
    error.PartialWorkspace(value) -> "partial workspace: " <> value
  }
}

pub fn hook_error_summary(err: error.HookError) -> String {
  case err {
    error.HookFailed(name, status, diagnostics) ->
      error.hook_code(err)
      <> ": "
      <> name
      <> " exited "
      <> int.to_string(status)
      <> ": "
      <> diagnostics
    error.HookTimedOut(name) ->
      error.hook_code(err) <> ": " <> name <> " timed out"
    error.HookIo(message) -> error.hook_code(err) <> ": " <> message
  }
}
