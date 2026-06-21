pub type CodeSnapshotError {
  CodeSnapshotError(reason: String)
}

pub fn ensure_scherzo_modules_loaded() -> Result(Int, CodeSnapshotError) {
  ensure_scherzo_modules_loaded_ffi()
  |> map_error
}

pub fn describe_error(error: CodeSnapshotError) -> String {
  let CodeSnapshotError(reason) = error
  reason
}

fn map_error(result: Result(Int, String)) -> Result(Int, CodeSnapshotError) {
  case result {
    Ok(count) -> Ok(count)
    Error(reason) -> Error(CodeSnapshotError(reason))
  }
}

// nolint: stringly_typed_error -- startup FFI returns Erlang loader diagnostics that ensure_scherzo_modules_loaded immediately wraps.
@external(erlang, "scherzo_code_snapshot_ffi", "ensure_scherzo_modules_loaded")
fn ensure_scherzo_modules_loaded_ffi() -> Result(Int, String)
