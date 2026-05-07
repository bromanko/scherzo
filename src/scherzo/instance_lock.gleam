import gleam/string
import scherzo/path
import simplifile

pub opaque type Lock {
  Lock(path: String, handle: LockHandle)
}

type LockHandle

pub type LockError {
  LockAlreadyHeld(String)
  LockOpenFailed(reason: String)
  LockWriteFailed(reason: String)
  LockUnexpectedFfiFailure(function: String, detail: String)
}

pub fn acquire(workspace_root: String) -> Result(Lock, LockError) {
  case path.absolute(workspace_root) {
    Error(_) -> Error(LockOpenFailed("canonicalize workspace root failed"))
    Ok(canonical_root) -> {
      let state_dir = path.join(canonical_root, ".scherzo-state")
      case simplifile.create_directory_all(state_dir) {
        Error(_) -> Error(LockOpenFailed("create lock directory failed"))
        Ok(Nil) -> {
          let lock_path = path.join(state_dir, "instance.lock")
          let body =
            "Scherzo instance lock\nworkspace_root=" <> canonical_root <> "\n"
          case ffi_acquire(lock_path, body) {
            Ok(handle) -> Ok(Lock(path: lock_path, handle: handle))
            Error("exists") ->
              Error(LockAlreadyHeld(
                "instance lock already exists at "
                <> lock_path
                <> "; another Scherzo process may be running, or this is a stale lock that must be removed manually after verifying no Scherzo process is active",
              ))
            Error(message) -> Error(raw_lock_error("acquire", message))
          }
        }
      }
    }
  }
}

pub fn release(lock: Lock) -> Nil {
  ffi_release(lock.handle, lock.path)
}

pub fn error_message(error: LockError) -> String {
  case error {
    LockAlreadyHeld(message) -> message
    LockOpenFailed(reason) -> "open lock failed: " <> reason
    LockWriteFailed(reason) -> "write lock failed: " <> reason
    LockUnexpectedFfiFailure(function, detail) ->
      function <> " failed unexpectedly: " <> detail
  }
}

fn raw_lock_error(function: String, message: String) -> LockError {
  let #(tag, detail) = split_tag(redact_empty(message))
  case tag {
    "open" -> LockOpenFailed(detail)
    "write" -> LockWriteFailed(detail)
    "unexpected_ffi_failure" -> LockUnexpectedFfiFailure(function, detail)
    _ -> LockUnexpectedFfiFailure(function, message)
  }
}

fn split_tag(error: String) -> #(String, String) {
  case string.split_once(error, on: ":") {
    Ok(#(tag, detail)) -> #(tag, detail)
    Error(Nil) -> #(error, "")
  }
}

fn redact_empty(message: String) -> String {
  case string.trim(message) == "" {
    True -> "lock io failed"
    False -> message
  }
}

@external(erlang, "scherzo_lock_ffi", "acquire")
fn ffi_acquire(path: String, body: String) -> Result(LockHandle, String)

@external(erlang, "scherzo_lock_ffi", "release")
fn ffi_release(handle: LockHandle, path: String) -> Nil
