import gleam/string
import scherzo/path
import simplifile

pub opaque type Lock {
  Lock(path: String, handle: LockHandle)
}

type LockHandle

pub type LockError {
  LockAlreadyHeld(String)
  LockIo(String)
}

pub fn acquire(workspace_root: String) -> Result(Lock, LockError) {
  case path.absolute(workspace_root) {
    Error(_) -> Error(LockIo("canonicalize workspace root failed"))
    Ok(canonical_root) -> {
      let state_dir = path.join(canonical_root, ".scherzo-state")
      case simplifile.create_directory_all(state_dir) {
        Error(_) -> Error(LockIo("create lock directory failed"))
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
            Error(message) -> Error(LockIo(redact_empty(message)))
          }
        }
      }
    }
  }
}

pub fn release(lock: Lock) -> Nil {
  ffi_release(lock.handle, lock.path)
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
