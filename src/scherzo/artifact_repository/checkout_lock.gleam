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

pub fn acquire(checkout_dir: String) -> Result(Lock, LockError) {
  case path.absolute(checkout_dir) {
    Error(_) -> Error(LockOpenFailed("canonicalize checkout path failed"))
    Ok(canonical_checkout) -> {
      let lock_path = canonical_checkout <> ".publication.lock"
      let parent_dir = case path.dirname(canonical_checkout) {
        Ok(parent_dir) -> parent_dir
        Error(Nil) -> canonical_checkout
      }
      case simplifile.create_directory_all(parent_dir) {
        Error(_) -> Error(LockOpenFailed("create lock directory failed"))
        Ok(Nil) -> {
          let body =
            "Scherzo artifact publication lock\ncheckout_dir="
            <> canonical_checkout
            <> "\n"
          case ffi_acquire(lock_path, body) {
            Ok(handle) -> Ok(Lock(path: lock_path, handle: handle))
            Error("exists") ->
              Error(LockAlreadyHeld(
                "artifact publication lock already exists at "
                <> lock_path
                <> "; another publication may be running for this managed checkout",
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
