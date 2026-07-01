import gleam/result
import scherzo/managed_launch/grant
import scherzo/path
import simplifile

pub type LoadError {
  UnsafeGrantFile(String)
  GrantFileReadFailed(String)
  GrantFileDeleteFailed(String)
  GrantInvalid(grant.DecodeError)
}

pub fn load_grant(path: String, now_ms: Int) -> Result(grant.Grant, LoadError) {
  use Nil <- result.try(validate_private_regular_file(path))
  use contents <- result.try(read_contents(path))
  use Nil <- result.try(delete_after_read(path))
  grant.decode_string(contents, now_ms)
  |> result.map_error(GrantInvalid)
}

pub fn error_code(error: LoadError) -> String {
  case error {
    UnsafeGrantFile(code) -> code
    GrantFileReadFailed(_) -> "grant_file_read_failed"
    GrantFileDeleteFailed(_) -> "grant_file_delete_failed"
    GrantInvalid(error) -> grant.error_code(error)
  }
}

pub fn error_message(error: LoadError) -> String {
  case error {
    UnsafeGrantFile(code) -> unsafe_file_message(code)
    GrantFileReadFailed(message) ->
      "managed launch grant file could not be read: " <> message
    GrantFileDeleteFailed(message) ->
      "managed launch grant file could not be deleted after read: " <> message
    GrantInvalid(error) -> grant.error_message(error)
  }
}

fn validate_private_regular_file(path_value: String) -> Result(Nil, LoadError) {
  case path.dirname(path_value) {
    Ok(parent_dir) ->
      case ffi_validate_private_regular_file(path_value, parent_dir) {
        Ok(Nil) -> Ok(Nil)
        Error(code) -> Error(UnsafeGrantFile(code))
      }
    Error(Nil) -> Error(UnsafeGrantFile("grant_file_parent_non_directory"))
  }
}

fn read_contents(path_value: String) -> Result(String, LoadError) {
  simplifile.read(path_value)
  |> result.map_error(fn(error) {
    GrantFileReadFailed(simplifile.describe_error(error))
  })
}

fn delete_after_read(path_value: String) -> Result(Nil, LoadError) {
  case simplifile.delete(path_value) {
    Ok(Nil) | Error(simplifile.Enoent) -> Ok(Nil)
    Error(error) ->
      Error(GrantFileDeleteFailed(simplifile.describe_error(error)))
  }
}

fn unsafe_file_message(code: String) -> String {
  case code {
    "grant_file_symlink" -> "managed launch grant file must not be a symlink"
    "grant_file_non_regular" ->
      "managed launch grant file must be a regular file"
    "grant_file_owner_unknown" ->
      "managed launch grant file ownership could not be verified for the current user"
    "grant_file_wrong_owner" ->
      "managed launch grant file must be owned by the current user"
    "grant_file_permissions_loose" ->
      "managed launch grant file permissions must not allow group or world access"
    "grant_file_parent_symlink" ->
      "managed launch grant file parent directory must not be a symlink"
    "grant_file_parent_non_directory" ->
      "managed launch grant file parent path must be a directory"
    "grant_file_parent_wrong_owner" ->
      "managed launch grant file parent directory must be owned by the current user"
    "grant_file_parent_permissions_loose" ->
      "managed launch grant file parent directory must not allow group or world access"
    _ -> "managed launch grant file is unsafe"
  }
}

// nolint: stringly_typed_error -- leaf managed-launch FFI returns tagged unsafe-file codes that validate_private_regular_file immediately normalizes into LoadError.
@external(erlang, "scherzo_managed_launch_ffi", "validate_private_regular_file")
fn ffi_validate_private_regular_file(
  path_value: String,
  parent_dir: String,
) -> Result(Nil, String)
