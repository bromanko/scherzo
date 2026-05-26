import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/control/file as control_file
import scherzo/hash
import simplifile

pub const identity_file_name = "daemon_identity.json"

pub type DaemonIdentity {
  DaemonIdentity(daemon_id: String, boot_id: String, path: String)
}

pub type IdentityError {
  InvalidWorkspaceRoot(root: String)
  IdentityReadFailed(path: String, message: String)
  IdentityWriteFailed(path: String, message: String)
  IdentityInvalid(path: String, message: String)
  IdentityGenerationFailed(message: String)
}

pub fn path_for_workspace_root(workspace_root: String) -> String {
  trim_trailing_slash(workspace_root)
  <> "/"
  <> control_file.control_dir_name
  <> "/"
  <> identity_file_name
}

pub fn load_or_create(
  workspace_root: String,
) -> Result(DaemonIdentity, IdentityError) {
  let workspace_root = string.trim(workspace_root)
  case workspace_root == "" {
    True -> Error(InvalidWorkspaceRoot(workspace_root))
    False -> {
      let path = path_for_workspace_root(workspace_root)
      case file_exists(path) {
        True -> load_existing(path)
        False -> create_identity(path)
      }
    }
  }
}

pub fn error_message(error: IdentityError) -> String {
  case error {
    InvalidWorkspaceRoot(_) -> "workspace root must be non-empty"
    IdentityReadFailed(path, message) ->
      "failed to read daemon identity " <> path <> ": " <> message
    IdentityWriteFailed(path, message) ->
      "failed to write daemon identity " <> path <> ": " <> message
    IdentityInvalid(path, message) ->
      "invalid daemon identity " <> path <> ": " <> message
    IdentityGenerationFailed(message) ->
      "failed to generate daemon identity: " <> message
  }
}

fn load_existing(path: String) -> Result(DaemonIdentity, IdentityError) {
  use contents <- result_try(read_contents(path))
  use daemon_id <- result_try(parse_daemon_id(path, contents))
  use boot_id <- result_try(generate_id("boot_"))
  Ok(DaemonIdentity(daemon_id: daemon_id, boot_id: boot_id, path: path))
}

fn create_identity(path: String) -> Result(DaemonIdentity, IdentityError) {
  use daemon_id <- result_try(generate_id("daemon_"))
  use boot_id <- result_try(generate_id("boot_"))
  let dir = directory_name(path)
  use _ <- result_try(ensure_directory(dir, path))
  let persisted = persisted_identity_to_string(daemon_id)
  use _ <- result_try(write_contents(path, persisted))
  Ok(DaemonIdentity(daemon_id: daemon_id, boot_id: boot_id, path: path))
}

fn read_contents(path: String) -> Result(String, IdentityError) {
  case simplifile.read(path) {
    Ok(contents) -> Ok(contents)
    Error(err) ->
      Error(IdentityReadFailed(path, simplifile.describe_error(err)))
  }
}

fn write_contents(
  path: String,
  contents: String,
) -> Result(Nil, IdentityError) {
  case simplifile.write(path, contents) {
    Ok(Nil) -> Ok(Nil)
    Error(err) ->
      Error(IdentityWriteFailed(path, simplifile.describe_error(err)))
  }
}

fn ensure_directory(dir: String, path: String) -> Result(Nil, IdentityError) {
  case dir == "" {
    True -> Ok(Nil)
    False ->
      case simplifile.create_directory_all(dir) {
        Ok(Nil) -> Ok(Nil)
        Error(err) ->
          Error(IdentityWriteFailed(path, simplifile.describe_error(err)))
      }
  }
}

fn parse_daemon_id(
  path: String,
  contents: String,
) -> Result(String, IdentityError) {
  case json.parse(contents, persisted_identity_decoder()) {
    Ok(value) -> Ok(value)
    Error(_) -> validate_identity_contents(path, contents)
  }
}

fn validate_identity_contents(
  path: String,
  contents: String,
) -> Result(String, IdentityError) {
  case json.parse(contents, raw_persisted_identity_decoder()) {
    Error(_) -> Error(IdentityInvalid(path, "invalid daemon identity JSON"))
    Ok(RawPersistedIdentity(version:, daemon_id:)) -> {
      case version != 1 {
        True ->
          Error(IdentityInvalid(path, "unsupported daemon identity version"))
        False ->
          case daemon_id {
            None -> Error(IdentityInvalid(path, "daemon_id is required"))
            Some(daemon_id) ->
              case valid_prefixed_hex_id(daemon_id, "daemon_") {
                True -> Ok(daemon_id)
                False ->
                  Error(IdentityInvalid(path, "daemon_id has invalid shape"))
              }
          }
      }
    }
  }
}

type RawPersistedIdentity {
  RawPersistedIdentity(version: Int, daemon_id: Option(String))
}

fn raw_persisted_identity_decoder() -> decode.Decoder(RawPersistedIdentity) {
  use version <- decode.field("version", decode.int)
  use daemon_id <- decode.optional_field(
    "daemon_id",
    None,
    decode.optional(decode.string),
  )
  decode.success(RawPersistedIdentity(version: version, daemon_id: daemon_id))
}

fn persisted_identity_decoder() -> decode.Decoder(String) {
  use version <- decode.field("version", decode.int)
  use daemon_id <- decode.field("daemon_id", decode.string)
  case version != 1 {
    True -> decode.failure("", expected: "daemon identity version 1")
    False ->
      case valid_prefixed_hex_id(daemon_id, "daemon_") {
        True -> decode.success(daemon_id)
        False -> decode.failure("", expected: "daemon_<32 lowercase hex>")
      }
  }
}

fn persisted_identity_to_string(daemon_id: String) -> String {
  json.object([
    #("version", json.int(1)),
    #("daemon_id", json.string(daemon_id)),
  ])
  |> json.to_string
}

fn generate_id(prefix: String) -> Result(String, IdentityError) {
  case control_file.generate_token() {
    Ok(token) -> Ok(prefix <> string.slice(hash.sha256_hex(token), 0, 32))
    Error(control_file.TokenGenerationFailed(message)) ->
      Error(IdentityGenerationFailed(message))
    Error(other) ->
      Error(IdentityGenerationFailed(control_file_error_message(other)))
  }
}

fn control_file_error_message(error: control_file.ControlFileError) -> String {
  case error {
    control_file.ControlFileNotFound(path) -> "control file not found: " <> path
    control_file.ControlFileReadFailed(path, message) ->
      "control file read failed " <> path <> ": " <> message
    control_file.ControlFileWriteFailed(path, message) ->
      "control file write failed " <> path <> ": " <> message
    control_file.ControlFileInvalid(path, message) ->
      "control file invalid " <> path <> ": " <> message
    control_file.ControlFilePermissionFailed(path, message) ->
      "control file permission failed " <> path <> ": " <> message
    control_file.TokenGenerationFailed(message) -> message
  }
}

fn valid_prefixed_hex_id(value: String, prefix: String) -> Bool {
  string.starts_with(value, prefix)
  && string.length(value) == string.length(prefix) + 32
  && value
  |> string.drop_start(string.length(prefix))
  |> is_lower_hex_string
}

fn is_lower_hex_string(value: String) -> Bool {
  case string.to_graphemes(value) {
    [] -> False
    chars -> list.all(chars, is_lower_hex_char)
  }
}

fn is_lower_hex_char(char: String) -> Bool {
  case char {
    "0"
    | "1"
    | "2"
    | "3"
    | "4"
    | "5"
    | "6"
    | "7"
    | "8"
    | "9"
    | "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f" -> True
    _ -> False
  }
}

fn file_exists(path: String) -> Bool {
  case simplifile.is_file(path) {
    Ok(True) -> True
    _ -> False
  }
}

fn directory_name(path: String) -> String {
  case string.split(path, "/") |> list.reverse {
    [] -> ""
    [_] -> ""
    [_, ..reversed_dir] ->
      reversed_dir |> list.reverse |> string.join(with: "/")
  }
}

fn trim_trailing_slash(path: String) -> String {
  case string.ends_with(path, "/") {
    True -> string.drop_end(path, 1)
    False -> path
  }
}

fn result_try(
  result: Result(a, e),
  next: fn(a) -> Result(b, e),
) -> Result(b, e) {
  case result {
    Ok(value) -> next(value)
    Error(error) -> Error(error)
  }
}
