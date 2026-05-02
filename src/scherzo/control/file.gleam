import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import simplifile

pub const control_dir_name = ".scherzo-state"

pub const control_file_name = "control.json"

pub const default_discovery_path = ".scherzo/workspaces/.scherzo-state/control.json"

pub type ControlFile {
  ControlFile(
    host: String,
    port: Int,
    token: String,
    workspace_root: String,
    started_at_ms: Int,
  )
}

pub type ControlFileError {
  ControlFileNotFound(path: String)
  ControlFileReadFailed(path: String, message: String)
  ControlFileWriteFailed(path: String, message: String)
  ControlFileInvalid(path: String, message: String)
  ControlFilePermissionFailed(path: String, message: String)
  TokenGenerationFailed(message: String)
}

pub fn path_for_workspace(workspace_root: String) -> String {
  trim_trailing_slash(workspace_root)
  <> "/"
  <> control_dir_name
  <> "/"
  <> control_file_name
}

pub fn read(path: String) -> Result(ControlFile, ControlFileError) {
  case simplifile.read(path) {
    Error(err) ->
      Error(ControlFileReadFailed(path, simplifile.describe_error(err)))
    Ok(contents) ->
      case json.parse(contents, control_file_decoder()) {
        Ok(control_file) -> Ok(control_file)
        Error(_) -> Error(ControlFileInvalid(path, "invalid control file JSON"))
      }
  }
}

pub fn write(
  path: String,
  control_file: ControlFile,
) -> Result(Nil, ControlFileError) {
  let dir = directory_name(path)
  case ensure_directory(dir, path) {
    Error(err) -> Error(err)
    Ok(Nil) ->
      case simplifile.write(path, control_file_to_string(control_file)) {
        Error(err) ->
          Error(ControlFileWriteFailed(path, simplifile.describe_error(err)))
        Ok(Nil) ->
          case chmod_private(path) {
            Ok(Nil) -> Ok(Nil)
            Error(message) -> {
              let _ = simplifile.delete_file(path)
              Error(ControlFilePermissionFailed(path, message))
            }
          }
      }
  }
}

fn ensure_directory(
  dir: String,
  path: String,
) -> Result(Nil, ControlFileError) {
  case dir == "" {
    True -> Ok(Nil)
    False ->
      case simplifile.create_directory_all(dir) {
        Ok(Nil) -> Ok(Nil)
        Error(err) ->
          Error(ControlFileWriteFailed(path, simplifile.describe_error(err)))
      }
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

pub fn remove(path: String) -> Nil {
  let _ = simplifile.delete_file(path)
  Nil
}

pub fn discover(
  explicit_path: Option(String),
  env: fn(String) -> Option(String),
) -> Result(ControlFile, ControlFileError) {
  discover_with_default(explicit_path, env, default_discovery_path)
}

pub fn discover_with_default(
  explicit_path: Option(String),
  env: fn(String) -> Option(String),
  default_path: String,
) -> Result(ControlFile, ControlFileError) {
  case discover_path_with_default(explicit_path, env, default_path) {
    Ok(path) -> read(path)
    Error(err) -> Error(err)
  }
}

pub fn discover_path(
  explicit_path: Option(String),
  env: fn(String) -> Option(String),
) -> Result(String, ControlFileError) {
  discover_path_with_default(explicit_path, env, default_discovery_path)
}

pub fn discover_path_with_default(
  explicit_path: Option(String),
  env: fn(String) -> Option(String),
  default_path: String,
) -> Result(String, ControlFileError) {
  case explicit_path {
    Some(path) -> Ok(path)
    None ->
      case env("SCHERZO_CONTROL_FILE") {
        Some(path) -> Ok(path)
        None ->
          case file_exists(default_path) {
            True -> Ok(default_path)
            False -> Error(ControlFileNotFound(default_path))
          }
      }
  }
}

pub fn control_file_to_string(control_file: ControlFile) -> String {
  control_file_to_json(control_file) |> json.to_string
}

pub fn control_file_to_json(control_file: ControlFile) -> json.Json {
  json.object([
    #("version", json.int(1)),
    #("host", json.string(control_file.host)),
    #("port", json.int(control_file.port)),
    #("token", json.string(control_file.token)),
    #("workspace_root", json.string(control_file.workspace_root)),
    #("started_at_ms", json.int(control_file.started_at_ms)),
  ])
}

pub fn generate_token() -> Result(String, ControlFileError) {
  case ffi_generate_token(32) {
    Ok(token) -> Ok(token)
    Error(message) -> Error(TokenGenerationFailed(message))
  }
}

pub fn get_env(name: String) -> Option(String) {
  case ffi_getenv(name) {
    Ok(value) -> Some(value)
    Error(_) -> None
  }
}

fn file_exists(path: String) -> Bool {
  case simplifile.is_file(path) {
    Ok(True) -> True
    _ -> False
  }
}

fn control_file_decoder() -> decode.Decoder(ControlFile) {
  use version <- decode.field("version", decode.int)
  use host <- decode.field("host", decode.string)
  use port <- decode.field("port", decode.int)
  use token <- decode.field("token", decode.string)
  use workspace_root <- decode.field("workspace_root", decode.string)
  use started_at_ms <- decode.field("started_at_ms", decode.int)
  case version == 1 && host != "" && port > 0 && token != "" {
    True ->
      decode.success(ControlFile(
        host: host,
        port: port,
        token: token,
        workspace_root: workspace_root,
        started_at_ms: started_at_ms,
      ))
    False ->
      decode.failure(ControlFile("", 0, "", "", 0), expected: "ControlFile")
  }
}

@external(erlang, "scherzo_control_ffi", "generate_token")
fn ffi_generate_token(bytes: Int) -> Result(String, String)

@external(erlang, "scherzo_control_ffi", "chmod_private")
fn chmod_private(path: String) -> Result(Nil, String)

@external(erlang, "scherzo_control_ffi", "getenv")
fn ffi_getenv(name: String) -> Result(String, String)
