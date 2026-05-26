import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub const version = 1

pub type Hello {
  Hello(daemon_id: String, boot_id: String, capabilities: List(String))
}

pub type HelloError {
  HelloError(code: String, message: String)
}

type HelloFields {
  HelloFields(
    version: Option(Int),
    type_: Option(String),
    daemon_id: Option(String),
    boot_id: Option(String),
    auth: Option(String),
    capabilities: Option(List(String)),
  )
}

pub fn encode(
  daemon_id: String,
  boot_id: String,
  auth: String,
  capabilities: List(String),
) -> String {
  json.object([
    #("version", json.int(version)),
    #("type", json.string("hello")),
    #("daemon_id", json.string(daemon_id)),
    #("boot_id", json.string(boot_id)),
    #("auth", json.string(auth)),
    #("capabilities", json.array(capabilities, of: json.string)),
  ])
  |> json.to_string
}

pub fn decode(
  line: String,
  expected_auth: String,
) -> Result(Hello, HelloError) {
  case json.parse(line, decode.dynamic) {
    Ok(value) -> decode_dynamic(value, expected_auth)
    Error(_) -> Error(HelloError("bad_json", "malformed harness hello JSON"))
  }
}

pub fn decode_dynamic(
  value: Dynamic,
  expected_auth: String,
) -> Result(Hello, HelloError) {
  case decode.run(value, hello_fields_decoder()) {
    Ok(fields) -> hello_from_fields(fields, expected_auth)
    Error(_) -> Error(HelloError("invalid_hello", "invalid harness hello"))
  }
}

pub fn redact_auth(line: String) -> String {
  case json.parse(line, decode.dynamic) {
    Ok(value) ->
      case decode.run(value, auth_field_decoder()) {
        Ok(Some(_)) -> "[REDACTED]"
        _ -> redact_unparseable_auth(line)
      }
    Error(_) -> redact_unparseable_auth(line)
  }
}

fn auth_field_decoder() -> decode.Decoder(Option(String)) {
  use auth <- decode.optional_field(
    "auth",
    None,
    decode.optional(decode.string),
  )
  decode.success(auth)
}

fn redact_unparseable_auth(line: String) -> String {
  case string.contains(line, "\"auth\"") {
    True -> "[REDACTED]"
    False -> line
  }
}

fn hello_fields_decoder() -> decode.Decoder(HelloFields) {
  use version <- decode.optional_field(
    "version",
    None,
    decode.optional(decode.int),
  )
  use type_ <- decode.optional_field(
    "type",
    None,
    decode.optional(decode.string),
  )
  use daemon_id <- decode.optional_field(
    "daemon_id",
    None,
    decode.optional(decode.string),
  )
  use boot_id <- decode.optional_field(
    "boot_id",
    None,
    decode.optional(decode.string),
  )
  use auth <- decode.optional_field(
    "auth",
    None,
    decode.optional(decode.string),
  )
  use capabilities <- decode.optional_field(
    "capabilities",
    None,
    decode.optional(decode.list(decode.string)),
  )
  decode.success(HelloFields(
    version,
    type_,
    daemon_id,
    boot_id,
    auth,
    capabilities,
  ))
}

fn hello_from_fields(
  fields: HelloFields,
  expected_auth: String,
) -> Result(Hello, HelloError) {
  use _ <- result.try(required_version(fields.version))
  use _ <- result.try(required_type(fields.type_))
  use daemon_id <- result.try(required_id(
    fields.daemon_id,
    "daemon_id",
    "daemon_",
  ))
  use boot_id <- result.try(required_id(fields.boot_id, "boot_id", "boot_"))
  use _ <- result.try(required_auth(fields.auth, expected_auth))
  use capabilities <- result.try(required_capabilities(fields.capabilities))
  Ok(Hello(daemon_id, boot_id, capabilities))
}

fn required_version(found_version: Option(Int)) -> Result(Int, HelloError) {
  case found_version {
    Some(found) if found == version -> Ok(found)
    Some(found) ->
      Error(HelloError(
        "unsupported_version",
        "unsupported harness hello version: " <> int_to_string(found),
      ))
    None -> Error(HelloError("invalid_hello", "missing version"))
  }
}

fn required_type(type_: Option(String)) -> Result(String, HelloError) {
  case type_ {
    Some("hello") -> Ok("hello")
    Some(found) ->
      Error(HelloError(
        "invalid_hello_type",
        "invalid harness hello type: " <> found,
      ))
    None -> Error(HelloError("invalid_hello", "missing type"))
  }
}

fn required_id(
  value: Option(String),
  field_name: String,
  prefix: String,
) -> Result(String, HelloError) {
  case value {
    Some(value) ->
      case valid_prefixed_hex_id(value, prefix) {
        True -> Ok(value)
        False ->
          Error(HelloError("invalid_hello", field_name <> " has invalid shape"))
      }
    None -> Error(HelloError("invalid_hello", "missing " <> field_name))
  }
}

fn required_auth(
  auth: Option(String),
  expected_auth: String,
) -> Result(Nil, HelloError) {
  case auth {
    Some(auth) ->
      case auth == expected_auth {
        True -> Ok(Nil)
        False -> Error(HelloError("wrong_auth", "invalid harness auth token"))
      }
    None -> Error(HelloError("invalid_hello", "missing auth"))
  }
}

fn required_capabilities(
  capabilities: Option(List(String)),
) -> Result(List(String), HelloError) {
  case capabilities {
    Some(capabilities) -> Ok(capabilities)
    None -> Error(HelloError("invalid_hello", "missing capabilities"))
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

fn int_to_string(value: Int) -> String {
  value |> json.int |> json.to_string
}
