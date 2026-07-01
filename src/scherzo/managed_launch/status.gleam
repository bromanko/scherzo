import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/json_decode_error
import scherzo/log
import simplifile

pub type Status {
  Status(
    launch_id: Option(String),
    phase: String,
    ok: Bool,
    code: String,
    message: String,
    updated_at_ms: Int,
  )
}

pub type DecodeError {
  InvalidStatusJson(json.DecodeError)
  UnsupportedStatusVersion(Int)
}

type RawStatus {
  RawStatus(
    version: Int,
    launch_id: Option(String),
    phase: String,
    ok: Bool,
    code: String,
    message: String,
    updated_at_ms: Int,
  )
}

pub fn decode_string(contents: String) -> Result(Status, DecodeError) {
  case json.parse(contents, raw_status_decoder()) {
    Ok(raw) -> validate_raw_status(raw)
    Error(error) -> Error(InvalidStatusJson(error))
  }
}

pub fn redact(status: Status, secrets: List(String)) -> Status {
  Status(
    launch_id: status.launch_id,
    phase: log.redact("phase", status.phase, secrets),
    ok: status.ok,
    code: log.redact("code", status.code, secrets),
    message: log.redact("message", status.message, secrets),
    updated_at_ms: status.updated_at_ms,
  )
}

pub fn to_json(status: Status) -> json.Json {
  let Status(
    launch_id: launch_id,
    phase: phase,
    ok: ok,
    code: code,
    message: message,
    updated_at_ms: updated_at_ms,
  ) = status

  json.object([
    #("version", json.int(1)),
    #("launchId", option_json(launch_id, json.string)),
    #("phase", json.string(phase)),
    #("ok", json.bool(ok)),
    #("code", json.string(code)),
    #("message", json.string(message)),
    #("updatedAtMs", json.int(updated_at_ms)),
  ])
}

pub fn to_string(status: Status) -> String {
  status |> to_json |> json.to_string
}

pub fn to_redacted_string(status: Status, secrets: List(String)) -> String {
  status |> redact(secrets) |> to_string
}

pub fn to_log_fields(status: Status, secrets: List(String)) -> List(log.Field) {
  let safe = redact(status, secrets)
  [
    #("launch_id", option_to_log_value(safe.launch_id)),
    #("phase", safe.phase),
    #("ok", bool_to_string(safe.ok)),
    #("code", safe.code),
    #("message", safe.message),
    #("updated_at_ms", int_to_string(safe.updated_at_ms)),
  ]
}

pub fn write_atomic(
  path: String,
  status: Status,
  secrets: List(String),
) -> Result(Nil, simplifile.FileError) {
  let temp = path <> ".tmp"
  use Nil <- result.try(simplifile.write(
    temp,
    to_redacted_string(status, secrets),
  ))
  simplifile.rename(temp, path)
}

pub fn error_code(error: DecodeError) -> String {
  case error {
    InvalidStatusJson(_) -> "invalid_managed_launch_status_json"
    UnsupportedStatusVersion(_) -> "unsupported_managed_launch_status_version"
  }
}

pub fn error_message(error: DecodeError) -> String {
  case error {
    InvalidStatusJson(parse_error) ->
      "managed launch status JSON is invalid: "
      <> json_decode_error.to_string(parse_error)
    UnsupportedStatusVersion(version) ->
      "managed launch status version "
      <> int_to_string(version)
      <> " is not supported"
  }
}

fn validate_raw_status(raw: RawStatus) -> Result(Status, DecodeError) {
  case raw.version {
    1 ->
      Ok(Status(
        launch_id: raw.launch_id,
        phase: raw.phase,
        ok: raw.ok,
        code: raw.code,
        message: raw.message,
        updated_at_ms: raw.updated_at_ms,
      ))
    version -> Error(UnsupportedStatusVersion(version))
  }
}

fn raw_status_decoder() -> decode.Decoder(RawStatus) {
  use version <- decode.field("version", decode.int)
  use launch_id <- decode.optional_field(
    "launchId",
    None,
    decode.optional(decode.string),
  )
  use phase <- decode.field("phase", decode.string)
  use ok <- decode.field("ok", decode.bool)
  use code <- decode.field("code", decode.string)
  use message <- decode.field("message", decode.string)
  use updated_at_ms <- decode.field("updatedAtMs", decode.int)

  decode.success(RawStatus(
    version: version,
    launch_id: launch_id,
    phase: phase,
    ok: ok,
    code: code,
    message: message,
    updated_at_ms: updated_at_ms,
  ))
}

fn option_json(value: Option(a), encoder: fn(a) -> json.Json) -> json.Json {
  case value {
    Some(value) -> encoder(value)
    None -> json.null()
  }
}

fn option_to_log_value(value: Option(String)) -> String {
  case value {
    Some(value) -> value
    None -> ""
  }
}

fn bool_to_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
