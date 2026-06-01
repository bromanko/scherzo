import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/control/query/dto
import scherzo/control/query/types

pub const version = 1

pub fn request_to_json(request: types.QueryRequest) -> json.Json {
  json.object([
    #("version", json.int(version)),
    #("type", json.string(types.query_type(request))),
  ])
}

pub fn request_to_string(request: types.QueryRequest) -> String {
  request |> request_to_json |> json.to_string
}

pub fn decode_request(
  line: String,
) -> Result(types.QueryRequest, types.QueryError) {
  case json.parse(line, decode.dynamic) {
    Ok(value) -> decode_request_dynamic(value)
    Error(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "malformed query request JSON",
      ))
  }
}

pub fn decode_request_dynamic(
  value: Dynamic,
) -> Result(types.QueryRequest, types.QueryError) {
  case decode.run(value, request_fields_decoder()) {
    Ok(fields) -> request_from_fields(fields)
    Error(_) ->
      Error(types.QueryError(types.QueryBackendFailed, "invalid query request"))
  }
}

pub fn response_to_json(response: types.QueryResponse) -> json.Json {
  case response {
    types.StatusResponse(status) ->
      json.object([
        #("version", json.int(version)),
        #("ok", json.bool(True)),
        #("type", json.string(types.response_type(response))),
        #("status", dto.status_to_json(status)),
      ])
    types.MetricsResponse(metrics) ->
      json.object([
        #("version", json.int(version)),
        #("ok", json.bool(True)),
        #("type", json.string(types.response_type(response))),
        #("metrics", dto.operational_metrics_to_json(metrics)),
      ])
  }
}

pub fn response_to_string(response: types.QueryResponse) -> String {
  response |> response_to_json |> json.to_string
}

pub fn error_to_json(error: types.QueryError) -> json.Json {
  let types.QueryError(code: code, message: message) = error
  json.object([
    #("version", json.int(version)),
    #("ok", json.bool(False)),
    #(
      "error",
      json.object([
        #("code", json.string(types.error_code_to_string(code))),
        #("message", json.string(message)),
      ]),
    ),
  ])
}

pub fn error_to_string(error: types.QueryError) -> String {
  error |> error_to_json |> json.to_string
}

pub fn decode_response(
  line: String,
) -> Result(types.QueryResponse, types.QueryError) {
  case json.parse(line, decode.dynamic) {
    Ok(value) -> decode_response_dynamic(value)
    Error(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "malformed query response JSON",
      ))
  }
}

pub fn decode_response_dynamic(
  value: Dynamic,
) -> Result(types.QueryResponse, types.QueryError) {
  case decode.run(value, response_fields_decoder()) {
    Ok(fields) -> response_from_fields(fields)
    Error(_) ->
      Error(types.QueryError(types.QueryBackendFailed, "invalid query response"))
  }
}

type RequestFields {
  RequestFields(version: Option(Int), type_: Option(String))
}

type ResponseFields {
  ResponseFields(
    version: Option(Int),
    ok: Option(Bool),
    type_: Option(String),
    status: Option(Dynamic),
    metrics: Option(Dynamic),
    error: Option(ErrorFields),
  )
}

type ErrorFields {
  ErrorFields(code: Option(String), message: Option(String))
}

fn request_fields_decoder() -> decode.Decoder(RequestFields) {
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
  decode.success(RequestFields(version: version, type_: type_))
}

fn response_fields_decoder() -> decode.Decoder(ResponseFields) {
  use version <- decode.optional_field(
    "version",
    None,
    decode.optional(decode.int),
  )
  use ok <- decode.optional_field("ok", None, decode.optional(decode.bool))
  use type_ <- decode.optional_field(
    "type",
    None,
    decode.optional(decode.string),
  )
  use status <- decode.optional_field(
    "status",
    None,
    decode.optional(decode.dynamic),
  )
  use metrics <- decode.optional_field(
    "metrics",
    None,
    decode.optional(decode.dynamic),
  )
  use error <- decode.optional_field(
    "error",
    None,
    decode.optional(error_fields_decoder()),
  )
  decode.success(ResponseFields(
    version: version,
    ok: ok,
    type_: type_,
    status: status,
    metrics: metrics,
    error: error,
  ))
}

fn error_fields_decoder() -> decode.Decoder(ErrorFields) {
  use code <- decode.optional_field(
    "code",
    None,
    decode.optional(decode.string),
  )
  use message <- decode.optional_field(
    "message",
    None,
    decode.optional(decode.string),
  )
  decode.success(ErrorFields(code: code, message: message))
}

fn request_from_fields(
  fields: RequestFields,
) -> Result(types.QueryRequest, types.QueryError) {
  case require_version(fields.version) {
    Error(error) -> Error(error)
    Ok(Nil) ->
      case fields.type_ {
        Some("status") -> Ok(types.Status)
        Some("metrics") -> Ok(types.Metrics)
        Some(other) ->
          Error(types.QueryError(
            types.UnsupportedQuery,
            "unsupported query type: " <> other,
          ))
        None ->
          Error(types.QueryError(types.QueryBackendFailed, "missing query type"))
      }
  }
}

fn response_from_fields(
  fields: ResponseFields,
) -> Result(types.QueryResponse, types.QueryError) {
  case require_version(fields.version) {
    Error(error) -> Error(error)
    Ok(Nil) ->
      case fields.ok {
        Some(True) -> decode_success_response(fields)
        Some(False) -> decode_error_response(fields.error)
        None ->
          Error(types.QueryError(types.QueryBackendFailed, "missing ok flag"))
      }
  }
}

fn decode_success_response(
  fields: ResponseFields,
) -> Result(types.QueryResponse, types.QueryError) {
  case fields.type_ {
    Some("status") -> decode_status_response(fields.status)
    Some("metrics") -> decode_metrics_response(fields.metrics)
    Some(other) ->
      Error(types.QueryError(
        types.UnsupportedQuery,
        "unsupported query type: " <> other,
      ))
    None ->
      Error(types.QueryError(types.QueryBackendFailed, "missing query type"))
  }
}

fn decode_status_response(
  status: Option(Dynamic),
) -> Result(types.QueryResponse, types.QueryError) {
  case status {
    Some(status) ->
      dto.decode_status_dynamic(status) |> result.map(types.StatusResponse)
    None ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "missing query response payload",
      ))
  }
}

fn decode_metrics_response(
  metrics: Option(Dynamic),
) -> Result(types.QueryResponse, types.QueryError) {
  case metrics {
    Some(metrics) ->
      dto.decode_operational_metrics_dynamic(metrics)
      |> result.map(types.MetricsResponse)
    None ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "missing query response payload",
      ))
  }
}

fn decode_error_response(
  maybe_error: Option(ErrorFields),
) -> Result(types.QueryResponse, types.QueryError) {
  case maybe_error {
    Some(ErrorFields(code: Some(code), message: Some(message))) ->
      case types.error_code_from_string(code) {
        Ok(parsed) -> Error(types.QueryError(parsed, message))
        Error(_) ->
          Error(types.QueryError(
            types.QueryBackendFailed,
            "unknown query error code: " <> code,
          ))
      }
    _ ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "invalid query error payload",
      ))
  }
}

fn require_version(
  codec_version: Option(Int),
) -> Result(Nil, types.QueryError) {
  case codec_version {
    Some(value) if value == version -> Ok(Nil)
    Some(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "unsupported query codec version",
      ))
    None ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "missing query codec version",
      ))
  }
}
