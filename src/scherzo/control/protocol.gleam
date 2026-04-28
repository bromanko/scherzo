import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/domain
import scherzo/session/event
import scherzo/session/json as session_json

pub const version = 1

pub type Request {
  Ping(id: String, token: String)
  ListSessions(id: String, token: String)
  GetSession(id: String, token: String, session_id: String)
  GetEvents(
    id: String,
    token: String,
    session_id: String,
    after: Int,
    limit: Int,
  )
  StreamEvents(id: String, token: String, session_id: String, after: Int)
}

pub type ErrorBody {
  ErrorBody(code: String, message: String)
}

pub type Response {
  Response(
    id: String,
    ok: Bool,
    data: Option(json.Json),
    error: Option(ErrorBody),
  )
}

pub type RequestError {
  RequestError(id: String, code: String, message: String)
}

type RequestFields {
  RequestFields(
    version: Int,
    id: String,
    token: String,
    type_: String,
    session_id: Option(String),
    after: Int,
    limit: Int,
  )
}

pub fn request_id(request: Request) -> String {
  case request {
    Ping(id, _) -> id
    ListSessions(id, _) -> id
    GetSession(id, _, _) -> id
    GetEvents(id, _, _, _, _) -> id
    StreamEvents(id, _, _, _) -> id
  }
}

pub fn request_token(request: Request) -> String {
  case request {
    Ping(_, token) -> token
    ListSessions(_, token) -> token
    GetSession(_, token, _) -> token
    GetEvents(_, token, _, _, _) -> token
    StreamEvents(_, token, _, _) -> token
  }
}

pub fn request_to_string(request: Request) -> String {
  request_to_json(request) |> json.to_string
}

pub fn request_to_json(request: Request) -> json.Json {
  case request {
    Ping(id, token) -> base_request_entries(id, token, "ping") |> json.object
    ListSessions(id, token) ->
      base_request_entries(id, token, "list_sessions") |> json.object
    GetSession(id, token, session_id) ->
      [
        #("session_id", json.string(session_id)),
        ..base_request_entries(id, token, "get_session")
      ]
      |> json.object
    GetEvents(id, token, session_id, after, limit) ->
      [
        #("session_id", json.string(session_id)),
        #("after", json.int(after)),
        #("limit", json.int(limit)),
        ..base_request_entries(id, token, "get_events")
      ]
      |> json.object
    StreamEvents(id, token, session_id, after) ->
      [
        #("session_id", json.string(session_id)),
        #("after", json.int(after)),
        ..base_request_entries(id, token, "stream_events")
      ]
      |> json.object
  }
}

fn base_request_entries(
  id: String,
  token: String,
  type_: String,
) -> List(#(String, json.Json)) {
  [
    #("version", json.int(version)),
    #("id", json.string(id)),
    #("token", json.string(token)),
    #("type", json.string(type_)),
  ]
}

pub fn decode_request(line: String) -> Result(Request, RequestError) {
  case json.parse(line, request_fields_decoder()) {
    Error(_) -> Error(RequestError("unknown", "bad_json", "malformed JSON"))
    Ok(fields) -> validate_request_fields(fields)
  }
}

fn validate_request_fields(
  fields: RequestFields,
) -> Result(Request, RequestError) {
  case fields.version != version {
    True -> invalid(fields.id, "unsupported protocol version")
    False ->
      case fields.id == "" || fields.token == "" || fields.type_ == "" {
        True -> invalid(fields.id, "missing id, token, or type")
        False -> request_for_type(fields)
      }
  }
}

fn request_for_type(fields: RequestFields) -> Result(Request, RequestError) {
  case fields.type_ {
    "ping" -> Ok(Ping(fields.id, fields.token))
    "list_sessions" -> Ok(ListSessions(fields.id, fields.token))
    "get_session" ->
      case required_session_id(fields) {
        Ok(session_id) -> Ok(GetSession(fields.id, fields.token, session_id))
        Error(err) -> Error(err)
      }
    "get_events" ->
      case required_session_id(fields) {
        Error(err) -> Error(err)
        Ok(session_id) ->
          case fields.limit <= 0 {
            True ->
              Error(RequestError(
                fields.id,
                "invalid_limit",
                "limit must be positive",
              ))
            False ->
              case valid_after(fields) {
                Error(err) -> Error(err)
                Ok(after) ->
                  Ok(GetEvents(
                    fields.id,
                    fields.token,
                    session_id,
                    after,
                    fields.limit,
                  ))
              }
          }
      }
    "stream_events" ->
      case required_session_id(fields) {
        Error(err) -> Error(err)
        Ok(session_id) ->
          case valid_after(fields) {
            Error(err) -> Error(err)
            Ok(after) ->
              Ok(StreamEvents(fields.id, fields.token, session_id, after))
          }
      }
    other ->
      Error(RequestError(
        fields.id,
        "unknown_command",
        "unknown command: " <> other,
      ))
  }
}

fn required_session_id(fields: RequestFields) -> Result(String, RequestError) {
  case fields.session_id {
    Some("") -> invalid(fields.id, "session_id must not be empty")
    Some(session_id) -> Ok(session_id)
    None -> invalid(fields.id, "missing session_id")
  }
}

fn valid_after(fields: RequestFields) -> Result(Int, RequestError) {
  case fields.after < 0 {
    True -> invalid(fields.id, "after must be non-negative")
    False -> Ok(fields.after)
  }
}

fn invalid(id: String, message: String) -> Result(a, RequestError) {
  Error(RequestError(id, "invalid_request", message))
}

fn request_fields_decoder() -> decode.Decoder(RequestFields) {
  use version <- decode.optional_field("version", 0, decode.int)
  use id <- decode.optional_field("id", "", decode.string)
  use token <- decode.optional_field("token", "", decode.string)
  use type_ <- decode.optional_field("type", "", decode.string)
  use session_id <- decode.optional_field(
    "session_id",
    None,
    decode.optional(decode.string),
  )
  use after <- decode.optional_field("after", 0, decode.int)
  use limit <- decode.optional_field("limit", 100, decode.int)
  decode.success(RequestFields(
    version: version,
    id: id,
    token: token,
    type_: type_,
    session_id: session_id,
    after: after,
    limit: limit,
  ))
}

pub fn success_response(id: String, data: json.Json) -> Response {
  Response(id: id, ok: True, data: Some(data), error: None)
}

pub fn error_response(id: String, code: String, message: String) -> Response {
  Response(
    id: id,
    ok: False,
    data: None,
    error: Some(ErrorBody(code: code, message: message)),
  )
}

pub fn request_error_response(error: RequestError) -> Response {
  error_response(error.id, error.code, error.message)
}

pub fn response_to_string(response: Response) -> String {
  response_to_json(response) |> json.to_string
}

pub fn response_to_json(response: Response) -> json.Json {
  let required = [
    #("version", json.int(version)),
    #("id", json.string(response.id)),
    #("ok", json.bool(response.ok)),
  ]
  case response.ok {
    True ->
      [#("data", option_json(response.data)), ..required]
      |> list.reverse
      |> json.object
    False ->
      [#("error", error_body_to_json(option_error(response.error))), ..required]
      |> list.reverse
      |> json.object
  }
}

fn option_json(value: Option(json.Json)) -> json.Json {
  case value {
    Some(json) -> json
    None -> json.null()
  }
}

fn option_error(value: Option(ErrorBody)) -> ErrorBody {
  case value {
    Some(error) -> error
    None -> ErrorBody("unknown_error", "unknown error")
  }
}

fn error_body_to_json(error: ErrorBody) -> json.Json {
  json.object([
    #("code", json.string(error.code)),
    #("message", json.string(error.message)),
  ])
}

pub fn ping_data() -> json.Json {
  json.object([#("pong", json.bool(True))])
}

pub fn list_sessions_data(sessions: List(event.SessionSummary)) -> json.Json {
  json.object([
    #("sessions", json.array(sessions, of: session_json.summary_to_json)),
  ])
}

pub fn session_data(summary: Option(event.SessionSummary)) -> json.Json {
  json.object([
    #("session", json.nullable(summary, of: session_json.summary_to_json)),
  ])
}

pub fn event_page_data(page: event.EventPage) -> json.Json {
  session_json.page_to_json(page)
}

pub fn stream_started_data(session_id: String, after: Int) -> json.Json {
  json.object([
    #("streaming", json.bool(True)),
    #("session_id", json.string(session_id)),
    #("after", json.int(after)),
  ])
}

pub fn stream_event_to_string(
  id: String,
  stored_event: event.SessionEvent,
) -> String {
  json.object([
    #("version", json.int(version)),
    #("id", json.string(id)),
    #("stream", json.bool(True)),
    #("session_id", json.string(stored_event.session_id)),
    #("cursor", json.int(stored_event.cursor)),
    #("event", session_json.event_to_json(stored_event)),
  ])
  |> json.to_string
}

pub fn decode_response(line: String) -> Result(Response, ErrorBody) {
  case json.parse(line, response_decoder()) {
    Ok(response) -> Ok(response)
    Error(_) -> Error(ErrorBody("bad_json", "malformed response JSON"))
  }
}

pub fn decode_ping_response(line: String) -> Result(Nil, ErrorBody) {
  decode_response_result(line, ping_decoder())
}

pub fn decode_list_sessions_response(
  line: String,
) -> Result(List(event.SessionSummary), ErrorBody) {
  decode_response_result(
    line,
    decode.at(["sessions"], decode.list(of: session_summary_decoder())),
  )
}

pub fn decode_get_session_response(
  line: String,
) -> Result(Option(event.SessionSummary), ErrorBody) {
  decode_response_result(
    line,
    decode.at(["session"], decode.optional(session_summary_decoder())),
  )
}

pub fn decode_get_events_response(
  line: String,
) -> Result(event.EventPage, ErrorBody) {
  decode_response_result(line, event_page_decoder())
}

pub fn decode_stream_event(
  line: String,
) -> Result(event.SessionEvent, ErrorBody) {
  case json.parse(line, stream_event_decoder()) {
    Ok(stored_event) -> Ok(stored_event)
    Error(_) -> Error(ErrorBody("bad_json", "malformed stream event JSON"))
  }
}

fn decode_response_result(
  line: String,
  data_decoder: decode.Decoder(a),
) -> Result(a, ErrorBody) {
  case json.parse(line, response_result_decoder(data_decoder)) {
    Ok(result) -> result
    Error(_) -> Error(ErrorBody("bad_json", "malformed response JSON"))
  }
}

fn response_decoder() -> decode.Decoder(Response) {
  use id <- decode.optional_field("id", "unknown", decode.string)
  use ok <- decode.field("ok", decode.bool)
  use data <- decode.optional_field(
    "data",
    None,
    decode.optional(decode.dynamic),
  )
  use error <- decode.optional_field(
    "error",
    None,
    decode.optional(error_body_decoder()),
  )
  let data = option_dynamic_to_json(data)
  decode.success(Response(id: id, ok: ok, data: data, error: error))
}

fn response_result_decoder(
  data_decoder: decode.Decoder(a),
) -> decode.Decoder(Result(a, ErrorBody)) {
  use ok <- decode.field("ok", decode.bool)
  case ok {
    True -> {
      use data <- decode.field("data", data_decoder)
      decode.success(Ok(data))
    }
    False -> {
      use error <- decode.field("error", error_body_decoder())
      decode.success(Error(error))
    }
  }
}

fn ping_decoder() -> decode.Decoder(Nil) {
  use _pong <- decode.optional_field("pong", True, decode.bool)
  decode.success(Nil)
}

fn error_body_decoder() -> decode.Decoder(ErrorBody) {
  use code <- decode.field("code", decode.string)
  use message <- decode.optional_field("message", code, decode.string)
  decode.success(ErrorBody(code: code, message: message))
}

fn session_summary_decoder() -> decode.Decoder(event.SessionSummary) {
  use session_id <- decode.field("session_id", decode.string)
  use issue_id <- decode.field("issue_id", decode.string)
  use issue_identifier <- decode.field("issue_identifier", decode.string)
  use issue_title <- decode.optional_field("issue_title", "", decode.string)
  use workspace_path <- decode.field("workspace_path", decode.string)
  use pi_session_id <- decode.optional_field(
    "pi_session_id",
    None,
    decode.optional(decode.string),
  )
  use status_name <- decode.field("status", decode.string)
  use exit_reason <- decode.optional_field(
    "exit_reason",
    None,
    decode.optional(decode.string),
  )
  use current_turn <- decode.field("current_turn", decode.int)
  use started_at_ms <- decode.field("started_at_ms", decode.int)
  use last_event_at_ms <- decode.field("last_event_at_ms", decode.int)
  use token_totals <- decode.field("tokens", token_totals_decoder())
  decode.success(event.SessionSummary(
    session_id: session_id,
    issue_id: issue_id,
    issue_identifier: issue_identifier,
    issue_title: issue_title,
    workspace_path: workspace_path,
    pi_session_id: pi_session_id,
    status: status_from_string(status_name, exit_reason),
    current_turn: current_turn,
    started_at_ms: started_at_ms,
    last_event_at_ms: last_event_at_ms,
    token_totals: token_totals,
  ))
}

fn event_page_decoder() -> decode.Decoder(event.EventPage) {
  use events <- decode.field("events", decode.list(of: session_event_decoder()))
  use next_cursor <- decode.field("next_cursor", decode.int)
  use truncated <- decode.field("truncated", decode.bool)
  decode.success(event.EventPage(
    events: events,
    next_cursor: next_cursor,
    truncated: truncated,
  ))
}

fn stream_event_decoder() -> decode.Decoder(event.SessionEvent) {
  use stored_event <- decode.field("event", session_event_decoder())
  decode.success(stored_event)
}

fn session_event_decoder() -> decode.Decoder(event.SessionEvent) {
  use cursor <- decode.field("cursor", decode.int)
  use at_ms <- decode.field("at_ms", decode.int)
  use session_id <- decode.field("session_id", decode.string)
  use issue_id <- decode.field("issue_id", decode.string)
  use payload <- decode.then(event_payload_decoder())
  decode.success(event.SessionEvent(
    cursor: cursor,
    at_ms: at_ms,
    session_id: session_id,
    issue_id: issue_id,
    payload: payload,
  ))
}

fn event_payload_decoder() -> decode.Decoder(event.EventPayload) {
  use kind <- decode.field("kind", event_kind_decoder())
  use name <- decode.field("name", decode.string)
  use turn <- decode.optional_field("turn", None, decode.optional(decode.int))
  use pi_type <- decode.optional_field(
    "pi_type",
    None,
    decode.optional(decode.string),
  )
  use message <- decode.optional_field(
    "message",
    None,
    decode.optional(decode.string),
  )
  use request_id <- decode.optional_field(
    "request_id",
    None,
    decode.optional(decode.string),
  )
  use method <- decode.optional_field(
    "method",
    None,
    decode.optional(decode.string),
  )
  use tool_name <- decode.optional_field(
    "tool_name",
    None,
    decode.optional(decode.string),
  )
  use tokens <- decode.optional_field(
    "tokens",
    domain.zero_token_totals(),
    token_totals_decoder(),
  )
  use raw_json <- decode.optional_field(
    "raw_json",
    None,
    decode.optional(redacted_raw_json_decoder()),
  )
  decode.success(event.EventPayload(
    kind: kind,
    name: name,
    turn: turn,
    pi_type: pi_type,
    message: message,
    request_id: request_id,
    method: method,
    tool_name: tool_name,
    tokens: tokens,
    raw_json: raw_json,
  ))
}

fn token_totals_decoder() -> decode.Decoder(domain.TokenTotals) {
  use input <- decode.optional_field("input", 0, decode.int)
  use output <- decode.optional_field("output", 0, decode.int)
  use cache_read <- decode.optional_field("cache_read", 0, decode.int)
  use cache_write <- decode.optional_field("cache_write", 0, decode.int)
  use total <- decode.optional_field("total", 0, decode.int)
  decode.success(domain.TokenTotals(
    input: input,
    output: output,
    cache_read: cache_read,
    cache_write: cache_write,
    total: total,
  ))
}

fn redacted_raw_json_decoder() -> decode.Decoder(event.RedactedRawJson) {
  use value <- decode.field("value", decode.string)
  use truncated <- decode.field("truncated", decode.bool)
  decode.success(event.RedactedRawJson(value: value, truncated: truncated))
}

fn event_kind_decoder() -> decode.Decoder(event.EventKind) {
  use name <- decode.then(decode.string)
  decode.success(kind_from_string(name))
}

fn kind_from_string(name: String) -> event.EventKind {
  case name {
    "lifecycle" -> event.Lifecycle
    "pi" -> event.Pi
    "assistant_message" -> event.AssistantMessage
    "tool" -> event.Tool
    "ui_request" -> event.UiRequest
    "ui_response" -> event.UiResponse
    "token_stats" -> event.TokenStats
    "error" -> event.Error
    "pi_raw" -> event.PiRaw
    _ -> event.PiRaw
  }
}

fn status_from_string(
  name: String,
  exit_reason: Option(String),
) -> event.SessionStatus {
  case name {
    "preparing" -> event.Preparing
    "probing" -> event.Probing
    "running" -> event.Running
    "waiting_ui" -> event.WaitingUi
    "stopping" -> event.Stopping
    "exited" ->
      case exit_reason {
        Some(reason) -> event.Exited(reason)
        None -> event.Exited("unknown")
      }
    _ -> event.Exited("unknown_status:" <> name)
  }
}

fn option_dynamic_to_json(data: Option(Dynamic)) -> Option(json.Json) {
  case data {
    Some(value) -> Some(dynamic_to_json(value))
    None -> None
  }
}

@external(erlang, "scherzo_control_ffi", "dynamic_to_json")
fn dynamic_to_json(value: Dynamic) -> json.Json
