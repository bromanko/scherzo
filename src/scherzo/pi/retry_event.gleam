import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/error
import scherzo/json_value.{type JsonValue}
import scherzo/pi/protocol

pub type AutoRetryEvent {
  AutoRetryStart(
    attempt: Option(Int),
    max_attempts: Option(Int),
    delay_ms: Option(Int),
    error_message: Option(String),
  )
  AutoRetryEnd(success: Bool, attempt: Option(Int), final_error: Option(String))
}

pub fn from_record(record: protocol.RpcRecord) -> Option(AutoRetryEvent) {
  case record.type_ {
    "auto_retry_start" -> auto_retry_start(record)
    "auto_retry_end" -> auto_retry_end(record)
    _ -> None
  }
}

pub fn retryable_pi_error(error: error.PiRpcError) -> Bool {
  case error {
    error.PiProtocolError(detail) -> retryable_detail(detail)
    error.PiLaunchFailed(_)
    | error.PiMalformedJson(_)
    | error.PiReadTimeout
    | error.PiTurnTimeout
    | error.PiStallTimeout
    | error.PiExited(_)
    | error.PiContextWindowExhausted(..) -> False
  }
}

pub fn retryable_agent_error(error: error.AgentRunnerError) -> Bool {
  case error {
    error.PiFailed(pi_error) -> retryable_pi_error(pi_error)
    error.ProbeFailed(pi_error) -> retryable_pi_error(pi_error)
    _ -> False
  }
}

fn retryable_detail(detail: String) -> Bool {
  let normalized = detail |> string.lowercase
  list.any(retryable_needles(), fn(needle) {
    string.contains(normalized, needle)
  })
}

fn retryable_needles() -> List(String) {
  [
    "provider_transport_failure",
    "websocket error",
    "websocket closed",
    "connection reset",
    "connection refused",
    "connection lost",
    "econnreset",
    "etimedout",
    "fetch failed",
    "socket hang up",
    "ended without",
    "http2 request did not get a response",
    "terminated",
    "overloaded",
    "rate limit",
    "too many requests",
    "429",
    "500",
    "502",
    "503",
    "504",
    "service unavailable",
    "server error",
  ]
}

fn auto_retry_start(record: protocol.RpcRecord) -> Option(AutoRetryEvent) {
  let value = parsed_raw_json(record.raw_json)
  Some(AutoRetryStart(
    attempt: optional_int_at(value, "attempt"),
    max_attempts: optional_int_at(value, "maxAttempts"),
    delay_ms: optional_int_at(value, "delayMs"),
    error_message: first_optional_string([
      record.error_message,
      optional_string_at(value, "errorMessage"),
      optional_string_at(value, "error_message"),
    ]),
  ))
}

fn auto_retry_end(record: protocol.RpcRecord) -> Option(AutoRetryEvent) {
  let value = parsed_raw_json(record.raw_json)
  let success = case record.success {
    Some(success) -> success
    None -> optional_bool_at(value, "success") == Some(True)
  }
  Some(AutoRetryEnd(
    success: success,
    attempt: optional_int_at(value, "attempt"),
    final_error: first_optional_string([
      optional_string_at(value, "finalError"),
      optional_string_at(value, "final_error"),
      record.error_message,
    ]),
  ))
}

fn parsed_raw_json(raw_json: String) -> Option(JsonValue) {
  case json_value.parse(raw_json) {
    Ok(value) -> Some(value)
    Error(Nil) -> None
  }
}

fn optional_int_at(value: Option(JsonValue), key: String) -> Option(Int) {
  case value {
    Some(json_value.JObject(entries)) ->
      case object_get(entries, key) {
        Some(json_value.JInt(value)) -> Some(value)
        _ -> None
      }
    _ -> None
  }
}

fn optional_string_at(value: Option(JsonValue), key: String) -> Option(String) {
  case value {
    Some(json_value.JObject(entries)) ->
      case object_get(entries, key) {
        Some(json_value.JString(value)) -> non_empty(value)
        _ -> None
      }
    _ -> None
  }
}

fn optional_bool_at(value: Option(JsonValue), key: String) -> Option(Bool) {
  case value {
    Some(json_value.JObject(entries)) ->
      case object_get(entries, key) {
        Some(json_value.JBool(value)) -> Some(value)
        _ -> None
      }
    _ -> None
  }
}

fn object_get(
  entries: List(#(String, JsonValue)),
  key: String,
) -> Option(JsonValue) {
  case entries {
    [] -> None
    [#(entry_key, value), ..rest] ->
      case entry_key == key {
        True -> Some(value)
        False -> object_get(rest, key)
      }
  }
}

fn first_optional_string(values: List(Option(String))) -> Option(String) {
  case values {
    [] -> None
    [value, ..rest] ->
      case value {
        Some(text) ->
          case non_empty(text) {
            Some(text) -> Some(text)
            None -> first_optional_string(rest)
          }
        None -> first_optional_string(rest)
      }
  }
}

fn non_empty(value: String) -> Option(String) {
  let trimmed = string.trim(value)
  case trimmed == "" {
    True -> None
    False -> Some(value)
  }
}
