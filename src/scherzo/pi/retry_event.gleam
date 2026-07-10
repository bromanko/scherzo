import gleam/option.{type Option, None, Some}
import gleam/string
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

pub fn agent_end_will_retry(record: protocol.RpcRecord) -> Option(Bool) {
  case record.type_ {
    "agent_end" ->
      parsed_raw_json(record.raw_json) |> optional_bool_at("willRetry")
    _ -> None
  }
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
