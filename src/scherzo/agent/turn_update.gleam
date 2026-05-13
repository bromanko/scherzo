import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/pi_event
import scherzo/agent/types
import scherzo/log
import scherzo/pi/protocol
import scherzo/session/redaction
import scherzo/session/tokens as session_tokens

const max_tool_text_chars = 4096

const tool_text_truncated_suffix = "… [truncated]"

// While an operator UI request is pending, keep stdout reads short so
// command-subject responses are observed before the UI deadline expires.
const pending_ui_command_poll_ms = 50

pub fn read_timeout_for_pending_ui(
  configured_read_timeout_ms: Int,
  pending_ui: Option(a),
) -> Int {
  case pending_ui {
    Some(_) -> min_int(configured_read_timeout_ms, pending_ui_command_poll_ms)
    None -> configured_read_timeout_ms
  }
}

pub fn emit_operator_prompt_queued(
  issue_id: String,
  message: String,
  secrets: List(String),
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
) -> Nil {
  emit_update(
    issue_id,
    lifecycle_update_with_message(
      pi_event.OperatorPromptQueued,
      Some(redact_operator_message(message, secrets)),
    ),
  )
}

pub fn emit_records(
  issue_id: String,
  records: List(protocol.RpcRecord),
  turn: Int,
  secrets: List(String),
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
) -> Nil {
  case records {
    [] -> Nil
    [record, ..rest] -> {
      emit_update(issue_id, update_from_record(record, turn, secrets))
      emit_records(issue_id, rest, turn, secrets, emit_update)
    }
  }
}

pub fn lifecycle_update_with_request(
  name: pi_event.PiEvent,
  message: Option(String),
  request_id: String,
  method: String,
  turn: Int,
) -> types.RunnerUpdate {
  pi_runner_update(types.PiUpdate(
    event: name,
    message: message,
    raw_json: None,
    turn: Some(turn),
    request_id: Some(request_id),
    method: Some(method),
    pi_session_id: None,
    tokens: session_tokens.zero_token_totals(),
    tool_name: None,
    tool_input: None,
    tool_output: None,
    tool_status: None,
  ))
}

pub fn update_from_record(
  record: protocol.RpcRecord,
  turn: Int,
  secrets: List(String),
) -> types.RunnerUpdate {
  let event = pi_event.from_string(record.type_)
  let message = case event {
    pi_event.ExtensionUiRequest -> record.message
    _ -> record.delta
  }
  pi_runner_update(types.PiUpdate(
    event: event,
    message: redact_message(message, secrets),
    raw_json: Some(redaction.redact_raw_json(record.raw_json, secrets)),
    turn: Some(turn),
    request_id: record.id,
    method: record.method,
    pi_session_id: record.session_id,
    tokens: record.tokens,
    tool_name: record.tool_name,
    tool_input: normalize_tool_text(record.tool_input, secrets),
    tool_output: normalize_tool_text(record.tool_output, secrets),
    tool_status: normalize_tool_text(record.tool_status, secrets),
  ))
}

fn lifecycle_update_with_message(
  name: pi_event.PiEvent,
  message: Option(String),
) -> types.RunnerUpdate {
  pi_runner_update(types.PiUpdate(
    event: name,
    message: message,
    raw_json: None,
    turn: None,
    request_id: None,
    method: None,
    pi_session_id: None,
    tokens: session_tokens.zero_token_totals(),
    tool_name: None,
    tool_input: None,
    tool_output: None,
    tool_status: None,
  ))
}

fn pi_runner_update(update: types.PiUpdate) -> types.RunnerUpdate {
  types.RunnerPiUpdate(update)
}

fn min_int(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}

fn redact_operator_message(message: String, secrets: List(String)) -> String {
  log.redact("message", log.truncate(message, 200), secrets)
}

fn redact_message(
  message: Option(String),
  secrets: List(String),
) -> Option(String) {
  case message {
    Some(value) -> Some(log.redact("message", value, secrets))
    None -> None
  }
}

fn normalize_tool_text(
  value: Option(String),
  secrets: List(String),
) -> Option(String) {
  case value {
    None -> None
    Some(text) -> {
      let redacted = log.redact("tool", text, secrets)
      case string.length(redacted) > max_tool_text_chars {
        True ->
          Some(
            string.slice(redacted, at_index: 0, length: max_tool_text_chars)
            <> tool_text_truncated_suffix,
          )
        False -> Some(redacted)
      }
    }
  }
}
