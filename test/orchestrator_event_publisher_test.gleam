import gleam/option.{type Option, None, Some}
import scherzo/agent/pi_event
import scherzo/agent/types as agent_types
import scherzo/orchestrator/event_publisher
import scherzo/session/event
import scherzo/session/tokens as session_tokens

fn update(
  event_name: String,
  method: Option(String),
  raw_json: Option(event.RedactedRawJson),
  tool_name: Option(String),
  tokens: session_tokens.TokenTotals,
) -> agent_types.PiUpdate {
  agent_types.PiUpdate(
    event: pi_event.from_string(event_name),
    message: None,
    raw_json: raw_json,
    turn: None,
    request_id: None,
    method: method,
    pi_session_id: None,
    tokens: tokens,
    tool_name: tool_name,
    tool_input: None,
    tool_output: None,
    tool_status: None,
  )
}

pub fn event_publisher_classifies_raw_unknown_pi_event_test() {
  let payload =
    event_publisher.update_payload(update(
      "unknown_event",
      None,
      Some(event.RedactedRawJson("{\"event\":\"unknown_event\"}", False)),
      None,
      session_tokens.zero_token_totals(),
    ))
  assert payload.kind == event.PiRaw
  assert payload.pi_type == Some("unknown_event")
}

pub fn event_publisher_classifies_blocking_ui_request_test() {
  let payload =
    event_publisher.update_payload(update(
      "extension_ui_request",
      Some("input"),
      None,
      None,
      session_tokens.zero_token_totals(),
    ))
  assert payload.kind == event.UiRequest
}

pub fn event_publisher_classifies_nonblocking_ui_request_as_pi_test() {
  let payload =
    event_publisher.update_payload(update(
      "extension_ui_request",
      Some("notify"),
      None,
      None,
      session_tokens.zero_token_totals(),
    ))
  assert payload.kind == event.Pi
}

pub fn event_publisher_classifies_tool_shaped_message_test() {
  let payload =
    event_publisher.update_payload(update(
      "message",
      None,
      None,
      Some("shell"),
      session_tokens.zero_token_totals(),
    ))
  assert payload.kind == event.Tool
}

pub fn event_publisher_classifies_turn_finished_tokens_test() {
  let tokens =
    session_tokens.TokenTotals(
      input: 1,
      output: 2,
      cache_read: 3,
      cache_write: 4,
      total: 10,
    )
  let payload =
    event_publisher.update_payload(update(
      "turn_finished",
      None,
      None,
      None,
      tokens,
    ))
  assert payload.kind == event.TokenStats
  assert payload.tokens == tokens
}
