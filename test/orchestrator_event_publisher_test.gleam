import gleam/option.{type Option, None, Some}
import scherzo/agent/pi_event
import scherzo/agent/types as agent_types
import scherzo/orchestrator/event_publisher
import scherzo/session/event
import scherzo/session/hub
import scherzo/session/tokens as session_tokens
import scherzo/turn_telemetry

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

pub fn turn_update_payload_is_sanitized_for_all_lifecycle_names_test() {
  assert_turn_payload(turn_telemetry.EventStarted, None)
  assert_turn_payload(turn_telemetry.EventFinished, None)
  assert_turn_payload(
    turn_telemetry.EventFailed,
    Some(turn_telemetry.ReasonPiError),
  )
  assert_turn_payload(
    turn_telemetry.EventStopped,
    Some(turn_telemetry.ReasonOperatorStopAfterCurrentTurn),
  )
  assert_turn_payload(
    turn_telemetry.EventTimedOut,
    Some(turn_telemetry.ReasonPiStallTimeout),
  )
}

pub fn worker_update_turn_bridge_computes_token_delta_with_real_hub_test() {
  let assert Ok(subject) = hub.start(10, fn() { 1000 })
  hub.register_session(subject, summary("session-1"))

  event_publisher.worker_update(
    subject,
    "session-1",
    agent_types.RunnerTurnUpdate(turn_update(
      turn_telemetry.EventStarted,
      1,
      session_tokens.zero_token_totals(),
      None,
    )),
  )
  event_publisher.worker_update(
    subject,
    "session-1",
    agent_types.RunnerTurnUpdate(turn_update(
      turn_telemetry.EventFinished,
      1,
      token_totals(10, 5, 0, 0, 15),
      None,
    )),
  )

  let assert Ok(Some(summary)) = hub.get_session(subject, "session-1", 1000)
  assert summary.token_totals.total == 15
  assert summary.last_turn_token_delta.total == 15
  let assert Ok(page) = hub.events_after(subject, "session-1", 0, 10, 1000)
  let assert [_, finished] = page.events
  assert finished.payload.token_delta.total == 15
  hub.stop(subject)
}

pub fn worker_update_turn_bridge_terminal_does_not_retain_secrets_test() {
  let assert Ok(subject) = hub.start(10, fn() { 1000 })
  hub.register_session(subject, summary("session-1"))
  event_publisher.worker_update(
    subject,
    "session-1",
    agent_types.RunnerTurnUpdate(turn_update(
      turn_telemetry.EventStopped,
      1,
      session_tokens.zero_token_totals(),
      Some(turn_telemetry.ReasonOperatorStopAfterCurrentTurn),
    )),
  )

  let assert Ok(Some(summary)) = hub.get_session(subject, "session-1", 1000)
  assert summary.current_turn_status == Some(turn_telemetry.StatusStopped)
  assert summary.last_turn_reason
    == Some(turn_telemetry.ReasonOperatorStopAfterCurrentTurn)
  let assert Ok(page) = hub.events_after(subject, "session-1", 0, 10, 1000)
  let assert [stored_event] = page.events
  assert stored_event.payload.message == None
  assert stored_event.payload.raw_json == None
  hub.stop(subject)
}

fn assert_turn_payload(
  name: turn_telemetry.TurnEventName,
  reason: Option(turn_telemetry.TurnReason),
) -> Nil {
  let payload =
    event_publisher.turn_update_payload(turn_update(
      name,
      3,
      token_totals(1, 2, 0, 0, 3),
      reason,
    ))
  assert payload.kind == event.Turn
  assert payload.name == event.TurnName(name)
  assert payload.turn == Some(3)
  assert payload.raw_json == None
  assert payload.message == None
  assert payload.tool_input == None
  assert payload.tool_output == None
  assert payload.reason == reason
}

fn turn_update(
  name: turn_telemetry.TurnEventName,
  turn: Int,
  tokens: session_tokens.TokenTotals,
  reason: Option(turn_telemetry.TurnReason),
) -> turn_telemetry.TurnLifecycleUpdate {
  turn_telemetry.TurnLifecycleUpdate(
    name: name,
    turn: turn,
    tokens: tokens,
    reason: reason,
  )
}

fn summary(session_id: String) -> event.SessionSummary {
  event.SessionSummary(
    session_id: session_id,
    display_name: session_id,
    issue_id: "issue-1",
    issue_identifier: "ABC-1",
    issue_title: "Turn telemetry",
    workspace_path: "test/tmp/workspaces/ABC-1",
    pi_session_id: None,
    status: event.Running,
    recovery: None,
    current_turn: 0,
    current_turn_status: None,
    current_turn_started_at_ms: None,
    last_turn_finished_at_ms: None,
    last_turn_duration_ms: None,
    last_turn_token_delta: session_tokens.zero_token_totals(),
    last_turn_reason: None,
    started_at_ms: 0,
    last_event_at_ms: 0,
    token_totals: session_tokens.zero_token_totals(),
  )
}

fn token_totals(
  input: Int,
  output: Int,
  cache_read: Int,
  cache_write: Int,
  total: Int,
) -> session_tokens.TokenTotals {
  session_tokens.TokenTotals(
    input: input,
    output: output,
    cache_read: cache_read,
    cache_write: cache_write,
    total: total,
  )
}
