import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/session/event
import scherzo/session/hub
import scherzo/session/json as session_json
import scherzo/session/reason
import scherzo/session/tokens as session_tokens
import scherzo/turn_telemetry

fn summary(session_id: String) -> event.SessionSummary {
  event.SessionSummary(
    session_id: session_id,
    display_name: session_id,
    issue_id: "issue-1",
    issue_identifier: "ABC-123",
    issue_title: "Fix tests",
    workspace_path: "test/tmp/workspaces/ABC-123",
    pi_session_id: None,
    status: event.Preparing,
    recovery: None,
    current_turn: 0,
    current_turn_status: None,
    current_turn_started_at_ms: None,
    last_turn_finished_at_ms: None,
    last_turn_duration_ms: None,
    last_turn_token_delta: session_tokens.zero_token_totals(),
    last_turn_reason: None,
    started_at_ms: 10,
    last_event_at_ms: 10,
    token_totals: session_tokens.zero_token_totals(),
  )
}

fn payload(name: String) -> event.EventPayload {
  let assert Ok(event_name) = event.name_from_string(name)
  event.empty_payload(event.Lifecycle, event_name)
}

fn interrupted_recovery(run_id: String) -> event.RecoveryInfo {
  event.RecoveryInfo(
    status: event.Interrupted,
    source: "projection.run_interrupted",
    message: Some("daemon_restart"),
    safe_actions: [event.Inspect, event.ViewEvents, event.Retry, event.Park],
    workflow_run_id: Some(run_id),
    workflow_step_id: None,
    current_pi_session_id: None,
    previous_pi_session_id: None,
    park_reason: None,
    park_release_policy: None,
    parked_at_ms: None,
    drift_kind: None,
    retention_until_ms: None,
    cleanup_eligible_at_ms: None,
    cleanup_phase: None,
  )
}

pub fn hub_registers_lists_and_finishes_session_test() {
  let assert Ok(subject) = hub.start(10, fn() { 123 })

  hub.register_session(subject, summary("session-1"))
  let assert Ok([registered]) = hub.list_sessions(subject, 1000)
  assert registered.status == event.Preparing

  hub.update_status(subject, "session-1", event.Running)
  let assert Ok(Some(running)) = hub.get_session(subject, "session-1", 1000)
  assert running.status == event.Running

  hub.finish_session(subject, "session-1", reason.Normal)
  let assert Ok(Some(finished)) = hub.get_session(subject, "session-1", 1000)
  assert finished.status == event.Exited(reason.Normal)
  hub.stop(subject)
}

pub fn hub_assigns_monotonic_cursors_and_timestamps_test() {
  let assert Ok(subject) = hub.start(10, fn() { 456 })
  hub.register_session(subject, summary("session-1"))
  hub.publish(subject, "session-1", payload("one"))
  hub.publish(subject, "session-1", payload("two"))
  hub.publish(subject, "session-1", payload("three"))

  let assert Ok(page) = hub.events_after(subject, "session-1", 0, 10, 1000)
  assert event_cursors(page.events) == [1, 2, 3]
  assert event_timestamps(page.events) == [456, 456, 456]
  hub.stop(subject)
}

pub fn hub_updates_summary_recovery_from_payload_test() {
  let assert Ok(subject) = hub.start(10, fn() { 789 })
  hub.register_session(subject, summary("session-1"))
  let recovery = interrupted_recovery("run-1")
  hub.publish(
    subject,
    "session-1",
    event.EventPayload(
      ..payload("recovery_interrupted"),
      recovery: Some(recovery),
      message: Some("daemon_restart"),
    ),
  )

  let assert Ok(Some(updated)) = hub.get_session(subject, "session-1", 1000)
  let assert Some(summary_recovery) = updated.recovery
  assert summary_recovery.status == event.Interrupted
  assert summary_recovery.workflow_run_id == Some("run-1")
  assert updated.last_event_at_ms == 789

  let assert Ok(page) = hub.events_after(subject, "session-1", 0, 10, 1000)
  let assert [stored_event] = page.events
  let assert Some(event_recovery) = stored_event.payload.recovery
  assert event_recovery.source == "projection.run_interrupted"
  hub.stop(subject)
}

pub fn hub_replays_events_after_cursor_without_duplicates_test() {
  let assert Ok(subject) = hub.start(10, fn() { 1 })
  hub.register_session(subject, summary("session-1"))
  hub.publish(subject, "session-1", payload("one"))
  hub.publish(subject, "session-1", payload("two"))
  hub.publish(subject, "session-1", payload("three"))

  let assert Ok(page) = hub.events_after(subject, "session-1", 1, 10, 1000)
  assert event_cursors(page.events) == [2, 3]
  assert page.next_cursor == 3
  assert page.truncated == False
  hub.stop(subject)
}

pub fn hub_truncates_old_events_test() {
  let assert Ok(subject) = hub.start(2, fn() { 1 })
  hub.register_session(subject, summary("session-1"))
  hub.publish(subject, "session-1", payload("one"))
  hub.publish(subject, "session-1", payload("two"))
  hub.publish(subject, "session-1", payload("three"))

  let assert Ok(page) = hub.events_after(subject, "session-1", 0, 10, 1000)
  assert event_cursors(page.events) == [2, 3]
  assert page.next_cursor == 3
  assert page.truncated == True
  hub.stop(subject)
}

pub fn hub_rejects_invalid_replay_limit_test() {
  let assert Ok(subject) = hub.start(2, fn() { 1 })
  hub.register_session(subject, summary("session-1"))
  hub.publish(subject, "session-1", payload("one"))
  hub.publish(subject, "session-1", payload("two"))
  hub.publish(subject, "session-1", payload("three"))

  assert hub.events_after(subject, "session-1", 0, 0, 1000)
    == Error(hub.InvalidLimit(0))
  let assert Ok(page) = hub.events_after(subject, "session-1", 0, 999, 1000)
  assert list.length(page.events) == 2
  hub.stop(subject)
}

pub fn hub_evicts_oldest_sessions_when_session_limit_is_reached_test() {
  let assert Ok(subject) = hub.start_with_limits(2, 10, fn() { 1 })
  hub.register_session(subject, summary("session-1"))
  hub.publish(subject, "session-1", payload("one"))
  hub.register_session(subject, summary("session-2"))
  hub.register_session(subject, summary("session-3"))

  let assert Ok(sessions) = hub.list_sessions(subject, 1000)
  assert list.length(sessions) == 2
  assert hub.get_session(subject, "session-1", 1000) == Ok(None)
  let assert Ok(Some(_)) = hub.get_session(subject, "session-2", 1000)
  let assert Ok(Some(_)) = hub.get_session(subject, "session-3", 1000)
  assert hub.events_after(subject, "session-1", 0, 10, 1000)
    == Error(hub.SessionNotFound("session-1"))
  hub.stop(subject)
}

pub fn turn_started_updates_session_summary_test() {
  let assert Ok(subject) = hub.start(10, fn() { 1000 })
  hub.register_session(subject, summary("session-1"))
  hub.publish(
    subject,
    "session-1",
    turn_payload(turn_telemetry.EventStarted, 1),
  )

  let assert Ok(Some(summary)) = hub.get_session(subject, "session-1", 1000)
  assert summary.current_turn == 1
  assert summary.current_turn_status == Some(turn_telemetry.StatusRunning)
  assert summary.current_turn_started_at_ms == Some(1000)
  hub.stop(subject)
}

pub fn turn_finished_computes_duration_and_token_delta_test() {
  let assert Ok(subject) = hub.start(10, fn() { 2500 })
  hub.register_session(
    subject,
    event.SessionSummary(
      ..summary("session-1"),
      current_turn: 1,
      current_turn_status: Some(turn_telemetry.StatusRunning),
      current_turn_started_at_ms: Some(1000),
    ),
  )
  hub.publish(
    subject,
    "session-1",
    event.EventPayload(
      ..turn_payload(turn_telemetry.EventFinished, 1),
      tokens: token_totals(10, 5, 0, 0, 15),
    ),
  )

  let assert Ok(Some(summary)) = hub.get_session(subject, "session-1", 1000)
  assert summary.current_turn_status == Some(turn_telemetry.StatusFinished)
  assert summary.last_turn_duration_ms == Some(1500)
  assert summary.token_totals.total == 15
  assert summary.last_turn_token_delta.total == 15
  let assert Ok(page) = hub.events_after(subject, "session-1", 0, 10, 1000)
  let assert [finished] = page.events
  assert finished.payload.turn_duration_ms == Some(1500)
  assert finished.payload.token_delta.total == 15
  hub.stop(subject)
}

pub fn turn_terminal_paths_set_status_reason_and_sanitize_test() {
  assert_terminal(
    turn_telemetry.EventFailed,
    turn_telemetry.StatusFailed,
    turn_telemetry.ReasonPiError,
  )
  assert_terminal(
    turn_telemetry.EventStopped,
    turn_telemetry.StatusStopped,
    turn_telemetry.ReasonOperatorStopAfterCurrentTurn,
  )
  assert_terminal(
    turn_telemetry.EventTimedOut,
    turn_telemetry.StatusTimedOut,
    turn_telemetry.ReasonPiStallTimeout,
  )
}

pub fn turn_payload_sanitization_strips_message_tool_and_raw_json_test() {
  let assert Ok(subject) = hub.start(10, fn() { 1000 })
  hub.register_session(subject, summary("session-1"))
  hub.publish(
    subject,
    "session-1",
    event.EventPayload(
      ..turn_payload(turn_telemetry.EventStarted, 1),
      message: Some("SECRET_PROMPT"),
      tool_input: Some("tool_input_value"),
      tool_output: Some("full transcript"),
      tool_status: Some("secret status"),
      raw_json: Some(event.RedactedRawJson(
        value: "{\"secret\":true}",
        truncated: False,
      )),
    ),
  )

  let assert Ok(page) = hub.events_after(subject, "session-1", 0, 10, 1000)
  let assert [stored_event] = page.events
  assert stored_event.payload.message == None
  assert stored_event.payload.tool_input == None
  assert stored_event.payload.raw_json == None
  let encoded = session_json.event_to_string(stored_event)
  assert !string.contains(encoded, "SECRET_PROMPT")
  assert !string.contains(encoded, "full transcript")
  assert !string.contains(encoded, "tool_input_value")
  assert !string.contains(encoded, "{\"secret\":true}")
  hub.stop(subject)
}

fn assert_terminal(
  name: turn_telemetry.TurnEventName,
  status: turn_telemetry.TurnStatus,
  reason: turn_telemetry.TurnReason,
) -> Nil {
  let assert Ok(subject) = hub.start(10, fn() { 1500 })
  hub.register_session(
    subject,
    event.SessionSummary(
      ..summary("session-terminal"),
      current_turn: 1,
      current_turn_status: Some(turn_telemetry.StatusRunning),
      current_turn_started_at_ms: Some(1000),
    ),
  )
  hub.publish(
    subject,
    "session-terminal",
    event.EventPayload(
      ..turn_payload(name, 1),
      reason: Some(reason),
      raw_json: Some(event.RedactedRawJson(
        value: "SECRET_PROMPT",
        truncated: False,
      )),
    ),
  )
  let assert Ok(Some(summary)) =
    hub.get_session(subject, "session-terminal", 1000)
  assert summary.current_turn_status == Some(status)
  assert summary.last_turn_reason == Some(reason)
  let assert Ok(page) =
    hub.events_after(subject, "session-terminal", 0, 10, 1000)
  let assert [terminal] = page.events
  assert terminal.payload.reason == Some(reason)
  assert terminal.payload.raw_json == None
  hub.stop(subject)
}

fn turn_payload(
  name: turn_telemetry.TurnEventName,
  turn: Int,
) -> event.EventPayload {
  event.EventPayload(
    ..event.empty_payload(event.Turn, event.TurnName(name)),
    turn: Some(turn),
    turn_status: turn_telemetry.status_for_event_name(name),
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

fn event_cursors(events: List(event.SessionEvent)) -> List(Int) {
  list.map(events, fn(stored_event) { stored_event.cursor })
}

fn event_timestamps(events: List(event.SessionEvent)) -> List(Int) {
  list.map(events, fn(stored_event) { stored_event.at_ms })
}
