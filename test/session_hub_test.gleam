import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/agent/pi_event
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
  event.decoded_empty_payload(event.Lifecycle, name)
}

fn interrupted_recovery(run_id: String) -> event.RecoveryInfo {
  event.RecoveryInfo(
    status: event.Interrupted,
    source: "projection.run_interrupted",
    message: Some("daemon_restart"),
    safe_actions: [event.Inspect, event.ViewEvents, event.Retry, event.Park],
    workflow_run_id: Some(run_id),
    workflow_step_id: None,
    workflow_attempt_index: None,
    parent_session_id: None,
    orphan_status: None,
    issue_state: None,
    recommended_action: None,
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

pub fn hub_aggregates_child_step_progress_into_parent_summary_test() {
  let assert Ok(subject) = hub.start(10, fn() { 2000 })
  hub.register_session(subject, summary("parent-run"))
  hub.register_session(
    subject,
    event.SessionSummary(
      ..summary("child-step"),
      recovery: Some(
        event.RecoveryInfo(
          ..interrupted_recovery("parent-run"),
          parent_session_id: Some("parent-run"),
          workflow_step_id: Some("implement"),
        ),
      ),
    ),
  )

  hub.publish(
    subject,
    "child-step",
    turn_payload(turn_telemetry.EventStarted, 1),
  )
  hub.publish(
    subject,
    "child-step",
    turn_payload(turn_telemetry.EventFinished, 1)
      |> event.with_payload_tokens(token_totals(3, 4, 5, 6, 18)),
  )

  let assert Ok(Some(parent)) = hub.get_session(subject, "parent-run", 1000)
  assert parent.current_turn == 1
  assert parent.current_turn_status == Some(turn_telemetry.StatusFinished)
  assert parent.last_turn_token_delta.total == 18
  assert parent.token_totals.total == 18
  assert parent.last_event_at_ms == 2000
  let assert Ok([child, listed_parent]) = hub.list_sessions(subject, 1000)
  assert child.session_id == "child-step"
  assert listed_parent.session_id == "parent-run"
  hub.stop(subject)
}

pub fn hub_finish_session_finalizes_running_turn_summary_test() {
  let assert Ok(subject) = hub.start(10, fn() { 2500 })
  hub.register_session(
    subject,
    event.SessionSummary(
      ..summary("session-running-turn"),
      current_turn: 2,
      current_turn_status: Some(turn_telemetry.StatusRunning),
      current_turn_started_at_ms: Some(1000),
    ),
  )

  hub.finish_session(subject, "session-running-turn", reason.Failed)

  let assert Ok(Some(finished)) =
    hub.get_session(subject, "session-running-turn", 1000)
  assert finished.status == event.Exited(reason.Failed)
  assert finished.current_turn_status == Some(turn_telemetry.StatusFailed)
  assert finished.last_turn_reason == Some(turn_telemetry.ReasonPiError)
  assert finished.last_turn_finished_at_ms == Some(2500)
  assert finished.last_turn_duration_ms == Some(1500)
  hub.stop(subject)
}

pub fn hub_finish_session_preserves_operator_abort_turn_reason_test() {
  let assert Ok(subject) = hub.start(10, fn() { 2500 })
  hub.register_session(
    subject,
    event.SessionSummary(
      ..summary("session-operator-abort-turn"),
      current_turn: 2,
      current_turn_status: Some(turn_telemetry.StatusRunning),
      current_turn_started_at_ms: Some(1000),
    ),
  )

  hub.finish_session(
    subject,
    "session-operator-abort-turn",
    reason.OperatorAbort,
  )

  let assert Ok(Some(finished)) =
    hub.get_session(subject, "session-operator-abort-turn", 1000)
  assert finished.status == event.Exited(reason.OperatorAbort)
  assert finished.current_turn_status == Some(turn_telemetry.StatusStopped)
  assert finished.last_turn_reason == Some(turn_telemetry.ReasonOperatorAbort)
  assert finished.last_turn_finished_at_ms == Some(2500)
  assert finished.last_turn_duration_ms == Some(1500)
  hub.stop(subject)
}

pub fn hub_finish_session_preserves_stopped_turn_reason_test() {
  let assert Ok(subject) = hub.start(10, fn() { 2500 })
  hub.register_session(
    subject,
    event.SessionSummary(
      ..summary("session-stopped-turn"),
      current_turn: 2,
      current_turn_status: Some(turn_telemetry.StatusRunning),
      current_turn_started_at_ms: Some(1000),
    ),
  )

  hub.finish_session(subject, "session-stopped-turn", reason.Stopped)

  let assert Ok(Some(finished)) =
    hub.get_session(subject, "session-stopped-turn", 1000)
  assert finished.status == event.Exited(reason.Stopped)
  assert finished.current_turn_status == Some(turn_telemetry.StatusStopped)
  assert finished.last_turn_reason
    == Some(turn_telemetry.ReasonOperatorStopAfterCurrentTurn)
  assert finished.last_turn_finished_at_ms == Some(2500)
  assert finished.last_turn_duration_ms == Some(1500)
  hub.stop(subject)
}

pub fn hub_exit_status_is_sticky_after_late_status_updates_test() {
  let assert Ok(subject) = hub.start(10, fn() { 2500 })
  hub.register_session(
    subject,
    event.SessionSummary(
      ..summary("session-late-status"),
      current_turn: 2,
      current_turn_status: Some(turn_telemetry.StatusRunning),
      current_turn_started_at_ms: Some(1000),
    ),
  )

  hub.finish_session(subject, "session-late-status", reason.OperatorAbort)
  hub.update_status(subject, "session-late-status", event.Running)
  hub.finish_session(subject, "session-late-status", reason.Failed)

  let assert Ok(Some(finished)) =
    hub.get_session(subject, "session-late-status", 1000)
  assert finished.status == event.Exited(reason.OperatorAbort)
  assert finished.current_turn_status == Some(turn_telemetry.StatusStopped)
  assert finished.last_turn_reason == Some(turn_telemetry.ReasonOperatorAbort)
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
    event.lifecycle_payload(
      event.RecoveryInterrupted,
      Some("daemon_restart"),
      Some(recovery),
    ),
  )

  let assert Ok(Some(updated)) = hub.get_session(subject, "session-1", 1000)
  let assert Some(summary_recovery) = updated.recovery
  assert summary_recovery.status == event.Interrupted
  assert summary_recovery.workflow_run_id == Some("run-1")
  assert updated.last_event_at_ms == 789

  let assert Ok(page) = hub.events_after(subject, "session-1", 0, 10, 1000)
  let assert [stored_event] = page.events
  let assert Some(event_recovery) = event.payload_recovery(stored_event.payload)
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

pub fn hub_prunes_exited_session_events_to_smaller_tail_test() {
  let assert Ok(subject) = hub.start_with_limits(100, 50, fn() { 1 })
  hub.register_session(subject, summary("session-1"))
  publish_numbered_events(subject, "session-1", 1, 30)

  hub.finish_session(subject, "session-1", reason.Normal)

  let assert Ok(page) = hub.events_after(subject, "session-1", 0, 50, 1000)
  assert list.length(page.events) == hub.default_max_exited_events_per_session
  let assert Ok(first_event) = list.first(page.events)
  let assert Ok(last_event) = list.last(page.events)
  assert first_event.cursor
    == 30 - hub.default_max_exited_events_per_session + 1
  assert last_event.cursor == 30
  assert page.truncated == True

  let assert Ok(recent_page) =
    hub.events_after(subject, "session-1", first_event.cursor, 50, 1000)
  assert recent_page.truncated == False
  hub.stop(subject)
}

pub fn hub_compacts_exited_session_heavy_event_fields_test() {
  let assert Ok(subject) = hub.start_with_limits(100, 50, fn() { 1 })
  let long_text = string.repeat("x", times: 10_000)
  hub.register_session(subject, summary("session-1"))
  hub.publish(
    subject,
    "session-1",
    event.pi_event_payload(
      pi_event.ToolExecutionUpdate,
      None,
      Some(long_text),
      None,
      None,
      None,
      Some(long_text),
      Some(long_text),
      None,
      session_tokens.zero_token_totals(),
      None,
    ),
  )

  hub.finish_session(subject, "session-1", reason.Normal)

  let assert Ok(page) = hub.events_after(subject, "session-1", 0, 10, 1000)
  let assert [stored_event] = page.events
  let expected_compacted_text =
    string.slice(long_text, 0, 4096) <> "… [truncated after session exit]"
  assert event.payload_message(stored_event.payload)
    == Some(expected_compacted_text)
  assert event.payload_tool_input(stored_event.payload)
    == Some(expected_compacted_text)
  assert event.payload_tool_output(stored_event.payload)
    == Some(expected_compacted_text)
  assert event.payload_raw_json(stored_event.payload) == None
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
    turn_payload(turn_telemetry.EventFinished, 1)
      |> event.with_payload_tokens(token_totals(10, 5, 0, 0, 15)),
  )

  let assert Ok(Some(summary)) = hub.get_session(subject, "session-1", 1000)
  assert summary.current_turn_status == Some(turn_telemetry.StatusFinished)
  assert summary.last_turn_duration_ms == Some(1500)
  assert summary.token_totals.total == 15
  assert summary.last_turn_token_delta.total == 15
  let assert Ok(page) = hub.events_after(subject, "session-1", 0, 10, 1000)
  let assert [finished] = page.events
  assert event.payload_turn_duration_ms(finished.payload) == Some(1500)
  assert event.payload_token_delta(finished.payload).total == 15
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

pub fn turn_payload_cannot_carry_message_tool_or_raw_json_test() {
  let assert Ok(subject) = hub.start(10, fn() { 1000 })
  hub.register_session(subject, summary("session-1"))
  hub.publish(
    subject,
    "session-1",
    turn_payload(turn_telemetry.EventStarted, 1),
  )

  let assert Ok(page) = hub.events_after(subject, "session-1", 0, 10, 1000)
  let assert [stored_event] = page.events
  assert event.payload_message(stored_event.payload) == None
  assert event.payload_tool_input(stored_event.payload) == None
  assert event.payload_raw_json(stored_event.payload) == None
  let encoded = session_json.event_to_string(stored_event)
  assert string.contains(encoded, "\"message\":null")
  assert string.contains(encoded, "\"tool_input\":null")
  assert string.contains(encoded, "\"raw_json\":null")
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
    event.turn_payload(
      name,
      1,
      session_tokens.zero_token_totals(),
      Some(reason),
    ),
  )
  let assert Ok(Some(summary)) =
    hub.get_session(subject, "session-terminal", 1000)
  assert summary.current_turn_status == Some(status)
  assert summary.last_turn_reason == Some(reason)
  let assert Ok(page) =
    hub.events_after(subject, "session-terminal", 0, 10, 1000)
  let assert [terminal] = page.events
  assert event.payload_reason(terminal.payload) == Some(reason)
  assert event.payload_raw_json(terminal.payload) == None
  hub.stop(subject)
}

fn turn_payload(
  name: turn_telemetry.TurnEventName,
  turn: Int,
) -> event.EventPayload {
  event.turn_payload(name, turn, session_tokens.zero_token_totals(), None)
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

fn publish_numbered_events(
  subject: process.Subject(hub.Message),
  session_id: String,
  next: Int,
  remaining: Int,
) -> Nil {
  case remaining <= 0 {
    True -> Nil
    False -> {
      hub.publish(subject, session_id, payload("event-" <> int.to_string(next)))
      publish_numbered_events(subject, session_id, next + 1, remaining - 1)
    }
  }
}
