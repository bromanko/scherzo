import gleam/list
import gleam/option.{None, Some}
import scherzo/domain
import scherzo/session/event
import scherzo/session/hub
import scherzo/session/reason

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
    current_turn: 0,
    started_at_ms: 10,
    last_event_at_ms: 10,
    token_totals: domain.zero_token_totals(),
  )
}

fn payload(name: String) -> event.EventPayload {
  let assert Ok(event_name) = event.name_from_string(name)
  event.empty_payload(event.Lifecycle, event_name)
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

fn event_cursors(events: List(event.SessionEvent)) -> List(Int) {
  list.map(events, fn(stored_event) { stored_event.cursor })
}

fn event_timestamps(events: List(event.SessionEvent)) -> List(Int) {
  list.map(events, fn(stored_event) { stored_event.at_ms })
}
