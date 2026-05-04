import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/control/command
import scherzo/control/file
import scherzo/control/protocol
import scherzo/ctl
import scherzo/session/event
import scherzo/session/tokens as session_tokens
import scherzo/terminal/style

type OutMsg {
  OutLine(String)
  OutInline(String)
}

fn control_file() -> file.ControlFile {
  file.ControlFile(
    host: "127.0.0.1",
    port: 1,
    token: "token",
    workspace_root: "test/tmp/ctl-attach-render/workspaces",
    started_at_ms: 1,
  )
}

fn write_control_file(path: String) -> Nil {
  let assert Ok(Nil) = file.write(path, control_file())
  Nil
}

fn summary() -> event.SessionSummary {
  event.SessionSummary(
    session_id: "session-1",
    display_name: "session-1",
    issue_id: "issue-1",
    issue_identifier: "ABC-1",
    issue_title: "Attach renderer",
    workspace_path: "/tmp/workspace",
    pi_session_id: None,
    status: event.Running,
    recovery: None,
    current_turn: 1,
    current_turn_status: None,
    current_turn_started_at_ms: None,
    last_turn_finished_at_ms: None,
    last_turn_duration_ms: None,
    last_turn_token_delta: session_tokens.zero_token_totals(),
    last_turn_reason: None,
    started_at_ms: 1,
    last_event_at_ms: 10,
    token_totals: session_tokens.zero_token_totals(),
  )
}

fn evt(cursor: Int, payload: event.EventPayload) -> event.SessionEvent {
  event.SessionEvent(
    cursor: cursor,
    at_ms: 100 + cursor,
    session_id: "session-1",
    issue_id: "issue-1",
    payload: payload,
  )
}

fn payload(kind: event.EventKind, name: String) -> event.EventPayload {
  let assert Ok(event_name) = event.name_from_string(name)
  event.empty_payload(kind, event_name)
}

fn assistant(cursor: Int, text: String) -> event.SessionEvent {
  evt(
    cursor,
    event.EventPayload(
      ..payload(event.AssistantMessage, "message_update"),
      turn: Some(1),
      message: Some(text),
    ),
  )
}

fn tool(cursor: Int, output: String) -> event.SessionEvent {
  evt(
    cursor,
    event.EventPayload(
      ..payload(event.Tool, "tool_execution_update"),
      turn: Some(1),
      tool_name: Some("bash"),
      tool_output: Some(output),
    ),
  )
}

fn output(subject: process.Subject(OutMsg)) -> ctl.Output {
  ctl.Output(
    line: fn(text) {
      process.send(subject, OutLine(text))
      Nil
    },
    inline: fn(text) {
      process.send(subject, OutInline(text))
      Nil
    },
  )
}

fn drain_output(subject: process.Subject(OutMsg)) -> String {
  drain_output_loop(subject, "")
}

fn drain_output_loop(subject: process.Subject(OutMsg), acc: String) -> String {
  case process.receive(subject, within: 10) {
    Ok(OutLine(text)) -> drain_output_loop(subject, acc <> text <> "\n")
    Ok(OutInline(text)) -> drain_output_loop(subject, acc <> text)
    Error(Nil) -> acc
  }
}

fn deps(
  replay_events: List(event.SessionEvent),
  stream_events: List(event.SessionEvent),
) -> ctl.ControlClient {
  ctl.ControlClient(
    list_sessions: fn(_) {
      Ok(event.SessionList(sessions: [summary()], now_ms: 0))
    },
    get_session: fn(_, _) { Ok(Some(summary())) },
    get_events: fn(_, _, cursor, _) {
      Ok(event.EventPage(
        events: list.filter(replay_events, fn(stored_event) {
          stored_event.cursor > cursor
        }),
        next_cursor: last_cursor_after(replay_events, cursor),
        truncated: False,
      ))
    },
    stream_events: fn(_, _, _, callback) {
      list.each(stream_events, fn(stored_event) {
        let _ = callback(stored_event)
        Nil
      })
      Ok(Nil)
    },
    apply_command: fn(_, operator_command) {
      Ok(command.applied(operator_command, None))
    },
    raw_request: fn(_, request) { Ok(protocol.request_to_string(request)) },
  )
}

fn last_cursor_after(events: List(event.SessionEvent), cursor: Int) -> Int {
  events
  |> list.fold(cursor, fn(acc, stored_event) {
    case stored_event.cursor > acc {
      True -> stored_event.cursor
      False -> acc
    }
  })
}

pub fn attach_pretty_replays_and_follows_without_duplicate_cursors_test() {
  let path = "test/tmp/ctl-attach-render/pretty-control.json"
  write_control_file(path)
  let subject = process.new_subject()
  let replay = [
    evt(1, payload(event.Lifecycle, "turn_start") |> with_turn(1)),
    assistant(2, "Hello "),
  ]
  let live = [
    assistant(2, "DUP"),
    assistant(3, "world"),
    assistant(3, "DUP2"),
    tool(4, "ok"),
  ]

  let result =
    ctl.run_with_deps(
      ctl.Attach(
        Some(path),
        ctl.Pretty,
        style.ColorNever,
        ctl.Follow,
        0,
        False,
        "session-1",
      ),
      deps(replay, live),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "ABC-1 Attach renderer\n")
  assert string.contains(
    transcript,
    "Scherzo pass 1\n\nthinking\n  Hello world\n\ntool bash\n  output\n    ok\n",
  )
  assert !string.contains(transcript, "turn 1 started")
  assert !string.contains(transcript, "turn 1 ended")
  assert !string.contains(transcript, "pi cycle")
  assert !string.contains(transcript, "DUP")
  assert !string.contains(transcript, "DUP2")
}

pub fn attach_verbose_shows_pi_cycles_when_requested_test() {
  let path = "test/tmp/ctl-attach-render/verbose-control.json"
  write_control_file(path)
  let subject = process.new_subject()
  let replay = [
    evt(1, payload(event.Lifecycle, "turn_start") |> with_turn(1)),
    evt(2, payload(event.Lifecycle, "turn_end") |> with_turn(1)),
    evt(3, payload(event.Lifecycle, "turn_start") |> with_turn(1)),
    assistant(4, "hi"),
    evt(5, payload(event.Lifecycle, "turn_end") |> with_turn(1)),
  ]

  let result =
    ctl.run_with_deps(
      ctl.Attach(
        Some(path),
        ctl.Pretty,
        style.ColorNever,
        ctl.NoFollow,
        0,
        True,
        "session-1",
      ),
      deps(replay, []),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "Scherzo pass 1\n\npi cycle 1 started")
  assert string.contains(transcript, "pi cycle 1 ended\n\n")
  assert string.contains(transcript, "pi cycle 2 started\n\n")
  assert string.contains(transcript, "thinking\n  hi\n\npi cycle 2 ended")
  assert !string.contains(transcript, "turn 1 started")
}

pub fn attach_raw_and_json_follow_skip_replayed_duplicates_test() {
  let path = "test/tmp/ctl-attach-render/raw-json-control.json"
  write_control_file(path)
  let replay = [assistant(1, "Hello")]
  let live = [assistant(1, "DUP"), assistant(2, "world"), assistant(2, "DUP2")]

  let raw_subject = process.new_subject()
  let raw_result =
    ctl.run_with_deps(
      ctl.Attach(
        Some(path),
        ctl.Raw,
        style.ColorNever,
        ctl.Follow,
        0,
        False,
        "session-1",
      ),
      deps(replay, live),
      output(raw_subject),
    )
  assert raw_result == Ok(Nil)
  let raw = drain_output(raw_subject)
  assert string.contains(raw, "1 101 session-1")
  assert string.contains(raw, "2 102 session-1")
  assert string.contains(raw, "world")
  assert !string.contains(raw, "DUP")
  assert !string.contains(raw, "DUP2")

  let json_subject = process.new_subject()
  let json_result =
    ctl.run_with_deps(
      ctl.Attach(
        Some(path),
        ctl.Json,
        style.ColorNever,
        ctl.Follow,
        0,
        False,
        "session-1",
      ),
      deps(replay, live),
      output(json_subject),
    )
  assert json_result == Ok(Nil)
  let json = drain_output(json_subject)
  assert string.contains(json, "\"stream\":true")
  assert string.contains(json, "\"cursor\":1")
  assert string.contains(json, "\"cursor\":2")
  assert !string.contains(json, "DUP")
  assert !string.contains(json, "DUP2")
}

pub fn events_pretty_uses_paginated_replay_helper_test() {
  let deps =
    ctl.ControlClient(
      list_sessions: fn(_) {
        Ok(event.SessionList(sessions: [summary()], now_ms: 0))
      },
      get_session: fn(_, _) { Ok(Some(summary())) },
      get_events: fn(_, _, cursor, _) {
        case cursor {
          0 ->
            Ok(event.EventPage(
              events: [assistant(1, "one")],
              next_cursor: 1,
              truncated: True,
            ))
          1 ->
            Ok(event.EventPage(
              events: [assistant(2, "two")],
              next_cursor: 2,
              truncated: False,
            ))
          _ ->
            Ok(event.EventPage(
              events: [],
              next_cursor: cursor,
              truncated: False,
            ))
        }
      },
      stream_events: fn(_, _, _, _) { Ok(Nil) },
      apply_command: fn(_, operator_command) {
        Ok(command.applied(operator_command, None))
      },
      raw_request: fn(_, request) { Ok(protocol.request_to_string(request)) },
    )

  let assert Ok(replay) =
    ctl.fetch_replay_pages(deps, control_file(), "session-1", 0, 1)

  assert list.map(replay.events, fn(stored_event) { stored_event.cursor })
    == [1, 2]
  assert replay.last_cursor == 2
  assert replay.truncated == True
}

fn with_turn(payload: event.EventPayload, turn: Int) -> event.EventPayload {
  event.EventPayload(..payload, turn: Some(turn))
}
