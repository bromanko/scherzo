import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/control/command
import scherzo/control/file
import scherzo/control/protocol
import scherzo/ctl
import scherzo/session/event
import scherzo/session/reason
import scherzo/session/tokens as session_tokens
import scherzo/terminal/style
import scherzo/turn_telemetry

const ps_now_ms = -576_460_678_330

type OutMsg {
  OutLine(String)
  OutInline(String)
}

fn control_file() -> file.ControlFile {
  file.ControlFile(
    host: "127.0.0.1",
    port: 1,
    token: "token",
    workspace_root: "test/tmp/ctl-ps/workspaces",
    started_at_ms: 1,
  )
}

fn write_control_file(path: String) -> Nil {
  let assert Ok(Nil) = file.write(path, control_file())
  Nil
}

fn session_summary(
  session_id: String,
  last_event_at_ms: Int,
) -> event.SessionSummary {
  session_summary_with_status(session_id, last_event_at_ms, event.Running)
}

fn session_summary_with_status(
  session_id: String,
  last_event_at_ms: Int,
  status: event.SessionStatus,
) -> event.SessionSummary {
  event.SessionSummary(
    session_id: session_id,
    display_name: session_id,
    issue_id: "issue-1",
    issue_identifier: "LIV-41",
    issue_title: "Improve ctl ps output readability",
    workspace_path: "/tmp/workspace",
    pi_session_id: None,
    status: status,
    current_turn: 1,
    current_turn_status: None,
    current_turn_started_at_ms: None,
    last_turn_finished_at_ms: None,
    last_turn_duration_ms: None,
    last_turn_token_delta: session_tokens.zero_token_totals(),
    last_turn_reason: None,
    started_at_ms: last_event_at_ms - 1000,
    last_event_at_ms: last_event_at_ms,
    token_totals: session_tokens.zero_token_totals(),
  )
}

fn ps_deps(
  sessions: List(event.SessionSummary),
  now_ms: Int,
  raw_response: String,
) -> ctl.ControlClient {
  ctl.ControlClient(
    list_sessions: fn(_) {
      Ok(event.SessionList(sessions: sessions, now_ms: now_ms))
    },
    get_session: fn(_, _) { Ok(None) },
    get_events: fn(_, _, _, _) {
      Ok(event.EventPage(events: [], next_cursor: 0, truncated: False))
    },
    stream_events: fn(_, _, _, _) { Ok(Nil) },
    apply_command: fn(_, operator_command) {
      Ok(command.applied(operator_command, None))
    },
    raw_request: fn(_, request) {
      case raw_response == "" {
        True -> Ok(protocol.request_to_string(request))
        False -> Ok(raw_response)
      }
    },
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

fn output_lines(transcript: String) -> List(String) {
  string.trim(transcript) |> string.split(on: "\n")
}

fn table_columns(row: String) -> List(String) {
  row
  |> string.split(on: " ")
  |> list.filter(fn(value) { value != "" })
}

pub fn parse_ping_ps_session_events_and_attach_test() {
  assert ctl.parse(["ping"]) == Ok(ctl.Ping(None, False))
  assert ctl.parse(["ps", "--json"]) == Ok(ctl.Ps(None, True))
  assert ctl.parse(["session", "ABC-1", "--control-file", "state/control.json"])
    == Ok(ctl.Session(Some("state/control.json"), False, "ABC-1"))
  assert ctl.parse(["events", "ABC-1"])
    == Ok(ctl.Events(None, ctl.Raw, style.ColorNever, 0, False, "ABC-1"))
  assert ctl.parse(["events", "ABC-1", "--json"])
    == Ok(ctl.Events(None, ctl.Json, style.ColorNever, 0, False, "ABC-1"))
  assert ctl.parse(["events", "--pretty", "ABC-1"])
    == Ok(ctl.Events(None, ctl.Pretty, style.ColorAuto, 0, False, "ABC-1"))
  assert ctl.parse(["events", "--pretty", "--verbose", "ABC-1"])
    == Ok(ctl.Events(None, ctl.Pretty, style.ColorAuto, 0, True, "ABC-1"))
  assert ctl.parse(["attach", "ABC-1"])
    == Ok(ctl.Attach(
      None,
      ctl.Pretty,
      style.ColorAuto,
      ctl.Follow,
      0,
      False,
      "ABC-1",
    ))
  assert ctl.parse(["attach", "--verbose", "ABC-1"])
    == Ok(ctl.Attach(
      None,
      ctl.Pretty,
      style.ColorAuto,
      ctl.Follow,
      0,
      True,
      "ABC-1",
    ))
  assert ctl.parse(["attach", "--raw", "ABC-1"])
    == Ok(ctl.Attach(
      None,
      ctl.Raw,
      style.ColorNever,
      ctl.Follow,
      0,
      False,
      "ABC-1",
    ))
  assert ctl.parse(["attach", "--json", "--verbose", "ABC-1"])
    == Ok(ctl.Attach(
      None,
      ctl.Json,
      style.ColorNever,
      ctl.Follow,
      0,
      True,
      "ABC-1",
    ))
  assert ctl.parse(["attach", "--raw", "ABC-1", "--json"])
    == Ok(ctl.Attach(
      None,
      ctl.Json,
      style.ColorNever,
      ctl.Follow,
      0,
      False,
      "ABC-1",
    ))
  assert ctl.parse(["attach", "--no-follow", "ABC-1"])
    == Ok(ctl.Attach(
      None,
      ctl.Pretty,
      style.ColorAuto,
      ctl.NoFollow,
      0,
      False,
      "ABC-1",
    ))
  assert ctl.parse(["attach", "--since-cursor", "40", "ABC-1"])
    == Ok(ctl.Attach(
      None,
      ctl.Pretty,
      style.ColorAuto,
      ctl.Follow,
      40,
      False,
      "ABC-1",
    ))
  assert ctl.parse(["attach", "--color=never", "ABC-1"])
    == Ok(ctl.Attach(
      None,
      ctl.Pretty,
      style.ColorNever,
      ctl.Follow,
      0,
      False,
      "ABC-1",
    ))
}

pub fn parse_operator_commands_test() {
  assert ctl.parse(["pause"])
    == Ok(ctl.Operator(None, False, command.PauseDispatch))
  assert ctl.parse(["resume"])
    == Ok(ctl.Operator(None, False, command.ResumeDispatch))
  assert ctl.parse(["reload"])
    == Ok(ctl.Operator(None, False, command.ReloadWorkflow))
  assert ctl.parse(["retry", "ABC-123"])
    == Ok(ctl.Operator(
      None,
      False,
      command.RetryIssue(command.IssueIdentifier("ABC-123")),
    ))
  assert ctl.parse(["park", "ABC-123", "--reason", "manual", "--yes"])
    == Ok(ctl.Operator(
      None,
      False,
      command.ParkIssue(command.IssueIdentifier("ABC-123"), "manual"),
    ))
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["park", "ABC-123", "--reason", "manual"])
  assert ctl.parse(["unpark", "ABC-123"])
    == Ok(ctl.Operator(
      None,
      False,
      command.UnparkIssue(command.IssueIdentifier("ABC-123")),
    ))
  assert ctl.parse(["abort", "session-1", "--yes"])
    == Ok(ctl.Operator(None, False, command.AbortSession("session-1")))
  let assert Error(ctl.UsageError(_)) = ctl.parse(["abort", "session-1"])
  assert ctl.parse(["stop-after-turn", "session-1", "--yes"])
    == Ok(ctl.Operator(None, False, command.StopAfterCurrentTurn("session-1")))
  assert ctl.parse(["prompt", "session-1", "continue"])
    == Ok(ctl.Operator(
      None,
      False,
      command.PromptSession("session-1", "continue"),
    ))
  assert ctl.parse(["ui", "respond", "session-1", "ui-1", "--cancel"])
    == Ok(ctl.Operator(
      None,
      False,
      command.RespondUi("session-1", "ui-1", command.UiCancel),
    ))
  assert ctl.parse(["ui", "respond", "session-1", "ui-1", "--value", "ok"])
    == Ok(ctl.Operator(
      None,
      False,
      command.RespondUi("session-1", "ui-1", command.UiValue("ok")),
    ))
}

pub fn parse_rejects_usage_errors_test() {
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["attach", "--raw", "--pretty", "ABC-1"])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["attach", "--since-cursor", "-1", "ABC-1"])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["attach", "--since-cursor", "wat", "ABC-1"])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["attach", "--color=bad", "ABC-1"])
  let assert Error(ctl.UsageError(_)) = ctl.parse(["ps", "--control-file"])
  let assert Error(ctl.UsageError(_)) = ctl.parse(["unknown"])
}

pub fn usage_mentions_commands_and_options_test() {
  let usage = ctl.usage()
  assert string.contains(usage, "ping")
  assert string.contains(usage, "ps")
  assert string.contains(usage, "LAST EVENT is daemon-relative age")
  assert string.contains(usage, "session <session-ref>")
  assert string.contains(usage, "events <session-ref>")
  assert string.contains(usage, "events --pretty <session-ref>")
  assert string.contains(usage, "events --pretty --verbose <session-ref>")
  assert string.contains(usage, "attach <session-ref>")
  assert string.contains(usage, "attach --verbose <session-ref>")
  assert string.contains(usage, "attach --raw <session-ref>")
  assert string.contains(usage, "attach --raw --json <session-ref>")
  assert string.contains(usage, "pause")
  assert string.contains(usage, "abort <session-ref> --yes")
  assert string.contains(usage, "ui respond")
  assert string.contains(usage, "--control-file <path>")
  assert string.contains(usage, "--json")
  assert string.contains(usage, "--verbose")
  assert string.contains(usage, "--since-cursor <n>")
}

pub fn ps_human_table_uses_display_name_and_matches_header_order_test() {
  let path = "test/tmp/ctl-ps/table-order-control.json"
  write_control_file(path)
  let canonical_session_id = "canonical-session-id-should-stay-hidden"
  let display_name = "LIV-43-fancy-narwhal-finger"
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Ps(Some(path), False),
      ps_deps(
        [
          event.SessionSummary(
            ..session_summary(canonical_session_id, ps_now_ms - 12_000),
            display_name: display_name,
            issue_identifier: "LIV-43",
            current_turn: 7,
          ),
        ],
        ps_now_ms,
        "",
      ),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  let assert [header, row] = output_lines(transcript)
  assert table_columns(header)
    == ["SESSION", "ISSUE", "TURN", "STATUS", "LAST", "EVENT"]
  assert string.contains(header, "LAST EVENT")
  assert !string.contains(transcript, "LAST_EVENT")
  assert string.contains(transcript, "LIV-43")
  assert string.contains(transcript, "…")
  assert !string.contains(transcript, canonical_session_id)

  let assert [
    session_col,
    issue_col,
    turn_label,
    turn_col,
    status_col,
    age_value,
    age_unit,
  ] = table_columns(row)
  assert string.contains(session_col, "…")
  assert issue_col == "LIV-43"
  assert turn_label == "turn"
  assert turn_col == "7"
  assert status_col == "running"
  assert age_value == "12s"
  assert age_unit == "ago"
}

pub fn ps_human_table_shortens_long_session_names_and_formats_last_event_age_test() {
  let path = "test/tmp/ctl-ps/table-control.json"
  write_control_file(path)
  let top_level_session_name = "LONGISSUE-12345--576460690849-123456789"
  let step_session_name =
    "LONGISSUE-12345--576460690849-123456789-validate_draft"
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Ps(Some(path), False),
      ps_deps(
        [
          session_summary(top_level_session_name, -576_460_690_330),
          session_summary(step_session_name, ps_now_ms - 180_000),
        ],
        ps_now_ms,
        "",
      ),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "SESSION")
  assert string.contains(transcript, "LAST EVENT")
  assert !string.contains(transcript, "LAST_EVENT")
  assert string.contains(transcript, "12s ago")
  assert string.contains(transcript, "3m ago")
  assert string.contains(transcript, "…")
  assert string.contains(transcript, "123456789")
  assert string.contains(transcript, "date_draft")
  assert !string.contains(transcript, top_level_session_name)
  assert !string.contains(transcript, step_session_name)
  assert !string.contains(transcript, "-576460690330")

  let rows = output_lines(transcript)
  assert list.all(rows, fn(row) { string.length(row) <= 80 })
}

pub fn ps_human_table_shows_exit_outcomes_test() {
  let path = "test/tmp/ctl-ps/exits-control.json"
  write_control_file(path)
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Ps(Some(path), False),
      ps_deps(
        [
          session_summary_with_status(
            "sid-1",
            ps_now_ms,
            event.Exited(reason.Normal),
          ),
          session_summary_with_status(
            "sid-2",
            ps_now_ms,
            event.Exited(reason.Failed),
          ),
          session_summary_with_status(
            "sid-3",
            ps_now_ms,
            event.Exited(reason.WorkerDown),
          ),
          session_summary_with_status(
            "sid-4",
            ps_now_ms,
            event.Exited(reason.OperatorAbort),
          ),
          session_summary_with_status(
            "sid-5",
            ps_now_ms,
            event.Exited(reason.OperatorStopAfterCurrentTurn),
          ),
          session_summary_with_status("sid-6", ps_now_ms, event.WaitingUi),
        ],
        ps_now_ms,
        "",
      ),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "success")
  assert string.contains(transcript, "failed")
  assert string.contains(transcript, "worker_down")
  assert string.contains(transcript, "operator_abort")
  assert string.contains(transcript, "op_stop_after")
  assert string.contains(transcript, "waiting_ui")
  assert !string.contains(transcript, "exited")

  let rows = string.trim(transcript) |> string.split(on: "\n")
  assert list.all(rows, fn(row) { string.length(row) <= 80 })
}

pub fn ps_human_table_ellipsizes_long_display_name_without_shifting_columns_test() {
  let path = "test/tmp/ctl-ps/table-long-name-control.json"
  write_control_file(path)
  let display_name =
    "LIV-44-this-is-a-very-long-session-display-name-that-keeps-going"
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Ps(Some(path), False),
      ps_deps(
        [
          event.SessionSummary(
            ..session_summary("canonical-session-id", ps_now_ms - 180_000),
            display_name: display_name,
            issue_identifier: "LIV-44",
            current_turn: 42,
          ),
        ],
        ps_now_ms,
        "",
      ),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  let assert [_, row] = output_lines(transcript)
  let assert [
    session_col,
    issue_col,
    turn_label,
    turn_col,
    status_col,
    age_value,
    age_unit,
  ] = table_columns(row)
  assert string.contains(session_col, "…")
  assert !string.contains(transcript, display_name)
  assert issue_col == "LIV-44"
  assert turn_label == "turn"
  assert turn_col == "42"
  assert status_col == "running"
  assert age_value == "3m"
  assert age_unit == "ago"
  assert string.length(row) <= 80
}

pub fn ctl_turn_telemetry_human_and_raw_outputs_test() {
  let path = "test/tmp/ctl-ps/turn-control.json"
  write_control_file(path)
  let summary =
    event.SessionSummary(
      ..session_summary("session-turn", ps_now_ms - 1000),
      current_turn: 3,
      current_turn_status: Some(turn_telemetry.StatusRunning),
      current_turn_started_at_ms: Some(ps_now_ms - 2000),
    )
  let deps = turn_deps(summary)

  let ps_subject = process.new_subject()
  assert ctl.run_with_deps(ctl.Ps(Some(path), False), deps, output(ps_subject))
    == Ok(Nil)
  let ps_transcript = drain_output(ps_subject)
  assert string.contains(ps_transcript, "turn 3 running")

  let session_subject = process.new_subject()
  assert ctl.run_with_deps(
      ctl.Session(Some(path), False, "session-turn"),
      deps,
      output(session_subject),
    )
    == Ok(Nil)
  let session_transcript = drain_output(session_subject)
  assert string.contains(session_transcript, "turn: turn 3 running")
  assert string.contains(session_transcript, "turn_started_at_ms:")

  let events_subject = process.new_subject()
  assert ctl.run_with_deps(
      ctl.Events(
        Some(path),
        ctl.Pretty,
        style.ColorNever,
        0,
        False,
        "session-turn",
      ),
      deps,
      output(events_subject),
    )
    == Ok(Nil)
  let events_transcript = drain_output(events_subject)
  assert string.contains(events_transcript, "turn 3 finished")
  assert string.contains(events_transcript, "+15 tok")

  let attach_subject = process.new_subject()
  assert ctl.run_with_deps(
      ctl.Attach(
        Some(path),
        ctl.Raw,
        style.ColorNever,
        ctl.NoFollow,
        0,
        False,
        "session-turn",
      ),
      deps,
      output(attach_subject),
    )
    == Ok(Nil)
  let attach_transcript = drain_output(attach_subject)
  assert string.contains(attach_transcript, "kind=turn")
  assert string.contains(attach_transcript, "name=turn_finished")
  assert string.contains(attach_transcript, "turn=3")
  assert string.contains(attach_transcript, "turn_status=finished")
}

pub fn ps_json_preserves_full_session_ids_and_raw_fields_test() {
  let path = "test/tmp/ctl-ps/json-control.json"
  write_control_file(path)
  let session_id = "LONGISSUE-12345--576460690849-123456789-validate_draft"
  let raw_response =
    "{\"session_id\":\""
    <> session_id
    <> "\",\"status\":\"exited\",\"exit_reason\":\"failed\",\"last_event_at_ms\":-576460690330}"
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Ps(Some(path), True),
      ps_deps([], ps_now_ms, raw_response),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, session_id)
  assert string.contains(transcript, "\"status\":\"exited\"")
  assert string.contains(transcript, "\"exit_reason\":\"failed\"")
  assert string.contains(transcript, "-576460690330")
  assert !string.contains(transcript, "…")
}

pub fn session_display_ref_resolves_to_canonical_and_prints_both_names_test() {
  let path = "test/tmp/ctl-ps/session-ref-control.json"
  write_control_file(path)
  let canonical_session_id = "LIV-43--576460751551-1"
  let display_name = "liv-43-fancy-narwhal-finger"
  let summary =
    event.SessionSummary(
      ..session_summary(canonical_session_id, ps_now_ms - 1000),
      display_name: display_name,
      issue_identifier: "LIV-43",
    )
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Session(Some(path), False, display_name),
      session_ref_deps([summary]),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "display_name: " <> display_name)
  assert string.contains(transcript, "session_id: " <> canonical_session_id)
}

pub fn events_json_ref_prefers_exact_session_id_over_display_name_test() {
  let path = "test/tmp/ctl-ps/exact-ref-control.json"
  write_control_file(path)
  let canonical_session_id = "canonical-session-id"
  let sessions = [
    event.SessionSummary(
      ..session_summary(canonical_session_id, ps_now_ms - 1000),
      display_name: "readable-name",
    ),
    event.SessionSummary(
      ..session_summary("other-session-id", ps_now_ms - 1000),
      display_name: canonical_session_id,
    ),
  ]
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Events(
        Some(path),
        ctl.Json,
        style.ColorNever,
        0,
        False,
        canonical_session_id,
      ),
      session_ref_deps(sessions),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(
    transcript,
    "\"session_id\":\"" <> canonical_session_id <> "\"",
  )
  assert !string.contains(transcript, "other-session-id")
}

pub fn attach_display_ref_replays_canonical_session_events_test() {
  let path = "test/tmp/ctl-ps/attach-ref-control.json"
  write_control_file(path)
  let canonical_session_id = "LIV-43--576460751551-1"
  let display_name = "liv-43-fancy-narwhal-finger"
  let summary =
    event.SessionSummary(
      ..session_summary(canonical_session_id, ps_now_ms - 1000),
      display_name: display_name,
    )
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Attach(
        Some(path),
        ctl.Raw,
        style.ColorNever,
        ctl.NoFollow,
        0,
        False,
        display_name,
      ),
      session_ref_deps([summary]),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "1 10 " <> canonical_session_id)
}

pub fn operator_command_by_display_ref_routes_to_canonical_session_test() {
  let path = "test/tmp/ctl-ps/operator-ref-control.json"
  write_control_file(path)
  let canonical_session_id = "LIV-43--576460751551-1"
  let display_name = "liv-43-fancy-narwhal-finger"
  let summary =
    event.SessionSummary(
      ..session_summary(canonical_session_id, ps_now_ms - 1000),
      display_name: display_name,
    )
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Operator(Some(path), False, command.AbortSession(display_name)),
      session_ref_deps([summary]),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(
    transcript,
    "abort applied target=" <> canonical_session_id,
  )
  assert !string.contains(transcript, "target=" <> display_name)
}

pub fn ambiguous_display_ref_returns_clear_error_test() {
  let path = "test/tmp/ctl-ps/ambiguous-ref-control.json"
  write_control_file(path)
  let display_name = "liv-43-fancy-narwhal-finger"
  let sessions = [
    event.SessionSummary(
      ..session_summary("session-a", ps_now_ms - 1000),
      display_name: display_name,
    ),
    event.SessionSummary(
      ..session_summary("session-b", ps_now_ms - 1000),
      display_name: display_name,
    ),
  ]
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Session(Some(path), False, display_name),
      session_ref_deps(sessions),
      output(subject),
    )

  let assert Error(err) = result
  assert ctl.error_code(err) == "ambiguous_session_ref"
  assert string.contains(ctl.error_message(err), "ambiguous")
  assert string.contains(ctl.error_message(err), "canonical session_id")
}

fn turn_deps(summary: event.SessionSummary) -> ctl.ControlClient {
  ctl.ControlClient(
    list_sessions: fn(_) {
      Ok(event.SessionList(sessions: [summary], now_ms: ps_now_ms))
    },
    get_session: fn(_, session_id) {
      case session_id == summary.session_id {
        True -> Ok(Some(summary))
        False -> Ok(None)
      }
    },
    get_events: fn(_, _, cursor, _) {
      case cursor {
        0 ->
          Ok(event.EventPage(
            events: [turn_finished_event(summary.session_id)],
            next_cursor: 1,
            truncated: False,
          ))
        _ ->
          Ok(event.EventPage(events: [], next_cursor: cursor, truncated: False))
      }
    },
    stream_events: fn(_, _, _, _) { Ok(Nil) },
    apply_command: fn(_, operator_command) {
      Ok(command.applied(operator_command, None))
    },
    raw_request: fn(_, request) { Ok(protocol.request_to_string(request)) },
  )
}

fn turn_finished_event(session_id: String) -> event.SessionEvent {
  event.SessionEvent(
    cursor: 1,
    at_ms: 10,
    session_id: session_id,
    issue_id: "issue-1",
    payload: event.EventPayload(
      ..event.empty_payload(
        event.Turn,
        event.TurnName(turn_telemetry.EventFinished),
      ),
      turn: Some(3),
      turn_status: Some(turn_telemetry.StatusFinished),
      turn_duration_ms: Some(1500),
      token_delta: session_tokens.TokenTotals(
        input: 10,
        output: 5,
        cache_read: 0,
        cache_write: 0,
        total: 15,
      ),
    ),
  )
}

fn session_ref_deps(sessions: List(event.SessionSummary)) -> ctl.ControlClient {
  ctl.ControlClient(
    list_sessions: fn(_) {
      Ok(event.SessionList(sessions: sessions, now_ms: ps_now_ms))
    },
    get_session: fn(_, session_id) {
      Ok(summary_by_session_id(sessions, session_id))
    },
    get_events: fn(_, session_id, cursor, _) {
      case summary_by_session_id(sessions, session_id), cursor {
        Some(_), 0 ->
          Ok(event.EventPage(
            events: [replay_event(session_id)],
            next_cursor: 1,
            truncated: False,
          ))
        _, _ ->
          Ok(event.EventPage(events: [], next_cursor: cursor, truncated: False))
      }
    },
    stream_events: fn(_, _, _, _) { Ok(Nil) },
    apply_command: fn(_, operator_command) {
      Ok(command.applied(operator_command, None))
    },
    raw_request: fn(_, request) { Ok(protocol.request_to_string(request)) },
  )
}

fn summary_by_session_id(
  sessions: List(event.SessionSummary),
  session_id: String,
) -> Option(event.SessionSummary) {
  case list.filter(sessions, fn(summary) { summary.session_id == session_id }) {
    [summary, ..] -> Some(summary)
    [] -> None
  }
}

fn replay_event(session_id: String) -> event.SessionEvent {
  event.SessionEvent(
    cursor: 1,
    at_ms: 10,
    session_id: session_id,
    issue_id: "issue-1",
    payload: event.empty_payload(
      event.Lifecycle,
      event.LifecycleName(event.WorkerStarted),
    ),
  )
}
