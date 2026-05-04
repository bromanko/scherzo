import gleam/option.{None, Some}
import gleam/string
import scherzo/session/event
import scherzo/session/tokens as session_tokens
import scherzo/terminal/render
import scherzo/terminal/sanitize
import scherzo/terminal/style
import scherzo/turn_telemetry

fn summary() -> event.SessionSummary {
  event.SessionSummary(
    session_id: "session-1",
    display_name: "session-1",
    issue_id: "issue-1",
    issue_identifier: "ABC-1",
    issue_title: "Render attach",
    workspace_path: "/tmp/workspace",
    pi_session_id: None,
    status: event.Running,
    current_turn: 1,
    current_turn_status: None,
    current_turn_started_at_ms: None,
    last_turn_finished_at_ms: None,
    last_turn_duration_ms: None,
    last_turn_token_delta: session_tokens.zero_token_totals(),
    last_turn_reason: None,
    started_at_ms: 10,
    last_event_at_ms: 20,
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

fn options() -> render.RenderOptions {
  render.default_options(style.ColorNever)
}

pub fn render_header_and_truncation_warning_test() {
  let page = event.EventPage(events: [], next_cursor: 0, truncated: True)

  let transcript =
    render.render_page(summary(), page, options()) |> render.chunks_to_string

  assert string.contains(transcript, "ABC-1 Render attach\n")
  assert string.contains(transcript, "workspace: /tmp/workspace\n")
  assert string.contains(transcript, "session: session-1\n")
  assert string.contains(transcript, "status: running\n")
  assert string.contains(
    transcript,
    "warning: older retained events were dropped",
  )
}

pub fn sanitize_escapes_terminal_controls_test() {
  assert sanitize.text("a\u{1b}[31m\n\t\u{7}\u{7f}\u{9b}31m")
    == "a␛[31m␊␉␇␡\\u{9B}31m"
}

pub fn sanitize_block_lines_preserves_newline_layout_and_escapes_controls_test() {
  assert sanitize.block_lines("a\nb") == ["a", "b"]
  assert sanitize.block_lines("a\r\nb") == ["a", "b"]
  assert sanitize.block_lines("a\nb\n") == ["a", "b", ""]
  let escaped = sanitize.block_lines("safe \u{1b}[31m\r\u{9b}0m")
  assert escaped == ["safe ␛[31m␍\\u{9B}0m"]
  assert !string.contains(string.concat(escaped), "\u{1b}[31m")
}

pub fn render_color_always_styles_section_labels_test() {
  let events = [
    evt(1, payload(event.Lifecycle, "turn_start") |> with_turn(1)),
    evt(
      2,
      payload(event.AssistantMessage, "message_update")
        |> with_turn(1)
        |> with_message("checking"),
    ),
    evt(
      3,
      payload(event.Tool, "tool_execution_start")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_input("gleam test"),
    ),
    evt(
      4,
      payload(event.Tool, "tool_execution_update")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_output("ok"),
    ),
    evt(
      5,
      payload(event.Tool, "tool_execution_end")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_status("success"),
    ),
  ]

  let #(_, chunks) =
    render.render_events(
      render.initial_state(0),
      events,
      render.default_options(style.ColorAlways),
    )
  let transcript = render.chunks_to_string(chunks)

  assert string.contains(transcript, "\u{1b}[1mScherzo pass 1")
  assert string.contains(
    transcript,
    "\u{1b}[3m\u{1b}[38;2;128;128;128mthinking",
  )
  assert string.contains(transcript, "\u{1b}[1m\u{1b}[48;2;40;40;50mtool bash")
  assert string.contains(transcript, "\u{1b}[48;2;40;40;50m  input\u{1b}[0m")
  assert string.contains(transcript, "\u{1b}[48;2;40;40;50m  output\u{1b}[0m")
  assert string.contains(
    transcript,
    "\u{1b}[48;2;40;40;50m  status: success\u{1b}[0m",
  )
}

pub fn render_sanitizes_untrusted_text_and_keeps_own_ansi_test() {
  let escape_probe = "\u{1b}[5n"
  let unsafe_summary =
    event.SessionSummary(
      ..summary(),
      issue_title: "Render " <> escape_probe,
      workspace_path: "/tmp/" <> "\u{1b}]0;x\u{7}",
    )
  let events = [
    evt(1, payload(event.Lifecycle, "turn_start") |> with_turn(1)),
    evt(
      2,
      payload(event.AssistantMessage, "message_update")
        |> with_turn(1)
        |> with_message("hello " <> escape_probe <> "\r"),
    ),
    evt(
      3,
      payload(event.Tool, "tool_execution_start")
        |> with_turn(1)
        |> with_tool_name("bash" <> escape_probe)
        |> with_tool_input("cmd" <> "\u{9b}" <> "31m"),
    ),
    evt(
      4,
      payload(event.UiRequest, "extension_ui_request")
        |> with_turn(1)
        |> with_method("confirm" <> escape_probe)
        |> with_request_id("ui" <> "\u{7}")
        |> with_message("approve" <> "\u{8}"),
    ),
    evt(
      5,
      event.EventPayload(
        ..payload(event.PiRaw, "unknown" <> escape_probe),
        pi_type: Some("mystery" <> escape_probe),
        raw_json: Some(event.RedactedRawJson(
          value: "{\"x\":\"" <> escape_probe <> "\"}",
          truncated: False,
        )),
      ),
    ),
    evt(
      6,
      payload(event.Error, "error_name")
        |> with_turn(1)
        |> with_message("bad" <> escape_probe),
    ),
  ]
  let opts =
    render.RenderOptions(
      color_mode: style.ColorAlways,
      show_lifecycle: False,
      show_raw_unknown: True,
      show_pi_cycles: False,
    )

  let transcript =
    render.render_page(
      unsafe_summary,
      event.EventPage(events: events, next_cursor: 6, truncated: False),
      opts,
    )
    |> render.chunks_to_string

  assert string.contains(transcript, "\u{1b}[1m")
  assert string.contains(
    transcript,
    "\u{1b}[3m\u{1b}[38;2;128;128;128mthinking",
  )
  assert !string.contains(transcript, "\u{1b}[5n")
  assert !string.contains(transcript, "\u{1b}]0;")
  assert string.contains(transcript, "Render ␛[5n")
  assert string.contains(transcript, "/tmp/␛]0;x␇")
  assert string.contains(transcript, "Scherzo pass 1")
  assert string.contains(transcript, "  hello ␛[5n␍")
  assert string.contains(
    transcript,
    "\u{1b}[1m\u{1b}[48;2;40;40;50mtool bash␛[5n",
  )
  assert string.contains(
    transcript,
    "\u{1b}[48;2;40;40;50m  input\u{1b}[0m\n\u{1b}[48;2;40;40;50m    cmd\\u{9B}31m",
  )
  assert string.contains(transcript, "UI request waiting: confirm␛[5n #ui␇")
  assert string.contains(transcript, "approve␈")
  assert string.contains(transcript, "event mystery␛[5n")
  assert string.contains(transcript, "raw: {\"x\":\"␛[5n\"}")
  assert string.contains(transcript, "error: bad␛[5n")
}

pub fn render_defaults_to_scherzo_pass_and_hides_pi_cycles_test() {
  let events = [
    evt(1, payload(event.Lifecycle, "turn_start") |> with_turn(1)),
    evt(2, payload(event.Lifecycle, "turn_end") |> with_turn(1)),
    evt(3, payload(event.Lifecycle, "turn_start") |> with_turn(1)),
    evt(
      4,
      payload(event.AssistantMessage, "message_update")
        |> with_turn(1)
        |> with_message("Hello "),
    ),
    evt(
      5,
      payload(event.AssistantMessage, "message_update")
        |> with_turn(1)
        |> with_message("world"),
    ),
    evt(
      6,
      payload(event.Tool, "tool_execution_start")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_input("gleam test"),
    ),
    evt(
      7,
      payload(event.Tool, "tool_execution_update")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_output("ok"),
    ),
    evt(
      8,
      payload(event.Tool, "tool_execution_end")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_status("success"),
    ),
    evt(
      9,
      payload(event.UiRequest, "extension_ui_request")
        |> with_turn(1)
        |> with_method("confirm")
        |> with_request_id("ui-1")
        |> with_message("approve?"),
    ),
    evt(
      10,
      event.EventPayload(
        ..payload(event.TokenStats, "turn_finished"),
        tokens: session_tokens.TokenTotals(
          input: 10,
          output: 20,
          cache_read: 3,
          cache_write: 4,
          total: 37,
        ),
      ),
    ),
    evt(11, payload(event.Lifecycle, "turn_end") |> with_turn(1)),
  ]

  let #(_, chunks) =
    render.render_events(render.initial_state(0), events, options())
  let transcript = render.chunks_to_string(chunks)

  assert transcript
    == "Scherzo pass 1\n\nthinking\n  Hello world\n\ntool bash\n  input\n    gleam test\n  output\n    ok\n  status: success\n\nUI request waiting: confirm #ui-1\n  approve?\n\nScherzo pass 1 tokens: input=10 output=20 cache_read=3 cache_write=4 total=37\n\n"
  assert !string.contains(transcript, "turn 1 started")
  assert !string.contains(transcript, "turn 1 ended")
  assert !string.contains(transcript, "pi cycle")
}

pub fn render_turn_events_are_visible_by_default_test() {
  let events = [
    evt(
      1,
      event.EventPayload(
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
    ),
  ]

  let #(_, chunks) =
    render.render_events(render.initial_state(0), events, options())
  let transcript = render.chunks_to_string(chunks)

  assert string.contains(transcript, "Scherzo pass 3")
  assert string.contains(transcript, "turn 3 finished 1.5s +15 tok")
}

pub fn render_verbose_shows_pi_cycle_labels_test() {
  let events = [
    evt(1, payload(event.Lifecycle, "turn_start") |> with_turn(1)),
    evt(2, payload(event.Lifecycle, "turn_end") |> with_turn(1)),
    evt(3, payload(event.Lifecycle, "turn_start") |> with_turn(1)),
    evt(
      4,
      payload(event.AssistantMessage, "message_update")
        |> with_turn(1)
        |> with_message("hi"),
    ),
    evt(5, payload(event.Lifecycle, "turn_end") |> with_turn(1)),
  ]

  let #(_, chunks) =
    render.render_events(
      render.initial_state(0),
      events,
      render.verbose_options(style.ColorNever),
    )

  assert render.chunks_to_string(chunks)
    == "Scherzo pass 1\n\npi cycle 1 started\n\npi cycle 1 ended\n\npi cycle 2 started\n\nthinking\n  hi\n\npi cycle 2 ended\n\n"
}

pub fn render_continued_pass_and_suppresses_duplicate_cursor_test() {
  let events = [
    evt(
      5,
      payload(event.AssistantMessage, "message_update")
        |> with_turn(2)
        |> with_message("old"),
    ),
    evt(
      6,
      payload(event.AssistantMessage, "message_update")
        |> with_turn(2)
        |> with_message("new"),
    ),
    evt(
      7,
      payload(event.Tool, "tool_execution_start")
        |> with_turn(2)
        |> with_tool_name("read"),
    ),
  ]

  let #(_, chunks) =
    render.render_events(render.initial_state(5), events, options())

  assert render.chunks_to_string(chunks)
    == "Scherzo pass 2\n\nthinking\n  new\n\ntool read\n"
}

pub fn render_assistant_multiline_body_test() {
  let events = [
    evt(1, payload(event.Lifecycle, "turn_start") |> with_turn(1)),
    evt(
      2,
      payload(event.AssistantMessage, "message_update")
        |> with_turn(1)
        |> with_message("first\nsecond"),
    ),
  ]

  let #(_, chunks) =
    render.render_events(render.initial_state(0), events, options())

  assert render.chunks_to_string(chunks)
    == "Scherzo pass 1\n\nthinking\n  first\n  second"
}

pub fn render_assistant_newline_split_across_deltas_test() {
  let events = [
    evt(1, payload(event.Lifecycle, "turn_start") |> with_turn(1)),
    evt(
      2,
      payload(event.AssistantMessage, "message_update")
        |> with_turn(1)
        |> with_message("first\n"),
    ),
    evt(
      3,
      payload(event.AssistantMessage, "message_update")
        |> with_turn(1)
        |> with_message("second"),
    ),
  ]

  let #(_, chunks) =
    render.render_events(render.initial_state(0), events, options())

  assert render.chunks_to_string(chunks)
    == "Scherzo pass 1\n\nthinking\n  first\n  second"
}

pub fn render_assistant_adjacent_deltas_stay_on_one_line_test() {
  let events = [
    evt(1, payload(event.Lifecycle, "turn_start") |> with_turn(1)),
    evt(
      2,
      payload(event.AssistantMessage, "message_update")
        |> with_turn(1)
        |> with_message("Hello "),
    ),
    evt(
      3,
      payload(event.AssistantMessage, "message_update")
        |> with_turn(1)
        |> with_message("world"),
    ),
  ]

  let #(_, chunks) =
    render.render_events(render.initial_state(0), events, options())

  assert render.chunks_to_string(chunks)
    == "Scherzo pass 1\n\nthinking\n  Hello world"
}

pub fn render_tool_multiline_sections_and_repeated_output_updates_test() {
  let events = [
    evt(1, payload(event.Lifecycle, "turn_start") |> with_turn(1)),
    evt(
      2,
      payload(event.Tool, "tool_execution_start")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_input("gleam test\n--target erlang"),
    ),
    evt(
      3,
      payload(event.Tool, "tool_execution_update")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_output("line one\n"),
    ),
    evt(
      4,
      payload(event.Tool, "tool_execution_update")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_output("line two"),
    ),
    evt(
      5,
      payload(event.Tool, "tool_execution_end")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_status("success"),
    ),
  ]

  let #(_, chunks) =
    render.render_events(render.initial_state(0), events, options())

  assert render.chunks_to_string(chunks)
    == "Scherzo pass 1\n\ntool bash\n  input\n    gleam test\n    --target erlang\n  output\n    line one\n    line two\n  status: success\n\n"
}

pub fn render_tool_repeated_plain_input_updates_are_not_dropped_test() {
  let events = [
    evt(1, payload(event.Lifecycle, "turn_start") |> with_turn(1)),
    evt(
      2,
      payload(event.Tool, "tool_execution_update")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_input("echo"),
    ),
    evt(
      3,
      payload(event.Tool, "tool_execution_update")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_input("echo hello"),
    ),
  ]

  let #(_, chunks) =
    render.render_events(render.initial_state(0), events, options())

  assert render.chunks_to_string(chunks)
    == "Scherzo pass 1\n\ntool bash\n  input\n    echo\n    echo hello\n"
}

pub fn render_tool_repeated_input_updates_are_collapsed_test() {
  let events = [
    evt(1, payload(event.Lifecycle, "turn_start") |> with_turn(1)),
    evt(
      2,
      payload(event.Tool, "tool_execution_update")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_input(
          "[structured tool input; use --json for raw details]",
        ),
    ),
    evt(
      3,
      payload(event.Tool, "tool_execution_start")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_input(
          "[structured tool input; use --json for raw details]",
        ),
    ),
    evt(
      4,
      payload(event.Tool, "tool_execution_start")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_input(
          "[structured tool input; use --json for raw details]",
        ),
    ),
    evt(
      5,
      payload(event.Tool, "tool_execution_end")
        |> with_turn(1)
        |> with_tool_name("bash"),
    ),
    evt(
      6,
      payload(event.Tool, "tool_execution_update")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_input(
          "[structured tool input; use --json for raw details]",
        ),
    ),
    evt(
      7,
      payload(event.Tool, "tool_execution_start")
        |> with_turn(1)
        |> with_tool_name("bash"),
    ),
    evt(
      8,
      payload(event.Tool, "tool_execution_update")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_output("ok"),
    ),
  ]

  let #(_, chunks) =
    render.render_events(render.initial_state(0), events, options())

  assert render.chunks_to_string(chunks)
    == "Scherzo pass 1\n\ntool bash\n  input\n    [structured tool input; use --json for raw details]\ntool bash\n  output\n    ok\n"
}

pub fn render_tool_output_display_truncation_test() {
  let long_output = string.repeat("x\n", times: 45)
  let events = [
    evt(1, payload(event.Lifecycle, "turn_start") |> with_turn(1)),
    evt(
      2,
      payload(event.Tool, "tool_execution_update")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_output(long_output),
    ),
  ]

  let #(_, chunks) =
    render.render_events(render.initial_state(0), events, options())
  let transcript = render.chunks_to_string(chunks)

  assert string.contains(transcript, "display truncated; use --json")
}

pub fn render_tool_label_resets_across_hidden_pass_boundary_test() {
  let events = [
    evt(1, payload(event.Lifecycle, "turn_start") |> with_turn(1)),
    evt(
      2,
      payload(event.Tool, "tool_execution_update")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_output("first"),
    ),
    evt(3, payload(event.Lifecycle, "turn_start") |> with_turn(2)),
    evt(
      4,
      payload(event.Tool, "tool_execution_update")
        |> with_turn(2)
        |> with_tool_name("bash")
        |> with_tool_output("second"),
    ),
  ]

  let #(_, chunks) =
    render.render_events(render.initial_state(0), events, options())

  assert render.chunks_to_string(chunks)
    == "Scherzo pass 1\n\ntool bash\n  output\n    first\nScherzo pass 2\n\ntool bash\n  output\n    second\n"
}

pub fn render_ui_request_body_and_pass_token_summary_test() {
  let events = [
    evt(1, payload(event.Lifecycle, "turn_start") |> with_turn(1)),
    evt(
      2,
      payload(event.UiRequest, "extension_ui_request")
        |> with_turn(1)
        |> with_method("confirm")
        |> with_request_id("ui-1")
        |> with_message("line one\nline two"),
    ),
    evt(
      3,
      event.EventPayload(
        ..payload(event.TokenStats, "turn_finished")
        |> with_turn(1),
        tokens: session_tokens.TokenTotals(
          input: 1,
          output: 2,
          cache_read: 0,
          cache_write: 0,
          total: 3,
        ),
      ),
    ),
  ]

  let #(_, chunks) =
    render.render_events(render.initial_state(0), events, options())

  assert render.chunks_to_string(chunks)
    == "Scherzo pass 1\n\nUI request waiting: confirm #ui-1\n  line one\n  line two\n\nScherzo pass 1 tokens: input=1 output=2 cache_read=0 cache_write=0 total=3\n\n"
}

pub fn render_unknown_event_fallback_can_show_raw_excerpt_test() {
  let raw = "{\"type\":\"mystery\",\"detail\":\"one\\ntwo\"}"
  let stored_event =
    evt(
      1,
      event.EventPayload(
        ..payload(event.PiRaw, "unknown"),
        pi_type: Some("mystery"),
        raw_json: Some(event.RedactedRawJson(value: raw, truncated: False)),
      ),
    )
  let opts =
    render.RenderOptions(
      color_mode: style.ColorNever,
      show_lifecycle: False,
      show_raw_unknown: True,
      show_pi_cycles: False,
    )

  let #(_, chunks) =
    render.render_event(render.initial_state(0), stored_event, opts)

  assert render.chunks_to_string(chunks)
    == "event mystery\n  raw: {\"type\":\"mystery\",\"detail\":\"one\\ntwo\"}\n\n"
}

fn with_turn(payload: event.EventPayload, turn: Int) -> event.EventPayload {
  event.EventPayload(..payload, turn: Some(turn))
}

fn with_message(
  payload: event.EventPayload,
  message: String,
) -> event.EventPayload {
  event.EventPayload(..payload, message: Some(message))
}

fn with_tool_name(
  payload: event.EventPayload,
  name: String,
) -> event.EventPayload {
  event.EventPayload(..payload, tool_name: Some(name))
}

fn with_tool_input(
  payload: event.EventPayload,
  input: String,
) -> event.EventPayload {
  event.EventPayload(..payload, tool_input: Some(input))
}

fn with_tool_output(
  payload: event.EventPayload,
  output: String,
) -> event.EventPayload {
  event.EventPayload(..payload, tool_output: Some(output))
}

fn with_tool_status(
  payload: event.EventPayload,
  status: String,
) -> event.EventPayload {
  event.EventPayload(..payload, tool_status: Some(status))
}

fn with_method(
  payload: event.EventPayload,
  method: String,
) -> event.EventPayload {
  event.EventPayload(..payload, method: Some(method))
}

fn with_request_id(
  payload: event.EventPayload,
  request_id: String,
) -> event.EventPayload {
  event.EventPayload(..payload, request_id: Some(request_id))
}
