import gleam/option.{None, Some}
import gleam/string
import scherzo/domain
import scherzo/session/event
import scherzo/terminal/render
import scherzo/terminal/sanitize
import scherzo/terminal/style

fn summary() -> event.SessionSummary {
  event.SessionSummary(
    session_id: "session-1",
    issue_id: "issue-1",
    issue_identifier: "ABC-1",
    issue_title: "Render attach",
    workspace_path: "/tmp/workspace",
    pi_session_id: None,
    status: event.Running,
    current_turn: 1,
    started_at_ms: 10,
    last_event_at_ms: 20,
    token_totals: domain.zero_token_totals(),
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
  event.empty_payload(kind, name)
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
    )

  let transcript =
    render.render_page(
      unsafe_summary,
      event.EventPage(events: events, next_cursor: 6, truncated: False),
      opts,
    )
    |> render.chunks_to_string

  assert string.contains(transcript, "\u{1b}[1m")
  assert !string.contains(transcript, "\u{1b}[5n")
  assert !string.contains(transcript, "\u{1b}]0;")
  assert string.contains(transcript, "Render ␛[5n")
  assert string.contains(transcript, "/tmp/␛]0;x␇")
  assert string.contains(transcript, "hello ␛[5n␍")
  assert string.contains(transcript, "tool bash␛[5n")
  assert string.contains(transcript, "input: cmd\\u{9B}31m")
  assert string.contains(transcript, "UI request: confirm␛[5n #ui␇")
  assert string.contains(transcript, "approve␈")
  assert string.contains(transcript, "event mystery␛[5n")
  assert string.contains(transcript, "raw: {\"x\":\"␛[5n\"}")
  assert string.contains(transcript, "error: bad␛[5n")
}

pub fn render_groups_turns_assistant_tools_ui_and_tokens_test() {
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
    evt(
      4,
      payload(event.Tool, "tool_execution_start")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_input("gleam test"),
    ),
    evt(
      5,
      payload(event.Tool, "tool_execution_update")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_output("ok"),
    ),
    evt(
      6,
      payload(event.Tool, "tool_execution_end")
        |> with_turn(1)
        |> with_tool_name("bash")
        |> with_tool_status("success"),
    ),
    evt(
      7,
      payload(event.UiRequest, "extension_ui_request")
        |> with_turn(1)
        |> with_method("confirm")
        |> with_request_id("ui-1")
        |> with_message("approve?"),
    ),
    evt(
      8,
      event.EventPayload(
        ..payload(event.TokenStats, "turn_finished"),
        tokens: domain.TokenTotals(
          input: 10,
          output: 20,
          cache_read: 3,
          cache_write: 4,
          total: 37,
        ),
      ),
    ),
    evt(9, payload(event.Lifecycle, "turn_end") |> with_turn(1)),
  ]

  let #(_, chunks) =
    render.render_events(render.initial_state(0), events, options())

  assert render.chunks_to_string(chunks)
    == "▶ turn 1 started\nassistant:\n  Hello world\ntool bash\n  input: gleam test\n  output: ok\n  status: success\nUI request: confirm #ui-1\n  approve?\ntokens: input=10 output=20 cache_read=3 cache_write=4 total=37\n✓ turn 1 ended\n"
}

pub fn render_continued_turn_and_suppresses_duplicate_cursor_test() {
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
    == "▶ turn 2 continued\nassistant:\n  new\ntool read\n"
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
    )

  let #(_, chunks) =
    render.render_event(render.initial_state(0), stored_event, opts)

  assert render.chunks_to_string(chunks)
    == "event mystery\n  raw: {\"type\":\"mystery\",\"detail\":\"one\\ntwo\"}\n"
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
