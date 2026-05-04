import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/pi_event
import scherzo/session/event
import scherzo/session/tokens as session_tokens
import scherzo/terminal/sanitize
import scherzo/terminal/style
import scherzo/turn_telemetry

const default_max_body_lines = 40

const default_max_body_line_chars = 200

const display_truncation_note = "… [display truncated; use --json for retained raw event]"

const structured_tool_input_placeholder = "[structured tool input; use --json for raw details]"

pub type RenderChunk {
  Line(String)
  Inline(String)
}

pub type ToolSection {
  ToolInput
  ToolOutput
}

pub type RenderState {
  RenderState(
    last_cursor: Int,
    current_pass: Option(Int),
    displayed_pass: Option(Int),
    pi_cycle: Int,
    assistant_active: Bool,
    assistant_line_open: Bool,
    active_tool_label: Option(String),
    active_tool_section: Option(ToolSection),
    structured_tool_input_labels: List(String),
  )
}

pub type RenderOptions {
  RenderOptions(
    color_mode: style.ColorMode,
    show_lifecycle: Bool,
    show_raw_unknown: Bool,
    show_pi_cycles: Bool,
  )
}

pub fn initial_state(since_cursor: Int) -> RenderState {
  RenderState(
    last_cursor: since_cursor,
    current_pass: None,
    displayed_pass: None,
    pi_cycle: 0,
    assistant_active: False,
    assistant_line_open: False,
    active_tool_label: None,
    active_tool_section: None,
    structured_tool_input_labels: [],
  )
}

pub fn default_options(color_mode: style.ColorMode) -> RenderOptions {
  RenderOptions(
    color_mode: color_mode,
    show_lifecycle: False,
    show_raw_unknown: False,
    show_pi_cycles: False,
  )
}

pub fn verbose_options(color_mode: style.ColorMode) -> RenderOptions {
  RenderOptions(
    color_mode: color_mode,
    show_lifecycle: True,
    show_raw_unknown: True,
    show_pi_cycles: True,
  )
}

pub fn chunks_to_string(chunks: List(RenderChunk)) -> String {
  chunks
  |> list.fold("", fn(acc, chunk) {
    case chunk {
      Line(text) -> acc <> text <> "\n"
      Inline(text) -> acc <> text
    }
  })
}

pub fn render_header(
  summary: event.SessionSummary,
  options: RenderOptions,
) -> List(RenderChunk) {
  list.flatten([
    [
      Line(style.heading(
        options.color_mode,
        sanitize.text(summary.issue_identifier <> " " <> summary.issue_title),
      )),
      Line(
        style.meta_label(options.color_mode, "workspace:")
        <> " "
        <> sanitize.text(summary.workspace_path),
      ),
      Line(
        style.meta_label(options.color_mode, "session:")
        <> " "
        <> sanitize.text(summary.display_name),
      ),
    ],
    render_status_lines(summary, options),
  ])
}

fn render_status_lines(
  summary: event.SessionSummary,
  options: RenderOptions,
) -> List(RenderChunk) {
  [
    Line(
      style.meta_label(options.color_mode, "status:")
      <> " "
      <> event.status_to_string(summary.status),
    ),
    Line(
      style.meta_label(options.color_mode, "turn:")
      <> " "
      <> summary_turn_line(summary),
    ),
    Line(""),
  ]
}

fn summary_turn_line(summary: event.SessionSummary) -> String {
  let base = "turn " <> int.to_string(summary.current_turn)
  let with_status = case summary.current_turn_status {
    Some(status) -> base <> " " <> turn_telemetry.status_to_string(status)
    None -> base
  }
  let with_duration = case summary.last_turn_duration_ms {
    Some(duration) -> with_status <> " " <> format_duration(duration)
    None -> with_status
  }
  case summary.last_turn_token_delta.total > 0 {
    True ->
      with_duration
      <> " +"
      <> int.to_string(summary.last_turn_token_delta.total)
      <> " tok"
    False -> with_duration
  }
}

pub fn render_truncation_warning(options: RenderOptions) -> List(RenderChunk) {
  [
    Line(style.warning(
      options.color_mode,
      "warning: older retained events were dropped before this replay window",
    )),
    Line(""),
  ]
}

pub fn render_event(
  state: RenderState,
  stored_event: event.SessionEvent,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  case stored_event.cursor <= state.last_cursor {
    True -> #(state, [])
    False -> {
      let base_state = RenderState(..state, last_cursor: stored_event.cursor)
      let #(next_state, chunks) =
        render_fresh_event(base_state, stored_event, options)
      #(RenderState(..next_state, last_cursor: stored_event.cursor), chunks)
    }
  }
}

pub fn render_events(
  state: RenderState,
  events: List(event.SessionEvent),
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  list.fold(events, #(state, []), fn(acc, stored_event) {
    let #(state, chunks) = acc
    let #(state, event_chunks) = render_event(state, stored_event, options)
    #(state, list.append(chunks, event_chunks))
  })
}

pub fn render_page(
  summary: event.SessionSummary,
  page: event.EventPage,
  options: RenderOptions,
) -> List(RenderChunk) {
  let warning = case page.truncated {
    True -> render_truncation_warning(options)
    False -> []
  }
  let #(_, chunks) = render_events(initial_state(0), page.events, options)
  list.flatten([render_header(summary, options), warning, chunks])
}

fn render_fresh_event(
  state: RenderState,
  stored_event: event.SessionEvent,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let payload = stored_event.payload
  case payload.name {
    event.PiName(pi_event.TurnStart) ->
      render_pi_cycle_start(state, payload, options)
    event.PiName(pi_event.TurnEnd) ->
      render_pi_cycle_end(state, payload, options)
    _ ->
      case payload.kind {
        event.AssistantMessage -> render_assistant(state, payload, options)
        event.Tool -> render_tool(state, payload, options)
        event.UiRequest -> render_ui_request(state, payload, options)
        event.UiResponse -> render_ui_response(state, payload, options)
        event.TokenStats -> render_tokens(state, payload, options)
        event.Turn -> render_turn(state, payload, options)
        event.PiRaw -> render_pi_raw(state, payload, options)
        event.Error -> render_error_event(state, payload, options)
        event.Lifecycle | event.Pi ->
          render_pi_or_lifecycle(state, payload, options)
      }
  }
}

fn render_pi_cycle_start(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let observed =
    observe_pass(RenderState(..state, pi_cycle: state.pi_cycle + 1), payload)
  case options.show_pi_cycles {
    False -> #(observed, [])
    True -> {
      let #(observed, close_chunks) =
        close_assistant(observed, options.color_mode)
      let #(observed, tool_close_chunks) =
        close_tool_with_gap(observed, options.color_mode)
      let #(observed, heading_chunks) =
        ensure_pass_heading(observed, payload, options)
      #(
        observed,
        list.flatten([
          close_chunks,
          tool_close_chunks,
          heading_chunks,
          [
            Line(style.dim(
              options.color_mode,
              "pi cycle " <> int.to_string(observed.pi_cycle) <> " started",
            )),
            Line(""),
          ],
        ]),
      )
    }
  }
}

fn render_pi_cycle_end(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let observed = observe_pass(state, payload)
  case options.show_pi_cycles, observed.pi_cycle > 0 {
    True, True -> {
      let #(observed, close_chunks) =
        close_assistant(observed, options.color_mode)
      let #(observed, tool_close_chunks) =
        close_tool_with_gap(observed, options.color_mode)
      let #(observed, heading_chunks) =
        ensure_pass_heading(observed, payload, options)
      #(
        observed,
        list.flatten([
          close_chunks,
          tool_close_chunks,
          heading_chunks,
          [
            Line(style.dim(
              options.color_mode,
              "pi cycle " <> int.to_string(observed.pi_cycle) <> " ended",
            )),
            Line(""),
          ],
        ]),
      )
    }
    _, _ -> #(observed, [])
  }
}

fn render_assistant(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let delta = option_string(payload.message, "")
  case delta == "" {
    True -> #(observe_pass(state, payload), [])
    False -> {
      let #(state, close_chunks) = case assistant_must_close(state, payload) {
        True -> close_assistant(state, options.color_mode)
        False -> #(state, [])
      }
      let #(state, tool_close_chunks) =
        close_tool_with_gap(state, options.color_mode)
      let #(state, heading_chunks) =
        ensure_pass_heading(state, payload, options)
      let label_chunks = case state.assistant_active {
        True -> []
        False -> [Line(style.thinking_label(options.color_mode, "thinking"))]
      }
      let state =
        RenderState(
          ..state,
          assistant_active: True,
          active_tool_label: None,
          active_tool_section: None,
        )
      let #(state, body_chunks) = assistant_delta_chunks(state, delta)
      #(
        state,
        list.flatten([
          close_chunks,
          tool_close_chunks,
          heading_chunks,
          label_chunks,
          body_chunks,
        ]),
      )
    }
  }
}

fn render_tool(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let label = tool_label(payload)
  let state = observe_pass(state, payload)
  let suppress_structured_input =
    should_suppress_structured_tool_input(state, label, payload)
  let payload = case suppress_structured_input {
    True -> event.EventPayload(..payload, tool_input: None)
    False -> payload
  }
  let detailless_structured_tool_seen =
    !tool_has_visible_details(payload)
    && list.contains(state.structured_tool_input_labels, label)
  case detailless_structured_tool_seen {
    True -> {
      let state = case tool_closes(payload) {
        True -> close_tool(state)
        False -> state
      }
      #(state, [])
    }
    False -> {
      let #(state, close_chunks) = close_assistant(state, options.color_mode)
      let #(state, heading_chunks) =
        ensure_pass_heading(state, payload, options)
      let needs_label = tool_needs_label(state, label, payload)
      let state = case needs_label {
        True -> RenderState(..state, active_tool_section: None)
        False -> state
      }
      let label_chunks = case needs_label {
        True ->
          list.append(tool_top_padding_chunks(options.color_mode), [
            Line(style.tool_label(options.color_mode, label)),
          ])
        False -> []
      }
      let #(active_section, detail_chunks) =
        tool_detail_chunks(
          state.active_tool_section,
          payload,
          options.color_mode,
        )
      let state =
        mark_structured_tool_input_displayed(
          state,
          label,
          payload,
          suppress_structured_input,
        )
      let should_close = tool_closes(payload)
      let next_label = case should_close {
        True -> None
        False -> Some(label)
      }
      let next_section = case should_close {
        True -> None
        False -> active_section
      }
      let section_chunks =
        list.flatten([close_chunks, heading_chunks, label_chunks, detail_chunks])
      let section_chunks = case should_close, section_chunks {
        True, [_, ..] ->
          list.append(section_chunks, [
            Line(style.tool_gap_line(options.color_mode)),
          ])
        _, _ -> section_chunks
      }
      #(
        RenderState(
          ..state,
          assistant_active: False,
          assistant_line_open: False,
          active_tool_label: next_label,
          active_tool_section: next_section,
        ),
        section_chunks,
      )
    }
  }
}

fn render_ui_request(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let #(state, close_chunks) = close_assistant(state, options.color_mode)
  let #(state, tool_close_chunks) =
    close_tool_with_gap(state, options.color_mode)
  let #(state, heading_chunks) = ensure_pass_heading(state, payload, options)
  let method = safe_option_string(payload.method, "unknown")
  let request = safe_option_string(payload.request_id, "")
  let suffix = case request == "" {
    True -> ""
    False -> " #" <> request
  }
  let message_chunks = case payload.message {
    Some(message) -> plain_block_chunks("  ", message)
    None -> []
  }
  #(
    RenderState(..state, assistant_active: False, assistant_line_open: False),
    list.flatten([
      close_chunks,
      tool_close_chunks,
      heading_chunks,
      [
        Line(style.warning(
          options.color_mode,
          "UI request waiting: " <> method <> suffix,
        )),
      ],
      message_chunks,
      [Line("")],
    ]),
  )
}

fn render_ui_response(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let #(state, close_chunks) = close_assistant(state, options.color_mode)
  let #(state, tool_close_chunks) =
    close_tool_with_gap(state, options.color_mode)
  let #(state, heading_chunks) = ensure_pass_heading(state, payload, options)
  let method =
    safe_option_string(payload.method, event.name_to_string(payload.name))
  #(
    RenderState(..state, assistant_active: False, assistant_line_open: False),
    list.flatten([
      close_chunks,
      tool_close_chunks,
      heading_chunks,
      [Line(style.dim(options.color_mode, "UI response: " <> method)), Line("")],
    ]),
  )
}

fn render_turn(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let #(state, close_chunks) = close_assistant(state, options.color_mode)
  let #(state, tool_close_chunks) =
    close_tool_with_gap(state, options.color_mode)
  let #(state, heading_chunks) = ensure_pass_heading(state, payload, options)
  #(
    RenderState(..state, assistant_active: False, assistant_line_open: False),
    list.flatten([
      close_chunks,
      tool_close_chunks,
      heading_chunks,
      [Line(style.dim(options.color_mode, turn_event_line(payload))), Line("")],
    ]),
  )
}

fn turn_event_line(payload: event.EventPayload) -> String {
  let turn = case payload.turn {
    Some(turn) -> int.to_string(turn)
    None -> "?"
  }
  let status = case payload.turn_status {
    Some(status) -> turn_telemetry.status_to_string(status)
    None -> event.name_to_string(payload.name)
  }
  "turn "
  <> turn
  <> " "
  <> status
  <> turn_duration_suffix(payload.turn_duration_ms)
  <> turn_token_delta_suffix(payload.token_delta.total)
  <> turn_reason_suffix(payload.reason)
}

fn turn_duration_suffix(duration_ms: Option(Int)) -> String {
  case duration_ms {
    Some(duration_ms) -> " " <> format_duration(duration_ms)
    None -> ""
  }
}

fn turn_token_delta_suffix(total: Int) -> String {
  case total > 0 {
    True -> " +" <> int.to_string(total) <> " tok"
    False -> ""
  }
}

fn turn_reason_suffix(reason: Option(turn_telemetry.TurnReason)) -> String {
  case reason {
    Some(reason) -> " reason=" <> turn_telemetry.reason_to_string(reason)
    None -> ""
  }
}

fn format_duration(duration_ms: Int) -> String {
  case duration_ms < 1000 {
    True -> int.to_string(duration_ms) <> "ms"
    False -> {
      let tenths = duration_ms / 100
      let whole = tenths / 10
      let decimal = tenths - whole * 10
      int.to_string(whole) <> "." <> int.to_string(decimal) <> "s"
    }
  }
}

fn render_tokens(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let #(state, close_chunks) = close_assistant(state, options.color_mode)
  let #(state, tool_close_chunks) =
    close_tool_with_gap(state, options.color_mode)
  case tokens_are_nonzero(payload.tokens) {
    False -> #(
      observe_pass(state, payload),
      list.append(close_chunks, tool_close_chunks),
    )
    True -> {
      let #(state, heading_chunks) =
        ensure_pass_heading(state, payload, options)
      let pass = visible_pass(state, payload)
      #(
        RenderState(
          ..state,
          assistant_active: False,
          assistant_line_open: False,
        ),
        list.flatten([
          close_chunks,
          tool_close_chunks,
          heading_chunks,
          [
            Line(style.dim(options.color_mode, token_line(payload.tokens, pass))),
            Line(""),
          ],
        ]),
      )
    }
  }
}

fn render_pi_raw(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  case options.show_raw_unknown {
    True -> render_unknown(state, payload, options)
    False -> #(observe_pass(state, payload), [])
  }
}

fn render_unknown(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let #(state, close_chunks) = close_assistant(state, options.color_mode)
  let #(state, tool_close_chunks) =
    close_tool_with_gap(state, options.color_mode)
  let #(state, heading_chunks) = ensure_pass_heading(state, payload, options)
  let name =
    safe_option_string(payload.pi_type, event.name_to_string(payload.name))
  let raw_chunks = case options.show_raw_unknown, payload.raw_json {
    True, Some(raw) -> [
      Line(style.raw_label(
        options.color_mode,
        "  raw: " <> sanitize.text(compact_raw(raw.value)),
      )),
    ]
    _, _ -> []
  }
  #(
    RenderState(..state, assistant_active: False, assistant_line_open: False),
    list.flatten([
      close_chunks,
      tool_close_chunks,
      heading_chunks,
      [Line(style.raw_label(options.color_mode, "event " <> name))],
      raw_chunks,
      [Line("")],
    ]),
  )
}

fn render_error_event(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let #(state, close_chunks) = close_assistant(state, options.color_mode)
  let #(state, tool_close_chunks) =
    close_tool_with_gap(state, options.color_mode)
  let #(state, heading_chunks) = ensure_pass_heading(state, payload, options)
  let message =
    safe_option_string(payload.message, event.name_to_string(payload.name))
  let #(_, detail_chunks) =
    tool_detail_chunks(None, payload, options.color_mode)
  #(
    RenderState(..state, assistant_active: False, assistant_line_open: False),
    list.flatten([
      close_chunks,
      tool_close_chunks,
      heading_chunks,
      [Line(style.error(options.color_mode, "error: " <> message))],
      detail_chunks,
      [Line("")],
    ]),
  )
}

fn render_pi_or_lifecycle(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  case payload.kind, payload.message, options.show_lifecycle {
    event.Pi, Some(_), _ ->
      render_assistant(
        state,
        event.EventPayload(..payload, kind: event.AssistantMessage),
        options,
      )
    _, _, True -> render_unknown(state, payload, options)
    _, _, False -> #(observe_pass(state, payload), [])
  }
}

fn ensure_pass_heading(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  case visible_pass(state, payload) {
    Some(pass) -> {
      let state = observe_visible_pass(state, pass)
      case state.displayed_pass == Some(pass) {
        True -> #(state, [])
        False -> #(RenderState(..state, displayed_pass: Some(pass)), [
          Line(style.heading(
            options.color_mode,
            "Scherzo pass " <> int.to_string(pass),
          )),
          Line(""),
        ])
      }
    }
    None -> #(state, [])
  }
}

fn observe_pass(
  state: RenderState,
  payload: event.EventPayload,
) -> RenderState {
  case payload.turn {
    Some(pass) -> observe_visible_pass(state, pass)
    None -> state
  }
}

fn observe_visible_pass(state: RenderState, pass: Int) -> RenderState {
  case state.current_pass == Some(pass) {
    True -> RenderState(..state, current_pass: Some(pass))
    False ->
      RenderState(
        ..state,
        current_pass: Some(pass),
        active_tool_label: None,
        active_tool_section: None,
        structured_tool_input_labels: [],
      )
  }
}

fn visible_pass(
  state: RenderState,
  payload: event.EventPayload,
) -> Option(Int) {
  case payload.turn {
    Some(pass) -> Some(pass)
    None -> state.current_pass
  }
}

fn assistant_must_close(
  state: RenderState,
  payload: event.EventPayload,
) -> Bool {
  case state.assistant_active, visible_pass(state, payload) {
    True, Some(pass) -> state.displayed_pass != Some(pass)
    _, _ -> False
  }
}

fn close_assistant(
  state: RenderState,
  _color_mode: style.ColorMode,
) -> #(RenderState, List(RenderChunk)) {
  case state.assistant_active, state.assistant_line_open {
    True, True -> #(
      RenderState(..state, assistant_active: False, assistant_line_open: False),
      [Line(""), Line("")],
    )
    True, False -> #(
      RenderState(..state, assistant_active: False, assistant_line_open: False),
      [Line("")],
    )
    False, _ -> #(RenderState(..state, assistant_line_open: False), [])
  }
}

fn close_tool(state: RenderState) -> RenderState {
  RenderState(..state, active_tool_label: None, active_tool_section: None)
}

fn close_tool_with_gap(
  state: RenderState,
  color_mode: style.ColorMode,
) -> #(RenderState, List(RenderChunk)) {
  case state.active_tool_label {
    Some(_) -> #(close_tool(state), [Line(style.tool_gap_line(color_mode))])
    None -> #(close_tool(state), [])
  }
}

fn tool_top_padding_chunks(color_mode: style.ColorMode) -> List(RenderChunk) {
  case style.color_enabled(color_mode) {
    True -> [Line(style.tool_gap_line(color_mode))]
    False -> []
  }
}

fn assistant_delta_chunks(
  state: RenderState,
  delta: String,
) -> #(RenderState, List(RenderChunk)) {
  let lines = sanitize.block_lines(delta)
  let ends_with_newline = block_ends_with_newline(delta)
  let #(chunks, line_open) =
    assistant_lines(lines, state.assistant_line_open, ends_with_newline)
  #(RenderState(..state, assistant_line_open: line_open), chunks)
}

fn assistant_lines(
  lines: List(String),
  line_open: Bool,
  ends_with_newline: Bool,
) -> #(List(RenderChunk), Bool) {
  case lines {
    [] -> #([], line_open)
    [line] -> {
      case ends_with_newline && line == "" {
        True -> #([], False)
        False -> {
          let prefix = case line_open {
            True -> ""
            False -> "  "
          }
          #([Inline(prefix <> line)], True)
        }
      }
    }
    [line, ..rest] -> {
      let prefix = case line_open {
        True -> ""
        False -> "  "
      }
      let #(rest_chunks, rest_open) =
        assistant_lines(rest, False, ends_with_newline)
      #(list.append([Inline(prefix <> line), Line("")], rest_chunks), rest_open)
    }
  }
}

fn block_ends_with_newline(value: String) -> Bool {
  value
  |> string.replace(each: "\r\n", with: "\n")
  |> string.ends_with("\n")
}

fn tool_needs_label(
  state: RenderState,
  label: String,
  payload: event.EventPayload,
) -> Bool {
  case state.active_tool_label == Some(label) {
    False -> True
    True ->
      case
        event.name_to_string(payload.name),
        has_text(payload.tool_input),
        state.active_tool_section
      {
        "tool_execution_start", _, _ -> True
        _, True, Some(ToolInput) -> False
        _, True, _ -> True
        _, False, _ -> False
      }
  }
}

fn should_suppress_structured_tool_input(
  state: RenderState,
  label: String,
  payload: event.EventPayload,
) -> Bool {
  case payload.tool_input {
    Some(value) ->
      value == structured_tool_input_placeholder
      && list.contains(state.structured_tool_input_labels, label)
    None -> False
  }
}

fn mark_structured_tool_input_displayed(
  state: RenderState,
  label: String,
  payload: event.EventPayload,
  suppressed: Bool,
) -> RenderState {
  case payload.tool_input, suppressed {
    Some(value), False ->
      case
        value == structured_tool_input_placeholder,
        list.contains(state.structured_tool_input_labels, label)
      {
        True, False ->
          RenderState(..state, structured_tool_input_labels: [
            label,
            ..state.structured_tool_input_labels
          ])
        _, _ -> state
      }
    _, _ -> state
  }
}

fn tool_has_visible_details(payload: event.EventPayload) -> Bool {
  has_text(payload.tool_input)
  || has_text(payload.tool_output)
  || has_text(payload.tool_status)
}

fn tool_detail_chunks(
  active_section: Option(ToolSection),
  payload: event.EventPayload,
  color_mode: style.ColorMode,
) -> #(Option(ToolSection), List(RenderChunk)) {
  let #(active_section, input_chunks) =
    tool_section_chunks(
      active_section,
      ToolInput,
      "input",
      payload.tool_input,
      color_mode,
    )
  let #(active_section, output_chunks) =
    tool_section_chunks(
      active_section,
      ToolOutput,
      "output",
      payload.tool_output,
      color_mode,
    )
  let status_chunks = case payload.tool_status {
    Some(status) ->
      case status == "" {
        True -> []
        False -> [Line(tool_status_line(color_mode, sanitize.text(status)))]
      }
    None -> []
  }
  let active_section = case status_chunks {
    [] -> active_section
    _ -> None
  }
  #(active_section, list.flatten([input_chunks, output_chunks, status_chunks]))
}

fn tool_section_chunks(
  active_section: Option(ToolSection),
  section: ToolSection,
  heading: String,
  value: Option(String),
  color_mode: style.ColorMode,
) -> #(Option(ToolSection), List(RenderChunk)) {
  case value {
    Some(value) ->
      case value == "" {
        True -> #(active_section, [])
        False -> {
          let heading_chunks = case active_section == Some(section) {
            True -> []
            False -> [
              Line(tool_section_heading(color_mode, section, "  " <> heading)),
            ]
          }
          let body_chunks = display_block_chunks("    ", value, color_mode)
          #(Some(section), list.append(heading_chunks, body_chunks))
        }
      }
    None -> #(active_section, [])
  }
}

fn tool_section_heading(
  color_mode: style.ColorMode,
  section: ToolSection,
  text: String,
) -> String {
  case section {
    ToolInput -> style.input_label(color_mode, text)
    ToolOutput -> style.output_label(color_mode, text)
  }
}

fn tool_status_line(color_mode: style.ColorMode, status: String) -> String {
  let line = "  status: " <> status
  case string.lowercase(status) {
    "success" | "succeeded" | "ok" ->
      style.success_status_label(color_mode, line)
    "failed" | "failure" | "error" -> style.error_status_label(color_mode, line)
    _ -> style.status_label(color_mode, line)
  }
}

fn display_block_chunks(
  indent: String,
  value: String,
  color_mode: style.ColorMode,
) -> List(RenderChunk) {
  value
  |> display_lines
  |> list.map(fn(line) {
    Line(style.tool_body_line(color_mode, indent <> line))
  })
}

fn plain_block_chunks(indent: String, value: String) -> List(RenderChunk) {
  case value == "" {
    True -> []
    False ->
      value
      |> body_block_lines
      |> list.map(fn(line) { Line(indent <> line) })
  }
}

fn display_lines(value: String) -> List(String) {
  let #(lines, truncated) =
    sanitize.bounded_body_lines(
      value,
      default_max_body_lines,
      default_max_body_line_chars,
      display_truncation_note,
    )
  case truncated {
    True -> list.append(lines, [display_truncation_note])
    False -> lines
  }
}

fn body_block_lines(value: String) -> List(String) {
  let lines = sanitize.block_lines(value)
  case block_ends_with_newline(value), list.reverse(lines) {
    True, ["", ..rest] -> list.reverse(rest)
    _, _ -> lines
  }
}

fn tool_closes(payload: event.EventPayload) -> Bool {
  case
    payload.tool_status,
    string.ends_with(event.name_to_string(payload.name), "_end")
  {
    Some(_), _ -> True
    None, True -> True
    None, False -> False
  }
}

fn tool_label(payload: event.EventPayload) -> String {
  case payload.tool_name {
    Some(name) -> "tool " <> sanitize.text(name)
    None -> "tool " <> sanitize.text(event.name_to_string(payload.name))
  }
}

fn has_text(value: Option(String)) -> Bool {
  case value {
    Some(value) -> value != ""
    None -> False
  }
}

fn safe_option_string(value: Option(String), default: String) -> String {
  sanitize.text(option_string(value, default))
}

fn option_string(value: Option(String), default: String) -> String {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn token_line(tokens: session_tokens.TokenTotals, pass: Option(Int)) -> String {
  let prefix = case pass {
    Some(pass) -> "Scherzo pass " <> int.to_string(pass) <> " tokens"
    None -> "tokens"
  }
  prefix
  <> ": input="
  <> int.to_string(tokens.input)
  <> " output="
  <> int.to_string(tokens.output)
  <> " cache_read="
  <> int.to_string(tokens.cache_read)
  <> " cache_write="
  <> int.to_string(tokens.cache_write)
  <> " total="
  <> int.to_string(tokens.total)
}

fn tokens_are_nonzero(tokens: session_tokens.TokenTotals) -> Bool {
  tokens.input > 0
  || tokens.output > 0
  || tokens.cache_read > 0
  || tokens.cache_write > 0
  || tokens.total > 0
}

fn compact_raw(raw: String) -> String {
  raw
  |> string.replace(each: "\n", with: " ")
  |> string.slice(at_index: 0, length: 512)
}
