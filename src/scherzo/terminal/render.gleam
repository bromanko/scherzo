import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/domain
import scherzo/session/event
import scherzo/terminal/sanitize
import scherzo/terminal/style

pub type RenderChunk {
  Line(String)
  Inline(String)
}

pub type RenderState {
  RenderState(
    last_cursor: Int,
    current_turn: Option(Int),
    assistant_open: Bool,
    active_tool_label: Option(String),
  )
}

pub type RenderOptions {
  RenderOptions(
    color_mode: style.ColorMode,
    show_lifecycle: Bool,
    show_raw_unknown: Bool,
  )
}

pub fn initial_state(since_cursor: Int) -> RenderState {
  RenderState(
    last_cursor: since_cursor,
    current_turn: None,
    assistant_open: False,
    active_tool_label: None,
  )
}

pub fn default_options(color_mode: style.ColorMode) -> RenderOptions {
  RenderOptions(
    color_mode: color_mode,
    show_lifecycle: False,
    show_raw_unknown: False,
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
  [
    Line(style.heading(
      options.color_mode,
      sanitize.text(summary.issue_identifier <> " " <> summary.issue_title),
    )),
    Line("workspace: " <> sanitize.text(summary.workspace_path)),
    Line("session: " <> sanitize.text(summary.session_id)),
    Line("status: " <> event.status_to_string(summary.status)),
    Line(""),
  ]
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
    "turn_start" -> render_turn_start(state, payload, options)
    "turn_end" -> render_turn_end(state, payload, options)
    _ ->
      case payload.kind {
        event.AssistantMessage -> render_assistant(state, payload, options)
        event.Tool -> render_tool(state, payload, options)
        event.UiRequest -> render_ui_request(state, payload, options)
        event.UiResponse -> render_ui_response(state, payload, options)
        event.TokenStats -> render_tokens(state, payload, options)
        event.PiRaw -> render_unknown(state, payload, options)
        event.Error -> render_error_event(state, payload, options)
        event.Lifecycle | event.Pi ->
          render_pi_or_lifecycle(state, payload, options)
      }
  }
}

fn render_turn_start(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let #(state, close_chunks) = close_assistant(state)
  let turn = turn_label(payload.turn)
  let line = style.heading(options.color_mode, "▶ turn " <> turn <> " started")
  #(
    RenderState(
      ..state,
      current_turn: payload.turn,
      assistant_open: False,
      active_tool_label: None,
    ),
    list.append(close_chunks, [Line(line)]),
  )
}

fn render_turn_end(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let #(state, close_chunks) = close_assistant(state)
  let turn = case payload.turn {
    Some(turn) -> Some(turn)
    None -> state.current_turn
  }
  let line =
    style.dim(options.color_mode, "✓ turn " <> turn_label(turn) <> " ended")
  #(
    RenderState(
      ..state,
      current_turn: turn,
      assistant_open: False,
      active_tool_label: None,
    ),
    list.append(close_chunks, [Line(line)]),
  )
}

fn render_assistant(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let #(state, heading_chunks) = ensure_turn_heading(state, payload, options)
  let delta = case payload.message {
    Some(message) -> sanitize.text(message)
    None -> ""
  }
  case delta == "" {
    True -> #(state, heading_chunks)
    False -> {
      let chunks = case state.assistant_open {
        True -> [Inline(delta)]
        False -> [
          Line(style.assistant_label(options.color_mode, "assistant:")),
          Inline("  " <> delta),
        ]
      }
      #(
        RenderState(..state, assistant_open: True, active_tool_label: None),
        list.append(heading_chunks, chunks),
      )
    }
  }
}

fn render_tool(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let #(state, close_chunks) = close_assistant(state)
  let #(state, heading_chunks) = ensure_turn_heading(state, payload, options)
  let label = tool_label(payload)
  let needs_label = case
    state.active_tool_label == Some(label),
    payload.tool_input
  {
    True, None -> False
    _, _ -> True
  }
  let label_chunks = case needs_label {
    True -> [Line(style.tool_label(options.color_mode, label))]
    False -> []
  }
  let detail_chunks = tool_detail_chunks(payload)
  #(
    RenderState(
      ..state,
      assistant_open: False,
      active_tool_label: next_active_tool(label, payload),
    ),
    list.flatten([close_chunks, heading_chunks, label_chunks, detail_chunks]),
  )
}

fn render_ui_request(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let #(state, close_chunks) = close_assistant(state)
  let #(state, heading_chunks) = ensure_turn_heading(state, payload, options)
  let method = safe_option_string(payload.method, "unknown")
  let request = safe_option_string(payload.request_id, "")
  let suffix = case request == "" {
    True -> ""
    False -> " #" <> request
  }
  let message_chunks = case payload.message {
    Some(message) -> [Line("  " <> sanitize.text(message))]
    None -> []
  }
  #(
    RenderState(..state, assistant_open: False, active_tool_label: None),
    list.flatten([
      close_chunks,
      heading_chunks,
      [
        Line(style.warning(
          options.color_mode,
          "UI request: " <> method <> suffix,
        )),
      ],
      message_chunks,
    ]),
  )
}

fn render_ui_response(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let #(state, close_chunks) = close_assistant(state)
  let #(state, heading_chunks) = ensure_turn_heading(state, payload, options)
  let method = safe_option_string(payload.method, payload.name)
  #(
    RenderState(..state, assistant_open: False, active_tool_label: None),
    list.flatten([
      close_chunks,
      heading_chunks,
      [Line(style.dim(options.color_mode, "UI response: " <> method))],
    ]),
  )
}

fn render_tokens(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let #(state, close_chunks) = close_assistant(state)
  case tokens_are_nonzero(payload.tokens) {
    False -> #(state, close_chunks)
    True -> #(
      RenderState(..state, assistant_open: False, active_tool_label: None),
      list.append(close_chunks, [
        Line(style.dim(options.color_mode, token_line(payload.tokens))),
      ]),
    )
  }
}

fn render_unknown(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let #(state, close_chunks) = close_assistant(state)
  let #(state, heading_chunks) = ensure_turn_heading(state, payload, options)
  let name = safe_option_string(payload.pi_type, payload.name)
  let raw_chunks = case options.show_raw_unknown, payload.raw_json {
    True, Some(raw) -> [
      Line("  raw: " <> sanitize.text(compact_raw(raw.value))),
    ]
    _, _ -> []
  }
  #(
    RenderState(..state, assistant_open: False, active_tool_label: None),
    list.flatten([
      close_chunks,
      heading_chunks,
      [Line("event " <> name)],
      raw_chunks,
    ]),
  )
}

fn render_error_event(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  let #(state, close_chunks) = close_assistant(state)
  let message = safe_option_string(payload.message, payload.name)
  #(
    RenderState(..state, assistant_open: False, active_tool_label: None),
    list.append(close_chunks, [
      Line(style.error(options.color_mode, "error: " <> message)),
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
    _, _, False -> #(state, [])
  }
}

fn ensure_turn_heading(
  state: RenderState,
  payload: event.EventPayload,
  options: RenderOptions,
) -> #(RenderState, List(RenderChunk)) {
  case state.current_turn, payload.turn {
    None, Some(turn) -> #(RenderState(..state, current_turn: Some(turn)), [
      Line(style.heading(
        options.color_mode,
        "▶ turn " <> int.to_string(turn) <> " continued",
      )),
    ])
    _, _ -> #(state, [])
  }
}

fn close_assistant(state: RenderState) -> #(RenderState, List(RenderChunk)) {
  case state.assistant_open {
    True -> #(RenderState(..state, assistant_open: False), [Line("")])
    False -> #(state, [])
  }
}

fn turn_label(turn: Option(Int)) -> String {
  case turn {
    Some(turn) -> int.to_string(turn)
    None -> "?"
  }
}

fn tool_label(payload: event.EventPayload) -> String {
  case payload.tool_name {
    Some(name) -> "tool " <> sanitize.text(name)
    None -> "tool " <> sanitize.text(payload.name)
  }
}

fn tool_detail_chunks(payload: event.EventPayload) -> List(RenderChunk) {
  list.flatten([
    option_line("  input: ", payload.tool_input),
    option_line("  output: ", payload.tool_output),
    option_line("  status: ", payload.tool_status),
  ])
}

fn option_line(prefix: String, value: Option(String)) -> List(RenderChunk) {
  case value {
    Some(value) -> [Line(prefix <> sanitize.text(value))]
    None -> []
  }
}

fn next_active_tool(
  label: String,
  payload: event.EventPayload,
) -> Option(String) {
  case payload.tool_status, string.ends_with(payload.name, "_end") {
    Some(_), _ -> None
    None, True -> None
    None, False -> Some(label)
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

fn token_line(tokens: domain.TokenTotals) -> String {
  "tokens: input="
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

fn tokens_are_nonzero(tokens: domain.TokenTotals) -> Bool {
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
