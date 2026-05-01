import gleam/erlang/process
import gleam/option.{type Option, None, Some}
import scherzo/agent/runner
import scherzo/domain
import scherzo/session/event as session_event
import scherzo/session/hub

pub fn worker_update(
  event_hub: process.Subject(hub.Message),
  session_id: String,
  update: runner.PiUpdate,
) -> Nil {
  case status_for_update(update) {
    Some(status) -> hub.update_status(event_hub, session_id, status)
    None -> Nil
  }
  case update.pi_session_id {
    Some(pi_session_id) ->
      hub.update_pi_session(event_hub, session_id, pi_session_id)
    None -> Nil
  }
  case tokens_are_nonzero(update.tokens) {
    True -> hub.update_tokens(event_hub, session_id, update.tokens)
    False -> Nil
  }
  hub.publish(event_hub, session_id, update_payload(update))
}

pub fn lifecycle(
  event_hub: process.Subject(hub.Message),
  session_id: String,
  name: String,
  message: Option(String),
) -> Nil {
  hub.publish(
    event_hub,
    session_id,
    session_event.EventPayload(
      kind: session_event.Lifecycle,
      name: name,
      turn: None,
      pi_type: None,
      message: message,
      request_id: None,
      method: None,
      tool_name: None,
      tool_input: None,
      tool_output: None,
      tool_status: None,
      tokens: domain.zero_token_totals(),
      raw_json: None,
    ),
  )
}

pub fn update_payload(update: runner.PiUpdate) -> session_event.EventPayload {
  session_event.EventPayload(
    kind: kind_for_update(update),
    name: update.event,
    turn: update.turn,
    pi_type: pi_type_for_update(update),
    message: update.message,
    request_id: update.request_id,
    method: update.method,
    tool_name: update.tool_name,
    tool_input: update.tool_input,
    tool_output: update.tool_output,
    tool_status: update.tool_status,
    tokens: update.tokens,
    raw_json: update.raw_json,
  )
}

pub fn kind_for_update(update: runner.PiUpdate) -> session_event.EventKind {
  case update.event {
    "probe_started" | "probe_finished" | "pi_session_started" ->
      session_event.Lifecycle
    "turn_finished" -> session_event.TokenStats
    "message_start" | "message_update" | "message_end" ->
      session_event.AssistantMessage
    "tool_execution_start" | "tool_execution_update" | "tool_execution_end" ->
      session_event.Tool
    "message" ->
      case
        update.tool_name,
        update.tool_input,
        update.tool_output,
        update.tool_status
      {
        Some(_), _, _, _
        | _, Some(_), _, _
        | _, _, Some(_), _
        | _, _, _, Some(_)
        -> session_event.Tool
        _, _, _, _ -> session_event.Pi
      }
    "extension_ui_request" ->
      case is_blocking_ui_method(update.method) {
        True -> session_event.UiRequest
        False -> session_event.Pi
      }
    "extension_ui_response" -> session_event.UiResponse
    "agent_start" | "turn_start" | "turn_end" | "agent_end" -> session_event.Pi
    _ ->
      case update.raw_json {
        Some(_) -> session_event.PiRaw
        None -> session_event.Lifecycle
      }
  }
}

pub fn pi_type_for_update(update: runner.PiUpdate) -> Option(String) {
  case update.raw_json {
    Some(_) -> Some(update.event)
    None -> None
  }
}

pub fn status_for_update(
  update: runner.PiUpdate,
) -> Option(session_event.SessionStatus) {
  case update.event {
    "probe_started" | "probe_finished" -> Some(session_event.Probing)
    "pi_session_started" -> Some(session_event.Running)
    "extension_ui_request" ->
      case is_blocking_ui_method(update.method) {
        True -> Some(session_event.WaitingUi)
        False -> Some(session_event.Running)
      }
    "extension_ui_response" | "turn_finished" -> Some(session_event.Running)
    _ ->
      case update.raw_json {
        Some(_) -> Some(session_event.Running)
        None -> None
      }
  }
}

pub fn is_blocking_ui_method(method: Option(String)) -> Bool {
  case method {
    Some("select") | Some("confirm") | Some("input") | Some("editor") -> True
    _ -> False
  }
}

pub fn tokens_are_nonzero(tokens: domain.TokenTotals) -> Bool {
  tokens.input > 0
  || tokens.output > 0
  || tokens.cache_read > 0
  || tokens.cache_write > 0
  || tokens.total > 0
}
