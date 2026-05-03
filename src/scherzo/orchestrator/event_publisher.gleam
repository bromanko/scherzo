import gleam/erlang/process
import gleam/option.{type Option, None, Some}
import scherzo/agent/pi_event
import scherzo/agent/types as agent_types
import scherzo/domain
import scherzo/session/event as session_event
import scherzo/session/hub

pub fn worker_update(
  event_hub: process.Subject(hub.Message),
  session_id: String,
  update: agent_types.PiUpdate,
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
  name: session_event.LifecycleEventName,
  message: Option(String),
) -> Nil {
  hub.publish(
    event_hub,
    session_id,
    session_event.EventPayload(
      kind: session_event.Lifecycle,
      name: session_event.LifecycleName(name),
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

pub fn update_payload(
  update: agent_types.PiUpdate,
) -> session_event.EventPayload {
  session_event.EventPayload(
    kind: kind_for_update(update),
    name: session_event.PiName(update.event),
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

pub fn kind_for_update(
  update: agent_types.PiUpdate,
) -> session_event.EventKind {
  case update.event {
    pi_event.ProbeStarted
    | pi_event.ProbeFinished
    | pi_event.PiSessionStarted -> session_event.Lifecycle
    pi_event.TurnFinished -> session_event.TokenStats
    pi_event.MessageStart | pi_event.MessageUpdate | pi_event.MessageEnd ->
      session_event.AssistantMessage
    pi_event.ToolExecutionStart
    | pi_event.ToolExecutionUpdate
    | pi_event.ToolExecutionEnd -> session_event.Tool
    pi_event.Message ->
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
    pi_event.ExtensionUiRequest ->
      case is_blocking_ui_method(update.method) {
        True -> session_event.UiRequest
        False -> session_event.Pi
      }
    pi_event.ExtensionUiResponse -> session_event.UiResponse
    pi_event.AgentStart
    | pi_event.TurnStart
    | pi_event.TurnEnd
    | pi_event.AgentEnd -> session_event.Pi
    pi_event.UnknownPiEvent(_) ->
      case update.raw_json {
        Some(_) -> session_event.PiRaw
        None -> session_event.Lifecycle
      }
    _ -> session_event.Lifecycle
  }
}

pub fn pi_type_for_update(update: agent_types.PiUpdate) -> Option(String) {
  case update.raw_json {
    Some(_) -> Some(pi_event.to_string(update.event))
    None -> None
  }
}

pub fn status_for_update(
  update: agent_types.PiUpdate,
) -> Option(session_event.SessionStatus) {
  case update.event {
    pi_event.ProbeStarted | pi_event.ProbeFinished ->
      Some(session_event.Probing)
    pi_event.PiSessionStarted -> Some(session_event.Running)
    pi_event.ExtensionUiRequest ->
      case is_blocking_ui_method(update.method) {
        True -> Some(session_event.WaitingUi)
        False -> Some(session_event.Running)
      }
    pi_event.ExtensionUiResponse | pi_event.TurnFinished ->
      Some(session_event.Running)
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
