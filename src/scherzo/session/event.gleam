import gleam/option.{type Option, None, Some}
import scherzo/domain

pub type SessionStatus {
  Preparing
  Probing
  Running
  WaitingUi
  Stopping
  Exited(reason: String)
}

pub type EventKind {
  Lifecycle
  Pi
  AssistantMessage
  Tool
  UiRequest
  UiResponse
  TokenStats
  Error
  PiRaw
}

pub type RedactedRawJson {
  RedactedRawJson(value: String, truncated: Bool)
}

pub type EventPayload {
  EventPayload(
    kind: EventKind,
    name: String,
    turn: Option(Int),
    pi_type: Option(String),
    message: Option(String),
    request_id: Option(String),
    method: Option(String),
    tool_name: Option(String),
    tokens: domain.TokenTotals,
    raw_json: Option(RedactedRawJson),
  )
}

pub type SessionSummary {
  SessionSummary(
    session_id: String,
    issue_id: String,
    issue_identifier: String,
    issue_title: String,
    workspace_path: String,
    pi_session_id: Option(String),
    status: SessionStatus,
    current_turn: Int,
    started_at_ms: Int,
    last_event_at_ms: Int,
    token_totals: domain.TokenTotals,
  )
}

pub type SessionEvent {
  SessionEvent(
    cursor: Int,
    at_ms: Int,
    session_id: String,
    issue_id: String,
    payload: EventPayload,
  )
}

pub type EventPage {
  EventPage(events: List(SessionEvent), next_cursor: Int, truncated: Bool)
}

pub fn empty_payload(kind: EventKind, name: String) -> EventPayload {
  EventPayload(
    kind: kind,
    name: name,
    turn: None,
    pi_type: None,
    message: None,
    request_id: None,
    method: None,
    tool_name: None,
    tokens: domain.zero_token_totals(),
    raw_json: None,
  )
}

pub fn status_to_string(status: SessionStatus) -> String {
  case status {
    Preparing -> "preparing"
    Probing -> "probing"
    Running -> "running"
    WaitingUi -> "waiting_ui"
    Stopping -> "stopping"
    Exited(_) -> "exited"
  }
}

pub fn exit_reason(status: SessionStatus) -> Option(String) {
  case status {
    Exited(reason) -> Some(reason)
    _ -> None
  }
}

pub fn kind_to_string(kind: EventKind) -> String {
  case kind {
    Lifecycle -> "lifecycle"
    Pi -> "pi"
    AssistantMessage -> "assistant_message"
    Tool -> "tool"
    UiRequest -> "ui_request"
    UiResponse -> "ui_response"
    TokenStats -> "token_stats"
    Error -> "error"
    PiRaw -> "pi_raw"
  }
}
