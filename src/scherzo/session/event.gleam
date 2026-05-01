import gleam/option.{type Option, None, Some}
import scherzo/agent/pi_event
import scherzo/domain
import scherzo/session/reason

pub type SessionStatus {
  Preparing
  Probing
  Running
  WaitingUi
  Stopping
  Exited(reason: reason.WorkerExitReason)
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

pub type LifecycleEventName {
  DispatchStarted
  WorkerStarted
  StopRequested
  WorkerExited
  WorkerDown
  RetryScheduled
  OperatorCommand
  StepStarted
}

pub type EventName {
  LifecycleName(LifecycleEventName)
  PiName(pi_event.PiEvent)
}

pub type RedactedRawJson {
  RedactedRawJson(value: String, truncated: Bool)
}

pub type EventPayload {
  EventPayload(
    kind: EventKind,
    name: EventName,
    turn: Option(Int),
    pi_type: Option(String),
    message: Option(String),
    request_id: Option(String),
    method: Option(String),
    tool_name: Option(String),
    tool_input: Option(String),
    tool_output: Option(String),
    tool_status: Option(String),
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

pub fn empty_payload(kind: EventKind, name: EventName) -> EventPayload {
  EventPayload(
    kind: kind,
    name: name,
    turn: None,
    pi_type: None,
    message: None,
    request_id: None,
    method: None,
    tool_name: None,
    tool_input: None,
    tool_output: None,
    tool_status: None,
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

pub fn exit_reason(status: SessionStatus) -> Option(reason.WorkerExitReason) {
  case status {
    Exited(reason) -> Some(reason)
    _ -> None
  }
}

pub fn lifecycle_name_to_string(name: LifecycleEventName) -> String {
  case name {
    DispatchStarted -> "dispatch_started"
    WorkerStarted -> "worker_started"
    StopRequested -> "stop_requested"
    WorkerExited -> "worker_exited"
    WorkerDown -> "worker_down"
    RetryScheduled -> "retry_scheduled"
    OperatorCommand -> "operator_command"
    StepStarted -> "step_started"
  }
}

pub fn name_to_string(name: EventName) -> String {
  case name {
    LifecycleName(name) -> lifecycle_name_to_string(name)
    PiName(name) -> pi_event.to_string(name)
  }
}

pub fn lifecycle_name_from_string(value: String) -> Option(LifecycleEventName) {
  case value {
    "dispatch_started" -> Some(DispatchStarted)
    "worker_started" -> Some(WorkerStarted)
    "stop_requested" -> Some(StopRequested)
    "worker_exited" -> Some(WorkerExited)
    "worker_down" -> Some(WorkerDown)
    "retry_scheduled" -> Some(RetryScheduled)
    "operator_command" -> Some(OperatorCommand)
    "step_started" -> Some(StepStarted)
    _ -> None
  }
}

pub fn name_from_string(value: String) -> Result(EventName, Nil) {
  case lifecycle_name_from_string(value) {
    Some(name) -> Ok(LifecycleName(name))
    None -> Ok(PiName(pi_event.from_string(value)))
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
