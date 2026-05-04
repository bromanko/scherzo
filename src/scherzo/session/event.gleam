import gleam/option.{type Option, None, Some}
import scherzo/agent/pi_event
import scherzo/session/reason
import scherzo/session/tokens as session_tokens
import scherzo/turn_telemetry

pub type SessionStatus {
  Preparing
  Probing
  Running
  WaitingUi
  Stopping
  Exited(reason: reason.WorkerExitReason)
}

pub type RecoveryStatus {
  Recovered
  Interrupted
  Resumed
  InspectionNeeded
  Blocked
  Parked
  Cleanup
  DriftDetected
  OldStateResetRequired
}

pub type CleanupPhase {
  Retained
  Eligible
  Deleting
  Deleted
}

pub type RecoveryAction {
  Inspect
  ViewEvents
  Retry
  Park
  Unpark
  CleanupDryRunAction
  ArchiveOldState
  DiscardOldState
  ReinitializeState
}

pub type RecoveryInfo {
  RecoveryInfo(
    status: RecoveryStatus,
    source: String,
    message: Option(String),
    safe_actions: List(RecoveryAction),
    workflow_run_id: Option(String),
    workflow_step_id: Option(String),
    current_pi_session_id: Option(String),
    previous_pi_session_id: Option(String),
    park_reason: Option(String),
    park_release_policy: Option(String),
    parked_at_ms: Option(Int),
    drift_kind: Option(String),
    retention_until_ms: Option(Int),
    cleanup_eligible_at_ms: Option(Int),
    cleanup_phase: Option(CleanupPhase),
  )
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
  Turn
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
  RecoveryDetected
  RecoveryInterrupted
  RecoveryParked
  RecoveryCleanup
  OldStateResetRequiredEvent
  CleanupDryRun
  CleanupStarted
  CleanupCompleted
}

pub type EventName {
  LifecycleName(LifecycleEventName)
  PiName(pi_event.PiEvent)
  TurnName(turn_telemetry.TurnEventName)
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
    recovery: Option(RecoveryInfo),
    request_id: Option(String),
    method: Option(String),
    tool_name: Option(String),
    tool_input: Option(String),
    tool_output: Option(String),
    tool_status: Option(String),
    tokens: session_tokens.TokenTotals,
    turn_status: Option(turn_telemetry.TurnStatus),
    turn_started_at_ms: Option(Int),
    turn_finished_at_ms: Option(Int),
    turn_duration_ms: Option(Int),
    token_delta: session_tokens.TokenTotals,
    reason: Option(turn_telemetry.TurnReason),
    raw_json: Option(RedactedRawJson),
  )
}

pub type SessionSummary {
  SessionSummary(
    session_id: String,
    display_name: String,
    issue_id: String,
    issue_identifier: String,
    issue_title: String,
    workspace_path: String,
    pi_session_id: Option(String),
    status: SessionStatus,
    recovery: Option(RecoveryInfo),
    current_turn: Int,
    current_turn_status: Option(turn_telemetry.TurnStatus),
    current_turn_started_at_ms: Option(Int),
    last_turn_finished_at_ms: Option(Int),
    last_turn_duration_ms: Option(Int),
    last_turn_token_delta: session_tokens.TokenTotals,
    last_turn_reason: Option(turn_telemetry.TurnReason),
    started_at_ms: Int,
    last_event_at_ms: Int,
    token_totals: session_tokens.TokenTotals,
  )
}

pub type SessionList {
  SessionList(sessions: List(SessionSummary), now_ms: Int)
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
    recovery: None,
    request_id: None,
    method: None,
    tool_name: None,
    tool_input: None,
    tool_output: None,
    tool_status: None,
    tokens: session_tokens.zero_token_totals(),
    turn_status: None,
    turn_started_at_ms: None,
    turn_finished_at_ms: None,
    turn_duration_ms: None,
    token_delta: session_tokens.zero_token_totals(),
    reason: None,
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

pub fn recovery_status_to_string(status: RecoveryStatus) -> String {
  case status {
    Recovered -> "recovered"
    Interrupted -> "interrupted"
    Resumed -> "resumed"
    InspectionNeeded -> "inspection_needed"
    Blocked -> "blocked"
    Parked -> "parked"
    Cleanup -> "cleanup"
    DriftDetected -> "drift_detected"
    OldStateResetRequired -> "old_state_reset_required"
  }
}

pub fn recovery_status_from_string(value: String) -> Option(RecoveryStatus) {
  case value {
    "recovered" -> Some(Recovered)
    "interrupted" -> Some(Interrupted)
    "resumed" -> Some(Resumed)
    "inspection_needed" -> Some(InspectionNeeded)
    "blocked" -> Some(Blocked)
    "parked" -> Some(Parked)
    "cleanup" -> Some(Cleanup)
    "drift_detected" -> Some(DriftDetected)
    "old_state_reset_required" -> Some(OldStateResetRequired)
    _ -> None
  }
}

pub fn cleanup_phase_to_string(phase: CleanupPhase) -> String {
  case phase {
    Retained -> "retained"
    Eligible -> "eligible"
    Deleting -> "deleting"
    Deleted -> "deleted"
  }
}

pub fn cleanup_phase_from_string(value: String) -> Option(CleanupPhase) {
  case value {
    "retained" -> Some(Retained)
    "eligible" -> Some(Eligible)
    "deleting" -> Some(Deleting)
    "deleted" -> Some(Deleted)
    _ -> None
  }
}

pub fn recovery_action_to_string(action: RecoveryAction) -> String {
  case action {
    Inspect -> "inspect"
    ViewEvents -> "view_events"
    Retry -> "retry"
    Park -> "park"
    Unpark -> "unpark"
    CleanupDryRunAction -> "cleanup_dry_run"
    ArchiveOldState -> "archive_old_state"
    DiscardOldState -> "discard_old_state"
    ReinitializeState -> "reinitialize_state"
  }
}

pub fn recovery_action_from_string(value: String) -> Option(RecoveryAction) {
  case value {
    "inspect" -> Some(Inspect)
    "view_events" -> Some(ViewEvents)
    "retry" -> Some(Retry)
    "park" -> Some(Park)
    "unpark" -> Some(Unpark)
    "cleanup_dry_run" -> Some(CleanupDryRunAction)
    "archive_old_state" -> Some(ArchiveOldState)
    "discard_old_state" -> Some(DiscardOldState)
    "reinitialize_state" -> Some(ReinitializeState)
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
    RecoveryDetected -> "recovery_detected"
    RecoveryInterrupted -> "recovery_interrupted"
    RecoveryParked -> "recovery_parked"
    RecoveryCleanup -> "recovery_cleanup"
    OldStateResetRequiredEvent -> "old_state_reset_required"
    CleanupDryRun -> "cleanup_dry_run"
    CleanupStarted -> "cleanup_started"
    CleanupCompleted -> "cleanup_completed"
  }
}

pub fn name_to_string(name: EventName) -> String {
  case name {
    LifecycleName(name) -> lifecycle_name_to_string(name)
    PiName(name) -> pi_event.to_string(name)
    TurnName(name) -> turn_telemetry.event_name_to_string(name)
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
    "recovery_detected" -> Some(RecoveryDetected)
    "recovery_interrupted" -> Some(RecoveryInterrupted)
    "recovery_parked" -> Some(RecoveryParked)
    "recovery_cleanup" -> Some(RecoveryCleanup)
    "old_state_reset_required" -> Some(OldStateResetRequiredEvent)
    "cleanup_dry_run" -> Some(CleanupDryRun)
    "cleanup_started" -> Some(CleanupStarted)
    "cleanup_completed" -> Some(CleanupCompleted)
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
    Turn -> "turn"
  }
}
