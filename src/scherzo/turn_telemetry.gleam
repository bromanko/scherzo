import gleam/option.{type Option, None, Some}
import scherzo/session/tokens as session_tokens

pub type TurnStatus {
  StatusRunning
  StatusFinished
  StatusFailed
  StatusStopped
  StatusTimedOut
}

pub type TurnEventName {
  EventStarted
  EventFinished
  EventFailed
  EventStopped
  EventTimedOut
  EventUnknown(String)
}

pub type TurnReason {
  ReasonOperatorStopAfterCurrentTurn
  ReasonOperatorAbort
  ReasonPiStallTimeout
  ReasonPiTurnTimeout
  ReasonPiError
  ReasonStateRefreshFailed
}

pub type TurnLifecycleUpdate {
  TurnLifecycleUpdate(
    name: TurnEventName,
    turn: Int,
    tokens: session_tokens.TokenTotals,
    reason: Option(TurnReason),
  )
}

pub fn status_to_string(status: TurnStatus) -> String {
  case status {
    StatusRunning -> "running"
    StatusFinished -> "finished"
    StatusFailed -> "failed"
    StatusStopped -> "stopped"
    StatusTimedOut -> "timed_out"
  }
}

pub fn status_from_string(value: String) -> Option(TurnStatus) {
  case value {
    "running" -> Some(StatusRunning)
    "finished" -> Some(StatusFinished)
    "failed" -> Some(StatusFailed)
    "stopped" -> Some(StatusStopped)
    "timed_out" -> Some(StatusTimedOut)
    _ -> None
  }
}

pub fn event_name_to_string(name: TurnEventName) -> String {
  case name {
    EventStarted -> "turn_started"
    EventFinished -> "turn_finished"
    EventFailed -> "turn_failed"
    EventStopped -> "turn_stopped"
    EventTimedOut -> "turn_timed_out"
    EventUnknown(value) -> value
  }
}

pub fn event_name_from_string(value: String) -> Option(TurnEventName) {
  case value {
    "turn_started" -> Some(EventStarted)
    "turn_finished" -> Some(EventFinished)
    "turn_failed" -> Some(EventFailed)
    "turn_stopped" -> Some(EventStopped)
    "turn_timed_out" -> Some(EventTimedOut)
    _ -> None
  }
}

pub fn reason_to_string(reason: TurnReason) -> String {
  case reason {
    ReasonOperatorStopAfterCurrentTurn -> "operator_stop_after_current_turn"
    ReasonOperatorAbort -> "operator_abort"
    ReasonPiStallTimeout -> "pi_stall_timeout"
    ReasonPiTurnTimeout -> "pi_turn_timeout"
    ReasonPiError -> "pi_error"
    ReasonStateRefreshFailed -> "state_refresh_failed"
  }
}

pub fn reason_from_string(value: String) -> Option(TurnReason) {
  case value {
    "operator_stop_after_current_turn" ->
      Some(ReasonOperatorStopAfterCurrentTurn)
    "operator_abort" -> Some(ReasonOperatorAbort)
    "pi_stall_timeout" -> Some(ReasonPiStallTimeout)
    "pi_turn_timeout" -> Some(ReasonPiTurnTimeout)
    "pi_error" -> Some(ReasonPiError)
    "state_refresh_failed" -> Some(ReasonStateRefreshFailed)
    _ -> None
  }
}

pub fn status_for_event_name(name: TurnEventName) -> Option(TurnStatus) {
  case name {
    EventStarted -> Some(StatusRunning)
    EventFinished -> Some(StatusFinished)
    EventFailed -> Some(StatusFailed)
    EventStopped -> Some(StatusStopped)
    EventTimedOut -> Some(StatusTimedOut)
    EventUnknown(_) -> None
  }
}
