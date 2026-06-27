import gleam/option.{type Option, None, Some}
import gleam/string
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
    workflow_attempt_index: Option(Int),
    parent_session_id: Option(String),
    orphan_status: Option(String),
    issue_state: Option(String),
    recommended_action: Option(String),
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

type LifecyclePayloadName {
  SessionLifecycleName(LifecycleEventName)
  PiLifecycleName(pi_event.PiEvent)
}

pub type RedactedRawJson {
  RedactedRawJson(value: String, truncated: Bool)
}

pub opaque type EventPayload {
  LifecyclePayload(
    name: LifecyclePayloadName,
    turn: Option(Int),
    message: Option(String),
    recovery: Option(RecoveryInfo),
    tokens: session_tokens.TokenTotals,
  )
  PiPayload(
    name: pi_event.PiEvent,
    turn: Option(Int),
    message: Option(String),
    tokens: session_tokens.TokenTotals,
  )
  AssistantMessagePayload(
    name: pi_event.PiEvent,
    turn: Option(Int),
    message: Option(String),
    tokens: session_tokens.TokenTotals,
  )
  ToolPayload(
    name: pi_event.PiEvent,
    turn: Option(Int),
    message: Option(String),
    request_id: Option(String),
    method: Option(String),
    tool_name: Option(String),
    tool_input: Option(String),
    tool_output: Option(String),
    tool_status: Option(String),
    tokens: session_tokens.TokenTotals,
  )
  UiRequestPayload(
    name: pi_event.PiEvent,
    turn: Option(Int),
    message: Option(String),
    request_id: Option(String),
    method: Option(String),
    tokens: session_tokens.TokenTotals,
  )
  UiResponsePayload(
    name: pi_event.PiEvent,
    turn: Option(Int),
    message: Option(String),
    request_id: Option(String),
    method: Option(String),
    tokens: session_tokens.TokenTotals,
  )
  TokenStatsPayload(
    name: pi_event.PiEvent,
    turn: Option(Int),
    tokens: session_tokens.TokenTotals,
  )
  ErrorPayload(
    name: pi_event.PiEvent,
    turn: Option(Int),
    message: Option(String),
    tool_name: Option(String),
    tool_input: Option(String),
    tool_output: Option(String),
    tool_status: Option(String),
    tokens: session_tokens.TokenTotals,
  )
  PiRawPayload(
    name: pi_event.PiEvent,
    turn: Option(Int),
    pi_type: Option(String),
    message: Option(String),
    tokens: session_tokens.TokenTotals,
    raw_json: Option(RedactedRawJson),
  )
  TurnPayload(
    name: turn_telemetry.TurnEventName,
    turn: Option(Int),
    turn_status: Option(turn_telemetry.TurnStatus),
    turn_started_at_ms: Option(Int),
    turn_finished_at_ms: Option(Int),
    turn_duration_ms: Option(Int),
    tokens: session_tokens.TokenTotals,
    token_delta: session_tokens.TokenTotals,
    reason: Option(turn_telemetry.TurnReason),
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

pub fn lifecycle_payload(
  name: LifecycleEventName,
  message: Option(String),
  recovery: Option(RecoveryInfo),
) -> EventPayload {
  lifecycle_payload_with_details(
    name,
    None,
    message,
    recovery,
    session_tokens.zero_token_totals(),
  )
}

pub fn lifecycle_payload_with_details(
  name: LifecycleEventName,
  turn: Option(Int),
  message: Option(String),
  recovery: Option(RecoveryInfo),
  tokens: session_tokens.TokenTotals,
) -> EventPayload {
  LifecyclePayload(
    name: SessionLifecycleName(name),
    turn: turn,
    message: message,
    recovery: recovery,
    tokens: tokens,
  )
}

pub fn pi_event_payload(
  name: pi_event.PiEvent,
  turn: Option(Int),
  message: Option(String),
  request_id: Option(String),
  method: Option(String),
  tool_name: Option(String),
  tool_input: Option(String),
  tool_output: Option(String),
  tool_status: Option(String),
  tokens: session_tokens.TokenTotals,
  raw_json: Option(RedactedRawJson),
) -> EventPayload {
  case name, raw_json {
    pi_event.UnknownPiEvent(_), Some(raw_json) ->
      PiRawPayload(
        name: name,
        turn: turn,
        pi_type: Some(pi_event.to_string(name)),
        message: message,
        tokens: tokens,
        raw_json: Some(raw_json),
      )
    _, _ ->
      case name {
        pi_event.ProbeStarted
        | pi_event.ProbeFinished
        | pi_event.PiSessionStarted ->
          pi_lifecycle_payload(name, turn, message, tokens)
        pi_event.TurnFinished ->
          TokenStatsPayload(name: name, turn: turn, tokens: tokens)
        pi_event.MessageStart | pi_event.MessageUpdate | pi_event.MessageEnd ->
          AssistantMessagePayload(
            name: name,
            turn: turn,
            message: message,
            tokens: tokens,
          )
        pi_event.ToolExecutionStart
        | pi_event.ToolExecutionUpdate
        | pi_event.ToolExecutionEnd ->
          ToolPayload(
            name: name,
            turn: turn,
            message: message,
            request_id: request_id,
            method: method,
            tool_name: tool_name,
            tool_input: tool_input,
            tool_output: tool_output,
            tool_status: tool_status,
            tokens: tokens,
          )
        pi_event.Message ->
          case
            has_tool_fields(tool_name, tool_input, tool_output, tool_status)
          {
            True ->
              ToolPayload(
                name: name,
                turn: turn,
                message: message,
                request_id: request_id,
                method: method,
                tool_name: tool_name,
                tool_input: tool_input,
                tool_output: tool_output,
                tool_status: tool_status,
                tokens: tokens,
              )
            False ->
              PiPayload(
                name: name,
                turn: turn,
                message: message,
                tokens: tokens,
              )
          }
        pi_event.ExtensionUiRequest ->
          case is_blocking_ui_method(method) {
            True ->
              UiRequestPayload(
                name: name,
                turn: turn,
                message: message,
                request_id: request_id,
                method: method,
                tokens: tokens,
              )
            False ->
              PiPayload(
                name: name,
                turn: turn,
                message: message,
                tokens: tokens,
              )
          }
        pi_event.ExtensionUiResponse ->
          UiResponsePayload(
            name: name,
            turn: turn,
            message: message,
            request_id: request_id,
            method: method,
            tokens: tokens,
          )
        pi_event.UnknownPiEvent(_) ->
          pi_lifecycle_payload(name, turn, message, tokens)
        pi_event.AgentStart
        | pi_event.TurnStart
        | pi_event.TurnEnd
        | pi_event.AgentEnd
        | pi_event.AutoRetryStart
        | pi_event.AutoRetryEnd ->
          PiPayload(name: name, turn: turn, message: message, tokens: tokens)
        _ -> pi_lifecycle_payload(name, turn, message, tokens)
      }
  }
}

fn pi_lifecycle_payload(
  name: pi_event.PiEvent,
  turn: Option(Int),
  message: Option(String),
  tokens: session_tokens.TokenTotals,
) -> EventPayload {
  LifecyclePayload(
    name: PiLifecycleName(name),
    turn: turn,
    message: message,
    recovery: None,
    tokens: tokens,
  )
}

fn has_tool_fields(
  tool_name: Option(String),
  tool_input: Option(String),
  tool_output: Option(String),
  tool_status: Option(String),
) -> Bool {
  case tool_name, tool_input, tool_output, tool_status {
    Some(_), _, _, _ | _, Some(_), _, _ | _, _, Some(_), _ | _, _, _, Some(_) ->
      True
    _, _, _, _ -> False
  }
}

fn is_blocking_ui_method(method: Option(String)) -> Bool {
  case method {
    Some("select") | Some("confirm") | Some("input") | Some("editor") -> True
    _ -> False
  }
}

pub fn error_payload(
  name: String,
  message: Option(String),
  tool_name: Option(String),
  tool_input: Option(String),
  tool_output: Option(String),
  tool_status: Option(String),
) -> EventPayload {
  ErrorPayload(
    name: pi_event.UnknownPiEvent(name),
    turn: None,
    message: message,
    tool_name: tool_name,
    tool_input: tool_input,
    tool_output: tool_output,
    tool_status: tool_status,
    tokens: session_tokens.zero_token_totals(),
  )
}

pub fn turn_payload(
  name: turn_telemetry.TurnEventName,
  turn: Int,
  tokens: session_tokens.TokenTotals,
  reason: Option(turn_telemetry.TurnReason),
) -> EventPayload {
  decoded_turn_payload(
    name,
    Some(turn),
    turn_telemetry.status_for_event_name(name),
    None,
    None,
    None,
    tokens,
    session_tokens.zero_token_totals(),
    reason,
  )
}

pub fn decoded_payload(
  kind: EventKind,
  name_string: String,
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
) -> EventPayload {
  case kind {
    Lifecycle ->
      LifecyclePayload(
        name: decoded_lifecycle_payload_name(name_string),
        turn: turn,
        message: message,
        recovery: recovery,
        tokens: tokens,
      )
    Pi ->
      PiPayload(
        name: pi_event.from_string(name_string),
        turn: turn,
        message: message,
        tokens: tokens,
      )
    AssistantMessage ->
      AssistantMessagePayload(
        name: pi_event.from_string(name_string),
        turn: turn,
        message: message,
        tokens: tokens,
      )
    Tool ->
      ToolPayload(
        name: pi_event.from_string(name_string),
        turn: turn,
        message: message,
        request_id: request_id,
        method: method,
        tool_name: tool_name,
        tool_input: tool_input,
        tool_output: tool_output,
        tool_status: tool_status,
        tokens: tokens,
      )
    UiRequest ->
      UiRequestPayload(
        name: pi_event.from_string(name_string),
        turn: turn,
        message: message,
        request_id: request_id,
        method: method,
        tokens: tokens,
      )
    UiResponse ->
      UiResponsePayload(
        name: pi_event.from_string(name_string),
        turn: turn,
        message: message,
        request_id: request_id,
        method: method,
        tokens: tokens,
      )
    TokenStats ->
      TokenStatsPayload(
        name: pi_event.from_string(name_string),
        turn: turn,
        tokens: tokens,
      )
    Error ->
      ErrorPayload(
        name: pi_event.from_string(name_string),
        turn: turn,
        message: message,
        tool_name: tool_name,
        tool_input: tool_input,
        tool_output: tool_output,
        tool_status: tool_status,
        tokens: tokens,
      )
    PiRaw ->
      PiRawPayload(
        name: pi_event.from_string(name_string),
        turn: turn,
        pi_type: pi_type,
        message: message,
        tokens: tokens,
        raw_json: raw_json,
      )
    Turn ->
      decoded_turn_payload(
        decoded_turn_event_name(name_string),
        turn,
        turn_status,
        turn_started_at_ms,
        turn_finished_at_ms,
        turn_duration_ms,
        tokens,
        token_delta,
        reason,
      )
  }
}

pub fn decoded_empty_payload(
  kind: EventKind,
  name_string: String,
) -> EventPayload {
  decoded_payload(
    kind,
    name_string,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    session_tokens.zero_token_totals(),
    None,
    None,
    None,
    None,
    session_tokens.zero_token_totals(),
    None,
    None,
  )
}

fn decoded_lifecycle_payload_name(name_string: String) -> LifecyclePayloadName {
  case lifecycle_name_from_string(name_string) {
    Some(name) -> SessionLifecycleName(name)
    None -> PiLifecycleName(pi_event.from_string(name_string))
  }
}

fn decoded_turn_event_name(
  name_string: String,
) -> turn_telemetry.TurnEventName {
  case turn_telemetry.event_name_from_string(name_string) {
    Some(name) -> name
    None -> turn_telemetry.EventUnknown(name_string)
  }
}

fn decoded_turn_payload(
  name: turn_telemetry.TurnEventName,
  turn: Option(Int),
  turn_status: Option(turn_telemetry.TurnStatus),
  turn_started_at_ms: Option(Int),
  turn_finished_at_ms: Option(Int),
  turn_duration_ms: Option(Int),
  tokens: session_tokens.TokenTotals,
  token_delta: session_tokens.TokenTotals,
  reason: Option(turn_telemetry.TurnReason),
) -> EventPayload {
  TurnPayload(
    name: name,
    turn: turn,
    turn_status: turn_status,
    turn_started_at_ms: turn_started_at_ms,
    turn_finished_at_ms: turn_finished_at_ms,
    turn_duration_ms: turn_duration_ms,
    tokens: tokens,
    token_delta: token_delta,
    reason: reason,
  )
}

pub fn payload_kind(payload: EventPayload) -> EventKind {
  case payload {
    LifecyclePayload(..) -> Lifecycle
    PiPayload(..) -> Pi
    AssistantMessagePayload(..) -> AssistantMessage
    ToolPayload(..) -> Tool
    UiRequestPayload(..) -> UiRequest
    UiResponsePayload(..) -> UiResponse
    TokenStatsPayload(..) -> TokenStats
    ErrorPayload(..) -> Error
    PiRawPayload(..) -> PiRaw
    TurnPayload(..) -> Turn
  }
}

pub fn payload_name(payload: EventPayload) -> EventName {
  case payload {
    LifecyclePayload(name: SessionLifecycleName(name), ..) ->
      LifecycleName(name)
    LifecyclePayload(name: PiLifecycleName(name), ..) -> PiName(name)
    PiPayload(name: name, ..) -> PiName(name)
    AssistantMessagePayload(name: name, ..) -> PiName(name)
    ToolPayload(name: name, ..) -> PiName(name)
    UiRequestPayload(name: name, ..) -> PiName(name)
    UiResponsePayload(name: name, ..) -> PiName(name)
    TokenStatsPayload(name: name, ..) -> PiName(name)
    ErrorPayload(name: name, ..) -> PiName(name)
    PiRawPayload(name: name, ..) -> PiName(name)
    TurnPayload(name: name, ..) -> TurnName(name)
  }
}

pub fn payload_name_to_string(payload: EventPayload) -> String {
  payload |> payload_name |> name_to_string
}

pub fn payload_turn(payload: EventPayload) -> Option(Int) {
  case payload {
    LifecyclePayload(turn: turn, ..)
    | PiPayload(turn: turn, ..)
    | AssistantMessagePayload(turn: turn, ..)
    | ToolPayload(turn: turn, ..)
    | UiRequestPayload(turn: turn, ..)
    | UiResponsePayload(turn: turn, ..)
    | TokenStatsPayload(turn: turn, ..)
    | ErrorPayload(turn: turn, ..)
    | PiRawPayload(turn: turn, ..)
    | TurnPayload(turn: turn, ..) -> turn
  }
}

pub fn payload_pi_type(payload: EventPayload) -> Option(String) {
  case payload {
    PiRawPayload(pi_type: pi_type, ..) -> pi_type
    _ -> None
  }
}

pub fn payload_message(payload: EventPayload) -> Option(String) {
  case payload {
    LifecyclePayload(message: message, ..)
    | PiPayload(message: message, ..)
    | AssistantMessagePayload(message: message, ..)
    | ToolPayload(message: message, ..)
    | UiRequestPayload(message: message, ..)
    | UiResponsePayload(message: message, ..)
    | ErrorPayload(message: message, ..)
    | PiRawPayload(message: message, ..) -> message
    TokenStatsPayload(..) | TurnPayload(..) -> None
  }
}

pub fn payload_recovery(payload: EventPayload) -> Option(RecoveryInfo) {
  case payload {
    LifecyclePayload(recovery: recovery, ..) -> recovery
    _ -> None
  }
}

pub fn payload_request_id(payload: EventPayload) -> Option(String) {
  case payload {
    ToolPayload(request_id: request_id, ..)
    | UiRequestPayload(request_id: request_id, ..)
    | UiResponsePayload(request_id: request_id, ..) -> request_id
    _ -> None
  }
}

pub fn payload_method(payload: EventPayload) -> Option(String) {
  case payload {
    ToolPayload(method: method, ..)
    | UiRequestPayload(method: method, ..)
    | UiResponsePayload(method: method, ..) -> method
    _ -> None
  }
}

pub fn payload_tool_name(payload: EventPayload) -> Option(String) {
  case payload {
    ToolPayload(tool_name: tool_name, ..)
    | ErrorPayload(tool_name: tool_name, ..) -> tool_name
    _ -> None
  }
}

pub fn payload_tool_input(payload: EventPayload) -> Option(String) {
  case payload {
    ToolPayload(tool_input: tool_input, ..)
    | ErrorPayload(tool_input: tool_input, ..) -> tool_input
    _ -> None
  }
}

pub fn payload_tool_output(payload: EventPayload) -> Option(String) {
  case payload {
    ToolPayload(tool_output: tool_output, ..)
    | ErrorPayload(tool_output: tool_output, ..) -> tool_output
    _ -> None
  }
}

pub fn payload_tool_status(payload: EventPayload) -> Option(String) {
  case payload {
    ToolPayload(tool_status: tool_status, ..)
    | ErrorPayload(tool_status: tool_status, ..) -> tool_status
    _ -> None
  }
}

pub fn payload_tokens(payload: EventPayload) -> session_tokens.TokenTotals {
  case payload {
    LifecyclePayload(tokens: tokens, ..)
    | PiPayload(tokens: tokens, ..)
    | AssistantMessagePayload(tokens: tokens, ..)
    | ToolPayload(tokens: tokens, ..)
    | UiRequestPayload(tokens: tokens, ..)
    | UiResponsePayload(tokens: tokens, ..)
    | TokenStatsPayload(tokens: tokens, ..)
    | ErrorPayload(tokens: tokens, ..)
    | PiRawPayload(tokens: tokens, ..)
    | TurnPayload(tokens: tokens, ..) -> tokens
  }
}

pub fn payload_turn_status(
  payload: EventPayload,
) -> Option(turn_telemetry.TurnStatus) {
  case payload {
    TurnPayload(turn_status: turn_status, ..) -> turn_status
    _ -> None
  }
}

pub fn payload_turn_started_at_ms(payload: EventPayload) -> Option(Int) {
  case payload {
    TurnPayload(turn_started_at_ms: turn_started_at_ms, ..) ->
      turn_started_at_ms
    _ -> None
  }
}

pub fn payload_turn_finished_at_ms(payload: EventPayload) -> Option(Int) {
  case payload {
    TurnPayload(turn_finished_at_ms: turn_finished_at_ms, ..) ->
      turn_finished_at_ms
    _ -> None
  }
}

pub fn payload_turn_duration_ms(payload: EventPayload) -> Option(Int) {
  case payload {
    TurnPayload(turn_duration_ms: turn_duration_ms, ..) -> turn_duration_ms
    _ -> None
  }
}

pub fn payload_token_delta(
  payload: EventPayload,
) -> session_tokens.TokenTotals {
  case payload {
    TurnPayload(token_delta: token_delta, ..) -> token_delta
    _ -> session_tokens.zero_token_totals()
  }
}

pub fn payload_reason(
  payload: EventPayload,
) -> Option(turn_telemetry.TurnReason) {
  case payload {
    TurnPayload(reason: reason, ..) -> reason
    _ -> None
  }
}

pub fn payload_raw_json(payload: EventPayload) -> Option(RedactedRawJson) {
  case payload {
    PiRawPayload(raw_json: raw_json, ..) -> raw_json
    _ -> None
  }
}

pub fn with_payload_turn(payload: EventPayload, turn: Int) -> EventPayload {
  rebuild_payload_with_common_fields(
    payload,
    Some(turn),
    payload_tokens(payload),
  )
}

pub fn with_payload_tokens(
  payload: EventPayload,
  tokens: session_tokens.TokenTotals,
) -> EventPayload {
  rebuild_payload_with_common_fields(payload, payload_turn(payload), tokens)
}

fn rebuild_payload_with_common_fields(
  payload: EventPayload,
  turn: Option(Int),
  tokens: session_tokens.TokenTotals,
) -> EventPayload {
  decoded_payload(
    payload_kind(payload),
    payload_name_to_string(payload),
    turn,
    payload_pi_type(payload),
    payload_message(payload),
    payload_recovery(payload),
    payload_request_id(payload),
    payload_method(payload),
    payload_tool_name(payload),
    payload_tool_input(payload),
    payload_tool_output(payload),
    payload_tool_status(payload),
    tokens,
    payload_turn_status(payload),
    payload_turn_started_at_ms(payload),
    payload_turn_finished_at_ms(payload),
    payload_turn_duration_ms(payload),
    payload_token_delta(payload),
    payload_reason(payload),
    payload_raw_json(payload),
  )
}

pub fn turn_payload_status_for_name(
  payload: EventPayload,
) -> Option(turn_telemetry.TurnStatus) {
  case payload {
    TurnPayload(name: name, turn_status: turn_status, ..) ->
      case turn_telemetry.status_for_event_name(name) {
        Some(status) -> Some(status)
        None -> turn_status
      }
    _ -> payload_turn_status(payload)
  }
}

pub fn with_turn_started_details(
  payload: EventPayload,
  turn: Int,
  status: Option(turn_telemetry.TurnStatus),
  started_at_ms: Int,
) -> EventPayload {
  case payload {
    TurnPayload(name: name, tokens: tokens, reason: reason, ..) ->
      TurnPayload(
        name: name,
        turn: Some(turn),
        turn_status: status,
        turn_started_at_ms: Some(started_at_ms),
        turn_finished_at_ms: None,
        turn_duration_ms: None,
        tokens: tokens,
        token_delta: session_tokens.zero_token_totals(),
        reason: reason,
      )
    _ -> payload
  }
}

pub fn with_turn_terminal_details(
  payload: EventPayload,
  turn: Int,
  status: Option(turn_telemetry.TurnStatus),
  finished_at_ms: Int,
  duration_ms: Option(Int),
  token_delta: session_tokens.TokenTotals,
) -> EventPayload {
  case payload {
    TurnPayload(
      name: name,
      turn_started_at_ms: turn_started_at_ms,
      tokens: tokens,
      reason: reason,
      ..,
    ) ->
      TurnPayload(
        name: name,
        turn: Some(turn),
        turn_status: status,
        turn_started_at_ms: turn_started_at_ms,
        turn_finished_at_ms: Some(finished_at_ms),
        turn_duration_ms: duration_ms,
        tokens: tokens,
        token_delta: token_delta,
        reason: reason,
      )
    _ -> payload
  }
}

pub fn without_tool_input(payload: EventPayload) -> EventPayload {
  case payload {
    ToolPayload(
      name: name,
      turn: turn,
      message: message,
      request_id: request_id,
      method: method,
      tool_name: tool_name,
      tool_output: tool_output,
      tool_status: tool_status,
      tokens: tokens,
      ..,
    ) ->
      ToolPayload(
        name: name,
        turn: turn,
        message: message,
        request_id: request_id,
        method: method,
        tool_name: tool_name,
        tool_input: None,
        tool_output: tool_output,
        tool_status: tool_status,
        tokens: tokens,
      )
    _ -> payload
  }
}

pub fn as_assistant_message_payload(payload: EventPayload) -> EventPayload {
  AssistantMessagePayload(
    name: payload_pi_event_name(payload),
    turn: payload_turn(payload),
    message: payload_message(payload),
    tokens: payload_tokens(payload),
  )
}

fn payload_pi_event_name(payload: EventPayload) -> pi_event.PiEvent {
  case payload_name(payload) {
    PiName(name) -> name
    LifecycleName(name) ->
      pi_event.UnknownPiEvent(lifecycle_name_to_string(name))
    TurnName(name) ->
      pi_event.UnknownPiEvent(turn_telemetry.event_name_to_string(name))
  }
}

pub fn compact_for_exit(payload: EventPayload) -> EventPayload {
  case payload {
    LifecyclePayload(
      name: name,
      turn: turn,
      message: message,
      recovery: recovery,
      tokens: tokens,
    ) ->
      LifecyclePayload(
        name: name,
        turn: turn,
        message: compact_optional_text(message),
        recovery: recovery,
        tokens: tokens,
      )
    PiPayload(name: name, turn: turn, message: message, tokens: tokens) ->
      PiPayload(
        name: name,
        turn: turn,
        message: compact_optional_text(message),
        tokens: tokens,
      )
    AssistantMessagePayload(
      name: name,
      turn: turn,
      message: message,
      tokens: tokens,
    ) ->
      AssistantMessagePayload(
        name: name,
        turn: turn,
        message: compact_optional_text(message),
        tokens: tokens,
      )
    ToolPayload(
      name: name,
      turn: turn,
      message: message,
      request_id: request_id,
      method: method,
      tool_name: tool_name,
      tool_input: tool_input,
      tool_output: tool_output,
      tool_status: tool_status,
      tokens: tokens,
    ) ->
      ToolPayload(
        name: name,
        turn: turn,
        message: compact_optional_text(message),
        request_id: request_id,
        method: method,
        tool_name: tool_name,
        tool_input: compact_optional_text(tool_input),
        tool_output: compact_optional_text(tool_output),
        tool_status: tool_status,
        tokens: tokens,
      )
    UiRequestPayload(
      name: name,
      turn: turn,
      message: message,
      request_id: request_id,
      method: method,
      tokens: tokens,
    ) ->
      UiRequestPayload(
        name: name,
        turn: turn,
        message: compact_optional_text(message),
        request_id: request_id,
        method: method,
        tokens: tokens,
      )
    UiResponsePayload(
      name: name,
      turn: turn,
      message: message,
      request_id: request_id,
      method: method,
      tokens: tokens,
    ) ->
      UiResponsePayload(
        name: name,
        turn: turn,
        message: compact_optional_text(message),
        request_id: request_id,
        method: method,
        tokens: tokens,
      )
    TokenStatsPayload(..) | TurnPayload(..) -> payload
    ErrorPayload(
      name: name,
      turn: turn,
      message: message,
      tool_name: tool_name,
      tool_input: tool_input,
      tool_output: tool_output,
      tool_status: tool_status,
      tokens: tokens,
    ) ->
      ErrorPayload(
        name: name,
        turn: turn,
        message: compact_optional_text(message),
        tool_name: tool_name,
        tool_input: compact_optional_text(tool_input),
        tool_output: compact_optional_text(tool_output),
        tool_status: tool_status,
        tokens: tokens,
      )
    PiRawPayload(
      name: name,
      turn: turn,
      pi_type: pi_type,
      message: message,
      tokens: tokens,
      ..,
    ) ->
      PiRawPayload(
        name: name,
        turn: turn,
        pi_type: pi_type,
        message: compact_optional_text(message),
        tokens: tokens,
        raw_json: None,
      )
  }
}

fn compact_optional_text(value: Option(String)) -> Option(String) {
  case value {
    Some(value) -> Some(compact_text(value))
    None -> None
  }
}

const max_exited_event_text_chars = 4096

fn compact_text(value: String) -> String {
  case string.length(value) > max_exited_event_text_chars {
    True ->
      string.slice(value, 0, max_exited_event_text_chars)
      <> "… [truncated after session exit]"
    False -> value
  }
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
