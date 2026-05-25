import gleam/option.{type Option, Some}

pub type PiEvent {
  ProbeStarted
  ProbeFinished
  PiSessionStarted
  TurnFinished
  MessageStart
  MessageUpdate
  MessageEnd
  ToolExecutionStart
  ToolExecutionUpdate
  ToolExecutionEnd
  Message
  ExtensionUiRequest
  ExtensionUiResponse
  AgentStart
  TurnStart
  TurnEnd
  AgentEnd
  AutoRetryStart
  AutoRetryEnd
  OperatorPromptSent
  OperatorPromptQueued
  OperatorPromptDropped
  OperatorUiTimeout
  PiAbortSent
  PiAbortFailed
  PiTerminateFailed
  AfterRunHookFailed
  ContextRecoveryStarted
  ContextRecoverySucceeded
  ContextRecoveryFailed
  ContextRecoveryArtifactWriteFailed
  UnknownPiEvent(name: String)
}

pub fn to_string(event: PiEvent) -> String {
  case event {
    ProbeStarted -> "probe_started"
    ProbeFinished -> "probe_finished"
    PiSessionStarted -> "pi_session_started"
    TurnFinished -> "turn_finished"
    MessageStart -> "message_start"
    MessageUpdate -> "message_update"
    MessageEnd -> "message_end"
    ToolExecutionStart -> "tool_execution_start"
    ToolExecutionUpdate -> "tool_execution_update"
    ToolExecutionEnd -> "tool_execution_end"
    Message -> "message"
    ExtensionUiRequest -> "extension_ui_request"
    ExtensionUiResponse -> "extension_ui_response"
    AgentStart -> "agent_start"
    TurnStart -> "turn_start"
    TurnEnd -> "turn_end"
    AgentEnd -> "agent_end"
    AutoRetryStart -> "auto_retry_start"
    AutoRetryEnd -> "auto_retry_end"
    OperatorPromptSent -> "operator_prompt_sent"
    OperatorPromptQueued -> "operator_prompt_queued"
    OperatorPromptDropped -> "operator_prompt_dropped"
    OperatorUiTimeout -> "operator_ui_timeout"
    PiAbortSent -> "pi_abort_sent"
    PiAbortFailed -> "pi_abort_failed"
    PiTerminateFailed -> "pi_terminate_failed"
    AfterRunHookFailed -> "after_run_hook_failed"
    ContextRecoveryStarted -> "context_recovery_started"
    ContextRecoverySucceeded -> "context_recovery_succeeded"
    ContextRecoveryFailed -> "context_recovery_failed"
    ContextRecoveryArtifactWriteFailed ->
      "context_recovery_artifact_write_failed"
    UnknownPiEvent(name) -> name
  }
}

pub fn from_string(name: String) -> PiEvent {
  case name {
    "probe_started" -> ProbeStarted
    "probe_finished" -> ProbeFinished
    "pi_session_started" -> PiSessionStarted
    "turn_finished" -> TurnFinished
    "message_start" -> MessageStart
    "message_update" -> MessageUpdate
    "message_end" -> MessageEnd
    "tool_execution_start" -> ToolExecutionStart
    "tool_execution_update" -> ToolExecutionUpdate
    "tool_execution_end" -> ToolExecutionEnd
    "message" -> Message
    "extension_ui_request" -> ExtensionUiRequest
    "extension_ui_response" -> ExtensionUiResponse
    "agent_start" -> AgentStart
    "turn_start" -> TurnStart
    "turn_end" -> TurnEnd
    "agent_end" -> AgentEnd
    "auto_retry_start" -> AutoRetryStart
    "auto_retry_end" -> AutoRetryEnd
    "operator_prompt_sent" -> OperatorPromptSent
    "operator_prompt_queued" -> OperatorPromptQueued
    "operator_prompt_dropped" -> OperatorPromptDropped
    "operator_ui_timeout" -> OperatorUiTimeout
    "pi_abort_sent" -> PiAbortSent
    "pi_abort_failed" -> PiAbortFailed
    "pi_terminate_failed" -> PiTerminateFailed
    "after_run_hook_failed" -> AfterRunHookFailed
    "context_recovery_started" -> ContextRecoveryStarted
    "context_recovery_succeeded" -> ContextRecoverySucceeded
    "context_recovery_failed" -> ContextRecoveryFailed
    "context_recovery_artifact_write_failed" ->
      ContextRecoveryArtifactWriteFailed
    _ -> UnknownPiEvent(name)
  }
}

pub fn is_message_update(event: PiEvent) -> Bool {
  event == MessageUpdate
}

pub fn is_token_stats(event: PiEvent) -> Bool {
  event == TurnFinished
}

pub fn is_blocking_ui_request(event: PiEvent, method: Option(String)) -> Bool {
  case event, method {
    ExtensionUiRequest, Some("select")
    | ExtensionUiRequest, Some("confirm")
    | ExtensionUiRequest, Some("input")
    | ExtensionUiRequest, Some("editor")
    -> True
    _, _ -> False
  }
}
