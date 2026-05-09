import gleam/int

pub type ConfigError {
  UnsupportedTrackerKind(String)
  MissingTrackerApiKey
  MissingTrackerProjectSlug
  InvalidConfig(String)
  InvalidScheduledJobOverlap(String)
  ScheduledJobCatchUpUnsupported(String)
  ScheduledJobUnsupportedInputs(String)
  DispatchValidationFailed(String)
}

pub type TemplateError {
  TemplateRenderError(String)
}

pub type WorkspaceError {
  UnsafeWorkspaceKey(String)
  WorkspaceOutsideRoot(String)
  WorkspaceCollision(String)
  WorkspaceIo(String)
  PartialWorkspace(String)
}

pub type HookError {
  HookFailed(String, Int, String)
  HookTimedOut(String)
  HookIo(String)
}

pub type TrackerError {
  LinearApiRequest(String)
  LinearApiStatus(Int)
  LinearGraphqlErrors(String)
  LinearUnknownPayload(String)
  LinearMissingEndCursor
  LinearUploadStatus(Int)
  LinearAttachmentError(String)
}

pub type PiRpcError {
  PiLaunchFailed(String)
  PiMalformedJson(String)
  PiReadTimeout
  PiTurnTimeout
  PiStallTimeout
  PiExited(Int)
  PiProtocolError(String)
}

pub type AgentRunnerError {
  PromptFailed(TemplateError)
  WorkspaceFailed(WorkspaceError)
  HookFailedError(HookError)
  WorkflowHookFailed(HookError)
  ProbeFailed(PiRpcError)
  PiFailed(PiRpcError)
  WorkflowCommandFailed(code: String, step_id: String, detail: String)
  StateRefreshFailed(TrackerError)
  OperatorAbort
  OperatorStopAfterCurrentTurn
}

pub type OrchestratorError {
  NoSlots
  RetryPollFailed
  InvalidWorkflow
  StartupFailed(String)
}

pub type SubprocessError {
  SubprocessStartFailed(String)
  SubprocessReadTimeout
  SubprocessLineTooLong
  SubprocessExited(Int)
}

pub type ScherzoError {
  Config(ConfigError)
  Template(TemplateError)
  Workspace(WorkspaceError)
  Hook(HookError)
  Tracker(TrackerError)
  PiRpc(PiRpcError)
  Agent(AgentRunnerError)
  Orchestrator(OrchestratorError)
  Subprocess(SubprocessError)
}

pub fn code(error: ScherzoError) -> String {
  case error {
    Config(error) -> config_code(error)
    Template(error) -> template_code(error)
    Workspace(error) -> workspace_code(error)
    Hook(error) -> hook_code(error)
    Tracker(error) -> tracker_code(error)
    PiRpc(error) -> pi_rpc_code(error)
    Agent(error) -> agent_code(error)
    Orchestrator(error) -> orchestrator_code(error)
    Subprocess(error) -> subprocess_code(error)
  }
}

pub fn config_code(error: ConfigError) -> String {
  case error {
    UnsupportedTrackerKind(_) -> "unsupported_tracker_kind"
    MissingTrackerApiKey -> "missing_tracker_api_key"
    MissingTrackerProjectSlug -> "missing_tracker_project_slug"
    InvalidConfig(_) -> "invalid_config"
    InvalidScheduledJobOverlap(_) -> "invalid_scheduled_job_overlap"
    ScheduledJobCatchUpUnsupported(_) -> "scheduled_job_catch_up_unsupported"
    ScheduledJobUnsupportedInputs(_) -> "scheduled_job_unsupported_inputs"
    DispatchValidationFailed(_) -> "dispatch_validation_failed"
  }
}

pub fn config_message(error: ConfigError) -> String {
  case error {
    UnsupportedTrackerKind(kind) -> "unsupported tracker kind: " <> kind
    MissingTrackerApiKey -> "tracker.api_key or LINEAR_API_KEY is required"
    MissingTrackerProjectSlug -> "tracker.project_slug is required"
    InvalidConfig(message) -> message
    InvalidScheduledJobOverlap(message) -> message
    ScheduledJobCatchUpUnsupported(message) -> message
    ScheduledJobUnsupportedInputs(message) -> message
    DispatchValidationFailed(message) -> message
  }
}

pub fn template_code(error: TemplateError) -> String {
  case error {
    TemplateRenderError(_) -> "template_render_error"
  }
}

pub fn workspace_code(error: WorkspaceError) -> String {
  case error {
    UnsafeWorkspaceKey(_) -> "unsafe_workspace_key"
    WorkspaceOutsideRoot(_) -> "workspace_outside_root"
    WorkspaceCollision(_) -> "workspace_collision"
    WorkspaceIo(_) -> "workspace_io"
    PartialWorkspace(_) -> "partial_workspace"
  }
}

pub fn hook_code(error: HookError) -> String {
  case error {
    HookFailed(_, _, _) -> "hook_failed"
    HookTimedOut(_) -> "hook_timed_out"
    HookIo(_) -> "hook_io"
  }
}

pub fn tracker_code(error: TrackerError) -> String {
  case error {
    LinearApiRequest(_) -> "linear_api_request"
    LinearApiStatus(_) -> "linear_api_status"
    LinearGraphqlErrors(_) -> "linear_graphql_errors"
    LinearUnknownPayload(_) -> "linear_unknown_payload"
    LinearMissingEndCursor -> "linear_missing_end_cursor"
    LinearUploadStatus(_) -> "linear_upload_status"
    LinearAttachmentError(_) -> "linear_attachment_error"
  }
}

pub fn pi_rpc_code(error: PiRpcError) -> String {
  case error {
    PiLaunchFailed(_) -> "pi_launch_failed"
    PiMalformedJson(_) -> "pi_malformed_json"
    PiReadTimeout -> "pi_read_timeout"
    PiTurnTimeout -> "pi_turn_timeout"
    PiStallTimeout -> "pi_stall_timeout"
    PiExited(_) -> "pi_exited"
    PiProtocolError(_) -> "pi_protocol_error"
  }
}

pub fn agent_code(error: AgentRunnerError) -> String {
  case error {
    PromptFailed(_) -> "agent_prompt_failed"
    WorkspaceFailed(_) -> "agent_workspace_failed"
    HookFailedError(_) -> "agent_hook_failed"
    WorkflowHookFailed(_) -> "workflow_hook_failed"
    ProbeFailed(_) -> "agent_probe_failed"
    PiFailed(_) -> "agent_pi_failed"
    WorkflowCommandFailed(code: code, ..) -> code
    StateRefreshFailed(_) -> "agent_state_refresh_failed"
    OperatorAbort -> "agent_operator_abort"
    OperatorStopAfterCurrentTurn -> "agent_operator_stop_after_current_turn"
  }
}

pub fn agent_artifact_detail(error: AgentRunnerError) -> String {
  "agent step failed:" <> agent_code(error) <> agent_detail_suffix(error)
}

pub fn agent_detail_suffix(error: AgentRunnerError) -> String {
  case error {
    PromptFailed(TemplateRenderError(message)) ->
      "\ntemplate render error: " <> message
    ProbeFailed(pi_error) -> "\n" <> pi_rpc_detail(pi_error)
    PiFailed(pi_error) -> "\n" <> pi_rpc_detail(pi_error)
    WorkflowCommandFailed(detail: detail, ..) -> "\n" <> detail
    StateRefreshFailed(tracker_error) ->
      "\ntracker refresh error: " <> tracker_code(tracker_error)
    OperatorAbort -> "\noperator requested abort"
    OperatorStopAfterCurrentTurn ->
      "\noperator requested stop after current turn"
    WorkspaceFailed(_) | HookFailedError(_) | WorkflowHookFailed(_) -> ""
  }
}

pub fn pi_rpc_detail(error: PiRpcError) -> String {
  case error {
    PiLaunchFailed(message) -> "pi launch failed: " <> message
    PiMalformedJson(line) -> "pi emitted malformed JSON: " <> line
    PiReadTimeout -> "timed out waiting for pi RPC response"
    PiTurnTimeout -> "pi turn timeout elapsed before agent_end"
    PiStallTimeout -> "pi stall timeout elapsed without output"
    PiExited(status) ->
      "pi process exited with status " <> int.to_string(status)
    PiProtocolError(message) -> "pi protocol error: " <> message
  }
}

pub fn orchestrator_code(error: OrchestratorError) -> String {
  case error {
    NoSlots -> "no_slots"
    RetryPollFailed -> "retry_poll_failed"
    InvalidWorkflow -> "invalid_workflow"
    StartupFailed(_) -> "startup_failed"
  }
}

pub fn subprocess_code(error: SubprocessError) -> String {
  case error {
    SubprocessStartFailed(_) -> "subprocess_start_failed"
    SubprocessReadTimeout -> "subprocess_read_timeout"
    SubprocessLineTooLong -> "subprocess_line_too_long"
    SubprocessExited(_) -> "subprocess_exited"
  }
}
