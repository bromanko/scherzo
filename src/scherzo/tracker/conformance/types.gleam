import gleam/option.{type Option}
import scherzo/task
import scherzo/tracker/conformance/profile

pub const schema_version = 1

pub const max_driver_timeout_ms = 60_000

pub const max_http_retry_attempts = 3

pub const max_http_retry_backoff_ms = 1000

pub const max_external_diagnostics_chars = 4096

pub type Manifest {
  Manifest(
    schema_version: Int,
    adapter_kind: String,
    driver: DriverConfig,
    profile: ProfileConfig,
    fixtures: FixtureConfig,
    probes: List(ProbeConfig),
    hooks: HooksConfig,
    report: ReportConfig,
  )
}

pub type ManifestError {
  ManifestError(code: String, message: String)
}

pub type DriverConfig {
  CliDriverConfig(command: DriverCommand, timeout_ms: Int)
  HttpDriverConfig(endpoint: HttpEndpointConfig, timeout_ms: Int)
}

pub type DriverTransport {
  CliTransport
  HttpTransport
}

pub type HttpEndpointConfig {
  HttpEndpointConfig(
    url: String,
    headers: List(HttpHeaderConfig),
    retry: HttpRetryConfig,
  )
}

pub type HttpHeaderConfig {
  HttpHeaderConfig(name: String, value_from_env: String, value_prefix: String)
}

pub type HttpRetryConfig {
  HttpRetryConfig(max_attempts: Int, backoff_ms: Int)
}

pub type DriverCommand {
  DriverCommand(
    executable: String,
    args: List(String),
    cwd: String,
    env: List(EnvVar),
  )
}

pub type EnvVar {
  EnvVar(name: String, value: String)
}

pub type ProfileConfig {
  ProfileConfig(
    name: profile.ProfileName,
    capabilities: List(profile.Capability),
    requested_packs: List(profile.PackName),
    adapter_operations: List(profile.AdapterOperation),
  )
}

pub type FixtureTaskDeclaration {
  FixtureTaskDeclaration(
    name: String,
    ref: task.TaskRef,
    operator_refs: List(String),
    purpose: String,
  )
}

pub type FixtureConfig {
  FixtureConfig(task_file: String, tasks: List(FixtureTaskDeclaration))
}

pub type ProbeConfig {
  ProbeConfig(name: String, command: HookCommand)
}

pub type HooksConfig {
  HooksConfig(setup: Option(HookCommand), cleanup: Option(HookCommand))
}

pub type HookCommand {
  HookCommand(executable: String, args: List(String), cwd: String)
}

pub type ReportConfig {
  ReportConfig(redact: List(String))
}

pub type DriverRequest {
  DriverRequest(
    schema_version: Int,
    request_id: String,
    operation: profile.AdapterOperation,
    payload: RequestPayload,
  )
}

pub type RequestPayload {
  FetchCandidatesPayload(task_search: TaskSearchPayload)
  RefreshByRefsPayload(refs: List(task.TaskRef))
  LookupByOperatorRefPayload(operator_ref: String)
  CommentsPostOrUpdatePayload(comment: CommentRequestPayload)
  StateTransitionPayload(transition: StateTransitionRequestPayload)
}

pub type TaskSearchPayload {
  TaskSearchPayload(
    active_states: List(String),
    dispatch_states: List(String),
    terminal_states: List(String),
    workflow_labels: List(String),
    limit: Int,
  )
}

pub type CommentRequestPayload {
  CommentRequestPayload(
    task: task.TaskRef,
    body: String,
    mode: CommentWriteMode,
  )
}

pub type CommentWriteMode {
  CreateOnlyComment
  UpdateExistingComment(comment_id: String, allow_create_fallback: Bool)
}

pub type StateTransitionRequestPayload {
  StateTransitionRequestPayload(
    task: task.TaskRef,
    target_state_id: Option(String),
    target_state_name: String,
    reason: String,
  )
}

pub type DriverResponse {
  DriverResponseSuccess(
    schema_version: Int,
    request_id: String,
    result: ResponseResult,
  )
  DriverResponseError(
    schema_version: Int,
    request_id: String,
    error: DriverError,
  )
}

pub type ResponseResult {
  TaskListResult(tasks: List(task.Task))
  OptionalTaskResult(task: Option(task.Task))
  CommentResult(comment: CommentReceiptPayload)
  StateTransitionResult(transition: StateTransitionReceiptPayload)
}

pub type CommentReceiptPayload {
  CommentReceiptPayload(
    id: String,
    task: task.TaskRef,
    url: Option(String),
    created: Bool,
  )
}

pub type StateTransitionReceiptPayload {
  StateTransitionReceiptPayload(task: task.TaskRef, state: task.TaskState)
}

pub type DriverError {
  DriverError(
    kind: DriverErrorKind,
    message: String,
    ref: Option(task.TaskRef),
    capability: Option(String),
  )
}

pub type DriverErrorKind {
  UnauthorizedError
  NotFoundError
  TransientError
  PermanentError
  UnsupportedCapabilityError
  DecodeFailedError
}

pub type CaseStatus {
  PassedStatus
  FailedStatus
  SkippedStatus
  SetupFailedStatus
  ProbeFailedStatus
  CleanupFailedStatus
}

pub type TranscriptEvidence {
  TranscriptEvidence(body: String, truncated: Bool, original_chars: Int)
}

pub type CaseResult {
  CaseResult(
    id: String,
    operation: String,
    status: CaseStatus,
    request_id: String,
    message: String,
    diagnostics: String,
    expected_summary: String,
    actual_summary: String,
    request_transcript: TranscriptEvidence,
    response_transcript: Option(TranscriptEvidence),
    recovery_guidance: String,
  )
}

pub type HookResult {
  HookResult(
    phase: String,
    status: CaseStatus,
    message: String,
    diagnostics: String,
    recovery_guidance: String,
  )
}

pub type ProbeResult {
  ProbeResult(
    name: String,
    status: CaseStatus,
    message: String,
    diagnostics: String,
    recovery_guidance: String,
  )
}

pub type Report {
  Report(
    schema_version: Int,
    adapter_kind: String,
    profile: String,
    passed: Int,
    failed: Int,
    skipped: Int,
    setup_failed: Int,
    probe_failed: Int,
    cleanup_failed: Int,
    case_results: List(CaseResult),
    hook_results: List(HookResult),
    probe_results: List(ProbeResult),
  )
}

pub type RunResult {
  RunResult(report: Report, summary: String, exit_code: Int)
}
