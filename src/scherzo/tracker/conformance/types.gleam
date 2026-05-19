import gleam/option.{type Option}
import scherzo/task
import scherzo/tracker/conformance/profile

pub const schema_version = 1

pub const max_driver_timeout_ms = 60_000

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
  DriverConfig(
    transport: DriverTransport,
    command: DriverCommand,
    timeout_ms: Int,
  )
}

pub type DriverTransport {
  CliTransport
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
    adapter_operations: List(profile.AdapterOperation),
  )
}

pub type FixtureConfig {
  FixtureConfig(task_file: String)
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

pub type CaseResult {
  CaseResult(
    id: String,
    operation: String,
    status: CaseStatus,
    request_id: String,
    message: String,
    diagnostics: String,
  )
}

pub type HookResult {
  HookResult(
    phase: String,
    status: CaseStatus,
    message: String,
    diagnostics: String,
  )
}

pub type ProbeResult {
  ProbeResult(
    name: String,
    status: CaseStatus,
    message: String,
    diagnostics: String,
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
