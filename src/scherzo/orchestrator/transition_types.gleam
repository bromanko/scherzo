import gleam/dict
import gleam/option.{type Option, None}
import scherzo/agent/types as agent_types
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/control/linear_parser
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/state as orchestrator_state
import scherzo/review_lane_preflight
import scherzo/review_lane_preflight_policy
import scherzo/session/event as session_event
import scherzo/session/reason as session_reason
import scherzo/state/ledger
import scherzo/state/record
import scherzo/state/recovery
import scherzo/task
import scherzo/tracker/adapter
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_dag

pub type State {
  State(
    runtime: orchestrator_state.RuntimeState,
    workers: WorkerDirectory,
    pending_claims: dict.Dict(String, PendingClaim),
    pending_dispatch_validations: dict.Dict(String, PendingDispatchValidation),
    next_dispatch_validation_generation: Int,
    next_session_sequence: Int,
    pending_linear_command_acks: dict.Dict(String, PendingLinearCommandAck),
    in_flight_linear_command_acks: dict.Dict(String, Bool),
  )
}

pub type Message {
  SnapshotRequested
  StartupRecoveryApplied(
    retry_timers: List(recovery.RecoveredRetry),
    cleanup_workspaces: List(recovery.CleanupRequest),
    outbox_to_replay: List(recovery.OutboxReplay),
    park_reports: List(adapter.ParkReport),
    warnings: List(String),
    secrets: List(String),
  )
  PollTick(generation: Int, poll: PollSnapshot)
  CandidateFetchStartRequested(generation: Int, context: DispatchContext)
  RunningRefreshCompleted(
    generation: Int,
    poll: PollSnapshot,
    result: Result(List(tracker_issue.Issue), String),
    context: DispatchContext,
  )
  CandidateFetchCompleted(
    generation: Int,
    poll: PollSnapshot,
    result: Result(List(tracker_issue.Issue), String),
    context: DispatchContext,
  )
  RemoteCommandSubmitted(
    event: adapter.RemoteCommandEvent,
    parsed: linear_parser.ParsedLinearCommand,
    safe_excerpt: String,
  )
  RemoteCommandApplied(
    backend_kind: String,
    event_id: String,
    task_remote_id: String,
    command_name: String,
    result: command.CommandResult,
    message_excerpt: String,
    ack_body: Option(String),
  )
  RemoteCommandAckRequested(
    backend_kind: String,
    task_remote_id: String,
    event_id: String,
    body: String,
    outbox_recorded: Bool,
    outbox_kind: String,
  )
  RemoteCommandAckFinished(
    backend_kind: String,
    task_remote_id: String,
    event_id: String,
    outbox_kind: String,
    result: Result(Nil, String),
  )
  RetryPendingRemoteCommandAcks
  OperatorCommandSubmitted(
    request: effects_types.OperatorCommandRequest,
    context: DispatchContext,
    issue_resolution: OperatorIssueResolution,
    parked_issue_resolution: ParkedIssueResolution,
  )
  RemoteCommandPhaseFinished(
    candidates: List(tracker_issue.Issue),
    dispatch_after: Bool,
    context: DispatchContext,
  )
  DispatchCandidates(
    candidates: List(tracker_issue.Issue),
    context: DispatchContext,
  )
  DispatchValidationCompleted(
    issue_id: String,
    generation: Int,
    result: Result(tracker_issue.Issue, DispatchValidationError),
    context: DispatchContext,
  )
  HandoffClaimCompleted(
    issue_id: String,
    run_id: String,
    result: HandoffClaimResult,
  )
  RetryTick(issue_id: String, generation: Int, context: DispatchContext)
  RetryRefreshCompleted(
    issue_id: String,
    generation: Int,
    result: Result(List(tracker_issue.Issue), String),
    context: DispatchContext,
  )
  ClaimLedgerAppendRequested(
    correlation_id: String,
    issue_id: String,
    run_id: String,
    session_id: String,
    bodies: List(record.RecordBody),
    failure_event: String,
  )
  LedgerAppendCompleted(
    correlation_id: String,
    continuation: effects_types.LedgerContinuation,
    result: Result(Nil, ledger.LedgerError),
    now_ms: Int,
  )
  WorkerStartSucceeded(issue_id: String, run_id: String, session_id: String)
  WorkerStartFailed(
    issue_id: String,
    run_id: String,
    session_id: String,
    reason: String,
  )
  WorkerCommandReady(issue_id: String, run_id: String)
  WorkerFinished(
    issue_id: String,
    run_id: String,
    result: Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
    context: WorkerLifecycleContext,
  )
  WorkerDown(resolution: WorkerDownResolution, context: WorkerLifecycleContext)
  WorkerStopRequested(
    session_id: String,
    reason: session_reason.WorkerExitReason,
    context: WorkerLifecycleContext,
  )
  YamlStepStarted(session_id: String, run_id: String)
  YamlStepFinished(session_id: String)
  ShutdownRequested(stop_effect_runner: Bool)
}

pub type Outcome {
  Outcome(state: State, effects: List(effects_types.Effect))
}

pub type WorkerDirectory {
  WorkerDirectory(
    by_issue: dict.Dict(String, WorkerEntry),
    by_session: dict.Dict(String, String),
    route_to_session: dict.Dict(String, String),
    yaml_step_runs: dict.Dict(String, String),
    stopped_yaml_runs: dict.Dict(String, session_reason.WorkerExitReason),
  )
}

pub type WorkerEntry {
  WorkerEntry(
    task_ref: task.TaskRef,
    issue_id: String,
    run_id: String,
    session_id: String,
    issue: tracker_issue.Issue,
    workspace_path: String,
    workflow_id: String,
    command_route_id: String,
    status: WorkerStatus,
    recovery: Option(session_event.RecoveryInfo),
  )
}

pub type WorkerStatus {
  WorkerStarting
  WorkerRunning
  WorkerStopping(reason: String)
  WorkerFinishedStatus
}

pub type WorkerLifecycleContext {
  WorkerLifecycleContext(
    effective: config_types.EffectiveConfig,
    now_ms: Int,
    secrets: List(String),
  )
}

pub type WorkerFinishResult {
  WorkerSucceeded(success: agent_types.WorkerSuccess)
  WorkerFailed(failure: agent_types.WorkerFailure, kind: WorkerFailureKind)
}

pub type WorkerFailureKind {
  StandardWorkerFailure(reason_text: String)
  RecoveryResumeValidationFailure(reason_text: String)
  OperatorWorkerFailure(reason: session_reason.WorkerExitReason)
  WorkerDownFailure
}

pub type WorkerDownResolution {
  KnownWorkerDown(issue_id: String, run_id: String, session_id: String)
  WorkerDownStale(issue_id: String)
  UnknownWorkerDown
}

pub type PendingLinearCommandAck {
  PendingLinearCommandAck(
    backend_kind: String,
    task_remote_id: String,
    body: String,
    outbox_recorded: Bool,
    outbox_kind: String,
  )
}

pub type OperatorIssueResolution {
  OperatorIssueNotResolved
  OperatorIssueResolved(tracker_issue.Issue)
  OperatorIssueNotFound
  OperatorIssueRejected(reason: String)
  OperatorIssueNotAllowed(reason: String)
  OperatorIssueResolutionFailed
}

pub type ParkedIssueResolution {
  ParkedIssueNotResolved
  ParkedIssueResolved(issue_id: String)
  ParkedIssueNotFound
  ParkedIssueRejected(reason: String)
  ParkedIssueNotAllowed(reason: String)
  ParkedIssueResolutionFailed
}

pub type PendingClaim {
  PendingClaim(
    task_ref: task.TaskRef,
    issue_id: String,
    run_id: String,
    session_id: String,
    workspace_path: String,
    workflow_id: String,
    command_route_id: String,
    route_label: String,
    issue: tracker_issue.Issue,
    recovery: Option(session_event.RecoveryInfo),
    remaining_candidates: List(tracker_issue.Issue),
    dispatch_context: DispatchContext,
  )
}

pub type PendingDispatchValidation {
  PendingDispatchValidation(
    task_ref: task.TaskRef,
    issue: tracker_issue.Issue,
    remaining_candidates: List(tracker_issue.Issue),
    generation: Int,
    requested_at_ms: Int,
  )
}

pub type PollSnapshot {
  PollSnapshot(generation: Int, in_flight: Option(Int))
}

pub type DispatchContext {
  DispatchContext(
    effective: config_types.EffectiveConfig,
    routing: config_types.RoutingConfig,
    available_workflow_ids: List(String),
    dispatch_enabled: Bool,
    operator_paused: Bool,
    active_issue_ids: List(String),
    active_issues: List(tracker_issue.Issue),
    reserved_non_issue_slots: Int,
    workspace_root: String,
    now_ms: Int,
    recovery_by_issue: dict.Dict(String, session_event.RecoveryInfo),
    review_lane_preflight: ReviewLanePreflightContext,
  )
}

pub type ReviewLanePreflightContext {
  ReviewLanePreflightContext(
    config_dir: String,
    workflow_dags: dict.Dict(String, workflow_dag.WorkflowDag),
    policy: review_lane_preflight_policy.Policy,
    override: Option(review_lane_preflight.PreflightResult),
  )
}

pub fn dispatch_context(
  effective: config_types.EffectiveConfig,
  routing: config_types.RoutingConfig,
  workflow_dags: dict.Dict(String, workflow_dag.WorkflowDag),
  dispatch_enabled: Bool,
  operator_paused: Bool,
  active_issue_ids: List(String),
  active_issues: List(tracker_issue.Issue),
  reserved_non_issue_slots: Int,
  workspace_root: String,
  now_ms: Int,
  recovery_by_issue: dict.Dict(String, session_event.RecoveryInfo),
  config_dir: String,
) -> DispatchContext {
  DispatchContext(
    effective: effective,
    routing: routing,
    available_workflow_ids: dict.keys(workflow_dags),
    dispatch_enabled: dispatch_enabled,
    operator_paused: operator_paused,
    active_issue_ids: active_issue_ids,
    active_issues: active_issues,
    reserved_non_issue_slots: reserved_non_issue_slots,
    workspace_root: workspace_root,
    now_ms: now_ms,
    recovery_by_issue: recovery_by_issue,
    review_lane_preflight: review_lane_preflight_context(
      config_dir,
      workflow_dags,
    ),
  )
}

pub fn review_lane_preflight_context(
  config_dir: String,
  workflow_dags: dict.Dict(String, workflow_dag.WorkflowDag),
) -> ReviewLanePreflightContext {
  ReviewLanePreflightContext(
    config_dir: config_dir,
    workflow_dags: workflow_dags,
    policy: review_lane_preflight_policy.from_env(),
    override: None,
  )
}

pub type DispatchValidationError {
  DispatchValidationTrackerError(String)
  DispatchValidationMissingIssue
  DispatchValidationDuplicateIssue
  DispatchValidationIdMismatch(expected: String, actual: String)
}

pub type HandoffClaimResult {
  HandoffClaimSucceeded(bodies: List(record.RecordBody))
  HandoffClaimFailed(error: String)
  HandoffClaimStartRecordFailed(reason: String)
}

pub fn new_worker_directory() -> WorkerDirectory {
  WorkerDirectory(
    by_issue: dict.new(),
    by_session: dict.new(),
    route_to_session: dict.new(),
    yaml_step_runs: dict.new(),
    stopped_yaml_runs: dict.new(),
  )
}
