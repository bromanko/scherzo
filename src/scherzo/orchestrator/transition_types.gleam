import gleam/dict
import gleam/option.{type Option}
import scherzo/config/types as config_types
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/state as orchestrator_state
import scherzo/session/event as session_event
import scherzo/state/ledger
import scherzo/state/record
import scherzo/tracker/issue as tracker_issue

pub type State {
  State(
    runtime: orchestrator_state.RuntimeState,
    workers: WorkerDirectory,
    pending_claims: dict.Dict(String, PendingClaim),
    pending_dispatch_validations: dict.Dict(String, PendingDispatchValidation),
    next_dispatch_validation_generation: Int,
    next_session_sequence: Int,
  )
}

pub type Message {
  SnapshotRequested
  PollTick(generation: Int, poll: PollSnapshot)
  CandidateFetchStartRequested(generation: Int, context: DispatchContext)
  CandidateFetchCompleted(
    generation: Int,
    poll: PollSnapshot,
    result: Result(List(tracker_issue.Issue), String),
    context: DispatchContext,
  )
  LinearCommandPhaseFinished(
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
}

pub type Outcome {
  Outcome(state: State, effects: List(effects_types.Effect))
}

pub type WorkerDirectory {
  WorkerDirectory(
    by_issue: dict.Dict(String, WorkerEntry),
    by_session: dict.Dict(String, String),
    route_to_session: dict.Dict(String, String),
  )
}

pub type WorkerEntry {
  WorkerEntry(
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

pub type PendingClaim {
  PendingClaim(
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
  )
}
