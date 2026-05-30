import gleam/option.{type Option}
import scherzo/agent/types as agent_types
import scherzo/control/command
import scherzo/log
import scherzo/orchestrator/identity
import scherzo/orchestrator/reason
import scherzo/orchestrator/state as orchestrator_state
import scherzo/session/event as session_event
import scherzo/session/reason as session_reason
import scherzo/session/tokens as session_tokens
import scherzo/state/ledger_batch
import scherzo/task
import scherzo/tracker/adapter
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_policy

pub type Effect {
  Log(level: String, event: String, fields: List(log.Field))
  AppendLedger(request: LedgerAppend)
  StartWorker(request: WorkerStart)
  ReplySnapshot(orchestrator_state.RuntimeState)
  MarkPollInFlight(generation: Int)
  ScheduleNextPoll
  FetchCandidates(generation: Int)
  BeginDispatchValidation(issue_id: String, generation: Int)
  ReserveSessionSequence(sequence: Int)
  ClaimIssue(
    task_ref: task.TaskRef,
    issue: tracker_issue.Issue,
    workspace_path: String,
    run_id: String,
  )
  ReportInvalidWorkflow(
    issue: tracker_issue.Issue,
    violation: workflow_policy.IssueWorkflowViolation,
    violation_fingerprint: String,
    reporting_policy_fingerprint: String,
  )
  RemoveRetryTimer(issue_id: String)
  FinishRetryRefresh(issue_id: String)
  DeferRetryTimer(issue_id: String, generation: Int, delay_ms: Int)
  BeginRetryRefresh(issue_id: String, generation: Int)
  ScheduleRetryTimer(
    issue_id: String,
    delay_ms: Int,
    generation: Int,
    retry_reason: reason.RetryReason,
  )
  ScheduleRecoveredRetryTimer(issue_id: String, delay_ms: Int, generation: Int)
  CancelRetryTimer(issue_id: String, generation: Int, cancel_reason: String)
  ReleaseClaim(issue_id: String)
  ClearRecovery(issue_id: String)
  WorkerStartFailed(request: WorkerStart, reason: String)
  RemoveWorker(identity: WorkerIdentity, demonitor: Bool)
  PublishWorkerExited(request: WorkerExitPublication)
  ReportWorkerSuccess(
    identity: WorkerIdentity,
    success: agent_types.WorkerSuccess,
  )
  ReportWorkerFailure(
    identity: WorkerIdentity,
    failure: agent_types.WorkerFailure,
  )
  CleanupWorkspace(workspace_path: String)
  ParkIssue(
    parked: orchestrator_state.ParkedEntry,
    source_run_id: Option(String),
  )
  ReportPark(report: adapter.ParkReport)
  StopWorker(identity: WorkerIdentity, reason: session_reason.WorkerExitReason)
  StopWorkerAfterIssueRefresh(
    identity: WorkerIdentity,
    reason: reason.StopReason,
  )
  RegisterYamlStepStarted(
    session_id: identity.SessionId,
    run_id: identity.RunId,
  )
  FinishYamlStepRoute(session_id: identity.SessionId)
  FinishYamlStepSession(
    session_id: identity.SessionId,
    reason: session_reason.WorkerExitReason,
  )
  FinishYamlStepSessionsForRun(
    run_id: identity.RunId,
    reason: session_reason.WorkerExitReason,
  )
  ClearYamlStepRoutesForRun(run_id: identity.RunId)
  MarkYamlRunStopping(
    run_id: identity.RunId,
    reason: session_reason.WorkerExitReason,
  )
  ShutdownRuntime(stop_effect_runner: Bool)
  SetOperatorPaused(paused: Bool)
  ApplyOperatorCommand(request: OperatorCommandRequest)
  FinishOperatorCommand(
    request: OperatorCommandRequest,
    result: command.CommandResult,
  )
  ReportParkEffect(
    issue_id: String,
    issue_identifier: String,
    reason: String,
    release_policy: String,
    source_run_id: Option(String),
  )
}

pub type LedgerAppend {
  LedgerAppend(
    correlation_id: String,
    batch: ledger_batch.LedgerBatch,
    failure_event: String,
    policy: LedgerPolicy,
  )
}

pub type LedgerPolicy {
  ContinueRegardless
  StopBatchOnFailure
  ContinueWith(continuation: LedgerContinuation)
}

pub type LedgerContinuation {
  NoLedgerContinuation
  SpawnClaimedWorker(
    task_identity: identity.TaskIdentity,
    issue_id: identity.IssueId,
    run_id: identity.RunId,
    session_id: identity.SessionId,
  )
  ReportParkAfterLedger(
    issue_id: String,
    issue_identifier: String,
    reason: String,
    release_policy: String,
    source_run_id: Option(String),
  )
}

pub type OperatorCommandSource {
  LocalOperatorCommand
}

pub type OperatorCommandRequest {
  OperatorCommandRequest(
    source: OperatorCommandSource,
    operator_command: command.OperatorCommand,
    timeout_ms: Int,
  )
}

pub type WorkerStart {
  WorkerStart(
    task_ref: task.TaskRef,
    issue_id: identity.IssueId,
    run_id: identity.RunId,
    session_id: identity.SessionId,
    command_route_id: String,
    issue: tracker_issue.Issue,
    workspace_path: String,
    workflow_id: String,
    route_label: String,
    recovery: Option(session_event.RecoveryInfo),
  )
}

pub type WorkerIdentity {
  WorkerIdentity(
    task_ref: task.TaskRef,
    issue_id: identity.IssueId,
    run_id: identity.RunId,
    session_id: identity.SessionId,
    issue: tracker_issue.Issue,
    workspace_path: String,
    workflow_id: String,
    command_route_id: String,
  )
}

pub type WorkerExitPublication {
  WorkerExitPublication(
    identity: WorkerIdentity,
    reason_text: String,
    exit_reason: session_reason.WorkerExitReason,
    tokens: session_tokens.TokenTotals,
    update_tokens: Bool,
  )
}
