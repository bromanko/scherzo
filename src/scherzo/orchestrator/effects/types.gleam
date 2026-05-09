import gleam/option.{type Option}
import scherzo/log
import scherzo/orchestrator/reason
import scherzo/orchestrator/state as orchestrator_state
import scherzo/session/event as session_event
import scherzo/state/record
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
  FetchLinearCommands(
    generation: Int,
    issue_ids: List(String),
    candidates: List(tracker_issue.Issue),
    dispatch_after: Bool,
  )
  BeginDispatchValidation(issue_id: String, generation: Int)
  ReserveSessionSequence(sequence: Int)
  ClaimIssue(issue: tracker_issue.Issue, workspace_path: String, run_id: String)
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
  CancelRetryTimer(issue_id: String, generation: Int, cancel_reason: String)
  ReleaseClaim(issue_id: String)
  ClearRecovery(issue_id: String)
}

pub type LedgerAppend {
  LedgerAppend(
    correlation_id: String,
    bodies: List(record.RecordBody),
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
  SpawnClaimedWorker(issue_id: String, run_id: String, session_id: String)
}

pub type WorkerStart {
  WorkerStart(
    issue_id: String,
    run_id: String,
    session_id: String,
    command_route_id: String,
    issue: tracker_issue.Issue,
    workspace_path: String,
    workflow_id: String,
    route_label: String,
    recovery: Option(session_event.RecoveryInfo),
  )
}
