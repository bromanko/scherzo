import birl.{type Time}
import gleam/dict.{type Dict}
import gleam/option.{type Option}
import scherzo/orchestrator/reason
import scherzo/session/live as session_live
import scherzo/session/tokens as session_tokens
import scherzo/tracker/issue as tracker_issue

pub type RetryEntry {
  RetryEntry(issue_id: String, delay_ms: Int, timer_generation: Int)
}

pub type RunningEntry {
  RunningEntry(
    issue: tracker_issue.Issue,
    workspace_path: String,
    session: Option(session_live.LiveSession),
  )
}

pub type IssueCounter {
  IssueCounter(failure_attempts: Int, worker_sessions: Int)
}

pub fn new_issue_counter() -> IssueCounter {
  IssueCounter(failure_attempts: 0, worker_sessions: 0)
}

pub type ParkReleasePolicy {
  ExplicitUnparkOnly
  AutoUnparkOnIssueChange(issue_fingerprint: String)
}

pub fn park_release_policy_from_string(
  release_policy: String,
  issue_fingerprint: String,
) -> ParkReleasePolicy {
  case release_policy {
    "auto_unpark_on_issue_change" -> AutoUnparkOnIssueChange(issue_fingerprint)
    _ -> ExplicitUnparkOnly
  }
}

pub type ParkedEntry {
  ParkedEntry(
    issue_id: String,
    identifier: String,
    reason: reason.ParkReason,
    release_policy: ParkReleasePolicy,
    parked_at_ms: Int,
  )
}

pub type InvalidWorkflowReport {
  InvalidWorkflowReport(
    issue_id: String,
    identifier: String,
    violation_code: String,
    violation_fingerprint: String,
    reporting_policy_fingerprint: String,
    observed_updated_at: Option(Time),
    observed_labels_fingerprint: String,
    attempted_at_ms: Int,
    last_result: String,
  )
}

pub type BlockedDependencyReport {
  BlockedDependencyReport(
    issue_id: String,
    identifier: String,
    phase: String,
    blocker_fingerprint: String,
    observed_updated_at: Option(Time),
    terminal_state_policy_fingerprint: String,
    attempted_at_ms: Int,
    last_result: String,
  )
}

pub type RuntimeState {
  RuntimeState(
    poll_interval_ms: Int,
    max_concurrent_agents: Int,
    running: Dict(String, RunningEntry),
    claimed: Dict(String, String),
    retry_attempts: Dict(String, RetryEntry),
    issue_counters: Dict(String, IssueCounter),
    parked: Dict(String, ParkedEntry),
    invalid_workflow_reports: Dict(String, InvalidWorkflowReport),
    blocked_dependency_reports: Dict(String, BlockedDependencyReport),
    completed: Dict(String, tracker_issue.Issue),
    aggregate_pi_totals: session_tokens.TokenTotals,
    latest_rate_limit_payload: Option(String),
  )
}
