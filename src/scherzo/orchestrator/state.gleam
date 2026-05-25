import birl.{type Time}
import gleam/dict.{type Dict}
import gleam/int
import gleam/option.{type Option, None}
import gleam/string
import scherzo/orchestrator/reason
import scherzo/session/live as session_live
import scherzo/session/tokens as session_tokens
import scherzo/task
import scherzo/tracker/issue as tracker_issue

pub fn task_ref_identity(ref: task.TaskRef) -> String {
  let #(backend_kind, remote_id) = task.identity(ref)
  encode_identity_component(backend_kind)
  <> "|"
  <> encode_identity_component(remote_id)
}

pub fn task_identity(item: task.Task) -> String {
  let task.Task(ref: ref, ..) = item
  task_ref_identity(ref)
}

pub fn issue_ref(issue: tracker_issue.Issue) -> task.TaskRef {
  task.from_legacy_issue(issue).ref
}

// Linear compatibility boundary: legacy orchestrator paths still receive a
// tracker_issue.Issue, so derive the TaskRef identity from the Linear-shaped
// issue at the edge instead of using the bare issue id as a runtime key.
pub fn issue_identity(issue: tracker_issue.Issue) -> String {
  task.from_legacy_issue(issue).ref |> task_ref_identity
}

// Linear compatibility boundary: timer, ledger, and operator continuations are
// still serialized with bare issue ids. Convert them before touching runtime
// dictionaries.
pub fn linear_issue_id_identity(issue_id: String) -> String {
  linear_issue_id_ref(issue_id) |> task_ref_identity
}

pub fn linear_issue_id_ref(issue_id: String) -> task.TaskRef {
  task.TaskRef(
    backend_kind: "linear",
    remote_id: issue_id,
    key: None,
    url: None,
  )
}

fn encode_identity_component(value: String) -> String {
  int.to_string(string.length(value)) <> ":" <> value
}

pub type RetryEntry {
  RetryEntry(
    task_ref: task.TaskRef,
    issue_id: String,
    delay_ms: Int,
    timer_generation: Int,
  )
}

pub type RunningEntry {
  RunningEntry(
    task: task.Task,
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
    task_ref: task.TaskRef,
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
