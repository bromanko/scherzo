import birl.{type Time}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None}
import gleam/order.{type Order, Eq}
import gleam/string
import scherzo/config/types as config_types
import scherzo/runtime/identity
import scherzo/runtime/reason
import scherzo/session/live as session_live
import scherzo/session/tokens as session_tokens
import scherzo/task
import scherzo/tracker/issue as tracker_issue

pub type TaskIdentity =
  identity.TaskIdentity

pub fn task_identity_to_string(task_identity: TaskIdentity) -> String {
  identity.to_string(task_identity)
}

pub fn new(config: config_types.EffectiveConfig) -> RuntimeState {
  RuntimeState(
    poll_interval_ms: config.polling.interval_ms,
    max_concurrent_agents: config.agent.max_concurrent_agents,
    running: dict.new(),
    claimed: dict.new(),
    retry_attempts: dict.new(),
    issue_counters: dict.new(),
    parked: dict.new(),
    invalid_workflow_reports: dict.new(),
    blocked_dependency_reports: dict.new(),
    completed: dict.new(),
    completed_at_ms: dict.new(),
    aggregate_pi_totals: session_tokens.zero_token_totals(),
    latest_rate_limit_payload: None,
  )
}

pub fn task_ref_identity(ref: task.TaskRef) -> identity.TaskIdentity {
  identity.task_ref(ref)
}

pub fn task_identity(item: task.Task) -> identity.TaskIdentity {
  identity.task(item)
}

pub fn issue_ref(issue: tracker_issue.Issue) -> task.TaskRef {
  identity.issue_ref(issue)
}

pub fn issue_ref_for_backend(
  issue: tracker_issue.Issue,
  backend_kind: String,
) -> task.TaskRef {
  identity.issue_ref_for_backend(issue, backend_kind)
}

// Linear compatibility boundary: legacy runtime paths still receive a
// tracker_issue.Issue, so derive the TaskRef identity from the Linear-shaped
// issue at the edge instead of using the bare issue id as a runtime key.
pub fn issue_identity(issue: tracker_issue.Issue) -> identity.TaskIdentity {
  identity.issue(issue)
}

pub fn issue_identity_for_backend(
  issue: tracker_issue.Issue,
  backend_kind: String,
) -> identity.TaskIdentity {
  identity.issue_for_backend(issue, backend_kind)
}

// Linear compatibility boundary: timer, ledger, and operator continuations are
// still serialized with bare issue ids. Convert them before touching runtime
// dictionaries.
pub fn linear_issue_id_identity(issue_id: String) -> identity.TaskIdentity {
  identity.linear_issue_id(issue_id)
}

pub fn issue_id_identity_for_backend(
  issue_id: String,
  backend_kind: String,
) -> identity.TaskIdentity {
  identity.issue_id_for_backend(issue_id, backend_kind)
}

pub fn linear_issue_id_ref(issue_id: String) -> task.TaskRef {
  identity.linear_issue_id_ref(issue_id)
}

pub fn issue_id_ref_for_backend(
  issue_id: String,
  backend_kind: String,
) -> task.TaskRef {
  identity.issue_id_ref_for_backend(issue_id, backend_kind)
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

const completed_cache_limit = 1024

pub fn cache_completed_task(
  state: RuntimeState,
  task_identity: identity.TaskIdentity,
  issue: tracker_issue.Issue,
  completed_at_ms: Int,
) -> RuntimeState {
  RuntimeState(
    ..state,
    completed: dict.insert(state.completed, task_identity, issue),
    completed_at_ms: dict.insert(
      state.completed_at_ms,
      task_identity,
      completed_at_ms,
    ),
  )
  |> trim_completed_cache
}

fn trim_completed_cache(state: RuntimeState) -> RuntimeState {
  case dict.size(state.completed) <= completed_cache_limit {
    True -> state
    False -> {
      let kept_entries =
        state.completed
        |> dict.to_list
        |> list.sort(by: fn(a, b) {
          compare_completed_entries(a, b, state.completed_at_ms)
        })
        |> list.take(completed_cache_limit)
      let kept_completed_at_ms =
        kept_entries
        |> list.map(fn(entry) {
          let #(task_identity, _) = entry
          #(
            task_identity,
            completed_at_for_identity(state.completed_at_ms, task_identity),
          )
        })
        |> dict.from_list
      RuntimeState(
        ..state,
        completed: dict.from_list(kept_entries),
        completed_at_ms: kept_completed_at_ms,
      )
    }
  }
}

fn compare_completed_entries(
  a: #(identity.TaskIdentity, tracker_issue.Issue),
  b: #(identity.TaskIdentity, tracker_issue.Issue),
  completed_at_ms: Dict(identity.TaskIdentity, Int),
) -> Order {
  let #(a_id, _) = a
  let #(b_id, _) = b
  case
    int.compare(
      completed_at_for_identity(completed_at_ms, b_id),
      completed_at_for_identity(completed_at_ms, a_id),
    )
  {
    Eq -> string.compare(identity.to_string(a_id), identity.to_string(b_id))
    order -> order
  }
}

fn completed_at_for_identity(
  completed_at_ms: Dict(identity.TaskIdentity, Int),
  task_identity: identity.TaskIdentity,
) -> Int {
  case dict.get(completed_at_ms, task_identity) {
    Ok(at_ms) -> at_ms
    Error(Nil) -> 0
  }
}

pub fn release_task_claim(
  state: RuntimeState,
  ref: task.TaskRef,
) -> RuntimeState {
  let task_identity = task_ref_identity(ref)
  RuntimeState(
    ..state,
    claimed: dict.delete(state.claimed, task_identity),
    retry_attempts: dict.delete(state.retry_attempts, task_identity),
  )
}

pub fn release_successful_task_claim(
  state: RuntimeState,
  ref: task.TaskRef,
) -> RuntimeState {
  let task_identity = task_ref_identity(ref)
  RuntimeState(
    ..release_task_claim(state, ref),
    issue_counters: dict.delete(state.issue_counters, task_identity),
  )
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
    running: Dict(identity.TaskIdentity, RunningEntry),
    claimed: Dict(identity.TaskIdentity, String),
    retry_attempts: Dict(identity.TaskIdentity, RetryEntry),
    issue_counters: Dict(identity.TaskIdentity, IssueCounter),
    parked: Dict(identity.TaskIdentity, ParkedEntry),
    invalid_workflow_reports: Dict(identity.TaskIdentity, InvalidWorkflowReport),
    blocked_dependency_reports: Dict(String, BlockedDependencyReport),
    completed: Dict(identity.TaskIdentity, tracker_issue.Issue),
    completed_at_ms: Dict(identity.TaskIdentity, Int),
    aggregate_pi_totals: session_tokens.TokenTotals,
    latest_rate_limit_payload: Option(String),
  )
}
