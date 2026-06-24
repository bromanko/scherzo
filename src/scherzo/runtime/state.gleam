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
    task_lifecycles: dict.new(),
    running: dict.new(),
    claimed: dict.new(),
    retry_attempts: dict.new(),
    issue_counters: dict.new(),
    parked: dict.new(),
    invalid_workflow_reports: dict.new(),
    blocked_dependency_reports: dict.new(),
    completed: dict.new(),
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

pub type CompletedEntry {
  CompletedEntry(issue: tracker_issue.Issue, completed_at_ms: Int)
}

pub fn completed_issue(entry: CompletedEntry) -> tracker_issue.Issue {
  entry.issue
}

pub fn completed_timestamp_ms(entry: CompletedEntry) -> Int {
  entry.completed_at_ms
}

pub fn completed_for(
  state: RuntimeState,
  task_identity: identity.TaskIdentity,
) -> Result(tracker_issue.Issue, Nil) {
  case dict.get(state.completed, task_identity) {
    Ok(entry) -> Ok(entry.issue)
    Error(Nil) -> Error(Nil)
  }
}

pub fn completed_issues(state: RuntimeState) -> List(tracker_issue.Issue) {
  state.completed
  |> dict.values
  |> list.map(completed_issue)
}

pub type TaskRuntimeLifecycle {
  TaskClaimed(identifier: String)
  TaskRunning(entry: RunningEntry)
  TaskRetrying(entry: RetryEntry, identifier: String)
  TaskParked(entry: ParkedEntry)
  TaskCompleted(entry: CompletedEntry)
}

pub fn task_lifecycle(
  state: RuntimeState,
  task_identity: identity.TaskIdentity,
) -> Result(TaskRuntimeLifecycle, Nil) {
  dict.get(state.task_lifecycles, task_identity)
}

const completed_cache_limit = 1024

pub fn mark_task_claimed(
  state: RuntimeState,
  task_identity: identity.TaskIdentity,
  identifier: String,
) -> RuntimeState {
  replace_task_lifecycle(state, task_identity, TaskClaimed(identifier))
}

pub fn mark_task_running(
  state: RuntimeState,
  task_identity: identity.TaskIdentity,
  entry: RunningEntry,
) -> RuntimeState {
  replace_task_lifecycle(state, task_identity, TaskRunning(entry))
}

pub fn mark_task_retrying(
  state: RuntimeState,
  task_identity: identity.TaskIdentity,
  entry: RetryEntry,
  identifier: String,
) -> RuntimeState {
  replace_task_lifecycle(state, task_identity, TaskRetrying(entry, identifier))
}

pub fn mark_task_parked(
  state: RuntimeState,
  task_identity: identity.TaskIdentity,
  entry: ParkedEntry,
) -> RuntimeState {
  replace_task_lifecycle(state, task_identity, TaskParked(entry))
}

pub fn cache_completed_task(
  state: RuntimeState,
  task_identity: identity.TaskIdentity,
  issue: tracker_issue.Issue,
  completed_at_ms: Int,
) -> RuntimeState {
  replace_task_lifecycle(
    state,
    task_identity,
    TaskCompleted(CompletedEntry(issue, completed_at_ms)),
  )
  |> trim_completed_cache
}

pub fn clear_task_lifecycle(
  state: RuntimeState,
  task_identity: identity.TaskIdentity,
) -> RuntimeState {
  clear_lifecycle_indexes(state, task_identity)
}

pub fn clear_active_task_lifecycles(state: RuntimeState) -> RuntimeState {
  let retained_lifecycles =
    state.task_lifecycles
    |> dict.to_list
    |> list.filter(fn(entry) {
      let #(_, lifecycle) = entry
      case lifecycle {
        TaskParked(_) | TaskCompleted(_) -> True
        TaskClaimed(_) | TaskRunning(_) | TaskRetrying(_, _) -> False
      }
    })
    |> dict.from_list
  RuntimeState(
    ..state,
    task_lifecycles: retained_lifecycles,
    running: dict.new(),
    claimed: dict.new(),
    retry_attempts: dict.new(),
  )
}

pub fn clear_task_retry(
  state: RuntimeState,
  task_identity: identity.TaskIdentity,
) -> RuntimeState {
  let identifier = identifier_for_retry_clear(state, task_identity)
  let state =
    RuntimeState(
      ..state,
      retry_attempts: dict.delete(state.retry_attempts, task_identity),
      task_lifecycles: dict.delete(state.task_lifecycles, task_identity),
    )
  case string.trim(identifier) == "" {
    True -> state
    False -> mark_task_claimed(state, task_identity, identifier)
  }
}

fn identifier_for_retry_clear(
  state: RuntimeState,
  task_identity: identity.TaskIdentity,
) -> String {
  case dict.get(state.task_lifecycles, task_identity) {
    Ok(TaskRetrying(_, identifier)) -> identifier
    Ok(TaskClaimed(identifier)) -> identifier
    Ok(TaskRunning(entry)) -> entry.issue.identifier
    Ok(TaskParked(entry)) -> entry.identifier
    Ok(TaskCompleted(entry)) -> entry.issue.identifier
    Error(Nil) ->
      case dict.get(state.claimed, task_identity) {
        Ok(identifier) -> identifier
        Error(Nil) -> ""
      }
  }
}

fn replace_task_lifecycle(
  state: RuntimeState,
  task_identity: identity.TaskIdentity,
  lifecycle: TaskRuntimeLifecycle,
) -> RuntimeState {
  let state = clear_lifecycle_indexes(state, task_identity)
  let state =
    RuntimeState(
      ..state,
      task_lifecycles: dict.insert(
        state.task_lifecycles,
        task_identity,
        lifecycle,
      ),
    )
  insert_lifecycle_indexes(state, task_identity, lifecycle)
}

fn clear_lifecycle_indexes(
  state: RuntimeState,
  task_identity: identity.TaskIdentity,
) -> RuntimeState {
  RuntimeState(
    ..state,
    task_lifecycles: dict.delete(state.task_lifecycles, task_identity),
    running: dict.delete(state.running, task_identity),
    claimed: dict.delete(state.claimed, task_identity),
    retry_attempts: dict.delete(state.retry_attempts, task_identity),
    parked: dict.delete(state.parked, task_identity),
    completed: dict.delete(state.completed, task_identity),
  )
}

fn insert_lifecycle_indexes(
  state: RuntimeState,
  task_identity: identity.TaskIdentity,
  lifecycle: TaskRuntimeLifecycle,
) -> RuntimeState {
  case lifecycle {
    TaskClaimed(identifier) ->
      RuntimeState(
        ..state,
        claimed: dict.insert(state.claimed, task_identity, identifier),
      )
    TaskRunning(entry) ->
      RuntimeState(
        ..state,
        running: dict.insert(state.running, task_identity, entry),
        claimed: dict.insert(
          state.claimed,
          task_identity,
          entry.issue.identifier,
        ),
      )
    TaskRetrying(entry, identifier) ->
      RuntimeState(
        ..state,
        retry_attempts: dict.insert(state.retry_attempts, task_identity, entry),
        claimed: dict.insert(state.claimed, task_identity, identifier),
      )
    TaskParked(entry) ->
      RuntimeState(
        ..state,
        parked: dict.insert(state.parked, task_identity, entry),
      )
    TaskCompleted(entry) ->
      RuntimeState(
        ..state,
        completed: dict.insert(state.completed, task_identity, entry),
      )
  }
}

fn trim_completed_cache(state: RuntimeState) -> RuntimeState {
  case dict.size(state.completed) <= completed_cache_limit {
    True -> state
    False -> {
      let kept_entries =
        state.completed
        |> dict.to_list
        |> list.sort(by: compare_completed_entries)
        |> list.take(completed_cache_limit)
      let kept_identity_lookup =
        kept_entries
        |> list.map(fn(entry) {
          let #(task_identity, _) = entry
          #(task_identity, Nil)
        })
        |> dict.from_list
      let kept_lifecycles =
        state.task_lifecycles
        |> dict.to_list
        |> list.filter(fn(entry) {
          let #(task_identity, lifecycle) = entry
          case lifecycle {
            TaskCompleted(_) ->
              dict.has_key(kept_identity_lookup, task_identity)
            _ -> True
          }
        })
        |> dict.from_list
      RuntimeState(
        ..state,
        task_lifecycles: kept_lifecycles,
        completed: dict.from_list(kept_entries),
      )
    }
  }
}

fn compare_completed_entries(
  a: #(identity.TaskIdentity, CompletedEntry),
  b: #(identity.TaskIdentity, CompletedEntry),
) -> Order {
  let #(a_id, a_entry) = a
  let #(b_id, b_entry) = b
  case int.compare(b_entry.completed_at_ms, a_entry.completed_at_ms) {
    Eq -> string.compare(identity.to_string(a_id), identity.to_string(b_id))
    order -> order
  }
}

pub fn release_task_claim(
  state: RuntimeState,
  ref: task.TaskRef,
) -> RuntimeState {
  let task_identity = task_ref_identity(ref)
  let state_without_claim =
    RuntimeState(
      ..state,
      claimed: dict.delete(state.claimed, task_identity),
      retry_attempts: dict.delete(state.retry_attempts, task_identity),
    )
  case dict.get(state.task_lifecycles, task_identity) {
    Ok(TaskClaimed(_)) | Ok(TaskRunning(_)) | Ok(TaskRetrying(_, _)) ->
      clear_task_lifecycle(state_without_claim, task_identity)
    Ok(TaskParked(_)) | Ok(TaskCompleted(_)) | Error(Nil) -> state_without_claim
  }
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
    // Primary per-task lifecycle. The maps below are derived indexes retained
    // for existing lookup paths and must be updated through the helpers above.
    task_lifecycles: Dict(identity.TaskIdentity, TaskRuntimeLifecycle),
    running: Dict(identity.TaskIdentity, RunningEntry),
    claimed: Dict(identity.TaskIdentity, String),
    retry_attempts: Dict(identity.TaskIdentity, RetryEntry),
    issue_counters: Dict(identity.TaskIdentity, IssueCounter),
    parked: Dict(identity.TaskIdentity, ParkedEntry),
    invalid_workflow_reports: Dict(identity.TaskIdentity, InvalidWorkflowReport),
    blocked_dependency_reports: Dict(String, BlockedDependencyReport),
    completed: Dict(identity.TaskIdentity, CompletedEntry),
    aggregate_pi_totals: session_tokens.TokenTotals,
    latest_rate_limit_payload: Option(String),
  )
}
