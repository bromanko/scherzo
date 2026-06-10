import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq, Gt, Lt}
import gleam/string
import scherzo/config/types as config_types
import scherzo/runtime/reason
import scherzo/runtime/recovery_policy
import scherzo/runtime/state as orchestrator_state
import scherzo/session/tokens as session_tokens
import scherzo/task
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_policy

const invalid_workflow_report_cache_limit = 1024

const blocked_dependency_report_cache_limit = 1024

pub type Effect {
  Dispatch(tracker_issue.Issue)
  ScheduleRetry(
    issue_id: String,
    delay_ms: Int,
    generation: Int,
    reason: reason.RetryReason,
    previous_retry: Option(orchestrator_state.RetryEntry),
  )
  CancelRetry(
    issue_id: String,
    generation: Int,
    reason: String,
    previous_retry: Option(orchestrator_state.RetryEntry),
  )
  CleanupWorkspace(path: String)
  ReleaseClaim(issue_id: String)
  StopWorker(issue_id: String, reason: reason.StopReason)
  ParkIssue(issue_id: String, reason: reason.ParkReason)
}

pub type Transition {
  Transition(state: orchestrator_state.RuntimeState, effects: List(Effect))
}

pub type WorkflowCleanupPolicy {
  AlreadyCleaned
  CleanupWorkflowWorkspace(String)
}

pub type BlockerDecision {
  BlockersSatisfied
  BlockedByDependency(
    open_blockers: List(tracker_issue.BlockerRef),
    incomplete: Bool,
  )
}

pub fn new_state(
  config: config_types.EffectiveConfig,
) -> orchestrator_state.RuntimeState {
  recovery_policy.new_state(config)
}

pub fn sort_candidates(
  issues: List(tracker_issue.Issue),
) -> List(tracker_issue.Issue) {
  list.sort(issues, by: compare_issue)
}

fn compare_issue(a: tracker_issue.Issue, b: tracker_issue.Issue) -> Order {
  case compare_priority(a.priority, b.priority) {
    Eq -> string.compare(a.identifier, b.identifier)
    other -> other
  }
}

fn compare_priority(a: Option(Int), b: Option(Int)) -> Order {
  case a, b {
    Some(a), Some(b) -> int.compare(a, b)
    Some(_), None -> Lt
    None, Some(_) -> Gt
    None, None -> Eq
  }
}

pub fn issue_fingerprint(issue: tracker_issue.Issue) -> String {
  recovery_policy.issue_fingerprint(issue)
}

pub fn issues_by_id(
  issues: List(tracker_issue.Issue),
) -> Dict(String, tracker_issue.Issue) {
  recovery_policy.issues_by_id(issues)
}

pub fn issue_identity(
  issue: tracker_issue.Issue,
) -> orchestrator_state.TaskIdentity {
  orchestrator_state.issue_identity(issue)
}

fn encode_string(value: String) -> String {
  int.to_string(string.length(value)) <> ":" <> value
}

fn bool_to_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn encode_optional_string(value: Option(String)) -> String {
  case value {
    None -> "none"
    Some(value) -> "some:" <> encode_string(value)
  }
}

fn encode_optional_issue_state(
  value: Option(issue_state.IssueState),
) -> String {
  case value {
    None -> "none"
    Some(value) -> "some:" <> encode_string(issue_state.to_string(value))
  }
}

fn blocker_fingerprint(blockers: List(tracker_issue.BlockerRef)) -> String {
  blockers
  |> list.map(fn(blocker) {
    [
      encode_optional_string(blocker.id),
      encode_optional_string(blocker.identifier),
      encode_optional_issue_state(blocker.state),
    ]
    |> string.join(with: ",")
  })
  |> list.sort(by: string.compare)
  |> string.join(with: ";")
}

pub fn should_dispatch(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
) -> Bool {
  is_dispatch_state(config, issue.state)
  && dispatch_preconditions_satisfied(state, config, issue)
  && workflow_policy_satisfied(config, issue)
}

pub fn dispatch_preconditions_satisfied(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
) -> Bool {
  dispatch_preconditions_satisfied_without_slot_capacity(state, config, issue)
  && slots_available(state, config, issue.state)
}

pub fn dispatch_preconditions_satisfied_without_slot_capacity(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
) -> Bool {
  let identity = orchestrator_state.issue_identity(issue)
  issue_has_required_fields(issue)
  && is_active(config, issue.state)
  && !is_terminal(config, issue.state)
  && !dict.has_key(state.running, identity)
  && !dict.has_key(state.claimed, identity)
  && !is_parked_for_issue(state, issue)
  && blockers_satisfied(config, issue)
}

pub fn workflow_policy_satisfied(
  config: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
) -> Bool {
  workflow_policy.classify_issue(config.linear_contract, issue)
  |> workflow_policy.workflow_satisfied
}

pub fn is_active(
  config: config_types.EffectiveConfig,
  state: issue_state.IssueState,
) -> Bool {
  recovery_policy.is_active(config, state)
}

pub fn is_dispatch_state(
  config: config_types.EffectiveConfig,
  state: issue_state.IssueState,
) -> Bool {
  issue_state.contains_normalized(config.tracker.dispatch_states, state)
}

pub fn is_terminal(
  config: config_types.EffectiveConfig,
  state: issue_state.IssueState,
) -> Bool {
  recovery_policy.is_terminal(config, state)
}

pub fn retry_candidate_precondition_failure(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  issue_id: String,
  issue: tracker_issue.Issue,
) -> Option(String) {
  case
    issue.id != issue_id,
    issue_has_required_fields(issue),
    is_terminal(config, issue.state),
    config_types.retry_state_allowed(config, issue.state),
    dict.has_key(state.running, orchestrator_state.issue_identity(issue)),
    retry_claim_allowed(state, issue_id),
    is_parked_for_issue(state, issue),
    blockers_satisfied(config, issue)
  {
    True, _, _, _, _, _, _, _ -> Some("retry_issue_id_mismatch")
    _, False, _, _, _, _, _, _ -> Some("retry_missing_required_fields")
    _, _, True, _, _, _, _, _ ->
      Some("retry_terminal_state:" <> issue_state.to_string(issue.state))
    _, _, _, False, _, _, _, _ ->
      Some(config_types.retry_non_retryable_state_reason(issue.state))
    _, _, _, _, True, _, _, _ -> Some("retry_issue_already_running")
    _, _, _, _, _, False, _, _ -> Some("retry_issue_already_claimed")
    _, _, _, _, _, _, True, _ -> Some("retry_issue_parked")
    _, _, _, _, _, _, _, False -> Some("retry_blocked_by_dependency")
    _, _, _, _, _, _, _, True -> None
  }
}

fn retry_claim_allowed(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
) -> Bool {
  let identity = orchestrator_state.linear_issue_id_identity(issue_id)
  case dict.has_key(state.claimed, identity) {
    False -> True
    True -> dict.has_key(state.retry_attempts, identity)
  }
}

fn issue_has_required_fields(issue: tracker_issue.Issue) -> Bool {
  string.trim(issue.id) != ""
  && string.trim(issue.identifier) != ""
  && string.trim(issue.title) != ""
  && string.trim(issue_state.to_string(issue.state)) != ""
}

fn is_parked_for_issue(
  state: orchestrator_state.RuntimeState,
  issue: tracker_issue.Issue,
) -> Bool {
  case dict.get(state.parked, orchestrator_state.issue_identity(issue)) {
    Ok(parked) -> park_blocks_dispatch(parked, issue)
    Error(Nil) -> False
  }
}

fn park_blocks_dispatch(
  parked: orchestrator_state.ParkedEntry,
  issue: tracker_issue.Issue,
) -> Bool {
  case parked.release_policy {
    orchestrator_state.ExplicitUnparkOnly -> True
    orchestrator_state.AutoUnparkOnIssueChange(stored) ->
      tracker_issue.fingerprint_matches(stored, issue)
  }
}

fn slots_available(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  issue_state_value: issue_state.IssueState,
) -> Bool {
  case config.agent.max_concurrent_agents == 0 {
    True -> False
    False ->
      dict.size(state.running) < config.agent.max_concurrent_agents
      && per_state_slot_available(state, config, issue_state_value)
  }
}

fn per_state_slot_available(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  issue_state_value: issue_state.IssueState,
) -> Bool {
  let key = issue_state.key(issue_state_value)
  case dict.get(config.agent.max_concurrent_agents_by_state, key) {
    Error(Nil) -> True
    Ok(limit) -> running_count_for_state(state, key) < limit
  }
}

fn running_count_for_state(
  state: orchestrator_state.RuntimeState,
  normalized_state: issue_state.IssueStateKey,
) -> Int {
  state.running
  |> dict.to_list
  |> list.filter(fn(entry) {
    let #(_, running) = entry
    issue_state.key(running.issue.state) == normalized_state
  })
  |> list.length
}

fn blockers_satisfied(
  config: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
) -> Bool {
  case blocker_decision(config, issue) {
    BlockersSatisfied -> True
    BlockedByDependency(_, _) -> False
  }
}

pub fn blocker_decision(
  config: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
) -> BlockerDecision {
  let open_blockers =
    issue.blocked_by
    |> list.filter(fn(blocker) {
      case blocker.state {
        Some(state) -> !is_terminal(config, state)
        None -> True
      }
    })
  case issue.blocked_by_complete, open_blockers {
    True, [] -> BlockersSatisfied
    complete, blockers ->
      BlockedByDependency(open_blockers: blockers, incomplete: !complete)
  }
}

pub fn apply_worker_start(
  state: orchestrator_state.RuntimeState,
  issue: tracker_issue.Issue,
  workspace_path: String,
) -> orchestrator_state.RuntimeState {
  apply_task_start(state, task.from_legacy_issue(issue), workspace_path)
}

pub fn apply_task_ref_start(
  state: orchestrator_state.RuntimeState,
  ref: task.TaskRef,
  issue: tracker_issue.Issue,
  workspace_path: String,
) -> orchestrator_state.RuntimeState {
  apply_task_start(
    state,
    task.Task(..task.from_legacy_issue(issue), ref: ref),
    workspace_path,
  )
}

pub fn apply_task_start(
  state: orchestrator_state.RuntimeState,
  item: task.Task,
  workspace_path: String,
) -> orchestrator_state.RuntimeState {
  let task.Task(ref: ref, ..) = item
  let issue = task.to_runtime_issue(item)
  let identity = orchestrator_state.task_ref_identity(ref)
  orchestrator_state.RuntimeState(
    ..state,
    running: dict.insert(
      state.running,
      identity,
      orchestrator_state.RunningEntry(
        task: item,
        issue: issue,
        workspace_path: workspace_path,
        session: None,
      ),
    ),
    claimed: dict.insert(state.claimed, identity, issue.identifier),
  )
}

pub fn apply_workflow_success(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  issue_id: String,
  final_issue: tracker_issue.Issue,
  tokens: session_tokens.TokenTotals,
  now_ms: Int,
  cleanup: WorkflowCleanupPolicy,
) -> Transition {
  apply_task_workflow_success(
    state,
    config,
    orchestrator_state.linear_issue_id_ref(issue_id),
    issue_id,
    final_issue,
    tokens,
    now_ms,
    cleanup,
  )
}

pub fn apply_task_workflow_success(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  ref: task.TaskRef,
  issue_id: String,
  final_issue: tracker_issue.Issue,
  tokens: session_tokens.TokenTotals,
  _now_ms: Int,
  cleanup: WorkflowCleanupPolicy,
) -> Transition {
  let base = state_after_task_worker_exit(state, ref, final_issue, tokens)
  case is_terminal(config, final_issue.state) {
    True -> {
      let cleanup_effect_list = case cleanup {
        AlreadyCleaned -> []
        CleanupWorkflowWorkspace(path) -> cleanup_effects(path)
      }
      Transition(
        state: release_task_claim(base, ref),
        effects: list.append(cleanup_effect_list, [ReleaseClaim(issue_id)]),
      )
    }
    False ->
      Transition(state: release_task_claim(base, ref), effects: [
        ReleaseClaim(issue_id),
      ])
  }
}

pub fn apply_worker_success(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  issue_id: String,
  final_issue: tracker_issue.Issue,
  tokens: session_tokens.TokenTotals,
  now_ms: Int,
) -> Transition {
  apply_worker_success_with_workspace_path(
    state,
    config,
    issue_id,
    final_issue,
    "",
    tokens,
    now_ms,
  )
}

pub fn apply_worker_success_with_workspace_path(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  issue_id: String,
  final_issue: tracker_issue.Issue,
  workspace_path: String,
  tokens: session_tokens.TokenTotals,
  now_ms: Int,
) -> Transition {
  apply_task_worker_success_with_workspace_path(
    state,
    config,
    orchestrator_state.linear_issue_id_ref(issue_id),
    issue_id,
    final_issue,
    workspace_path,
    tokens,
    now_ms,
  )
}

pub fn apply_task_worker_success_with_workspace_path(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  ref: task.TaskRef,
  issue_id: String,
  final_issue: tracker_issue.Issue,
  workspace_path: String,
  tokens: session_tokens.TokenTotals,
  now_ms: Int,
) -> Transition {
  let identity = orchestrator_state.task_ref_identity(ref)
  let workspace_path = case string.trim(workspace_path) == "" {
    True ->
      case dict.get(state.running, identity) {
        Ok(entry) -> entry.workspace_path
        Error(Nil) -> ""
      }
    False -> workspace_path
  }
  let base = state_after_task_worker_exit(state, ref, final_issue, tokens)
  case is_terminal(config, final_issue.state) {
    True ->
      Transition(
        state: release_task_claim(base, ref),
        effects: list.append(cleanup_effects(workspace_path), [
          ReleaseClaim(issue_id),
        ]),
      )
    False ->
      case is_active(config, final_issue.state) {
        False ->
          Transition(state: release_task_claim(base, ref), effects: [
            ReleaseClaim(issue_id),
          ])
        True ->
          continue_or_park_task(
            base,
            config,
            ref,
            issue_id,
            final_issue,
            now_ms,
          )
      }
  }
}

pub fn apply_worker_failure(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  issue_id: String,
  baseline_issue: tracker_issue.Issue,
  now_ms: Int,
) -> Transition {
  apply_task_worker_failure(
    state,
    config,
    orchestrator_state.linear_issue_id_ref(issue_id),
    issue_id,
    baseline_issue,
    now_ms,
  )
}

pub fn apply_task_worker_failure(
  state: orchestrator_state.RuntimeState,
  _config: config_types.EffectiveConfig,
  ref: task.TaskRef,
  issue_id: String,
  baseline_issue: tracker_issue.Issue,
  now_ms: Int,
) -> Transition {
  let baseline_issue = issue_with_lifecycle_id(baseline_issue, issue_id)
  let identity = orchestrator_state.task_ref_identity(ref)
  let state =
    orchestrator_state.RuntimeState(
      ..state,
      running: dict.delete(state.running, identity),
    )
  let counter = get_task_counter(state, ref)
  let failures = counter.failure_attempts + 1
  let counter =
    orchestrator_state.IssueCounter(..counter, failure_attempts: failures)
  let state = put_task_counter(state, ref, counter)
  park_task(state, ref, baseline_issue, reason.ParkWorkerFailure, now_ms)
}

fn issue_with_lifecycle_id(
  issue: tracker_issue.Issue,
  issue_id: String,
) -> tracker_issue.Issue {
  case issue.id == issue_id {
    True -> issue
    False -> tracker_issue.Issue(..issue, id: issue_id)
  }
}

fn continue_or_park_task(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  ref: task.TaskRef,
  issue_id: String,
  issue: tracker_issue.Issue,
  now_ms: Int,
) -> Transition {
  let counter = get_task_counter(state, ref)
  let sessions = counter.worker_sessions + 1
  let counter =
    orchestrator_state.IssueCounter(..counter, worker_sessions: sessions)
  let state = put_task_counter(state, ref, counter)
  case sessions >= config.agent.max_sessions_per_issue {
    True ->
      park_task(
        state,
        ref,
        issue_with_lifecycle_id(issue, issue_id),
        reason.ParkMaxSessionsPerIssue,
        now_ms,
      )
    False ->
      schedule_task_retry(
        state,
        ref,
        issue_id,
        1000,
        reason.RetryAfterContinuation,
      )
  }
}

fn cleanup_effects(workspace_path: String) -> List(Effect) {
  case string.trim(workspace_path) == "" {
    True -> []
    False -> [CleanupWorkspace(workspace_path)]
  }
}

fn state_after_task_worker_exit(
  state: orchestrator_state.RuntimeState,
  ref: task.TaskRef,
  final_issue: tracker_issue.Issue,
  tokens: session_tokens.TokenTotals,
) -> orchestrator_state.RuntimeState {
  let identity = orchestrator_state.task_ref_identity(ref)
  orchestrator_state.RuntimeState(
    ..state,
    running: dict.delete(state.running, identity),
    completed: dict.insert(state.completed, identity, final_issue),
    aggregate_pi_totals: add_tokens(state.aggregate_pi_totals, tokens),
  )
}

pub fn schedule_retry(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
  delay_ms: Int,
  reason: reason.RetryReason,
) -> Transition {
  schedule_task_retry(
    state,
    orchestrator_state.linear_issue_id_ref(issue_id),
    issue_id,
    delay_ms,
    reason,
  )
}

pub fn schedule_task_retry(
  state: orchestrator_state.RuntimeState,
  ref: task.TaskRef,
  issue_id: String,
  delay_ms: Int,
  reason: reason.RetryReason,
) -> Transition {
  let identity = orchestrator_state.task_ref_identity(ref)
  let previous_retry = case dict.get(state.retry_attempts, identity) {
    Ok(entry) -> Some(entry)
    Error(Nil) -> None
  }
  let current_generation = case previous_retry {
    Some(entry) -> Some(entry.timer_generation)
    None -> None
  }
  let generation = recovery_policy.next_generation(current_generation)
  let retry =
    orchestrator_state.RetryEntry(
      task_ref: ref,
      issue_id: issue_id,
      delay_ms: delay_ms,
      timer_generation: generation,
    )
  Transition(
    state: orchestrator_state.RuntimeState(
      ..state,
      retry_attempts: dict.insert(state.retry_attempts, identity, retry),
    ),
    effects: [
      ScheduleRetry(issue_id, delay_ms, generation, reason, previous_retry),
    ],
  )
}

pub fn handle_retry_candidate(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  issue_id: String,
  candidate: Result(Option(tracker_issue.Issue), String),
) -> Transition {
  case candidate {
    Error(_) ->
      schedule_retry_with_backoff(
        state,
        config,
        issue_id,
        reason.RetryPollFailed,
      )
    Ok(None) -> release_retry_claim(state, issue_id, "retry_issue_missing")
    Ok(Some(issue)) -> {
      let state = unpark_if_issue_changed(state, issue)
      case
        retry_candidate_precondition_failure(state, config, issue_id, issue)
      {
        Some(reason) -> release_retry_claim(state, issue_id, reason)
        None ->
          case workflow_policy_satisfied(config, issue) {
            False ->
              release_retry_claim(
                state,
                issue_id,
                "retry_workflow_policy_invalid",
              )
            True ->
              case slots_available(state, config, issue.state) {
                True -> dispatch_retry_claim(state, issue_id, issue)
                False ->
                  schedule_retry_with_backoff(
                    state,
                    config,
                    issue_id,
                    reason.RetryNoSlots,
                  )
              }
          }
      }
    }
  }
}

fn release_retry_claim(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
  cancel_reason: String,
) -> Transition {
  let generation = retry_generation(state, issue_id)
  let previous_retry =
    current_retry_entry(state, issue_id)
    |> option.from_result
  Transition(
    state: release_claim(clear_retry(state, issue_id), issue_id),
    effects: [
      CancelRetry(issue_id, generation, cancel_reason, previous_retry),
      ReleaseClaim(issue_id),
    ],
  )
}

fn dispatch_retry_claim(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
  issue: tracker_issue.Issue,
) -> Transition {
  let generation = retry_generation(state, issue_id)
  let previous_retry =
    current_retry_entry(state, issue_id)
    |> option.from_result
  Transition(state: clear_retry(state, issue_id), effects: [
    CancelRetry(issue_id, generation, "retry_dispatch", previous_retry),
    Dispatch(issue),
  ])
}

pub fn schedule_retry_with_backoff(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  issue_id: String,
  reason: reason.RetryReason,
) -> Transition {
  schedule_retry(
    state,
    issue_id,
    retry_backoff_delay(state, config, issue_id),
    reason,
  )
}

fn retry_backoff_delay(
  state: orchestrator_state.RuntimeState,
  _config: config_types.EffectiveConfig,
  issue_id: String,
) -> Int {
  let identity = orchestrator_state.linear_issue_id_identity(issue_id)
  let attempt = case dict.get(state.retry_attempts, identity) {
    Ok(entry) -> recovery_policy.next_attempt_index(entry.timer_generation)
    Error(Nil) -> recovery_policy.first_attempt_index()
  }
  recovery_policy.backoff_delay(
    attempt,
    recovery_policy.default_max_backoff_ms(),
  )
}

pub fn reconcile_issue(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  refreshed: tracker_issue.Issue,
) -> Transition {
  reconcile_task_issue(
    state,
    config,
    task.from_legacy_issue(refreshed).ref,
    refreshed,
  )
}

pub fn reconcile_task_issue(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  ref: task.TaskRef,
  refreshed: tracker_issue.Issue,
) -> Transition {
  let identity = orchestrator_state.task_ref_identity(ref)
  case dict.get(state.running, identity) {
    Error(Nil) -> Transition(state: state, effects: [])
    Ok(entry) ->
      case is_terminal(config, refreshed.state) {
        True ->
          Transition(
            state: release_task_claim(
              orchestrator_state.RuntimeState(
                ..state,
                running: dict.delete(state.running, identity),
              ),
              ref,
            ),
            effects: [
              StopWorker(refreshed.id, reason.StopTerminal),
              ..cleanup_effects(entry.workspace_path)
            ],
          )
        False ->
          case is_active(config, refreshed.state) {
            True -> {
              let refreshed_task =
                task.Task(..task.from_legacy_issue(refreshed), ref: ref)
              Transition(
                state: orchestrator_state.RuntimeState(
                  ..state,
                  running: dict.insert(
                    state.running,
                    identity,
                    orchestrator_state.RunningEntry(
                      ..entry,
                      task: refreshed_task,
                      issue: refreshed,
                    ),
                  ),
                ),
                effects: [],
              )
            }
            False ->
              Transition(
                state: release_task_claim(
                  orchestrator_state.RuntimeState(
                    ..state,
                    running: dict.delete(state.running, identity),
                  ),
                  ref,
                ),
                effects: [StopWorker(refreshed.id, reason.StopNonActive)],
              )
          }
      }
  }
}

pub fn unpark_if_issue_changed(
  state: orchestrator_state.RuntimeState,
  issue: tracker_issue.Issue,
) -> orchestrator_state.RuntimeState {
  let identity = orchestrator_state.issue_identity(issue)
  case dict.get(state.parked, identity) {
    Ok(parked) ->
      case parked.release_policy {
        orchestrator_state.ExplicitUnparkOnly -> state
        orchestrator_state.AutoUnparkOnIssueChange(stored) ->
          case tracker_issue.fingerprint_matches(stored, issue) {
            True -> state
            False ->
              orchestrator_state.RuntimeState(
                ..state,
                claimed: dict.delete(state.claimed, identity),
                parked: dict.delete(state.parked, identity),
                retry_attempts: dict.delete(state.retry_attempts, identity),
                issue_counters: dict.delete(state.issue_counters, identity),
              )
          }
      }
    Error(Nil) -> state
  }
}

pub fn backoff_delay(attempt: Int, max_ms: Int) -> Int {
  recovery_policy.backoff_delay(attempt, max_ms)
}

pub fn default_max_backoff_ms() -> Int {
  recovery_policy.default_max_backoff_ms()
}

pub fn add_tokens(
  a: session_tokens.TokenTotals,
  b: session_tokens.TokenTotals,
) -> session_tokens.TokenTotals {
  session_tokens.TokenTotals(
    input: a.input + b.input,
    output: a.output + b.output,
    cache_read: a.cache_read + b.cache_read,
    cache_write: a.cache_write + b.cache_write,
    total: a.total + b.total,
  )
}

fn park_task(
  state: orchestrator_state.RuntimeState,
  ref: task.TaskRef,
  baseline_issue: tracker_issue.Issue,
  reason: reason.ParkReason,
  now_ms: Int,
) -> Transition {
  let issue_id = baseline_issue.id
  let identity = orchestrator_state.task_ref_identity(ref)
  let identifier = case dict.get(state.claimed, identity) {
    Ok(identifier) -> identifier
    Error(Nil) -> baseline_issue.identifier
  }
  let parked =
    orchestrator_state.ParkedEntry(
      task_ref: ref,
      issue_id: issue_id,
      identifier: identifier,
      reason: reason,
      release_policy: orchestrator_state.AutoUnparkOnIssueChange(
        issue_fingerprint(baseline_issue),
      ),
      parked_at_ms: now_ms,
    )
  Transition(
    state: orchestrator_state.RuntimeState(
      ..state,
      claimed: dict.delete(state.claimed, identity),
      parked: dict.insert(state.parked, identity, parked),
      retry_attempts: dict.delete(state.retry_attempts, identity),
    ),
    effects: [ParkIssue(issue_id, reason), ReleaseClaim(issue_id)],
  )
}

fn clear_retry(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
) -> orchestrator_state.RuntimeState {
  clear_task_retry(state, orchestrator_state.linear_issue_id_ref(issue_id))
}

fn clear_task_retry(
  state: orchestrator_state.RuntimeState,
  ref: task.TaskRef,
) -> orchestrator_state.RuntimeState {
  let identity = orchestrator_state.task_ref_identity(ref)
  orchestrator_state.RuntimeState(
    ..state,
    retry_attempts: dict.delete(state.retry_attempts, identity),
  )
}

pub fn stop_retry_for_policy_invalid(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
) -> Transition {
  let generation = retry_generation(state, issue_id)
  let previous_retry =
    current_retry_entry(state, issue_id)
    |> option.from_result
  Transition(
    state: release_claim(clear_retry(state, issue_id), issue_id),
    effects: [
      CancelRetry(issue_id, generation, "policy_invalid", previous_retry),
      ReleaseClaim(issue_id),
    ],
  )
}

pub fn stop_retry_for_dependency_blocked(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
) -> Transition {
  let generation = retry_generation(state, issue_id)
  let previous_retry =
    current_retry_entry(state, issue_id)
    |> option.from_result
  Transition(
    state: release_claim(clear_retry(state, issue_id), issue_id),
    effects: [
      CancelRetry(
        issue_id,
        generation,
        "linear_dependency_blocked",
        previous_retry,
      ),
      ReleaseClaim(issue_id),
    ],
  )
}

fn retry_generation(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
) -> Int {
  case current_retry_entry(state, issue_id) {
    Ok(entry) -> entry.timer_generation
    Error(Nil) -> 0
  }
}

fn current_retry_entry(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
) -> Result(orchestrator_state.RetryEntry, Nil) {
  let identity = orchestrator_state.linear_issue_id_identity(issue_id)
  dict.get(state.retry_attempts, identity)
}

pub fn blocked_dependency_fingerprint(
  config: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
  phase: String,
  decision: BlockerDecision,
) -> String {
  [
    encode_string(phase),
    encode_string(bool_to_string(issue.blocked_by_complete)),
    encode_string(bool_to_string(blocker_decision_incomplete(decision))),
    encode_string(terminal_state_policy_fingerprint(config)),
    blocker_fingerprint(issue.blocked_by),
  ]
  |> string.join(with: "|")
}

pub fn terminal_state_policy_fingerprint(
  config: config_types.EffectiveConfig,
) -> String {
  config.tracker.terminal_states
  |> list.map(fn(state) { issue_state.key_to_string(issue_state.key(state)) })
  |> list.sort(by: string.compare)
  |> string.join(with: ",")
}

pub fn blocker_decision_incomplete(decision: BlockerDecision) -> Bool {
  case decision {
    BlockersSatisfied -> False
    BlockedByDependency(_, incomplete) -> incomplete
  }
}

pub fn already_reported_blocked_dependency(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
  phase: String,
  decision: BlockerDecision,
) -> Bool {
  let key =
    blocked_dependency_report_key(
      orchestrator_state.issue_identity(issue),
      phase,
    )
  case dict.get(state.blocked_dependency_reports, key) {
    Error(Nil) -> False
    Ok(report) ->
      report.last_result != "failed"
      && report.observed_updated_at == issue.updated_at
      && report.blocker_fingerprint
      == blocked_dependency_fingerprint(config, issue, phase, decision)
      && report.terminal_state_policy_fingerprint
      == terminal_state_policy_fingerprint(config)
  }
}

pub fn mark_blocked_dependency_reported(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
  phase: String,
  decision: BlockerDecision,
  now_ms: Int,
) -> orchestrator_state.RuntimeState {
  let key =
    blocked_dependency_report_key(
      orchestrator_state.issue_identity(issue),
      phase,
    )
  let report =
    orchestrator_state.BlockedDependencyReport(
      issue_id: issue.id,
      identifier: issue.identifier,
      phase: phase,
      blocker_fingerprint: blocked_dependency_fingerprint(
        config,
        issue,
        phase,
        decision,
      ),
      observed_updated_at: issue.updated_at,
      terminal_state_policy_fingerprint: terminal_state_policy_fingerprint(
        config,
      ),
      attempted_at_ms: now_ms,
      last_result: "logged",
    )
  orchestrator_state.RuntimeState(
    ..state,
    blocked_dependency_reports: dict.insert(
        state.blocked_dependency_reports,
        key,
        report,
      )
      |> trim_blocked_dependency_reports,
  )
}

pub fn clear_blocked_dependency_report(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
  phase: String,
) -> orchestrator_state.RuntimeState {
  let identity = orchestrator_state.linear_issue_id_identity(issue_id)
  orchestrator_state.RuntimeState(
    ..state,
    blocked_dependency_reports: dict.delete(
      state.blocked_dependency_reports,
      blocked_dependency_report_key(identity, phase),
    ),
  )
}

fn blocked_dependency_report_key(
  task_identity: orchestrator_state.TaskIdentity,
  phase: String,
) -> String {
  orchestrator_state.task_identity_to_string(task_identity) <> "|" <> phase
}

fn trim_blocked_dependency_reports(
  reports: dict.Dict(String, orchestrator_state.BlockedDependencyReport),
) -> dict.Dict(String, orchestrator_state.BlockedDependencyReport) {
  case dict.size(reports) <= blocked_dependency_report_cache_limit {
    True -> reports
    False ->
      reports
      |> dict.to_list
      |> list.sort(by: compare_blocked_dependency_report_entries)
      |> list.take(blocked_dependency_report_cache_limit)
      |> dict.from_list
  }
}

fn compare_blocked_dependency_report_entries(
  a: #(String, orchestrator_state.BlockedDependencyReport),
  b: #(String, orchestrator_state.BlockedDependencyReport),
) -> Order {
  let #(a_id, a_report) = a
  let #(b_id, b_report) = b
  case int.compare(b_report.attempted_at_ms, a_report.attempted_at_ms) {
    Eq -> string.compare(a_id, b_id)
    order -> order
  }
}

pub fn already_attempted_invalid_workflow(
  state: orchestrator_state.RuntimeState,
  issue: tracker_issue.Issue,
  violation: workflow_policy.IssueWorkflowViolation,
  config: config_types.LinearContractConfig,
) -> Bool {
  case
    dict.get(
      state.invalid_workflow_reports,
      orchestrator_state.issue_identity(issue),
    )
  {
    Error(Nil) -> False
    Ok(report) ->
      report.last_result != "failed"
      && report.observed_updated_at == issue.updated_at
      && report.observed_labels_fingerprint
      == workflow_policy.observed_labels_fingerprint(issue)
      && report.violation_fingerprint
      == workflow_policy.violation_fingerprint(violation)
      && report.reporting_policy_fingerprint
      == workflow_policy.reporting_policy_fingerprint(config)
  }
}

pub fn mark_invalid_workflow_report_pending(
  state: orchestrator_state.RuntimeState,
  issue: tracker_issue.Issue,
  violation: workflow_policy.IssueWorkflowViolation,
  config: config_types.LinearContractConfig,
  now_ms: Int,
) -> orchestrator_state.RuntimeState {
  let report =
    orchestrator_state.InvalidWorkflowReport(
      issue_id: issue.id,
      identifier: issue.identifier,
      violation_code: workflow_policy.violation_code(violation),
      violation_fingerprint: workflow_policy.violation_fingerprint(violation),
      reporting_policy_fingerprint: workflow_policy.reporting_policy_fingerprint(
        config,
      ),
      observed_updated_at: issue.updated_at,
      observed_labels_fingerprint: workflow_policy.observed_labels_fingerprint(
        issue,
      ),
      attempted_at_ms: now_ms,
      last_result: "pending",
    )
  orchestrator_state.RuntimeState(
    ..state,
    invalid_workflow_reports: dict.insert(
        state.invalid_workflow_reports,
        orchestrator_state.issue_identity(issue),
        report,
      )
      |> trim_invalid_workflow_reports,
  )
}

pub fn mark_invalid_workflow_report_result(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
  violation_fingerprint: String,
  reporting_policy_fingerprint: String,
  last_result: String,
) -> orchestrator_state.RuntimeState {
  let identity = orchestrator_state.linear_issue_id_identity(issue_id)
  case dict.get(state.invalid_workflow_reports, identity) {
    Error(Nil) -> state
    Ok(report) ->
      case
        report.violation_fingerprint == violation_fingerprint
        && report.reporting_policy_fingerprint == reporting_policy_fingerprint
      {
        False -> state
        True ->
          orchestrator_state.RuntimeState(
            ..state,
            invalid_workflow_reports: dict.insert(
              state.invalid_workflow_reports,
              identity,
              orchestrator_state.InvalidWorkflowReport(
                ..report,
                last_result: last_result,
              ),
            ),
          )
      }
  }
}

pub fn clear_invalid_workflow_report(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
) -> orchestrator_state.RuntimeState {
  orchestrator_state.RuntimeState(
    ..state,
    invalid_workflow_reports: dict.delete(
      state.invalid_workflow_reports,
      orchestrator_state.linear_issue_id_identity(issue_id),
    ),
  )
}

fn trim_invalid_workflow_reports(
  reports: dict.Dict(
    orchestrator_state.TaskIdentity,
    orchestrator_state.InvalidWorkflowReport,
  ),
) -> dict.Dict(
  orchestrator_state.TaskIdentity,
  orchestrator_state.InvalidWorkflowReport,
) {
  case dict.size(reports) <= invalid_workflow_report_cache_limit {
    True -> reports
    False ->
      reports
      |> dict.to_list
      |> list.sort(by: compare_invalid_workflow_report_entries)
      |> list.take(invalid_workflow_report_cache_limit)
      |> dict.from_list
  }
}

fn compare_invalid_workflow_report_entries(
  a: #(
    orchestrator_state.TaskIdentity,
    orchestrator_state.InvalidWorkflowReport,
  ),
  b: #(
    orchestrator_state.TaskIdentity,
    orchestrator_state.InvalidWorkflowReport,
  ),
) -> Order {
  let #(a_id, a_report) = a
  let #(b_id, b_report) = b
  case int.compare(b_report.attempted_at_ms, a_report.attempted_at_ms) {
    Eq ->
      string.compare(
        orchestrator_state.task_identity_to_string(a_id),
        orchestrator_state.task_identity_to_string(b_id),
      )
    order -> order
  }
}

fn release_claim(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
) -> orchestrator_state.RuntimeState {
  release_task_claim(state, orchestrator_state.linear_issue_id_ref(issue_id))
}

fn release_task_claim(
  state: orchestrator_state.RuntimeState,
  ref: task.TaskRef,
) -> orchestrator_state.RuntimeState {
  let identity = orchestrator_state.task_ref_identity(ref)
  orchestrator_state.RuntimeState(
    ..state,
    claimed: dict.delete(state.claimed, identity),
    retry_attempts: dict.delete(state.retry_attempts, identity),
  )
}

fn get_task_counter(
  state: orchestrator_state.RuntimeState,
  ref: task.TaskRef,
) -> orchestrator_state.IssueCounter {
  let identity = orchestrator_state.task_ref_identity(ref)
  case dict.get(state.issue_counters, identity) {
    Ok(counter) -> counter
    Error(Nil) -> orchestrator_state.new_issue_counter()
  }
}

fn put_task_counter(
  state: orchestrator_state.RuntimeState,
  ref: task.TaskRef,
  counter: orchestrator_state.IssueCounter,
) -> orchestrator_state.RuntimeState {
  let identity = orchestrator_state.task_ref_identity(ref)
  orchestrator_state.RuntimeState(
    ..state,
    issue_counters: dict.insert(state.issue_counters, identity, counter),
  )
}
