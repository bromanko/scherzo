import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq, Gt, Lt}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/orchestrator/reason
import scherzo/orchestrator/state as orchestrator_state
import scherzo/session/tokens as session_tokens
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
  )
  CancelRetry(issue_id: String, generation: Int, reason: String)
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
  orchestrator_state.RuntimeState(
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
    aggregate_pi_totals: session_tokens.zero_token_totals(),
    latest_rate_limit_payload: None,
  )
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
  tracker_issue.content_fingerprint(issue)
}

pub fn issues_by_id(
  issues: List(tracker_issue.Issue),
) -> Dict(String, tracker_issue.Issue) {
  issues
  |> list.map(fn(issue) { #(issue.id, issue) })
  |> dict.from_list
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
  issue_has_required_fields(issue)
  && is_active(config, issue.state)
  && !is_terminal(config, issue.state)
  && !dict.has_key(state.running, issue.id)
  && !dict.has_key(state.claimed, issue.id)
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
  issue_state.contains_normalized(config.tracker.active_states, state)
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
  issue_state.contains_normalized(config.tracker.terminal_states, state)
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
    dict.has_key(state.running, issue.id),
    retry_claim_allowed(state, issue.id),
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
  case dict.has_key(state.claimed, issue_id) {
    False -> True
    True -> dict.has_key(state.retry_attempts, issue_id)
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
  case dict.get(state.parked, issue.id) {
    Ok(parked) -> park_blocks_dispatch(parked, issue)
    Error(_) -> False
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
    Error(_) -> True
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
  orchestrator_state.RuntimeState(
    ..state,
    running: dict.insert(
      state.running,
      issue.id,
      orchestrator_state.RunningEntry(
        issue: issue,
        workspace_path: workspace_path,
        session: None,
      ),
    ),
    claimed: dict.insert(state.claimed, issue.id, issue.identifier),
  )
}

pub fn apply_workflow_success(
  state: orchestrator_state.RuntimeState,
  _config: config_types.EffectiveConfig,
  issue_id: String,
  final_issue: tracker_issue.Issue,
  tokens: session_tokens.TokenTotals,
  _now_ms: Int,
  cleanup: WorkflowCleanupPolicy,
) -> Transition {
  let base = state_after_worker_exit(state, issue_id, final_issue, tokens)
  let cleanup_effect_list = case cleanup {
    AlreadyCleaned -> []
    CleanupWorkflowWorkspace(path) -> cleanup_effects(path)
  }
  Transition(
    state: release_claim(base, issue_id),
    effects: list.append(cleanup_effect_list, [ReleaseClaim(issue_id)]),
  )
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
  let workspace_path = case string.trim(workspace_path) == "" {
    True ->
      case dict.get(state.running, issue_id) {
        Ok(entry) -> entry.workspace_path
        Error(_) -> ""
      }
    False -> workspace_path
  }
  let base = state_after_worker_exit(state, issue_id, final_issue, tokens)
  case is_terminal(config, final_issue.state) {
    True ->
      Transition(
        state: release_claim(base, issue_id),
        effects: list.append(cleanup_effects(workspace_path), [
          ReleaseClaim(issue_id),
        ]),
      )
    False ->
      case is_active(config, final_issue.state) {
        False ->
          Transition(state: release_claim(base, issue_id), effects: [
            ReleaseClaim(issue_id),
          ])
        True -> continue_or_park(base, config, final_issue, now_ms)
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
  let baseline_issue = issue_with_lifecycle_id(baseline_issue, issue_id)
  let state =
    orchestrator_state.RuntimeState(
      ..state,
      running: dict.delete(state.running, issue_id),
    )
  let counter = get_counter(state, issue_id)
  let failures = counter.failure_attempts + 1
  let counter =
    orchestrator_state.IssueCounter(..counter, failure_attempts: failures)
  let state = put_counter(state, issue_id, counter)
  case failures >= config.agent.max_retry_attempts {
    True -> park(state, baseline_issue, reason.ParkMaxRetryAttempts, now_ms)
    False ->
      schedule_retry(
        state,
        issue_id,
        backoff_delay(failures, config.agent.max_retry_backoff_ms),
        reason.RetryAfterFailure,
      )
  }
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

fn continue_or_park(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
  now_ms: Int,
) -> Transition {
  let counter = get_counter(state, issue.id)
  let sessions = counter.worker_sessions + 1
  let counter =
    orchestrator_state.IssueCounter(..counter, worker_sessions: sessions)
  let state = put_counter(state, issue.id, counter)
  case sessions >= config.agent.max_sessions_per_issue {
    True -> park(state, issue, reason.ParkMaxSessionsPerIssue, now_ms)
    False ->
      schedule_retry(state, issue.id, 1000, reason.RetryAfterContinuation)
  }
}

fn cleanup_effects(workspace_path: String) -> List(Effect) {
  case string.trim(workspace_path) == "" {
    True -> []
    False -> [CleanupWorkspace(workspace_path)]
  }
}

fn state_after_worker_exit(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
  final_issue: tracker_issue.Issue,
  tokens: session_tokens.TokenTotals,
) -> orchestrator_state.RuntimeState {
  orchestrator_state.RuntimeState(
    ..state,
    running: dict.delete(state.running, issue_id),
    completed: dict.insert(state.completed, issue_id, final_issue),
    aggregate_pi_totals: add_tokens(state.aggregate_pi_totals, tokens),
  )
}

pub fn schedule_retry(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
  delay_ms: Int,
  reason: reason.RetryReason,
) -> Transition {
  let generation = case dict.get(state.retry_attempts, issue_id) {
    Ok(entry) -> entry.timer_generation + 1
    Error(_) -> 1
  }
  let retry =
    orchestrator_state.RetryEntry(
      issue_id: issue_id,
      delay_ms: delay_ms,
      timer_generation: generation,
    )
  Transition(
    state: orchestrator_state.RuntimeState(
      ..state,
      retry_attempts: dict.insert(state.retry_attempts, issue_id, retry),
    ),
    effects: [
      CancelRetry(issue_id, generation, "reschedule_retry"),
      ScheduleRetry(issue_id, delay_ms, generation, reason),
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
  Transition(
    state: release_claim(clear_retry(state, issue_id), issue_id),
    effects: [
      CancelRetry(issue_id, generation, cancel_reason),
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
  Transition(state: clear_retry(state, issue_id), effects: [
    CancelRetry(issue_id, generation, "retry_dispatch"),
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
  config: config_types.EffectiveConfig,
  issue_id: String,
) -> Int {
  let attempt = case dict.get(state.retry_attempts, issue_id) {
    Ok(entry) -> entry.timer_generation + 1
    Error(_) -> 1
  }
  backoff_delay(attempt, config.agent.max_retry_backoff_ms)
}

pub fn reconcile_issue(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  refreshed: tracker_issue.Issue,
) -> Transition {
  case dict.get(state.running, refreshed.id) {
    Error(_) -> Transition(state: state, effects: [])
    Ok(entry) ->
      case is_terminal(config, refreshed.state) {
        True ->
          Transition(
            state: release_claim(
              orchestrator_state.RuntimeState(
                ..state,
                running: dict.delete(state.running, refreshed.id),
              ),
              refreshed.id,
            ),
            effects: [
              StopWorker(refreshed.id, reason.StopTerminal),
              ..cleanup_effects(entry.workspace_path)
            ],
          )
        False ->
          case is_active(config, refreshed.state) {
            True ->
              Transition(
                state: orchestrator_state.RuntimeState(
                  ..state,
                  running: dict.insert(
                    state.running,
                    refreshed.id,
                    orchestrator_state.RunningEntry(..entry, issue: refreshed),
                  ),
                ),
                effects: [],
              )
            False ->
              Transition(
                state: release_claim(
                  orchestrator_state.RuntimeState(
                    ..state,
                    running: dict.delete(state.running, refreshed.id),
                  ),
                  refreshed.id,
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
  case dict.get(state.parked, issue.id) {
    Ok(parked) ->
      case parked.release_policy {
        orchestrator_state.ExplicitUnparkOnly -> state
        orchestrator_state.AutoUnparkOnIssueChange(stored) ->
          case tracker_issue.fingerprint_matches(stored, issue) {
            True -> state
            False ->
              orchestrator_state.RuntimeState(
                ..state,
                claimed: dict.delete(state.claimed, issue.id),
                parked: dict.delete(state.parked, issue.id),
                retry_attempts: dict.delete(state.retry_attempts, issue.id),
                issue_counters: dict.delete(state.issue_counters, issue.id),
              )
          }
      }
    Error(_) -> state
  }
}

pub fn backoff_delay(attempt: Int, max_ms: Int) -> Int {
  backoff_delay_loop(10_000, attempt - 1, max_ms)
}

fn backoff_delay_loop(
  delay_ms: Int,
  remaining_doubles: Int,
  max_ms: Int,
) -> Int {
  case delay_ms >= max_ms {
    True -> max_ms
    False ->
      case remaining_doubles <= 0 {
        True -> delay_ms
        False -> backoff_delay_loop(delay_ms * 2, remaining_doubles - 1, max_ms)
      }
  }
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

fn park(
  state: orchestrator_state.RuntimeState,
  baseline_issue: tracker_issue.Issue,
  reason: reason.ParkReason,
  now_ms: Int,
) -> Transition {
  let issue_id = baseline_issue.id
  let identifier =
    dict.get(state.claimed, issue_id)
    |> result.unwrap(baseline_issue.identifier)
  let parked =
    orchestrator_state.ParkedEntry(
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
      claimed: dict.delete(state.claimed, issue_id),
      parked: dict.insert(state.parked, issue_id, parked),
      retry_attempts: dict.delete(state.retry_attempts, issue_id),
    ),
    effects: [ParkIssue(issue_id, reason), ReleaseClaim(issue_id)],
  )
}

fn clear_retry(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
) -> orchestrator_state.RuntimeState {
  orchestrator_state.RuntimeState(
    ..state,
    retry_attempts: dict.delete(state.retry_attempts, issue_id),
  )
}

pub fn stop_retry_for_policy_invalid(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
) -> Transition {
  let generation = retry_generation(state, issue_id)
  Transition(
    state: release_claim(clear_retry(state, issue_id), issue_id),
    effects: [
      CancelRetry(issue_id, generation, "policy_invalid"),
      ReleaseClaim(issue_id),
    ],
  )
}

pub fn stop_retry_for_dependency_blocked(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
) -> Transition {
  let generation = retry_generation(state, issue_id)
  Transition(
    state: release_claim(clear_retry(state, issue_id), issue_id),
    effects: [
      CancelRetry(issue_id, generation, "linear_dependency_blocked"),
      ReleaseClaim(issue_id),
    ],
  )
}

fn retry_generation(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
) -> Int {
  case dict.get(state.retry_attempts, issue_id) {
    Ok(entry) -> entry.timer_generation
    Error(_) -> 0
  }
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
  let key = blocked_dependency_report_key(issue.id, phase)
  case dict.get(state.blocked_dependency_reports, key) {
    Error(_) -> False
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
  let key = blocked_dependency_report_key(issue.id, phase)
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
  orchestrator_state.RuntimeState(
    ..state,
    blocked_dependency_reports: dict.delete(
      state.blocked_dependency_reports,
      blocked_dependency_report_key(issue_id, phase),
    ),
  )
}

fn blocked_dependency_report_key(issue_id: String, phase: String) -> String {
  issue_id <> "|" <> phase
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
  case dict.get(state.invalid_workflow_reports, issue.id) {
    Error(_) -> False
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
        issue.id,
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
  case dict.get(state.invalid_workflow_reports, issue_id) {
    Error(_) -> state
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
              issue_id,
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
      issue_id,
    ),
  )
}

fn trim_invalid_workflow_reports(
  reports: dict.Dict(String, orchestrator_state.InvalidWorkflowReport),
) -> dict.Dict(String, orchestrator_state.InvalidWorkflowReport) {
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
  a: #(String, orchestrator_state.InvalidWorkflowReport),
  b: #(String, orchestrator_state.InvalidWorkflowReport),
) -> Order {
  let #(a_id, a_report) = a
  let #(b_id, b_report) = b
  case int.compare(b_report.attempted_at_ms, a_report.attempted_at_ms) {
    Eq -> string.compare(a_id, b_id)
    order -> order
  }
}

fn release_claim(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
) -> orchestrator_state.RuntimeState {
  orchestrator_state.RuntimeState(
    ..state,
    claimed: dict.delete(state.claimed, issue_id),
    retry_attempts: dict.delete(state.retry_attempts, issue_id),
  )
}

fn get_counter(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
) -> orchestrator_state.IssueCounter {
  dict.get(state.issue_counters, issue_id)
  |> result.unwrap(orchestrator_state.new_issue_counter())
}

fn put_counter(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
  counter: orchestrator_state.IssueCounter,
) -> orchestrator_state.RuntimeState {
  orchestrator_state.RuntimeState(
    ..state,
    issue_counters: dict.insert(state.issue_counters, issue_id, counter),
  )
}
