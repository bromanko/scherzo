import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq, Gt, Lt}
import gleam/string
import scherzo/domain
import scherzo/orchestrator/reason
import scherzo/tracker/state as issue_state
import scherzo/workflow_policy

const invalid_workflow_report_cache_limit = 1024

pub type Effect {
  Dispatch(domain.Issue)
  ScheduleRetry(
    issue_id: String,
    delay_ms: Int,
    generation: Int,
    reason: reason.RetryReason,
  )
  CancelRetry(issue_id: String)
  CleanupWorkspace(path: String)
  ReleaseClaim(issue_id: String)
  StopWorker(issue_id: String, reason: reason.StopReason)
  ParkIssue(issue_id: String, reason: reason.ParkReason)
}

pub type Transition {
  Transition(state: domain.RuntimeState, effects: List(Effect))
}

pub type WorkflowCleanupPolicy {
  AlreadyCleaned
  CleanupWorkflowWorkspace(String)
}

pub fn new_state(config: domain.EffectiveConfig) -> domain.RuntimeState {
  domain.RuntimeState(
    poll_interval_ms: config.polling.interval_ms,
    max_concurrent_agents: config.agent.max_concurrent_agents,
    running: dict.new(),
    claimed: dict.new(),
    retry_attempts: dict.new(),
    issue_counters: dict.new(),
    parked: dict.new(),
    invalid_workflow_reports: dict.new(),
    completed: dict.new(),
    aggregate_pi_totals: domain.zero_token_totals(),
    latest_rate_limit_payload: None,
  )
}

pub fn sort_candidates(issues: List(domain.Issue)) -> List(domain.Issue) {
  list.sort(issues, by: compare_issue)
}

fn compare_issue(a: domain.Issue, b: domain.Issue) -> Order {
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

pub fn issue_fingerprint(issue: domain.Issue) -> String {
  [
    encode_string(issue.id),
    encode_string(issue.identifier),
    encode_string(issue.title),
    encode_optional_string(issue.description),
    encode_optional_int(issue.priority),
    encode_string(issue_state.to_string(issue.state)),
    encode_optional_string(issue.branch_name),
    blocker_fingerprint(issue.blocked_by),
  ]
  |> string.join(with: "|")
}

fn encode_string(value: String) -> String {
  int.to_string(string.length(value)) <> ":" <> value
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

fn encode_optional_int(value: Option(Int)) -> String {
  case value {
    None -> "none"
    Some(value) -> "some:" <> encode_string(int.to_string(value))
  }
}

fn blocker_fingerprint(blockers: List(domain.BlockerRef)) -> String {
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
  state: domain.RuntimeState,
  config: domain.EffectiveConfig,
  issue: domain.Issue,
) -> Bool {
  dispatch_preconditions_satisfied(state, config, issue)
  && workflow_policy_satisfied(config, issue)
}

pub fn dispatch_preconditions_satisfied(
  state: domain.RuntimeState,
  config: domain.EffectiveConfig,
  issue: domain.Issue,
) -> Bool {
  dispatch_preconditions_satisfied_without_slot_capacity(state, config, issue)
  && slots_available(state, config, issue.state)
}

pub fn dispatch_preconditions_satisfied_without_slot_capacity(
  state: domain.RuntimeState,
  config: domain.EffectiveConfig,
  issue: domain.Issue,
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
  config: domain.EffectiveConfig,
  issue: domain.Issue,
) -> Bool {
  workflow_policy.classify_issue(config.linear_contract, issue)
  |> workflow_policy.workflow_satisfied
}

pub fn is_active(
  config: domain.EffectiveConfig,
  state: issue_state.IssueState,
) -> Bool {
  contains_normalized(config.tracker.active_states, state)
}

pub fn is_terminal(
  config: domain.EffectiveConfig,
  state: issue_state.IssueState,
) -> Bool {
  contains_normalized(config.tracker.terminal_states, state)
}

pub fn retry_candidate_preconditions_satisfied(
  state: domain.RuntimeState,
  config: domain.EffectiveConfig,
  issue_id: String,
  issue: domain.Issue,
) -> Bool {
  retry_candidate_preconditions_satisfied_without_slot_capacity(
    state,
    config,
    issue_id,
    issue,
  )
  && slots_available(state, config, issue.state)
}

pub fn retry_candidate_preconditions_satisfied_without_slot_capacity(
  state: domain.RuntimeState,
  config: domain.EffectiveConfig,
  issue_id: String,
  issue: domain.Issue,
) -> Bool {
  issue.id == issue_id
  && issue_has_required_fields(issue)
  && is_active(config, issue.state)
  && !is_terminal(config, issue.state)
  && !dict.has_key(state.running, issue.id)
  && retry_claim_allowed(state, issue.id)
  && !is_parked_for_issue(state, issue)
  && blockers_satisfied(config, issue)
}

fn retry_claim_allowed(state: domain.RuntimeState, issue_id: String) -> Bool {
  case dict.has_key(state.claimed, issue_id) {
    False -> True
    True -> dict.has_key(state.retry_attempts, issue_id)
  }
}

fn issue_has_required_fields(issue: domain.Issue) -> Bool {
  string.trim(issue.id) != ""
  && string.trim(issue.identifier) != ""
  && string.trim(issue.title) != ""
  && string.trim(issue_state.to_string(issue.state)) != ""
}

fn is_parked_for_issue(
  state: domain.RuntimeState,
  issue: domain.Issue,
) -> Bool {
  case dict.get(state.parked, issue.id) {
    Ok(parked) -> park_blocks_dispatch(parked, issue)
    Error(_) -> False
  }
}

fn park_blocks_dispatch(
  parked: domain.ParkedEntry,
  issue: domain.Issue,
) -> Bool {
  case parked.release_policy {
    domain.ExplicitUnparkOnly -> True
    domain.AutoUnparkOnIssueChange(stored) -> stored == issue_fingerprint(issue)
  }
}

fn slots_available(
  state: domain.RuntimeState,
  config: domain.EffectiveConfig,
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
  state: domain.RuntimeState,
  config: domain.EffectiveConfig,
  issue_state_value: issue_state.IssueState,
) -> Bool {
  let key = issue_state.key(issue_state_value)
  case dict.get(config.agent.max_concurrent_agents_by_state, key) {
    Error(_) -> True
    Ok(limit) -> running_count_for_state(state, key) < limit
  }
}

fn running_count_for_state(
  state: domain.RuntimeState,
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
  config: domain.EffectiveConfig,
  issue: domain.Issue,
) -> Bool {
  case issue_state.equals_key(issue.state, issue_state.todo_key()) {
    False -> True
    True ->
      issue.blocked_by
      |> list.all(fn(blocker) {
        case blocker.state {
          Some(state) -> is_terminal(config, state)
          None -> False
        }
      })
  }
}

pub fn apply_worker_start(
  state: domain.RuntimeState,
  issue: domain.Issue,
  workspace_path: String,
) -> domain.RuntimeState {
  domain.RuntimeState(
    ..state,
    running: dict.insert(
      state.running,
      issue.id,
      domain.RunningEntry(
        issue: issue,
        workspace_path: workspace_path,
        session: None,
      ),
    ),
    claimed: dict.insert(state.claimed, issue.id, issue.identifier),
  )
}

pub fn apply_workflow_success(
  state: domain.RuntimeState,
  _config: domain.EffectiveConfig,
  issue_id: String,
  final_issue: domain.Issue,
  tokens: domain.TokenTotals,
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
  state: domain.RuntimeState,
  config: domain.EffectiveConfig,
  issue_id: String,
  final_issue: domain.Issue,
  tokens: domain.TokenTotals,
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
  state: domain.RuntimeState,
  config: domain.EffectiveConfig,
  issue_id: String,
  final_issue: domain.Issue,
  workspace_path: String,
  tokens: domain.TokenTotals,
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
  state: domain.RuntimeState,
  config: domain.EffectiveConfig,
  issue_id: String,
  baseline_issue: domain.Issue,
  now_ms: Int,
) -> Transition {
  let baseline_issue = issue_with_lifecycle_id(baseline_issue, issue_id)
  let state =
    domain.RuntimeState(..state, running: dict.delete(state.running, issue_id))
  let counter = get_counter(state, issue_id)
  let failures = counter.failure_attempts + 1
  let counter = domain.IssueCounter(..counter, failure_attempts: failures)
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
  issue: domain.Issue,
  issue_id: String,
) -> domain.Issue {
  case issue.id == issue_id {
    True -> issue
    False -> domain.Issue(..issue, id: issue_id)
  }
}

fn continue_or_park(
  state: domain.RuntimeState,
  config: domain.EffectiveConfig,
  issue: domain.Issue,
  now_ms: Int,
) -> Transition {
  let counter = get_counter(state, issue.id)
  let sessions = counter.worker_sessions + 1
  let counter = domain.IssueCounter(..counter, worker_sessions: sessions)
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
  state: domain.RuntimeState,
  issue_id: String,
  final_issue: domain.Issue,
  tokens: domain.TokenTotals,
) -> domain.RuntimeState {
  domain.RuntimeState(
    ..state,
    running: dict.delete(state.running, issue_id),
    completed: dict.insert(state.completed, issue_id, final_issue),
    aggregate_pi_totals: add_tokens(state.aggregate_pi_totals, tokens),
  )
}

pub fn schedule_retry(
  state: domain.RuntimeState,
  issue_id: String,
  delay_ms: Int,
  reason: reason.RetryReason,
) -> Transition {
  let generation = case dict.get(state.retry_attempts, issue_id) {
    Ok(entry) -> entry.timer_generation + 1
    Error(_) -> 1
  }
  let retry =
    domain.RetryEntry(
      issue_id: issue_id,
      delay_ms: delay_ms,
      timer_generation: generation,
    )
  Transition(
    state: domain.RuntimeState(
      ..state,
      retry_attempts: dict.insert(state.retry_attempts, issue_id, retry),
    ),
    effects: [
      CancelRetry(issue_id),
      ScheduleRetry(issue_id, delay_ms, generation, reason),
    ],
  )
}

pub fn handle_retry_candidate(
  state: domain.RuntimeState,
  config: domain.EffectiveConfig,
  issue_id: String,
  candidate: Result(Option(domain.Issue), String),
) -> Transition {
  case candidate {
    Error(_) -> schedule_retry(state, issue_id, 1000, reason.RetryPollFailed)
    Ok(None) ->
      Transition(
        state: release_claim(clear_retry(state, issue_id), issue_id),
        effects: [ReleaseClaim(issue_id)],
      )
    Ok(Some(issue)) -> {
      let state = unpark_if_issue_changed(state, issue)
      case
        retry_candidate_preconditions_satisfied(state, config, issue_id, issue)
        && workflow_policy_satisfied(config, issue)
      {
        True ->
          Transition(state: clear_retry(state, issue_id), effects: [
            Dispatch(issue),
          ])
        False -> schedule_retry(state, issue_id, 1000, reason.RetryNoSlots)
      }
    }
  }
}

pub fn reconcile_issue(
  state: domain.RuntimeState,
  config: domain.EffectiveConfig,
  refreshed: domain.Issue,
) -> Transition {
  case dict.get(state.running, refreshed.id) {
    Error(_) -> Transition(state: state, effects: [])
    Ok(entry) ->
      case is_terminal(config, refreshed.state) {
        True ->
          Transition(
            state: release_claim(
              domain.RuntimeState(
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
                state: domain.RuntimeState(
                  ..state,
                  running: dict.insert(
                    state.running,
                    refreshed.id,
                    domain.RunningEntry(..entry, issue: refreshed),
                  ),
                ),
                effects: [],
              )
            False ->
              Transition(
                state: release_claim(
                  domain.RuntimeState(
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
  state: domain.RuntimeState,
  issue: domain.Issue,
) -> domain.RuntimeState {
  case dict.get(state.parked, issue.id) {
    Ok(parked) ->
      case parked.release_policy {
        domain.ExplicitUnparkOnly -> state
        domain.AutoUnparkOnIssueChange(stored) ->
          case stored == issue_fingerprint(issue) {
            True -> state
            False ->
              domain.RuntimeState(
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
  let base = 10_000 * int_power(2, attempt - 1)
  case base > max_ms {
    True -> max_ms
    False -> base
  }
}

fn int_power(base: Int, exponent: Int) -> Int {
  case exponent <= 0 {
    True -> 1
    False -> base * int_power(base, exponent - 1)
  }
}

pub fn add_tokens(
  a: domain.TokenTotals,
  b: domain.TokenTotals,
) -> domain.TokenTotals {
  domain.TokenTotals(
    input: a.input + b.input,
    output: a.output + b.output,
    cache_read: a.cache_read + b.cache_read,
    cache_write: a.cache_write + b.cache_write,
    total: a.total + b.total,
  )
}

fn park(
  state: domain.RuntimeState,
  baseline_issue: domain.Issue,
  reason: reason.ParkReason,
  now_ms: Int,
) -> Transition {
  let issue_id = baseline_issue.id
  let identifier =
    dict.get(state.claimed, issue_id)
    |> result_unwrap(baseline_issue.identifier)
  let parked =
    domain.ParkedEntry(
      issue_id: issue_id,
      identifier: identifier,
      reason: reason,
      release_policy: domain.AutoUnparkOnIssueChange(issue_fingerprint(
        baseline_issue,
      )),
      parked_at_ms: now_ms,
    )
  Transition(
    state: domain.RuntimeState(
      ..state,
      claimed: dict.delete(state.claimed, issue_id),
      parked: dict.insert(state.parked, issue_id, parked),
      retry_attempts: dict.delete(state.retry_attempts, issue_id),
    ),
    effects: [ParkIssue(issue_id, reason), ReleaseClaim(issue_id)],
  )
}

fn clear_retry(
  state: domain.RuntimeState,
  issue_id: String,
) -> domain.RuntimeState {
  domain.RuntimeState(
    ..state,
    retry_attempts: dict.delete(state.retry_attempts, issue_id),
  )
}

pub fn stop_retry_for_policy_invalid(
  state: domain.RuntimeState,
  issue_id: String,
) -> Transition {
  Transition(
    state: release_claim(clear_retry(state, issue_id), issue_id),
    effects: [CancelRetry(issue_id), ReleaseClaim(issue_id)],
  )
}

pub fn already_attempted_invalid_workflow(
  state: domain.RuntimeState,
  issue: domain.Issue,
  violation: workflow_policy.IssueWorkflowViolation,
  config: domain.LinearContractConfig,
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
  state: domain.RuntimeState,
  issue: domain.Issue,
  violation: workflow_policy.IssueWorkflowViolation,
  config: domain.LinearContractConfig,
  now_ms: Int,
) -> domain.RuntimeState {
  let report =
    domain.InvalidWorkflowReport(
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
  domain.RuntimeState(
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
  state: domain.RuntimeState,
  issue_id: String,
  violation_fingerprint: String,
  reporting_policy_fingerprint: String,
  last_result: String,
) -> domain.RuntimeState {
  case dict.get(state.invalid_workflow_reports, issue_id) {
    Error(_) -> state
    Ok(report) ->
      case
        report.violation_fingerprint == violation_fingerprint
        && report.reporting_policy_fingerprint == reporting_policy_fingerprint
      {
        False -> state
        True ->
          domain.RuntimeState(
            ..state,
            invalid_workflow_reports: dict.insert(
              state.invalid_workflow_reports,
              issue_id,
              domain.InvalidWorkflowReport(..report, last_result: last_result),
            ),
          )
      }
  }
}

pub fn clear_invalid_workflow_report(
  state: domain.RuntimeState,
  issue_id: String,
) -> domain.RuntimeState {
  domain.RuntimeState(
    ..state,
    invalid_workflow_reports: dict.delete(
      state.invalid_workflow_reports,
      issue_id,
    ),
  )
}

fn trim_invalid_workflow_reports(
  reports: dict.Dict(String, domain.InvalidWorkflowReport),
) -> dict.Dict(String, domain.InvalidWorkflowReport) {
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
  a: #(String, domain.InvalidWorkflowReport),
  b: #(String, domain.InvalidWorkflowReport),
) -> Order {
  let #(a_id, a_report) = a
  let #(b_id, b_report) = b
  case int.compare(b_report.attempted_at_ms, a_report.attempted_at_ms) {
    Eq -> string.compare(a_id, b_id)
    order -> order
  }
}

fn release_claim(
  state: domain.RuntimeState,
  issue_id: String,
) -> domain.RuntimeState {
  domain.RuntimeState(
    ..state,
    claimed: dict.delete(state.claimed, issue_id),
    retry_attempts: dict.delete(state.retry_attempts, issue_id),
  )
}

fn get_counter(
  state: domain.RuntimeState,
  issue_id: String,
) -> domain.IssueCounter {
  dict.get(state.issue_counters, issue_id)
  |> result_unwrap(domain.new_issue_counter())
}

fn put_counter(
  state: domain.RuntimeState,
  issue_id: String,
  counter: domain.IssueCounter,
) -> domain.RuntimeState {
  domain.RuntimeState(
    ..state,
    issue_counters: dict.insert(state.issue_counters, issue_id, counter),
  )
}

fn contains_normalized(
  states: List(issue_state.IssueState),
  state: issue_state.IssueState,
) -> Bool {
  list.any(states, fn(s) { issue_state.equals_normalized(s, state) })
}

fn result_unwrap(result: Result(a, b), default: a) -> a {
  case result {
    Ok(value) -> value
    Error(_) -> default
  }
}
