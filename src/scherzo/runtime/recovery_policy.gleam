import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import scherzo/config/types as config_types
import scherzo/retry_policy
import scherzo/runtime/state as runtime_state
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

pub fn new_state(
  config: config_types.EffectiveConfig,
) -> runtime_state.RuntimeState {
  runtime_state.new(config)
}

pub fn issues_by_id(
  issues: List(tracker_issue.Issue),
) -> Dict(String, tracker_issue.Issue) {
  issues
  |> list.map(fn(issue) { #(issue.id, issue) })
  |> dict.from_list
}

pub fn issue_fingerprint(issue: tracker_issue.Issue) -> String {
  tracker_issue.content_fingerprint(issue)
}

pub fn is_active(
  config: config_types.EffectiveConfig,
  state: issue_state.IssueState,
) -> Bool {
  issue_state.contains_normalized(config.tracker.active_states, state)
}

pub fn is_terminal(
  config: config_types.EffectiveConfig,
  state: issue_state.IssueState,
) -> Bool {
  issue_state.contains_normalized(config.tracker.terminal_states, state)
}

pub fn backoff_delay(attempt: Int, max_ms: Int) -> Int {
  retry_policy.backoff_delay(attempt, max_ms)
}

pub fn first_attempt_index() -> Int {
  retry_policy.first_attempt_index()
}

pub fn next_attempt_index(current_attempt_index: Int) -> Int {
  retry_policy.next_attempt_index(current_attempt_index)
}

pub fn completed_attempts_exhausted(
  completed_attempt_count: Int,
  max_attempt_count: Int,
) -> Bool {
  retry_policy.completed_attempts_exhausted(
    completed_attempt_count,
    max_attempt_count,
  )
}

pub fn next_generation(current_generation: Option(Int)) -> Int {
  retry_policy.next_generation(current_generation)
}
