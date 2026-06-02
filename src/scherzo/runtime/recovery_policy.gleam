import gleam/dict.{type Dict}
import gleam/list
import scherzo/config/types as config_types
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
