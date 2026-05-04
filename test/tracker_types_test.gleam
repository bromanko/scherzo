import gleam/dict
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state

pub fn tracker_kind_parses_case_insensitively_test() {
  assert tracker_kind.from_string("linear") == Ok(tracker_kind.LinearTracker)
  assert tracker_kind.from_string(" LINEAR ") == Ok(tracker_kind.LinearTracker)
  assert tracker_kind.from_string("github") == Error(Nil)
  assert tracker_kind.to_string(tracker_kind.LinearTracker) == "linear"
}

pub fn issue_state_keeps_display_text_and_normalizes_keys_test() {
  let todo_issue_state = issue_state.from_string_unchecked("Todo")
  let spaced = issue_state.from_string_unchecked(" todo ")

  assert issue_state.to_string(todo_issue_state) == "Todo"
  assert issue_state.to_string(spaced) == "todo"
  assert issue_state.key(todo_issue_state) == issue_state.key(spaced)
  assert issue_state.equals_key(todo_issue_state, issue_state.todo_key())
  assert issue_state.key_to_string(issue_state.key(todo_issue_state)) == "todo"
}

pub fn issue_state_keys_drive_policy_maps_test() {
  let limits = dict.from_list([#(issue_state.key_from_string("todo"), 2)])
  let issue = issue_state.from_string_unchecked("Todo")

  assert dict.get(limits, issue_state.key(issue)) == Ok(2)
}
