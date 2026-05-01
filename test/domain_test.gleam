import birl
import gleam/dict
import gleam/option.{None, Some}
import scherzo/domain
import scherzo/orchestrator/reason
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state

pub fn issue_records_labels_and_blockers_test() {
  let issue =
    domain.Issue(
      id: "issue-id",
      identifier: "ABC-123",
      title: "Fix tests",
      description: Some("Broken tests"),
      priority: Some(1),
      state: issue_state.from_string_unchecked("Todo"),
      branch_name: Some("abc-123-fix-tests"),
      url: Some("https://linear.app/example/ABC-123"),
      labels: ["bug", "tests"],
      blocked_by: [
        domain.BlockerRef(
          id: Some("blocker-id"),
          identifier: Some("ABC-1"),
          state: Some(issue_state.from_string_unchecked("Done")),
        ),
      ],
      created_at: Some(birl.from_unix(0)),
      updated_at: Some(birl.from_unix(1)),
    )

  assert issue.identifier == "ABC-123"
  assert issue.labels == ["bug", "tests"]
  let assert [blocker] = issue.blocked_by
  assert blocker.state == Some(issue_state.from_string_unchecked("Done"))
}

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

pub fn default_token_totals_are_zero_test() {
  let totals = domain.zero_token_totals()
  assert totals.input == 0
  assert totals.output == 0
  assert totals.cache_read == 0
  assert totals.cache_write == 0
  assert totals.total == 0
}

pub fn default_issue_counter_is_zero_test() {
  let counter = domain.new_issue_counter()
  assert counter.failure_attempts == 0
  assert counter.worker_sessions == 0
}

pub fn parked_issue_records_release_policy_test() {
  let parked =
    domain.ParkedEntry(
      issue_id: "issue-id",
      identifier: "ABC-123",
      reason: reason.ParkMaxRetryAttempts,
      release_policy: domain.AutoUnparkOnIssueChange("fingerprint"),
      parked_at_ms: 1000,
    )

  assert parked.release_policy == domain.AutoUnparkOnIssueChange("fingerprint")
  assert parked.reason == reason.ParkMaxRetryAttempts
  assert reason.park_to_string(parked.reason) == "max_retry_attempts"
}

pub fn runtime_state_holds_scheduler_collections_test() {
  let state =
    domain.RuntimeState(
      poll_interval_ms: 30_000,
      max_concurrent_agents: 10,
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

  assert state.poll_interval_ms == 30_000
  assert state.max_concurrent_agents == 10
}
