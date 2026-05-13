import gleam/dict
import gleam/option.{None}
import scherzo/orchestrator/reason
import scherzo/orchestrator/state as orchestrator_state
import scherzo/session/tokens as session_tokens

pub fn default_issue_counter_is_zero_test() {
  let counter = orchestrator_state.new_issue_counter()
  assert counter.failure_attempts == 0
  assert counter.worker_sessions == 0
}

pub fn parked_issue_records_release_policy_test() {
  let parked =
    orchestrator_state.ParkedEntry(
      issue_id: "issue-id",
      identifier: "ABC-123",
      reason: reason.ParkMaxRetryAttempts,
      release_policy: orchestrator_state.AutoUnparkOnIssueChange("fingerprint"),
      parked_at_ms: 1000,
    )

  assert parked.release_policy
    == orchestrator_state.AutoUnparkOnIssueChange("fingerprint")
  assert parked.reason == reason.ParkMaxRetryAttempts
  assert reason.park_to_string(parked.reason) == "max_retry_attempts"
}

pub fn runtime_state_holds_scheduler_collections_test() {
  let state =
    orchestrator_state.RuntimeState(
      poll_interval_ms: 30_000,
      max_concurrent_agents: 10,
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

  assert state.poll_interval_ms == 30_000
  assert state.max_concurrent_agents == 10
}
