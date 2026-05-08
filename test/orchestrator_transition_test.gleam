import gleam/dict
import gleam/option.{None}
import scherzo/orchestrator/state as orchestrator_state
import scherzo/orchestrator/transition
import scherzo/orchestrator/transition_types
import scherzo/session/tokens as session_tokens
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

pub fn fixture_issue() -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "issue-1",
    identifier: "ABC-1",
    title: "Implement transition kernel",
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked("Todo"),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}

pub fn fixture_runtime() -> orchestrator_state.RuntimeState {
  orchestrator_state.RuntimeState(
    poll_interval_ms: 30_000,
    max_concurrent_agents: 2,
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

pub fn fixture_state() -> transition_types.State {
  transition_types.State(
    runtime: fixture_runtime(),
    workers: transition_types.new_worker_directory(),
    pending_claims: dict.new(),
  )
}

pub fn state_with_pending_claim(
  issue: tracker_issue.Issue,
) -> transition_types.State {
  let state = fixture_state()
  transition_types.State(
    ..state,
    pending_claims: dict.insert(
      state.pending_claims,
      issue.id,
      transition_types.PendingClaim(
        issue_id: issue.id,
        run_id: "run-1",
        session_id: "session-1",
        workspace_path: "test/tmp/workspaces/ABC-1",
        workflow_id: "default",
        command_route_id: "worker:run-1:1",
        route_label: issue.identifier,
        issue: issue,
        recovery: None,
        remaining_candidates: [],
      ),
    ),
  )
}

pub fn snapshot_returns_runtime_state_test() {
  let state = fixture_state()

  assert transition.snapshot(state) == state.runtime
}
