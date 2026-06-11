import gleam/dict
import gleam/option.{None}
import gleam/string
import orchestrator_transition_invariant_helpers as invariant_helpers
import orchestrator_transition_test
import scherzo/orchestrator/effects/interpreter
import scherzo/orchestrator/task_lifecycle
import scherzo/orchestrator/transition_invariants
import scherzo/orchestrator/transition_runner
import scherzo/orchestrator/transition_types
import scherzo/runtime/identity
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
import scherzo/session/reason as session_reason
import scherzo/task
import scherzo/tracker/issue as tracker_issue

pub fn fixture_state_satisfies_transition_invariants_test() {
  invariant_helpers.assert_valid_state(
    orchestrator_transition_test.fixture_state(),
  )
}

pub fn completed_history_can_coexist_with_running_continuation_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = state_with_running_worker(issue)
  let task_identity = orchestrator_state.issue_identity(issue)
  let state =
    transition_types.State(
      ..state,
      runtime: orchestrator_state.RuntimeState(
        ..state.runtime,
        completed: dict.from_list([#(task_identity, issue)]),
      ),
    )

  invariant_helpers.assert_valid_state(state)
}

pub fn running_and_parked_conflict_reports_clear_error_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = state_with_running_worker(issue)
  let task_ref = task.from_legacy_issue(issue).ref
  let task_identity = orchestrator_state.task_ref_identity(task_ref)
  let parked =
    orchestrator_state.ParkedEntry(
      task_ref: task_ref,
      issue_id: issue.id,
      identifier: issue.identifier,
      reason: orchestrator_reason.ParkOperator("operator_hold"),
      release_policy: orchestrator_state.ExplicitUnparkOnly,
      parked_at_ms: 123,
    )
  let state =
    transition_types.State(
      ..state,
      runtime: orchestrator_state.RuntimeState(
        ..state.runtime,
        parked: dict.insert(state.runtime.parked, task_identity, parked),
      ),
    )

  invariant_helpers.assert_state_error(state, "running_parked_conflict")
  let report = case transition_invariants.check(state) {
    Error(errors) -> transition_invariants.format_errors(errors)
    Ok(Nil) -> ""
  }
  assert string.contains(report, "running_parked_conflict")
  assert string.contains(report, "runtime.running and runtime.parked")
}

pub fn retry_without_claim_reports_mismatch_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let task_ref = task.from_legacy_issue(issue).ref
  let task_identity = orchestrator_state.task_ref_identity(task_ref)
  let runtime =
    orchestrator_state.RuntimeState(
      ..orchestrator_transition_test.fixture_runtime(),
      retry_attempts: dict.from_list([
        #(
          task_identity,
          orchestrator_state.RetryEntry(
            task_ref: task_ref,
            issue_id: issue.id,
            delay_ms: 1000,
            timer_generation: 1,
          ),
        ),
      ]),
    )

  invariant_helpers.assert_runtime_error(runtime, "retry_claim_missing")
}

pub fn worker_directory_index_drift_reports_clear_error_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = state_with_running_worker(issue)
  let wrong_issue =
    tracker_issue.Issue(..issue, id: "issue-2", identifier: "ABC-2")
  let wrong_identity = orchestrator_state.issue_identity(wrong_issue)
  let workers =
    transition_types.WorkerDirectory(
      ..state.workers,
      by_session: dict.from_list([#("session-1", wrong_identity)]),
    )
  let state = transition_types.State(..state, workers: workers)

  invariant_helpers.assert_state_error(state, "worker_session_index_drift")
}

pub fn yaml_step_run_dangling_reports_clear_error_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = state_with_running_worker(issue)
  let workers =
    transition_types.WorkerDirectory(
      ..state.workers,
      yaml_step_runs: dict.from_list([#("step-session", "missing-run")]),
    )
  let state = transition_types.State(..state, workers: workers)

  invariant_helpers.assert_state_error(state, "yaml_step_run_dangling")
}

pub fn stopped_yaml_run_active_worker_conflict_reports_clear_error_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = state_with_running_worker(issue)
  let workers =
    transition_types.WorkerDirectory(
      ..state.workers,
      stopped_yaml_runs: dict.from_list([
        #("run-1", session_reason.OperatorStopAfterCurrentTurn),
      ]),
    )
  let state = transition_types.State(..state, workers: workers)

  invariant_helpers.assert_state_error(
    state,
    "stopped_yaml_run_active_worker_conflict",
  )
}

pub fn pending_dispatch_validation_generation_unreserved_reports_clear_error_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = state_with_pending_dispatch_validation(issue, 1, 1)

  invariant_helpers.assert_state_error(
    state,
    "pending_dispatch_validation_generation_unreserved",
  )
}

pub fn pending_dispatch_validation_claimed_conflict_reports_clear_error_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = state_with_pending_dispatch_validation(issue, 1, 2)
  let task_identity = orchestrator_state.issue_identity(issue)
  let state =
    transition_types.State(
      ..state,
      runtime: orchestrator_state.RuntimeState(
        ..state.runtime,
        claimed: dict.from_list([#(task_identity, issue.identifier)]),
      ),
    )

  invariant_helpers.assert_state_error(
    state,
    "pending_dispatch_validation_claimed_conflict",
  )
}

pub fn pending_review_lane_preflight_generation_unreserved_reports_clear_error_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = state_with_pending_review_lane_preflight(issue, 1, 1)

  invariant_helpers.assert_state_error(
    state,
    "pending_review_lane_preflight_generation_unreserved",
  )
}

pub fn pending_review_lane_preflight_claimed_retry_state_is_allowed_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = state_with_pending_review_lane_preflight(issue, 1, 2)
  let task_identity = orchestrator_state.issue_identity(issue)
  let state =
    transition_types.State(
      ..state,
      runtime: orchestrator_state.RuntimeState(
        ..state.runtime,
        claimed: dict.from_list([#(task_identity, issue.identifier)]),
      ),
    )

  invariant_helpers.assert_valid_state(state)
}

pub fn pending_review_lane_preflight_counts_toward_slot_overcommit_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = state_with_pending_review_lane_preflight(issue, 1, 2)
  let state =
    transition_types.State(
      ..state,
      runtime: orchestrator_state.RuntimeState(
        ..state.runtime,
        max_concurrent_agents: 0,
      ),
    )

  invariant_helpers.assert_state_error(state, "pending_slot_overcommit")
}

pub fn pending_slot_overcommit_reports_clear_error_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let other_issue =
    tracker_issue.Issue(..issue, id: "issue-2", identifier: "ABC-2")
  let state = orchestrator_transition_test.state_with_pending_claim(issue)
  let other_ref = task.from_legacy_issue(other_issue).ref
  let other_identity = orchestrator_state.task_ref_identity(other_ref)
  let other_pending =
    transition_types.PendingClaim(
      task_ref: other_ref,
      issue_id: other_issue.id,
      run_id: "run-2",
      session_id: "session-2",
      workspace_path: "test/tmp/workspaces/ABC-2",
      workflow_id: "default",
      command_route_id: "worker:run-2:2",
      route_label: other_issue.identifier,
      issue: other_issue,
      recovery: None,
      remaining_candidates: [],
      dispatch_context: orchestrator_transition_test.fixture_context(),
      previous_retry_generation: 0,
      retry_cancellation: None,
    )
  let state =
    transition_types.State(
      ..state,
      runtime: orchestrator_state.RuntimeState(
        ..state.runtime,
        max_concurrent_agents: 1,
      ),
      pending_claims: dict.insert(
        state.pending_claims,
        other_identity,
        other_pending,
      ),
    )

  invariant_helpers.assert_state_error(state, "pending_slot_overcommit")
}

pub fn stale_lifecycle_message_no_op_preserves_invariants_test() {
  let state = orchestrator_transition_test.fixture_state()
  let transition_runner.RunResult(state: next, shell: _, exhausted: exhausted) =
    invariant_helpers.run_and_assert(
      state: state,
      shell: interpreter.new_shell_state(fn(_) { Ok(Nil) }, fn() { 123 }),
      messages: [
        transition_types.WorkerDown(
          transition_types.WorkerDownStale(identity.issue_id_from_string(
            "issue-1",
          )),
          transition_types.WorkerLifecycleContext(
            effective: orchestrator_transition_test.fixture_effective(),
            now_ms: 123,
            secrets: [],
          ),
        ),
      ],
      max_messages: 4,
    )

  assert exhausted == False
  assert next == state
}

fn state_with_pending_dispatch_validation(
  issue: tracker_issue.Issue,
  generation: Int,
  next_generation: Int,
) -> transition_types.State {
  let task_ref = task.from_legacy_issue(issue).ref
  transition_types.State(
    ..orchestrator_transition_test.fixture_state(),
    pending_dispatch_validations: dict.from_list([
      #(
        orchestrator_state.task_ref_identity(task_ref),
        transition_types.PendingDispatchValidation(
          task_ref: task_ref,
          issue: issue,
          remaining_candidates: [],
          generation: generation,
          requested_at_ms: 123,
        ),
      ),
    ]),
    lifecycle: validating_lifecycle(task_ref, issue, generation),
    next_dispatch_validation_generation: next_generation,
  )
}

fn state_with_pending_review_lane_preflight(
  issue: tracker_issue.Issue,
  generation: Int,
  next_generation: Int,
) -> transition_types.State {
  let task_ref = task.from_legacy_issue(issue).ref
  transition_types.State(
    ..orchestrator_transition_test.fixture_state(),
    pending_review_lane_preflights: dict.from_list([
      #(
        orchestrator_state.task_ref_identity(task_ref),
        transition_types.PendingReviewLanePreflight(
          task_ref: task_ref,
          issue: issue,
          remaining_candidates: [],
          generation: generation,
          workflow_id: "default",
          previous_retry_generation: 0,
          retry_cancellation: None,
        ),
      ),
    ]),
    lifecycle: validating_lifecycle(task_ref, issue, generation),
    next_dispatch_validation_generation: next_generation,
  )
}

fn validating_lifecycle(
  task_ref: task.TaskRef,
  issue: tracker_issue.Issue,
  generation: Int,
) -> task_lifecycle.TaskDirectory {
  let assert Ok(directory) =
    task_lifecycle.put(
      task_lifecycle.new(),
      task_lifecycle.Validating(
        task_ref: task_ref,
        issue: issue,
        generation: generation,
      ),
    )
  directory
}

fn state_with_running_worker(
  issue: tracker_issue.Issue,
) -> transition_types.State {
  let task_ref = task.from_legacy_issue(issue).ref
  let task_identity = orchestrator_state.task_ref_identity(task_ref)
  let runtime =
    orchestrator_state.RuntimeState(
      ..orchestrator_transition_test.fixture_runtime(),
      running: dict.from_list([
        #(
          task_identity,
          orchestrator_state.RunningEntry(
            task: task.from_legacy_issue(issue),
            issue: issue,
            workspace_path: "test/tmp/workspaces/ABC-1",
            session: None,
          ),
        ),
      ]),
      claimed: dict.from_list([#(task_identity, issue.identifier)]),
    )
  let entry =
    transition_types.WorkerEntry(
      task_ref: task_ref,
      issue_id: issue.id,
      run_id: "run-1",
      session_id: "session-1",
      issue: issue,
      workspace_path: "test/tmp/workspaces/ABC-1",
      workflow_id: "default",
      command_route_id: "worker:run-1:1",
      status: transition_types.WorkerRunning,
      recovery: None,
    )
  transition_types.State(
    ..orchestrator_transition_test.fixture_state(),
    runtime: runtime,
    workers: transition_types.WorkerDirectory(
      by_issue: dict.from_list([#(task_identity, entry)]),
      by_session: dict.from_list([#("session-1", task_identity)]),
      route_to_session: dict.from_list([#("worker:run-1:1", "session-1")]),
      yaml_step_runs: dict.new(),
      stopped_yaml_runs: dict.new(),
    ),
  )
}
