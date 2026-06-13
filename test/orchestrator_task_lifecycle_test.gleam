import gleam/dict
import gleam/option.{None, Some}
import orchestrator_transition_test
import scherzo/orchestrator/retry_scheduler
import scherzo/orchestrator/task_lifecycle
import scherzo/orchestrator/task_lifecycle_legacy
import scherzo/orchestrator/transition
import scherzo/orchestrator/transition_types
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
import scherzo/task
import scherzo/tracker/issue as tracker_issue

pub fn lifecycle_queries_and_counts_cover_each_state_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let task_ref = task.from_legacy_issue(issue).ref
  let directory = task_lifecycle.new()
  let assert Ok(directory) =
    task_lifecycle.put(
      directory,
      task_lifecycle.Validating(task_ref: task_ref, issue: issue, generation: 1),
    )
  let assert Ok(directory) =
    task_lifecycle.put(
      directory,
      task_lifecycle.Claiming(
        task_ref: task.TaskRef(
          ..task_ref,
          remote_id: "issue-2",
          key: Some("ABC-2"),
        ),
        issue: tracker_issue.Issue(..issue, id: "issue-2", identifier: "ABC-2"),
        run_id: "run-2",
        session_id: "session-2",
      ),
    )
  let assert Ok(directory) =
    task_lifecycle.put(
      directory,
      task_lifecycle.Running(
        task_ref: task.TaskRef(
          ..task_ref,
          remote_id: "issue-3",
          key: Some("ABC-3"),
        ),
        issue: tracker_issue.Issue(..issue, id: "issue-3", identifier: "ABC-3"),
        run_id: "run-3",
        session_id: "session-3",
        workspace_path: "test/tmp/workspaces/ABC-3",
      ),
    )
  let assert Ok(directory) =
    task_lifecycle.put(
      directory,
      task_lifecycle.RetryRefreshing(
        task_ref: task.TaskRef(
          ..task_ref,
          remote_id: "issue-4",
          key: Some("ABC-4"),
        ),
        issue_id: "issue-4",
        generation: 4,
        delay_ms: 1000,
      ),
    )
  let assert Ok(directory) =
    task_lifecycle.put(
      directory,
      task_lifecycle.Parked(
        task_ref: task.TaskRef(
          ..task_ref,
          remote_id: "issue-5",
          key: Some("ABC-5"),
        ),
        issue_id: "issue-5",
        identifier: "ABC-5",
      ),
    )
  let assert Ok(directory) =
    task_lifecycle.put(
      directory,
      task_lifecycle.Completed(
        task_ref: task.TaskRef(
          ..task_ref,
          remote_id: "issue-6",
          key: Some("ABC-6"),
        ),
        issue: tracker_issue.Issue(..issue, id: "issue-6", identifier: "ABC-6"),
      ),
    )

  assert task_lifecycle.size(directory) == 6
  let counts = task_lifecycle.counts(directory)
  assert counts.validating == 1
  assert counts.claiming == 1
  assert counts.running == 1
  assert counts.retry_refreshing == 1
  assert counts.parked == 1
  assert counts.completed == 1

  let assert Ok(validating) =
    task_lifecycle.get(
      directory,
      orchestrator_state.linear_issue_id_identity("issue-1"),
    )
  assert task_lifecycle.is_active_or_pending(validating)
  assert task_lifecycle.consumes_dispatch_slot(validating)
  assert !task_lifecycle.holds_tracker_claim(validating)

  let assert Ok(running) =
    task_lifecycle.get(
      directory,
      orchestrator_state.linear_issue_id_identity("issue-3"),
    )
  assert task_lifecycle.holds_tracker_claim(running)
  assert task_lifecycle.has_live_worker(running)

  let assert Ok(retrying) =
    task_lifecycle.get(
      directory,
      orchestrator_state.linear_issue_id_identity("issue-4"),
    )
  assert task_lifecycle.is_retry_refreshing(retrying)
  assert !task_lifecycle.consumes_dispatch_slot(retrying)

  let assert Ok(completed) =
    task_lifecycle.get(
      directory,
      orchestrator_state.linear_issue_id_identity("issue-6"),
    )
  assert task_lifecycle.is_completed(completed)
  assert !task_lifecycle.is_active_or_pending(completed)
}

pub fn adapter_projects_legacy_sources_into_typed_lifecycle_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let validating_issue =
    tracker_issue.Issue(..issue, id: "issue-2", identifier: "ABC-2")
  let retry_issue =
    tracker_issue.Issue(..issue, id: "issue-3", identifier: "ABC-3")
  let running_issue =
    tracker_issue.Issue(..issue, id: "issue-4", identifier: "ABC-4")
  let parked_issue =
    tracker_issue.Issue(..issue, id: "issue-5", identifier: "ABC-5")
  let completed_issue =
    tracker_issue.Issue(..issue, id: "issue-6", identifier: "ABC-6")
  let base = orchestrator_transition_test.state_with_pending_claim(issue)
  let state =
    transition_types.State(
      ..base,
      pending_dispatch_validations: dict.from_list([
        #(
          orchestrator_state.issue_identity(validating_issue),
          transition_types.PendingDispatchValidation(
            task_ref: task.from_legacy_issue(validating_issue).ref,
            issue: validating_issue,
            remaining_candidates: [],
            generation: 2,
          ),
        ),
      ]),
      runtime: orchestrator_state.RuntimeState(
        ..base.runtime,
        running: dict.from_list([
          #(
            orchestrator_state.issue_identity(running_issue),
            orchestrator_state.RunningEntry(
              task: task.from_legacy_issue(running_issue),
              issue: running_issue,
              workspace_path: "test/tmp/workspaces/ABC-4",
              session: None,
            ),
          ),
        ]),
        retry_attempts: dict.from_list([
          #(
            orchestrator_state.issue_identity(retry_issue),
            orchestrator_state.RetryEntry(
              task_ref: task.from_legacy_issue(retry_issue).ref,
              issue_id: retry_issue.id,
              delay_ms: 1000,
              timer_generation: 7,
            ),
          ),
        ]),
        parked: dict.from_list([
          #(
            orchestrator_state.issue_identity(parked_issue),
            orchestrator_state.ParkedEntry(
              task_ref: task.from_legacy_issue(parked_issue).ref,
              issue_id: parked_issue.id,
              identifier: parked_issue.identifier,
              reason: orchestrator_reason.ParkOperator("hold"),
              release_policy: orchestrator_state.ExplicitUnparkOnly,
              parked_at_ms: 1,
            ),
          ),
        ]),
        completed: dict.from_list([
          #(orchestrator_state.issue_identity(completed_issue), completed_issue),
        ]),
      ),
      workers: worker_directory_for(
        running_issue,
        transition_types.WorkerRunning,
      ),
    )
  let retries = retry_scheduler.new()
  let assert Ok(retries) =
    retry_scheduler.begin_task_refresh(
      retries,
      task.from_legacy_issue(retry_issue).ref,
      7,
    )

  let assert Ok(directory) =
    task_lifecycle_legacy.from_legacy_state(state, retries)
  let counts = task_lifecycle.counts(directory)
  assert counts.validating == 1
  assert counts.claiming == 1
  assert counts.running == 1
  assert counts.retry_refreshing == 1
  assert counts.parked == 1
  assert counts.completed == 1
}

pub fn running_and_parked_conflict_returns_explicit_error_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state =
    transition_types.State(
      ..orchestrator_transition_test.fixture_state(),
      runtime: orchestrator_state.RuntimeState(
        ..orchestrator_transition_test.fixture_runtime(),
        running: dict.from_list([
          #(
            orchestrator_state.issue_identity(issue),
            orchestrator_state.RunningEntry(
              task: task.from_legacy_issue(issue),
              issue: issue,
              workspace_path: "test/tmp/workspaces/ABC-1",
              session: None,
            ),
          ),
        ]),
        parked: dict.from_list([
          #(
            orchestrator_state.issue_identity(issue),
            orchestrator_state.ParkedEntry(
              task_ref: task.from_legacy_issue(issue).ref,
              issue_id: issue.id,
              identifier: issue.identifier,
              reason: orchestrator_reason.ParkOperator("hold"),
              release_policy: orchestrator_state.ExplicitUnparkOnly,
              parked_at_ms: 10,
            ),
          ),
        ]),
      ),
      workers: worker_directory_for(issue, transition_types.WorkerRunning),
    )

  let assert Error(error) =
    task_lifecycle_legacy.from_legacy_state(state, retry_scheduler.new())
  assert task_lifecycle.error_code(error) == "conflicting_lifecycle_sources"
}

pub fn claiming_and_retry_waiting_conflict_returns_explicit_error_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let base = orchestrator_transition_test.state_with_pending_claim(issue)
  let state =
    transition_types.State(
      ..base,
      runtime: orchestrator_state.RuntimeState(
        ..base.runtime,
        retry_attempts: dict.from_list([
          #(
            orchestrator_state.issue_identity(issue),
            orchestrator_state.RetryEntry(
              task_ref: task.from_legacy_issue(issue).ref,
              issue_id: issue.id,
              delay_ms: 1000,
              timer_generation: 3,
            ),
          ),
        ]),
      ),
    )

  let assert Error(error) =
    task_lifecycle_legacy.from_legacy_state(state, retry_scheduler.new())
  assert task_lifecycle.error_code(error) == "conflicting_lifecycle_sources"
}

pub fn completed_and_active_conflict_returns_explicit_error_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let task_ref = task.from_legacy_issue(issue).ref
  let state =
    transition_types.State(
      ..orchestrator_transition_test.fixture_state(),
      pending_dispatch_validations: dict.from_list([
        #(
          orchestrator_state.issue_identity(issue),
          transition_types.PendingDispatchValidation(
            task_ref: task_ref,
            issue: issue,
            remaining_candidates: [],
            generation: 1,
          ),
        ),
      ]),
      runtime: orchestrator_state.RuntimeState(
        ..orchestrator_transition_test.fixture_runtime(),
        completed: dict.from_list([
          #(orchestrator_state.issue_identity(issue), issue),
        ]),
      ),
    )

  let assert Error(error) =
    task_lifecycle_legacy.from_legacy_state(state, retry_scheduler.new())
  assert task_lifecycle.error_code(error) == "conflicting_lifecycle_sources"
}

pub fn stale_refresh_without_retry_waiting_returns_explicit_error_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let retries = retry_scheduler.new()
  let assert Ok(retries) =
    retry_scheduler.begin_task_refresh(
      retries,
      task.from_legacy_issue(issue).ref,
      9,
    )

  let assert Error(error) =
    task_lifecycle_legacy.from_legacy_state(
      orchestrator_transition_test.fixture_state(),
      retries,
    )
  assert task_lifecycle.error_code(error) == "missing_retry_waiting_for_refresh"
}

pub fn claimed_without_backing_lifecycle_returns_explicit_error_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state =
    transition_types.State(
      ..orchestrator_transition_test.fixture_state(),
      runtime: orchestrator_state.RuntimeState(
        ..orchestrator_transition_test.fixture_runtime(),
        claimed: dict.from_list([
          #(orchestrator_state.issue_identity(issue), issue.identifier),
        ]),
      ),
    )

  let assert Error(error) =
    task_lifecycle_legacy.from_legacy_state(state, retry_scheduler.new())
  assert task_lifecycle.error_code(error) == "missing_claimed_lifecycle"
}

pub fn transition_normalization_tracks_retry_refresh_lifecycle_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let runtime =
    orchestrator_state.RuntimeState(
      ..orchestrator_transition_test.fixture_runtime(),
      claimed: dict.from_list([
        #(orchestrator_state.issue_identity(issue), issue.identifier),
      ]),
      retry_attempts: dict.from_list([
        #(
          orchestrator_state.issue_identity(issue),
          orchestrator_state.RetryEntry(
            task_ref: task.from_legacy_issue(issue).ref,
            issue_id: issue.id,
            delay_ms: 1000,
            timer_generation: 1,
          ),
        ),
      ]),
    )
  let state =
    transition_types.State(
      ..orchestrator_transition_test.fixture_state(),
      runtime: runtime,
    )

  let transition_types.Outcome(state: refreshing, ..) =
    transition.handle(
      transition_types.RetryTick(
        issue.id,
        1,
        orchestrator_transition_test.fixture_context(),
      ),
      state,
    )
  let assert Ok(lifecycle) =
    task_lifecycle.get(
      refreshing.lifecycle,
      orchestrator_state.issue_identity(issue),
    )
  assert task_lifecycle.is_retry_refreshing(lifecycle)

  let transition_types.Outcome(state: waiting, ..) =
    transition.handle(
      transition_types.RetryRefreshCompleted(
        issue.id,
        1,
        Error("tracker_failed"),
        orchestrator_transition_test.fixture_context(),
      ),
      refreshing,
    )
  let assert Ok(lifecycle) =
    task_lifecycle.get(
      waiting.lifecycle,
      orchestrator_state.issue_identity(issue),
    )
  assert task_lifecycle.is_retry_waiting(lifecycle)
}

pub fn startup_recovery_projects_recovered_retries_into_lifecycle_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let task_ref = task.from_legacy_issue(issue).ref
  let runtime =
    orchestrator_state.RuntimeState(
      ..orchestrator_transition_test.fixture_runtime(),
      claimed: dict.from_list([
        #(orchestrator_state.issue_identity(issue), issue.id),
      ]),
      retry_attempts: dict.from_list([
        #(
          orchestrator_state.issue_identity(issue),
          orchestrator_state.RetryEntry(
            task_ref: task_ref,
            issue_id: issue.id,
            delay_ms: 2500,
            timer_generation: 3,
          ),
        ),
      ]),
    )
  let assert Ok(directory) =
    task_lifecycle_legacy.from_transition_state(
      transition_types.State(
        ..orchestrator_transition_test.fixture_state(),
        runtime: runtime,
      ),
    )
  let assert Ok(lifecycle) =
    task_lifecycle.get(directory, orchestrator_state.issue_identity(issue))
  assert task_lifecycle.is_retry_waiting(lifecycle)
  assert task_lifecycle.has_tracker_claim(
    directory,
    orchestrator_state.issue_identity(issue),
  )
}

fn worker_directory_for(
  issue: tracker_issue.Issue,
  status: transition_types.WorkerStatus,
) -> transition_types.WorkerDirectory {
  let identity = orchestrator_state.issue_identity(issue)
  let worker =
    transition_types.WorkerEntry(
      task_ref: task.from_legacy_issue(issue).ref,
      issue_id: issue.id,
      run_id: "run-" <> issue.id,
      session_id: "session-" <> issue.id,
      issue: issue,
      workspace_path: "test/tmp/workspaces/" <> issue.identifier,
      workflow_id: "default",
      workflow_snapshot: None,
      command_route_id: "worker:" <> issue.id,
      status: status,
      recovery: None,
    )

  transition_types.WorkerDirectory(
    by_issue: dict.from_list([#(identity, worker)]),
    by_session: dict.from_list([#(worker.session_id, identity)]),
    route_to_session: dict.from_list([
      #(worker.command_route_id, worker.session_id),
    ]),
    yaml_step_runs: dict.new(),
    stopped_yaml_runs: dict.new(),
  )
}
