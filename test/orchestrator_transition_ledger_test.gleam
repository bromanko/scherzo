import gleam/dict
import gleam/option.{None}
import orchestrator_transition_test
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/identity
import scherzo/orchestrator/state as orchestrator_state
import scherzo/orchestrator/transition
import scherzo/orchestrator/transition_types
import scherzo/state/ledger
import scherzo/task

pub fn ledger_spawn_continuation_success_emits_start_worker_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = orchestrator_transition_test.state_with_pending_claim(issue)

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.LedgerAppendCompleted(
        correlation_id: "claim:issue-1:run-1",
        continuation: effects_types.SpawnClaimedWorker(
          task_identity: orchestrator_state.linear_issue_id_identity("issue-1"),
          issue_id: identity.issue_id_from_string("issue-1"),
          run_id: identity.run_id_from_string("run-1"),
          session_id: identity.session_id_from_string("session-1"),
        ),
        result: Ok(Nil),
        now_ms: 123,
      ),
      state,
    )

  let identity = orchestrator_state.issue_identity(issue)
  assert dict.get(next.pending_claims, identity) == Error(Nil)
  let assert Ok(running_entry) = dict.get(next.runtime.running, identity)
  assert running_entry.issue == issue
  assert running_entry.workspace_path == "test/tmp/workspaces/ABC-1"
  assert running_entry.session == None
  let assert Ok(worker_entry) = dict.get(next.workers.by_issue, identity)
  assert worker_entry.status == transition_types.WorkerStarting
  assert next.workers.by_session == dict.from_list([#("session-1", identity)])
  assert next.workers.route_to_session
    == dict.from_list([#("worker:run-1:1", "session-1")])
  assert effects
    == [
      effects_types.StartWorker(effects_types.WorkerStart(
        task_ref: task.from_legacy_issue(issue).ref,
        issue_id: identity.issue_id_from_string("issue-1"),
        run_id: identity.run_id_from_string("run-1"),
        session_id: identity.session_id_from_string("session-1"),
        command_route_id: "worker:run-1:1",
        issue: issue,
        workspace_path: "test/tmp/workspaces/ABC-1",
        workflow_id: "default",
        route_label: "ABC-1",
        recovery: None,
      )),
    ]
}

pub fn ledger_spawn_continuation_failure_clears_pending_without_starting_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = orchestrator_transition_test.state_with_pending_claim(issue)

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.LedgerAppendCompleted(
        correlation_id: "claim:issue-1:run-1",
        continuation: effects_types.SpawnClaimedWorker(
          task_identity: orchestrator_state.linear_issue_id_identity("issue-1"),
          issue_id: identity.issue_id_from_string("issue-1"),
          run_id: identity.run_id_from_string("run-1"),
          session_id: identity.session_id_from_string("session-1"),
        ),
        result: Error(ledger.Io("disk full")),
        now_ms: 123,
      ),
      state,
    )

  let identity = orchestrator_state.issue_identity(issue)
  assert dict.get(next.pending_claims, identity) == Error(Nil)
  assert dict.get(next.runtime.running, identity) == Error(Nil)
  assert dict.get(next.workers.by_issue, identity) == Error(Nil)
  assert effects
    == [
      effects_types.Log("warn", "ledger_append_failed", [
        #("issue_id", "issue-1"),
        #("run_id", "run-1"),
        #("correlation_id", "claim:issue-1:run-1"),
        #("error", "io"),
      ]),
    ]
}
