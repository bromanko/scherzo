import gleam/dict
import gleam/option.{None}
import orchestrator_transition_test
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/transition
import scherzo/orchestrator/transition_types
import scherzo/state/ledger

pub fn ledger_spawn_continuation_success_emits_start_worker_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = orchestrator_transition_test.state_with_pending_claim(issue)

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.LedgerAppendCompleted(
        correlation_id: "claim:issue-1:run-1",
        continuation: effects_types.SpawnClaimedWorker(
          issue_id: "issue-1",
          run_id: "run-1",
          session_id: "session-1",
        ),
        result: Ok(Nil),
        now_ms: 123,
      ),
      state,
    )

  assert dict.get(next.pending_claims, "issue-1") == Error(Nil)
  let assert Ok(running_entry) = dict.get(next.runtime.running, "issue-1")
  assert running_entry.issue == issue
  assert running_entry.workspace_path == "test/tmp/workspaces/ABC-1"
  assert running_entry.session == None
  let assert Ok(worker_entry) = dict.get(next.workers.by_issue, "issue-1")
  assert worker_entry.status == transition_types.WorkerStarting
  assert next.workers.by_session == dict.from_list([#("session-1", "issue-1")])
  assert next.workers.route_to_session
    == dict.from_list([#("worker:run-1:1", "session-1")])
  assert effects
    == [
      effects_types.StartWorker(effects_types.WorkerStart(
        issue_id: "issue-1",
        run_id: "run-1",
        session_id: "session-1",
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
          issue_id: "issue-1",
          run_id: "run-1",
          session_id: "session-1",
        ),
        result: Error(ledger.Io("disk full")),
        now_ms: 123,
      ),
      state,
    )

  assert dict.get(next.pending_claims, "issue-1") == Error(Nil)
  assert dict.get(next.runtime.running, "issue-1") == Error(Nil)
  assert dict.get(next.workers.by_issue, "issue-1") == Error(Nil)
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
