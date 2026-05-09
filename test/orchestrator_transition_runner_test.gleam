import gleam/dict
import gleam/list
import gleam/option.{None}
import orchestrator_transition_test
import scherzo/orchestrator/effects/interpreter
import scherzo/orchestrator/state as orchestrator_state
import scherzo/orchestrator/transition_runner
import scherzo/orchestrator/transition_types
import scherzo/state/record

pub fn transition_runner_applies_effects_and_follow_ups_in_order_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = orchestrator_transition_test.state_with_pending_claim(issue)
  let shell = event_shell()

  let transition_runner.RunResult(
    state: next,
    shell: shell,
    exhausted: exhausted,
  ) =
    transition_runner.run(
      state: state,
      shell: shell,
      messages: [claim_ledger_append_requested()],
      max_messages: 8,
    )

  assert exhausted == False
  assert interpreter.data(shell)
    == ["append:claim:issue-1:run-1", "start:run-1"]
  assert dict.get(next.pending_claims, "issue-1") == Error(Nil)
  assert dict.get(next.runtime.running, "issue-1")
    == Ok(orchestrator_state.RunningEntry(
      issue: issue,
      workspace_path: "test/tmp/workspaces/ABC-1",
      session: None,
    ))
  assert dict.get(next.runtime.claimed, "issue-1") == Ok("ABC-1")
}

pub fn transition_runner_applies_snapshot_reply_effect_test() {
  let state = orchestrator_transition_test.fixture_state()
  let shell = event_shell()

  let transition_runner.RunResult(
    state: next,
    shell: shell,
    exhausted: exhausted,
  ) =
    transition_runner.run(
      state: state,
      shell: shell,
      messages: [transition_types.SnapshotRequested],
      max_messages: 4,
    )

  assert exhausted == False
  assert next == state
  assert interpreter.data(shell) == ["snapshot"]
}

pub fn transition_runner_stops_at_message_limit_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = orchestrator_transition_test.state_with_pending_claim(issue)
  let shell = event_shell()

  let transition_runner.RunResult(
    state: next,
    shell: shell,
    exhausted: exhausted,
  ) =
    transition_runner.run(
      state: state,
      shell: shell,
      messages: [claim_ledger_append_requested()],
      max_messages: 1,
    )

  assert exhausted == True
  assert interpreter.data(shell) == ["append:claim:issue-1:run-1"]
  assert next == state
}

fn event_shell() -> interpreter.ShellState(List(String)) {
  interpreter.new_production_shell_state(
    data: [],
    append_ledger: fn(events, request) {
      #(list.append(events, ["append:" <> request.correlation_id]), Ok(Nil))
    },
    now_ms: fn(_) { 456 },
    log_effect: fn(events, _, event, _) {
      list.append(events, ["log:" <> event])
    },
    start_worker: fn(events, request) {
      list.append(events, ["start:" <> request.run_id])
    },
    reply_snapshot: fn(events, _) { list.append(events, ["snapshot"]) },
  )
}

fn claim_ledger_append_requested() -> transition_types.Message {
  transition_types.ClaimLedgerAppendRequested(
    correlation_id: "claim:issue-1:run-1",
    issue_id: "issue-1",
    run_id: "run-1",
    session_id: "session-1",
    bodies: [
      record.RunStarted(
        "run-1",
        "issue-1",
        "ABC-1",
        "test/tmp/workspaces/ABC-1",
      ),
    ],
    failure_event: "ledger_append_failed",
  )
}
