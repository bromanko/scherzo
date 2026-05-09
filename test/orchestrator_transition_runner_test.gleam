import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None}
import orchestrator_transition_test
import scherzo/orchestrator/effects/interpreter
import scherzo/orchestrator/state as orchestrator_state
import scherzo/orchestrator/transition_runner
import scherzo/orchestrator/transition_types
import scherzo/state/ledger
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

pub fn transition_runner_retry_continue_regardless_keeps_timer_after_append_failure_test() {
  let runtime =
    orchestrator_state.RuntimeState(
      ..orchestrator_transition_test.fixture_runtime(),
      claimed: dict.from_list([#("issue-1", "ABC-1")]),
      retry_attempts: dict.from_list([
        #(
          "issue-1",
          orchestrator_state.RetryEntry(
            issue_id: "issue-1",
            delay_ms: 10_000,
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
  let shell = append_failure_shell()

  let transition_runner.RunResult(shell: shell, exhausted: exhausted, ..) =
    transition_runner.run(
      state: state,
      shell: shell,
      messages: [
        transition_types.RetryRefreshCompleted(
          "issue-1",
          1,
          Error("api"),
          orchestrator_transition_test.fixture_context(),
        ),
      ],
      max_messages: 8,
    )

  assert exhausted == False
  assert interpreter.data(shell)
    == [
      "retry:finish:issue-1",
      "append:retry_cancel:issue-1:2",
      "retry:cancel:issue-1",
      "append:retry_schedule:issue-1:2",
      "retry:schedule:issue-1",
    ]
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
  shell_with_append_result(Ok(Nil))
}

fn append_failure_shell() -> interpreter.ShellState(List(String)) {
  shell_with_append_result(Error(ledger.Io("disk full")))
}

fn shell_with_append_result(
  append_result: Result(Nil, ledger.LedgerError),
) -> interpreter.ShellState(List(String)) {
  interpreter.new_production_shell_state(
    data: [],
    append_ledger: fn(events, request) {
      #(
        list.append(events, ["append:" <> request.correlation_id]),
        append_result,
      )
    },
    now_ms: fn(_) { 456 },
    log_effect: fn(events, _, event, _) {
      list.append(events, ["log:" <> event])
    },
    start_worker: fn(events, request) {
      list.append(events, ["start:" <> request.run_id])
    },
    reply_snapshot: fn(events, _) { list.append(events, ["snapshot"]) },
    mark_poll_in_flight: fn(events, generation) {
      list.append(events, ["poll:" <> int.to_string(generation)])
    },
    schedule_next_poll: fn(events) { list.append(events, ["poll:next"]) },
    fetch_candidates: fn(events, generation) {
      list.append(events, ["fetch:" <> int.to_string(generation)])
    },
    fetch_linear_commands: fn(events, _, _, _, _) { events },
    begin_dispatch_validation: fn(events, issue_id, _) {
      list.append(events, ["validate:" <> issue_id])
    },
    reserve_session_sequence: fn(events, sequence) {
      list.append(events, ["reserve:" <> int.to_string(sequence)])
    },
    claim_issue: fn(events, issue, _, _) {
      list.append(events, ["claim:" <> issue.id])
    },
    report_invalid_workflow: fn(events, issue, _, _, _) {
      list.append(events, ["invalid:" <> issue.id])
    },
    remove_retry_timer: fn(events, issue_id) {
      list.append(events, ["retry:remove:" <> issue_id])
    },
    finish_retry_refresh: fn(events, issue_id) {
      list.append(events, ["retry:finish:" <> issue_id])
    },
    defer_retry_timer: fn(events, issue_id, _, _) {
      list.append(events, ["retry:defer:" <> issue_id])
    },
    begin_retry_refresh: fn(events, issue_id, _) {
      list.append(events, ["retry:refresh:" <> issue_id])
    },
    schedule_retry_timer: fn(events, issue_id, _, _, _) {
      list.append(events, ["retry:schedule:" <> issue_id])
    },
    cancel_retry_timer: fn(events, issue_id, _, _) {
      list.append(events, ["retry:cancel:" <> issue_id])
    },
    release_claim: fn(events, issue_id) {
      list.append(events, ["release:" <> issue_id])
    },
    clear_recovery: fn(events, issue_id) {
      list.append(events, ["clear_recovery:" <> issue_id])
    },
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
