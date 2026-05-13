import gleam/option.{None}
import orchestrator_transition_test
import scherzo/orchestrator/effects/interpreter
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/transition_types
import scherzo/state/ledger

pub fn append_ledger_continue_with_success_returns_follow_up_message_test() {
  let shell =
    interpreter.new_shell_state(append_ledger: fn(_) { Ok(Nil) }, now_ms: fn() {
      456
    })
  let request = spawn_claim_ledger_append()

  let interpreter.ApplyResult(
    shell: shell,
    follow_up_messages: follow_up_messages,
  ) = interpreter.apply(shell, [effects_types.AppendLedger(request)])

  assert follow_up_messages
    == [
      transition_types.LedgerAppendCompleted(
        correlation_id: "claim:issue-1:run-1",
        continuation: effects_types.SpawnClaimedWorker(
          issue_id: "issue-1",
          run_id: "run-1",
          session_id: "session-1",
        ),
        result: Ok(Nil),
        now_ms: 456,
      ),
    ]
  assert interpreter.started_workers(shell) == []
}

pub fn append_ledger_continue_with_failure_returns_follow_up_message_test() {
  let shell =
    interpreter.new_shell_state(
      append_ledger: fn(_) { Error(ledger.Io("disk full")) },
      now_ms: fn() { 456 },
    )
  let request = spawn_claim_ledger_append()

  let interpreter.ApplyResult(
    shell: shell,
    follow_up_messages: follow_up_messages,
  ) = interpreter.apply(shell, [effects_types.AppendLedger(request)])

  assert follow_up_messages
    == [
      transition_types.LedgerAppendCompleted(
        correlation_id: "claim:issue-1:run-1",
        continuation: effects_types.SpawnClaimedWorker(
          issue_id: "issue-1",
          run_id: "run-1",
          session_id: "session-1",
        ),
        result: Error(ledger.Io("disk full")),
        now_ms: 456,
      ),
    ]
  assert interpreter.started_workers(shell) == []
}

pub fn append_ledger_continue_regardless_failure_does_not_block_later_effects_test() {
  let shell =
    interpreter.new_shell_state(
      append_ledger: fn(_) { Error(ledger.Io("disk full")) },
      now_ms: fn() { 456 },
    )
  let request =
    effects_types.LedgerAppend(
      correlation_id: "retry:issue-1:1",
      bodies: [],
      failure_event: "retry_append_failed",
      policy: effects_types.ContinueRegardless,
    )
  let worker_start = worker_start_request()

  let interpreter.ApplyResult(
    shell: shell,
    follow_up_messages: follow_up_messages,
  ) =
    interpreter.apply(shell, [
      effects_types.AppendLedger(request),
      effects_types.StartWorker(worker_start),
    ])

  assert follow_up_messages
    == [
      transition_types.WorkerStartSucceeded("issue-1", "run-1", "session-1"),
    ]
  assert interpreter.started_workers(shell) == [worker_start]
}

fn spawn_claim_ledger_append() -> effects_types.LedgerAppend {
  effects_types.LedgerAppend(
    correlation_id: "claim:issue-1:run-1",
    bodies: [],
    failure_event: "ledger_append_failed",
    policy: effects_types.ContinueWith(effects_types.SpawnClaimedWorker(
      issue_id: "issue-1",
      run_id: "run-1",
      session_id: "session-1",
    )),
  )
}

fn worker_start_request() -> effects_types.WorkerStart {
  effects_types.WorkerStart(
    issue_id: "issue-1",
    run_id: "run-1",
    session_id: "session-1",
    command_route_id: "worker:run-1:1",
    issue: orchestrator_transition_test.fixture_issue(),
    workspace_path: "test/tmp/workspaces/ABC-1",
    workflow_id: "default",
    route_label: "ABC-1",
    recovery: None,
  )
}
