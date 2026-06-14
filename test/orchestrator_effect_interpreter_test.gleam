import gleam/option.{None}
import orchestrator_transition_test
import scherzo/control/command
import scherzo/orchestrator/effects/interpreter
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/transition_types
import scherzo/runtime/identity
import scherzo/runtime/state as orchestrator_state
import scherzo/state/ledger
import scherzo/state/ledger_batch

pub fn append_ledger_claim_spawn_success_returns_follow_up_message_test() {
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
        continuation: transition_types.SpawnClaimedWorkerAfterAppend(
          task_identity: orchestrator_state.linear_issue_id_identity("issue-1"),
          issue_id: identity.issue_id_from_string("issue-1"),
          run_id: identity.run_id_from_string("run-1"),
          session_id: identity.session_id_from_string("session-1"),
        ),
        result: Ok(Nil),
        now_ms: 456,
      ),
    ]
  assert interpreter.started_workers(shell) == []
}

pub fn append_ledger_claim_spawn_failure_returns_follow_up_message_test() {
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
        continuation: transition_types.SpawnClaimedWorkerAfterAppend(
          task_identity: orchestrator_state.linear_issue_id_identity("issue-1"),
          issue_id: identity.issue_id_from_string("issue-1"),
          run_id: identity.run_id_from_string("run-1"),
          session_id: identity.session_id_from_string("session-1"),
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
      batch: ledger_batch.retry_cancelled("issue-1", 1, "test"),
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
      transition_types.WorkerStartSucceeded(
        identity.issue_id_from_string("issue-1"),
        identity.run_id_from_string("run-1"),
        identity.session_id_from_string("session-1"),
      ),
    ]
  assert interpreter.started_workers(shell) == [worker_start]
}

pub fn append_ledger_stop_daemon_failure_blocks_later_effects_test() {
  let shell =
    interpreter.new_shell_state(
      append_ledger: fn(_) { Error(ledger.Io("disk full")) },
      now_ms: fn() { 456 },
    )
  let request =
    effects_types.LedgerAppend(
      correlation_id: "worker_failure:issue-1:run-1",
      batch: ledger_batch.retry_cancelled("issue-1", 1, "test"),
      failure_event: "ledger_append_failed",
      policy: effects_types.StopBatchOnFailure,
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

  assert follow_up_messages == []
  assert interpreter.started_workers(shell) == []
}

pub fn apply_operator_command_follow_ups_precede_completion_test() {
  let request =
    effects_types.OperatorCommandRequest(
      correlation_id: "operator-1",
      source: effects_types.LocalOperatorCommand,
      operator_command: command.ReloadWorkflow,
      timeout_ms: 1000,
    )
  let runtime_reloaded =
    transition_types.WorkflowRuntimeReloaded(
      poll_interval_ms: 5000,
      max_concurrent_agents: 2,
    )
  let result = command.applied(request.operator_command, None)
  let shell =
    shell_with_apply_operator_command(fn(events, request) {
      #(events, command.applied(request.operator_command, None), [
        runtime_reloaded,
      ])
    })

  let interpreter.ApplyResult(follow_up_messages: follow_up_messages, ..) =
    interpreter.apply(shell, [effects_types.ApplyOperatorCommand(request)])

  assert follow_up_messages
    == [
      runtime_reloaded,
      transition_types.OperatorCommandCompleted(request, result),
    ]
}

fn spawn_claim_ledger_append() -> effects_types.LedgerAppend {
  effects_types.LedgerAppend(
    correlation_id: "claim:issue-1:run-1",
    batch: ledger_batch.retry_cancelled("issue-1", 1, "test"),
    failure_event: "ledger_append_failed",
    policy: effects_types.SpawnClaimedWorkerAfterAppend(
      task_identity: orchestrator_state.linear_issue_id_identity("issue-1"),
      issue_id: identity.issue_id_from_string("issue-1"),
      run_id: identity.run_id_from_string("run-1"),
      session_id: identity.session_id_from_string("session-1"),
    ),
  )
}

fn worker_start_request() -> effects_types.WorkerStart {
  effects_types.WorkerStart(
    task_ref: orchestrator_state.linear_issue_id_ref("issue-1"),
    issue_id: identity.issue_id_from_string("issue-1"),
    run_id: identity.run_id_from_string("run-1"),
    session_id: identity.session_id_from_string("session-1"),
    command_route_id: "worker:run-1:1",
    issue: orchestrator_transition_test.fixture_issue(),
    workspace_path: "test/tmp/workspaces/ABC-1",
    workflow_id: "default",
    workflow_snapshot: None,
    route_label: "ABC-1",
    recovery: None,
  )
}

fn shell_with_apply_operator_command(
  apply_operator_command: fn(
    List(effects_types.WorkerStart),
    effects_types.OperatorCommandRequest,
  ) ->
    #(
      List(effects_types.WorkerStart),
      command.CommandResult,
      List(transition_types.Message),
    ),
) -> interpreter.ShellState(List(effects_types.WorkerStart)) {
  interpreter.new_production_shell_state(
    data: [],
    append_ledger: fn(started_workers, _request) { #(started_workers, Ok(Nil)) },
    now_ms: fn(_) { 456 },
    log_effect: fn(started_workers, _, _, _) { started_workers },
    start_worker: fn(started_workers, _request) { #(started_workers, Ok(Nil)) },
    reply_snapshot: fn(started_workers, _) { started_workers },
    mark_poll_in_flight: fn(started_workers, _) { started_workers },
    schedule_next_poll: fn(started_workers) { started_workers },
    fetch_candidates: fn(started_workers, _) { started_workers },
    begin_dispatch_validation: fn(started_workers, _, _) { started_workers },
    begin_review_lane_preflight: fn(started_workers, _) { started_workers },
    reserve_session_sequence: fn(started_workers, _) { started_workers },
    claim_issue: fn(started_workers, _, _, _, _) { started_workers },
    report_invalid_workflow: fn(started_workers, _, _, _, _) { started_workers },
    replay_outbox: fn(started_workers, _) { started_workers },
    remove_retry_timer: fn(started_workers, _) { started_workers },
    finish_retry_refresh: fn(started_workers, _) { started_workers },
    defer_retry_timer: fn(started_workers, _, _, _) { started_workers },
    begin_retry_refresh: fn(started_workers, _, _) { started_workers },
    schedule_retry_timer: fn(started_workers, _, _, _, _) { started_workers },
    schedule_recovered_retry_timer: fn(started_workers, _, _, _) {
      started_workers
    },
    cancel_retry_timer: fn(started_workers, _, _, _) { started_workers },
    release_claim: fn(started_workers, _) { started_workers },
    clear_recovery: fn(started_workers, _) { started_workers },
    worker_start_failed: fn(started_workers, _, _) { started_workers },
    remove_worker: fn(started_workers, _, _) { started_workers },
    publish_worker_exited: fn(started_workers, _) { started_workers },
    report_worker_success: fn(started_workers, _, _) { started_workers },
    report_worker_failure: fn(started_workers, _, _) { started_workers },
    cleanup_workspace: fn(started_workers, _) { started_workers },
    park_issue: fn(started_workers, _, _) { started_workers },
    report_park: fn(started_workers, _) { started_workers },
    stop_worker: fn(started_workers, _, _) { started_workers },
    stop_worker_after_issue_refresh: fn(started_workers, _, _) {
      started_workers
    },
    register_yaml_step_started: fn(started_workers, _, _) { started_workers },
    finish_yaml_step_route: fn(started_workers, _) { started_workers },
    finish_yaml_step_session: fn(started_workers, _, _) { started_workers },
    finish_yaml_step_sessions_for_run: fn(started_workers, _, _) {
      started_workers
    },
    clear_yaml_step_routes_for_run: fn(started_workers, _) { started_workers },
    mark_yaml_run_stopping: fn(started_workers, _, _) { started_workers },
    shutdown_runtime: fn(started_workers, _) { started_workers },
    set_operator_paused: fn(started_workers, _) { started_workers },
    apply_operator_command: apply_operator_command,
    finish_operator_command: fn(started_workers, _, _) {
      #(started_workers, [])
    },
    report_park_effect: fn(started_workers, _, _, _, _, _) { started_workers },
  )
}
