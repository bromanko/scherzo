import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import orchestrator_transition_invariant_helpers as invariant_helpers
import orchestrator_transition_test
import scherzo/agent/types as agent_types
import scherzo/control/command
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/transition
import scherzo/orchestrator/transition_types
import scherzo/result_artifact
import scherzo/runtime/identity
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
import scherzo/session/tokens as session_tokens
import scherzo/state/ledger
import scherzo/state/ledger_batch
import scherzo/state/projection
import scherzo/state/record
import scherzo/task
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

pub fn operator_pause_command_appends_durable_pause_record_test() {
  let request =
    effects_types.OperatorCommandRequest(
      source: effects_types.LocalOperatorCommand,
      operator_command: command.PauseDispatch,
      timeout_ms: 1000,
    )

  let transition_types.Outcome(effects: effects, ..) =
    transition.handle(
      transition_types.OperatorCommandSubmitted(
        request: request,
        context: orchestrator_transition_test.fixture_context(),
        issue_resolution: transition_types.OperatorIssueNotResolved,
        parked_issue_resolution: transition_types.ParkedIssueNotResolved,
      ),
      orchestrator_transition_test.fixture_state(),
    )

  let assert [
    effects_types.SetOperatorPaused(True),
    effects_types.AppendLedger(append),
  ] = effects
  assert append.correlation_id == "operator_dispatch_pause:paused"
  assert append.failure_event == "operator_dispatch_pause_ledger_append_failed"
  assert ledger_batch.to_bodies(append.batch)
    == [record.DispatchPauseChanged(paused: True)]
  let assert effects_types.SetOperatorPausedAfterAppend(
    True,
    policy_request,
    success_result,
    failure_result,
  ) = append.policy
  assert policy_request == request
  assert command.status_to_string(success_result.status) == "applied"
  assert command.status_to_string(failure_result.status) == "rejected"
}

pub fn operator_pause_append_success_sets_paused_and_finishes_test() {
  let request =
    effects_types.OperatorCommandRequest(
      source: effects_types.LocalOperatorCommand,
      operator_command: command.PauseDispatch,
      timeout_ms: 1000,
    )
  let success_result = command.applied(command.PauseDispatch, Some("paused"))
  let failure_result =
    command.rejected(command.PauseDispatch, "ledger_append_failed", None)

  let transition_types.Outcome(effects: effects, ..) =
    transition.handle(
      transition_types.LedgerAppendCompleted(
        correlation_id: "operator_dispatch_pause:paused",
        continuation: transition_types.SetOperatorPausedAfterAppend(
          True,
          request,
          success_result,
          failure_result,
        ),
        result: Ok(Nil),
        now_ms: 123,
      ),
      orchestrator_transition_test.fixture_state(),
    )

  assert effects
    == [
      effects_types.SetOperatorPaused(True),
      effects_types.FinishOperatorCommand(request, success_result),
    ]
}

pub fn operator_pause_append_failure_keeps_runtime_paused_and_rejects_test() {
  let request =
    effects_types.OperatorCommandRequest(
      source: effects_types.LocalOperatorCommand,
      operator_command: command.PauseDispatch,
      timeout_ms: 1000,
    )
  let success_result = command.applied(command.PauseDispatch, Some("paused"))
  let failure_result =
    command.rejected(command.PauseDispatch, "ledger_append_failed", None)

  let transition_types.Outcome(effects: effects, ..) =
    transition.handle(
      transition_types.LedgerAppendCompleted(
        correlation_id: "operator_dispatch_pause:paused",
        continuation: transition_types.SetOperatorPausedAfterAppend(
          True,
          request,
          success_result,
          failure_result,
        ),
        result: Error(ledger.Io("disk full")),
        now_ms: 123,
      ),
      orchestrator_transition_test.fixture_state(),
    )

  assert list.any(effects, fn(effect) {
    effect == effects_types.SetOperatorPaused(True)
  })
  assert has_finished_operator_command(effects, request, failure_result)
  assert has_dispatch_pause_append_failed_log(effects, "paused")
}

pub fn operator_resume_append_failure_does_not_resume_and_rejects_test() {
  let request =
    effects_types.OperatorCommandRequest(
      source: effects_types.LocalOperatorCommand,
      operator_command: command.ResumeDispatch,
      timeout_ms: 1000,
    )
  let success_result = command.applied(command.ResumeDispatch, Some("resumed"))
  let failure_result =
    command.rejected(command.ResumeDispatch, "ledger_append_failed", None)

  let transition_types.Outcome(effects: effects, ..) =
    transition.handle(
      transition_types.LedgerAppendCompleted(
        correlation_id: "operator_dispatch_pause:resumed",
        continuation: transition_types.SetOperatorPausedAfterAppend(
          False,
          request,
          success_result,
          failure_result,
        ),
        result: Error(ledger.Io("disk full")),
        now_ms: 123,
      ),
      orchestrator_transition_test.fixture_state(),
    )

  assert !list.any(effects, fn(effect) {
    effect == effects_types.SetOperatorPaused(False)
  })
  assert has_finished_operator_command(effects, request, failure_result)
  assert has_dispatch_pause_append_failed_log(effects, "resumed")
}

pub fn projection_dispatch_pause_recovers_latest_status_test() {
  let projected =
    projection.fold([
      record.with_id("pause", 1, record.DispatchPauseChanged(True)),
      record.with_id("resume", 2, record.DispatchPauseChanged(False)),
    ])

  assert !projection.dispatch_paused(projected)
}

pub fn ledger_spawn_continuation_success_emits_start_worker_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = orchestrator_transition_test.state_with_pending_claim(issue)

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.LedgerAppendCompleted(
        correlation_id: "claim:issue-1:run-1",
        continuation: transition_types.SpawnClaimedWorkerAfterAppend(
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
        workflow_snapshot: Some(
          orchestrator_transition_test.fixture_workflow_snapshot(
            "default",
            issue,
            "run-1",
          ),
        ),
        route_label: "ABC-1",
        recovery: None,
      )),
    ]
}

pub fn ledger_spawn_continuation_failure_schedules_recovery_retry_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = orchestrator_transition_test.state_with_pending_claim(issue)

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.LedgerAppendCompleted(
        correlation_id: "claim:issue-1:run-1",
        continuation: transition_types.SpawnClaimedWorkerAfterAppend(
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
  let assert Ok(retry) = dict.get(next.runtime.retry_attempts, identity)
  assert retry.issue_id == "issue-1"
  assert retry.delay_ms == 10_000
  assert retry.timer_generation == 1
  assert dict.get(next.runtime.claimed, identity) == Ok("ABC-1")
  invariant_helpers.assert_valid_state(next)
  assert !list.any(effects, fn(effect) {
    case effect {
      effects_types.StartWorker(_) -> True
      _ -> False
    }
  })
  assert list.any(effects, fn(effect) {
    effect
    == effects_types.Log("warn", "ledger_append_failed", [
      #("issue_id", "issue-1"),
      #("run_id", "run-1"),
      #("correlation_id", "claim:issue-1:run-1"),
      #("error", "io"),
    ])
  })
  assert list.any(effects, fn(effect) {
    case effect {
      effects_types.AppendLedger(effects_types.LedgerAppend(
        correlation_id: "claim_start_retry_schedule:issue-1:1",
        batch: batch,
        failure_event: "ledger_append_failed",
        policy: effects_types.ScheduleRetryTimerAfterAppend(
          issue_id: "issue-1",
          delay_ms: 10_000,
          generation: 1,
          retry_reason: orchestrator_reason.RetryClaimStartLedgerAppendFailed,
          previous_retry: None,
        ),
      )) ->
        ledger_batch.to_bodies(batch)
        == [
          record.RetryScheduled(
            "issue-1",
            "ABC-1",
            10_000,
            1,
            "claim_start_ledger_append_failed",
          ),
        ]
      _ -> False
    }
  })
  assert !list.any(effects, fn(effect) {
    case effect {
      effects_types.ScheduleRetryTimer(_, _, _, _) -> True
      _ -> False
    }
  })
}

pub fn terminal_worker_success_appends_counter_reset_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let terminal =
    tracker_issue.Issue(
      ..issue,
      state: issue_state.from_string_unchecked("Done"),
    )
  let task_identity = orchestrator_state.issue_identity(issue)
  let state =
    running_worker_state_with_counter(
      issue,
      orchestrator_state.IssueCounter(failure_attempts: 1, worker_sessions: 1),
    )
  let success =
    agent_types.WorkerSuccess(
      final_issue: Some(terminal),
      final_classification: agent_types.FinalTerminal,
      workspace_path: "test/tmp/workspaces/ABC-1",
      tokens: session_tokens.zero_token_totals(),
      turns: 1,
      result: result_artifact.empty(),
    )

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.WorkerFinished(
        identity.issue_id_from_string(issue.id),
        identity.run_id_from_string("run-1"),
        Ok(success),
        lifecycle_context(789),
      ),
      state,
    )

  assert dict.get(next.runtime.issue_counters, task_identity) == Error(Nil)
  assert list.any(effects, fn(effect) {
    case effect {
      effects_types.AppendLedger(effects_types.LedgerAppend(
        correlation_id: "worker_finish:issue-1:run-1",
        batch: batch,
        ..,
      )) ->
        ledger_batch.to_bodies(batch)
        == [
          record.IssueCounterUpdated(
            issue_id: "issue-1",
            issue_identifier: "ABC-1",
            failure_attempts: 0,
            worker_sessions: 0,
            observed_updated_at_ms: 789,
            source_run_id: Some("run-1"),
          ),
        ]
      _ -> False
    }
  })
}

pub fn issue_reconcile_stop_appends_cancelled_workflow_record_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let terminal =
    tracker_issue.Issue(
      ..issue,
      state: issue_state.from_string_unchecked("Done"),
    )
  let task_identity = orchestrator_state.issue_identity(issue)
  let state =
    running_worker_state_with_counter(
      issue,
      orchestrator_state.new_issue_counter(),
    )

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.RunningRefreshCompleted(
        1,
        transition_types.PollSnapshot(1, Some(1)),
        Ok([terminal]),
        orchestrator_transition_test.fixture_context(),
      ),
      state,
    )

  assert dict.get(next.runtime.running, task_identity) == Error(Nil)
  assert_cancelled_workflow_append(
    effects,
    "workflow_cancelled_issue_reconcile:issue-1:run-1:terminal",
  )
  assert_stop_after_issue_refresh(effects, orchestrator_reason.StopTerminal)
}

pub fn issue_reconcile_non_active_stop_appends_cancelled_workflow_record_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let non_active =
    tracker_issue.Issue(
      ..issue,
      state: issue_state.from_string_unchecked("Backlog"),
    )
  let task_identity = orchestrator_state.issue_identity(issue)
  let state =
    running_worker_state_with_counter(
      issue,
      orchestrator_state.new_issue_counter(),
    )

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.RunningRefreshCompleted(
        1,
        transition_types.PollSnapshot(1, Some(1)),
        Ok([non_active]),
        orchestrator_transition_test.fixture_context(),
      ),
      state,
    )

  assert dict.get(next.runtime.running, task_identity) == Error(Nil)
  assert_cancelled_workflow_append(
    effects,
    "workflow_cancelled_issue_reconcile:issue-1:run-1:non_active",
  )
  assert_stop_after_issue_refresh(effects, orchestrator_reason.StopNonActive)
}

pub fn claim_start_recovery_retry_after_prior_retry_increments_generation_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let task_identity = orchestrator_state.issue_identity(issue)
  let runtime = orchestrator_transition_test.fixture_runtime()
  let runtime =
    orchestrator_state.RuntimeState(
      ..runtime,
      retry_attempts: dict.insert(
        runtime.retry_attempts,
        task_identity,
        orchestrator_state.RetryEntry(
          task_ref: task.from_legacy_issue(issue).ref,
          issue_id: issue.id,
          delay_ms: 40_000,
          timer_generation: 3,
        ),
      ),
      claimed: dict.insert(runtime.claimed, task_identity, "ABC-1"),
    )
  let state =
    transition_types.State(
      ..orchestrator_transition_test.fixture_state(),
      runtime: runtime,
    )
  invariant_helpers.assert_valid_state(state)

  let transition_types.Outcome(state: claiming, effects: retry_effects) =
    transition.handle(
      transition_types.RetryRefreshCompleted(
        issue.id,
        3,
        Ok([issue]),
        orchestrator_transition_test.fixture_context(),
      ),
      state,
    )

  let assert Ok(pending) = dict.get(claiming.pending_claims, task_identity)
  assert pending.previous_retry_generation == 3
  assert dict.get(claiming.runtime.retry_attempts, task_identity) == Error(Nil)
  assert dict.get(claiming.runtime.claimed, task_identity) == Ok("ABC-1")
  invariant_helpers.assert_valid_state(claiming)
  assert list.any(retry_effects, fn(effect) {
    case effect {
      effects_types.ClaimIssue(_, _, _, _) -> True
      _ -> False
    }
  })

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.LedgerAppendCompleted(
        correlation_id: "claim:issue-1:" <> pending.run_id,
        continuation: transition_types.SpawnClaimedWorkerAfterAppend(
          task_identity: task_identity,
          issue_id: identity.issue_id_from_string("issue-1"),
          run_id: identity.run_id_from_string(pending.run_id),
          session_id: identity.session_id_from_string(pending.session_id),
        ),
        result: Error(ledger.Io("disk full")),
        now_ms: 123,
      ),
      claiming,
    )

  let assert Ok(retry) = dict.get(next.runtime.retry_attempts, task_identity)
  assert retry.issue_id == "issue-1"
  assert retry.delay_ms == 40_000
  assert retry.timer_generation == 3
  assert dict.get(next.runtime.claimed, task_identity) == Ok("ABC-1")
  invariant_helpers.assert_valid_state(next)
  assert !list.any(effects, fn(effect) {
    case effect {
      effects_types.StartWorker(_) -> True
      _ -> False
    }
  })
  assert !list.any(effects, fn(effect) {
    case effect {
      effects_types.AppendLedger(_) -> True
      _ -> False
    }
  })
  assert list.any(effects, fn(effect) {
    effect == effects_types.DeferRetryTimer("issue-1", 3, 40_000)
  })
}

pub fn claim_start_batch_appends_retry_cancellation_after_workflow_start_test() {
  let workflow_started =
    record.WorkflowRunStartedWithTask(
      "run-1",
      "default",
      "workflow-fingerprint",
      "issue-1",
      "ABC-1",
      record.linear_task_ref_fields("issue-1", Some("ABC-1"), None),
      "issue-fingerprint",
      123,
      "test/tmp/workspaces/ABC-1",
    )
  let batch =
    ledger_batch.claim_started(
      workflow_started,
      "issue-1",
      "ABC-1",
      "test/tmp/workspaces/ABC-1",
      0,
      1,
      456,
    )
    |> ledger_batch.append_retry_cancelled("issue-1", 3, "retry_dispatch")

  assert ledger_batch.to_bodies(batch)
    == [
      workflow_started,
      record.KnownWorkspace("issue-1", "ABC-1", "test/tmp/workspaces/ABC-1"),
      record.IssueCounterUpdated("issue-1", "ABC-1", 0, 1, 456, None),
      record.RetryCancelled("issue-1", 3, "retry_dispatch"),
    ]
}

pub fn claim_requested_empty_batch_does_not_emit_append_or_start_worker_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = orchestrator_transition_test.state_with_pending_claim(issue)

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.HandoffClaimCompleted(
        task_identity: orchestrator_state.linear_issue_id_identity("issue-1"),
        issue_id: identity.issue_id_from_string("issue-1"),
        run_id: identity.run_id_from_string("run-1"),
        result: transition_types.HandoffClaimSucceeded(ledger_batch.empty()),
      ),
      state,
    )

  let identity = orchestrator_state.issue_identity(issue)
  assert dict.get(next.pending_claims, identity) == Error(Nil)
  assert effects
    == [
      effects_types.Log("warn", "claim_ledger_append_empty", [
        #("issue_id", "issue-1"),
        #("run_id", "run-1"),
        #("correlation_id", "claim:issue-1:run-1"),
      ]),
    ]
}

pub fn claim_requested_missing_workflow_start_does_not_emit_append_or_start_worker_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = orchestrator_transition_test.state_with_pending_claim(issue)

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.HandoffClaimCompleted(
        task_identity: orchestrator_state.linear_issue_id_identity("issue-1"),
        issue_id: identity.issue_id_from_string("issue-1"),
        run_id: identity.run_id_from_string("run-1"),
        result: transition_types.HandoffClaimSucceeded(
          ledger_batch.step_attempt_started(
            "run-1",
            "default",
            "build",
            1,
            "session-1",
            None,
            True,
          ),
        ),
      ),
      state,
    )

  let identity = orchestrator_state.issue_identity(issue)
  assert dict.get(next.pending_claims, identity) == Error(Nil)
  assert effects
    == [
      effects_types.Log("warn", "claim_ledger_append_invalid_claim_started", [
        #("issue_id", "issue-1"),
        #("run_id", "run-1"),
        #("correlation_id", "claim:issue-1:run-1"),
      ]),
    ]
}

fn assert_cancelled_workflow_append(
  effects: List(effects_types.Effect),
  correlation_id: String,
) {
  assert list.any(effects, fn(effect) {
    case effect {
      effects_types.AppendLedger(effects_types.LedgerAppend(
        correlation_id: id,
        batch: batch,
        failure_event: "workflow_terminal_append_failed",
        policy: effects_types.ContinueRegardless,
      )) ->
        id == correlation_id
        && ledger_batch.to_bodies(batch)
        == [
          record.WorkflowRunFinishedWithTask(
            "run-1",
            "default",
            "issue-1",
            record.linear_task_ref_fields("issue-1", Some("ABC-1"), None),
            "cancelled",
            0,
            0,
          ),
        ]
      _ -> False
    }
  })
}

fn assert_stop_after_issue_refresh(
  effects: List(effects_types.Effect),
  reason: orchestrator_reason.StopReason,
) {
  assert list.any(effects, fn(effect) {
    case effect {
      effects_types.StopWorkerAfterIssueRefresh(_, actual_reason) ->
        actual_reason == reason
      _ -> False
    }
  })
}

fn has_finished_operator_command(
  effects: List(effects_types.Effect),
  expected_request: effects_types.OperatorCommandRequest,
  expected_result: command.CommandResult,
) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.FinishOperatorCommand(request, result) ->
        request == expected_request && result == expected_result
      _ -> False
    }
  })
}

fn has_dispatch_pause_append_failed_log(
  effects: List(effects_types.Effect),
  expected_status: String,
) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.Log(
        "warn",
        "operator_dispatch_pause_ledger_append_failed",
        fields,
      ) ->
        field_equals(fields, "status", expected_status)
        && field_equals(fields, "error", "io")
      _ -> False
    }
  })
}

fn field_equals(
  fields: List(#(String, String)),
  expected_key: String,
  expected_value: String,
) -> Bool {
  list.any(fields, fn(field) {
    let #(key, value) = field
    key == expected_key && value == expected_value
  })
}

fn running_worker_state_with_counter(
  issue: tracker_issue.Issue,
  counter: orchestrator_state.IssueCounter,
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
      issue_counters: dict.from_list([#(task_identity, counter)]),
    )
  let worker =
    transition_types.WorkerEntry(
      task_ref: task_ref,
      issue_id: issue.id,
      run_id: "run-1",
      session_id: "session-1",
      issue: issue,
      workspace_path: "test/tmp/workspaces/ABC-1",
      workflow_id: "default",
      workflow_snapshot: None,
      command_route_id: "worker:run-1:1",
      status: transition_types.WorkerRunning,
      recovery: None,
    )
  transition_types.State(
    ..orchestrator_transition_test.fixture_state(),
    runtime: runtime,
    workers: transition_types.WorkerDirectory(
      by_issue: dict.from_list([#(task_identity, worker)]),
      by_session: dict.from_list([#("session-1", task_identity)]),
      route_to_session: dict.from_list([#("worker:run-1:1", "session-1")]),
      yaml_step_runs: dict.new(),
      stopped_yaml_runs: dict.new(),
    ),
  )
}

fn lifecycle_context(now_ms: Int) -> transition_types.WorkerLifecycleContext {
  transition_types.WorkerLifecycleContext(
    effective: orchestrator_transition_test.fixture_effective(),
    now_ms: now_ms,
    secrets: [],
  )
}
