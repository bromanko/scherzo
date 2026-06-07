import gleam/dict
import gleam/list
import gleam/option.{None}
import orchestrator_transition_invariant_helpers as invariant_helpers
import orchestrator_transition_test
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/transition
import scherzo/orchestrator/transition_types
import scherzo/runtime/identity
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
import scherzo/state/ledger
import scherzo/state/ledger_batch
import scherzo/state/record
import scherzo/task

pub fn ledger_spawn_continuation_success_emits_start_worker_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = orchestrator_transition_test.state_with_pending_claim(issue)

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.LedgerAppendCompleted(
        correlation_id: "claim:issue-1:run-1",
        continuation: effects_types.SpawnClaimedWorkerAfterAppend(
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

pub fn ledger_spawn_continuation_failure_schedules_recovery_retry_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = orchestrator_transition_test.state_with_pending_claim(issue)

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.LedgerAppendCompleted(
        correlation_id: "claim:issue-1:run-1",
        continuation: effects_types.SpawnClaimedWorkerAfterAppend(
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
        policy: effects_types.ContinueRegardless,
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
  assert list.any(effects, fn(effect) {
    effect
    == effects_types.ScheduleRetryTimer(
      "issue-1",
      10_000,
      1,
      orchestrator_reason.RetryClaimStartLedgerAppendFailed,
    )
  })
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
        continuation: effects_types.SpawnClaimedWorkerAfterAppend(
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
  assert retry.delay_ms == 80_000
  assert retry.timer_generation == 4
  assert dict.get(next.runtime.claimed, task_identity) == Ok("ABC-1")
  invariant_helpers.assert_valid_state(next)
  assert !list.any(effects, fn(effect) {
    case effect {
      effects_types.StartWorker(_) -> True
      _ -> False
    }
  })
  assert list.any(effects, fn(effect) {
    case effect {
      effects_types.AppendLedger(effects_types.LedgerAppend(
        correlation_id: "claim_start_retry_schedule:issue-1:4",
        batch: batch,
        failure_event: "ledger_append_failed",
        policy: effects_types.ContinueRegardless,
      )) ->
        ledger_batch.to_bodies(batch)
        == [
          record.RetryScheduled(
            "issue-1",
            "ABC-1",
            80_000,
            4,
            "claim_start_ledger_append_failed",
          ),
        ]
      _ -> False
    }
  })
  assert list.any(effects, fn(effect) {
    effect
    == effects_types.ScheduleRetryTimer(
      "issue-1",
      80_000,
      4,
      orchestrator_reason.RetryClaimStartLedgerAppendFailed,
    )
  })
}

pub fn claim_requested_empty_batch_does_not_emit_append_or_start_worker_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = orchestrator_transition_test.state_with_pending_claim(issue)

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.ClaimLedgerAppendRequested(
        correlation_id: "claim:issue-1:run-1",
        task_identity: orchestrator_state.linear_issue_id_identity("issue-1"),
        issue_id: identity.issue_id_from_string("issue-1"),
        run_id: identity.run_id_from_string("run-1"),
        session_id: identity.session_id_from_string("session-1"),
        batch: ledger_batch.empty(),
        failure_event: "ledger_append_failed",
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
      transition_types.ClaimLedgerAppendRequested(
        correlation_id: "claim:issue-1:run-1",
        task_identity: orchestrator_state.linear_issue_id_identity("issue-1"),
        issue_id: identity.issue_id_from_string("issue-1"),
        run_id: identity.run_id_from_string("run-1"),
        session_id: identity.session_id_from_string("session-1"),
        batch: ledger_batch.step_attempt_started(
          "run-1",
          "default",
          "build",
          1,
          "session-1",
          None,
          True,
        ),
        failure_event: "ledger_append_failed",
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
