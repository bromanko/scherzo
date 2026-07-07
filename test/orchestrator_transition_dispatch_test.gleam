import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import orchestrator_transition_invariant_helpers as invariant_helpers
import orchestrator_transition_test
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/orchestrator/daemon_transition_shell
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/task_lifecycle
import scherzo/orchestrator/task_lifecycle_legacy
import scherzo/orchestrator/transition
import scherzo/orchestrator/transition_types
import scherzo/review_lane_preflight
import scherzo/review_lane_preflight_policy
import scherzo/runtime/identity
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
import scherzo/session/event as session_event
import scherzo/state/ledger_batch
import scherzo/state/record
import scherzo/task
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_completion_policy

pub fn stale_poll_generation_ignored_test() {
  let transition_types.Outcome(effects: effects, ..) =
    transition.handle(
      transition_types.PollTick(
        1,
        transition_types.PollSnapshot(generation: 2, in_flight: None),
      ),
      orchestrator_transition_test.fixture_state(),
    )

  assert effects == []
}

pub fn initial_dispatch_skips_non_dispatch_state_test() {
  let candidate =
    tracker_issue.Issue(
      ..orchestrator_transition_test.fixture_issue(),
      state: issue_state.from_string_unchecked("In Progress"),
    )

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.DispatchCandidates(
        [candidate],
        orchestrator_transition_test.fixture_context(),
      ),
      orchestrator_transition_test.fixture_state(),
    )

  assert dict.size(next.pending_dispatch_validations) == 0
  assert effects == []
}

pub fn conflicting_lifecycle_sources_fail_closed_and_log_test() {
  assert_lifecycle_projection_error_blocks_dispatch(
    state_with_conflicting_lifecycle_sources(),
    "conflicting_lifecycle_sources",
  )
}

pub fn missing_claimed_lifecycle_fails_closed_and_logs_test() {
  assert_lifecycle_projection_error_blocks_dispatch(
    state_with_missing_claimed_lifecycle(),
    "missing_claimed_lifecycle",
  )
}

pub fn missing_retry_waiting_for_refresh_fails_closed_and_logs_test() {
  assert_lifecycle_projection_error_blocks_dispatch(
    state_with_missing_retry_waiting_for_refresh(),
    "missing_retry_waiting_for_refresh",
  )
}

pub fn running_worker_mismatch_fails_closed_and_logs_test() {
  assert_lifecycle_projection_error_blocks_dispatch(
    state_with_running_worker_mismatch(),
    "running_worker_mismatch",
  )
}

pub fn mixed_retry_refresh_identity_projects_without_fail_closed_test() {
  let issue = lifecycle_projection_error_issue()
  let state = state_with_retry_refresh_identity_mismatch(issue)
  let task_ref = task.from_legacy_issue(issue).ref
  let task_identity = orchestrator_state.task_ref_identity(task_ref)

  assert !daemon_transition_shell.lifecycle_projection_failed(state)
  let assert Ok(directory) = task_lifecycle_legacy.from_transition_state(state)
  let assert Ok(lifecycle) = task_lifecycle.get(directory, task_identity)
  let assert task_lifecycle.RetryRefreshing(_, issue_id, generation, delay_ms) =
    lifecycle
  assert issue_id == issue.id
  assert generation == 7
  assert delay_ms == 1000
}

pub fn automatic_retry_non_dispatch_state_dispatches_test() {
  let issue =
    tracker_issue.Issue(
      ..orchestrator_transition_test.fixture_issue(),
      state: issue_state.from_string_unchecked("Triage"),
    )

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.RetryRefreshCompleted(
        issue.id,
        1,
        Ok([issue]),
        context_with_failure_state("Triage"),
      ),
      state_with_retry(issue),
    )

  assert dict.has_key(
    next.pending_claims,
    orchestrator_state.issue_identity(issue),
  )
  assert !dict.has_key(
    next.runtime.retry_attempts,
    orchestrator_state.issue_identity(issue),
  )
  assert has_claim_issue(effects)
  assert has_pending_retry_cancellation(next, issue.id, "retry_dispatch")
  assert !has_cancel_retry_reason(effects, "retry_not_dispatchable")
}

pub fn automatic_retry_non_retryable_state_is_rejected_test() {
  let issue =
    tracker_issue.Issue(
      ..orchestrator_transition_test.fixture_issue(),
      state: issue_state.from_string_unchecked("Backlog"),
    )

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.RetryRefreshCompleted(
        issue.id,
        1,
        Ok([issue]),
        context_with_failure_state("Triage"),
      ),
      state_with_retry(issue),
    )

  assert !dict.has_key(
    next.pending_claims,
    orchestrator_state.issue_identity(issue),
  )
  assert !dict.has_key(
    next.runtime.retry_attempts,
    orchestrator_state.issue_identity(issue),
  )
  assert !dict.has_key(
    next.runtime.claimed,
    orchestrator_state.issue_identity(issue),
  )
  assert !has_claim_issue(effects)
  assert has_cancel_retry_reason(effects, "retry_non_retryable_state:Backlog")
}

pub fn automatic_retry_dispatch_state_dispatches_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let recovery = recovery_info("recovered-run")
  let context =
    transition_types.DispatchContext(
      ..orchestrator_transition_test.fixture_context(),
      recovery_by_issue: dict.from_list([#(issue.id, recovery)]),
    )

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.RetryRefreshCompleted(issue.id, 1, Ok([issue]), context),
      state_with_retry(issue),
    )

  let task_identity = orchestrator_state.issue_identity(issue)
  assert dict.has_key(next.pending_claims, task_identity)
  let assert Ok(pending) = dict.get(next.pending_claims, task_identity)
  assert pending.recovery == Some(recovery)
  assert !dict.has_key(next.runtime.retry_attempts, task_identity)
  assert has_claim_issue(effects)
  assert has_pending_retry_cancellation(next, issue.id, "retry_dispatch")
}

pub fn retry_handoff_claim_failed_restores_retry_state_test() {
  assert_retry_handoff_failure_restores_retry(
    transition_types.HandoffClaimFailed("linear_api_request"),
  )
}

pub fn retry_handoff_claim_start_record_failed_restores_retry_state_test() {
  assert_retry_handoff_failure_restores_retry(
    transition_types.HandoffClaimStartRecordFailed("append_failed"),
  )
}

pub fn retry_workflow_route_failure_restores_retry_state_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let context =
    transition_types.DispatchContext(
      ..orchestrator_transition_test.fixture_context(),
      routing: config_types.RoutingConfig(
        workflow_label_prefix: "workflow:",
        require_exactly_one_workflow_label: True,
        default_workflow: None,
        workflows: dict.from_list([#("default", "workflows/default.yaml")]),
      ),
    )

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.RetryRefreshCompleted(issue.id, 1, Ok([issue]), context),
      state_with_retry(issue),
    )

  assert_retry_pre_claim_failure_restores_retry(next, effects, issue)
  assert has_log(effects, "workflow_route_failed")
}

pub fn retry_workspace_path_failure_restores_retry_state_test() {
  let issue =
    tracker_issue.Issue(
      ..orchestrator_transition_test.fixture_issue(),
      identifier: ".",
    )

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.RetryRefreshCompleted(
        issue.id,
        1,
        Ok([issue]),
        orchestrator_transition_test.fixture_context(),
      ),
      state_with_retry(issue),
    )

  assert_retry_pre_claim_failure_restores_retry(next, effects, issue)
  assert has_log(effects, "dispatch_workspace_path_failed")
}

pub fn retry_workflow_snapshot_failure_restores_retry_state_test() {
  let issue = labelled_issue("issue-1", "ABC-1", "workflow:implementation")
  let policy =
    review_lane_preflight_policy.Policy(
      mode: review_lane_preflight_policy.Off,
      cache_ttl_seconds: 86_400,
      park_on_failure: True,
      strict_live_model_checks: False,
    )
  let context =
    transition_types.DispatchContext(
      ..context_requiring_preflight(policy),
      review_lane_preflight: transition_types.ReviewLanePreflightContext(
        config_dir: ".scherzo",
        workflow_dags: dict.new(),
        policy: policy,
        override: None,
      ),
    )

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.RetryRefreshCompleted(issue.id, 1, Ok([issue]), context),
      state_with_retry(issue),
    )

  assert_retry_pre_claim_failure_restores_retry(next, effects, issue)
  assert has_workflow_route_snapshot_failed_log(
    effects,
    "unknown_workflow_label",
  )
}

pub fn retry_refresh_completion_while_paused_defers_without_claim_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let context =
    transition_types.DispatchContext(
      ..orchestrator_transition_test.fixture_context(),
      operator_paused: True,
    )

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.RetryRefreshCompleted(issue.id, 1, Ok([issue]), context),
      state_with_retry(issue),
    )

  assert !dict.has_key(
    next.pending_claims,
    orchestrator_state.issue_identity(issue),
  )
  assert dict.has_key(
    next.runtime.retry_attempts,
    orchestrator_state.issue_identity(issue),
  )
  assert !has_claim_issue(effects)
  assert list.any(effects, fn(effect) {
    effect == effects_types.DeferRetryTimer(issue.id, 1, 60_000)
  })
  assert list.any(effects, fn(effect) {
    effect
    == effects_types.Log("info", "retry_deferred_dispatch_paused", [
      #("issue_id", issue.id),
    ])
  })
}

pub fn retry_tick_while_paused_uses_backoff_without_warn_spin_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let context =
    transition_types.DispatchContext(
      ..orchestrator_transition_test.fixture_context(),
      operator_paused: True,
    )

  let transition_types.Outcome(effects: effects, ..) =
    transition.handle(
      transition_types.RetryTick(issue.id, 1, context),
      state_with_retry(issue),
    )

  assert list.any(effects, fn(effect) {
    effect == effects_types.DeferRetryTimer(issue.id, 1, 60_000)
  })
  assert !list.any(effects, fn(effect) {
    case effect {
      effects_types.Log("warn", "retry_deferred_dispatch_unavailable", _) ->
        True
      _ -> False
    }
  })
}

pub fn retry_refresh_without_retry_state_is_treated_as_stale_test() {
  let issue = orchestrator_transition_test.fixture_issue()

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.RetryRefreshCompleted(
        issue.id,
        1,
        Ok([issue]),
        orchestrator_transition_test.fixture_context(),
      ),
      orchestrator_transition_test.fixture_state(),
    )

  assert dict.size(next.pending_claims) == 0
  assert !has_claim_issue(effects)
  assert !has_cancel_retry_reason(effects, "retry_dispatch")
  assert list.any(effects, fn(effect) {
    effect
    == effects_types.Log("info", "retry_timer_stale", [
      #("issue_id", issue.id),
    ])
  })
}

pub fn explicit_retry_non_dispatch_state_dispatches_test() {
  let issue =
    tracker_issue.Issue(
      ..orchestrator_transition_test.fixture_issue(),
      state: issue_state.from_string_unchecked("Triage"),
    )
  let request = retry_request(issue.id)

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.OperatorCommandSubmitted(
        request: request,
        context: context_with_failure_state("Triage"),
        issue_resolution: transition_types.OperatorIssueResolved(issue),
        parked_issue_resolution: transition_types.ParkedIssueNotResolved,
      ),
      orchestrator_transition_test.fixture_state(),
    )

  assert dict.has_key(
    next.pending_claims,
    orchestrator_state.issue_identity(issue),
  )
  assert has_claim_issue(effects)
  assert has_finished_operator_applied(effects)
}

pub fn explicit_retry_discards_stale_recovery_for_claim_and_worker_start_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let request = retry_request(issue.id)
  let other_issue_id = "other-issue"
  let other_recovery = recovery_info("other-run")
  let context =
    transition_types.DispatchContext(
      ..orchestrator_transition_test.fixture_context(),
      recovery_by_issue: dict.from_list([
        #(issue.id, recovery_info("stale-run")),
        #(other_issue_id, other_recovery),
      ]),
    )

  let transition_types.Outcome(state: claiming, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.OperatorCommandSubmitted(
        request: request,
        context: context,
        issue_resolution: transition_types.OperatorIssueResolved(issue),
        parked_issue_resolution: transition_types.ParkedIssueNotResolved,
      ),
      orchestrator_transition_test.fixture_state(),
    )

  let task_identity = orchestrator_state.issue_identity(issue)
  let assert Ok(pending) = dict.get(claiming.pending_claims, task_identity)
  assert pending.recovery == None
  assert dict.get(pending.dispatch_context.recovery_by_issue, issue.id)
    == Error(Nil)
  assert dict.get(pending.dispatch_context.recovery_by_issue, other_issue_id)
    == Ok(other_recovery)
  assert list.any(effects, fn(effect) {
    effect == effects_types.ClearRecovery(issue.id)
  })

  let transition_types.Outcome(effects: worker_effects, ..) =
    invariant_helpers.handle_and_assert(
      transition_types.LedgerAppendCompleted(
        correlation_id: "claim-start",
        continuation: transition_types.SpawnClaimedWorkerAfterAppend(
          task_identity: task_identity,
          issue_id: identity.issue_id_from_string(issue.id),
          run_id: identity.run_id_from_string(pending.run_id),
          session_id: identity.session_id_from_string(pending.session_id),
        ),
        result: Ok(Nil),
        now_ms: 123,
      ),
      claiming,
    )
  let assert [worker_start] = worker_start_requests(worker_effects)
  assert worker_start.recovery == None
}

pub fn explicit_retry_non_retryable_state_is_rejected_test() {
  let issue =
    tracker_issue.Issue(
      ..orchestrator_transition_test.fixture_issue(),
      state: issue_state.from_string_unchecked("Backlog"),
    )
  let request = retry_request(issue.id)

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.OperatorCommandSubmitted(
        request: request,
        context: context_with_failure_state("Triage"),
        issue_resolution: transition_types.OperatorIssueResolved(issue),
        parked_issue_resolution: transition_types.ParkedIssueNotResolved,
      ),
      orchestrator_transition_test.fixture_state(),
    )

  assert !dict.has_key(
    next.pending_claims,
    orchestrator_state.issue_identity(issue),
  )
  assert !has_claim_issue(effects)
  assert has_finished_operator_rejected(
    effects,
    "retry_non_retryable_state:Backlog",
  )
}

pub fn explicit_retry_auto_unparks_changed_issue_test() {
  let original = orchestrator_transition_test.fixture_issue()
  let issue =
    tracker_issue.Issue(
      ..original,
      title: original.title <> " (updated)",
      state: issue_state.from_string_unchecked("Triage"),
    )
  let request = retry_request(issue.id)
  let parked =
    orchestrator_state.ParkedEntry(
      task_ref: task.from_legacy_issue(original).ref,
      issue_id: original.id,
      identifier: original.identifier,
      reason: orchestrator_reason.ParkMaxRetryAttempts,
      release_policy: orchestrator_state.AutoUnparkOnIssueChange(
        tracker_issue.content_fingerprint(original),
      ),
      parked_at_ms: 123,
    )
  let runtime =
    orchestrator_state.RuntimeState(
      ..orchestrator_transition_test.fixture_runtime(),
      parked: dict.from_list([
        #(orchestrator_state.issue_identity(original), parked),
      ]),
    )
  let state =
    transition_types.State(
      ..orchestrator_transition_test.fixture_state(),
      runtime: runtime,
    )

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.OperatorCommandSubmitted(
        request: request,
        context: context_with_failure_state("Triage"),
        issue_resolution: transition_types.OperatorIssueResolved(issue),
        parked_issue_resolution: transition_types.ParkedIssueNotResolved,
      ),
      state,
    )

  assert dict.has_key(
    next.pending_claims,
    orchestrator_state.issue_identity(issue),
  )
  assert !dict.has_key(
    next.runtime.parked,
    orchestrator_state.issue_identity(issue),
  )
  assert has_claim_issue(effects)
  assert has_finished_operator_applied(effects)
}

pub fn explicit_retry_auto_unparks_failure_quarantine_test() {
  let issue =
    tracker_issue.Issue(
      ..orchestrator_transition_test.fixture_issue(),
      state: issue_state.from_string_unchecked("Triage"),
    )
  let request = retry_request(issue.id)
  let parked =
    orchestrator_state.ParkedEntry(
      task_ref: task.from_legacy_issue(issue).ref,
      issue_id: issue.id,
      identifier: issue.identifier,
      reason: orchestrator_reason.ParkWorkerFailure,
      release_policy: orchestrator_state.AutoUnparkOnIssueChange(
        tracker_issue.content_fingerprint(issue),
      ),
      parked_at_ms: 123,
    )
  let runtime =
    orchestrator_state.mark_task_parked(
      orchestrator_transition_test.fixture_runtime(),
      orchestrator_state.issue_identity(issue),
      parked,
    )
  let state =
    transition_types.State(
      ..orchestrator_transition_test.fixture_state(),
      runtime: runtime,
    )

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.OperatorCommandSubmitted(
        request: request,
        context: context_with_failure_state("Triage"),
        issue_resolution: transition_types.OperatorIssueResolved(issue),
        parked_issue_resolution: transition_types.ParkedIssueNotResolved,
      ),
      state,
    )

  assert dict.has_key(
    next.pending_claims,
    orchestrator_state.issue_identity(issue),
  )
  assert !dict.has_key(
    next.runtime.parked,
    orchestrator_state.issue_identity(issue),
  )
  assert has_claim_issue(effects)
  assert has_issue_unparked_append(effects, issue.id, "operator_retry")
  assert has_finished_operator_applied(effects)
}

pub fn explicit_retry_preserves_parked_safety_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let request = retry_request(issue.id)
  let parked =
    orchestrator_state.ParkedEntry(
      task_ref: task.from_legacy_issue(issue).ref,
      issue_id: issue.id,
      identifier: issue.identifier,
      reason: orchestrator_reason.ParkOperator("operator_hold"),
      release_policy: orchestrator_state.ExplicitUnparkOnly,
      parked_at_ms: 123,
    )
  let runtime =
    orchestrator_state.mark_task_parked(
      orchestrator_transition_test.fixture_runtime(),
      orchestrator_state.issue_identity(issue),
      parked,
    )
  let state =
    transition_types.State(
      ..orchestrator_transition_test.fixture_state(),
      runtime: runtime,
    )

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.OperatorCommandSubmitted(
        request: request,
        context: orchestrator_transition_test.fixture_context(),
        issue_resolution: transition_types.OperatorIssueResolved(issue),
        parked_issue_resolution: transition_types.ParkedIssueNotResolved,
      ),
      state,
    )

  assert !dict.has_key(
    next.pending_claims,
    orchestrator_state.issue_identity(issue),
  )
  assert dict.has_key(
    next.runtime.parked,
    orchestrator_state.issue_identity(issue),
  )
  assert !has_claim_issue(effects)
  assert has_finished_operator_rejected(effects, "retry_issue_parked")
  assert has_operator_rejection_message(
    effects,
    "retry rejected: issue is parked for operator_hold; no run, park, or tracker state was changed. Next safe command: scripts/scherzoctl unpark 'ABC-1' --json",
  )
}

pub fn explicit_retry_preserves_max_sessions_safety_park_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let request = retry_request(issue.id)
  let parked =
    orchestrator_state.ParkedEntry(
      task_ref: task.from_legacy_issue(issue).ref,
      issue_id: issue.id,
      identifier: issue.identifier,
      reason: orchestrator_reason.ParkMaxSessionsPerIssue,
      release_policy: orchestrator_state.AutoUnparkOnIssueChange(
        tracker_issue.content_fingerprint(issue),
      ),
      parked_at_ms: 123,
    )
  let runtime =
    orchestrator_state.mark_task_parked(
      orchestrator_transition_test.fixture_runtime(),
      orchestrator_state.issue_identity(issue),
      parked,
    )
  let state =
    transition_types.State(
      ..orchestrator_transition_test.fixture_state(),
      runtime: runtime,
    )

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.OperatorCommandSubmitted(
        request: request,
        context: orchestrator_transition_test.fixture_context(),
        issue_resolution: transition_types.OperatorIssueResolved(issue),
        parked_issue_resolution: transition_types.ParkedIssueNotResolved,
      ),
      state,
    )

  assert !dict.has_key(
    next.pending_claims,
    orchestrator_state.issue_identity(issue),
  )
  assert dict.has_key(
    next.runtime.parked,
    orchestrator_state.issue_identity(issue),
  )
  assert !has_claim_issue(effects)
  assert has_finished_operator_rejected(effects, "retry_issue_parked")
  assert has_operator_rejection_message(
    effects,
    "retry rejected: issue is parked for max_sessions_per_issue; no run, park, or tracker state was changed. Next safe command: scripts/scherzoctl unpark 'ABC-1' --json",
  )
}

pub fn explicit_retry_quotes_unpark_advice_target_test() {
  let issue =
    tracker_issue.Issue(
      ..orchestrator_transition_test.fixture_issue(),
      identifier: "ABC 1;$(touch x)",
    )
  let request = retry_request(issue.id)
  let parked =
    orchestrator_state.ParkedEntry(
      task_ref: task.from_legacy_issue(issue).ref,
      issue_id: issue.id,
      identifier: issue.identifier,
      reason: orchestrator_reason.ParkOperator("operator_hold"),
      release_policy: orchestrator_state.ExplicitUnparkOnly,
      parked_at_ms: 123,
    )
  let runtime =
    orchestrator_state.mark_task_parked(
      orchestrator_transition_test.fixture_runtime(),
      orchestrator_state.issue_identity(issue),
      parked,
    )
  let state =
    transition_types.State(
      ..orchestrator_transition_test.fixture_state(),
      runtime: runtime,
    )

  let transition_types.Outcome(effects: effects, ..) =
    invariant_helpers.handle_and_assert(
      transition_types.OperatorCommandSubmitted(
        request: request,
        context: orchestrator_transition_test.fixture_context(),
        issue_resolution: transition_types.OperatorIssueResolved(issue),
        parked_issue_resolution: transition_types.ParkedIssueNotResolved,
      ),
      state,
    )

  assert has_operator_rejection_message(
    effects,
    "retry rejected: issue is parked for operator_hold; no run, park, or tracker state was changed. Next safe command: scripts/scherzoctl unpark 'ABC 1;$(touch x)' --json",
  )
}

pub fn blocked_dependency_candidate_skipped_and_reported_test() {
  let blocker =
    tracker_issue.BlockerRef(
      id: Some("blocker-id"),
      identifier: Some("BLK-1"),
      state: Some(issue_state.from_string_unchecked("Todo")),
    )
  let candidate =
    tracker_issue.Issue(
      ..orchestrator_transition_test.fixture_issue(),
      blocked_by: [blocker],
      blocked_by_complete: True,
    )

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.DispatchCandidates(
        [candidate],
        orchestrator_transition_test.fixture_context(),
      ),
      orchestrator_transition_test.fixture_state(),
    )

  assert dict.size(next.pending_dispatch_validations) == 0
  let assert [effects_types.Log(_, "linear_dependency_blocked_candidate", _)] =
    effects
}

pub fn invalid_workflow_candidate_reported_test() {
  let context = enforcing_context()
  let candidate = orchestrator_transition_test.fixture_issue()

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.DispatchCandidates([candidate], context),
      orchestrator_transition_test.fixture_state(),
    )

  assert dict.has_key(
    next.runtime.invalid_workflow_reports,
    orchestrator_state.issue_identity(candidate),
  )
  assert list.any(effects, fn(effect) {
    case effect {
      effects_types.ReportInvalidWorkflow(issue, _, _, _) ->
        issue.id == candidate.id
      _ -> False
    }
  })
}

pub fn no_dispatch_slot_available_skips_validation_test() {
  let candidate = labelled_issue("issue-2", "ABC-2", "workflow:implementation")
  let running = labelled_issue("issue-1", "ABC-1", "workflow:implementation")
  let runtime =
    orchestrator_state.RuntimeState(
      ..orchestrator_transition_test.fixture_runtime(),
      running: dict.from_list([
        #(
          orchestrator_state.issue_identity(running),
          orchestrator_state.RunningEntry(
            task: task.from_legacy_issue(running),
            issue: running,
            workspace_path: "test/tmp/workspaces/ABC-1",
            session: None,
          ),
        ),
      ]),
    )
  let state =
    transition_types.State(
      ..orchestrator_transition_test.fixture_state(),
      runtime: runtime,
    )
  let context =
    transition_types.DispatchContext(
      ..orchestrator_transition_test.fixture_context(),
      effective: config_types.EffectiveConfig(
        ..orchestrator_transition_test.fixture_effective(),
        agent: config_types.AgentConfig(
          ..orchestrator_transition_test.fixture_effective().agent,
          max_concurrent_agents: 1,
        ),
      ),
    )

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.DispatchCandidates([candidate], context),
      state,
    )

  assert dict.size(next.pending_dispatch_validations) == 0
  assert !list.any(effects, fn(effect) {
    case effect {
      effects_types.BeginDispatchValidation(_, _) -> True
      _ -> False
    }
  })
}

pub fn dispatch_validation_completion_while_paused_does_not_claim_test() {
  let candidate = labelled_issue("issue-1", "ABC-1", "workflow:implementation")
  let context =
    transition_types.DispatchContext(
      ..context_requiring_preflight(review_lane_preflight_policy.Policy(
        mode: review_lane_preflight_policy.Off,
        cache_ttl_seconds: 86_400,
        park_on_failure: True,
        strict_live_model_checks: False,
      )),
      operator_paused: True,
    )
  let state = state_with_pending_dispatch_validation(candidate)

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.DispatchValidationCompleted(
        candidate.id,
        1,
        Ok(candidate),
        context,
      ),
      state,
    )

  assert dict.size(next.pending_dispatch_validations) == 0
  assert dict.size(next.pending_claims) == 0
  assert !has_claim_issue(effects)
  assert !has_start_worker(effects)
  assert list.any(effects, fn(effect) {
    case effect {
      effects_types.Log(
        "info",
        "dispatch_validation_precondition_failed",
        fields,
      ) -> field_equals(fields, "reason", "operator_paused")
      _ -> False
    }
  })
}

pub fn review_lane_preflight_blocks_claim_before_tracker_claim_test() {
  let candidate = labelled_issue("issue-1", "ABC-1", "workflow:implementation")
  let context =
    context_with_preflight(
      review_lane_preflight_policy.Policy(
        mode: review_lane_preflight_policy.OfflineRequired,
        cache_ttl_seconds: 86_400,
        park_on_failure: True,
        strict_live_model_checks: False,
      ),
      review_lane_preflight.failed(
        "cache-key",
        "structured_output_tool_spec_provider_incompatible_schema",
        "disallowed keyword enum",
        True,
      ),
    )
  let state = state_with_pending_dispatch_validation(candidate)

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.DispatchValidationCompleted(
        candidate.id,
        1,
        Ok(candidate),
        context,
      ),
      state,
    )

  assert dict.size(next.pending_claims) == 0
  assert dict.has_key(
    next.runtime.parked,
    orchestrator_state.issue_identity(candidate),
  )
  assert !has_claim_issue(effects)
  assert !has_start_worker(effects)
  assert has_park_issue(effects)
  assert has_preflight_failure_log(effects)
}

pub fn review_lane_preflight_park_off_blocks_without_tracker_mutation_test() {
  let candidate = labelled_issue("issue-1", "ABC-1", "workflow:implementation")
  let context =
    context_with_preflight(
      review_lane_preflight_policy.Policy(
        mode: review_lane_preflight_policy.OfflineRequired,
        cache_ttl_seconds: 86_400,
        park_on_failure: False,
        strict_live_model_checks: False,
      ),
      review_lane_preflight.failed(
        "cache-key",
        "structured_output_tool_spec_provider_incompatible_schema",
        "provider rejected enum",
        True,
      ),
    )
  let state = state_with_pending_dispatch_validation(candidate)

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.DispatchValidationCompleted(
        candidate.id,
        1,
        Ok(candidate),
        context,
      ),
      state,
    )

  assert dict.size(next.pending_claims) == 0
  assert !dict.has_key(
    next.runtime.parked,
    orchestrator_state.issue_identity(candidate),
  )
  assert !has_claim_issue(effects)
  assert !has_start_worker(effects)
  assert !has_park_issue(effects)
  assert has_preflight_failure_log(effects)
}

pub fn review_lane_preflight_off_mode_allows_claim_test() {
  let candidate = labelled_issue("issue-1", "ABC-1", "workflow:implementation")
  let context =
    context_with_preflight(
      review_lane_preflight_policy.Policy(
        mode: review_lane_preflight_policy.Off,
        cache_ttl_seconds: 86_400,
        park_on_failure: True,
        strict_live_model_checks: False,
      ),
      review_lane_preflight.failed(
        "cache-key",
        "structured_output_tool_spec_provider_incompatible_schema",
        "provider rejected enum",
        True,
      ),
    )
  let state = state_with_pending_dispatch_validation(candidate)

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.DispatchValidationCompleted(
        candidate.id,
        1,
        Ok(candidate),
        context,
      ),
      state,
    )

  assert dict.has_key(
    next.pending_claims,
    orchestrator_state.issue_identity(candidate),
  )
  assert has_claim_issue(effects)
  assert !has_park_issue(effects)
  assert !has_preflight_failure_log(effects)
}

pub fn review_lane_preflight_nonblocking_warning_allows_claim_test() {
  let candidate = labelled_issue("issue-1", "ABC-1", "workflow:implementation")
  let context =
    context_with_preflight(
      review_lane_preflight_policy.Policy(
        mode: review_lane_preflight_policy.RequiredLive,
        cache_ttl_seconds: 86_400,
        park_on_failure: True,
        strict_live_model_checks: False,
      ),
      review_lane_preflight.failed(
        "cache-key",
        "model_payload_invalid",
        "model did not produce valid minimal payload",
        False,
      ),
    )
  let state = state_with_pending_dispatch_validation(candidate)

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.DispatchValidationCompleted(
        candidate.id,
        1,
        Ok(candidate),
        context,
      ),
      state,
    )

  assert dict.has_key(
    next.pending_claims,
    orchestrator_state.issue_identity(candidate),
  )
  assert has_claim_issue(effects)
  assert !has_park_issue(effects)
  assert !has_preflight_failure_log(effects)
}

pub fn review_lane_preflight_required_live_missing_credentials_blocks_claim_test() {
  let candidate = labelled_issue("issue-1", "ABC-1", "workflow:implementation")
  let context =
    context_with_preflight(
      review_lane_preflight_policy.Policy(
        mode: review_lane_preflight_policy.RequiredLive,
        cache_ttl_seconds: 86_400,
        park_on_failure: True,
        strict_live_model_checks: False,
      ),
      review_lane_preflight.failed(
        "cache-key",
        "review_lane_live_credentials_missing",
        "live credentials are missing",
        True,
      ),
    )
  let state = state_with_pending_dispatch_validation(candidate)

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.DispatchValidationCompleted(
        candidate.id,
        1,
        Ok(candidate),
        context,
      ),
      state,
    )

  assert dict.size(next.pending_claims) == 0
  assert !has_claim_issue(effects)
  assert !has_start_worker(effects)
  assert has_park_issue(effects)
  assert has_preflight_failure_log(effects)
}

pub fn review_lane_preflight_cached_blocking_failure_blocks_claim_test() {
  let candidate = labelled_issue("issue-1", "ABC-1", "workflow:implementation")
  let context =
    context_with_preflight(
      review_lane_preflight_policy.Policy(
        mode: review_lane_preflight_policy.OfflineRequired,
        cache_ttl_seconds: 86_400,
        park_on_failure: True,
        strict_live_model_checks: False,
      ),
      review_lane_preflight.failed(
        "cached-key",
        "review_lane_preflight_cached_blocking_failure",
        "cached provider registration failure has not expired",
        True,
      ),
    )
  let state = state_with_pending_dispatch_validation(candidate)

  let transition_types.Outcome(effects: effects, ..) =
    invariant_helpers.handle_and_assert(
      transition_types.DispatchValidationCompleted(
        candidate.id,
        1,
        Ok(candidate),
        context,
      ),
      state,
    )

  assert !has_claim_issue(effects)
  assert !has_start_worker(effects)
  assert has_park_issue(effects)
  assert has_preflight_failure_log(effects)
}

pub fn review_lane_preflight_without_override_starts_effect_before_claim_test() {
  let candidate = labelled_issue("issue-1", "ABC-1", "workflow:implementation")
  let context =
    context_requiring_preflight(review_lane_preflight_policy.Policy(
      mode: review_lane_preflight_policy.OfflineRequired,
      cache_ttl_seconds: 86_400,
      park_on_failure: True,
      strict_live_model_checks: False,
    ))
  let state = state_with_pending_dispatch_validation(candidate)
  let identity = orchestrator_state.issue_identity(candidate)

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.DispatchValidationCompleted(
        candidate.id,
        1,
        Ok(candidate),
        context,
      ),
      state,
    )

  assert dict.size(next.pending_claims) == 0
  assert dict.has_key(next.pending_review_lane_preflights, identity)
  assert !has_claim_issue(effects)
  assert has_begin_review_lane_preflight(effects)
}

pub fn review_lane_preflight_completion_allows_claim_test() {
  let candidate = labelled_issue("issue-1", "ABC-1", "workflow:implementation")
  let context =
    context_requiring_preflight(review_lane_preflight_policy.Policy(
      mode: review_lane_preflight_policy.OfflineRequired,
      cache_ttl_seconds: 86_400,
      park_on_failure: True,
      strict_live_model_checks: False,
    ))
  let identity = orchestrator_state.issue_identity(candidate)
  let transition_types.Outcome(state: waiting, ..) =
    invariant_helpers.handle_and_assert(
      transition_types.DispatchValidationCompleted(
        candidate.id,
        1,
        Ok(candidate),
        context,
      ),
      state_with_pending_dispatch_validation(candidate),
    )
  let assert Ok(pending) =
    dict.get(waiting.pending_review_lane_preflights, identity)
  let completion_context =
    transition_types.DispatchContext(..context, now_ms: 999)

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.ReviewLanePreflightCompleted(
        identity,
        candidate.id,
        pending.generation,
        "implementation",
        completion_context,
        review_lane_preflight.passed("cache-key"),
      ),
      waiting,
    )

  assert dict.size(next.pending_review_lane_preflights) == 0
  let assert Ok(pending_claim) = dict.get(next.pending_claims, identity)
  assert pending_claim.run_id == "ABC-1-999-1"
  assert has_claim_issue(effects)
}

pub fn review_lane_preflight_completion_while_paused_does_not_claim_test() {
  let candidate = labelled_issue("issue-1", "ABC-1", "workflow:implementation")
  let context =
    context_requiring_preflight(review_lane_preflight_policy.Policy(
      mode: review_lane_preflight_policy.OfflineRequired,
      cache_ttl_seconds: 86_400,
      park_on_failure: True,
      strict_live_model_checks: False,
    ))
  let identity = orchestrator_state.issue_identity(candidate)
  let transition_types.Outcome(state: waiting, ..) =
    invariant_helpers.handle_and_assert(
      transition_types.DispatchValidationCompleted(
        candidate.id,
        1,
        Ok(candidate),
        context,
      ),
      state_with_pending_dispatch_validation(candidate),
    )
  let assert Ok(pending) =
    dict.get(waiting.pending_review_lane_preflights, identity)
  let completion_context =
    transition_types.DispatchContext(..context, operator_paused: True)

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.ReviewLanePreflightCompleted(
        identity,
        candidate.id,
        pending.generation,
        "implementation",
        completion_context,
        review_lane_preflight.passed("cache-key"),
      ),
      waiting,
    )

  assert dict.size(next.pending_review_lane_preflights) == 0
  assert dict.size(next.pending_claims) == 0
  assert !has_claim_issue(effects)
  assert !has_start_worker(effects)
  assert has_preflight_paused_log(effects)
}

pub fn retry_review_lane_preflight_completion_while_paused_restores_retry_test() {
  let candidate = labelled_issue("issue-1", "ABC-1", "workflow:implementation")
  let context =
    context_requiring_preflight(review_lane_preflight_policy.Policy(
      mode: review_lane_preflight_policy.OfflineRequired,
      cache_ttl_seconds: 86_400,
      park_on_failure: True,
      strict_live_model_checks: False,
    ))
  let identity = orchestrator_state.issue_identity(candidate)
  let transition_types.Outcome(state: waiting, ..) =
    invariant_helpers.handle_and_assert(
      transition_types.RetryRefreshCompleted(
        candidate.id,
        1,
        Ok([candidate]),
        context,
      ),
      state_with_retry(candidate),
    )
  let assert Ok(pending) =
    dict.get(waiting.pending_review_lane_preflights, identity)
  let completion_context =
    transition_types.DispatchContext(..context, operator_paused: True)

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.ReviewLanePreflightCompleted(
        identity,
        candidate.id,
        pending.generation,
        "implementation",
        completion_context,
        review_lane_preflight.passed("cache-key"),
      ),
      waiting,
    )

  assert dict.size(next.pending_review_lane_preflights) == 0
  assert dict.size(next.pending_claims) == 0
  assert dict.has_key(next.runtime.retry_attempts, identity)
  assert dict.has_key(next.runtime.claimed, identity)
  assert !has_claim_issue(effects)
  assert list.any(effects, fn(effect) {
    effect == effects_types.DeferRetryTimer(candidate.id, 1, 60_000)
  })
  assert has_preflight_paused_log(effects)
}

pub fn review_lane_preflight_completion_blocking_failure_parks_test() {
  let candidate = labelled_issue("issue-1", "ABC-1", "workflow:implementation")
  let context =
    context_requiring_preflight(review_lane_preflight_policy.Policy(
      mode: review_lane_preflight_policy.OfflineRequired,
      cache_ttl_seconds: 86_400,
      park_on_failure: True,
      strict_live_model_checks: False,
    ))
  let identity = orchestrator_state.issue_identity(candidate)
  let transition_types.Outcome(state: waiting, ..) =
    invariant_helpers.handle_and_assert(
      transition_types.DispatchValidationCompleted(
        candidate.id,
        1,
        Ok(candidate),
        context,
      ),
      state_with_pending_dispatch_validation(candidate),
    )
  let assert Ok(pending) =
    dict.get(waiting.pending_review_lane_preflights, identity)

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.ReviewLanePreflightCompleted(
        identity,
        candidate.id,
        pending.generation,
        "implementation",
        context,
        review_lane_preflight.failed(
          "cache-key",
          "structured_output_tool_spec_provider_incompatible_schema",
          "provider rejected enum",
          True,
        ),
      ),
      waiting,
    )

  assert dict.size(next.pending_review_lane_preflights) == 0
  assert dict.size(next.pending_claims) == 0
  assert dict.has_key(next.runtime.parked, identity)
  assert !has_claim_issue(effects)
  assert !has_start_worker(effects)
  assert has_park_issue(effects)
  assert has_preflight_failure_log(effects)
}

pub fn review_lane_preflight_stale_completion_does_not_claim_test() {
  let candidate = labelled_issue("issue-1", "ABC-1", "workflow:implementation")
  let context =
    context_requiring_preflight(review_lane_preflight_policy.Policy(
      mode: review_lane_preflight_policy.OfflineRequired,
      cache_ttl_seconds: 86_400,
      park_on_failure: True,
      strict_live_model_checks: False,
    ))
  let identity = orchestrator_state.issue_identity(candidate)
  let transition_types.Outcome(state: waiting, ..) =
    invariant_helpers.handle_and_assert(
      transition_types.DispatchValidationCompleted(
        candidate.id,
        1,
        Ok(candidate),
        context,
      ),
      state_with_pending_dispatch_validation(candidate),
    )
  let assert Ok(pending) =
    dict.get(waiting.pending_review_lane_preflights, identity)

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.ReviewLanePreflightCompleted(
        identity,
        candidate.id,
        pending.generation + 1,
        "implementation",
        context,
        review_lane_preflight.passed("cache-key"),
      ),
      waiting,
    )

  assert dict.has_key(next.pending_review_lane_preflights, identity)
  assert dict.size(next.pending_claims) == 0
  assert !has_claim_issue(effects)
  assert !has_park_issue(effects)
  assert has_preflight_stale_log(effects)
}

pub fn workflow_route_snapshot_failure_skips_tracker_claim_test() {
  let candidate = labelled_issue("issue-1", "ABC-1", "workflow:implementation")
  let policy =
    review_lane_preflight_policy.Policy(
      mode: review_lane_preflight_policy.Off,
      cache_ttl_seconds: 86_400,
      park_on_failure: True,
      strict_live_model_checks: False,
    )
  let result =
    review_lane_preflight.failed(
      "cache-key",
      "ignored_in_off_mode",
      "ignored in off mode",
      True,
    )
  let context =
    transition_types.DispatchContext(
      ..context_with_preflight(policy, result),
      review_lane_preflight: transition_types.ReviewLanePreflightContext(
        config_dir: ".scherzo",
        workflow_dags: dict.new(),
        policy: policy,
        override: Some(result),
      ),
    )
  let state = state_with_pending_dispatch_validation(candidate)

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.DispatchValidationCompleted(
        candidate.id,
        1,
        Ok(candidate),
        context,
      ),
      state,
    )

  assert dict.size(next.pending_claims) == 0
  assert dict.size(next.pending_dispatch_validations) == 0
  assert !has_claim_issue(effects)
  assert has_workflow_route_snapshot_failed_log(
    effects,
    "unknown_workflow_label",
  )
}

pub fn workflow_route_selection_sets_pending_claim_workflow_test() {
  let candidate = labelled_issue("issue-1", "ABC-1", "workflow:review")
  let context =
    transition_types.DispatchContext(
      ..orchestrator_transition_test.fixture_context(),
      routing: config_types.RoutingConfig(
        workflow_label_prefix: "workflow:",
        require_exactly_one_workflow_label: True,
        default_workflow: None,
        workflows: dict.from_list([
          #("implementation", "workflows/implementation.yaml"),
          #("review", "workflows/review.yaml"),
        ]),
      ),
      available_workflow_ids: ["implementation", "review"],
      review_lane_preflight: transition_types.ReviewLanePreflightContext(
        config_dir: ".scherzo",
        workflow_dags: dict.from_list([
          #(
            "implementation",
            orchestrator_transition_test.fixture_workflow_dag("implementation"),
          ),
          #(
            "review",
            orchestrator_transition_test.fixture_workflow_dag("review"),
          ),
        ]),
        policy: review_lane_preflight_policy.Policy(
          mode: review_lane_preflight_policy.Off,
          cache_ttl_seconds: 86_400,
          park_on_failure: True,
          strict_live_model_checks: False,
        ),
        override: None,
      ),
      now_ms: 456,
    )
  let task_ref = task.from_legacy_issue(candidate).ref
  let pending =
    transition_types.PendingDispatchValidation(
      task_ref: task_ref,
      issue: candidate,
      remaining_candidates: [],
      generation: 1,
    )
  let state =
    transition_types.State(
      ..orchestrator_transition_test.fixture_state(),
      pending_dispatch_validations: dict.from_list([
        #(orchestrator_state.task_ref_identity(task_ref), pending),
      ]),
      next_dispatch_validation_generation: 2,
      next_session_sequence: 3,
    )

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.DispatchValidationCompleted(
        candidate.id,
        1,
        Ok(candidate),
        context,
      ),
      state,
    )

  let assert Ok(claim) =
    dict.get(next.pending_claims, orchestrator_state.issue_identity(candidate))
  assert claim.workflow_id == "review"
  assert list.any(effects, fn(effect) {
    case effect {
      effects_types.ReserveSessionSequence(3) -> True
      _ -> False
    }
  })
  assert list.any(effects, fn(effect) {
    case effect {
      effects_types.ClaimIssue(_, issue, _, _, _) -> issue.id == candidate.id
      _ -> False
    }
  })
}

fn state_with_pending_dispatch_validation(
  candidate: tracker_issue.Issue,
) -> transition_types.State {
  let task_ref = task.from_legacy_issue(candidate).ref
  transition_types.State(
    ..orchestrator_transition_test.fixture_state(),
    pending_dispatch_validations: dict.from_list([
      #(
        orchestrator_state.issue_identity(candidate),
        transition_types.PendingDispatchValidation(
          task_ref: task_ref,
          issue: candidate,
          remaining_candidates: [],
          generation: 1,
        ),
      ),
    ]),
    lifecycle: {
      let assert Ok(directory) =
        task_lifecycle.put(
          task_lifecycle.new(),
          task_lifecycle.Validating(
            task_ref: task_ref,
            issue: candidate,
            generation: 1,
          ),
        )
      directory
    },
    next_dispatch_validation_generation: 2,
  )
}

fn assert_lifecycle_projection_error_blocks_dispatch(
  state: transition_types.State,
  expected_error_code: String,
) {
  assert daemon_transition_shell.lifecycle_projection_failed(state)

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.DispatchCandidates(
        [dispatch_candidate_issue()],
        orchestrator_transition_test.fixture_context(),
      ),
      state,
    )

  assert dict.size(next.pending_dispatch_validations) == 0
  assert !has_begin_dispatch_validation(effects)
  assert !has_claim_issue(effects)
  assert has_lifecycle_projection_error_log(effects, expected_error_code)
}

fn state_with_conflicting_lifecycle_sources() -> transition_types.State {
  let issue = lifecycle_projection_error_issue()
  let base = orchestrator_transition_test.state_with_pending_claim(issue)
  let task_ref = task.from_legacy_issue(issue).ref
  let task_identity = orchestrator_state.task_ref_identity(task_ref)
  transition_types.State(
    ..base,
    runtime: orchestrator_state.RuntimeState(
      ..base.runtime,
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
    ),
  )
}

fn state_with_missing_claimed_lifecycle() -> transition_types.State {
  let issue = lifecycle_projection_error_issue()
  transition_types.State(
    ..orchestrator_transition_test.fixture_state(),
    runtime: orchestrator_state.RuntimeState(
      ..orchestrator_transition_test.fixture_runtime(),
      claimed: dict.from_list([
        #(orchestrator_state.issue_identity(issue), issue.identifier),
      ]),
    ),
  )
}

fn state_with_missing_retry_waiting_for_refresh() -> transition_types.State {
  let issue = lifecycle_projection_error_issue()
  transition_types.State(
    ..orchestrator_transition_test.fixture_state(),
    retry_refresh_generations: dict.from_list([
      #(orchestrator_state.issue_identity(issue), 7),
    ]),
  )
}

fn state_with_running_worker_mismatch() -> transition_types.State {
  let issue = lifecycle_projection_error_issue()
  let task_value = task.from_legacy_issue(issue)
  let task_identity = orchestrator_state.task_ref_identity(task_value.ref)
  transition_types.State(
    ..orchestrator_transition_test.fixture_state(),
    runtime: orchestrator_state.RuntimeState(
      ..orchestrator_transition_test.fixture_runtime(),
      running: dict.from_list([
        #(
          task_identity,
          orchestrator_state.RunningEntry(
            task: task_value,
            issue: issue,
            workspace_path: "test/tmp/workspaces/" <> issue.identifier,
            session: None,
          ),
        ),
      ]),
    ),
  )
}

fn state_with_retry_refresh_identity_mismatch(
  issue: tracker_issue.Issue,
) -> transition_types.State {
  let task_ref = task.from_legacy_issue(issue).ref
  let task_identity = orchestrator_state.task_ref_identity(task_ref)
  transition_types.State(
    ..orchestrator_transition_test.fixture_state(),
    runtime: orchestrator_state.RuntimeState(
      ..orchestrator_transition_test.fixture_runtime(),
      retry_attempts: dict.from_list([
        #(
          task_identity,
          orchestrator_state.RetryEntry(
            task_ref: task_ref,
            issue_id: issue.id,
            delay_ms: 1000,
            timer_generation: 7,
          ),
        ),
      ]),
    ),
    retry_refresh_generations: dict.from_list([
      #(orchestrator_state.linear_issue_id_identity(issue.id), 7),
    ]),
  )
}

fn lifecycle_projection_error_issue() -> tracker_issue.Issue {
  tracker_issue.Issue(
    ..orchestrator_transition_test.fixture_issue(),
    id: "projection-error-issue",
    identifier: "ABC-PROJECTION",
  )
}

fn dispatch_candidate_issue() -> tracker_issue.Issue {
  tracker_issue.Issue(
    ..orchestrator_transition_test.fixture_issue(),
    id: "dispatch-candidate-issue",
    identifier: "ABC-CANDIDATE",
  )
}

fn assert_retry_handoff_failure_restores_retry(
  result: transition_types.HandoffClaimResult,
) {
  let issue = orchestrator_transition_test.fixture_issue()
  let task_identity = orchestrator_state.issue_identity(issue)
  let transition_types.Outcome(state: claiming, ..) =
    invariant_helpers.handle_and_assert(
      transition_types.RetryRefreshCompleted(
        issue.id,
        1,
        Ok([issue]),
        orchestrator_transition_test.fixture_context(),
      ),
      state_with_retry(issue),
    )
  let assert Ok(pending) = dict.get(claiming.pending_claims, task_identity)

  let transition_types.Outcome(state: next, effects: effects) =
    invariant_helpers.handle_and_assert(
      transition_types.HandoffClaimCompleted(
        task_identity: task_identity,
        issue_id: identity.issue_id_from_string(issue.id),
        run_id: identity.run_id_from_string(pending.run_id),
        result: result,
      ),
      claiming,
    )

  assert dict.get(next.pending_claims, task_identity) == Error(Nil)
  let assert Ok(retry) = dict.get(next.runtime.retry_attempts, task_identity)
  assert retry.issue_id == issue.id
  assert retry.delay_ms == 1000
  assert retry.timer_generation == 1
  assert dict.get(next.runtime.claimed, task_identity) == Ok(issue.identifier)
  assert list.any(effects, fn(effect) {
    effect == effects_types.DeferRetryTimer(issue.id, 1, 1000)
  })
  assert !has_cancel_retry_reason(effects, "retry_dispatch")
}

fn assert_retry_pre_claim_failure_restores_retry(
  state: transition_types.State,
  effects: List(effects_types.Effect),
  issue: tracker_issue.Issue,
) {
  let task_identity = orchestrator_state.issue_identity(issue)
  assert dict.size(state.pending_claims) == 0
  let assert Ok(retry) = dict.get(state.runtime.retry_attempts, task_identity)
  assert retry.issue_id == issue.id
  assert retry.delay_ms == 1000
  assert retry.timer_generation == 1
  assert dict.get(state.runtime.claimed, task_identity) == Ok(issue.identifier)
  assert !has_claim_issue(effects)
  assert list.any(effects, fn(effect) {
    effect == effects_types.DeferRetryTimer(issue.id, 1, 1000)
  })
  assert !has_cancel_retry_reason(effects, "retry_dispatch")
}

fn state_with_retry(issue: tracker_issue.Issue) -> transition_types.State {
  let task_ref = task.from_legacy_issue(issue).ref
  let task_identity = orchestrator_state.task_ref_identity(task_ref)
  let runtime =
    orchestrator_state.mark_task_retrying(
      orchestrator_transition_test.fixture_runtime(),
      task_identity,
      orchestrator_state.RetryEntry(
        task_ref: task_ref,
        issue_id: issue.id,
        delay_ms: 1000,
        timer_generation: 1,
      ),
      issue.identifier,
    )
  transition_types.State(
    ..orchestrator_transition_test.fixture_state(),
    runtime: runtime,
    lifecycle: {
      let assert Ok(directory) =
        task_lifecycle.put(
          task_lifecycle.new(),
          task_lifecycle.RetryWaiting(
            task_ref: task.from_legacy_issue(issue).ref,
            issue_id: issue.id,
            generation: 1,
            delay_ms: 1000,
          ),
        )
      directory
    },
  )
}

fn retry_request(_issue_id: String) -> effects_types.OperatorCommandRequest {
  effects_types.OperatorCommandRequest(
    correlation_id: "test-correlation",
    source: effects_types.LocalOperatorCommand,
    operator_command: command.RetryIssue(command.IssueId("issue-1")),
    timeout_ms: 1000,
  )
}

fn context_with_failure_state(
  state_name: String,
) -> transition_types.DispatchContext {
  let context = orchestrator_transition_test.fixture_context()
  let effective =
    config_types.EffectiveConfig(
      ..context.effective,
      handoff: config_types.HandoffConfig(
        ..context.effective.handoff,
        completion_states: Some(
          workflow_completion_policy.CompletionStatePolicy(
            default_completion_state: Some(
              workflow_completion_policy.StateByName("In Review"),
            ),
            no_review_completion_state: Some(
              workflow_completion_policy.StateByName("Done"),
            ),
            failure_state: Some(workflow_completion_policy.StateByName(
              state_name,
            )),
            partial_success_state: Some(workflow_completion_policy.StateByName(
              state_name,
            )),
            cancellation_state: None,
            workflows: dict.new(),
          ),
        ),
      ),
    )
  transition_types.DispatchContext(..context, effective: effective)
}

fn context_with_preflight(
  policy: review_lane_preflight_policy.Policy,
  result: review_lane_preflight.PreflightResult,
) -> transition_types.DispatchContext {
  let context = context_requiring_preflight(policy)
  transition_types.DispatchContext(
    ..context,
    review_lane_preflight: transition_types.ReviewLanePreflightContext(
      ..context.review_lane_preflight,
      override: Some(result),
    ),
  )
}

fn context_requiring_preflight(
  policy: review_lane_preflight_policy.Policy,
) -> transition_types.DispatchContext {
  transition_types.DispatchContext(
    ..orchestrator_transition_test.fixture_context(),
    routing: config_types.RoutingConfig(
      workflow_label_prefix: "workflow:",
      require_exactly_one_workflow_label: True,
      default_workflow: None,
      workflows: dict.from_list([
        #("implementation", "workflows/implementation.yaml"),
      ]),
    ),
    available_workflow_ids: ["implementation"],
    review_lane_preflight: transition_types.ReviewLanePreflightContext(
      config_dir: ".scherzo",
      workflow_dags: dict.from_list([
        #(
          "implementation",
          orchestrator_transition_test.fixture_workflow_dag("implementation"),
        ),
      ]),
      policy: policy,
      override: None,
    ),
  )
}

fn has_claim_issue(effects: List(effects_types.Effect)) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.ClaimIssue(_, _, _, _, _) -> True
      _ -> False
    }
  })
}

fn has_log(
  effects: List(effects_types.Effect),
  expected_event: String,
) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.Log(_, event, _) -> event == expected_event
      _ -> False
    }
  })
}

fn has_begin_dispatch_validation(effects: List(effects_types.Effect)) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.BeginDispatchValidation(_, _) -> True
      _ -> False
    }
  })
}

fn has_lifecycle_projection_error_log(
  effects: List(effects_types.Effect),
  expected_error_code: String,
) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.Log("error", "task_lifecycle_projection_failed", fields) ->
        field_equals(fields, "error_code", expected_error_code)
        && field_equals(fields, "fail_closed", "true")
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

fn has_start_worker(effects: List(effects_types.Effect)) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.StartWorker(_) -> True
      _ -> False
    }
  })
}

fn worker_start_requests(
  effects: List(effects_types.Effect),
) -> List(effects_types.WorkerStart) {
  list.filter_map(effects, fn(effect) {
    case effect {
      effects_types.StartWorker(request) -> Ok(request)
      _ -> Error(Nil)
    }
  })
}

fn recovery_info(run_id: String) -> session_event.RecoveryInfo {
  session_event.RecoveryInfo(
    status: session_event.Interrupted,
    source: "projection.run_interrupted",
    message: Some("daemon_restart"),
    safe_actions: [session_event.Retry],
    workflow_run_id: Some(run_id),
    workflow_step_id: None,
    workflow_attempt_index: None,
    parent_session_id: None,
    orphan_status: None,
    issue_state: None,
    recommended_action: Some("retry"),
    current_pi_session_id: None,
    previous_pi_session_id: Some("previous-" <> run_id),
    park_reason: None,
    park_release_policy: None,
    parked_at_ms: None,
    drift_kind: None,
    retention_until_ms: None,
    cleanup_eligible_at_ms: None,
    cleanup_phase: None,
  )
}

fn has_park_issue(effects: List(effects_types.Effect)) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.ParkIssue(_, _) -> True
      _ -> False
    }
  })
}

fn has_issue_unparked_append(
  effects: List(effects_types.Effect),
  expected_issue_id: String,
  expected_reason: String,
) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.AppendLedger(effects_types.LedgerAppend(batch: batch, ..)) ->
        ledger_batch.to_bodies(batch)
        |> list.any(fn(body) {
          case body {
            record.IssueUnparked(issue_id, _, reason) ->
              issue_id == expected_issue_id && reason == expected_reason
            _ -> False
          }
        })
      _ -> False
    }
  })
}

fn has_operator_rejection_message(
  effects: List(effects_types.Effect),
  expected: String,
) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.FinishOperatorCommand(_, result) ->
        result.message == Some(expected)
      _ -> False
    }
  })
}

fn has_preflight_failure_log(effects: List(effects_types.Effect)) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.Log(_, "review_infrastructure_preflight_failed", _) -> True
      _ -> False
    }
  })
}

fn has_preflight_paused_log(effects: List(effects_types.Effect)) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.Log(
        "info",
        "review_lane_preflight_precondition_failed",
        fields,
      ) -> field_equals(fields, "reason", "operator_paused")
      _ -> False
    }
  })
}

fn has_preflight_stale_log(effects: List(effects_types.Effect)) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.Log(_, "review_lane_preflight_stale", _) -> True
      _ -> False
    }
  })
}

fn has_begin_review_lane_preflight(
  effects: List(effects_types.Effect),
) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.BeginReviewLanePreflight(_) -> True
      _ -> False
    }
  })
}

fn has_workflow_route_snapshot_failed_log(
  effects: List(effects_types.Effect),
  expected_error: String,
) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.Log("warn", "workflow_route_snapshot_failed", fields) ->
        field_equals(fields, "error", expected_error)
      _ -> False
    }
  })
}

fn has_cancel_retry_reason(
  effects: List(effects_types.Effect),
  expected: String,
) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.CancelRetryTimer(_, _, reason) -> reason == expected
      effects_types.AppendLedger(effects_types.LedgerAppend(policy: policy, ..)) ->
        case policy {
          effects_types.CancelRetryTimerAfterAppend(cancel_reason: reason, ..) ->
            reason == expected
          _ -> False
        }
      _ -> False
    }
  })
}

fn has_pending_retry_cancellation(
  state: transition_types.State,
  issue_id: String,
  expected: String,
) -> Bool {
  state.pending_claims
  |> dict.values
  |> list.any(fn(pending) {
    pending.issue_id == issue_id
    && case pending.retry_cancellation {
      Some(transition_types.RetryCancellation(reason: reason, ..)) ->
        reason == expected
      None -> False
    }
  })
}

fn has_finished_operator_applied(effects: List(effects_types.Effect)) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.FinishOperatorCommand(_, result) ->
        command.status_to_string(result.status) == "applied"
      _ -> False
    }
  })
}

fn has_finished_operator_rejected(
  effects: List(effects_types.Effect),
  expected: String,
) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.FinishOperatorCommand(_, result) ->
        case result.status {
          command.Rejected(reason) -> reason == expected
          _ -> False
        }
      _ -> False
    }
  })
}

fn enforcing_context() -> transition_types.DispatchContext {
  let effective = orchestrator_transition_test.fixture_effective()
  let linear_contract =
    config_types.LinearContractConfig(
      ..effective.linear_contract,
      enforce_issue_workflow_labels: True,
      workflow_labels: ["implementation"],
    )
  transition_types.DispatchContext(
    ..orchestrator_transition_test.fixture_context(),
    effective: config_types.EffectiveConfig(
      ..effective,
      linear_contract: linear_contract,
    ),
  )
}

fn labelled_issue(
  id: String,
  identifier: String,
  label: String,
) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: id,
    identifier: identifier,
    title: "Issue " <> identifier,
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked("Todo"),
    branch_name: None,
    url: None,
    labels: [label],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}
