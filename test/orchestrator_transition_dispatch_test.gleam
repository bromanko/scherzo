import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import orchestrator_transition_invariant_helpers as invariant_helpers
import orchestrator_transition_test
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/task_lifecycle
import scherzo/orchestrator/transition
import scherzo/orchestrator/transition_types
import scherzo/review_lane_preflight
import scherzo/review_lane_preflight_policy
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
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
  assert has_cancel_retry_reason(effects, "retry_dispatch")
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

  assert dict.has_key(
    next.pending_claims,
    orchestrator_state.issue_identity(issue),
  )
  assert !dict.has_key(
    next.runtime.retry_attempts,
    orchestrator_state.issue_identity(issue),
  )
  assert has_claim_issue(effects)
  assert has_cancel_retry_reason(effects, "retry_dispatch")
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
    orchestrator_state.RuntimeState(
      ..orchestrator_transition_test.fixture_runtime(),
      parked: dict.from_list([
        #(orchestrator_state.issue_identity(issue), parked),
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
        policy: review_lane_preflight_policy.default(),
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
      requested_at_ms: 123,
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
      effects_types.ClaimIssue(_, issue, _, _) -> issue.id == candidate.id
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
          requested_at_ms: 123,
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

fn state_with_retry(issue: tracker_issue.Issue) -> transition_types.State {
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

fn retry_request(issue_id: String) -> effects_types.OperatorCommandRequest {
  effects_types.OperatorCommandRequest(
    source: effects_types.LocalOperatorCommand,
    operator_command: command.RetryIssue(command.IssueId(issue_id)),
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
            default_completion_state: workflow_completion_policy.StateByName(
              "In Review",
            ),
            no_review_completion_state: Some(
              workflow_completion_policy.StateByName("Done"),
            ),
            failure_state: workflow_completion_policy.StateByName(state_name),
            partial_success_state: workflow_completion_policy.StateByName(
              state_name,
            ),
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
      override: Some(result),
    ),
  )
}

fn has_claim_issue(effects: List(effects_types.Effect)) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.ClaimIssue(_, _, _, _) -> True
      _ -> False
    }
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

fn has_park_issue(effects: List(effects_types.Effect)) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.ParkIssue(_, _) -> True
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

fn has_cancel_retry_reason(
  effects: List(effects_types.Effect),
  expected: String,
) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      effects_types.CancelRetryTimer(_, _, reason) -> reason == expected
      _ -> False
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
