import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import orchestrator_transition_test
import scherzo/config/types as config_types
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/state as orchestrator_state
import scherzo/orchestrator/transition
import scherzo/orchestrator/transition_types
import scherzo/review_lane_preflight
import scherzo/review_lane_preflight_policy
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

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

  assert dict.has_key(next.runtime.invalid_workflow_reports, candidate.id)
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
          running.id,
          orchestrator_state.RunningEntry(
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
    transition.handle(
      transition_types.DispatchValidationCompleted(
        candidate.id,
        1,
        Ok(candidate),
        context,
      ),
      state,
    )

  assert dict.size(next.pending_claims) == 0
  assert dict.has_key(next.runtime.parked, candidate.id)
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
    transition.handle(
      transition_types.DispatchValidationCompleted(
        candidate.id,
        1,
        Ok(candidate),
        context,
      ),
      state,
    )

  assert dict.size(next.pending_claims) == 0
  assert !dict.has_key(next.runtime.parked, candidate.id)
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
    transition.handle(
      transition_types.DispatchValidationCompleted(
        candidate.id,
        1,
        Ok(candidate),
        context,
      ),
      state,
    )

  assert dict.has_key(next.pending_claims, candidate.id)
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
    transition.handle(
      transition_types.DispatchValidationCompleted(
        candidate.id,
        1,
        Ok(candidate),
        context,
      ),
      state,
    )

  assert dict.has_key(next.pending_claims, candidate.id)
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
    transition.handle(
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
    transition.handle(
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
  let pending =
    transition_types.PendingDispatchValidation(
      issue: candidate,
      remaining_candidates: [],
      generation: 1,
      requested_at_ms: 123,
    )
  let state =
    transition_types.State(
      ..orchestrator_transition_test.fixture_state(),
      pending_dispatch_validations: dict.from_list([#(candidate.id, pending)]),
      next_session_sequence: 3,
    )

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.DispatchValidationCompleted(
        candidate.id,
        1,
        Ok(candidate),
        context,
      ),
      state,
    )

  let assert Ok(claim) = dict.get(next.pending_claims, candidate.id)
  assert claim.workflow_id == "review"
  assert list.any(effects, fn(effect) {
    case effect {
      effects_types.ReserveSessionSequence(3) -> True
      _ -> False
    }
  })
  assert list.any(effects, fn(effect) {
    case effect {
      effects_types.ClaimIssue(issue, _, _) -> issue.id == candidate.id
      _ -> False
    }
  })
}

fn state_with_pending_dispatch_validation(
  candidate: tracker_issue.Issue,
) -> transition_types.State {
  transition_types.State(
    ..orchestrator_transition_test.fixture_state(),
    pending_dispatch_validations: dict.from_list([
      #(
        candidate.id,
        transition_types.PendingDispatchValidation(
          issue: candidate,
          remaining_candidates: [],
          generation: 1,
          requested_at_ms: 123,
        ),
      ),
    ]),
  )
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
      effects_types.ClaimIssue(_, _, _) -> True
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
