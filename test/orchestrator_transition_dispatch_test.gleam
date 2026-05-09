import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import orchestrator_transition_test
import scherzo/config/types as config_types
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/state as orchestrator_state
import scherzo/orchestrator/transition
import scherzo/orchestrator/transition_types
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
