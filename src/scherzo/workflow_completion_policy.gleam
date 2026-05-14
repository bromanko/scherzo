import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/result_artifact

pub type CompletionStatus {
  CompletionSucceeded
  CompletionPartiallySucceeded
  CompletionFailed
  CompletionCancelled
}

pub type ReviewRequirement {
  ReviewRequired
  ReviewNotRequired
  ReviewUnknown
}

pub type LinearStateRef {
  StateById(String)
  StateByName(String)
}

pub type CompletionArtifactSummary {
  CompletionArtifactSummary(
    kind: String,
    path: Option(String),
    reviewable: Bool,
  )
}

pub type WorkflowCompletionOutcome {
  WorkflowCompletionOutcome(
    status: CompletionStatus,
    artifacts: List(CompletionArtifactSummary),
    requires_review: ReviewRequirement,
    target_linear_state: Option(LinearStateRef),
    expected_artifacts_missing: Bool,
  )
}

pub type CompletionStateDecision {
  MoveToState(state: LinearStateRef, reason: String)
  LeaveLinearState(reason: String)
}

pub type CompletionStatePolicy {
  CompletionStatePolicy(
    default_completion_state: LinearStateRef,
    no_review_completion_state: Option(LinearStateRef),
    failure_state: LinearStateRef,
    partial_success_state: LinearStateRef,
    cancellation_state: Option(LinearStateRef),
    workflows: Dict(String, WorkflowCompletionOverride),
  )
}

pub type WorkflowCompletionOverride {
  WorkflowCompletionOverride(
    produces_reviewable_artifacts: Option(Bool),
    requires_review: Option(Bool),
    success_state: Option(LinearStateRef),
    no_review_completion_state: Option(LinearStateRef),
    failure_state: Option(LinearStateRef),
    partial_success_state: Option(LinearStateRef),
    cancellation_state: Option(LinearStateRef),
  )
}

pub fn default_override() -> WorkflowCompletionOverride {
  WorkflowCompletionOverride(
    produces_reviewable_artifacts: None,
    requires_review: None,
    success_state: None,
    no_review_completion_state: None,
    failure_state: None,
    partial_success_state: None,
    cancellation_state: None,
  )
}

pub fn choose_linear_completion_state(
  policy: CompletionStatePolicy,
  workflow_id: String,
  outcome: WorkflowCompletionOutcome,
) -> CompletionStateDecision {
  let override = workflow_override(policy, workflow_id)
  case outcome.target_linear_state {
    Some(state) -> MoveToState(state, "explicit workflow target state")
    None -> choose_without_explicit_target(policy, override, outcome)
  }
}

pub fn success_outcome(
  policy: Option(CompletionStatePolicy),
  workflow_id: String,
  success: agent_types.WorkerSuccess,
) -> WorkflowCompletionOutcome {
  let artifacts = success_artifacts(success)
  let override = case policy {
    Some(policy) -> workflow_override(policy, workflow_id)
    None -> default_override()
  }
  WorkflowCompletionOutcome(
    status: CompletionSucceeded,
    artifacts: artifacts,
    requires_review: review_requirement_from_override(override),
    target_linear_state: None,
    expected_artifacts_missing: expected_artifacts_missing(override, artifacts),
  )
}

pub fn failure_outcome() -> WorkflowCompletionOutcome {
  WorkflowCompletionOutcome(
    status: CompletionFailed,
    artifacts: [],
    requires_review: ReviewUnknown,
    target_linear_state: None,
    expected_artifacts_missing: False,
  )
}

pub fn cancellation_outcome() -> WorkflowCompletionOutcome {
  WorkflowCompletionOutcome(
    status: CompletionCancelled,
    artifacts: [],
    requires_review: ReviewUnknown,
    target_linear_state: None,
    expected_artifacts_missing: False,
  )
}

pub fn state_ref_label(ref: LinearStateRef) -> String {
  case ref {
    StateById(id) -> id
    StateByName(name) -> name
  }
}

pub fn decision_reason(decision: CompletionStateDecision) -> String {
  case decision {
    MoveToState(state, reason) ->
      "moved to " <> state_ref_label(state) <> " because " <> reason
    LeaveLinearState(reason) -> "left unchanged because " <> reason
  }
}

fn choose_without_explicit_target(
  policy: CompletionStatePolicy,
  override: WorkflowCompletionOverride,
  outcome: WorkflowCompletionOutcome,
) -> CompletionStateDecision {
  case outcome.status {
    CompletionCancelled ->
      case
        first_state([override.cancellation_state, policy.cancellation_state])
      {
        Some(state) -> MoveToState(state, "workflow was cancelled")
        None -> LeaveLinearState("workflow was cancelled")
      }
    CompletionFailed ->
      MoveToState(
        option_or(override.failure_state, policy.failure_state),
        "workflow failed",
      )
    CompletionPartiallySucceeded ->
      MoveToState(
        option_or(override.partial_success_state, policy.partial_success_state),
        "workflow partially succeeded",
      )
    CompletionSucceeded -> choose_success(policy, override, outcome)
  }
}

fn choose_success(
  policy: CompletionStatePolicy,
  override: WorkflowCompletionOverride,
  outcome: WorkflowCompletionOutcome,
) -> CompletionStateDecision {
  case outcome.expected_artifacts_missing {
    True ->
      MoveToState(
        option_or(override.partial_success_state, policy.partial_success_state),
        "expected reviewable artifacts were missing",
      )
    False ->
      case override.success_state {
        Some(state) -> MoveToState(state, "workflow-specific success state")
        None -> choose_success_without_override(policy, override, outcome)
      }
  }
}

fn choose_success_without_override(
  policy: CompletionStatePolicy,
  override: WorkflowCompletionOverride,
  outcome: WorkflowCompletionOutcome,
) -> CompletionStateDecision {
  case review_required(override, outcome) {
    True ->
      MoveToState(
        policy.default_completion_state,
        "reviewable artifacts were produced",
      )
    False ->
      case
        first_state([
          override.no_review_completion_state,
          policy.no_review_completion_state,
        ])
      {
        Some(state) -> MoveToState(state, "no review is required")
        None ->
          LeaveLinearState(
            "no review is required and no no-review completion state is configured",
          )
      }
  }
}

fn review_required(
  override: WorkflowCompletionOverride,
  outcome: WorkflowCompletionOutcome,
) -> Bool {
  case outcome.requires_review {
    ReviewRequired -> True
    ReviewNotRequired -> False
    ReviewUnknown ->
      case override.requires_review {
        Some(value) -> value
        None ->
          reviewable_artifacts_present(outcome.artifacts)
          || bool_option_true(override.produces_reviewable_artifacts)
      }
  }
}

fn review_requirement_from_override(
  override: WorkflowCompletionOverride,
) -> ReviewRequirement {
  case override.requires_review {
    Some(True) -> ReviewRequired
    Some(False) -> ReviewNotRequired
    None -> ReviewUnknown
  }
}

fn expected_artifacts_missing(
  override: WorkflowCompletionOverride,
  artifacts: List(CompletionArtifactSummary),
) -> Bool {
  bool_option_true(override.produces_reviewable_artifacts)
  && !reviewable_artifacts_present(artifacts)
}

fn success_artifacts(
  success: agent_types.WorkerSuccess,
) -> List(CompletionArtifactSummary) {
  case result_text_present(success.result) {
    True -> [
      CompletionArtifactSummary(
        kind: success.result.source,
        path: None,
        reviewable: True,
      ),
    ]
    False -> []
  }
}

fn result_text_present(artifact: result_artifact.ResultArtifact) -> Bool {
  case
    result_artifact.structured_final_response(artifact),
    artifact.final_response
  {
    Some(text), _ -> string.trim(text) != ""
    None, Some(text) -> string.trim(text) != ""
    None, None -> False
  }
}

fn workflow_override(
  policy: CompletionStatePolicy,
  workflow_id: String,
) -> WorkflowCompletionOverride {
  case
    dict.get(policy.workflows, string.trim(workflow_id) |> string.lowercase)
  {
    Ok(override) -> override
    Error(Nil) -> default_override()
  }
}

fn reviewable_artifacts_present(
  artifacts: List(CompletionArtifactSummary),
) -> Bool {
  list.any(artifacts, fn(artifact) { artifact.reviewable })
}

fn bool_option_true(value: Option(Bool)) -> Bool {
  case value {
    Some(True) -> True
    _ -> False
  }
}

fn first_state(values: List(Option(LinearStateRef))) -> Option(LinearStateRef) {
  case values {
    [] -> None
    [Some(value), ..] -> Some(value)
    [None, ..rest] -> first_state(rest)
  }
}

fn option_or(value: Option(a), fallback: a) -> a {
  case value {
    Some(value) -> value
    None -> fallback
  }
}
