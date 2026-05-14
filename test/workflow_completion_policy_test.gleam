import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import scherzo/workflow_completion_policy as policy

fn base_policy() -> policy.CompletionStatePolicy {
  policy.CompletionStatePolicy(
    default_completion_state: policy.StateByName("In Review"),
    no_review_completion_state: Some(policy.StateByName("Done")),
    failure_state: policy.StateByName("Needs Attention"),
    partial_success_state: policy.StateByName("Needs Attention"),
    cancellation_state: None,
    workflows: dict.new(),
  )
}

fn success_outcome(
  artifacts: List(policy.CompletionArtifactSummary),
  requires_review: policy.ReviewRequirement,
) -> policy.WorkflowCompletionOutcome {
  policy.WorkflowCompletionOutcome(
    status: policy.CompletionSucceeded,
    artifacts: artifacts,
    requires_review: requires_review,
    target_linear_state: None,
    expected_artifacts_missing: False,
  )
}

fn artifact() -> policy.CompletionArtifactSummary {
  policy.CompletionArtifactSummary(
    kind: "result",
    path: Some("docs/plans/example.md"),
    reviewable: True,
  )
}

pub fn artifact_success_chooses_review_state_test() {
  let decision =
    policy.choose_linear_completion_state(
      base_policy(),
      "execplan",
      success_outcome([artifact()], policy.ReviewUnknown),
    )

  assert decision
    == policy.MoveToState(
      policy.StateByName("In Review"),
      "reviewable artifacts were produced",
    )
}

pub fn no_review_success_chooses_no_review_state_test() {
  let decision =
    policy.choose_linear_completion_state(
      base_policy(),
      "maintenance",
      success_outcome([], policy.ReviewNotRequired),
    )

  assert decision
    == policy.MoveToState(policy.StateByName("Done"), "no review is required")
}

pub fn no_review_success_without_optional_state_leaves_state_test() {
  let decision =
    policy.choose_linear_completion_state(
      policy.CompletionStatePolicy(
        ..base_policy(),
        no_review_completion_state: None,
      ),
      "maintenance",
      success_outcome([], policy.ReviewNotRequired),
    )

  let assert policy.LeaveLinearState(reason) = decision
  assert string.contains(reason, "no-review completion state")
}

pub fn missing_expected_artifacts_choose_attention_test() {
  let decision =
    policy.choose_linear_completion_state(
      base_policy(),
      "execplan",
      policy.WorkflowCompletionOutcome(
        status: policy.CompletionSucceeded,
        artifacts: [],
        requires_review: policy.ReviewUnknown,
        target_linear_state: None,
        expected_artifacts_missing: True,
      ),
    )

  assert decision
    == policy.MoveToState(
      policy.StateByName("Needs Attention"),
      "expected reviewable artifacts were missing",
    )
}

pub fn failure_chooses_attention_test() {
  let decision =
    policy.choose_linear_completion_state(
      base_policy(),
      "execplan",
      policy.failure_outcome(),
    )

  assert decision
    == policy.MoveToState(
      policy.StateByName("Needs Attention"),
      "workflow failed",
    )
}

pub fn cancellation_leaves_state_by_default_test() {
  let decision =
    policy.choose_linear_completion_state(
      base_policy(),
      "execplan",
      policy.cancellation_outcome(),
    )

  assert decision == policy.LeaveLinearState("workflow was cancelled")
}

pub fn explicit_target_state_wins_test() {
  let decision =
    policy.choose_linear_completion_state(
      base_policy(),
      "execplan",
      policy.WorkflowCompletionOutcome(
        status: policy.CompletionSucceeded,
        artifacts: [artifact()],
        requires_review: policy.ReviewRequired,
        target_linear_state: Some(policy.StateById("state-explicit")),
        expected_artifacts_missing: False,
      ),
    )

  assert decision
    == policy.MoveToState(
      policy.StateById("state-explicit"),
      "explicit workflow target state",
    )
}

pub fn workflow_override_can_require_review_for_empty_success_test() {
  let configured =
    policy.CompletionStatePolicy(
      ..base_policy(),
      workflows: dict.from_list([
        #(
          "execplan",
          policy.WorkflowCompletionOverride(
            ..policy.default_override(),
            produces_reviewable_artifacts: Some(True),
          ),
        ),
      ]),
    )
  let decision =
    policy.choose_linear_completion_state(
      configured,
      "execplan",
      success_outcome([], policy.ReviewUnknown),
    )

  assert decision
    == policy.MoveToState(
      policy.StateByName("In Review"),
      "reviewable artifacts were produced",
    )
}
