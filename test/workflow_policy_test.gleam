import birl
import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import scherzo/config
import scherzo/domain
import scherzo/tracker/state as issue_state
import scherzo/workflow_policy

fn enforcing_config() -> domain.LinearContractConfig {
  domain.LinearContractConfig(
    ..config.default_linear_contract_config(),
    workflow_label_prefix: "workflow:",
    workflow_labels: ["bugfix", "research", "docs"],
    enforce_issue_workflow_labels: True,
  )
}

fn issue_with_labels(labels: List(String)) -> domain.Issue {
  domain.Issue(
    id: "issue-id",
    identifier: "ABC-1",
    title: "Title",
    description: None,
    priority: Some(1),
    state: issue_state.from_string_unchecked("Ready for Agent"),
    branch_name: None,
    url: None,
    labels: labels,
    blocked_by: [],
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(1)),
  )
}

pub fn selected_workflow_test() {
  let decision =
    workflow_policy.classify_issue(
      enforcing_config(),
      issue_with_labels(["workflow:bugfix", "backend"]),
    )
  let assert workflow_policy.WorkflowSelected(name, label) = decision
  assert name == "bugfix"
  assert label == "workflow:bugfix"
}

pub fn missing_multiple_and_unknown_labels_test() {
  let assert workflow_policy.WorkflowInvalid(missing) =
    workflow_policy.classify_issue(enforcing_config(), issue_with_labels([]))
  assert missing == workflow_policy.MissingWorkflowLabel
  assert workflow_policy.violation_code(missing) == "missing_workflow_label"

  let assert workflow_policy.WorkflowInvalid(multiple) =
    workflow_policy.classify_issue(
      enforcing_config(),
      issue_with_labels(["workflow:bugfix", "workflow:research"]),
    )
  assert multiple
    == workflow_policy.MultipleWorkflowLabels([
      "workflow:bugfix",
      "workflow:research",
    ])
  assert workflow_policy.violation_code(multiple) == "multiple_workflow_labels"
  assert string.contains(
    workflow_policy.violation_fingerprint(multiple),
    "workflow:research",
  )

  let assert workflow_policy.WorkflowInvalid(unknown) =
    workflow_policy.classify_issue(
      enforcing_config(),
      issue_with_labels(["workflow:surprise"]),
    )
  assert unknown == workflow_policy.UnknownWorkflowLabel("workflow:surprise")
  assert workflow_policy.violation_code(unknown) == "unknown_workflow_label"
  assert workflow_policy.violation_fingerprint(unknown)
    != workflow_policy.violation_fingerprint(
      workflow_policy.UnknownWorkflowLabel("workflow:other"),
    )
}

pub fn normalization_and_prefix_boundary_test() {
  let mixed =
    workflow_policy.classify_issue(
      enforcing_config(),
      issue_with_labels([" Workflow:Bugfix "]),
    )
  let assert workflow_policy.WorkflowSelected(name, label) = mixed
  assert name == "bugfix"
  assert label == "workflow:bugfix"

  let assert workflow_policy.WorkflowInvalid(boundary) =
    workflow_policy.classify_issue(
      enforcing_config(),
      issue_with_labels(["workflowish:bugfix"]),
    )
  assert boundary == workflow_policy.MissingWorkflowLabel
}

pub fn disabled_policy_and_label_fingerprints_test() {
  let disabled =
    domain.LinearContractConfig(
      ..enforcing_config(),
      enforce_issue_workflow_labels: False,
    )
  assert workflow_policy.classify_issue(disabled, issue_with_labels([]))
    == workflow_policy.WorkflowPolicyDisabled

  let none_updated =
    domain.Issue(..issue_with_labels(["B", "a"]), updated_at: None)
  assert workflow_policy.observed_labels_fingerprint(none_updated)
    == workflow_policy.observed_labels_fingerprint(
      issue_with_labels([" a ", "b"]),
    )
}

pub fn allowed_label_names_and_message_test() {
  assert workflow_policy.allowed_label_names(enforcing_config())
    == ["workflow:bugfix", "workflow:research", "workflow:docs"]
  let body =
    workflow_policy.violation_message(
      workflow_policy.UnknownWorkflowLabel("workflow:surprise"),
      enforcing_config(),
    )
  assert string.contains(body, "unknown workflow label")
  assert string.contains(body, "workflow:bugfix")
  assert string.contains(body, "workflow:surprise")
  assert string.contains(body, "configured ready state")

  let custom_ready =
    domain.LinearContractConfig(
      ..enforcing_config(),
      required_states: dict.from_list([#("ready", "Ready for Robots")]),
    )
  let custom_body =
    workflow_policy.violation_message(
      workflow_policy.MissingWorkflowLabel,
      custom_ready,
    )
  assert string.contains(custom_body, "Ready for Robots")
  assert !string.contains(custom_body, "Ready for Agent")
}

pub fn config_record_update_keeps_dict_import_live_test() {
  assert dict.size(enforcing_config().required_states) == 0
}
