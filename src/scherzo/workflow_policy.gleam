import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/domain

pub type IssueWorkflowDecision {
  WorkflowPolicyDisabled
  WorkflowSelected(name: String, label: String)
  WorkflowInvalid(violation: IssueWorkflowViolation)
}

pub type IssueWorkflowViolation {
  MissingWorkflowLabel
  MultipleWorkflowLabels(labels: List(String))
  UnknownWorkflowLabel(label: String)
}

pub fn classify_issue(
  config: domain.LinearContractConfig,
  issue: domain.Issue,
) -> IssueWorkflowDecision {
  case config.enforce_issue_workflow_labels {
    False -> WorkflowPolicyDisabled
    True -> {
      let prefix = normalize(config.workflow_label_prefix)
      let allowed = normalized_allowed_workflows(config)
      let workflow_labels = workflow_like_labels(issue.labels, prefix)
      case workflow_labels {
        [] -> WorkflowInvalid(MissingWorkflowLabel)
        [label] -> {
          let suffix = string.drop_start(label, string.length(prefix))
          case list.contains(allowed, suffix) {
            True -> WorkflowSelected(name: suffix, label: label)
            False -> WorkflowInvalid(UnknownWorkflowLabel(label))
          }
        }
        labels -> WorkflowInvalid(MultipleWorkflowLabels(labels))
      }
    }
  }
}

pub fn workflow_satisfied(decision: IssueWorkflowDecision) -> Bool {
  case decision {
    WorkflowPolicyDisabled -> True
    WorkflowSelected(_, _) -> True
    WorkflowInvalid(_) -> False
  }
}

pub fn allowed_label_names(config: domain.LinearContractConfig) -> List(String) {
  let prefix = normalize(config.workflow_label_prefix)
  normalized_allowed_workflows(config)
  |> list.map(fn(suffix) { prefix <> suffix })
}

pub fn violation_code(violation: IssueWorkflowViolation) -> String {
  case violation {
    MissingWorkflowLabel -> "missing_workflow_label"
    MultipleWorkflowLabels(_) -> "multiple_workflow_labels"
    UnknownWorkflowLabel(_) -> "unknown_workflow_label"
  }
}

pub fn violation_fingerprint(violation: IssueWorkflowViolation) -> String {
  case violation {
    MissingWorkflowLabel -> violation_code(violation)
    MultipleWorkflowLabels(labels) ->
      violation_code(violation)
      <> ":"
      <> fingerprint_strings(normalize_and_sort(labels))
    UnknownWorkflowLabel(label) ->
      violation_code(violation) <> ":" <> encode_string(normalize(label))
  }
}

pub fn observed_labels_fingerprint(issue: domain.Issue) -> String {
  issue.labels
  |> normalize_and_sort
  |> fingerprint_strings
}

pub fn reporting_policy_fingerprint(
  config: domain.LinearContractConfig,
) -> String {
  [
    "enabled:" <> bool_fingerprint(config.enabled),
    "enforce:" <> bool_fingerprint(config.enforce_issue_workflow_labels),
    "comment:" <> bool_fingerprint(config.comment_on_invalid_workflow),
    "invalid_state:" <> encode_optional_string(config.invalid_workflow_state_id),
    "prefix:" <> encode_string(normalize(config.workflow_label_prefix)),
    "workflows:" <> fingerprint_strings(normalized_allowed_workflows(config)),
    "ready:" <> encode_optional_string(ready_state_name(config)),
  ]
  |> string.join(with: "|")
}

pub fn violation_message(
  violation: IssueWorkflowViolation,
  config: domain.LinearContractConfig,
) -> String {
  let allowed = allowed_label_names(config)
  let expected = expected_labels_block(allowed)
  let ready_guidance = ready_state_guidance(config)
  case violation {
    MissingWorkflowLabel ->
      "Scherzo did not dispatch this issue because it has no workflow label.\n\n"
      <> expected
      <> "\n\nAdd exactly one workflow label, then "
      <> ready_guidance
    MultipleWorkflowLabels(labels) ->
      "Scherzo did not dispatch this issue because it has multiple workflow labels.\n\n"
      <> "Found:\n"
      <> bullet_block(normalize_and_sort(labels))
      <> "\n\n"
      <> expected
      <> "\n\nKeep exactly one workflow label, then "
      <> ready_guidance
    UnknownWorkflowLabel(label) ->
      "Scherzo did not dispatch this issue because it has an unknown workflow label.\n\n"
      <> "Found: "
      <> normalize(label)
      <> "\n\n"
      <> expected
      <> "\n\nReplace it with exactly one allowed workflow label, then "
      <> ready_guidance
  }
}

fn ready_state_guidance(config: domain.LinearContractConfig) -> String {
  case ready_state_name(config) {
    Some(state) -> "move the issue back to " <> state <> "."
    None -> "move the issue back to the configured ready state."
  }
}

fn ready_state_name(config: domain.LinearContractConfig) -> Option(String) {
  case dict.get(config.required_states, "ready") {
    Error(_) -> None
    Ok(state) -> {
      let state = string.trim(state)
      case state == "" {
        True -> None
        False -> Some(state)
      }
    }
  }
}

fn expected_labels_block(allowed: List(String)) -> String {
  "Expected exactly one of:\n" <> bullet_block(allowed)
}

fn bullet_block(values: List(String)) -> String {
  values
  |> list.map(fn(value) { "- " <> value })
  |> string.join(with: "\n")
}

fn workflow_like_labels(labels: List(String), prefix: String) -> List(String) {
  case prefix == "" {
    True -> []
    False ->
      labels
      |> list.map(normalize)
      |> list.filter(fn(label) { string.starts_with(label, prefix) })
  }
}

fn normalized_allowed_workflows(
  config: domain.LinearContractConfig,
) -> List(String) {
  config.workflow_labels
  |> list.map(normalize)
  |> list.filter(fn(label) { label != "" })
}

fn normalize_and_sort(values: List(String)) -> List(String) {
  values
  |> list.map(normalize)
  |> list.sort(by: string.compare)
}

fn normalize(value: String) -> String {
  value |> string.trim |> string.lowercase
}

fn fingerprint_strings(values: List(String)) -> String {
  values
  |> list.map(encode_string)
  |> string.join(with: "|")
}

fn encode_optional_string(value: Option(String)) -> String {
  case value {
    None -> "none"
    Some(value) -> "some:" <> encode_string(string.trim(value))
  }
}

fn encode_string(value: String) -> String {
  int.to_string(string.length(value)) <> ":" <> value
}

fn bool_fingerprint(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}
