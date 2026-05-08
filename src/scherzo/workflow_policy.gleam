import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/linear_comment_format as comment_format
import scherzo/tracker/issue as tracker_issue

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
  config: config_types.LinearContractConfig,
  issue: tracker_issue.Issue,
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

pub fn allowed_label_names(
  config: config_types.LinearContractConfig,
) -> List(String) {
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

pub fn observed_labels_fingerprint(issue: tracker_issue.Issue) -> String {
  issue.labels
  |> normalize_and_sort
  |> fingerprint_strings
}

pub fn reporting_policy_fingerprint(
  config: config_types.LinearContractConfig,
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
  config: config_types.LinearContractConfig,
) -> String {
  violation_comment("this issue", violation, config)
}

pub fn violation_comment(
  issue_identifier: String,
  violation: IssueWorkflowViolation,
  config: config_types.LinearContractConfig,
) -> String {
  let allowed = allowed_label_names(config)
  let body = case violation {
    MissingWorkflowLabel ->
      invalid_workflow_body(
        title: "🏷️ Scherzo needs one workflow label",
        issue_identifier: issue_identifier,
        problem: "missing_workflow_label",
        sections: [
          comment_format.section(
            "Summary",
            "Scherzo did not start this issue because it has no workflow label.",
          ),
          comment_format.section(
            "Next action",
            "Add exactly one allowed workflow label, then move the issue back to "
              <> ready_state_target(config)
              <> ".",
          ),
          allowed_labels_section(allowed),
        ],
      )
    MultipleWorkflowLabels(labels) ->
      invalid_workflow_body(
        title: "🏷️ Scherzo needs one workflow label",
        issue_identifier: issue_identifier,
        problem: "multiple_workflow_labels",
        sections: [
          comment_format.section(
            "Summary",
            "Scherzo found more than one workflow label and cannot choose a workflow safely.",
          ),
          labels_section("Found labels", normalize_and_sort(labels)),
          comment_format.section(
            "Next action",
            "Keep exactly one allowed workflow label, then move the issue back to "
              <> ready_state_target(config)
              <> ".",
          ),
          allowed_labels_section(allowed),
        ],
      )
    UnknownWorkflowLabel(label) ->
      invalid_workflow_body(
        title: "🏷️ Scherzo needs an allowed workflow label",
        issue_identifier: issue_identifier,
        problem: "unknown_workflow_label",
        sections: [
          comment_format.section(
            "Summary",
            "Scherzo found "
              <> comment_format.code_span(normalize(label), "workflow label")
              <> ", which is not configured as an allowed workflow label.",
          ),
          comment_format.section(
            "Next action",
            "Replace it with exactly one allowed workflow label, then move the issue back to "
              <> ready_state_target(config)
              <> ".",
          ),
          allowed_labels_section(allowed),
        ],
      )
  }
  comment_format.finalize_body("workflow_violation_comment", body, [])
}

fn invalid_workflow_body(
  title title_text: String,
  issue_identifier issue_identifier: String,
  problem problem: String,
  sections sections: List(String),
) -> String {
  [
    title_text,
    comment_format.summary_table([
      comment_format.SummaryRow(
        "Issue",
        comment_format.table_code(issue_identifier, "this issue"),
      ),
      comment_format.SummaryRow(
        "Status",
        comment_format.table_code("not dispatched", "not_dispatched"),
      ),
      comment_format.SummaryRow(
        "Problem",
        comment_format.table_code(problem, "workflow_label_problem"),
      ),
    ]),
    ..sections
  ]
  |> string.join(with: "\n\n")
}

fn allowed_labels_section(allowed: List(String)) -> String {
  labels_section("Allowed labels", allowed)
}

fn labels_section(title: String, labels: List(String)) -> String {
  let body =
    labels
    |> list.map(fn(label) {
      "- " <> comment_format.code_span(label, "workflow label")
    })
    |> string.join(with: "\n")
  comment_format.section(title, body)
}

fn ready_state_target(config: config_types.LinearContractConfig) -> String {
  case ready_state_name(config) {
    Some(state) -> comment_format.code_span(state, "ready state")
    None -> "the configured ready state"
  }
}

fn ready_state_name(
  config: config_types.LinearContractConfig,
) -> Option(String) {
  case dict.get(config.required_states, "ready") |> option.from_result {
    None -> None
    Some(state) -> {
      let state = string.trim(state)
      case state == "" {
        True -> None
        False -> Some(state)
      }
    }
  }
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
  config: config_types.LinearContractConfig,
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
