import birl
import gleam/erlang/process
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config
import scherzo/domain
import scherzo/error
import scherzo/linear
import scherzo/linear_triage
import scherzo/workflow_policy

fn tracker_config() -> domain.TrackerConfig {
  domain.TrackerConfig(
    kind: "linear",
    endpoint: "https://api.linear.test/graphql",
    api_key: Some("lin_api_secret"),
    project_slug: Some("TEST"),
    active_states: ["Ready for Agent"],
    terminal_states: ["Done"],
  )
}

fn contract_config(
  comment: Bool,
  state_id: Option(String),
) -> domain.LinearContractConfig {
  domain.LinearContractConfig(
    ..config.default_linear_contract_config(),
    workflow_labels: ["bugfix", "feature"],
    enforce_issue_workflow_labels: True,
    comment_on_invalid_workflow: comment,
    invalid_workflow_state_id: state_id,
  )
}

fn issue() -> domain.Issue {
  domain.Issue(
    id: "issue-id",
    identifier: "ABC-1",
    title: "Needs workflow",
    description: Some("description must not appear in triage comment"),
    priority: Some(1),
    state: "Ready for Agent",
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(1)),
  )
}

fn success_transport(
  observed: process.Subject(String),
) -> fn(linear.Request) -> Result(linear.Response, error.TrackerError) {
  fn(request: linear.Request) {
    process.send(observed, request.body)
    case string.contains(request.body, "commentCreate") {
      True ->
        Ok(linear.Response(
          200,
          "{\"data\":{\"commentCreate\":{\"success\":true}}}",
        ))
      False ->
        Ok(linear.Response(
          200,
          "{\"data\":{\"issueUpdate\":{\"success\":true}}}",
        ))
    }
  }
}

fn state_fails_transport(
  observed: process.Subject(String),
) -> fn(linear.Request) -> Result(linear.Response, error.TrackerError) {
  fn(request: linear.Request) {
    process.send(observed, request.body)
    case string.contains(request.body, "commentCreate") {
      True ->
        Ok(linear.Response(
          200,
          "{\"data\":{\"commentCreate\":{\"success\":true}}}",
        ))
      False -> Error(error.LinearApiStatus(500))
    }
  }
}

pub fn noop_report_builds_no_mutation_test() {
  let observed = process.new_subject()
  let client =
    linear_triage.triage_client(
      tracker_config(),
      contract_config(False, None),
      success_transport(observed),
    )
  assert client.report_invalid_workflow(
      issue(),
      workflow_policy.MissingWorkflowLabel,
    )
    == Ok(linear_triage.InvalidWorkflowReportNoop)
  assert process.receive(observed, within: 50) == Error(Nil)
}

pub fn comment_report_builds_one_comment_test() {
  let observed = process.new_subject()
  let client =
    linear_triage.triage_client(
      tracker_config(),
      contract_config(True, None),
      success_transport(observed),
    )
  assert client.report_invalid_workflow(
      issue(),
      workflow_policy.UnknownWorkflowLabel("workflow:surprise"),
    )
    == Ok(linear_triage.InvalidWorkflowReportComment)
  let assert Ok(comment_body) = process.receive(observed, within: 1000)
  assert string.contains(comment_body, "commentCreate")
  assert string.contains(comment_body, "workflow:surprise")
  assert string.contains(comment_body, "workflow:bugfix")
  assert !string.contains(comment_body, "description must not appear")
  assert process.receive(observed, within: 50) == Error(Nil)
}

pub fn state_only_and_comment_then_state_reports_test() {
  let observed = process.new_subject()
  let state_only =
    linear_triage.triage_client(
      tracker_config(),
      contract_config(False, Some("state-needs-workflow")),
      success_transport(observed),
    )
  assert state_only.report_invalid_workflow(
      issue(),
      workflow_policy.MissingWorkflowLabel,
    )
    == Ok(linear_triage.InvalidWorkflowReportState)
  let assert Ok(state_body) = process.receive(observed, within: 1000)
  assert string.contains(state_body, "issueUpdate")
  assert string.contains(state_body, "state-needs-workflow")

  let both =
    linear_triage.triage_client(
      tracker_config(),
      contract_config(True, Some("state-needs-workflow")),
      success_transport(observed),
    )
  assert both.report_invalid_workflow(
      issue(),
      workflow_policy.MissingWorkflowLabel,
    )
    == Ok(linear_triage.InvalidWorkflowReportCommentAndState)
  let assert Ok(comment_body) = process.receive(observed, within: 1000)
  let assert Ok(update_body) = process.receive(observed, within: 1000)
  assert string.contains(comment_body, "commentCreate")
  assert string.contains(update_body, "issueUpdate")
}

pub fn state_failure_after_comment_returns_error_test() {
  let observed = process.new_subject()
  let client =
    linear_triage.triage_client(
      tracker_config(),
      contract_config(True, Some("state-needs-workflow")),
      state_fails_transport(observed),
    )
  let assert Error(error.LinearApiStatus(500)) =
    client.report_invalid_workflow(
      issue(),
      workflow_policy.MissingWorkflowLabel,
    )
  let assert Ok(comment_body) = process.receive(observed, within: 1000)
  let assert Ok(update_body) = process.receive(observed, within: 1000)
  assert string.contains(comment_body, "commentCreate")
  assert string.contains(update_body, "issueUpdate")
}
