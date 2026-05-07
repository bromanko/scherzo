import birl
import gleam/erlang/process
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config
import scherzo/config/types as config_types
import scherzo/error
import scherzo/linear
import scherzo/linear_triage
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import scherzo/workflow_policy
import test_async

fn tracker_config() -> config_types.TrackerConfig {
  config_types.TrackerConfig(
    kind: tracker_kind.LinearTracker,
    endpoint: "https://api.linear.test/graphql",
    api_key: Some("lin_api_secret"),
    project_slug: Some("TEST"),
    active_states: issue_state.list_from_strings(["Ready for Agent"]),
    terminal_states: issue_state.list_from_strings(["Done"]),
  )
}

fn contract_config(
  comment: Bool,
  state_id: Option(String),
) -> config_types.LinearContractConfig {
  config_types.LinearContractConfig(
    ..config.default_linear_contract_config(),
    workflow_labels: ["bugfix", "feature"],
    enforce_issue_workflow_labels: True,
    comment_on_invalid_workflow: comment,
    invalid_workflow_state_id: state_id,
  )
}

fn issue() -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "issue-id",
    identifier: "ABC-1",
    title: "Needs workflow",
    description: Some("description must not appear in triage comment"),
    priority: Some(1),
    state: issue_state.from_string_unchecked("Ready for Agent"),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
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
  test_async.assert_no_extra_message_within(observed, 50)
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
  test_async.assert_no_extra_message_within(observed, 50)
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
