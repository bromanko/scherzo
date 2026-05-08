import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/linear
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_policy

pub type InvalidWorkflowReportOutcome {
  InvalidWorkflowReportNoop
  InvalidWorkflowReportComment
  InvalidWorkflowReportState
  InvalidWorkflowReportCommentAndState
}

pub type TriageClient {
  TriageClient(
    report_invalid_workflow: fn(
      tracker_issue.Issue,
      workflow_policy.IssueWorkflowViolation,
    ) -> Result(InvalidWorkflowReportOutcome, error.TrackerError),
  )
}

pub fn triage_client(
  tracker_config: config_types.TrackerConfig,
  contract_config: config_types.LinearContractConfig,
  transport: linear.Transport,
) -> TriageClient {
  TriageClient(report_invalid_workflow: fn(issue, violation) {
    report_invalid_workflow(
      tracker_config,
      contract_config,
      transport,
      issue,
      violation,
    )
  })
}

pub fn real_triage_client(
  tracker_config: config_types.TrackerConfig,
  contract_config: config_types.LinearContractConfig,
) -> TriageClient {
  triage_client(tracker_config, contract_config, linear.http_transport)
}

pub fn disabled_client() -> TriageClient {
  TriageClient(report_invalid_workflow: fn(_, _) {
    Ok(InvalidWorkflowReportNoop)
  })
}

fn report_invalid_workflow(
  tracker_config: config_types.TrackerConfig,
  contract_config: config_types.LinearContractConfig,
  transport: linear.Transport,
  issue: tracker_issue.Issue,
  violation: workflow_policy.IssueWorkflowViolation,
) -> Result(InvalidWorkflowReportOutcome, error.TrackerError) {
  let comment_enabled = contract_config.comment_on_invalid_workflow
  let state_id = normalized_state_id(contract_config.invalid_workflow_state_id)
  case comment_enabled, state_id {
    False, None -> Ok(InvalidWorkflowReportNoop)
    True, None -> {
      use _ <- try_tracker(post_comment(
        tracker_config,
        contract_config,
        transport,
        issue,
        violation,
      ))
      Ok(InvalidWorkflowReportComment)
    }
    False, Some(state_id) -> {
      use _ <- try_tracker(update_state(
        tracker_config,
        transport,
        issue.id,
        state_id,
      ))
      Ok(InvalidWorkflowReportState)
    }
    True, Some(state_id) -> {
      use _ <- try_tracker(post_comment(
        tracker_config,
        contract_config,
        transport,
        issue,
        violation,
      ))
      use _ <- try_tracker(update_state(
        tracker_config,
        transport,
        issue.id,
        state_id,
      ))
      Ok(InvalidWorkflowReportCommentAndState)
    }
  }
}

fn post_comment(
  tracker_config: config_types.TrackerConfig,
  contract_config: config_types.LinearContractConfig,
  transport: linear.Transport,
  issue: tracker_issue.Issue,
  violation: workflow_policy.IssueWorkflowViolation,
) -> Result(Nil, error.TrackerError) {
  let body =
    workflow_policy.violation_comment(
      issue.identifier,
      violation,
      contract_config,
    )
  use request <- try_tracker(linear.build_comment_create_request(
    tracker_config,
    issue.id,
    body,
  ))
  use response <- try_tracker(transport(request))
  linear.parse_mutation_response(response, "commentCreate")
}

fn update_state(
  tracker_config: config_types.TrackerConfig,
  transport: linear.Transport,
  issue_id: String,
  state_id: String,
) -> Result(Nil, error.TrackerError) {
  use request <- try_tracker(linear.build_issue_update_state_request(
    tracker_config,
    issue_id,
    state_id,
  ))
  use response <- try_tracker(transport(request))
  linear.parse_mutation_response(response, "issueUpdate")
}

fn normalized_state_id(value: Option(String)) -> Option(String) {
  case value {
    None -> None
    Some(value) -> {
      let value = string.trim(value)
      case value == "" {
        True -> None
        False -> Some(value)
      }
    }
  }
}

fn try_tracker(
  result: Result(a, error.TrackerError),
  next: fn(a) -> Result(b, error.TrackerError),
) -> Result(b, error.TrackerError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}
