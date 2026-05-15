import gleam/bit_array
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/config/types as config_types
import scherzo/error
import scherzo/handoff_format
import scherzo/linear
import scherzo/linear_attachment
import scherzo/result_artifact
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_completion_policy

pub type ParkReport {
  ParkReport(
    issue_id: String,
    issue_identifier: String,
    reason: String,
    release_policy: Option(String),
    run_id: Option(String),
  )
}

pub type Client {
  Client(
    claim_issue: fn(tracker_issue.Issue, String) ->
      Result(Nil, error.TrackerError),
    report_success: fn(tracker_issue.Issue, agent_types.WorkerSuccess, String) ->
      Result(Nil, error.TrackerError),
    report_success_for_workflow: fn(
      tracker_issue.Issue,
      agent_types.WorkerSuccess,
      String,
      String,
    ) -> Result(Nil, error.TrackerError),
    report_failure: fn(tracker_issue.Issue, agent_types.WorkerFailure, String) ->
      Result(Nil, error.TrackerError),
    report_failure_for_workflow: fn(
      tracker_issue.Issue,
      agent_types.WorkerFailure,
      String,
      String,
    ) -> Result(Nil, error.TrackerError),
    report_park: fn(ParkReport) -> Result(Nil, error.TrackerError),
  )
}

pub fn disabled_client() -> Client {
  Client(
    claim_issue: fn(_, _) { Ok(Nil) },
    report_success: fn(_, _, _) { Ok(Nil) },
    report_success_for_workflow: fn(_, _, _, _) { Ok(Nil) },
    report_failure: fn(_, _, _) { Ok(Nil) },
    report_failure_for_workflow: fn(_, _, _, _) { Ok(Nil) },
    report_park: fn(_) { Ok(Nil) },
  )
}

pub fn linear_client(
  tracker_config: config_types.TrackerConfig,
  handoff_config: config_types.HandoffConfig,
  transport: linear.Transport,
) -> Client {
  linear_client_with_attachment_dependencies(
    tracker_config,
    handoff_config,
    linear_attachment.real_dependencies(transport),
  )
}

pub fn linear_client_with_attachment_dependencies(
  tracker_config: config_types.TrackerConfig,
  handoff_config: config_types.HandoffConfig,
  dependencies: linear_attachment.Dependencies,
) -> Client {
  case handoff_config.enabled {
    False -> disabled_client()
    True ->
      Client(
        claim_issue: fn(issue, run_id) {
          claim_issue(
            tracker_config,
            handoff_config,
            dependencies.graphql_transport,
            issue,
            run_id,
          )
        },
        report_success: fn(issue, success, run_id) {
          report_success(
            tracker_config,
            handoff_config,
            dependencies,
            issue,
            success,
            run_id,
            "",
          )
        },
        report_success_for_workflow: fn(issue, success, run_id, workflow_id) {
          report_success(
            tracker_config,
            handoff_config,
            dependencies,
            issue,
            success,
            run_id,
            workflow_id,
          )
        },
        report_failure: fn(issue, failure, run_id) {
          report_failure(
            tracker_config,
            handoff_config,
            dependencies.graphql_transport,
            issue,
            failure,
            run_id,
            "",
          )
        },
        report_failure_for_workflow: fn(issue, failure, run_id, workflow_id) {
          report_failure(
            tracker_config,
            handoff_config,
            dependencies.graphql_transport,
            issue,
            failure,
            run_id,
            workflow_id,
          )
        },
        report_park: fn(report) {
          report_park(
            tracker_config,
            handoff_config,
            dependencies.graphql_transport,
            report,
          )
        },
      )
  }
}

fn claim_issue(
  tracker_config: config_types.TrackerConfig,
  handoff_config: config_types.HandoffConfig,
  transport: linear.Transport,
  issue: tracker_issue.Issue,
  run_id: String,
) -> Result(Nil, error.TrackerError) {
  use _ <- try_tracker(run_comment(
    handoff_config.comment_on_claim,
    tracker_config,
    transport,
    issue.id,
    handoff_format.claim_comment(
      issue.identifier,
      run_id,
      tracker_secrets(tracker_config),
    ),
  ))
  run_state_update(
    tracker_config,
    transport,
    issue.id,
    handoff_config.claim_state_id,
  )
}

fn report_success(
  tracker_config: config_types.TrackerConfig,
  handoff_config: config_types.HandoffConfig,
  dependencies: linear_attachment.Dependencies,
  issue: tracker_issue.Issue,
  success: agent_types.WorkerSuccess,
  run_id: String,
  workflow_id: String,
) -> Result(Nil, error.TrackerError) {
  let decision =
    success_completion_decision(handoff_config, workflow_id, success)
  let tracking_state =
    option.map(decision, workflow_completion_policy.decision_reason)
  let attachment_filename =
    success_attachment_filename(handoff_config, issue, success, run_id)
  let options =
    handoff_format.SuccessCommentOptions(
      include_result: handoff_config.include_result_on_success,
      attachment_filename: attachment_filename,
    )
  let secrets = tracker_secrets(tracker_config)
  case handoff_config.attach_result_on_success {
    False -> {
      use _ <- try_tracker(run_comment(
        handoff_config.comment_on_success,
        tracker_config,
        dependencies.graphql_transport,
        issue.id,
        handoff_format.success_comment_with_tracking(
          issue,
          success,
          run_id,
          options,
          tracking_state,
          secrets,
        ),
      ))
      run_success_state_update(
        tracker_config,
        handoff_config,
        dependencies.graphql_transport,
        issue.id,
        decision,
      )
    }
    True -> {
      use comment <- try_tracker(create_success_comment(
        tracker_config,
        dependencies.graphql_transport,
        issue,
        success,
        run_id,
        options,
        tracking_state,
        secrets,
      ))
      use _ <- try_tracker(maybe_attach_success_result(
        tracker_config,
        handoff_config,
        dependencies,
        issue,
        success,
        run_id,
        comment.id,
        attachment_filename,
      ))
      run_success_state_update(
        tracker_config,
        handoff_config,
        dependencies.graphql_transport,
        issue.id,
        decision,
      )
    }
  }
}

fn report_failure(
  tracker_config: config_types.TrackerConfig,
  handoff_config: config_types.HandoffConfig,
  transport: linear.Transport,
  issue: tracker_issue.Issue,
  failure: agent_types.WorkerFailure,
  run_id: String,
  workflow_id: String,
) -> Result(Nil, error.TrackerError) {
  let decision =
    failure_completion_decision(handoff_config, workflow_id, failure)
  let tracking_state =
    option.map(decision, workflow_completion_policy.decision_reason)
  use _ <- try_tracker(run_comment(
    handoff_config.comment_on_failure,
    tracker_config,
    transport,
    issue.id,
    handoff_format.failure_comment_with_tracking(
      issue,
      failure,
      run_id,
      tracking_state,
      tracker_secrets(tracker_config),
    ),
  ))
  run_failure_state_update(
    tracker_config,
    handoff_config,
    transport,
    issue.id,
    decision,
  )
}

fn report_park(
  tracker_config: config_types.TrackerConfig,
  handoff_config: config_types.HandoffConfig,
  transport: linear.Transport,
  report: ParkReport,
) -> Result(Nil, error.TrackerError) {
  run_comment(
    handoff_config.comment_on_park,
    tracker_config,
    transport,
    report.issue_id,
    handoff_format.park_comment(
      report.issue_identifier,
      report.reason,
      report.release_policy,
      report.run_id,
      tracker_secrets(tracker_config),
    ),
  )
}

fn create_success_comment(
  tracker_config: config_types.TrackerConfig,
  transport: linear.Transport,
  issue: tracker_issue.Issue,
  success: agent_types.WorkerSuccess,
  run_id: String,
  options: handoff_format.SuccessCommentOptions,
  tracking_state: Option(String),
  secrets: List(String),
) -> Result(linear.LinearCommentDocument, error.TrackerError) {
  use request <- try_tracker(linear.build_comment_create_request(
    tracker_config,
    issue.id,
    handoff_format.success_comment_with_tracking(
      issue,
      success,
      run_id,
      options,
      tracking_state,
      secrets,
    ),
  ))
  use response <- try_tracker(transport(request))
  linear.parse_comment_create_response(response)
}

fn success_attachment_filename(
  handoff_config: config_types.HandoffConfig,
  issue: tracker_issue.Issue,
  success: agent_types.WorkerSuccess,
  run_id: String,
) -> Option(String) {
  case handoff_config.attach_result_on_success, success.result.final_response {
    True, Some(_) ->
      Some(handoff_format.success_result_filename(issue.identifier, run_id))
    _, _ -> None
  }
}

fn maybe_attach_success_result(
  tracker_config: config_types.TrackerConfig,
  handoff_config: config_types.HandoffConfig,
  dependencies: linear_attachment.Dependencies,
  issue: tracker_issue.Issue,
  success: agent_types.WorkerSuccess,
  run_id: String,
  comment_id: String,
  attachment_filename: Option(String),
) -> Result(Nil, error.TrackerError) {
  let attachment_success =
    limit_success_result_for_attachment(
      success,
      handoff_config.result_max_chars,
    )
  case
    attachment_filename,
    handoff_format.success_result_attachment_markdown(
      issue,
      attachment_success,
      run_id,
      tracker_secrets(tracker_config),
    )
  {
    _, None -> Ok(Nil)
    None, Some(_) -> Ok(Nil)
    Some(filename), Some(markdown) -> {
      let body = bit_array.from_string(markdown)
      use _ <- try_tracker(
        linear_attachment.validate_attachment_size(bit_array.byte_size(body)),
      )
      use _ <- try_tracker(linear_attachment.attach_markdown_to_comment(
        tracker_config,
        comment_id,
        filename,
        body,
        linear_attachment.AttachOptions(
          fallback_to_markdown_link: handoff_config.attachment_fallback_to_markdown_link,
          dedupe_by_filename: True,
        ),
        dependencies,
      ))
      Ok(Nil)
    }
  }
}

fn success_completion_decision(
  handoff_config: config_types.HandoffConfig,
  workflow_id: String,
  success: agent_types.WorkerSuccess,
) -> Option(workflow_completion_policy.CompletionStateDecision) {
  case handoff_config.completion_states {
    None -> None
    Some(policy) ->
      Some(workflow_completion_policy.choose_linear_completion_state(
        policy,
        workflow_id,
        workflow_completion_policy.success_outcome(
          handoff_config.completion_states,
          workflow_id,
          success,
        ),
      ))
  }
}

fn failure_completion_decision(
  handoff_config: config_types.HandoffConfig,
  workflow_id: String,
  failure: agent_types.WorkerFailure,
) -> Option(workflow_completion_policy.CompletionStateDecision) {
  case handoff_config.completion_states {
    None -> None
    Some(policy) ->
      Some(workflow_completion_policy.choose_linear_completion_state(
        policy,
        workflow_id,
        failure_completion_outcome(failure),
      ))
  }
}

fn failure_completion_outcome(
  failure: agent_types.WorkerFailure,
) -> workflow_completion_policy.WorkflowCompletionOutcome {
  case failure.reason {
    error.OperatorAbort -> workflow_completion_policy.cancellation_outcome()
    error.OperatorStopAfterCurrentTurn ->
      workflow_completion_policy.cancellation_outcome()
    _ -> workflow_completion_policy.failure_outcome()
  }
}

fn run_success_state_update(
  tracker_config: config_types.TrackerConfig,
  handoff_config: config_types.HandoffConfig,
  transport: linear.Transport,
  issue_id: String,
  decision: Option(workflow_completion_policy.CompletionStateDecision),
) -> Result(Nil, error.TrackerError) {
  case decision {
    Some(decision) ->
      run_completion_state_update(tracker_config, transport, issue_id, decision)
    None ->
      run_state_update(
        tracker_config,
        transport,
        issue_id,
        handoff_config.success_state_id,
      )
  }
}

fn run_failure_state_update(
  tracker_config: config_types.TrackerConfig,
  handoff_config: config_types.HandoffConfig,
  transport: linear.Transport,
  issue_id: String,
  decision: Option(workflow_completion_policy.CompletionStateDecision),
) -> Result(Nil, error.TrackerError) {
  case decision {
    Some(decision) ->
      run_completion_state_update(tracker_config, transport, issue_id, decision)
    None ->
      run_state_update(
        tracker_config,
        transport,
        issue_id,
        handoff_config.failure_state_id,
      )
  }
}

fn run_completion_state_update(
  tracker_config: config_types.TrackerConfig,
  transport: linear.Transport,
  issue_id: String,
  decision: workflow_completion_policy.CompletionStateDecision,
) -> Result(Nil, error.TrackerError) {
  case decision {
    workflow_completion_policy.LeaveLinearState(_) -> Ok(Nil)
    workflow_completion_policy.MoveToState(state, _) ->
      run_state_ref_update(tracker_config, transport, issue_id, state)
  }
}

fn run_state_ref_update(
  tracker_config: config_types.TrackerConfig,
  transport: linear.Transport,
  issue_id: String,
  state: workflow_completion_policy.LinearStateRef,
) -> Result(Nil, error.TrackerError) {
  case state {
    workflow_completion_policy.StateById(state_id) ->
      run_state_update(tracker_config, transport, issue_id, Some(state_id))
    workflow_completion_policy.StateByName(name) -> {
      use request <- try_tracker(linear.build_issue_team_states_request(
        tracker_config,
        issue_id,
      ))
      use response <- try_tracker(transport(request))
      use states <- try_tracker(linear.parse_issue_team_states_response(
        response,
      ))
      case linear.resolve_state_name(states, name) {
        Ok(state_id) ->
          run_state_update(tracker_config, transport, issue_id, Some(state_id))
        Error(reason) -> Error(completion_state_resolution_error(name, reason))
      }
    }
  }
}

fn completion_state_resolution_error(
  name: String,
  reason: linear.StateNameResolutionError,
) -> error.TrackerError {
  let reason_text = case reason {
    linear.StateNameNotFound -> "not found"
    linear.StateNameAmbiguous -> "ambiguous"
  }
  error.LinearApiRequest(
    "configured completion state "
    <> name
    <> " is "
    <> reason_text
    <> "; run scherzo doctor --check tracker-contract and see docs/runbooks/linear-completion-states.md",
  )
}

fn limit_success_result_for_attachment(
  success: agent_types.WorkerSuccess,
  max_chars: Int,
) -> agent_types.WorkerSuccess {
  case success.result.final_response {
    None -> success
    Some(text) ->
      case string.length(text) > max_chars {
        False -> success
        True ->
          agent_types.WorkerSuccess(
            ..success,
            result: result_artifact.ResultArtifact(
              ..success.result,
              final_response: Some(string.slice(
                from: text,
                at_index: 0,
                length: max_chars,
              )),
              truncated: True,
            ),
          )
      }
  }
}

fn run_comment(
  enabled: Bool,
  tracker_config: config_types.TrackerConfig,
  transport: linear.Transport,
  issue_id: String,
  body: String,
) -> Result(Nil, error.TrackerError) {
  case enabled {
    False -> Ok(Nil)
    True -> {
      use request <- try_tracker(linear.build_comment_create_request(
        tracker_config,
        issue_id,
        body,
      ))
      use response <- try_tracker(transport(request))
      linear.parse_mutation_response(response, "commentCreate")
    }
  }
}

fn run_state_update(
  tracker_config: config_types.TrackerConfig,
  transport: linear.Transport,
  issue_id: String,
  state_id: Option(String),
) -> Result(Nil, error.TrackerError) {
  case state_id {
    None -> Ok(Nil)
    Some(state_id) -> {
      use request <- try_tracker(linear.build_issue_update_state_request(
        tracker_config,
        issue_id,
        state_id,
      ))
      use response <- try_tracker(transport(request))
      linear.parse_mutation_response(response, "issueUpdate")
    }
  }
}

fn tracker_secrets(tracker_config: config_types.TrackerConfig) -> List(String) {
  case tracker_config.api_key {
    Some(value) -> [value]
    None -> []
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
