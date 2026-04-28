import gleam/int
import gleam/option.{type Option, None, Some}
import scherzo/agent/runner
import scherzo/domain
import scherzo/error
import scherzo/linear

pub type Client {
  Client(
    claim_issue: fn(domain.Issue, String) -> Result(Nil, error.TrackerError),
    report_success: fn(domain.Issue, runner.WorkerSuccess, String) ->
      Result(Nil, error.TrackerError),
    report_failure: fn(domain.Issue, runner.WorkerFailure, String) ->
      Result(Nil, error.TrackerError),
  )
}

pub fn disabled_client() -> Client {
  Client(
    claim_issue: fn(_, _) { Ok(Nil) },
    report_success: fn(_, _, _) { Ok(Nil) },
    report_failure: fn(_, _, _) { Ok(Nil) },
  )
}

pub fn linear_client(
  tracker_config: domain.TrackerConfig,
  handoff_config: domain.HandoffConfig,
  transport: linear.Transport,
) -> Client {
  case handoff_config.enabled {
    False -> disabled_client()
    True ->
      Client(
        claim_issue: fn(issue, run_id) {
          claim_issue(tracker_config, handoff_config, transport, issue, run_id)
        },
        report_success: fn(issue, success, run_id) {
          report_success(
            tracker_config,
            handoff_config,
            transport,
            issue,
            success,
            run_id,
          )
        },
        report_failure: fn(issue, failure, run_id) {
          report_failure(
            tracker_config,
            handoff_config,
            transport,
            issue,
            failure,
            run_id,
          )
        },
      )
  }
}

fn claim_issue(
  tracker_config: domain.TrackerConfig,
  handoff_config: domain.HandoffConfig,
  transport: linear.Transport,
  issue: domain.Issue,
  run_id: String,
) -> Result(Nil, error.TrackerError) {
  use _ <- try_tracker(run_comment(
    handoff_config.comment_on_claim,
    tracker_config,
    transport,
    issue.id,
    "Scherzo claimed " <> issue.identifier <> " for run " <> run_id <> ".",
  ))
  run_state_update(
    tracker_config,
    transport,
    issue.id,
    handoff_config.claim_state_id,
  )
}

fn report_success(
  tracker_config: domain.TrackerConfig,
  handoff_config: domain.HandoffConfig,
  transport: linear.Transport,
  issue: domain.Issue,
  success: runner.WorkerSuccess,
  run_id: String,
) -> Result(Nil, error.TrackerError) {
  use _ <- try_tracker(run_comment(
    handoff_config.comment_on_success,
    tracker_config,
    transport,
    issue.id,
    "Scherzo completed run "
      <> run_id
      <> " for "
      <> issue.identifier
      <> " with classification "
      <> classification_to_string(success.final_classification)
      <> " and "
      <> int.to_string(success.tokens.total)
      <> " total pi tokens.",
  ))
  run_state_update(
    tracker_config,
    transport,
    issue.id,
    handoff_config.success_state_id,
  )
}

fn report_failure(
  tracker_config: domain.TrackerConfig,
  handoff_config: domain.HandoffConfig,
  transport: linear.Transport,
  issue: domain.Issue,
  failure: runner.WorkerFailure,
  run_id: String,
) -> Result(Nil, error.TrackerError) {
  use _ <- try_tracker(run_comment(
    handoff_config.comment_on_failure,
    tracker_config,
    transport,
    issue.id,
    "Scherzo failed run "
      <> run_id
      <> " for "
      <> issue.identifier
      <> " with error "
      <> error.agent_code(failure.reason)
      <> ".",
  ))
  run_state_update(
    tracker_config,
    transport,
    issue.id,
    handoff_config.failure_state_id,
  )
}

fn run_comment(
  enabled: Bool,
  tracker_config: domain.TrackerConfig,
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
  tracker_config: domain.TrackerConfig,
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

fn classification_to_string(
  classification: runner.FinalClassification,
) -> String {
  case classification {
    runner.FinalActive -> "active"
    runner.FinalTerminal -> "terminal"
    runner.FinalNonActive -> "non_active"
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
