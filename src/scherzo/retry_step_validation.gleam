import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/state/projection
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_attempt

pub type Failure {
  Failure(reason: String, message: String)
}

pub fn validate_drift(
  recorded_workflow_id: String,
  current_workflow_id: String,
  recorded_workflow_fingerprint: String,
  current_workflow_fingerprint: String,
  recorded_issue_fingerprint: String,
  current_issue_fingerprint: String,
) -> Result(Nil, Failure) {
  case recorded_workflow_id != current_workflow_id {
    True ->
      Error(Failure(reason: "workflow_drift", message: "workflow id drifted"))
    False ->
      case recorded_workflow_fingerprint != current_workflow_fingerprint {
        True ->
          Error(Failure(
            reason: "workflow_drift",
            message: "workflow fingerprint drifted",
          ))
        False ->
          case
            tracker_issue.fingerprint_equivalent(
              recorded_issue_fingerprint,
              current_issue_fingerprint,
            )
          {
            True -> Ok(Nil)
            False ->
              Error(Failure(
                reason: "issue_drift",
                message: "issue fingerprint drifted",
              ))
          }
      }
  }
}

pub fn validate_workflow_identity(
  recorded_workflow_id: String,
  current_workflow_id: String,
  recorded_workflow_fingerprint: String,
  current_workflow_fingerprint: String,
) -> Result(Nil, Failure) {
  validate_drift(
    recorded_workflow_id,
    current_workflow_id,
    recorded_workflow_fingerprint,
    current_workflow_fingerprint,
    "",
    "",
  )
}

pub fn recovery_drift_for_mode(
  retry_step retry_step: Bool,
  run_id run_id: String,
  recorded_workflow_id recorded_workflow_id: String,
  current_workflow_id current_workflow_id: String,
  recorded_workflow_fingerprint recorded_workflow_fingerprint: String,
  current_workflow_fingerprint current_workflow_fingerprint: String,
  recorded_issue_fingerprint recorded_issue_fingerprint: String,
  current_issue_fingerprint current_issue_fingerprint: String,
) -> Option(#(String, String)) {
  case retry_step {
    True ->
      retry_step_recovery_drift(
        run_id,
        recorded_workflow_id,
        current_workflow_id,
        recorded_workflow_fingerprint,
        current_workflow_fingerprint,
        recorded_issue_fingerprint,
        current_issue_fingerprint,
      )
    False ->
      workflow_attempt.recovery_drift_reason(
        run_id,
        recorded_workflow_id,
        current_workflow_id,
        recorded_workflow_fingerprint,
        current_workflow_fingerprint,
        recorded_issue_fingerprint,
        current_issue_fingerprint,
      )
  }
}

pub fn recorded_or_current(recorded: String, current: String) -> String {
  case string.trim(recorded) == "" {
    True -> current
    False -> recorded
  }
}

pub fn validation_rejection_message(
  failure: Failure,
  run_id: String,
  step_id: Option(String),
) -> String {
  "retry-step rejected: "
  <> failure.reason
  <> " ("
  <> failure.message
  <> "); no run, park, or tracker state was changed. Next safe command: "
  <> next_safe_command(run_id, step_id)
}

pub fn operation_failure_message(
  reason: String,
  detail: Option(String),
  run_id: String,
  step_id: Option(String),
) -> String {
  let detail_text = case detail {
    Some(message) -> " (" <> message <> ")"
    None -> ""
  }
  "retry-step rejected: "
  <> reason
  <> detail_text
  <> "; no run, park, or tracker state was changed. Next safe command: "
  <> next_safe_command(run_id, step_id)
}

pub fn stable_rejection_reason(reason: String) -> String {
  case string.starts_with(reason, "workflow_definition_drift:") {
    True -> "workflow_drift"
    False ->
      case string.starts_with(reason, "issue_content_drift:") {
        True -> "issue_drift"
        False -> reason
      }
  }
}

pub fn parked_issue_can_retry_step(
  projection_state: projection.Projection,
  run_id: String,
  issue_id: String,
) -> Bool {
  case dict.get(projection_state.parked_issues, issue_id) {
    Ok(parked) ->
      retry_step_can_use_parked_issue_reason(parked.reason)
      && workflow_run_interrupted_for_parked_issue(
        projection_state,
        run_id,
        issue_id,
        parked.reason,
      )
    Error(Nil) -> False
  }
}

fn retry_step_can_use_parked_issue_reason(reason: String) -> Bool {
  string.starts_with(reason, "issue_content_drift:")
  || string.starts_with(reason, "issue_state_drift:")
}

fn workflow_run_interrupted_for_parked_issue(
  projection_state: projection.Projection,
  run_id: String,
  issue_id: String,
  reason: String,
) -> Bool {
  case dict.get(projection_state.workflow_runs, run_id) {
    Ok(projection.WorkflowRunInterrupted(
      issue_id: run_issue_id,
      reason: run_reason,
      ..,
    )) -> run_issue_id == issue_id && run_reason == reason
    _ -> False
  }
}

fn retry_step_recovery_drift(
  run_id: String,
  recorded_workflow_id: String,
  current_workflow_id: String,
  recorded_workflow_fingerprint: String,
  current_workflow_fingerprint: String,
  _recorded_issue_fingerprint: String,
  _current_issue_fingerprint: String,
) -> Option(#(String, String)) {
  case
    validate_workflow_identity(
      recorded_workflow_id,
      current_workflow_id,
      recorded_workflow_fingerprint,
      current_workflow_fingerprint,
    )
  {
    Ok(Nil) -> None
    Error(failure) ->
      Some(#(
        failure.reason,
        "workflow_recovery_rejected_retry_step:"
          <> run_id
          <> ":"
          <> failure.reason
          <> ":"
          <> failure.message,
      ))
  }
}

fn next_safe_command(run_id: String, step_id: Option(String)) -> String {
  let step = case step_id {
    Some(step) -> step
    None -> "<step-id>"
  }
  "scripts/scherzoctl run retry-step " <> run_id <> " --step " <> step
}
