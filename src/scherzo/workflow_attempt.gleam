import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/orchestrator/core
import scherzo/state/projection
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_dag
import scherzo/workflow_fingerprint as fingerprint

pub const recovery_pi_resume_validation_failed = "recovery_pi_resume_validation_failed"

pub type StepAttemptContext {
  StepAttemptContext(
    run_id: String,
    issue_id: String,
    issue_identifier: String,
    workflow_id: String,
    workflow_fingerprint: String,
    step_id: String,
    workspace_name: String,
    attempt_index: Int,
    workspace_path: String,
    continuation_capable: Bool,
    continuation_session_file: Option(String),
  )
}

pub type AgentPromptMode {
  OriginalPrompt(String)
  StructuredOutputRetryPrompt(String)
  StepRecoveryPrompt(String)
  RecoveryPrompt(String)
}

pub type PiSessionObservation {
  PiSessionObservation(
    run_id: String,
    issue_id: String,
    issue_identifier: String,
    workflow_id: String,
    workflow_fingerprint: String,
    step_id: String,
    workspace_name: String,
    attempt_index: Int,
    workspace_path: String,
    session_id: String,
    session_file: String,
  )
}

pub type PiContinuation {
  PiContinuation(
    run_id: String,
    issue_id: String,
    issue_identifier: String,
    workflow_id: String,
    workflow_fingerprint: String,
    step_id: String,
    workspace_name: String,
    attempt_index: Int,
    workspace_path: String,
    session_id: String,
    session_file: String,
    recovery_prompt: String,
  )
}

pub fn attempt_key(
  run_id: String,
  workflow_id: String,
  step_id: String,
  workspace_name: String,
  attempt_index: Int,
  workspace_path: String,
) -> String {
  [
    run_id,
    workflow_id,
    step_id,
    workspace_name,
    int.to_string(attempt_index),
    workspace_path,
  ]
  |> list.map(length_prefixed)
  |> string.join(with: "")
}

pub fn projection_key(
  run_id: String,
  step_id: String,
  attempt_index: Int,
) -> String {
  attempt_key(run_id, "", step_id, "", attempt_index, "")
}

pub fn issue_fingerprint(issue: tracker_issue.Issue) -> String {
  core.issue_fingerprint(issue)
}

pub fn recovery_drift_reason(
  run_id: String,
  recorded_workflow_id: String,
  current_workflow_id: String,
  recorded_workflow_fingerprint: String,
  current_workflow_fingerprint: String,
  recorded_issue_fingerprint: String,
  current_issue_fingerprint: String,
) -> Option(#(String, String)) {
  case recorded_workflow_id != current_workflow_id {
    True ->
      Some(#(
        "workflow_definition_drift:workflow_id_changed",
        "workflow_recovery_parked_workflow_definition_drift:"
          <> run_id
          <> ":workflow_id_changed",
      ))
    False ->
      case recorded_workflow_fingerprint != current_workflow_fingerprint {
        True ->
          Some(#(
            "workflow_definition_drift:workflow_fingerprint_changed",
            "workflow_recovery_parked_workflow_definition_drift:"
              <> run_id
              <> ":workflow_fingerprint_changed",
          ))
        False ->
          case
            tracker_issue.fingerprint_equivalent(
              recorded_issue_fingerprint,
              current_issue_fingerprint,
            )
          {
            True -> None
            False ->
              Some(#(
                "issue_content_drift:issue_fingerprint_changed",
                "workflow_recovery_parked_issue_content_drift:"
                  <> run_id
                  <> ":issue_fingerprint_changed",
              ))
          }
      }
  }
}

pub fn recovery_issue_state_drift(
  effective_config: Option(config_types.EffectiveConfig),
  issue: tracker_issue.Issue,
  run_id: String,
) -> Option(#(String, String)) {
  case recovery_terminal_issue_state_drift(effective_config, issue, run_id) {
    Some(drift) -> Some(drift)
    None ->
      case effective_config {
        None -> None
        Some(config) ->
          case core.is_active(config, issue.state) {
            True -> None
            False ->
              Some(issue_state_drift_reason(
                run_id,
                "non_active_state",
                issue_state.to_string(issue.state),
              ))
          }
      }
  }
}

pub fn recovery_terminal_issue_state_drift(
  effective_config: Option(config_types.EffectiveConfig),
  issue: tracker_issue.Issue,
  run_id: String,
) -> Option(#(String, String)) {
  case effective_config {
    None -> None
    Some(config) ->
      case core.is_terminal(config, issue.state) {
        True ->
          Some(issue_state_drift_reason(
            run_id,
            "terminal_state",
            issue_state.to_string(issue.state),
          ))
        False -> None
      }
  }
}

fn issue_state_drift_reason(
  run_id: String,
  reason: String,
  state: String,
) -> #(String, String) {
  #(
    "issue_state_drift:" <> reason,
    "workflow_recovery_parked_issue_state_drift:"
      <> run_id
      <> ":"
      <> reason
      <> ":"
      <> state,
  )
}

pub fn parked_issue_should_survive(
  parked: projection.ParkedIssue,
  issue_id: String,
  issue_by_id: Dict(String, tracker_issue.Issue),
) -> Bool {
  case parked.release_policy {
    "auto_unpark_on_issue_change" ->
      case dict.get(issue_by_id, issue_id) {
        Ok(issue) ->
          tracker_issue.fingerprint_matches(parked.issue_fingerprint, issue)
        Error(Nil) -> True
      }
    _ -> True
  }
}

pub fn remaining_retry_delay(
  status: projection.RetryStatus,
  now_ms: Int,
) -> Int {
  case projection.retry_due_at_ms(status) {
    Ok(due_at_ms) ->
      case due_at_ms > now_ms {
        True -> due_at_ms - now_ms
        False -> 0
      }
    Error(Nil) -> 0
  }
}

pub fn workflow_fingerprint(
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
) -> String {
  fingerprint.for_execution(workflow_dag.id(dag), dag, orchestrator)
  |> result.unwrap("workflow_fingerprint_failed")
}

pub fn render_recovery_prompt(
  template: String,
  context: StepAttemptContext,
) -> String {
  template
  |> replace("{run_id}", context.run_id)
  |> replace("{workflow_id}", context.workflow_id)
  |> replace("{step_id}", context.step_id)
  |> replace("{workspace_name}", context.workspace_name)
  |> replace("{attempt_index}", int.to_string(context.attempt_index))
}

fn length_prefixed(value: String) -> String {
  int.to_string(string.length(value)) <> ":" <> value <> ";"
}

fn replace(value: String, pattern: String, replacement: String) -> String {
  string.replace(value, pattern, replacement)
}
