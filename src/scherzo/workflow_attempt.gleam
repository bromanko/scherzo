import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/orchestrator/core
import scherzo/tracker/issue as tracker_issue
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

pub fn workflow_fingerprint(
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
) -> String {
  fingerprint.for_execution(dag.id, dag, orchestrator)
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
