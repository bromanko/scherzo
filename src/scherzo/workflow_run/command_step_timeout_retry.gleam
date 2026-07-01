import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/step_artifact
import scherzo/workflow_run/step_worker_pool

const max_attempts_value = 2

const watchdog_exit_code = 124

const watchdog_metadata_marker = "timeout_kind: step_batch_watchdog"

pub fn max_attempts() -> Int {
  max_attempts_value
}

pub fn failure_code() -> String {
  step_worker_pool.step_batch_timeout_failure_code
}

pub fn unrecovered_failure_code() -> String {
  "step_batch_timeout_unrecovered"
}

pub fn next_retry_attempt(
  is_command_step is_command_step: Bool,
  artifact artifact: step_artifact.StepArtifact,
  batch_safe_to_retry batch_safe_to_retry: Bool,
  interrupted_step_ids interrupted_step_ids: List(String),
  attempt_index attempt_index: Int,
) -> Option(Int) {
  case
    is_command_step
    && is_watchdog_timeout_artifact(artifact)
    && batch_safe_to_retry
    && interrupted_step_ids == []
    && attempt_index < max_attempts_value
  {
    True -> Some(attempt_index + 1)
    False -> None
  }
}

pub fn retry_exhausted(
  is_command_step is_command_step: Bool,
  artifact artifact: step_artifact.StepArtifact,
  attempt_index attempt_index: Int,
) -> Bool {
  is_command_step
  && is_watchdog_timeout_artifact(artifact)
  && attempt_index >= max_attempts_value
}

pub fn is_failure_code(code: String) -> Bool {
  code == failure_code()
}

pub fn is_watchdog_timeout_artifact(
  artifact: step_artifact.StepArtifact,
) -> Bool {
  artifact.failure_code == Some(failure_code())
  && artifact.exit_code == Some(watchdog_exit_code)
  && artifact.timed_out
  && string.contains(artifact.stderr, watchdog_metadata_marker)
}

pub fn terminal_reason(step_id: String) -> String {
  "workflow_step_failed:" <> unrecovered_failure_code() <> ":step=" <> step_id
}

pub fn report_failure_code(reason: String, artifact_code: String) -> String {
  case
    is_failure_code(artifact_code) && reason_has_unrecovered_timeout(reason)
  {
    True -> unrecovered_failure_code()
    False -> artifact_code
  }
}

pub fn retry_scheduled_diagnostic_reason(
  step_id: String,
  failed_attempt_index: Int,
  retry_attempt_index: Int,
) -> String {
  "command_step_timeout_retry_scheduled:step="
  <> step_id
  <> ":failed_attempt="
  <> int.to_string(failed_attempt_index)
  <> ":retry_attempt="
  <> int.to_string(retry_attempt_index)
  <> ":max_attempts="
  <> int.to_string(max_attempts_value)
  <> ":failure_code="
  <> failure_code()
}

pub fn retry_exhausted_diagnostic_reason(
  step_id: String,
  failed_attempt_index: Int,
) -> String {
  "command_step_timeout_retry_exhausted:step="
  <> step_id
  <> ":failed_attempt="
  <> int.to_string(failed_attempt_index)
  <> ":max_attempts="
  <> int.to_string(max_attempts_value)
  <> ":failure_code="
  <> failure_code()
}

pub fn handoff_next_action(code: String) -> Option(String) {
  case code == unrecovered_failure_code(), is_failure_code(code) {
    True, _ ->
      Some(
        "Scherzo retried the command-step batch timeout within its step retry budget, but the timeout was not recovered. Inspect the retained workspace and command diagnostics, then retry the workflow only when safe.",
      )
    _, True ->
      Some(
        "Scherzo recorded a command-step batch timeout marker that was not eligible for automatic step retry. Inspect the retained workspace and command diagnostics, then retry the workflow only when safe.",
      )
    _, _ -> None
  }
}

fn reason_has_unrecovered_timeout(reason: String) -> Bool {
  string.contains(reason, "workflow_step_failed:" <> unrecovered_failure_code())
}
