import gleam/option.{type Option, None, Some}
import scherzo/agent/types as agent_types
import scherzo/config/types as config_types
import scherzo/error
import scherzo/step_artifact
import scherzo/workflow_attempt

pub fn agent_failure_artifact(
  step_id: String,
  failure: agent_types.WorkerFailure,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
) -> step_artifact.StepArtifact {
  let detail = error.agent_artifact_detail(failure.reason)
  let stderr = case is_recovery_resume_validation_failure(failure.reason) {
    True ->
      "SCHERZO_FAILURE_CODE="
      <> workflow_attempt.recovery_pi_resume_validation_failed
      <> "\n"
      <> detail
    False -> detail
  }
  let artifact =
    step_artifact.from_command_result(
      step_id,
      1,
      "",
      stderr,
      False,
      secrets,
      limits,
    )
  step_artifact.StepArtifact(
    ..artifact,
    summary_text: artifact.summary_text
      <> context_recovery_summary_suffix(failure.reason),
  )
}

fn context_recovery_summary_suffix(reason: error.AgentRunnerError) -> String {
  case reason {
    error.ContextRecoveryExhausted(
      recovery_method: recovery_method,
      context_artifact_ref: context_artifact_ref,
      result_artifact_ref: result_artifact_ref,
      ..,
    ) ->
      " context_recovery=failed recovery_exhausted=true recovery_method="
      <> recovery_method
      <> summary_ref("context_artifact", context_artifact_ref)
      <> summary_ref("result_artifact", result_artifact_ref)
    _ -> ""
  }
}

fn summary_ref(label: String, ref: Option(String)) -> String {
  case ref {
    Some(ref) -> " " <> label <> "=" <> ref
    _ -> ""
  }
}

fn is_recovery_resume_validation_failure(
  reason: error.AgentRunnerError,
) -> Bool {
  case reason {
    error.PiFailed(error.PiProtocolError(message)) ->
      message == workflow_attempt.recovery_pi_resume_validation_failed
    _ -> False
  }
}

pub fn is_recovery_resume_validation_artifact(
  artifact: step_artifact.StepArtifact,
) -> Bool {
  artifact.failure_code
  == Some(workflow_attempt.recovery_pi_resume_validation_failed)
}

pub fn agent_reason_for_artifact(
  artifact: step_artifact.StepArtifact,
) -> Option(error.AgentRunnerError) {
  case is_recovery_resume_validation_artifact(artifact) {
    True ->
      Some(
        error.PiFailed(error.PiProtocolError(
          workflow_attempt.recovery_pi_resume_validation_failed,
        )),
      )
    False -> None
  }
}
