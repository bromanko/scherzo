import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/config/types as config_types
import scherzo/error
import scherzo/result_artifact
import scherzo/step_artifact
import scherzo/structured_output
import scherzo/structured_output_metadata
import scherzo/structured_output_source
import scherzo/structured_output_tool_spec
import scherzo/workflow_attempt
import scherzo/workflow_checkpoint
import scherzo/workflow_dag
import scherzo/workflow_run/step_context.{type StepContext, StepContext}

pub fn prepare_structured_output_tool_context(
  context: StepContext,
  structured_output_spec: Option(workflow_dag.StructuredOutputSpec),
) -> Result(StepContext, structured_output_tool_spec.ToolSpecError) {
  case structured_output_spec {
    Some(spec) ->
      case structured_output_tool_spec.schema_path_for_source(spec.source) {
        Some(_) -> {
          use tool_spec <- result.try(
            structured_output_tool_spec.for_step(
              structured_output_tool_spec.BuildInput(
                workflow_id: context.workflow_id,
                run_id: context.run_id,
                step_id: context.step_id,
                attempt_index: context.attempt_index,
                repository_root: structured_output.validator_repo_root(
                  context.config_dir,
                  context.workspace_path,
                ),
                spec: spec,
              ),
            ),
          )
          use written <- result.try(structured_output_tool_spec.write(
            tool_spec,
            context.run_root,
          ))
          Ok(
            StepContext(..context, extra_pi_env: [
              structured_output_tool_spec.env_pair(written),
            ]),
          )
        }
        None -> Ok(context)
      }
    None -> Ok(context)
  }
}

pub fn structured_output_retry_diagnostic(
  spec: Option(workflow_dag.StructuredOutputSpec),
  artifact: step_artifact.StepArtifact,
) -> Option(step_artifact.StructuredOutputRetryDiagnostic) {
  case spec {
    Some(spec) ->
      case
        spec.required
        && spec.validation_retries > 0
        && structured_output_artifact_retryable(artifact)
      {
        True -> structured_output_attempt_diagnostic(1, artifact)
        False -> None
      }
    None -> None
  }
}

fn structured_output_artifact_retryable(
  artifact: step_artifact.StepArtifact,
) -> Bool {
  case artifact.structured_output {
    Some(step_artifact.StructuredOutputError(_, _, _, Some(details), _)) ->
      details.retryable
    _ -> is_structured_output_validation_failure(artifact.failure_code)
  }
}

fn is_structured_output_validation_failure(code: Option(String)) -> Bool {
  case code {
    Some("structured_output_artifact_write_failed")
    | Some("structured_output_json_schema_config_error")
    | Some("structured_output_command_config_error")
    | Some("structured_output_command_timeout") -> False
    Some(value) -> string.starts_with(value, "structured_output_")
    None -> False
  }
}

pub fn structured_output_attempt_diagnostic(
  attempt: Int,
  artifact: step_artifact.StepArtifact,
) -> Option(step_artifact.StructuredOutputRetryDiagnostic) {
  case artifact.structured_output {
    Some(step_artifact.StructuredOutputValid(_)) ->
      Some(step_artifact.StructuredOutputRetryDiagnostic(
        attempt: attempt,
        status: "valid",
        failure_code: None,
        message: "required structured output validated",
      ))
    Some(step_artifact.StructuredOutputError(_, _, message, details, _)) ->
      Some(step_artifact.StructuredOutputRetryDiagnostic(
        attempt: attempt,
        status: "error",
        failure_code: artifact.failure_code,
        message: structured_output_retry_message(message, details),
      ))
    _ -> None
  }
}

fn structured_output_retry_message(
  message: String,
  details: Option(step_artifact.StructuredOutputErrorDetails),
) -> String {
  case details {
    Some(details) ->
      message <> "\nRetryable: " <> bool_string(details.retryable)
    None -> message
  }
}

fn bool_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

pub fn agent_success_artifact(
  step: workflow_dag.WorkflowStep,
  context: StepContext,
  success: agent_types.WorkerSuccess,
  structured_output_spec: Option(workflow_dag.StructuredOutputSpec),
  secrets: List(String),
  limits: config_types.ArtifactLimits,
  checkpoint: workflow_checkpoint.Writer,
) -> step_artifact.StepArtifact {
  case structured_output_spec {
    None -> step_artifact.from_agent_success(step.id, success, secrets, limits)
    Some(spec) ->
      agent_success_with_structured_output(
        step,
        context,
        success,
        spec,
        secrets,
        limits,
        checkpoint,
      )
  }
}

fn agent_success_with_structured_output(
  step: workflow_dag.WorkflowStep,
  context: StepContext,
  success: agent_types.WorkerSuccess,
  spec: workflow_dag.StructuredOutputSpec,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
  checkpoint: workflow_checkpoint.Writer,
) -> step_artifact.StepArtifact {
  let base = step_artifact.from_agent_success(step.id, success, secrets, limits)
  let format = workflow_dag.structured_output_format_to_string(spec.format)
  let workflow_dag.StructuredObjectSchema(required_keys) = spec.schema
  case
    structured_output.validate_agent_result(
      spec,
      success.result,
      secrets,
      structured_output.default_validator_runner(
        structured_output.default_validator_context(
          context.config_dir,
          context.run_root,
          context.workflow_id,
          context.workflow_bundle_dir,
          context.run_id,
          step.id,
          context.attempt_index,
          context.workspace_path,
          spec.artifact_name,
          format,
          spec.source,
        ),
        secrets,
      ),
    )
  {
    Ok(structured_output.StructuredOutputAbsent) ->
      step_artifact.StepArtifact(
        ..base,
        structured_output: Some(step_artifact.StructuredOutputAbsent(
          spec.artifact_name,
          format,
          "not_applicable",
        )),
      )
    Ok(structured_output.StructuredOutputPresent(payload_json)) ->
      write_structured_output_artifact(
        step,
        context,
        success,
        spec,
        format,
        required_keys,
        payload_json,
        secrets,
        limits,
        checkpoint,
      )
    Error(error) -> {
      let failure_code = structured_output.error_code(error)
      step_artifact.from_agent_structured_output_error_with_details(
        step.id,
        success,
        secrets,
        limits,
        failure_code,
        structured_output.error_message_for_step(error, step.id),
        spec.artifact_name,
        format,
        Some(step_artifact.StructuredOutputErrorDetails(
          code: failure_code,
          retryable: structured_output.error_retryable(error),
          validator_name: structured_output.error_validator_name(error),
          validator_type: structured_output.error_validator_type(error),
          diagnostic_summary: structured_output.error_diagnostic_summary(error),
          stdout_truncated: structured_output.error_stdout_truncated(error),
          stderr_truncated: structured_output.error_stderr_truncated(error),
        )),
      )
    }
  }
}

fn write_structured_output_artifact(
  step: workflow_dag.WorkflowStep,
  context: StepContext,
  success: agent_types.WorkerSuccess,
  spec: workflow_dag.StructuredOutputSpec,
  format: String,
  required_keys: List(String),
  payload_json: String,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
  checkpoint: workflow_checkpoint.Writer,
) -> step_artifact.StepArtifact {
  let validation =
    structured_output_metadata.from_spec_with_receipt(
      spec,
      structured_output.validator_repo_root(
        context.config_dir,
        context.workspace_path,
      ),
      receipt_json_for_source(spec.source, success.result.tool_calls),
    )
  let write =
    workflow_checkpoint.StructuredOutputWrite(
      run_id: context.run_id,
      workflow_id: context.workflow_id,
      step_id: step.id,
      attempt_index: context.attempt_index,
      artifact_name: spec.artifact_name,
      format: format,
      schema_required_keys: required_keys,
      validation: validation,
      payload_json: payload_json,
    )
  case checkpoint.write_structured_output_artifact(write) {
    Ok(written) ->
      step_artifact.from_agent_success_with_valid_structured_output(
        step.id,
        success,
        secrets,
        limits,
        step_artifact.StructuredOutputMetadata(
          artifact_name: spec.artifact_name,
          format: format,
          ref: written.ref,
          path: written.path,
          uri: written.uri,
          display_path: written.display_path,
          local_path: written.local_path,
          sha256: written.sha256,
          bytes: written.bytes,
          schema_status: "valid",
          source_type: validation.source_type,
          source_tool_name: validation.source_tool_name,
          source_parameters_schema_path: validation.source_parameters_schema_path,
          source_parameters_schema_sha256: validation.source_parameters_schema_sha256,
          source_receipt_json: validation.source_receipt_json,
          baseline_required_keys: required_keys,
          validators: structured_output_metadata.validator_summaries(validation),
          retry: None,
        ),
      )
    Error(error) -> {
      let message =
        "step "
        <> step.id
        <> " structured output artifact write failed: "
        <> workflow_checkpoint.describe_error(error)
      step_artifact.from_agent_structured_output_error(
        step.id,
        success,
        secrets,
        limits,
        "structured_output_artifact_write_failed",
        message,
        spec.artifact_name,
        format,
      )
    }
  }
}

fn receipt_json_for_source(
  source: structured_output_source.StructuredOutputSource,
  tool_calls: List(result_artifact.ToolCallSubmission),
) -> Option(String) {
  case source {
    structured_output_source.PiToolCallSource(tool_name, _, _, _) ->
      receipt_json_for_tool(tool_calls, tool_name)
    structured_output_source.FinalResponseSource -> None
  }
}

fn receipt_json_for_tool(
  tool_calls: List(result_artifact.ToolCallSubmission),
  tool_name: String,
) -> Option(String) {
  case tool_calls {
    [] -> None
    [call, ..rest] ->
      case call.name == tool_name, call.receipt_json {
        True, Some(receipt_json) -> Some(receipt_json)
        _, _ -> receipt_json_for_tool(rest, tool_name)
      }
  }
}

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
    None -> ""
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
