import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import scherzo/agent/types as agent_types
import scherzo/config/types as config_types
import scherzo/model_config
import scherzo/result_artifact
import scherzo/session/tokens as session_tokens
import scherzo/step_artifact
import scherzo/structured_output_metadata
import scherzo/template
import scherzo/workflow_dag

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 12,
    template_field_max_chars: 12,
    workflow_summary_max_chars: 200,
  )
}

fn agent_success(text: String) -> agent_types.WorkerSuccess {
  agent_types.WorkerSuccess(
    final_issue: None,
    final_classification: agent_types.FinalTerminal,
    workspace_path: "workspace",
    tokens: session_tokens.zero_token_totals(),
    turns: 1,
    result: result_artifact.from_final_response(Some(text), False, "test"),
  )
}

fn lookup(
  locals: List(#(String, template.Value)),
  key: String,
) -> template.Value {
  let assert Ok(value) = list.key_find(locals, key)
  value
}

pub fn agent_success_artifact_exposes_template_local_test() {
  let artifact =
    step_artifact.from_agent_success(
      "code_review",
      agent_success("Looks good"),
      [],
      limits(),
    )
  let locals =
    step_artifact.to_template_locals(
      dict.from_list([#("code_review", artifact)]),
    )
  assert lookup(locals, "steps.code_review.status")
    == template.VString("success")
  assert lookup(locals, "steps.code_review.final_response")
    == template.VString("Looks good")
}

pub fn agent_success_without_final_response_stays_successful_test() {
  let success =
    agent_types.WorkerSuccess(
      final_issue: None,
      final_classification: agent_types.FinalTerminal,
      workspace_path: "workspace",
      tokens: session_tokens.zero_token_totals(),
      turns: 1,
      result: result_artifact.from_final_response(None, False, "none"),
    )
  let artifact =
    step_artifact.from_agent_success("research", success, [], limits())

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.failure_code == None
  assert artifact.final_response == None
  assert artifact.final_response_truncated == False
}

pub fn structured_output_metadata_encodes_decodes_and_exposes_template_locals_test() {
  let metadata =
    step_artifact.StructuredOutputMetadata(
      artifact_name: "review_result",
      format: "json",
      ref: "runs/run-1/review_json/attempt-0/structured/review_result.json",
      path: "test/tmp/artifact.json",
      sha256: "abc123",
      bytes: 42,
      schema_status: "valid",
      source_type: "final_response",
      source_tool_name: None,
      baseline_required_keys: ["summary", "findings"],
      validators: [
        structured_output_metadata.ValidatorSummary(
          name: "shape",
          validator_type: "json_schema",
          status: "passed",
        ),
      ],
      retry: None,
    )
  let artifact =
    step_artifact.from_agent_success_with_valid_structured_output(
      "review_json",
      agent_success("{\"summary\":\"ok\",\"findings\":[]}"),
      [],
      limits(),
      metadata,
    )

  let assert Ok(decoded) =
    step_artifact.decode_string(step_artifact.to_string(artifact))
  assert decoded.structured_output == artifact.structured_output
  let locals =
    step_artifact.to_template_locals(
      dict.from_list([#("review_json", artifact)]),
    )
  assert lookup(locals, "steps.review_json.structured_output.status")
    == template.VString("valid")
  assert lookup(locals, "steps.review_json.structured_output.artifact_name")
    == template.VString("review_result")
  assert lookup(locals, "steps.review_json.structured_output.format")
    == template.VString("json")
  assert lookup(locals, "steps.review_json.structured_output.ref")
    == template.VString(
      "runs/run-1/review_json/attempt-0/structured/review_result.json",
    )
  assert lookup(locals, "steps.review_json.structured_output.path")
    == template.VString("test/tmp/artifact.json")
  assert lookup(locals, "steps.review_json.structured_output.sha256")
    == template.VString("abc123")
  assert lookup(locals, "steps.review_json.structured_output.bytes")
    == template.VInt(42)
  assert lookup(locals, "steps.review_json.structured_output.schema_status")
    == template.VString("valid")
  assert lookup(locals, "steps.review_json.structured_output.error")
    == template.VNil
  assert lookup(locals, "steps.review_json.structured_output.retry_outcome")
    == template.VNil
}

pub fn optional_absent_structured_output_exposes_absent_status_test() {
  let artifact =
    step_artifact.from_agent_success_with_absent_structured_output(
      "review_json",
      agent_success(""),
      [],
      limits(),
      "review_result",
      "json",
      "not_applicable",
    )
  let locals =
    step_artifact.to_template_locals(
      dict.from_list([#("review_json", artifact)]),
    )

  assert lookup(locals, "steps.review_json.structured_output.status")
    == template.VString("absent")
  assert lookup(locals, "steps.review_json.structured_output.artifact_name")
    == template.VString("review_result")
  assert lookup(locals, "steps.review_json.structured_output.format")
    == template.VString("json")
  assert lookup(locals, "steps.review_json.structured_output.ref")
    == template.VNil
  assert lookup(locals, "steps.review_json.structured_output.path")
    == template.VNil
  assert lookup(locals, "steps.review_json.structured_output.sha256")
    == template.VNil
  assert lookup(locals, "steps.review_json.structured_output.bytes")
    == template.VNil
  assert lookup(locals, "steps.review_json.structured_output.schema_status")
    == template.VString("not_applicable")
  assert lookup(locals, "steps.review_json.structured_output.error")
    == template.VNil
}

pub fn structured_output_error_exposes_error_status_test() {
  let artifact =
    step_artifact.from_agent_structured_output_error(
      "review_json",
      agent_success("not json"),
      [],
      limits(),
      "structured_output_invalid_json",
      "step review_json required a JSON-only final response",
      "review_result",
      "json",
    )
  let assert Ok(decoded) =
    step_artifact.decode_string(step_artifact.to_string(artifact))
  assert decoded.failure_code == Some("structured_output_invalid_json")
  let locals =
    step_artifact.to_template_locals(
      dict.from_list([#("review_json", artifact)]),
    )
  assert lookup(locals, "steps.review_json.structured_output.status")
    == template.VString("error")
  assert lookup(locals, "steps.review_json.structured_output.error")
    == template.VString("step review_json required a JSON-only final response")
  assert lookup(locals, "steps.review_json.structured_output.path")
    == template.VNil
}

pub fn structured_output_error_details_encode_decode_and_expose_locals_test() {
  let artifact =
    step_artifact.from_agent_structured_output_error_with_details(
      "review_json",
      agent_success("not json"),
      [],
      limits(),
      "structured_output_command_rejected",
      "step review_json validator rejected payload",
      "review_result",
      "json",
      Some(step_artifact.StructuredOutputErrorDetails(
        code: "structured_output_command_rejected",
        retryable: True,
        validator_name: Some("shape"),
        validator_type: Some("command"),
        diagnostic_summary: "lane.category is required",
        stdout_truncated: False,
        stderr_truncated: True,
      )),
    )
  let assert Ok(decoded) =
    step_artifact.decode_string(step_artifact.to_string(artifact))
  let assert Some(step_artifact.StructuredOutputError(_, _, _, Some(details), _)) =
    decoded.structured_output
  assert details.retryable
  assert details.validator_name == Some("shape")
  assert details.stderr_truncated
  let locals =
    step_artifact.to_template_locals(
      dict.from_list([#("review_json", decoded)]),
    )
  assert lookup(
      locals,
      "steps.review_json.structured_output.failure_validator_name",
    )
    == template.VString("shape")
  assert lookup(locals, "steps.review_json.structured_output.failure_retryable")
    == template.VBool(True)
}

pub fn structured_output_retry_info_encodes_decodes_and_exposes_locals_test() {
  let artifact =
    step_artifact.from_agent_structured_output_error(
      "review_json",
      agent_success("not json"),
      [],
      limits(),
      "structured_output_invalid_json",
      "step review_json required a JSON-only final response",
      "review_result",
      "json",
    )
  let retry =
    step_artifact.StructuredOutputRetryInfo(
      max_retries: 1,
      attempts: 2,
      outcome: "failed",
      diagnostics: [
        step_artifact.StructuredOutputRetryDiagnostic(
          attempt: 1,
          status: "error",
          failure_code: Some("structured_output_invalid_json"),
          message: "initial malformed JSON",
        ),
        step_artifact.StructuredOutputRetryDiagnostic(
          attempt: 2,
          status: "error",
          failure_code: Some("structured_output_invalid_json"),
          message: "retry malformed JSON",
        ),
      ],
    )
  let artifact =
    step_artifact.with_structured_output_retry_info(artifact, retry)
  let assert Ok(decoded) =
    step_artifact.decode_string(step_artifact.to_string(artifact))
  let assert Some(step_artifact.StructuredOutputError(_, _, _, _, Some(retry))) =
    decoded.structured_output
  assert retry.outcome == "failed"
  assert retry.attempts == 2
  let locals =
    step_artifact.to_template_locals(
      dict.from_list([#("review_json", decoded)]),
    )
  assert lookup(locals, "steps.review_json.structured_output.retry_outcome")
    == template.VString("failed")
  assert lookup(locals, "steps.review_json.structured_output.retry_attempts")
    == template.VInt(2)
}

pub fn step_without_structured_output_exposes_not_configured_status_test() {
  let artifact =
    step_artifact.from_agent_success(
      "review_json",
      agent_success("plain text"),
      [],
      limits(),
    )
  let assert Ok(decoded) =
    step_artifact.decode_string(step_artifact.to_string(artifact))
  assert decoded.structured_output == None
  let locals =
    step_artifact.to_template_locals(
      dict.from_list([#("review_json", artifact)]),
    )
  assert lookup(locals, "steps.review_json.structured_output.status")
    == template.VString("not_configured")
  assert lookup(locals, "steps.review_json.structured_output.path")
    == template.VNil
  assert lookup(locals, "steps.review_json.structured_output.ref")
    == template.VNil
}

pub fn command_success_artifact_exposes_exit_and_stdout_test() {
  let artifact =
    step_artifact.from_command_result(
      "test_after_implement",
      0,
      "all passed",
      "",
      False,
      [],
      limits(),
    )
  let locals =
    step_artifact.to_template_locals(
      dict.from_list([#("test_after_implement", artifact)]),
    )
  assert lookup(locals, "steps.test_after_implement.status")
    == template.VString("success")
  assert lookup(locals, "steps.test_after_implement.exit_code")
    == template.VInt(0)
  assert lookup(locals, "steps.test_after_implement.stdout")
    == template.VString("all passed")
}

pub fn prepare_artifacts_expose_source_preparation_alias_test() {
  assert source_preparation_stdout_for("prepare_plan", "PLAN=ok")
    == template.VString("PLAN=ok")
  assert source_preparation_stdout_for("prepare_context", "ISSUE=ok")
    == template.VString("ISSUE=ok")
}

fn source_preparation_stdout_for(
  step_id: String,
  stdout: String,
) -> template.Value {
  let artifact =
    step_artifact.from_command_result(
      step_id,
      0,
      stdout,
      "",
      False,
      [],
      limits(),
    )
  let locals =
    step_artifact.to_template_locals(dict.from_list([#(step_id, artifact)]))
  lookup(locals, "steps.source_preparation.stdout")
}

pub fn command_failure_and_timeout_are_artifacts_test() {
  let artifact =
    step_artifact.from_command_result(
      "test_after_implement",
      124,
      "",
      "timed out",
      True,
      [],
      limits(),
    )
  assert artifact.status == step_artifact.StepFailed
  assert step_artifact.status_to_string(artifact.status) == "failure"
  assert artifact.exit_code == Some(124)
  assert artifact.timed_out == True
}

pub fn truncates_command_streams_and_sets_flags_test() {
  let artifact =
    step_artifact.from_command_result(
      "long_command",
      0,
      "12345678901234567890",
      "abcdefghijklmnop",
      False,
      [],
      limits(),
    )
  assert artifact.stdout == "123456789012..."
  assert artifact.stderr == "abcdefghijkl..."
  assert artifact.stdout_truncated == True
  assert artifact.stderr_truncated == True
}

pub fn redacts_fake_secret_before_exposing_artifacts_test() {
  let artifact =
    step_artifact.from_command_result(
      "secret_command",
      0,
      "token test-key",
      "stderr test-key",
      False,
      ["test-key"],
      limits(),
    )
  assert artifact.stdout == "token [REDAC..."
  assert artifact.stderr == "stderr [REDA..."
}

pub fn workflow_result_uses_terminal_step_and_summary_test() {
  let dag =
    workflow_dag.WorkflowDag(
      id: "implementation",
      description: None,
      workspace_profile: None,
      workspace_capabilities: [],
      max_parallel_steps: 2,
      steps: [
        workflow_dag.WorkflowStep(
          id: "implement",
          kind: workflow_dag.AgentStep(
            workflow_dag.PromptInline("implement"),
            None,
          ),
          depends_on: [],
          workspace: workflow_dag.WorkspaceRef(name: "main", from: None),
          on_failure: workflow_dag.FailWorkflow,
          model_settings: model_config.default_settings(),
        ),
        workflow_dag.WorkflowStep(
          id: "final_test",
          kind: workflow_dag.CommandStep(run: "gleam test", timeout_ms: None),
          depends_on: ["implement"],
          workspace: workflow_dag.WorkspaceRef(name: "main", from: None),
          on_failure: workflow_dag.FailWorkflow,
          model_settings: model_config.default_settings(),
        ),
      ],
    )
  let implement_artifact =
    step_artifact.from_agent_success(
      "implement",
      agent_success("implemented"),
      [],
      limits(),
    )
  let final =
    step_artifact.from_command_result(
      "final_test",
      0,
      "all passed",
      "",
      False,
      [],
      limits(),
    )
  let result =
    step_artifact.workflow_result_artifact(
      dag,
      dict.from_list([
        #("implement", implement_artifact),
        #("final_test", final),
      ]),
      limits(),
    )
  let assert Some(text) = result.final_response
  assert text
    == "all passed\n\nWorkflow step summary:\nimplement success agent\nfinal_test success command exit_code=0"
  assert result.source == "workflow_dag"
}
