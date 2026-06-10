import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/result_artifact
import scherzo/step_artifact
import scherzo/structured_output_tool_spec
import scherzo/workflow_step_recovery

fn recovery_call(
  arguments_json: Option(String),
) -> result_artifact.ToolCallSubmission {
  result_artifact.ToolCallSubmission(
    name: workflow_step_recovery.tool_name,
    arguments_json: arguments_json,
    status: None,
    sibling_count: 1,
    receipt_json: None,
  )
}

pub fn tool_spec_builds_provider_compatible_recovery_tool_test() {
  let assert Ok(tool_spec) =
    workflow_step_recovery.tool_spec(
      "implementation",
      "run-1",
      "repair",
      1,
      ".",
    )

  assert tool_spec.tool_name == workflow_step_recovery.tool_name
  let encoded = structured_output_tool_spec.to_string(tool_spec)
  assert string.contains(
    encoded,
    "\"tool_name\":\"submit_workflow_step_recovery_result\"",
  )
  assert string.contains(encoded, "\"parameters_schema\":")
  assert !string.contains(encoded, "\"enum\"")
}

pub fn parses_recheck_decision_test() {
  let artifact =
    result_artifact.from_final_response_with_tool_calls(None, False, "test", [
      recovery_call(Some(
        "{\"schema_version\":1,\"artifact_type\":\"workflow_step_recovery_result\",\"decision\":\"recheck\",\"summary\":\"Fixed tests\",\"reason\":\"Ready for recheck\"}",
      )),
    ])

  let assert Ok(workflow_step_recovery.Recheck(
    summary: "Fixed tests",
    reason: "Ready for recheck",
  )) = workflow_step_recovery.decision_from_result(artifact)
}

pub fn recovery_input_json_includes_context_policy_and_redacts_test() {
  let failed_artifact =
    step_artifact.from_command_result_with_metadata(
      "verify",
      Some("gleam test"),
      1,
      None,
      Some(".scherzo/command-step-diagnostics/verify.txt"),
      "stdout",
      "validation failed TOP_SECRET",
      False,
      ["TOP_SECRET"],
      limits(),
      False,
      False,
    )

  let payload =
    workflow_step_recovery.recovery_input_json(
      "implementation",
      "run-1",
      "verify",
      1,
      failed_artifact,
      ["TOP_SECRET"],
    )

  assert string.contains(
    payload,
    "\"schema_version\":\"scherzo.workflow_recovery_input.v1\"",
  )
  assert string.contains(payload, "\"workflow_id\":\"implementation\"")
  assert string.contains(payload, "\"run_id\":\"run-1\"")
  assert string.contains(payload, "\"step_id\":\"verify\"")
  assert string.contains(payload, "\"attempt_index\":1")
  assert string.contains(
    payload,
    "\"diagnostic_refs\":[\".scherzo/command-step-diagnostics/verify.txt\"]",
  )
  assert string.contains(payload, "\"allowed_actions\"")
  assert string.contains(payload, "\"forbidden_actions\"")
  assert !string.contains(payload, "TOP_SECRET")
}

pub fn recovery_input_json_hides_absolute_diagnostic_refs_test() {
  let absolute_diagnostic_path = "/tmp/scherzo-secret-diagnostics/verify.txt"
  let failed_artifact =
    step_artifact.from_command_result_with_metadata(
      "verify",
      Some("gleam test"),
      1,
      None,
      Some(absolute_diagnostic_path),
      "stdout",
      "validation failed",
      False,
      [],
      limits(),
      False,
      False,
    )

  let payload =
    workflow_step_recovery.recovery_input_json(
      "implementation",
      "run-1",
      "verify",
      1,
      failed_artifact,
      [],
    )
  let prompt =
    workflow_step_recovery.prompt(
      "repair",
      workflow_step_recovery.RecoveryInputArtifact(
        ref: "runs/run-1/verify/attempt-1/recovery-1/workflow_step_recovery_input.json",
        payload_json: payload,
      ),
    )

  assert !string.contains(payload, absolute_diagnostic_path)
  assert !string.contains(prompt, absolute_diagnostic_path)
  assert string.contains(
    payload,
    "\"diagnostic_refs\":[\"<absolute path hidden>\"]",
  )
}

pub fn recovery_input_json_includes_structured_output_refs_and_reason_code_test() {
  let metadata =
    step_artifact.StructuredOutputMetadata(
      artifact_name: "plan_completion_verdict",
      format: "json",
      ref: "runs/run-1/verify/attempt-1/plan_completion_verdict.json",
      path: "artifacts/runs/run-1/verify/attempt-1/plan_completion_verdict.json",
      uri: "file://artifacts/runs/run-1/verify/attempt-1/plan_completion_verdict.json",
      display_path: "runs/run-1/verify/attempt-1/plan_completion_verdict.json",
      local_path: Some("test/tmp/plan_completion_verdict.json"),
      sha256: "abc123",
      bytes: 42,
      schema_status: "valid",
      source_type: "pi_tool_call",
      source_tool_name: Some("submit_plan_completion_verdict"),
      source_parameters_schema_path: None,
      source_parameters_schema_sha256: None,
      source_receipt_json: None,
      baseline_required_keys: [],
      validators: [],
      retry: None,
    )
  let failed_artifact =
    step_artifact.StepArtifact(
      step_id: "verify",
      status: step_artifact.StepFailed,
      final_response: None,
      exit_code: None,
      command: None,
      duration_ms: None,
      diagnostic_path: None,
      failure_code: Some("plan_completion_incomplete"),
      stdout: "",
      stderr: "",
      timed_out: False,
      final_response_truncated: False,
      stdout_truncated: False,
      stderr_truncated: False,
      summary_text: "Plan completion verifier reported incomplete implementation.",
      structured_output: Some(step_artifact.StructuredOutputValid(metadata)),
    )

  let payload =
    workflow_step_recovery.recovery_input_json(
      "implementation",
      "run-1",
      "verify",
      1,
      failed_artifact,
      [],
    )

  assert string.contains(
    payload,
    "\"structured_output_refs\":[{\"artifact_name\":\"plan_completion_verdict\",\"ref\":\"runs/run-1/verify/attempt-1/plan_completion_verdict.json\"}]",
  )
  assert string.contains(
    payload,
    "\"reason_code\":\"plan_completion_incomplete\"",
  )
}

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 200,
    template_field_max_chars: 200,
    workflow_summary_max_chars: 200,
  )
}

pub fn rejects_missing_recovery_result_test() {
  let artifact = result_artifact.from_final_response(None, False, "test")

  let assert Error(error) =
    workflow_step_recovery.decision_from_result(artifact)
  assert workflow_step_recovery.describe_error(error)
    == "recovery_result_missing"
}

pub fn rejects_duplicate_recovery_results_test() {
  let call =
    recovery_call(Some(
      "{\"schema_version\":1,\"artifact_type\":\"workflow_step_recovery_result\",\"decision\":\"gave_up\",\"summary\":\"No fix\",\"reason\":\"Needs a human\"}",
    ))
  let artifact =
    result_artifact.from_final_response_with_tool_calls(None, False, "test", [
      call,
      call,
    ])

  let assert Error(error) =
    workflow_step_recovery.decision_from_result(artifact)
  assert workflow_step_recovery.describe_error(error)
    == "recovery_result_duplicate"
}

pub fn rejects_sibling_tool_calls_test() {
  let artifact =
    result_artifact.from_final_response_with_tool_calls(None, False, "test", [
      result_artifact.ToolCallSubmission(
        ..recovery_call(Some(
          "{\"schema_version\":1,\"artifact_type\":\"workflow_step_recovery_result\",\"decision\":\"gave_up\",\"summary\":\"No fix\",\"reason\":\"Needs a human\"}",
        )),
        sibling_count: 2,
      ),
    ])

  let assert Error(error) =
    workflow_step_recovery.decision_from_result(artifact)
  assert workflow_step_recovery.describe_error(error)
    == "recovery_result_has_sibling_tool_calls"
}

pub fn rejects_missing_arguments_test() {
  let artifact =
    result_artifact.from_final_response_with_tool_calls(None, False, "test", [
      recovery_call(None),
    ])

  let assert Error(error) =
    workflow_step_recovery.decision_from_result(artifact)
  assert workflow_step_recovery.describe_error(error)
    == "recovery_result_missing_arguments"
}

pub fn rejects_malformed_json_test() {
  let artifact =
    result_artifact.from_final_response_with_tool_calls(None, False, "test", [
      recovery_call(Some("{")),
    ])

  let assert Error(error) =
    workflow_step_recovery.decision_from_result(artifact)
  assert workflow_step_recovery.describe_error(error)
    == "recovery_result_malformed"
}

pub fn rejects_wrong_artifact_type_test() {
  let artifact =
    result_artifact.from_final_response_with_tool_calls(None, False, "test", [
      recovery_call(Some(
        "{\"schema_version\":1,\"artifact_type\":\"wrong\",\"decision\":\"gave_up\",\"summary\":\"No fix\",\"reason\":\"Needs a human\"}",
      )),
    ])

  let assert Error(error) =
    workflow_step_recovery.decision_from_result(artifact)
  assert workflow_step_recovery.describe_error(error)
    == "recovery_result_wrong_artifact_type"
}

pub fn rejects_wrong_schema_version_test() {
  let artifact =
    result_artifact.from_final_response_with_tool_calls(None, False, "test", [
      recovery_call(Some(
        "{\"schema_version\":2,\"artifact_type\":\"workflow_step_recovery_result\",\"decision\":\"gave_up\",\"summary\":\"No fix\",\"reason\":\"Needs a human\"}",
      )),
    ])

  let assert Error(error) =
    workflow_step_recovery.decision_from_result(artifact)
  assert workflow_step_recovery.describe_error(error)
    == "recovery_result_wrong_schema_version"
}

pub fn parses_gave_up_decision_test() {
  let artifact =
    result_artifact.from_final_response_with_tool_calls(None, False, "test", [
      recovery_call(Some(
        "{\"schema_version\":1,\"artifact_type\":\"workflow_step_recovery_result\",\"decision\":\"gave_up\",\"summary\":\"No fix\",\"reason\":\"Needs a human\"}",
      )),
    ])

  let assert Ok(workflow_step_recovery.GaveUp(
    summary: "No fix",
    reason: "Needs a human",
  )) = workflow_step_recovery.decision_from_result(artifact)
}

pub fn artifact_json_uses_decision_field_and_redacts_test() {
  let payload =
    workflow_step_recovery.artifact_json(
      "recheck",
      "patched TOP_SECRET",
      "ready TOP_SECRET",
      ["TOP_SECRET"],
    )

  assert string.contains(payload, "\"decision\":\"recheck\"")
  assert !string.contains(payload, "\"result\"")
  assert !string.contains(payload, "TOP_SECRET")
  assert string.contains(payload, "patched [REDACTED]")
  assert string.contains(payload, "ready [REDACTED]")
}
