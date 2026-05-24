import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/result_artifact
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

pub fn parses_retry_requested_decision_test() {
  let artifact =
    result_artifact.from_final_response_with_tool_calls(None, False, "test", [
      recovery_call(Some(
        "{\"schema_version\":1,\"artifact_type\":\"workflow_step_recovery_result\",\"decision\":\"retry_requested\",\"summary\":\"Fixed tests\",\"reason\":\"Ready for retry\"}",
      )),
    ])

  let assert Ok(workflow_step_recovery.RetryRequested(
    summary: "Fixed tests",
    reason: "Ready for retry",
  )) = workflow_step_recovery.decision_from_result(artifact)
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
      "retry_requested",
      "patched TOP_SECRET",
      "ready TOP_SECRET",
      ["TOP_SECRET"],
    )

  assert string.contains(payload, "\"decision\":\"retry_requested\"")
  assert !string.contains(payload, "\"result\"")
  assert !string.contains(payload, "TOP_SECRET")
  assert string.contains(payload, "patched [REDACTED]")
  assert string.contains(payload, "ready [REDACTED]")
}
