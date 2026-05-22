import gleam/option.{type Option, None, Some}
import scherzo/result_artifact
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
