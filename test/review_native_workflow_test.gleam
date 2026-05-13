import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/result_artifact
import scherzo/structured_output
import scherzo/structured_output_source
import scherzo/workflow_dag
import simplifile

const submit_review_lane_draft_tool = "submit_review_lane_draft"

fn review_native_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(contents) =
    simplifile.read(".scherzo/workflows/review-native.yml")
  let assert Ok(dag) = workflow_dag.parse(contents)
  dag
}

fn implementation_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(contents) =
    simplifile.read(".scherzo/workflows/implementation.yaml")
  let assert Ok(dag) = workflow_dag.parse(contents)
  dag
}

fn lane_spec(
  dag: workflow_dag.WorkflowDag,
  step_id: String,
) -> workflow_dag.StructuredOutputSpec {
  let assert Ok(step) = workflow_dag.step_by_id(dag, step_id)
  let assert workflow_dag.AgentStep(_, Some(spec)) = step.kind
  spec
}

fn assert_review_tool_source(spec: workflow_dag.StructuredOutputSpec) -> Nil {
  assert spec.source
    == structured_output_source.PiToolCallSource(
      tool_name: submit_review_lane_draft_tool,
      require_single: True,
      reject_sibling_tool_calls: True,
    )
}

fn assert_lane_workspace_from_main(
  dag: workflow_dag.WorkflowDag,
  step_id: String,
  workspace_name: String,
) -> Nil {
  let assert Ok(step) = workflow_dag.step_by_id(dag, step_id)
  assert step.workspace
    == workflow_dag.WorkspaceRef(name: workspace_name, from: Some("main"))
}

fn assert_native_review_lane_workspaces_are_isolated(
  dag: workflow_dag.WorkflowDag,
) -> Nil {
  assert_lane_workspace_from_main(dag, "lane_correctness", "review-correctness")
  assert_lane_workspace_from_main(
    dag,
    "lane_test_quality",
    "review-test-quality",
  )
  assert_lane_workspace_from_main(
    dag,
    "lane_idioms_maintainability",
    "review-idioms-maintainability",
  )
  assert_lane_workspace_from_main(
    dag,
    "lane_security_performance",
    "review-security-performance",
  )
}

fn assert_contains(contents: String, expected: String) -> Nil {
  case string.contains(contents, expected) {
    True -> Nil
    False -> {
      let message = "expected text not found: " <> expected
      panic as message
    }
  }
}

fn assert_not_contains(contents: String, unexpected: String) -> Nil {
  case string.contains(contents, unexpected) {
    False -> Nil
    True -> {
      let message = "unexpected text still present: " <> unexpected
      panic as message
    }
  }
}

fn assert_list_contains(values: List(String), expected: String) -> Nil {
  case list.contains(values, expected) {
    True -> Nil
    False -> {
      let message = "expected list item not found: " <> expected
      panic as message
    }
  }
}

fn valid_review_lane_draft_json() -> String {
  "{\"schema_version\":1,\"artifact_type\":\"review_lane_draft\",\"generated_at_utc\":\"2026-05-11T00:00:00Z\",\"producer\":{\"name\":\"review-native-workflow-test\",\"version\":\"1\",\"mode\":\"native\"},\"lane\":{\"id\":\"correctness\",\"name\":\"Correctness reviewer\",\"category\":\"correctness\",\"version\":\"1\"},\"input_refs\":[],\"draft_findings\":[],\"review_notes\":[],\"evidence_requests\":[],\"self_check\":{\"inspected_diff\":true,\"used_repository_relative_paths\":true},\"remote_mutations\":\"none\"}"
}

fn validate_result(
  spec: workflow_dag.StructuredOutputSpec,
  result: result_artifact.ResultArtifact,
) -> Result(
  structured_output.StructuredOutputValidation,
  structured_output.StructuredOutputError,
) {
  structured_output.validate_agent_result(
    spec,
    result,
    [],
    structured_output.default_validator_runner(
      structured_output.default_validator_context(
        ".scherzo",
        ".",
        "review_native",
        "test_run",
        "lane_correctness",
        1,
        ".",
        spec.artifact_name,
        "json",
        spec.source,
      ),
      [],
    ),
  )
}

pub fn review_native_lane_steps_use_submit_review_lane_draft_tool_source_test() {
  let dag = review_native_dag()

  assert_review_tool_source(lane_spec(dag, "lane_correctness"))
  assert_review_tool_source(lane_spec(dag, "lane_test_quality"))
  assert_review_tool_source(lane_spec(dag, "lane_idioms_maintainability"))
  assert_review_tool_source(lane_spec(dag, "lane_security_performance"))
}

pub fn review_native_lane_steps_use_isolated_derived_workspaces_test() {
  assert_native_review_lane_workspaces_are_isolated(review_native_dag())
}

pub fn review_lane_draft_tool_is_enabled_for_implementation_lane_steps_test() {
  let assert Ok(extension) =
    simplifile.read(".pi/extensions/scherzo-review-lane-draft/index.ts")

  assert_contains(extension, "\"implementation\"")
  assert_contains(extension, "SCHERZO_STEP_ID")
  assert_contains(extension, "lane_correctness")
  assert_contains(extension, "review-native-contract-spike")
}

pub fn implementation_workflow_uses_native_agent_lane_steps_test() {
  let dag = implementation_dag()

  assert_review_tool_source(lane_spec(dag, "lane_correctness"))
  assert_review_tool_source(lane_spec(dag, "lane_test_quality"))
  assert_review_tool_source(lane_spec(dag, "lane_idioms_maintainability"))
  assert_review_tool_source(lane_spec(dag, "lane_security_performance"))
  assert_native_review_lane_workspaces_are_isolated(dag)

  let assert Ok(cutover_step) =
    workflow_dag.step_by_id(dag, "assert_native_review_cutover")
  let assert workflow_dag.CommandStep(cutover_run, _) = cutover_step.kind
  assert_contains(cutover_run, "refuses fixture/scenario/heuristic")
  assert_contains(cutover_run, "SCHERZO_STAGED_REVIEW_AGENT_BACKEND")

  let assert Ok(prepare_step) = workflow_dag.step_by_id(dag, "prepare_review")
  let assert workflow_dag.CommandStep(prepare_run, _) = prepare_step.kind
  assert_contains(prepare_run, "prepare-native")
  assert_not_contains(prepare_run, "--native-review-scenario")
  assert_not_contains(prepare_run, "--agent-backend")

  let assert Ok(mutation_check) =
    workflow_dag.step_by_id(dag, "assert_clean_after_lanes")
  assert mutation_check.on_failure == workflow_dag.FailWorkflow

  let assert Ok(validate_step) =
    workflow_dag.step_by_id(dag, "validate_native_review_artifacts")
  let assert workflow_dag.CommandStep(validate_run, _) = validate_step.kind
  assert_contains(
    validate_run,
    "native review infrastructure issue blocks publication",
  )
  assert_contains(validate_run, "lane_failed")
  assert_contains(validate_run, "execution_issues")

  let assert Ok(code_review) = workflow_dag.step_by_id(dag, "code_review")
  assert_list_contains(
    code_review.depends_on,
    "validate_native_review_artifacts",
  )
}

pub fn review_native_rejects_final_response_only_and_accepts_tool_submission_test() {
  let spec = lane_spec(review_native_dag(), "lane_correctness")
  let final_response_only =
    result_artifact.from_final_response_with_tool_calls(
      Some(valid_review_lane_draft_json()),
      False,
      "review_native_workflow_test",
      [],
    )

  let assert Error(missing_tool_call) =
    validate_result(spec, final_response_only)
  assert structured_output.error_code(missing_tool_call)
    == "structured_output_tool_call_missing"
  assert_contains(
    structured_output.error_message(missing_tool_call),
    submit_review_lane_draft_tool,
  )

  let tool_call_result =
    result_artifact.from_final_response_with_tool_calls(
      None,
      False,
      "review_native_workflow_test",
      [
        result_artifact.ToolCallSubmission(
          name: submit_review_lane_draft_tool,
          arguments_json: Some(valid_review_lane_draft_json()),
          status: Some("success"),
          sibling_count: 1,
        ),
      ],
    )

  let assert Ok(structured_output.StructuredOutputPresent(payload)) =
    validate_result(spec, tool_call_result)
  assert_contains(payload, "review_lane_draft")
}
