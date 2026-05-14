import gleam/option.{None, Some}
import gleam/string
import scherzo/json_value
import scherzo/structured_output_source
import scherzo/structured_output_tool_spec
import scherzo/workflow_dag
import simplifile

fn spec(schema_path: String) -> workflow_dag.StructuredOutputSpec {
  workflow_dag.StructuredOutputSpec(
    artifact_name: "review_lane_draft",
    required: True,
    source: structured_output_source.PiToolCallSource(
      tool_name: "submit_structured_output",
      require_single: True,
      reject_sibling_tool_calls: True,
      parameters_schema_path: Some(schema_path),
    ),
    format: workflow_dag.StructuredJson,
    schema: workflow_dag.StructuredObjectSchema([
      "schema_version",
      "artifact_type",
    ]),
    validators: [
      workflow_dag.JsonSchemaValidator(
        name: "review_lane_draft_schema",
        path: schema_path,
        draft: Some("2020-12"),
      ),
    ],
    validation_retries: 1,
  )
}

fn build(
  schema_path: String,
) -> Result(
  structured_output_tool_spec.ToolSpec,
  structured_output_tool_spec.ToolSpecError,
) {
  structured_output_tool_spec.for_step(structured_output_tool_spec.BuildInput(
    workflow_id: "review-native",
    run_id: "run-1",
    step_id: "lane_correctness",
    attempt_index: 2,
    repository_root: ".",
    spec: spec(schema_path),
  ))
}

fn assert_error_code(
  result: Result(a, structured_output_tool_spec.ToolSpecError),
  code: String,
) -> Nil {
  let assert Error(error) = result
  assert error.code == code
}

pub fn structured_output_tool_spec_builds_raw_schema_spec_test() {
  let assert Ok(tool_spec) =
    build("docs/schemas/review-lane-draft.correctness.v1.schema.json")

  assert tool_spec.workflow_id == "review-native"
  assert tool_spec.run_id == "run-1"
  assert tool_spec.step_id == "lane_correctness"
  assert tool_spec.attempt_index == 2
  assert tool_spec.artifact_name == "review_lane_draft"
  assert tool_spec.tool_name == "submit_structured_output"
  assert tool_spec.parameters_schema_path
    == "docs/schemas/review-lane-draft.correctness.v1.schema.json"
  assert string.length(tool_spec.parameters_schema_sha256) == 64
  let assert json_value.JObject(_) = tool_spec.parameters_schema
  assert tool_spec.require_single
  assert tool_spec.reject_sibling_tool_calls
  assert tool_spec.terminate

  let encoded = structured_output_tool_spec.to_string(tool_spec)
  let assert Ok(_) = json_value.parse(encoded)
  assert string.contains(encoded, "\"parameters_schema\":")
  assert string.contains(encoded, "\"terminate\":true")
}

pub fn structured_output_tool_spec_rejects_unsafe_schema_paths_test() {
  assert_error_code(
    structured_output_tool_spec.validate_schema_path("/tmp/schema.json"),
    "structured_output_parameters_schema_path_invalid",
  )
  assert_error_code(
    structured_output_tool_spec.validate_schema_path("../schema.json"),
    "structured_output_parameters_schema_path_invalid",
  )
  assert_error_code(
    structured_output_tool_spec.validate_schema_path("$SCHEMA/schema.json"),
    "structured_output_parameters_schema_path_invalid",
  )
  assert_error_code(
    structured_output_tool_spec.validate_schema_path("C:/schema.json"),
    "structured_output_parameters_schema_path_invalid",
  )
  assert_error_code(
    structured_output_tool_spec.validate_schema_path(
      "<absolute-local-path>/schema.json",
    ),
    "structured_output_parameters_schema_path_invalid",
  )
}

pub fn structured_output_tool_spec_reports_malformed_schema_json_test() {
  let dir = "test/tmp/structured-output-tool-spec"
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  let schema_path = dir <> "/malformed.schema.json"
  let assert Ok(Nil) = simplifile.write(schema_path, "{ not json\n")

  assert_error_code(
    build(schema_path),
    "structured_output_tool_spec_schema_malformed_json",
  )
}

pub fn structured_output_tool_spec_writes_retained_path_test() {
  let run_root = "test/tmp/structured-output-tool-spec-write/run-root"
  let _ = simplifile.delete(run_root)
  let assert Ok(tool_spec) =
    build("docs/schemas/review-lane-draft.correctness.v1.schema.json")
  let assert Ok(written) =
    structured_output_tool_spec.write(tool_spec, run_root)

  assert written.run_root_relative_path
    == "artifacts/structured-output-specs/lane_correctness-attempt-2.json"
  assert string.contains(
    written.env_path,
    "artifacts/structured-output-specs/lane_correctness-attempt-2.json",
  )
  let assert Ok(contents) = simplifile.read(written.env_path)
  assert string.contains(contents, "submit_structured_output")
  assert structured_output_tool_spec.env_pair(written).0
    == structured_output_tool_spec.spec_env_var
}

pub fn structured_output_tool_spec_requires_generic_schema_path_test() {
  let legacy_spec =
    workflow_dag.StructuredOutputSpec(
      artifact_name: "review_lane_draft",
      required: True,
      source: structured_output_source.PiToolCallSource(
        tool_name: "submit_structured_output",
        require_single: True,
        reject_sibling_tool_calls: True,
        parameters_schema_path: None,
      ),
      format: workflow_dag.StructuredJson,
      schema: workflow_dag.StructuredObjectSchema([]),
      validators: [],
      validation_retries: 1,
    )
  assert_error_code(
    structured_output_tool_spec.for_step(structured_output_tool_spec.BuildInput(
      workflow_id: "review-native",
      run_id: "run-1",
      step_id: "lane_correctness",
      attempt_index: 1,
      repository_root: ".",
      spec: legacy_spec,
    )),
    "structured_output_tool_spec_missing_schema_path",
  )
}
