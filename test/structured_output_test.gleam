import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/json_value
import scherzo/result_artifact
import scherzo/structured_output
import scherzo/structured_output_source
import scherzo/structured_output_validator
import scherzo/workflow_dag
import simplifile

fn spec(required: Bool) -> workflow_dag.StructuredOutputSpec {
  workflow_dag.StructuredOutputSpec(
    artifact_name: "review_result",
    required: required,
    source: structured_output_source.FinalResponseSource,
    format: workflow_dag.StructuredJson,
    schema: workflow_dag.StructuredObjectSchema(["summary", "findings"]),
    validators: [],
    validation_retries: 1,
  )
}

fn json_schema_review_spec(
  source: structured_output_source.StructuredOutputSource,
  required: Bool,
  schema_path: String,
) -> workflow_dag.StructuredOutputSpec {
  workflow_dag.StructuredOutputSpec(
    artifact_name: "review_lane_draft",
    required: required,
    source: source,
    format: workflow_dag.StructuredJson,
    schema: workflow_dag.StructuredObjectSchema([
      "schema_version",
      "artifact_type",
    ]),
    validators: [
      workflow_dag.JsonSchemaValidator(
        name: "review_lane_shape",
        path: schema_path,
        draft: Some("2020-12"),
      ),
    ],
    validation_retries: 1,
  )
}

fn tool_source_spec() -> workflow_dag.StructuredOutputSpec {
  workflow_dag.StructuredOutputSpec(
    artifact_name: "example_artifact",
    required: True,
    source: structured_output_source.PiToolCallSource(
      tool_name: "submit_example_artifact",
      parameters_schema_path: None,
    ),
    format: workflow_dag.StructuredJson,
    schema: workflow_dag.StructuredObjectSchema([
      "schema_version",
      "artifact_type",
    ]),
    validators: [],
    validation_retries: 1,
  )
}

fn review_lane_draft_spec() -> workflow_dag.StructuredOutputSpec {
  workflow_dag.StructuredOutputSpec(
    artifact_name: "review_lane_draft",
    required: True,
    source: structured_output_source.FinalResponseSource,
    format: workflow_dag.StructuredJson,
    schema: workflow_dag.StructuredObjectSchema([
      "schema_version",
      "artifact_type",
      "generated_at_utc",
      "producer",
      "lane",
      "input_refs",
      "draft_findings",
      "review_notes",
      "evidence_requests",
      "self_check",
      "remote_mutations",
    ]),
    validators: [
      workflow_dag.CommandValidator(
        name: "review_lane_draft_compat",
        argv: [
          "python3",
          ".scherzo/workflows/scripts/scherzo-review",
          "validate-structured-output",
          "--validator",
          "review_lane_draft",
        ],
        timeout_ms: 30_000,
        working_directory: workflow_dag.ValidatorInRepository,
        env: [],
      ),
    ],
    validation_retries: 1,
  )
}

pub fn validator_repo_root_prefers_step_workspace_test() {
  assert structured_output.validator_repo_root(
      "/daemon/repo/.scherzo",
      "/run/workspaces/main",
    )
    == "/run/workspaces/main"
}

pub fn validator_repo_root_treats_dot_workspace_as_missing_test() {
  assert structured_output.validator_repo_root("/daemon/repo/.scherzo", ".")
    == "/daemon/repo"
}

pub fn default_validator_context_uses_schema_repository_root_test() {
  let context =
    structured_output.default_validator_context(
      "/daemon/repo/.scherzo",
      "/run/root",
      "test_workflow",
      "/daemon/repo/.scherzo/workflows",
      "test_run",
      "review_json",
      1,
      "/run/workspaces/main",
      "review_lane_draft",
      "json",
      structured_output_source.FinalResponseSource,
    )

  assert context.repository_root == "/daemon/repo"
  assert context.workspace_path == "/run/workspaces/main"
}

fn validate(
  required: Bool,
  response: Option(String),
  truncated: Bool,
) -> Result(
  structured_output.StructuredOutputValidation,
  structured_output.StructuredOutputError,
) {
  structured_output.validate_final_response(
    spec(required),
    response,
    truncated,
    [],
    structured_output.noop_validator_runner,
  )
}

fn validate_tool_source(
  tool_calls: List(result_artifact.ToolCallSubmission),
) -> Result(
  structured_output.StructuredOutputValidation,
  structured_output.StructuredOutputError,
) {
  structured_output.validate_agent_result(
    tool_source_spec(),
    result_artifact.from_final_response_with_tool_calls(
      Some(
        "{\"schema_version\":1,\"artifact_type\":\"final_response_not_source\"}",
      ),
      False,
      "test",
      tool_calls,
    ),
    [],
    structured_output.noop_validator_runner,
  )
}

fn fixture_text(path: String) -> String {
  let assert Ok(contents) = simplifile.read(path)
  contents
}

fn json_schema_runner(
  source: structured_output_source.StructuredOutputSource,
) -> structured_output.ValidatorRunner {
  structured_output.default_validator_runner(
    structured_output.default_validator_context(
      ".scherzo",
      ".",
      "test_workflow",
      ".scherzo/workflows",
      "test_run",
      "review_json",
      1,
      ".",
      "review_lane_draft",
      "json",
      source,
    ),
    [],
  )
}

fn validate_json_schema_result(
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
    json_schema_runner(spec.source),
  )
}

fn tool_call(
  name: String,
  arguments_json: Option(String),
  status: Option(String),
  sibling_count: Int,
) -> result_artifact.ToolCallSubmission {
  result_artifact.ToolCallSubmission(
    name: name,
    arguments_json: arguments_json,
    status: status,
    sibling_count: sibling_count,
    receipt_json: None,
  )
}

pub fn required_structured_output_accepts_valid_json_test() {
  let assert Ok(structured_output.StructuredOutputPresent(payload)) =
    validate(True, Some("{\"summary\":\"ok\",\"findings\":[]}"), False)
  let assert Ok(json_value.JObject(entries)) = json_value.parse(payload)
  assert json_value.object_has_key(entries, "summary")
  assert json_value.object_has_key(entries, "findings")
}

pub fn required_structured_output_rejects_missing_invalid_truncated_and_schema_test() {
  let assert Error(invalid_json) = validate(True, Some("not json"), False)
  assert structured_output.error_code(invalid_json)
    == "structured_output_invalid_json"

  let assert Error(missing_none) = validate(True, None, False)
  assert structured_output.error_code(missing_none)
    == "structured_output_missing"

  let assert Error(missing_blank) = validate(True, Some("   "), False)
  assert structured_output.error_code(missing_blank)
    == "structured_output_missing"

  let assert Error(truncated) =
    validate(True, Some("{\"summary\":\"ok\",\"findings\":[]}"), True)
  assert structured_output.error_code(truncated)
    == "structured_output_truncated"

  let assert Error(not_object) = validate(True, Some("[]"), False)
  assert structured_output.error_code(not_object)
    == "structured_output_schema_invalid"

  let assert Error(missing_key) =
    validate(True, Some("{\"summary\":\"ok\"}"), False)
  assert structured_output.error_code(missing_key)
    == "structured_output_schema_invalid"
  assert string.contains(
    structured_output.error_message(missing_key),
    "findings",
  )
}

pub fn optional_structured_output_absence_succeeds_but_invalid_present_fails_test() {
  assert validate(False, None, False)
    == Ok(structured_output.StructuredOutputAbsent)
  assert validate(False, Some("   "), False)
    == Ok(structured_output.StructuredOutputAbsent)

  let assert Ok(structured_output.StructuredOutputPresent(_)) =
    validate(False, Some("{\"summary\":\"ok\",\"findings\":[]}"), False)

  let assert Error(invalid_json) = validate(False, Some("not json"), False)
  assert structured_output.error_code(invalid_json)
    == "structured_output_invalid_json"

  let assert Error(truncated) = validate(False, None, True)
  assert structured_output.error_code(truncated)
    == "structured_output_truncated"
}

pub fn pi_tool_call_source_accepts_matching_successful_object_arguments_test() {
  let assert Ok(structured_output.StructuredOutputPresent(payload)) =
    validate_tool_source([
      tool_call(
        "submit_example_artifact",
        Some("{\"schema_version\":1,\"artifact_type\":\"example\"}"),
        Some("success"),
        1,
      ),
    ])
  let assert Ok(json_value.JObject(entries)) = json_value.parse(payload)
  assert json_value.object_has_key(entries, "schema_version")
  assert json_value.object_has_key(entries, "artifact_type")
}

pub fn json_schema_validator_runs_for_final_response_source_test() {
  let source = structured_output_source.FinalResponseSource
  let spec =
    json_schema_review_spec(
      source,
      True,
      "test/fixtures/structured_output/review_lane_draft.schema.json",
    )
  let result =
    result_artifact.from_final_response(
      Some(fixture_text(
        "test/fixtures/structured_output/review_lane_payload_valid.json",
      )),
      False,
      "test",
    )
  let assert Ok(structured_output.StructuredOutputPresent(_)) =
    validate_json_schema_result(spec, result)
}

pub fn json_schema_validator_runs_for_pi_tool_call_source_test() {
  let source =
    structured_output_source.PiToolCallSource(
      tool_name: "submit_review_lane_draft",
      parameters_schema_path: None,
    )
  let spec =
    json_schema_review_spec(
      source,
      True,
      "test/fixtures/structured_output/review_lane_draft.schema.json",
    )
  let result =
    result_artifact.from_final_response_with_tool_calls(
      Some("{\"schema_version\":999,\"artifact_type\":\"ignored\"}"),
      False,
      "test",
      [
        tool_call(
          "submit_review_lane_draft",
          Some(fixture_text(
            "test/fixtures/structured_output/review_lane_payload_valid.json",
          )),
          Some("success"),
          1,
        ),
      ],
    )
  let assert Ok(structured_output.StructuredOutputPresent(_)) =
    validate_json_schema_result(spec, result)
}

pub fn json_schema_mismatch_is_retryable_validator_failure_test() {
  let source = structured_output_source.FinalResponseSource
  let spec =
    json_schema_review_spec(
      source,
      True,
      "test/fixtures/structured_output/review_lane_draft.schema.json",
    )
  let result =
    result_artifact.from_final_response(
      Some(fixture_text(
        "test/fixtures/structured_output/review_lane_payload_invalid.json",
      )),
      False,
      "test",
    )
  let assert Error(error) = validate_json_schema_result(spec, result)

  assert structured_output.error_code(error)
    == "structured_output_json_schema_rejected"
  assert structured_output.error_retryable(error)
  assert structured_output.error_validator_type(error) == Some("json_schema")
  assert structured_output.error_validator_name(error)
    == Some("review_lane_shape")
}

pub fn json_schema_missing_schema_is_non_retryable_validator_failure_test() {
  let source = structured_output_source.FinalResponseSource
  let spec =
    json_schema_review_spec(
      source,
      True,
      "test/fixtures/structured_output/missing.schema.json",
    )
  let result =
    result_artifact.from_final_response(
      Some(fixture_text(
        "test/fixtures/structured_output/review_lane_payload_valid.json",
      )),
      False,
      "test",
    )
  let assert Error(error) = validate_json_schema_result(spec, result)

  assert structured_output.error_code(error)
    == "structured_output_json_schema_config_error"
  assert !structured_output.error_retryable(error)
  assert structured_output.error_validator_type(error) == Some("json_schema")
}

pub fn optional_absent_output_does_not_run_json_schema_validator_test() {
  let source = structured_output_source.FinalResponseSource
  let spec =
    json_schema_review_spec(
      source,
      False,
      "test/fixtures/structured_output/missing.schema.json",
    )
  let result = result_artifact.from_final_response(None, False, "test")

  assert validate_json_schema_result(spec, result)
    == Ok(structured_output.StructuredOutputAbsent)
}

pub fn pi_tool_call_source_rejects_missing_wrong_failed_bad_multiple_and_sibling_test() {
  let assert Error(missing) = validate_tool_source([])
  assert structured_output.error_code(missing)
    == "structured_output_tool_call_missing"

  let assert Error(wrong_name) =
    validate_tool_source([
      tool_call(
        "submit_wrong_artifact",
        Some("{\"schema_version\":1,\"artifact_type\":\"example\"}"),
        Some("success"),
        1,
      ),
    ])
  assert structured_output.error_code(wrong_name)
    == "structured_output_tool_call_wrong_name"

  let assert Error(failed) =
    validate_tool_source([
      tool_call(
        "submit_example_artifact",
        Some("{\"schema_version\":1,\"artifact_type\":\"example\"}"),
        Some("failed"),
        1,
      ),
    ])
  assert structured_output.error_code(failed)
    == "structured_output_tool_call_failed"

  let assert Error(malformed) =
    validate_tool_source([
      tool_call(
        "submit_example_artifact",
        Some("{not json"),
        Some("success"),
        1,
      ),
    ])
  assert structured_output.error_code(malformed)
    == "structured_output_tool_call_arguments_invalid"

  let assert Error(non_object) =
    validate_tool_source([
      tool_call("submit_example_artifact", Some("[]"), Some("success"), 1),
    ])
  assert structured_output.error_code(non_object)
    == "structured_output_tool_call_arguments_invalid"

  let assert Error(multiple) =
    validate_tool_source([
      tool_call(
        "submit_example_artifact",
        Some("{\"schema_version\":1,\"artifact_type\":\"example\"}"),
        Some("success"),
        1,
      ),
      tool_call(
        "submit_example_artifact",
        Some("{\"schema_version\":1,\"artifact_type\":\"example\"}"),
        Some("success"),
        1,
      ),
    ])
  assert structured_output.error_code(multiple)
    == "structured_output_tool_call_multiple"

  let assert Error(sibling) =
    validate_tool_source([
      tool_call(
        "submit_example_artifact",
        Some("{\"schema_version\":1,\"artifact_type\":\"example\"}"),
        Some("success"),
        2,
      ),
    ])
  assert structured_output.error_code(sibling)
    == "structured_output_tool_call_sibling"
}

pub fn redacts_secret_strings_before_returning_payload_test() {
  let assert Ok(structured_output.StructuredOutputPresent(payload)) =
    structured_output.validate_final_response(
      spec(True),
      Some("{\"summary\":\"token-123\",\"findings\":[\"token-123\"]}"),
      False,
      ["token-123"],
      structured_output.noop_validator_runner,
    )

  assert !string.contains(payload, "token-123")
  let assert Ok(json_value.JObject(entries)) = json_value.parse(payload)
  assert json_value.object_has_key(entries, "summary")
  assert json_value.object_has_key(entries, "findings")
}

pub fn generic_validator_dispatches_and_preserves_diagnostics_test() {
  let failing_runner = fn(validator, _value, _payload, _index) {
    Error(structured_output_validator.ValidatorFailure(
      validator_name: workflow_dag.structured_output_validator_name(validator),
      validator_type: workflow_dag.structured_output_validator_type_to_string(
        validator,
      ),
      code: "structured_output_command_rejected",
      message: "lane.category is required",
      retryable: True,
      diagnostic_summary: "lane.category is required",
      stdout_truncated: False,
      stderr_truncated: False,
    ))
  }
  let assert Error(error) =
    structured_output.validate_final_response(
      review_lane_draft_spec(),
      Some(valid_review_lane_draft_json()),
      False,
      [],
      failing_runner,
    )

  assert structured_output.error_code(error)
    == "structured_output_command_rejected"
  assert structured_output.error_retryable(error)
  assert string.contains(
    structured_output.error_message(error),
    "validator review_lane_draft_compat (command) failed",
  )
  assert string.contains(
    structured_output.error_message(error),
    "lane.category",
  )
}

pub fn review_lane_draft_validator_accepts_valid_draft_test() {
  let assert Ok(structured_output.StructuredOutputPresent(_)) =
    validate_review_lane_draft(valid_review_lane_draft_json())
}

pub fn review_lane_draft_validator_rejects_missing_nested_lane_metadata_test() {
  let assert Error(error) =
    validate_review_lane_draft(missing_lane_category_and_version_json())

  assert structured_output.error_code(error)
    == "structured_output_command_rejected"
  assert string.contains(
    structured_output.error_message(error),
    "lane.category",
  )
}

pub fn review_lane_draft_validator_rejects_stale_finding_shape_test() {
  let assert Error(error) =
    validate_review_lane_draft(stale_finding_shape_json())

  assert structured_output.error_code(error)
    == "structured_output_command_rejected"
  assert string.contains(
    structured_output.error_message(error),
    "draft_findings[].draft_finding_id",
  )
}

pub fn review_lane_draft_validator_rejects_malformed_input_refs_test() {
  let assert Error(error) =
    validate_review_lane_draft(malformed_input_refs_json())

  assert structured_output.error_code(error)
    == "structured_output_command_rejected"
  assert string.contains(
    structured_output.error_message(error),
    "input_refs[0].path",
  )
}

fn validate_review_lane_draft(
  payload: String,
) -> Result(
  structured_output.StructuredOutputValidation,
  structured_output.StructuredOutputError,
) {
  structured_output.validate_final_response(
    review_lane_draft_spec(),
    Some(payload),
    False,
    [],
    structured_output.default_validator_runner(
      structured_output.default_validator_context(
        ".scherzo",
        ".",
        "test_workflow",
        ".scherzo/workflows",
        "test_run",
        "review_json",
        1,
        ".",
        "review_lane_draft",
        "json",
        structured_output_source.FinalResponseSource,
      ),
      [],
    ),
  )
}

fn valid_review_lane_draft_json() -> String {
  "{\"schema_version\":1,\"artifact_type\":\"review_lane_draft\",\"generated_at_utc\":\"2026-05-10T00:00:00Z\",\"producer\":{\"name\":\"structured-output-test\",\"version\":\"1\",\"mode\":\"native\"},\"lane\":{\"id\":\"correctness\",\"name\":\"Correctness reviewer\",\"category\":\"correctness\",\"version\":\"1\"},\"input_refs\":[],\"draft_findings\":[],\"review_notes\":[],\"evidence_requests\":[],\"self_check\":{\"inspected_diff\":true,\"used_repository_relative_paths\":true},\"remote_mutations\":\"none\"}"
}

fn missing_lane_category_and_version_json() -> String {
  "{\"schema_version\":1,\"artifact_type\":\"review_lane_draft\",\"generated_at_utc\":\"2026-05-10T00:00:00Z\",\"producer\":{\"name\":\"structured-output-test\",\"version\":\"1\",\"mode\":\"native\"},\"lane\":{\"id\":\"correctness\",\"name\":\"Correctness reviewer\"},\"input_refs\":[],\"draft_findings\":[],\"review_notes\":[],\"evidence_requests\":[],\"self_check\":{\"inspected_diff\":true,\"used_repository_relative_paths\":true},\"remote_mutations\":\"none\"}"
}

fn stale_finding_shape_json() -> String {
  "{\"schema_version\":1,\"artifact_type\":\"review_lane_draft\",\"generated_at_utc\":\"2026-05-10T00:00:00Z\",\"producer\":{\"name\":\"structured-output-test\",\"version\":\"1\",\"mode\":\"native\"},\"lane\":{\"id\":\"security-performance\",\"name\":\"Security / performance risk reviewer\",\"category\":\"security-performance\",\"version\":\"1\"},\"input_refs\":[],\"draft_findings\":[{\"id\":\"F1\",\"title\":\"Legacy finding shape\",\"category\":\"security\",\"severity\":\"high\",\"paths\":[\"src/example.gleam\"],\"recommendation\":\"Use the current draft finding shape.\"}],\"review_notes\":[],\"evidence_requests\":[],\"self_check\":{\"inspected_diff\":true,\"used_repository_relative_paths\":true},\"remote_mutations\":\"none\"}"
}

fn malformed_input_refs_json() -> String {
  "{\"schema_version\":1,\"artifact_type\":\"review_lane_draft\",\"generated_at_utc\":\"2026-05-10T00:00:00Z\",\"producer\":{\"name\":\"structured-output-test\",\"version\":\"1\",\"mode\":\"native\"},\"lane\":{\"id\":\"correctness\",\"name\":\"Correctness reviewer\",\"category\":\"correctness\",\"version\":\"1\"},\"input_refs\":[{\"artifact_type\":\"review_brief\"}],\"draft_findings\":[],\"review_notes\":[],\"evidence_requests\":[],\"self_check\":{\"inspected_diff\":true,\"used_repository_relative_paths\":true},\"remote_mutations\":\"none\"}"
}

fn review_lane_json(lane_id: String) -> String {
  "{\"schema_version\":1,\"artifact_type\":\"review_lane_draft\",\"generated_at_utc\":\"2026-05-13T00:00:00Z\",\"producer\":{\"name\":\"structured-output-test\",\"version\":\"1\",\"mode\":\"test\"},\"lane\":{\"id\":\""
  <> lane_id
  <> "\",\"name\":\"Lane\",\"category\":\"correctness\",\"version\":\"1\"},\"input_refs\":[],\"draft_findings\":[],\"review_notes\":[],\"evidence_requests\":[],\"self_check\":{\"inspected_diff\":true,\"used_repository_relative_paths\":true},\"remote_mutations\":\"none\"}"
}

pub fn pi_tool_call_source_still_runs_downstream_json_schema_after_acceptance_test() {
  let source =
    structured_output_source.PiToolCallSource(
      tool_name: "submit_structured_output",
      parameters_schema_path: Some(
        ".scherzo/workflows/schemas/review-lane-draft.correctness.v1.schema.json",
      ),
    )
  let spec =
    json_schema_review_spec(
      source,
      True,
      ".scherzo/workflows/schemas/review-lane-draft.correctness.v1.schema.json",
    )
  let result =
    result_artifact.from_final_response_with_tool_calls(
      Some(review_lane_json("correctness")),
      False,
      "test",
      [
        tool_call(
          "submit_structured_output",
          Some(review_lane_json("test-quality")),
          Some("success"),
          1,
        ),
      ],
    )
  let assert Error(error) = validate_json_schema_result(spec, result)

  assert structured_output.error_code(error)
    == "structured_output_json_schema_rejected"
}
