import gleam/option.{None, Some}
import gleam/string
import scherzo/json_value
import scherzo/structured_output_json_schema
import scherzo/structured_output_validator
import scherzo/workflow_dag
import simplifile

fn payload(path: String) -> json_value.JsonValue {
  let assert Ok(contents) = simplifile.read(path)
  let assert Ok(value) = json_value.parse(contents)
  value
}

fn context(
  validator: workflow_dag.StructuredOutputValidator,
) -> structured_output_validator.ValidatorContext {
  structured_output_validator.base_context(
    ".scherzo",
    ".",
    "test/tmp/run-root",
    "test_workflow",
    "run-1",
    "review",
    1,
    ".",
    "review_lane_draft",
    "json",
    "final_response",
    None,
  )
  |> structured_output_validator.for_validator(validator, 0)
}

fn schema_validator(path: String) -> workflow_dag.StructuredOutputValidator {
  workflow_dag.JsonSchemaValidator(
    name: "review_lane_shape",
    path: path,
    draft: Some("2020-12"),
  )
}

pub fn json_schema_accepts_valid_payload_test() {
  let validator =
    schema_validator(
      "test/fixtures/structured_output/review_lane_draft.schema.json",
    )
  assert structured_output_json_schema.run_json_schema_validator(
      validator,
      payload("test/fixtures/structured_output/review_lane_payload_valid.json"),
      context(validator),
      [],
    )
    == Ok(structured_output_validator.ValidatorPass)
}

pub fn json_schema_rejects_invalid_payload_with_instance_path_test() {
  let validator =
    schema_validator(
      "test/fixtures/structured_output/review_lane_draft.schema.json",
    )
  let assert Error(error) =
    structured_output_json_schema.run_json_schema_validator(
      validator,
      payload(
        "test/fixtures/structured_output/review_lane_payload_invalid.json",
      ),
      context(validator),
      [],
    )

  assert error.code == "structured_output_json_schema_rejected"
  assert error.retryable
  assert error.validator_name == "review_lane_shape"
  assert string.contains(error.diagnostic_summary, "instance_path=/findings")
}

pub fn json_schema_missing_file_is_non_retryable_config_error_test() {
  let validator =
    schema_validator("test/fixtures/structured_output/missing.schema.json")
  let assert Error(error) =
    structured_output_json_schema.run_json_schema_validator(
      validator,
      payload("test/fixtures/structured_output/review_lane_payload_valid.json"),
      context(validator),
      [],
    )

  assert error.code == "structured_output_json_schema_config_error"
  assert !error.retryable
}

pub fn json_schema_invalid_schema_is_non_retryable_config_error_test() {
  let validator =
    schema_validator(
      "test/fixtures/structured_output/invalid_schema.schema.json",
    )
  let assert Error(error) =
    structured_output_json_schema.run_json_schema_validator(
      validator,
      payload("test/fixtures/structured_output/review_lane_payload_valid.json"),
      context(validator),
      [],
    )

  assert error.code == "structured_output_json_schema_config_error"
  assert !error.retryable
}

pub fn json_schema_rejects_absolute_or_traversal_paths_test() {
  let absolute = schema_validator("/tmp/review_lane_draft.schema.json")
  let traversal = schema_validator("../review_lane_draft.schema.json")

  let assert Error(absolute_error) =
    structured_output_json_schema.run_json_schema_validator(
      absolute,
      payload("test/fixtures/structured_output/review_lane_payload_valid.json"),
      context(absolute),
      [],
    )
  let assert Error(traversal_error) =
    structured_output_json_schema.run_json_schema_validator(
      traversal,
      payload("test/fixtures/structured_output/review_lane_payload_valid.json"),
      context(traversal),
      [],
    )

  assert absolute_error.code == "structured_output_json_schema_config_error"
  assert traversal_error.code == "structured_output_json_schema_config_error"
}
