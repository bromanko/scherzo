import gleam/option.{None, Some}
import gleam/string
import scherzo/structured_output
import scherzo/structured_output_source
import scherzo/structured_output_validator
import scherzo/workflow_dag
import scherzo/workstream/foundation
import simplifile

fn fixture_text(path: String) -> String {
  let assert Ok(contents) = simplifile.read(path)
  contents
}

fn handoff_validator() -> workflow_dag.StructuredOutputValidator {
  workflow_dag.JsonSchemaValidator(
    name: "handoff_shape",
    path: "test/fixtures/workstream/foundation/handoff.schema.json",
    draft: Some("2020-12"),
  )
}

fn validator_context() -> structured_output_validator.ValidatorContext {
  structured_output_validator.base_context(
    ".scherzo",
    ".",
    "test/tmp/workstream-foundation/run-root",
    "test_workflow",
    ".scherzo/workflows",
    "run-1",
    "validate_handoff",
    1,
    ".",
    "handoff",
    "json",
    structured_output.source_type_to_string(
      structured_output_source.FinalResponseSource,
    ),
    None,
  )
}

fn validation_spec(
  validators: List(workflow_dag.StructuredOutputValidator),
) -> foundation.ArtifactValidationSpec {
  foundation.ArtifactValidationSpec(
    artifact_type: "scherzo.handoff.v1",
    artifact_name: "handoff",
    required_keys: ["schema_version", "artifact_type", "summary"],
    validators: validators,
  )
}

pub fn workstream_json_schema_validator_accepts_valid_handoff_fixture_test() {
  let assert Ok(structured_output.StructuredOutputPresent(_)) =
    foundation.validate_json_artifact(
      validation_spec([handoff_validator()]),
      fixture_text("test/fixtures/workstream/foundation/handoff_valid.json"),
      validator_context(),
      [],
    )
}

pub fn workstream_json_schema_validator_rejects_invalid_handoff_fixture_with_stable_error_kind_test() {
  let assert Error(error) =
    foundation.validate_json_artifact(
      validation_spec([handoff_validator()]),
      fixture_text(
        "test/fixtures/workstream/foundation/handoff_invalid_missing_workstream.json",
      ),
      validator_context(),
      [],
    )

  assert foundation.error_code(error)
    == "structured_output_json_schema_rejected"
  assert string.contains(foundation.error_message(error), "handoff_shape")
}

pub fn workstream_required_keys_are_enforced_before_configured_validators_test() {
  let spec =
    foundation.ArtifactValidationSpec(
      artifact_type: "scherzo.handoff.v1",
      artifact_name: "handoff",
      required_keys: [
        "schema_version",
        "artifact_type",
        "summary",
        "required_by_foundation_seam",
      ],
      validators: [handoff_validator()],
    )

  let assert Error(error) =
    foundation.validate_json_artifact(
      spec,
      fixture_text("test/fixtures/workstream/foundation/handoff_valid.json"),
      validator_context(),
      [],
    )

  assert foundation.error_code(error) == "structured_output_schema_invalid"
  assert string.contains(
    foundation.error_message(error),
    "required_by_foundation_seam",
  )
}

pub fn workstream_validator_unconfigured_fails_closed_test() {
  let assert Error(error) =
    foundation.validate_json_artifact(
      validation_spec([]),
      fixture_text("test/fixtures/workstream/foundation/handoff_valid.json"),
      validator_context(),
      [],
    )

  assert foundation.error_code(error)
    == "workstream_artifact_validator_unconfigured"
}
