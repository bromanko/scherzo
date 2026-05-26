import gleam/option.{None, Some}
import gleam/string
import scherzo/json_value
import scherzo/structured_output
import scherzo/structured_output_source
import scherzo/structured_output_validator
import scherzo/workflow_dag
import scherzo/workstream/artifacts
import scherzo/workstream/foundation
import scherzo/workstream/types
import simplifile

fn fixture_text(path: String) -> String {
  let assert Ok(contents) = simplifile.read(path)
  contents
}

fn fixture_json(path: String) -> json_value.JsonValue {
  let assert Ok(value) = path |> fixture_text |> json_value.parse
  value
}

fn assert_round_trip(
  path: String,
  decoder: fn(String) -> Result(a, types.SpecError),
  encoder: fn(a) -> String,
) {
  let source = fixture_json(path)
  let assert Ok(value) = decoder(fixture_text(path))
  let assert Ok(encoded) = json_value.parse(encoder(value))
  assert encoded == source
}

fn assert_error_code(
  path: String,
  decoder: fn(String) -> Result(a, types.SpecError),
  expected: String,
) {
  let assert Error(error) = decoder(fixture_text(path))
  assert types.error_code(error) == expected
}

fn assert_inline_error_code(
  contents: String,
  decoder: fn(String) -> Result(a, types.SpecError),
  expected: String,
) {
  let assert Error(error) = decoder(contents)
  assert types.error_code(error) == expected
}

fn validator_context() -> structured_output_validator.ValidatorContext {
  structured_output_validator.base_context(
    ".scherzo",
    ".",
    "test/tmp/workstream-specs/run-root",
    "test_workflow",
    ".scherzo/workflows",
    "run-1",
    "validate_workstream_artifact",
    1,
    ".",
    "artifact",
    "json",
    structured_output.source_type_to_string(
      structured_output_source.FinalResponseSource,
    ),
    None,
  )
}

fn schema_validator(
  name: String,
  path: String,
) -> workflow_dag.StructuredOutputValidator {
  workflow_dag.JsonSchemaValidator(
    name: name,
    path: path,
    draft: Some("2020-12"),
  )
}

pub fn workstream_fixture_round_trip_test() {
  assert_round_trip(
    "test/fixtures/workstream/specs/workstream_valid.json",
    artifacts.decode_workstream,
    artifacts.workstream_to_string,
  )
}

pub fn legacy_workstream_issue_ref_decodes_as_linear_task_ref_test() {
  let assert Ok(decoded) =
    artifacts.decode_workstream(
      "{\"schema_version\":1,\"artifact_type\":\"scherzo.workstream.v1\",\"artifact_id\":\"workstream-linear-liv-370\",\"workstream_id\":\"linear:LIV-370\",\"issue\":{\"id\":\"issue-370\"},\"status\":\"active\",\"summary\":\"summary\",\"produced_artifacts\":[],\"next_actions\":[]}",
    )

  assert decoded.task_ref
    == types.TaskRef(
      backend_kind: "linear",
      remote_id: "issue-370",
      key: None,
      url: None,
    )
}

pub fn handoff_fixture_round_trip_test() {
  assert_round_trip(
    "test/fixtures/workstream/specs/handoff_valid.json",
    artifacts.decode_handoff,
    artifacts.handoff_to_string,
  )
}

pub fn decision_fixture_round_trip_test() {
  assert_round_trip(
    "test/fixtures/workstream/specs/decision_valid_approve.json",
    artifacts.decode_decision,
    artifacts.decision_to_string,
  )
}

pub fn input_bundle_fixture_round_trip_test() {
  assert_round_trip(
    "test/fixtures/workstream/specs/input_bundle_valid.json",
    artifacts.decode_input_bundle,
    artifacts.input_bundle_to_string,
  )
}

pub fn assignment_fixture_round_trip_test() {
  assert_round_trip(
    "test/fixtures/workstream/specs/assignment_valid.json",
    artifacts.decode_assignment,
    artifacts.assignment_to_string,
  )
}

pub fn next_action_fixture_round_trip_test() {
  assert_round_trip(
    "test/fixtures/workstream/specs/next_action_valid.json",
    artifacts.decode_next_action,
    artifacts.next_action_to_string,
  )
}

pub fn missing_headers_return_stable_error_codes_test() {
  assert_error_code(
    "test/fixtures/workstream/specs/workstream_invalid_missing_artifact_type.json",
    artifacts.decode_workstream,
    "workstream_artifact_type_missing",
  )
  assert_error_code(
    "test/fixtures/workstream/specs/workstream_invalid_missing_schema_version.json",
    artifacts.decode_workstream,
    "workstream_schema_version_missing",
  )
  assert_error_code(
    "test/fixtures/workstream/specs/handoff_invalid_missing_artifact_type.json",
    artifacts.decode_handoff,
    "workstream_artifact_type_missing",
  )
  assert_error_code(
    "test/fixtures/workstream/specs/handoff_invalid_missing_schema_version.json",
    artifacts.decode_handoff,
    "workstream_schema_version_missing",
  )
}

pub fn invalid_handoff_snapshot_and_paths_return_stable_error_codes_test() {
  assert_error_code(
    "test/fixtures/workstream/specs/handoff_invalid_output_missing_ref.json",
    artifacts.decode_handoff,
    "workstream_snapshot_ref_missing",
  )
  assert_error_code(
    "test/fixtures/workstream/specs/handoff_invalid_absolute_original_path.json",
    artifacts.decode_handoff,
    "workstream_original_path_invalid",
  )
  assert_error_code(
    "test/fixtures/workstream/specs/handoff_invalid_parent_original_path.json",
    artifacts.decode_handoff,
    "workstream_original_path_invalid",
  )

  let terminal_parent =
    fixture_text("test/fixtures/workstream/specs/handoff_valid.json")
    |> string.replace(
      each: "\"original_path\": \"artifacts/review/handoff.json\"",
      with: "\"original_path\": \"artifacts/review/..\"",
    )
  assert_inline_error_code(
    terminal_parent,
    artifacts.decode_handoff,
    "workstream_original_path_invalid",
  )
}

pub fn snapshot_ref_hash_must_match_sha256_test() {
  let mismatched_hash =
    fixture_text("test/fixtures/workstream/specs/handoff_valid.json")
    |> string.replace(
      each: "\"sha256\": \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"",
      with: "\"sha256\": \"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\"",
    )

  assert_inline_error_code(
    mismatched_hash,
    artifacts.decode_handoff,
    "workstream_snapshot_hash_mismatch",
  )
}

pub fn missing_schema_required_fields_return_stable_error_codes_test() {
  assert_inline_error_code(
    "{\"schema_version\":1,\"artifact_type\":\"scherzo.workstream.v1\",\"artifact_id\":\"workstream-linear-liv-370\",\"workstream_id\":\"linear:LIV-370\",\"issue\":{\"id\":\"LIV-370\"},\"status\":\"active\",\"summary\":\"summary\",\"next_actions\":[]}",
    artifacts.decode_workstream,
    "workstream_produced_artifacts_missing",
  )
  assert_inline_error_code(
    "{\"schema_version\":1,\"artifact_type\":\"scherzo.handoff.v1\",\"artifact_id\":\"handoff-linear-liv-370\",\"workstream_id\":\"linear:LIV-370\",\"phase_id\":\"artifact-specs\",\"summary\":\"summary\",\"recommended_next_actions\":[],\"open_questions\":[]}",
    artifacts.decode_handoff,
    "workstream_outputs_missing",
  )
  assert_inline_error_code(
    "{\"schema_version\":1,\"artifact_type\":\"scherzo.input_bundle.v1\",\"artifact_id\":\"input-bundle-linear-liv-370\",\"workstream_id\":\"linear:LIV-370\",\"source_handoff_ref\":\"workstream-artifacts/sha256/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.json\",\"workflow_id\":\"execplan-revision\"}",
    artifacts.decode_input_bundle,
    "workstream_inputs_missing",
  )
  assert_inline_error_code(
    "{\"schema_version\":1,\"artifact_type\":\"scherzo.next_action.v1\",\"artifact_id\":\"next-action-linear-liv-370-review\",\"workstream_id\":\"linear:LIV-370\",\"action_id\":\"review-fixtures\",\"workflow_id\":\"execplan-revision\",\"state\":\"available\",\"priority\":10,\"inputs\":[]}",
    artifacts.decode_next_action,
    "workstream_auto_enqueue_missing",
  )
}

pub fn optional_artifact_fields_reject_wrong_types_test() {
  let bad_playbook =
    fixture_text("test/fixtures/workstream/specs/assignment_valid.json")
    |> string.replace(
      each: "\"playbook_id\": \"ticket-1-specs\"",
      with: "\"playbook_id\": false",
    )
  assert_inline_error_code(
    bad_playbook,
    artifacts.decode_assignment,
    "workstream_playbook_id_invalid",
  )

  let bad_gate =
    fixture_text("test/fixtures/workstream/specs/next_action_valid.json")
    |> string.replace(
      each: "\"requires_gate\": \"human_review\"",
      with: "\"requires_gate\": false",
    )
  assert_inline_error_code(
    bad_gate,
    artifacts.decode_next_action,
    "workstream_requires_gate_invalid",
  )
}

pub fn input_bundle_metadata_fields_reject_invalid_values_test() {
  assert_inline_error_code(
    input_bundle_with_binding_field("\"sha256\": false"),
    artifacts.decode_input_bundle,
    "workstream_sha256_invalid",
  )
  assert_inline_error_code(
    input_bundle_with_binding_field(
      "\"sha256\": \"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc\"",
    ),
    artifacts.decode_input_bundle,
    "workstream_snapshot_hash_mismatch",
  )
  assert_inline_error_code(
    input_bundle_with_binding_field("\"bytes\": 0"),
    artifacts.decode_input_bundle,
    "workstream_bytes_invalid",
  )
  assert_inline_error_code(
    input_bundle_with_binding_field("\"media_type\": false"),
    artifacts.decode_input_bundle,
    "workstream_media_type_invalid",
  )
  assert_inline_error_code(
    input_bundle_with_binding_field("\"original_path\": \"../secret\""),
    artifacts.decode_input_bundle,
    "workstream_original_path_invalid",
  )
  assert_inline_error_code(
    input_bundle_with_binding_field("\"artifact_type\": false"),
    artifacts.decode_input_bundle,
    "workstream_artifact_type_invalid",
  )
  assert_inline_error_code(
    input_bundle_with_binding_field("\"source_kind\": false"),
    artifacts.decode_input_bundle,
    "workstream_source_kind_invalid",
  )
  assert_inline_error_code(
    input_bundle_with_top_level_field("\"source_kind\": false"),
    artifacts.decode_input_bundle,
    "workstream_source_kind_invalid",
  )
  assert_inline_error_code(
    input_bundle_with_top_level_field("\"source_reason\": false"),
    artifacts.decode_input_bundle,
    "workstream_source_reason_invalid",
  )
}

fn input_bundle_with_binding_field(field: String) -> String {
  fixture_text("test/fixtures/workstream/specs/input_bundle_valid.json")
  |> string.replace(
    each: "\"value_ref\": \"workstream-artifacts/sha256/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb.json\"",
    with: "\"value_ref\": \"workstream-artifacts/sha256/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb.json\",\n      "
      <> field,
  )
}

fn input_bundle_with_top_level_field(field: String) -> String {
  fixture_text("test/fixtures/workstream/specs/input_bundle_valid.json")
  |> string.replace(
    each: "\"workflow_id\": \"execplan-implementation\",",
    with: "\"workflow_id\": \"execplan-implementation\",\n  " <> field <> ",",
  )
}

pub fn invalid_artifact_variants_return_stable_error_codes_test() {
  assert_error_code(
    "test/fixtures/workstream/specs/decision_invalid_unknown_kind.json",
    artifacts.decode_decision,
    "workstream_decision_kind_unknown",
  )
  assert_error_code(
    "test/fixtures/workstream/specs/input_bundle_invalid_missing_source_handoff_ref.json",
    artifacts.decode_input_bundle,
    "workstream_snapshot_ref_missing",
  )
  assert_error_code(
    "test/fixtures/workstream/specs/assignment_invalid_missing_workstream_id.json",
    artifacts.decode_assignment,
    "workstream_workstream_id_missing",
  )
  assert_error_code(
    "test/fixtures/workstream/specs/next_action_invalid_unknown_state.json",
    artifacts.decode_next_action,
    "workstream_next_action_state_unknown",
  )
}

pub fn validator_foundation_accepts_handoff_fixture_test() {
  let spec =
    foundation.ArtifactValidationSpec(
      artifact_type: types.handoff_artifact_type,
      artifact_name: "handoff",
      required_keys: ["schema_version", "artifact_type", "summary"],
      validators: [
        schema_validator(
          "handoff_shape",
          ".scherzo/workflows/schemas/workstream/handoff.v1.schema.json",
        ),
      ],
    )

  let assert Ok(structured_output.StructuredOutputPresent(_)) =
    foundation.validate_json_artifact(
      spec,
      fixture_text("test/fixtures/workstream/specs/handoff_valid.json"),
      validator_context(),
      [],
    )
}

pub fn validator_foundation_accepts_decision_fixture_test() {
  let spec =
    foundation.ArtifactValidationSpec(
      artifact_type: types.decision_artifact_type,
      artifact_name: "decision",
      required_keys: ["schema_version", "artifact_type", "summary"],
      validators: [
        schema_validator(
          "decision_shape",
          ".scherzo/workflows/schemas/workstream/decision.v1.schema.json",
        ),
      ],
    )

  let assert Ok(structured_output.StructuredOutputPresent(_)) =
    foundation.validate_json_artifact(
      spec,
      fixture_text("test/fixtures/workstream/specs/decision_valid_approve.json"),
      validator_context(),
      [],
    )
}

pub fn validator_foundation_rejects_invalid_handoff_fixture_test() {
  let spec =
    foundation.ArtifactValidationSpec(
      artifact_type: types.handoff_artifact_type,
      artifact_name: "handoff",
      required_keys: ["schema_version", "artifact_type", "summary"],
      validators: [
        schema_validator(
          "handoff_shape",
          ".scherzo/workflows/schemas/workstream/handoff.v1.schema.json",
        ),
      ],
    )

  let assert Error(error) =
    foundation.validate_json_artifact(
      spec,
      fixture_text(
        "test/fixtures/workstream/specs/handoff_invalid_output_missing_ref.json",
      ),
      validator_context(),
      [],
    )

  assert foundation.error_code(error)
    == "structured_output_json_schema_rejected"

  let terminal_parent =
    fixture_text("test/fixtures/workstream/specs/handoff_valid.json")
    |> string.replace(
      each: "\"original_path\": \"artifacts/review/handoff.json\"",
      with: "\"original_path\": \"artifacts/review/..\"",
    )
  let assert Error(parent_error) =
    foundation.validate_json_artifact(
      spec,
      terminal_parent,
      validator_context(),
      [],
    )
  assert foundation.error_code(parent_error)
    == "structured_output_json_schema_rejected"
}
