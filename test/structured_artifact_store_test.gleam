import gleam/bit_array
import gleam/option.{None, Some}
import gleam/string
import scherzo/json_value
import scherzo/state/artifact_store
import scherzo/structured_output_metadata
import scherzo/structured_output_source
import scherzo/workflow_dag
import simplifile

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

fn fixture_payload() -> String {
  let assert Ok(contents) =
    simplifile.read(
      "test/fixtures/structured_output/review_lane_payload_valid.json",
    )
  contents
}

pub fn structured_output_artifact_store_writes_wrapped_json_test() {
  let root = "test/tmp/structured-artifact-store/roundtrip"
  reset_dir(root)
  let store = artifact_store.new(root)

  let assert Ok(ref) =
    artifact_store.write_structured_output_artifact(
      store,
      "run-1",
      "structured_review",
      "review_json",
      0,
      "review_result",
      "json",
      ["summary", "findings"],
      structured_output_metadata.baseline_only(["summary", "findings"]),
      "{\"summary\":\"ok\",\"findings\":[]}",
    )

  assert ref.ref
    == "runs/run-1/review_json/attempt-0/structured/review_result.json"
  assert string.ends_with(ref.path, ".scherzo-state/artifacts/" <> ref.ref)
  assert ref.display_path == ".scherzo-state/artifacts/" <> ref.ref
  assert ref.local_path == Some(ref.path)
  assert string.starts_with(ref.uri, "file://")
  let assert Ok(contents) = simplifile.read(ref.path)
  assert ref.bytes == bit_array.byte_size(bit_array.from_string(contents))
  assert ref.sha256 != ""
  assert string.contains(contents, "\"artifact_type\":\"structured_output\"")
  assert string.contains(contents, "\"payload\":")
  assert string.contains(contents, "\"validation\":")
  assert string.contains(contents, "\"summary\":\"ok\"")

  let assert Ok(decoded) =
    artifact_store.read_structured_output_artifact(store, ref.ref, ref.sha256)
  assert decoded.run_id == "run-1"
  assert decoded.workflow_id == "structured_review"
  assert decoded.step_id == "review_json"
  assert decoded.attempt_index == 0
  assert decoded.artifact_name == "review_result"
  assert decoded.format == "json"
  assert decoded.schema_required_keys == ["summary", "findings"]
  let assert json_value.JObject(entries) = decoded.payload
  assert json_value.object_has_key(entries, "summary")
  assert json_value.object_has_key(entries, "findings")
}

pub fn structured_output_artifact_store_writes_json_schema_metadata_test() {
  let root = "test/tmp/structured-artifact-store/json-schema-metadata"
  reset_dir(root)
  let store = artifact_store.new(root)
  let source_tool_name = Some("submit_review_lane_draft")
  let spec =
    workflow_dag.StructuredOutputSpec(
      artifact_name: "review_lane_draft",
      required: True,
      source: structured_output_source.PiToolCallSource(
        tool_name: "submit_review_lane_draft",
        require_single: True,
        reject_sibling_tool_calls: True,
        parameters_schema_path: None,
      ),
      format: workflow_dag.StructuredJson,
      schema: workflow_dag.StructuredObjectSchema([
        "schema_version",
        "artifact_type",
      ]),
      validators: [
        workflow_dag.JsonSchemaValidator(
          name: "review_lane_shape",
          path: "test/fixtures/structured_output/review_lane_draft.schema.json",
          draft: Some("2020-12"),
        ),
      ],
      validation_retries: 1,
    )

  let assert Ok(ref) =
    artifact_store.write_structured_output_artifact(
      store,
      "run-1",
      "structured_review",
      "review_json",
      0,
      "review_lane_draft",
      "json",
      ["schema_version", "artifact_type"],
      structured_output_metadata.from_spec(spec, "."),
      fixture_payload(),
    )

  let assert Ok(contents) = simplifile.read(ref.path)
  assert string.contains(contents, "\"source_type\":\"pi_tool_call\"")
  assert string.contains(
    contents,
    "\"source_tool_name\":\"submit_review_lane_draft\"",
  )
  assert string.contains(contents, "\"type\":\"json_schema\"")
  assert string.contains(
    contents,
    "\"schema_path\":\"test/fixtures/structured_output/review_lane_draft.schema.json\"",
  )
  assert string.contains(contents, "\"schema_sha256\":\"")
  assert string.contains(contents, "\"draft\":\"2020-12\"")

  let assert Ok(decoded) =
    artifact_store.read_structured_output_artifact(store, ref.ref, ref.sha256)
  assert decoded.source_type == "pi_tool_call"
  assert decoded.source_tool_name == source_tool_name
}

pub fn structured_output_artifact_store_receives_redacted_payload_test() {
  let root = "test/tmp/structured-artifact-store/redacted"
  reset_dir(root)
  let store = artifact_store.new(root)

  let assert Ok(ref) =
    artifact_store.write_structured_output_artifact(
      store,
      "run-1",
      "structured_review",
      "review_json",
      0,
      "review_result",
      "json",
      ["summary"],
      structured_output_metadata.baseline_only(["summary"]),
      "{\"summary\":\"[REDACTED]\"}",
    )

  let assert Ok(contents) = simplifile.read(ref.path)
  assert string.contains(contents, "[REDACTED]")
  assert !string.contains(contents, "token-123")
}
