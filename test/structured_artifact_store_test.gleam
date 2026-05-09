import gleam/bit_array
import gleam/string
import scherzo/json_value
import scherzo/state/artifact_store
import simplifile

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
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
      "{\"summary\":\"ok\",\"findings\":[]}",
    )

  assert ref.ref
    == "runs/run-1/review_json/attempt-0/structured/review_result.json"
  assert string.ends_with(ref.path, ".scherzo-state/artifacts/" <> ref.ref)
  let assert Ok(contents) = simplifile.read(ref.path)
  assert ref.bytes == bit_array.byte_size(bit_array.from_string(contents))
  assert ref.sha256 != ""
  assert string.contains(contents, "\"artifact_type\":\"structured_output\"")
  assert string.contains(contents, "\"payload\":")
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
      "{\"summary\":\"[REDACTED]\"}",
    )

  let assert Ok(contents) = simplifile.read(ref.path)
  assert string.contains(contents, "[REDACTED]")
  assert !string.contains(contents, "token-123")
}
