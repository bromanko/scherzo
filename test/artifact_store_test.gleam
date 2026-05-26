import gleam/bit_array
import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/path
import scherzo/state/artifact_store
import scherzo/step_artifact
import simplifile
import support/artifact_store_fixtures
import support/test_helpers

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 1000,
    template_field_max_chars: 1000,
    workflow_summary_max_chars: 4000,
  )
}

fn artifact_root(root: String) -> String {
  root <> "/.scherzo-state/artifacts"
}

pub fn artifact_store_writes_relative_hash_verified_artifacts_test() {
  let root = "test/tmp/artifact-store/roundtrip"
  test_helpers.reset_dir(root)
  let store = artifact_store.new(root)
  let artifact =
    step_artifact.from_command_result(
      "build step",
      0,
      "stdøut",
      "stderr",
      False,
      [],
      limits(),
    )

  let assert Ok(ref) =
    artifact_store.write_step_artifact(
      store,
      "run-1",
      "workflow-alpha",
      "build step",
      1,
      artifact,
    )
  assert !string.starts_with(ref.ref, "/")
  assert string.contains(ref.ref, "build_step-")
  let full_path = artifact_root(root) <> "/" <> ref.ref
  let assert Ok(stored_contents) = simplifile.read(full_path)
  assert ref.bytes
    == bit_array.byte_size(bit_array.from_string(stored_contents))
  let assert Ok(location) = artifact_store.location(store, ref.ref)
  assert location.display_path == ".scherzo-state/artifacts/" <> ref.ref
  assert location.local_path == Some(full_path)
  let assert Ok(decoded) =
    artifact_store.read_step_artifact(store, ref.ref, ref.sha256)
  assert decoded == artifact
}

pub fn custom_store_round_trips_without_local_path_test() {
  let root = "test/tmp/artifact-store/custom-no-local-path"
  test_helpers.reset_dir(root)
  let store = artifact_store_fixtures.hidden_local_path_store(root)
  let artifact =
    step_artifact.from_command_result(
      "build",
      0,
      "stdout",
      "stderr",
      False,
      [],
      limits(),
    )

  let assert Ok(ref) =
    artifact_store.write_step_artifact(
      store,
      "run-1",
      "workflow-alpha",
      "build",
      1,
      artifact,
    )
  let assert Ok(location) = artifact_store.location(store, ref.ref)
  assert location.local_path == None
  assert location.display_path == "artifacts://" <> ref.ref
  let assert Ok(decoded) =
    artifact_store.read_step_artifact(store, ref.ref, ref.sha256)
  assert decoded == artifact
}

pub fn artifact_store_fails_closed_for_missing_and_corrupt_artifacts_test() {
  let root = "test/tmp/artifact-store/fail-closed"
  test_helpers.reset_dir(root)
  let store = artifact_store.new(root)
  let artifact =
    step_artifact.from_command_result(
      "build",
      0,
      "stdout",
      "stderr",
      False,
      [],
      limits(),
    )
  let assert Ok(ref) =
    artifact_store.write_step_artifact(
      store,
      "run-1",
      "workflow-alpha",
      "build",
      1,
      artifact,
    )
  let full_path = artifact_root(root) <> "/" <> ref.ref
  let assert Ok(Nil) = simplifile.write(full_path, "corrupt")
  let assert Error(artifact_store.CorruptStepArtifact(_)) =
    artifact_store.read_step_artifact(store, ref.ref, ref.sha256)
  let assert Ok(Nil) = simplifile.delete_file(at: full_path)
  let assert Error(artifact_store.MissingStepArtifact(_)) =
    artifact_store.read_step_artifact(store, ref.ref, ref.sha256)
}

pub fn decode_step_artifact_contents_reports_json_decode_context_test() {
  let malformed =
    "{\"schema_version\":2,\"run_id\":1,\"workflow_id\":\"workflow-alpha\",\"step_id\":\"build\",\"attempt_index\":1,\"artifact\":{}}"

  let assert Error(artifact_store.DecodeArtifactFailed(message)) =
    artifact_store.decode_step_artifact_contents(malformed)
  assert string.starts_with(message, "invalid_stored_step_artifact:")
  assert string.contains(message, "path=run_id")
  assert string.contains(message, "expected=String")
}

pub fn artifact_store_rejects_unsafe_refs_test() {
  let root = "test/tmp/artifact-store/invalid-ref"
  test_helpers.reset_dir(root)
  let store = artifact_store.new(root)

  let assert Error(artifact_store.InvalidArtifactRef(_)) =
    artifact_store.read_artifact_unverified(store, "")
  let assert Error(artifact_store.InvalidArtifactRef(_)) =
    artifact_store.read_artifact_unverified(store, "/absolute")
  let assert Error(artifact_store.InvalidArtifactRef(_)) =
    artifact_store.read_artifact_unverified(store, "../escape")
}

pub fn filesystem_uri_escapes_spaces_in_workspace_path_test() {
  let root = "test/tmp/artifact-store/uri with spaces"
  test_helpers.reset_dir(root)
  let store = artifact_store.new(root)

  let assert Ok(ref) =
    artifact_store.write_output_blob(store, "run-1", "notes", ".txt", "hello")
  let assert Ok(location) = artifact_store.location(store, ref.ref)

  assert string.starts_with(location.uri, "file://")
  assert string.contains(location.uri, "uri%20with%20spaces")
  assert !string.contains(location.uri, "uri with spaces")
}

pub fn write_atomic_uses_unique_temp_and_leaves_no_success_temp_test() {
  let root = "test/tmp/artifact-store/write-atomic-success"
  test_helpers.reset_dir(root)
  let final_path = root <> "/artifact.json"

  let assert Ok(Nil) = artifact_store.write_atomic(final_path, "payload")

  let assert Ok(contents) = simplifile.read(final_path)
  assert contents == "payload"
  assert temp_entries(root) == []
}

pub fn write_atomic_reports_open_temp_phase_for_missing_parent_test() {
  let root = "test/tmp/artifact-store/write-atomic-missing-parent"
  test_helpers.reset_dir(root)

  let result =
    artifact_store.write_atomic(root <> "/missing/artifact.json", "payload")

  let assert Error(artifact_store.OpenTempFailed(_)) = result
  assert temp_entries(root) == []
}

pub fn write_atomic_concurrent_writers_leave_one_complete_payload_test() {
  let root = "test/tmp/artifact-store/write-atomic-concurrent"
  test_helpers.reset_dir(root)
  let final_path = root <> "/artifact.json"
  let payload_a = string.repeat("a", times: 10_000)
  let payload_b = string.repeat("b", times: 10_000)
  let subject = process.new_subject()

  let _ =
    process.spawn(fn() {
      process.send(subject, artifact_store.write_atomic(final_path, payload_a))
    })
  let _ =
    process.spawn(fn() {
      process.send(subject, artifact_store.write_atomic(final_path, payload_b))
    })

  let assert Ok(Ok(Nil)) = process.receive(subject, within: 1000)
  let assert Ok(Ok(Nil)) = process.receive(subject, within: 1000)
  let assert Ok(final) = simplifile.read(final_path)
  assert final == payload_a || final == payload_b
  assert temp_entries(root) == []
}

fn temp_entries(dir: String) -> List(String) {
  let assert Ok(entries) = simplifile.read_directory(dir)
  list.filter(entries, fn(entry) {
    string.contains(entry, ".scherzo-") && string.ends_with(entry, ".tmp")
  })
}

pub fn recovery_artifact_rewrite_with_identical_bytes_is_idempotent_test() {
  let root = "test/tmp/artifact-store/recovery-artifact-identical"
  test_helpers.reset_dir(root)
  let store = artifact_store.new(root)
  let payload =
    "{\"artifact_type\":\"workflow_step_recovery_result\",\"schema_version\":1,\"decision\":\"gave_up\",\"summary\":\"No fix\",\"reason\":\"Needs a human\"}"

  let assert Ok(first) =
    artifact_store.write_recovery_artifact_json(
      store,
      "run-1",
      "implement",
      1,
      1,
      "workflow_step_recovery_result",
      payload,
    )
  let assert Ok(second) =
    artifact_store.write_recovery_artifact_json(
      store,
      "run-1",
      "implement",
      1,
      1,
      "workflow_step_recovery_result",
      payload,
    )

  assert second.ref == first.ref
  assert second.sha256 == first.sha256
  assert second.bytes == first.bytes
  let assert Ok(stored) =
    artifact_store.read_artifact_unverified(store, first.ref)
  assert stored == payload
}

pub fn recovery_artifact_rewrite_with_different_bytes_conflicts_test() {
  let root = "test/tmp/artifact-store/recovery-artifact-conflict"
  test_helpers.reset_dir(root)
  let store = artifact_store.new(root)
  let first_payload =
    "{\"artifact_type\":\"workflow_step_recovery_result\",\"schema_version\":1,\"decision\":\"gave_up\",\"summary\":\"No fix\",\"reason\":\"Needs a human\"}"
  let second_payload =
    "{\"artifact_type\":\"workflow_step_recovery_result\",\"schema_version\":1,\"decision\":\"retry_requested\",\"summary\":\"Patched\",\"reason\":\"Ready\"}"

  let assert Ok(first) =
    artifact_store.write_recovery_artifact_json(
      store,
      "run-1",
      "implement",
      1,
      1,
      "workflow_step_recovery_result",
      first_payload,
    )
  let assert Error(artifact_store.DecodeArtifactFailed(
    "immutable_recovery_artifact_conflict",
  )) =
    artifact_store.write_recovery_artifact_json(
      store,
      "run-1",
      "implement",
      1,
      1,
      "workflow_step_recovery_result",
      second_payload,
    )

  let assert Ok(stored) =
    artifact_store.read_artifact_unverified(store, first.ref)
  assert stored == first_payload
}

pub fn recovery_artifact_refs_distinguish_sanitized_step_collisions_test() {
  let root = "test/tmp/artifact-store/recovery-artifact-step-collisions"
  test_helpers.reset_dir(root)
  let store = artifact_store.new(root)
  let first_payload =
    "{\"artifact_type\":\"workflow_step_recovery_result\",\"schema_version\":1,\"decision\":\"gave_up\",\"summary\":\"First\",\"reason\":\"Needs a human\"}"
  let second_payload =
    "{\"artifact_type\":\"workflow_step_recovery_result\",\"schema_version\":1,\"decision\":\"gave_up\",\"summary\":\"Second\",\"reason\":\"Needs a human\"}"

  let assert Ok(first) =
    artifact_store.write_recovery_artifact_json(
      store,
      "run-1",
      "review/fix",
      1,
      1,
      "workflow_step_recovery_result",
      first_payload,
    )
  let assert Ok(second) =
    artifact_store.write_recovery_artifact_json(
      store,
      "run-1",
      "review_fix",
      1,
      1,
      "workflow_step_recovery_result",
      second_payload,
    )

  assert first.ref != second.ref
  let assert Ok(first_stored) =
    artifact_store.read_artifact_unverified(store, first.ref)
  let assert Ok(second_stored) =
    artifact_store.read_artifact_unverified(store, second.ref)
  assert first_stored == first_payload
  assert second_stored == second_payload
}

pub fn artifact_store_writes_contract_manifests_and_output_blobs_test() {
  let root = "test/tmp/artifact-store/contract-manifests"
  test_helpers.reset_dir(root)
  let store = artifact_store.new(root)

  let assert Ok(inputs) =
    artifact_store.write_input_manifest(store, "run-1", "{\"inputs\":[]}")
  assert inputs.ref == "runs/run-1/inputs.v1.json"
  assert inputs.bytes == 13
  let assert Ok(input_contents) =
    artifact_store.read_artifact_unverified(store, inputs.ref)
  assert input_contents == "{\"inputs\":[]}"

  let assert Ok(outputs) =
    artifact_store.write_output_manifest(store, "run-1", "{\"outputs\":[]}")
  assert outputs.ref == "runs/run-1/outputs.v1.json"
  let assert Ok(blob) =
    artifact_store.write_output_blob(
      store,
      "run-1",
      "findings",
      ".md",
      "# Findings",
    )
  assert blob.ref == "runs/run-1/outputs/findings.md"
  let assert Ok(blob_contents) =
    artifact_store.read_artifact_unverified(store, blob.ref)
  assert blob_contents == "# Findings"
}

pub fn restore_filesystem_artifact_bytes_restores_deleted_ref_test() {
  let root = "test/tmp/artifact-store/restore-deleted"
  test_helpers.reset_dir(root)
  let store = artifact_store.new(root)
  let ref = "runs/run-1/upstream/attempt-1.json"
  let original = bit_array.from_string("{\"status\":\"ok\"}")

  let assert Ok(artifact_store.ImmutableWritten) =
    artifact_store.write_immutable_artifact_bytes(store, ref, original)
  let full_path = artifact_root(root) <> "/" <> ref
  let assert Ok(Nil) = simplifile.delete_file(at: full_path)

  let assert Ok(Nil) =
    artifact_store.restore_filesystem_artifact_bytes(root, ref, original)
  let assert Ok(restored) = artifact_store.read_file_bytes(full_path)
  assert restored == original
}

pub fn restore_filesystem_artifact_bytes_reports_obstructing_directory_test() {
  let root = "test/tmp/artifact-store/restore-obstructed"
  test_helpers.reset_dir(root)
  let ref = "runs/run-1/upstream/attempt-1.json"
  let full_path = artifact_root(root) <> "/" <> ref
  let parent = path.dirname(full_path) |> result.unwrap(full_path)
  let assert Ok(Nil) = simplifile.create_directory_all(full_path)
  let assert Ok(Nil) = simplifile.create_directory_all(parent)

  let assert Error(artifact_store.ArtifactWriteFailed(_)) =
    artifact_store.restore_filesystem_artifact_bytes(
      root,
      ref,
      bit_array.from_string("{\"status\":\"ok\"}"),
    )
}
