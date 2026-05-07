import gleam/bit_array
import gleam/erlang/process
import gleam/list
import gleam/string
import scherzo/config/types as config_types
import scherzo/state/artifact_store
import scherzo/step_artifact
import simplifile

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 1000,
    template_field_max_chars: 1000,
    workflow_summary_max_chars: 4000,
  )
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

pub fn artifact_store_writes_relative_hash_verified_artifacts_test() {
  let root = "test/tmp/artifact-store/roundtrip"
  reset_dir(root)
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
  let full_path = root <> "/.scherzo-state/artifacts/" <> ref.ref
  let assert Ok(stored_contents) = simplifile.read(full_path)
  assert ref.bytes
    == bit_array.byte_size(bit_array.from_string(stored_contents))
  let assert Ok(decoded) =
    artifact_store.read_step_artifact(store, ref.ref, ref.sha256)
  assert decoded == artifact
}

pub fn artifact_store_fails_closed_for_missing_and_corrupt_artifacts_test() {
  let root = "test/tmp/artifact-store/fail-closed"
  reset_dir(root)
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
  let full_path = root <> "/.scherzo-state/artifacts/" <> ref.ref
  let assert Ok(Nil) = simplifile.write(full_path, "corrupt")
  let assert Error(artifact_store.CorruptStepArtifact(_)) =
    artifact_store.read_step_artifact(store, ref.ref, ref.sha256)
  let assert Ok(Nil) = simplifile.delete_file(at: full_path)
  let assert Error(artifact_store.MissingStepArtifact(_)) =
    artifact_store.read_step_artifact(store, ref.ref, ref.sha256)
}

pub fn write_atomic_uses_unique_temp_and_leaves_no_success_temp_test() {
  let root = "test/tmp/artifact-store/write-atomic-success"
  reset_dir(root)
  let final_path = root <> "/artifact.json"

  let assert Ok(Nil) = artifact_store.write_atomic(final_path, "payload")

  let assert Ok(contents) = simplifile.read(final_path)
  assert contents == "payload"
  assert temp_entries(root) == []
}

pub fn write_atomic_reports_open_temp_phase_for_missing_parent_test() {
  let root = "test/tmp/artifact-store/write-atomic-missing-parent"
  reset_dir(root)

  let result =
    artifact_store.write_atomic(root <> "/missing/artifact.json", "payload")

  let assert Error(artifact_store.OpenTempFailed(_)) = result
  assert temp_entries(root) == []
}

pub fn write_atomic_concurrent_writers_leave_one_complete_payload_test() {
  let root = "test/tmp/artifact-store/write-atomic-concurrent"
  reset_dir(root)
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
