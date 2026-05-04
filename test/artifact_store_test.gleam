import gleam/bit_array
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
