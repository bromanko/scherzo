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

fn artifact_root(root: String) -> String {
  root <> "/.scherzo-state/artifacts"
}

fn hidden_local_path_store(root: String) -> artifact_store.Store {
  let store_root = artifact_root(root)
  artifact_store.custom(
    "hidden-local-path",
    artifact_store.StoreCallbacks(
      write: fn(ref, contents) {
        let final_path = store_root <> "/" <> ref
        let parent = path.dirname(final_path) |> result.unwrap(final_path)
        use Nil <- result.try(
          simplifile.create_directory_all(parent)
          |> result.map_error(fn(error) {
            artifact_store.ArtifactIo(simplifile.describe_error(error))
          }),
        )
        artifact_store.write_atomic(final_path, contents)
        |> result.map_error(fn(error) {
          artifact_store.ArtifactWriteFailed(error)
        })
      },
      read: fn(ref) {
        simplifile.read(store_root <> "/" <> ref)
        |> result.map_error(fn(error) {
          case error {
            simplifile.Enoent -> artifact_store.MissingStepArtifact(ref)
            _ -> artifact_store.ArtifactIo(simplifile.describe_error(error))
          }
        })
      },
      write_immutable_bytes: fn(ref, contents) {
        artifact_store.write_immutable(store_root <> "/" <> ref, contents)
        |> result.map_error(fn(error) {
          artifact_store.ArtifactWriteFailed(error)
        })
      },
      read_bytes: fn(ref) {
        artifact_store.read_file_bytes(store_root <> "/" <> ref)
        |> result.map_error(fn(error) {
          case error {
            artifact_store.MissingStepArtifact(_) ->
              artifact_store.MissingStepArtifact(ref)
            _ -> error
          }
        })
      },
      locate: fn(ref) {
        Ok(artifact_store.ArtifactLocation(
          ref: ref,
          uri: "artifact://hidden-local-path/" <> ref,
          display_path: "artifacts://" <> ref,
          local_path: None,
        ))
      },
    ),
  )
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
  reset_dir(root)
  let store = hidden_local_path_store(root)
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
  let full_path = artifact_root(root) <> "/" <> ref.ref
  let assert Ok(Nil) = simplifile.write(full_path, "corrupt")
  let assert Error(artifact_store.CorruptStepArtifact(_)) =
    artifact_store.read_step_artifact(store, ref.ref, ref.sha256)
  let assert Ok(Nil) = simplifile.delete_file(at: full_path)
  let assert Error(artifact_store.MissingStepArtifact(_)) =
    artifact_store.read_step_artifact(store, ref.ref, ref.sha256)
}

pub fn artifact_store_rejects_unsafe_refs_test() {
  let root = "test/tmp/artifact-store/invalid-ref"
  reset_dir(root)
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
  reset_dir(root)
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

pub fn artifact_store_writes_contract_manifests_and_output_blobs_test() {
  let root = "test/tmp/artifact-store/contract-manifests"
  reset_dir(root)
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
