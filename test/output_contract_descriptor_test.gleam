import gleam/bit_array
import gleam/int
import gleam/option.{type Option, None, Some}
import scherzo/hash
import scherzo/state/artifact_store
import scherzo/workflow_checkpoint
import scherzo/workflow_contract
import scherzo/workflow_contract_manifest as manifest
import scherzo/workflow_run/output_contract_descriptor
import simplifile

pub fn retained_artifact_set_validation_uses_checkpoint_local_paths_test() {
  let root = "test/tmp/output-contract-descriptor/retained-local-paths"
  let _ = simplifile.delete(root)
  let assert Ok(Nil) = simplifile.create_directory_all(root)

  let child_ref = "runs/run-1/outputs/reference.txt"
  let child_path = root <> "/reference.txt"
  let child_contents = "hello retained child\n"
  let child_bytes = bit_array.from_string(child_contents)
  let child_sha256 = hash.sha256_hex_bytes(child_bytes)
  let child_size = bit_array.byte_size(child_bytes)
  let assert Ok(Nil) = simplifile.write(child_path, child_contents)

  let artifact_set_ref = "runs/run-1/outputs/visual_artifacts.json"
  let artifact_set_path = root <> "/visual_artifacts.json"
  let artifact_set_contents =
    "{\"entries\":[{\"name\":\"reference\",\"kind\":\"file\",\"ref\":\""
    <> child_ref
    <> "\",\"sha256\":\""
    <> child_sha256
    <> "\",\"bytes\":"
    <> int.to_string(child_size)
    <> ",\"media_type\":\"text/plain\"}]}"
  let artifact_set_bytes = bit_array.from_string(artifact_set_contents)
  let assert Ok(Nil) =
    simplifile.write(artifact_set_path, artifact_set_contents)

  let value =
    manifest.present_run_artifact(
      workflow_contract.GenericArtifactSet,
      manifest.ArtifactWritten(
        ref: artifact_set_ref,
        sha256: hash.sha256_hex_bytes(artifact_set_bytes),
        bytes: bit_array.byte_size(artifact_set_bytes),
      ),
      "application/json",
      None,
    )
  let checkpoint =
    workflow_checkpoint.Writer(
      ..workflow_checkpoint.noop_writer(),
      artifact_location: fn(ref) {
        Ok(artifact_store.ArtifactLocation(
          ref: ref,
          uri: "artifact://test/" <> ref,
          display_path: ref,
          local_path: local_path_for(
            ref,
            artifact_set_ref,
            artifact_set_path,
            child_ref,
            child_path,
          ),
        ))
      },
    )

  let assert Ok(Nil) =
    output_contract_descriptor.validate_retained_output_descriptor(
      workflow_contract.OutputSpec(
        name: "visual_artifacts",
        type_: workflow_contract.GenericArtifactSet,
        required: True,
        description: None,
        source: None,
        descriptor: None,
      ),
      value,
      artifact_set_bytes,
      checkpoint,
    )
}

fn local_path_for(
  ref: String,
  artifact_set_ref: String,
  artifact_set_path: String,
  child_ref: String,
  child_path: String,
) -> Option(String) {
  case ref == artifact_set_ref, ref == child_ref {
    True, _ -> Some(artifact_set_path)
    _, True -> Some(child_path)
    _, _ -> None
  }
}
