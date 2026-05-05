import gleam/result
import scherzo/state/artifact_store
import scherzo/step_artifact

pub type WorkflowStepTerminalStatus {
  StepSucceeded
  StepFailedContinued
  StepFailedFatal
}

pub type ArtifactStorageError {
  ArtifactMissing(artifact_ref: String)
  ArtifactCorrupt(artifact_ref: String, reason: String)
  ArtifactWriteFailed(artifact_ref: String, reason: String)
}

pub fn artifact_ref(run_id: String, step_id: String, attempt: Int) -> String {
  artifact_store.artifact_ref(run_id, step_id, attempt)
}

pub fn write_step_artifact(
  state_root: String,
  run_id: String,
  step_id: String,
  attempt: Int,
  artifact: step_artifact.StepArtifact,
) -> Result(String, ArtifactStorageError) {
  let ref = artifact_ref(run_id, step_id, attempt)
  artifact_store.write_step_artifact(
    artifact_store.new(state_root),
    run_id,
    "",
    step_id,
    attempt,
    artifact,
  )
  |> result.map(fn(written) { written.ref })
  |> result.map_error(fn(error) {
    ArtifactWriteFailed(ref, describe_artifact_error(error))
  })
}

pub fn read_step_artifact(
  state_root: String,
  artifact_ref: String,
) -> Result(step_artifact.StepArtifact, ArtifactStorageError) {
  artifact_store.read_step_artifact_unverified(
    artifact_store.new(state_root),
    artifact_ref,
  )
  |> result.map_error(fn(error) { map_read_error(artifact_ref, error) })
}

fn map_read_error(
  artifact_ref: String,
  error: artifact_store.ArtifactError,
) -> ArtifactStorageError {
  case error {
    artifact_store.MissingStepArtifact(_) -> ArtifactMissing(artifact_ref)
    artifact_store.CorruptStepArtifact(_) ->
      ArtifactCorrupt(artifact_ref, "checksum_mismatch")
    artifact_store.DecodeArtifactFailed(reason) ->
      ArtifactCorrupt(artifact_ref, reason)
    other -> ArtifactCorrupt(artifact_ref, describe_artifact_error(other))
  }
}

fn describe_artifact_error(error: artifact_store.ArtifactError) -> String {
  case error {
    artifact_store.ArtifactIo(message) -> message
    artifact_store.MissingStepArtifact(ref) -> "missing_step_artifact:" <> ref
    artifact_store.CorruptStepArtifact(ref) -> "corrupt_step_artifact:" <> ref
    artifact_store.InvalidArtifactRef(ref) -> "invalid_artifact_ref:" <> ref
    artifact_store.DecodeArtifactFailed(reason) ->
      "decode_artifact_failed:" <> reason
    artifact_store.DirectorySyncUnsupported(reason) ->
      "directory_sync_unsupported:" <> reason
  }
}
