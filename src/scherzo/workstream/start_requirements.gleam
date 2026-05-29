import gleam/bit_array
import gleam/dict
import gleam/list
import gleam/result
import scherzo/hash
import scherzo/state/projection
import scherzo/workflow_checkpoint
import scherzo/workstream/artifacts
import scherzo/workstream/start_key
import scherzo/workstream/types

pub type RequirementError {
  RequirementError(code: String, message: String)
}

pub type VerifiedPayload {
  VerifiedPayload(contents: String, bytes: Int)
}

pub fn read_verified_snapshot(
  checkpoint: workflow_checkpoint.Writer,
  ref: String,
  expected_sha256: String,
) -> Result(VerifiedPayload, RequirementError) {
  use Nil <- result.try(validate_snapshot_ref(ref, expected_sha256))
  use contents <- result.try(
    checkpoint.read_artifact(ref)
    |> result.map_error(checkpoint_error("snapshot_read_failed")),
  )
  let actual = hash.sha256_hex(contents)
  case actual == expected_sha256 {
    False ->
      error("snapshot_hash_mismatch", "snapshot hash mismatch for " <> ref)
    True ->
      Ok(VerifiedPayload(
        contents: contents,
        bytes: bit_array.byte_size(bit_array.from_string(contents)),
      ))
  }
}

pub fn require_recorded_handoff(
  projected: projection.Projection,
  workstream_id: String,
  handoff_ref: String,
  handoff_sha256: String,
) -> Result(Nil, RequirementError) {
  use workstream <- result.try(require_workstream(projected, workstream_id))
  case dict.get(workstream.handoffs, handoff_ref) {
    Error(Nil) ->
      error("handoff_not_recorded", "handoff is not recorded in the ledger")
    Ok(handoff) ->
      case handoff.handoff_sha256 == handoff_sha256 {
        True -> Ok(Nil)
        False ->
          error(
            "handoff_hash_mismatch",
            "recorded handoff hash does not match requested hash",
          )
      }
  }
}

pub fn require_recorded_input_bundle(
  projected: projection.Projection,
  workstream_id: String,
  bundle_ref: String,
  bundle_sha256: String,
) -> Result(Nil, RequirementError) {
  use workstream <- result.try(require_workstream(projected, workstream_id))
  case dict.get(workstream.artifacts, bundle_ref) {
    Error(Nil) ->
      error(
        "input_bundle_not_recorded",
        "input bundle is not recorded in the ledger",
      )
    Ok(artifact) ->
      case
        artifact.artifact_type == types.input_bundle_artifact_type
        && artifact.snapshot_sha256 == bundle_sha256
      {
        True -> Ok(Nil)
        False ->
          error(
            "input_bundle_record_mismatch",
            "recorded input bundle metadata does not match requested snapshot",
          )
      }
  }
}

pub fn require_input_bundle_workflow(
  bundle: types.InputBundleArtifact,
  workflow_id: String,
) -> Result(Nil, RequirementError) {
  case bundle.workflow_id == workflow_id {
    True -> Ok(Nil)
    False ->
      error(
        "input_bundle_workflow_mismatch",
        "input bundle was created for workflow " <> bundle.workflow_id,
      )
  }
}

pub fn require_recommended_next_action(
  projected: projection.Projection,
  checkpoint: workflow_checkpoint.Writer,
  handoff: types.HandoffArtifact,
  workflow_id: String,
  action_id: String,
) -> Result(Nil, RequirementError) {
  case handoff.recommended_next_actions {
    [] -> Ok(Nil)
    recommended_ids -> {
      use workstream <- result.try(require_workstream(
        projected,
        handoff.workstream_id,
      ))
      let candidates =
        workstream.artifacts
        |> dict.values
        |> list.filter(fn(artifact) {
          artifact.artifact_type == types.next_action_artifact_type
          && list.contains(recommended_ids, artifact.artifact_id)
        })
      case candidates {
        [] ->
          error(
            "next_action_not_recorded",
            "handoff recommended next action artifacts are not recorded",
          )
        _ ->
          require_matching_next_action(
            candidates,
            checkpoint,
            workflow_id,
            action_id,
          )
      }
    }
  }
}

fn require_matching_next_action(
  candidates: List(projection.WorkstreamArtifactSnapshot),
  checkpoint: workflow_checkpoint.Writer,
  workflow_id: String,
  action_id: String,
) -> Result(Nil, RequirementError) {
  case candidates {
    [] ->
      error(
        "next_action_mismatch",
        "requested workflow/action is not recommended by the handoff",
      )
    [artifact, ..rest] -> {
      use next_action <- result.try(read_recorded_next_action(
        checkpoint,
        artifact,
      ))
      case
        next_action.action_id == action_id
        && next_action.workflow_id == workflow_id
      {
        True -> Ok(Nil)
        False ->
          require_matching_next_action(rest, checkpoint, workflow_id, action_id)
      }
    }
  }
}

fn read_recorded_next_action(
  checkpoint: workflow_checkpoint.Writer,
  artifact: projection.WorkstreamArtifactSnapshot,
) -> Result(types.NextActionArtifact, RequirementError) {
  use payload <- result.try(read_verified_snapshot(
    checkpoint,
    artifact.snapshot_ref,
    artifact.snapshot_sha256,
  ))
  artifacts.decode_next_action(payload.contents)
  |> result.map_error(spec_error("next_action_invalid"))
}

fn require_workstream(
  projected: projection.Projection,
  workstream_id: String,
) -> Result(projection.WorkstreamStatus, RequirementError) {
  case dict.get(projected.workstreams, workstream_id) {
    Ok(workstream) -> Ok(workstream)
    Error(Nil) ->
      error("workstream_not_recorded", "workstream is not recorded in ledger")
  }
}

fn validate_snapshot_ref(
  ref: String,
  expected_sha256: String,
) -> Result(Nil, RequirementError) {
  case start_key.valid_snapshot_ref(ref, expected_sha256) {
    True -> Ok(Nil)
    False ->
      error(
        "snapshot_ref_invalid",
        "snapshot ref must match a lowercase sha256",
      )
  }
}

fn spec_error(prefix: String) -> fn(types.SpecError) -> RequirementError {
  fn(err) {
    RequirementError(
      prefix <> ":" <> types.error_code(err),
      types.error_message(err),
    )
  }
}

fn checkpoint_error(
  code: String,
) -> fn(workflow_checkpoint.CheckpointError) -> RequirementError {
  fn(err) { RequirementError(code, workflow_checkpoint.describe_error(err)) }
}

fn error(code: String, message: String) -> Result(a, RequirementError) {
  Error(RequirementError(code, message))
}
