import gleam/bit_array
import gleam/option.{None, Some}
import gleam/result
import scherzo/hash
import scherzo/runtime_bundle
import scherzo/state/artifact_store
import scherzo/state/projection
import scherzo/workflow_attempt
import scherzo/workflow_dag
import scherzo/workflow_interface_snapshot

pub fn current_workflow(
  projected: projection.Projection,
  bundle: runtime_bundle.RuntimeBundle,
  root: String,
  run_id: String,
  workflow_id: String,
) -> Result(workflow_dag.WorkflowDag, #(String, String)) {
  use #(_, workflow) <- result.try(
    runtime_bundle.workflow_by_id(bundle, workflow_id)
    |> result.map_error(fn(error) {
      let runtime_bundle.BundleError(code: code, message: message) = error
      #(code, message)
    }),
  )
  use _ <- result.try(ensure_current_routes_safe(
    projected,
    bundle,
    root,
    run_id,
    workflow,
  ))
  Ok(workflow)
}

pub fn ensure_current_routes_safe(
  projected: projection.Projection,
  bundle: runtime_bundle.RuntimeBundle,
  root: String,
  run_id: String,
  workflow: workflow_dag.WorkflowDag,
) -> Result(Nil, #(String, String)) {
  use provenance <- result.try(
    projection.workflow_run_provenance(projected, run_id)
    |> result.map_error(fn(_) {
      #(
        "publication_route_discovery_unsafe",
        "workflow run provenance is unavailable; cannot safely discover declared publication routes for run "
          <> run_id,
      )
    }),
  )
  let current_fingerprint =
    workflow_attempt.workflow_fingerprint(workflow, bundle.orchestrator)
  validate_current_routes_against_snapshot(
    projected,
    root,
    run_id,
    provenance.workflow_fingerprint,
    current_fingerprint,
    workflow,
  )
}

fn validate_current_routes_against_snapshot(
  projected: projection.Projection,
  root: String,
  run_id: String,
  recorded_fingerprint: String,
  current_fingerprint: String,
  workflow: workflow_dag.WorkflowDag,
) -> Result(Nil, #(String, String)) {
  case projection.workflow_interface_snapshot(projected, run_id) {
    None ->
      Error(#(
        "publication_route_discovery_unsafe",
        missing_snapshot_message(
          run_id,
          recorded_fingerprint,
          current_fingerprint,
        ),
      ))
    Some(ref) -> {
      use snapshot <- result.try(load_snapshot(root, ref))
      use _ <- result.try(validate_snapshot_fingerprint(
        run_id,
        recorded_fingerprint,
        snapshot.workflow_fingerprint,
      ))
      let current_snapshot =
        workflow_interface_snapshot.from_dag(workflow, current_fingerprint)
      case
        snapshot.workflow_id == workflow_dag.id(workflow)
        && snapshot.publication_routes == current_snapshot.publication_routes
      {
        True -> Ok(Nil)
        False ->
          Error(#(
            "publication_route_discovery_unsafe",
            "current workflow publication routes differ from the run-pinned workflow interface snapshot; cannot safely discover declared publication routes for run "
              <> run_id,
          ))
      }
    }
  }
}

fn missing_snapshot_message(
  run_id: String,
  recorded_fingerprint: String,
  current_fingerprint: String,
) -> String {
  let drift = case recorded_fingerprint == current_fingerprint {
    True -> ""
    False ->
      "current workflow fingerprint does not match retained run fingerprint and "
  }
  drift
  <> "no run-pinned workflow interface snapshot is available; cannot safely discover declared publication routes for run "
  <> run_id
}

fn validate_snapshot_fingerprint(
  run_id: String,
  recorded_fingerprint: String,
  snapshot_fingerprint: String,
) -> Result(Nil, #(String, String)) {
  case
    recorded_fingerprint == "" || recorded_fingerprint == snapshot_fingerprint
  {
    True -> Ok(Nil)
    False ->
      Error(#(
        "publication_route_discovery_unsafe",
        "run-pinned workflow interface snapshot fingerprint does not match retained run fingerprint; cannot safely discover declared publication routes for run "
          <> run_id,
      ))
  }
}

fn load_snapshot(
  root: String,
  ref: projection.WorkflowInterfaceSnapshotRef,
) -> Result(
  workflow_interface_snapshot.WorkflowInterfaceSnapshot,
  #(String, String),
) {
  use contents <- result.try(
    artifact_store.read_artifact_unverified(
      artifact_store.new(root),
      ref.artifact_ref,
    )
    |> result.map_error(fn(error) {
      #(
        "publication_route_discovery_snapshot_unavailable",
        "run-pinned workflow interface snapshot could not be read: "
          <> ref.artifact_ref
          <> " ("
          <> artifact_error_message(error)
          <> ")",
      )
    }),
  )
  use _ <- result.try(verify_snapshot_bytes(ref, contents))
  workflow_interface_snapshot.decode_string(contents)
  |> result.map_error(fn(error) {
    let workflow_interface_snapshot.InvalidSnapshot(message) = error
    #("publication_route_discovery_snapshot_invalid", message)
  })
}

fn verify_snapshot_bytes(
  ref: projection.WorkflowInterfaceSnapshotRef,
  contents: String,
) -> Result(Nil, #(String, String)) {
  let actual_sha256 = hash.sha256_hex(contents)
  let actual_bytes = bit_array.byte_size(bit_array.from_string(contents))
  case
    actual_sha256 == ref.artifact_sha256,
    actual_bytes == ref.artifact_bytes
  {
    True, True -> Ok(Nil)
    False, _ ->
      Error(#(
        "publication_route_discovery_snapshot_hash_mismatch",
        "run-pinned workflow interface snapshot hash did not match ledger ref: "
          <> ref.artifact_ref,
      ))
    _, False ->
      Error(#(
        "publication_route_discovery_snapshot_size_mismatch",
        "run-pinned workflow interface snapshot byte count did not match ledger ref: "
          <> ref.artifact_ref,
      ))
  }
}

fn artifact_error_message(error: artifact_store.ArtifactError) -> String {
  case error {
    artifact_store.ArtifactIo(message)
    | artifact_store.CorruptStepArtifact(message)
    | artifact_store.InvalidArtifactRef(message)
    | artifact_store.DecodeArtifactFailed(message)
    | artifact_store.DirectorySyncUnsupported(message) -> message
    artifact_store.MissingStepArtifact(ref) -> "missing artifact: " <> ref
    artifact_store.ArtifactWriteFailed(write_error) ->
      artifact_store.artifact_write_error_to_string(write_error)
  }
}
