import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/hash
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/projection

pub const protected_checkpoint_restored = "protected_checkpoint_restored"

pub const recovery_artifact_restore_failed = "recovery_artifact_restore_failed"

pub type CheckpointKind {
  StepAttemptArtifact
  WorkflowInputManifest
  WorkflowOutputManifest
}

pub type SnapshotEntry {
  SnapshotEntry(
    kind: CheckpointKind,
    ref: String,
    expected_sha256: String,
    current_sha256: String,
    local_path: String,
    backup_bytes: BitArray,
  )
}

pub type Snapshot {
  Snapshot(entries: List(SnapshotEntry))
}

pub type GuardEvent {
  ProtectedCheckpointRestored(
    kind: CheckpointKind,
    ref: String,
    expected_sha256: String,
    observed_status: String,
  )
}

pub type GuardError {
  GuardError(code: String, message: String)
}

type ProtectedCheckpointRef {
  ProtectedCheckpointRef(
    kind: CheckpointKind,
    ref: String,
    expected_sha256: String,
  )
}

pub fn code(error: GuardError) -> String {
  error.code
}

pub fn describe_error(error: GuardError) -> String {
  error.message
}

pub fn snapshot_for_run(
  workspace_root: String,
  run_id: String,
) -> Result(Snapshot, GuardError) {
  case ledger.path_for_workspace_root(workspace_root) {
    Error(error) ->
      Error(guard_error(
        "load ledger path failed: " <> describe_ledger_error(error),
      ))
    Ok(ledger_path) ->
      case ledger.load_projection(ledger_path) {
        Error(error) ->
          Error(guard_error(
            "load projection failed: " <> describe_ledger_error(error),
          ))
        Ok(state) ->
          snapshot_for_projection(
            artifact_store.new(workspace_root),
            state,
            run_id,
          )
      }
  }
}

pub fn snapshot_for_projection(
  store: artifact_store.Store,
  state: projection.Projection,
  run_id: String,
) -> Result(Snapshot, GuardError) {
  case collect_protected_refs(state, run_id) {
    Error(error) -> Error(error)
    Ok(refs) -> snapshot_entries(refs, store, [])
  }
}

fn snapshot_entries(
  refs: List(ProtectedCheckpointRef),
  store: artifact_store.Store,
  acc: List(SnapshotEntry),
) -> Result(Snapshot, GuardError) {
  case refs {
    [] -> Ok(Snapshot(list.reverse(acc)))
    [checkpoint, ..rest] -> {
      let location_result = artifact_store.location(store, checkpoint.ref)
      case location_result {
        Error(error) ->
          Error(guard_error(
            "protected checkpoint location lookup failed for "
            <> checkpoint.ref
            <> ": "
            <> describe_artifact_error(error),
          ))
        Ok(location) ->
          case location.local_path {
            None ->
              Error(guard_error(
                "protected checkpoint has no local_path: " <> checkpoint.ref,
              ))
            Some(local_path) ->
              case
                artifact_store.read_artifact_bytes_unverified(
                  store,
                  checkpoint.ref,
                )
              {
                Error(error) ->
                  Error(guard_error(
                    "protected checkpoint read failed for "
                    <> checkpoint.ref
                    <> ": "
                    <> describe_artifact_error(error),
                  ))
                Ok(backup_bytes) -> {
                  let current_sha256 = hash.sha256_hex_bytes(backup_bytes)
                  case current_sha256 == checkpoint.expected_sha256 {
                    False ->
                      Error(guard_error(
                        "protected checkpoint hash mismatch for "
                        <> checkpoint.ref
                        <> ": expected "
                        <> checkpoint.expected_sha256
                        <> ", observed "
                        <> current_sha256,
                      ))
                    True ->
                      snapshot_entries(rest, store, [
                        SnapshotEntry(
                          kind: checkpoint.kind,
                          ref: checkpoint.ref,
                          expected_sha256: checkpoint.expected_sha256,
                          current_sha256: current_sha256,
                          local_path: local_path,
                          backup_bytes: backup_bytes,
                        ),
                        ..acc
                      ])
                  }
                }
              }
          }
      }
    }
  }
}

pub fn restore_after_recovery(
  workspace_root: String,
  snapshot: Snapshot,
) -> Result(List(GuardEvent), GuardError) {
  restore_snapshot_entries(
    artifact_store.new(workspace_root),
    snapshot.entries,
    [],
    Some(workspace_root),
  )
}

pub fn restore_after_recovery_with_store(
  store: artifact_store.Store,
  snapshot: Snapshot,
) -> Result(List(GuardEvent), GuardError) {
  restore_snapshot_entries(store, snapshot.entries, [], None)
}

fn restore_snapshot_entries(
  store: artifact_store.Store,
  entries: List(SnapshotEntry),
  acc: List(GuardEvent),
  restore_root: Option(String),
) -> Result(List(GuardEvent), GuardError) {
  case entries {
    [] -> Ok(list.reverse(acc))
    [entry, ..rest] ->
      case artifact_store.read_artifact_bytes_unverified(store, entry.ref) {
        Ok(current_bytes) ->
          case hash.sha256_hex_bytes(current_bytes) == entry.expected_sha256 {
            True -> restore_snapshot_entries(store, rest, acc, restore_root)
            False ->
              case restore_snapshot_entry(store, entry, restore_root) {
                Ok(Nil) ->
                  restore_snapshot_entries(
                    store,
                    rest,
                    [
                      ProtectedCheckpointRestored(
                        kind: entry.kind,
                        ref: entry.ref,
                        expected_sha256: entry.expected_sha256,
                        observed_status: "mutated",
                      ),
                      ..acc
                    ],
                    restore_root,
                  )
                Error(error) -> Error(error)
              }
          }
        Error(artifact_store.MissingStepArtifact(_)) ->
          case restore_snapshot_entry(store, entry, restore_root) {
            Ok(Nil) ->
              restore_snapshot_entries(
                store,
                rest,
                [
                  ProtectedCheckpointRestored(
                    kind: entry.kind,
                    ref: entry.ref,
                    expected_sha256: entry.expected_sha256,
                    observed_status: "missing",
                  ),
                  ..acc
                ],
                restore_root,
              )
            Error(error) -> Error(error)
          }
        Error(error) ->
          Error(guard_error(
            "protected checkpoint reread failed for "
            <> entry.ref
            <> ": "
            <> describe_artifact_error(error),
          ))
      }
  }
}

pub fn events_to_diagnostic(events: List(GuardEvent)) -> String {
  events
  |> list.map(fn(event) {
    case event {
      ProtectedCheckpointRestored(kind, ref, expected_sha256, observed_status) ->
        protected_checkpoint_restored
        <> " kind="
        <> checkpoint_kind_name(kind)
        <> " ref="
        <> ref
        <> " expected_sha256="
        <> expected_sha256
        <> " observed="
        <> observed_status
    }
  })
  |> string.join(with: "; ")
}

fn restore_snapshot_entry(
  store: artifact_store.Store,
  entry: SnapshotEntry,
  restore_root: Option(String),
) -> Result(Nil, GuardError) {
  let write_result = case restore_root {
    Some(workspace_root) ->
      artifact_store.restore_filesystem_artifact_bytes(
        workspace_root,
        entry.ref,
        entry.backup_bytes,
      )
    None ->
      artifact_store.write_atomic_bytes(entry.local_path, entry.backup_bytes)
      |> result.map_error(fn(error) {
        artifact_store.ArtifactWriteFailed(error)
      })
  }
  case write_result {
    Error(error) ->
      Error(guard_error(
        "protected checkpoint restore write failed for "
        <> entry.ref
        <> ": "
        <> describe_artifact_error(error),
      ))
    Ok(Nil) ->
      case artifact_store.read_artifact_bytes_unverified(store, entry.ref) {
        Error(error) ->
          Error(guard_error(
            "protected checkpoint restore verification read failed for "
            <> entry.ref
            <> ": "
            <> describe_artifact_error(error),
          ))
        Ok(restored) ->
          case hash.sha256_hex_bytes(restored) == entry.expected_sha256 {
            True -> Ok(Nil)
            False ->
              Error(guard_error(
                "protected checkpoint restore verification failed for "
                <> entry.ref,
              ))
          }
      }
  }
}

fn collect_protected_refs(
  state: projection.Projection,
  run_id: String,
) -> Result(List(ProtectedCheckpointRef), GuardError) {
  let attempts =
    state.step_attempts
    |> dict.values
    |> list.fold([], fn(acc, status) {
      case status {
        projection.StepAttemptFinishedStatus(
          run_id: status_run_id,
          artifact_ref: artifact_ref,
          artifact_sha256: artifact_sha256,
          ..,
        )
          if status_run_id == run_id
        -> [
          ProtectedCheckpointRef(
            kind: StepAttemptArtifact,
            ref: artifact_ref,
            expected_sha256: artifact_sha256,
          ),
          ..acc
        ]
        _ -> acc
      }
    })
    |> list.reverse
  let manifests =
    workflow_manifest_refs(state, run_id, [])
    |> list.reverse
  deduplicate_refs(list.append(attempts, manifests), dict.new(), [])
}

fn workflow_manifest_refs(
  state: projection.Projection,
  run_id: String,
  acc: List(ProtectedCheckpointRef),
) -> List(ProtectedCheckpointRef) {
  let acc = case projection.workflow_input_manifest(state, run_id) {
    Some(projection.WorkflowContractManifestRef(
      artifact_ref: artifact_ref,
      artifact_sha256: artifact_sha256,
      ..,
    )) -> [
      ProtectedCheckpointRef(
        kind: WorkflowInputManifest,
        ref: artifact_ref,
        expected_sha256: artifact_sha256,
      ),
      ..acc
    ]
    None -> acc
  }
  case projection.workflow_output_manifest(state, run_id) {
    Some(projection.WorkflowContractManifestRef(
      artifact_ref: artifact_ref,
      artifact_sha256: artifact_sha256,
      ..,
    )) -> [
      ProtectedCheckpointRef(
        kind: WorkflowOutputManifest,
        ref: artifact_ref,
        expected_sha256: artifact_sha256,
      ),
      ..acc
    ]
    None -> acc
  }
}

fn deduplicate_refs(
  refs: List(ProtectedCheckpointRef),
  seen: Dict(String, ProtectedCheckpointRef),
  ordered: List(ProtectedCheckpointRef),
) -> Result(List(ProtectedCheckpointRef), GuardError) {
  case refs {
    [] -> Ok(list.reverse(ordered))
    [checkpoint, ..rest] ->
      case dict.get(seen, checkpoint.ref) {
        Ok(existing) ->
          case existing.expected_sha256 == checkpoint.expected_sha256 {
            True -> deduplicate_refs(rest, seen, ordered)
            False ->
              Error(guard_error(
                "conflicting protected checkpoint hashes for " <> checkpoint.ref,
              ))
          }
        Error(Nil) ->
          deduplicate_refs(rest, dict.insert(seen, checkpoint.ref, checkpoint), [
            checkpoint,
            ..ordered
          ])
      }
  }
}

fn checkpoint_kind_name(kind: CheckpointKind) -> String {
  case kind {
    StepAttemptArtifact -> "step_attempt_artifact"
    WorkflowInputManifest -> "workflow_input_manifest"
    WorkflowOutputManifest -> "workflow_output_manifest"
  }
}

fn guard_error(message: String) -> GuardError {
  GuardError(code: recovery_artifact_restore_failed, message: message)
}

fn describe_ledger_error(error: ledger.LedgerError) -> String {
  ledger.ledger_error_to_string(error)
}

fn describe_artifact_error(error: artifact_store.ArtifactError) -> String {
  case error {
    artifact_store.ArtifactIo(reason) -> reason
    artifact_store.ArtifactWriteFailed(reason) ->
      artifact_store.artifact_write_error_to_string(reason)
    artifact_store.MissingStepArtifact(ref) -> "missing: " <> ref
    artifact_store.CorruptStepArtifact(reason) -> reason
    artifact_store.InvalidArtifactRef(ref) -> "invalid artifact ref: " <> ref
    artifact_store.DecodeArtifactFailed(reason) -> reason
    artifact_store.DirectorySyncUnsupported(reason) -> reason
  }
}
