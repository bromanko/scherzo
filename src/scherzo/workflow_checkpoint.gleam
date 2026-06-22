import gleam/bit_array
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/artifact_publication_manifest
import scherzo/hash
import scherzo/session/tokens as session_tokens
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/ledger_batch
import scherzo/state/projection
import scherzo/state/record
import scherzo/step_artifact
import scherzo/structured_output_metadata
import scherzo/workflow_attempt
import scherzo/workflow_contract_manifest
import scherzo/workflow_identity
import scherzo/workspace_run
import scherzo/workstream/artifact_store as workstream_artifact_store

pub type TaskRef =
  record.TaskRefFields

pub type CheckpointError {
  CheckpointAppendFailed(String)
  CheckpointArtifactFailed(String)
}

pub type ArtifactWritten {
  ArtifactWritten(ref: String, sha256: String, bytes: Int)
}

pub type StructuredOutputWrite {
  StructuredOutputWrite(
    run_id: String,
    workflow_id: String,
    step_id: String,
    attempt_index: Int,
    artifact_name: String,
    format: String,
    schema_required_keys: List(String),
    validation: structured_output_metadata.ValidationMetadata,
    payload_json: String,
  )
}

pub type StructuredArtifactWritten {
  StructuredArtifactWritten(
    ref: String,
    path: String,
    uri: String,
    display_path: String,
    local_path: Option(String),
    sha256: String,
    bytes: Int,
  )
}

pub type RecoveryArtifactWrite {
  RecoveryArtifactWrite(
    run_id: String,
    workflow_id: String,
    step_id: String,
    failed_attempt_index: Int,
    recovery_attempt_number: Int,
    artifact_name: String,
    payload_json: String,
  )
}

pub type RecoveryArtifactWritten {
  RecoveryArtifactWritten(
    ref: String,
    path: String,
    uri: String,
    display_path: String,
    local_path: Option(String),
    sha256: String,
    bytes: Int,
  )
}

pub type StepRecoveryStarted {
  StepRecoveryStarted(
    run_id: String,
    workflow_id: String,
    step_id: String,
    failed_attempt_index: Int,
    recovery_attempt_number: Int,
    recovery_session_id: String,
    model: Option(String),
    prompt_ref: String,
  )
}

pub type StepRecoveryFinished {
  StepRecoveryFinished(
    run_id: String,
    workflow_id: String,
    step_id: String,
    failed_attempt_index: Int,
    recovery_attempt_number: Int,
    recovery_session_id: String,
    result: String,
    summary: String,
    reason: String,
    retry_attempt_index: Option(Int),
  )
}

pub type WorkflowStarted {
  WorkflowStarted(
    run_id: String,
    workflow_id: String,
    workflow_fingerprint: String,
    issue_id: String,
    issue_identifier: String,
    task_ref: Option(record.TaskRefFields),
    issue_fingerprint: String,
    observed_updated_at_ms: Int,
    run_root: String,
  )
}

pub type WorkflowFinished {
  WorkflowFinished(
    run_id: String,
    workflow_id: String,
    issue_id: String,
    task_ref: Option(record.TaskRefFields),
    outcome: String,
    token_total: Int,
    turns: Int,
  )
}

pub type WorkflowDiagnostic {
  WorkflowDiagnostic(
    run_id: String,
    workflow_id: String,
    issue_id: String,
    reason: String,
  )
}

pub type WorkflowContractManifestRecorded {
  WorkflowContractManifestRecorded(
    run_id: String,
    workflow_id: String,
    workflow_fingerprint: String,
    artifact: ArtifactWritten,
  )
}

pub type WorkflowOutputBlobWrite {
  WorkflowOutputBlobWrite(
    run_id: String,
    output_name: String,
    extension: String,
    contents: BitArray,
  )
}

pub type WorkflowPublicationManifestWrite {
  WorkflowPublicationManifestWrite(
    run_id: String,
    publication_id: String,
    attempt_key: String,
    payload_json: String,
  )
}

pub type StepFinished {
  StepFinished(
    run_id: String,
    workflow_id: String,
    step_id: String,
    attempt_index: Int,
    outcome: String,
    workspace_name: String,
    workspace_path: String,
    token_total: Int,
    turns: Int,
  )
}

pub type Writer {
  Writer(
    now_ms: fn() -> Int,
    workflow_started: fn(WorkflowStarted) -> Result(Nil, CheckpointError),
    workflow_finished: fn(WorkflowFinished) -> Result(Nil, CheckpointError),
    workflow_diagnostic: fn(WorkflowDiagnostic) -> Result(Nil, CheckpointError),
    step_prepared: fn(
      String,
      String,
      String,
      workspace_run.PreparedStepWorkspace,
    ) -> Result(Nil, CheckpointError),
    step_started: fn(String, String, String, Int, String, Option(String), Bool) ->
      Result(Nil, CheckpointError),
    step_continuation_started: fn(String, String, String, Int, String) ->
      Result(Nil, CheckpointError),
    step_pi_session_recorded: fn(workflow_attempt.PiSessionObservation) ->
      Result(Nil, CheckpointError),
    write_step_artifact: fn(StepFinished, step_artifact.StepArtifact) ->
      Result(ArtifactWritten, CheckpointError),
    write_structured_output_artifact: fn(StructuredOutputWrite) ->
      Result(StructuredArtifactWritten, CheckpointError),
    write_recovery_artifact: fn(RecoveryArtifactWrite) ->
      Result(RecoveryArtifactWritten, CheckpointError),
    read_artifact: fn(String) -> Result(String, CheckpointError),
    artifact_location: fn(String) ->
      Result(artifact_store.ArtifactLocation, CheckpointError),
    snapshot_existing_artifact_ref: fn(String, String, Int, String, String) ->
      Result(workstream_artifact_store.Snapshot, CheckpointError),
    snapshot_workstream_bytes: fn(String, String, BitArray) ->
      Result(workstream_artifact_store.Snapshot, CheckpointError),
    append_workstream_record_idempotent: fn(record.LedgerRecord) ->
      Result(ledger.AppendIdempotentResult, CheckpointError),
    append_workstream_start_records: fn(
      List(record.LedgerRecord),
      record.LedgerRecord,
    ) -> Result(ledger.AppendWorkstreamStartResult, CheckpointError),
    write_workflow_inputs_manifest: fn(String, String) ->
      Result(ArtifactWritten, CheckpointError),
    workflow_inputs_recorded: fn(WorkflowContractManifestRecorded) ->
      Result(Nil, CheckpointError),
    write_workflow_outputs_manifest: fn(String, String) ->
      Result(ArtifactWritten, CheckpointError),
    workflow_outputs_recorded: fn(WorkflowContractManifestRecorded) ->
      Result(Nil, CheckpointError),
    write_publication_manifest: fn(WorkflowPublicationManifestWrite) ->
      Result(ArtifactWritten, CheckpointError),
    publication_attempt_recorded: fn(record.LedgerRecord) ->
      Result(ledger.AppendIdempotentResult, CheckpointError),
    write_workflow_output_blob: fn(WorkflowOutputBlobWrite) ->
      Result(ArtifactWritten, CheckpointError),
    step_finished: fn(StepFinished, ArtifactWritten) ->
      Result(Nil, CheckpointError),
    step_recovery_started: fn(StepRecoveryStarted) ->
      Result(Nil, CheckpointError),
    step_recovery_finished: fn(StepRecoveryFinished) ->
      Result(Nil, CheckpointError),
    step_interrupted: fn(String, String, String, Int, String) ->
      Result(Nil, CheckpointError),
  )
}

pub fn noop_writer() -> Writer {
  Writer(
    now_ms: fn() { 0 },
    workflow_started: fn(_) { Ok(Nil) },
    workflow_finished: fn(_) { Ok(Nil) },
    workflow_diagnostic: fn(_) { Ok(Nil) },
    step_prepared: fn(_, _, _, _) { Ok(Nil) },
    step_started: fn(_, _, _, _, _, _, _) { Ok(Nil) },
    step_continuation_started: fn(_, _, _, _, _) { Ok(Nil) },
    step_pi_session_recorded: fn(_) { Ok(Nil) },
    write_step_artifact: fn(finished, _artifact) {
      Ok(ArtifactWritten(
        ref: "noop/"
          <> workflow_identity.step_component(finished.step_id)
          <> "/attempt-"
          <> int.to_string(finished.attempt_index)
          <> ".json",
        sha256: "noop",
        bytes: 0,
      ))
    },
    write_structured_output_artifact: fn(write) {
      Ok(StructuredArtifactWritten(
        ref: "noop/"
          <> workflow_identity.step_component(write.step_id)
          <> "/attempt-"
          <> int.to_string(write.attempt_index)
          <> "/structured/"
          <> workflow_identity.safe_component(write.artifact_name, "artifact")
          <> ".json",
        path: "noop-structured-output-artifact.json",
        uri: "artifact://noop/structured-output",
        display_path: "noop-structured-output-artifact.json",
        local_path: Some("noop-structured-output-artifact.json"),
        sha256: "noop",
        bytes: 0,
      ))
    },
    write_recovery_artifact: fn(write) {
      Ok(RecoveryArtifactWritten(
        ref: "noop/"
          <> workflow_identity.step_component(write.step_id)
          <> "/attempt-"
          <> int.to_string(write.failed_attempt_index)
          <> "/recovery-"
          <> int.to_string(write.recovery_attempt_number)
          <> "/"
          <> workflow_identity.safe_component(write.artifact_name, "artifact")
          <> ".json",
        path: "noop-recovery-artifact.json",
        uri: "artifact://noop/recovery",
        display_path: "noop-recovery-artifact.json",
        local_path: Some("noop-recovery-artifact.json"),
        sha256: "noop",
        bytes: 0,
      ))
    },
    read_artifact: fn(_) {
      Error(CheckpointArtifactFailed("noop_read_artifact"))
    },
    artifact_location: fn(ref) {
      Ok(artifact_store.ArtifactLocation(
        ref: ref,
        uri: "artifact://noop/" <> ref,
        display_path: ref,
        local_path: None,
      ))
    },
    snapshot_existing_artifact_ref: fn(
      _artifact_ref,
      _expected_sha256,
      _expected_bytes,
      original_ref,
      media_type,
    ) {
      Ok(workstream_artifact_store.Snapshot(
        ref: "noop/workstream/"
          <> workflow_identity.safe_component(original_ref, "artifact"),
        sha256: "noop",
        bytes: 0,
        original_path: original_ref,
        media_type: media_type,
      ))
    },
    snapshot_workstream_bytes: fn(original_path, media_type, _contents) {
      Ok(workstream_artifact_store.Snapshot(
        ref: "noop/workstream/"
          <> workflow_identity.safe_component(original_path, "artifact"),
        sha256: "noop",
        bytes: 0,
        original_path: original_path,
        media_type: media_type,
      ))
    },
    append_workstream_record_idempotent: fn(_record) { Ok(ledger.Appended) },
    append_workstream_start_records: fn(_, _) {
      Ok(ledger.WorkstreamStartRecordsAppended)
    },
    write_workflow_inputs_manifest: fn(run_id, _contents) {
      Ok(ArtifactWritten(
        ref: "noop/" <> run_id <> "/inputs.v1.json",
        sha256: "noop",
        bytes: 0,
      ))
    },
    workflow_inputs_recorded: fn(_) { Ok(Nil) },
    write_workflow_outputs_manifest: fn(run_id, _contents) {
      Ok(ArtifactWritten(
        ref: "noop/" <> run_id <> "/outputs.v1.json",
        sha256: "noop",
        bytes: 0,
      ))
    },
    workflow_outputs_recorded: fn(_) { Ok(Nil) },
    write_publication_manifest: fn(write) {
      Ok(ArtifactWritten(
        ref: "runs/"
          <> write.run_id
          <> "/publications/"
          <> write.publication_id
          <> "/"
          <> write.attempt_key
          <> ".json",
        sha256: hash.sha256_hex(write.payload_json),
        bytes: bit_array.byte_size(bit_array.from_string(write.payload_json)),
      ))
    },
    publication_attempt_recorded: fn(ledger_record) {
      Ok(ledger.AlreadyRecorded(ledger_record))
    },
    write_workflow_output_blob: fn(write) {
      Ok(ArtifactWritten(
        ref: "noop/"
          <> write.run_id
          <> "/outputs/"
          <> workflow_identity.safe_component(write.output_name, "output")
          <> write.extension,
        sha256: hash.sha256_hex_bytes(write.contents),
        bytes: bit_array.byte_size(write.contents),
      ))
    },
    step_finished: fn(_, _) { Ok(Nil) },
    step_recovery_started: fn(_) { Ok(Nil) },
    step_recovery_finished: fn(_) { Ok(Nil) },
    step_interrupted: fn(_, _, _, _, _) { Ok(Nil) },
  )
}

pub fn ledger_writer(workspace_root: String, now_ms: fn() -> Int) -> Writer {
  ledger_writer_with_artifact_store(
    workspace_root,
    now_ms,
    artifact_store.new(workspace_root),
  )
}

pub fn recollection_ledger_writer(
  workspace_root: String,
  now_ms: fn() -> Int,
  recollection_index: Int,
) -> Writer {
  let base = ledger_writer(workspace_root, now_ms)
  let store = artifact_store.new(workspace_root)
  Writer(
    ..base,
    write_workflow_outputs_manifest: fn(run_id, contents) {
      artifact_store.write_output_manifest_for_recollection(
        store,
        run_id,
        recollection_index,
        contents,
      )
      |> result.map(artifact_ref_to_written)
      |> result.map_error(fn(error) {
        CheckpointArtifactFailed(describe_artifact_error(error))
      })
    },
    workflow_outputs_recorded: fn(recorded: WorkflowContractManifestRecorded) {
      append_body(
        workspace_root,
        now_ms,
        record.WorkflowRunOutputsRecorded(
          recorded.run_id,
          recorded.workflow_id,
          recorded.workflow_fingerprint,
          recorded.artifact.ref,
          recorded.artifact.sha256,
          recorded.artifact.bytes,
        ),
      )
    },
    write_workflow_output_blob: fn(write: WorkflowOutputBlobWrite) {
      artifact_store.write_output_blob_bytes_for_recollection(
        store,
        write.run_id,
        write.output_name,
        write.extension,
        recollection_index,
        write.contents,
      )
      |> result.map(artifact_ref_to_written)
      |> result.map_error(fn(error) {
        CheckpointArtifactFailed(describe_artifact_error(error))
      })
    },
  )
}

pub fn next_output_recollection_index(
  workspace_root: String,
  run_id: String,
) -> Result(Int, CheckpointError) {
  use ledger_path <- result.try(
    ledger.path_for_workspace_root(workspace_root)
    |> result.map_error(fn(error) {
      CheckpointAppendFailed(describe_ledger_error(error))
    }),
  )
  use read <- result.try(
    ledger.read_records(ledger_path)
    |> result.map_error(fn(error) {
      CheckpointAppendFailed(describe_ledger_error(error))
    }),
  )
  Ok(
    read.records
    |> list.fold(0, fn(count, ledger_record) {
      case ledger_record.body {
        record.WorkflowRunOutputsRecorded(run_id: recorded_run_id, ..)
          if recorded_run_id == run_id
        -> count + 1
        _ -> count
      }
    })
    |> fn(count) { count + 1 },
  )
}

pub fn corrupt_tolerant_ledger_writer(
  workspace_root: String,
  now_ms: fn() -> Int,
) -> Writer {
  case ledger.path_for_workspace_root(workspace_root) {
    Error(error) -> {
      let _message = describe_ledger_error(error)
      ledger_writer(workspace_root, now_ms)
    }
    Ok(ledger_path) ->
      case ledger.load_projection(ledger_path) {
        Ok(_) -> ledger_writer(workspace_root, now_ms)
        Error(ledger.CorruptRecord(..)) -> noop_writer()
        Error(error) -> {
          let _message = describe_ledger_error(error)
          ledger_writer(workspace_root, now_ms)
        }
      }
  }
}

pub fn ledger_writer_with_artifact_store(
  workspace_root: String,
  now_ms: fn() -> Int,
  store: artifact_store.Store,
) -> Writer {
  Writer(
    now_ms: now_ms,
    workflow_started: fn(started) {
      use projection_state <- result.try(load_projection(workspace_root))
      case projection.has_workflow_run(projection_state, started.run_id) {
        True -> Ok(Nil)
        False ->
          append_body(workspace_root, now_ms, workflow_started_body(started))
      }
    },
    workflow_finished: fn(finished) {
      append_body(workspace_root, now_ms, workflow_finished_body(finished))
    },
    workflow_diagnostic: fn(diagnostic) {
      append_body(
        workspace_root,
        now_ms,
        record.WorkflowRunDiagnostic(
          diagnostic.run_id,
          diagnostic.workflow_id,
          diagnostic.issue_id,
          diagnostic.reason,
        ),
      )
    },
    step_prepared: fn(run_id, workflow_id, step_id, workspace) {
      append_batch(
        workspace_root,
        now_ms,
        ledger_batch.step_attempt_prepared(
          run_id,
          workflow_id,
          step_id,
          workspace.attempt_index,
          workspace.workspace_name,
          workspace.path,
          workspace.run_root,
          workspace.source_workspace_name,
          workspace.source_workspace_path,
        ),
      )
    },
    step_started: fn(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      operator_session_id,
      external_session_ref,
      continuation_capable,
    ) {
      append_batch(
        workspace_root,
        now_ms,
        ledger_batch.step_attempt_started(
          run_id,
          workflow_id,
          step_id,
          attempt_index,
          operator_session_id,
          external_session_ref,
          continuation_capable,
        ),
      )
    },
    step_continuation_started: fn(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      session_id,
    ) {
      append_batch(
        workspace_root,
        now_ms,
        ledger_batch.step_attempt_continuation_started(
          run_id,
          workflow_id,
          step_id,
          attempt_index,
          session_id,
        ),
      )
    },
    step_pi_session_recorded: fn(observation) {
      append_batch(
        workspace_root,
        now_ms,
        ledger_batch.step_attempt_pi_session_recorded_with_task(
          observation.run_id,
          observation.issue_id,
          observation.issue_identifier,
          observation.workflow_id,
          observation.workflow_fingerprint,
          observation.step_id,
          observation.workspace_name,
          observation.attempt_index,
          observation.workspace_path,
          observation.session_id,
          observation.session_file,
        ),
      )
    },
    write_step_artifact: fn(finished, artifact) {
      artifact_store.write_step_artifact(
        store,
        finished.run_id,
        finished.workflow_id,
        finished.step_id,
        finished.attempt_index,
        artifact,
      )
      |> result.map(fn(ref) {
        ArtifactWritten(ref: ref.ref, sha256: ref.sha256, bytes: ref.bytes)
      })
      |> result.map_error(fn(error) {
        CheckpointArtifactFailed(describe_artifact_error(error))
      })
    },
    write_structured_output_artifact: fn(write) {
      artifact_store.write_structured_output_artifact(
        store,
        write.run_id,
        write.workflow_id,
        write.step_id,
        write.attempt_index,
        write.artifact_name,
        write.format,
        write.schema_required_keys,
        write.validation,
        write.payload_json,
      )
      |> result.map(fn(ref) {
        StructuredArtifactWritten(
          ref: ref.ref,
          path: ref.path,
          uri: ref.uri,
          display_path: ref.display_path,
          local_path: ref.local_path,
          sha256: ref.sha256,
          bytes: ref.bytes,
        )
      })
      |> result.map_error(fn(error) {
        CheckpointArtifactFailed(describe_artifact_error(error))
      })
    },
    write_recovery_artifact: fn(write) {
      artifact_store.write_recovery_artifact_json(
        store,
        write.run_id,
        write.step_id,
        write.failed_attempt_index,
        write.recovery_attempt_number,
        write.artifact_name,
        write.payload_json,
      )
      |> result.map(fn(ref) {
        RecoveryArtifactWritten(
          ref: ref.ref,
          path: ref.path,
          uri: ref.uri,
          display_path: ref.display_path,
          local_path: ref.local_path,
          sha256: ref.sha256,
          bytes: ref.bytes,
        )
      })
      |> result.map_error(fn(error) {
        CheckpointArtifactFailed(describe_artifact_error(error))
      })
    },
    read_artifact: fn(ref) {
      artifact_store.read_artifact_unverified(store, ref)
      |> result.map_error(fn(error) {
        CheckpointArtifactFailed(describe_artifact_error(error))
      })
    },
    artifact_location: fn(ref) {
      artifact_store.location(store, ref)
      |> result.map_error(fn(error) {
        CheckpointArtifactFailed(describe_artifact_error(error))
      })
    },
    snapshot_existing_artifact_ref: fn(
      artifact_ref,
      expected_sha256,
      expected_bytes,
      original_ref,
      media_type,
    ) {
      workstream_artifact_store.snapshot_existing_artifact_ref(
        store,
        artifact_ref,
        expected_sha256,
        expected_bytes,
        original_ref,
        media_type,
      )
      |> result.map_error(fn(error) {
        CheckpointArtifactFailed(describe_workstream_snapshot_error(error))
      })
    },
    snapshot_workstream_bytes: fn(original_path, media_type, contents) {
      workstream_artifact_store.snapshot_bytes(
        store,
        original_path,
        media_type,
        contents,
      )
      |> result.map_error(fn(error) {
        CheckpointArtifactFailed(describe_workstream_snapshot_error(error))
      })
    },
    append_workstream_record_idempotent: fn(ledger_record) {
      use ledger_path <- result.try(
        ledger.path_for_workspace_root(workspace_root)
        |> result.map_error(fn(error) {
          CheckpointAppendFailed(describe_ledger_error(error))
        }),
      )
      ledger.append_idempotent(ledger_path, ledger_record, True)
      |> result.map_error(fn(error) {
        case error {
          ledger.AppendLedgerError(ledger_error) ->
            CheckpointAppendFailed(describe_ledger_error(ledger_error))
          ledger.RecordIdConflict(record_id) ->
            CheckpointAppendFailed("record_id_conflict:" <> record_id)
        }
      })
    },
    append_workstream_start_records: fn(records, queued_record) {
      use ledger_path <- result.try(
        ledger.path_for_workspace_root(workspace_root)
        |> result.map_error(fn(error) {
          CheckpointAppendFailed(describe_ledger_error(error))
        }),
      )
      ledger.append_workstream_start_records(
        ledger_path,
        records,
        queued_record,
        True,
      )
      |> result.map_error(fn(error) {
        case error {
          ledger.AppendStartLedgerError(ledger_error) ->
            CheckpointAppendFailed(describe_ledger_error(ledger_error))
          ledger.AppendStartRecordIdConflict(record_id) ->
            CheckpointAppendFailed("record_id_conflict:" <> record_id)
          ledger.AppendStartInvalidQueueRecord ->
            CheckpointAppendFailed("invalid_workstream_start_queue_record")
        }
      })
    },
    write_workflow_inputs_manifest: fn(run_id, contents) {
      use existing <- result.try(existing_input_record(workspace_root, run_id))
      case existing {
        Some(artifact) -> Ok(artifact)
        None ->
          case reusable_input_manifest(store, run_id, contents) {
            Ok(Some(artifact)) -> Ok(artifact)
            Ok(None) ->
              artifact_store.write_input_manifest(store, run_id, contents)
              |> result.map(artifact_ref_to_written)
              |> result.map_error(fn(error) {
                CheckpointArtifactFailed(describe_artifact_error(error))
              })
            Error(error) -> Error(error)
          }
      }
    },
    workflow_inputs_recorded: fn(recorded) {
      use existing <- result.try(existing_input_record(
        workspace_root,
        recorded.run_id,
      ))
      case existing {
        Some(_) -> Ok(Nil)
        None ->
          append_body(
            workspace_root,
            now_ms,
            record.WorkflowRunInputsRecorded(
              recorded.run_id,
              recorded.workflow_id,
              recorded.workflow_fingerprint,
              recorded.artifact.ref,
              recorded.artifact.sha256,
              recorded.artifact.bytes,
            ),
          )
      }
    },
    write_workflow_outputs_manifest: fn(run_id, contents) {
      use existing <- result.try(existing_output_record(workspace_root, run_id))
      case existing {
        Some(artifact) -> Ok(artifact)
        None -> {
          use repair_generation <- result.try(current_repair_generation(
            workspace_root,
            run_id,
          ))
          case
            reusable_output_manifest(
              store,
              output_manifest_ref(run_id, repair_generation),
              repair_generation,
              contents,
            )
          {
            Ok(Some(artifact)) -> Ok(artifact)
            Ok(None) ->
              artifact_store.write_output_manifest_for_generation(
                store,
                run_id,
                repair_generation,
                contents,
              )
              |> result.map(artifact_ref_to_written)
              |> result.map_error(fn(error) {
                CheckpointArtifactFailed(describe_artifact_error(error))
              })
            Error(error) -> Error(error)
          }
        }
      }
    },
    workflow_outputs_recorded: fn(recorded) {
      use existing <- result.try(existing_output_record(
        workspace_root,
        recorded.run_id,
      ))
      case existing {
        Some(_) -> Ok(Nil)
        None ->
          append_body(
            workspace_root,
            now_ms,
            record.WorkflowRunOutputsRecorded(
              recorded.run_id,
              recorded.workflow_id,
              recorded.workflow_fingerprint,
              recorded.artifact.ref,
              recorded.artifact.sha256,
              recorded.artifact.bytes,
            ),
          )
      }
    },
    write_publication_manifest: fn(write) {
      let ref =
        artifact_publication_manifest.manifest_ref(
          write.run_id,
          write.publication_id,
          write.attempt_key,
        )
      case reusable_publication_manifest(store, ref, write.payload_json) {
        Ok(Some(artifact)) -> Ok(artifact)
        Ok(None) -> {
          let bytes = bit_array.from_string(write.payload_json)
          case
            artifact_store.write_immutable_artifact_bytes(store, ref, bytes)
          {
            Ok(artifact_store.ImmutableConflict) ->
              case
                reusable_publication_manifest(store, ref, write.payload_json)
              {
                Ok(Some(artifact)) -> Ok(artifact)
                Ok(None) ->
                  Error(CheckpointArtifactFailed(
                    "publication_manifest_conflict:" <> ref,
                  ))
                Error(error) -> Error(error)
              }
            Ok(_) ->
              Ok(ArtifactWritten(
                ref: ref,
                sha256: hash.sha256_hex(write.payload_json),
                bytes: bit_array.byte_size(bytes),
              ))
            Error(error) ->
              Error(CheckpointArtifactFailed(describe_artifact_error(error)))
          }
        }
        Error(error) -> Error(error)
      }
    },
    publication_attempt_recorded: fn(ledger_record) {
      use ledger_path <- result.try(
        ledger.path_for_workspace_root(workspace_root)
        |> result.map_error(fn(error) {
          CheckpointAppendFailed(describe_ledger_error(error))
        }),
      )
      ledger.append_idempotent(ledger_path, ledger_record, True)
      |> result.map_error(fn(error) {
        case error {
          ledger.AppendLedgerError(ledger_error) ->
            CheckpointAppendFailed(describe_ledger_error(ledger_error))
          ledger.RecordIdConflict(record_id) ->
            CheckpointAppendFailed("record_id_conflict:" <> record_id)
        }
      })
    },
    write_workflow_output_blob: fn(write) {
      use repair_generation <- result.try(current_repair_generation(
        workspace_root,
        write.run_id,
      ))
      artifact_store.write_output_blob_bytes_for_generation(
        store,
        write.run_id,
        write.output_name,
        write.extension,
        repair_generation,
        write.contents,
      )
      |> result.map(artifact_ref_to_written)
      |> result.map_error(fn(error) {
        CheckpointArtifactFailed(describe_artifact_error(error))
      })
    },
    step_finished: fn(finished, artifact_ref) {
      append_batch(
        workspace_root,
        now_ms,
        ledger_batch.step_attempt_finished(
          finished.run_id,
          finished.workflow_id,
          finished.step_id,
          finished.attempt_index,
          finished.outcome,
          artifact_ref.ref,
          artifact_ref.sha256,
          finished.workspace_name,
          finished.workspace_path,
          finished.token_total,
          finished.turns,
        ),
      )
    },
    step_recovery_started: fn(started) {
      append_batch(
        workspace_root,
        now_ms,
        ledger_batch.workflow_step_recovery_started(
          started.run_id,
          started.workflow_id,
          started.step_id,
          started.failed_attempt_index,
          started.recovery_attempt_number,
          started.recovery_session_id,
          started.model,
          started.prompt_ref,
        ),
      )
    },
    step_recovery_finished: fn(finished) {
      append_batch(
        workspace_root,
        now_ms,
        ledger_batch.workflow_step_recovery_finished(
          finished.run_id,
          finished.workflow_id,
          finished.step_id,
          finished.failed_attempt_index,
          finished.recovery_attempt_number,
          finished.recovery_session_id,
          finished.result,
          finished.summary,
          finished.reason,
          finished.retry_attempt_index,
        ),
      )
    },
    step_interrupted: fn(run_id, workflow_id, step_id, attempt_index, reason) {
      append_batch(
        workspace_root,
        now_ms,
        ledger_batch.step_attempt_interrupted(
          run_id,
          workflow_id,
          step_id,
          attempt_index,
          reason,
        ),
      )
    },
  )
}

pub fn linear_task_ref_for_issue(
  issue_id: String,
  issue_identifier: String,
  issue_url: Option(String),
) -> Option(record.TaskRefFields) {
  case string.trim(issue_id) == "" {
    True -> None
    False ->
      Some(record.linear_task_ref_fields(
        issue_id,
        Some(issue_identifier),
        issue_url,
      ))
  }
}

fn workflow_started_body(started: WorkflowStarted) -> record.RecordBody {
  case started.task_ref {
    Some(task_ref) ->
      record.WorkflowRunStartedWithTask(
        started.run_id,
        started.workflow_id,
        started.workflow_fingerprint,
        started.issue_id,
        started.issue_identifier,
        task_ref,
        started.issue_fingerprint,
        started.observed_updated_at_ms,
        started.run_root,
      )
    None ->
      record.WorkflowRunStarted(
        started.run_id,
        started.workflow_id,
        started.workflow_fingerprint,
        started.issue_id,
        started.issue_identifier,
        started.issue_fingerprint,
        started.observed_updated_at_ms,
        started.run_root,
      )
  }
}

fn workflow_finished_body(finished: WorkflowFinished) -> record.RecordBody {
  case finished.task_ref {
    Some(task_ref) ->
      record.WorkflowRunFinishedWithTask(
        finished.run_id,
        finished.workflow_id,
        finished.issue_id,
        task_ref,
        finished.outcome,
        finished.token_total,
        finished.turns,
      )
    None ->
      record.WorkflowRunFinished(
        finished.run_id,
        finished.workflow_id,
        finished.issue_id,
        finished.outcome,
        finished.token_total,
        finished.turns,
      )
  }
}

fn artifact_ref_to_written(ref: artifact_store.ArtifactRef) -> ArtifactWritten {
  ArtifactWritten(ref: ref.ref, sha256: ref.sha256, bytes: ref.bytes)
}

fn artifact_from_contents(ref: String, contents: String) -> ArtifactWritten {
  ArtifactWritten(
    ref: ref,
    sha256: hash.sha256_hex(contents),
    bytes: bit_array.byte_size(bit_array.from_string(contents)),
  )
}

fn existing_input_record(
  workspace_root: String,
  run_id: String,
) -> Result(Option(ArtifactWritten), CheckpointError) {
  use projection_state <- result.try(load_projection(workspace_root))
  Ok(case projection.workflow_input_manifest(projection_state, run_id) {
    Some(manifest) -> Some(manifest_ref_to_written(manifest))
    None -> None
  })
}

fn existing_output_record(
  workspace_root: String,
  run_id: String,
) -> Result(Option(ArtifactWritten), CheckpointError) {
  use projection_state <- result.try(load_projection(workspace_root))
  Ok(case projection.workflow_output_manifest(projection_state, run_id) {
    Some(manifest) -> Some(manifest_ref_to_written(manifest))
    None -> None
  })
}

fn current_repair_generation(
  workspace_root: String,
  run_id: String,
) -> Result(Int, CheckpointError) {
  use projection_state <- result.try(load_projection(workspace_root))
  Ok(case projection.latest_workflow_repair(projection_state, run_id) {
    Some(repair) -> repair.generation
    None -> 0
  })
}

fn load_projection(
  workspace_root: String,
) -> Result(projection.Projection, CheckpointError) {
  use ledger_path <- result.try(
    ledger.path_for_workspace_root(workspace_root)
    |> result.map_error(fn(error) {
      CheckpointAppendFailed(describe_ledger_error(error))
    }),
  )
  ledger.load_projection(ledger_path)
  |> result.map_error(fn(error) {
    CheckpointAppendFailed(describe_ledger_error(error))
  })
}

fn manifest_ref_to_written(
  manifest: projection.WorkflowContractManifestRef,
) -> ArtifactWritten {
  ArtifactWritten(
    ref: manifest.artifact_ref,
    sha256: manifest.artifact_sha256,
    bytes: manifest.artifact_bytes,
  )
}

fn reusable_input_manifest(
  store: artifact_store.Store,
  run_id: String,
  desired: String,
) -> Result(Option(ArtifactWritten), CheckpointError) {
  let ref = artifact_store.input_manifest_ref(run_id)
  case artifact_store.read_artifact_unverified(store, ref) {
    Error(artifact_store.MissingStepArtifact(_)) -> Ok(None)
    Error(error) ->
      Error(CheckpointArtifactFailed(describe_artifact_error(error)))
    Ok(existing) -> {
      let existing_sha = hash.sha256_hex(existing)
      let desired_sha = hash.sha256_hex(desired)
      case
        workflow_contract_manifest.decode_input_manifest(existing),
        workflow_contract_manifest.decode_input_manifest(desired)
      {
        Ok(existing_manifest), Ok(desired_manifest)
          if existing_manifest.run_id == desired_manifest.run_id
          && existing_manifest.workflow_id == desired_manifest.workflow_id
          && existing_manifest.workflow_fingerprint
          == desired_manifest.workflow_fingerprint
          && existing_sha == desired_sha
        -> Ok(Some(artifact_from_contents(ref, existing)))
        _, _ ->
          Error(CheckpointArtifactFailed(
            "existing_input_manifest_mismatch:" <> ref,
          ))
      }
    }
  }
}

fn output_manifest_ref(run_id: String, repair_generation: Int) -> String {
  artifact_store.output_manifest_ref_for_generation(run_id, repair_generation)
}

fn output_manifest_mismatch_code(repair_generation: Int) -> String {
  case repair_generation > 0 {
    True -> "existing_repaired_output_manifest_mismatch"
    False -> "existing_output_manifest_mismatch"
  }
}

fn reusable_output_manifest(
  store: artifact_store.Store,
  ref: String,
  repair_generation: Int,
  desired: String,
) -> Result(Option(ArtifactWritten), CheckpointError) {
  case artifact_store.read_artifact_unverified(store, ref) {
    Error(artifact_store.MissingStepArtifact(_)) -> Ok(None)
    Error(error) ->
      Error(CheckpointArtifactFailed(describe_artifact_error(error)))
    Ok(existing) -> {
      let existing_sha = hash.sha256_hex(existing)
      let desired_sha = hash.sha256_hex(desired)
      case
        workflow_contract_manifest.decode_output_manifest(existing),
        workflow_contract_manifest.decode_output_manifest(desired)
      {
        Ok(existing_manifest), Ok(desired_manifest)
          if existing_manifest.run_id == desired_manifest.run_id
          && existing_manifest.workflow_id == desired_manifest.workflow_id
          && existing_manifest.workflow_fingerprint
          == desired_manifest.workflow_fingerprint
          && existing_sha == desired_sha
        -> Ok(Some(artifact_from_contents(ref, existing)))
        _, _ ->
          Error(CheckpointArtifactFailed(
            output_manifest_mismatch_code(repair_generation) <> ":" <> ref,
          ))
      }
    }
  }
}

fn reusable_publication_manifest(
  store: artifact_store.Store,
  ref: String,
  desired: String,
) -> Result(Option(ArtifactWritten), CheckpointError) {
  case artifact_store.read_artifact_unverified(store, ref) {
    Error(artifact_store.MissingStepArtifact(_)) -> Ok(None)
    Error(error) ->
      Error(CheckpointArtifactFailed(describe_artifact_error(error)))
    Ok(existing) ->
      case publication_manifest_replay_equivalent(existing, desired) {
        True -> Ok(Some(artifact_from_contents(ref, existing)))
        False ->
          Error(CheckpointArtifactFailed(
            "publication_manifest_conflict:" <> ref,
          ))
      }
  }
}

fn publication_manifest_replay_equivalent(
  existing: String,
  desired: String,
) -> Bool {
  existing == desired
  || normalize_publication_generated_at(existing)
  == normalize_publication_generated_at(desired)
}

fn normalize_publication_generated_at(contents: String) -> String {
  case string.split_once(contents, on: "\"generated_at_ms\":") {
    Ok(#(before, generated_and_rest)) ->
      case string.split_once(generated_and_rest, on: ",\"dry_run_manifest\"") {
        Ok(#(_, rest)) -> before <> "\"dry_run_manifest\"" <> rest
        Error(_) -> contents
      }
    Error(_) -> contents
  }
}

pub fn step_outcome(
  artifact: step_artifact.StepArtifact,
  on_failure on_failure_continue: Bool,
) -> String {
  case step_artifact.succeeded(artifact.status) {
    True -> "completed"
    False ->
      case on_failure_continue {
        True -> "failed_continued"
        False -> "failed_fatal"
      }
  }
}

pub fn tokens_total(tokens: session_tokens.TokenTotals) -> Int {
  tokens.total
}

pub fn describe_error(error: CheckpointError) -> String {
  case error {
    CheckpointAppendFailed(reason) -> reason
    CheckpointArtifactFailed(reason) -> reason
  }
}

fn append_batch(
  workspace_root: String,
  now_ms: fn() -> Int,
  batch: ledger_batch.LedgerBatch,
) -> Result(Nil, CheckpointError) {
  case ledger_batch.to_bodies(batch) {
    [body] -> append_body(workspace_root, now_ms, body)
    bodies -> append_bodies(workspace_root, now_ms, bodies)
  }
}

fn append_body(
  workspace_root: String,
  now_ms: fn() -> Int,
  body: record.RecordBody,
) -> Result(Nil, CheckpointError) {
  use ledger_path <- result.try(
    ledger.path_for_workspace_root(workspace_root)
    |> result.map_error(fn(error) {
      CheckpointAppendFailed(describe_ledger_error(error))
    }),
  )
  ledger.append(ledger_path, record.new(now_ms(), 1, body), True)
  |> result.map_error(fn(error) {
    CheckpointAppendFailed(describe_ledger_error(error))
  })
}

fn append_bodies(
  workspace_root: String,
  now_ms: fn() -> Int,
  bodies: List(record.RecordBody),
) -> Result(Nil, CheckpointError) {
  use ledger_path <- result.try(
    ledger.path_for_workspace_root(workspace_root)
    |> result.map_error(fn(error) {
      CheckpointAppendFailed(describe_ledger_error(error))
    }),
  )
  let recs = list.index_map(bodies, fn(b, i) { record.new(now_ms(), i + 1, b) })
  ledger.append_many(ledger_path, recs, True)
  |> result.map_error(fn(error) {
    CheckpointAppendFailed(describe_ledger_error(error))
  })
}

fn describe_ledger_error(error: ledger.LedgerError) -> String {
  ledger.ledger_error_to_string(error)
}

fn describe_artifact_error(error: artifact_store.ArtifactError) -> String {
  case error {
    artifact_store.ArtifactIo(message) -> message
    artifact_store.ArtifactWriteFailed(error) ->
      artifact_store.artifact_write_error_to_string(error)
    artifact_store.MissingStepArtifact(ref) -> "missing_step_artifact:" <> ref
    artifact_store.CorruptStepArtifact(ref) -> "corrupt_step_artifact:" <> ref
    artifact_store.InvalidArtifactRef(ref) -> "invalid_artifact_ref:" <> ref
    artifact_store.DecodeArtifactFailed(reason) ->
      "decode_artifact_failed:" <> reason
    artifact_store.DirectorySyncUnsupported(reason) ->
      "directory_sync_unsupported:" <> reason
  }
}

fn describe_workstream_snapshot_error(
  error: workstream_artifact_store.SnapshotError,
) -> String {
  case error {
    workstream_artifact_store.InvalidOriginalPath ->
      "workstream_invalid_original_path"
    workstream_artifact_store.MissingSourcePath(path) ->
      "workstream_missing_source_path:" <> path
    workstream_artifact_store.SourcePathEscapesRepo(path) ->
      "workstream_source_path_escapes_repo:" <> path
    workstream_artifact_store.InvalidExistingArtifactRef(ref) ->
      "workstream_invalid_existing_artifact_ref:" <> ref
    workstream_artifact_store.MissingExistingArtifact(ref) ->
      "workstream_missing_existing_artifact:" <> ref
    workstream_artifact_store.ExistingArtifactMismatch(ref) ->
      "workstream_existing_artifact_mismatch:" <> ref
    workstream_artifact_store.SnapshotIo(reason) ->
      "workstream_snapshot_io:" <> reason
    workstream_artifact_store.SnapshotWriteConflict(ref) ->
      "workstream_snapshot_write_conflict:" <> ref
    workstream_artifact_store.CorruptSnapshot(ref) ->
      "workstream_corrupt_snapshot:" <> ref
  }
}
