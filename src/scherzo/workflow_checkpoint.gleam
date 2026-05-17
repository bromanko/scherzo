import gleam/bit_array
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/hash
import scherzo/session/tokens as session_tokens
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/record
import scherzo/step_artifact
import scherzo/structured_output_metadata
import scherzo/workflow_attempt
import scherzo/workflow_contract_manifest
import scherzo/workflow_identity
import scherzo/workspace_run

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
    sha256: String,
    bytes: Int,
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
    contents: String,
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
    workflow_finished: fn(WorkflowFinished) -> Result(Nil, CheckpointError),
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
    write_workflow_inputs_manifest: fn(String, String) ->
      Result(ArtifactWritten, CheckpointError),
    workflow_inputs_recorded: fn(WorkflowContractManifestRecorded) ->
      Result(Nil, CheckpointError),
    write_workflow_outputs_manifest: fn(String, String) ->
      Result(ArtifactWritten, CheckpointError),
    workflow_outputs_recorded: fn(WorkflowContractManifestRecorded) ->
      Result(Nil, CheckpointError),
    write_workflow_output_blob: fn(WorkflowOutputBlobWrite) ->
      Result(ArtifactWritten, CheckpointError),
    step_finished: fn(StepFinished, ArtifactWritten) ->
      Result(Nil, CheckpointError),
    step_interrupted: fn(String, String, String, Int, String) ->
      Result(Nil, CheckpointError),
  )
}

pub fn noop_writer() -> Writer {
  Writer(
    workflow_finished: fn(_) { Ok(Nil) },
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
        sha256: "noop",
        bytes: 0,
      ))
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
    write_workflow_output_blob: fn(write) {
      Ok(ArtifactWritten(
        ref: "noop/"
          <> write.run_id
          <> "/outputs/"
          <> workflow_identity.safe_component(write.output_name, "output")
          <> write.extension,
        sha256: "noop",
        bytes: 0,
      ))
    },
    step_finished: fn(_, _) { Ok(Nil) },
    step_interrupted: fn(_, _, _, _, _) { Ok(Nil) },
  )
}

pub fn ledger_writer(workspace_root: String, now_ms: fn() -> Int) -> Writer {
  let store = artifact_store.new(workspace_root)
  Writer(
    workflow_finished: fn(finished) {
      append_body(workspace_root, now_ms, workflow_finished_body(finished))
    },
    step_prepared: fn(run_id, workflow_id, step_id, workspace) {
      append_body(
        workspace_root,
        now_ms,
        record.StepAttemptPrepared(
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
      append_body(
        workspace_root,
        now_ms,
        record.StepAttemptStarted(
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
      append_body(
        workspace_root,
        now_ms,
        record.StepAttemptContinuationStarted(
          run_id,
          workflow_id,
          step_id,
          attempt_index,
          session_id,
        ),
      )
    },
    step_pi_session_recorded: fn(observation) {
      append_body(
        workspace_root,
        now_ms,
        record.StepAttemptPiSessionRecordedWithTask(
          observation.run_id,
          observation.issue_id,
          observation.issue_identifier,
          record.legacy_linear_task_ref_fields(
            observation.issue_id,
            observation.issue_identifier,
          ),
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
          sha256: ref.sha256,
          bytes: ref.bytes,
        )
      })
      |> result.map_error(fn(error) {
        CheckpointArtifactFailed(describe_artifact_error(error))
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
        None ->
          case reusable_output_manifest(store, run_id, contents) {
            Ok(Some(artifact)) -> Ok(artifact)
            Ok(None) ->
              artifact_store.write_output_manifest(store, run_id, contents)
              |> result.map(artifact_ref_to_written)
              |> result.map_error(fn(error) {
                CheckpointArtifactFailed(describe_artifact_error(error))
              })
            Error(error) -> Error(error)
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
    write_workflow_output_blob: fn(write) {
      artifact_store.write_output_blob(
        store,
        write.run_id,
        write.output_name,
        write.extension,
        write.contents,
      )
      |> result.map(artifact_ref_to_written)
      |> result.map_error(fn(error) {
        CheckpointArtifactFailed(describe_artifact_error(error))
      })
    },
    step_finished: fn(finished, artifact_ref) {
      append_body(
        workspace_root,
        now_ms,
        record.StepAttemptFinished(
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
    step_interrupted: fn(run_id, workflow_id, step_id, attempt_index, reason) {
      append_body(
        workspace_root,
        now_ms,
        record.StepAttemptInterrupted(
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
  existing_manifest_record(workspace_root, run_id, input_record_artifact)
}

fn existing_output_record(
  workspace_root: String,
  run_id: String,
) -> Result(Option(ArtifactWritten), CheckpointError) {
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
    list.fold(read.records, None, fn(found, ledger_record) {
      case ledger_record.body {
        record.WorkflowRepairRequested(run_id: repaired_run_id, ..)
          if repaired_run_id == run_id
        -> None
        _ ->
          case output_record_artifact(ledger_record.body, run_id) {
            Some(artifact) -> Some(artifact)
            None -> found
          }
      }
    }),
  )
}

fn existing_manifest_record(
  workspace_root: String,
  run_id: String,
  selector: fn(record.RecordBody, String) -> Option(ArtifactWritten),
) -> Result(Option(ArtifactWritten), CheckpointError) {
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
    list.fold(read.records, None, fn(found, ledger_record) {
      case selector(ledger_record.body, run_id) {
        Some(artifact) -> Some(artifact)
        None -> found
      }
    }),
  )
}

fn input_record_artifact(
  body: record.RecordBody,
  run_id: String,
) -> Option(ArtifactWritten) {
  case body {
    record.WorkflowRunInputsRecorded(
      run_id: recorded_run_id,
      artifact_ref: ref,
      artifact_sha256: sha,
      artifact_bytes: bytes,
      ..,
    )
      if recorded_run_id == run_id
    -> Some(ArtifactWritten(ref: ref, sha256: sha, bytes: bytes))
    _ -> None
  }
}

fn output_record_artifact(
  body: record.RecordBody,
  run_id: String,
) -> Option(ArtifactWritten) {
  case body {
    record.WorkflowRunOutputsRecorded(
      run_id: recorded_run_id,
      artifact_ref: ref,
      artifact_sha256: sha,
      artifact_bytes: bytes,
      ..,
    )
      if recorded_run_id == run_id
    -> Some(ArtifactWritten(ref: ref, sha256: sha, bytes: bytes))
    _ -> None
  }
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

fn reusable_output_manifest(
  store: artifact_store.Store,
  run_id: String,
  desired: String,
) -> Result(Option(ArtifactWritten), CheckpointError) {
  let ref = artifact_store.output_manifest_ref(run_id)
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
            "existing_output_manifest_mismatch:" <> ref,
          ))
      }
    }
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

fn describe_ledger_error(error: ledger.LedgerError) -> String {
  case error {
    ledger.Io(message) -> message
    ledger.LedgerFfiFailed(error) -> ledger.ledger_ffi_error_to_string(error)
    ledger.UnsupportedVersion(version) ->
      "unsupported ledger schema version " <> int.to_string(version)
    ledger.CorruptRecord(line, reason) ->
      "corrupt ledger record at line " <> int.to_string(line) <> ": " <> reason
  }
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
