import gleam/int
import gleam/option.{type Option}
import gleam/result
import scherzo/session/tokens as session_tokens
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/record
import scherzo/step_artifact
import scherzo/workflow_attempt
import scherzo/workflow_identity
import scherzo/workspace_run

pub type CheckpointError {
  CheckpointAppendFailed(String)
  CheckpointArtifactFailed(String)
}

pub type ArtifactWritten {
  ArtifactWritten(ref: String, sha256: String, bytes: Int)
}

pub type WorkflowFinished {
  WorkflowFinished(
    run_id: String,
    workflow_id: String,
    issue_id: String,
    outcome: String,
    token_total: Int,
    turns: Int,
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
    step_finished: fn(_, _) { Ok(Nil) },
    step_interrupted: fn(_, _, _, _, _) { Ok(Nil) },
  )
}

pub fn ledger_writer(workspace_root: String, now_ms: fn() -> Int) -> Writer {
  let store = artifact_store.new(workspace_root)
  Writer(
    workflow_finished: fn(finished) {
      append_body(
        workspace_root,
        now_ms,
        record.WorkflowRunFinished(
          finished.run_id,
          finished.workflow_id,
          finished.issue_id,
          finished.outcome,
          finished.token_total,
          finished.turns,
        ),
      )
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
        record.StepAttemptPiSessionRecorded(
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

pub fn step_outcome(
  artifact: step_artifact.StepArtifact,
  on_failure_continue: Bool,
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
    ledger.UnsupportedVersion(version) ->
      "unsupported ledger schema version " <> int.to_string(version)
    ledger.CorruptRecord(line, reason) ->
      "corrupt ledger record at line " <> int.to_string(line) <> ": " <> reason
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
