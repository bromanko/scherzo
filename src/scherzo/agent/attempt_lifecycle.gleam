import gleam/option.{type Option, None, Some}
import scherzo/agent/context_exhaustion
import scherzo/agent/context_recovery_artifact
import scherzo/agent/context_recovery_prompt
import scherzo/agent/pi_event
import scherzo/agent/types
import scherzo/config as config_module
import scherzo/config/types as config_types
import scherzo/error
import scherzo/log
import scherzo/pi/client
import scherzo/session/tokens as session_tokens
import scherzo/state/artifact_store
import scherzo/workflow_attempt
import scherzo/workspace

pub fn cleanup(
  session: client.Session,
  issue_id: String,
  workspace_path: String,
  config: config_types.EffectiveConfig,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
) -> Nil {
  terminate(issue_id, session, emit_update)
  after_run(issue_id, workspace_path, config, emit_update)
}

pub fn terminate(
  issue_id: String,
  session: client.Session,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
) -> Nil {
  case client.terminate(session) {
    Ok(Nil) -> Nil
    Error(err) ->
      emit_update(
        issue_id,
        lifecycle_update_with_message(
          pi_event.PiTerminateFailed,
          Some(error.pi_rpc_detail(err)),
        ),
      )
  }
}

pub fn after_run(
  issue_id: String,
  workspace_path: String,
  config: config_types.EffectiveConfig,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
) -> Nil {
  case workspace.after_run(workspace_path, config.hooks) {
    workspace.AfterRunFailed(diagnostic) ->
      emit_update(
        issue_id,
        lifecycle_update_with_message(
          pi_event.AfterRunHookFailed,
          Some(log.redact(
            "after_run_hook_failed",
            diagnostic,
            config_module.resolved_secrets(config),
          )),
        ),
      )
    workspace.AfterRunSkipped | workspace.AfterRunSucceeded(_) -> Nil
  }
}

pub fn write_recovery_prompt(
  issue_id: String,
  workspace_root: String,
  context: workflow_attempt.StepAttemptContext,
  prompt: String,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
) -> Nil {
  artifact_write(
    issue_id,
    "attempt-2-recovery-prompt.md",
    context_recovery_artifact.write_recovery_prompt(
      artifact_store.new(workspace_root),
      context.run_id,
      context.workflow_id,
      context.step_id,
      context.attempt_index,
      prompt,
    ),
    emit_update,
  )
}

pub fn write_recovery_result(
  issue_id: String,
  workspace_root: String,
  context: workflow_attempt.StepAttemptContext,
  outcome: String,
  method: context_recovery_prompt.RecoveryMethod,
  recovery_exhausted: Bool,
  compaction_event_reasons: List(String),
  compaction_attempt: context_recovery_artifact.CompactionAttemptDiagnostic,
  final_failure: Option(context_recovery_artifact.RecoveryFailureDiagnostic),
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
) -> Nil {
  artifact_write(
    issue_id,
    "attempt-2-result.json",
    context_recovery_artifact.write_result(
      artifact_store.new(workspace_root),
      context.run_id,
      context.workflow_id,
      context.step_id,
      context.attempt_index,
      outcome,
      method,
      recovery_exhausted,
      compaction_event_reasons,
      compaction_attempt,
      final_failure,
    ),
    emit_update,
  )
}

pub fn write_recovery_result_refs(
  issue_id: String,
  workspace_root: String,
  context: workflow_attempt.StepAttemptContext,
  outcome: String,
  method: context_recovery_prompt.RecoveryMethod,
  recovery_exhausted: Bool,
  compaction_event_reasons: List(String),
  compaction_attempt: context_recovery_artifact.CompactionAttemptDiagnostic,
  final_failure: Option(context_recovery_artifact.RecoveryFailureDiagnostic),
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
) -> #(Option(String), Option(String)) {
  let result =
    context_recovery_artifact.write_result(
      artifact_store.new(workspace_root),
      context.run_id,
      context.workflow_id,
      context.step_id,
      context.attempt_index,
      outcome,
      method,
      recovery_exhausted,
      compaction_event_reasons,
      compaction_attempt,
      final_failure,
    )
  result_refs(issue_id, result, emit_update)
}

pub fn write_terminal_exhausted(
  issue_id: String,
  workspace_root: String,
  context: workflow_attempt.StepAttemptContext,
  recovery_attempt: Int,
  exhaustion: context_exhaustion.ContextExhaustion,
  prompt_excerpt_ref: String,
  method: context_recovery_prompt.RecoveryMethod,
  failure: context_recovery_artifact.RecoveryFailureDiagnostic,
  result_ref: Option(String),
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
) -> Nil {
  artifact_write(
    issue_id,
    "context-window-exhausted.json",
    context_recovery_artifact.write_terminal_exhausted(
      context_recovery_artifact.TerminalExhaustionInput(
        store: artifact_store.new(workspace_root),
        run_id: context.run_id,
        workflow_id: context.workflow_id,
        step_id: context.step_id,
        step_attempt_index: context.attempt_index,
        pi_attempt: recovery_attempt,
        context: exhaustion,
        prompt_excerpt_ref: prompt_excerpt_ref,
        recovery_method: method,
        failure: failure,
        result_ref: result_ref,
      ),
    ),
    emit_update,
  )
}

pub fn artifact_write(
  issue_id: String,
  artifact_name: String,
  result: Result(
    artifact_store.StructuredArtifactRef,
    artifact_store.ArtifactError,
  ),
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
) -> Nil {
  case result {
    Ok(_) -> Nil
    Error(err) ->
      emit_update(
        issue_id,
        lifecycle_update_with_message(
          pi_event.ContextRecoveryArtifactWriteFailed,
          Some(artifact_name <> ": " <> artifact_error_to_string(err)),
        ),
      )
  }
}

pub fn result_refs(
  issue_id: String,
  result: Result(
    artifact_store.StructuredArtifactRef,
    artifact_store.ArtifactError,
  ),
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
) -> #(Option(String), Option(String)) {
  case result {
    Ok(result_artifact) -> #(
      Some(result_artifact.ref),
      Some(result_artifact.display_path),
    )
    Error(err) -> {
      emit_update(
        issue_id,
        lifecycle_update_with_message(
          pi_event.ContextRecoveryArtifactWriteFailed,
          Some("attempt-2-result.json: " <> artifact_error_to_string(err)),
        ),
      )
      #(None, None)
    }
  }
}

pub fn artifact_error_to_string(err: artifact_store.ArtifactError) -> String {
  case err {
    artifact_store.ArtifactIo(message) -> "artifact_io: " <> message
    artifact_store.ArtifactWriteFailed(write_error) ->
      artifact_store.artifact_write_error_to_string(write_error)
    artifact_store.MissingStepArtifact(ref) -> "missing artifact: " <> ref
    artifact_store.CorruptStepArtifact(ref) -> "corrupt artifact: " <> ref
    artifact_store.InvalidArtifactRef(ref) -> "invalid artifact ref: " <> ref
    artifact_store.DecodeArtifactFailed(message) ->
      "decode artifact failed: " <> message
    artifact_store.DirectorySyncUnsupported(message) ->
      "directory sync unsupported: " <> message
  }
}

fn lifecycle_update_with_message(
  name: pi_event.PiEvent,
  message: Option(String),
) -> types.RunnerUpdate {
  types.RunnerPiUpdate(types.PiUpdate(
    event: name,
    message: message,
    raw_json: None,
    turn: None,
    request_id: None,
    method: None,
    pi_session_id: None,
    tokens: session_tokens.zero_token_totals(),
    tool_name: None,
    tool_input: None,
    tool_output: None,
    tool_status: None,
  ))
}
