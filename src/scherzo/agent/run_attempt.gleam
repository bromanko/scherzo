import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/context_exhaustion
import scherzo/agent/context_recovery_artifact
import scherzo/agent/context_recovery_prompt
import scherzo/agent/operator_control
import scherzo/agent/pi_diagnostic
import scherzo/agent/pi_event
import scherzo/agent/probe
import scherzo/agent/turn_loop
import scherzo/agent/types
import scherzo/agent/worker_command
import scherzo/config as config_module
import scherzo/config/types as config_types
import scherzo/error
import scherzo/log
import scherzo/pi/client
import scherzo/pi/command as pi_command
import scherzo/pi/protocol
import scherzo/result_artifact
import scherzo/session/redaction
import scherzo/session/tokens as session_tokens
import scherzo/state/artifact_store
import scherzo/template
import scherzo/tracker
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/turn_telemetry
import scherzo/workflow_attempt
import scherzo/workspace

pub type RunAttempt {
  RunAttempt(
    issue: tracker_issue.Issue,
    attempt: Option(Int),
    workspace_path: String,
  )
}

const max_tool_text_chars = 4096

const tool_text_truncated_suffix = "… [truncated]"

type BeforeTurn {
  StartTurn(prompt_queue: List(String))
  ExitBeforeTurn(failure: types.WorkerFailure)
}

pub fn run_attempt(
  issue: tracker_issue.Issue,
  attempt: Option(Int),
  prompt_template: String,
  config: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  let command_subject = process.new_subject()
  run_attempt_with_commands(
    issue,
    attempt,
    prompt_template,
    config,
    tracker_client,
    emit_update,
    command_subject,
  )
}

pub fn run_attempt_with_commands(
  issue: tracker_issue.Issue,
  attempt: Option(Int),
  prompt_template: String,
  config: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  run_attempt_with_command_ready(
    issue,
    attempt,
    prompt_template,
    config,
    tracker_client,
    emit_update,
    command_subject,
    fn() { Nil },
  )
}

pub fn run_attempt_with_command_ready(
  issue: tracker_issue.Issue,
  attempt: Option(Int),
  prompt_template: String,
  config: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  on_command_ready: fn() -> Nil,
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  case workspace.prepare(issue.identifier, config.workspace, config.hooks) {
    Error(workspace.WorkspaceFailure(err)) ->
      Error(worker_failure(error.WorkspaceFailed(err), None))
    Error(workspace.HookFailure(err)) ->
      Error(worker_failure(error.HookFailedError(err), None))
    Ok(prepared) ->
      run_prepared(
        issue,
        attempt,
        prompt_template,
        config,
        tracker_client,
        emit_update,
        command_subject,
        on_command_ready,
        prepared,
      )
  }
}

pub fn run_prompt_in_workspace(
  issue: tracker_issue.Issue,
  prompt: String,
  config: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  on_command_ready: fn() -> Nil,
  workspace_path: String,
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  run_prompt_mode_in_workspace(
    issue,
    workflow_attempt.OriginalPrompt(prompt),
    workflow_attempt.StepAttemptContext(
      run_id: "",
      issue_id: issue.id,
      issue_identifier: issue.identifier,
      workflow_id: "",
      workflow_fingerprint: "",
      step_id: "",
      workspace_name: "",
      attempt_index: 0,
      workspace_path: workspace_path,
      continuation_capable: False,
      continuation_session_file: None,
    ),
    config,
    tracker_client,
    emit_update,
    command_subject,
    on_command_ready,
    workspace_path,
    fn(_) { Nil },
  )
}

pub fn run_prompt_mode_in_workspace(
  issue: tracker_issue.Issue,
  prompt_mode: workflow_attempt.AgentPromptMode,
  attempt_context: workflow_attempt.StepAttemptContext,
  config: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  on_command_ready: fn() -> Nil,
  workspace_path: String,
  record_pi_session: fn(workflow_attempt.PiSessionObservation) -> Nil,
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  case config.pi.compatibility_probe {
    True -> {
      emit_update(issue.id, lifecycle_update(pi_event.ProbeStarted))
      case probe_for_config(config, workspace_path) {
        Error(err) -> {
          let _ = workspace.after_run(workspace_path, config.hooks)
          Error(worker_failure(error.ProbeFailed(err), Some(workspace_path)))
        }
        Ok(Nil) -> {
          emit_update(issue.id, lifecycle_update(pi_event.ProbeFinished))
          run_pi_loop(
            issue,
            prompt_mode,
            attempt_context,
            config,
            tracker_client,
            emit_update,
            command_subject,
            on_command_ready,
            workspace_path,
            record_pi_session,
          )
        }
      }
    }
    False ->
      run_pi_loop(
        issue,
        prompt_mode,
        attempt_context,
        config,
        tracker_client,
        emit_update,
        command_subject,
        on_command_ready,
        workspace_path,
        record_pi_session,
      )
  }
}

fn probe_for_config(
  config: config_types.EffectiveConfig,
  workspace_path: String,
) -> Result(Nil, error.PiRpcError) {
  case config.pi.session_persistence.enabled {
    False ->
      probe.probe(config.pi.command, workspace_path, config.pi.read_timeout_ms)
    True ->
      case pi_command.build_launch(config.pi, pi_command.FreshPersistent) {
        Error(_) -> Error(error.PiProtocolError("invalid persistent pi launch"))
        Ok(spec) ->
          case
            client.launch_spec(
              spec,
              workspace_path,
              "scherzo compatibility probe",
              False,
              config.pi.read_timeout_ms,
            )
          {
            Ok(session) -> {
              case
                client.get_session_stats(session, config.pi.read_timeout_ms)
              {
                Ok(#(session, _)) -> {
                  let _ = client.terminate(session)
                  Ok(Nil)
                }
                Error(err) -> {
                  let _ = client.terminate(session)
                  Error(err)
                }
              }
            }
            Error(err) -> Error(err)
          }
      }
  }
}

fn run_prepared(
  issue: tracker_issue.Issue,
  attempt: Option(Int),
  prompt_template: String,
  config: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  on_command_ready: fn() -> Nil,
  prepared: workspace.PreparedWorkspace,
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  case template.render(prompt_template, issue, attempt) {
    Error(err) -> {
      let _ = workspace.after_run(prepared.path, config.hooks)
      Error(worker_failure(error.PromptFailed(err), Some(prepared.path)))
    }
    Ok(prompt) ->
      case config.pi.compatibility_probe {
        True -> {
          emit_update(issue.id, lifecycle_update(pi_event.ProbeStarted))
          case probe_for_config(config, prepared.path) {
            Error(err) -> {
              let _ = workspace.after_run(prepared.path, config.hooks)
              Error(worker_failure(error.ProbeFailed(err), Some(prepared.path)))
            }
            Ok(Nil) -> {
              emit_update(issue.id, lifecycle_update(pi_event.ProbeFinished))
              run_pi_loop(
                issue,
                workflow_attempt.OriginalPrompt(prompt),
                workflow_attempt.StepAttemptContext(
                  run_id: "",
                  issue_id: issue.id,
                  issue_identifier: issue.identifier,
                  workflow_id: "",
                  workflow_fingerprint: "",
                  step_id: "",
                  workspace_name: "",
                  attempt_index: 0,
                  workspace_path: prepared.path,
                  continuation_capable: False,
                  continuation_session_file: None,
                ),
                config,
                tracker_client,
                emit_update,
                command_subject,
                on_command_ready,
                prepared.path,
                fn(_) { Nil },
              )
            }
          }
        }
        False ->
          run_pi_loop(
            issue,
            workflow_attempt.OriginalPrompt(prompt),
            workflow_attempt.StepAttemptContext(
              run_id: "",
              issue_id: issue.id,
              issue_identifier: issue.identifier,
              workflow_id: "",
              workflow_fingerprint: "",
              step_id: "",
              workspace_name: "",
              attempt_index: 0,
              workspace_path: prepared.path,
              continuation_capable: False,
              continuation_session_file: None,
            ),
            config,
            tracker_client,
            emit_update,
            command_subject,
            on_command_ready,
            prepared.path,
            fn(_) { Nil },
          )
      }
  }
}

fn launch_for_prompt_mode(
  mode: workflow_attempt.AgentPromptMode,
  attempt_context: workflow_attempt.StepAttemptContext,
  config: config_types.EffectiveConfig,
  workspace_path: String,
  issue: tracker_issue.Issue,
) -> Result(client.Session, error.PiRpcError) {
  case mode {
    workflow_attempt.OriginalPrompt(_)
    | workflow_attempt.StructuredOutputRetryPrompt(_)
    | workflow_attempt.StepRecoveryPrompt(_) -> {
      let launch_mode = case config.pi.session_persistence.enabled {
        True -> pi_command.FreshPersistent
        False -> pi_command.FreshNoSession
      }
      case pi_command.build_launch(config.pi, launch_mode) {
        Error(_) -> Error(error.PiProtocolError("invalid pi launch config"))
        Ok(spec) ->
          client.launch_spec(
            spec,
            workspace_path,
            issue.identifier <> ": " <> issue.title,
            config.pi.auto_retry,
            config.pi.read_timeout_ms,
          )
      }
    }
    workflow_attempt.RecoveryPrompt(_) ->
      case attempt_context.continuation_session_file {
        Some(session_file) ->
          launch_continuation(config, workspace_path, session_file)
        None -> Error(error.PiProtocolError("missing recorded pi session"))
      }
  }
}

fn launch_continuation(
  config: config_types.EffectiveConfig,
  workspace_path: String,
  session_file: String,
) -> Result(client.Session, error.PiRpcError) {
  case
    pi_command.build_launch(config.pi, pi_command.ContinueSession(session_file))
  {
    Error(_) -> Error(error.PiProtocolError("invalid pi continuation launch"))
    Ok(spec) ->
      client.reopen_session_for_continuation(
        spec,
        workspace_path,
        session_file,
        config.pi.read_timeout_ms,
      )
  }
}

fn prompt_text(mode: workflow_attempt.AgentPromptMode) -> String {
  case mode {
    workflow_attempt.OriginalPrompt(prompt) -> prompt
    workflow_attempt.StructuredOutputRetryPrompt(prompt) -> prompt
    workflow_attempt.StepRecoveryPrompt(prompt) -> prompt
    workflow_attempt.RecoveryPrompt(prompt) -> prompt
  }
}

fn record_session_observation(
  mode: workflow_attempt.AgentPromptMode,
  context: workflow_attempt.StepAttemptContext,
  session: client.Session,
  record_pi_session: fn(workflow_attempt.PiSessionObservation) -> Nil,
) -> Nil {
  let recordable_prompt = case mode {
    workflow_attempt.OriginalPrompt(_)
    | workflow_attempt.StructuredOutputRetryPrompt(_) -> True
    workflow_attempt.StepRecoveryPrompt(_)
    | workflow_attempt.RecoveryPrompt(_) -> False
  }
  case
    recordable_prompt,
    context.continuation_capable,
    session.session_id,
    session.session_file
  {
    True, True, Some(session_id), Some(session_file) ->
      case string.trim(session_id) == "" || string.trim(session_file) == "" {
        True -> Nil
        False ->
          record_pi_session(workflow_attempt.PiSessionObservation(
            run_id: context.run_id,
            issue_id: context.issue_id,
            issue_identifier: context.issue_identifier,
            workflow_id: context.workflow_id,
            workflow_fingerprint: context.workflow_fingerprint,
            step_id: context.step_id,
            workspace_name: context.workspace_name,
            attempt_index: context.attempt_index,
            workspace_path: context.workspace_path,
            session_id: session_id,
            session_file: session_file,
          ))
      }
    _, _, _, _ -> Nil
  }
}

fn run_pi_loop(
  issue: tracker_issue.Issue,
  first_prompt: workflow_attempt.AgentPromptMode,
  attempt_context: workflow_attempt.StepAttemptContext,
  config: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  on_command_ready: fn() -> Nil,
  workspace_path: String,
  record_pi_session: fn(workflow_attempt.PiSessionObservation) -> Nil,
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  case
    launch_for_prompt_mode(
      first_prompt,
      attempt_context,
      config,
      workspace_path,
      issue,
    )
  {
    Error(err) -> {
      let _ = workspace.after_run(workspace_path, config.hooks)
      Error(launch_worker_failure(first_prompt, err, workspace_path))
    }
    Ok(session) -> {
      emit_pi_session_started(issue.id, session, config, emit_update)
      record_session_observation(
        first_prompt,
        attempt_context,
        session,
        record_pi_session,
      )
      on_command_ready()
      loop_turns(
        session,
        issue,
        prompt_text(first_prompt),
        1,
        session_tokens.zero_token_totals(),
        result_artifact.empty(),
        config,
        tracker_client,
        emit_update,
        command_subject,
        [],
        False,
        workspace_path,
        attempt_context,
        0,
      )
    }
  }
}

fn launch_worker_failure(
  prompt_mode: workflow_attempt.AgentPromptMode,
  err: error.PiRpcError,
  workspace_path: String,
) -> types.WorkerFailure {
  case prompt_mode {
    workflow_attempt.RecoveryPrompt(_) ->
      worker_failure(
        error.PiFailed(error.PiProtocolError(
          workflow_attempt.recovery_pi_resume_validation_failed,
        )),
        Some(workspace_path),
      )
    workflow_attempt.OriginalPrompt(_)
    | workflow_attempt.StructuredOutputRetryPrompt(_)
    | workflow_attempt.StepRecoveryPrompt(_) ->
      worker_failure(error.PiFailed(err), Some(workspace_path))
  }
}

fn loop_turns(
  session: client.Session,
  issue: tracker_issue.Issue,
  prompt: String,
  turn: Int,
  totals: session_tokens.TokenTotals,
  result: result_artifact.ResultArtifact,
  config: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  stop_after_turn: Bool,
  workspace_path: String,
  attempt_context: workflow_attempt.StepAttemptContext,
  context_recovery_attempts: Int,
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  case stop_after_turn {
    True ->
      finish_operator_stop(
        session,
        issue.id,
        workspace_path,
        config,
        emit_update,
        prompt_queue,
        totals,
        None,
        None,
      )
    False ->
      case
        handle_between_turn_commands(
          session,
          issue.id,
          workspace_path,
          config,
          emit_update,
          command_subject,
          prompt_queue,
          totals,
        )
      {
        ExitBeforeTurn(failure) -> Error(failure)
        StartTurn(prompt_queue) -> {
          let #(prompt, prompt_queue, from_operator) =
            take_next_prompt(prompt_queue, prompt)
          case from_operator {
            True ->
              emit_update(
                issue.id,
                lifecycle_update_with_message(
                  pi_event.OperatorPromptSent,
                  Some(redact_operator_message(
                    prompt,
                    config_module.resolved_secrets(config),
                  )),
                ),
              )
            False -> Nil
          }
          case client.send_prompt(session, prompt, config.pi.read_timeout_ms) {
            Error(err) ->
              handle_context_or_fail(
                session,
                issue,
                prompt,
                turn,
                totals,
                result,
                config,
                tracker_client,
                emit_update,
                command_subject,
                prompt_queue,
                workspace_path,
                attempt_context,
                context_recovery_attempts,
                err,
                None,
              )
            Ok(#(session, skipped)) -> {
              emit_records(
                issue.id,
                skipped,
                turn,
                config_module.resolved_secrets(config),
                emit_update,
              )
              emit_update(issue.id, turn_started_update(turn))
              let now = monotonic_ms()
              let turn_deadline_ms = now + config.pi.turn_timeout_ms
              let stall_deadline_ms = now + config.pi.stall_timeout_ms
              let turn_context =
                turn_loop.Context(
                  issue_id: issue.id,
                  turn: turn,
                  totals: totals,
                  config: config,
                  emit_update: emit_update,
                  command_subject: command_subject,
                  turn_deadline_ms: turn_deadline_ms,
                  cleanup_failure: fn(
                    session,
                    issue_id,
                    prompt_queue,
                    reason,
                    tokens,
                    final_issue,
                  ) {
                    cleanup_failure(
                      session,
                      issue_id,
                      workspace_path,
                      config,
                      emit_update,
                      prompt_queue,
                      reason,
                      tokens,
                      final_issue,
                      Some(turn),
                    )
                  },
                  handle_abort: fn(
                    session,
                    issue_id,
                    prompt_queue,
                    totals,
                    reply,
                  ) {
                    handle_abort_command(
                      session,
                      issue_id,
                      workspace_path,
                      config,
                      emit_update,
                      prompt_queue,
                      totals,
                      reply,
                      Some(turn),
                    )
                  },
                )
              case
                turn_loop.run_active_turn(
                  turn_context,
                  session,
                  prompt_queue,
                  False,
                  None,
                  skipped,
                  stall_deadline_ms,
                )
              {
                Error(turn_loop.FinalFailure(failure)) -> Error(failure)
                Error(turn_loop.RecoverableContextExhaustion(
                  session: session,
                  prompt_queue: prompt_queue,
                  reason: err,
                  tokens: tokens,
                )) ->
                  handle_context_or_fail(
                    session,
                    issue,
                    prompt,
                    turn,
                    tokens,
                    result,
                    config,
                    tracker_client,
                    emit_update,
                    command_subject,
                    prompt_queue,
                    workspace_path,
                    attempt_context,
                    context_recovery_attempts,
                    err,
                    Some(turn),
                  )
                Ok(turn_loop.ActiveTurn(
                  session,
                  prompt_queue,
                  stop_after_turn,
                  records,
                )) -> {
                  let turn_result =
                    result_artifact.from_records(
                      records,
                      config_module.resolved_secrets(config),
                      config.handoff.result_max_chars,
                    )
                  let result =
                    result_artifact.append(
                      result,
                      turn_result,
                      config.handoff.result_max_chars,
                    )
                  finish_after_turn(
                    session,
                    issue,
                    turn,
                    totals,
                    result,
                    config,
                    tracker_client,
                    emit_update,
                    command_subject,
                    prompt_queue,
                    stop_after_turn,
                    workspace_path,
                    attempt_context,
                    context_recovery_attempts,
                  )
                }
              }
            }
          }
        }
      }
  }
}

fn handle_context_or_fail(
  session: client.Session,
  issue: tracker_issue.Issue,
  failed_prompt: String,
  turn: Int,
  totals: session_tokens.TokenTotals,
  result: result_artifact.ResultArtifact,
  config: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  workspace_path: String,
  attempt_context: workflow_attempt.StepAttemptContext,
  context_recovery_attempts: Int,
  err: error.PiRpcError,
  active_turn: Option(Int),
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  case context_exhaustion.from_pi_rpc_error(err) {
    None ->
      fail_pi(
        session,
        issue.id,
        workspace_path,
        config,
        emit_update,
        prompt_queue,
        err,
        totals,
        active_turn,
      )
    Some(context) ->
      case
        context_recovery_attempts >= config.agent.context_recovery_max_attempts
      {
        True ->
          Error(cleanup_failure(
            session,
            issue.id,
            workspace_path,
            config,
            emit_update,
            prompt_queue,
            error.PiFailed(err),
            totals,
            None,
            active_turn,
          ))
        False ->
          recover_context_exhaustion(
            session,
            issue,
            failed_prompt,
            turn,
            totals,
            result,
            config,
            tracker_client,
            emit_update,
            command_subject,
            prompt_queue,
            workspace_path,
            attempt_context,
            context_recovery_attempts + 1,
            context,
          )
      }
  }
}

fn recover_context_exhaustion(
  session: client.Session,
  issue: tracker_issue.Issue,
  failed_prompt: String,
  turn: Int,
  totals: session_tokens.TokenTotals,
  result: result_artifact.ResultArtifact,
  config: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  workspace_path: String,
  attempt_context: workflow_attempt.StepAttemptContext,
  recovery_attempt: Int,
  context: context_exhaustion.ContextExhaustion,
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  emit_update(
    issue.id,
    lifecycle_update_with_message(
      pi_event.ContextRecoveryStarted,
      Some(
        "context window exhausted; compacting Pi session before retrying step "
        <> attempt_context.step_id,
      ),
    ),
  )
  let store = artifact_store.new(config.workspace.root)
  let method = context_recovery_prompt.PiRpcCompact
  let artifacts_input =
    context_recovery_artifact.RecoveryArtifactInput(
      store: store,
      run_id: attempt_context.run_id,
      workflow_id: attempt_context.workflow_id,
      step_id: attempt_context.step_id,
      step_attempt_index: attempt_context.attempt_index,
      pi_attempt: recovery_attempt,
      context: context,
      original_prompt: failed_prompt,
      secrets: config_module.resolved_secrets(config),
      workspace_path: workspace_path,
      recovery_attempted: True,
      recovery_exhausted: False,
      recovery_method: method,
    )
  case context_recovery_artifact.write_initial(artifacts_input) {
    Error(_) ->
      Error(cleanup_failure(
        session,
        issue.id,
        workspace_path,
        config,
        emit_update,
        prompt_queue,
        error.PiFailed(error.PiProtocolError(
          "context recovery artifact write failed",
        )),
        totals,
        None,
        Some(turn),
      ))
    Ok(artifacts) ->
      try_compacted_recovery(
        session,
        issue,
        turn,
        totals,
        result,
        config,
        tracker_client,
        emit_update,
        command_subject,
        prompt_queue,
        workspace_path,
        attempt_context,
        recovery_attempt,
        context,
        artifacts,
      )
  }
}

fn try_compacted_recovery(
  session: client.Session,
  issue: tracker_issue.Issue,
  turn: Int,
  totals: session_tokens.TokenTotals,
  result: result_artifact.ResultArtifact,
  config: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  workspace_path: String,
  attempt_context: workflow_attempt.StepAttemptContext,
  recovery_attempt: Int,
  context: context_exhaustion.ContextExhaustion,
  artifacts: context_recovery_artifact.RecoveryArtifacts,
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  let instructions =
    "Compact prior context for Scherzo workflow step "
    <> attempt_context.step_id
    <> ". Preserve durable workspace state, completed edits, important artifact refs, and next actions."
  case
    client.compact_with_diagnostics(
      session,
      Some(instructions),
      config.pi.read_timeout_ms,
    )
  {
    Ok(#(session, skipped)) -> {
      emit_records(
        issue.id,
        skipped,
        turn,
        config_module.resolved_secrets(config),
        emit_update,
      )
      let reasons = context_recovery_artifact.compaction_event_reasons(skipped)
      let compaction_attempt =
        context_recovery_artifact.compaction_succeeded(
          reasons,
          context_recovery_artifact.compaction_event_count(skipped),
        )
      let prompt =
        context_recovery_artifact.build_recovery_prompt(
          issue,
          config,
          attempt_context,
          recovery_attempt,
          context_recovery_prompt.PiRpcCompact,
          reasons,
          artifacts,
        )
      let _ =
        context_recovery_artifact.write_recovery_prompt(
          artifact_store.new(config.workspace.root),
          attempt_context.run_id,
          attempt_context.workflow_id,
          attempt_context.step_id,
          attempt_context.attempt_index,
          prompt,
        )
      run_recovery_prompt(
        session,
        issue,
        prompt,
        turn,
        totals,
        result,
        config,
        tracker_client,
        emit_update,
        command_subject,
        prompt_queue,
        workspace_path,
        attempt_context,
        recovery_attempt,
        context,
        artifacts,
        context_recovery_prompt.PiRpcCompact,
        reasons,
        compaction_attempt,
      )
    }
    Error(failure) -> {
      emit_records(
        issue.id,
        failure.skipped,
        turn,
        config_module.resolved_secrets(config),
        emit_update,
      )
      let reasons =
        context_recovery_artifact.compaction_event_reasons(failure.skipped)
      let compaction_attempt =
        context_recovery_artifact.compaction_failed(
          failure.error,
          reasons,
          context_recovery_artifact.compaction_event_count(failure.skipped),
          failure.response,
          config_module.resolved_secrets(config),
          workspace_path,
        )
      fresh_session_recovery(
        session,
        issue,
        turn,
        totals,
        result,
        config,
        tracker_client,
        emit_update,
        command_subject,
        prompt_queue,
        workspace_path,
        attempt_context,
        recovery_attempt,
        context,
        artifacts,
        compaction_attempt,
      )
    }
  }
}

fn fresh_session_recovery(
  failed_session: client.Session,
  issue: tracker_issue.Issue,
  turn: Int,
  totals: session_tokens.TokenTotals,
  result: result_artifact.ResultArtifact,
  config: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  workspace_path: String,
  attempt_context: workflow_attempt.StepAttemptContext,
  recovery_attempt: Int,
  context: context_exhaustion.ContextExhaustion,
  artifacts: context_recovery_artifact.RecoveryArtifacts,
  compaction_attempt: context_recovery_artifact.CompactionAttemptDiagnostic,
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  emit_update(
    issue.id,
    lifecycle_update_with_message(
      pi_event.ContextRecoveryStarted,
      Some(context_recovery_artifact.fresh_session_recovery_message(
        attempt_context.step_id,
        compaction_attempt,
      )),
    ),
  )
  let _ = client.terminate(failed_session)
  let prompt =
    context_recovery_artifact.build_recovery_prompt(
      issue,
      config,
      attempt_context,
      recovery_attempt,
      context_recovery_prompt.FreshSession,
      compaction_attempt.event_reasons,
      artifacts,
    )
  let _ =
    context_recovery_artifact.write_recovery_prompt(
      artifact_store.new(config.workspace.root),
      attempt_context.run_id,
      attempt_context.workflow_id,
      attempt_context.step_id,
      attempt_context.attempt_index,
      prompt,
    )
  case
    launch_for_prompt_mode(
      workflow_attempt.OriginalPrompt(prompt),
      attempt_context,
      config,
      workspace_path,
      issue,
    )
  {
    Error(err) -> {
      let reason = error.PiFailed(err)
      let _ =
        context_recovery_artifact.write_result(
          artifact_store.new(config.workspace.root),
          attempt_context.run_id,
          attempt_context.workflow_id,
          attempt_context.step_id,
          attempt_context.attempt_index,
          "failed",
          context_recovery_prompt.FreshSession,
          context_failure_reason(reason),
          compaction_attempt.event_reasons,
          compaction_attempt,
          Some(context_recovery_artifact.failure_diagnostic(reason)),
        )
      let _ = workspace.after_run(workspace_path, config.hooks)
      Error(worker_failure_with(reason, Some(workspace_path), totals, None))
    }
    Ok(session) -> {
      emit_pi_session_started(issue.id, session, config, emit_update)
      run_recovery_prompt(
        session,
        issue,
        prompt,
        turn,
        totals,
        result,
        config,
        tracker_client,
        emit_update,
        command_subject,
        prompt_queue,
        workspace_path,
        attempt_context,
        recovery_attempt,
        context,
        artifacts,
        context_recovery_prompt.FreshSession,
        compaction_attempt.event_reasons,
        compaction_attempt,
      )
    }
  }
}

fn run_recovery_prompt(
  session: client.Session,
  issue: tracker_issue.Issue,
  prompt: String,
  turn: Int,
  totals: session_tokens.TokenTotals,
  result: result_artifact.ResultArtifact,
  config: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  workspace_path: String,
  attempt_context: workflow_attempt.StepAttemptContext,
  recovery_attempt: Int,
  context: context_exhaustion.ContextExhaustion,
  artifacts: context_recovery_artifact.RecoveryArtifacts,
  method: context_recovery_prompt.RecoveryMethod,
  compaction_event_reasons: List(String),
  compaction_attempt: context_recovery_artifact.CompactionAttemptDiagnostic,
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  let outcome =
    loop_turns(
      session,
      issue,
      prompt,
      turn + 1,
      totals,
      result,
      config,
      tracker_client,
      emit_update,
      command_subject,
      prompt_queue,
      False,
      workspace_path,
      attempt_context,
      recovery_attempt,
    )
  case outcome {
    Ok(success) -> {
      emit_update(
        issue.id,
        lifecycle_update_with_message(
          pi_event.ContextRecoverySucceeded,
          Some(
            "context recovery succeeded with "
            <> context_recovery_prompt.recovery_method_to_string(method),
          ),
        ),
      )
      let _ =
        context_recovery_artifact.write_result(
          artifact_store.new(config.workspace.root),
          attempt_context.run_id,
          attempt_context.workflow_id,
          attempt_context.step_id,
          attempt_context.attempt_index,
          "succeeded",
          method,
          False,
          compaction_event_reasons,
          compaction_attempt,
          None,
        )
      Ok(success)
    }
    Error(failure) -> {
      let store = artifact_store.new(config.workspace.root)
      let recovery_exhausted = context_failure_reason(failure.reason)
      let failure_diagnostic =
        context_recovery_artifact.failure_diagnostic(failure.reason)
      let result_write =
        context_recovery_artifact.write_result(
          store,
          attempt_context.run_id,
          attempt_context.workflow_id,
          attempt_context.step_id,
          attempt_context.attempt_index,
          "failed",
          method,
          recovery_exhausted,
          compaction_event_reasons,
          compaction_attempt,
          Some(failure_diagnostic),
        )
      let result_ref = case result_write {
        Ok(result_artifact) -> Some(result_artifact.ref)
        Error(_) -> None
      }
      let result_display_path = case result_write {
        Ok(result_artifact) -> Some(result_artifact.display_path)
        Error(_) -> None
      }
      case recovery_exhausted {
        True -> {
          let _ =
            context_recovery_artifact.write_terminal_exhausted(
              context_recovery_artifact.TerminalExhaustionInput(
                store: store,
                run_id: attempt_context.run_id,
                workflow_id: attempt_context.workflow_id,
                step_id: attempt_context.step_id,
                step_attempt_index: attempt_context.attempt_index,
                pi_attempt: recovery_attempt,
                context: context,
                prompt_excerpt_ref: artifacts.prompt_excerpt_ref,
                recovery_method: method,
                failure: failure_diagnostic,
                result_ref: result_ref,
              ),
            )
          Nil
        }
        False -> Nil
      }
      emit_update(
        issue.id,
        lifecycle_update_with_message(
          pi_event.ContextRecoveryFailed,
          Some(context_recovery_failed_message(
            method,
            recovery_exhausted,
            result_display_path,
          )),
        ),
      )
      Error(mark_context_recovery_exhausted(
        failure,
        method,
        artifacts.error_ref,
        result_ref,
      ))
    }
  }
}

fn mark_context_recovery_exhausted(
  failure: types.WorkerFailure,
  method: context_recovery_prompt.RecoveryMethod,
  context_artifact_ref: String,
  result_artifact_ref: Option(String),
) -> types.WorkerFailure {
  case failure.reason {
    error.PiFailed(pi_error) ->
      case context_exhaustion.from_pi_rpc_error(pi_error) {
        Some(_) ->
          types.WorkerFailure(
            ..failure,
            reason: error.ContextRecoveryExhausted(
              recovery_method: context_recovery_prompt.recovery_method_to_string(
                method,
              ),
              context_artifact_ref: Some(context_artifact_ref),
              result_artifact_ref: result_artifact_ref,
              final_error: pi_error,
            ),
          )
        None -> failure
      }
    _ -> failure
  }
}

fn context_failure_reason(reason: error.AgentRunnerError) -> Bool {
  case reason {
    error.ContextRecoveryExhausted(..) -> True
    error.PiFailed(pi_error) ->
      case context_exhaustion.from_pi_rpc_error(pi_error) {
        Some(_) -> True
        None -> False
      }
    _ -> False
  }
}

fn context_recovery_failed_message(
  method: context_recovery_prompt.RecoveryMethod,
  recovery_exhausted: Bool,
  result_display_path: Option(String),
) -> String {
  let method_text = context_recovery_prompt.recovery_method_to_string(method)
  let base = case recovery_exhausted {
    True ->
      "context recovery attempted but exhausted; outcome=failed recovery_exhausted=true recovery_method="
      <> method_text
    False ->
      "context recovery failed; outcome=failed recovery_exhausted=false recovery_method="
      <> method_text
  }
  base <> recovery_result_suffix(result_display_path)
}

fn recovery_result_suffix(result_display_path: Option(String)) -> String {
  case result_display_path {
    Some(result_display_path) -> " terminal_diagnostics=" <> result_display_path
    None -> ""
  }
}

fn finish_after_turn(
  session: client.Session,
  issue: tracker_issue.Issue,
  turn: Int,
  prior_totals: session_tokens.TokenTotals,
  result: result_artifact.ResultArtifact,
  config: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  stop_after_turn: Bool,
  workspace_path: String,
  attempt_context: workflow_attempt.StepAttemptContext,
  context_recovery_attempts: Int,
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  case client.get_session_stats(session, config.pi.read_timeout_ms) {
    Error(err) ->
      fail_pi(
        session,
        issue.id,
        workspace_path,
        config,
        emit_update,
        prompt_queue,
        err,
        prior_totals,
        Some(turn),
      )
    Ok(#(session, turn_tokens)) -> {
      let totals = add_tokens(prior_totals, turn_tokens)
      case tracker_client.fetch_issue_states_by_ids([issue.id]) {
        Error(err) ->
          Error(cleanup_failure(
            session,
            issue.id,
            workspace_path,
            config,
            emit_update,
            prompt_queue,
            error.StateRefreshFailed(err),
            totals,
            None,
            Some(turn),
          ))
        Ok([final_issue]) ->
          decide_after_refresh(
            session,
            final_issue,
            turn,
            totals,
            result,
            config,
            tracker_client,
            emit_update,
            command_subject,
            prompt_queue,
            stop_after_turn,
            workspace_path,
            attempt_context,
            context_recovery_attempts,
          )
        Ok(_) ->
          decide_after_refresh(
            session,
            issue,
            turn,
            totals,
            result,
            config,
            tracker_client,
            emit_update,
            command_subject,
            prompt_queue,
            stop_after_turn,
            workspace_path,
            attempt_context,
            context_recovery_attempts,
          )
      }
    }
  }
}

fn decide_after_refresh(
  session: client.Session,
  issue: tracker_issue.Issue,
  turn: Int,
  totals: session_tokens.TokenTotals,
  result: result_artifact.ResultArtifact,
  config: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  stop_after_turn: Bool,
  workspace_path: String,
  attempt_context: workflow_attempt.StepAttemptContext,
  context_recovery_attempts: Int,
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  case stop_after_turn {
    True ->
      finish_operator_stop(
        session,
        issue.id,
        workspace_path,
        config,
        emit_update,
        prompt_queue,
        totals,
        Some(turn),
        Some(issue),
      )
    False -> {
      emit_update(issue.id, turn_finished_update(turn, totals))
      let classification = classify(config, issue.state)
      case classification {
        types.FinalTerminal | types.FinalNonActive ->
          finish_success(
            session,
            issue,
            classification,
            workspace_path,
            totals,
            turn,
            result,
            config,
            emit_update,
            prompt_queue,
          )
        types.FinalActive ->
          case turn >= config.agent.max_turns {
            True ->
              finish_success(
                session,
                issue,
                types.FinalActive,
                workspace_path,
                totals,
                turn,
                result,
                config,
                emit_update,
                prompt_queue,
              )
            False ->
              loop_turns(
                session,
                issue,
                "Continue working on "
                  <> issue.identifier
                  <> ". Do not repeat the original task prompt; report progress or complete the remaining work.",
                turn + 1,
                totals,
                result,
                config,
                tracker_client,
                emit_update,
                command_subject,
                prompt_queue,
                False,
                workspace_path,
                attempt_context,
                context_recovery_attempts,
              )
          }
      }
    }
  }
}

fn finish_success(
  session: client.Session,
  issue: tracker_issue.Issue,
  classification: types.FinalClassification,
  workspace_path: String,
  totals: session_tokens.TokenTotals,
  turns: Int,
  result: result_artifact.ResultArtifact,
  config: config_types.EffectiveConfig,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  prompt_queue: List(String),
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  let _ = client.terminate(session)
  let _ = workspace.after_run(workspace_path, config.hooks)
  emit_dropped_prompts(
    issue.id,
    prompt_queue,
    config_module.resolved_secrets(config),
    emit_update,
  )
  Ok(types.WorkerSuccess(
    final_issue: Some(issue),
    final_classification: classification,
    workspace_path: workspace_path,
    tokens: totals,
    turns: turns,
    result: result,
  ))
}

fn handle_between_turn_commands(
  session: client.Session,
  issue_id: String,
  workspace_path: String,
  config: config_types.EffectiveConfig,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  totals: session_tokens.TokenTotals,
) -> BeforeTurn {
  case process.receive(command_subject, within: 0) {
    Error(_) -> StartTurn(prompt_queue)
    Ok(command) -> {
      let state = operator_control.from_parts(prompt_queue, False, None)
      let #(state, effects) =
        operator_control.handle_command(
          operator_control.BetweenTurns,
          state,
          command,
        )
      interpret_between_turn_effects(
        session,
        issue_id,
        workspace_path,
        config,
        emit_update,
        command_subject,
        state,
        effects,
        totals,
      )
    }
  }
}

fn interpret_between_turn_effects(
  session: client.Session,
  issue_id: String,
  workspace_path: String,
  config: config_types.EffectiveConfig,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  state: operator_control.State,
  effects: List(operator_control.Effect),
  totals: session_tokens.TokenTotals,
) -> BeforeTurn {
  case effects {
    [] ->
      handle_between_turn_commands(
        session,
        issue_id,
        workspace_path,
        config,
        emit_update,
        command_subject,
        state.prompt_queue,
        totals,
      )
    [effect, ..rest] ->
      case effect {
        operator_control.Reply(reply, reply_value) -> {
          process.send(reply, reply_value)
          interpret_between_turn_effects(
            session,
            issue_id,
            workspace_path,
            config,
            emit_update,
            command_subject,
            state,
            rest,
            totals,
          )
        }
        operator_control.EmitPromptQueued(message) -> {
          emit_operator_prompt_queued(issue_id, message, config, emit_update)
          interpret_between_turn_effects(
            session,
            issue_id,
            workspace_path,
            config,
            emit_update,
            command_subject,
            state,
            rest,
            totals,
          )
        }
        operator_control.AbortRequested(reply) -> {
          let failure =
            handle_abort_command(
              session,
              issue_id,
              workspace_path,
              config,
              emit_update,
              state.prompt_queue,
              totals,
              reply,
              None,
            )
          ExitBeforeTurn(failure)
        }
        operator_control.StopBeforeNextTurn(_) -> {
          let failure =
            stop_failure(
              session,
              issue_id,
              workspace_path,
              config,
              emit_update,
              state.prompt_queue,
              totals,
              None,
              None,
            )
          ExitBeforeTurn(failure)
        }
        operator_control.SendUiCancel(reply, _) -> {
          process.send(
            reply,
            worker_command.NotAllowed(
              "ui_request_not_pending",
              Some("no operator UI request is pending"),
            ),
          )
          interpret_between_turn_effects(
            session,
            issue_id,
            workspace_path,
            config,
            emit_update,
            command_subject,
            state,
            rest,
            totals,
          )
        }
        operator_control.SendUiValue(reply, _, _) -> {
          process.send(
            reply,
            worker_command.NotAllowed(
              "ui_request_not_pending",
              Some("no operator UI request is pending"),
            ),
          )
          interpret_between_turn_effects(
            session,
            issue_id,
            workspace_path,
            config,
            emit_update,
            command_subject,
            state,
            rest,
            totals,
          )
        }
      }
  }
}

fn handle_abort_command(
  session: client.Session,
  issue_id: String,
  workspace_path: String,
  config: config_types.EffectiveConfig,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  prompt_queue: List(String),
  totals: session_tokens.TokenTotals,
  reply: process.Subject(worker_command.Reply),
  turn: Option(Int),
) -> types.WorkerFailure {
  case client.send_abort(session, config.pi.read_timeout_ms) {
    Ok(#(_session, skipped)) -> {
      emit_records(
        issue_id,
        skipped,
        active_turn_or_zero(turn),
        config_module.resolved_secrets(config),
        emit_update,
      )
      emit_update(issue_id, lifecycle_update(pi_event.PiAbortSent))
      process.send(reply, worker_command.Applied(Some("abort sent")))
    }
    Error(err) -> {
      emit_update(
        issue_id,
        lifecycle_update_with_message(
          pi_event.PiAbortFailed,
          Some(error.pi_rpc_code(err)),
        ),
      )
      process.send(reply, worker_command.Applied(Some("abort requested")))
    }
  }
  emit_turn_stop_if_active(issue_id, emit_update, turn, totals)
  let _ = client.terminate(session)
  let _ = workspace.after_run(workspace_path, config.hooks)
  emit_dropped_prompts(
    issue_id,
    prompt_queue,
    config_module.resolved_secrets(config),
    emit_update,
  )
  worker_failure_with(error.OperatorAbort, Some(workspace_path), totals, None)
}

fn stop_failure(
  session: client.Session,
  issue_id: String,
  workspace_path: String,
  config: config_types.EffectiveConfig,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  prompt_queue: List(String),
  totals: session_tokens.TokenTotals,
  turn: Option(Int),
  final_issue: Option(tracker_issue.Issue),
) -> types.WorkerFailure {
  emit_turn_stop_after_turn_if_active(issue_id, emit_update, turn, totals)
  let _ = client.terminate(session)
  let _ = workspace.after_run(workspace_path, config.hooks)
  emit_dropped_prompts(
    issue_id,
    prompt_queue,
    config_module.resolved_secrets(config),
    emit_update,
  )
  worker_failure_with(
    error.OperatorStopAfterCurrentTurn,
    Some(workspace_path),
    totals,
    final_issue,
  )
}

fn finish_operator_stop(
  session: client.Session,
  issue_id: String,
  workspace_path: String,
  config: config_types.EffectiveConfig,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  prompt_queue: List(String),
  totals: session_tokens.TokenTotals,
  turn: Option(Int),
  final_issue: Option(tracker_issue.Issue),
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  Error(stop_failure(
    session,
    issue_id,
    workspace_path,
    config,
    emit_update,
    prompt_queue,
    totals,
    turn,
    final_issue,
  ))
}

fn cleanup_failure(
  session: client.Session,
  issue_id: String,
  workspace_path: String,
  config: config_types.EffectiveConfig,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  prompt_queue: List(String),
  reason: error.AgentRunnerError,
  tokens: session_tokens.TokenTotals,
  final_issue: Option(tracker_issue.Issue),
  turn: Option(Int),
) -> types.WorkerFailure {
  emit_turn_failure_if_active(issue_id, emit_update, turn, reason, tokens)
  emit_wrapper_failure_diagnostic(
    issue_id,
    session,
    config,
    emit_update,
    reason,
    turn,
  )
  let _ = client.terminate(session)
  let _ = workspace.after_run(workspace_path, config.hooks)
  emit_dropped_prompts(
    issue_id,
    prompt_queue,
    config_module.resolved_secrets(config),
    emit_update,
  )
  worker_failure_with(reason, Some(workspace_path), tokens, final_issue)
}

fn fail_pi(
  session: client.Session,
  issue_id: String,
  workspace_path: String,
  config: config_types.EffectiveConfig,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  prompt_queue: List(String),
  err: error.PiRpcError,
  tokens: session_tokens.TokenTotals,
  turn: Option(Int),
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  Error(cleanup_failure(
    session,
    issue_id,
    workspace_path,
    config,
    emit_update,
    prompt_queue,
    error.PiFailed(err),
    tokens,
    None,
    turn,
  ))
}

fn active_turn_or_zero(turn: Option(Int)) -> Int {
  case turn {
    Some(turn) -> turn
    None -> 0
  }
}

fn emit_turn_stop_if_active(
  issue_id: String,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  turn: Option(Int),
  totals: session_tokens.TokenTotals,
) -> Nil {
  case turn {
    Some(turn) ->
      emit_update(
        issue_id,
        turn_stopped_update(turn, turn_telemetry.ReasonOperatorAbort, totals),
      )
    None -> Nil
  }
}

fn emit_turn_stop_after_turn_if_active(
  issue_id: String,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  turn: Option(Int),
  totals: session_tokens.TokenTotals,
) -> Nil {
  case turn {
    Some(turn) ->
      emit_update(
        issue_id,
        turn_stopped_update(
          turn,
          turn_telemetry.ReasonOperatorStopAfterCurrentTurn,
          totals,
        ),
      )
    None -> Nil
  }
}

fn emit_turn_failure_if_active(
  issue_id: String,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  turn: Option(Int),
  reason: error.AgentRunnerError,
  totals: session_tokens.TokenTotals,
) -> Nil {
  case turn {
    None -> Nil
    Some(turn) ->
      case reason {
        error.ContextRecoveryExhausted(..) ->
          emit_update(
            issue_id,
            turn_failed_update(turn, turn_telemetry.ReasonPiError, totals),
          )
        error.PiFailed(error.PiStallTimeout) ->
          emit_update(
            issue_id,
            turn_timed_out_update(
              turn,
              turn_telemetry.ReasonPiStallTimeout,
              totals,
            ),
          )
        error.PiFailed(error.PiTurnTimeout) ->
          emit_update(
            issue_id,
            turn_timed_out_update(
              turn,
              turn_telemetry.ReasonPiTurnTimeout,
              totals,
            ),
          )
        error.PiFailed(_) ->
          emit_update(
            issue_id,
            turn_failed_update(turn, turn_telemetry.ReasonPiError, totals),
          )
        error.StateRefreshFailed(_) ->
          emit_update(
            issue_id,
            turn_failed_update(
              turn,
              turn_telemetry.ReasonStateRefreshFailed,
              totals,
            ),
          )
        _ -> Nil
      }
  }
}

fn worker_failure(
  reason: error.AgentRunnerError,
  workspace_path: Option(String),
) -> types.WorkerFailure {
  worker_failure_with(
    reason,
    workspace_path,
    session_tokens.zero_token_totals(),
    None,
  )
}

fn worker_failure_with(
  reason: error.AgentRunnerError,
  workspace_path: Option(String),
  tokens: session_tokens.TokenTotals,
  final_issue: Option(tracker_issue.Issue),
) -> types.WorkerFailure {
  types.WorkerFailure(
    reason: reason,
    workspace_path: workspace_path,
    tokens: tokens,
    final_issue: final_issue,
  )
}

fn take_next_prompt(
  prompt_queue: List(String),
  default_prompt: String,
) -> #(String, List(String), Bool) {
  case prompt_queue {
    [] -> #(default_prompt, [], False)
    [message, ..rest] -> #(message, rest, True)
  }
}

fn emit_operator_prompt_queued(
  issue_id: String,
  message: String,
  config: config_types.EffectiveConfig,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
) -> Nil {
  emit_update(
    issue_id,
    lifecycle_update_with_message(
      pi_event.OperatorPromptQueued,
      Some(redact_operator_message(
        message,
        config_module.resolved_secrets(config),
      )),
    ),
  )
}

fn emit_dropped_prompts(
  issue_id: String,
  prompt_queue: List(String),
  secrets: List(String),
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
) -> Nil {
  case prompt_queue {
    [] -> Nil
    [message, ..rest] -> {
      emit_update(
        issue_id,
        lifecycle_update_with_message(
          pi_event.OperatorPromptDropped,
          Some(redact_operator_message(message, secrets)),
        ),
      )
      emit_dropped_prompts(issue_id, rest, secrets, emit_update)
    }
  }
}

fn redact_operator_message(message: String, secrets: List(String)) -> String {
  log.redact("message", log.truncate(message, 200), secrets)
}

fn emit_records(
  issue_id: String,
  records: List(protocol.RpcRecord),
  turn: Int,
  secrets: List(String),
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
) -> Nil {
  case records {
    [] -> Nil
    [record, ..rest] -> {
      emit_update(issue_id, update_from_record(record, turn, secrets))
      emit_records(issue_id, rest, turn, secrets, emit_update)
    }
  }
}

pub fn turn_started_update(turn: Int) -> types.RunnerUpdate {
  turn_update(
    turn_telemetry.EventStarted,
    turn,
    session_tokens.zero_token_totals(),
    None,
  )
}

pub fn turn_finished_update(
  turn: Int,
  totals: session_tokens.TokenTotals,
) -> types.RunnerUpdate {
  turn_update(turn_telemetry.EventFinished, turn, totals, None)
}

pub fn turn_stopped_update(
  turn: Int,
  reason: turn_telemetry.TurnReason,
  totals: session_tokens.TokenTotals,
) -> types.RunnerUpdate {
  turn_update(turn_telemetry.EventStopped, turn, totals, Some(reason))
}

pub fn turn_failed_update(
  turn: Int,
  reason: turn_telemetry.TurnReason,
  totals: session_tokens.TokenTotals,
) -> types.RunnerUpdate {
  turn_update(turn_telemetry.EventFailed, turn, totals, Some(reason))
}

pub fn turn_timed_out_update(
  turn: Int,
  reason: turn_telemetry.TurnReason,
  totals: session_tokens.TokenTotals,
) -> types.RunnerUpdate {
  turn_update(turn_telemetry.EventTimedOut, turn, totals, Some(reason))
}

fn turn_update(
  name: turn_telemetry.TurnEventName,
  turn: Int,
  tokens: session_tokens.TokenTotals,
  reason: Option(turn_telemetry.TurnReason),
) -> types.RunnerUpdate {
  types.RunnerTurnUpdate(turn_telemetry.TurnLifecycleUpdate(
    name: name,
    turn: turn,
    tokens: tokens,
    reason: reason,
  ))
}

fn lifecycle_update(name: pi_event.PiEvent) -> types.RunnerUpdate {
  lifecycle_update_with_message(name, None)
}

fn lifecycle_update_with_message(
  name: pi_event.PiEvent,
  message: Option(String),
) -> types.RunnerUpdate {
  pi_runner_update(types.PiUpdate(
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

fn emit_pi_session_started(
  issue_id: String,
  session: client.Session,
  config: config_types.EffectiveConfig,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
) -> Nil {
  emit_update(issue_id, pi_session_started_update(session.session_id))
  emit_session_file_diagnostic(issue_id, session, config, emit_update)
}

fn emit_session_file_diagnostic(
  issue_id: String,
  session: client.Session,
  config: config_types.EffectiveConfig,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
) -> Nil {
  case pi_diagnostic.session_file_update(session, config) {
    Some(update) -> emit_update(issue_id, update)
    None -> Nil
  }
}

fn emit_wrapper_failure_diagnostic(
  issue_id: String,
  session: client.Session,
  config: config_types.EffectiveConfig,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  reason: error.AgentRunnerError,
  turn: Option(Int),
) -> Nil {
  case pi_diagnostic.wrapper_failure_update(session, reason, turn, config) {
    Some(update) -> emit_update(issue_id, update)
    None -> Nil
  }
}

fn pi_session_started_update(
  pi_session_id: Option(String),
) -> types.RunnerUpdate {
  pi_runner_update(types.PiUpdate(
    event: pi_event.PiSessionStarted,
    message: None,
    raw_json: None,
    turn: None,
    request_id: None,
    method: None,
    pi_session_id: pi_session_id,
    tokens: session_tokens.zero_token_totals(),
    tool_name: None,
    tool_input: None,
    tool_output: None,
    tool_status: None,
  ))
}

fn update_from_record(
  record: protocol.RpcRecord,
  turn: Int,
  secrets: List(String),
) -> types.RunnerUpdate {
  let event = pi_event.from_string(record.type_)
  let message = case event {
    pi_event.ExtensionUiRequest -> record.message
    _ -> record.delta
  }
  pi_runner_update(types.PiUpdate(
    event: event,
    message: redact_message(message, secrets),
    raw_json: Some(redaction.redact_raw_json(record.raw_json, secrets)),
    turn: Some(turn),
    request_id: record.id,
    method: record.method,
    pi_session_id: record.session_id,
    tokens: record.tokens,
    tool_name: record.tool_name,
    tool_input: normalize_tool_text(record.tool_input, secrets),
    tool_output: normalize_tool_text(record.tool_output, secrets),
    tool_status: normalize_tool_text(record.tool_status, secrets),
  ))
}

fn pi_runner_update(update: types.PiUpdate) -> types.RunnerUpdate {
  types.RunnerPiUpdate(update)
}

fn redact_message(
  message: Option(String),
  secrets: List(String),
) -> Option(String) {
  case message {
    Some(value) -> Some(log.redact("message", value, secrets))
    None -> None
  }
}

fn normalize_tool_text(
  value: Option(String),
  secrets: List(String),
) -> Option(String) {
  case value {
    None -> None
    Some(text) -> {
      let redacted = log.redact("tool", text, secrets)
      case string.length(redacted) > max_tool_text_chars {
        True ->
          Some(
            string.slice(redacted, at_index: 0, length: max_tool_text_chars)
            <> tool_text_truncated_suffix,
          )
        False -> Some(redacted)
      }
    }
  }
}

fn classify(
  config: config_types.EffectiveConfig,
  state: issue_state.IssueState,
) -> types.FinalClassification {
  case contains(config.tracker.terminal_states, state) {
    True -> types.FinalTerminal
    False ->
      case contains(config.tracker.active_states, state) {
        True -> types.FinalActive
        False -> types.FinalNonActive
      }
  }
}

fn contains(
  states: List(issue_state.IssueState),
  state: issue_state.IssueState,
) -> Bool {
  list.any(states, fn(s) { issue_state.equals_normalized(s, state) })
}

fn add_tokens(
  a: session_tokens.TokenTotals,
  b: session_tokens.TokenTotals,
) -> session_tokens.TokenTotals {
  session_tokens.TokenTotals(
    input: a.input + b.input,
    output: a.output + b.output,
    cache_read: a.cache_read + b.cache_read,
    cache_write: a.cache_write + b.cache_write,
    total: a.total + b.total,
  )
}

@external(erlang, "scherzo_time_ffi", "monotonic_ms")
fn monotonic_ms() -> Int
