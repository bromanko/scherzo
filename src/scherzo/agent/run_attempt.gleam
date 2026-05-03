import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/operator_control
import scherzo/agent/pi_event
import scherzo/agent/probe
import scherzo/agent/turn_loop
import scherzo/agent/types
import scherzo/agent/worker_command
import scherzo/config as config_module
import scherzo/domain
import scherzo/error
import scherzo/log
import scherzo/pi/client
import scherzo/pi/protocol
import scherzo/result_artifact
import scherzo/session/redaction
import scherzo/template
import scherzo/tracker
import scherzo/tracker/state as issue_state
import scherzo/workspace

const max_tool_text_chars = 4096

const tool_text_truncated_suffix = "… [truncated]"

type BeforeTurn {
  StartTurn(prompt_queue: List(String))
  ExitBeforeTurn(failure: types.WorkerFailure)
}

pub fn run_attempt(
  issue: domain.Issue,
  attempt: Option(Int),
  prompt_template: String,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.PiUpdate) -> Nil,
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
  issue: domain.Issue,
  attempt: Option(Int),
  prompt_template: String,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.PiUpdate) -> Nil,
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
  issue: domain.Issue,
  attempt: Option(Int),
  prompt_template: String,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.PiUpdate) -> Nil,
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
  issue: domain.Issue,
  prompt: String,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  on_command_ready: fn() -> Nil,
  workspace_path: String,
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  case config.pi.compatibility_probe {
    True -> {
      emit_update(issue.id, lifecycle_update(pi_event.ProbeStarted))
      case
        probe.probe(
          config.pi.command,
          workspace_path,
          config.pi.read_timeout_ms,
        )
      {
        Error(err) -> {
          let _ = workspace.after_run(workspace_path, config.hooks)
          Error(worker_failure(error.ProbeFailed(err), Some(workspace_path)))
        }
        Ok(Nil) -> {
          emit_update(issue.id, lifecycle_update(pi_event.ProbeFinished))
          run_pi_loop(
            issue,
            prompt,
            config,
            tracker_client,
            emit_update,
            command_subject,
            on_command_ready,
            workspace_path,
          )
        }
      }
    }
    False ->
      run_pi_loop(
        issue,
        prompt,
        config,
        tracker_client,
        emit_update,
        command_subject,
        on_command_ready,
        workspace_path,
      )
  }
}

fn run_prepared(
  issue: domain.Issue,
  attempt: Option(Int),
  prompt_template: String,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.PiUpdate) -> Nil,
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
          case
            probe.probe(
              config.pi.command,
              prepared.path,
              config.pi.read_timeout_ms,
            )
          {
            Error(err) -> {
              let _ = workspace.after_run(prepared.path, config.hooks)
              Error(worker_failure(error.ProbeFailed(err), Some(prepared.path)))
            }
            Ok(Nil) -> {
              emit_update(issue.id, lifecycle_update(pi_event.ProbeFinished))
              run_pi_loop(
                issue,
                prompt,
                config,
                tracker_client,
                emit_update,
                command_subject,
                on_command_ready,
                prepared.path,
              )
            }
          }
        }
        False ->
          run_pi_loop(
            issue,
            prompt,
            config,
            tracker_client,
            emit_update,
            command_subject,
            on_command_ready,
            prepared.path,
          )
      }
  }
}

fn run_pi_loop(
  issue: domain.Issue,
  first_prompt: String,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  on_command_ready: fn() -> Nil,
  workspace_path: String,
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  case
    client.launch(
      config.pi.command,
      workspace_path,
      issue.identifier <> ": " <> issue.title,
      config.pi.auto_retry,
      config.pi.read_timeout_ms,
    )
  {
    Error(err) -> {
      let _ = workspace.after_run(workspace_path, config.hooks)
      Error(worker_failure(error.PiFailed(err), Some(workspace_path)))
    }
    Ok(session) -> {
      emit_update(issue.id, pi_session_started_update(session.session_id))
      on_command_ready()
      loop_turns(
        session,
        issue,
        first_prompt,
        1,
        domain.zero_token_totals(),
        result_artifact.empty(),
        config,
        tracker_client,
        emit_update,
        command_subject,
        [],
        False,
        workspace_path,
      )
    }
  }
}

fn loop_turns(
  session: client.Session,
  issue: domain.Issue,
  prompt: String,
  turn: Int,
  totals: domain.TokenTotals,
  result: domain.ResultArtifact,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  stop_after_turn: Bool,
  workspace_path: String,
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
              fail_pi(
                session,
                issue.id,
                workspace_path,
                config,
                emit_update,
                prompt_queue,
                err,
                totals,
              )
            Ok(#(session, skipped)) -> {
              emit_records(
                issue.id,
                skipped,
                turn,
                config_module.resolved_secrets(config),
                emit_update,
              )
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
                Error(failure) -> Error(failure)
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
                  )
                }
              }
            }
          }
        }
      }
  }
}

fn finish_after_turn(
  session: client.Session,
  issue: domain.Issue,
  turn: Int,
  prior_totals: domain.TokenTotals,
  result: domain.ResultArtifact,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  stop_after_turn: Bool,
  workspace_path: String,
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
      )
    Ok(#(session, turn_tokens)) -> {
      let totals = add_tokens(prior_totals, turn_tokens)
      emit_update(issue.id, token_update(pi_event.TurnFinished, turn, totals))
      case tracker_client.fetch_issue_states_by_ids([issue.id]) {
        Error(err) -> {
          let _ = client.terminate(session)
          let _ = workspace.after_run(workspace_path, config.hooks)
          emit_dropped_prompts(
            issue.id,
            prompt_queue,
            config_module.resolved_secrets(config),
            emit_update,
          )
          Error(worker_failure_with(
            error.StateRefreshFailed(err),
            Some(workspace_path),
            totals,
            None,
          ))
        }
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
          )
      }
    }
  }
}

fn decide_after_refresh(
  session: client.Session,
  issue: domain.Issue,
  turn: Int,
  totals: domain.TokenTotals,
  result: domain.ResultArtifact,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  stop_after_turn: Bool,
  workspace_path: String,
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
        Some(issue),
      )
    False -> {
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
              )
          }
      }
    }
  }
}

fn finish_success(
  session: client.Session,
  issue: domain.Issue,
  classification: types.FinalClassification,
  workspace_path: String,
  totals: domain.TokenTotals,
  turns: Int,
  result: domain.ResultArtifact,
  config: domain.EffectiveConfig,
  emit_update: fn(String, types.PiUpdate) -> Nil,
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
  config: domain.EffectiveConfig,
  emit_update: fn(String, types.PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  totals: domain.TokenTotals,
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
  config: domain.EffectiveConfig,
  emit_update: fn(String, types.PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  state: operator_control.State,
  effects: List(operator_control.Effect),
  totals: domain.TokenTotals,
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
  config: domain.EffectiveConfig,
  emit_update: fn(String, types.PiUpdate) -> Nil,
  prompt_queue: List(String),
  totals: domain.TokenTotals,
  reply: process.Subject(worker_command.Reply),
) -> types.WorkerFailure {
  case client.send_abort(session, config.pi.read_timeout_ms) {
    Ok(#(_session, skipped)) -> {
      emit_records(
        issue_id,
        skipped,
        0,
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
  config: domain.EffectiveConfig,
  emit_update: fn(String, types.PiUpdate) -> Nil,
  prompt_queue: List(String),
  totals: domain.TokenTotals,
  final_issue: Option(domain.Issue),
) -> types.WorkerFailure {
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
  config: domain.EffectiveConfig,
  emit_update: fn(String, types.PiUpdate) -> Nil,
  prompt_queue: List(String),
  totals: domain.TokenTotals,
  final_issue: Option(domain.Issue),
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  Error(stop_failure(
    session,
    issue_id,
    workspace_path,
    config,
    emit_update,
    prompt_queue,
    totals,
    final_issue,
  ))
}

fn cleanup_failure(
  session: client.Session,
  issue_id: String,
  workspace_path: String,
  config: domain.EffectiveConfig,
  emit_update: fn(String, types.PiUpdate) -> Nil,
  prompt_queue: List(String),
  reason: error.AgentRunnerError,
  tokens: domain.TokenTotals,
  final_issue: Option(domain.Issue),
) -> types.WorkerFailure {
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
  config: domain.EffectiveConfig,
  emit_update: fn(String, types.PiUpdate) -> Nil,
  prompt_queue: List(String),
  err: error.PiRpcError,
  tokens: domain.TokenTotals,
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
  ))
}

fn worker_failure(
  reason: error.AgentRunnerError,
  workspace_path: Option(String),
) -> types.WorkerFailure {
  worker_failure_with(reason, workspace_path, domain.zero_token_totals(), None)
}

fn worker_failure_with(
  reason: error.AgentRunnerError,
  workspace_path: Option(String),
  tokens: domain.TokenTotals,
  final_issue: Option(domain.Issue),
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
  config: domain.EffectiveConfig,
  emit_update: fn(String, types.PiUpdate) -> Nil,
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
  emit_update: fn(String, types.PiUpdate) -> Nil,
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
  emit_update: fn(String, types.PiUpdate) -> Nil,
) -> Nil {
  case records {
    [] -> Nil
    [record, ..rest] -> {
      emit_update(issue_id, update_from_record(record, turn, secrets))
      emit_records(issue_id, rest, turn, secrets, emit_update)
    }
  }
}

fn lifecycle_update(name: pi_event.PiEvent) -> types.PiUpdate {
  lifecycle_update_with_message(name, None)
}

fn lifecycle_update_with_message(
  name: pi_event.PiEvent,
  message: Option(String),
) -> types.PiUpdate {
  types.PiUpdate(
    event: name,
    message: message,
    raw_json: None,
    turn: None,
    request_id: None,
    method: None,
    pi_session_id: None,
    tokens: domain.zero_token_totals(),
    tool_name: None,
    tool_input: None,
    tool_output: None,
    tool_status: None,
  )
}

fn pi_session_started_update(pi_session_id: Option(String)) -> types.PiUpdate {
  types.PiUpdate(
    event: pi_event.PiSessionStarted,
    message: None,
    raw_json: None,
    turn: None,
    request_id: None,
    method: None,
    pi_session_id: pi_session_id,
    tokens: domain.zero_token_totals(),
    tool_name: None,
    tool_input: None,
    tool_output: None,
    tool_status: None,
  )
}

fn token_update(
  name: pi_event.PiEvent,
  turn: Int,
  tokens: domain.TokenTotals,
) -> types.PiUpdate {
  types.PiUpdate(
    event: name,
    message: None,
    raw_json: None,
    turn: Some(turn),
    request_id: None,
    method: None,
    pi_session_id: None,
    tokens: tokens,
    tool_name: None,
    tool_input: None,
    tool_output: None,
    tool_status: None,
  )
}

fn update_from_record(
  record: protocol.RpcRecord,
  turn: Int,
  secrets: List(String),
) -> types.PiUpdate {
  let event = pi_event.from_string(record.type_)
  let message = case event {
    pi_event.ExtensionUiRequest -> record.message
    _ -> record.delta
  }
  types.PiUpdate(
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
  )
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
  config: domain.EffectiveConfig,
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
  a: domain.TokenTotals,
  b: domain.TokenTotals,
) -> domain.TokenTotals {
  domain.TokenTotals(
    input: a.input + b.input,
    output: a.output + b.output,
    cache_read: a.cache_read + b.cache_read,
    cache_write: a.cache_write + b.cache_write,
    total: a.total + b.total,
  )
}

@external(erlang, "scherzo_time_ffi", "monotonic_ms")
fn monotonic_ms() -> Int
