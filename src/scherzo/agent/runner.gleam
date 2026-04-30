import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/pi_rpc
import scherzo/agent/probe
import scherzo/agent/worker_command
import scherzo/config as config_module
import scherzo/control/command
import scherzo/domain
import scherzo/error
import scherzo/log
import scherzo/result_artifact
import scherzo/session/event as session_event
import scherzo/session/redaction
import scherzo/template
import scherzo/tracker
import scherzo/workspace

const max_tool_text_chars = 4096

const tool_text_truncated_suffix = "… [truncated]"

pub type FinalClassification {
  FinalActive
  FinalTerminal
  FinalNonActive
}

pub type WorkerSuccess {
  WorkerSuccess(
    final_issue: Option(domain.Issue),
    final_classification: FinalClassification,
    workspace_path: String,
    tokens: domain.TokenTotals,
    turns: Int,
    result: domain.ResultArtifact,
  )
}

pub type WorkerFailure {
  WorkerFailure(
    reason: error.AgentRunnerError,
    workspace_path: Option(String),
    tokens: domain.TokenTotals,
    final_issue: Option(domain.Issue),
  )
}

pub type PiUpdate {
  PiUpdate(
    event: String,
    message: Option(String),
    raw_json: Option(session_event.RedactedRawJson),
    turn: Option(Int),
    request_id: Option(String),
    method: Option(String),
    pi_session_id: Option(String),
    tokens: domain.TokenTotals,
    tool_name: Option(String),
    tool_input: Option(String),
    tool_output: Option(String),
    tool_status: Option(String),
  )
}

type PendingUi {
  PendingUi(
    request_id: String,
    method: String,
    message: Option(String),
    created_at_ms: Int,
    deadline_ms: Int,
  )
}

type ActiveCommandState {
  ActiveCommandState(
    session: pi_rpc.Session,
    prompt_queue: List(String),
    stop_after_turn: Bool,
    pending_ui: Option(PendingUi),
    stall_deadline_ms: Int,
    records: List(pi_rpc.RpcRecord),
  )
}

type ActiveTurn {
  ActiveTurn(
    session: pi_rpc.Session,
    prompt_queue: List(String),
    stop_after_turn: Bool,
    records: List(pi_rpc.RpcRecord),
  )
}

type BeforeTurn {
  StartTurn(prompt_queue: List(String))
  ExitBeforeTurn(failure: WorkerFailure)
}

pub fn run_attempt(
  issue: domain.Issue,
  attempt: Option(Int),
  workflow: domain.WorkflowDefinition,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, PiUpdate) -> Nil,
) -> Result(WorkerSuccess, WorkerFailure) {
  let command_subject = process.new_subject()
  run_attempt_with_commands(
    issue,
    attempt,
    workflow,
    config,
    tracker_client,
    emit_update,
    command_subject,
  )
}

pub fn run_attempt_with_commands(
  issue: domain.Issue,
  attempt: Option(Int),
  workflow: domain.WorkflowDefinition,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
) -> Result(WorkerSuccess, WorkerFailure) {
  run_attempt_with_command_ready(
    issue,
    attempt,
    workflow,
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
  workflow: domain.WorkflowDefinition,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  on_command_ready: fn() -> Nil,
) -> Result(WorkerSuccess, WorkerFailure) {
  case workspace.prepare(issue.identifier, config.workspace, config.hooks) {
    Error(workspace.WorkspaceFailure(err)) ->
      Error(worker_failure(error.WorkspaceFailed(err), None))
    Error(workspace.HookFailure(err)) ->
      Error(worker_failure(error.HookFailedError(err), None))
    Ok(prepared) ->
      run_prepared(
        issue,
        attempt,
        workflow,
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
  emit_update: fn(String, PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  on_command_ready: fn() -> Nil,
  workspace_path: String,
) -> Result(WorkerSuccess, WorkerFailure) {
  case config.pi.compatibility_probe {
    True -> {
      emit_update(issue.id, lifecycle_update("probe_started"))
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
          emit_update(issue.id, lifecycle_update("probe_finished"))
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
  workflow: domain.WorkflowDefinition,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  on_command_ready: fn() -> Nil,
  prepared: workspace.PreparedWorkspace,
) -> Result(WorkerSuccess, WorkerFailure) {
  case template.render(workflow.prompt_template, issue, attempt) {
    Error(err) -> {
      let _ = workspace.after_run(prepared.path, config.hooks)
      Error(worker_failure(error.PromptFailed(err), Some(prepared.path)))
    }
    Ok(prompt) ->
      case config.pi.compatibility_probe {
        True -> {
          emit_update(issue.id, lifecycle_update("probe_started"))
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
              emit_update(issue.id, lifecycle_update("probe_finished"))
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
  emit_update: fn(String, PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  on_command_ready: fn() -> Nil,
  workspace_path: String,
) -> Result(WorkerSuccess, WorkerFailure) {
  case
    pi_rpc.launch(
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
  session: pi_rpc.Session,
  issue: domain.Issue,
  prompt: String,
  turn: Int,
  totals: domain.TokenTotals,
  result: domain.ResultArtifact,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  stop_after_turn: Bool,
  workspace_path: String,
) -> Result(WorkerSuccess, WorkerFailure) {
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
                  "operator_prompt_sent",
                  Some(redact_operator_message(
                    prompt,
                    config_module.resolved_secrets(config),
                  )),
                ),
              )
            False -> Nil
          }
          case pi_rpc.send_prompt(session, prompt, config.pi.read_timeout_ms) {
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
              case
                active_turn_loop(
                  session,
                  issue.id,
                  turn,
                  totals,
                  config,
                  emit_update,
                  command_subject,
                  prompt_queue,
                  False,
                  None,
                  skipped,
                  turn_deadline_ms,
                  stall_deadline_ms,
                  workspace_path,
                )
              {
                Error(failure) -> Error(failure)
                Ok(ActiveTurn(session, prompt_queue, stop_after_turn, records)) -> {
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
  session: pi_rpc.Session,
  issue: domain.Issue,
  turn: Int,
  prior_totals: domain.TokenTotals,
  result: domain.ResultArtifact,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  stop_after_turn: Bool,
  workspace_path: String,
) -> Result(WorkerSuccess, WorkerFailure) {
  case pi_rpc.get_session_stats(session, config.pi.read_timeout_ms) {
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
      emit_update(issue.id, token_update("turn_finished", turn, totals))
      case tracker_client.fetch_issue_states_by_ids([issue.id]) {
        Error(err) -> {
          let _ = pi_rpc.terminate(session)
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
  session: pi_rpc.Session,
  issue: domain.Issue,
  turn: Int,
  totals: domain.TokenTotals,
  result: domain.ResultArtifact,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  stop_after_turn: Bool,
  workspace_path: String,
) -> Result(WorkerSuccess, WorkerFailure) {
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
        FinalTerminal | FinalNonActive ->
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
        FinalActive ->
          case turn >= config.agent.max_turns {
            True ->
              finish_success(
                session,
                issue,
                FinalActive,
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
  session: pi_rpc.Session,
  issue: domain.Issue,
  classification: FinalClassification,
  workspace_path: String,
  totals: domain.TokenTotals,
  turns: Int,
  result: domain.ResultArtifact,
  config: domain.EffectiveConfig,
  emit_update: fn(String, PiUpdate) -> Nil,
  prompt_queue: List(String),
) -> Result(WorkerSuccess, WorkerFailure) {
  let _ = pi_rpc.terminate(session)
  let _ = workspace.after_run(workspace_path, config.hooks)
  emit_dropped_prompts(
    issue.id,
    prompt_queue,
    config_module.resolved_secrets(config),
    emit_update,
  )
  Ok(WorkerSuccess(
    final_issue: Some(issue),
    final_classification: classification,
    workspace_path: workspace_path,
    tokens: totals,
    turns: turns,
    result: result,
  ))
}

fn handle_between_turn_commands(
  session: pi_rpc.Session,
  issue_id: String,
  workspace_path: String,
  config: domain.EffectiveConfig,
  emit_update: fn(String, PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  totals: domain.TokenTotals,
) -> BeforeTurn {
  case process.receive(command_subject, within: 0) {
    Error(_) -> StartTurn(prompt_queue)
    Ok(command) ->
      case command {
        worker_command.Abort(reply) -> {
          let failure =
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
          ExitBeforeTurn(failure)
        }
        worker_command.StopAfterCurrentTurn(reply) -> {
          process.send(
            reply,
            worker_command.Applied(Some("stopped before next turn")),
          )
          let failure =
            stop_failure(
              session,
              issue_id,
              workspace_path,
              config,
              emit_update,
              prompt_queue,
              totals,
              None,
            )
          ExitBeforeTurn(failure)
        }
        worker_command.QueuePrompt(message, reply) -> {
          case operator_prompt_too_large(message) {
            True -> {
              process.send(
                reply,
                worker_command.Rejected(
                  "prompt_too_large",
                  Some("operator prompt is too large"),
                ),
              )
              handle_between_turn_commands(
                session,
                issue_id,
                workspace_path,
                config,
                emit_update,
                command_subject,
                prompt_queue,
                totals,
              )
            }
            False ->
              case list.length(prompt_queue) >= 10 {
                True -> {
                  process.send(
                    reply,
                    worker_command.Rejected(
                      "prompt_queue_full",
                      Some("prompt queue is full"),
                    ),
                  )
                  handle_between_turn_commands(
                    session,
                    issue_id,
                    workspace_path,
                    config,
                    emit_update,
                    command_subject,
                    prompt_queue,
                    totals,
                  )
                }
                False -> {
                  let prompt_queue = list.append(prompt_queue, [message])
                  emit_operator_prompt_queued(
                    issue_id,
                    message,
                    config,
                    emit_update,
                  )
                  process.send(
                    reply,
                    worker_command.Applied(Some("prompt accepted for next turn")),
                  )
                  handle_between_turn_commands(
                    session,
                    issue_id,
                    workspace_path,
                    config,
                    emit_update,
                    command_subject,
                    prompt_queue,
                    totals,
                  )
                }
              }
          }
        }
        worker_command.RespondToUi(_, response, reply) -> {
          case ui_response_too_large(response) {
            True ->
              process.send(
                reply,
                worker_command.Rejected(
                  "ui_response_too_large",
                  Some("operator UI response value is too large"),
                ),
              )
            False ->
              process.send(
                reply,
                worker_command.NotAllowed(
                  "ui_request_not_pending",
                  Some("no operator UI request is pending"),
                ),
              )
          }
          handle_between_turn_commands(
            session,
            issue_id,
            workspace_path,
            config,
            emit_update,
            command_subject,
            prompt_queue,
            totals,
          )
        }
      }
  }
}

fn active_turn_loop(
  session: pi_rpc.Session,
  issue_id: String,
  turn: Int,
  totals: domain.TokenTotals,
  config: domain.EffectiveConfig,
  emit_update: fn(String, PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  stop_after_turn: Bool,
  pending_ui: Option(PendingUi),
  turn_records: List(pi_rpc.RpcRecord),
  turn_deadline_ms: Int,
  stall_deadline_ms: Int,
  workspace_path: String,
) -> Result(ActiveTurn, WorkerFailure) {
  case process.receive(command_subject, within: 0) {
    Ok(command) -> {
      use state <- try_active(handle_active_command(
        command,
        session,
        issue_id,
        turn,
        totals,
        config,
        emit_update,
        prompt_queue,
        stop_after_turn,
        pending_ui,
        turn_records,
        stall_deadline_ms,
        workspace_path,
      ))
      active_turn_loop(
        state.session,
        issue_id,
        turn,
        totals,
        config,
        emit_update,
        command_subject,
        state.prompt_queue,
        state.stop_after_turn,
        state.pending_ui,
        state.records,
        turn_deadline_ms,
        state.stall_deadline_ms,
        workspace_path,
      )
    }
    Error(_) -> {
      let effective_stall_deadline = case pending_ui {
        Some(ui) -> ui.deadline_ms
        None -> stall_deadline_ms
      }
      case
        pi_rpc.read_turn_record(
          session,
          config.pi.read_timeout_ms,
          turn_deadline_ms,
          effective_stall_deadline,
        )
      {
        Error(error.PiStallTimeout) ->
          case pending_ui {
            Some(ui) ->
              handle_operator_ui_timeout(
                session,
                issue_id,
                turn,
                totals,
                config,
                emit_update,
                command_subject,
                prompt_queue,
                stop_after_turn,
                ui,
                turn_records,
                turn_deadline_ms,
                workspace_path,
              )
            None ->
              Error(cleanup_failure(
                session,
                issue_id,
                workspace_path,
                config,
                emit_update,
                prompt_queue,
                error.PiFailed(error.PiStallTimeout),
                totals,
                None,
              ))
          }
        Error(err) ->
          Error(cleanup_failure(
            session,
            issue_id,
            workspace_path,
            config,
            emit_update,
            prompt_queue,
            error.PiFailed(err),
            totals,
            None,
          ))
        Ok(#(session, None)) ->
          active_turn_loop(
            session,
            issue_id,
            turn,
            totals,
            config,
            emit_update,
            command_subject,
            prompt_queue,
            stop_after_turn,
            pending_ui,
            turn_records,
            turn_deadline_ms,
            stall_deadline_ms,
            workspace_path,
          )
        Ok(#(session, Some(record))) ->
          handle_turn_record(
            session,
            record,
            issue_id,
            turn,
            totals,
            config,
            emit_update,
            command_subject,
            prompt_queue,
            stop_after_turn,
            pending_ui,
            turn_records,
            turn_deadline_ms,
            stall_deadline_ms,
            workspace_path,
          )
      }
    }
  }
}

fn handle_active_command(
  command: worker_command.Command,
  session: pi_rpc.Session,
  issue_id: String,
  turn: Int,
  totals: domain.TokenTotals,
  config: domain.EffectiveConfig,
  emit_update: fn(String, PiUpdate) -> Nil,
  prompt_queue: List(String),
  stop_after_turn: Bool,
  pending_ui: Option(PendingUi),
  turn_records: List(pi_rpc.RpcRecord),
  stall_deadline_ms: Int,
  workspace_path: String,
) -> Result(ActiveCommandState, WorkerFailure) {
  case command {
    worker_command.Abort(reply) ->
      Error(handle_abort_command(
        session,
        issue_id,
        workspace_path,
        config,
        emit_update,
        prompt_queue,
        totals,
        reply,
      ))
    worker_command.StopAfterCurrentTurn(reply) -> {
      process.send(
        reply,
        worker_command.Queued(Some("stop requested after current turn")),
      )
      Ok(ActiveCommandState(
        session: session,
        prompt_queue: prompt_queue,
        stop_after_turn: True,
        pending_ui: pending_ui,
        stall_deadline_ms: stall_deadline_ms,
        records: turn_records,
      ))
    }
    worker_command.QueuePrompt(message, reply) -> {
      case operator_prompt_too_large(message) {
        True -> {
          process.send(
            reply,
            worker_command.Rejected(
              "prompt_too_large",
              Some("operator prompt is too large"),
            ),
          )
          Ok(ActiveCommandState(
            session: session,
            prompt_queue: prompt_queue,
            stop_after_turn: stop_after_turn,
            pending_ui: pending_ui,
            stall_deadline_ms: stall_deadline_ms,
            records: turn_records,
          ))
        }
        False ->
          case list.length(prompt_queue) >= 10 {
            True -> {
              process.send(
                reply,
                worker_command.Rejected(
                  "prompt_queue_full",
                  Some("prompt queue is full"),
                ),
              )
              Ok(ActiveCommandState(
                session: session,
                prompt_queue: prompt_queue,
                stop_after_turn: stop_after_turn,
                pending_ui: pending_ui,
                stall_deadline_ms: stall_deadline_ms,
                records: turn_records,
              ))
            }
            False -> {
              let prompt_queue = list.append(prompt_queue, [message])
              emit_operator_prompt_queued(
                issue_id,
                message,
                config,
                emit_update,
              )
              process.send(
                reply,
                worker_command.Queued(Some("prompt queued for next turn")),
              )
              Ok(ActiveCommandState(
                session: session,
                prompt_queue: prompt_queue,
                stop_after_turn: stop_after_turn,
                pending_ui: pending_ui,
                stall_deadline_ms: stall_deadline_ms,
                records: turn_records,
              ))
            }
          }
      }
    }
    worker_command.RespondToUi(request_id, response, reply) ->
      handle_ui_response_command(
        session,
        issue_id,
        turn,
        config,
        emit_update,
        prompt_queue,
        stop_after_turn,
        pending_ui,
        turn_records,
        stall_deadline_ms,
        request_id,
        response,
        reply,
      )
  }
}

fn handle_turn_record(
  session: pi_rpc.Session,
  record: pi_rpc.RpcRecord,
  issue_id: String,
  turn: Int,
  totals: domain.TokenTotals,
  config: domain.EffectiveConfig,
  emit_update: fn(String, PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  stop_after_turn: Bool,
  pending_ui: Option(PendingUi),
  turn_records: List(pi_rpc.RpcRecord),
  turn_deadline_ms: Int,
  stall_deadline_ms: Int,
  workspace_path: String,
) -> Result(ActiveTurn, WorkerFailure) {
  let secrets = config_module.resolved_secrets(config)
  emit_update(issue_id, update_from_record(record, turn, secrets))
  let turn_records = list.append(turn_records, [record])
  case record.type_ {
    "agent_end" ->
      case pending_ui {
        None ->
          Ok(ActiveTurn(session, prompt_queue, stop_after_turn, turn_records))
        Some(_) ->
          Error(cleanup_failure(
            session,
            issue_id,
            workspace_path,
            config,
            emit_update,
            prompt_queue,
            error.PiFailed(error.PiProtocolError(
              "agent ended with pending UI request",
            )),
            totals,
            None,
          ))
      }
    "extension_ui_request" ->
      handle_extension_ui_record(
        session,
        record,
        issue_id,
        turn,
        totals,
        config,
        emit_update,
        command_subject,
        prompt_queue,
        stop_after_turn,
        pending_ui,
        turn_records,
        turn_deadline_ms,
        stall_deadline_ms,
        workspace_path,
      )
    _ ->
      active_turn_loop(
        session,
        issue_id,
        turn,
        totals,
        config,
        emit_update,
        command_subject,
        prompt_queue,
        stop_after_turn,
        pending_ui,
        turn_records,
        turn_deadline_ms,
        monotonic_ms() + config.pi.stall_timeout_ms,
        workspace_path,
      )
  }
}

fn handle_extension_ui_record(
  session: pi_rpc.Session,
  record: pi_rpc.RpcRecord,
  issue_id: String,
  turn: Int,
  totals: domain.TokenTotals,
  config: domain.EffectiveConfig,
  emit_update: fn(String, PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  stop_after_turn: Bool,
  pending_ui: Option(PendingUi),
  turn_records: List(pi_rpc.RpcRecord),
  turn_deadline_ms: Int,
  stall_deadline_ms: Int,
  workspace_path: String,
) -> Result(ActiveTurn, WorkerFailure) {
  case is_blocking_ui_method(record.method) {
    False ->
      active_turn_loop(
        session,
        issue_id,
        turn,
        totals,
        config,
        emit_update,
        command_subject,
        prompt_queue,
        stop_after_turn,
        pending_ui,
        turn_records,
        turn_deadline_ms,
        monotonic_ms() + config.pi.stall_timeout_ms,
        workspace_path,
      )
    True ->
      case pending_ui {
        Some(_) ->
          Error(cleanup_failure(
            session,
            issue_id,
            workspace_path,
            config,
            emit_update,
            prompt_queue,
            error.PiFailed(error.PiProtocolError("nested operator UI request")),
            totals,
            None,
          ))
        None ->
          case record.id, record.method {
            Some(request_id), Some(method) ->
              handle_blocking_ui_policy(
                session,
                record,
                request_id,
                method,
                issue_id,
                turn,
                totals,
                config,
                emit_update,
                command_subject,
                prompt_queue,
                stop_after_turn,
                turn_records,
                turn_deadline_ms,
                stall_deadline_ms,
                workspace_path,
              )
            _, _ ->
              Error(cleanup_failure(
                session,
                issue_id,
                workspace_path,
                config,
                emit_update,
                prompt_queue,
                error.PiFailed(error.PiProtocolError(
                  "extension UI request missing id",
                )),
                totals,
                None,
              ))
          }
      }
  }
}

fn handle_blocking_ui_policy(
  session: pi_rpc.Session,
  record: pi_rpc.RpcRecord,
  request_id: String,
  method: String,
  issue_id: String,
  turn: Int,
  totals: domain.TokenTotals,
  config: domain.EffectiveConfig,
  emit_update: fn(String, PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  stop_after_turn: Bool,
  turn_records: List(pi_rpc.RpcRecord),
  turn_deadline_ms: Int,
  stall_deadline_ms: Int,
  workspace_path: String,
) -> Result(ActiveTurn, WorkerFailure) {
  case config.pi.ui_request_policy {
    domain.Fail ->
      Error(cleanup_failure(
        session,
        issue_id,
        workspace_path,
        config,
        emit_update,
        prompt_queue,
        error.PiFailed(error.PiProtocolError(
          "extension UI request blocked by policy",
        )),
        totals,
        None,
      ))
    domain.Ignore ->
      active_turn_loop(
        session,
        issue_id,
        turn,
        totals,
        config,
        emit_update,
        command_subject,
        prompt_queue,
        stop_after_turn,
        None,
        turn_records,
        turn_deadline_ms,
        monotonic_ms() + config.pi.stall_timeout_ms,
        workspace_path,
      )
    domain.Cancel -> {
      case
        pi_rpc.send_extension_ui_cancel(
          session,
          request_id,
          config.pi.read_timeout_ms,
        )
      {
        Ok(#(session, skipped)) -> {
          emit_records(
            issue_id,
            skipped,
            turn,
            config_module.resolved_secrets(config),
            emit_update,
          )
          emit_update(
            issue_id,
            lifecycle_update_with_request(
              "extension_ui_response",
              Some("cancelled"),
              request_id,
              method,
              turn,
            ),
          )
          let turn_records = list.append(turn_records, skipped)
          active_turn_loop(
            session,
            issue_id,
            turn,
            totals,
            config,
            emit_update,
            command_subject,
            prompt_queue,
            stop_after_turn,
            None,
            turn_records,
            turn_deadline_ms,
            monotonic_ms() + config.pi.stall_timeout_ms,
            workspace_path,
          )
        }
        Error(err) ->
          Error(cleanup_failure(
            session,
            issue_id,
            workspace_path,
            config,
            emit_update,
            prompt_queue,
            error.PiFailed(err),
            totals,
            None,
          ))
      }
    }
    domain.Operator -> {
      let now = monotonic_ms()
      let pending_ui =
        PendingUi(
          request_id: request_id,
          method: method,
          message: record.message,
          created_at_ms: now,
          deadline_ms: now + config.pi.ui_request_timeout_ms,
        )
      let _ = pending_ui.message
      let _ = pending_ui.created_at_ms
      active_turn_loop(
        session,
        issue_id,
        turn,
        totals,
        config,
        emit_update,
        command_subject,
        prompt_queue,
        stop_after_turn,
        Some(pending_ui),
        turn_records,
        turn_deadline_ms,
        stall_deadline_ms,
        workspace_path,
      )
    }
  }
}

fn handle_ui_response_command(
  session: pi_rpc.Session,
  issue_id: String,
  turn: Int,
  config: domain.EffectiveConfig,
  emit_update: fn(String, PiUpdate) -> Nil,
  prompt_queue: List(String),
  stop_after_turn: Bool,
  pending_ui: Option(PendingUi),
  turn_records: List(pi_rpc.RpcRecord),
  stall_deadline_ms: Int,
  request_id: String,
  response: command.UiResponse,
  reply: process.Subject(worker_command.Reply),
) -> Result(ActiveCommandState, WorkerFailure) {
  case ui_response_too_large(response) {
    True -> {
      process.send(
        reply,
        worker_command.Rejected(
          "ui_response_too_large",
          Some("operator UI response value is too large"),
        ),
      )
      Ok(ActiveCommandState(
        session: session,
        prompt_queue: prompt_queue,
        stop_after_turn: stop_after_turn,
        pending_ui: pending_ui,
        stall_deadline_ms: stall_deadline_ms,
        records: turn_records,
      ))
    }
    False ->
      case pending_ui {
        None -> {
          process.send(
            reply,
            worker_command.NotAllowed(
              "ui_request_not_pending",
              Some("no operator UI request is pending"),
            ),
          )
          Ok(ActiveCommandState(
            session: session,
            prompt_queue: prompt_queue,
            stop_after_turn: stop_after_turn,
            pending_ui: pending_ui,
            stall_deadline_ms: stall_deadline_ms,
            records: turn_records,
          ))
        }
        Some(ui) ->
          case ui.request_id == request_id {
            False -> {
              process.send(
                reply,
                worker_command.Rejected(
                  "ui_request_not_pending",
                  Some("that UI request is not pending"),
                ),
              )
              Ok(ActiveCommandState(
                session: session,
                prompt_queue: prompt_queue,
                stop_after_turn: stop_after_turn,
                pending_ui: Some(ui),
                stall_deadline_ms: stall_deadline_ms,
                records: turn_records,
              ))
            }
            True -> {
              let sent = case response {
                command.UiCancel ->
                  pi_rpc.send_extension_ui_cancel(
                    session,
                    request_id,
                    config.pi.read_timeout_ms,
                  )
                command.UiValue(value) ->
                  pi_rpc.send_extension_ui_value(
                    session,
                    request_id,
                    value,
                    config.pi.read_timeout_ms,
                  )
              }
              case sent {
                Error(err) -> {
                  process.send(
                    reply,
                    worker_command.Rejected(
                      "ui_response_failed",
                      Some(error.pi_rpc_code(err)),
                    ),
                  )
                  Ok(ActiveCommandState(
                    session: session,
                    prompt_queue: prompt_queue,
                    stop_after_turn: stop_after_turn,
                    pending_ui: Some(ui),
                    stall_deadline_ms: stall_deadline_ms,
                    records: turn_records,
                  ))
                }
                Ok(#(session, skipped)) -> {
                  emit_records(
                    issue_id,
                    skipped,
                    turn,
                    config_module.resolved_secrets(config),
                    emit_update,
                  )
                  process.send(
                    reply,
                    worker_command.Applied(Some("ui response sent")),
                  )
                  emit_update(
                    issue_id,
                    lifecycle_update_with_request(
                      "extension_ui_response",
                      Some("operator response sent"),
                      request_id,
                      ui.method,
                      turn,
                    ),
                  )
                  let turn_records = list.append(turn_records, skipped)
                  Ok(ActiveCommandState(
                    session: session,
                    prompt_queue: prompt_queue,
                    stop_after_turn: stop_after_turn,
                    pending_ui: None,
                    stall_deadline_ms: monotonic_ms()
                      + config.pi.stall_timeout_ms,
                    records: turn_records,
                  ))
                }
              }
            }
          }
      }
  }
}

fn handle_operator_ui_timeout(
  session: pi_rpc.Session,
  issue_id: String,
  turn: Int,
  totals: domain.TokenTotals,
  config: domain.EffectiveConfig,
  emit_update: fn(String, PiUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  prompt_queue: List(String),
  stop_after_turn: Bool,
  ui: PendingUi,
  turn_records: List(pi_rpc.RpcRecord),
  turn_deadline_ms: Int,
  workspace_path: String,
) -> Result(ActiveTurn, WorkerFailure) {
  case
    pi_rpc.send_extension_ui_cancel(
      session,
      ui.request_id,
      config.pi.read_timeout_ms,
    )
  {
    Error(err) ->
      Error(cleanup_failure(
        session,
        issue_id,
        workspace_path,
        config,
        emit_update,
        prompt_queue,
        error.PiFailed(err),
        totals,
        None,
      ))
    Ok(#(session, skipped)) -> {
      emit_records(
        issue_id,
        skipped,
        turn,
        config_module.resolved_secrets(config),
        emit_update,
      )
      emit_update(
        issue_id,
        lifecycle_update_with_request(
          "operator_ui_timeout",
          Some("operator UI request timed out"),
          ui.request_id,
          ui.method,
          turn,
        ),
      )
      emit_update(
        issue_id,
        lifecycle_update_with_request(
          "extension_ui_response",
          Some("cancelled"),
          ui.request_id,
          ui.method,
          turn,
        ),
      )
      let turn_records = list.append(turn_records, skipped)
      active_turn_loop(
        session,
        issue_id,
        turn,
        totals,
        config,
        emit_update,
        command_subject,
        prompt_queue,
        stop_after_turn,
        None,
        turn_records,
        turn_deadline_ms,
        monotonic_ms() + config.pi.stall_timeout_ms,
        workspace_path,
      )
    }
  }
}

fn handle_abort_command(
  session: pi_rpc.Session,
  issue_id: String,
  workspace_path: String,
  config: domain.EffectiveConfig,
  emit_update: fn(String, PiUpdate) -> Nil,
  prompt_queue: List(String),
  totals: domain.TokenTotals,
  reply: process.Subject(worker_command.Reply),
) -> WorkerFailure {
  case pi_rpc.send_abort(session, config.pi.read_timeout_ms) {
    Ok(#(_session, skipped)) -> {
      emit_records(
        issue_id,
        skipped,
        0,
        config_module.resolved_secrets(config),
        emit_update,
      )
      emit_update(issue_id, lifecycle_update("pi_abort_sent"))
      process.send(reply, worker_command.Applied(Some("abort sent")))
    }
    Error(err) -> {
      emit_update(
        issue_id,
        lifecycle_update_with_message(
          "pi_abort_failed",
          Some(error.pi_rpc_code(err)),
        ),
      )
      process.send(reply, worker_command.Applied(Some("abort requested")))
    }
  }
  let _ = pi_rpc.terminate(session)
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
  session: pi_rpc.Session,
  issue_id: String,
  workspace_path: String,
  config: domain.EffectiveConfig,
  emit_update: fn(String, PiUpdate) -> Nil,
  prompt_queue: List(String),
  totals: domain.TokenTotals,
  final_issue: Option(domain.Issue),
) -> WorkerFailure {
  let _ = pi_rpc.terminate(session)
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
  session: pi_rpc.Session,
  issue_id: String,
  workspace_path: String,
  config: domain.EffectiveConfig,
  emit_update: fn(String, PiUpdate) -> Nil,
  prompt_queue: List(String),
  totals: domain.TokenTotals,
  final_issue: Option(domain.Issue),
) -> Result(WorkerSuccess, WorkerFailure) {
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
  session: pi_rpc.Session,
  issue_id: String,
  workspace_path: String,
  config: domain.EffectiveConfig,
  emit_update: fn(String, PiUpdate) -> Nil,
  prompt_queue: List(String),
  reason: error.AgentRunnerError,
  tokens: domain.TokenTotals,
  final_issue: Option(domain.Issue),
) -> WorkerFailure {
  let _ = pi_rpc.terminate(session)
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
  session: pi_rpc.Session,
  issue_id: String,
  workspace_path: String,
  config: domain.EffectiveConfig,
  emit_update: fn(String, PiUpdate) -> Nil,
  prompt_queue: List(String),
  err: error.PiRpcError,
  tokens: domain.TokenTotals,
) -> Result(WorkerSuccess, WorkerFailure) {
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
) -> WorkerFailure {
  worker_failure_with(reason, workspace_path, domain.zero_token_totals(), None)
}

fn worker_failure_with(
  reason: error.AgentRunnerError,
  workspace_path: Option(String),
  tokens: domain.TokenTotals,
  final_issue: Option(domain.Issue),
) -> WorkerFailure {
  WorkerFailure(
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
  emit_update: fn(String, PiUpdate) -> Nil,
) -> Nil {
  emit_update(
    issue_id,
    lifecycle_update_with_message(
      "operator_prompt_queued",
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
  emit_update: fn(String, PiUpdate) -> Nil,
) -> Nil {
  case prompt_queue {
    [] -> Nil
    [message, ..rest] -> {
      emit_update(
        issue_id,
        lifecycle_update_with_message(
          "operator_prompt_dropped",
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

fn operator_prompt_too_large(message: String) -> Bool {
  string.length(message) > worker_command.max_operator_prompt_chars
}

fn ui_response_too_large(response: command.UiResponse) -> Bool {
  case response {
    command.UiCancel -> False
    command.UiValue(value) ->
      string.length(value) > worker_command.max_operator_ui_value_chars
  }
}

fn emit_records(
  issue_id: String,
  records: List(pi_rpc.RpcRecord),
  turn: Int,
  secrets: List(String),
  emit_update: fn(String, PiUpdate) -> Nil,
) -> Nil {
  case records {
    [] -> Nil
    [record, ..rest] -> {
      emit_update(issue_id, update_from_record(record, turn, secrets))
      emit_records(issue_id, rest, turn, secrets, emit_update)
    }
  }
}

fn is_blocking_ui_method(method: Option(String)) -> Bool {
  case method {
    Some("select") | Some("confirm") | Some("input") | Some("editor") -> True
    _ -> False
  }
}

fn try_active(
  result: Result(a, WorkerFailure),
  next: fn(a) -> Result(b, WorkerFailure),
) -> Result(b, WorkerFailure) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}

fn lifecycle_update(name: String) -> PiUpdate {
  lifecycle_update_with_message(name, None)
}

fn lifecycle_update_with_message(
  name: String,
  message: Option(String),
) -> PiUpdate {
  PiUpdate(
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

fn lifecycle_update_with_request(
  name: String,
  message: Option(String),
  request_id: String,
  method: String,
  turn: Int,
) -> PiUpdate {
  PiUpdate(
    event: name,
    message: message,
    raw_json: None,
    turn: Some(turn),
    request_id: Some(request_id),
    method: Some(method),
    pi_session_id: None,
    tokens: domain.zero_token_totals(),
    tool_name: None,
    tool_input: None,
    tool_output: None,
    tool_status: None,
  )
}

fn pi_session_started_update(pi_session_id: Option(String)) -> PiUpdate {
  PiUpdate(
    event: "pi_session_started",
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

fn token_update(name: String, turn: Int, tokens: domain.TokenTotals) -> PiUpdate {
  PiUpdate(
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
  record: pi_rpc.RpcRecord,
  turn: Int,
  secrets: List(String),
) -> PiUpdate {
  let message = case record.type_ {
    "extension_ui_request" -> record.message
    _ -> record.delta
  }
  PiUpdate(
    event: record.type_,
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
  state: String,
) -> FinalClassification {
  case contains(config.tracker.terminal_states, state) {
    True -> FinalTerminal
    False ->
      case contains(config.tracker.active_states, state) {
        True -> FinalActive
        False -> FinalNonActive
      }
  }
}

fn contains(states: List(String), state: String) -> Bool {
  list.any(states, fn(s) { string_lower(s) == string_lower(state) })
}

fn string_lower(value: String) -> String {
  value |> string.trim |> string.lowercase
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
