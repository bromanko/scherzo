import gleam/erlang/process
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/agent/auto_retry
import scherzo/agent/context_exhaustion
import scherzo/agent/operator_control
import scherzo/agent/pi_event
import scherzo/agent/turn_protocol as turn_check
import scherzo/agent/turn_result_buffer as turn_buffer
import scherzo/agent/turn_update
import scherzo/agent/types
import scherzo/agent/worker_command
import scherzo/config as config_module
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/error
import scherzo/pi/client
import scherzo/pi/protocol
import scherzo/pi/retry_event
import scherzo/session/tokens as session_tokens
import scherzo/tracker/issue as tracker_issue

pub type Context {
  Context(
    issue_id: String,
    turn: Int,
    totals: session_tokens.TokenTotals,
    config: config_types.EffectiveConfig,
    emit_update: fn(String, types.RunnerUpdate) -> Nil,
    command_subject: process.Subject(worker_command.Command),
    turn_deadline_ms: Int,
    cleanup_failure: fn(
      client.Session,
      String,
      List(String),
      error.AgentRunnerError,
      session_tokens.TokenTotals,
      Option(tracker_issue.Issue),
    ) -> types.WorkerFailure,
    handle_abort: fn(
      client.Session,
      String,
      List(String),
      session_tokens.TokenTotals,
      process.Subject(worker_command.Reply),
    ) -> types.WorkerFailure,
  )
}

pub type ActiveTurn {
  ActiveTurn(
    session: client.Session,
    prompt_queue: List(String),
    stop_after_turn: Bool,
    records: List(protocol.RpcRecord),
  )
}

pub type ActiveTurnFailure {
  FinalFailure(types.WorkerFailure)
  RecoverableContextExhaustion(
    session: client.Session,
    prompt_queue: List(String),
    reason: error.PiRpcError,
    tokens: session_tokens.TokenTotals,
  )
}

type ActiveCommandState {
  ActiveCommandState(
    session: client.Session,
    prompt_queue: List(String),
    stop_after_turn: Bool,
    pending_ui: Option(operator_control.PendingUi),
    stall_deadline_ms: Int,
    records: turn_buffer.Buffer,
    pending_auto_retry: auto_retry.State,
  )
}

pub fn run_active_turn(
  context: Context,
  session: client.Session,
  prompt_queue: List(String),
  stop_after_turn: Bool,
  pending_ui: Option(operator_control.PendingUi),
  turn_records: List(protocol.RpcRecord),
  stall_deadline_ms: Int,
) -> Result(ActiveTurn, ActiveTurnFailure) {
  active_turn_loop(
    context,
    session,
    prompt_queue,
    stop_after_turn,
    pending_ui,
    turn_buffer.from_records(turn_records),
    stall_deadline_ms,
    auto_retry.initial(),
  )
}

fn active_turn_loop(
  context: Context,
  session: client.Session,
  prompt_queue: List(String),
  stop_after_turn: Bool,
  pending_ui: Option(operator_control.PendingUi),
  turn_records: turn_buffer.Buffer,
  stall_deadline_ms: Int,
  pending_auto_retry: auto_retry.State,
) -> Result(ActiveTurn, ActiveTurnFailure) {
  case process.receive(context.command_subject, within: 0) {
    Ok(command) -> {
      use state <- result.try(handle_active_command(
        context,
        command,
        session,
        prompt_queue,
        stop_after_turn,
        pending_ui,
        turn_records,
        stall_deadline_ms,
        pending_auto_retry,
      ))
      active_turn_loop(
        context,
        state.session,
        state.prompt_queue,
        state.stop_after_turn,
        state.pending_ui,
        state.records,
        state.stall_deadline_ms,
        state.pending_auto_retry,
      )
    }
    Error(_) -> {
      let base_stall_deadline = case pending_ui {
        Some(ui) -> ui.deadline_ms
        None -> stall_deadline_ms
      }
      let effective_stall_deadline =
        auto_retry.effective_stall_deadline(
          pending_auto_retry,
          base_stall_deadline,
        )
      let read_timeout_ms =
        turn_update.read_timeout_for_pending_ui(
          context.config.pi.read_timeout_ms,
          pending_ui,
        )
      case
        client.read_turn_record(
          session,
          read_timeout_ms,
          context.turn_deadline_ms,
          effective_stall_deadline,
        )
      {
        Error(error.PiStallTimeout) ->
          case pending_ui {
            Some(ui) ->
              handle_operator_ui_timeout(
                context,
                session,
                prompt_queue,
                stop_after_turn,
                ui,
                turn_records,
                pending_auto_retry,
              )
            None ->
              case
                auto_retry.deadline_expired(pending_auto_retry, monotonic_ms())
              {
                True ->
                  Error(final_deferred_retry_failure(
                    context,
                    session,
                    prompt_queue,
                    pending_auto_retry,
                    None,
                  ))
                False ->
                  Error(
                    FinalFailure(context.cleanup_failure(
                      session,
                      context.issue_id,
                      prompt_queue,
                      error.PiFailed(error.PiStallTimeout),
                      context.totals,
                      None,
                    )),
                  )
              }
          }
        Error(err) ->
          Error(recoverable_or_final(
            context,
            session,
            prompt_queue,
            turn_check.read_error(err, turn_buffer.to_records(turn_records)),
          ))
        Ok(#(session, None)) ->
          active_turn_loop(
            context,
            session,
            prompt_queue,
            stop_after_turn,
            pending_ui,
            turn_records,
            stall_deadline_ms,
            pending_auto_retry,
          )
        Ok(#(session, Some(record))) ->
          handle_turn_record(
            context,
            session,
            record,
            prompt_queue,
            stop_after_turn,
            pending_ui,
            turn_records,
            stall_deadline_ms,
            pending_auto_retry,
          )
      }
    }
  }
}

fn handle_active_command(
  context: Context,
  command: worker_command.Command,
  session: client.Session,
  prompt_queue: List(String),
  stop_after_turn: Bool,
  pending_ui: Option(operator_control.PendingUi),
  turn_records: turn_buffer.Buffer,
  stall_deadline_ms: Int,
  pending_auto_retry: auto_retry.State,
) -> Result(ActiveCommandState, ActiveTurnFailure) {
  let previous_state =
    operator_control.from_parts(prompt_queue, stop_after_turn, pending_ui)
  let #(state, effects) =
    operator_control.handle_command(
      operator_control.ActiveTurn,
      previous_state,
      command,
    )

  interpret_active_effects(
    context,
    session,
    previous_state,
    state,
    effects,
    turn_records,
    stall_deadline_ms,
    pending_auto_retry,
  )
}

fn interpret_active_effects(
  context: Context,
  session: client.Session,
  previous_state: operator_control.State,
  state: operator_control.State,
  effects: List(operator_control.Effect),
  turn_records: turn_buffer.Buffer,
  stall_deadline_ms: Int,
  pending_auto_retry: auto_retry.State,
) -> Result(ActiveCommandState, ActiveTurnFailure) {
  case effects {
    [] ->
      Ok(active_command_state(
        session,
        state,
        stall_deadline_ms,
        turn_records,
        pending_auto_retry,
      ))
    [effect, ..rest] ->
      case effect {
        operator_control.Reply(reply, reply_value) -> {
          process.send(reply, reply_value)
          interpret_active_effects(
            context,
            session,
            previous_state,
            state,
            rest,
            turn_records,
            stall_deadline_ms,
            pending_auto_retry,
          )
        }
        operator_control.EmitPromptQueued(message) -> {
          turn_update.emit_operator_prompt_queued(
            context.issue_id,
            message,
            config_module.resolved_secrets(context.config),
            context.emit_update,
          )
          interpret_active_effects(
            context,
            session,
            previous_state,
            state,
            rest,
            turn_records,
            stall_deadline_ms,
            pending_auto_retry,
          )
        }
        operator_control.AbortRequested(reply) ->
          Error(
            FinalFailure(context.handle_abort(
              session,
              context.issue_id,
              state.prompt_queue,
              context.totals,
              reply,
            )),
          )
        operator_control.StopBeforeNextTurn(_) ->
          interpret_active_effects(
            context,
            session,
            previous_state,
            state,
            rest,
            turn_records,
            stall_deadline_ms,
            pending_auto_retry,
          )
        operator_control.SendUiCancel(reply, request_id) -> {
          use active_state <- result.try(send_active_ui_response(
            context,
            session,
            previous_state.pending_ui,
            state,
            turn_records,
            stall_deadline_ms,
            pending_auto_retry,
            request_id,
            command.UiCancel,
            reply,
          ))
          interpret_active_effects(
            context,
            active_state.session,
            active_to_control_state(active_state),
            active_to_control_state(active_state),
            rest,
            active_state.records,
            active_state.stall_deadline_ms,
            active_state.pending_auto_retry,
          )
        }
        operator_control.SendUiValue(reply, request_id, value) -> {
          use active_state <- result.try(send_active_ui_response(
            context,
            session,
            previous_state.pending_ui,
            state,
            turn_records,
            stall_deadline_ms,
            pending_auto_retry,
            request_id,
            command.UiValue(value),
            reply,
          ))
          interpret_active_effects(
            context,
            active_state.session,
            active_to_control_state(active_state),
            active_to_control_state(active_state),
            rest,
            active_state.records,
            active_state.stall_deadline_ms,
            active_state.pending_auto_retry,
          )
        }
      }
  }
}

fn send_active_ui_response(
  context: Context,
  session: client.Session,
  previous_pending_ui: Option(operator_control.PendingUi),
  state: operator_control.State,
  turn_records: turn_buffer.Buffer,
  stall_deadline_ms: Int,
  pending_auto_retry: auto_retry.State,
  request_id: String,
  response: command.UiResponse,
  reply: process.Subject(worker_command.Reply),
) -> Result(ActiveCommandState, ActiveTurnFailure) {
  let sent = case response {
    command.UiCancel ->
      client.send_extension_ui_cancel(
        session,
        request_id,
        context.config.pi.read_timeout_ms,
      )
    command.UiValue(value) ->
      client.send_extension_ui_value(
        session,
        request_id,
        value,
        context.config.pi.read_timeout_ms,
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
      let state =
        operator_control.from_parts(
          state.prompt_queue,
          state.stop_after_turn,
          previous_pending_ui,
        )
      Ok(active_command_state(
        session,
        state,
        stall_deadline_ms,
        turn_records,
        pending_auto_retry,
      ))
    }
    Ok(#(session, skipped)) -> {
      turn_update.emit_records(
        context.issue_id,
        skipped,
        context.turn,
        config_module.resolved_secrets(context.config),
        context.emit_update,
      )
      process.send(reply, worker_command.Applied(Some("ui response sent")))
      let method = case previous_pending_ui {
        Some(ui) -> ui.method
        None -> ""
      }
      context.emit_update(
        context.issue_id,
        turn_update.lifecycle_update_with_request(
          pi_event.ExtensionUiResponse,
          Some("operator response sent"),
          request_id,
          method,
          context.turn,
        ),
      )
      let turn_records = turn_buffer.append_records(turn_records, skipped)
      Ok(active_command_state(
        session,
        state,
        monotonic_ms() + context.config.pi.stall_timeout_ms,
        turn_records,
        pending_auto_retry,
      ))
    }
  }
}

fn active_command_state(
  session: client.Session,
  state: operator_control.State,
  stall_deadline_ms: Int,
  turn_records: turn_buffer.Buffer,
  pending_auto_retry: auto_retry.State,
) -> ActiveCommandState {
  ActiveCommandState(
    session: session,
    prompt_queue: state.prompt_queue,
    stop_after_turn: state.stop_after_turn,
    pending_ui: state.pending_ui,
    stall_deadline_ms: stall_deadline_ms,
    records: turn_records,
    pending_auto_retry: pending_auto_retry,
  )
}

fn active_to_control_state(
  state: ActiveCommandState,
) -> operator_control.State {
  operator_control.from_parts(
    state.prompt_queue,
    state.stop_after_turn,
    state.pending_ui,
  )
}

fn handle_turn_record(
  context: Context,
  session: client.Session,
  record: protocol.RpcRecord,
  prompt_queue: List(String),
  stop_after_turn: Bool,
  pending_ui: Option(operator_control.PendingUi),
  turn_records: turn_buffer.Buffer,
  stall_deadline_ms: Int,
  pending_auto_retry: auto_retry.State,
) -> Result(ActiveTurn, ActiveTurnFailure) {
  let secrets = config_module.resolved_secrets(context.config)
  let event = pi_event.from_string(record.type_)
  context.emit_update(
    context.issue_id,
    turn_update.update_from_record(record, context.turn, secrets),
  )
  let turn_records = turn_buffer.append_record(turn_records, record)
  case retry_event.from_record(record) {
    Some(retry_event.AutoRetryStart(..)) ->
      active_turn_loop(
        context,
        session,
        prompt_queue,
        stop_after_turn,
        pending_ui,
        turn_records,
        monotonic_ms() + context.config.pi.stall_timeout_ms,
        auto_retry.mark_started(pending_auto_retry),
      )
    Some(retry_event.AutoRetryEnd(success: True, ..)) ->
      case auto_retry.agent_end_seen(pending_auto_retry) {
        True ->
          finish_active_turn_after_agent_end(
            context,
            session,
            prompt_queue,
            stop_after_turn,
            turn_records,
          )
        False ->
          active_turn_loop(
            context,
            session,
            prompt_queue,
            stop_after_turn,
            pending_ui,
            turn_records,
            monotonic_ms() + context.config.pi.stall_timeout_ms,
            auto_retry.initial(),
          )
      }
    Some(retry_event.AutoRetryEnd(success: False, final_error: final_error, ..)) ->
      Error(final_deferred_retry_failure(
        context,
        session,
        prompt_queue,
        pending_auto_retry,
        final_error,
      ))
    None ->
      case turn_check.stop_reason_failure(record) {
        Some(err) ->
          case auto_retry.should_defer(context.config.pi, err) {
            True ->
              active_turn_loop(
                context,
                session,
                prompt_queue,
                stop_after_turn,
                pending_ui,
                turn_records,
                monotonic_ms() + context.config.pi.stall_timeout_ms,
                auto_retry.defer_failure(
                  pending_auto_retry,
                  err,
                  auto_retry.decision_deadline_ms(
                    monotonic_ms(),
                    context.config.pi.read_timeout_ms,
                  ),
                ),
              )
            False ->
              Error(recoverable_or_final(context, session, prompt_queue, err))
          }
        None ->
          case event {
            pi_event.AgentEnd ->
              handle_agent_end_record(
                context,
                session,
                prompt_queue,
                stop_after_turn,
                pending_ui,
                turn_records,
                pending_auto_retry,
              )
            pi_event.ExtensionUiRequest ->
              handle_extension_ui_record(
                context,
                session,
                record,
                prompt_queue,
                stop_after_turn,
                pending_ui,
                turn_records,
                stall_deadline_ms,
                auto_retry.mark_output_event(pending_auto_retry, event),
              )
            _ ->
              active_turn_loop(
                context,
                session,
                prompt_queue,
                stop_after_turn,
                pending_ui,
                turn_records,
                monotonic_ms() + context.config.pi.stall_timeout_ms,
                auto_retry.mark_output_event(pending_auto_retry, event),
              )
          }
      }
  }
}

fn handle_agent_end_record(
  context: Context,
  session: client.Session,
  prompt_queue: List(String),
  stop_after_turn: Bool,
  pending_ui: Option(operator_control.PendingUi),
  turn_records: turn_buffer.Buffer,
  pending_auto_retry: auto_retry.State,
) -> Result(ActiveTurn, ActiveTurnFailure) {
  case pending_ui {
    Some(_) ->
      Error(
        FinalFailure(context.cleanup_failure(
          session,
          context.issue_id,
          prompt_queue,
          error.PiFailed(error.PiProtocolError(
            "agent ended with pending UI request",
          )),
          context.totals,
          None,
        )),
      )
    None ->
      case pending_auto_retry {
        auto_retry.NoPendingAutoRetry ->
          finish_active_turn_after_agent_end(
            context,
            session,
            prompt_queue,
            stop_after_turn,
            turn_records,
          )
        auto_retry.PendingAutoRetry(..) ->
          active_turn_loop(
            context,
            session,
            prompt_queue,
            stop_after_turn,
            None,
            turn_records,
            monotonic_ms() + context.config.pi.stall_timeout_ms,
            auto_retry.mark_agent_end(
              pending_auto_retry,
              auto_retry.decision_deadline_ms(
                monotonic_ms(),
                context.config.pi.read_timeout_ms,
              ),
            ),
          )
      }
  }
}

fn finish_active_turn_after_agent_end(
  context: Context,
  session: client.Session,
  prompt_queue: List(String),
  stop_after_turn: Bool,
  turn_records: turn_buffer.Buffer,
) -> Result(ActiveTurn, ActiveTurnFailure) {
  case turn_check.finish_after_agent_end(turn_buffer.to_records(turn_records)) {
    Ok(records) ->
      Ok(ActiveTurn(session, prompt_queue, stop_after_turn, records))
    Error(err) ->
      Error(
        FinalFailure(context.cleanup_failure(
          session,
          context.issue_id,
          prompt_queue,
          error.PiFailed(err),
          context.totals,
          None,
        )),
      )
  }
}

fn recoverable_or_final(
  context: Context,
  session: client.Session,
  prompt_queue: List(String),
  err: error.PiRpcError,
) -> ActiveTurnFailure {
  case context_exhaustion.from_pi_rpc_error(err) {
    Some(_) ->
      RecoverableContextExhaustion(
        session: session,
        prompt_queue: prompt_queue,
        reason: err,
        tokens: context.totals,
      )
    None ->
      FinalFailure(context.cleanup_failure(
        session,
        context.issue_id,
        prompt_queue,
        error.PiFailed(err),
        context.totals,
        None,
      ))
  }
}

fn handle_extension_ui_record(
  context: Context,
  session: client.Session,
  record: protocol.RpcRecord,
  prompt_queue: List(String),
  stop_after_turn: Bool,
  pending_ui: Option(operator_control.PendingUi),
  turn_records: turn_buffer.Buffer,
  stall_deadline_ms: Int,
  pending_auto_retry: auto_retry.State,
) -> Result(ActiveTurn, ActiveTurnFailure) {
  case
    pi_event.is_blocking_ui_request(pi_event.ExtensionUiRequest, record.method)
  {
    False ->
      active_turn_loop(
        context,
        session,
        prompt_queue,
        stop_after_turn,
        pending_ui,
        turn_records,
        monotonic_ms() + context.config.pi.stall_timeout_ms,
        pending_auto_retry,
      )
    True ->
      case pending_ui {
        Some(_) ->
          Error(
            FinalFailure(context.cleanup_failure(
              session,
              context.issue_id,
              prompt_queue,
              error.PiFailed(error.PiProtocolError("nested operator UI request")),
              context.totals,
              None,
            )),
          )
        None ->
          case record.id, record.method {
            Some(request_id), Some(method) ->
              handle_blocking_ui_policy(
                context,
                session,
                record,
                request_id,
                method,
                prompt_queue,
                stop_after_turn,
                turn_records,
                stall_deadline_ms,
                pending_auto_retry,
              )
            _, _ ->
              Error(
                FinalFailure(context.cleanup_failure(
                  session,
                  context.issue_id,
                  prompt_queue,
                  error.PiFailed(error.PiProtocolError(
                    "extension UI request missing id",
                  )),
                  context.totals,
                  None,
                )),
              )
          }
      }
  }
}

fn handle_blocking_ui_policy(
  context: Context,
  session: client.Session,
  record: protocol.RpcRecord,
  request_id: String,
  method: String,
  prompt_queue: List(String),
  stop_after_turn: Bool,
  turn_records: turn_buffer.Buffer,
  stall_deadline_ms: Int,
  pending_auto_retry: auto_retry.State,
) -> Result(ActiveTurn, ActiveTurnFailure) {
  case context.config.pi.ui_request_policy {
    config_types.Fail ->
      Error(
        FinalFailure(context.cleanup_failure(
          session,
          context.issue_id,
          prompt_queue,
          error.PiFailed(error.PiProtocolError(
            "extension UI request blocked by policy",
          )),
          context.totals,
          None,
        )),
      )
    config_types.Ignore ->
      active_turn_loop(
        context,
        session,
        prompt_queue,
        stop_after_turn,
        None,
        turn_records,
        monotonic_ms() + context.config.pi.stall_timeout_ms,
        pending_auto_retry,
      )
    config_types.Cancel -> {
      case
        client.send_extension_ui_cancel(
          session,
          request_id,
          context.config.pi.read_timeout_ms,
        )
      {
        Ok(#(session, skipped)) -> {
          turn_update.emit_records(
            context.issue_id,
            skipped,
            context.turn,
            config_module.resolved_secrets(context.config),
            context.emit_update,
          )
          context.emit_update(
            context.issue_id,
            turn_update.lifecycle_update_with_request(
              pi_event.ExtensionUiResponse,
              Some("cancelled"),
              request_id,
              method,
              context.turn,
            ),
          )
          let turn_records = turn_buffer.append_records(turn_records, skipped)
          active_turn_loop(
            context,
            session,
            prompt_queue,
            stop_after_turn,
            None,
            turn_records,
            monotonic_ms() + context.config.pi.stall_timeout_ms,
            pending_auto_retry,
          )
        }
        Error(err) ->
          Error(
            FinalFailure(context.cleanup_failure(
              session,
              context.issue_id,
              prompt_queue,
              error.PiFailed(err),
              context.totals,
              None,
            )),
          )
      }
    }
    config_types.Operator -> {
      let now = monotonic_ms()
      let pending_ui =
        operator_control.PendingUi(
          request_id: request_id,
          method: method,
          message: record.message,
          created_at_ms: now,
          deadline_ms: now + context.config.pi.ui_request_timeout_ms,
        )
      active_turn_loop(
        context,
        session,
        prompt_queue,
        stop_after_turn,
        Some(pending_ui),
        turn_records,
        stall_deadline_ms,
        pending_auto_retry,
      )
    }
  }
}

fn handle_operator_ui_timeout(
  context: Context,
  session: client.Session,
  prompt_queue: List(String),
  stop_after_turn: Bool,
  ui: operator_control.PendingUi,
  turn_records: turn_buffer.Buffer,
  pending_auto_retry: auto_retry.State,
) -> Result(ActiveTurn, ActiveTurnFailure) {
  case
    client.send_extension_ui_cancel(
      session,
      ui.request_id,
      context.config.pi.read_timeout_ms,
    )
  {
    Error(err) ->
      Error(
        FinalFailure(context.cleanup_failure(
          session,
          context.issue_id,
          prompt_queue,
          error.PiFailed(err),
          context.totals,
          None,
        )),
      )
    Ok(#(session, skipped)) -> {
      turn_update.emit_records(
        context.issue_id,
        skipped,
        context.turn,
        config_module.resolved_secrets(context.config),
        context.emit_update,
      )
      context.emit_update(
        context.issue_id,
        turn_update.lifecycle_update_with_request(
          pi_event.OperatorUiTimeout,
          Some("operator UI request timed out"),
          ui.request_id,
          ui.method,
          context.turn,
        ),
      )
      context.emit_update(
        context.issue_id,
        turn_update.lifecycle_update_with_request(
          pi_event.ExtensionUiResponse,
          Some("cancelled"),
          ui.request_id,
          ui.method,
          context.turn,
        ),
      )
      let turn_records = turn_buffer.append_records(turn_records, skipped)
      active_turn_loop(
        context,
        session,
        prompt_queue,
        stop_after_turn,
        None,
        turn_records,
        monotonic_ms() + context.config.pi.stall_timeout_ms,
        pending_auto_retry,
      )
    }
  }
}

fn final_deferred_retry_failure(
  context: Context,
  session: client.Session,
  prompt_queue: List(String),
  pending_auto_retry: auto_retry.State,
  final_error: Option(String),
) -> ActiveTurnFailure {
  let err = auto_retry.deferred_error(pending_auto_retry, final_error)
  FinalFailure(context.cleanup_failure(
    session,
    context.issue_id,
    prompt_queue,
    error.PiFailed(err),
    context.totals,
    None,
  ))
}

@external(erlang, "scherzo_time_ffi", "monotonic_ms")
fn monotonic_ms() -> Int
