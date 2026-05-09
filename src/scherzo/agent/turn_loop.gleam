import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/operator_control
import scherzo/agent/pi_event
import scherzo/agent/types
import scherzo/agent/worker_command
import scherzo/config as config_module
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/error
import scherzo/log
import scherzo/pi/client
import scherzo/pi/protocol
import scherzo/session/redaction
import scherzo/session/tokens as session_tokens
import scherzo/tracker/issue as tracker_issue

const max_tool_text_chars = 4096

const tool_text_truncated_suffix = "… [truncated]"

// While an operator UI request is pending, keep stdout reads short so
// command-subject responses are observed before the UI deadline expires.
const pending_ui_command_poll_ms = 50

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

type ActiveCommandState {
  ActiveCommandState(
    session: client.Session,
    prompt_queue: List(String),
    stop_after_turn: Bool,
    pending_ui: Option(operator_control.PendingUi),
    stall_deadline_ms: Int,
    records: List(protocol.RpcRecord),
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
) -> Result(ActiveTurn, types.WorkerFailure) {
  active_turn_loop(
    context,
    session,
    prompt_queue,
    stop_after_turn,
    pending_ui,
    turn_records,
    stall_deadline_ms,
  )
}

fn active_turn_loop(
  context: Context,
  session: client.Session,
  prompt_queue: List(String),
  stop_after_turn: Bool,
  pending_ui: Option(operator_control.PendingUi),
  turn_records: List(protocol.RpcRecord),
  stall_deadline_ms: Int,
) -> Result(ActiveTurn, types.WorkerFailure) {
  case process.receive(context.command_subject, within: 0) {
    Ok(command) -> {
      use state <- try_active(handle_active_command(
        context,
        command,
        session,
        prompt_queue,
        stop_after_turn,
        pending_ui,
        turn_records,
        stall_deadline_ms,
      ))
      active_turn_loop(
        context,
        state.session,
        state.prompt_queue,
        state.stop_after_turn,
        state.pending_ui,
        state.records,
        state.stall_deadline_ms,
      )
    }
    Error(_) -> {
      let effective_stall_deadline = case pending_ui {
        Some(ui) -> ui.deadline_ms
        None -> stall_deadline_ms
      }
      let read_timeout_ms =
        read_timeout_for_pending_ui(
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
              )
            None ->
              Error(context.cleanup_failure(
                session,
                context.issue_id,
                prompt_queue,
                error.PiFailed(error.PiStallTimeout),
                context.totals,
                None,
              ))
          }
        Error(err) ->
          Error(context.cleanup_failure(
            session,
            context.issue_id,
            prompt_queue,
            error.PiFailed(err),
            context.totals,
            None,
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
  turn_records: List(protocol.RpcRecord),
  stall_deadline_ms: Int,
) -> Result(ActiveCommandState, types.WorkerFailure) {
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
  )
}

fn interpret_active_effects(
  context: Context,
  session: client.Session,
  previous_state: operator_control.State,
  state: operator_control.State,
  effects: List(operator_control.Effect),
  turn_records: List(protocol.RpcRecord),
  stall_deadline_ms: Int,
) -> Result(ActiveCommandState, types.WorkerFailure) {
  case effects {
    [] ->
      Ok(active_command_state(session, state, stall_deadline_ms, turn_records))
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
          )
        }
        operator_control.EmitPromptQueued(message) -> {
          emit_operator_prompt_queued(context, message)
          interpret_active_effects(
            context,
            session,
            previous_state,
            state,
            rest,
            turn_records,
            stall_deadline_ms,
          )
        }
        operator_control.AbortRequested(reply) ->
          Error(context.handle_abort(
            session,
            context.issue_id,
            state.prompt_queue,
            context.totals,
            reply,
          ))
        operator_control.StopBeforeNextTurn(_) ->
          interpret_active_effects(
            context,
            session,
            previous_state,
            state,
            rest,
            turn_records,
            stall_deadline_ms,
          )
        operator_control.SendUiCancel(reply, request_id) -> {
          use active_state <- try_active(send_active_ui_response(
            context,
            session,
            previous_state.pending_ui,
            state,
            turn_records,
            stall_deadline_ms,
            request_id,
            command.UiCancel,
            reply,
          ))
          let state = active_to_control_state(active_state)
          interpret_active_effects(
            context,
            active_state.session,
            state,
            state,
            rest,
            active_state.records,
            active_state.stall_deadline_ms,
          )
        }
        operator_control.SendUiValue(reply, request_id, value) -> {
          use active_state <- try_active(send_active_ui_response(
            context,
            session,
            previous_state.pending_ui,
            state,
            turn_records,
            stall_deadline_ms,
            request_id,
            command.UiValue(value),
            reply,
          ))
          let state = active_to_control_state(active_state)
          interpret_active_effects(
            context,
            active_state.session,
            state,
            state,
            rest,
            active_state.records,
            active_state.stall_deadline_ms,
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
  turn_records: List(protocol.RpcRecord),
  stall_deadline_ms: Int,
  request_id: String,
  response: command.UiResponse,
  reply: process.Subject(worker_command.Reply),
) -> Result(ActiveCommandState, types.WorkerFailure) {
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
      Ok(active_command_state(session, state, stall_deadline_ms, turn_records))
    }
    Ok(#(session, skipped)) -> {
      emit_records(
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
        lifecycle_update_with_request(
          pi_event.ExtensionUiResponse,
          Some("operator response sent"),
          request_id,
          method,
          context.turn,
        ),
      )
      let turn_records = list.append(turn_records, skipped)
      Ok(active_command_state(
        session,
        state,
        monotonic_ms() + context.config.pi.stall_timeout_ms,
        turn_records,
      ))
    }
  }
}

fn active_command_state(
  session: client.Session,
  state: operator_control.State,
  stall_deadline_ms: Int,
  turn_records: List(protocol.RpcRecord),
) -> ActiveCommandState {
  ActiveCommandState(
    session: session,
    prompt_queue: state.prompt_queue,
    stop_after_turn: state.stop_after_turn,
    pending_ui: state.pending_ui,
    stall_deadline_ms: stall_deadline_ms,
    records: turn_records,
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
  turn_records: List(protocol.RpcRecord),
  stall_deadline_ms: Int,
) -> Result(ActiveTurn, types.WorkerFailure) {
  let secrets = config_module.resolved_secrets(context.config)
  let event = pi_event.from_string(record.type_)
  context.emit_update(
    context.issue_id,
    update_from_record(record, context.turn, secrets),
  )
  let turn_records = list.append(turn_records, [record])
  case stop_reason_failure(record) {
    Some(err) ->
      Error(context.cleanup_failure(
        session,
        context.issue_id,
        prompt_queue,
        error.PiFailed(err),
        context.totals,
        None,
      ))
    None ->
      case event {
        pi_event.AgentEnd ->
          case pending_ui {
            None ->
              Ok(ActiveTurn(
                session,
                prompt_queue,
                stop_after_turn,
                turn_records,
              ))
            Some(_) ->
              Error(context.cleanup_failure(
                session,
                context.issue_id,
                prompt_queue,
                error.PiFailed(error.PiProtocolError(
                  "agent ended with pending UI request",
                )),
                context.totals,
                None,
              ))
          }
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
          )
      }
  }
}

fn stop_reason_failure(record: protocol.RpcRecord) -> Option(error.PiRpcError) {
  case record.stop_reason {
    None -> None
    Some(reason) -> {
      let normalized = reason |> string.trim |> string.lowercase
      case normalized == "error" {
        True -> Some(error.PiProtocolError(stop_reason_failure_message(record)))
        False -> None
      }
    }
  }
}

fn stop_reason_failure_message(record: protocol.RpcRecord) -> String {
  let base = "pi " <> record.type_ <> " reported stopReason=error"
  case record.error_message {
    None -> base
    Some(message) -> {
      let message = string.trim(message)
      case message == "" {
        True -> base
        False -> base <> ": " <> message
      }
    }
  }
}

fn handle_extension_ui_record(
  context: Context,
  session: client.Session,
  record: protocol.RpcRecord,
  prompt_queue: List(String),
  stop_after_turn: Bool,
  pending_ui: Option(operator_control.PendingUi),
  turn_records: List(protocol.RpcRecord),
  stall_deadline_ms: Int,
) -> Result(ActiveTurn, types.WorkerFailure) {
  case is_blocking_ui_method(record.method) {
    False ->
      active_turn_loop(
        context,
        session,
        prompt_queue,
        stop_after_turn,
        pending_ui,
        turn_records,
        monotonic_ms() + context.config.pi.stall_timeout_ms,
      )
    True ->
      case pending_ui {
        Some(_) ->
          Error(context.cleanup_failure(
            session,
            context.issue_id,
            prompt_queue,
            error.PiFailed(error.PiProtocolError("nested operator UI request")),
            context.totals,
            None,
          ))
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
              )
            _, _ ->
              Error(context.cleanup_failure(
                session,
                context.issue_id,
                prompt_queue,
                error.PiFailed(error.PiProtocolError(
                  "extension UI request missing id",
                )),
                context.totals,
                None,
              ))
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
  turn_records: List(protocol.RpcRecord),
  stall_deadline_ms: Int,
) -> Result(ActiveTurn, types.WorkerFailure) {
  case context.config.pi.ui_request_policy {
    config_types.Fail ->
      Error(context.cleanup_failure(
        session,
        context.issue_id,
        prompt_queue,
        error.PiFailed(error.PiProtocolError(
          "extension UI request blocked by policy",
        )),
        context.totals,
        None,
      ))
    config_types.Ignore ->
      active_turn_loop(
        context,
        session,
        prompt_queue,
        stop_after_turn,
        None,
        turn_records,
        monotonic_ms() + context.config.pi.stall_timeout_ms,
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
          emit_records(
            context.issue_id,
            skipped,
            context.turn,
            config_module.resolved_secrets(context.config),
            context.emit_update,
          )
          context.emit_update(
            context.issue_id,
            lifecycle_update_with_request(
              pi_event.ExtensionUiResponse,
              Some("cancelled"),
              request_id,
              method,
              context.turn,
            ),
          )
          let turn_records = list.append(turn_records, skipped)
          active_turn_loop(
            context,
            session,
            prompt_queue,
            stop_after_turn,
            None,
            turn_records,
            monotonic_ms() + context.config.pi.stall_timeout_ms,
          )
        }
        Error(err) ->
          Error(context.cleanup_failure(
            session,
            context.issue_id,
            prompt_queue,
            error.PiFailed(err),
            context.totals,
            None,
          ))
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
      let _ = pending_ui.message
      let _ = pending_ui.created_at_ms
      active_turn_loop(
        context,
        session,
        prompt_queue,
        stop_after_turn,
        Some(pending_ui),
        turn_records,
        stall_deadline_ms,
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
  turn_records: List(protocol.RpcRecord),
) -> Result(ActiveTurn, types.WorkerFailure) {
  case
    client.send_extension_ui_cancel(
      session,
      ui.request_id,
      context.config.pi.read_timeout_ms,
    )
  {
    Error(err) ->
      Error(context.cleanup_failure(
        session,
        context.issue_id,
        prompt_queue,
        error.PiFailed(err),
        context.totals,
        None,
      ))
    Ok(#(session, skipped)) -> {
      emit_records(
        context.issue_id,
        skipped,
        context.turn,
        config_module.resolved_secrets(context.config),
        context.emit_update,
      )
      context.emit_update(
        context.issue_id,
        lifecycle_update_with_request(
          pi_event.OperatorUiTimeout,
          Some("operator UI request timed out"),
          ui.request_id,
          ui.method,
          context.turn,
        ),
      )
      context.emit_update(
        context.issue_id,
        lifecycle_update_with_request(
          pi_event.ExtensionUiResponse,
          Some("cancelled"),
          ui.request_id,
          ui.method,
          context.turn,
        ),
      )
      let turn_records = list.append(turn_records, skipped)
      active_turn_loop(
        context,
        session,
        prompt_queue,
        stop_after_turn,
        None,
        turn_records,
        monotonic_ms() + context.config.pi.stall_timeout_ms,
      )
    }
  }
}

fn emit_operator_prompt_queued(context: Context, message: String) -> Nil {
  context.emit_update(
    context.issue_id,
    lifecycle_update_with_message(
      pi_event.OperatorPromptQueued,
      Some(redact_operator_message(
        message,
        config_module.resolved_secrets(context.config),
      )),
    ),
  )
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

fn is_blocking_ui_method(method: Option(String)) -> Bool {
  case method {
    Some("select") | Some("confirm") | Some("input") | Some("editor") -> True
    _ -> False
  }
}

fn read_timeout_for_pending_ui(
  configured_read_timeout_ms: Int,
  pending_ui: Option(operator_control.PendingUi),
) -> Int {
  case pending_ui {
    Some(_) -> min_int(configured_read_timeout_ms, pending_ui_command_poll_ms)
    None -> configured_read_timeout_ms
  }
}

fn min_int(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}

fn try_active(
  result: Result(a, types.WorkerFailure),
  next: fn(a) -> Result(b, types.WorkerFailure),
) -> Result(b, types.WorkerFailure) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
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

fn lifecycle_update_with_request(
  name: pi_event.PiEvent,
  message: Option(String),
  request_id: String,
  method: String,
  turn: Int,
) -> types.RunnerUpdate {
  pi_runner_update(types.PiUpdate(
    event: name,
    message: message,
    raw_json: None,
    turn: Some(turn),
    request_id: Some(request_id),
    method: Some(method),
    pi_session_id: None,
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

fn redact_operator_message(message: String, secrets: List(String)) -> String {
  log.redact("message", log.truncate(message, 200), secrets)
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

@external(erlang, "scherzo_time_ffi", "monotonic_ms")
fn monotonic_ms() -> Int
