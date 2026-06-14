import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import scherzo/control/command
import scherzo/control/remote/ui_protocol
import scherzo/control/remote_command_router
import scherzo/log
import scherzo/session/event

const max_in_flight_commands = 8

pub type Settings {
  Settings(
    server_url: String,
    websocket_url: String,
    daemon_id: String,
    boot_id: String,
    daemon_label: Option(String),
    credential: String,
    heartbeat_interval_ms: Int,
    state_interval_ms: Int,
    retry_initial_ms: Int,
    retry_max_ms: Int,
    connect_timeout_ms: Int,
    command_timeout_ms: Int,
    command_bridge_enabled: Bool,
    redaction_secrets: List(String),
  )
}

pub type Dependencies(connection, timer) {
  Dependencies(
    now_ms: fn() -> Int,
    connect: fn(String, String, Int) -> Result(connection, String),
    send_text: fn(connection, String, Int) -> Result(Nil, String),
    recv_text: fn(connection, Int) -> Result(String, String),
    close: fn(connection) -> Nil,
    send_after: fn(process.Subject(Message), Int, Message) -> timer,
    cancel_timer: fn(timer) -> Nil,
    list_sessions: fn() -> Result(List(event.SessionSummary), String),
    dispatch_paused: fn(Int) -> Result(Bool, String),
    apply_command: fn(command.OperatorCommand, Int) ->
      Result(command.CommandResult, Nil),
    logger: fn(String, String, List(log.Field), List(String)) ->
      Result(Nil, Nil),
  )
}

pub type ClientError {
  ClientError(code: String, message: String)
}

pub opaque type Handle {
  Handle(subject: process.Subject(Message), pid: process.Pid)
}

pub opaque type Message {
  AttemptConnect
  HeartbeatTick
  StateTick
  ReaderText(Int, String)
  ReaderFailed(Int, String)
  ApplyCompleted(Int, String, command.OperatorCommand, command.CommandResult)
  Shutdown(process.Subject(Nil))
}

type State(connection, timer) {
  State(
    subject: process.Subject(Message),
    settings: Settings,
    dependencies: Dependencies(connection, timer),
    connection: Option(connection),
    heartbeat_timer: Option(timer),
    state_timer: Option(timer),
    retry_timer: Option(timer),
    next_retry_ms: Int,
    connection_generation: Int,
    current_heartbeat_interval_ms: Int,
    stopped_for_repair: Bool,
    router: remote_command_router.State,
    last_known_dispatch_paused: Bool,
  )
}

pub fn start(
  settings: Settings,
  dependencies: Dependencies(connection, timer),
) -> Result(Handle, ClientError) {
  let settings = normalize_settings(settings)
  emit_command_bridge_startup_log(settings, dependencies)
  let builder =
    actor.new_with_initialiser(1000, fn(subject) {
      let state =
        State(
          subject: subject,
          settings: settings,
          dependencies: dependencies,
          connection: None,
          heartbeat_timer: None,
          state_timer: None,
          retry_timer: None,
          next_retry_ms: settings.retry_initial_ms,
          connection_generation: 0,
          current_heartbeat_interval_ms: settings.heartbeat_interval_ms,
          stopped_for_repair: False,
          router: remote_command_router.new(),
          last_known_dispatch_paused: False,
        )
        |> schedule_attempt_connect(0)
      let selector = process.new_selector() |> process.select(subject)
      actor.initialised(state)
      |> actor.selecting(selector)
      |> actor.returning(Handle(subject, process.self()))
      |> Ok
    })
    |> actor.on_message(handle_message)

  case actor.start(builder) {
    Ok(started) -> {
      process.unlink(started.pid)
      Ok(started.data)
    }
    Error(_) ->
      Error(ClientError(
        "ui_websocket_client_start_failed",
        "actor start failed",
      ))
  }
}

pub fn stop(handle: Handle, timeout_ms: Int) -> Result(Nil, Nil) {
  let Handle(subject, _) = handle
  let reply = process.new_subject()
  process.send(subject, Shutdown(reply))
  process.receive(reply, within: timeout_ms)
}

pub fn monitor(handle: Handle) -> process.Monitor {
  let Handle(_, pid) = handle
  process.monitor(pid)
}

pub fn kill(handle: Handle) -> Nil {
  let Handle(_, pid) = handle
  process.kill(pid)
}

fn handle_message(
  state: State(connection, timer),
  message: Message,
) -> actor.Next(State(connection, timer), Message) {
  case message {
    AttemptConnect -> actor.continue(attempt_connect(state))
    HeartbeatTick -> actor.continue(send_heartbeat_tick(state))
    StateTick -> actor.continue(send_state_tick(state))
    ReaderText(generation, payload) ->
      actor.continue(handle_reader_text(state, generation, payload))
    ReaderFailed(generation, message) ->
      actor.continue(handle_reader_failed(state, generation, message))
    ApplyCompleted(generation, command_id, operator_command, result) ->
      actor.continue(handle_apply_completed(
        state,
        generation,
        command_id,
        operator_command,
        result,
      ))
    Shutdown(reply) -> {
      shutdown_runtime(state)
      process.send(reply, Nil)
      actor.stop()
    }
  }
}

fn attempt_connect(
  state: State(connection, timer),
) -> State(connection, timer) {
  let state = State(..state, retry_timer: None)
  case state.connection, state.stopped_for_repair {
    Some(_), _ | _, True -> state
    None, False -> {
      emit_log(state, "info", "ui_websocket_connecting", [
        #("server_url", state.settings.server_url),
      ])
      case
        state.dependencies.connect(
          state.settings.websocket_url,
          state.settings.credential,
          state.settings.connect_timeout_ms,
        )
      {
        Ok(connection) -> handle_connected(state, connection)
        Error(message) ->
          schedule_retry_after_failure(
            state,
            "ui_websocket_connect_failed",
            message,
          )
      }
    }
  }
}

fn handle_connected(
  state: State(connection, timer),
  connection: connection,
) -> State(connection, timer) {
  case send_hello(connection, state) {
    Ok(Nil) ->
      case send_heartbeat(connection, state) {
        Ok(Nil) ->
          case send_state_snapshot(connection, state) {
            Ok(Nil) -> {
              let generation = state.connection_generation + 1
              spawn_reader(
                state.subject,
                state.dependencies.recv_text,
                connection,
                generation,
              )
              State(
                ..state,
                connection: Some(connection),
                next_retry_ms: state.settings.retry_initial_ms,
                connection_generation: generation,
              )
              |> schedule_heartbeat_timer()
              |> schedule_state_timer()
            }
            Error(message) ->
              retry_after_send_failure(
                state,
                connection,
                "ui_websocket_state_send_failed",
                message,
              )
          }
        Error(message) ->
          retry_after_send_failure(
            state,
            connection,
            "ui_websocket_heartbeat_send_failed",
            message,
          )
      }
    Error(message) ->
      retry_after_send_failure(
        state,
        connection,
        "ui_websocket_hello_send_failed",
        message,
      )
  }
}

fn send_heartbeat_tick(
  state: State(connection, timer),
) -> State(connection, timer) {
  case state.connection {
    None -> State(..state, heartbeat_timer: None)
    Some(connection) -> {
      let state = State(..state, heartbeat_timer: None)
      case send_heartbeat(connection, state) {
        Ok(Nil) -> schedule_heartbeat_timer(state)
        Error(message) ->
          retry_after_send_failure(
            state,
            connection,
            "ui_websocket_heartbeat_send_failed",
            message,
          )
      }
    }
  }
}

fn send_state_tick(
  state: State(connection, timer),
) -> State(connection, timer) {
  case state.connection {
    None -> State(..state, state_timer: None)
    Some(connection) -> {
      let state = State(..state, state_timer: None)
      case send_state_snapshot(connection, state) {
        Ok(Nil) -> schedule_state_timer(state)
        Error(message) ->
          retry_after_send_failure(
            state,
            connection,
            "ui_websocket_state_send_failed",
            message,
          )
      }
    }
  }
}

fn handle_reader_text(
  state: State(connection, timer),
  generation: Int,
  payload: String,
) -> State(connection, timer) {
  case generation == state.connection_generation, state.connection {
    True, Some(connection) ->
      case ui_protocol.decode_server_message(payload) {
        Ok(ui_protocol.ServerHello(interval)) ->
          apply_server_hello(state, interval)
        Ok(ui_protocol.CredentialRevoked(reason)) ->
          stop_for_repair(
            state,
            connection,
            "ui_websocket_credential_revoked",
            reason,
          )
        Ok(ui_protocol.DaemonIdentityRevoked(reason)) ->
          stop_for_repair(
            state,
            connection,
            "ui_websocket_daemon_identity_revoked",
            reason,
          )
        Ok(ui_protocol.ServerCommand(
          command_id,
          daemon_id,
          boot_id,
          operator_command,
        )) ->
          handle_server_command(
            state,
            connection,
            generation,
            command_id,
            daemon_id,
            boot_id,
            operator_command,
          )
        Ok(ui_protocol.UnknownServerMessage(type_)) -> {
          emit_log(state, "debug", "ui_websocket_unknown_inbound", [
            #("type", type_),
          ])
          state
        }
        Error(error) -> handle_bad_inbound(state, connection, payload, error)
      }
    _, _ -> state
  }
}

fn handle_bad_inbound(
  state: State(connection, timer),
  connection: connection,
  payload: String,
  error: ui_protocol.DecodeError,
) -> State(connection, timer) {
  let ui_protocol.DecodeError(code: code, message: message) = error
  emit_log(state, "warn", "ui_websocket_bad_inbound", [
    #("code", code),
    #("reason", message),
  ])
  case ui_protocol.decode_server_command_rejection(payload) {
    Ok(#(command_id, result)) ->
      handle_bad_server_command(state, connection, command_id, result)
    Error(ui_protocol.DecodeError(code: rejection_code, message: _)) -> {
      emit_log(state, "debug", "ui_websocket_unrepliable_bad_inbound", [
        #("code", rejection_code),
      ])
      state
    }
  }
}

fn handle_bad_server_command(
  state: State(connection, timer),
  connection: connection,
  command_id: String,
  result: command.CommandResult,
) -> State(connection, timer) {
  let #(router, decision) =
    remote_command_router.register_rejection(state.router, command_id, result)
  let state = State(..state, router: router)
  case decision {
    remote_command_router.StartApply ->
      send_command_result_for_state(state, connection, command_id, result)
    remote_command_router.DuplicateInFlight -> {
      emit_log(state, "debug", "ui_websocket_server_command_duplicate", [
        #("server_command_id", command_id),
      ])
      state
    }
    remote_command_router.ReplayCompleted(result) ->
      send_command_result_for_state(state, connection, command_id, result)
    remote_command_router.Reject(result) ->
      send_command_result_for_state(state, connection, command_id, result)
  }
}

fn handle_server_command(
  state: State(connection, timer),
  connection: connection,
  generation: Int,
  command_id: String,
  daemon_id: String,
  boot_id: String,
  operator_command: command.OperatorCommand,
) -> State(connection, timer) {
  let #(router, decision) =
    remote_command_router.register_limited(
      state.router,
      command_id,
      operator_command,
      max_in_flight_commands,
    )
  let state = State(..state, router: router)
  case decision {
    remote_command_router.StartApply ->
      case
        command_without_apply_result(
          state,
          operator_command,
          daemon_id,
          boot_id,
        )
      {
        Some(result) ->
          complete_and_send_command_result(
            state,
            connection,
            command_id,
            result,
          )
        None -> {
          emit_log(state, "info", "ui_websocket_server_command_received", [
            #("server_command_id", command_id),
            #("command", command.command_name(operator_command)),
          ])
          spawn_apply_worker(
            state.subject,
            state.dependencies.apply_command,
            generation,
            command_id,
            operator_command,
            state.settings.command_timeout_ms,
          )
          state
        }
      }
    remote_command_router.DuplicateInFlight -> {
      emit_log(state, "debug", "ui_websocket_server_command_duplicate", [
        #("server_command_id", command_id),
      ])
      state
    }
    remote_command_router.ReplayCompleted(result) ->
      send_command_result_for_state(state, connection, command_id, result)
    remote_command_router.Reject(result) ->
      send_command_result_for_state(state, connection, command_id, result)
  }
}

fn command_without_apply_result(
  state: State(connection, timer),
  operator_command: command.OperatorCommand,
  daemon_id: String,
  boot_id: String,
) -> Option(command.CommandResult) {
  case state.settings.command_bridge_enabled {
    False ->
      Some(command.not_allowed(
        operator_command,
        "command_bridge_disabled",
        Some("remote command bridge is disabled"),
      ))
    True ->
      case
        daemon_id == state.settings.daemon_id,
        boot_id == state.settings.boot_id
      {
        False, _ ->
          Some(command.not_allowed(
            operator_command,
            "daemon_id_mismatch",
            Some("server command daemonId does not match this daemon"),
          ))
        _, False ->
          Some(command.not_allowed(
            operator_command,
            "boot_id_mismatch",
            Some("server command bootId does not match this daemon boot"),
          ))
        True, True -> None
      }
  }
}

fn handle_apply_completed(
  state: State(connection, timer),
  generation: Int,
  command_id: String,
  operator_command: command.OperatorCommand,
  result: command.CommandResult,
) -> State(connection, timer) {
  let state =
    State(
      ..state,
      router: remote_command_router.complete(state.router, command_id, result),
      last_known_dispatch_paused: update_known_dispatch_paused(
        state.last_known_dispatch_paused,
        operator_command,
        result,
      ),
    )
  emit_log(state, "info", "ui_websocket_server_command_completed", [
    #("server_command_id", command_id),
    #("command", command.command_name(operator_command)),
    #("status", command.status_to_string(result.status)),
  ])
  case generation == state.connection_generation, state.connection {
    True, Some(connection) ->
      send_command_result_and_state(state, connection, command_id, result)
    _, _ -> state
  }
}

fn complete_and_send_command_result(
  state: State(connection, timer),
  connection: connection,
  command_id: String,
  result: command.CommandResult,
) -> State(connection, timer) {
  let state =
    State(
      ..state,
      router: remote_command_router.complete(state.router, command_id, result),
    )
  send_command_result_for_state(state, connection, command_id, result)
}

fn send_command_result_for_state(
  state: State(connection, timer),
  connection: connection,
  command_id: String,
  result: command.CommandResult,
) -> State(connection, timer) {
  case send_command_result(connection, state, command_id, result) {
    Ok(Nil) -> state
    Error(message) ->
      retry_after_send_failure(
        state,
        connection,
        "ui_websocket_command_result_send_failed",
        message,
      )
  }
}

fn send_command_result_and_state(
  state: State(connection, timer),
  connection: connection,
  command_id: String,
  result: command.CommandResult,
) -> State(connection, timer) {
  case send_command_result(connection, state, command_id, result) {
    Ok(Nil) ->
      case send_state_snapshot(connection, state) {
        Ok(Nil) -> state
        Error(message) ->
          retry_after_send_failure(
            state,
            connection,
            "ui_websocket_state_send_failed",
            message,
          )
      }
    Error(message) ->
      retry_after_send_failure(
        state,
        connection,
        "ui_websocket_command_result_send_failed",
        message,
      )
  }
}

fn send_command_result(
  connection: connection,
  state: State(connection, timer),
  command_id: String,
  result: command.CommandResult,
) -> Result(Nil, String) {
  ui_protocol.encode_command_result(command_id, result)
  |> state.dependencies.send_text(
    connection,
    _,
    state.settings.connect_timeout_ms,
  )
}

fn apply_server_hello(
  state: State(connection, timer),
  interval: Option(Int),
) -> State(connection, timer) {
  let interval = case interval {
    Some(interval) if interval >= state.settings.heartbeat_interval_ms ->
      interval
    _ -> state.settings.heartbeat_interval_ms
  }
  cancel_optional_timer(state.dependencies.cancel_timer, state.heartbeat_timer)
  State(..state, heartbeat_timer: None, current_heartbeat_interval_ms: interval)
  |> schedule_heartbeat_timer()
}

fn handle_reader_failed(
  state: State(connection, timer),
  generation: Int,
  message: String,
) -> State(connection, timer) {
  case generation == state.connection_generation, state.connection {
    True, Some(connection) ->
      retry_after_send_failure(
        state,
        connection,
        "ui_websocket_recv_failed",
        message,
      )
    _, _ -> state
  }
}

fn send_hello(
  connection: connection,
  state: State(connection, timer),
) -> Result(Nil, String) {
  ui_protocol.encode_daemon_hello(
    state.settings.daemon_id,
    state.settings.boot_id,
    state.settings.daemon_label,
  )
  |> state.dependencies.send_text(
    connection,
    _,
    state.settings.connect_timeout_ms,
  )
}

fn send_heartbeat(
  connection: connection,
  state: State(connection, timer),
) -> Result(Nil, String) {
  ui_protocol.encode_heartbeat(
    state.dependencies.now_ms(),
    state.settings.daemon_label,
  )
  |> state.dependencies.send_text(
    connection,
    _,
    state.settings.connect_timeout_ms,
  )
}

fn send_state_snapshot(
  connection: connection,
  state: State(connection, timer),
) -> Result(Nil, String) {
  let dispatch_paused = case
    state.dependencies.dispatch_paused(state.settings.command_timeout_ms)
  {
    Ok(dispatch_paused) -> dispatch_paused
    Error(_) -> state.last_known_dispatch_paused
  }
  use sessions <- result.try(state.dependencies.list_sessions())
  let snapshots = sessions |> list.map(ui_protocol.session_from_summary)
  ui_protocol.encode_daemon_state(
    state.dependencies.now_ms(),
    dispatch_paused,
    state.settings.daemon_label,
    snapshots,
  )
  |> state.dependencies.send_text(
    connection,
    _,
    state.settings.connect_timeout_ms,
  )
}

fn retry_after_send_failure(
  state: State(connection, timer),
  connection: connection,
  event: String,
  message: String,
) -> State(connection, timer) {
  state.dependencies.close(connection)
  schedule_retry_after_failure(State(..state, connection: None), event, message)
}

fn stop_for_repair(
  state: State(connection, timer),
  connection: connection,
  event: String,
  reason: String,
) -> State(connection, timer) {
  state.dependencies.close(connection)
  emit_log(state, "warn", event, [#("reason", reason)])
  cancel_connection_timers(
    State(..state, connection: None, stopped_for_repair: True),
  )
}

fn schedule_retry_after_failure(
  state: State(connection, timer),
  event: String,
  message: String,
) -> State(connection, timer) {
  let retry_delay_ms = state.next_retry_ms
  emit_log(state, "warn", event, [
    #("reason", message),
    #("retry_delay_ms", int.to_string(retry_delay_ms)),
  ])
  State(
    ..cancel_connection_timers(state),
    next_retry_ms: next_retry_delay_ms(state, retry_delay_ms),
  )
  |> schedule_attempt_connect(retry_delay_ms)
}

fn next_retry_delay_ms(
  state: State(connection, timer),
  retry_delay_ms: Int,
) -> Int {
  let doubled = retry_delay_ms * 2
  case doubled > state.settings.retry_max_ms {
    True -> state.settings.retry_max_ms
    False -> doubled
  }
}

fn schedule_attempt_connect(
  state: State(connection, timer),
  delay_ms: Int,
) -> State(connection, timer) {
  let timer =
    state.dependencies.send_after(state.subject, delay_ms, AttemptConnect)
  State(..state, retry_timer: Some(timer))
}

fn schedule_heartbeat_timer(
  state: State(connection, timer),
) -> State(connection, timer) {
  let timer =
    state.dependencies.send_after(
      state.subject,
      state.current_heartbeat_interval_ms,
      HeartbeatTick,
    )
  State(..state, heartbeat_timer: Some(timer))
}

fn schedule_state_timer(
  state: State(connection, timer),
) -> State(connection, timer) {
  let timer =
    state.dependencies.send_after(
      state.subject,
      state.settings.state_interval_ms,
      StateTick,
    )
  State(..state, state_timer: Some(timer))
}

fn cancel_connection_timers(
  state: State(connection, timer),
) -> State(connection, timer) {
  cancel_optional_timer(state.dependencies.cancel_timer, state.heartbeat_timer)
  cancel_optional_timer(state.dependencies.cancel_timer, state.state_timer)
  cancel_optional_timer(state.dependencies.cancel_timer, state.retry_timer)
  State(..state, heartbeat_timer: None, state_timer: None, retry_timer: None)
}

fn cancel_optional_timer(
  cancel: fn(timer) -> Nil,
  timer: Option(timer),
) -> Nil {
  case timer {
    Some(timer) -> cancel(timer)
    None -> Nil
  }
}

fn shutdown_runtime(state: State(connection, timer)) -> Nil {
  let state = cancel_connection_timers(state)
  case state.connection {
    Some(connection) -> state.dependencies.close(connection)
    None -> Nil
  }
}

fn emit_command_bridge_startup_log(
  settings: Settings,
  dependencies: Dependencies(connection, timer),
) -> Nil {
  case settings.command_bridge_enabled {
    True -> {
      let _ =
        dependencies.logger(
          "info",
          "ui_websocket_command_bridge_enabled",
          [#("message", "remote command/result bridge enabled")],
          settings.redaction_secrets,
        )
      Nil
    }
    False -> Nil
  }
}

fn emit_log(
  state: State(connection, timer),
  level: String,
  event: String,
  fields: List(log.Field),
) -> Nil {
  case
    state.dependencies.logger(
      level,
      event,
      fields,
      state.settings.redaction_secrets,
    )
  {
    Ok(Nil) | Error(Nil) -> Nil
  }
}

fn spawn_reader(
  subject: process.Subject(Message),
  recv_text: fn(connection, Int) -> Result(String, String),
  connection: connection,
  generation: Int,
) -> Nil {
  let _ =
    process.spawn_unlinked(fn() {
      reader_loop(subject, recv_text, connection, generation)
    })
  Nil
}

fn reader_loop(
  subject: process.Subject(Message),
  recv_text: fn(connection, Int) -> Result(String, String),
  connection: connection,
  generation: Int,
) -> Nil {
  case recv_text(connection, 1000) {
    Ok(payload) -> {
      process.send(subject, ReaderText(generation, payload))
      reader_loop(subject, recv_text, connection, generation)
    }
    Error("timeout") -> reader_loop(subject, recv_text, connection, generation)
    Error(message) -> process.send(subject, ReaderFailed(generation, message))
  }
}

fn spawn_apply_worker(
  subject: process.Subject(Message),
  apply_command: fn(command.OperatorCommand, Int) ->
    Result(command.CommandResult, Nil),
  generation: Int,
  command_id: String,
  operator_command: command.OperatorCommand,
  timeout_ms: Int,
) -> Nil {
  let _worker =
    process.spawn_unlinked(fn() {
      let result = case apply_command(operator_command, timeout_ms) {
        Ok(result) -> result
        Error(Nil) ->
          command.rejected(
            operator_command,
            "remote_command_timeout",
            Some("remote command timed out"),
          )
      }
      process.send(
        subject,
        ApplyCompleted(generation, command_id, operator_command, result),
      )
    })
  Nil
}

fn update_known_dispatch_paused(
  current: Bool,
  operator_command: command.OperatorCommand,
  result: command.CommandResult,
) -> Bool {
  case result.status {
    command.Applied ->
      case operator_command {
        command.PauseDispatch -> True
        command.ResumeDispatch -> False
        _ -> current
      }
    _ -> current
  }
}

fn normalize_settings(settings: Settings) -> Settings {
  let retry_initial_ms = normalize_positive(settings.retry_initial_ms)
  let retry_max_ms = case settings.retry_max_ms < retry_initial_ms {
    True -> retry_initial_ms
    False -> normalize_positive(settings.retry_max_ms)
  }
  Settings(
    ..settings,
    heartbeat_interval_ms: normalize_positive(settings.heartbeat_interval_ms),
    state_interval_ms: normalize_positive(settings.state_interval_ms),
    retry_initial_ms: retry_initial_ms,
    retry_max_ms: retry_max_ms,
    connect_timeout_ms: normalize_positive(settings.connect_timeout_ms),
    command_timeout_ms: normalize_positive(settings.command_timeout_ms),
  )
}

fn normalize_positive(value: Int) -> Int {
  case value <= 0 {
    True -> 1
    False -> value
  }
}
