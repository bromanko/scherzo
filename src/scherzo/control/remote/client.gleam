import gleam/erlang/process
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import scherzo/control/remote_envelope
import scherzo/control/remote_harness_hello
import scherzo/log

pub type Settings {
  Settings(
    endpoint: String,
    daemon_id: String,
    boot_id: String,
    enrollment_token: String,
    capabilities: List(String),
    heartbeat_interval_ms: Int,
    state_interval_ms: Int,
    retry_initial_ms: Int,
    retry_max_ms: Int,
    connect_timeout_ms: Int,
    redaction_secrets: List(String),
  )
}

pub type Dependencies(connection, timer) {
  Dependencies(
    now_ms: fn() -> Int,
    connect: fn(String, Int) -> Result(connection, String),
    send_line: fn(connection, String, Int) -> Result(Nil, String),
    close: fn(connection) -> Nil,
    send_after: fn(process.Subject(Message), Int, Message) -> timer,
    cancel_timer: fn(timer) -> Nil,
    list_sessions: fn() -> Result(List(remote_envelope.RemoteSession), String),
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
  )
}

pub fn start(
  settings: Settings,
  dependencies: Dependencies(connection, timer),
) -> Result(Handle, ClientError) {
  let settings = normalize_settings(settings)
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
      Error(ClientError("remote_client_start_failed", "actor start failed"))
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
  case state.connection {
    Some(_) -> state
    None -> {
      emit_log(state, "info", "remote_client_connecting", [
        #("endpoint", state.settings.endpoint),
        #("enrollment_token", state.settings.enrollment_token),
      ])
      case
        state.dependencies.connect(
          state.settings.endpoint,
          state.settings.connect_timeout_ms,
        )
      {
        Ok(connection) -> handle_connected(state, connection)
        Error(message) ->
          schedule_retry_after_failure(state, "connect_failed", message)
      }
    }
  }
}

fn handle_connected(
  state: State(connection, timer),
  connection: connection,
) -> State(connection, timer) {
  emit_log(state, "info", "remote_client_connected", [
    #("endpoint", state.settings.endpoint),
    #("enrollment_token", state.settings.enrollment_token),
  ])
  case send_hello(connection, state) {
    Ok(Nil) ->
      case send_heartbeat(connection, state) {
        Ok(Nil) ->
          case send_state_snapshot(connection, state) {
            Ok(Nil) ->
              State(
                ..state,
                connection: Some(connection),
                next_retry_ms: state.settings.retry_initial_ms,
              )
              |> schedule_heartbeat_timer()
              |> schedule_state_timer()
            Error(message) ->
              retry_after_send_failure(
                state,
                connection,
                "state_send_failed",
                message,
              )
          }
        Error(message) ->
          retry_after_send_failure(
            state,
            connection,
            "heartbeat_send_failed",
            message,
          )
      }
    Error(message) ->
      retry_after_send_failure(state, connection, "hello_send_failed", message)
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
            "heartbeat_send_failed",
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
            "state_send_failed",
            message,
          )
      }
    }
  }
}

fn send_hello(
  connection: connection,
  state: State(connection, timer),
) -> Result(Nil, String) {
  remote_harness_hello.encode(
    state.settings.daemon_id,
    state.settings.boot_id,
    state.settings.enrollment_token,
    state.settings.capabilities,
  )
  |> state.dependencies.send_line(
    connection,
    _,
    state.settings.connect_timeout_ms,
  )
}

fn send_heartbeat(
  connection: connection,
  state: State(connection, timer),
) -> Result(Nil, String) {
  remote_envelope.RemoteHeartbeat(state.dependencies.now_ms())
  |> remote_envelope.to_string
  |> state.dependencies.send_line(
    connection,
    _,
    state.settings.connect_timeout_ms,
  )
}

fn send_state_snapshot(
  connection: connection,
  state: State(connection, timer),
) -> Result(Nil, String) {
  use sessions <- result.try(state.dependencies.list_sessions())
  remote_envelope.RemoteStateSnapshot(
    now_ms: state.dependencies.now_ms(),
    sessions: sessions,
  )
  |> remote_envelope.to_string
  |> state.dependencies.send_line(
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

fn schedule_retry_after_failure(
  state: State(connection, timer),
  event: String,
  message: String,
) -> State(connection, timer) {
  let retry_delay_ms = state.next_retry_ms
  emit_log(state, "warn", event, [
    #("endpoint", state.settings.endpoint),
    #("reason", message),
    #("retry_delay_ms", int.to_string(retry_delay_ms)),
    #("enrollment_token", state.settings.enrollment_token),
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
      state.settings.heartbeat_interval_ms,
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
    Ok(Nil) -> Nil
    Error(Nil) -> Nil
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
  )
}

fn normalize_positive(value: Int) -> Int {
  case value <= 0 {
    True -> 1
    False -> value
  }
}
