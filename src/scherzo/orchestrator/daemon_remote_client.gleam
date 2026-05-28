import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/uri
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/control/remote/client
import scherzo/control/remote_envelope
import scherzo/daemon_identity
import scherzo/log
import scherzo/session/event
import scherzo/session/hub

pub type StartError {
  StartError(code: String, message: String)
}

pub opaque type Handle {
  Handle(client.Handle)
}

type Socket

pub fn start(
  effective: config_types.EffectiveConfig,
  event_hub: process.Subject(hub.Message),
  secrets: List(String),
  logger: fn(String, String, List(log.Field), List(String)) -> Result(Nil, Nil),
) -> Result(Handle, StartError) {
  start_with_control(
    effective,
    event_hub,
    fn(operator_command, _timeout_ms) {
      Ok(command.rejected(
        operator_command,
        "remote_control_unavailable",
        Some("remote control callbacks unavailable"),
      ))
    },
    fn(_) { Ok(False) },
    secrets,
    logger,
  )
}

pub fn start_with_control(
  effective: config_types.EffectiveConfig,
  event_hub: process.Subject(hub.Message),
  apply_command: fn(command.OperatorCommand, Int) ->
    Result(command.CommandResult, Nil),
  dispatch_paused: fn(Int) -> Result(Bool, Nil),
  secrets: List(String),
  logger: fn(String, String, List(log.Field), List(String)) -> Result(Nil, Nil),
) -> Result(Handle, StartError) {
  use identity <- result.try(load_daemon_identity(effective.workspace.root))
  use endpoint <- result.try(required_ui_server_endpoint(effective))
  use _ <- result.try(validate_endpoint(endpoint))
  use enrollment_token <- result.try(required_ui_server_enrollment_token(
    effective,
  ))
  let settings =
    client.Settings(
      endpoint: endpoint,
      daemon_id: identity.daemon_id,
      boot_id: identity.boot_id,
      enrollment_token: enrollment_token,
      capabilities: ["control_commands", "session_snapshots"],
      heartbeat_interval_ms: 5000,
      state_interval_ms: 5000,
      retry_initial_ms: 500,
      retry_max_ms: 30_000,
      connect_timeout_ms: 1000,
      command_timeout_ms: 1000,
      redaction_secrets: secrets,
    )
  let dependencies =
    client.Dependencies(
      now_ms: wall_clock_ms,
      connect: connect_endpoint,
      send_line: socket_send_line,
      recv_line: socket_recv_line,
      close: socket_close,
      send_after: process.send_after,
      cancel_timer: fn(timer) {
        let _ = process.cancel_timer(timer)
        Nil
      },
      list_sessions: fn() { list_sessions_for_remote_snapshot(event_hub, 1000) },
      apply_command: apply_command,
      dispatch_paused: fn(timeout_ms) {
        case dispatch_paused(timeout_ms) {
          Ok(paused) -> Ok(paused)
          Error(Nil) -> Error("daemon_dispatch_paused_timeout")
        }
      },
      logger: logger,
    )
  case client.start(settings, dependencies) {
    Ok(handle) -> Ok(Handle(handle))
    Error(client.ClientError(code: code, message: message)) ->
      Error(StartError(code, message))
  }
}

pub fn wrap(handle: client.Handle) -> Handle {
  Handle(handle)
}

pub fn stop(handle: Handle, timeout_ms: Int) -> Result(Nil, Nil) {
  let Handle(handle) = handle
  client.stop(handle, timeout_ms)
}

pub fn monitor(handle: Handle) -> process.Monitor {
  let Handle(handle) = handle
  client.monitor(handle)
}

pub fn kill(handle: Handle) -> Nil {
  let Handle(handle) = handle
  client.kill(handle)
}

fn load_daemon_identity(
  workspace_root: String,
) -> Result(daemon_identity.DaemonIdentity, StartError) {
  case daemon_identity.load_or_create(workspace_root) {
    Ok(identity) -> Ok(identity)
    Error(err) ->
      Error(StartError(
        "daemon_identity_failed",
        daemon_identity.error_message(err),
      ))
  }
}

fn required_ui_server_endpoint(
  effective: config_types.EffectiveConfig,
) -> Result(String, StartError) {
  case effective.ui_server.endpoint {
    Some(endpoint) -> Ok(endpoint)
    None ->
      Error(StartError(
        "remote_client_config_missing",
        "ui_server.endpoint is required when enabled",
      ))
  }
}

fn validate_endpoint(endpoint: String) -> Result(Nil, StartError) {
  case parse_endpoint(endpoint) {
    Ok(_) -> Ok(Nil)
    Error(message) ->
      Error(StartError(message, "invalid ui_server.endpoint: " <> message))
  }
}

fn required_ui_server_enrollment_token(
  effective: config_types.EffectiveConfig,
) -> Result(String, StartError) {
  case effective.ui_server.enrollment_token {
    Some(enrollment_token) -> Ok(enrollment_token)
    None ->
      Error(StartError(
        "remote_client_config_missing",
        "ui_server.enrollment_token is required when enabled",
      ))
  }
}

pub fn list_sessions_for_remote_snapshot(
  event_hub: process.Subject(hub.Message),
  timeout_ms: Int,
) -> Result(List(remote_envelope.RemoteSession), String) {
  case hub.list_sessions_snapshot(event_hub, timeout_ms) {
    Ok(event.SessionList(sessions: sessions, ..)) ->
      Ok(sessions |> list.map(remote_session_from_summary))
    Error(hub.ActorCallTimeout) -> Error("event_hub_list_sessions_timeout")
    Error(hub.HubUnavailable) -> Error("event_hub_unavailable")
    Error(hub.InvalidLimit(_)) -> Error("event_hub_list_sessions_invalid_limit")
    Error(hub.SessionNotFound(_)) -> Error("event_hub_session_not_found")
  }
}

fn remote_session_from_summary(
  summary: event.SessionSummary,
) -> remote_envelope.RemoteSession {
  remote_envelope.RemoteSession(
    session_id: summary.session_id,
    display_name: summary.display_name,
    issue_identifier: summary.issue_identifier,
    status: event.status_to_string(summary.status),
    current_turn: summary.current_turn,
    last_event_at_ms: summary.last_event_at_ms,
  )
}

fn connect_endpoint(
  endpoint: String,
  timeout_ms: Int,
) -> Result(Socket, String) {
  use #(host, port) <- result.try(parse_endpoint(endpoint))
  socket_connect(host, port, timeout_ms)
}

fn parse_endpoint(endpoint: String) -> Result(#(String, Int), String) {
  case uri.parse(endpoint) {
    Ok(uri.Uri(host: Some(host), port: port, ..)) ->
      Ok(
        #(host, case port {
          Some(port) -> port
          None -> 443
        }),
      )
    Ok(_) -> Error("remote_client_endpoint_missing_host")
    Error(_) -> Error("remote_client_endpoint_invalid")
  }
}

// nolint: stringly_typed_error -- erlang socket ffi returns raw transport errors.
@external(erlang, "scherzo_control_ffi", "connect")
fn socket_connect(
  host: String,
  port: Int,
  timeout_ms: Int,
) -> Result(Socket, String)

// nolint: stringly_typed_error -- erlang socket ffi returns raw transport errors.
@external(erlang, "scherzo_control_ffi", "send_line")
fn socket_send_line(
  socket: Socket,
  line: String,
  timeout_ms: Int,
) -> Result(Nil, String)

// nolint: stringly_typed_error -- erlang socket ffi returns raw transport errors.
@external(erlang, "scherzo_control_ffi", "recv_line")
fn socket_recv_line(socket: Socket, timeout_ms: Int) -> Result(String, String)

@external(erlang, "scherzo_control_ffi", "close_socket")
fn socket_close(socket: Socket) -> Nil

@external(erlang, "scherzo_time_ffi", "wall_clock_ms")
fn wall_clock_ms() -> Int
