import gleam/erlang/process
import gleam/option.{None, Some}
import gleam/result
import scherzo/config/types as config_types
import scherzo/control/remote/credential_store
import scherzo/control/remote/ui_websocket_client
import scherzo/control/remote/url
import scherzo/daemon_identity
import scherzo/log
import scherzo/session/event
import scherzo/session/hub

pub type StartError {
  StartError(code: String, message: String)
}

pub opaque type Handle {
  Handle(ui_websocket_client.Handle)
}

type Socket

pub fn start(
  effective: config_types.EffectiveConfig,
  event_hub: process.Subject(hub.Message),
  secrets: List(String),
  logger: fn(String, String, List(log.Field), List(String)) -> Result(Nil, Nil),
) -> Result(Handle, StartError) {
  start_with_control(effective, event_hub, fn(_) { Ok(False) }, secrets, logger)
}

pub fn start_with_control(
  effective: config_types.EffectiveConfig,
  event_hub: process.Subject(hub.Message),
  dispatch_paused: fn(Int) -> Result(Bool, Nil),
  secrets: List(String),
  logger: fn(String, String, List(log.Field), List(String)) -> Result(Nil, Nil),
) -> Result(Handle, StartError) {
  use identity <- result.try(load_daemon_identity(effective.workspace.root))
  use validated <- result.try(required_validated_ui_server(effective))
  use credential_ref <- result.try(required_credential_ref(effective))
  use stored <- result.try(load_stored_credential(
    credential_ref,
    validated.base_url,
    identity.daemon_id,
  ))
  let settings =
    ui_websocket_client.Settings(
      server_url: validated.base_url,
      websocket_url: validated.websocket_url,
      daemon_id: identity.daemon_id,
      boot_id: identity.boot_id,
      daemon_label: effective.ui_server.daemon_label,
      credential: stored.secret,
      heartbeat_interval_ms: effective.ui_server.heartbeat_interval_ms,
      state_interval_ms: effective.ui_server.state_interval_ms,
      retry_initial_ms: effective.ui_server.retry_initial_ms,
      retry_max_ms: effective.ui_server.retry_max_ms,
      connect_timeout_ms: 1000,
      command_bridge_enabled: effective.ui_server.command_bridge_enabled,
      redaction_secrets: [stored.secret, ..secrets],
    )
  let dependencies =
    ui_websocket_client.Dependencies(
      now_ms: wall_clock_ms,
      connect: connect_endpoint,
      send_text: socket_send_text,
      recv_text: socket_recv_text,
      close: socket_close,
      send_after: process.send_after,
      cancel_timer: fn(timer) {
        let _ = process.cancel_timer(timer)
        Nil
      },
      list_sessions: fn() { list_sessions_for_remote_snapshot(event_hub, 1000) },
      dispatch_paused: fn(timeout_ms) {
        case dispatch_paused(timeout_ms) {
          Ok(paused) -> Ok(paused)
          Error(Nil) -> Error("daemon_dispatch_paused_timeout")
        }
      },
      logger: logger,
    )
  case ui_websocket_client.start(settings, dependencies) {
    Ok(handle) -> Ok(Handle(handle))
    Error(ui_websocket_client.ClientError(code: code, message: message)) ->
      Error(StartError(code, message))
  }
}

pub fn wrap(handle: ui_websocket_client.Handle) -> Handle {
  Handle(handle)
}

pub fn stop(handle: Handle, timeout_ms: Int) -> Result(Nil, Nil) {
  let Handle(handle) = handle
  ui_websocket_client.stop(handle, timeout_ms)
}

pub fn monitor(handle: Handle) -> process.Monitor {
  let Handle(handle) = handle
  ui_websocket_client.monitor(handle)
}

pub fn kill(handle: Handle) -> Nil {
  let Handle(handle) = handle
  ui_websocket_client.kill(handle)
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

fn required_validated_ui_server(
  effective: config_types.EffectiveConfig,
) -> Result(url.ValidatedUrl, StartError) {
  case effective.ui_server.endpoint {
    Some(endpoint) ->
      url.validate_server_url(endpoint, allow_loopback: True)
      |> result.map_error(fn(error) {
        StartError(url.error_code(error), url.error_message(error))
      })
    None ->
      Error(StartError(
        "remote_client_config_missing",
        "ui_server.endpoint is required when enabled",
      ))
  }
}

fn required_credential_ref(
  effective: config_types.EffectiveConfig,
) -> Result(credential_store.CredentialRef, StartError) {
  case effective.ui_server.credential_ref {
    Some(profile) ->
      credential_store.normalize_credential_ref(profile)
      |> result.map_error(fn(message) {
        StartError("invalid_credential_ref", message)
      })
    None ->
      Error(StartError(
        "remote_client_config_missing",
        "ui_server.credential_ref is required when enabled",
      ))
  }
}

fn load_stored_credential(
  ref: credential_store.CredentialRef,
  server_url: String,
  daemon_id: String,
) -> Result(credential_store.StoredCredential, StartError) {
  case credential_store.read_credential(ref, server_url, daemon_id) {
    Ok(Some(credential)) -> Ok(credential)
    Ok(None) ->
      Error(StartError(
        "missing_daemon_credential",
        "run scherzo connect to store a daemon credential before enabling ui_server",
      ))
    Error(error) ->
      Error(StartError(
        "credential_store_failed",
        credential_store.error_message(error),
      ))
  }
}

pub fn list_sessions_for_remote_snapshot(
  event_hub: process.Subject(hub.Message),
  timeout_ms: Int,
) -> Result(List(event.SessionSummary), String) {
  case hub.list_sessions_snapshot(event_hub, timeout_ms) {
    Ok(event.SessionList(sessions: sessions, ..)) -> Ok(sessions)
    Error(hub.ActorCallTimeout) -> Error("event_hub_list_sessions_timeout")
    Error(hub.HubUnavailable) -> Error("event_hub_unavailable")
    Error(hub.InvalidLimit(_)) -> Error("event_hub_list_sessions_invalid_limit")
    Error(hub.SessionNotFound(_)) -> Error("event_hub_session_not_found")
  }
}

fn connect_endpoint(
  websocket_url: String,
  credential: String,
  timeout_ms: Int,
) -> Result(Socket, String) {
  websocket_connect(websocket_url, credential, timeout_ms)
}

fn socket_send_text(
  socket: Socket,
  payload: String,
  timeout_ms: Int,
) -> Result(Nil, String) {
  websocket_send_text(socket, payload, timeout_ms)
}

fn socket_recv_text(socket: Socket, timeout_ms: Int) -> Result(String, String) {
  websocket_recv_text(socket, timeout_ms)
}

// nolint: stringly_typed_error -- websocket ffi returns raw transport errors.
@external(erlang, "scherzo_remote_websocket_ffi", "websocket_connect")
fn websocket_connect(
  websocket_url: String,
  credential: String,
  timeout_ms: Int,
) -> Result(Socket, String)

// nolint: stringly_typed_error -- websocket ffi returns raw transport errors.
@external(erlang, "scherzo_remote_websocket_ffi", "websocket_send_text")
fn websocket_send_text(
  socket: Socket,
  line: String,
  timeout_ms: Int,
) -> Result(Nil, String)

// nolint: stringly_typed_error -- websocket ffi returns raw transport errors.
@external(erlang, "scherzo_remote_websocket_ffi", "websocket_recv_text")
fn websocket_recv_text(
  socket: Socket,
  timeout_ms: Int,
) -> Result(String, String)

@external(erlang, "scherzo_remote_websocket_ffi", "websocket_close")
fn socket_close(socket: Socket) -> Nil

@external(erlang, "scherzo_time_ffi", "wall_clock_ms")
fn wall_clock_ms() -> Int
