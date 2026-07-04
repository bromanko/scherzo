import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/control/query/types as query_types
import scherzo/control/remote/credential_store
import scherzo/control/remote/ui_managed_auth
import scherzo/control/remote/ui_protocol
import scherzo/control/remote/ui_websocket_client
import scherzo/control/remote/url
import scherzo/daemon_identity
import scherzo/log
import scherzo/managed_launch/grant as managed_launch_grant
import scherzo/path
import scherzo/session/event
import scherzo/session/hub
import scherzo/version
import scherzo/work_item_invalidation

pub type StartError {
  StartError(code: String, message: String)
}

pub opaque type Handle {
  Handle(ui_websocket_client.Handle)
}

type Socket

pub type AgentSlotOccupancyError {
  MetricsQueryFailed(query_types.QueryErrorCode)
  UnexpectedMetricsQueryResponse
}

pub fn start(
  effective: config_types.EffectiveConfig,
  managed_launch: Option(managed_launch_grant.Grant),
  event_hub: process.Subject(hub.Message),
  secrets: List(String),
  logger: fn(String, String, List(log.Field), List(String)) -> Result(Nil, Nil),
) -> Result(Handle, StartError) {
  start_with_managed_auth_rejection(
    effective,
    managed_launch,
    event_hub,
    fn(_) { Nil },
    secrets,
    logger,
  )
}

pub fn start_with_managed_auth_rejection(
  effective: config_types.EffectiveConfig,
  managed_launch: Option(managed_launch_grant.Grant),
  event_hub: process.Subject(hub.Message),
  managed_auth_rejected: fn(String) -> Nil,
  secrets: List(String),
  logger: fn(String, String, List(log.Field), List(String)) -> Result(Nil, Nil),
) -> Result(Handle, StartError) {
  start_with_control_and_managed_auth_rejection(
    effective,
    managed_launch,
    event_hub,
    fn(operator_command, _) {
      Ok(command.not_allowed(
        operator_command,
        "remote_command_unavailable",
        Some("remote command bridge is unavailable"),
      ))
    },
    fn(_) { Ok(False) },
    fn(_, _) {
      Error(query_types.QueryError(
        query_types.UnsupportedQuery,
        "ui websocket query bridge is unavailable",
      ))
    },
    managed_auth_rejected,
    secrets,
    logger,
  )
}

pub fn start_with_control(
  effective: config_types.EffectiveConfig,
  managed_launch: Option(managed_launch_grant.Grant),
  event_hub: process.Subject(hub.Message),
  apply_command: fn(command.OperatorCommand, Int) ->
    Result(command.CommandResult, Nil),
  dispatch_paused: fn(Int) -> Result(Bool, Nil),
  execute_query: fn(query_types.QueryRequest, Int) ->
    Result(query_types.QueryResponse, query_types.QueryError),
  secrets: List(String),
  logger: fn(String, String, List(log.Field), List(String)) -> Result(Nil, Nil),
) -> Result(Handle, StartError) {
  start_with_control_and_managed_auth_rejection(
    effective,
    managed_launch,
    event_hub,
    apply_command,
    dispatch_paused,
    execute_query,
    fn(_) { Nil },
    secrets,
    logger,
  )
}

pub fn start_with_control_and_managed_auth_rejection(
  effective: config_types.EffectiveConfig,
  managed_launch: Option(managed_launch_grant.Grant),
  event_hub: process.Subject(hub.Message),
  apply_command: fn(command.OperatorCommand, Int) ->
    Result(command.CommandResult, Nil),
  dispatch_paused: fn(Int) -> Result(Bool, Nil),
  execute_query: fn(query_types.QueryRequest, Int) ->
    Result(query_types.QueryResponse, query_types.QueryError),
  managed_auth_rejected: fn(String) -> Nil,
  secrets: List(String),
  logger: fn(String, String, List(log.Field), List(String)) -> Result(Nil, Nil),
) -> Result(Handle, StartError) {
  use identity <- result.try(load_daemon_identity(effective.workspace.root))
  use settings <- result.try(build_settings(
    effective,
    managed_launch,
    identity,
    secrets,
  ))
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
      agent_slot_occupancy: fn(timeout_ms) {
        case agent_slot_occupancy_from_query(execute_query, timeout_ms) {
          Ok(occupied_slots) -> Ok(occupied_slots)
          Error(error) -> Error(agent_slot_occupancy_error_message(error))
        }
      },
      dispatch_paused: fn(timeout_ms) {
        case dispatch_paused(timeout_ms) {
          Ok(paused) -> Ok(paused)
          Error(Nil) -> Error("daemon_dispatch_paused_timeout")
        }
      },
      apply_command: apply_command,
      execute_query: execute_query,
      managed_auth_rejected: managed_auth_rejected,
      logger: logger,
    )
  case ui_websocket_client.start(settings, dependencies) {
    Ok(handle) -> Ok(Handle(handle))
    Error(ui_websocket_client.ClientError(code: code, message: message)) ->
      Error(StartError(code, message))
  }
}

fn build_settings(
  effective: config_types.EffectiveConfig,
  managed_launch: Option(managed_launch_grant.Grant),
  identity: daemon_identity.DaemonIdentity,
  secrets: List(String),
) -> Result(ui_websocket_client.Settings, StartError) {
  case managed_launch {
    Some(grant) ->
      build_managed_launch_settings(effective, grant, identity, secrets)
    None -> build_durable_settings(effective, identity, secrets)
  }
}

fn build_durable_settings(
  effective: config_types.EffectiveConfig,
  identity: daemon_identity.DaemonIdentity,
  secrets: List(String),
) -> Result(ui_websocket_client.Settings, StartError) {
  case effective.ui_server {
    config_types.UiServerDisabled(..) ->
      Error(StartError("remote_client_config_disabled", "ui_server is disabled"))
    config_types.UiServerEnabled(
      endpoint: endpoint,
      credential_ref: credential_ref_name,
      daemon_label: daemon_label,
      command_bridge_enabled: command_bridge_enabled,
      heartbeat_interval_ms: heartbeat_interval_ms,
      state_interval_ms: state_interval_ms,
      retry_initial_ms: retry_initial_ms,
      retry_max_ms: retry_max_ms,
    ) -> {
      use validated <- result.try(validated_ui_server_endpoint(endpoint))
      use credential_ref <- result.try(normalized_credential_ref(
        credential_ref_name,
      ))
      use stored <- result.try(load_stored_credential(
        credential_ref,
        validated.base_url,
        identity.daemon_id,
      ))
      Ok(
        ui_websocket_client.Settings(
          server_url: validated.base_url,
          websocket_url: validated.websocket_url,
          daemon_id: identity.daemon_id,
          boot_id: identity.boot_id,
          runtime_metadata: ui_protocol.RuntimeMetadata(
            host: local_hostname(),
            scherzo_version: version.string(),
            daemon_label: daemon_label,
            agent_slot_capacity: effective.agent.max_concurrent_agents,
            managed_launch_context: None,
          ),
          credential: stored.secret,
          managed_launch_auth: None,
          heartbeat_interval_ms: heartbeat_interval_ms,
          state_interval_ms: state_interval_ms,
          retry_initial_ms: retry_initial_ms,
          retry_max_ms: retry_max_ms,
          connect_timeout_ms: 1000,
          command_timeout_ms: effective.control.command_timeout_ms,
          query_timeout_ms: effective.control.command_timeout_ms,
          command_bridge_enabled: command_bridge_enabled,
          redaction_secrets: [stored.secret, ..secrets],
        ),
      )
    }
  }
}

fn build_managed_launch_settings(
  effective: config_types.EffectiveConfig,
  grant: managed_launch_grant.Grant,
  identity: daemon_identity.DaemonIdentity,
  secrets: List(String),
) -> Result(ui_websocket_client.Settings, StartError) {
  let #(
    daemon_label,
    heartbeat_interval_ms,
    state_interval_ms,
    retry_initial_ms,
    retry_max_ms,
    command_bridge_enabled,
  ) = managed_launch_runtime_settings(effective, grant)
  let capabilities =
    effective_managed_launch_capabilities(grant, command_bridge_enabled)
  Ok(ui_websocket_client.Settings(
    server_url: grant.endpoint.base_url,
    websocket_url: grant.endpoint.websocket_url,
    daemon_id: identity.daemon_id,
    boot_id: identity.boot_id,
    runtime_metadata: ui_protocol.RuntimeMetadata(
      host: local_hostname(),
      scherzo_version: version.string(),
      daemon_label: daemon_label,
      agent_slot_capacity: effective.agent.max_concurrent_agents,
      managed_launch_context: Some(ui_protocol.ManagedLaunchContext(
        launch_id: grant.launch_id,
        capabilities: capabilities,
      )),
    ),
    credential: "",
    managed_launch_auth: Some(ui_managed_auth.ManagedLaunchAuth(
      launch_credential: Some(grant.credential),
      launch_expires_at_ms: grant.expires_at_ms,
      runtime_credential: None,
    )),
    heartbeat_interval_ms: heartbeat_interval_ms,
    state_interval_ms: state_interval_ms,
    retry_initial_ms: retry_initial_ms,
    retry_max_ms: retry_max_ms,
    connect_timeout_ms: 1000,
    command_timeout_ms: effective.control.command_timeout_ms,
    query_timeout_ms: effective.control.command_timeout_ms,
    command_bridge_enabled: command_bridge_enabled,
    redaction_secrets: secrets,
  ))
}

fn managed_launch_runtime_settings(
  effective: config_types.EffectiveConfig,
  grant: managed_launch_grant.Grant,
) -> #(Option(String), Int, Int, Int, Int, Bool) {
  case effective.ui_server {
    config_types.UiServerEnabled(
      daemon_label: configured_label,
      command_bridge_enabled: configured_bridge,
      heartbeat_interval_ms: heartbeat_interval_ms,
      state_interval_ms: state_interval_ms,
      retry_initial_ms: retry_initial_ms,
      retry_max_ms: retry_max_ms,
      ..,
    ) -> #(
      preferred_daemon_label(grant.daemon_label, configured_label),
      heartbeat_interval_ms,
      state_interval_ms,
      retry_initial_ms,
      retry_max_ms,
      configured_bridge
        && grant.command_bridge_enabled
        && managed_launch_grant.has_capability(
        grant,
        managed_launch_grant.Command,
      ),
    )
    config_types.UiServerDisabled(daemon_label: configured_label, ..) -> #(
      preferred_daemon_label(grant.daemon_label, configured_label),
      5000,
      5000,
      500,
      5000,
      grant.command_bridge_enabled
        && managed_launch_grant.has_capability(
        grant,
        managed_launch_grant.Command,
      ),
    )
  }
}

fn effective_managed_launch_capabilities(
  grant: managed_launch_grant.Grant,
  command_bridge_enabled: Bool,
) -> List(managed_launch_grant.Capability) {
  case command_bridge_enabled {
    True -> grant.capabilities
    False ->
      list.filter(grant.capabilities, fn(capability) {
        capability != managed_launch_grant.Command
      })
  }
}

fn preferred_daemon_label(
  grant_label: Option(String),
  configured_label: Option(String),
) -> Option(String) {
  case grant_label {
    Some(_) -> grant_label
    None -> configured_label
  }
}

fn agent_slot_occupancy_from_query(
  execute_query: fn(query_types.QueryRequest, Int) ->
    Result(query_types.QueryResponse, query_types.QueryError),
  timeout_ms: Int,
) -> Result(Int, AgentSlotOccupancyError) {
  agent_slot_occupancy_from_query_response(execute_query(
    query_types.Metrics,
    timeout_ms,
  ))
}

pub fn agent_slot_occupancy_from_query_response(
  query_result: Result(query_types.QueryResponse, query_types.QueryError),
) -> Result(Int, AgentSlotOccupancyError) {
  case query_result {
    Ok(query_types.MetricsResponse(metrics)) ->
      Ok(query_types.operational_metrics_agent_slot_occupancy(metrics))
    Ok(_) -> Error(UnexpectedMetricsQueryResponse)
    Error(query_types.QueryError(code: code, ..)) ->
      Error(MetricsQueryFailed(code))
  }
}

fn agent_slot_occupancy_error_message(
  error: AgentSlotOccupancyError,
) -> String {
  case error {
    UnexpectedMetricsQueryResponse -> "daemon_metrics_query_unexpected_response"
    MetricsQueryFailed(code) ->
      "daemon_metrics_query_failed:" <> query_types.error_code_to_string(code)
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

pub fn notify_work_item_invalidation(
  handle: Handle,
  event: work_item_invalidation.Event,
) -> Nil {
  let Handle(handle) = handle
  ui_websocket_client.notify_work_item_invalidation(handle, event)
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

fn validated_ui_server_endpoint(
  endpoint: String,
) -> Result(url.ValidatedUrl, StartError) {
  url.validate_server_url(endpoint, allow_loopback: True)
  |> result.map_error(fn(error) {
    StartError(url.error_code(error), url.error_message(error))
  })
}

fn normalized_credential_ref(
  profile: String,
) -> Result(credential_store.CredentialRef, StartError) {
  credential_store.normalize_credential_ref(profile)
  |> result.map_error(fn(message) {
    StartError("invalid_credential_ref", message)
  })
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

fn local_hostname() -> String {
  case first_non_empty_env(["HOSTNAME", "COMPUTERNAME"]) {
    Some(hostname) -> hostname
    None -> "unknown"
  }
}

fn first_non_empty_env(names: List(String)) -> Option(String) {
  case names {
    [] -> None
    [name, ..rest] ->
      case path.env(name) {
        Some(value) -> {
          let value = string.trim(value)
          case value == "" {
            True -> first_non_empty_env(rest)
            False -> Some(value)
          }
        }
        None -> first_non_empty_env(rest)
      }
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
