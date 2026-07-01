import gleam/erlang/process
import gleam/option.{None, Some}
import gleam/string
import scherzo/config
import scherzo/config/types as config_types
import scherzo/control/query/types as query_types
import scherzo/control/remote/credential_store
import scherzo/daemon_identity
import scherzo/managed_launch/grant as managed_launch_grant
import scherzo/orchestrator/daemon_remote_client
import scherzo/session/hub
import support/remote_ui_test_server
import support/test_helpers
import test_async

type QueryRequestCall {
  QueryRequestCall(query: query_types.QueryRequest, timeout_ms: Int)
}

pub fn daemon_remote_client_rejects_disabled_config_test() {
  let root = "test/tmp/daemon-remote-client-disabled"
  test_helpers.reset_dir(root)
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let effective =
    effective_config_with_ui_server(
      root,
      config_types.UiServerDisabled(
        endpoint: None,
        credential_ref: None,
        daemon_label: None,
      ),
    )

  let assert Error(daemon_remote_client.StartError(code, message)) =
    daemon_remote_client.start(effective, None, event_hub, [], fn(_, _, _, _) {
      Ok(Nil)
    })
  assert code == "remote_client_config_disabled"
  assert message == "ui_server is disabled"
  hub.stop(event_hub)
}

pub fn daemon_remote_client_requires_stored_credential_test() {
  let root = "test/tmp/daemon-remote-client-missing-credential"
  test_helpers.reset_dir(root)
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let effective =
    effective_config(root, "https://ui.example.test", "work-laptop")

  let assert Error(daemon_remote_client.StartError(code, message)) =
    daemon_remote_client.start(effective, None, event_hub, [], fn(_, _, _, _) {
      Ok(Nil)
    })
  assert code == "missing_daemon_credential"
  assert message != ""
  hub.stop(event_hub)
}

pub fn daemon_remote_client_rejects_invalid_loopback_endpoint_test() {
  let root = "test/tmp/daemon-remote-client-invalid-endpoint"
  test_helpers.reset_dir(root)
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let effective = effective_config(root, "http://0.0.0.0:3000", "work-laptop")

  let assert Error(daemon_remote_client.StartError(code, _)) =
    daemon_remote_client.start(effective, None, event_hub, [], fn(_, _, _, _) {
      Ok(Nil)
    })
  assert code == "invalid_loopback_url"
  hub.stop(event_hub)
}

pub fn daemon_remote_client_uses_websocket_authorization_handshake_test() {
  let root = "test/tmp/daemon-remote-client-websocket"
  test_helpers.reset_dir(root)
  let transcript_path = root <> "/transcript.log"
  let server = remote_ui_test_server.start("dcred_secret_1", transcript_path)
  let assert Ok(identity) = daemon_identity.load_or_create(root)
  let assert Ok(ref) = credential_store.normalize_credential_ref("work-laptop")
  let assert Ok(_) =
    credential_store.write_credential(
      ref,
      remote_ui_test_server.server_url(server),
      identity.daemon_id,
      credential_store.DaemonCredential(Some("cred-1"), "dcred_secret_1"),
      False,
    )
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let effective =
    effective_config(
      root,
      remote_ui_test_server.server_url(server),
      "work-laptop",
    )

  let assert Ok(handle) =
    daemon_remote_client.start(effective, None, event_hub, [], fn(_, _, _, _) {
      Ok(Nil)
    })
  let transcript =
    remote_ui_test_server.wait_for_contains(
      transcript_path,
      "daemon_state",
      100,
    )
  assert string.contains(transcript, "authorization=Bearer dcred_secret_1")
  assert string.contains(transcript, "daemon_hello")
  assert string.contains(transcript, "heartbeat")
  assert string.contains(transcript, "daemon_state")
  assert string.contains(transcript, "\"state\":{")
  assert string.contains(transcript, "\"agentSlots\":{")
  assert string.contains(transcript, "\"event\":{")
  assert daemon_remote_client.stop(handle, 1000) == Ok(Nil)
  hub.stop(event_hub)
  remote_ui_test_server.stop(server)
}

pub fn daemon_remote_client_supports_managed_launch_without_durable_pairing_test() {
  let root = "test/tmp/daemon-remote-client-managed-launch"
  test_helpers.reset_dir(root)
  let transcript_path = root <> "/transcript.log"
  let server = remote_ui_test_server.start("launch_secret_1", transcript_path)
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let effective =
    effective_config_with_ui_server(
      root,
      config_types.UiServerDisabled(
        endpoint: None,
        credential_ref: None,
        daemon_label: Some("Configured Label"),
      ),
    )
  let assert Ok(grant) =
    managed_launch_grant.decode_string(
      "{\"version\":1,\"launchId\":\"launch-123\",\"endpoint\":\""
        <> remote_ui_test_server.server_url(server)
        <> "\",\"credential\":\"launch_secret_1\",\"daemonLabel\":\"Grant Label\",\"capabilities\":[\"state\",\"query\"],\"commandBridgeEnabled\":false,\"expiresAt\":\"2999-01-01T00:00:00Z\"}",
      0,
    )

  let assert Ok(handle) =
    daemon_remote_client.start(
      effective,
      Some(grant),
      event_hub,
      [],
      fn(_, _, _, _) { Ok(Nil) },
    )
  let transcript =
    remote_ui_test_server.wait_for_contains(transcript_path, "launch-123", 100)
  assert string.contains(transcript, "authorization=Bearer launch_secret_1")
  assert string.contains(transcript, "\"launchId\":\"launch-123\"")
  assert string.contains(transcript, "\"capabilities\":[\"state\",\"query\"]")
  assert string.contains(transcript, "\"daemonLabel\":\"Grant Label\"")
  assert daemon_remote_client.stop(handle, 1000) == Ok(Nil)
  hub.stop(event_hub)
  remote_ui_test_server.stop(server)
}

pub fn daemon_remote_client_omits_command_capability_when_bridge_is_disabled_test() {
  let root = "test/tmp/daemon-remote-client-managed-launch-command-disabled"
  test_helpers.reset_dir(root)
  let transcript_path = root <> "/transcript.log"
  let server = remote_ui_test_server.start("launch_secret_1", transcript_path)
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let effective =
    effective_config_with_ui_server(
      root,
      config_types.UiServerDisabled(
        endpoint: None,
        credential_ref: None,
        daemon_label: Some("Configured Label"),
      ),
    )
  let assert Ok(grant) =
    managed_launch_grant.decode_string(
      "{\"version\":1,\"launchId\":\"launch-123\",\"endpoint\":\""
        <> remote_ui_test_server.server_url(server)
        <> "\",\"credential\":\"launch_secret_1\",\"daemonLabel\":\"Grant Label\",\"capabilities\":[\"state\",\"query\",\"command\"],\"commandBridgeEnabled\":false,\"expiresAt\":\"2999-01-01T00:00:00Z\"}",
      0,
    )

  let assert Ok(handle) =
    daemon_remote_client.start(
      effective,
      Some(grant),
      event_hub,
      [],
      fn(_, _, _, _) { Ok(Nil) },
    )
  let transcript =
    remote_ui_test_server.wait_for_contains(transcript_path, "launch-123", 100)
  assert string.contains(transcript, "\"capabilities\":[\"state\",\"query\"]")
  assert !string.contains(transcript, "\"command\"")
  assert daemon_remote_client.stop(handle, 1000) == Ok(Nil)
  hub.stop(event_hub)
  remote_ui_test_server.stop(server)
}

pub fn daemon_remote_client_sums_metrics_slot_occupancy_fields_test() {
  let result =
    daemon_remote_client.agent_slot_occupancy_from_query_response(
      Ok(
        query_types.MetricsResponse(metrics_dto(
          active_sessions: 99,
          running_workers: 1,
          running_scheduled_workers: 2,
          queued_claims: 3,
          pending_dispatch_validations: 4,
          pending_review_lane_preflights: 5,
        )),
      ),
    )

  assert result == Ok(15)
}

pub fn daemon_remote_client_clamps_negative_metrics_slot_occupancy_test() {
  let result =
    daemon_remote_client.agent_slot_occupancy_from_query_response(
      Ok(
        query_types.MetricsResponse(metrics_dto(
          active_sessions: 0,
          running_workers: -1,
          running_scheduled_workers: -1,
          queued_claims: -1,
          pending_dispatch_validations: -1,
          pending_review_lane_preflights: -1,
        )),
      ),
    )

  assert result == Ok(0)
}

pub fn daemon_remote_client_rejects_unexpected_metrics_query_response_test() {
  let result =
    daemon_remote_client.agent_slot_occupancy_from_query_response(
      Ok(
        query_types.StatusResponse(query_types.StatusDto(
          daemon_id: "daemon-1",
          boot_id: "boot-1",
          dispatch_paused: False,
          ui_server_enabled: True,
          supported_queries: query_types.supported_queries(),
        )),
      ),
    )

  assert result == Error(daemon_remote_client.UnexpectedMetricsQueryResponse)
}

pub fn daemon_remote_client_maps_metrics_query_error_code_test() {
  let result =
    daemon_remote_client.agent_slot_occupancy_from_query_response(
      Error(query_types.QueryError(query_types.QueryTimeout, "timed out")),
    )

  assert result
    == Error(daemon_remote_client.MetricsQueryFailed(query_types.QueryTimeout))
}

pub fn daemon_remote_client_reports_agent_slots_from_metrics_bridge_test() {
  let root = "test/tmp/daemon-remote-client-metrics-bridge"
  test_helpers.reset_dir(root)
  let transcript_path = root <> "/transcript.log"
  let server = remote_ui_test_server.start("dcred_secret_1", transcript_path)
  let assert Ok(identity) = daemon_identity.load_or_create(root)
  let assert Ok(ref) = credential_store.normalize_credential_ref("work-laptop")
  let assert Ok(_) =
    credential_store.write_credential(
      ref,
      remote_ui_test_server.server_url(server),
      identity.daemon_id,
      credential_store.DaemonCredential(Some("cred-1"), "dcred_secret_1"),
      False,
    )
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let effective =
    effective_config(
      root,
      remote_ui_test_server.server_url(server),
      "work-laptop",
    )
  let query_requests = process.new_subject()

  let assert Ok(handle) =
    daemon_remote_client.start_with_control(
      effective,
      None,
      event_hub,
      fn(_, _) { Error(Nil) },
      fn(_) { Ok(False) },
      fn(query, timeout_ms) {
        process.send(query_requests, QueryRequestCall(query, timeout_ms))
        Ok(metrics_response(active_sessions: 2, running_workers: 1))
      },
      [],
      fn(_, _, _, _) { Ok(Nil) },
    )
  let QueryRequestCall(query, timeout_ms) =
    test_async.expect_message(query_requests)
  assert query == query_types.Metrics
  assert timeout_ms == 1000
  let transcript =
    remote_ui_test_server.wait_for_contains(transcript_path, "\"used\":1", 100)
  assert string.contains(transcript, "\"active\":1")
  assert string.contains(transcript, "\"used\":1")
  assert daemon_remote_client.stop(handle, 1000) == Ok(Nil)
  hub.stop(event_hub)
  remote_ui_test_server.stop(server)
}

fn metrics_response(
  active_sessions active_sessions: Int,
  running_workers running_workers: Int,
) -> query_types.QueryResponse {
  query_types.MetricsResponse(metrics_dto(
    active_sessions: active_sessions,
    running_workers: running_workers,
    running_scheduled_workers: 0,
    queued_claims: 0,
    pending_dispatch_validations: 0,
    pending_review_lane_preflights: 0,
  ))
}

fn metrics_dto(
  active_sessions active_sessions: Int,
  running_workers running_workers: Int,
  running_scheduled_workers running_scheduled_workers: Int,
  queued_claims queued_claims: Int,
  pending_dispatch_validations pending_dispatch_validations: Int,
  pending_review_lane_preflights pending_review_lane_preflights: Int,
) -> query_types.OperationalMetricsDto {
  query_types.OperationalMetricsDto(
    schema_version: query_types.operational_metrics_schema_version,
    daemon_id: "daemon-1",
    boot_id: "boot-1",
    sampled_at_ms: 123,
    dispatch_paused: False,
    ui_server_enabled: True,
    remote_client_status: "enabled",
    workflow_count: 1,
    scheduled_job_count: 0,
    active_sessions: active_sessions,
    running_workers: running_workers,
    running_scheduled_workers: running_scheduled_workers,
    queued_claims: queued_claims,
    pending_dispatch_validations: pending_dispatch_validations,
    pending_review_lane_preflights: pending_review_lane_preflights,
    claimed_tasks: 0,
    retry_tasks: 0,
    parked_tasks: 0,
    completed_tasks: 0,
    pending_outbox_count: 0,
    in_flight_outbox_count: 0,
    retryable_outbox_count: 0,
    permanent_outbox_count: 0,
    poll_generation: 1,
    poll_in_flight: False,
    poll_timer_active: False,
    retry_timer_count: 0,
    retry_refresh_in_flight_count: 0,
    lifecycle_projection_failed: False,
    scheduled_due_count: 0,
    scheduled_next_due_count: 0,
    scheduled_pending_count: 0,
    scheduled_retry_count: 0,
    scheduled_report_retry_count: 0,
    scheduled_retry_timer_count: 0,
    scheduled_report_retry_timer_count: 0,
    token_totals: query_types.TokenTotalsDto(
      input: 0,
      output: 0,
      cache_read: 0,
      cache_write: 0,
      total: 0,
    ),
  )
}

fn effective_config(
  root: String,
  endpoint: String,
  credential_ref: String,
) -> config_types.EffectiveConfig {
  effective_config_with_ui_server(
    root,
    config_types.UiServerEnabled(
      endpoint: endpoint,
      credential_ref: credential_ref,
      daemon_label: None,
      command_bridge_enabled: False,
      heartbeat_interval_ms: 1000,
      state_interval_ms: 1000,
      retry_initial_ms: 50,
      retry_max_ms: 100,
    ),
  )
}

fn effective_config_with_ui_server(
  root: String,
  ui_server: config_types.UiServerConfig,
) -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    tracker: config.default_tracker_config(),
    polling: config.default_polling_config(),
    workspace: config_types.WorkspaceConfig(root: root),
    control: config.default_control_config(),
    hooks: config.default_hooks_config(),
    agent: config.default_agent_config(),
    pi: config.default_pi_config(),
    handoff: config.default_handoff_config(),
    linear_contract: config.default_linear_contract_config(),
    linear_commands: config.default_linear_command_config(),
    ui_server: ui_server,
  )
}
