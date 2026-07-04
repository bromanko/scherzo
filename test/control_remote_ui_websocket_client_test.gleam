import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/control/command
import scherzo/control/query/types as query_types
import scherzo/control/remote/ui_managed_auth
import scherzo/control/remote/ui_protocol
import scherzo/control/remote/ui_websocket_client
import scherzo/log
import scherzo/managed_launch/grant as managed_launch_grant
import scherzo/session/event
import scherzo/session/tokens as session_tokens
import scherzo/work_item_invalidation
import simplifile
import support/test_helpers
import test_async

type Connection {
  Connection(outbound: process.Subject(String), inbound_path: String)
}

type ConnectRequest {
  ConnectRequest(url: String, credential: String)
}

type ApplyRequest {
  ApplyRequest(command: command.OperatorCommand, timeout_ms: Int)
}

type QueryRequestCall {
  QueryRequestCall(query: query_types.QueryRequest, timeout_ms: Int)
}

type AgentSlotOccupancyRequest {
  AgentSlotOccupancyRequest(timeout_ms: Int)
}

type QueryBehavior {
  QueryImmediately
  QueryError(query_types.QueryError)
  QueryBlock(test_async.Barrier)
}

type Fixture {
  Fixture(
    settings: ui_websocket_client.Settings,
    deps: ui_websocket_client.Dependencies(Connection, process.Timer),
    outbound: process.Subject(String),
    connects: process.Subject(ConnectRequest),
    delays: process.Subject(Int),
    logs: process.Subject(String),
    apply_requests: process.Subject(ApplyRequest),
    query_requests: process.Subject(QueryRequestCall),
    closes: process.Subject(Nil),
    managed_auth_rejections: process.Subject(String),
    inbound_path: String,
  )
}

fn new_fixture() -> Fixture {
  new_fixture_with_behavior(QueryImmediately)
}

fn new_fixture_with_behavior(query_behavior: QueryBehavior) -> Fixture {
  let root = "test/tmp/ui-websocket-client/" <> int.to_string(unique_integer())
  test_helpers.reset_dir(root)
  let outbound = process.new_subject()
  let connects = process.new_subject()
  let delays = process.new_subject()
  let logs = process.new_subject()
  let apply_requests = process.new_subject()
  let query_requests = process.new_subject()
  let closes = process.new_subject()
  let managed_auth_rejections = process.new_subject()
  let inbound_path = root <> "/inbound.txt"
  let connection = Connection(outbound, inbound_path)
  let settings =
    ui_websocket_client.Settings(
      server_url: "https://ui.example.test",
      websocket_url: "wss://ui.example.test/api/daemons/ws",
      daemon_id: "daemon_abc",
      boot_id: "boot_abc",
      runtime_metadata: ui_protocol.RuntimeMetadata(
        "test-host",
        "scherzo test-version",
        None,
        4,
        None,
      ),
      credential: "dcred_secret_1",
      managed_launch_auth: None,
      heartbeat_interval_ms: 1000,
      state_interval_ms: 1000,
      retry_initial_ms: 50,
      retry_max_ms: 100,
      connect_timeout_ms: 50,
      command_timeout_ms: 75,
      query_timeout_ms: 60_000,
      command_bridge_enabled: False,
      redaction_secrets: ["dcred_secret_1"],
    )
  let deps =
    ui_websocket_client.Dependencies(
      now_ms: fn() { 42 },
      connect: fn(url, credential, _) {
        process.send(connects, ConnectRequest(url, credential))
        Ok(connection)
      },
      send_text: fn(connection, payload, _) {
        process.send(connection.outbound, payload)
        Ok(Nil)
      },
      recv_text: fn(connection, _) {
        read_inbound_line(connection.inbound_path)
      },
      close: fn(_) {
        process.send(closes, Nil)
        Nil
      },
      send_after: fn(subject, delay, message) {
        process.send(delays, delay)
        process.send_after(subject, delay, message)
      },
      cancel_timer: fn(timer) {
        let _ = process.cancel_timer(timer)
        Nil
      },
      list_sessions: fn() { Ok([session_summary()]) },
      agent_slot_occupancy: fn(_) { Ok(1) },
      dispatch_paused: fn(_) { Ok(False) },
      apply_command: fn(operator_command, timeout_ms) {
        process.send(apply_requests, ApplyRequest(operator_command, timeout_ms))
        Ok(command_result_for(operator_command))
      },
      execute_query: fn(query, timeout_ms) {
        process.send(query_requests, QueryRequestCall(query, timeout_ms))
        case query_behavior {
          QueryImmediately -> status_query_result(settings)
          QueryError(error) -> Error(error)
          QueryBlock(barrier) -> {
            test_async.block_until_released(barrier)
            status_query_result(settings)
          }
        }
      },
      managed_auth_rejected: fn(message) {
        process.send(managed_auth_rejections, message)
      },
      logger: fn(level, event, fields, secrets) {
        process.send(logs, log.format(level, event, fields, secrets))
        Ok(Nil)
      },
    )
  Fixture(
    settings,
    deps,
    outbound,
    connects,
    delays,
    logs,
    apply_requests,
    query_requests,
    closes,
    managed_auth_rejections,
    inbound_path,
  )
}

fn command_result_for(
  operator_command: command.OperatorCommand,
) -> command.CommandResult {
  case operator_command {
    command.PauseDispatch ->
      command.applied(
        operator_command,
        Some("dispatch paused; pending_claims=0"),
      )
    command.ResumeDispatch ->
      command.applied(operator_command, Some("dispatch resumed"))
    command.ReloadWorkflow ->
      command.applied(operator_command, Some("workflow reloaded"))
    _ -> command.not_found(operator_command, Some("not found"))
  }
}

fn status_query_result(
  settings: ui_websocket_client.Settings,
) -> Result(query_types.QueryResponse, query_types.QueryError) {
  Ok(
    query_types.StatusResponse(
      query_types.StatusDto(
        daemon_id: settings.daemon_id,
        boot_id: settings.boot_id,
        dispatch_paused: False,
        ui_server_enabled: True,
        supported_queries: ["status"],
      ),
    ),
  )
}

pub fn ui_websocket_client_sends_handshake_hello_heartbeat_and_state_test() {
  let fixture = new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(fixture.settings, fixture.deps)
  let ConnectRequest(url, credential) =
    test_async.expect_message(fixture.connects)
  assert url == fixture.settings.websocket_url
  assert credential == "dcred_secret_1"
  assert string.contains(
    test_async.expect_message(fixture.outbound),
    "daemon_hello",
  )
  assert string.contains(
    test_async.expect_message(fixture.outbound),
    "heartbeat",
  )
  assert string.contains(
    test_async.expect_message(fixture.outbound),
    "daemon_state",
  )
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_sends_runtime_state_and_heartbeat_event_test() {
  let Fixture(settings:, deps:, outbound:, ..) = new_fixture()
  let assert Ok(handle) = ui_websocket_client.start(settings, deps)

  let hello = test_async.expect_message(outbound)
  assert string.contains(hello, "\"type\":\"daemon_hello\"")
  assert string.contains(hello, "\"state\":{")
  assert string.contains(hello, "\"host\":\"test-host\"")
  assert string.contains(hello, "\"version\":\"scherzo test-version\"")
  assert string.contains(hello, "\"capacity\":4")
  assert string.contains(hello, "\"active\":1")
  assert string.contains(hello, "\"used\":1")
  assert string.contains(hello, "\"known\":true")

  let heartbeat = test_async.expect_message(outbound)
  assert string.contains(heartbeat, "\"type\":\"heartbeat\"")
  assert string.contains(heartbeat, "\"state\":{")
  assert string.contains(heartbeat, "\"event\":{")
  assert string.contains(heartbeat, "\"kind\":\"lifecycle\"")
  assert string.contains(heartbeat, "\"type\":\"heartbeat\"")
  assert string.contains(heartbeat, "\"message\":\"daemon heartbeat\"")

  let state = test_async.expect_message(outbound)
  assert string.contains(state, "\"type\":\"daemon_state\"")
  assert string.contains(state, "\"state\":{")
  assert string.contains(state, "\"agentSlots\":{")
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_reports_agent_slots_from_occupancy_not_sessions_test() {
  let Fixture(settings:, deps:, outbound:, ..) = new_fixture()
  let deps =
    ui_websocket_client.Dependencies(
      ..deps,
      list_sessions: fn() {
        Ok([
          session_summary_with_status("workflow-parent-session", event.Running),
          session_summary_with_status(
            "workflow-step-run-abc-implement-a0-abc123def456",
            event.Running,
          ),
        ])
      },
      agent_slot_occupancy: fn(_) { Ok(1) },
    )
  let assert Ok(handle) = ui_websocket_client.start(settings, deps)

  let hello = test_async.expect_message(outbound)
  assert string.contains(hello, "\"type\":\"daemon_hello\"")
  assert string.contains(hello, "\"active\":1")
  assert string.contains(hello, "\"used\":1")
  assert string.contains(hello, "\"known\":true")

  let heartbeat = test_async.expect_message(outbound)
  assert string.contains(heartbeat, "\"type\":\"heartbeat\"")
  assert string.contains(heartbeat, "\"active\":1")
  assert string.contains(heartbeat, "\"used\":1")
  assert string.contains(heartbeat, "\"known\":true")

  let state = test_async.expect_message(outbound)
  assert string.contains(state, "\"type\":\"daemon_state\"")
  assert string.contains(state, "\"active\":1")
  assert string.contains(state, "\"used\":1")
  assert string.contains(state, "\"known\":true")
  assert string.contains(state, "workflow-parent-session")
  assert string.contains(
    state,
    "workflow-step-run-abc-implement-a0-abc123def456",
  )
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_reuses_connect_snapshot_and_cached_heartbeat_test() {
  let Fixture(settings:, deps:, outbound:, ..) = new_fixture()
  let list_calls = process.new_subject()
  let slot_calls = process.new_subject()
  let deps =
    ui_websocket_client.Dependencies(
      ..deps,
      list_sessions: fn() {
        process.send(list_calls, Nil)
        Ok([session_summary()])
      },
      agent_slot_occupancy: fn(timeout_ms) {
        process.send(slot_calls, AgentSlotOccupancyRequest(timeout_ms))
        Ok(1)
      },
    )
  let settings =
    ui_websocket_client.Settings(
      ..settings,
      heartbeat_interval_ms: 100,
      state_interval_ms: 1000,
      query_timeout_ms: 60_000,
    )
  let assert Ok(handle) = ui_websocket_client.start(settings, deps)

  let AgentSlotOccupancyRequest(timeout_ms) =
    test_async.expect_message(slot_calls)
  assert timeout_ms == 1000
  expect_initial_outbound(outbound)
  assert list.length(test_async.drain_subject(list_calls)) == 1
  test_async.assert_no_extra_message_within(slot_calls, 50)

  let heartbeat = test_async.expect_message(outbound)
  assert string.contains(heartbeat, "\"type\":\"heartbeat\"")
  test_async.assert_no_extra_message_within(list_calls, 50)
  test_async.assert_no_extra_message_within(slot_calls, 50)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_marks_agent_slots_unknown_when_occupancy_fails_test() {
  let Fixture(settings:, deps:, outbound:, ..) = new_fixture()
  let deps =
    ui_websocket_client.Dependencies(..deps, agent_slot_occupancy: fn(_) {
      Error("slot_occupancy_unavailable")
    })
  let assert Ok(handle) = ui_websocket_client.start(settings, deps)

  let hello = test_async.expect_message(outbound)
  assert string.contains(hello, "\"type\":\"daemon_hello\"")
  assert string.contains(hello, "\"active\":0")
  assert string.contains(hello, "\"used\":0")
  assert string.contains(hello, "\"known\":false")

  let heartbeat = test_async.expect_message(outbound)
  assert string.contains(heartbeat, "\"type\":\"heartbeat\"")
  assert string.contains(heartbeat, "\"active\":0")
  assert string.contains(heartbeat, "\"used\":0")
  assert string.contains(heartbeat, "\"known\":false")

  let state = test_async.expect_message(outbound)
  assert string.contains(state, "\"type\":\"daemon_state\"")
  assert string.contains(state, "\"active\":0")
  assert string.contains(state, "\"used\":0")
  assert string.contains(state, "\"known\":false")
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_sends_daemon_label_metadata_test() {
  let Fixture(settings:, deps:, outbound:, ..) = new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(
        ..settings,
        runtime_metadata: ui_protocol.RuntimeMetadata(
          "test-host",
          "scherzo test-version",
          Some("Project Foo / MacBook"),
          4,
          None,
        ),
      ),
      deps,
    )
  assert string.contains(
    test_async.expect_message(outbound),
    "\"daemonLabel\":\"Project Foo / MacBook\"",
  )
  assert string.contains(
    test_async.expect_message(outbound),
    "\"daemonLabel\":\"Project Foo / MacBook\"",
  )
  assert string.contains(
    test_async.expect_message(outbound),
    "\"daemonLabel\":\"Project Foo / MacBook\"",
  )
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_sends_managed_launch_hello_metadata_test() {
  let Fixture(settings:, deps:, outbound:, ..) = new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(
        ..settings,
        runtime_metadata: ui_protocol.RuntimeMetadata(
          "test-host",
          "scherzo test-version",
          Some("Managed"),
          4,
          Some(
            ui_protocol.ManagedLaunchContext(
              launch_id: "launch-123",
              capabilities: [
                managed_launch_grant.State,
                managed_launch_grant.Query,
              ],
            ),
          ),
        ),
      ),
      deps,
    )
  let hello = test_async.expect_message(outbound)
  assert string.contains(hello, "\"launchId\":\"launch-123\"")
  assert string.contains(hello, "\"capabilities\":[\"state\",\"query\"]")
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_reconnects_after_reader_failure_test() {
  let fixture = new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(fixture.settings, fixture.deps)
  let _ = test_async.expect_message(fixture.connects)
  expect_initial_outbound(fixture.outbound)
  append_inbound_line(fixture.inbound_path, "FAIL:down")
  expect_delay(fixture.delays, 50, 10)
  let _ = test_async.expect_message(fixture.connects)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_reconnects_with_managed_runtime_credential_test() {
  let fixture = new_fixture()
  let settings =
    managed_launch_settings(fixture.settings, "launch_secret_1", 5000)
  let assert Ok(handle) = ui_websocket_client.start(settings, fixture.deps)
  let ConnectRequest(_, first_credential) =
    test_async.expect_message(fixture.connects)
  assert first_credential == "launch_secret_1"
  expect_initial_outbound(fixture.outbound)

  append_inbound_line(
    fixture.inbound_path,
    "{\"type\":\"server_hello\",\"runtimeCredential\":\"runtime_secret_1\"}",
  )
  append_inbound_line(fixture.inbound_path, "FAIL:down")

  let _ = expect_log_contains(fixture.logs, "ui_websocket_recv_failed")
  let ConnectRequest(_, reconnect_credential) =
    test_async.expect_message(fixture.connects)
  assert reconnect_credential == "runtime_secret_1"
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_reexchanges_launch_grant_when_ack_is_lost_test() {
  let fixture = new_fixture()
  let settings =
    managed_launch_settings(fixture.settings, "launch_secret_1", 5000)
  let assert Ok(handle) = ui_websocket_client.start(settings, fixture.deps)
  let ConnectRequest(_, first_credential) =
    test_async.expect_message(fixture.connects)
  assert first_credential == "launch_secret_1"
  expect_initial_outbound(fixture.outbound)

  append_inbound_line(fixture.inbound_path, "FAIL:lost_ack")

  let _ = expect_log_contains(fixture.logs, "ui_websocket_recv_failed")
  let ConnectRequest(_, reconnect_credential) =
    test_async.expect_message(fixture.connects)
  assert reconnect_credential == "launch_secret_1"
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_rejects_managed_mode_when_grant_expired_without_runtime_credential_test() {
  let fixture = new_fixture()
  let settings = managed_launch_settings(fixture.settings, "launch_secret_1", 1)
  let assert Ok(handle) = ui_websocket_client.start(settings, fixture.deps)

  let message = test_async.expect_message(fixture.managed_auth_rejections)
  assert string.contains(message, "managed launch credential")
  test_async.assert_no_extra_message_within(fixture.connects, 50)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_reports_managed_permanent_auth_rejection_test() {
  let fixture = new_fixture()
  let settings =
    managed_launch_settings(fixture.settings, "launch_secret_1", 5000)
  let assert Ok(handle) = ui_websocket_client.start(settings, fixture.deps)
  let _ = test_async.expect_message(fixture.connects)
  expect_initial_outbound(fixture.outbound)

  append_inbound_line(
    fixture.inbound_path,
    "{\"type\":\"server_hello\",\"runtimeCredential\":\"runtime_secret_1\"}",
  )
  append_inbound_line(
    fixture.inbound_path,
    "FAIL:websocket_close:1008:credential-invalid runtime_secret_1",
  )

  let log_entry = expect_log_contains(fixture.logs, "ui_websocket_recv_failed")
  assert string.contains(log_entry, "[REDACTED]")
  let message = test_async.expect_message(fixture.managed_auth_rejections)
  assert !string.contains(message, "runtime_secret_1")
  assert !string.contains(message, "launch_secret_1")
  test_async.assert_no_extra_message_within(fixture.connects, 100)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_retries_managed_transient_connect_failure_test() {
  let Fixture(
    settings:,
    deps:,
    connects:,
    delays:,
    managed_auth_rejections:,
    ..,
  ) = new_fixture()
  let settings = managed_launch_settings(settings, "launch_secret_1", 5000)
  let deps =
    ui_websocket_client.Dependencies(..deps, connect: fn(url, credential, _) {
      process.send(connects, ConnectRequest(url, credential))
      Error("econnrefused")
    })
  let assert Ok(handle) = ui_websocket_client.start(settings, deps)

  let ConnectRequest(_, credential) = test_async.expect_message(connects)
  assert credential == "launch_secret_1"
  expect_delay(delays, 50, 10)
  test_async.assert_no_extra_message_within(managed_auth_rejections, 50)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_retries_managed_5xx_with_auth_words_test() {
  let Fixture(
    settings:,
    deps:,
    connects:,
    delays:,
    managed_auth_rejections:,
    ..,
  ) = new_fixture()
  let settings = managed_launch_settings(settings, "launch_secret_1", 5000)
  let deps =
    ui_websocket_client.Dependencies(..deps, connect: fn(url, credential, _) {
      process.send(connects, ConnectRequest(url, credential))
      Error("websocket_http_status:503:credential-invalid during api restart")
    })
  let assert Ok(handle) = ui_websocket_client.start(settings, deps)

  let ConnectRequest(_, credential) = test_async.expect_message(connects)
  assert credential == "launch_secret_1"
  expect_delay(delays, 50, 10)
  test_async.assert_no_extra_message_within(managed_auth_rejections, 50)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_stops_retrying_after_revocation_test() {
  let fixture = new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(fixture.settings, fixture.deps)
  let _ = test_async.expect_message(fixture.connects)
  expect_initial_outbound(fixture.outbound)
  append_inbound_line(
    fixture.inbound_path,
    "{\"type\":\"credential_revoked\",\"reason\":\"server revoked dcred_secret_1\"}",
  )
  let entry =
    expect_log_contains(fixture.logs, "ui_websocket_credential_revoked")
  assert string.contains(entry, "[REDACTED]")
  test_async.assert_no_extra_message_within(fixture.connects, 100)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_stops_after_daemon_identity_revocation_test() {
  let fixture = new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(fixture.settings, fixture.deps)
  let _ = test_async.expect_message(fixture.connects)
  expect_initial_outbound(fixture.outbound)
  append_inbound_line(
    fixture.inbound_path,
    "{\"type\":\"daemon_identity_revoked\",\"reason\":\"operator revoked daemon\"}",
  )
  let entry =
    expect_log_contains(fixture.logs, "ui_websocket_daemon_identity_revoked")
  assert string.contains(entry, "operator revoked daemon")
  test_async.assert_no_extra_message_within(fixture.connects, 100)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_logs_when_command_bridge_is_enabled_test() {
  let Fixture(settings:, deps:, logs:, ..) = new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(..settings, command_bridge_enabled: True),
      deps,
    )

  let entry = expect_log_contains(logs, "ui_websocket_command_bridge_enabled")
  assert string.contains(entry, "remote command/result bridge enabled")
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_sends_work_item_invalidation_event_test() {
  let Fixture(settings:, deps:, outbound:, ..) = new_fixture()
  let assert Ok(handle) = ui_websocket_client.start(settings, deps)
  expect_initial_outbound(outbound)

  ui_websocket_client.notify_work_item_invalidation(
    handle,
    work_item_invalidation.new(
      work_item_invalidation.PollRefresh,
      [
        work_item_invalidation.AffectedTaskRef(
          "linear",
          "issue-1",
          Some("LIV-1"),
        ),
      ],
      has_unknown_refs: False,
    ),
  )

  let invalidation =
    expect_next_outbound_contains(
      outbound,
      "\"type\":\"work_item_invalidation\"",
    )
  assert string.contains(invalidation, "\"daemonId\":\"daemon_abc\"")
  assert string.contains(invalidation, "\"bootId\":\"boot_abc\"")
  assert string.contains(invalidation, "\"sentAtMs\":42")
  assert string.contains(invalidation, "\"source\":\"poll_refresh\"")
  assert string.contains(invalidation, "\"displayId\":\"LIV-1\"")
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_handles_query_request_test() {
  let Fixture(settings:, deps:, outbound:, query_requests:, inbound_path:, ..) =
    new_fixture()
  let assert Ok(handle) = ui_websocket_client.start(settings, deps)
  expect_initial_outbound(outbound)

  append_inbound_line(inbound_path, status_query_request("query-1"))
  let QueryRequestCall(query, timeout_ms) =
    test_async.expect_message(query_requests)
  assert query == query_types.Status
  assert timeout_ms == settings.query_timeout_ms
  let response =
    expect_next_outbound_contains(outbound, "\"queryId\":\"query-1\"")
  assert string.contains(response, "\"type\":\"query_response\"")
  assert string.contains(response, "\"ok\":true")
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_rejects_malformed_query_request_without_execution_test() {
  let fixture = new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(fixture.settings, fixture.deps)
  expect_initial_outbound(fixture.outbound)

  append_inbound_line(
    fixture.inbound_path,
    "{\"type\":\"query_request\",\"queryId\":\"query-bad\",\"daemonId\":\"daemon_abc\",\"bootId\":\"boot_abc\",\"query\":{\"version\":1,\"type\":\"mystery\",\"raw\":\"dcred_secret_1 provider payload\"}}",
  )
  let response =
    expect_next_outbound_contains(fixture.outbound, "\"queryId\":\"query-bad\"")
  assert string.contains(response, "\"ok\":false")
  assert string.contains(response, "\"code\":\"unsupported_query\"")
  assert !string.contains(response, "dcred_secret_1")
  assert !string.contains(response, "provider payload")
  test_async.assert_no_extra_message_within(fixture.query_requests, 50)
  let log_entry = expect_log_contains(fixture.logs, "ui_websocket_bad_inbound")
  assert !string.contains(log_entry, "dcred_secret_1")
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_rejects_query_when_managed_launch_lacks_query_capability_test() {
  let Fixture(settings:, deps:, outbound:, query_requests:, inbound_path:, ..) =
    new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(
        ..settings,
        runtime_metadata: ui_protocol.RuntimeMetadata(
          "test-host",
          "scherzo test-version",
          None,
          4,
          Some(
            ui_protocol.ManagedLaunchContext(
              launch_id: "launch-123",
              capabilities: [managed_launch_grant.State],
            ),
          ),
        ),
      ),
      deps,
    )
  expect_initial_outbound(outbound)
  append_inbound_line(inbound_path, status_query_request("query-no-cap"))
  let response =
    expect_next_outbound_contains(outbound, "\"queryId\":\"query-no-cap\"")
  assert string.contains(response, "\"code\":\"unsupported_query\"")
  test_async.assert_no_extra_message_within(query_requests, 50)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_rejects_query_for_identity_mismatch_test() {
  let fixture = new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(fixture.settings, fixture.deps)
  expect_initial_outbound(fixture.outbound)

  append_inbound_line(
    fixture.inbound_path,
    status_query_request_for("query-wrong-daemon", "other_daemon", "boot_abc"),
  )
  let daemon_response =
    expect_next_outbound_contains(
      fixture.outbound,
      "\"queryId\":\"query-wrong-daemon\"",
    )
  assert string.contains(daemon_response, "\"code\":\"query_backend_failed\"")

  append_inbound_line(
    fixture.inbound_path,
    status_query_request_for("query-wrong-boot", "daemon_abc", "other_boot"),
  )
  let boot_response =
    expect_next_outbound_contains(
      fixture.outbound,
      "\"queryId\":\"query-wrong-boot\"",
    )
  assert string.contains(boot_response, "\"code\":\"query_backend_failed\"")
  test_async.assert_no_extra_message_within(fixture.query_requests, 50)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_propagates_query_backend_error_test() {
  let query_error =
    query_types.QueryError(query_types.QueryBackendFailed, "backend failed")
  let fixture = new_fixture_with_behavior(QueryError(query_error))
  let assert Ok(handle) =
    ui_websocket_client.start(fixture.settings, fixture.deps)
  expect_initial_outbound(fixture.outbound)

  append_inbound_line(fixture.inbound_path, status_query_request("query-error"))
  let QueryRequestCall(query, _) =
    test_async.expect_message(fixture.query_requests)
  assert query == query_types.Status
  let response =
    expect_next_outbound_contains(
      fixture.outbound,
      "\"queryId\":\"query-error\"",
    )
  assert string.contains(response, "\"ok\":false")
  assert string.contains(response, "\"code\":\"query_backend_failed\"")
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_times_out_blocked_query_test() {
  let barrier = test_async.new_barrier()
  let Fixture(settings:, deps:, outbound:, query_requests:, inbound_path:, ..) =
    new_fixture_with_behavior(QueryBlock(barrier))
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(..settings, query_timeout_ms: 20),
      deps,
    )
  expect_initial_outbound(outbound)

  append_inbound_line(inbound_path, status_query_request("query-timeout"))
  let QueryRequestCall(query, timeout_ms) =
    test_async.expect_message(query_requests)
  assert query == query_types.Status
  assert timeout_ms == 20
  let response =
    expect_next_outbound_contains(outbound, "\"queryId\":\"query-timeout\"")
  assert string.contains(response, "\"code\":\"query_timeout\"")
  test_async.release_barrier_if_waiting(barrier)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_rejects_ninth_in_flight_query_test() {
  let barrier = test_async.new_barrier()
  let Fixture(settings:, deps:, outbound:, query_requests:, inbound_path:, ..) =
    new_fixture_with_behavior(QueryBlock(barrier))
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(
        ..settings,
        heartbeat_interval_ms: 60_000,
        state_interval_ms: 60_000,
        query_timeout_ms: 60_000,
      ),
      deps,
    )
  expect_initial_outbound(outbound)

  append_inbound_block(inbound_path, query_request_block("query-", 1, 9))
  expect_status_query_requests(query_requests, 8)
  let response =
    expect_next_outbound_contains(outbound, "\"queryId\":\"query-9\"")
  assert string.contains(response, "\"code\":\"query_overloaded\"")
  release_barriers(barrier, 8)
  let _ = test_async.drain_subject(outbound)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_query_does_not_block_heartbeat_test() {
  let barrier = test_async.new_barrier()
  let Fixture(settings:, deps:, outbound:, query_requests:, inbound_path:, ..) =
    new_fixture_with_behavior(QueryBlock(barrier))
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(
        ..settings,
        heartbeat_interval_ms: 20,
        state_interval_ms: 30,
        query_timeout_ms: 100,
      ),
      deps,
    )
  expect_initial_outbound(outbound)

  append_inbound_line(inbound_path, status_query_request("query-blocked"))
  let _ = test_async.expect_message(query_requests)
  assert eventually_has_outbound(outbound, "\"type\":\"heartbeat\"")
  assert eventually_has_outbound(outbound, "\"type\":\"daemon_state\"")
  test_async.release_barrier(barrier)
  let _ = eventually_has_outbound(outbound, "\"queryId\":\"query-blocked\"")
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_query_and_command_are_compatible_test() {
  let barrier = test_async.new_barrier()
  let Fixture(
    settings:,
    deps:,
    outbound:,
    apply_requests:,
    query_requests:,
    inbound_path:,
    ..,
  ) = new_fixture_with_behavior(QueryBlock(barrier))
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(
        ..settings,
        command_bridge_enabled: True,
        query_timeout_ms: 60_000,
      ),
      deps,
    )
  expect_initial_outbound(outbound)

  append_inbound_line(inbound_path, status_query_request("query-compat"))
  let _ = test_async.expect_message(query_requests)

  append_inbound_line(inbound_path, server_command_frame("scmd_pause", "pause"))
  let ApplyRequest(command_, _) = test_async.expect_message(apply_requests)
  assert command_ == command.PauseDispatch
  let command_result =
    expect_next_outbound_contains(
      outbound,
      "\"serverCommandId\":\"scmd_pause\"",
    )
  assert string.contains(command_result, "\"type\":\"command_result\"")
  let state =
    expect_next_outbound_contains(outbound, "\"type\":\"daemon_state\"")
  assert string.contains(state, "\"dispatchPaused\"")

  test_async.release_barrier(barrier)
  let query_result =
    expect_next_outbound_contains(outbound, "\"queryId\":\"query-compat\"")
  assert string.contains(query_result, "\"type\":\"query_response\"")
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_suppresses_stale_query_completion_after_reconnect_test() {
  let barrier = test_async.new_barrier()
  let fixture = new_fixture_with_behavior(QueryBlock(barrier))
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(..fixture.settings, query_timeout_ms: 200),
      fixture.deps,
    )
  let _ = test_async.expect_message(fixture.connects)
  expect_initial_outbound(fixture.outbound)

  append_inbound_line(fixture.inbound_path, status_query_request("query-stale"))
  let _ = test_async.expect_message(fixture.query_requests)
  append_inbound_line(fixture.inbound_path, "FAIL:down")
  let _ = expect_log_contains(fixture.logs, "ui_websocket_recv_failed")
  let _ = test_async.expect_message(fixture.connects)
  expect_initial_outbound(fixture.outbound)
  test_async.release_barrier_if_waiting(barrier)
  test_async.assert_no_extra_message_within(fixture.outbound, 100)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_shutdown_cleans_up_queries_test() {
  let barrier = test_async.new_barrier()
  let fixture = new_fixture_with_behavior(QueryBlock(barrier))
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(..fixture.settings, query_timeout_ms: 200),
      fixture.deps,
    )
  expect_initial_outbound(fixture.outbound)

  append_inbound_line(fixture.inbound_path, status_query_request("query-stop"))
  let _ = test_async.expect_message(fixture.query_requests)
  let assert Ok(Nil) = ui_websocket_client.stop(handle, 1000)
  let response =
    expect_next_outbound_contains(
      fixture.outbound,
      "\"queryId\":\"query-stop\"",
    )
  assert string.contains(response, "\"code\":\"query_shutdown\"")
  let _ = test_async.expect_message(fixture.closes)
  test_async.release_barrier_if_waiting(barrier)
}

pub fn ui_websocket_client_applies_pause_resume_reload_server_commands_test() {
  let Fixture(settings:, deps:, outbound:, apply_requests:, inbound_path:, ..) =
    new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(..settings, command_bridge_enabled: True),
      deps,
    )
  expect_initial_outbound(outbound)

  append_inbound_line(inbound_path, server_command_frame("scmd_pause", "pause"))
  let ApplyRequest(pause_command, pause_timeout) =
    test_async.expect_message(apply_requests)
  assert pause_command == command.PauseDispatch
  assert pause_timeout == settings.command_timeout_ms
  let pause_result =
    expect_next_outbound_contains(
      outbound,
      "\"serverCommandId\":\"scmd_pause\"",
    )
  assert string.contains(pause_result, "\"type\":\"command_result\"")
  assert string.contains(pause_result, "\"command\":\"pause\"")
  assert string.contains(pause_result, "\"status\":\"applied\"")
  let pause_state =
    expect_next_outbound_contains(outbound, "\"type\":\"daemon_state\"")
  assert string.contains(pause_state, "\"dispatchPaused\"")

  append_inbound_line(
    inbound_path,
    server_command_frame("scmd_resume", "resume"),
  )
  let ApplyRequest(resume_command, _) =
    test_async.expect_message(apply_requests)
  assert resume_command == command.ResumeDispatch
  let resume_result =
    expect_next_outbound_contains(
      outbound,
      "\"serverCommandId\":\"scmd_resume\"",
    )
  assert string.contains(resume_result, "\"command\":\"resume\"")
  assert string.contains(resume_result, "\"status\":\"applied\"")
  let resume_state =
    expect_next_outbound_contains(outbound, "\"type\":\"daemon_state\"")
  assert string.contains(resume_state, "\"dispatchPaused\"")

  append_inbound_line(
    inbound_path,
    server_command_frame("scmd_reload", "reload"),
  )
  let ApplyRequest(reload_command, _) =
    test_async.expect_message(apply_requests)
  assert reload_command == command.ReloadWorkflow
  let reload_result =
    expect_next_outbound_contains(
      outbound,
      "\"serverCommandId\":\"scmd_reload\"",
    )
  assert string.contains(reload_result, "\"command\":\"reload\"")
  assert string.contains(reload_result, "\"status\":\"applied\"")
  let reload_state =
    expect_next_outbound_contains(outbound, "\"type\":\"daemon_state\"")
  assert string.contains(reload_state, "\"dispatchPaused\"")
  test_async.assert_no_extra_message_within(outbound, 50)

  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_sends_command_result_operation_id_test() {
  let Fixture(settings:, deps:, outbound:, apply_requests:, inbound_path:, ..) =
    new_fixture()
  let deps =
    ui_websocket_client.Dependencies(
      ..deps,
      apply_command: fn(operator_command, timeout_ms) {
        process.send(apply_requests, ApplyRequest(operator_command, timeout_ms))
        Ok(command.CommandResult(
          command: command.command_name(operator_command),
          status: command.Queued,
          target: command.command_target(operator_command),
          message: Some("queued durable repair"),
          operation_id: Some("op-queued-1"),
        ))
      },
    )
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(..settings, command_bridge_enabled: True),
      deps,
    )
  expect_initial_outbound(outbound)

  append_inbound_line(inbound_path, server_command_frame("scmd_pause", "pause"))
  let ApplyRequest(command_, _) = test_async.expect_message(apply_requests)
  assert command_ == command.PauseDispatch
  let result =
    expect_next_outbound_contains(
      outbound,
      "\"serverCommandId\":\"scmd_pause\"",
    )
  assert string.contains(result, "\"status\":\"queued\"")
  assert string.contains(result, "\"operation_id\":\"op-queued-1\"")

  let _ = expect_next_outbound_contains(outbound, "\"type\":\"daemon_state\"")
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_rejects_malformed_server_command_test() {
  let Fixture(settings:, deps:, outbound:, apply_requests:, inbound_path:, ..) =
    new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(..settings, command_bridge_enabled: True),
      deps,
    )
  expect_initial_outbound(outbound)

  append_inbound_line(
    inbound_path,
    server_command_frame_with_command("scmd_bad", "{\"type\":\"mystery\"}"),
  )

  let result =
    expect_next_outbound_contains(outbound, "\"serverCommandId\":\"scmd_bad\"")
  assert string.contains(result, "\"type\":\"command_result\"")
  assert string.contains(result, "\"command\":\"mystery\"")
  assert string.contains(result, "\"status\":\"rejected\"")
  assert string.contains(result, "\"reason\":\"unknown_command\"")
  test_async.assert_no_extra_message_within(apply_requests, 50)
  test_async.assert_no_extra_message_within(outbound, 50)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_rejects_server_command_when_managed_launch_lacks_command_capability_test() {
  let Fixture(settings:, deps:, outbound:, apply_requests:, inbound_path:, ..) =
    new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(
        ..settings,
        command_bridge_enabled: True,
        runtime_metadata: ui_protocol.RuntimeMetadata(
          "test-host",
          "scherzo test-version",
          None,
          4,
          Some(
            ui_protocol.ManagedLaunchContext(
              launch_id: "launch-123",
              capabilities: [
                managed_launch_grant.State,
                managed_launch_grant.Query,
              ],
            ),
          ),
        ),
      ),
      deps,
    )
  expect_initial_outbound(outbound)

  append_inbound_line(
    inbound_path,
    server_command_frame("scmd_no_command_cap", "pause"),
  )

  let result =
    expect_next_outbound_contains(
      outbound,
      "\"serverCommandId\":\"scmd_no_command_cap\"",
    )
  assert string.contains(
    result,
    "\"reason\":\"managed_launch_command_capability_denied\"",
  )
  test_async.assert_no_extra_message_within(apply_requests, 50)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_rejects_server_command_when_bridge_disabled_test() {
  let Fixture(settings:, deps:, outbound:, apply_requests:, inbound_path:, ..) =
    new_fixture()
  let assert Ok(handle) = ui_websocket_client.start(settings, deps)
  expect_initial_outbound(outbound)

  append_inbound_line(
    inbound_path,
    server_command_frame("scmd_disabled", "pause"),
  )

  let result =
    expect_next_outbound_contains(
      outbound,
      "\"serverCommandId\":\"scmd_disabled\"",
    )
  assert string.contains(result, "\"type\":\"command_result\"")
  assert string.contains(result, "\"command\":\"pause\"")
  assert string.contains(result, "\"status\":\"not_allowed\"")
  assert string.contains(result, "\"reason\":\"command_bridge_disabled\"")
  test_async.assert_no_extra_message_within(apply_requests, 50)
  test_async.assert_no_extra_message_within(outbound, 50)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_rejects_server_command_for_identity_mismatch_test() {
  let Fixture(settings:, deps:, outbound:, apply_requests:, inbound_path:, ..) =
    new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(..settings, command_bridge_enabled: True),
      deps,
    )
  expect_initial_outbound(outbound)

  append_inbound_line(
    inbound_path,
    server_command_frame_for(
      "scmd_wrong_daemon",
      "other_daemon",
      "boot_abc",
      "pause",
    ),
  )
  let daemon_result =
    expect_next_outbound_contains(
      outbound,
      "\"serverCommandId\":\"scmd_wrong_daemon\"",
    )
  assert string.contains(daemon_result, "\"status\":\"not_allowed\"")
  assert string.contains(daemon_result, "\"reason\":\"daemon_id_mismatch\"")

  append_inbound_line(
    inbound_path,
    server_command_frame_for(
      "scmd_wrong_boot",
      "daemon_abc",
      "other_boot",
      "resume",
    ),
  )
  let boot_result =
    expect_next_outbound_contains(
      outbound,
      "\"serverCommandId\":\"scmd_wrong_boot\"",
    )
  assert string.contains(boot_result, "\"status\":\"not_allowed\"")
  assert string.contains(boot_result, "\"reason\":\"boot_id_mismatch\"")
  test_async.assert_no_extra_message_within(apply_requests, 50)
  test_async.assert_no_extra_message_within(outbound, 50)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_sends_timeout_rejection_when_apply_times_out_test() {
  let Fixture(settings:, deps:, outbound:, apply_requests:, inbound_path:, ..) =
    new_fixture()
  let deps =
    ui_websocket_client.Dependencies(
      ..deps,
      apply_command: fn(operator_command, timeout_ms) {
        process.send(apply_requests, ApplyRequest(operator_command, timeout_ms))
        Error(Nil)
      },
    )
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(..settings, command_bridge_enabled: True),
      deps,
    )
  expect_initial_outbound(outbound)

  append_inbound_line(
    inbound_path,
    server_command_frame("scmd_timeout", "pause"),
  )
  let ApplyRequest(command_, timeout_ms) =
    test_async.expect_message(apply_requests)
  assert command_ == command.PauseDispatch
  assert timeout_ms == settings.command_timeout_ms
  let result =
    expect_next_outbound_contains(
      outbound,
      "\"serverCommandId\":\"scmd_timeout\"",
    )
  assert string.contains(result, "\"status\":\"rejected\"")
  assert string.contains(result, "\"reason\":\"remote_command_timeout\"")
  let _ = expect_next_outbound_contains(outbound, "\"type\":\"daemon_state\"")
  test_async.assert_no_extra_message_within(outbound, 50)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_rejects_server_command_when_in_flight_limit_reached_test() {
  let Fixture(settings:, deps:, outbound:, apply_requests:, inbound_path:, ..) =
    new_fixture()
  let barrier = test_async.new_barrier()
  let deps =
    ui_websocket_client.Dependencies(
      ..deps,
      apply_command: fn(operator_command, timeout_ms) {
        process.send(apply_requests, ApplyRequest(operator_command, timeout_ms))
        test_async.block_until_released(barrier)
        Ok(command_result_for(operator_command))
      },
    )
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(
        ..settings,
        command_bridge_enabled: True,
        heartbeat_interval_ms: 60_000,
        state_interval_ms: 60_000,
      ),
      deps,
    )
  expect_initial_outbound(outbound)

  append_pause_commands(inbound_path, "scmd_inflight_", 1, 9)
  expect_pause_apply_requests(apply_requests, 8)
  let result =
    expect_next_outbound_contains(
      outbound,
      "\"serverCommandId\":\"scmd_inflight_9\"",
    )
  assert string.contains(result, "\"status\":\"rejected\"")
  assert string.contains(result, "\"reason\":\"remote_command_overloaded\"")
  test_async.assert_no_extra_message_within(apply_requests, 50)
  test_async.assert_no_extra_message_within(outbound, 50)
  release_barriers(barrier, 8)
  let _ = test_async.drain_subject(outbound)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_replays_completed_server_command_without_reapplying_test() {
  let Fixture(settings:, deps:, outbound:, apply_requests:, inbound_path:, ..) =
    new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(..settings, command_bridge_enabled: True),
      deps,
    )
  expect_initial_outbound(outbound)

  append_inbound_line(inbound_path, server_command_frame("scmd_once", "pause"))
  let ApplyRequest(command_, _) = test_async.expect_message(apply_requests)
  assert command_ == command.PauseDispatch
  let _ =
    expect_next_outbound_contains(outbound, "\"serverCommandId\":\"scmd_once\"")
  let _ = expect_next_outbound_contains(outbound, "\"type\":\"daemon_state\"")

  append_inbound_line(inbound_path, server_command_frame("scmd_once", "pause"))
  let replay =
    expect_next_outbound_contains(outbound, "\"serverCommandId\":\"scmd_once\"")
  assert string.contains(replay, "\"command\":\"pause\"")
  assert string.contains(replay, "\"status\":\"applied\"")
  test_async.assert_no_extra_message_within(apply_requests, 50)
  test_async.assert_no_extra_message_within(outbound, 50)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_reconnects_after_command_result_send_failure_test() {
  let Fixture(
    settings:,
    deps:,
    outbound:,
    connects:,
    logs:,
    closes:,
    apply_requests:,
    inbound_path:,
    ..,
  ) = new_fixture()
  let deps =
    ui_websocket_client.Dependencies(..deps, send_text: fn(_, payload, _) {
      case string.contains(payload, "\"type\":\"command_result\"") {
        True -> Error("send failed")
        False -> {
          process.send(outbound, payload)
          Ok(Nil)
        }
      }
    })
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(..settings, command_bridge_enabled: True),
      deps,
    )
  let _ = test_async.expect_message(connects)
  expect_initial_outbound(outbound)

  append_inbound_line(
    inbound_path,
    server_command_frame("scmd_send_fail", "pause"),
  )
  let ApplyRequest(command_, _) = test_async.expect_message(apply_requests)
  assert command_ == command.PauseDispatch
  let _ = test_async.expect_message(closes)
  let log_entry =
    expect_log_contains(logs, "ui_websocket_command_result_send_failed")
  assert string.contains(log_entry, "send failed")
  let _ = test_async.expect_message(connects)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_reconnects_after_work_item_invalidation_send_failure_test() {
  let Fixture(settings:, deps:, outbound:, connects:, logs:, closes:, ..) =
    new_fixture()
  let deps =
    ui_websocket_client.Dependencies(..deps, send_text: fn(_, payload, _) {
      case string.contains(payload, "\"type\":\"work_item_invalidation\"") {
        True -> Error("invalidation send failed")
        False -> {
          process.send(outbound, payload)
          Ok(Nil)
        }
      }
    })
  let assert Ok(handle) = ui_websocket_client.start(settings, deps)
  let _ = test_async.expect_message(connects)
  expect_initial_outbound(outbound)

  ui_websocket_client.notify_work_item_invalidation(
    handle,
    work_item_invalidation.new(
      work_item_invalidation.PollRefresh,
      [
        work_item_invalidation.AffectedTaskRef(
          "linear",
          "issue-1",
          Some("LIV-1"),
        ),
      ],
      has_unknown_refs: False,
    ),
  )

  let _ = test_async.expect_message(closes)
  let log_entry =
    expect_log_contains(logs, "ui_websocket_work_item_invalidation_send_failed")
  assert string.contains(log_entry, "invalidation send failed")
  let _ = test_async.expect_message(connects)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_ignores_too_fast_server_heartbeat_test() {
  let fixture = new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(fixture.settings, fixture.deps)
  let _ = test_async.expect_message(fixture.connects)
  expect_initial_outbound(fixture.outbound)
  let _ = test_async.drain_subject(fixture.delays)

  append_inbound_line(
    fixture.inbound_path,
    "{\"type\":\"server_hello\",\"heartbeatIntervalMs\":1}",
  )
  let delay = test_async.expect_message(fixture.delays)
  assert delay == fixture.settings.heartbeat_interval_ms
  assert delay != 1
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

fn managed_launch_settings(
  settings: ui_websocket_client.Settings,
  launch_credential: String,
  expires_at_ms: Int,
) -> ui_websocket_client.Settings {
  ui_websocket_client.Settings(
    ..settings,
    credential: "",
    managed_launch_auth: Some(ui_managed_auth.ManagedLaunchAuth(
      launch_credential: Some(launch_credential),
      launch_expires_at_ms: expires_at_ms,
      runtime_credential: None,
    )),
    redaction_secrets: [],
  )
}

fn expect_initial_outbound(outbound: process.Subject(String)) -> Nil {
  assert string.contains(test_async.expect_message(outbound), "daemon_hello")
  assert string.contains(test_async.expect_message(outbound), "heartbeat")
  assert string.contains(test_async.expect_message(outbound), "daemon_state")
}

fn expect_delay(
  subject: process.Subject(Int),
  expected: Int,
  attempts_remaining: Int,
) -> Nil {
  assert attempts_remaining > 0
  let delay = test_async.expect_message(subject)
  case delay == expected {
    True -> Nil
    False -> expect_delay(subject, expected, attempts_remaining - 1)
  }
}

fn expect_next_outbound_contains(
  outbound: process.Subject(String),
  fragment: String,
) -> String {
  let message = test_async.expect_message(outbound)
  assert string.contains(message, fragment)
  message
}

fn eventually_has_outbound(
  outbound: process.Subject(String),
  fragment: String,
) -> Bool {
  let message = test_async.expect_message(outbound)
  case string.contains(message, fragment) {
    True -> True
    False -> eventually_has_outbound(outbound, fragment)
  }
}

fn append_pause_commands(
  path: String,
  prefix: String,
  next: Int,
  last: Int,
) -> Nil {
  append_inbound_block(path, pause_command_block(prefix, next, last, ""))
}

fn pause_command_block(
  prefix: String,
  next: Int,
  last: Int,
  acc: String,
) -> String {
  case next > last {
    True -> acc
    False ->
      pause_command_block(
        prefix,
        next + 1,
        last,
        acc
          <> server_command_frame(prefix <> int.to_string(next), "pause")
          <> "\n",
      )
  }
}

fn query_request_block(prefix: String, next: Int, last: Int) -> String {
  case next > last {
    True -> ""
    False ->
      status_query_request(prefix <> int.to_string(next))
      <> "\n"
      <> query_request_block(prefix, next + 1, last)
  }
}

fn append_inbound_block(path: String, block: String) -> Nil {
  let existing = case simplifile.read(path) {
    Ok(contents) -> contents
    Error(_) -> ""
  }
  let assert Ok(Nil) = simplifile.write(path, existing <> block)
  Nil
}

fn expect_pause_apply_requests(
  subject: process.Subject(ApplyRequest),
  remaining: Int,
) -> Nil {
  case remaining <= 0 {
    True -> Nil
    False -> {
      let ApplyRequest(operator_command, _) = test_async.expect_message(subject)
      assert operator_command == command.PauseDispatch
      expect_pause_apply_requests(subject, remaining - 1)
    }
  }
}

fn expect_status_query_requests(
  subject: process.Subject(QueryRequestCall),
  remaining: Int,
) -> Nil {
  case remaining <= 0 {
    True -> Nil
    False -> {
      let QueryRequestCall(query, _) = test_async.expect_message(subject)
      assert query == query_types.Status
      expect_status_query_requests(subject, remaining - 1)
    }
  }
}

fn release_barriers(barrier: test_async.Barrier, remaining: Int) -> Nil {
  case remaining <= 0 {
    True -> Nil
    False -> {
      test_async.release_barrier(barrier)
      release_barriers(barrier, remaining - 1)
    }
  }
}

fn status_query_request(query_id: String) -> String {
  status_query_request_for(query_id, "daemon_abc", "boot_abc")
}

fn status_query_request_for(
  query_id: String,
  daemon_id: String,
  boot_id: String,
) -> String {
  "{\"type\":\"query_request\",\"queryId\":\""
  <> query_id
  <> "\",\"daemonId\":\""
  <> daemon_id
  <> "\",\"bootId\":\""
  <> boot_id
  <> "\",\"query\":{\"version\":1,\"type\":\"status\"}}"
}

fn server_command_frame(command_id: String, command_type: String) -> String {
  server_command_frame_with_command(
    command_id,
    "{\"type\":\"" <> command_type <> "\"}",
  )
}

fn server_command_frame_for(
  command_id: String,
  daemon_id: String,
  boot_id: String,
  command_type: String,
) -> String {
  "{\"type\":\"server_command\",\"serverCommandId\":\""
  <> command_id
  <> "\",\"daemonId\":\""
  <> daemon_id
  <> "\",\"bootId\":\""
  <> boot_id
  <> "\",\"command\":{\"type\":\""
  <> command_type
  <> "\"}}"
}

fn server_command_frame_with_command(
  command_id: String,
  command_json: String,
) -> String {
  "{\"type\":\"server_command\",\"serverCommandId\":\""
  <> command_id
  <> "\",\"daemonId\":\"daemon_abc\",\"bootId\":\"boot_abc\",\"command\":"
  <> command_json
  <> "}"
}

fn expect_log_contains(
  subject: process.Subject(String),
  event: String,
) -> String {
  let entry = test_async.expect_message(subject)
  case string.contains(entry, event) {
    True -> entry
    False -> expect_log_contains(subject, event)
  }
}

fn session_summary() -> event.SessionSummary {
  session_summary_with_status("session-1", event.Running)
}

fn session_summary_with_status(
  session_id: String,
  status: event.SessionStatus,
) -> event.SessionSummary {
  event.SessionSummary(
    session_id: session_id,
    display_name: "Demo session",
    issue_id: "issue-1",
    issue_identifier: "LIV-1",
    issue_title: "Remote state",
    workspace_path: "test/tmp/workspace",
    pi_session_id: None,
    status: status,
    recovery: None,
    current_turn: 3,
    current_turn_status: None,
    last_turn_finished_at_ms: None,
    last_turn_duration_ms: None,
    last_turn_token_delta: session_tokens.zero_token_totals(),
    current_turn_started_at_ms: None,
    last_turn_reason: None,
    started_at_ms: 10,
    last_event_at_ms: 123,
    token_totals: session_tokens.zero_token_totals(),
  )
}

fn append_inbound_line(path: String, line: String) -> Nil {
  let existing = case simplifile.read(path) {
    Ok(contents) -> contents
    Error(_) -> ""
  }
  let assert Ok(Nil) = simplifile.write(path, existing <> line <> "\n")
  Nil
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer() -> Int

fn read_inbound_line(path: String) -> Result(String, String) {
  case simplifile.read(path) {
    Error(_) -> Error("timeout")
    Ok(contents) ->
      case string.split(contents, "\n") {
        [first, ..rest] if first != "" -> {
          let remaining = string.join(rest, with: "\n")
          let assert Ok(Nil) = simplifile.write(path, remaining)
          case string.starts_with(first, "FAIL:") {
            True -> Error(string.drop_start(first, 5))
            False -> Ok(first)
          }
        }
        _ -> Error("timeout")
      }
  }
}
