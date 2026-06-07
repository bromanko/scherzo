import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/control/remote/ui_websocket_client
import scherzo/log
import scherzo/session/event
import scherzo/session/tokens as session_tokens
import simplifile
import support/test_helpers
import test_async

type Connection {
  Connection(outbound: process.Subject(String), inbound_path: String)
}

type ConnectRequest {
  ConnectRequest(url: String, credential: String)
}

type Fixture {
  Fixture(
    settings: ui_websocket_client.Settings,
    deps: ui_websocket_client.Dependencies(Connection, process.Timer),
    outbound: process.Subject(String),
    connects: process.Subject(ConnectRequest),
    delays: process.Subject(Int),
    logs: process.Subject(String),
    inbound_path: String,
  )
}

fn new_fixture() -> Fixture {
  let root = "test/tmp/ui-websocket-client/" <> int.to_string(unique_integer())
  test_helpers.reset_dir(root)
  let outbound = process.new_subject()
  let connects = process.new_subject()
  let delays = process.new_subject()
  let logs = process.new_subject()
  let inbound_path = root <> "/inbound.txt"
  let connection = Connection(outbound, inbound_path)
  let settings =
    ui_websocket_client.Settings(
      server_url: "https://ui.example.test",
      websocket_url: "wss://ui.example.test/api/daemons/ws",
      daemon_id: "daemon_abc",
      boot_id: "boot_abc",
      daemon_label: None,
      credential: "dcred_secret_1",
      heartbeat_interval_ms: 1000,
      state_interval_ms: 1000,
      retry_initial_ms: 50,
      retry_max_ms: 100,
      connect_timeout_ms: 50,
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
      close: fn(_) { Nil },
      send_after: fn(subject, delay, message) {
        process.send(delays, delay)
        process.send_after(subject, delay, message)
      },
      cancel_timer: fn(timer) {
        let _ = process.cancel_timer(timer)
        Nil
      },
      list_sessions: fn() { Ok([session_summary()]) },
      dispatch_paused: fn(_) { Ok(False) },
      logger: fn(level, event, fields, secrets) {
        process.send(logs, log.format(level, event, fields, secrets))
        Ok(Nil)
      },
    )
  Fixture(settings, deps, outbound, connects, delays, logs, inbound_path)
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

pub fn ui_websocket_client_sends_daemon_label_metadata_test() {
  let Fixture(settings:, deps:, outbound:, ..) = new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(
        ..settings,
        daemon_label: Some("Project Foo / MacBook"),
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

pub fn ui_websocket_client_reconnects_after_reader_failure_test() {
  let fixture = new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(fixture.settings, fixture.deps)
  let _ = test_async.expect_message(fixture.connects)
  let _ = test_async.expect_message(fixture.outbound)
  let _ = test_async.expect_message(fixture.outbound)
  let _ = test_async.expect_message(fixture.outbound)
  append_inbound_line(fixture.inbound_path, "FAIL:down")
  let delay = test_async.expect_message(fixture.delays)
  assert delay == 0 || delay == 1000 || delay == 50
  let delays = test_async.drain_subject(fixture.delays)
  assert list.contains(delays, 50)
  let _ = test_async.expect_message(fixture.connects)
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_stops_retrying_after_revocation_test() {
  let fixture = new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(fixture.settings, fixture.deps)
  let _ = test_async.expect_message(fixture.connects)
  let _ = test_async.expect_message(fixture.outbound)
  let _ = test_async.expect_message(fixture.outbound)
  let _ = test_async.expect_message(fixture.outbound)
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
  let _ = test_async.expect_message(fixture.outbound)
  let _ = test_async.expect_message(fixture.outbound)
  let _ = test_async.expect_message(fixture.outbound)
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

pub fn ui_websocket_client_warns_when_command_bridge_is_enabled_test() {
  let Fixture(settings:, deps:, logs:, ..) = new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(
      ui_websocket_client.Settings(..settings, command_bridge_enabled: True),
      deps,
    )

  let entry = expect_log_contains(logs, "ui_websocket_command_bridge_disabled")
  assert string.contains(entry, "command_bridge_enabled is not implemented yet")
  assert ui_websocket_client.stop(handle, 1000) == Ok(Nil)
}

pub fn ui_websocket_client_ignores_too_fast_server_heartbeat_test() {
  let fixture = new_fixture()
  let assert Ok(handle) =
    ui_websocket_client.start(fixture.settings, fixture.deps)
  let _ = test_async.expect_message(fixture.connects)
  let _ = test_async.expect_message(fixture.outbound)
  let _ = test_async.expect_message(fixture.outbound)
  let _ = test_async.expect_message(fixture.outbound)
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
  event.SessionSummary(
    session_id: "session-1",
    display_name: "Demo session",
    issue_id: "issue-1",
    issue_identifier: "LIV-1",
    issue_title: "Remote state",
    workspace_path: "test/tmp/workspace",
    pi_session_id: None,
    status: event.Running,
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
