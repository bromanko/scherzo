import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/control/command
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

type ApplyRequest {
  ApplyRequest(command: command.OperatorCommand, timeout_ms: Int)
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
    closes: process.Subject(Nil),
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
  let apply_requests = process.new_subject()
  let closes = process.new_subject()
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
      command_timeout_ms: 75,
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
      dispatch_paused: fn(_) { Ok(False) },
      apply_command: fn(operator_command, timeout_ms) {
        process.send(apply_requests, ApplyRequest(operator_command, timeout_ms))
        Ok(command_result_for(operator_command))
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
    closes,
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
    // This test intentionally holds apply workers at the barrier; keep periodic
    // frames from interleaving with the overloaded-command assertion on slow CI.
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

fn expect_initial_outbound(outbound: process.Subject(String)) -> Nil {
  assert string.contains(test_async.expect_message(outbound), "daemon_hello")
  assert string.contains(test_async.expect_message(outbound), "heartbeat")
  assert string.contains(test_async.expect_message(outbound), "daemon_state")
}

fn expect_next_outbound_contains(
  outbound: process.Subject(String),
  fragment: String,
) -> String {
  let message = test_async.expect_message(outbound)
  assert string.contains(message, fragment)
  message
}

fn append_pause_commands(
  path: String,
  prefix: String,
  next: Int,
  last: Int,
) -> Nil {
  case next > last {
    True -> Nil
    False -> {
      append_inbound_line(
        path,
        server_command_frame(prefix <> int.to_string(next), "pause"),
      )
      append_pause_commands(path, prefix, next + 1, last)
    }
  }
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

fn release_barriers(barrier: test_async.Barrier, remaining: Int) -> Nil {
  case remaining <= 0 {
    True -> Nil
    False -> {
      test_async.release_barrier(barrier)
      release_barriers(barrier, remaining - 1)
    }
  }
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
