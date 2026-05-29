import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/string
import scherzo/control/command
import scherzo/control/remote/client
import scherzo/control/remote_envelope
import scherzo/control/remote_harness_hello
import scherzo/log
import simplifile
import support/test_helpers
import test_async

type Connection {
  Connection(
    outbound: process.Subject(String),
    inbound_path: String,
    closed_path: String,
  )
}

type Fixture {
  Fixture(
    settings: client.Settings,
    dependencies: client.Dependencies(Connection, process.Timer),
    inbound_root: String,
    connect_requests: process.Subject(ConnectRequest),
    outbound: process.Subject(String),
    closes: process.Subject(String),
    scheduled_delays: process.Subject(Int),
    cancelled_timers: process.Subject(Int),
    logs: process.Subject(String),
    apply_requests: process.Subject(ApplyRequest),
  )
}

type ConnectRequest {
  ConnectRequest(String, Int, process.Subject(Result(Connection, String)))
}

type ApplyBehavior {
  ApplyImmediately
  ApplyDelay(Int)
  ApplyTimeout
}

type SendFailure {
  NoSendFailure
  FailHello
  FailHeartbeat
  FailState
  FailFirstCommandReceipt
}

type SessionMode {
  SessionList(List(remote_envelope.RemoteSession))
  SessionFailure(String)
}

type ApplyRequest {
  ApplyRequest(command.OperatorCommand, Int)
}

pub fn remote_client_sends_hello_heartbeat_and_state_snapshot_test() {
  let session = session_fixture()
  let fixture =
    new_fixture(SessionList([session]), NoSendFailure, ApplyImmediately)

  let assert Ok(handle) = client.start(fixture.settings, fixture.dependencies)
  let _ = connect_ok(fixture)

  assert_hello(fixture)
  assert_heartbeat(fixture)
  let assert remote_envelope.RemoteStateSnapshot(
    now_ms,
    dispatch_paused,
    sessions,
  ) = receive_envelope(fixture.outbound)
  assert now_ms == 101
  assert dispatch_paused == False
  assert sessions == [session]

  let assert Ok(Nil) = client.stop(handle, 1000)
}

pub fn remote_client_receipt_result_and_state_order_for_pause_test() {
  let session = session_fixture()
  let fixture =
    new_fixture(SessionList([session]), NoSendFailure, ApplyImmediately)

  let assert Ok(handle) = client.start(fixture.settings, fixture.dependencies)
  let connection = connect_ok(fixture)
  let _ = assert_hello(fixture)
  let _ = assert_heartbeat(fixture)
  let _ = receive_envelope(fixture.outbound)

  send_server_command(connection, "pause-1", command.PauseDispatch)

  let assert remote_envelope.RemoteCommandReceipt(command_id, accepted, _) =
    receive_envelope_of_kind(fixture.outbound, "command_receipt")
  assert command_id == "pause-1"
  assert accepted

  let ApplyRequest(applied_command, timeout_ms) =
    test_async.expect_message(fixture.apply_requests)
  assert applied_command == command.PauseDispatch
  assert timeout_ms == fixture.settings.command_timeout_ms

  let assert remote_envelope.RemoteCommandResult(result_id, result) =
    receive_envelope_of_kind(fixture.outbound, "command_result")
  assert result_id == "pause-1"
  assert result.status == command.Applied

  let assert remote_envelope.RemoteStateSnapshot(_, dispatch_paused, _) =
    receive_envelope(fixture.outbound)
  assert dispatch_paused

  let assert Ok(Nil) = client.stop(handle, 1000)
}

pub fn remote_client_rejects_apply_timeout_with_existing_status_reason_and_message_test() {
  let fixture =
    new_fixture(SessionList([session_fixture()]), NoSendFailure, ApplyTimeout)

  let assert Ok(handle) = client.start(fixture.settings, fixture.dependencies)
  let connection = connect_ok(fixture)
  let _ = assert_hello(fixture)
  let _ = assert_heartbeat(fixture)
  let _ = receive_envelope(fixture.outbound)

  send_server_command(connection, "pause-1", command.PauseDispatch)

  let assert remote_envelope.RemoteCommandReceipt(command_id, accepted, _) =
    receive_envelope_of_kind(fixture.outbound, "command_receipt")
  assert command_id == "pause-1"
  assert accepted

  let ApplyRequest(applied_command, timeout_ms) =
    test_async.expect_message(fixture.apply_requests)
  assert applied_command == command.PauseDispatch
  assert timeout_ms == fixture.settings.command_timeout_ms

  let assert remote_envelope.RemoteCommandResult(result_id, result) =
    receive_envelope_of_kind(fixture.outbound, "command_result")
  assert result_id == "pause-1"
  assert result.status == command.Rejected("remote_command_timeout")
  assert result.message == Some("remote command timed out")

  let assert remote_envelope.RemoteStateSnapshot(_, dispatch_paused, _) =
    receive_envelope(fixture.outbound)
  assert dispatch_paused == False

  let assert Ok(Nil) = client.stop(handle, 1000)
}

pub fn remote_client_in_flight_duplicate_emits_single_apply_test() {
  let fixture =
    new_fixture(
      SessionList([session_fixture()]),
      NoSendFailure,
      ApplyDelay(100),
    )

  let assert Ok(handle) = client.start(fixture.settings, fixture.dependencies)
  let connection = connect_ok(fixture)
  let _ = assert_hello(fixture)
  let _ = assert_heartbeat(fixture)
  let _ = receive_envelope(fixture.outbound)

  send_server_command(connection, "pause-1", command.PauseDispatch)

  let assert remote_envelope.RemoteCommandReceipt(_, accepted_first, _) =
    receive_envelope_of_kind(fixture.outbound, "command_receipt")
  assert accepted_first
  let _ = test_async.expect_message(fixture.apply_requests)

  send_server_command(connection, "pause-1", command.PauseDispatch)

  test_async.assert_no_extra_message_within(fixture.apply_requests, 50)

  let assert remote_envelope.RemoteCommandResult(_, result) =
    receive_envelope_of_kind(fixture.outbound, "command_result")
  assert result.status == command.Applied
  let _ = receive_envelope(fixture.outbound)

  let assert Ok(Nil) = client.stop(handle, 1000)
}

pub fn remote_client_completed_duplicate_reuses_cached_result_test() {
  let fixture =
    new_fixture(
      SessionList([session_fixture()]),
      NoSendFailure,
      ApplyImmediately,
    )

  let assert Ok(handle) = client.start(fixture.settings, fixture.dependencies)
  let connection = connect_ok(fixture)
  let _ = assert_hello(fixture)
  let _ = assert_heartbeat(fixture)
  let _ = receive_envelope(fixture.outbound)

  send_server_command(connection, "pause-1", command.PauseDispatch)
  let _ = receive_envelope(fixture.outbound)
  let _ = test_async.expect_message(fixture.apply_requests)
  let _ = receive_envelope(fixture.outbound)
  let _ = receive_envelope(fixture.outbound)

  send_server_command(connection, "pause-1", command.PauseDispatch)
  let assert remote_envelope.RemoteCommandReceipt(_, accepted, message) =
    receive_envelope(fixture.outbound)
  assert accepted
  assert message == Some("command result replayed")
  let assert remote_envelope.RemoteCommandResult(_, result) =
    receive_envelope(fixture.outbound)
  assert result.status == command.Applied
  let _ = receive_envelope(fixture.outbound)
  test_async.assert_no_extra_message_within(fixture.apply_requests, 50)

  let assert Ok(Nil) = client.stop(handle, 1000)
}

pub fn remote_client_conflicting_duplicate_rejected_before_apply_test() {
  let fixture =
    new_fixture(
      SessionList([session_fixture()]),
      NoSendFailure,
      ApplyImmediately,
    )

  let assert Ok(handle) = client.start(fixture.settings, fixture.dependencies)
  let connection = connect_ok(fixture)
  let _ = assert_hello(fixture)
  let _ = assert_heartbeat(fixture)
  let _ = receive_envelope(fixture.outbound)

  send_server_command(connection, "cmd-1", command.PauseDispatch)
  let _ = receive_envelope(fixture.outbound)
  let _ = test_async.expect_message(fixture.apply_requests)
  let _ = receive_envelope(fixture.outbound)
  let _ = receive_envelope(fixture.outbound)

  send_server_command(connection, "cmd-1", command.ResumeDispatch)
  let assert remote_envelope.RemoteCommandReceipt(_, accepted, _) =
    receive_envelope(fixture.outbound)
  assert !accepted
  let assert remote_envelope.RemoteCommandResult(_, result) =
    receive_envelope(fixture.outbound)
  assert result.status == command.Rejected("remote_command_id_conflict")
  let _ = receive_envelope(fixture.outbound)
  test_async.assert_no_extra_message_within(fixture.apply_requests, 50)

  let assert Ok(Nil) = client.stop(handle, 1000)
}

pub fn remote_client_rejects_unsupported_remote_command_before_apply_test() {
  let fixture =
    new_fixture(
      SessionList([session_fixture()]),
      NoSendFailure,
      ApplyImmediately,
    )

  let assert Ok(handle) = client.start(fixture.settings, fixture.dependencies)
  let connection = connect_ok(fixture)
  let _ = assert_hello(fixture)
  let _ = assert_heartbeat(fixture)
  let _ = receive_envelope(fixture.outbound)

  send_server_command(connection, "cmd-1", command.ReloadWorkflow)
  let assert remote_envelope.RemoteCommandReceipt(_, accepted, _) =
    receive_envelope(fixture.outbound)
  assert !accepted
  let assert remote_envelope.RemoteCommandResult(_, result) =
    receive_envelope(fixture.outbound)
  assert result.status == command.Rejected("unsupported_remote_command")
  let _ = receive_envelope(fixture.outbound)
  test_async.assert_no_extra_message_within(fixture.apply_requests, 50)

  let assert Ok(Nil) = client.stop(handle, 1000)
}

pub fn remote_client_inbound_reader_does_not_block_heartbeat_or_state_test() {
  let settings =
    client.Settings(
      ..base_settings(),
      heartbeat_interval_ms: 20,
      state_interval_ms: 30,
    )
  let fixture =
    new_fixture_with_settings(
      settings,
      SessionList([session_fixture()]),
      NoSendFailure,
      ApplyDelay(100),
    )

  let assert Ok(handle) = client.start(fixture.settings, fixture.dependencies)
  let connection = connect_ok(fixture)
  let _ = assert_hello(fixture)
  let _ = assert_heartbeat(fixture)
  let _ = receive_envelope(fixture.outbound)

  send_server_command(connection, "pause-1", command.PauseDispatch)
  let _ = receive_envelope(fixture.outbound)
  let _ = test_async.expect_message(fixture.apply_requests)

  assert eventually_has_envelope_kind(fixture.outbound, "heartbeat")
  assert eventually_has_envelope_kind(fixture.outbound, "state_snapshot")

  let assert Ok(Nil) = client.stop(handle, 1000)
}

pub fn remote_client_ignores_malformed_and_unexpected_inbound_lines_test() {
  let settings =
    client.Settings(
      ..base_settings(),
      heartbeat_interval_ms: 20,
      state_interval_ms: 30,
    )
  let fixture =
    new_fixture_with_settings(
      settings,
      SessionList([session_fixture()]),
      NoSendFailure,
      ApplyImmediately,
    )

  let assert Ok(handle) = client.start(fixture.settings, fixture.dependencies)
  let connection = connect_ok(fixture)
  let _ = assert_hello(fixture)
  let _ = assert_heartbeat(fixture)
  let _ = receive_envelope(fixture.outbound)
  let _ = test_async.drain_subject(fixture.logs)

  append_inbound_line(connection.inbound_path, "{not-json}")
  append_inbound_line(
    connection.inbound_path,
    remote_envelope.RemoteHeartbeat(999) |> remote_envelope.to_string,
  )

  assert eventually_has_log(fixture.logs, "remote_client_bad_inbound")
  assert eventually_has_log(fixture.logs, "remote_client_unexpected_inbound")
  test_async.assert_no_extra_message_within(fixture.apply_requests, 50)
  assert eventually_has_envelope_kind(fixture.outbound, "heartbeat")
  assert eventually_has_envelope_kind(fixture.outbound, "state_snapshot")

  let assert Ok(Nil) = client.stop(handle, 1000)
}

pub fn remote_client_retries_with_bounded_backoff_test() {
  let fixture = new_fixture(SessionList([]), NoSendFailure, ApplyImmediately)
  let settings =
    client.Settings(..fixture.settings, retry_initial_ms: 5, retry_max_ms: 10)

  let assert Ok(handle) = client.start(settings, fixture.dependencies)

  assert test_async.expect_message(fixture.scheduled_delays) == 0
  let ConnectRequest(_, _, first_reply) =
    test_async.expect_message(fixture.connect_requests)
  process.send(first_reply, Error("unreachable-1"))

  assert test_async.expect_message(fixture.scheduled_delays) == 5
  let ConnectRequest(_, _, second_reply) =
    test_async.expect_message(fixture.connect_requests)
  process.send(second_reply, Error("unreachable-2"))

  assert test_async.expect_message(fixture.scheduled_delays) == 10
  let ConnectRequest(_, _, third_reply) =
    test_async.expect_message(fixture.connect_requests)
  process.send(third_reply, Error("unreachable-3"))

  assert test_async.expect_message(fixture.scheduled_delays) == 10
  let assert Ok(Nil) = client.stop(handle, 1000)
}

pub fn remote_client_retries_after_state_send_failure_test() {
  let fixture =
    new_fixture(SessionList([session_fixture()]), FailState, ApplyImmediately)
  let settings =
    client.Settings(
      ..fixture.settings,
      retry_initial_ms: 1000,
      retry_max_ms: 1000,
    )

  let assert Ok(handle) = client.start(settings, fixture.dependencies)
  assert test_async.expect_message(fixture.scheduled_delays) == 0
  let _ = connect_ok(fixture)
  let _ = assert_hello(fixture)
  let _ = assert_heartbeat(fixture)
  assert test_async.expect_message(fixture.closes) == "closed"
  assert test_async.expect_message(fixture.scheduled_delays) == 1000
  let assert Ok(Nil) = client.stop(handle, 1000)
}

pub fn remote_client_caches_stale_apply_completion_across_reconnect_test() {
  let fixture =
    new_fixture(
      SessionList([session_fixture()]),
      NoSendFailure,
      ApplyDelay(100),
    )

  let assert Ok(handle) = client.start(fixture.settings, fixture.dependencies)
  assert test_async.expect_message(fixture.scheduled_delays) == 0
  let first_connection = connect_ok(fixture)
  let _ = assert_hello(fixture)
  let _ = assert_heartbeat(fixture)
  let _ = receive_envelope(fixture.outbound)

  send_server_command(first_connection, "pause-1", command.PauseDispatch)
  let _ = receive_envelope_of_kind(fixture.outbound, "command_receipt")
  let _ = test_async.expect_message(fixture.apply_requests)

  close_connection(first_connection)
  assert test_async.expect_message(fixture.closes) == "closed"
  assert list.contains(test_async.drain_subject(fixture.scheduled_delays), 25)

  let second_connection = connect_ok(fixture)
  let _ = assert_hello(fixture)
  let _ = assert_heartbeat(fixture)
  let _ = receive_envelope(fixture.outbound)

  test_async.assert_no_extra_message_within(fixture.outbound, 150)

  send_server_command(second_connection, "pause-1", command.PauseDispatch)
  let assert remote_envelope.RemoteCommandReceipt(command_id, accepted, _) =
    receive_envelope_of_kind(fixture.outbound, "command_receipt")
  assert command_id == "pause-1"
  assert accepted
  let assert remote_envelope.RemoteCommandResult(result_id, result) =
    receive_envelope_of_kind(fixture.outbound, "command_result")
  assert result_id == "pause-1"
  assert result.status == command.Applied
  let assert remote_envelope.RemoteStateSnapshot(_, dispatch_paused, _) =
    receive_envelope_of_kind(fixture.outbound, "state_snapshot")
  assert dispatch_paused
  test_async.assert_no_extra_message_within(fixture.apply_requests, 50)

  let assert Ok(Nil) = client.stop(handle, 1000)
}

pub fn remote_client_clears_inflight_when_command_receipt_send_fails_test() {
  let fixture =
    new_fixture(
      SessionList([session_fixture()]),
      FailFirstCommandReceipt,
      ApplyImmediately,
    )

  let assert Ok(handle) = client.start(fixture.settings, fixture.dependencies)
  let first_connection = connect_ok(fixture)
  let _ = assert_hello(fixture)
  let _ = assert_heartbeat(fixture)
  let _ = receive_envelope(fixture.outbound)

  send_server_command(first_connection, "pause-1", command.PauseDispatch)
  assert test_async.expect_message(fixture.closes) == "closed"
  test_async.assert_no_extra_message_within(fixture.apply_requests, 50)

  let second_connection = connect_ok(fixture)
  let _ = assert_hello(fixture)
  let _ = assert_heartbeat(fixture)
  let _ = receive_envelope(fixture.outbound)

  send_server_command(second_connection, "pause-1", command.PauseDispatch)
  let assert remote_envelope.RemoteCommandReceipt(command_id, accepted, _) =
    receive_envelope_of_kind(fixture.outbound, "command_receipt")
  assert command_id == "pause-1"
  assert accepted
  let ApplyRequest(applied_command, _) =
    test_async.expect_message(fixture.apply_requests)
  assert applied_command == command.PauseDispatch

  let assert Ok(Nil) = client.stop(handle, 1000)
}

pub fn remote_client_stop_cancels_timers_and_closes_connection_test() {
  let fixture = new_fixture(SessionList([]), NoSendFailure, ApplyImmediately)

  let assert Ok(handle) = client.start(fixture.settings, fixture.dependencies)
  let _ = connect_ok(fixture)
  let _ = assert_hello(fixture)
  let _ = assert_heartbeat(fixture)
  let _ = receive_envelope(fixture.outbound)

  let assert Ok(Nil) = client.stop(handle, 1000)
  assert test_async.expect_message(fixture.closes) == "closed"
  assert list.length(test_async.drain_subject(fixture.cancelled_timers)) >= 2
}

pub fn remote_client_redacts_tokens_in_logs_test() {
  let fixture = new_fixture(SessionList([]), NoSendFailure, ApplyImmediately)
  let settings =
    client.Settings(
      ..fixture.settings,
      enrollment_token: "secret-token",
      redaction_secrets: ["secret-token"],
    )

  let assert Ok(handle) = client.start(settings, fixture.dependencies)
  let ConnectRequest(_, _, reply) =
    test_async.expect_message(fixture.connect_requests)
  process.send(reply, Error("connect refused"))

  let logs = [
    test_async.expect_message(fixture.logs),
    test_async.expect_message(fixture.logs),
  ]
  assert list.any(logs, fn(entry) { string.contains(entry, "[REDACTED]") })
  assert !list.any(logs, fn(entry) { string.contains(entry, "secret-token") })

  let assert Ok(Nil) = client.stop(handle, 1000)
}

fn new_fixture(
  session_mode: SessionMode,
  send_failure: SendFailure,
  apply_behavior: ApplyBehavior,
) -> Fixture {
  new_fixture_with_settings(
    base_settings(),
    session_mode,
    send_failure,
    apply_behavior,
  )
}

fn new_fixture_with_settings(
  settings: client.Settings,
  session_mode: SessionMode,
  send_failure: SendFailure,
  apply_behavior: ApplyBehavior,
) -> Fixture {
  let root =
    "test/tmp/control-remote-client/" <> int.to_string(unique_integer())
  test_helpers.reset_dir(root)
  let connect_requests = process.new_subject()
  let outbound = process.new_subject()
  let closes = process.new_subject()
  let scheduled_delays = process.new_subject()
  let cancelled_timers = process.new_subject()
  let logs = process.new_subject()
  let apply_requests = process.new_subject()
  let dependencies =
    client.Dependencies(
      now_ms: fn() { 101 },
      connect: fn(endpoint, timeout_ms) {
        let reply = process.new_subject()
        process.send(
          connect_requests,
          ConnectRequest(endpoint, timeout_ms, reply),
        )
        let assert Ok(result) = process.receive(reply, within: 1000)
        result
      },
      send_line: fn(connection, line, _timeout_ms) {
        case
          should_fail_send(send_failure, line, settings.enrollment_token, root)
        {
          True -> Error("injected_send_failure")
          False -> {
            process.send(connection.outbound, line)
            Ok(Nil)
          }
        }
      },
      recv_line: fn(connection, _timeout_ms) {
        case simplifile.read(connection.closed_path) {
          Ok(_) -> Error("closed")
          Error(_) -> read_inbound_line(connection.inbound_path)
        }
      },
      close: fn(connection) {
        let assert Ok(Nil) = simplifile.write(connection.closed_path, "closed")
        process.send(closes, "closed")
        Nil
      },
      send_after: fn(subject, delay_ms, message) {
        process.send(scheduled_delays, delay_ms)
        process.send_after(subject, delay_ms, message)
      },
      cancel_timer: fn(timer) {
        process.send(cancelled_timers, 1)
        let _ = process.cancel_timer(timer)
        Nil
      },
      list_sessions: fn() {
        case session_mode {
          SessionList(sessions) -> Ok(sessions)
          SessionFailure(message) -> Error(message)
        }
      },
      apply_command: fn(operator_command, timeout_ms) {
        process.send(apply_requests, ApplyRequest(operator_command, timeout_ms))
        case apply_behavior {
          ApplyImmediately ->
            Ok(command.applied(operator_command, Some("applied")))
          ApplyDelay(delay_ms) -> {
            process.sleep(delay_ms)
            Ok(command.applied(operator_command, Some("applied")))
          }
          ApplyTimeout -> Error(Nil)
        }
      },
      dispatch_paused: fn(_timeout_ms) { Error("dispatch_state_unavailable") },
      logger: fn(level, event, fields, secrets) {
        process.send(logs, log.format(level, event, fields, secrets))
        Ok(Nil)
      },
    )
  Fixture(
    settings,
    dependencies,
    root,
    connect_requests,
    outbound,
    closes,
    scheduled_delays,
    cancelled_timers,
    logs,
    apply_requests,
  )
}

fn base_settings() -> client.Settings {
  client.Settings(
    endpoint: "https://ui.example.test",
    daemon_id: "daemon_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    boot_id: "boot_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
    enrollment_token: "test-token",
    capabilities: ["control_commands", "session_snapshots"],
    heartbeat_interval_ms: 250,
    state_interval_ms: 500,
    retry_initial_ms: 25,
    retry_max_ms: 100,
    connect_timeout_ms: 50,
    command_timeout_ms: 75,
    redaction_secrets: ["test-token"],
  )
}

fn session_fixture() -> remote_envelope.RemoteSession {
  remote_envelope.RemoteSession(
    session_id: "session-1",
    display_name: "Demo session",
    issue_identifier: "LIV-686",
    status: "running",
    current_turn: 3,
    last_event_at_ms: 123,
  )
}

fn connect_ok(fixture: Fixture) -> Connection {
  let ConnectRequest(_, _, reply) =
    test_async.expect_message(fixture.connect_requests)
  let connection =
    Connection(
      fixture.outbound,
      fixture.inbound_root <> "/inbound-" <> int.to_string(unique_integer()),
      fixture.inbound_root <> "/closed-" <> int.to_string(unique_integer()),
    )
  process.send(reply, Ok(connection))
  connection
}

fn assert_hello(fixture: Fixture) {
  let hello_line = test_async.expect_message(fixture.outbound)
  let assert Ok(hello) =
    remote_harness_hello.decode(hello_line, fixture.settings.enrollment_token)
  hello
}

fn assert_heartbeat(fixture: Fixture) -> Nil {
  let assert remote_envelope.RemoteHeartbeat(sent_at_ms) =
    receive_envelope(fixture.outbound)
  assert sent_at_ms == 101
  Nil
}

fn receive_envelope(
  lines: process.Subject(String),
) -> remote_envelope.Envelope {
  let line = test_async.expect_message(lines)
  let assert Ok(envelope) = remote_envelope.decode(line)
  envelope
}

fn receive_envelope_of_kind(
  lines: process.Subject(String),
  expected: String,
) -> remote_envelope.Envelope {
  let envelope = receive_envelope(lines)
  case envelope_kind(envelope) == expected {
    True -> envelope
    False -> receive_envelope_of_kind(lines, expected)
  }
}

fn send_server_command(
  connection: Connection,
  command_id: String,
  operator_command: command.OperatorCommand,
) -> Nil {
  append_inbound_line(
    connection.inbound_path,
    remote_envelope.RemoteServerCommand(command_id, operator_command)
      |> remote_envelope.to_string,
  )
}

fn close_connection(connection: Connection) -> Nil {
  let assert Ok(Nil) = simplifile.write(connection.closed_path, "closed")
  Nil
}

fn eventually_has_envelope_kind(
  lines: process.Subject(String),
  expected: String,
) -> Bool {
  case process.receive(lines, within: 200) {
    Ok(line) ->
      case remote_envelope.decode(line) {
        Ok(envelope) ->
          case envelope_kind(envelope) == expected {
            True -> True
            False -> eventually_has_envelope_kind(lines, expected)
          }
        Error(_) -> eventually_has_envelope_kind(lines, expected)
      }
    Error(Nil) -> False
  }
}

fn eventually_has_log(logs: process.Subject(String), expected: String) -> Bool {
  case process.receive(logs, within: 200) {
    Ok(entry) ->
      case string.contains(entry, expected) {
        True -> True
        False -> eventually_has_log(logs, expected)
      }
    Error(Nil) -> False
  }
}

fn envelope_kind(envelope: remote_envelope.Envelope) -> String {
  case envelope {
    remote_envelope.RemoteHello(_) -> "hello"
    remote_envelope.RemoteHeartbeat(_) -> "heartbeat"
    remote_envelope.RemoteServerCommand(_, _) -> "server_command"
    remote_envelope.RemoteCommandReceipt(_, _, _) -> "command_receipt"
    remote_envelope.RemoteCommandResult(_, _) -> "command_result"
    remote_envelope.RemoteStateSnapshot(_, _, _) -> "state_snapshot"
  }
}

fn append_inbound_line(path: String, line: String) -> Nil {
  let existing = case simplifile.read(path) {
    Ok(contents) -> contents
    Error(_) -> ""
  }
  let assert Ok(Nil) = simplifile.write(path, existing <> line <> "\n")
  Nil
}

fn read_inbound_line(path: String) -> Result(String, String) {
  case simplifile.read(path) {
    Error(_) -> Error("timeout")
    Ok(contents) ->
      case string.split(contents, "\n") {
        [first, ..rest] if first != "" -> {
          let remaining = rest |> string.join(with: "\n")
          let assert Ok(Nil) = simplifile.write(path, remaining)
          Ok(first)
        }
        _ -> Error("timeout")
      }
  }
}

fn should_fail_send(
  send_failure: SendFailure,
  line: String,
  enrollment_token: String,
  root: String,
) -> Bool {
  case send_failure {
    NoSendFailure -> False
    FailHello ->
      case remote_harness_hello.decode(line, enrollment_token) {
        Ok(_) -> True
        Error(_) -> False
      }
    FailHeartbeat ->
      case remote_envelope.decode(line) {
        Ok(remote_envelope.RemoteHeartbeat(_)) -> True
        _ -> False
      }
    FailState ->
      case remote_envelope.decode(line) {
        Ok(remote_envelope.RemoteStateSnapshot(_, _, _)) -> True
        _ -> False
      }
    FailFirstCommandReceipt ->
      case remote_envelope.decode(line) {
        Ok(remote_envelope.RemoteCommandReceipt(_, _, _)) ->
          consume_once(root <> "/first-command-receipt-failure")
        _ -> False
      }
  }
}

fn consume_once(path: String) -> Bool {
  case simplifile.read(path) {
    Ok(_) -> False
    Error(_) -> {
      let assert Ok(Nil) = simplifile.write(path, "used")
      True
    }
  }
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer() -> Int
