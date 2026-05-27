import gleam/erlang/process
import gleam/list
import gleam/string
import scherzo/control/remote/client
import scherzo/control/remote_envelope
import scherzo/control/remote_harness_hello
import scherzo/log
import test_async

pub fn remote_client_sends_hello_heartbeat_and_state_snapshot_test() {
  let session =
    remote_envelope.RemoteSession(
      session_id: "session-1",
      display_name: "Demo session",
      issue_identifier: "LIV-686",
      status: "running",
      current_turn: 3,
      last_event_at_ms: 123,
    )
  let fixture = new_fixture([session])

  let assert Ok(handle) = client.start(fixture.settings, fixture.dependencies)
  let ConnectRequest(_, _, reply) =
    test_async.expect_message(fixture.connect_requests)
  process.send(reply, Ok(fixture.lines))

  let hello_line = test_async.expect_message(fixture.lines)
  let assert Ok(hello) =
    remote_harness_hello.decode(hello_line, fixture.settings.enrollment_token)
  assert hello.daemon_id == fixture.settings.daemon_id
  assert hello.boot_id == fixture.settings.boot_id
  assert hello.capabilities == fixture.settings.capabilities

  let heartbeat_line = test_async.expect_message(fixture.lines)
  let assert Ok(remote_envelope.RemoteHeartbeat(sent_at_ms)) =
    remote_envelope.decode(heartbeat_line)
  assert sent_at_ms == 101

  let state_line = test_async.expect_message(fixture.lines)
  let assert Ok(remote_envelope.RemoteStateSnapshot(now_ms, sessions)) =
    remote_envelope.decode(state_line)
  assert now_ms == 101
  assert sessions == [session]

  let assert Ok(Nil) = client.stop(handle, 1000)
}

pub fn remote_client_retries_with_bounded_backoff_test() {
  let fixture = new_fixture([])
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

pub fn remote_client_retries_after_hello_send_failure_test() {
  let fixture = new_fixture_with_send_failure([], FailHello)
  let settings =
    client.Settings(
      ..fixture.settings,
      retry_initial_ms: 1000,
      retry_max_ms: 1000,
    )

  let assert Ok(handle) = client.start(settings, fixture.dependencies)
  assert test_async.expect_message(fixture.scheduled_delays) == 0
  let ConnectRequest(_, _, reply) =
    test_async.expect_message(fixture.connect_requests)
  process.send(reply, Ok(fixture.lines))

  assert test_async.expect_message(fixture.closes) == "closed"
  assert test_async.expect_message(fixture.scheduled_delays) == 1000
  test_async.assert_no_extra_message_within(fixture.lines, 50)

  let assert Ok(Nil) = client.stop(handle, 1000)
}

pub fn remote_client_retries_after_heartbeat_send_failure_test() {
  let fixture = new_fixture_with_send_failure([], FailHeartbeat)
  let settings =
    client.Settings(
      ..fixture.settings,
      retry_initial_ms: 1000,
      retry_max_ms: 1000,
    )

  let assert Ok(handle) = client.start(settings, fixture.dependencies)
  assert test_async.expect_message(fixture.scheduled_delays) == 0
  let ConnectRequest(_, _, reply) =
    test_async.expect_message(fixture.connect_requests)
  process.send(reply, Ok(fixture.lines))

  let hello_line = test_async.expect_message(fixture.lines)
  let assert Ok(_) =
    remote_harness_hello.decode(hello_line, fixture.settings.enrollment_token)
  assert test_async.expect_message(fixture.closes) == "closed"
  assert test_async.expect_message(fixture.scheduled_delays) == 1000
  test_async.assert_no_extra_message_within(fixture.lines, 50)

  let assert Ok(Nil) = client.stop(handle, 1000)
}

pub fn remote_client_retries_after_state_send_failure_test() {
  let fixture = new_fixture_with_send_failure([], FailState)
  let settings =
    client.Settings(
      ..fixture.settings,
      retry_initial_ms: 1000,
      retry_max_ms: 1000,
    )

  let assert Ok(handle) = client.start(settings, fixture.dependencies)
  assert test_async.expect_message(fixture.scheduled_delays) == 0
  let ConnectRequest(_, _, reply) =
    test_async.expect_message(fixture.connect_requests)
  process.send(reply, Ok(fixture.lines))

  let hello_line = test_async.expect_message(fixture.lines)
  let assert Ok(_) =
    remote_harness_hello.decode(hello_line, fixture.settings.enrollment_token)
  let heartbeat_line = test_async.expect_message(fixture.lines)
  let assert Ok(remote_envelope.RemoteHeartbeat(_)) =
    remote_envelope.decode(heartbeat_line)
  assert test_async.expect_message(fixture.closes) == "closed"
  assert test_async.expect_message(fixture.scheduled_delays) == 1000
  test_async.assert_no_extra_message_within(fixture.lines, 50)

  let assert Ok(Nil) = client.stop(handle, 1000)
}

pub fn remote_client_retries_after_session_snapshot_failure_test() {
  let fixture =
    new_fixture_with_session_failure("event_hub_list_sessions_timeout")
  let settings =
    client.Settings(
      ..fixture.settings,
      retry_initial_ms: 1000,
      retry_max_ms: 1000,
    )

  let assert Ok(handle) = client.start(settings, fixture.dependencies)
  assert test_async.expect_message(fixture.scheduled_delays) == 0
  let ConnectRequest(_, _, reply) =
    test_async.expect_message(fixture.connect_requests)
  process.send(reply, Ok(fixture.lines))

  let hello_line = test_async.expect_message(fixture.lines)
  let assert Ok(_) =
    remote_harness_hello.decode(hello_line, fixture.settings.enrollment_token)
  let heartbeat_line = test_async.expect_message(fixture.lines)
  let assert Ok(remote_envelope.RemoteHeartbeat(_)) =
    remote_envelope.decode(heartbeat_line)
  assert test_async.expect_message(fixture.closes) == "closed"
  assert test_async.expect_message(fixture.scheduled_delays) == 1000
  test_async.assert_no_extra_message_within(fixture.lines, 50)

  let assert Ok(Nil) = client.stop(handle, 1000)
}

pub fn remote_client_stop_cancels_timers_and_closes_connection_test() {
  let fixture = new_fixture([])

  let assert Ok(handle) = client.start(fixture.settings, fixture.dependencies)
  let ConnectRequest(_, _, reply) =
    test_async.expect_message(fixture.connect_requests)
  process.send(reply, Ok(fixture.lines))

  let _ = test_async.expect_message(fixture.lines)
  let _ = test_async.expect_message(fixture.lines)
  let _ = test_async.expect_message(fixture.lines)

  let assert Ok(Nil) = client.stop(handle, 1000)

  assert test_async.expect_message(fixture.closes) == "closed"
  assert list.length(test_async.drain_subject(fixture.cancelled_timers)) >= 2
  test_async.assert_no_extra_message_within(fixture.lines, 50)
}

pub fn remote_client_redacts_tokens_in_logs_test() {
  let fixture = new_fixture([])
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

type Fixture {
  Fixture(
    settings: client.Settings,
    dependencies: client.Dependencies(process.Subject(String), process.Timer),
    connect_requests: process.Subject(ConnectRequest),
    lines: process.Subject(String),
    closes: process.Subject(String),
    scheduled_delays: process.Subject(Int),
    cancelled_timers: process.Subject(Int),
    logs: process.Subject(String),
  )
}

type SessionMode {
  SessionList(List(remote_envelope.RemoteSession))
  SessionFailure(String)
}

type ConnectRequest {
  ConnectRequest(
    String,
    Int,
    process.Subject(Result(process.Subject(String), String)),
  )
}

type SendFailure {
  NoSendFailure
  FailHello
  FailHeartbeat
  FailState
}

fn new_fixture(sessions: List(remote_envelope.RemoteSession)) -> Fixture {
  new_fixture_with_modes(SessionList(sessions), NoSendFailure)
}

fn new_fixture_with_send_failure(
  sessions: List(remote_envelope.RemoteSession),
  send_failure: SendFailure,
) -> Fixture {
  new_fixture_with_modes(SessionList(sessions), send_failure)
}

fn new_fixture_with_session_failure(message: String) -> Fixture {
  new_fixture_with_modes(SessionFailure(message), NoSendFailure)
}

fn new_fixture_with_modes(
  session_mode: SessionMode,
  send_failure: SendFailure,
) -> Fixture {
  let connect_requests = process.new_subject()
  let lines = process.new_subject()
  let closes = process.new_subject()
  let scheduled_delays = process.new_subject()
  let cancelled_timers = process.new_subject()
  let logs = process.new_subject()
  let settings =
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
      redaction_secrets: ["test-token"],
    )
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
        case should_fail_send(send_failure, line, settings.enrollment_token) {
          True -> Error("injected_send_failure")
          False -> {
            process.send(connection, line)
            Ok(Nil)
          }
        }
      },
      close: fn(_connection) {
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
      logger: fn(level, event, fields, secrets) {
        process.send(logs, log.format(level, event, fields, secrets))
        Ok(Nil)
      },
    )
  Fixture(
    settings: settings,
    dependencies: dependencies,
    connect_requests: connect_requests,
    lines: lines,
    closes: closes,
    scheduled_delays: scheduled_delays,
    cancelled_timers: cancelled_timers,
    logs: logs,
  )
}

fn should_fail_send(
  send_failure: SendFailure,
  line: String,
  enrollment_token: String,
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
        Ok(remote_envelope.RemoteStateSnapshot(_, _)) -> True
        _ -> False
      }
  }
}
