import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/agent/pi_event
import scherzo/control/client
import scherzo/control/command
import scherzo/control/file
import scherzo/control/protocol
import scherzo/control/server
import scherzo/session/event
import scherzo/session/hub
import scherzo/session/reason
import scherzo/session/tokens as session_tokens

fn summary(session_id: String) -> event.SessionSummary {
  event.SessionSummary(
    session_id: session_id,
    display_name: session_id,
    issue_id: "issue-1",
    issue_identifier: "ABC-1",
    issue_title: "Title ABC-1",
    workspace_path: "test/tmp/control-server/workspaces/ABC-1",
    pi_session_id: None,
    status: event.Running,
    recovery: None,
    current_turn: 1,
    current_turn_status: None,
    current_turn_started_at_ms: None,
    last_turn_finished_at_ms: None,
    last_turn_duration_ms: None,
    last_turn_token_delta: session_tokens.zero_token_totals(),
    last_turn_reason: None,
    started_at_ms: 10,
    last_event_at_ms: 10,
    token_totals: session_tokens.zero_token_totals(),
  )
}

fn payload(name: String) -> event.EventPayload {
  event.empty_payload(
    event.Lifecycle,
    event.PiName(pi_event.UnknownPiEvent(name)),
  )
}

fn start_hub_with_session(session_id: String) -> process.Subject(hub.Message) {
  let assert Ok(subject) = hub.start(20, fn() { 100 })
  hub.register_session(subject, summary(session_id))
  let assert Ok(Some(_)) = hub.get_session(subject, session_id, 1000)
  subject
}

fn start_server_for_hub(
  subject: process.Subject(hub.Message),
  token: String,
) -> #(server.Server, file.ControlFile) {
  start_server_for_backend(server.event_hub_store(subject), token, 500)
}

fn start_server_for_backend(
  backend: server.Backend,
  token: String,
  command_timeout_ms: Int,
) -> #(server.Server, file.ControlFile) {
  start_server_for_backend_with_event_timeout(
    backend,
    token,
    500,
    command_timeout_ms,
  )
}

fn start_server_for_backend_with_event_timeout(
  backend: server.Backend,
  token: String,
  event_timeout_ms: Int,
  command_timeout_ms: Int,
) -> #(server.Server, file.ControlFile) {
  let assert Ok(server_handle) =
    server.start(
      server.Settings(
        host: "127.0.0.1",
        port: 0,
        token: token,
        event_timeout_ms: event_timeout_ms,
        stream_poll_ms: 20,
        command_timeout_ms: command_timeout_ms,
      ),
      backend,
    )
  let control_file =
    file.ControlFile(
      host: "127.0.0.1",
      port: server.bound_port(server_handle),
      token: token,
      workspace_root: "test/tmp/control-server/workspaces",
      started_at_ms: 1,
    )
  #(server_handle, control_file)
}

fn backend_with_command(
  subject: process.Subject(hub.Message),
  command_subject: process.Subject(command.OperatorCommand),
) -> server.Backend {
  server.Backend(
    ..server.event_hub_store(subject),
    apply_command: fn(operator_command, _) {
      process.send(command_subject, operator_command)
      Ok(command.applied(operator_command, Some("done")))
    },
  )
}

fn slow_session_backend(sleep_ms: Int) -> server.Backend {
  server.Backend(
    list_sessions: fn(_) {
      process.sleep(sleep_ms)
      Ok(event.SessionList(sessions: [], now_ms: 100))
    },
    get_session: fn(_, _) {
      process.sleep(sleep_ms)
      Ok(None)
    },
    events_after: fn(_, cursor, _, _) {
      process.sleep(sleep_ms)
      Ok(event.EventPage(events: [], next_cursor: cursor, truncated: False))
    },
    apply_command: fn(operator_command, _) {
      Ok(command.applied(operator_command, None))
    },
  )
}

fn event_hub_timeout_backend() -> server.Backend {
  server.Backend(
    list_sessions: fn(_) { Error(hub.ActorCallTimeout) },
    get_session: fn(_, _) { Error(hub.ActorCallTimeout) },
    events_after: fn(_, _, _, _) { Error(hub.ActorCallTimeout) },
    apply_command: fn(operator_command, _) {
      Ok(command.applied(operator_command, None))
    },
  )
}

fn assert_session_backend_timeout_message(message: String) -> Nil {
  assert string.contains(message, "control server is reachable")
  assert string.contains(message, "session backend")
  assert string.contains(message, "configured timeout")
}

pub fn server_rejects_bad_token_test() {
  let subject = start_hub_with_session("session-bad-token")
  let #(server_handle, control_file) =
    start_server_for_hub(subject, "good-token")
  let bad_control_file = file.ControlFile(..control_file, token: "bad-token")

  let assert Error(client.RequestFailed(code, _)) =
    client.ping(bad_control_file)
  assert code == "unauthorized"

  server.stop(server_handle)
  hub.stop(subject)
}

pub fn server_applies_authenticated_mutating_command_test() {
  let subject = start_hub_with_session("session-command")
  let command_subject = process.new_subject()
  let #(server_handle, control_file) =
    start_server_for_backend(
      backend_with_command(subject, command_subject),
      "token",
      500,
    )

  let assert Ok(result) =
    client.apply_command(control_file, command.PauseDispatch)
  assert result.command == "pause"
  assert command.status_to_string(result.status) == "applied"
  let assert Ok(command.PauseDispatch) =
    process.receive(command_subject, within: 1000)

  server.stop(server_handle)
  hub.stop(subject)
}

pub fn server_rejects_bad_token_before_mutating_backend_test() {
  let subject = start_hub_with_session("session-command-bad-token")
  let command_subject = process.new_subject()
  let #(server_handle, control_file) =
    start_server_for_backend(
      backend_with_command(subject, command_subject),
      "good-token",
      500,
    )
  let bad_control_file = file.ControlFile(..control_file, token: "bad-token")

  let assert Error(client.RequestFailed(code, _)) =
    client.apply_command(bad_control_file, command.PauseDispatch)
  assert code == "unauthorized"
  let assert Error(Nil) = process.receive(command_subject, within: 100)

  server.stop(server_handle)
  hub.stop(subject)
}

pub fn server_times_out_slow_mutating_backend_test() {
  let subject = start_hub_with_session("session-command-timeout")
  let backend =
    server.Backend(
      ..server.event_hub_store(subject),
      apply_command: fn(operator_command, _) {
        let _ = operator_command
        process.sleep(200)
        Ok(command.applied(command.PauseDispatch, None))
      },
    )
  let #(server_handle, control_file) =
    start_server_for_backend(backend, "token", 20)

  let assert Error(client.RequestFailed(code, _)) =
    client.apply_command(control_file, command.PauseDispatch)
  assert code == "command_timeout"

  server.stop(server_handle)
  hub.stop(subject)
}

pub fn server_returns_structured_timeout_when_session_backend_does_not_reply_test() {
  let #(server_handle, control_file) =
    start_server_for_backend_with_event_timeout(
      slow_session_backend(200),
      "token",
      20,
      500,
    )

  assert client.ping(control_file) == Ok(Nil)
  let assert Ok(raw) =
    client.raw_request(control_file, protocol.ListSessions("list-timeout", ""))
  assert string.contains(raw, "\"ok\":false")
  assert string.contains(raw, "\"code\":\"session_backend_timeout\"")
  assert string.contains(raw, "control server is reachable")

  let assert Error(client.RequestFailed(list_code, list_message)) =
    client.list_sessions_snapshot(control_file)
  assert list_code == "session_backend_timeout"
  assert_session_backend_timeout_message(list_message)

  let assert Error(client.RequestFailed(session_code, session_message)) =
    client.get_session(control_file, "session-timeout")
  assert session_code == "session_backend_timeout"
  assert_session_backend_timeout_message(session_message)

  let assert Error(client.RequestFailed(events_code, events_message)) =
    client.get_events(control_file, "session-timeout", 0, 10)
  assert events_code == "session_backend_timeout"
  assert_session_backend_timeout_message(events_message)

  server.stop(server_handle)
}

pub fn server_returns_event_hub_timeout_code_for_actor_call_timeout_test() {
  let #(server_handle, control_file) =
    start_server_for_backend(event_hub_timeout_backend(), "token", 500)

  let assert Error(client.RequestFailed(code, message)) =
    client.list_sessions_snapshot(control_file)
  assert code == "event_hub_timeout"
  assert string.contains(message, "control server is reachable")
  assert string.contains(message, "EventHub")
  assert string.contains(message, "configured timeout")

  server.stop(server_handle)
}

pub fn server_lists_sessions_with_good_token_test() {
  let subject = start_hub_with_session("session-list")
  let #(server_handle, control_file) = start_server_for_hub(subject, "token")

  let assert Ok(sessions) = client.list_sessions(control_file)
  assert list.map(sessions, fn(summary) { summary.session_id })
    == ["session-list"]
  let assert Ok(snapshot) = client.list_sessions_snapshot(control_file)
  assert snapshot.now_ms == 100
  assert list.map(snapshot.sessions, fn(summary) { summary.session_id })
    == ["session-list"]
  let assert Ok(response) =
    client.request(control_file, protocol.ListSessions("request-1", ""))
  let assert Some(data) = response.data
  assert string.contains(json.to_string(data), "session-list")

  server.stop(server_handle)
  hub.stop(subject)
}

pub fn server_returns_event_page_test() {
  let subject = start_hub_with_session("session-events")
  hub.publish(subject, "session-events", payload("first"))
  hub.publish(subject, "session-events", payload("second"))
  let assert Ok(waited_page) =
    hub.events_after(subject, "session-events", 0, 10, 1000)
  assert list.length(waited_page.events) == 2

  let #(server_handle, control_file) = start_server_for_hub(subject, "token")
  let assert Ok(page) = client.get_events(control_file, "session-events", 0, 10)

  assert list.map(page.events, fn(stored_event) {
      event.name_to_string(stored_event.payload.name)
    })
    == ["first", "second"]
  assert page.truncated == False

  server.stop(server_handle)
  hub.stop(subject)
}

pub fn server_returns_large_event_page_test() {
  let subject = start_hub_with_session("session-large-events")
  let large_raw = string.repeat("x", times: 20_000)
  hub.publish(
    subject,
    "session-large-events",
    event.EventPayload(
      ..payload("large_raw_json"),
      kind: event.PiRaw,
      raw_json: Some(event.RedactedRawJson(value: large_raw, truncated: False)),
    ),
  )
  let assert Ok(waited_page) =
    hub.events_after(subject, "session-large-events", 0, 10, 1000)
  assert list.length(waited_page.events) == 1

  let #(server_handle, control_file) = start_server_for_hub(subject, "token")
  let assert Ok(page) =
    client.get_events(control_file, "session-large-events", 0, 10)

  let assert [stored_event] = page.events
  let assert Some(raw_json) = stored_event.payload.raw_json
  assert string.length(raw_json.value) == 20_000

  server.stop(server_handle)
  hub.stop(subject)
}

pub fn server_stream_closes_after_exited_session_replay_test() {
  let subject = start_hub_with_session("session-exited-stream")
  hub.publish(subject, "session-exited-stream", payload("only"))
  hub.finish_session(subject, "session-exited-stream", reason.Normal)
  let assert Ok(_) =
    hub.events_after(subject, "session-exited-stream", 0, 10, 1000)
  let #(server_handle, control_file) = start_server_for_hub(subject, "token")
  let event_subject = process.new_subject()

  let assert Ok(Nil) =
    client.stream_events(
      control_file,
      "session-exited-stream",
      0,
      fn(stored_event) {
        process.send(
          event_subject,
          event.name_to_string(stored_event.payload.name),
        )
        client.Continue
      },
    )
  let assert Ok("only") = process.receive(event_subject, within: 1000)

  server.stop(server_handle)
  hub.stop(subject)
}

pub fn server_streams_events_by_polling_after_cursor_test() {
  let subject = start_hub_with_session("session-stream")
  hub.publish(subject, "session-stream", payload("before"))
  let assert Ok(_) = hub.events_after(subject, "session-stream", 0, 10, 1000)
  let #(server_handle, control_file) = start_server_for_hub(subject, "token")
  let event_subject = process.new_subject()
  let done_subject = process.new_subject()

  let _ =
    process.spawn_unlinked(fn() {
      let result =
        client.stream_events(
          control_file,
          "session-stream",
          0,
          fn(stored_event) {
            let event_name = event.name_to_string(stored_event.payload.name)
            process.send(event_subject, event_name)
            case event_name == "after" {
              True -> client.Stop
              False -> client.Continue
            }
          },
        )
      process.send(done_subject, result)
    })

  let assert Ok("before") = process.receive(event_subject, within: 2000)
  hub.publish(subject, "session-stream", payload("after"))
  let assert Ok("after") = process.receive(event_subject, within: 2000)
  let assert Ok(Ok(Nil)) = process.receive(done_subject, within: 2000)
  assert client.ping(control_file) == Ok(Nil)

  server.stop(server_handle)
  hub.stop(subject)
}

pub fn server_stop_closes_active_stream_test() {
  let subject = start_hub_with_session("session-active-stream")
  hub.publish(subject, "session-active-stream", payload("initial"))
  let assert Ok(_) =
    hub.events_after(subject, "session-active-stream", 0, 10, 1000)
  let #(server_handle, control_file) = start_server_for_hub(subject, "token")
  let event_subject = process.new_subject()
  let done_subject = process.new_subject()

  let _ =
    process.spawn_unlinked(fn() {
      let result =
        client.stream_events(
          control_file,
          "session-active-stream",
          0,
          fn(stored_event) {
            process.send(
              event_subject,
              event.name_to_string(stored_event.payload.name),
            )
            client.Continue
          },
        )
      process.send(done_subject, result)
    })

  let assert Ok("initial") = process.receive(event_subject, within: 2000)
  server.stop(server_handle)
  let assert Ok(Ok(Nil)) = process.receive(done_subject, within: 2000)

  hub.stop(subject)
}
