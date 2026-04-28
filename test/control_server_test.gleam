import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/control/client
import scherzo/control/file
import scherzo/control/protocol
import scherzo/control/server
import scherzo/domain
import scherzo/session/event
import scherzo/session/hub

fn summary(session_id: String) -> event.SessionSummary {
  event.SessionSummary(
    session_id: session_id,
    issue_id: "issue-1",
    issue_identifier: "ABC-1",
    issue_title: "Title ABC-1",
    workspace_path: "test/tmp/control-server/workspaces/ABC-1",
    pi_session_id: None,
    status: event.Running,
    current_turn: 1,
    started_at_ms: 10,
    last_event_at_ms: 10,
    token_totals: domain.zero_token_totals(),
  )
}

fn payload(name: String) -> event.EventPayload {
  event.EventPayload(
    kind: event.Lifecycle,
    name: name,
    turn: None,
    pi_type: None,
    message: None,
    request_id: None,
    method: None,
    tool_name: None,
    tokens: domain.zero_token_totals(),
    raw_json: None,
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
  let assert Ok(server_handle) =
    server.start(
      server.Settings(
        host: "127.0.0.1",
        port: 0,
        token: token,
        event_timeout_ms: 500,
        stream_poll_ms: 20,
      ),
      server.event_hub_store(subject),
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

pub fn server_lists_sessions_with_good_token_test() {
  let subject = start_hub_with_session("session-list")
  let #(server_handle, control_file) = start_server_for_hub(subject, "token")

  let assert Ok(sessions) = client.list_sessions(control_file)
  assert list.map(sessions, fn(summary) { summary.session_id })
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

  assert list.map(page.events, fn(stored_event) { stored_event.payload.name })
    == ["first", "second"]
  assert page.truncated == False

  server.stop(server_handle)
  hub.stop(subject)
}

pub fn server_stream_closes_after_exited_session_replay_test() {
  let subject = start_hub_with_session("session-exited-stream")
  hub.publish(subject, "session-exited-stream", payload("only"))
  hub.finish_session(subject, "session-exited-stream", "normal")
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
        process.send(event_subject, stored_event.payload.name)
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
            process.send(event_subject, stored_event.payload.name)
            case stored_event.payload.name == "after" {
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
            process.send(event_subject, stored_event.payload.name)
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
