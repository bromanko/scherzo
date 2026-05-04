import gleam/erlang/process
import gleam/int
import gleam/option.{type Option, None, Some}
import scherzo/control/command
import scherzo/control/protocol
import scherzo/session/event
import scherzo/session/hub

pub type Settings {
  Settings(
    host: String,
    port: Int,
    token: String,
    event_timeout_ms: Int,
    stream_poll_ms: Int,
    command_timeout_ms: Int,
  )
}

pub type Backend {
  Backend(
    list_sessions: fn(Int) -> Result(event.SessionList, hub.HubError),
    get_session: fn(String, Int) ->
      Result(Option(event.SessionSummary), hub.HubError),
    events_after: fn(String, Int, Int, Int) ->
      Result(event.EventPage, hub.HubError),
    apply_command: fn(command.OperatorCommand, Int) ->
      Result(command.CommandResult, Nil),
  )
}

pub opaque type Server {
  Server(listener: Listener, accept_pid: process.Pid, port: Int)
}

type Listener

type Socket

pub type ServerError {
  ServerStartFailed(message: String)
}

pub fn default_settings(token: String) -> Settings {
  Settings(
    host: "127.0.0.1",
    port: 0,
    token: token,
    event_timeout_ms: 500,
    stream_poll_ms: 100,
    command_timeout_ms: 500,
  )
}

pub fn event_hub_store(subject: process.Subject(hub.Message)) -> Backend {
  Backend(
    list_sessions: fn(timeout_ms) {
      hub.list_sessions_snapshot(subject, timeout_ms)
    },
    get_session: fn(session_id, timeout_ms) {
      hub.get_session(subject, session_id, timeout_ms)
    },
    events_after: fn(session_id, cursor, limit, timeout_ms) {
      hub.events_after(subject, session_id, cursor, limit, timeout_ms)
    },
    apply_command: fn(operator_command, _) {
      Ok(command.not_allowed(
        operator_command,
        "command_backend_unavailable",
        Some("mutating commands are not available"),
      ))
    },
  )
}

pub fn start(
  settings: Settings,
  store: Backend,
) -> Result(Server, ServerError) {
  case ffi_listen(settings.host, settings.port) {
    Error(message) -> Error(ServerStartFailed(message))
    Ok(listener) -> {
      let port = ffi_bound_port(listener)
      let accept_pid =
        process.spawn_unlinked(fn() {
          process.trap_exits(True)
          accept_loop(listener, settings, store)
        })
      Ok(Server(listener: listener, accept_pid: accept_pid, port: port))
    }
  }
}

pub fn bound_port(server: Server) -> Int {
  server.port
}

pub fn stop(server: Server) -> Nil {
  process.kill(server.accept_pid)
  ffi_close_listener(server.listener)
}

fn accept_loop(listener: Listener, settings: Settings, store: Backend) -> Nil {
  drain_trapped_exits()
  case ffi_accept(listener) {
    Ok(socket) -> {
      let _ = process.spawn(fn() { handle_connection(socket, settings, store) })
      accept_loop(listener, settings, store)
    }
    Error(_) -> Nil
  }
}

fn drain_trapped_exits() -> Nil {
  let selector =
    process.new_selector()
    |> process.select_trapped_exits(fn(_) { Nil })
  case process.selector_receive(selector, within: 0) {
    Ok(Nil) -> drain_trapped_exits()
    Error(_) -> Nil
  }
}

fn handle_connection(
  socket: Socket,
  settings: Settings,
  store: Backend,
) -> Nil {
  case ffi_recv_line(socket, 5000) {
    Error(_) -> close_socket(socket)
    Ok(line) ->
      case protocol.decode_request(line) {
        Error(err) -> {
          let _ = send_response(socket, protocol.request_error_response(err))
          close_socket(socket)
        }
        Ok(request) ->
          case protocol.request_token(request) == settings.token {
            False -> {
              let _ =
                send_response(
                  socket,
                  protocol.error_response(
                    protocol.request_id(request),
                    "unauthorized",
                    "invalid control token",
                  ),
                )
              close_socket(socket)
            }
            True -> handle_authorized_request(socket, settings, store, request)
          }
      }
  }
}

fn handle_authorized_request(
  socket: Socket,
  settings: Settings,
  store: Backend,
  request: protocol.Request,
) -> Nil {
  case request {
    protocol.Ping(id, _) -> {
      let _ =
        send_response(
          socket,
          protocol.success_response(id, protocol.ping_data()),
        )
      close_socket(socket)
    }
    protocol.ListSessions(id, _) -> {
      let response = case
        call_session_backend(store.list_sessions, settings.event_timeout_ms)
      {
        Ok(snapshot) ->
          protocol.success_response(id, protocol.list_sessions_data(snapshot))
        Error(err) -> error_for_session_backend(id, err)
      }
      let _ = send_response(socket, response)
      close_socket(socket)
    }
    protocol.GetSession(id, _, session_id) -> {
      let response = case
        call_session_backend(
          fn(timeout_ms) { store.get_session(session_id, timeout_ms) },
          settings.event_timeout_ms,
        )
      {
        Ok(summary) ->
          protocol.success_response(id, protocol.session_data(summary))
        Error(err) -> error_for_session_backend(id, err)
      }
      let _ = send_response(socket, response)
      close_socket(socket)
    }
    protocol.GetEvents(id, _, session_id, after, limit) -> {
      let response = case
        call_session_backend(
          fn(timeout_ms) {
            store.events_after(session_id, after, limit, timeout_ms)
          },
          settings.event_timeout_ms,
        )
      {
        Ok(page) ->
          protocol.success_response(id, protocol.event_page_data(page))
        Error(err) -> error_for_session_backend(id, err)
      }
      let _ = send_response(socket, response)
      close_socket(socket)
    }
    protocol.StreamEvents(id, _, session_id, after) ->
      start_stream(socket, settings, store, id, session_id, after)
    _ -> handle_command_request(socket, settings, store, request)
  }
}

fn handle_command_request(
  socket: Socket,
  settings: Settings,
  store: Backend,
  request: protocol.Request,
) -> Nil {
  let id = protocol.request_id(request)
  let response = case protocol.request_operator_command(request) {
    Some(operator_command) ->
      case
        call_command_backend(
          store,
          operator_command,
          settings.command_timeout_ms,
        )
      {
        Ok(result) ->
          protocol.success_response(id, protocol.command_result_data(result))
        Error(Nil) ->
          protocol.error_response(
            id,
            "command_timeout",
            "operator command timed out",
          )
      }
    None ->
      protocol.error_response(id, "invalid_request", "not a command request")
  }
  let _ = send_response(socket, response)
  close_socket(socket)
}

type SessionBackendError {
  SessionBackendTimeout
  SessionBackendHubError(hub.HubError)
}

fn call_session_backend(
  operation: fn(Int) -> Result(a, hub.HubError),
  timeout_ms: Int,
) -> Result(a, SessionBackendError) {
  let reply = process.new_subject()
  let operation_timeout_ms = backend_operation_timeout(timeout_ms)
  let _ =
    process.spawn_unlinked(fn() {
      process.send(reply, operation(operation_timeout_ms))
    })
  case process.receive(reply, within: timeout_ms) {
    Ok(Ok(value)) -> Ok(value)
    Ok(Error(err)) -> Error(SessionBackendHubError(err))
    Error(Nil) -> Error(SessionBackendTimeout)
  }
}

fn backend_operation_timeout(timeout_ms: Int) -> Int {
  case timeout_ms > 25 {
    True -> timeout_ms - 25
    False -> max_int(timeout_ms / 2, 1)
  }
}

fn call_command_backend(
  store: Backend,
  operator_command: command.OperatorCommand,
  timeout_ms: Int,
) -> Result(command.CommandResult, Nil) {
  let reply = process.new_subject()
  let _ =
    process.spawn_unlinked(fn() {
      process.send(reply, store.apply_command(operator_command, timeout_ms))
    })
  case process.receive(reply, within: timeout_ms) {
    Ok(Ok(result)) -> Ok(result)
    Ok(Error(Nil)) | Error(Nil) -> Error(Nil)
  }
}

fn start_stream(
  socket: Socket,
  settings: Settings,
  store: Backend,
  id: String,
  session_id: String,
  after: Int,
) -> Nil {
  case store.get_session(session_id, settings.event_timeout_ms) {
    Ok(Some(_)) -> {
      case
        send_response(
          socket,
          protocol.success_response(
            id,
            protocol.stream_started_data(session_id, after),
          ),
        )
      {
        Ok(Nil) -> stream_loop(socket, settings, store, id, session_id, after)
        Error(_) -> close_socket(socket)
      }
    }
    Ok(None) -> {
      let _ =
        send_response(
          socket,
          protocol.error_response(
            id,
            "missing_session",
            "session not found: " <> session_id,
          ),
        )
      close_socket(socket)
    }
    Error(err) -> {
      let _ = send_response(socket, error_for_hub(id, err))
      close_socket(socket)
    }
  }
}

fn stream_loop(
  socket: Socket,
  settings: Settings,
  store: Backend,
  id: String,
  session_id: String,
  cursor: Int,
) -> Nil {
  case store.events_after(session_id, cursor, 50, settings.event_timeout_ms) {
    Error(err) -> {
      let _ = send_response(socket, error_for_hub(id, err))
      close_socket(socket)
    }
    Ok(page) ->
      case send_stream_events(socket, id, page.events, cursor) {
        Error(_) -> close_socket(socket)
        Ok(next_cursor) -> {
          case
            stream_should_close(
              store,
              session_id,
              settings.event_timeout_ms,
              page,
            )
          {
            True -> close_socket(socket)
            False -> {
              process.sleep(settings.stream_poll_ms)
              stream_loop(socket, settings, store, id, session_id, next_cursor)
            }
          }
        }
      }
  }
}

fn stream_should_close(
  store: Backend,
  session_id: String,
  timeout_ms: Int,
  page: event.EventPage,
) -> Bool {
  case page.events {
    [_, ..] -> False
    [] ->
      case store.get_session(session_id, timeout_ms) {
        Ok(Some(summary)) ->
          case summary.status {
            event.Exited(_) -> True
            _ -> False
          }
        _ -> True
      }
  }
}

fn send_stream_events(
  socket: Socket,
  id: String,
  events: List(event.SessionEvent),
  cursor: Int,
) -> Result(Int, String) {
  case events {
    [] -> Ok(cursor)
    [stored_event, ..rest] ->
      case
        ffi_send_line(
          socket,
          protocol.stream_event_to_string(id, stored_event),
          5000,
        )
      {
        Ok(Nil) -> send_stream_events(socket, id, rest, stored_event.cursor)
        Error(message) -> Error(message)
      }
  }
}

fn error_for_session_backend(
  id: String,
  err: SessionBackendError,
) -> protocol.Response {
  case err {
    SessionBackendTimeout ->
      protocol.error_response(
        id,
        "session_backend_timeout",
        session_backend_timeout_message(),
      )
    SessionBackendHubError(err) -> error_for_hub(id, err)
  }
}

fn error_for_hub(id: String, err: hub.HubError) -> protocol.Response {
  case err {
    hub.SessionNotFound(session_id) ->
      protocol.error_response(
        id,
        "missing_session",
        "session not found: " <> session_id,
      )
    hub.InvalidLimit(limit) ->
      protocol.error_response(
        id,
        "invalid_limit",
        "invalid event limit: " <> int.to_string(limit),
      )
    hub.HubUnavailable ->
      protocol.error_response(
        id,
        "event_hub_unavailable",
        "event hub unavailable",
      )
    hub.ActorCallTimeout ->
      protocol.error_response(
        id,
        "event_hub_timeout",
        event_hub_timeout_message(),
      )
  }
}

fn session_backend_timeout_message() -> String {
  "control server is reachable, but the session backend did not answer within the configured timeout"
}

fn event_hub_timeout_message() -> String {
  "control server is reachable, but the EventHub did not answer within the configured timeout"
}

fn max_int(a: Int, b: Int) -> Int {
  case a > b {
    True -> a
    False -> b
  }
}

fn send_response(
  socket: Socket,
  response: protocol.Response,
) -> Result(Nil, String) {
  ffi_send_line(socket, protocol.response_to_string(response), 5000)
}

fn close_socket(socket: Socket) -> Nil {
  ffi_close_socket(socket)
}

@external(erlang, "scherzo_control_ffi", "listen")
fn ffi_listen(host: String, port: Int) -> Result(Listener, String)

@external(erlang, "scherzo_control_ffi", "accept")
fn ffi_accept(listener: Listener) -> Result(Socket, String)

@external(erlang, "scherzo_control_ffi", "send_line")
fn ffi_send_line(
  socket: Socket,
  line: String,
  timeout_ms: Int,
) -> Result(Nil, String)

@external(erlang, "scherzo_control_ffi", "recv_line")
fn ffi_recv_line(socket: Socket, timeout_ms: Int) -> Result(String, String)

@external(erlang, "scherzo_control_ffi", "close_socket")
fn ffi_close_socket(socket: Socket) -> Nil

@external(erlang, "scherzo_control_ffi", "close_listener")
fn ffi_close_listener(listener: Listener) -> Nil

@external(erlang, "scherzo_control_ffi", "bound_port")
fn ffi_bound_port(listener: Listener) -> Int
