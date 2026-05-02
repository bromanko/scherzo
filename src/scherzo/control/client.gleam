import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/control/command
import scherzo/control/file
import scherzo/control/protocol
import scherzo/session/event

pub type StreamAction {
  Continue
  Stop
}

pub type ControlError {
  ConnectionFailed(message: String)
  RequestFailed(code: String, message: String)
  ProtocolFailed(message: String)
}

type Socket

pub fn request(
  control_file: file.ControlFile,
  request: protocol.Request,
) -> Result(protocol.Response, ControlError) {
  use line <- try_control(raw_request(control_file, request))
  protocol.decode_response(line) |> map_error_body
}

pub fn raw_request(
  control_file: file.ControlFile,
  request: protocol.Request,
) -> Result(String, ControlError) {
  use socket <- try_control(connect(control_file))
  let request = authenticate(control_file, request)
  let result = case
    ffi_send_line(socket, protocol.request_to_string(request), 5000)
  {
    Error(message) -> Error(ConnectionFailed(message))
    Ok(Nil) ->
      case ffi_recv_line(socket, 5000) {
        Ok(line) -> Ok(line)
        Error(message) -> Error(ConnectionFailed(message))
      }
  }
  ffi_close_socket(socket)
  result
}

pub fn ping(control_file: file.ControlFile) -> Result(Nil, ControlError) {
  use line <- try_control(raw_request(control_file, protocol.Ping("1", "")))
  protocol.decode_ping_response(line) |> map_error_body
}

pub fn list_sessions(
  control_file: file.ControlFile,
) -> Result(List(event.SessionSummary), ControlError) {
  use snapshot <- try_control(list_sessions_snapshot(control_file))
  Ok(snapshot.sessions)
}

pub fn list_sessions_snapshot(
  control_file: file.ControlFile,
) -> Result(event.SessionList, ControlError) {
  use line <- try_control(raw_request(
    control_file,
    protocol.ListSessions("1", ""),
  ))
  protocol.decode_list_sessions_snapshot_response(line) |> map_error_body
}

pub fn get_session(
  control_file: file.ControlFile,
  session_id: String,
) -> Result(Option(event.SessionSummary), ControlError) {
  use line <- try_control(raw_request(
    control_file,
    protocol.GetSession("1", "", session_id),
  ))
  protocol.decode_get_session_response(line) |> map_error_body
}

pub fn get_events(
  control_file: file.ControlFile,
  session_id: String,
  after: Int,
  limit: Int,
) -> Result(event.EventPage, ControlError) {
  use line <- try_control(raw_request(
    control_file,
    protocol.GetEvents("1", "", session_id, after, limit),
  ))
  protocol.decode_get_events_response(line) |> map_error_body
}

pub fn apply_command(
  control_file: file.ControlFile,
  operator_command: command.OperatorCommand,
) -> Result(command.CommandResult, ControlError) {
  use line <- try_control(raw_request(
    control_file,
    protocol.command_request("1", "", operator_command),
  ))
  protocol.decode_command_result_response(line) |> map_error_body
}

pub fn stream_events(
  control_file: file.ControlFile,
  session_id: String,
  after: Int,
  on_event: fn(event.SessionEvent) -> StreamAction,
) -> Result(Nil, ControlError) {
  use socket <- try_control(connect(control_file))
  let request =
    protocol.StreamEvents("1", control_file.token, session_id, after)
  case ffi_send_line(socket, protocol.request_to_string(request), 5000) {
    Error(message) -> {
      ffi_close_socket(socket)
      Error(ConnectionFailed(message))
    }
    Ok(Nil) ->
      case ffi_recv_line(socket, 5000) {
        Error(message) -> {
          ffi_close_socket(socket)
          Error(ConnectionFailed(message))
        }
        Ok(line) ->
          case protocol.decode_response(line) {
            Error(error) -> {
              ffi_close_socket(socket)
              Error(RequestFailed(error.code, error.message))
            }
            Ok(response) ->
              case response.ok {
                False -> {
                  ffi_close_socket(socket)
                  Error(response_error(response))
                }
                True -> stream_loop(socket, on_event)
              }
          }
      }
  }
}

fn stream_loop(
  socket: Socket,
  on_event: fn(event.SessionEvent) -> StreamAction,
) -> Result(Nil, ControlError) {
  case ffi_recv_line(socket, 1000) {
    Error("timeout") -> stream_loop(socket, on_event)
    Error("closed") -> Ok(Nil)
    Error(message) -> {
      ffi_close_socket(socket)
      Error(ConnectionFailed(message))
    }
    Ok(line) ->
      case protocol.decode_stream_event(line) {
        Error(error) -> {
          ffi_close_socket(socket)
          Error(RequestFailed(error.code, error.message))
        }
        Ok(stored_event) ->
          case on_event(stored_event) {
            Continue -> stream_loop(socket, on_event)
            Stop -> {
              ffi_close_socket(socket)
              Ok(Nil)
            }
          }
      }
  }
}

fn connect(control_file: file.ControlFile) -> Result(Socket, ControlError) {
  ffi_connect(control_file.host, control_file.port, 5000)
  |> result_map_error_connection
}

fn authenticate(
  control_file: file.ControlFile,
  request: protocol.Request,
) -> protocol.Request {
  case request {
    protocol.Ping(id, _) -> protocol.Ping(id, control_file.token)
    protocol.ListSessions(id, _) ->
      protocol.ListSessions(id, control_file.token)
    protocol.GetSession(id, _, session_id) ->
      protocol.GetSession(id, control_file.token, session_id)
    protocol.GetEvents(id, _, session_id, after, limit) ->
      protocol.GetEvents(id, control_file.token, session_id, after, limit)
    protocol.StreamEvents(id, _, session_id, after) ->
      protocol.StreamEvents(id, control_file.token, session_id, after)
    protocol.Pause(id, _) -> protocol.Pause(id, control_file.token)
    protocol.Resume(id, _) -> protocol.Resume(id, control_file.token)
    protocol.ReloadWorkflow(id, _) ->
      protocol.ReloadWorkflow(id, control_file.token)
    protocol.RetryIssue(id, _, issue_ref) ->
      protocol.RetryIssue(id, control_file.token, issue_ref)
    protocol.ParkIssue(id, _, issue_ref, reason) ->
      protocol.ParkIssue(id, control_file.token, issue_ref, reason)
    protocol.UnparkIssue(id, _, issue_ref) ->
      protocol.UnparkIssue(id, control_file.token, issue_ref)
    protocol.AbortSession(id, _, session_id) ->
      protocol.AbortSession(id, control_file.token, session_id)
    protocol.StopAfterCurrentTurn(id, _, session_id) ->
      protocol.StopAfterCurrentTurn(id, control_file.token, session_id)
    protocol.PromptSession(id, _, session_id, message) ->
      protocol.PromptSession(id, control_file.token, session_id, message)
    protocol.RespondUi(id, _, session_id, request_id, response) ->
      protocol.RespondUi(
        id,
        control_file.token,
        session_id,
        request_id,
        response,
      )
  }
}

fn response_error(response: protocol.Response) -> ControlError {
  case response.error {
    Some(error) -> RequestFailed(error.code, error.message)
    None -> RequestFailed("unknown_error", "request failed")
  }
}

fn map_error_body(
  result: Result(a, protocol.ErrorBody),
) -> Result(a, ControlError) {
  case result {
    Ok(value) -> Ok(value)
    Error(error) -> Error(RequestFailed(error.code, error.message))
  }
}

fn result_map_error_connection(
  result: Result(Socket, String),
) -> Result(Socket, ControlError) {
  case result {
    Ok(socket) -> Ok(socket)
    Error(message) -> Error(ConnectionFailed(message))
  }
}

fn try_control(
  result: Result(a, ControlError),
  next: fn(a) -> Result(b, ControlError),
) -> Result(b, ControlError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}

pub fn error_code(error: ControlError) -> String {
  case error {
    ConnectionFailed(_) -> "connection_failed"
    RequestFailed(code, _) -> code
    ProtocolFailed(_) -> "protocol_failed"
  }
}

pub fn error_message(error: ControlError) -> String {
  case error {
    ConnectionFailed(message) -> message
    RequestFailed(_, message) -> message
    ProtocolFailed(message) -> message
  }
}

pub fn compact_event_line(stored_event: event.SessionEvent) -> String {
  int.to_string(stored_event.cursor)
  <> " "
  <> int.to_string(stored_event.at_ms)
  <> " "
  <> stored_event.session_id
  <> " "
  <> event.kind_to_string(stored_event.payload.kind)
  <> " "
  <> event.name_to_string(stored_event.payload.name)
  <> compact_message(stored_event.payload.message)
}

fn compact_message(message: Option(String)) -> String {
  case message {
    Some(value) -> " " <> string.replace(value, each: "\n", with: " ")
    None -> ""
  }
}

@external(erlang, "scherzo_control_ffi", "connect")
fn ffi_connect(
  host: String,
  port: Int,
  timeout_ms: Int,
) -> Result(Socket, String)

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
