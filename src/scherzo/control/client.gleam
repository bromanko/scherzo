import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/control/command
import scherzo/control/file
import scherzo/control/protocol
import scherzo/control/query/codec as query_codec
import scherzo/control/query/types as query_types
import scherzo/session/event
import scherzo/turn_telemetry

pub type StreamAction {
  Continue
  Stop
}

pub type ControlTransportError {
  NonLoopbackHostRejected(host: String)
  Timeout
  Closed
  LineTooLong(max_bytes: Int)
  SendFailed(reason: String)
  ReceiveFailed(reason: String)
  ConnectFailed(reason: String)
  UnexpectedFfiFailure(function: String, detail: String)
}

pub type ControlError {
  ConnectionFailed(error: ControlTransportError)
  RequestFailed(code: String, message: String)
  ProtocolFailed(message: String)
}

pub type ControlTarget {
  ControlTarget(control_path: String, control_file: file.ControlFile)
}

type Socket

const request_transport_timeout_ms = 5000

const operator_command_response_timeout_ms = 30_000

pub fn discover_target(
  explicit_path: Option(String),
  env: fn(String) -> Option(String),
) -> Result(ControlTarget, file.ControlFileError) {
  case file.discover_path(explicit_path, env) {
    Error(err) -> Error(err)
    Ok(control_path) ->
      case file.read(control_path) {
        Error(err) -> Error(err)
        Ok(control_file) ->
          Ok(ControlTarget(
            control_path: control_path,
            control_file: control_file,
          ))
      }
  }
}

pub fn target_response_line(line: String, target: ControlTarget) -> String {
  case protocol.decode_response(line) {
    Ok(response) ->
      protocol.response_to_json_with_fields(response, [
        #("target", target_to_json(target)),
      ])
      |> json.to_string
    Error(decode_error) -> {
      let _target_annotation_decode_error = decode_error
      line
    }
  }
}

fn target_to_json(target: ControlTarget) -> json.Json {
  json.object([
    #("control_file_path", json.string(target.control_path)),
    #("workspace_root", json.string(target.control_file.workspace_root)),
    #("host", json.string(target.control_file.host)),
    #("port", json.int(target.control_file.port)),
  ])
}

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
  let timeout_ms = request_response_timeout_ms(request)
  let result = case
    send_line(
      socket,
      protocol.request_to_string(request),
      request_transport_timeout_ms,
    )
  {
    Error(error) -> Error(ConnectionFailed(error))
    Ok(Nil) ->
      case recv_line(socket, timeout_ms) {
        Ok(line) -> Ok(line)
        Error(error) -> Error(ConnectionFailed(error))
      }
  }
  // The response (or send/receive error) is already determined; close is a
  // best-effort transport cleanup and must not mask the request result.
  let _best_effort_socket_close_result = ffi_close_socket(socket)
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

pub fn query(
  control_file: file.ControlFile,
  query: query_types.QueryRequest,
) -> Result(query_types.QueryResponse, ControlError) {
  use line <- try_control(raw_request(
    control_file,
    protocol.query_request("1", "", query),
  ))
  case protocol.decode_response(line) {
    Error(error) -> Error(RequestFailed(error.code, error.message))
    Ok(response) ->
      case response.ok, response.data {
        False, _ -> Error(response_error(response))
        True, Some(data) ->
          case query_codec.decode_response(json.to_string(data)) {
            Ok(query_response) -> Ok(query_response)
            Error(query_types.QueryError(code: code, message: message)) ->
              Error(RequestFailed(
                query_types.error_code_to_string(code),
                message,
              ))
          }
        True, None -> Error(ProtocolFailed("missing query response payload"))
      }
  }
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
  case
    send_line(
      socket,
      protocol.request_to_string(request),
      request_transport_timeout_ms,
    )
  {
    Error(error) -> {
      ffi_close_socket(socket)
      Error(ConnectionFailed(error))
    }
    Ok(Nil) ->
      case recv_line(socket, request_transport_timeout_ms) {
        Error(error) -> {
          ffi_close_socket(socket)
          Error(ConnectionFailed(error))
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
  case recv_line(socket, 1000) {
    Error(Timeout) -> stream_loop(socket, on_event)
    Error(Closed) -> {
      ffi_close_socket(socket)
      Ok(Nil)
    }
    Error(error) -> {
      ffi_close_socket(socket)
      Error(ConnectionFailed(error))
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

fn request_response_timeout_ms(request: protocol.Request) -> Int {
  case protocol.request_operator_command(request) {
    Some(_) -> operator_command_response_timeout_ms
    None -> request_transport_timeout_ms
  }
}

fn connect(control_file: file.ControlFile) -> Result(Socket, ControlError) {
  ffi_connect(
    control_file.host,
    control_file.port,
    request_transport_timeout_ms,
  )
  |> result.map_error(fn(error) {
    ConnectionFailed(raw_connect_error(control_file.host, error))
  })
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
    protocol.Query(id, _, query) ->
      protocol.Query(id, control_file.token, query)
    protocol.Pause(id, _) -> protocol.Pause(id, control_file.token)
    protocol.Resume(id, _) -> protocol.Resume(id, control_file.token)
    protocol.ReloadWorkflow(id, _) ->
      protocol.ReloadWorkflow(id, control_file.token)
    protocol.RetryIssue(id, _, issue_ref) ->
      protocol.RetryIssue(id, control_file.token, issue_ref)
    protocol.RetryWorkflowStep(id, _, target, step_id) ->
      protocol.RetryWorkflowStep(id, control_file.token, target, step_id)
    protocol.ParkIssue(id, _, issue_ref, reason) ->
      protocol.ParkIssue(id, control_file.token, issue_ref, reason)
    protocol.UnparkIssue(id, _, issue_ref) ->
      protocol.UnparkIssue(id, control_file.token, issue_ref)
    protocol.AbortSession(id, _, session_id) ->
      protocol.AbortSession(id, control_file.token, session_id)
    protocol.StopAfterCurrentTurn(id, _, session_id) ->
      protocol.StopAfterCurrentTurn(id, control_file.token, session_id)
    protocol.CleanupOrphanSteps(id, _, run_id, dry_run) ->
      protocol.CleanupOrphanSteps(id, control_file.token, run_id, dry_run)
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
    protocol.RunScheduleNow(id, _, job_id) ->
      protocol.RunScheduleNow(id, control_file.token, job_id)
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
    ConnectionFailed(error) -> transport_error_message(error)
    RequestFailed(_, message) -> message
    ProtocolFailed(message) -> message
  }
}

pub fn compact_event_line(stored_event: event.SessionEvent) -> String {
  case stored_event.payload.kind {
    event.Turn -> compact_turn_event_line(stored_event)
    _ ->
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
}

fn compact_turn_event_line(stored_event: event.SessionEvent) -> String {
  let payload = stored_event.payload
  "cursor="
  <> int.to_string(stored_event.cursor)
  <> " at_ms="
  <> int.to_string(stored_event.at_ms)
  <> " session="
  <> stored_event.session_id
  <> " kind=turn name="
  <> event.name_to_string(payload.name)
  <> compact_turn_field(payload.turn)
  <> compact_turn_status_field(payload.turn_status)
  <> compact_duration_field(payload.turn_duration_ms)
  <> compact_token_delta_field(payload.token_delta.total)
  <> compact_reason_field(payload.reason)
}

fn compact_turn_field(turn: Option(Int)) -> String {
  case turn {
    Some(turn) -> " turn=" <> int.to_string(turn)
    None -> ""
  }
}

fn compact_turn_status_field(
  status: Option(turn_telemetry.TurnStatus),
) -> String {
  case status {
    Some(status) -> " turn_status=" <> turn_telemetry.status_to_string(status)
    None -> ""
  }
}

fn compact_duration_field(duration: Option(Int)) -> String {
  case duration {
    Some(duration) -> " duration_ms=" <> int.to_string(duration)
    None -> ""
  }
}

fn compact_token_delta_field(total: Int) -> String {
  case total > 0 {
    True -> " token_delta_total=" <> int.to_string(total)
    False -> ""
  }
}

fn compact_reason_field(reason: Option(turn_telemetry.TurnReason)) -> String {
  case reason {
    Some(reason) -> " reason=" <> turn_telemetry.reason_to_string(reason)
    None -> ""
  }
}

fn compact_message(message: Option(String)) -> String {
  case message {
    Some(value) -> " " <> string.replace(value, each: "\n", with: " ")
    None -> ""
  }
}

fn send_line(
  socket: Socket,
  line: String,
  timeout_ms: Int,
) -> Result(Nil, ControlTransportError) {
  ffi_send_line(socket, line, timeout_ms)
  |> result.map_error(fn(error) { raw_transport_error("send_line", error) })
}

fn recv_line(
  socket: Socket,
  timeout_ms: Int,
) -> Result(String, ControlTransportError) {
  ffi_recv_line(socket, timeout_ms)
  |> result.map_error(fn(error) { raw_transport_error("recv_line", error) })
}

fn raw_connect_error(host: String, error: String) -> ControlTransportError {
  case error {
    "non_loopback_host_rejected" -> NonLoopbackHostRejected(host)
    _ -> raw_transport_error("connect", error)
  }
}

fn raw_transport_error(
  function: String,
  error: String,
) -> ControlTransportError {
  case error {
    "non_loopback_host_rejected" -> NonLoopbackHostRejected("")
    "timeout" -> Timeout
    "closed" -> Closed
    "line_too_long" -> LineTooLong(8_388_608)
    _ ->
      case function {
        "connect" -> ConnectFailed(error)
        "send_line" -> SendFailed(error)
        "recv_line" -> ReceiveFailed(error)
        _ -> UnexpectedFfiFailure(function, error)
      }
  }
}

fn transport_error_message(error: ControlTransportError) -> String {
  case error {
    NonLoopbackHostRejected(host) -> "non-loopback host rejected: " <> host
    Timeout -> "timeout"
    Closed -> "closed"
    LineTooLong(max_bytes) ->
      "line too long (max " <> int.to_string(max_bytes) <> " bytes)"
    SendFailed(reason) -> reason
    ReceiveFailed(reason) -> reason
    ConnectFailed(reason) -> reason
    UnexpectedFfiFailure(function, detail) ->
      function <> " failed unexpectedly: " <> detail
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
