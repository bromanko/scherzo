import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/control/command
import scherzo/control/defaults
import scherzo/control/file
import scherzo/control/protocol
import scherzo/control/query/codec as query_codec
import scherzo/control/query/types as query_types
import scherzo/control/timeout_policy
import scherzo/ctl/timeout_settings
import scherzo/session/event
import scherzo/turn_telemetry

pub type StreamAction {
  Continue
  Stop
}

pub type ControlTransportError {
  NonLoopbackHostRejected(host: String)
  Timeout
  ConnectTimeout
  SendTimeout
  ReceiveTimeout
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

pub fn operator_command_response_timeout_ms(
  control_file: file.ControlFile,
) -> Int {
  control_file.command_timeout_ms + defaults.command_response_timeout_grace_ms
}

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
      case query_timeout_response_line(response, target) {
        Some(line) -> line
        None ->
          protocol.response_to_json_with_fields(response, [
            #("target", target_to_json(target)),
          ])
          |> json.to_string
      }
    Error(decode_error) ->
      case string.starts_with(string.trim(line), "{") {
        True -> line
        False -> bad_response_line(target, decode_error)
      }
  }
}

fn query_timeout_response_line(
  response: protocol.Response,
  target: ControlTarget,
) -> Option(String) {
  case response.ok, response.data {
    True, Some(data) ->
      case query_codec.decode_response(json.to_string(data)) {
        Error(query_types.QueryError(
          code: query_types.QueryTimeout,
          message: message,
        )) ->
          Some(
            json.object([
              #("version", json.int(protocol.version)),
              #("id", json.string(response.id)),
              #("ok", json.bool(False)),
              #("target", target_to_json(target)),
              #(
                "error",
                timeout_policy.error_json(timeout_policy.TimeoutError(
                  phase: timeout_policy.DaemonActorQuery,
                  timeout_ms: timeout_settings.current_timeout_ms(),
                  accepted: timeout_policy.AcceptedFalse,
                  retryable: True,
                  message: message,
                  suggested_next_command: None,
                )),
              ),
            ])
            |> json.to_string,
          )
        _ -> None
      }
    _, _ -> None
  }
}

fn bad_response_line(
  target: ControlTarget,
  error: protocol.ErrorBody,
) -> String {
  let protocol.ErrorBody(code: code, message: message) = error
  json.object([
    #("version", json.int(protocol.version)),
    #("id", json.string("1")),
    #("ok", json.bool(False)),
    #("target", target_to_json(target)),
    #(
      "error",
      json.object([
        #("code", json.string("bad_response")),
        #(
          "phase",
          json.string(timeout_policy.phase_string(
            timeout_policy.RequestRoundTrip,
          )),
        ),
        #("timeout_ms", json.int(timeout_settings.current_timeout_ms())),
        #("accepted", json.string("unknown")),
        #("retryable", json.bool(False)),
        #(
          "message",
          json.string("Daemon returned an invalid control response: " <> code),
        ),
        #("detail", json.string(message)),
      ]),
    ),
  ])
  |> json.to_string
}

pub fn target_to_json(target: ControlTarget) -> json.Json {
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
  raw_request_with_response_timeout(
    control_file,
    request,
    request_response_timeout_ms(control_file, request),
  )
}

fn raw_request_with_response_timeout(
  control_file: file.ControlFile,
  request: protocol.Request,
  response_timeout_ms: Int,
) -> Result(String, ControlError) {
  use socket <- try_control(connect(control_file))
  let request = authenticate(control_file, request)
  let result = case
    send_line(
      socket,
      protocol.request_to_string(request),
      timeout_settings.current_timeout_ms(),
    )
  {
    Error(error) -> Error(ConnectionFailed(error))
    Ok(Nil) ->
      case recv_line(socket, response_timeout_ms) {
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
  apply_command_with_response_timeout(
    control_file,
    operator_command,
    operator_command_response_timeout_ms(control_file),
  )
}

pub fn apply_command_with_response_timeout(
  control_file: file.ControlFile,
  operator_command: command.OperatorCommand,
  response_timeout_ms: Int,
) -> Result(command.CommandResult, ControlError) {
  use line <- try_control(raw_request_with_response_timeout(
    control_file,
    protocol.command_request("1", "", operator_command),
    response_timeout_ms,
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
      timeout_settings.current_timeout_ms(),
    )
  {
    Error(error) -> {
      ffi_close_socket(socket)
      Error(ConnectionFailed(error))
    }
    Ok(Nil) ->
      case recv_line(socket, timeout_settings.current_timeout_ms()) {
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
    Error(Timeout) | Error(ReceiveTimeout) -> stream_loop(socket, on_event)
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

fn request_response_timeout_ms(
  control_file: file.ControlFile,
  request: protocol.Request,
) -> Int {
  case protocol.request_operator_command(request), request {
    Some(_), _ -> operator_command_response_timeout_ms(control_file)
    None, protocol.Query(_, _, _) ->
      operator_command_response_timeout_ms(control_file)
    None, _ -> timeout_settings.current_timeout_ms()
  }
}

fn connect(control_file: file.ControlFile) -> Result(Socket, ControlError) {
  ffi_connect(
    control_file.host,
    control_file.port,
    timeout_settings.current_timeout_ms(),
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
    protocol.RetryIssueStartFresh(id, _, issue_ref, reason) ->
      protocol.RetryIssueStartFresh(id, control_file.token, issue_ref, reason)
    protocol.RetryWorkflowStep(id, _, target, step_id) ->
      protocol.RetryWorkflowStep(id, control_file.token, target, step_id)
    protocol.RecollectWorkflowOutputs(id, _, run_id) ->
      protocol.RecollectWorkflowOutputs(id, control_file.token, run_id)
    protocol.RunFinalize(
      id,
      _,
      run_id,
      validate,
      outputs,
      publish,
      update_tracker,
      dry_run,
      reason,
    ) ->
      protocol.RunFinalize(
        id,
        control_file.token,
        run_id,
        validate,
        outputs,
        publish,
        update_tracker,
        dry_run,
        reason,
      )
    protocol.RetryArtifactPublication(id, _, run_id, publication_id) ->
      protocol.RetryArtifactPublication(
        id,
        control_file.token,
        run_id,
        publication_id,
      )
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
    protocol.ReenableSchedule(id, _, job_id) ->
      protocol.ReenableSchedule(id, control_file.token, job_id)
    protocol.WorkItemAction(id, _, request) ->
      protocol.WorkItemAction(id, control_file.token, request)
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

pub fn timeout_error(
  error: ControlError,
  command: String,
) -> Option(timeout_policy.TimeoutError) {
  timeout_error_for_context(error, command, False, None)
}

pub fn timeout_error_for_request(
  error: ControlError,
  request: protocol.Request,
) -> Option(timeout_policy.TimeoutError) {
  timeout_error_for_context(
    error,
    request_cli_name(request),
    is_mutating_request(request),
    safe_read_command_for_request(request),
  )
}

fn timeout_error_for_context(
  error: ControlError,
  command: String,
  is_mutating: Bool,
  safe_read_command: Option(String),
) -> Option(timeout_policy.TimeoutError) {
  case error {
    ConnectionFailed(Timeout)
    | ConnectionFailed(ConnectTimeout)
    | ConnectionFailed(ConnectFailed(_)) ->
      Some(timeout_policy.TimeoutError(
        phase: timeout_policy.DaemonConnect,
        timeout_ms: timeout_settings.current_timeout_ms(),
        accepted: timeout_policy.AcceptedFalse,
        retryable: True,
        message: "Could not connect to the Scherzo daemon within the configured control budget.",
        suggested_next_command: Some(retry_command(command)),
      ))
    ConnectionFailed(SendTimeout)
    | ConnectionFailed(ReceiveTimeout)
    | ConnectionFailed(Closed)
    | ConnectionFailed(ReceiveFailed(_)) ->
      Some(after_send_timeout_error(command, is_mutating, safe_read_command))
    RequestFailed("query_timeout", message) ->
      Some(timeout_policy.TimeoutError(
        phase: timeout_policy.DaemonActorQuery,
        timeout_ms: timeout_settings.current_timeout_ms(),
        accepted: timeout_policy.AcceptedFalse,
        retryable: True,
        message: message,
        suggested_next_command: Some(retry_command(command)),
      ))
    _ -> None
  }
}

fn after_send_timeout_error(
  command: String,
  is_mutating: Bool,
  safe_read_command: Option(String),
) -> timeout_policy.TimeoutError {
  case is_mutating {
    True ->
      timeout_policy.TimeoutError(
        phase: timeout_policy.OperationAdmission,
        timeout_ms: timeout_settings.current_timeout_ms(),
        accepted: timeout_policy.AcceptedUnknown,
        retryable: False,
        message: "Timed out waiting for the daemon to admit the mutating request; acceptance is unknown.",
        suggested_next_command: Some(safe_read_or_default(safe_read_command)),
      )
    False ->
      timeout_policy.TimeoutError(
        phase: timeout_policy.RequestRoundTrip,
        timeout_ms: timeout_settings.current_timeout_ms(),
        accepted: timeout_policy.AcceptedFalse,
        retryable: True,
        message: "Timed out waiting for the daemon response after sending the read request.",
        suggested_next_command: Some(retry_command(command)),
      )
  }
}

fn retry_command(command: String) -> String {
  "scripts/scherzoctl " <> command <> " --json --timeout 10s"
}

fn safe_read_or_default(command: Option(String)) -> String {
  case command {
    Some(command) -> command
    None -> "scripts/scherzoctl ps --json --timeout 10s"
  }
}

fn is_mutating_request(request: protocol.Request) -> Bool {
  case protocol.request_operator_command(request) {
    Some(_) -> True
    None -> False
  }
}

fn request_cli_name(request: protocol.Request) -> String {
  case request {
    protocol.Ping(_, _) -> "ping"
    protocol.ListSessions(_, _) -> "ps"
    protocol.GetSession(_, _, session_id) -> "session " <> session_id
    protocol.GetEvents(_, _, session_id, _, _) -> "events " <> session_id
    protocol.Query(_, _, query_types.Status) -> "query status"
    protocol.Query(_, _, query_types.Metrics) -> "query metrics"
    protocol.Query(_, _, query_types.OperationStatus(query)) ->
      "query operation-status " <> query.operation_id
    protocol.Query(_, _, _) -> "query status"
    _ ->
      case protocol.request_operator_command(request) {
        Some(operator_command) -> operator_command_cli_name(operator_command)
        None -> "ping"
      }
  }
}

fn operator_command_cli_name(
  operator_command: command.OperatorCommand,
) -> String {
  let name = command.command_name(operator_command) |> string.replace("_", "-")
  case command.command_target(operator_command) {
    Some(target) -> name <> " " <> target
    None -> name
  }
}

fn safe_read_command_for_request(request: protocol.Request) -> Option(String) {
  case protocol.request_operator_command(request) {
    Some(operator_command) -> safe_read_command_for_operator(operator_command)
    None -> None
  }
}

fn safe_read_command_for_operator(
  operator_command: command.OperatorCommand,
) -> Option(String) {
  case operator_command {
    command.AbortSession(session_id)
    | command.StopAfterCurrentTurn(session_id)
    | command.PromptSession(session_id, _)
    | command.RespondUi(session_id, _, _) ->
      Some(
        "scripts/scherzoctl events " <> session_id <> " --json --timeout 10s",
      )
    command.RetryIssue(issue_ref)
    | command.RetryIssueStartFresh(issue_ref, _)
    | command.ParkIssue(issue_ref, _)
    | command.UnparkIssue(issue_ref) ->
      Some(
        "scripts/scherzoctl task show "
        <> command.issue_ref_to_string(issue_ref)
        <> " --json --timeout 10s",
      )
    command.RetryWorkflowStep(_, _)
    | command.RecollectWorkflowOutputs(_)
    | command.RunFinalize(_, _, _, _, _, _, _)
    | command.RetryArtifactPublication(_, _)
    | command.CleanupOrphanSteps(_, _)
    | command.RunScheduleNow(_)
    | command.ReenableSchedule(_)
    | command.WorkItemAction(_) ->
      Some("scripts/scherzoctl ps --json --timeout 10s")
    command.PauseDispatch | command.ResumeDispatch | command.ReloadWorkflow ->
      Some("scripts/scherzoctl query status --json --timeout 10s")
  }
}

pub fn compact_event_line(stored_event: event.SessionEvent) -> String {
  case event.payload_kind(stored_event.payload) {
    event.Turn -> compact_turn_event_line(stored_event)
    _ ->
      int.to_string(stored_event.cursor)
      <> " "
      <> int.to_string(stored_event.at_ms)
      <> " "
      <> stored_event.session_id
      <> " "
      <> event.kind_to_string(event.payload_kind(stored_event.payload))
      <> " "
      <> event.payload_name_to_string(stored_event.payload)
      <> compact_message(event.payload_message(stored_event.payload))
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
  <> event.payload_name_to_string(payload)
  <> compact_turn_field(event.payload_turn(payload))
  <> compact_turn_status_field(event.payload_turn_status(payload))
  <> compact_duration_field(event.payload_turn_duration_ms(payload))
  <> compact_token_delta_field(event.payload_token_delta(payload).total)
  <> compact_reason_field(event.payload_reason(payload))
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
    "timeout" ->
      case function {
        "connect" -> ConnectTimeout
        "send_line" -> SendTimeout
        "recv_line" -> ReceiveTimeout
        _ -> Timeout
      }
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
    Timeout | ConnectTimeout | SendTimeout | ReceiveTimeout -> "timeout"
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
