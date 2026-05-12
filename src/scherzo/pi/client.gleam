import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/context_exhaustion
import scherzo/error
import scherzo/pi/command as pi_command
import scherzo/pi/protocol
import scherzo/port
import scherzo/session/tokens as session_tokens

const max_interleaved_response_records = 100

const max_interleaved_response_bytes = 1_000_000

pub type Session {
  Session(
    process: port.Process,
    command: String,
    cwd: String,
    session_id: Option(String),
    session_file: Option(String),
    reported_cwd: Option(String),
    next_id: Int,
  )
}

pub type RpcCommandFailure {
  RpcCommandFailure(
    error: error.PiRpcError,
    skipped: List(protocol.RpcRecord),
    response: Option(protocol.RpcRecord),
  )
}

pub fn launch(
  command: String,
  cwd: String,
  session_name: String,
  auto_retry: Bool,
  read_timeout_ms: Int,
) -> Result(Session, error.PiRpcError) {
  launch_spec(
    pi_command.ShellLaunch(command),
    cwd,
    session_name,
    auto_retry,
    read_timeout_ms,
  )
}

pub fn launch_spec(
  spec: pi_command.LaunchSpec,
  cwd: String,
  session_name: String,
  auto_retry: Bool,
  read_timeout_ms: Int,
) -> Result(Session, error.PiRpcError) {
  case start_launch(spec, cwd) |> map_port_start_error {
    Error(err) -> Error(err)
    Ok(process) -> {
      let session =
        Session(
          process: process,
          command: describe_launch(spec),
          cwd: cwd,
          session_id: None,
          session_file: None,
          reported_cwd: None,
          next_id: 1,
        )
      case
        complete_launch_handshake(
          session,
          session_name,
          auto_retry,
          read_timeout_ms,
        )
      {
        Ok(session) -> Ok(session)
        Error(err) -> {
          let _ = port.terminate(process)
          Error(err)
        }
      }
    }
  }
}

fn complete_launch_handshake(
  session: Session,
  session_name: String,
  auto_retry: Bool,
  read_timeout_ms: Int,
) -> Result(Session, error.PiRpcError) {
  use session <- try_pi(send_expect_success(
    session,
    "set_session_name",
    protocol.encode_set_session_name,
    session_name,
    read_timeout_ms,
  ))
  use session <- try_pi(send_auto_retry(session, auto_retry, read_timeout_ms))
  use pair <- try_pi(send_get_state(session, read_timeout_ms))
  let #(session, record) = pair
  Ok(
    Session(
      ..session,
      session_id: record.session_id,
      session_file: record.session_file,
      reported_cwd: record.cwd,
    ),
  )
}

pub fn reopen_session_for_continuation(
  spec: pi_command.LaunchSpec,
  cwd: String,
  expected_session_file: String,
  read_timeout_ms: Int,
) -> Result(Session, error.PiRpcError) {
  use session <- try_pi(launch_spec(
    spec,
    cwd,
    "Scherzo recovered workflow step",
    False,
    read_timeout_ms,
  ))
  case validate_reopened_state(session, expected_session_file, cwd) {
    Ok(Nil) -> Ok(session)
    Error(err) -> {
      let _ = terminate(session)
      Error(err)
    }
  }
}

fn start_launch(
  spec: pi_command.LaunchSpec,
  cwd: String,
) -> Result(port.Process, port.PortError) {
  case spec {
    pi_command.ShellLaunch(command) -> port.start(command, cwd)
    pi_command.ArgvLaunch(executable, args, env) ->
      port.start_argv(executable, args, cwd, env)
  }
}

fn describe_launch(spec: pi_command.LaunchSpec) -> String {
  case spec {
    pi_command.ShellLaunch(command) -> command
    pi_command.ArgvLaunch(executable, args, _) ->
      string.join([executable, ..args], with: " ")
  }
}

fn validate_reopened_state(
  session: Session,
  expected_session_file: String,
  expected_cwd: String,
) -> Result(Nil, error.PiRpcError) {
  case session.session_id {
    None -> Error(error.PiProtocolError("reopened session missing sessionId"))
    Some(value) ->
      case string.trim(value) == "" {
        True ->
          Error(error.PiProtocolError("reopened session missing sessionId"))
        False ->
          validate_reopened_session_file(
            session.session_file,
            expected_session_file,
            session.reported_cwd,
            expected_cwd,
          )
      }
  }
}

fn validate_reopened_session_file(
  actual: Option(String),
  expected_session_file: String,
  reported_cwd: Option(String),
  expected_cwd: String,
) -> Result(Nil, error.PiRpcError) {
  case actual {
    None -> Error(error.PiProtocolError("reopened session missing sessionFile"))
    Some(value) ->
      case value == expected_session_file {
        False -> Error(error.PiProtocolError("reopened sessionFile mismatch"))
        True -> validate_reopened_cwd(reported_cwd, expected_cwd)
      }
  }
}

fn validate_reopened_cwd(
  reported_cwd: Option(String),
  expected_cwd: String,
) -> Result(Nil, error.PiRpcError) {
  case reported_cwd {
    None -> Ok(Nil)
    Some(actual_cwd) ->
      case actual_cwd == expected_cwd {
        True -> Ok(Nil)
        False -> Error(error.PiProtocolError("reopened cwd mismatch"))
      }
  }
}

pub fn send_prompt(
  session: Session,
  message: String,
  read_timeout_ms: Int,
) -> Result(#(Session, List(protocol.RpcRecord)), error.PiRpcError) {
  let id = int_to_string(session.next_id)
  use _ <- try_pi(
    port.send_line(session.process, protocol.encode_prompt(id, message))
    |> map_port_error,
  )
  use pair <- try_pi(
    read_until_response_collect(session.process, id, read_timeout_ms, []),
  )
  let #(record, skipped) = pair
  case record.success {
    Some(True) ->
      Ok(#(Session(..session, next_id: session.next_id + 1), skipped))
    _ -> Error(classified_rejected_command("prompt rejected", record))
  }
}

pub fn compact(
  session: Session,
  custom_instructions: Option(String),
  read_timeout_ms: Int,
) -> Result(#(Session, List(protocol.RpcRecord)), error.PiRpcError) {
  case compact_with_diagnostics(session, custom_instructions, read_timeout_ms) {
    Ok(value) -> Ok(value)
    Error(failure) -> Error(failure.error)
  }
}

pub fn compact_with_diagnostics(
  session: Session,
  custom_instructions: Option(String),
  read_timeout_ms: Int,
) -> Result(#(Session, List(protocol.RpcRecord)), RpcCommandFailure) {
  let id = int_to_string(session.next_id)
  case
    port.send_line(
      session.process,
      protocol.encode_compact(id, custom_instructions),
    )
    |> map_port_error
  {
    Error(err) ->
      Error(RpcCommandFailure(error: err, skipped: [], response: None))
    Ok(_) ->
      case
        read_until_response_collect_diagnostics(
          session.process,
          id,
          read_timeout_ms,
          [],
        )
      {
        Error(failure) -> Error(failure)
        Ok(#(record, skipped)) ->
          case record.success {
            Some(True) ->
              Ok(#(Session(..session, next_id: session.next_id + 1), skipped))
            _ ->
              Error(RpcCommandFailure(
                error: classified_rejected_command("compact failed", record),
                skipped: skipped,
                response: Some(record),
              ))
          }
      }
  }
}

pub fn set_auto_compaction(
  session: Session,
  read_timeout_ms: Int,
  enabled enabled: Bool,
) -> Result(#(Session, List(protocol.RpcRecord)), error.PiRpcError) {
  let id = int_to_string(session.next_id)
  use _ <- try_pi(
    port.send_line(
      session.process,
      protocol.encode_set_auto_compaction(id, enabled: enabled),
    )
    |> map_port_error,
  )
  use pair <- try_pi(
    read_until_response_collect(session.process, id, read_timeout_ms, []),
  )
  let #(record, skipped) = pair
  case record.success {
    Some(True) ->
      Ok(#(Session(..session, next_id: session.next_id + 1), skipped))
    _ ->
      Error(classified_rejected_command("set_auto_compaction failed", record))
  }
}

pub fn read_turn_record(
  session: Session,
  read_timeout_ms: Int,
  turn_deadline_ms: Int,
  stall_deadline_ms: Int,
) -> Result(#(Session, Option(protocol.RpcRecord)), error.PiRpcError) {
  use maybe_line <- try_pi(read_turn_line(
    session.process,
    read_timeout_ms,
    turn_deadline_ms,
    stall_deadline_ms,
  ))
  case maybe_line {
    None -> Ok(#(session, None))
    Some(line) -> {
      use record <- try_pi(protocol.decode_record(line))
      Ok(#(session, Some(record)))
    }
  }
}

pub fn send_abort(
  session: Session,
  read_timeout_ms: Int,
) -> Result(#(Session, List(protocol.RpcRecord)), error.PiRpcError) {
  let id = int_to_string(session.next_id)
  use _ <- try_pi(
    port.send_line(session.process, protocol.encode_abort(id)) |> map_port_error,
  )
  use pair <- try_pi(
    read_until_response_collect(session.process, id, read_timeout_ms, []),
  )
  let #(record, skipped) = pair
  case record.success {
    Some(True) ->
      Ok(#(Session(..session, next_id: session.next_id + 1), skipped))
    _ -> Error(error.PiProtocolError("abort failed"))
  }
}

pub fn send_extension_ui_cancel(
  session: Session,
  request_id: String,
  read_timeout_ms: Int,
) -> Result(#(Session, List(protocol.RpcRecord)), error.PiRpcError) {
  use _ <- try_pi(
    port.send_line(
      session.process,
      protocol.encode_extension_ui_response(request_id),
    )
    |> map_port_error,
  )
  use pair <- try_pi(
    read_until_response_collect(
      session.process,
      request_id,
      read_timeout_ms,
      [],
    ),
  )
  let #(record, skipped) = pair
  case record.success {
    Some(True) -> Ok(#(session, skipped))
    _ -> Error(error.PiProtocolError("extension_ui_response failed"))
  }
}

pub fn send_extension_ui_value(
  session: Session,
  request_id: String,
  value: String,
  read_timeout_ms: Int,
) -> Result(#(Session, List(protocol.RpcRecord)), error.PiRpcError) {
  use _ <- try_pi(
    port.send_line(
      session.process,
      protocol.encode_extension_ui_value_response(request_id, value),
    )
    |> map_port_error,
  )
  use pair <- try_pi(
    read_until_response_collect(
      session.process,
      request_id,
      read_timeout_ms,
      [],
    ),
  )
  let #(record, skipped) = pair
  case record.success {
    Some(True) -> Ok(#(session, skipped))
    _ -> Error(error.PiProtocolError("extension_ui_response failed"))
  }
}

pub fn get_session_stats(
  session: Session,
  read_timeout_ms: Int,
) -> Result(#(Session, session_tokens.TokenTotals), error.PiRpcError) {
  let id = int_to_string(session.next_id)
  use _ <- try_pi(
    port.send_line(session.process, protocol.encode_get_session_stats(id))
    |> map_port_error,
  )
  use record <- try_pi(read_until_response(session.process, id, read_timeout_ms))
  case record.success {
    Some(True) ->
      Ok(#(Session(..session, next_id: session.next_id + 1), record.tokens))
    _ -> Error(error.PiProtocolError("get_session_stats failed"))
  }
}

pub fn terminate(session: Session) -> Result(Nil, error.PiRpcError) {
  port.terminate(session.process) |> map_port_error
}

fn send_expect_success(
  session: Session,
  command: String,
  encoder: fn(String, String) -> String,
  value: String,
  read_timeout_ms: Int,
) -> Result(Session, error.PiRpcError) {
  let id = int_to_string(session.next_id)
  use _ <- try_pi(
    port.send_line(session.process, encoder(id, value)) |> map_port_error,
  )
  use record <- try_pi(read_until_response(session.process, id, read_timeout_ms))
  case record.success {
    Some(True) -> Ok(Session(..session, next_id: session.next_id + 1))
    _ -> Error(error.PiProtocolError(command <> " failed"))
  }
}

fn classified_rejected_command(
  fallback: String,
  record: protocol.RpcRecord,
) -> error.PiRpcError {
  case context_exhaustion.from_rpc_record(record) {
    Some(context) -> context_exhaustion.to_pi_rpc_error(context)
    None ->
      case record.error_message {
        Some(message) -> error.PiProtocolError(fallback <> ": " <> message)
        None -> error.PiProtocolError(fallback)
      }
  }
}

fn send_auto_retry(
  session: Session,
  enabled: Bool,
  read_timeout_ms: Int,
) -> Result(Session, error.PiRpcError) {
  let id = int_to_string(session.next_id)
  use _ <- try_pi(
    port.send_line(
      session.process,
      protocol.encode_set_auto_retry(id, enabled: enabled),
    )
    |> map_port_error,
  )
  use record <- try_pi(read_until_response(session.process, id, read_timeout_ms))
  case record.success {
    Some(True) -> Ok(Session(..session, next_id: session.next_id + 1))
    _ -> Error(error.PiProtocolError("set_auto_retry failed"))
  }
}

fn send_get_state(
  session: Session,
  read_timeout_ms: Int,
) -> Result(#(Session, protocol.RpcRecord), error.PiRpcError) {
  let id = int_to_string(session.next_id)
  use _ <- try_pi(
    port.send_line(session.process, protocol.encode_get_state(id))
    |> map_port_error,
  )
  use record <- try_pi(read_until_response(session.process, id, read_timeout_ms))
  case record.success {
    Some(True) ->
      Ok(#(Session(..session, next_id: session.next_id + 1), record))
    _ -> Error(error.PiProtocolError("get_state failed"))
  }
}

fn read_until_response(
  process: port.Process,
  id: String,
  timeout_ms: Int,
) -> Result(protocol.RpcRecord, error.PiRpcError) {
  use pair <- try_pi(read_until_response_collect(process, id, timeout_ms, []))
  let #(record, _skipped) = pair
  Ok(record)
}

fn read_until_response_collect(
  process: port.Process,
  id: String,
  timeout_ms: Int,
  skipped: List(protocol.RpcRecord),
) -> Result(#(protocol.RpcRecord, List(protocol.RpcRecord)), error.PiRpcError) {
  case
    read_until_response_collect_diagnostics(process, id, timeout_ms, skipped)
  {
    Ok(pair) -> Ok(pair)
    Error(failure) -> Error(failure.error)
  }
}

fn read_until_response_collect_diagnostics(
  process: port.Process,
  id: String,
  timeout_ms: Int,
  skipped: List(protocol.RpcRecord),
) -> Result(#(protocol.RpcRecord, List(protocol.RpcRecord)), RpcCommandFailure) {
  read_until_response_collect_until(
    process,
    id,
    timeout_ms,
    monotonic_ms() + timeout_ms,
    skipped,
    list.length(skipped),
    skipped_record_bytes(skipped),
  )
}

fn read_until_response_collect_until(
  process: port.Process,
  id: String,
  timeout_ms: Int,
  deadline_ms: Int,
  skipped: List(protocol.RpcRecord),
  skipped_count: Int,
  skipped_bytes: Int,
) -> Result(#(protocol.RpcRecord, List(protocol.RpcRecord)), RpcCommandFailure) {
  let remaining_ms = deadline_ms - monotonic_ms()
  case remaining_ms <= 0 {
    True -> read_failure(error.PiReadTimeout, skipped)
    False -> {
      let read_timeout_ms = min_int(timeout_ms, remaining_ms)
      case port.read_stdout_line(process, read_timeout_ms) |> map_port_error {
        Error(err) -> read_failure(err, skipped)
        Ok(line) ->
          case protocol.decode_record(line) {
            Error(err) -> read_failure(err, skipped)
            Ok(record) ->
              case record.id == Some(id) && record.type_ == "response" {
                True -> Ok(#(record, list.reverse(skipped)))
                False ->
                  read_until_response_collect_after_skipped_record(
                    process,
                    id,
                    timeout_ms,
                    deadline_ms,
                    skipped,
                    skipped_count,
                    skipped_bytes,
                    record,
                  )
              }
          }
      }
    }
  }
}

fn read_until_response_collect_after_skipped_record(
  process: port.Process,
  id: String,
  timeout_ms: Int,
  deadline_ms: Int,
  skipped: List(protocol.RpcRecord),
  skipped_count: Int,
  skipped_bytes: Int,
  record: protocol.RpcRecord,
) -> Result(#(protocol.RpcRecord, List(protocol.RpcRecord)), RpcCommandFailure) {
  let skipped_count = skipped_count + 1
  let skipped_bytes = skipped_bytes + string.length(record.raw_json)
  case
    skipped_count > max_interleaved_response_records
    || skipped_bytes > max_interleaved_response_bytes
  {
    True ->
      read_failure(error.PiProtocolError("too many interleaved records"), [
        record,
        ..skipped
      ])
    False ->
      read_until_response_collect_until(
        process,
        id,
        timeout_ms,
        deadline_ms,
        [record, ..skipped],
        skipped_count,
        skipped_bytes,
      )
  }
}

fn read_failure(
  err: error.PiRpcError,
  skipped: List(protocol.RpcRecord),
) -> Result(a, RpcCommandFailure) {
  Error(RpcCommandFailure(
    error: err,
    skipped: list.reverse(skipped),
    response: None,
  ))
}

fn skipped_record_bytes(records: List(protocol.RpcRecord)) -> Int {
  case records {
    [] -> 0
    [record, ..rest] ->
      string.length(record.raw_json) + skipped_record_bytes(rest)
  }
}

fn read_turn_line(
  process: port.Process,
  read_timeout_ms: Int,
  turn_deadline_ms: Int,
  stall_deadline_ms: Int,
) -> Result(Option(String), error.PiRpcError) {
  let now = monotonic_ms()
  let remaining_turn_ms = turn_deadline_ms - now
  let remaining_stall_ms = stall_deadline_ms - now
  case remaining_turn_ms <= 0 {
    True -> Error(error.PiTurnTimeout)
    False ->
      case remaining_stall_ms <= 0 {
        True -> Error(error.PiStallTimeout)
        False -> {
          let timeout_ms =
            read_timeout_ms
            |> min_int(remaining_turn_ms)
            |> min_int(remaining_stall_ms)
          case port.read_stdout_line(process, timeout_ms) {
            Ok(line) -> Ok(Some(line))
            Error(port.ReadTimeout) -> {
              let now = monotonic_ms()
              case now >= turn_deadline_ms {
                True -> Error(error.PiTurnTimeout)
                False ->
                  case now >= stall_deadline_ms {
                    True -> Error(error.PiStallTimeout)
                    False -> Ok(None)
                  }
              }
            }
            Error(err) -> map_port_error(Error(err))
          }
        }
      }
  }
}

fn min_int(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}

fn map_port_start_error(
  result: Result(port.Process, port.PortError),
) -> Result(port.Process, error.PiRpcError) {
  case result {
    Ok(process) -> Ok(process)
    Error(err) -> Error(error.PiLaunchFailed(port_error_to_string(err)))
  }
}

fn map_port_error(
  result: Result(a, port.PortError),
) -> Result(a, error.PiRpcError) {
  case result {
    Ok(value) -> Ok(value)
    Error(port.ReadTimeout) -> Error(error.PiReadTimeout)
    Error(port.ProcessExited(status)) -> Error(error.PiExited(status))
    Error(err) -> Error(error.PiProtocolError(port_error_to_string(err)))
  }
}

fn port_error_to_string(err: port.PortError) -> String {
  port.port_error_to_string(err)
}

fn try_pi(
  result: Result(a, error.PiRpcError),
  next: fn(a) -> Result(b, error.PiRpcError),
) -> Result(b, error.PiRpcError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String

@external(erlang, "scherzo_time_ffi", "monotonic_ms")
fn monotonic_ms() -> Int
