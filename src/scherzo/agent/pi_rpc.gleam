import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/domain
import scherzo/error
import scherzo/port

pub type Session {
  Session(
    process: port.Process,
    command: String,
    cwd: String,
    session_id: Option(String),
    next_id: Int,
  )
}

pub type RpcRecord {
  RpcRecord(
    type_: String,
    id: Option(String),
    command: Option(String),
    success: Option(Bool),
    session_id: Option(String),
    delta: Option(String),
    method: Option(String),
    tokens: domain.TokenTotals,
    raw_json: String,
  )
}

pub fn encode_set_session_name(id: String, name: String) -> String {
  json.object([
    #("id", json.string(id)),
    #("type", json.string("set_session_name")),
    #("name", json.string(name)),
  ])
  |> json.to_string
}

pub fn encode_set_auto_retry(id: String, enabled: Bool) -> String {
  json.object([
    #("id", json.string(id)),
    #("type", json.string("set_auto_retry")),
    #("enabled", json.bool(enabled)),
  ])
  |> json.to_string
}

pub fn encode_get_state(id: String) -> String {
  json.object([#("id", json.string(id)), #("type", json.string("get_state"))])
  |> json.to_string
}

pub fn encode_prompt(id: String, message: String) -> String {
  json.object([
    #("id", json.string(id)),
    #("type", json.string("prompt")),
    #("message", json.string(message)),
  ])
  |> json.to_string
}

pub fn encode_abort(id: String) -> String {
  json.object([#("id", json.string(id)), #("type", json.string("abort"))])
  |> json.to_string
}

pub fn encode_get_session_stats(id: String) -> String {
  json.object([
    #("id", json.string(id)),
    #("type", json.string("get_session_stats")),
  ])
  |> json.to_string
}

pub fn encode_extension_ui_response(id: String) -> String {
  json.object([
    #("id", json.string(id)),
    #("type", json.string("extension_ui_response")),
    #("cancelled", json.bool(True)),
  ])
  |> json.to_string
}

pub fn decode_record(line: String) -> Result(RpcRecord, error.PiRpcError) {
  case json.parse(line, record_decoder(line)) {
    Ok(record) -> Ok(record)
    Error(_) -> Error(error.PiMalformedJson(line))
  }
}

pub fn launch(
  command: String,
  cwd: String,
  session_name: String,
  auto_retry: Bool,
  read_timeout_ms: Int,
) -> Result(Session, error.PiRpcError) {
  use process <- try_pi(port.start(command, cwd) |> map_port_start_error)
  let session =
    Session(
      process: process,
      command: command,
      cwd: cwd,
      session_id: None,
      next_id: 1,
    )
  use session <- try_pi(send_expect_success(
    session,
    "set_session_name",
    encode_set_session_name,
    session_name,
    read_timeout_ms,
  ))
  use session <- try_pi(send_auto_retry(session, auto_retry, read_timeout_ms))
  use pair <- try_pi(send_get_state(session, read_timeout_ms))
  let #(session, record) = pair
  Ok(Session(..session, session_id: record.session_id))
}

pub fn prompt(
  session: Session,
  message: String,
  read_timeout_ms: Int,
  turn_timeout_ms: Int,
  stall_timeout_ms: Int,
  on_event: fn(RpcRecord) -> Nil,
) -> Result(#(Session, List(RpcRecord)), error.PiRpcError) {
  prompt_with_ui_policy(
    session,
    message,
    read_timeout_ms,
    turn_timeout_ms,
    stall_timeout_ms,
    domain.Cancel,
    on_event,
  )
}

pub fn prompt_with_ui_policy(
  session: Session,
  message: String,
  read_timeout_ms: Int,
  turn_timeout_ms: Int,
  stall_timeout_ms: Int,
  ui_request_policy: domain.UiRequestPolicy,
  on_event: fn(RpcRecord) -> Nil,
) -> Result(#(Session, List(RpcRecord)), error.PiRpcError) {
  let id = int_to_string(session.next_id)
  use _ <- try_pi(
    port.send_line(session.process, encode_prompt(id, message))
    |> map_port_error,
  )
  use record <- try_pi(read_until_response(session.process, id, read_timeout_ms))
  case record.success {
    Some(True) -> {
      let now = monotonic_ms()
      read_events_until_agent_end(
        Session(..session, next_id: session.next_id + 1),
        read_timeout_ms,
        stall_timeout_ms,
        now + turn_timeout_ms,
        now + stall_timeout_ms,
        [],
        ui_request_policy,
        on_event,
      )
    }
    _ -> Error(error.PiProtocolError("prompt rejected"))
  }
}

pub fn get_session_stats(
  session: Session,
  read_timeout_ms: Int,
) -> Result(#(Session, domain.TokenTotals), error.PiRpcError) {
  let id = int_to_string(session.next_id)
  use _ <- try_pi(
    port.send_line(session.process, encode_get_session_stats(id))
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

fn send_auto_retry(
  session: Session,
  enabled: Bool,
  read_timeout_ms: Int,
) -> Result(Session, error.PiRpcError) {
  let id = int_to_string(session.next_id)
  use _ <- try_pi(
    port.send_line(session.process, encode_set_auto_retry(id, enabled))
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
) -> Result(#(Session, RpcRecord), error.PiRpcError) {
  let id = int_to_string(session.next_id)
  use _ <- try_pi(
    port.send_line(session.process, encode_get_state(id)) |> map_port_error,
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
) -> Result(RpcRecord, error.PiRpcError) {
  use line <- try_pi(
    port.read_stdout_line(process, timeout_ms) |> map_port_error,
  )
  use record <- try_pi(decode_record(line))
  case record.id == Some(id) && record.type_ == "response" {
    True -> Ok(record)
    False -> read_until_response(process, id, timeout_ms)
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

fn read_events_until_agent_end(
  session: Session,
  read_timeout_ms: Int,
  stall_timeout_ms: Int,
  turn_deadline_ms: Int,
  stall_deadline_ms: Int,
  acc: List(RpcRecord),
  ui_request_policy: domain.UiRequestPolicy,
  on_event: fn(RpcRecord) -> Nil,
) -> Result(#(Session, List(RpcRecord)), error.PiRpcError) {
  use maybe_line <- try_pi(read_turn_line(
    session.process,
    read_timeout_ms,
    turn_deadline_ms,
    stall_deadline_ms,
  ))
  case maybe_line {
    None ->
      read_events_until_agent_end(
        session,
        read_timeout_ms,
        stall_timeout_ms,
        turn_deadline_ms,
        stall_deadline_ms,
        acc,
        ui_request_policy,
        on_event,
      )
    Some(line) -> {
      use record <- try_pi(decode_record(line))
      on_event(record)
      let next_stall_deadline_ms = monotonic_ms() + stall_timeout_ms
      case record.type_ {
        "extension_ui_request" -> {
          case record.method {
            Some("select") | Some("confirm") | Some("input") | Some("editor") -> {
              case record.id {
                Some(id) -> {
                  handle_blocking_ui_request(
                    session,
                    id,
                    read_timeout_ms,
                    stall_timeout_ms,
                    turn_deadline_ms,
                    next_stall_deadline_ms,
                    [record, ..acc],
                    ui_request_policy,
                    on_event,
                  )
                }
                None ->
                  Error(error.PiProtocolError("extension UI request missing id"))
              }
            }
            _ ->
              read_events_until_agent_end(
                session,
                read_timeout_ms,
                stall_timeout_ms,
                turn_deadline_ms,
                next_stall_deadline_ms,
                [record, ..acc],
                ui_request_policy,
                on_event,
              )
          }
        }
        "agent_end" -> Ok(#(session, list.reverse([record, ..acc])))
        _ ->
          read_events_until_agent_end(
            session,
            read_timeout_ms,
            stall_timeout_ms,
            turn_deadline_ms,
            next_stall_deadline_ms,
            [record, ..acc],
            ui_request_policy,
            on_event,
          )
      }
    }
  }
}

fn handle_blocking_ui_request(
  session: Session,
  request_id: String,
  read_timeout_ms: Int,
  stall_timeout_ms: Int,
  turn_deadline_ms: Int,
  stall_deadline_ms: Int,
  acc: List(RpcRecord),
  ui_request_policy: domain.UiRequestPolicy,
  on_event: fn(RpcRecord) -> Nil,
) -> Result(#(Session, List(RpcRecord)), error.PiRpcError) {
  case ui_request_policy {
    domain.Fail ->
      Error(error.PiProtocolError("extension UI request blocked by policy"))
    domain.Ignore ->
      read_events_until_agent_end(
        session,
        read_timeout_ms,
        stall_timeout_ms,
        turn_deadline_ms,
        stall_deadline_ms,
        acc,
        ui_request_policy,
        on_event,
      )
    domain.Cancel -> {
      let _ =
        port.send_line(
          session.process,
          encode_extension_ui_response(request_id),
        )
      read_events_until_agent_end(
        session,
        read_timeout_ms,
        stall_timeout_ms,
        turn_deadline_ms,
        stall_deadline_ms,
        acc,
        ui_request_policy,
        on_event,
      )
    }
    domain.Operator ->
      Error(error.PiProtocolError("operator UI policy is not implemented"))
  }
}

type MessageObject {
  MessageObject(
    role: Option(String),
    tool_name: Option(String),
    is_error: Option(Bool),
    content: List(ContentItem),
  )
}

type ContentItem {
  ContentItem(
    type_: String,
    text: Option(String),
    name: Option(String),
    command: Option(String),
  )
}

const structured_tool_input_placeholder = "[structured tool input; use --json for raw details]"

fn record_decoder(raw_json: String) -> decode.Decoder(RpcRecord) {
  use type_ <- decode.field("type", decode.string)
  use id <- decode.optional_field("id", None, decode.optional(decode.string))
  use command <- decode.optional_field(
    "command",
    None,
    decode.optional(decode.string),
  )
  use success <- decode.optional_field(
    "success",
    None,
    decode.optional(decode.bool),
  )
  use data <- decode.optional_field("data", empty_data(), data_decoder())
  use delta <- decode.optional_field(
    "delta",
    None,
    decode.optional(decode.string),
  )
  use method <- decode.optional_field(
    "method",
    None,
    decode.optional(decode.string),
  )
  decode.success(RpcRecord(
    type_: type_,
    id: id,
    command: command,
    success: success,
    session_id: data.session_id,
    delta: delta,
    method: method,
    tokens: data.tokens,
    raw_json: raw_json,
  ))
}

pub type Data {
  Data(session_id: Option(String), tokens: domain.TokenTotals)
}

fn empty_data() -> Data {
  Data(session_id: None, tokens: domain.zero_token_totals())
}

fn data_decoder() -> decode.Decoder(Data) {
  use session_id <- decode.optional_field(
    "sessionId",
    None,
    decode.optional(decode.string),
  )
  use tokens <- decode.optional_field(
    "tokens",
    domain.zero_token_totals(),
    tokens_decoder(),
  )
  decode.success(Data(session_id: session_id, tokens: tokens))
}

fn tokens_decoder() -> decode.Decoder(domain.TokenTotals) {
  use input <- decode.optional_field("input", 0, decode.int)
  use output <- decode.optional_field("output", 0, decode.int)
  use cache_read <- decode.optional_field("cacheRead", 0, decode.int)
  use cache_write <- decode.optional_field("cacheWrite", 0, decode.int)
  use total <- decode.optional_field("total", 0, decode.int)
  decode.success(domain.TokenTotals(
    input: input,
    output: output,
    cache_read: cache_read,
    cache_write: cache_write,
    total: total,
  ))
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
  case err {
    port.StartFailed(message) -> message
    port.SendFailed(message) -> message
    port.ReadTimeout -> "read timeout"
    port.LineTooLong -> "line too long"
    port.ProcessExited(status) -> "process exited " <> int_to_string(status)
    port.PortClosed -> "port closed"
    port.DiagnosticsFailed(message) -> message
    port.TerminateFailed(message) -> message
    port.AwaitTimeout -> "await timeout"
    port.AwaitFailed(message) -> message
  }
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
