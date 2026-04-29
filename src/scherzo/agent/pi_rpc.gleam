import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/domain
import scherzo/error
import scherzo/port

const max_interleaved_response_records = 100

const max_interleaved_response_bytes = 1_000_000

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
    message: Option(String),
    method: Option(String),
    tokens: domain.TokenTotals,
    tool_name: Option(String),
    tool_input: Option(String),
    tool_output: Option(String),
    tool_status: Option(String),
    assistant_messages: List(String),
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

pub fn encode_extension_ui_value_response(id: String, value: String) -> String {
  json.object([
    #("id", json.string(id)),
    #("type", json.string("extension_ui_response")),
    #("cancelled", json.bool(False)),
    #("value", json.string(value)),
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

pub fn send_prompt(
  session: Session,
  message: String,
  read_timeout_ms: Int,
) -> Result(#(Session, List(RpcRecord)), error.PiRpcError) {
  let id = int_to_string(session.next_id)
  use _ <- try_pi(
    port.send_line(session.process, encode_prompt(id, message))
    |> map_port_error,
  )
  use pair <- try_pi(
    read_until_response_collect(session.process, id, read_timeout_ms, []),
  )
  let #(record, skipped) = pair
  case record.success {
    Some(True) ->
      Ok(#(Session(..session, next_id: session.next_id + 1), skipped))
    _ -> Error(error.PiProtocolError("prompt rejected"))
  }
}

pub fn read_turn_record(
  session: Session,
  read_timeout_ms: Int,
  turn_deadline_ms: Int,
  stall_deadline_ms: Int,
) -> Result(#(Session, Option(RpcRecord)), error.PiRpcError) {
  use maybe_line <- try_pi(read_turn_line(
    session.process,
    read_timeout_ms,
    turn_deadline_ms,
    stall_deadline_ms,
  ))
  case maybe_line {
    None -> Ok(#(session, None))
    Some(line) -> {
      use record <- try_pi(decode_record(line))
      Ok(#(session, Some(record)))
    }
  }
}

pub fn send_abort(
  session: Session,
  read_timeout_ms: Int,
) -> Result(#(Session, List(RpcRecord)), error.PiRpcError) {
  let id = int_to_string(session.next_id)
  use _ <- try_pi(
    port.send_line(session.process, encode_abort(id)) |> map_port_error,
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
) -> Result(#(Session, List(RpcRecord)), error.PiRpcError) {
  use _ <- try_pi(
    port.send_line(session.process, encode_extension_ui_response(request_id))
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
) -> Result(#(Session, List(RpcRecord)), error.PiRpcError) {
  use _ <- try_pi(
    port.send_line(
      session.process,
      encode_extension_ui_value_response(request_id, value),
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
  use pair <- try_pi(read_until_response_collect(process, id, timeout_ms, []))
  let #(record, _skipped) = pair
  Ok(record)
}

fn read_until_response_collect(
  process: port.Process,
  id: String,
  timeout_ms: Int,
  skipped: List(RpcRecord),
) -> Result(#(RpcRecord, List(RpcRecord)), error.PiRpcError) {
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
  skipped: List(RpcRecord),
  skipped_count: Int,
  skipped_bytes: Int,
) -> Result(#(RpcRecord, List(RpcRecord)), error.PiRpcError) {
  let remaining_ms = deadline_ms - monotonic_ms()
  case remaining_ms <= 0 {
    True -> Error(error.PiReadTimeout)
    False -> {
      let read_timeout_ms = min_int(timeout_ms, remaining_ms)
      use line <- try_pi(
        port.read_stdout_line(process, read_timeout_ms) |> map_port_error,
      )
      use record <- try_pi(decode_record(line))
      case record.id == Some(id) && record.type_ == "response" {
        True -> Ok(#(record, list.reverse(skipped)))
        False -> {
          let skipped_count = skipped_count + 1
          let skipped_bytes = skipped_bytes + string.length(record.raw_json)
          case
            skipped_count > max_interleaved_response_records
            || skipped_bytes > max_interleaved_response_bytes
          {
            True -> Error(error.PiProtocolError("too many interleaved records"))
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
      }
    }
  }
}

fn skipped_record_bytes(records: List(RpcRecord)) -> Int {
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

type AgentEndMessage {
  AgentEndMessage(role: Option(String), content: Option(String))
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
    tolerant_optional_string_decoder(),
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
  use message <- decode.optional_field(
    "message",
    None,
    tolerant_optional_string_decoder(),
  )
  use message_object <- decode.optional_field(
    "message",
    empty_message_object(),
    tolerant_message_object_decoder(),
  )
  use assistant_messages <- decode.optional_field(
    "messages",
    [],
    tolerant_agent_end_messages_decoder(),
  )
  use top_tool_name_camel <- decode.optional_field(
    "toolName",
    None,
    tolerant_optional_string_decoder(),
  )
  use top_tool_name_snake <- decode.optional_field(
    "tool_name",
    None,
    tolerant_optional_string_decoder(),
  )
  use top_name <- decode.optional_field(
    "name",
    None,
    tolerant_optional_string_decoder(),
  )
  use top_command <- decode.optional_field(
    "command",
    None,
    structured_optional_string_decoder(),
  )
  use top_input <- decode.optional_field(
    "input",
    None,
    structured_optional_string_decoder(),
  )
  use top_args <- decode.optional_field(
    "args",
    None,
    structured_optional_string_decoder(),
  )
  use top_output <- decode.optional_field(
    "output",
    None,
    tolerant_optional_string_decoder(),
  )
  use top_stdout <- decode.optional_field(
    "stdout",
    None,
    tolerant_optional_string_decoder(),
  )
  use top_stderr <- decode.optional_field(
    "stderr",
    None,
    tolerant_optional_string_decoder(),
  )
  use top_status <- decode.optional_field(
    "status",
    None,
    tolerant_optional_string_decoder(),
  )
  use top_result <- decode.optional_field(
    "result",
    None,
    tolerant_optional_string_decoder(),
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
    message: message,
    method: method,
    tokens: data.tokens,
    tool_name: tool_name_for_record(
      type_,
      message_object,
      top_tool_name_camel,
      top_tool_name_snake,
      top_name,
      data.tool_name,
    ),
    tool_input: tool_input_for_record(
      message_object,
      top_command,
      top_input,
      top_args,
      data.tool_input,
    ),
    tool_output: tool_output_for_record(
      type_,
      message_object,
      top_output,
      top_stdout,
      top_stderr,
      delta,
      data.tool_output,
    ),
    tool_status: tool_status_for_record(
      type_,
      message_object,
      top_status,
      top_result,
      success,
      data.tool_status,
    ),
    assistant_messages: assistant_messages,
    raw_json: raw_json,
  ))
}

fn tolerant_optional_string_decoder() -> decode.Decoder(Option(String)) {
  decode.one_of(decode.optional(decode.string), or: [
    decode.dynamic |> decode.map(fn(_) { None }),
  ])
}

fn structured_optional_string_decoder() -> decode.Decoder(Option(String)) {
  decode.one_of(decode.optional(decode.string), or: [
    decode.dynamic
    |> decode.map(fn(_) { Some(structured_tool_input_placeholder) }),
  ])
}

fn tolerant_message_object_decoder() -> decode.Decoder(MessageObject) {
  decode.one_of(message_object_decoder(), or: [
    decode.dynamic |> decode.map(fn(_) { empty_message_object() }),
  ])
}

fn tolerant_agent_end_messages_decoder() -> decode.Decoder(List(String)) {
  decode.one_of(agent_end_messages_decoder(), or: [
    decode.dynamic |> decode.map(fn(_) { [] }),
  ])
}

fn agent_end_messages_decoder() -> decode.Decoder(List(String)) {
  decode.list(of: agent_end_message_decoder())
  |> decode.map(assistant_message_texts)
}

fn agent_end_message_decoder() -> decode.Decoder(AgentEndMessage) {
  use role <- decode.optional_field(
    "role",
    None,
    tolerant_optional_string_decoder(),
  )
  use content <- decode.optional_field(
    "content",
    None,
    tolerant_optional_string_decoder(),
  )
  decode.success(AgentEndMessage(role: role, content: content))
}

fn assistant_message_texts(messages: List(AgentEndMessage)) -> List(String) {
  list.filter_map(messages, fn(message) {
    case message.role, message.content {
      Some("assistant"), Some(content) -> non_empty(content) |> option_to_result
      _, _ -> Error(Nil)
    }
  })
}

fn option_to_result(value: Option(a)) -> Result(a, Nil) {
  case value {
    Some(value) -> Ok(value)
    None -> Error(Nil)
  }
}

fn message_object_decoder() -> decode.Decoder(MessageObject) {
  use role <- decode.optional_field(
    "role",
    None,
    tolerant_optional_string_decoder(),
  )
  use tool_name <- decode.optional_field(
    "toolName",
    None,
    tolerant_optional_string_decoder(),
  )
  use is_error <- decode.optional_field(
    "isError",
    None,
    decode.one_of(decode.optional(decode.bool), or: [
      decode.dynamic |> decode.map(fn(_) { None }),
    ]),
  )
  use content <- decode.optional_field(
    "content",
    [],
    decode.one_of(decode.list(of: content_item_decoder()), or: [
      decode.dynamic |> decode.map(fn(_) { [] }),
    ]),
  )
  decode.success(MessageObject(
    role: role,
    tool_name: tool_name,
    is_error: is_error,
    content: content,
  ))
}

fn content_item_decoder() -> decode.Decoder(ContentItem) {
  use type_ <- decode.optional_field("type", "", decode.string)
  use text <- decode.optional_field(
    "text",
    None,
    tolerant_optional_string_decoder(),
  )
  use name <- decode.optional_field(
    "name",
    None,
    tolerant_optional_string_decoder(),
  )
  use command <- decode.then(decode.optionally_at(
    ["arguments", "command"],
    None,
    structured_optional_string_decoder(),
  ))
  decode.success(ContentItem(
    type_: type_,
    text: text,
    name: name,
    command: command,
  ))
}

fn empty_message_object() -> MessageObject {
  MessageObject(role: None, tool_name: None, is_error: None, content: [])
}

fn tool_name_for_record(
  type_: String,
  message: MessageObject,
  top_tool_name_camel: Option(String),
  top_tool_name_snake: Option(String),
  top_name: Option(String),
  data_tool_name: Option(String),
) -> Option(String) {
  case message.role {
    Some("toolResult") -> first_non_empty([message.tool_name])
    Some("assistant") -> first_tool_call_name(message.content)
    _ ->
      case string.starts_with(type_, "tool_execution_") {
        True ->
          first_non_empty([
            top_tool_name_camel,
            top_tool_name_snake,
            top_name,
            data_tool_name,
          ])
        False -> data_tool_name
      }
  }
}

fn tool_input_for_record(
  message: MessageObject,
  top_command: Option(String),
  top_input: Option(String),
  top_args: Option(String),
  data_tool_input: Option(String),
) -> Option(String) {
  case message.role {
    Some("assistant") -> first_tool_call_command(message.content)
    _ -> first_non_empty([top_command, top_input, top_args, data_tool_input])
  }
}

fn tool_output_for_record(
  type_: String,
  message: MessageObject,
  top_output: Option(String),
  top_stdout: Option(String),
  top_stderr: Option(String),
  delta: Option(String),
  data_tool_output: Option(String),
) -> Option(String) {
  case message.role {
    Some("toolResult") -> first_text_content(message.content)
    _ ->
      case type_ == "tool_execution_update" {
        True ->
          first_non_empty([
            top_output,
            top_stdout,
            top_stderr,
            delta,
            data_tool_output,
          ])
        False ->
          first_non_empty([top_output, top_stdout, top_stderr, data_tool_output])
      }
  }
}

fn tool_status_for_record(
  type_: String,
  message: MessageObject,
  top_status: Option(String),
  top_result: Option(String),
  success: Option(Bool),
  data_tool_status: Option(String),
) -> Option(String) {
  case message.role {
    Some("toolResult") -> status_from_success(message.is_error)
    _ -> {
      let status = first_non_empty([top_status, top_result, data_tool_status])
      case status, string.starts_with(type_, "tool_execution_"), success {
        Some(_), _, _ -> status
        None, True, Some(True) -> Some("success")
        None, True, Some(False) -> Some("failed")
        None, _, _ -> None
      }
    }
  }
}

fn status_from_success(is_error: Option(Bool)) -> Option(String) {
  case is_error {
    Some(True) -> Some("failed")
    Some(False) -> Some("success")
    None -> None
  }
}

fn first_tool_call_name(items: List(ContentItem)) -> Option(String) {
  case items {
    [] -> None
    [item, ..rest] ->
      case item.type_ == "toolCall", item.name {
        True, Some(name) -> non_empty(name)
        _, _ -> first_tool_call_name(rest)
      }
  }
}

fn first_tool_call_command(items: List(ContentItem)) -> Option(String) {
  case items {
    [] -> None
    [item, ..rest] ->
      case item.type_ == "toolCall", item.command {
        True, Some(command) -> non_empty(command)
        _, _ -> first_tool_call_command(rest)
      }
  }
}

fn first_text_content(items: List(ContentItem)) -> Option(String) {
  case items {
    [] -> None
    [item, ..rest] ->
      case item.text {
        Some(text) ->
          case non_empty(text) {
            Some(value) -> Some(value)
            None -> first_text_content(rest)
          }
        None -> first_text_content(rest)
      }
  }
}

fn first_non_empty(values: List(Option(String))) -> Option(String) {
  case values {
    [] -> None
    [value, ..rest] ->
      case value {
        Some(text) ->
          case non_empty(text) {
            Some(text) -> Some(text)
            None -> first_non_empty(rest)
          }
        None -> first_non_empty(rest)
      }
  }
}

fn non_empty(value: String) -> Option(String) {
  let trimmed = string.trim(value)
  case trimmed == "" {
    True -> None
    False -> Some(value)
  }
}

pub type Data {
  Data(
    session_id: Option(String),
    tokens: domain.TokenTotals,
    tool_name: Option(String),
    tool_input: Option(String),
    tool_output: Option(String),
    tool_status: Option(String),
  )
}

fn empty_data() -> Data {
  Data(
    session_id: None,
    tokens: domain.zero_token_totals(),
    tool_name: None,
    tool_input: None,
    tool_output: None,
    tool_status: None,
  )
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
  use tool_name_camel <- decode.optional_field(
    "toolName",
    None,
    tolerant_optional_string_decoder(),
  )
  use tool_name_snake <- decode.optional_field(
    "tool_name",
    None,
    tolerant_optional_string_decoder(),
  )
  use name <- decode.optional_field(
    "name",
    None,
    tolerant_optional_string_decoder(),
  )
  use command <- decode.optional_field(
    "command",
    None,
    structured_optional_string_decoder(),
  )
  use input <- decode.optional_field(
    "input",
    None,
    structured_optional_string_decoder(),
  )
  use args <- decode.optional_field(
    "args",
    None,
    structured_optional_string_decoder(),
  )
  use output <- decode.optional_field(
    "output",
    None,
    tolerant_optional_string_decoder(),
  )
  use stdout <- decode.optional_field(
    "stdout",
    None,
    tolerant_optional_string_decoder(),
  )
  use stderr <- decode.optional_field(
    "stderr",
    None,
    tolerant_optional_string_decoder(),
  )
  use status <- decode.optional_field(
    "status",
    None,
    tolerant_optional_string_decoder(),
  )
  use result <- decode.optional_field(
    "result",
    None,
    tolerant_optional_string_decoder(),
  )
  decode.success(Data(
    session_id: session_id,
    tokens: tokens,
    tool_name: first_non_empty([tool_name_camel, tool_name_snake, name]),
    tool_input: first_non_empty([command, input, args]),
    tool_output: first_non_empty([output, stdout, stderr]),
    tool_status: first_non_empty([status, result]),
  ))
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
