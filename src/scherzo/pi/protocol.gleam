import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/error
import scherzo/session/tokens as session_tokens

pub type RpcRecord {
  RpcRecord(
    type_: String,
    id: Option(String),
    command: Option(String),
    success: Option(Bool),
    session_id: Option(String),
    session_file: Option(String),
    cwd: Option(String),
    delta: Option(String),
    assistant_event_type: Option(String),
    message: Option(String),
    method: Option(String),
    tokens: session_tokens.TokenTotals,
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
  use top_delta <- decode.optional_field(
    "delta",
    None,
    decode.optional(decode.string),
  )
  use assistant_event_type <- decode.then(decode.optionally_at(
    ["assistantMessageEvent", "type"],
    None,
    decode.optional(decode.string),
  ))
  use assistant_event_delta <- decode.then(decode.optionally_at(
    ["assistantMessageEvent", "delta"],
    None,
    decode.optional(decode.string),
  ))
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
  let delta =
    first_non_empty([
      top_delta,
      text_assistant_event_delta(assistant_event_type, assistant_event_delta),
    ])
  let assistant_messages =
    list.append(
      assistant_messages,
      completed_assistant_message_texts(type_, message_object),
    )
  decode.success(RpcRecord(
    type_: type_,
    id: id,
    command: command,
    success: success,
    session_id: data.session_id,
    session_file: data.session_file,
    cwd: data.cwd,
    delta: delta,
    assistant_event_type: assistant_event_type,
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
    agent_end_content_decoder(),
  )
  decode.success(AgentEndMessage(role: role, content: content))
}

fn agent_end_content_decoder() -> decode.Decoder(Option(String)) {
  decode.one_of(decode.optional(decode.string), or: [
    decode.list(of: content_item_decoder())
      |> decode.map(all_text_content),
    decode.dynamic |> decode.map(fn(_) { None }),
  ])
}

fn text_assistant_event_delta(
  event_type: Option(String),
  delta: Option(String),
) -> Option(String) {
  case event_type, delta {
    Some("text_delta"), Some(value) -> Some(value)
    _, _ -> None
  }
}

fn completed_assistant_message_texts(
  event_type: String,
  message: MessageObject,
) -> List(String) {
  case event_type {
    "message" -> assistant_message_text(message)
    "message_end" -> assistant_message_text(message)
    "turn_end" -> assistant_message_text(message)
    _ -> []
  }
}

fn assistant_message_text(message: MessageObject) -> List(String) {
  case message.role, all_text_content(message.content) {
    Some("assistant"), Some(content) -> [content]
    _, _ -> []
  }
}

fn all_text_content(items: List(ContentItem)) -> Option(String) {
  let texts =
    list.filter_map(items, fn(item) {
      case item.text {
        Some(text) -> non_empty(text) |> option_to_result
        None -> Error(Nil)
      }
    })
  case texts {
    [] -> None
    _ -> Some(string.join(texts, with: "\n"))
  }
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
    session_file: Option(String),
    cwd: Option(String),
    tokens: session_tokens.TokenTotals,
    tool_name: Option(String),
    tool_input: Option(String),
    tool_output: Option(String),
    tool_status: Option(String),
  )
}

fn empty_data() -> Data {
  Data(
    session_id: None,
    session_file: None,
    cwd: None,
    tokens: session_tokens.zero_token_totals(),
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
    tolerant_optional_string_decoder(),
  )
  use session_file <- decode.optional_field(
    "sessionFile",
    None,
    tolerant_optional_string_decoder(),
  )
  use cwd <- decode.optional_field(
    "cwd",
    None,
    tolerant_optional_string_decoder(),
  )
  use tokens <- decode.optional_field(
    "tokens",
    session_tokens.zero_token_totals(),
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
    session_file: session_file,
    cwd: cwd,
    tokens: tokens,
    tool_name: first_non_empty([tool_name_camel, tool_name_snake, name]),
    tool_input: first_non_empty([command, input, args]),
    tool_output: first_non_empty([output, stdout, stderr]),
    tool_status: first_non_empty([status, result]),
  ))
}

fn tokens_decoder() -> decode.Decoder(session_tokens.TokenTotals) {
  use input <- decode.optional_field("input", 0, decode.int)
  use output <- decode.optional_field("output", 0, decode.int)
  use cache_read <- decode.optional_field("cacheRead", 0, decode.int)
  use cache_write <- decode.optional_field("cacheWrite", 0, decode.int)
  use total <- decode.optional_field("total", 0, decode.int)
  decode.success(session_tokens.TokenTotals(
    input: input,
    output: output,
    cache_read: cache_read,
    cache_write: cache_write,
    total: total,
  ))
}
