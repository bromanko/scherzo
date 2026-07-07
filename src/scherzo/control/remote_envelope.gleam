import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/control/command
import scherzo/control/query/codec as query_codec
import scherzo/control/query/types as query_types

pub const version = 1

pub type DecodeError {
  DecodeError(code: String, message: String)
}

pub type RemoteSession {
  RemoteSession(
    session_id: String,
    display_name: String,
    issue_identifier: String,
    status: String,
    current_turn: Int,
    started_at_ms: Option(Int),
    last_event_at_ms: Int,
    activity_label: Option(String),
    current_step_id: Option(String),
    current_step_label: Option(String),
  )
}

pub type Envelope {
  RemoteHello(capabilities: List(String))
  RemoteHeartbeat(sent_at_ms: Int)
  RemoteServerCommand(command_id: String, command: command.OperatorCommand)
  RemoteQueryRequest(query_id: String, query: query_types.QueryRequest)
  RemoteCommandReceipt(
    command_id: String,
    accepted: Bool,
    message: Option(String),
  )
  RemoteCommandResult(command_id: String, result: command.CommandResult)
  RemoteQueryResponse(
    query_id: String,
    result: Result(query_types.QueryResponse, query_types.QueryError),
  )
  RemoteStateSnapshot(
    now_ms: Int,
    dispatch_paused: Bool,
    sessions: List(RemoteSession),
  )
}

type EnvelopeFields {
  EnvelopeFields(
    version: Option(Int),
    type_: Option(String),
    capabilities: Option(List(String)),
    sent_at_ms: Option(Int),
    command_id: Option(String),
    query_id: Option(String),
    accepted: Option(Bool),
    message: Option(String),
    command: Option(Dynamic),
    query: Option(Dynamic),
    result: Option(Dynamic),
    now_ms: Option(Int),
    dispatch_paused: Option(Bool),
    sessions: Option(List(RemoteSession)),
  )
}

pub fn to_json(envelope: Envelope) -> json.Json {
  case envelope {
    RemoteHello(capabilities) ->
      [
        #("capabilities", json.array(capabilities, of: json.string)),
        ..base_entries("hello")
      ]
      |> json.object
    RemoteHeartbeat(sent_at_ms) ->
      [#("sent_at_ms", json.int(sent_at_ms)), ..base_entries("heartbeat")]
      |> json.object
    RemoteServerCommand(command_id, operator_command) ->
      [
        #("command_id", json.string(command_id)),
        #("command", command.operator_command_to_json(operator_command)),
        ..base_entries("server_command")
      ]
      |> json.object
    RemoteQueryRequest(query_id, query) ->
      [
        #("query_id", json.string(query_id)),
        #("query", query_codec.request_to_json(query)),
        ..base_entries("query_request")
      ]
      |> json.object
    RemoteCommandReceipt(command_id, accepted, message) ->
      list.append(optional_message_entries(message), [
        #("accepted", json.bool(accepted)),
        #("command_id", json.string(command_id)),
        ..base_entries("command_receipt")
      ])
      |> json.object
    RemoteCommandResult(command_id, result) ->
      [
        #("command_id", json.string(command_id)),
        #("result", command.command_result_to_json(result)),
        ..base_entries("command_result")
      ]
      |> json.object
    RemoteQueryResponse(query_id, result) ->
      [
        #("query_id", json.string(query_id)),
        #("result", query_result_to_json(result)),
        ..base_entries("query_response")
      ]
      |> json.object
    RemoteStateSnapshot(now_ms, dispatch_paused, sessions) ->
      [
        #("now_ms", json.int(now_ms)),
        #("dispatch_paused", json.bool(dispatch_paused)),
        #("sessions", json.array(sessions, of: remote_session_to_json)),
        ..base_entries("state_snapshot")
      ]
      |> json.object
  }
}

pub fn to_string(envelope: Envelope) -> String {
  envelope |> to_json |> json.to_string
}

pub fn decode(line: String) -> Result(Envelope, DecodeError) {
  case json.parse(line, decode.dynamic) {
    Ok(value) -> decode_dynamic(value)
    Error(_) -> Error(DecodeError("bad_json", "malformed remote envelope JSON"))
  }
}

pub fn decode_server_command_rejection(
  line: String,
) -> Result(#(String, command.CommandResult), DecodeError) {
  case json.parse(line, decode.dynamic) {
    Ok(value) -> decode_server_command_rejection_dynamic(value)
    Error(_) -> Error(DecodeError("bad_json", "malformed remote envelope JSON"))
  }
}

pub fn decode_dynamic(value: Dynamic) -> Result(Envelope, DecodeError) {
  case decode.run(value, envelope_fields_decoder()) {
    Ok(fields) -> envelope_from_fields(fields)
    Error(_) ->
      Error(DecodeError("invalid_envelope", "invalid remote envelope"))
  }
}

fn base_entries(type_: String) -> List(#(String, json.Json)) {
  [#("version", json.int(version)), #("type", json.string(type_))]
}

fn optional_message_entries(
  message: Option(String),
) -> List(#(String, json.Json)) {
  case message {
    Some(message) -> [#("message", json.string(message))]
    None -> []
  }
}

fn optional_string_entry(
  fields: List(#(String, json.Json)),
  name: String,
  value: Option(String),
) -> List(#(String, json.Json)) {
  case value {
    Some(value) -> [#(name, json.string(value)), ..fields]
    None -> fields
  }
}

fn optional_int_entry(
  fields: List(#(String, json.Json)),
  name: String,
  value: Option(Int),
) -> List(#(String, json.Json)) {
  case value {
    Some(value) -> [#(name, json.int(value)), ..fields]
    None -> fields
  }
}

fn query_result_to_json(
  result: Result(query_types.QueryResponse, query_types.QueryError),
) -> json.Json {
  case result {
    Ok(response) -> query_codec.response_to_json(response)
    Error(error) -> query_codec.error_to_json(error)
  }
}

fn remote_session_to_json(session: RemoteSession) -> json.Json {
  [
    #("session_id", json.string(session.session_id)),
    #("display_name", json.string(session.display_name)),
    #("issue_identifier", json.string(session.issue_identifier)),
    #("status", json.string(session.status)),
    #("current_turn", json.int(session.current_turn)),
    #("last_event_at_ms", json.int(session.last_event_at_ms)),
  ]
  |> optional_int_entry("started_at_ms", session.started_at_ms)
  |> optional_string_entry("activity_label", session.activity_label)
  |> optional_string_entry("current_step_id", session.current_step_id)
  |> optional_string_entry("current_step_label", session.current_step_label)
  |> json.object
}

fn envelope_fields_decoder() -> decode.Decoder(EnvelopeFields) {
  use version <- decode.optional_field(
    "version",
    None,
    decode.optional(decode.int),
  )
  use type_ <- decode.optional_field(
    "type",
    None,
    decode.optional(decode.string),
  )
  use capabilities <- decode.optional_field(
    "capabilities",
    None,
    decode.optional(decode.list(decode.string)),
  )
  use sent_at_ms <- decode.optional_field(
    "sent_at_ms",
    None,
    decode.optional(decode.int),
  )
  use command_id <- decode.optional_field(
    "command_id",
    None,
    decode.optional(decode.string),
  )
  use query_id <- decode.optional_field(
    "query_id",
    None,
    decode.optional(decode.string),
  )
  use accepted <- decode.optional_field(
    "accepted",
    None,
    decode.optional(decode.bool),
  )
  use message <- decode.optional_field(
    "message",
    None,
    decode.optional(decode.string),
  )
  use command_value <- decode.optional_field(
    "command",
    None,
    decode.optional(decode.dynamic),
  )
  use query_value <- decode.optional_field(
    "query",
    None,
    decode.optional(decode.dynamic),
  )
  use result_value <- decode.optional_field(
    "result",
    None,
    decode.optional(decode.dynamic),
  )
  use now_ms <- decode.optional_field(
    "now_ms",
    None,
    decode.optional(decode.int),
  )
  use dispatch_paused <- decode.optional_field(
    "dispatch_paused",
    None,
    decode.optional(decode.bool),
  )
  use sessions <- decode.optional_field(
    "sessions",
    None,
    decode.optional(decode.list(remote_session_decoder())),
  )
  decode.success(EnvelopeFields(
    version: version,
    type_: type_,
    capabilities: capabilities,
    sent_at_ms: sent_at_ms,
    command_id: command_id,
    query_id: query_id,
    accepted: accepted,
    message: message,
    command: command_value,
    query: query_value,
    result: result_value,
    now_ms: now_ms,
    dispatch_paused: dispatch_paused,
    sessions: sessions,
  ))
}

fn remote_session_decoder() -> decode.Decoder(RemoteSession) {
  use session_id <- decode.field("session_id", decode.string)
  use display_name <- decode.field("display_name", decode.string)
  use issue_identifier <- decode.field("issue_identifier", decode.string)
  use status <- decode.field("status", decode.string)
  use current_turn <- decode.field("current_turn", decode.int)
  use started_at_ms <- decode.optional_field(
    "started_at_ms",
    None,
    decode.optional(decode.int),
  )
  use last_event_at_ms <- decode.field("last_event_at_ms", decode.int)
  use activity_label <- decode.optional_field(
    "activity_label",
    None,
    decode.optional(decode.string),
  )
  use current_step_id <- decode.optional_field(
    "current_step_id",
    None,
    decode.optional(decode.string),
  )
  use current_step_label <- decode.optional_field(
    "current_step_label",
    None,
    decode.optional(decode.string),
  )
  decode.success(RemoteSession(
    session_id: session_id,
    display_name: display_name,
    issue_identifier: issue_identifier,
    status: status,
    current_turn: current_turn,
    started_at_ms: started_at_ms,
    last_event_at_ms: last_event_at_ms,
    activity_label: activity_label,
    current_step_id: current_step_id,
    current_step_label: current_step_label,
  ))
}

fn envelope_from_fields(
  fields: EnvelopeFields,
) -> Result(Envelope, DecodeError) {
  use _ <- result.try(required_version(fields.version))
  use type_ <- result.try(required_type(fields.type_))
  case type_ {
    "hello" ->
      required_capabilities(fields.capabilities) |> result.map(RemoteHello)
    "heartbeat" ->
      required_int_field(fields.sent_at_ms, "sent_at_ms")
      |> result.map(RemoteHeartbeat)
    "server_command" -> {
      use command_id <- result.try(required_string_field(
        fields.command_id,
        "command_id",
      ))
      use nested <- result.try(required_dynamic_field(fields.command, "command"))
      use operator_command <- result.try(decode_nested_command(nested))
      Ok(RemoteServerCommand(command_id, operator_command))
    }
    "query_request" -> {
      use query_id <- result.try(required_string_field(
        fields.query_id,
        "query_id",
      ))
      use nested <- result.try(required_dynamic_field(fields.query, "query"))
      use query <- result.try(decode_nested_query_request(nested))
      Ok(RemoteQueryRequest(query_id, query))
    }
    "command_receipt" -> {
      use command_id <- result.try(required_string_field(
        fields.command_id,
        "command_id",
      ))
      use accepted <- result.try(required_bool_field(
        fields.accepted,
        "accepted",
      ))
      Ok(RemoteCommandReceipt(command_id, accepted, fields.message))
    }
    "command_result" -> {
      use command_id <- result.try(required_string_field(
        fields.command_id,
        "command_id",
      ))
      use nested <- result.try(required_dynamic_field(fields.result, "result"))
      use command_result <- result.try(decode_nested_result(nested))
      Ok(RemoteCommandResult(command_id, command_result))
    }
    "query_response" -> {
      use query_id <- result.try(required_string_field(
        fields.query_id,
        "query_id",
      ))
      use nested <- result.try(required_dynamic_field(fields.result, "result"))
      let query_result = decode_nested_query_response(nested)
      Ok(RemoteQueryResponse(query_id, query_result))
    }
    "state_snapshot" -> {
      use now_ms <- result.try(required_int_field(fields.now_ms, "now_ms"))
      use dispatch_paused <- result.try(required_bool_field(
        fields.dispatch_paused,
        "dispatch_paused",
      ))
      use sessions <- result.try(required_sessions(fields.sessions))
      Ok(RemoteStateSnapshot(now_ms, dispatch_paused, sessions))
    }
    _ ->
      Error(DecodeError(
        "unknown_envelope_type",
        "unknown envelope type: " <> type_,
      ))
  }
}

fn required_version(found_version: Option(Int)) -> Result(Int, DecodeError) {
  case found_version {
    Some(found) if found == version -> Ok(found)
    Some(found) ->
      Error(DecodeError(
        "unsupported_version",
        "unsupported envelope version: " <> int_to_string(found),
      ))
    None -> Error(DecodeError("invalid_envelope", "missing version"))
  }
}

fn required_type(type_: Option(String)) -> Result(String, DecodeError) {
  case type_ {
    Some(type_) -> {
      let type_ = string.trim(type_)
      case type_ == "" {
        True -> Error(DecodeError("invalid_envelope", "missing type"))
        False -> Ok(type_)
      }
    }
    None -> Error(DecodeError("invalid_envelope", "missing type"))
  }
}

fn required_capabilities(
  capabilities: Option(List(String)),
) -> Result(List(String), DecodeError) {
  case capabilities {
    Some(capabilities) -> Ok(capabilities)
    None -> Error(DecodeError("invalid_envelope", "missing capabilities"))
  }
}

fn required_sessions(
  sessions: Option(List(RemoteSession)),
) -> Result(List(RemoteSession), DecodeError) {
  case sessions {
    Some(sessions) -> Ok(sessions)
    None -> Error(DecodeError("invalid_envelope", "missing sessions"))
  }
}

fn required_string_field(
  value: Option(String),
  field_name: String,
) -> Result(String, DecodeError) {
  case value {
    Some(value) -> {
      let value = string.trim(value)
      case value == "" {
        True ->
          Error(DecodeError(
            "invalid_envelope",
            field_name <> " must not be empty",
          ))
        False -> Ok(value)
      }
    }
    None -> Error(DecodeError("invalid_envelope", "missing " <> field_name))
  }
}

fn required_int_field(
  value: Option(Int),
  field_name: String,
) -> Result(Int, DecodeError) {
  case value {
    Some(value) -> Ok(value)
    None -> Error(DecodeError("invalid_envelope", "missing " <> field_name))
  }
}

fn required_bool_field(
  value: Option(Bool),
  field_name: String,
) -> Result(Bool, DecodeError) {
  case value {
    Some(value) -> Ok(value)
    None -> Error(DecodeError("invalid_envelope", "missing " <> field_name))
  }
}

fn required_dynamic_field(
  value: Option(Dynamic),
  field_name: String,
) -> Result(Dynamic, DecodeError) {
  case value {
    Some(value) -> Ok(value)
    None -> Error(DecodeError("invalid_envelope", "missing " <> field_name))
  }
}

fn decode_nested_command(
  value: Dynamic,
) -> Result(command.OperatorCommand, DecodeError) {
  case command.decode_operator_command_dynamic(value) {
    Ok(operator_command) -> Ok(operator_command)
    Error(command.CodecError(code: code, message: message)) ->
      Error(DecodeError(code, message))
  }
}

fn decode_nested_query_request(
  value: Dynamic,
) -> Result(query_types.QueryRequest, DecodeError) {
  case query_codec.decode_request_dynamic(value) {
    Ok(query) -> Ok(query)
    Error(query_types.QueryError(code: code, message: message)) ->
      Error(DecodeError(query_types.error_code_to_string(code), message))
  }
}

fn decode_nested_result(
  value: Dynamic,
) -> Result(command.CommandResult, DecodeError) {
  case command.decode_command_result_dynamic(value) {
    Ok(command_result) -> Ok(command_result)
    Error(command.CodecError(code: code, message: message)) ->
      Error(DecodeError(code, message))
  }
}

fn decode_nested_query_response(
  value: Dynamic,
) -> Result(query_types.QueryResponse, query_types.QueryError) {
  query_codec.decode_response_dynamic(value)
}

fn decode_server_command_rejection_dynamic(
  value: Dynamic,
) -> Result(#(String, command.CommandResult), DecodeError) {
  case decode.run(value, envelope_fields_decoder()) {
    Ok(fields) -> server_command_rejection_from_fields(fields)
    Error(_) ->
      Error(DecodeError("invalid_envelope", "invalid remote envelope"))
  }
}

fn server_command_rejection_from_fields(
  fields: EnvelopeFields,
) -> Result(#(String, command.CommandResult), DecodeError) {
  use _ <- result.try(required_version(fields.version))
  use type_ <- result.try(required_type(fields.type_))
  case type_ {
    "server_command" -> {
      use command_id <- result.try(required_string_field(
        fields.command_id,
        "command_id",
      ))
      case required_dynamic_field(fields.command, "command") {
        Ok(nested) ->
          case decode_nested_command(nested) {
            Ok(_) ->
              Error(DecodeError(
                "valid_server_command",
                "server_command payload is valid",
              ))
            Error(err) ->
              Ok(#(
                command_id,
                rejected_decode_result(command_name_from_dynamic(nested), err),
              ))
          }
        Error(err) -> Ok(#(command_id, rejected_decode_result("unknown", err)))
      }
    }
    _ ->
      Error(DecodeError(
        "not_server_command",
        "remote envelope is not a server_command",
      ))
  }
}

fn rejected_decode_result(
  command_name: String,
  error: DecodeError,
) -> command.CommandResult {
  let DecodeError(code: code, message: message) = error
  command.CommandResult(
    command: normalize_command_name(command_name),
    status: command.Rejected(code),
    target: None,
    message: Some(message),
    operation_id: None,
  )
}

fn command_name_from_dynamic(value: Dynamic) -> String {
  case decode.run(value, command_type_decoder()) {
    Ok(Some(command_name)) -> normalize_command_name(command_name)
    Ok(None) -> "unknown"
    Error(_) -> "unknown"
  }
}

fn command_type_decoder() -> decode.Decoder(Option(String)) {
  use type_ <- decode.optional_field(
    "type",
    None,
    decode.optional(decode.string),
  )
  decode.success(type_)
}

fn normalize_command_name(command_name: String) -> String {
  let command_name = string.trim(command_name)
  case command_name == "" {
    True -> "unknown"
    False -> command_name
  }
}

fn int_to_string(value: Int) -> String {
  value |> json.int |> json.to_string
}
