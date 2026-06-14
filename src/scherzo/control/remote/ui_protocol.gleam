import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/control/command
import scherzo/session/event

pub type SessionSnapshot {
  SessionSnapshot(
    session_id: String,
    display_name: String,
    issue_identifier: String,
    status: String,
    current_turn: Int,
    last_event_at_ms: Int,
  )
}

pub type ClientMessage {
  DaemonHello(daemon_id: String, boot_id: String, daemon_label: Option(String))
  Heartbeat(sent_at_ms: Int, daemon_label: Option(String))
  DaemonState(
    sent_at_ms: Int,
    dispatch_paused: Bool,
    daemon_label: Option(String),
    sessions: List(SessionSnapshot),
  )
  CommandResult(server_command_id: String, result: command.CommandResult)
}

pub type ServerMessage {
  ServerHello(heartbeat_interval_ms: Option(Int))
  CredentialRevoked(reason: String)
  DaemonIdentityRevoked(reason: String)
  ServerCommand(
    server_command_id: String,
    daemon_id: String,
    boot_id: String,
    command: command.OperatorCommand,
  )
  UnknownServerMessage(String)
}

pub type DecodeError {
  DecodeError(code: String, message: String)
}

type ServerMessageFields {
  ServerMessageFields(
    type_: Option(String),
    heartbeat_interval_ms: Option(Int),
    reason: Option(String),
    server_command_id: Option(String),
    daemon_id: Option(String),
    boot_id: Option(String),
    command: Option(Dynamic),
  )
}

pub fn session_from_summary(summary: event.SessionSummary) -> SessionSnapshot {
  SessionSnapshot(
    session_id: summary.session_id,
    display_name: summary.display_name,
    issue_identifier: summary.issue_identifier,
    status: event.status_to_string(summary.status),
    current_turn: summary.current_turn,
    last_event_at_ms: summary.last_event_at_ms,
  )
}

pub fn encode_client_message(message: ClientMessage) -> String {
  case message {
    DaemonHello(daemon_id, boot_id, daemon_label) ->
      encode_daemon_hello(daemon_id, boot_id, daemon_label)
    Heartbeat(sent_at_ms, daemon_label) ->
      encode_heartbeat(sent_at_ms, daemon_label)
    DaemonState(sent_at_ms, dispatch_paused, daemon_label, sessions) ->
      encode_daemon_state(sent_at_ms, dispatch_paused, daemon_label, sessions)
    CommandResult(server_command_id, result) ->
      encode_command_result(server_command_id, result)
  }
}

pub fn encode_daemon_hello(
  daemon_id: String,
  boot_id: String,
  daemon_label: Option(String),
) -> String {
  [
    #("type", json.string("daemon_hello")),
    #("daemonId", json.string(daemon_id)),
    #("bootId", json.string(boot_id)),
  ]
  |> with_optional_daemon_label(daemon_label)
  |> json.object
  |> json.to_string
}

pub fn encode_heartbeat(
  sent_at_ms: Int,
  daemon_label: Option(String),
) -> String {
  [
    #("type", json.string("heartbeat")),
    #("sentAtMs", json.int(sent_at_ms)),
  ]
  |> with_optional_daemon_label(daemon_label)
  |> json.object
  |> json.to_string
}

pub fn encode_daemon_state(
  sent_at_ms: Int,
  dispatch_paused: Bool,
  daemon_label: Option(String),
  sessions: List(SessionSnapshot),
) -> String {
  [
    #("type", json.string("daemon_state")),
    #("sentAtMs", json.int(sent_at_ms)),
    #("dispatchPaused", json.bool(dispatch_paused)),
    #("sessions", json.array(sessions, of: session_to_json)),
  ]
  |> with_optional_daemon_label(daemon_label)
  |> json.object
  |> json.to_string
}

pub fn encode_command_result(
  server_command_id: String,
  command_result: command.CommandResult,
) -> String {
  json.object([
    #("type", json.string("command_result")),
    #("serverCommandId", json.string(server_command_id)),
    #("result", command.command_result_to_json(command_result)),
  ])
  |> json.to_string
}

fn with_optional_daemon_label(
  fields: List(#(String, json.Json)),
  daemon_label: Option(String),
) -> List(#(String, json.Json)) {
  case daemon_label {
    Some(label) -> [#("daemonLabel", json.string(label)), ..fields]
    None -> fields
  }
}

pub fn decode_server_message(
  payload: String,
) -> Result(ServerMessage, DecodeError) {
  case json.parse(payload, decode.dynamic) {
    Ok(value) -> decode_server_message_dynamic(value)
    Error(_) -> Error(DecodeError("bad_json", "malformed UI websocket JSON"))
  }
}

pub fn decode_server_command_rejection(
  payload: String,
) -> Result(#(String, command.CommandResult), DecodeError) {
  case json.parse(payload, decode.dynamic) {
    Ok(value) -> decode_server_command_rejection_dynamic(value)
    Error(_) -> Error(DecodeError("bad_json", "malformed UI websocket JSON"))
  }
}

fn decode_server_message_dynamic(
  value: Dynamic,
) -> Result(ServerMessage, DecodeError) {
  case decode.run(value, server_message_fields_decoder()) {
    Ok(fields) -> server_message_from_fields(fields)
    Error(_) ->
      Error(DecodeError("invalid_message", "invalid UI websocket message"))
  }
}

fn server_message_fields_decoder() -> decode.Decoder(ServerMessageFields) {
  use type_ <- decode.optional_field(
    "type",
    None,
    decode.optional(decode.string),
  )
  use heartbeat_interval_ms <- decode.optional_field(
    "heartbeatIntervalMs",
    None,
    decode.optional(decode.int),
  )
  use reason <- decode.optional_field(
    "reason",
    None,
    decode.optional(decode.string),
  )
  use server_command_id <- decode.optional_field(
    "serverCommandId",
    None,
    decode.optional(decode.string),
  )
  use daemon_id <- decode.optional_field(
    "daemonId",
    None,
    decode.optional(decode.string),
  )
  use boot_id <- decode.optional_field(
    "bootId",
    None,
    decode.optional(decode.string),
  )
  use command_value <- decode.optional_field(
    "command",
    None,
    decode.optional(decode.dynamic),
  )
  decode.success(ServerMessageFields(
    type_: type_,
    heartbeat_interval_ms: heartbeat_interval_ms,
    reason: reason,
    server_command_id: server_command_id,
    daemon_id: daemon_id,
    boot_id: boot_id,
    command: command_value,
  ))
}

fn server_message_from_fields(
  fields: ServerMessageFields,
) -> Result(ServerMessage, DecodeError) {
  use type_ <- result.try(required_type(fields.type_))
  case type_ {
    "server_hello" -> Ok(ServerHello(fields.heartbeat_interval_ms))
    "credential_revoked" ->
      Ok(CredentialRevoked(option_string(fields.reason, "credential revoked")))
    "daemon_identity_revoked" ->
      Ok(
        DaemonIdentityRevoked(option_string(
          fields.reason,
          "daemon identity revoked",
        )),
      )
    "server_command" -> {
      use server_command_id <- result.try(required_string_field(
        fields.server_command_id,
        "serverCommandId",
      ))
      use daemon_id <- result.try(required_string_field(
        fields.daemon_id,
        "daemonId",
      ))
      use boot_id <- result.try(required_string_field(fields.boot_id, "bootId"))
      use nested <- result.try(required_dynamic_field(fields.command, "command"))
      use operator_command <- result.try(decode_nested_command(nested))
      Ok(ServerCommand(server_command_id, daemon_id, boot_id, operator_command))
    }
    other -> Ok(UnknownServerMessage(other))
  }
}

fn option_string(value: Option(String), default: String) -> String {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn decode_server_command_rejection_dynamic(
  value: Dynamic,
) -> Result(#(String, command.CommandResult), DecodeError) {
  case decode.run(value, server_message_fields_decoder()) {
    Ok(fields) -> server_command_rejection_from_fields(fields)
    Error(_) ->
      Error(DecodeError("invalid_message", "invalid UI websocket message"))
  }
}

fn server_command_rejection_from_fields(
  fields: ServerMessageFields,
) -> Result(#(String, command.CommandResult), DecodeError) {
  use type_ <- result.try(required_type(fields.type_))
  case type_ {
    "server_command" -> {
      use server_command_id <- result.try(required_string_field(
        fields.server_command_id,
        "serverCommandId",
      ))
      let command_name = command_name_from_optional_dynamic(fields.command)
      case required_string_field(fields.daemon_id, "daemonId") {
        Error(error) ->
          Ok(#(server_command_id, rejected_decode_result(command_name, error)))
        Ok(_) ->
          case required_string_field(fields.boot_id, "bootId") {
            Error(error) ->
              Ok(#(
                server_command_id,
                rejected_decode_result(command_name, error),
              ))
            Ok(_) ->
              case required_dynamic_field(fields.command, "command") {
                Ok(nested) ->
                  case decode_nested_command(nested) {
                    Ok(_) ->
                      Error(DecodeError(
                        "valid_server_command",
                        "server_command payload is valid",
                      ))
                    Error(error) ->
                      Ok(#(
                        server_command_id,
                        rejected_decode_result(
                          command_name_from_dynamic(nested),
                          error,
                        ),
                      ))
                  }
                Error(error) ->
                  Ok(#(
                    server_command_id,
                    rejected_decode_result("unknown", error),
                  ))
              }
          }
      }
    }
    _ ->
      Error(DecodeError(
        "not_server_command",
        "UI websocket message is not a server_command",
      ))
  }
}

fn required_type(type_: Option(String)) -> Result(String, DecodeError) {
  case type_ {
    Some(type_) -> {
      let type_ = string.trim(type_)
      case type_ == "" {
        True -> Error(DecodeError("invalid_message", "missing type"))
        False -> Ok(type_)
      }
    }
    None -> Error(DecodeError("invalid_message", "missing type"))
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
            "invalid_message",
            field_name <> " must not be empty",
          ))
        False -> Ok(value)
      }
    }
    None -> Error(DecodeError("invalid_message", "missing " <> field_name))
  }
}

fn required_dynamic_field(
  value: Option(Dynamic),
  field_name: String,
) -> Result(Dynamic, DecodeError) {
  case value {
    Some(value) -> Ok(value)
    None -> Error(DecodeError("invalid_message", "missing " <> field_name))
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
  )
}

fn command_name_from_optional_dynamic(value: Option(Dynamic)) -> String {
  case value {
    Some(value) -> command_name_from_dynamic(value)
    None -> "unknown"
  }
}

fn command_name_from_dynamic(value: Dynamic) -> String {
  case decode.run(value, command_type_decoder()) {
    Ok(Some(command_name)) -> normalize_command_name(command_name)
    Ok(None) -> "unknown"
    Error(error) -> unknown_command_name_for_decode_error(error)
  }
}

fn unknown_command_name_for_decode_error(_error: a) -> String {
  "unknown"
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

fn session_to_json(session: SessionSnapshot) -> json.Json {
  json.object([
    #("sessionId", json.string(session.session_id)),
    #("displayName", json.string(session.display_name)),
    #("issueIdentifier", json.string(session.issue_identifier)),
    #("status", json.string(session.status)),
    #("currentTurn", json.int(session.current_turn)),
    #("lastEventAtMs", json.int(session.last_event_at_ms)),
  ])
}
