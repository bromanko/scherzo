import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/control/command
import scherzo/control/query/codec as query_codec
import scherzo/control/query/types as query_types
import scherzo/managed_launch/grant as managed_launch_grant
import scherzo/session/event
import scherzo/work_item_invalidation

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

pub type AgentSlotState {
  AgentSlotState(capacity: Int, active: Int, used: Int, known: Bool)
}

pub type DaemonRuntimeState {
  DaemonRuntimeState(
    host: String,
    version: String,
    daemon_label: Option(String),
    agent_slots: AgentSlotState,
  )
}

pub type ManagedLaunchContext {
  ManagedLaunchContext(
    launch_id: String,
    capabilities: List(managed_launch_grant.Capability),
  )
}

pub type RuntimeMetadata {
  RuntimeMetadata(
    host: String,
    scherzo_version: String,
    daemon_label: Option(String),
    agent_slot_capacity: Int,
    managed_launch_context: Option(ManagedLaunchContext),
  )
}

pub type DaemonEvent {
  DaemonEvent(kind: String, type_: String, message: String)
}

pub type RunningQuery(timer) {
  RunningQuery(
    query_id: String,
    worker: process.Pid,
    generation: Int,
    timer: timer,
  )
}

pub type ClientMessage {
  DaemonHello(
    daemon_id: String,
    boot_id: String,
    daemon_label: Option(String),
    state: DaemonRuntimeState,
  )
  Heartbeat(
    sent_at_ms: Int,
    daemon_label: Option(String),
    state: DaemonRuntimeState,
    event: Option(DaemonEvent),
  )
  DaemonState(
    sent_at_ms: Int,
    dispatch_paused: Bool,
    daemon_label: Option(String),
    state: DaemonRuntimeState,
    sessions: List(SessionSnapshot),
  )
  CommandResult(server_command_id: String, result: command.CommandResult)
  QueryResponse(
    query_id: String,
    result: Result(query_types.QueryResponse, query_types.QueryError),
  )
  WorkItemInvalidation(
    daemon_id: String,
    boot_id: String,
    sent_at_ms: Int,
    daemon_label: Option(String),
    event: work_item_invalidation.Event,
  )
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
  QueryRequest(
    query_id: String,
    daemon_id: String,
    boot_id: String,
    query: query_types.QueryRequest,
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
    query_id: Option(String),
    query: Option(Dynamic),
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

pub fn runtime_daemon_label(metadata: RuntimeMetadata) -> Option(String) {
  metadata.daemon_label
}

pub fn runtime_managed_launch_context(
  metadata: RuntimeMetadata,
) -> Option(ManagedLaunchContext) {
  metadata.managed_launch_context
}

pub fn pop_running_query(
  running_queries: List(RunningQuery(timer)),
  worker: process.Pid,
) -> Result(#(RunningQuery(timer), List(RunningQuery(timer))), Nil) {
  pop_running_query_loop(running_queries, worker, [])
}

fn pop_running_query_loop(
  remaining: List(RunningQuery(timer)),
  worker: process.Pid,
  acc: List(RunningQuery(timer)),
) -> Result(#(RunningQuery(timer), List(RunningQuery(timer))), Nil) {
  case remaining {
    [] -> Error(Nil)
    [entry, ..rest] ->
      case entry.worker == worker {
        True -> Ok(#(entry, list.append(list.reverse(acc), rest)))
        False -> pop_running_query_loop(rest, worker, [entry, ..acc])
      }
  }
}

pub fn encode_daemon_hello_with_runtime(
  daemon_id: String,
  boot_id: String,
  metadata: RuntimeMetadata,
  agent_slot_occupancy_result: Result(Int, a),
) -> String {
  encode_daemon_hello(
    daemon_id,
    boot_id,
    metadata.daemon_label,
    metadata.managed_launch_context,
    runtime_state_from_agent_slot_result(metadata, agent_slot_occupancy_result),
  )
}

pub fn encode_heartbeat_with_runtime(
  sent_at_ms: Int,
  metadata: RuntimeMetadata,
  agent_slot_occupancy_result: Result(Int, a),
) -> String {
  encode_heartbeat_with_state(
    sent_at_ms,
    metadata,
    runtime_state_from_agent_slot_result(metadata, agent_slot_occupancy_result),
  )
}

pub fn encode_heartbeat_with_state(
  sent_at_ms: Int,
  metadata: RuntimeMetadata,
  state: DaemonRuntimeState,
) -> String {
  encode_heartbeat(
    sent_at_ms,
    metadata.daemon_label,
    state,
    Some(heartbeat_event()),
  )
}

pub fn encode_daemon_state_with_runtime(
  sent_at_ms: Int,
  dispatch_paused: Bool,
  metadata: RuntimeMetadata,
  agent_slot_occupancy_result: Result(Int, a),
  snapshots: List(SessionSnapshot),
) -> String {
  encode_daemon_state(
    sent_at_ms,
    dispatch_paused,
    metadata.daemon_label,
    runtime_state_from_agent_slot_result(metadata, agent_slot_occupancy_result),
    snapshots,
  )
}

pub fn encode_client_message(message: ClientMessage) -> String {
  case message {
    DaemonHello(daemon_id, boot_id, daemon_label, state) ->
      encode_daemon_hello(daemon_id, boot_id, daemon_label, None, state)
    Heartbeat(sent_at_ms, daemon_label, state, event) ->
      encode_heartbeat(sent_at_ms, daemon_label, state, event)
    DaemonState(sent_at_ms, dispatch_paused, daemon_label, state, sessions) ->
      encode_daemon_state(
        sent_at_ms,
        dispatch_paused,
        daemon_label,
        state,
        sessions,
      )
    CommandResult(server_command_id, result) ->
      encode_command_result(server_command_id, result)
    QueryResponse(query_id, result) -> encode_query_response(query_id, result)
    WorkItemInvalidation(daemon_id, boot_id, sent_at_ms, daemon_label, event) ->
      encode_work_item_invalidation(
        daemon_id,
        boot_id,
        sent_at_ms,
        daemon_label,
        event,
      )
  }
}

pub fn encode_daemon_hello(
  daemon_id: String,
  boot_id: String,
  daemon_label: Option(String),
  managed_launch_context: Option(ManagedLaunchContext),
  state: DaemonRuntimeState,
) -> String {
  [
    #("type", json.string("daemon_hello")),
    #("daemonId", json.string(daemon_id)),
    #("bootId", json.string(boot_id)),
    #("state", daemon_runtime_state_to_json(state)),
  ]
  |> with_optional_managed_launch_context(managed_launch_context)
  |> with_optional_daemon_label(daemon_label)
  |> json.object
  |> json.to_string
}

pub fn encode_heartbeat(
  sent_at_ms: Int,
  daemon_label: Option(String),
  state: DaemonRuntimeState,
  event: Option(DaemonEvent),
) -> String {
  [
    #("type", json.string("heartbeat")),
    #("sentAtMs", json.int(sent_at_ms)),
    #("state", daemon_runtime_state_to_json(state)),
  ]
  |> with_optional_daemon_event(event)
  |> with_optional_daemon_label(daemon_label)
  |> json.object
  |> json.to_string
}

pub fn encode_daemon_state(
  sent_at_ms: Int,
  dispatch_paused: Bool,
  daemon_label: Option(String),
  state: DaemonRuntimeState,
  sessions: List(SessionSnapshot),
) -> String {
  [
    #("type", json.string("daemon_state")),
    #("sentAtMs", json.int(sent_at_ms)),
    #("dispatchPaused", json.bool(dispatch_paused)),
    #("state", daemon_runtime_state_to_json(state)),
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

pub fn encode_query_response(
  query_id: String,
  result: Result(query_types.QueryResponse, query_types.QueryError),
) -> String {
  let result_json = case result {
    Ok(response) -> query_codec.response_to_json(response)
    Error(error) -> query_codec.error_to_json(error)
  }

  json.object([
    #("type", json.string("query_response")),
    #("queryId", json.string(query_id)),
    #("result", result_json),
  ])
  |> json.to_string
}

pub fn encode_work_item_invalidation(
  daemon_id: String,
  boot_id: String,
  sent_at_ms: Int,
  daemon_label: Option(String),
  event: work_item_invalidation.Event,
) -> String {
  [
    #("type", json.string("work_item_invalidation")),
    #("daemonId", json.string(daemon_id)),
    #("bootId", json.string(boot_id)),
    #("sentAtMs", json.int(sent_at_ms)),
    #("source", json.string(invalidation_source_to_string(event.source))),
    #("taskRefs", json.array(event.task_refs, of: task_ref_to_json)),
    #("hasUnknownRefs", json.bool(event.has_unknown_refs)),
    #("refsTruncated", json.bool(event.refs_truncated)),
  ]
  |> with_optional_daemon_label(daemon_label)
  |> json.object
  |> json.to_string
}

fn invalidation_source_to_string(
  source: work_item_invalidation.Source,
) -> String {
  case source {
    work_item_invalidation.PollRefresh -> "poll_refresh"
    work_item_invalidation.TrackerRefresh -> "tracker_refresh"
    work_item_invalidation.WorkflowObserved -> "workflow_observed"
    work_item_invalidation.ManualRefresh -> "manual_refresh"
  }
}

fn task_ref_to_json(ref: work_item_invalidation.AffectedTaskRef) -> json.Json {
  let fields = [
    #("provider", json.string(ref.provider)),
    #("id", json.string(ref.id)),
  ]
  case ref.display_id {
    Some(display_id) ->
      [#("displayId", json.string(display_id)), ..fields]
      |> json.object
    None -> json.object(fields)
  }
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

fn with_optional_managed_launch_context(
  fields: List(#(String, json.Json)),
  managed_launch_context: Option(ManagedLaunchContext),
) -> List(#(String, json.Json)) {
  case managed_launch_context {
    Some(ManagedLaunchContext(launch_id, capabilities)) -> [
      #("launchId", json.string(launch_id)),
      #(
        "capabilities",
        json.array(
          managed_launch_grant.capabilities_to_strings(capabilities),
          of: json.string,
        ),
      ),
      ..fields
    ]
    None -> fields
  }
}

fn with_optional_daemon_event(
  fields: List(#(String, json.Json)),
  event: Option(DaemonEvent),
) -> List(#(String, json.Json)) {
  case event {
    Some(event) -> [#("event", daemon_event_to_json(event)), ..fields]
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

pub fn decode_query_request_rejection(
  payload: String,
) -> Result(#(String, query_types.QueryError), DecodeError) {
  case json.parse(payload, decode.dynamic) {
    Ok(value) -> decode_query_request_rejection_dynamic(value)
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
  use query_id <- decode.optional_field(
    "queryId",
    None,
    decode.optional(decode.string),
  )
  use query_value <- decode.optional_field(
    "query",
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
    query_id: query_id,
    query: query_value,
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
    "query_request" -> {
      use query_id <- result.try(required_string_field(
        fields.query_id,
        "queryId",
      ))
      use daemon_id <- result.try(required_string_field(
        fields.daemon_id,
        "daemonId",
      ))
      use boot_id <- result.try(required_string_field(fields.boot_id, "bootId"))
      use nested <- result.try(required_dynamic_field(fields.query, "query"))
      use query <- result.try(decode_nested_query_as_decode_error(nested))
      Ok(QueryRequest(query_id, daemon_id, boot_id, query))
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

fn decode_query_request_rejection_dynamic(
  value: Dynamic,
) -> Result(#(String, query_types.QueryError), DecodeError) {
  case decode.run(value, server_message_fields_decoder()) {
    Ok(fields) -> query_request_rejection_from_fields(fields)
    Error(_) ->
      Error(DecodeError("invalid_message", "invalid UI websocket message"))
  }
}

fn query_request_rejection_from_fields(
  fields: ServerMessageFields,
) -> Result(#(String, query_types.QueryError), DecodeError) {
  use type_ <- result.try(required_type(fields.type_))
  case type_ {
    "query_request" -> {
      use query_id <- result.try(required_string_field(
        fields.query_id,
        "queryId",
      ))
      case required_string_field(fields.daemon_id, "daemonId") {
        Error(error) -> Ok(#(query_id, query_error_from_decode_error(error)))
        Ok(_) ->
          case required_string_field(fields.boot_id, "bootId") {
            Error(error) ->
              Ok(#(query_id, query_error_from_decode_error(error)))
            Ok(_) ->
              case required_dynamic_field(fields.query, "query") {
                Ok(nested) ->
                  case decode_nested_query(nested) {
                    Ok(_) ->
                      Error(DecodeError(
                        "valid_query_request",
                        "query_request payload is valid",
                      ))
                    Error(error) -> Ok(#(query_id, error))
                  }
                Error(error) ->
                  Ok(#(query_id, query_error_from_decode_error(error)))
              }
          }
      }
    }
    _ ->
      Error(DecodeError(
        "not_query_request",
        "UI websocket message is not a query_request",
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

fn decode_nested_query(
  value: Dynamic,
) -> Result(query_types.QueryRequest, query_types.QueryError) {
  query_codec.decode_request_dynamic(value)
}

fn decode_nested_query_as_decode_error(
  value: Dynamic,
) -> Result(query_types.QueryRequest, DecodeError) {
  case decode_nested_query(value) {
    Ok(query) -> Ok(query)
    Error(query_types.QueryError(code: code, message: message)) ->
      Error(DecodeError(query_types.error_code_to_string(code), message))
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

fn query_error_from_decode_error(error: DecodeError) -> query_types.QueryError {
  let DecodeError(message: message, ..) = error
  query_types.QueryError(query_types.QueryBackendFailed, message)
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

pub fn runtime_state_from_agent_slot_occupancy(
  metadata: RuntimeMetadata,
  occupied_slots: Int,
) -> DaemonRuntimeState {
  let occupied_slots = normalize_occupied_agent_slots(occupied_slots)
  DaemonRuntimeState(
    metadata.host,
    metadata.scherzo_version,
    metadata.daemon_label,
    AgentSlotState(
      normalize_agent_slot_capacity(metadata.agent_slot_capacity),
      occupied_slots,
      occupied_slots,
      True,
    ),
  )
}

pub fn runtime_state_from_agent_slot_result(
  metadata: RuntimeMetadata,
  agent_slot_occupancy_result: Result(Int, a),
) -> DaemonRuntimeState {
  case agent_slot_occupancy_result {
    Ok(occupied_slots) ->
      runtime_state_from_agent_slot_occupancy(metadata, occupied_slots)
    // nolint: thrown_away_error -- occupancy failure is represented explicitly as agentSlots.known=false.
    Error(_) -> runtime_state_with_unknown_agent_slots(metadata)
  }
}

pub fn runtime_state_with_unknown_agent_slots(
  metadata: RuntimeMetadata,
) -> DaemonRuntimeState {
  DaemonRuntimeState(
    metadata.host,
    metadata.scherzo_version,
    metadata.daemon_label,
    AgentSlotState(
      normalize_agent_slot_capacity(metadata.agent_slot_capacity),
      0,
      0,
      False,
    ),
  )
}

fn heartbeat_event() -> DaemonEvent {
  DaemonEvent("lifecycle", "heartbeat", "daemon heartbeat")
}

fn normalize_occupied_agent_slots(value: Int) -> Int {
  case value < 0 {
    True -> 0
    False -> value
  }
}

fn normalize_agent_slot_capacity(value: Int) -> Int {
  case value < 0 {
    True -> 0
    False -> value
  }
}

fn daemon_runtime_state_to_json(state: DaemonRuntimeState) -> json.Json {
  [
    #("schemaVersion", json.int(1)),
    #("host", json.string(state.host)),
    #("version", json.string(state.version)),
    #("agentSlots", agent_slots_to_json(state.agent_slots)),
  ]
  |> with_optional_daemon_label(state.daemon_label)
  |> json.object
}

fn agent_slots_to_json(slots: AgentSlotState) -> json.Json {
  json.object([
    #("capacity", json.int(slots.capacity)),
    #("active", json.int(slots.active)),
    #("used", json.int(slots.used)),
    #("known", json.bool(slots.known)),
  ])
}

fn daemon_event_to_json(event: DaemonEvent) -> json.Json {
  json.object([
    #("kind", json.string(event.kind)),
    #("type", json.string(event.type_)),
    #("message", json.string(event.message)),
  ])
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
