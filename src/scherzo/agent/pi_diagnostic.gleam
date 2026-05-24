import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/pi_event
import scherzo/agent/types
import scherzo/config as config_module
import scherzo/config/types as config_types
import scherzo/error
import scherzo/json_value.{type JsonValue}
import scherzo/log
import scherzo/pi/client
import scherzo/pi/protocol
import scherzo/session/tokens as session_tokens
import simplifile

pub type State {
  State(
    turn_end_seen: Bool,
    agent_end_seen: Bool,
    suspicious_empty_assistant_seen: Bool,
    last_event: Option(String),
  )
}

const max_diagnostic_chars = 4096

const max_detail_chars = 512

pub fn initial_state() -> State {
  State(
    turn_end_seen: False,
    agent_end_seen: False,
    suspicious_empty_assistant_seen: False,
    last_event: None,
  )
}

pub fn observe_record(state: State, record: protocol.RpcRecord) -> State {
  State(
    turn_end_seen: state.turn_end_seen || record.type_ == "turn_end",
    agent_end_seen: state.agent_end_seen || record.type_ == "agent_end",
    suspicious_empty_assistant_seen: state.suspicious_empty_assistant_seen
      || is_empty_assistant_start(record),
    last_event: Some(record.type_),
  )
}

pub fn empty_assistant_update(
  record: protocol.RpcRecord,
  turn: Int,
  config: config_types.EffectiveConfig,
) -> Option(types.RunnerUpdate) {
  case is_empty_assistant_start(record) {
    False -> None
    True ->
      Some(update(
        "kind=provider_empty_response_candidate"
          <> " detail=assistant message_start had stopReason=stop but no content, tool calls, or token usage"
          <> " next_expected=turn_end_or_agent_end"
          <> " content_count=0 tool_calls=0 token_usage=0"
          <> message_metadata_suffix(record.raw_json),
        Some(turn),
        config,
      ))
  }
}

pub fn terminal_update(
  session: client.Session,
  err: error.PiRpcError,
  state: State,
  turn: Int,
  config: config_types.EffectiveConfig,
) -> types.RunnerUpdate {
  update(
    "kind="
      <> terminal_kind(err, state)
      <> " reason="
      <> error.pi_rpc_code(err)
      <> " detail="
      <> log.truncate(error.pi_rpc_detail(err), max_detail_chars)
      <> " turn_end_seen="
      <> bool_text(state.turn_end_seen)
      <> " agent_end_seen="
      <> bool_text(state.agent_end_seen)
      <> " suspicious_empty_assistant_seen="
      <> bool_text(state.suspicious_empty_assistant_seen)
      <> option_suffix(" last_event=", state.last_event)
      <> " stdout_jsonl_retained_as_session_events=true"
      <> wrapper_diagnostics_suffix(session, config),
    Some(turn),
    config,
  )
}

pub fn session_file_update(
  session: client.Session,
  config: config_types.EffectiveConfig,
) -> Option(types.RunnerUpdate) {
  case session.session_file {
    None -> None
    Some(session_file) ->
      case string.trim(session_file) == "" {
        True -> None
        False ->
          case simplifile.is_file(session_file) {
            Ok(True) -> None
            Ok(False) ->
              Some(update(
                session_file_message("missing", session, session_file),
                None,
                config,
              ))
            Error(_) ->
              Some(update(
                session_file_message("stat_failed", session, session_file),
                None,
                config,
              ))
          }
      }
  }
}

pub fn wrapper_failure_update(
  session: client.Session,
  reason: error.AgentRunnerError,
  turn: Option(Int),
  config: config_types.EffectiveConfig,
) -> Option(types.RunnerUpdate) {
  case pi_failure_reason(reason) {
    None -> None
    Some(pi_error) ->
      case client.read_diagnostics(session) {
        Ok(diagnostics) ->
          case string.trim(diagnostics) == "" {
            True -> None
            False ->
              Some(update(
                wrapper_failure_message(pi_error, diagnostics, config),
                turn,
                config,
              ))
          }
        Error(err) ->
          Some(update(
            "kind=wrapper_stderr_unavailable reason="
              <> error.pi_rpc_code(pi_error)
              <> " diagnostic_read_error="
              <> error.pi_rpc_code(err),
            turn,
            config,
          ))
      }
  }
}

fn update(
  message: String,
  turn: Option(Int),
  config: config_types.EffectiveConfig,
) -> types.RunnerUpdate {
  let message =
    log.redact(
      "pi_protocol_diagnostic",
      message,
      config_module.resolved_secrets(config),
    )
    |> log.truncate(max_diagnostic_chars)
  types.RunnerPiUpdate(types.PiUpdate(
    event: pi_event.PiProtocolDiagnostic,
    message: Some(message),
    raw_json: None,
    turn: turn,
    request_id: None,
    method: None,
    pi_session_id: None,
    tokens: session_tokens.zero_token_totals(),
    tool_name: None,
    tool_input: None,
    tool_output: None,
    tool_status: None,
  ))
}

fn is_empty_assistant_start(record: protocol.RpcRecord) -> Bool {
  record.type_ == "message_start"
  && list.is_empty(record.tool_calls)
  && case json_value.parse(record.raw_json) {
    Error(_) -> False
    Ok(value) ->
      string_at(value, ["message", "role"]) == Some("assistant")
      && stop_reason_is_stop(string_at(value, ["message", "stopReason"]))
      && array_length_at(value, ["message", "content"]) == Some(0)
      && zero_usage(value)
  }
}

fn zero_usage(value: JsonValue) -> Bool {
  case value_at(value, ["message", "usage"]) {
    None -> False
    Some(_) ->
      zero_int_at(value, ["message", "usage", "input"])
      && zero_int_at(value, ["message", "usage", "output"])
      && zero_int_at(value, ["message", "usage", "cacheRead"])
      && zero_int_at(value, ["message", "usage", "cacheWrite"])
  }
}

fn stop_reason_is_stop(reason: Option(String)) -> Bool {
  case reason {
    Some(value) -> {
      let normalized = value |> string.trim |> string.lowercase
      normalized == "stop"
    }
    None -> False
  }
}

fn terminal_kind(err: error.PiRpcError, state: State) -> String {
  case err {
    error.PiStallTimeout ->
      case state.turn_end_seen {
        True -> "stream_stalled_before_agent_end"
        False -> "stream_stalled_before_turn_end"
      }
    error.PiTurnTimeout ->
      case state.turn_end_seen {
        True -> "stream_timed_out_before_agent_end"
        False -> "stream_timed_out_before_turn_end"
      }
    error.PiExited(_) | error.PiMalformedJson(_) | error.PiReadTimeout ->
      "wrapper_transport_failure"
    error.PiLaunchFailed(_) -> "wrapper_transport_failure"
    error.PiProtocolError(_) -> "pi_protocol_failure"
    error.PiContextWindowExhausted(..) -> "provider_protocol_failure"
  }
}

fn wrapper_diagnostics_suffix(
  session: client.Session,
  config: config_types.EffectiveConfig,
) -> String {
  case client.read_diagnostics(session) {
    Error(err) -> " wrapper_stderr_unavailable=" <> error.pi_rpc_code(err)
    Ok(diagnostics) -> {
      let diagnostics = string.trim(diagnostics)
      case diagnostics == "" {
        True -> " wrapper_stderr=empty"
        False ->
          " wrapper_stderr_excerpt=" <> redact_diagnostics(diagnostics, config)
      }
    }
  }
}

fn wrapper_failure_message(
  pi_error: error.PiRpcError,
  diagnostics: String,
  config: config_types.EffectiveConfig,
) -> String {
  "kind=wrapper_stderr reason="
  <> error.pi_rpc_code(pi_error)
  <> " wrapper_stderr_excerpt="
  <> redact_diagnostics(diagnostics, config)
}

fn redact_diagnostics(
  diagnostics: String,
  config: config_types.EffectiveConfig,
) -> String {
  let diagnostics = string.trim(diagnostics) |> redact_provider_secrets
  log.redact(
    "pi_wrapper_stderr",
    diagnostics,
    config_module.resolved_secrets(config),
  )
  |> log.truncate(max_diagnostic_chars)
}

fn redact_provider_secrets(diagnostics: String) -> String {
  diagnostics
  |> string.split(on: "\n")
  |> list.map(redact_provider_secret_line)
  |> string.join(with: "\n")
}

fn redact_provider_secret_line(line: String) -> String {
  case sensitive_diagnostic_line(line) {
    True -> "[REDACTED]"
    False -> line
  }
}

fn sensitive_diagnostic_line(line: String) -> Bool {
  let lower = string.lowercase(line)
  contains_any(lower, [
    "api_key",
    "apikey",
    "api-key",
    "x-api-key",
    "authorization",
    "bearer ",
    "token",
    "secret",
    "sk-",
    "ghp_",
    "github_pat_",
    "xoxb-",
    "xoxp-",
    "aiza",
  ])
}

fn contains_any(value: String, needles: List(String)) -> Bool {
  case needles {
    [] -> False
    [needle, ..rest] ->
      string.contains(value, needle) || contains_any(value, rest)
  }
}

fn session_file_message(
  status: String,
  session: client.Session,
  session_file: String,
) -> String {
  "kind=transcript_persistence_failure status="
  <> status
  <> " detail=reported Pi session_file does not exist after pi_session_started"
  <> " session_id="
  <> option_text(session.session_id)
  <> " session_file="
  <> session_file
}

fn message_metadata_suffix(raw_json: String) -> String {
  case json_value.parse(raw_json) {
    Error(_) -> ""
    Ok(value) ->
      option_suffix(" provider=", string_at(value, ["message", "provider"]))
      <> option_suffix(" model=", string_at(value, ["message", "model"]))
      <> option_suffix(" api=", string_at(value, ["message", "api"]))
  }
}

fn string_at(value: JsonValue, path: List(String)) -> Option(String) {
  case value_at(value, path) {
    Some(json_value.JString(text)) -> Some(text)
    _ -> None
  }
}

fn array_length_at(value: JsonValue, path: List(String)) -> Option(Int) {
  case value_at(value, path) {
    Some(json_value.JArray(items)) -> Some(list.length(items))
    _ -> None
  }
}

fn zero_int_at(value: JsonValue, path: List(String)) -> Bool {
  case value_at(value, path) {
    Some(json_value.JInt(0)) -> True
    Some(json_value.JFloat(0.0)) -> True
    _ -> False
  }
}

fn value_at(value: JsonValue, path: List(String)) -> Option(JsonValue) {
  case path, value {
    [], _ -> Some(value)
    [key, ..rest], json_value.JObject(entries) ->
      case object_get(entries, key) {
        Some(child) -> value_at(child, rest)
        None -> None
      }
    _, _ -> None
  }
}

fn object_get(
  entries: List(#(String, JsonValue)),
  key: String,
) -> Option(JsonValue) {
  case entries {
    [] -> None
    [#(entry_key, value), ..rest] ->
      case entry_key == key {
        True -> Some(value)
        False -> object_get(rest, key)
      }
  }
}

fn pi_failure_reason(
  reason: error.AgentRunnerError,
) -> Option(error.PiRpcError) {
  case reason {
    error.PiFailed(pi_error) -> Some(pi_error)
    error.ContextRecoveryExhausted(final_error: pi_error, ..) -> Some(pi_error)
    _ -> None
  }
}

fn option_suffix(label: String, value: Option(String)) -> String {
  case value {
    None -> ""
    Some(text) ->
      case string.trim(text) == "" {
        True -> ""
        False -> label <> text
      }
  }
}

fn option_text(value: Option(String)) -> String {
  case value {
    Some(text) -> text
    None -> "none"
  }
}

fn bool_text(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}
