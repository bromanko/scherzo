import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/context_exhaustion
import scherzo/agent/pi_event
import scherzo/error
import scherzo/pi/protocol

pub fn read_error(
  err: error.PiRpcError,
  records: List(protocol.RpcRecord),
) -> error.PiRpcError {
  case has_successful_turn_end(records) {
    True -> err
    False ->
      case err {
        error.PiExited(status) ->
          error.PiProtocolError(stream_without_turn_end_message(
            "child_exited status=" <> int.to_string(status),
            records,
          ))
        error.PiProtocolError(message) ->
          case is_stream_closed_message(message) {
            True ->
              error.PiProtocolError(stream_without_turn_end_message(
                "stream_closed detail=" <> message,
                records,
              ))
            False -> err
          }
        _ -> err
      }
  }
}

pub fn finish_after_agent_end(
  records: List(protocol.RpcRecord),
) -> Result(List(protocol.RpcRecord), error.PiRpcError) {
  case has_successful_turn_end(records) {
    True -> Ok(records)
    False -> Error(agent_end_without_turn_end_error(records))
  }
}

fn has_successful_turn_end(records: List(protocol.RpcRecord)) -> Bool {
  case records {
    [] -> False
    [record, ..rest] ->
      case successful_turn_end_record(record) {
        True -> True
        False -> has_successful_turn_end(rest)
      }
  }
}

fn agent_end_without_turn_end_error(
  records: List(protocol.RpcRecord),
) -> error.PiRpcError {
  error.PiProtocolError(
    "agent_end received before successful turn_end; "
    <> last_record_diagnostic(records),
  )
}

fn successful_turn_end_record(record: protocol.RpcRecord) -> Bool {
  case record.type_, stop_reason_failure(record) {
    "turn_end", None -> True
    _, _ -> False
  }
}

pub fn stop_reason_failure(
  record: protocol.RpcRecord,
) -> Option(error.PiRpcError) {
  case context_exhaustion.from_rpc_record(record) {
    Some(context) -> Some(context_exhaustion.to_pi_rpc_error(context))
    None ->
      case record.stop_reason {
        None -> None
        Some(reason) -> {
          let normalized = reason |> string.trim |> string.lowercase
          case normalized == "error" {
            True ->
              Some(error.PiProtocolError(stop_reason_failure_message(record)))
            False -> None
          }
        }
      }
  }
}

fn stop_reason_failure_message(record: protocol.RpcRecord) -> String {
  let base = "pi " <> record.type_ <> " reported stopReason=error"
  case record.error_message {
    None -> base
    Some(message) -> {
      let message = string.trim(message)
      case message == "" {
        True -> base
        False -> base <> ": " <> message
      }
    }
  }
}

fn is_stream_closed_message(message: String) -> Bool {
  let normalized = message |> string.trim |> string.lowercase
  normalized == "port closed" || normalized == "pi stream closed"
}

fn stream_without_turn_end_message(
  termination: String,
  records: List(protocol.RpcRecord),
) -> String {
  "pi_stream_ended_without_turn_end: "
  <> termination
  <> "; "
  <> last_record_diagnostic(records)
}

fn last_record_diagnostic(records: List(protocol.RpcRecord)) -> String {
  case last_record(records) {
    None ->
      "last_event_cursor=unknown last_event_kind=none last_event_type=none"
    Some(record) ->
      "last_event_cursor="
      <> cursor_text(record.raw_json)
      <> " last_event_kind="
      <> record_kind(record)
      <> " last_event_type="
      <> record.type_
  }
}

fn cursor_text(raw_json: String) -> String {
  case json.parse(raw_json, cursor_decoder()) {
    Ok(cursor) -> int.to_string(cursor)
    Error(_) -> "unknown"
  }
}

fn cursor_decoder() -> decode.Decoder(Int) {
  use cursor <- decode.field("cursor", decode.int)
  decode.success(cursor)
}

fn last_record(
  records: List(protocol.RpcRecord),
) -> Option(protocol.RpcRecord) {
  case records {
    [] -> None
    [record] -> Some(record)
    [_, ..rest] -> last_record(rest)
  }
}

fn record_kind(record: protocol.RpcRecord) -> String {
  case pi_event.from_string(record.type_) {
    pi_event.MessageStart | pi_event.MessageUpdate | pi_event.MessageEnd ->
      "assistant_message"
    pi_event.ToolExecutionStart
    | pi_event.ToolExecutionUpdate
    | pi_event.ToolExecutionEnd -> "tool"
    pi_event.ExtensionUiRequest -> "ui_request"
    pi_event.ExtensionUiResponse -> "ui_response"
    pi_event.TurnFinished -> "token_stats"
    pi_event.UnknownPiEvent(_) -> "pi_raw"
    _ -> "pi"
  }
}
