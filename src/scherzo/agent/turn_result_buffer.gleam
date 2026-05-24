import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/pi/protocol

pub opaque type Buffer {
  Buffer(reversed_records: List(protocol.RpcRecord))
}

/// Keep only Pi records needed to build the durable turn result and diagnose
/// terminal stream/protocol failures.
///
/// Scherzo still emits every streaming record to the live session hub as it is
/// observed. The turn result buffer is only for post-turn materialization, so
/// plain token-level message updates should not be retained here.
pub fn retain_record(record: protocol.RpcRecord) -> Bool {
  record.type_ == "agent_end"
  || is_boundary_record(record.type_)
  || !list.is_empty(record.assistant_messages)
  || !list.is_empty(record.tool_calls)
  || has_non_empty(record.tool_name)
  || has_non_empty(record.tool_call_id)
  || has_non_empty(record.tool_status)
  || has_non_empty(record.tool_output)
}

pub fn retain_records(
  records: List(protocol.RpcRecord),
) -> List(protocol.RpcRecord) {
  records |> list.filter(retain_record)
}

pub fn from_records(records: List(protocol.RpcRecord)) -> Buffer {
  Buffer(reversed_records: records |> retain_records |> list.reverse)
}

pub fn to_records(buffer: Buffer) -> List(protocol.RpcRecord) {
  buffer.reversed_records |> list.reverse
}

pub fn append_record(buffer: Buffer, record: protocol.RpcRecord) -> Buffer {
  case retain_record(record) {
    True -> Buffer(reversed_records: [record, ..buffer.reversed_records])
    False -> buffer
  }
}

pub fn append_records(
  buffer: Buffer,
  additions: List(protocol.RpcRecord),
) -> Buffer {
  let additions = additions |> retain_records |> list.reverse
  Buffer(reversed_records: list.append(additions, buffer.reversed_records))
}

fn is_boundary_record(type_: String) -> Bool {
  case type_ {
    "agent_start"
    | "turn_start"
    | "turn_end"
    | "message_start"
    | "message_end"
    | "auto_retry_start"
    | "auto_retry_end" -> True
    _ -> False
  }
}

fn has_non_empty(value: Option(String)) -> Bool {
  case value {
    Some(text) -> string.trim(text) != ""
    None -> False
  }
}
