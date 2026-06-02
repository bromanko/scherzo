import gleam/list
import scherzo/agent/turn_result_buffer
import scherzo/pi/protocol

fn decode(line: String) -> protocol.RpcRecord {
  let assert Ok(record) = protocol.decode_record(line)
  record
}

pub fn drops_plain_streaming_message_updates_from_result_buffer_test() {
  let update = decode("{\"type\":\"message_update\",\"delta\":\"token\"}")

  assert turn_result_buffer.retain_record(update) == False
}

pub fn keeps_boundary_records_for_terminal_diagnostics_test() {
  let agent_start = decode("{\"type\":\"agent_start\"}")
  let turn_start = decode("{\"type\":\"turn_start\"}")
  let message_start = decode("{\"cursor\":15044,\"type\":\"message_start\"}")
  let turn_end = decode("{\"type\":\"turn_end\"}")
  let auto_retry_start = decode("{\"type\":\"auto_retry_start\"}")
  let auto_retry_end = decode("{\"type\":\"auto_retry_end\"}")
  let update = decode("{\"type\":\"message_update\",\"delta\":\"token\"}")

  let records = [
    agent_start,
    turn_start,
    message_start,
    update,
    turn_end,
    auto_retry_start,
    auto_retry_end,
  ]

  assert turn_result_buffer.retain_records(records)
    == [
      agent_start,
      turn_start,
      message_start,
      turn_end,
      auto_retry_start,
      auto_retry_end,
    ]
}

pub fn keeps_completed_messages_agent_end_and_tool_records_test() {
  let completed =
    decode(
      "{\"type\":\"message_end\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"final\"}]}}",
    )
  let agent_end =
    decode(
      "{\"type\":\"agent_end\",\"messages\":[{\"role\":\"assistant\",\"content\":\"done\"}]}",
    )
  let tool_call =
    decode(
      "{\"type\":\"message_update\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"toolCall\",\"id\":\"call_1\",\"name\":\"read\",\"arguments\":{\"path\":\"README.md\"}}]}}",
    )
  let tool_result =
    decode(
      "{\"type\":\"message\",\"message\":{\"role\":\"toolResult\",\"toolCallId\":\"call_1\",\"toolName\":\"read\",\"isError\":false,\"content\":[{\"type\":\"text\",\"text\":\"ok\"}]}}",
    )

  assert turn_result_buffer.retain_record(completed) == True
  assert turn_result_buffer.retain_record(agent_end) == True
  assert turn_result_buffer.retain_record(tool_call) == True
  assert turn_result_buffer.retain_record(tool_result) == True
}

pub fn buffer_appends_retained_records_in_observation_order_test() {
  let update = decode("{\"type\":\"message_update\",\"delta\":\"token\"}")
  let message_end =
    decode(
      "{\"type\":\"message_end\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"complete final\"}]}}",
    )
  let agent_end =
    decode(
      "{\"type\":\"agent_end\",\"messages\":[{\"role\":\"assistant\",\"content\":\"done\"}]}",
    )

  let buffer = turn_result_buffer.from_records([update, message_end])
  let buffer = turn_result_buffer.append_record(buffer, update)
  let buffer = turn_result_buffer.append_records(buffer, [update, agent_end])

  assert turn_result_buffer.to_records(buffer) == [message_end, agent_end]
}

pub fn high_volume_token_stream_retains_only_result_relevant_records_test() {
  let update = decode("{\"type\":\"message_update\",\"delta\":\"token\"}")
  let message_end =
    decode(
      "{\"type\":\"message_end\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"complete final\"}]}}",
    )
  let agent_end =
    decode(
      "{\"type\":\"agent_end\",\"messages\":[{\"role\":\"assistant\",\"content\":\"done\"}]}",
    )
  let stream = repeat_record(update, 50_000, [])
  let records = list.append(stream, [message_end, agent_end])

  let retained = turn_result_buffer.retain_records(records)

  assert retained == [message_end, agent_end]
}

fn repeat_record(
  record: protocol.RpcRecord,
  remaining: Int,
  acc: List(protocol.RpcRecord),
) -> List(protocol.RpcRecord) {
  case remaining <= 0 {
    True -> acc
    False -> repeat_record(record, remaining - 1, [record, ..acc])
  }
}
