import gleam/option.{None, Some}
import gleam/string
import scherzo/agent/pi_rpc
import scherzo/result_artifact

fn decode(line: String) -> pi_rpc.RpcRecord {
  let assert Ok(record) = pi_rpc.decode_record(line)
  record
}

pub fn prefers_agent_end_assistant_message_test() {
  let records = [
    decode("{\"type\":\"message_update\",\"delta\":\"draft\"}"),
    decode(
      "{\"type\":\"agent_end\",\"messages\":[{\"role\":\"assistant\",\"content\":\"final\"}]}",
    ),
  ]

  let artifact = result_artifact.from_records(records, [], 8000)

  assert artifact.final_response == Some("final")
  assert artifact.source == "agent_end_messages"
  assert artifact.truncated == False
}

pub fn falls_back_to_message_update_deltas_test() {
  let records = [
    decode("{\"type\":\"message_update\",\"delta\":\"hello \"}"),
    decode("{\"type\":\"message_update\",\"delta\":\"world\"}"),
  ]

  let artifact = result_artifact.from_records(records, [], 8000)

  assert artifact.final_response == Some("hello world")
  assert artifact.source == "message_update_delta"
}

pub fn ignores_tool_and_lifecycle_events_test() {
  let records = [
    decode("{\"type\":\"tool_execution_start\",\"toolName\":\"bash\"}"),
    decode("{\"type\":\"tool_execution_update\",\"stdout\":\"secret output\"}"),
    decode("{\"type\":\"turn_start\"}"),
    decode("{\"type\":\"agent_end\",\"messages\":[]}"),
  ]

  let artifact = result_artifact.from_records(records, [], 8000)

  assert artifact.final_response == None
  assert artifact.source == "none"
}

pub fn redacts_final_message_result_text_test() {
  let records = [
    decode(
      "{\"type\":\"agent_end\",\"messages\":[{\"role\":\"assistant\",\"content\":\"answer secret-key\"}]}",
    ),
  ]

  let artifact = result_artifact.from_records(records, ["secret-key"], 8000)

  let assert Some(text) = artifact.final_response
  assert string.contains(text, "[REDACTED]")
  assert !string.contains(text, "secret-key")
}

pub fn redacts_and_truncates_delta_fallback_text_test() {
  let records = [
    decode(
      "{\"type\":\"message_update\",\"delta\":\"prefix secret-key suffix extra text\"}",
    ),
  ]

  let artifact = result_artifact.from_records(records, ["secret-key"], 20)

  let assert Some(text) = artifact.final_response
  assert string.contains(text, "[REDACTED]")
  assert !string.contains(text, "secret-key")
  assert artifact.truncated == True
}

pub fn append_combines_turn_results_test() {
  let first =
    result_artifact.from_records(
      [decode("{\"type\":\"message_update\",\"delta\":\"first\"}")],
      [],
      8000,
    )
  let second =
    result_artifact.from_records(
      [decode("{\"type\":\"message_update\",\"delta\":\"second\"}")],
      [],
      8000,
    )

  let combined = result_artifact.append(first, second, 8000)

  assert combined.final_response == Some("first\n\nsecond")
  assert combined.source == "combined_turns"
  assert combined.truncated == False
}
