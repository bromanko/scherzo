import gleam/option.{None, Some}
import gleam/string
import scherzo/pi/protocol
import simplifile

pub fn codec_helpers_encode_commands_test() {
  assert string.contains(
    protocol.encode_set_session_name("1", "name"),
    "set_session_name",
  )
  assert string.contains(
    protocol.encode_set_auto_retry("2", enabled: True),
    "set_auto_retry",
  )
  assert string.contains(protocol.encode_prompt("3", "hello"), "prompt")
  assert string.contains(protocol.encode_get_state("4"), "get_state")
  assert string.contains(
    protocol.encode_get_session_stats("5"),
    "get_session_stats",
  )
}

pub fn decode_response_and_event_test() {
  let assert Ok(response) =
    protocol.decode_record(
      "{\"id\":\"1\",\"type\":\"response\",\"command\":\"get_state\",\"success\":true,\"data\":{\"sessionId\":\"fake\"}}",
    )
  assert response.id == Some("1")
  assert response.session_id == Some("fake")
  let assert Ok(event) =
    protocol.decode_record("{\"type\":\"message_update\",\"delta\":\"hi\"}")
  assert event.delta == Some("hi")
  assert string.contains(event.raw_json, "message_update")

  let assert Ok(codex_event) =
    protocol.decode_record(
      "{\"type\":\"message_update\",\"assistantMessageEvent\":{\"type\":\"text_delta\",\"delta\":\"nested hi\"}}",
    )
  assert codex_event.delta == Some("nested hi")

  let assert Ok(thinking_event) =
    protocol.decode_record(
      "{\"type\":\"message_update\",\"assistantMessageEvent\":{\"type\":\"thinking_delta\",\"delta\":\"scratch\"}}",
    )
  assert thinking_event.delta == None
  assert thinking_event.assistant_event_type == Some("thinking_delta")
}

pub fn decode_extension_ui_request_message_test() {
  let assert Ok(record) =
    protocol.decode_record(
      "{\"id\":\"ui-1\",\"type\":\"extension_ui_request\",\"method\":\"confirm\",\"message\":\"continue?\"}",
    )
  assert record.message == Some("continue?")
}

pub fn decode_stop_reason_error_fields_test() {
  let assert Ok(top_level) =
    protocol.decode_record(
      "{\"type\":\"turn_end\",\"stopReason\":\"error\",\"errorMessage\":\"terminated\"}",
    )
  assert top_level.stop_reason == Some("error")
  assert top_level.error_message == Some("terminated")

  let assert Ok(nested_message) =
    protocol.decode_record(
      "{\"type\":\"message\",\"message\":{\"role\":\"assistant\",\"stopReason\":\"error\",\"errorMessage\":\"terminated\",\"content\":[{\"type\":\"text\",\"text\":\"partial\"}]}}",
    )
  assert nested_message.stop_reason == Some("error")
  assert nested_message.error_message == Some("terminated")
  assert nested_message.assistant_messages == ["partial"]
}

pub fn decode_agent_end_assistant_messages_test() {
  let assert Ok(record) =
    protocol.decode_record(
      "{\"type\":\"agent_end\",\"messages\":[{\"role\":\"assistant\",\"content\":\"final answer\"},{\"role\":\"user\",\"content\":\"ignored\"},{\"role\":\"assistant\",\"content\":123}]}",
    )
  assert record.assistant_messages == ["final answer"]

  let assert Ok(codex_record) =
    protocol.decode_record(
      "{\"type\":\"agent_end\",\"messages\":[{\"role\":\"assistant\",\"content\":[{\"type\":\"thinking\",\"thinking\":\"hidden\"},{\"type\":\"text\",\"text\":\"visible final\"},{\"type\":\"text\",\"text\":\"second block\"}]},{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"ignored\"}]}]}",
    )
  assert codex_record.assistant_messages == ["visible final\nsecond block"]
}

pub fn decode_captured_assistant_tool_call_and_tool_result_test() {
  let assert Ok(contents) =
    simplifile.read("test/fixtures/pi_tool_events_captured.jsonl")
  let assert [call_line, result_line] =
    string.split(string.trim(contents), "\n")

  let assert Ok(call) = protocol.decode_record(call_line)
  assert call.type_ == "message"
  assert call.tool_name == Some("bash")
  assert call.tool_input == Some("gc prime")
  assert call.tool_output == None
  assert call.tool_status == None
  let assert [tool_call] = call.tool_calls
  assert tool_call.name == "bash"
  assert tool_call.arguments_json
    == Some("{\"command\":\"gc prime\",\"timeout\":120}")
  assert tool_call.sibling_count == 1

  let assert Ok(result) = protocol.decode_record(result_line)
  assert result.type_ == "message"
  assert result.tool_name == Some("bash")
  assert result.tool_input == None
  assert result.tool_output
    == Some(
      "/bin/bash: gc: command not found\n\n\nCommand exited with code 127",
    )
  assert result.tool_status == Some("failed")
  assert result.tool_call_id == Some("call_example")
}

pub fn decode_top_level_and_data_tool_execution_aliases_test() {
  let assert Ok(start) =
    protocol.decode_record(
      "{\"type\":\"tool_execution_start\",\"toolName\":\"bash\",\"command\":\"gleam test\"}",
    )
  assert start.tool_name == Some("bash")
  assert start.tool_input == Some("gleam test")

  let assert Ok(update) =
    protocol.decode_record(
      "{\"type\":\"tool_execution_update\",\"data\":{\"tool_name\":\"bash\",\"stdout\":\"ok\"}}",
    )
  assert update.tool_name == Some("bash")
  assert update.tool_output == Some("ok")

  let assert Ok(end) =
    protocol.decode_record(
      "{\"type\":\"tool_execution_end\",\"toolName\":\"bash\",\"success\":false}",
    )
  assert end.tool_status == Some("failed")

  let assert Ok(structured) =
    protocol.decode_record(
      "{\"type\":\"tool_execution_start\",\"toolName\":\"bash\",\"command\":{\"argv\":[\"gleam\",\"test\"]}}",
    )
  assert structured.tool_input
    == Some("[structured tool input; use --json for raw details]")
  let assert [structured_call] = structured.tool_calls
  assert structured_call.arguments_json
    == Some("{\"argv\":[\"gleam\",\"test\"]}")
}
