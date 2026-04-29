import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/pi_rpc
import scherzo/agent/probe
import scherzo/domain
import scherzo/error
import scherzo/path
import simplifile

fn reset_dir(dir: String) -> Nil {
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  Nil
}

fn fake_pi() -> String {
  let assert Ok(abs) = path.absolute("test/fixtures/fake_pi_rpc.sh")
  abs
}

pub fn codec_helpers_encode_commands_test() {
  assert string.contains(
    pi_rpc.encode_set_session_name("1", "name"),
    "set_session_name",
  )
  assert string.contains(
    pi_rpc.encode_set_auto_retry("2", True),
    "set_auto_retry",
  )
  assert string.contains(pi_rpc.encode_prompt("3", "hello"), "prompt")
  assert string.contains(pi_rpc.encode_get_state("4"), "get_state")
  assert string.contains(
    pi_rpc.encode_get_session_stats("5"),
    "get_session_stats",
  )
}

pub fn decode_response_and_event_test() {
  let assert Ok(response) =
    pi_rpc.decode_record(
      "{\"id\":\"1\",\"type\":\"response\",\"command\":\"get_state\",\"success\":true,\"data\":{\"sessionId\":\"fake\"}}",
    )
  assert response.id == Some("1")
  assert response.session_id == Some("fake")
  let assert Ok(event) =
    pi_rpc.decode_record("{\"type\":\"message_update\",\"delta\":\"hi\"}")
  assert event.delta == Some("hi")
  assert string.contains(event.raw_json, "message_update")
}

pub fn stepwise_prompt_read_and_stats_with_fake_pi_test() {
  let cwd = "test/tmp/pi-rpc-stepwise"
  reset_dir(cwd)
  let assert Ok(Nil) = simplifile.write(cwd <> "/POPULATED", "yes")
  let assert Ok(session) =
    pi_rpc.launch(fake_pi(), cwd, "ABC-123: Title", True, 1000)
  let assert Ok(#(session, skipped)) =
    pi_rpc.send_prompt(session, "Do work", 1000)
  assert skipped == []
  let assert Ok(#(session, Some(agent_start))) =
    pi_rpc.read_turn_record(session, 1000, 9_999_999_999, 9_999_999_999)
  let assert Ok(#(session, Some(turn_start))) =
    pi_rpc.read_turn_record(session, 1000, 9_999_999_999, 9_999_999_999)
  let assert Ok(#(session, Some(message_update))) =
    pi_rpc.read_turn_record(session, 1000, 9_999_999_999, 9_999_999_999)
  let assert Ok(#(session, Some(turn_end))) =
    pi_rpc.read_turn_record(session, 1000, 9_999_999_999, 9_999_999_999)
  let assert Ok(#(session, Some(agent_end))) =
    pi_rpc.read_turn_record(session, 1000, 9_999_999_999, 9_999_999_999)
  assert [
      agent_start.type_,
      turn_start.type_,
      message_update.type_,
      turn_end.type_,
      agent_end.type_,
    ]
    == ["agent_start", "turn_start", "message_update", "turn_end", "agent_end"]
  let assert Ok(#(_, totals)) = pi_rpc.get_session_stats(session, 1000)
  assert totals.total == 3
}

pub fn read_turn_record_uses_absolute_deadlines_test() {
  let cwd = "test/tmp/pi-rpc-absolute-deadlines"
  reset_dir(cwd)
  let command = "FAKE_PI_NO_OUTPUT_AFTER_PROMPT=1 " <> fake_pi()
  let assert Ok(session) = pi_rpc.launch(command, cwd, "name", False, 1000)
  let assert Ok(#(session, _)) = pi_rpc.send_prompt(session, "prompt", 1000)
  let assert Error(error.PiTurnTimeout) =
    pi_rpc.read_turn_record(session, 10, -9_999_999_999_999, 9_999_999_999)
  let _ = pi_rpc.terminate(session)

  let assert Ok(session) = pi_rpc.launch(command, cwd, "name", False, 1000)
  let assert Ok(#(session, _)) = pi_rpc.send_prompt(session, "prompt", 1000)
  let assert Error(error.PiStallTimeout) =
    pi_rpc.read_turn_record(session, 10, 9_999_999_999, -9_999_999_999_999)
  let _ = pi_rpc.terminate(session)
}

pub fn decode_extension_ui_request_message_test() {
  let assert Ok(record) =
    pi_rpc.decode_record(
      "{\"id\":\"ui-1\",\"type\":\"extension_ui_request\",\"method\":\"confirm\",\"message\":\"continue?\"}",
    )
  assert record.message == Some("continue?")
}

pub fn decode_agent_end_assistant_messages_test() {
  let assert Ok(record) =
    pi_rpc.decode_record(
      "{\"type\":\"agent_end\",\"messages\":[{\"role\":\"assistant\",\"content\":\"final answer\"},{\"role\":\"user\",\"content\":\"ignored\"},{\"role\":\"assistant\",\"content\":123}]}",
    )
  assert record.assistant_messages == ["final answer"]
}

pub fn decode_captured_assistant_tool_call_and_tool_result_test() {
  let assert Ok(contents) =
    simplifile.read("test/fixtures/pi_tool_events_captured.jsonl")
  let assert [call_line, result_line] =
    string.split(string.trim(contents), "\n")

  let assert Ok(call) = pi_rpc.decode_record(call_line)
  assert call.type_ == "message"
  assert call.tool_name == Some("bash")
  assert call.tool_input == Some("gc prime")
  assert call.tool_output == None
  assert call.tool_status == None

  let assert Ok(result) = pi_rpc.decode_record(result_line)
  assert result.type_ == "message"
  assert result.tool_name == Some("bash")
  assert result.tool_input == None
  assert result.tool_output
    == Some(
      "/bin/bash: gc: command not found\n\n\nCommand exited with code 127",
    )
  assert result.tool_status == Some("failed")
}

pub fn decode_top_level_and_data_tool_execution_aliases_test() {
  let assert Ok(start) =
    pi_rpc.decode_record(
      "{\"type\":\"tool_execution_start\",\"toolName\":\"bash\",\"command\":\"gleam test\"}",
    )
  assert start.tool_name == Some("bash")
  assert start.tool_input == Some("gleam test")

  let assert Ok(update) =
    pi_rpc.decode_record(
      "{\"type\":\"tool_execution_update\",\"data\":{\"tool_name\":\"bash\",\"stdout\":\"ok\"}}",
    )
  assert update.tool_name == Some("bash")
  assert update.tool_output == Some("ok")

  let assert Ok(end) =
    pi_rpc.decode_record(
      "{\"type\":\"tool_execution_end\",\"toolName\":\"bash\",\"success\":false}",
    )
  assert end.tool_status == Some("failed")

  let assert Ok(structured) =
    pi_rpc.decode_record(
      "{\"type\":\"tool_execution_start\",\"toolName\":\"bash\",\"command\":{\"argv\":[\"gleam\",\"test\"]}}",
    )
  assert structured.tool_input
    == Some("[structured tool input; use --json for raw details]")
}

pub fn send_abort_and_ui_response_helpers_test() {
  let cwd = "test/tmp/pi-rpc-command-helpers"
  reset_dir(cwd)
  let assert Ok(transcript) = path.absolute(cwd <> "/transcript.jsonl")
  let command =
    "FAKE_PI_INTERLEAVE_EVENT_BEFORE_COMMAND_RESPONSE=1 FAKE_PI_TRANSCRIPT="
    <> transcript
    <> " "
    <> fake_pi()
  let assert Ok(session) = pi_rpc.launch(command, cwd, "name", False, 1000)
  let assert Ok(#(session, skipped)) = pi_rpc.send_abort(session, 1000)
  assert list_types(skipped) == ["message_update"]
  let assert Ok(#(session, _)) =
    pi_rpc.send_extension_ui_cancel(session, "ui-1", 1000)
  let assert Ok(#(session, _)) =
    pi_rpc.send_extension_ui_value(session, "ui-2", "ok", 1000)
  let _ = pi_rpc.terminate(session)
  let assert Ok(contents) = simplifile.read(transcript)
  assert string.contains(contents, "abort")
  assert string.contains(contents, "extension_ui_response")
  assert string.contains(contents, "cancelled")
  assert string.contains(contents, "ok")
}

pub fn launch_prompt_and_stats_with_fake_pi_test() {
  let cwd = "test/tmp/pi-rpc-workspace"
  reset_dir(cwd)
  let assert Ok(Nil) = simplifile.write(cwd <> "/POPULATED", "yes")
  let assert Ok(session) =
    pi_rpc.launch(fake_pi(), cwd, "ABC-123: Title", True, 1000)
  assert session.session_id == Some("fake-session")
  let assert Ok(#(session, events)) =
    pi_rpc.prompt(session, "Do work", 1000, 5000, 300_000, ignore_event)
  assert list_types(events)
    == ["agent_start", "turn_start", "message_update", "turn_end", "agent_end"]
  let assert Ok(#(_, totals)) = pi_rpc.get_session_stats(session, 1000)
  assert totals.total == 3
}

pub fn prompt_with_fake_tool_events_surfaces_tool_records_test() {
  let cwd = "test/tmp/pi-rpc-tool-events"
  reset_dir(cwd)
  let assert Ok(Nil) = simplifile.write(cwd <> "/POPULATED", "yes")
  let command = "FAKE_PI_TOOL=1 " <> fake_pi()
  let assert Ok(session) = pi_rpc.launch(command, cwd, "name", False, 1000)
  let assert Ok(#(_, events)) =
    pi_rpc.prompt(session, "Do work", 1000, 5000, 300_000, ignore_event)

  assert list_types(events)
    == [
      "agent_start",
      "turn_start",
      "message_update",
      "message",
      "message",
      "turn_end",
      "agent_end",
    ]
  let assert Some(call) = find_record_with_tool_input(events)
  assert call.tool_name == Some("bash")
  assert call.tool_input == Some("gleam test")
  let assert Some(result) = find_record_with_tool_output(events)
  assert result.tool_name == Some("bash")
  assert result.tool_output == Some("2 failures")
  assert result.tool_status == Some("failed")
}

pub fn probe_launches_without_prompt_test() {
  let cwd = "test/tmp/pi-probe-workspace"
  reset_dir(cwd)
  let assert Ok(transcript) = path.absolute(cwd <> "/transcript.jsonl")
  let command = "FAKE_PI_TRANSCRIPT=" <> transcript <> " " <> fake_pi()
  let assert Ok(Nil) = probe.probe(command, cwd, 1000)
  let assert Ok(contents) = simplifile.read(transcript)
  assert string.contains(contents, "set_session_name")
  assert string.contains(contents, "get_state")
  assert string.contains(contents, "get_session_stats")
  assert !string.contains(contents, "prompt")
}

pub fn malformed_json_and_timeout_fail_test() {
  let cwd = "test/tmp/pi-rpc-failure"
  reset_dir(cwd)
  let assert Error(_) =
    pi_rpc.launch("FAKE_PI_MALFORMED=1 " <> fake_pi(), cwd, "name", False, 1000)
  let assert Error(_) =
    pi_rpc.launch("FAKE_PI_DELAY_MS=2000 " <> fake_pi(), cwd, "name", False, 10)
}

pub fn prompt_allows_short_read_timeouts_until_event_test() {
  let cwd = "test/tmp/pi-rpc-delayed-event"
  reset_dir(cwd)
  let command = "FAKE_PI_DELAY_EVENT_MS=100 " <> fake_pi()
  let assert Ok(session) = pi_rpc.launch(command, cwd, "name", False, 1000)
  let assert Ok(#(session, events)) =
    pi_rpc.prompt(session, "prompt", 20, 1000, 500, ignore_event)
  let assert Ok(#(session, _)) = pi_rpc.get_session_stats(session, 1000)
  let _ = pi_rpc.terminate(session)
  assert list_types(events)
    == ["agent_start", "turn_start", "message_update", "turn_end", "agent_end"]
}

pub fn prompt_fails_when_stall_timeout_expires_test() {
  let cwd = "test/tmp/pi-rpc-stall-timeout"
  reset_dir(cwd)
  let command = "FAKE_PI_NO_OUTPUT_AFTER_PROMPT=1 " <> fake_pi()
  let assert Ok(session) = pi_rpc.launch(command, cwd, "name", False, 1000)
  let assert Error(error.PiStallTimeout) =
    pi_rpc.prompt(session, "prompt", 1000, 1000, 50, ignore_event)
  let _ = pi_rpc.terminate(session)
}

pub fn prompt_fails_when_turn_timeout_expires_before_agent_end_test() {
  let cwd = "test/tmp/pi-rpc-turn-timeout-before-end"
  reset_dir(cwd)
  let command = "FAKE_PI_NO_AGENT_END=1 " <> fake_pi()
  let assert Ok(session) = pi_rpc.launch(command, cwd, "name", False, 1000)
  let assert Error(error.PiTurnTimeout) =
    pi_rpc.prompt(session, "prompt", 1000, 80, 1000, ignore_event)
  let _ = pi_rpc.terminate(session)
}

pub fn turn_timeout_and_failed_stats_are_errors_test() {
  let cwd = "test/tmp/pi-rpc-timeout"
  reset_dir(cwd)
  let command = "FAKE_PI_STALL_AFTER_PROMPT=200 " <> fake_pi()
  let assert Ok(session) = pi_rpc.launch(command, cwd, "name", False, 1000)
  let assert Error(error.PiTurnTimeout) =
    pi_rpc.prompt(session, "prompt", 1000, 20, 300_000, ignore_event)
  let _ = pi_rpc.terminate(session)

  let cwd = "test/tmp/pi-rpc-stats-fail"
  reset_dir(cwd)
  let assert Ok(session) =
    pi_rpc.launch(
      "FAKE_PI_STATS_FAIL=1 " <> fake_pi(),
      cwd,
      "name",
      False,
      1000,
    )
  let assert Error(error.PiProtocolError(_)) =
    pi_rpc.get_session_stats(session, 1000)
}

pub fn extension_ui_fail_policy_rejects_dialog_test() {
  let cwd = "test/tmp/pi-rpc-ui-fail"
  reset_dir(cwd)
  let command = "FAKE_PI_UI_DIALOG=1 " <> fake_pi()
  let assert Ok(session) = pi_rpc.launch(command, cwd, "name", False, 1000)
  let assert Error(error.PiProtocolError(_)) =
    pi_rpc.prompt_with_ui_policy(
      session,
      "prompt",
      1000,
      5000,
      300_000,
      domain.Fail,
      ignore_event,
    )
  let _ = pi_rpc.terminate(session)
}

pub fn extension_ui_ignore_policy_does_not_send_cancel_test() {
  let cwd = "test/tmp/pi-rpc-ui-ignore"
  reset_dir(cwd)
  let assert Ok(transcript) = path.absolute(cwd <> "/transcript.jsonl")
  let command =
    "FAKE_PI_UI_DIALOG=1 FAKE_PI_TRANSCRIPT=" <> transcript <> " " <> fake_pi()
  let assert Ok(session) = pi_rpc.launch(command, cwd, "name", False, 1000)
  let assert Ok(#(session, _events)) =
    pi_rpc.prompt_with_ui_policy(
      session,
      "prompt",
      1000,
      5000,
      300_000,
      domain.Ignore,
      ignore_event,
    )
  let assert Ok(#(_, _)) = pi_rpc.get_session_stats(session, 1000)
  let assert Ok(contents) = simplifile.read(transcript)
  assert !string.contains(contents, "extension_ui_response")
}

pub fn extension_ui_operator_policy_rejects_instead_of_cancelling_test() {
  let cwd = "test/tmp/pi-rpc-ui-operator"
  reset_dir(cwd)
  let assert Ok(transcript) = path.absolute(cwd <> "/transcript.jsonl")
  let command =
    "FAKE_PI_UI_DIALOG=1 FAKE_PI_TRANSCRIPT=" <> transcript <> " " <> fake_pi()
  let assert Ok(session) = pi_rpc.launch(command, cwd, "name", False, 1000)
  let assert Error(error.PiProtocolError(_)) =
    pi_rpc.prompt_with_ui_policy(
      session,
      "prompt",
      1000,
      5000,
      300_000,
      domain.Operator,
      ignore_event,
    )
  let _ = pi_rpc.terminate(session)
  let assert Ok(contents) = simplifile.read(transcript)
  assert !string.contains(contents, "extension_ui_response")
}

pub fn extension_ui_dialog_is_cancelled_test() {
  let cwd = "test/tmp/pi-rpc-ui"
  reset_dir(cwd)
  let assert Ok(transcript) = path.absolute(cwd <> "/transcript.jsonl")
  let command =
    "FAKE_PI_UI_DIALOG=1 FAKE_PI_TRANSCRIPT=" <> transcript <> " " <> fake_pi()
  let assert Ok(session) = pi_rpc.launch(command, cwd, "name", False, 1000)
  let assert Ok(#(session, events)) =
    pi_rpc.prompt(session, "prompt", 1000, 5000, 300_000, ignore_event)
  let assert Ok(#(_, _)) = pi_rpc.get_session_stats(session, 1000)
  assert list_types(events)
    == [
      "agent_start",
      "turn_start",
      "message_update",
      "extension_ui_request",
      "turn_end",
      "agent_end",
    ]
  let assert Ok(contents) = simplifile.read(transcript)
  assert string.contains(contents, "extension_ui_response")
  assert string.contains(contents, "cancelled")
}

fn ignore_event(_event: pi_rpc.RpcRecord) -> Nil {
  Nil
}

fn find_record_with_tool_input(
  events: List(pi_rpc.RpcRecord),
) -> Option(pi_rpc.RpcRecord) {
  case events {
    [] -> None
    [event, ..rest] ->
      case event.tool_input {
        Some(_) -> Some(event)
        None -> find_record_with_tool_input(rest)
      }
  }
}

fn find_record_with_tool_output(
  events: List(pi_rpc.RpcRecord),
) -> Option(pi_rpc.RpcRecord) {
  case events {
    [] -> None
    [event, ..rest] ->
      case event.tool_output {
        Some(_) -> Some(event)
        None -> find_record_with_tool_output(rest)
      }
  }
}

fn list_types(events: List(pi_rpc.RpcRecord)) -> List(String) {
  case events {
    [] -> []
    [event, ..rest] -> [event.type_, ..list_types(rest)]
  }
}
