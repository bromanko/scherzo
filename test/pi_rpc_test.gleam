import gleam/option.{Some}
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

fn list_types(events: List(pi_rpc.RpcRecord)) -> List(String) {
  case events {
    [] -> []
    [event, ..rest] -> [event.type_, ..list_types(rest)]
  }
}
