import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/probe
import scherzo/error
import scherzo/path
import scherzo/pi/client
import scherzo/pi/command as pi_command
import scherzo/pi/protocol
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

pub fn structured_argv_launch_records_cwd_argv_and_session_file_test() {
  let cwd = "test/tmp/pi-rpc-argv-launch"
  reset_dir(cwd)
  let assert Ok(abs_cwd) = path.absolute(cwd)
  let assert Ok(argv_log) = path.absolute(cwd <> "/argv.log")
  let session_file = abs_cwd <> "/fresh.pi-session"
  let spec =
    pi_command.ArgvLaunch(fake_pi(), ["--mode", "rpc"], [
      #("FAKE_PI_ARGV_LOG", argv_log),
      #("FAKE_PI_SESSION_FILE", session_file),
    ])

  let assert Ok(session) =
    client.launch_spec(spec, abs_cwd, "ABC-123: Title", True, 1000)
  assert session.session_id == Some("fake-session")
  assert session.session_file == Some(session_file)
  assert session.reported_cwd == Some(abs_cwd)
  let _ = client.terminate(session)

  let assert Ok(contents) = simplifile.read(argv_log)
  assert string.contains(contents, "cwd=" <> abs_cwd)
  assert string.contains(contents, "argv[0]=" <> fake_pi())
  assert string.contains(contents, "argv[1]=--mode")
  assert string.contains(contents, "argv[2]=rpc")
}

pub fn continuation_reopen_uses_recorded_session_file_and_validates_before_prompt_test() {
  let cwd = "test/tmp/pi-rpc-continuation"
  reset_dir(cwd)
  let assert Ok(abs_cwd) = path.absolute(cwd)
  let assert Ok(argv_log) = path.absolute(cwd <> "/argv.log")
  let assert Ok(transcript) = path.absolute(cwd <> "/transcript.jsonl")
  let session_file = abs_cwd <> "/captured.pi-session"
  let spec =
    pi_command.ArgvLaunch(
      fake_pi(),
      ["--mode", "rpc", "--session", session_file],
      [#("FAKE_PI_ARGV_LOG", argv_log), #("FAKE_PI_TRANSCRIPT", transcript)],
    )

  let assert Ok(session) =
    client.reopen_session_for_continuation(spec, abs_cwd, session_file, 1000)
  let assert Ok(#(session, _)) = client.send_prompt(session, "RECOVERY", 1000)
  let _ = client.terminate(session)

  let assert Ok(argv_contents) = simplifile.read(argv_log)
  assert string.contains(argv_contents, "cwd=" <> abs_cwd)
  assert string.contains(argv_contents, "argv[3]=--session")
  assert string.contains(argv_contents, "argv[4]=" <> session_file)
  let assert Ok(transcript_contents) = simplifile.read(transcript)
  assert string.contains(transcript_contents, "RECOVERY")
}

pub fn continuation_reopen_validation_failure_sends_no_prompt_test() {
  let cwd = "test/tmp/pi-rpc-continuation-validation-failure"
  reset_dir(cwd)
  let assert Ok(abs_cwd) = path.absolute(cwd)
  let assert Ok(transcript) = path.absolute(cwd <> "/transcript.jsonl")
  let session_file = abs_cwd <> "/captured.pi-session"
  let spec =
    pi_command.ArgvLaunch(
      fake_pi(),
      ["--mode", "rpc", "--session", session_file],
      [
        #("FAKE_PI_TRANSCRIPT", transcript),
        #("FAKE_PI_SESSION_FILE_MISMATCH", abs_cwd <> "/other.pi-session"),
      ],
    )

  let reopen_result =
    client.reopen_session_for_continuation(spec, abs_cwd, session_file, 1000)
  terminate_if_launch_succeeded(reopen_result)
  let assert Error(error.PiProtocolError(_)) = reopen_result
  let assert Ok(contents) = simplifile.read(transcript)
  assert string.contains(contents, "get_state")
  assert !string.contains(contents, "prompt")
}

pub fn stepwise_prompt_read_and_stats_with_fake_pi_test() {
  let cwd = "test/tmp/pi-rpc-stepwise"
  reset_dir(cwd)
  let assert Ok(Nil) = simplifile.write(cwd <> "/POPULATED", "yes")
  let assert Ok(session) =
    client.launch(fake_pi(), cwd, "ABC-123: Title", True, 1000)
  let assert Ok(#(session, skipped)) =
    client.send_prompt(session, "Do work", 1000)
  assert skipped == []
  let assert Ok(#(session, Some(agent_start))) =
    client.read_turn_record(session, 1000, 9_999_999_999, 9_999_999_999)
  let assert Ok(#(session, Some(turn_start))) =
    client.read_turn_record(session, 1000, 9_999_999_999, 9_999_999_999)
  let assert Ok(#(session, Some(message_update))) =
    client.read_turn_record(session, 1000, 9_999_999_999, 9_999_999_999)
  let assert Ok(#(session, Some(turn_end))) =
    client.read_turn_record(session, 1000, 9_999_999_999, 9_999_999_999)
  let assert Ok(#(session, Some(agent_end))) =
    client.read_turn_record(session, 1000, 9_999_999_999, 9_999_999_999)
  assert [
      agent_start.type_,
      turn_start.type_,
      message_update.type_,
      turn_end.type_,
      agent_end.type_,
    ]
    == ["agent_start", "turn_start", "message_update", "turn_end", "agent_end"]
  let stats_result = client.get_session_stats(session, 1000)
  let _ = client.terminate(session)
  let assert Ok(#(_session, totals)) = stats_result
  assert totals.total == 3
}

pub fn read_turn_record_uses_absolute_deadlines_test() {
  let cwd = "test/tmp/pi-rpc-absolute-deadlines"
  reset_dir(cwd)
  let command = "FAKE_PI_NO_OUTPUT_AFTER_PROMPT=1 " <> fake_pi()
  let assert Ok(session) = client.launch(command, cwd, "name", False, 1000)
  let assert Ok(#(session, _)) = client.send_prompt(session, "prompt", 1000)
  let turn_result =
    client.read_turn_record(session, 10, -9_999_999_999_999, 9_999_999_999)
  let _ = client.terminate(session)
  let assert Error(error.PiTurnTimeout) = turn_result

  let assert Ok(session) = client.launch(command, cwd, "name", False, 1000)
  let assert Ok(#(session, _)) = client.send_prompt(session, "prompt", 1000)
  let stall_result =
    client.read_turn_record(session, 10, 9_999_999_999, -9_999_999_999_999)
  let _ = client.terminate(session)
  let assert Error(error.PiStallTimeout) = stall_result
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
  let assert Ok(session) = client.launch(command, cwd, "name", False, 1000)
  let assert Ok(#(session, skipped)) = client.send_abort(session, 1000)
  assert list_types(skipped) == ["message_update"]
  let assert Ok(#(session, _)) =
    client.send_extension_ui_cancel(session, "ui-1", 1000)
  let assert Ok(#(session, _)) =
    client.send_extension_ui_value(session, "ui-2", "ok", 1000)
  let _ = client.terminate(session)
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
    client.launch(fake_pi(), cwd, "ABC-123: Title", True, 1000)
  assert session.session_id == Some("fake-session")
  let assert Ok(#(session, events)) =
    collect_prompt(session, "Do work", 1000, 5000, 300_000)
  assert list_types(events)
    == ["agent_start", "turn_start", "message_update", "turn_end", "agent_end"]
  let stats_result = client.get_session_stats(session, 1000)
  let _ = client.terminate(session)
  let assert Ok(#(_session, totals)) = stats_result
  assert totals.total == 3
}

pub fn prompt_with_fake_tool_events_surfaces_tool_records_test() {
  let cwd = "test/tmp/pi-rpc-tool-events"
  reset_dir(cwd)
  let assert Ok(Nil) = simplifile.write(cwd <> "/POPULATED", "yes")
  let command = "FAKE_PI_TOOL=1 " <> fake_pi()
  let assert Ok(session) = client.launch(command, cwd, "name", False, 1000)
  let assert Ok(#(session, events)) =
    collect_prompt(session, "Do work", 1000, 5000, 300_000)
  let _ = client.terminate(session)

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
  let malformed_result =
    client.launch("FAKE_PI_MALFORMED=1 " <> fake_pi(), cwd, "name", False, 1000)
  terminate_if_launch_succeeded(malformed_result)
  let assert Error(_) = malformed_result

  let timeout_result =
    client.launch("FAKE_PI_DELAY_MS=2000 " <> fake_pi(), cwd, "name", False, 10)
  terminate_if_launch_succeeded(timeout_result)
  let assert Error(_) = timeout_result
}

pub fn launch_spec_terminates_fake_pi_after_handshake_failure_test() {
  let cwd = "test/tmp/pi-rpc-launch-handshake-failure"
  reset_dir(cwd)
  let assert Ok(abs_cwd) = path.absolute(cwd)
  let assert Ok(pid_file) = path.absolute(cwd <> "/fake-pi.pid")
  let spec =
    pi_command.ArgvLaunch(fake_pi(), ["--mode", "rpc"], [
      #("FAKE_PI_GET_STATE_FAIL", "1"),
      #("FAKE_PI_PID_FILE", pid_file),
    ])

  let launch_result = client.launch_spec(spec, abs_cwd, "name", False, 1000)
  terminate_if_launch_succeeded(launch_result)
  let assert Error(error.PiProtocolError(_)) = launch_result
  let assert Ok(fake_pid) = read_pid_file(pid_file)
  assert wait_until_dead(fake_pid, 50)
}

pub fn prompt_allows_short_read_timeouts_until_event_test() {
  let cwd = "test/tmp/pi-rpc-delayed-event"
  reset_dir(cwd)
  let command = "FAKE_PI_DELAY_EVENT_MS=100 " <> fake_pi()
  let assert Ok(session) = client.launch(command, cwd, "name", False, 1000)
  let assert Ok(#(session, events)) =
    collect_prompt(session, "prompt", 20, 1000, 500)
  let stats_result = client.get_session_stats(session, 1000)
  let _ = client.terminate(session)
  let assert Ok(#(_session, _)) = stats_result
  assert list_types(events)
    == ["agent_start", "turn_start", "message_update", "turn_end", "agent_end"]
}

pub fn prompt_fails_when_stall_timeout_expires_test() {
  let cwd = "test/tmp/pi-rpc-stall-timeout"
  reset_dir(cwd)
  let command = "FAKE_PI_NO_OUTPUT_AFTER_PROMPT=1 " <> fake_pi()
  let assert Ok(session) = client.launch(command, cwd, "name", False, 1000)
  let prompt_result = collect_prompt(session, "prompt", 1000, 1000, 50)
  let _ = client.terminate(session)
  let assert Error(error.PiStallTimeout) = prompt_result
}

pub fn prompt_fails_when_turn_timeout_expires_before_agent_end_test() {
  let cwd = "test/tmp/pi-rpc-turn-timeout-before-end"
  reset_dir(cwd)
  let command = "FAKE_PI_NO_AGENT_END=1 " <> fake_pi()
  let assert Ok(session) = client.launch(command, cwd, "name", False, 1000)
  let prompt_result = collect_prompt(session, "prompt", 1000, 80, 1000)
  let _ = client.terminate(session)
  let assert Error(error.PiTurnTimeout) = prompt_result
}

pub fn turn_timeout_and_failed_stats_are_errors_test() {
  let cwd = "test/tmp/pi-rpc-timeout"
  reset_dir(cwd)
  let command = "FAKE_PI_STALL_AFTER_PROMPT=200 " <> fake_pi()
  let assert Ok(session) = client.launch(command, cwd, "name", False, 1000)
  let prompt_result = collect_prompt(session, "prompt", 1000, 20, 300_000)
  let _ = client.terminate(session)
  let assert Error(error.PiTurnTimeout) = prompt_result

  let cwd = "test/tmp/pi-rpc-stats-fail"
  reset_dir(cwd)
  let assert Ok(session) =
    client.launch(
      "FAKE_PI_STATS_FAIL=1 " <> fake_pi(),
      cwd,
      "name",
      False,
      1000,
    )
  let stats_result = client.get_session_stats(session, 1000)
  let _ = client.terminate(session)
  let assert Error(error.PiProtocolError(_)) = stats_result
}

fn collect_prompt(
  session: client.Session,
  message: String,
  read_timeout_ms: Int,
  turn_timeout_ms: Int,
  stall_timeout_ms: Int,
) -> Result(#(client.Session, List(protocol.RpcRecord)), error.PiRpcError) {
  use pair <- try_pi(client.send_prompt(session, message, read_timeout_ms))
  let #(session, skipped) = pair
  let now = monotonic_ms()
  collect_turn(
    session,
    read_timeout_ms,
    stall_timeout_ms,
    now + turn_timeout_ms,
    now + stall_timeout_ms,
    list.reverse(skipped),
  )
}

fn collect_turn(
  session: client.Session,
  read_timeout_ms: Int,
  stall_timeout_ms: Int,
  turn_deadline_ms: Int,
  stall_deadline_ms: Int,
  acc: List(protocol.RpcRecord),
) -> Result(#(client.Session, List(protocol.RpcRecord)), error.PiRpcError) {
  use pair <- try_pi(client.read_turn_record(
    session,
    read_timeout_ms,
    turn_deadline_ms,
    stall_deadline_ms,
  ))
  let #(session, maybe_record) = pair
  case maybe_record {
    None ->
      collect_turn(
        session,
        read_timeout_ms,
        stall_timeout_ms,
        turn_deadline_ms,
        stall_deadline_ms,
        acc,
      )
    Some(record) -> {
      let acc = [record, ..acc]
      case record.type_ == "agent_end" {
        True -> Ok(#(session, list.reverse(acc)))
        False ->
          collect_turn(
            session,
            read_timeout_ms,
            stall_timeout_ms,
            turn_deadline_ms,
            monotonic_ms() + stall_timeout_ms,
            acc,
          )
      }
    }
  }
}

fn find_record_with_tool_input(
  events: List(protocol.RpcRecord),
) -> Option(protocol.RpcRecord) {
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
  events: List(protocol.RpcRecord),
) -> Option(protocol.RpcRecord) {
  case events {
    [] -> None
    [event, ..rest] ->
      case event.tool_output {
        Some(_) -> Some(event)
        None -> find_record_with_tool_output(rest)
      }
  }
}

fn list_types(events: List(protocol.RpcRecord)) -> List(String) {
  case events {
    [] -> []
    [event, ..rest] -> [event.type_, ..list_types(rest)]
  }
}

fn try_pi(
  result: Result(a, error.PiRpcError),
  next: fn(a) -> Result(b, error.PiRpcError),
) -> Result(b, error.PiRpcError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}

fn terminate_if_launch_succeeded(
  result: Result(client.Session, error.PiRpcError),
) -> Nil {
  case result {
    Ok(session) -> {
      let _ = client.terminate(session)
      Nil
    }
    Error(_) -> Nil
  }
}

fn read_pid_file(path: String) -> Result(Int, Nil) {
  read_pid_file_attempts(path, 50)
}

fn read_pid_file_attempts(path: String, attempts: Int) -> Result(Int, Nil) {
  case attempts <= 0 {
    True -> Error(Nil)
    False ->
      case simplifile.read(path) {
        Ok(contents) -> int.parse(string.trim(contents)) |> result_nil_error
        Error(_) -> {
          process.sleep(20)
          read_pid_file_attempts(path, attempts - 1)
        }
      }
  }
}

fn result_nil_error(result: Result(Int, a)) -> Result(Int, Nil) {
  case result {
    Ok(value) -> Ok(value)
    Error(_) -> Error(Nil)
  }
}

fn wait_until_dead(pid: Int, attempts: Int) -> Bool {
  case pid_alive(pid) {
    False -> True
    True ->
      case attempts <= 0 {
        True -> False
        False -> {
          process.sleep(20)
          wait_until_dead(pid, attempts - 1)
        }
      }
  }
}

@external(erlang, "scherzo_test_ffi", "pid_alive")
fn pid_alive(pid: Int) -> Bool

@external(erlang, "scherzo_time_ffi", "monotonic_ms")
fn monotonic_ms() -> Int
