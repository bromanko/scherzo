import gleam/option.{None, Some}
import gleam/string
import scherzo/control/command
import scherzo/control/remote_envelope

pub fn remote_envelope_roundtrips_all_message_shapes_test() {
  assert_roundtrip(
    remote_envelope.RemoteHello(["control_commands", "session_snapshots"]),
  )
  assert_roundtrip(remote_envelope.RemoteHeartbeat(1234))
  assert_roundtrip(remote_envelope.RemoteServerCommand(
    "cmd-1",
    command.PromptSession("session-1", "  continue please  "),
  ))
  assert_roundtrip(remote_envelope.RemoteCommandReceipt(
    "cmd-2",
    True,
    Some("accepted for execution"),
  ))
  assert_roundtrip(remote_envelope.RemoteCommandReceipt("cmd-3", False, None))
  assert_roundtrip(remote_envelope.RemoteCommandResult(
    "cmd-4",
    command.CommandResult(
      command: "prompt",
      status: command.NotAllowed("policy"),
      target: Some("session-1"),
      message: Some("policy denied"),
    ),
  ))
  assert_roundtrip(
    remote_envelope.RemoteStateSnapshot(999, False, [
      remote_envelope.RemoteSession(
        session_id: "session-1",
        display_name: "LIV-1-fancy-otter",
        issue_identifier: "LIV-1",
        status: "running",
        current_turn: 3,
        last_event_at_ms: 998,
      ),
    ]),
  )
}

pub fn remote_envelope_encoding_omits_local_loopback_fields_test() {
  let encoded =
    remote_envelope.RemoteServerCommand(
      "cmd-1",
      command.PromptSession("session-1", "continue"),
    )
    |> remote_envelope.to_string

  assert !string.contains(encoded, "\"token\"")
  assert !string.contains(encoded, "\"host\"")
  assert !string.contains(encoded, "\"port\"")
  assert !string.contains(encoded, "\"workspace_root\"")
  assert !string.contains(encoded, "\"control_file\"")
  assert !string.contains(encoded, "SCHERZO_CONTROL_FILE")
}

pub fn remote_envelope_rejects_bad_versions_types_and_shapes_test() {
  assert_invalid_envelope(
    "{\"version\":2,\"type\":\"hello\",\"capabilities\":[]}",
    "unsupported_version",
  )
  assert_invalid_envelope(
    "{\"type\":\"hello\",\"capabilities\":[]}",
    "invalid_envelope",
  )
  assert_invalid_envelope(
    "{\"version\":1,\"capabilities\":[]}",
    "invalid_envelope",
  )
  assert_invalid_envelope(
    "{\"version\":1,\"type\":\"mystery\"}",
    "unknown_envelope_type",
  )
  assert_invalid_envelope(
    "{\"version\":1,\"type\":\"server_command\"}",
    "invalid_envelope",
  )
  assert_invalid_envelope(
    "{\"version\":1,\"type\":\"command_result\",\"command_id\":\"cmd-1\"}",
    "invalid_envelope",
  )
  assert_invalid_envelope(
    "{\"version\":1,\"type\":\"state_snapshot\",\"now_ms\":10}",
    "invalid_envelope",
  )
  assert_invalid_envelope(
    "{\"version\":1,\"type\":\"state_snapshot\",\"now_ms\":10,\"sessions\":[]}",
    "invalid_envelope",
  )
}

pub fn remote_envelope_rejects_invalid_nested_command_payloads_test() {
  assert_invalid_envelope(
    "{\"version\":1,\"type\":\"server_command\",\"command_id\":\"cmd-1\",\"command\":{\"type\":\"mystery\"}}",
    "unknown_command",
  )
  assert_invalid_envelope(
    "{\"version\":1,\"type\":\"server_command\",\"command_id\":\"cmd-1\",\"command\":{\"type\":\"prompt\",\"message\":\"continue\"}}",
    "invalid_command",
  )
  assert_invalid_envelope(
    "{\"version\":1,\"type\":\"server_command\",\"command_id\":\"cmd-1\",\"command\":{\"type\":\"retry_step\",\"target\":\"ABC-1\",\"run_id\":\"run-1\"}}",
    "invalid_command",
  )
}

pub fn remote_envelope_rejects_invalid_nested_result_payloads_test() {
  assert_invalid_envelope(
    "{\"version\":1,\"type\":\"command_result\",\"command_id\":\"cmd-1\",\"result\":{\"status\":\"applied\"}}",
    "invalid_result",
  )
  assert_invalid_envelope(
    "{\"version\":1,\"type\":\"command_result\",\"command_id\":\"cmd-1\",\"result\":{\"command\":\"prompt\"}}",
    "invalid_result",
  )
  assert_invalid_envelope(
    "{\"version\":1,\"type\":\"command_result\",\"command_id\":\"cmd-1\",\"result\":{\"command\":\"prompt\",\"status\":123}}",
    "invalid_result",
  )
  assert_invalid_envelope(
    "{\"version\":1,\"type\":\"command_result\",\"command_id\":\"cmd-1\",\"result\":{\"command\":\"prompt\",\"status\":\"future_status\"}}",
    "invalid_result",
  )
}

pub fn remote_envelope_rejects_local_control_file_json_test() {
  assert_invalid_envelope(
    "{\"host\":\"127.0.0.1\",\"port\":4000,\"token\":\"secret\",\"workspace_root\":\"/tmp/work\"}",
    "invalid_envelope",
  )
}

fn assert_roundtrip(envelope: remote_envelope.Envelope) -> Nil {
  let encoded = remote_envelope.to_string(envelope)
  assert string.contains(encoded, "\"version\":1")
  let assert Ok(decoded) = remote_envelope.decode(encoded)
  assert decoded == envelope
}

fn assert_invalid_envelope(line: String, expected_code: String) -> Nil {
  let assert Error(remote_envelope.DecodeError(code: code, message: message)) =
    remote_envelope.decode(line)
  assert code == expected_code
  assert string.length(message) > 0
}
