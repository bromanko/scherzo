import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/control/remote_envelope
import scherzo/control/remote_harness
import scherzo/control/remote_liveness
import simplifile
import support/test_helpers

pub fn remote_harness_runs_live_loopback_and_updates_liveness_test() {
  let root = "test/tmp/remote-harness-live"
  test_helpers.reset_dir(root)
  let transcript_path = root <> "/transcript.json"
  let sessions = [
    remote_envelope.RemoteSession(
      session_id: "session-1",
      display_name: "Harness demo session",
      issue_identifier: "LIV-686",
      status: "running",
      current_turn: 1,
      last_event_at_ms: 101,
    ),
  ]
  let scenario =
    remote_harness.Scenario(
      expected_token: "test-token",
      client_token: "test-token",
      daemon_id: "daemon_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      boot_id: "boot_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      hello_now_ms: 100,
      heartbeat_now_ms: 101,
      stale_after_ms: 5,
      offline_after_ms: 10,
      send_malformed_hello: False,
      heartbeat_envelope: None,
      state_sessions: sessions,
      transcript_path: Some(transcript_path),
      run_nonce: "run_nonce_aaaaaaaaaaaaaaaaaaaaaaaa",
      use_real_client: True,
      command_demo: False,
      query_demo: False,
    )
  let assert Ok(report) = remote_harness.run_scenario(scenario)

  assert report.bound_port > 0
  let assert Ok(view) =
    remote_liveness.view(report.registry, report.daemon_id, 101)
  assert view.status == remote_liveness.Online
  assert view.last_seen_at_ms == 101
  assert list.length(report.observations) == 2
  assert matching_digest(report.events, "hello")
  assert matching_digest(report.events, "heartbeat")
  assert matching_digest(report.events, "state_snapshot")
  assert !string.contains(report.transcript_json, "test-token")
  assert string.contains(report.transcript_json, "[REDACTED]")
}

pub fn remote_harness_query_demo_records_live_query_evidence_test() {
  let root = "test/tmp/remote-harness-query-demo"
  test_helpers.reset_dir(root)
  let transcript_path = root <> "/query-transcript.json"

  let assert Ok(report) =
    remote_harness.run_query_demo("test-token", transcript_path)

  assert report.bound_port > 0
  assert event_exists(
    report.events,
    "server_send",
    "query_request",
    Some("query-1"),
  )
  assert event_exists(
    report.events,
    "server_recv",
    "query_response",
    Some("query-1"),
  )
  assert state_value_exists(report.events, False)
  assert !string.contains(report.transcript_json, "test-token")
  assert string.contains(report.transcript_json, "[REDACTED]")

  let assert Ok(contents) = simplifile.read(transcript_path)
  assert string.contains(contents, "query-1")
  assert string.contains(contents, "query_response")
}

pub fn remote_harness_command_demo_records_live_pause_resume_evidence_test() {
  let root = "test/tmp/remote-harness-command-demo"
  test_helpers.reset_dir(root)
  let transcript_path = root <> "/command-transcript.json"

  let assert Ok(report) =
    remote_harness.run_command_demo("test-token", transcript_path)

  assert report.bound_port > 0
  assert event_exists(
    report.events,
    "server_send",
    "server_command",
    Some("pause-1"),
  )
  assert event_exists(
    report.events,
    "server_send",
    "server_command",
    Some("resume-1"),
  )
  assert event_exists(
    report.events,
    "server_recv",
    "command_receipt",
    Some("pause-1"),
  )
  assert event_exists(
    report.events,
    "server_recv",
    "command_receipt",
    Some("resume-1"),
  )
  assert result_status_exists(report.events, "pause-1", "applied")
  assert result_status_exists(report.events, "resume-1", "applied")
  assert state_value_exists(report.events, True)
  assert state_value_exists(report.events, False)
  assert !string.contains(report.transcript_json, "test-token")
  assert string.contains(report.transcript_json, "[REDACTED]")
  assert string.contains(report.transcript_json, report.run_nonce)
  assert string.contains(
    report.transcript_json,
    int_to_string(report.bound_port),
  )

  let assert Ok(contents) = simplifile.read(transcript_path)
  assert string.contains(contents, "pause-1")
  assert string.contains(contents, "resume-1")
}

pub fn remote_harness_rejects_wrong_auth_without_online_entry_test() {
  let scenario =
    remote_harness.Scenario(
      expected_token: "test-token",
      client_token: "wrong-token",
      daemon_id: "daemon_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      boot_id: "boot_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      hello_now_ms: 100,
      heartbeat_now_ms: 101,
      stale_after_ms: 5,
      offline_after_ms: 10,
      send_malformed_hello: False,
      heartbeat_envelope: None,
      state_sessions: [],
      transcript_path: None,
      run_nonce: "run_nonce_wrong_auth_aaaaaaaaaaaa",
      use_real_client: False,
      command_demo: False,
      query_demo: False,
    )
  let assert Error(remote_harness.HarnessError(code: code, message: message)) =
    remote_harness.run_scenario(scenario)
  assert code == "wrong_auth"
  assert string.length(message) > 0
}

pub fn remote_harness_rejects_malformed_hello_without_online_entry_test() {
  let scenario =
    remote_harness.Scenario(
      expected_token: "test-token",
      client_token: "test-token",
      daemon_id: "daemon_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      boot_id: "boot_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      hello_now_ms: 100,
      heartbeat_now_ms: 101,
      stale_after_ms: 5,
      offline_after_ms: 10,
      send_malformed_hello: True,
      heartbeat_envelope: None,
      state_sessions: [],
      transcript_path: None,
      run_nonce: "run_nonce_bad_hello_aaaaaaaaaaaaa",
      use_real_client: False,
      command_demo: False,
      query_demo: False,
    )
  let assert Error(remote_harness.HarnessError(code: code, message: message)) =
    remote_harness.run_scenario(scenario)
  assert code == "bad_json"
  assert string.length(message) > 0
}

pub fn remote_harness_rejects_non_heartbeat_after_valid_hello_test() {
  let scenario =
    remote_harness.Scenario(
      expected_token: "test-token",
      client_token: "test-token",
      daemon_id: "daemon_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      boot_id: "boot_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      hello_now_ms: 100,
      heartbeat_now_ms: 101,
      stale_after_ms: 5,
      offline_after_ms: 10,
      send_malformed_hello: False,
      heartbeat_envelope: Some(remote_envelope.RemoteHello(["not_heartbeat"])),
      state_sessions: [],
      transcript_path: None,
      run_nonce: "run_nonce_bad_heartbeat_aaaaaaaa",
      use_real_client: False,
      command_demo: False,
      query_demo: False,
    )
  let assert Error(remote_harness.HarnessError(code: code, message: message)) =
    remote_harness.run_scenario(scenario)
  assert code == "invalid_heartbeat"
  assert string.length(message) > 0
}

pub fn remote_harness_uses_server_receipt_time_for_liveness_test() {
  let scenario =
    remote_harness.Scenario(
      expected_token: "test-token",
      client_token: "test-token",
      daemon_id: "daemon_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      boot_id: "boot_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      hello_now_ms: 100,
      heartbeat_now_ms: 101,
      stale_after_ms: 5,
      offline_after_ms: 10,
      send_malformed_hello: False,
      heartbeat_envelope: Some(remote_envelope.RemoteHeartbeat(1_000_000)),
      state_sessions: [],
      transcript_path: None,
      run_nonce: "run_nonce_future_heartbeat_aaaaa",
      use_real_client: False,
      command_demo: False,
      query_demo: False,
    )
  let assert Ok(report) = remote_harness.run_scenario(scenario)

  let assert Ok(view) =
    remote_liveness.view(report.registry, report.daemon_id, 101)
  assert view.last_seen_at_ms == 101
  assert view.status == remote_liveness.Online
}

pub fn remote_harness_demo_writes_distinct_live_transcripts_test() {
  let root = "test/tmp/remote-harness-demo"
  test_helpers.reset_dir(root)
  let first_path = root <> "/first.json"
  let second_path = root <> "/second.json"

  let assert Ok(first) = remote_harness.run_demo("test-token", first_path)
  let assert Ok(second) = remote_harness.run_demo("test-token", second_path)

  assert first.bound_port > 0
  assert second.bound_port > 0
  assert first.run_nonce != second.run_nonce
  assert matching_digest(first.events, "hello")
  assert matching_digest(first.events, "heartbeat")
  assert matching_digest(first.events, "state_snapshot")
  assert matching_digest(second.events, "hello")
  assert matching_digest(second.events, "heartbeat")
  assert matching_digest(second.events, "state_snapshot")

  let assert Ok(first_contents) = simplifile.read(first_path)
  let assert Ok(second_contents) = simplifile.read(second_path)
  assert string.contains(first_contents, first.run_nonce)
  assert string.contains(second_contents, second.run_nonce)
  assert !string.contains(first_contents, "test-token")
  assert !string.contains(second_contents, "test-token")
  assert string.contains(first_contents, "[REDACTED]")
  assert string.contains(second_contents, "[REDACTED]")
}

fn matching_digest(
  events: List(remote_harness.TranscriptEvent),
  kind: String,
) -> Bool {
  let client =
    list.filter(events, fn(event) {
      event.kind == kind && event.direction == "client_send"
    })
  let server =
    list.filter(events, fn(event) {
      event.kind == kind && event.direction == "server_recv"
    })
  case client, server {
    [remote_harness.TranscriptEvent(digest: Some(client_digest), ..)],
      [remote_harness.TranscriptEvent(digest: Some(server_digest), ..)]
    -> client_digest == server_digest
    _, _ -> False
  }
}

fn event_exists(
  events: List(remote_harness.TranscriptEvent),
  direction: String,
  kind: String,
  command_id: Option(String),
) -> Bool {
  list.any(events, fn(event) {
    event.direction == direction
    && event.kind == kind
    && event.command_id == command_id
  })
}

fn result_status_exists(
  events: List(remote_harness.TranscriptEvent),
  command_id: String,
  status: String,
) -> Bool {
  list.any(events, fn(event) {
    event.kind == "command_result"
    && event.command_id == Some(command_id)
    && event.command_status == Some(status)
  })
}

fn state_value_exists(
  events: List(remote_harness.TranscriptEvent),
  expected: Bool,
) -> Bool {
  list.any(events, fn(event) {
    event.kind == "state_snapshot" && event.dispatch_paused == Some(expected)
  })
}

fn int_to_string(value: Int) -> String {
  int.to_string(value)
}
