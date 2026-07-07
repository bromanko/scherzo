import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/control/command
import scherzo/control/query/types as query_types
import scherzo/control/remote/ui_protocol
import scherzo/managed_launch/grant as managed_launch_grant
import scherzo/session/event
import scherzo/session/recovery as session_recovery
import scherzo/session/tokens as session_tokens

pub fn ui_protocol_encodes_daemon_messages_test() {
  let runtime_state = test_runtime_state(None)
  let hello =
    ui_protocol.encode_client_message(ui_protocol.DaemonHello(
      "daemon_abc",
      "boot_abc",
      None,
      runtime_state,
    ))
  assert string.contains(hello, "daemon_hello")
  assert string.contains(hello, "daemonId")
  assert string.contains(hello, "\"state\":{")
  assert string.contains(hello, "\"host\":\"test-host\"")
  assert string.contains(hello, "\"version\":\"scherzo test-version\"")
  assert string.contains(hello, "\"agentSlots\":{")

  let managed_hello =
    ui_protocol.encode_daemon_hello(
      "daemon_abc",
      "boot_abc",
      Some("Managed"),
      Some(
        ui_protocol.ManagedLaunchContext(launch_id: "launch-123", capabilities: [
          managed_launch_grant.State,
          managed_launch_grant.Query,
        ]),
      ),
      runtime_state,
    )
  assert string.contains(managed_hello, "\"launchId\":\"launch-123\"")
  assert string.contains(
    managed_hello,
    "\"capabilities\":[\"state\",\"query\"]",
  )

  let heartbeat =
    ui_protocol.encode_client_message(ui_protocol.Heartbeat(
      42,
      None,
      runtime_state,
      Some(ui_protocol.DaemonEvent("lifecycle", "heartbeat", "daemon heartbeat")),
    ))
  assert string.contains(heartbeat, "heartbeat")
  assert string.contains(heartbeat, "\"event\":{")
  assert string.contains(heartbeat, "\"kind\":\"lifecycle\"")
  assert string.contains(heartbeat, "\"message\":\"daemon heartbeat\"")

  let state =
    ui_protocol.encode_client_message(
      ui_protocol.DaemonState(42, False, None, runtime_state, [
        ui_protocol.SessionSnapshot(
          "session-1",
          "Demo",
          "LIV-1",
          "running",
          3,
          10,
          99,
          Some("Running workflow"),
          None,
          None,
        ),
      ]),
    )
  assert string.contains(state, "daemon_state")
  assert string.contains(state, "dispatchPaused")
  assert string.contains(state, "sessionId")
}

pub fn ui_protocol_encodes_activity_fields_and_active_issue_rollup_test() {
  let runtime_state = test_runtime_state(None)
  let payload =
    ui_protocol.encode_client_message(
      ui_protocol.DaemonState(42, False, None, runtime_state, [
        ui_protocol.SessionSnapshot(
          "session-parent",
          "LIV-1 parent",
          "LIV-1",
          "running",
          1,
          100,
          130,
          Some("Running workflow"),
          None,
          None,
        ),
        ui_protocol.SessionSnapshot(
          "session-step",
          "LIV-1 step",
          "LIV-1",
          "running",
          2,
          150,
          250,
          Some("Editing daemon status copy"),
          Some("edit_daemon_status_copy"),
          Some("Edit daemon status copy"),
        ),
      ]),
    )

  assert string.contains(payload, "\"startedAtMs\":100")
  assert string.contains(
    payload,
    "\"activityLabel\":\"Editing daemon status copy\"",
  )
  assert string.contains(
    payload,
    "\"currentStepId\":\"edit_daemon_status_copy\"",
  )
  assert string.contains(payload, "\"activeIssues\":[")
  assert string.contains(payload, "\"lastEventAtMs\":250")
}

pub fn ui_protocol_rolls_up_active_issue_work_and_missing_optional_fields_test() {
  let active =
    ui_protocol.active_issue_work_from_sessions([
      ui_protocol.SessionSnapshot(
        "parent",
        "LIV-1 parent",
        "LIV-1",
        "running",
        1,
        100,
        120,
        Some("Running workflow"),
        None,
        None,
      ),
      ui_protocol.SessionSnapshot(
        "child",
        "LIV-1 child",
        "LIV-1",
        "running",
        2,
        180,
        240,
        Some("Run tests"),
        Some("run_tests"),
        Some("Run tests"),
      ),
      ui_protocol.SessionSnapshot(
        "old",
        "LIV-1 old",
        "LIV-1",
        "exited",
        3,
        10,
        300,
        Some("Old transcript-derived text should be ignored"),
        None,
        None,
      ),
      ui_protocol.SessionSnapshot(
        "missing",
        "LIV-2 missing",
        "LIV-2",
        "preparing",
        0,
        200,
        201,
        None,
        None,
        None,
      ),
    ])

  assert list.length(active) == 2
  let assert [liv1, liv2] = active
  assert liv1.issue_identifier == "LIV-1"
  assert liv1.status == "running"
  assert liv1.started_at_ms == 100
  assert liv1.last_event_at_ms == 240
  assert liv1.activity_label == Some("Run tests")
  assert liv1.current_step_id == Some("run_tests")
  assert liv2.issue_identifier == "LIV-2"
  assert liv2.activity_label == None
  assert liv2.current_step_id == None
}

pub fn ui_protocol_rollup_uses_status_winning_activity_for_operator_states_test() {
  let active =
    ui_protocol.active_issue_work_from_sessions([
      ui_protocol.SessionSnapshot(
        "running",
        "LIV-1 running",
        "LIV-1",
        "running",
        1,
        100,
        200,
        Some("Run tests"),
        Some("run_tests"),
        Some("Run tests"),
      ),
      ui_protocol.SessionSnapshot(
        "waiting",
        "LIV-1 waiting",
        "LIV-1",
        "waiting_ui",
        2,
        150,
        240,
        Some("Waiting for operator input"),
        None,
        None,
      ),
    ])

  let assert [liv1] = active
  assert liv1.status == "waiting_ui"
  assert liv1.started_at_ms == 100
  assert liv1.last_event_at_ms == 240
  assert liv1.activity_label == Some("Waiting for operator input")
  assert liv1.current_step_id == None
}

pub fn ui_protocol_sanitizes_activity_label_whitespace_before_control_escape_test() {
  let active =
    ui_protocol.active_issue_work_from_sessions([
      ui_protocol.SessionSnapshot(
        "running",
        "LIV-1 running",
        "LIV-1",
        "running",
        1,
        100,
        200,
        Some("Run\t\t tests\nnow\r please\u{1b}[31m"),
        None,
        None,
      ),
    ])

  let assert [liv1] = active
  assert liv1.activity_label == Some("Run tests now please␛[31m")
}

pub fn ui_protocol_derives_bounded_activity_label_and_started_time_from_summary_test() {
  let step_id =
    "editing_daemon_status_copy_that_has_a_very_long_suffix_for_status_density"
  let recovery =
    event.RecoveryInfo(
      ..session_recovery.base_info(event.Resumed, "test", None, []),
      workflow_step_id: Some(step_id),
    )
  let snapshot =
    ui_protocol.session_from_summary(event.SessionSummary(
      session_id: "session-activity",
      display_name: "LIV-1 activity",
      issue_id: "issue-1",
      issue_identifier: "LIV-1",
      issue_title: "Expose rich labels",
      workspace_path: "test/tmp/workspace",
      pi_session_id: None,
      status: event.Running,
      recovery: Some(recovery),
      current_turn: 4,
      current_turn_status: None,
      current_turn_started_at_ms: None,
      last_turn_finished_at_ms: None,
      last_turn_duration_ms: None,
      last_turn_token_delta: session_tokens.zero_token_totals(),
      last_turn_reason: None,
      started_at_ms: 1234,
      last_event_at_ms: 5678,
      token_totals: session_tokens.zero_token_totals(),
    ))

  let assert Some(label) = snapshot.activity_label
  assert snapshot.started_at_ms == 1234
  assert snapshot.last_event_at_ms == 5678
  assert snapshot.current_step_id == Some(step_id)
  assert string.length(label) <= ui_protocol.max_activity_label_chars
  assert !string.contains(label, "_")
}

pub fn ui_protocol_runtime_state_uses_agent_slot_occupancy_test() {
  let runtime_state =
    ui_protocol.runtime_state_from_agent_slot_occupancy(
      ui_protocol.RuntimeMetadata(
        "test-host",
        "scherzo test-version",
        None,
        -1,
        None,
      ),
      1,
    )
  let heartbeat = ui_protocol.encode_heartbeat(42, None, runtime_state, None)
  assert string.contains(heartbeat, "\"capacity\":0")
  assert string.contains(heartbeat, "\"active\":1")
  assert string.contains(heartbeat, "\"used\":1")
  assert string.contains(heartbeat, "\"known\":true")
}

pub fn ui_protocol_clamps_negative_agent_slot_occupancy_test() {
  let runtime_state =
    ui_protocol.runtime_state_from_agent_slot_occupancy(
      ui_protocol.RuntimeMetadata(
        "test-host",
        "scherzo test-version",
        None,
        4,
        None,
      ),
      -3,
    )
  let heartbeat = ui_protocol.encode_heartbeat(42, None, runtime_state, None)
  assert string.contains(heartbeat, "\"capacity\":4")
  assert string.contains(heartbeat, "\"active\":0")
  assert string.contains(heartbeat, "\"used\":0")
  assert string.contains(heartbeat, "\"known\":true")
}

pub fn ui_protocol_marks_runtime_state_unknown_on_slot_occupancy_error_test() {
  let heartbeat =
    ui_protocol.encode_heartbeat_with_runtime(
      42,
      ui_protocol.RuntimeMetadata(
        "test-host",
        "scherzo test-version",
        None,
        4,
        None,
      ),
      Error("slot_occupancy_unavailable"),
    )
  assert string.contains(heartbeat, "\"capacity\":4")
  assert string.contains(heartbeat, "\"active\":0")
  assert string.contains(heartbeat, "\"used\":0")
  assert string.contains(heartbeat, "\"known\":false")
}

pub fn ui_protocol_encodes_daemon_label_metadata_test() {
  let runtime_state = test_runtime_state(Some("Project Foo / MacBook"))
  let hello =
    ui_protocol.encode_client_message(ui_protocol.DaemonHello(
      "daemon_abc",
      "boot_abc",
      Some("Project Foo / MacBook"),
      runtime_state,
    ))
  assert string.contains(hello, "\"daemonLabel\":\"Project Foo / MacBook\"")

  let heartbeat =
    ui_protocol.encode_client_message(ui_protocol.Heartbeat(
      42,
      Some("Project Foo / MacBook"),
      runtime_state,
      None,
    ))
  assert string.contains(heartbeat, "\"daemonLabel\":\"Project Foo / MacBook\"")

  let state =
    ui_protocol.encode_client_message(
      ui_protocol.DaemonState(
        42,
        False,
        Some("Project Foo / MacBook"),
        runtime_state,
        [],
      ),
    )
  assert string.contains(state, "\"daemonLabel\":\"Project Foo / MacBook\"")
}

pub fn ui_protocol_decodes_server_messages_test() {
  let assert Ok(ui_protocol.ServerHello(Some(1500), None)) =
    ui_protocol.decode_server_message(
      "{\"type\":\"server_hello\",\"heartbeatIntervalMs\":1500}",
    )
  let assert Ok(ui_protocol.ServerHello(None, Some("runtime_secret_1"))) =
    ui_protocol.decode_server_message(
      "{\"type\":\"server_hello\",\"runtimeCredential\":\"runtime_secret_1\"}",
    )
  let assert Ok(ui_protocol.CredentialRevoked(reason)) =
    ui_protocol.decode_server_message(
      "{\"type\":\"credential_revoked\",\"reason\":\"revoked\"}",
    )
  assert reason == "revoked"

  let assert Ok(ui_protocol.DaemonIdentityRevoked(identity_reason)) =
    ui_protocol.decode_server_message(
      "{\"type\":\"daemon_identity_revoked\",\"reason\":\"identity revoked\"}",
    )
  assert identity_reason == "identity revoked"
}

pub fn ui_protocol_decodes_work_item_action_server_command_test() {
  let payload =
    "{\"type\":\"server_command\",\"serverCommandId\":\"cmd-1\",\"daemonId\":\"daemon_abc\",\"bootId\":\"boot_abc\",\"command\":{\"type\":\"work_item_action\",\"action_id\":\"work_subtask.cancel\",\"action_instance_id\":\"wia_1\",\"target_kind\":\"workflow_subtask\",\"target_provider\":\"linear\",\"target_id\":\"issue-1\",\"observed_fingerprint\":\"fp-1\",\"idempotency_key\":\"idem-1\",\"params\":[]}}"

  let assert Ok(ui_protocol.ServerCommand(
    _,
    _,
    _,
    command.WorkItemAction(request),
  )) = ui_protocol.decode_server_message(payload)
  assert request.action_id == "work_subtask.cancel"
  assert request.target_id == "issue-1"
}

pub fn ui_protocol_decodes_query_request_test() {
  let payload =
    "{\"type\":\"query_request\",\"queryId\":\"query-1\",\"daemonId\":\"daemon_abc\",\"bootId\":\"boot_abc\",\"query\":{\"version\":1,\"type\":\"status\"}}"

  let assert Ok(ui_protocol.QueryRequest(query_id, daemon_id, boot_id, query)) =
    ui_protocol.decode_server_message(payload)

  assert query_id == "query-1"
  assert daemon_id == "daemon_abc"
  assert boot_id == "boot_abc"
  assert query == query_types.Status
}

pub fn ui_protocol_encodes_query_response_test() {
  let ok_payload =
    ui_protocol.encode_query_response(
      "query-ok",
      Ok(
        query_types.StatusResponse(
          query_types.StatusDto(
            daemon_id: "daemon_abc",
            boot_id: "boot_abc",
            dispatch_paused: False,
            ui_server_enabled: True,
            supported_queries: ["status"],
          ),
        ),
      ),
    )
  assert string.contains(ok_payload, "\"type\":\"query_response\"")
  assert string.contains(ok_payload, "\"queryId\":\"query-ok\"")
  assert string.contains(ok_payload, "\"ok\":true")
  assert string.contains(ok_payload, "\"status\":{")

  let error_payload =
    ui_protocol.encode_query_response(
      "query-error",
      Error(query_types.QueryError(query_types.QueryTimeout, "query timed out")),
    )
  assert string.contains(error_payload, "\"type\":\"query_response\"")
  assert string.contains(error_payload, "\"queryId\":\"query-error\"")
  assert string.contains(error_payload, "\"ok\":false")
  assert string.contains(error_payload, "\"code\":\"query_timeout\"")
}

pub fn ui_protocol_rejects_malformed_query_request_with_query_id_test() {
  let payload =
    "{\"type\":\"query_request\",\"queryId\":\"query-bad\",\"daemonId\":\"daemon_abc\",\"bootId\":\"boot_abc\",\"query\":{\"version\":1,\"type\":\"mystery\"}}"

  let assert Error(ui_protocol.DecodeError(code: code, message: message)) =
    ui_protocol.decode_server_message(payload)
  assert code == "unsupported_query"
  assert string.contains(message, "unsupported query type")

  let assert Ok(#(query_id, query_error)) =
    ui_protocol.decode_query_request_rejection(payload)
  assert query_id == "query-bad"
  assert query_error
    == query_types.QueryError(
      query_types.UnsupportedQuery,
      "unsupported query type: mystery",
    )
}

pub fn ui_protocol_rejects_bad_json_test() {
  let assert Error(ui_protocol.DecodeError(code: code, message: message)) =
    ui_protocol.decode_server_message("{not-json}")
  assert code == "bad_json"
  assert message == "malformed UI websocket JSON"
}

fn test_runtime_state(
  daemon_label: Option(String),
) -> ui_protocol.DaemonRuntimeState {
  ui_protocol.DaemonRuntimeState(
    "test-host",
    "scherzo test-version",
    daemon_label,
    ui_protocol.AgentSlotState(4, 1, 1, True),
  )
}
