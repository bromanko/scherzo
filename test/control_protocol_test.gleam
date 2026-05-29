import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/agent/pi_event
import scherzo/control/command
import scherzo/control/protocol
import scherzo/session/event
import scherzo/session/reason as session_reason
import scherzo/session/tokens as session_tokens
import scherzo/turn_telemetry

pub fn decode_ping_request_requires_token_test() {
  let assert Ok(protocol.Ping("1", "secret")) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"ping\",\"id\":\"1\",\"token\":\"secret\"}",
    )

  let assert Error(protocol.RequestError(id: "1", code: code, message: _)) =
    protocol.decode_request("{\"version\":1,\"type\":\"ping\",\"id\":\"1\"}")
  assert code == "invalid_request"
}

pub fn unknown_command_returns_stable_error_test() {
  let assert Error(err) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"delete_everything\",\"id\":\"9\",\"token\":\"secret\"}",
    )
  assert err.code == "unknown_command"

  let encoded =
    protocol.request_error_response(err) |> protocol.response_to_string
  assert string.contains(encoded, "unknown_command")
  assert string.contains(encoded, "\"ok\":false")
  assert string.contains(encoded, "\"id\":\"9\"")
}

pub fn decode_request_rejects_empty_session_id_test() {
  let assert Error(protocol.RequestError(id: "2", code: code, message: _)) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"get_session\",\"id\":\"2\",\"token\":\"secret\",\"session_id\":\"\"}",
    )
  assert code == "invalid_request"

  let assert Error(protocol.RequestError(id: "3", code: code, message: _)) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"get_events\",\"id\":\"3\",\"token\":\"secret\",\"session_id\":\"\",\"after\":0,\"limit\":10}",
    )
  assert code == "invalid_request"

  let assert Error(protocol.RequestError(id: "6", code: code, message: _)) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"stream_events\",\"id\":\"6\",\"token\":\"secret\",\"session_id\":\"\",\"after\":0}",
    )
  assert code == "invalid_request"
}

pub fn decode_request_rejects_negative_after_test() {
  let assert Error(protocol.RequestError(id: "4", code: code, message: _)) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"get_events\",\"id\":\"4\",\"token\":\"secret\",\"session_id\":\"session-1\",\"after\":-1,\"limit\":10}",
    )
  assert code == "invalid_request"

  let assert Error(protocol.RequestError(id: "5", code: code, message: _)) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"stream_events\",\"id\":\"5\",\"token\":\"secret\",\"session_id\":\"session-1\",\"after\":-1}",
    )
  assert code == "invalid_request"
}

pub fn mutating_command_requests_roundtrip_to_operator_commands_test() {
  assert_command_roundtrip("1", command.PauseDispatch)
  assert_command_roundtrip("2", command.ResumeDispatch)
  assert_command_roundtrip("3", command.ReloadWorkflow)
  assert_command_roundtrip(
    "4",
    command.RetryIssue(command.IssueIdentifier("ABC-123")),
  )
  assert_command_roundtrip(
    "4b",
    command.RetryWorkflowStep(
      command.RetryWorkflowStepAutoTarget("ABC-123"),
      Some("build"),
    ),
  )
  assert_command_roundtrip(
    "5",
    command.ParkIssue(command.IssueId("issue-123"), "manual hold"),
  )
  assert_command_roundtrip(
    "6",
    command.UnparkIssue(command.IssueIdentifier("ABC-123")),
  )
  assert_command_roundtrip("7", command.AbortSession("session-1"))
  assert_command_roundtrip("8", command.StopAfterCurrentTurn("session-1"))
  assert_command_roundtrip(
    "9",
    command.PromptSession("session-1", "continue please"),
  )
  assert_command_roundtrip(
    "10",
    command.RespondUi("session-1", "ui-1", command.UiCancel),
  )
  assert_command_roundtrip(
    "11",
    command.RespondUi("session-1", "ui-2", command.UiValue("choice")),
  )
  assert_command_roundtrip("12", command.RunScheduleNow("nightly-repair"))
}

pub fn mutating_command_aliases_decode_test() {
  let assert Ok(protocol.ReloadWorkflow("1", "secret")) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"reload_workflow\",\"id\":\"1\",\"token\":\"secret\"}",
    )

  let assert Ok(protocol.RetryIssue(_, _, command.IssueId("issue-1"))) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"retry_issue\",\"id\":\"2\",\"token\":\"secret\",\"issue_id\":\"issue-1\"}",
    )

  let assert Ok(protocol.RetryWorkflowStep(
    _,
    _,
    command.RetryWorkflowStepRunId("run-1"),
    Some("step-1"),
  )) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"retry_step\",\"id\":\"2b\",\"token\":\"secret\",\"run_id\":\"run-1\",\"step_id\":\"step-1\"}",
    )

  let assert Ok(protocol.RespondUi(
    _,
    _,
    "session-1",
    "ui-1",
    command.UiValue("ok"),
  )) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"ui_respond\",\"id\":\"3\",\"token\":\"secret\",\"session_id\":\"session-1\",\"request_id\":\"ui-1\",\"value\":\"ok\"}",
    )

  let assert Ok(protocol.ParkIssue(_, _, command.IssueId("issue-2"), "hold")) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"park_issue\",\"id\":\"4\",\"token\":\"secret\",\"issue_id\":\"issue-2\",\"reason\":\"hold\"}",
    )

  let assert Ok(protocol.UnparkIssue(_, _, command.IssueIdentifier("ABC-2"))) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"unpark_issue\",\"id\":\"5\",\"token\":\"secret\",\"issue_identifier\":\"ABC-2\"}",
    )

  let assert Ok(protocol.AbortSession(_, _, "session-2")) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"abort_session\",\"id\":\"6\",\"token\":\"secret\",\"session_id\":\"session-2\"}",
    )

  let assert Ok(protocol.StopAfterCurrentTurn(_, _, "session-3")) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"stop_after_turn\",\"id\":\"7\",\"token\":\"secret\",\"session_id\":\"session-3\"}",
    )

  let assert Ok(protocol.PromptSession(_, _, "session-4", "hello")) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"prompt_session\",\"id\":\"8\",\"token\":\"secret\",\"session_id\":\"session-4\",\"message\":\"hello\"}",
    )

  let assert Ok(protocol.RunScheduleNow(_, _, "nightly-repair")) =
    protocol.decode_request(
      "{\"version\":1,\"type\":\"run_schedule_now\",\"id\":\"9\",\"token\":\"secret\",\"job_id\":\"nightly-repair\"}",
    )
}

pub fn invalid_mutating_commands_return_invalid_request_test() {
  assert_invalid_request(
    "{\"version\":1,\"type\":\"retry\",\"id\":\"1\",\"token\":\"secret\"}",
  )
  assert_invalid_request(
    "{\"version\":1,\"type\":\"retry\",\"id\":\"2\",\"token\":\"secret\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"ABC-1\"}",
  )
  assert_invalid_request(
    "{\"version\":1,\"type\":\"retry\",\"id\":\"3\",\"token\":\"secret\",\"issue_identifier\":\"   \"}",
  )
  assert_invalid_request(
    "{\"version\":1,\"type\":\"retry_step\",\"id\":\"3b\",\"token\":\"secret\"}",
  )
  assert_invalid_request(
    "{\"version\":1,\"type\":\"retry_step\",\"id\":\"3c\",\"token\":\"secret\",\"target\":\"ABC-1\",\"run_id\":\"run-1\"}",
  )
  assert_invalid_request(
    "{\"version\":1,\"type\":\"retry_step\",\"id\":\"3d\",\"token\":\"secret\",\"target\":\"ABC-1\",\"step_id\":\"   \"}",
  )
  assert_invalid_request(
    "{\"version\":1,\"type\":\"park\",\"id\":\"4\",\"token\":\"secret\",\"issue_id\":\"issue-1\"}",
  )
  assert_invalid_request(
    "{\"version\":1,\"type\":\"park\",\"id\":\"5\",\"token\":\"secret\",\"issue_id\":\"issue-1\",\"reason\":\"   \"}",
  )
  assert_invalid_request(
    "{\"version\":1,\"type\":\"prompt\",\"id\":\"6\",\"token\":\"secret\",\"session_id\":\"session-1\"}",
  )
  assert_invalid_request(
    "{\"version\":1,\"type\":\"prompt\",\"id\":\"7\",\"token\":\"secret\",\"session_id\":\"session-1\",\"message\":\"  \"}",
  )
  assert_invalid_request(
    "{\"version\":1,\"type\":\"respond_ui\",\"id\":\"8\",\"token\":\"secret\",\"session_id\":\"session-1\",\"cancel\":true}",
  )
  assert_invalid_request(
    "{\"version\":1,\"type\":\"respond_ui\",\"id\":\"9\",\"token\":\"secret\",\"session_id\":\"session-1\",\"request_id\":\"   \",\"cancel\":true}",
  )
  assert_invalid_request(
    "{\"version\":1,\"type\":\"respond_ui\",\"id\":\"10\",\"token\":\"secret\",\"session_id\":\"session-1\",\"request_id\":\"ui-1\",\"cancel\":false}",
  )
  assert_invalid_request(
    "{\"version\":1,\"type\":\"respond_ui\",\"id\":\"11\",\"token\":\"secret\",\"session_id\":\"session-1\",\"request_id\":\"ui-1\",\"cancel\":true,\"value\":\"ok\"}",
  )
  assert_invalid_request(
    "{\"version\":1,\"type\":\"respond_ui\",\"id\":\"12\",\"token\":\"secret\",\"session_id\":\"session-1\",\"request_id\":\"ui-1\"}",
  )
  assert_invalid_request(
    "{\"version\":1,\"type\":\"schedule_run_now\",\"id\":\"13\",\"token\":\"secret\"}",
  )
  assert_invalid_request(
    "{\"version\":1,\"type\":\"schedule_run_now\",\"id\":\"14\",\"token\":\"secret\",\"job_id\":\"   \"}",
  )
}

fn assert_command_roundtrip(
  id: String,
  operator_command: command.OperatorCommand,
) -> Nil {
  let request = protocol.command_request(id, "secret", operator_command)
  let assert Ok(decoded) =
    protocol.decode_request(protocol.request_to_string(request))
  assert protocol.request_id(decoded) == id
  assert protocol.request_token(decoded) == "secret"
  assert protocol.request_operator_command(decoded) == Some(operator_command)
}

fn assert_invalid_request(line: String) -> Nil {
  let assert Error(protocol.RequestError(id: _, code: code, message: _)) =
    protocol.decode_request(line)
  assert code == "invalid_request"
}

pub fn command_result_rejections_are_success_data_test() {
  let rejected =
    command.CommandResult(
      command: "abort",
      status: command.Rejected("busy"),
      target: Some("session-1"),
      message: Some("session is busy"),
    )
    |> protocol.command_result_data
  let rejected_encoded =
    protocol.success_response("cmd-1", rejected) |> protocol.response_to_string

  assert string.contains(rejected_encoded, "\"ok\":true")
  assert string.contains(rejected_encoded, "\"status\":\"rejected\"")
  assert string.contains(rejected_encoded, "\"reason\":\"busy\"")

  let assert Ok(decoded_rejected) =
    protocol.decode_command_result_response(rejected_encoded)
  assert decoded_rejected.command == "abort"
  assert decoded_rejected.target == Some("session-1")
  assert decoded_rejected.message == Some("session is busy")
  assert command.status_to_string(decoded_rejected.status) == "rejected"
  assert command.status_reason(decoded_rejected.status) == Some("busy")

  let not_allowed =
    command.CommandResult(
      command: "prompt",
      status: command.NotAllowed("policy"),
      target: Some("session-2"),
      message: Some("operator policy denied"),
    )
    |> protocol.command_result_data
  let not_allowed_encoded =
    protocol.success_response("cmd-2", not_allowed)
    |> protocol.response_to_string

  assert string.contains(not_allowed_encoded, "\"status\":\"not_allowed\"")
  assert string.contains(not_allowed_encoded, "\"reason\":\"policy\"")

  let assert Ok(decoded_not_allowed) =
    protocol.decode_command_result_response(not_allowed_encoded)
  assert decoded_not_allowed.command == "prompt"
  assert decoded_not_allowed.target == Some("session-2")
  assert decoded_not_allowed.message == Some("operator policy denied")
  assert command.status_to_string(decoded_not_allowed.status) == "not_allowed"
  assert command.status_reason(decoded_not_allowed.status) == Some("policy")
}

pub fn decode_events_response_accepts_missing_and_new_tool_fields_test() {
  let old_json =
    "{\"version\":1,\"id\":\"events-1\",\"ok\":true,\"data\":{\"events\":[{\"cursor\":1,\"at_ms\":100,\"session_id\":\"session-1\",\"issue_id\":\"issue-1\",\"kind\":\"tool\",\"name\":\"tool_execution_start\",\"turn\":1,\"pi_type\":null,\"message\":null,\"request_id\":null,\"method\":null,\"tool_name\":\"bash\",\"tokens\":{\"input\":0,\"output\":0,\"cache_read\":0,\"cache_write\":0,\"total\":0},\"raw_json\":null}],\"next_cursor\":1,\"truncated\":false}}"
  let assert Ok(old_page) = protocol.decode_get_events_response(old_json)
  let assert [old_event] = old_page.events
  assert old_event.payload.tool_name == Some("bash")
  assert old_event.payload.tool_input == None
  assert old_event.payload.tool_output == None
  assert old_event.payload.tool_status == None
  assert old_event.payload.token_delta == session_tokens.zero_token_totals()
  assert old_event.payload.turn_status == None

  let new_json =
    "{\"version\":1,\"id\":\"events-2\",\"ok\":true,\"data\":{\"events\":[{\"cursor\":2,\"at_ms\":101,\"session_id\":\"session-1\",\"issue_id\":\"issue-1\",\"kind\":\"tool\",\"name\":\"tool_execution_end\",\"turn\":1,\"pi_type\":null,\"message\":null,\"request_id\":null,\"method\":null,\"tool_name\":\"bash\",\"tool_input\":\"gleam test\",\"tool_output\":\"ok\",\"tool_status\":\"success\",\"tokens\":{\"input\":0,\"output\":0,\"cache_read\":0,\"cache_write\":0,\"total\":0},\"raw_json\":null}],\"next_cursor\":2,\"truncated\":false}}"
  let assert Ok(new_page) = protocol.decode_get_events_response(new_json)
  let assert [new_event] = new_page.events
  assert new_event.payload.tool_input == Some("gleam test")
  assert new_event.payload.tool_output == Some("ok")
  assert new_event.payload.tool_status == Some("success")
}

pub fn decode_events_response_uses_kind_when_decoding_event_name_test() {
  let tool_json =
    "{\"version\":1,\"id\":\"events-kind\",\"ok\":true,\"data\":{\"events\":[{\"cursor\":1,\"at_ms\":100,\"session_id\":\"session-1\",\"issue_id\":\"issue-1\",\"kind\":\"tool\",\"name\":\"worker_started\",\"tokens\":{\"input\":0,\"output\":0,\"cache_read\":0,\"cache_write\":0,\"total\":0}}],\"next_cursor\":1,\"truncated\":false}}"
  let assert Ok(tool_page) = protocol.decode_get_events_response(tool_json)
  let assert [tool_event] = tool_page.events
  assert tool_event.payload.name
    == event.PiName(pi_event.UnknownPiEvent("worker_started"))

  let lifecycle_json =
    "{\"version\":1,\"id\":\"events-lifecycle\",\"ok\":true,\"data\":{\"events\":[{\"cursor\":2,\"at_ms\":101,\"session_id\":\"session-1\",\"issue_id\":\"issue-1\",\"kind\":\"lifecycle\",\"name\":\"worker_started\",\"tokens\":{\"input\":0,\"output\":0,\"cache_read\":0,\"cache_write\":0,\"total\":0}}],\"next_cursor\":2,\"truncated\":false}}"
  let assert Ok(lifecycle_page) =
    protocol.decode_get_events_response(lifecycle_json)
  let assert [lifecycle_event] = lifecycle_page.events
  assert lifecycle_event.payload.name
    == event.LifecycleName(event.WorkerStarted)
}

pub fn decode_turn_event_response_with_token_delta_and_reason_test() {
  let line =
    "{\"version\":1,\"id\":\"events-turn\",\"ok\":true,\"data\":{\"events\":[{\"cursor\":3,\"at_ms\":2500,\"session_id\":\"session-1\",\"issue_id\":\"issue-1\",\"kind\":\"turn\",\"name\":\"turn_finished\",\"turn\":1,\"turn_status\":\"finished\",\"turn_duration_ms\":1500,\"tokens\":{\"input\":10,\"output\":5,\"cache_read\":0,\"cache_write\":0,\"total\":15},\"token_delta\":{\"input\":10,\"output\":5,\"cache_read\":0,\"cache_write\":0,\"total\":15},\"reason\":\"operator_stop_after_current_turn\"}],\"next_cursor\":3,\"truncated\":false}}"

  let assert Ok(page) = protocol.decode_get_events_response(line)
  let assert [stored_event] = page.events
  assert stored_event.payload.kind == event.Turn
  assert stored_event.payload.name
    == event.TurnName(turn_telemetry.EventFinished)
  assert stored_event.payload.turn_status == Some(turn_telemetry.StatusFinished)
  assert stored_event.payload.token_delta.total == 15
  assert stored_event.payload.reason
    == Some(turn_telemetry.ReasonOperatorStopAfterCurrentTurn)
}

pub fn decode_turn_event_rejects_free_form_secret_reason_test() {
  let line =
    "{\"version\":1,\"id\":\"events-turn\",\"ok\":true,\"data\":{\"events\":[{\"cursor\":1,\"at_ms\":100,\"session_id\":\"session-1\",\"issue_id\":\"issue-1\",\"kind\":\"turn\",\"name\":\"turn_failed\",\"turn\":1,\"reason\":\"SECRET_PROMPT in reason\",\"message\":\"SECRET_PROMPT\",\"raw_json\":{\"value\":\"SECRET_PROMPT\",\"truncated\":false}}],\"next_cursor\":1,\"truncated\":false}}"

  let assert Ok(page) = protocol.decode_get_events_response(line)
  let assert [stored_event] = page.events
  assert stored_event.payload.reason == None
  assert stored_event.payload.message == None
  assert stored_event.payload.raw_json == None
  let encoded = protocol.event_page_data(page) |> json.to_string
  assert !string.contains(encoded, "SECRET_PROMPT")
}

pub fn decode_unknown_future_turn_name_stays_turn_event_test() {
  let line =
    "{\"version\":1,\"id\":\"events-turn\",\"ok\":true,\"data\":{\"events\":[{\"cursor\":1,\"at_ms\":100,\"session_id\":\"session-1\",\"issue_id\":\"issue-1\",\"kind\":\"turn\",\"name\":\"turn_paused\",\"turn\":1}],\"next_cursor\":1,\"truncated\":false}}"

  let assert Ok(page) = protocol.decode_get_events_response(line)
  let assert [stored_event] = page.events
  assert stored_event.payload.kind == event.Turn
  assert stored_event.payload.name
    == event.TurnName(turn_telemetry.EventUnknown("turn_paused"))
}

pub fn session_summary_recovery_json_preserves_live_status_and_decodes_missing_test() {
  let recovery =
    event.RecoveryInfo(
      status: event.Interrupted,
      source: "projection.run_interrupted",
      message: Some("daemon_restart"),
      safe_actions: [event.Inspect, event.ViewEvents, event.Retry, event.Park],
      workflow_run_id: Some("run-1"),
      workflow_step_id: None,
      workflow_attempt_index: None,
      parent_session_id: None,
      orphan_status: None,
      issue_state: None,
      recommended_action: None,
      current_pi_session_id: Some("pi-current"),
      previous_pi_session_id: None,
      park_reason: None,
      park_release_policy: None,
      parked_at_ms: None,
      drift_kind: None,
      retention_until_ms: None,
      cleanup_eligible_at_ms: None,
      cleanup_phase: None,
    )
  let summary =
    event.SessionSummary(
      session_id: "session-1",
      display_name: "session-1",
      issue_id: "issue-1",
      issue_identifier: "SCH-1",
      issue_title: "Fix",
      workspace_path: "work",
      pi_session_id: Some("pi-current"),
      status: event.Running,
      recovery: Some(recovery),
      current_turn: 1,
      current_turn_status: None,
      current_turn_started_at_ms: None,
      last_turn_finished_at_ms: None,
      last_turn_duration_ms: None,
      last_turn_token_delta: session_tokens.zero_token_totals(),
      last_turn_reason: None,
      started_at_ms: 100,
      last_event_at_ms: 200,
      token_totals: session_tokens.zero_token_totals(),
    )
  let encoded =
    protocol.success_response(
      "sessions-1",
      protocol.list_sessions_data(event.SessionList([summary], 250)),
    )
    |> protocol.response_to_string

  assert string.contains(encoded, "\"status\":\"running\"")
  assert string.contains(encoded, "\"recovery\":{")
  assert string.contains(encoded, "\"status\":\"interrupted\"")
  assert string.contains(encoded, "\"workflow_run_id\":\"run-1\"")

  let assert Ok(snapshot) =
    protocol.decode_list_sessions_snapshot_response(encoded)
  let assert [decoded] = snapshot.sessions
  assert decoded.status == event.Running
  let assert Some(decoded_recovery) = decoded.recovery
  assert decoded_recovery.status == event.Interrupted
  assert decoded_recovery.workflow_run_id == Some("run-1")

  let missing_recovery_json =
    "{\"version\":1,\"id\":\"sessions-2\",\"ok\":true,\"data\":{\"sessions\":[{\"session_id\":\"session-2\",\"issue_id\":\"issue-2\",\"issue_identifier\":\"SCH-2\",\"issue_title\":\"Fix\",\"workspace_path\":\"work\",\"pi_session_id\":null,\"status\":\"running\",\"current_turn\":1,\"started_at_ms\":100,\"last_event_at_ms\":200,\"tokens\":{\"input\":0,\"output\":0,\"cache_read\":0,\"cache_write\":0,\"total\":0}}]}}"
  let assert Ok([missing]) =
    protocol.decode_list_sessions_response(missing_recovery_json)
  assert missing.recovery == None
}

pub fn decode_session_summary_maps_unknown_exit_reason_to_failed_test() {
  let line =
    "{\"version\":1,\"id\":\"sessions-1\",\"ok\":true,\"data\":{\"sessions\":[{\"session_id\":\"session-1\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"SCH-1\",\"issue_title\":\"Fix\",\"workspace_path\":\"work\",\"pi_session_id\":null,\"status\":\"exited\",\"exit_reason\":\"legacy_cancelled\",\"current_turn\":1,\"started_at_ms\":100,\"last_event_at_ms\":200,\"tokens\":{\"input\":0,\"output\":0,\"cache_read\":0,\"cache_write\":0,\"total\":0}}]}}"

  let assert Ok([summary]) = protocol.decode_list_sessions_response(line)
  assert summary.display_name == "session-1"
  assert summary.status == event.Exited(session_reason.Failed)
  assert summary.current_turn_status == None
  assert summary.current_turn_started_at_ms == None
  assert summary.last_turn_token_delta == session_tokens.zero_token_totals()
  assert summary.last_turn_reason == None
  let assert Ok(snapshot) =
    protocol.decode_list_sessions_snapshot_response(line)
  assert snapshot.now_ms == 200
}

pub fn decode_list_sessions_snapshot_reads_server_now_ms_test() {
  let line =
    "{\"version\":1,\"id\":\"sessions-1\",\"ok\":true,\"data\":{\"now_ms\":250,\"sessions\":[{\"session_id\":\"session-1\",\"display_name\":\"LIV-43-fancy-narwhal-finger\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"SCH-1\",\"issue_title\":\"Fix\",\"workspace_path\":\"work\",\"pi_session_id\":null,\"status\":\"running\",\"current_turn\":1,\"started_at_ms\":100,\"last_event_at_ms\":200,\"tokens\":{\"input\":0,\"output\":0,\"cache_read\":0,\"cache_write\":0,\"total\":0}}]}}"

  let assert Ok(snapshot) =
    protocol.decode_list_sessions_snapshot_response(line)
  assert snapshot.now_ms == 250
  assert list.map(snapshot.sessions, fn(summary) { summary.session_id })
    == ["session-1"]
  assert list.map(snapshot.sessions, fn(summary) { summary.display_name })
    == ["LIV-43-fancy-narwhal-finger"]
}

pub fn encode_events_response_contains_cursor_and_session_test() {
  let page =
    event.EventPage(
      events: [
        event.SessionEvent(
          cursor: 7,
          at_ms: 100,
          session_id: "session-1",
          issue_id: "issue-1",
          payload: event.empty_payload(
            event.Lifecycle,
            event.LifecycleName(event.WorkerStarted),
          ),
        ),
      ],
      next_cursor: 7,
      truncated: False,
    )

  let encoded =
    protocol.success_response("events-1", protocol.event_page_data(page))
    |> protocol.response_to_string

  assert string.contains(encoded, "\"id\":\"events-1\"")
  assert string.contains(encoded, "\"session_id\":\"session-1\"")
  assert string.contains(encoded, "\"cursor\":7")
  assert string.contains(encoded, "\"next_cursor\":7")
  assert string.contains(encoded, "\"truncated\":false")
  assert string.contains(encoded, "\"ok\":true")

  let assert Ok(decoded) = protocol.decode_get_events_response(encoded)
  assert decoded.next_cursor == 7
  assert decoded.truncated == False
}

pub fn lifecycle_event_response_carries_nullable_recovery_test() {
  let recovery =
    event.RecoveryInfo(
      status: event.Interrupted,
      source: "projection.run_interrupted",
      message: Some("daemon_restart"),
      safe_actions: [event.Inspect, event.ViewEvents, event.Retry, event.Park],
      workflow_run_id: Some("run-1"),
      workflow_step_id: None,
      workflow_attempt_index: None,
      parent_session_id: None,
      orphan_status: None,
      issue_state: None,
      recommended_action: None,
      current_pi_session_id: None,
      previous_pi_session_id: None,
      park_reason: None,
      park_release_policy: None,
      parked_at_ms: None,
      drift_kind: None,
      retention_until_ms: None,
      cleanup_eligible_at_ms: None,
      cleanup_phase: None,
    )
  let page =
    event.EventPage(
      events: [
        event.SessionEvent(
          cursor: 8,
          at_ms: 101,
          session_id: "session-1",
          issue_id: "issue-1",
          payload: event.EventPayload(
            ..event.empty_payload(
              event.Lifecycle,
              event.LifecycleName(event.RecoveryInterrupted),
            ),
            recovery: Some(recovery),
            message: Some("daemon_restart"),
          ),
        ),
      ],
      next_cursor: 8,
      truncated: False,
    )

  let encoded =
    protocol.success_response("events-2", protocol.event_page_data(page))
    |> protocol.response_to_string

  assert string.contains(encoded, "\"name\":\"recovery_interrupted\"")
  assert string.contains(encoded, "\"recovery\":{")
  assert string.contains(encoded, "\"workflow_run_id\":\"run-1\"")

  let assert Ok(decoded) = protocol.decode_get_events_response(encoded)
  let assert [stored_event] = decoded.events
  let assert Some(decoded_recovery) = stored_event.payload.recovery
  assert decoded_recovery.status == event.Interrupted
  assert decoded_recovery.workflow_run_id == Some("run-1")
}
