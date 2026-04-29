import gleam/option.{None, Some}
import gleam/string
import scherzo/control/command
import scherzo/control/protocol
import scherzo/domain
import scherzo/session/event

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
  let data =
    command.CommandResult(
      command: "abort",
      status: command.Rejected("busy"),
      target: Some("session-1"),
      message: Some("session is busy"),
    )
    |> protocol.command_result_data
  let encoded =
    protocol.success_response("cmd-1", data) |> protocol.response_to_string

  assert string.contains(encoded, "\"ok\":true")
  assert string.contains(encoded, "\"status\":\"rejected\"")
  assert string.contains(encoded, "\"reason\":\"busy\"")

  let assert Ok(decoded) = protocol.decode_command_result_response(encoded)
  assert decoded.command == "abort"
  assert command.status_to_string(decoded.status) == "rejected"
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

  let new_json =
    "{\"version\":1,\"id\":\"events-2\",\"ok\":true,\"data\":{\"events\":[{\"cursor\":2,\"at_ms\":101,\"session_id\":\"session-1\",\"issue_id\":\"issue-1\",\"kind\":\"tool\",\"name\":\"tool_execution_end\",\"turn\":1,\"pi_type\":null,\"message\":null,\"request_id\":null,\"method\":null,\"tool_name\":\"bash\",\"tool_input\":\"gleam test\",\"tool_output\":\"ok\",\"tool_status\":\"success\",\"tokens\":{\"input\":0,\"output\":0,\"cache_read\":0,\"cache_write\":0,\"total\":0},\"raw_json\":null}],\"next_cursor\":2,\"truncated\":false}}"
  let assert Ok(new_page) = protocol.decode_get_events_response(new_json)
  let assert [new_event] = new_page.events
  assert new_event.payload.tool_input == Some("gleam test")
  assert new_event.payload.tool_output == Some("ok")
  assert new_event.payload.tool_status == Some("success")
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
          payload: event.EventPayload(
            kind: event.Lifecycle,
            name: "worker_started",
            turn: None,
            pi_type: None,
            message: None,
            request_id: None,
            method: None,
            tool_name: None,
            tool_input: None,
            tool_output: None,
            tool_status: None,
            tokens: domain.zero_token_totals(),
            raw_json: None,
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
