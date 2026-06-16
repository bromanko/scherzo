import gleam/option.{None, Some}
import gleam/string
import scherzo/control/command
import scherzo/control/query/dto as query_dto
import scherzo/control/query/types as query_types
import scherzo/control/remote_envelope
import scherzo/task
import scherzo/work_item

pub fn remote_envelope_roundtrips_all_message_shapes_test() {
  assert_roundtrip(
    remote_envelope.RemoteHello(["control_commands", "session_snapshots"]),
  )
  assert_roundtrip(remote_envelope.RemoteHeartbeat(1234))
  assert_roundtrip(remote_envelope.RemoteServerCommand(
    "cmd-1",
    command.PromptSession("session-1", "  continue please  "),
  ))
  assert_roundtrip(remote_envelope.RemoteQueryRequest(
    "query-1",
    query_types.Status,
  ))
  assert_roundtrip(remote_envelope.RemoteQueryRequest(
    "query-1-metrics",
    query_types.Metrics,
  ))
  assert_roundtrip(remote_envelope.RemoteQueryRequest(
    "query-task-list",
    query_types.TaskList(query_types.TaskListQuery(
      states: [task.Ready],
      limit: 10,
      cursor: Some("cursor:10"),
    )),
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
  assert_roundtrip(remote_envelope.RemoteQueryResponse(
    "query-2",
    Ok(
      query_types.StatusResponse(
        query_types.StatusDto(
          daemon_id: "daemon-1",
          boot_id: "boot-1",
          dispatch_paused: False,
          ui_server_enabled: True,
          supported_queries: ["status"],
        ),
      ),
    ),
  ))
  assert_roundtrip(remote_envelope.RemoteQueryResponse(
    "query-2-metrics",
    Ok(query_types.MetricsResponse(
      query_types.default_operational_metrics_source(
        daemon_id: "daemon-1",
        boot_id: "boot-1",
      )
      |> query_dto.operational_metrics_from_source,
    )),
  ))
  assert_roundtrip(remote_envelope.RemoteQueryResponse(
    "query-3",
    Error(query_types.QueryError(query_types.QueryTimeout, "query timed out")),
  ))
  assert_roundtrip(remote_envelope.RemoteQueryResponse(
    "query-task-list-response",
    Ok(
      query_types.TaskListResponse(query_types.TaskListDto(
        items: [task_summary()],
        page: query_types.PageDto(next_cursor: None, has_more: False),
      )),
    ),
  ))
  assert_roundtrip(remote_envelope.RemoteQueryRequest(
    "query-work-item-show",
    query_types.WorkItemShow(
      query_types.WorkItemShowQuery(ref: query_types.TaskDisplayId("LIV-1")),
    ),
  ))
  assert_roundtrip(remote_envelope.RemoteQueryResponse(
    "query-work-item-show-response",
    Ok(
      query_types.WorkItemShowResponse(work_item.WorkItemDetail(
        summary: work_item.WorkItemSummary(
          id: "linear:issue-1",
          source: work_item.WorkItemSource(
            provider: "linear",
            id: "issue-1",
            display_id: Some("LIV-1"),
            url: None,
          ),
          title: "Work item",
          state: task.TaskState(
            id: Some("todo"),
            name: "Todo",
            category: task.Ready,
          ),
          labels: [],
          labels_truncated: False,
          created_at: None,
          updated_at: None,
        ),
        subtasks: [],
        subtasks_truncated: False,
      )),
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
    "{\"version\":1,\"type\":\"query_request\",\"query_id\":\"q-1\"}",
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

pub fn remote_envelope_extracts_rejected_server_command_result_with_id_test() {
  let assert Ok(#(command_id, result)) =
    remote_envelope.decode_server_command_rejection(
      "{\"version\":1,\"type\":\"server_command\",\"command_id\":\"cmd-1\",\"command\":{\"type\":\"mystery\"}}",
    )
  assert command_id == "cmd-1"
  assert result.command == "mystery"
  assert result.status == command.Rejected("unknown_command")
  assert result.message == Some("unknown command type: mystery")
}

pub fn remote_envelope_rejected_server_command_result_falls_back_to_unknown_command_test() {
  let assert Ok(#(command_id, result)) =
    remote_envelope.decode_server_command_rejection(
      "{\"version\":1,\"type\":\"server_command\",\"command_id\":\"cmd-2\"}",
    )
  assert command_id == "cmd-2"
  assert result.command == "unknown"
  assert result.status == command.Rejected("invalid_envelope")
  assert result.message == Some("missing command")
}

pub fn remote_envelope_rejects_invalid_nested_query_payloads_test() {
  assert_invalid_envelope(
    "{\"version\":1,\"type\":\"query_request\",\"query_id\":\"q-1\",\"query\":{\"version\":1,\"type\":\"mystery\"}}",
    "unsupported_query",
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

fn task_summary() -> query_types.TaskSummaryDto {
  query_types.TaskSummaryDto(
    id: "linear:issue-1",
    source: query_types.TaskSourceDto(
      provider: "linear",
      id: "issue-1",
      display_id: Some("LIV-1"),
      url: None,
    ),
    title: "Task list item",
    state: task.Ready,
    priority: None,
    labels: [],
    created_at: None,
    updated_at: None,
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
