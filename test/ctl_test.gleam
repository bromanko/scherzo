import gleam/bit_array
import gleam/dict
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_planner
import scherzo/artifact_publication_recording
import scherzo/artifact_repository/command_runner
import scherzo/cleanup
import scherzo/cleanup/cursor as cleanup_cursor
import scherzo/commit_stack_artifact
import scherzo/control/client as control_client
import scherzo/control/command
import scherzo/control/file
import scherzo/control/protocol
import scherzo/control/query/types as query_types
import scherzo/ctl
import scherzo/ctl/artifact_publication_retry as ctl_artifact_publication_retry
import scherzo/ctl/command_registry
import scherzo/ctl/workstream as ctl_workstream
import scherzo/hash
import scherzo/instance_lock
import scherzo/path
import scherzo/runtime_bundle
import scherzo/session/event
import scherzo/session/reason
import scherzo/session/tokens as session_tokens
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/record
import scherzo/task
import scherzo/terminal/style
import scherzo/turn_telemetry
import scherzo/workflow_attempt
import scherzo/workflow_contract
import scherzo/workflow_contract_manifest
import scherzo/workflow_dag
import scherzo/workflow_interface_snapshot
import scherzo/workspace
import scherzo/workspace_manifest
import simplifile
import support/test_helpers
import test_async

const ps_now_ms = -576_460_678_330

type OutMsg {
  OutLine(String)
  OutInline(String)
}

fn control_file() -> file.ControlFile {
  file.ControlFile(
    host: "127.0.0.1",
    port: 1,
    token: "token",
    workspace_root: "test/tmp/ctl-ps/workspaces",
    started_at_ms: 1,
    command_timeout_ms: 60_000,
  )
}

fn control_file_for_root(root: String) -> file.ControlFile {
  file.ControlFile(
    host: "127.0.0.1",
    port: 1,
    token: "token",
    workspace_root: root,
    started_at_ms: 1,
    command_timeout_ms: 60_000,
  )
}

fn write_control_file(path: String) -> Nil {
  let assert Ok(Nil) = file.write(path, control_file())
  Nil
}

fn write_control_file_for_root(path: String, root: String) -> Nil {
  let assert Ok(Nil) = file.write(path, control_file_for_root(root))
  Nil
}

fn caller_cwd_env(cwd: String) -> fn(String) -> Option(String) {
  fn(name) {
    case name == path.caller_cwd_env {
      True -> Some(cwd)
      False -> path.env(name)
    }
  }
}

fn session_summary(
  session_id: String,
  last_event_at_ms: Int,
) -> event.SessionSummary {
  session_summary_with_status(session_id, last_event_at_ms, event.Running)
}

fn session_summary_with_status(
  session_id: String,
  last_event_at_ms: Int,
  status: event.SessionStatus,
) -> event.SessionSummary {
  event.SessionSummary(
    session_id: session_id,
    display_name: session_id,
    issue_id: "issue-1",
    issue_identifier: "LIV-41",
    issue_title: "Improve ctl ps output readability",
    workspace_path: "/tmp/workspace",
    pi_session_id: None,
    status: status,
    recovery: None,
    current_turn: 1,
    current_turn_status: None,
    current_turn_started_at_ms: None,
    last_turn_finished_at_ms: None,
    last_turn_duration_ms: None,
    last_turn_token_delta: session_tokens.zero_token_totals(),
    last_turn_reason: None,
    started_at_ms: last_event_at_ms - 1000,
    last_event_at_ms: last_event_at_ms,
    token_totals: session_tokens.zero_token_totals(),
  )
}

fn ps_deps(
  sessions: List(event.SessionSummary),
  now_ms: Int,
  raw_response: String,
) -> ctl.ControlClient {
  ctl.ControlClient(
    list_sessions: fn(_) {
      Ok(event.SessionList(sessions: sessions, now_ms: now_ms))
    },
    get_session: fn(_, _) { Ok(None) },
    get_events: fn(_, _, _, _) {
      Ok(event.EventPage(events: [], next_cursor: 0, truncated: False))
    },
    stream_events: fn(_, _, _, _) { Ok(Nil) },
    query: fn(_, _) { Ok(query_status_response()) },
    apply_command: fn(_, operator_command) {
      Ok(command.applied(operator_command, None))
    },
    raw_request: fn(_, request) {
      case raw_response == "" {
        True -> Ok(protocol.request_to_string(request))
        False -> Ok(raw_response)
      }
    },
  )
}

fn query_status_response() -> query_types.QueryResponse {
  query_types.StatusResponse(
    query_types.StatusDto(
      daemon_id: "daemon-query",
      boot_id: "boot-query",
      dispatch_paused: True,
      ui_server_enabled: False,
      supported_queries: ["status"],
    ),
  )
}

fn query_metrics_response() -> query_types.QueryResponse {
  query_types.MetricsResponse(query_types.OperationalMetricsDto(
    schema_version: query_types.operational_metrics_schema_version,
    daemon_id: "daemon-query",
    boot_id: "boot-query",
    sampled_at_ms: 456,
    dispatch_paused: True,
    ui_server_enabled: False,
    remote_client_status: "disabled",
    workflow_count: 2,
    scheduled_job_count: 1,
    active_sessions: 3,
    running_workers: 2,
    running_scheduled_workers: 1,
    queued_claims: 4,
    pending_dispatch_validations: 5,
    pending_review_lane_preflights: 6,
    claimed_tasks: 7,
    retry_tasks: 8,
    parked_tasks: 9,
    completed_tasks: 10,
    pending_outbox_count: 11,
    in_flight_outbox_count: 12,
    retryable_outbox_count: 13,
    permanent_outbox_count: 14,
    poll_generation: 10,
    poll_in_flight: False,
    poll_timer_active: True,
    retry_timer_count: 11,
    retry_refresh_in_flight_count: 12,
    lifecycle_projection_failed: True,
    scheduled_due_count: 13,
    scheduled_next_due_count: 19,
    scheduled_pending_count: 14,
    scheduled_retry_count: 15,
    scheduled_report_retry_count: 16,
    scheduled_retry_timer_count: 17,
    scheduled_report_retry_timer_count: 18,
    token_totals: query_types.TokenTotalsDto(
      input: 19,
      output: 20,
      cache_read: 21,
      cache_write: 22,
      total: 42,
    ),
  ))
}

fn query_operation_status_response() -> query_types.QueryResponse {
  query_types.OperationStatusResponse(query_types.OperationStatusDto(
    operation_id: "op-123",
    kind: "retry_step",
    command: "retry_step",
    target: "run:run-1",
    run_id: Some("run-1"),
    issue_id: Some("issue-1"),
    issue_identifier: Some("LIV-1262"),
    requested_step_id: Some("apply_feedback"),
    publication_id: None,
    status: "completed",
    reason: None,
    message: Some("retry-step completed"),
    queued_at_ms: 1000,
    started_at_ms: Some(1001),
    finished_at_ms: Some(1002),
  ))
}

fn task_summary() -> query_types.TaskSummaryDto {
  query_types.TaskSummaryDto(
    id: "linear:issue-770",
    source: query_types.TaskSourceDto(
      provider: "linear",
      id: "issue-770",
      display_id: Some("LIV-770"),
      url: Some("https://linear.app/living-systems/issue/LIV-770"),
    ),
    title: "Implement task queries",
    state: task.Ready,
    priority: Some(query_types.TaskPriorityDto(value: 2, label: "High")),
    labels: [
      query_types.TaskLabelDto(
        id: Some("label-workflow"),
        name: "workflow:implementation",
      ),
    ],
    created_at: Some("2026-04-28T10:00:00Z"),
    updated_at: Some("2026-04-28T11:00:00Z"),
  )
}

fn task_list_response() -> query_types.QueryResponse {
  query_types.TaskListResponse(query_types.TaskListDto(
    items: [task_summary()],
    page: query_types.PageDto(next_cursor: Some("cursor:1"), has_more: True),
  ))
}

fn task_detail_response() -> query_types.QueryResponse {
  query_types.TaskShowResponse(query_types.TaskDetailDto(
    summary: task_summary(),
    description: query_types.TaskDescriptionDto(
      format: "markdown",
      body: "Detailed task body",
    ),
  ))
}

fn outbox_record() -> query_types.OutboxRecordDto {
  query_types.OutboxRecordDto(
    outbox_id: "outbox-1",
    kind: "linear_comment",
    status: query_types.OutboxRetryableStatus,
    task_ref: query_types.OutboxTaskRefDto(
      provider: "linear",
      id: "issue-1087",
      display_id: Some("LIV-1087"),
      url: Some("https://linear.app/living-systems/issue/LIV-1087"),
    ),
    dedupe_key: Some("dedupe-1"),
    attempt_count: Some(2),
    next_attempt_at_ms: Some(1234),
    last_error_code: Some("rate_limited"),
    pending_at_ms: None,
    attempted_at_ms: None,
    failed_at_ms: Some(1200),
    completed_at_ms: None,
    has_payload: True,
  )
}

fn outbox_list_response() -> query_types.QueryResponse {
  query_types.OutboxListResponse(query_types.OutboxListDto(
    items: [outbox_record()],
    page: query_types.PageDto(next_cursor: Some("cursor:1"), has_more: True),
  ))
}

fn outbox_show_response() -> query_types.QueryResponse {
  query_types.OutboxShowResponse(outbox_record())
}

fn task_detail_response_with_terminal_controls() -> query_types.QueryResponse {
  let esc = "\u{1b}"
  let bel = "\u{7}"
  let c1 = "\u{9b}"
  query_types.TaskShowResponse(query_types.TaskDetailDto(
    summary: query_types.TaskSummaryDto(
      id: "linear:issue-770" <> esc <> "[31m",
      source: query_types.TaskSourceDto(
        provider: "linear" <> esc <> "]0;provider" <> bel,
        id: "issue-770" <> esc <> "[31m",
        display_id: Some("LIV-770" <> esc <> "[31m"),
        url: Some("https://linear.example/" <> esc <> "]8;;bad" <> bel),
      ),
      title: "Implement" <> esc <> "[31m task",
      state: task.Ready,
      priority: Some(query_types.TaskPriorityDto(
        value: 2,
        label: "High" <> esc <> "[31m",
      )),
      labels: [
        query_types.TaskLabelDto(
          id: Some("label-workflow"),
          name: "workflow" <> esc <> "[31m",
        ),
      ],
      created_at: Some("2026-04-28T10:00:00Z" <> c1 <> "31m"),
      updated_at: None,
    ),
    description: query_types.TaskDescriptionDto(
      format: "markdown" <> esc <> "[31m",
      body: "body" <> esc <> "]0;owned" <> bel <> "\nline" <> c1 <> "31m",
    ),
  ))
}

fn output(subject: process.Subject(OutMsg)) -> ctl.Output {
  ctl.Output(line: subject_line(subject), inline: subject_inline(subject))
}

fn subject_line(subject: process.Subject(OutMsg)) -> fn(String) -> Nil {
  fn(text) {
    process.send(subject, OutLine(text))
    Nil
  }
}

fn subject_inline(subject: process.Subject(OutMsg)) -> fn(String) -> Nil {
  fn(text) {
    process.send(subject, OutInline(text))
    Nil
  }
}

fn drain_output(subject: process.Subject(OutMsg)) -> String {
  drain_output_messages(test_async.drain_subject(subject), "")
}

fn drain_output_messages(messages: List(OutMsg), acc: String) -> String {
  case messages {
    [] -> acc
    [OutLine(text), ..rest] -> drain_output_messages(rest, acc <> text <> "\n")
    [OutInline(text), ..rest] -> drain_output_messages(rest, acc <> text)
  }
}

fn output_lines(transcript: String) -> List(String) {
  string.trim(transcript) |> string.split(on: "\n")
}

fn table_columns(row: String) -> List(String) {
  row
  |> string.split(on: " ")
  |> list.filter(fn(value) { value != "" })
}

pub fn query_operation_status_wait_polls_until_terminal_test() {
  let path = "test/tmp/ctl-query/wait-control.json"
  let counter_path = "test/tmp/ctl-query/wait-counter.txt"
  write_control_file(path)
  let assert Ok(Nil) = simplifile.write(counter_path, "0")
  let output_subject = process.new_subject()
  let deps =
    ctl.ControlClient(..ps_deps([], ps_now_ms, ""), query: fn(_, query) {
      case query {
        query_types.OperationStatus(_) -> {
          let assert Ok(counter_text) = simplifile.read(counter_path)
          let next = case counter_text {
            "0" -> {
              let assert Ok(Nil) = simplifile.write(counter_path, "1")
              query_types.OperationStatusResponse(
                query_types.OperationStatusDto(
                  operation_id: "op-wait",
                  kind: "retry_step",
                  command: "retry_step",
                  target: "run:run-1",
                  run_id: Some("run-1"),
                  issue_id: None,
                  issue_identifier: None,
                  requested_step_id: None,
                  publication_id: None,
                  status: "running",
                  reason: None,
                  message: Some("still running"),
                  queued_at_ms: 1000,
                  started_at_ms: Some(1001),
                  finished_at_ms: None,
                ),
              )
            }
            _ -> query_operation_status_response()
          }
          Ok(next)
        }
        _ -> Ok(query_status_response())
      }
    })

  let assert Ok(Nil) =
    ctl.run_control_args_with_deps_and_env(
      [
        "query",
        "operation-status",
        "op-wait",
        "--wait",
        "--control-file",
        path,
      ],
      deps,
      output(output_subject),
      fn(_) { Nil },
      path.env,
    )

  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "operation_id: op-wait")
  assert string.contains(transcript, "status: completed")
}

pub fn query_status_human_executes_query_and_formats_status_test() {
  let path = "test/tmp/ctl-query/human-control.json"
  write_control_file(path)
  let output_subject = process.new_subject()
  let query_calls = process.new_subject()
  let deps =
    ctl.ControlClient(
      ..ps_deps([], ps_now_ms, ""),
      query: fn(control_file, query) {
        process.send(query_calls, #(control_file, query))
        Ok(query_status_response())
      },
    )

  let result =
    ctl.run_with_deps(
      ctl.Query(Some(path), False, query_types.Status),
      deps,
      output(output_subject),
    )

  assert result == Ok(Nil)
  let assert Ok(#(called_control_file, called_query)) =
    process.receive(query_calls, within: 1000)
  assert called_control_file.token == "token"
  assert called_query == query_types.Status
  assert output_lines(drain_output(output_subject))
    == [
      "daemon_id: daemon-query",
      "boot_id: boot-query",
      "dispatch_paused: true",
      "ui_server_enabled: false",
      "supported_queries: status",
    ]
}

pub fn query_metrics_human_executes_query_and_formats_metrics_test() {
  let path = "test/tmp/ctl-query/human-metrics-control.json"
  write_control_file(path)
  let output_subject = process.new_subject()
  let query_calls = process.new_subject()
  let deps =
    ctl.ControlClient(
      ..ps_deps([], ps_now_ms, ""),
      query: fn(control_file, query) {
        process.send(query_calls, #(control_file, query))
        Ok(query_metrics_response())
      },
    )

  let result =
    ctl.run_with_deps(
      ctl.Query(Some(path), False, query_types.Metrics),
      deps,
      output(output_subject),
    )

  assert result == Ok(Nil)
  let assert Ok(#(called_control_file, called_query)) =
    process.receive(query_calls, within: 1000)
  assert called_control_file.token == "token"
  assert called_query == query_types.Metrics
  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "daemon_id: daemon-query")
  assert string.contains(transcript, "active_sessions: 3")
  assert string.contains(transcript, "running_workers: 2")
  assert string.contains(transcript, "pending_review_lane_preflights: 6")
  assert string.contains(transcript, "scheduled_next_due_count: 19")
  assert string.contains(transcript, "lifecycle_projection_failed: true")
  assert string.contains(transcript, "token_total: 42")
}

pub fn query_operation_status_human_executes_query_and_formats_operation_test() {
  let path = "test/tmp/ctl-query/human-operation-status-control.json"
  write_control_file(path)
  let output_subject = process.new_subject()
  let query_calls = process.new_subject()
  let deps =
    ctl.ControlClient(
      ..ps_deps([], ps_now_ms, ""),
      query: fn(control_file, query) {
        process.send(query_calls, #(control_file, query))
        Ok(query_operation_status_response())
      },
    )

  let result =
    ctl.run_with_deps(
      ctl.Query(
        Some(path),
        False,
        query_types.OperationStatus(query_types.OperationStatusQuery(
          operation_id: "op-123",
        )),
      ),
      deps,
      output(output_subject),
    )

  assert result == Ok(Nil)
  let assert Ok(#(called_control_file, called_query)) =
    process.receive(query_calls, within: 1000)
  assert called_control_file.token == "token"
  assert called_query
    == query_types.OperationStatus(query_types.OperationStatusQuery(
      operation_id: "op-123",
    ))
  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "operation_id: op-123")
  assert string.contains(transcript, "status: completed")
  assert string.contains(transcript, "requested_step_id: apply_feedback")
}

pub fn query_status_json_uses_raw_request_with_query_payload_test() {
  let path = "test/tmp/ctl-query/json-control.json"
  write_control_file(path)
  let output_subject = process.new_subject()
  let raw_calls = process.new_subject()
  let raw_response =
    protocol.success_response(
      "1",
      protocol.query_data(Ok(query_status_response())),
    )
    |> protocol.response_to_string
  let deps =
    ctl.ControlClient(
      ..ps_deps([], ps_now_ms, raw_response),
      raw_request: fn(control_file, request) {
        process.send(raw_calls, #(control_file, request))
        Ok(raw_response)
      },
    )

  let result =
    ctl.run_with_deps(
      ctl.Query(Some(path), True, query_types.Status),
      deps,
      output(output_subject),
    )

  assert result == Ok(Nil)
  let assert Ok(#(called_control_file, called_request)) =
    process.receive(raw_calls, within: 1000)
  assert called_control_file.token == "token"
  assert called_request == protocol.Query("1", "", query_types.Status)
  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "\"target\"")
  assert string.contains(transcript, "\"type\":\"status\"")
  assert string.contains(transcript, "\"daemon_id\":\"daemon-query\"")
  assert !string.contains(transcript, "token")
}

pub fn query_operation_status_json_uses_raw_request_with_query_payload_test() {
  let path = "test/tmp/ctl-query/json-operation-status-control.json"
  write_control_file(path)
  let output_subject = process.new_subject()
  let raw_calls = process.new_subject()
  let raw_response =
    protocol.success_response(
      "1",
      protocol.query_data(Ok(query_operation_status_response())),
    )
    |> protocol.response_to_string
  let deps =
    ctl.ControlClient(
      ..ps_deps([], ps_now_ms, raw_response),
      raw_request: fn(control_file, request) {
        process.send(raw_calls, #(control_file, request))
        Ok(raw_response)
      },
    )

  let query =
    query_types.OperationStatus(query_types.OperationStatusQuery(
      operation_id: "op-123",
    ))
  let result =
    ctl.run_with_deps(
      ctl.Query(Some(path), True, query),
      deps,
      output(output_subject),
    )

  assert result == Ok(Nil)
  let assert Ok(#(called_control_file, called_request)) =
    process.receive(raw_calls, within: 1000)
  assert called_control_file.token == "token"
  assert called_request == protocol.Query("1", "", query)
  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "\"type\":\"operation_status\"")
  assert string.contains(transcript, "\"operation_id\":\"op-123\"")
  assert string.contains(transcript, "\"target\"")
}

pub fn query_timeout_json_includes_timeout_policy_fields_test() {
  let path = "test/tmp/ctl-query/json-timeout-control.json"
  write_control_file(path)
  let output_subject = process.new_subject()
  let raw_response =
    protocol.success_response(
      "1",
      protocol.query_data(
        Error(query_types.QueryError(
          query_types.QueryTimeout,
          "daemon actor query timed out while loading operation status",
        )),
      ),
    )
    |> protocol.response_to_string
  let deps =
    ctl.ControlClient(
      ..ps_deps([], ps_now_ms, raw_response),
      raw_request: fn(_, _) { Ok(raw_response) },
    )

  let assert Ok(Nil) =
    ctl.run_control_args_with_deps_and_env(
      ["query", "status", "--json", "--control-file", path],
      deps,
      output(output_subject),
      fn(_) { Nil },
      path.env,
    )

  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "\"code\":\"timeout\"")
  assert string.contains(transcript, "\"phase\":\"daemon_actor_query\"")
  assert string.contains(transcript, "\"timeout_ms\":5000")
  assert string.contains(transcript, "\"accepted\":false")
  assert string.contains(transcript, "\"retryable\":true")
}

pub fn query_timeout_json_returns_policy_error_when_no_safe_stale_cache_exists_test() {
  let path = "test/tmp/ctl-query/json-timeout-no-stale-control.json"
  write_control_file(path)
  let output_subject = process.new_subject()
  let raw_response =
    protocol.success_response(
      "1",
      protocol.query_data(
        Error(query_types.QueryError(
          query_types.QueryTimeout,
          "daemon actor query timed out and no safe stale data exists",
        )),
      ),
    )
    |> protocol.response_to_string
  let deps =
    ctl.ControlClient(
      ..ps_deps([], ps_now_ms, raw_response),
      raw_request: fn(_, _) { Ok(raw_response) },
    )

  let assert Ok(Nil) =
    ctl.run_control_args_with_deps_and_env(
      ["query", "metrics", "--json", "--control-file", path],
      deps,
      output(output_subject),
      fn(_) { Nil },
      path.env,
    )

  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "\"ok\":false")
  assert string.contains(transcript, "\"phase\":\"daemon_actor_query\"")
  assert !string.contains(transcript, "\"fresh\"")
  assert !string.contains(transcript, "\"stale_reason\"")
}

pub fn query_json_missing_control_file_uses_control_file_discovery_timeout_shape_test() {
  let output_subject = process.new_subject()

  let assert Ok(Nil) =
    ctl.run_control_args_with_deps_and_env(
      [
        "query",
        "status",
        "--json",
        "--control-file",
        "test/tmp/ctl-query/missing-control.json",
      ],
      ps_deps([], ps_now_ms, ""),
      output(output_subject),
      fn(_) { Nil },
      path.env,
    )

  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "\"code\":\"timeout\"")
  assert string.contains(transcript, "\"phase\":\"control_file_discovery\"")
  assert string.contains(transcript, "\"accepted\":false")
  assert string.contains(transcript, "\"retryable\":true")
}

pub fn non_json_query_timeout_returns_structured_human_timeout_lines_test() {
  let path = "test/tmp/ctl-query/human-timeout-control.json"
  write_control_file(path)
  let deps =
    ctl.ControlClient(..ps_deps([], ps_now_ms, ""), query: fn(_, _) {
      Error(control_client.RequestFailed(
        "query_timeout",
        "daemon actor query timed out while loading operation status",
      ))
    })

  let assert Error(ctl.Failed("timeout", message)) =
    ctl.run_with_deps(
      ctl.Query(Some(path), False, query_types.Status),
      deps,
      output(process.new_subject()),
    )

  assert string.contains(message, "Phase: daemon_actor_query")
  assert string.contains(message, "Accepted: false")
  assert string.contains(message, "Retryable: yes")
}

pub fn query_operation_status_json_wait_timeout_returns_ok_true_wait_metadata_test() {
  let path = "test/tmp/ctl-query/json-wait-timeout-control.json"
  write_control_file(path)
  let output_subject = process.new_subject()
  let raw_response =
    protocol.success_response(
      "1",
      protocol.query_data(
        Ok(
          query_types.OperationStatusResponse(query_types.OperationStatusDto(
            operation_id: "op-wait",
            kind: "retry_step",
            command: "retry_step",
            target: "run:run-1",
            run_id: Some("run-1"),
            issue_id: None,
            issue_identifier: None,
            requested_step_id: None,
            publication_id: None,
            status: "running",
            reason: None,
            message: Some("still running"),
            queued_at_ms: 1000,
            started_at_ms: Some(1001),
            finished_at_ms: None,
          )),
        ),
      ),
    )
    |> protocol.response_to_string
  let deps =
    ctl.ControlClient(
      ..ps_deps([], ps_now_ms, raw_response),
      query: fn(_, _) {
        Ok(
          query_types.OperationStatusResponse(query_types.OperationStatusDto(
            operation_id: "op-wait",
            kind: "retry_step",
            command: "retry_step",
            target: "run:run-1",
            run_id: Some("run-1"),
            issue_id: None,
            issue_identifier: None,
            requested_step_id: None,
            publication_id: None,
            status: "running",
            reason: None,
            message: Some("still running"),
            queued_at_ms: 1000,
            started_at_ms: Some(1001),
            finished_at_ms: None,
          )),
        )
      },
      raw_request: fn(_, _) { Ok(raw_response) },
    )

  let assert Ok(Nil) =
    ctl.run_control_args_with_deps_and_env(
      [
        "query",
        "operation-status",
        "op-wait",
        "--json",
        "--wait",
        "--timeout",
        "500ms",
        "--control-file",
        path,
      ],
      deps,
      output(output_subject),
      fn(_) { Nil },
      path.env,
    )

  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "\"ok\":true")
  assert string.contains(transcript, "\"phase\":\"operation_wait\"")
  assert string.contains(transcript, "\"accepted\":true")
  assert string.contains(transcript, "\"operation_id\":\"op-wait\"")
}

pub fn operator_json_post_send_timeout_reports_operation_admission_unknown_acceptance_test() {
  let path = "test/tmp/ctl-operator/json-timeout-control.json"
  write_control_file(path)
  let output_subject = process.new_subject()
  let raw_calls = process.new_subject()
  let deps =
    ctl.ControlClient(..ps_deps([], ps_now_ms, ""), raw_request: fn(_, request) {
      process.send(raw_calls, request)
      Error(
        control_client.ConnectionFailed(control_client.ReceiveFailed(
          "response timed out with token=SECRET and prompt=SECRET",
        )),
      )
    })

  let assert Ok(Nil) =
    ctl.run_control_args_with_deps_and_env(
      ["pause", "--json", "--timeout", "500ms", "--control-file", path],
      deps,
      output(output_subject),
      fn(_) { Nil },
      path.env,
    )

  let assert protocol.Pause(_, _) = test_async.expect_message(raw_calls)
  test_async.assert_no_extra_message(raw_calls)
  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "\"code\":\"timeout\"")
  assert string.contains(transcript, "\"phase\":\"operation_admission\"")
  assert string.contains(transcript, "\"accepted\":\"unknown\"")
  assert string.contains(transcript, "\"retryable\":false")
  assert !string.contains(transcript, "SECRET")
}

pub fn operator_json_connect_refused_reports_daemon_connect_phase_test() {
  let path = "test/tmp/ctl-operator/json-connect-refused-control.json"
  write_control_file(path)
  let output_subject = process.new_subject()
  let deps =
    ctl.ControlClient(..ps_deps([], ps_now_ms, ""), raw_request: fn(_, _) {
      Error(
        control_client.ConnectionFailed(control_client.ConnectFailed(
          "econnrefused token=SECRET",
        )),
      )
    })

  let assert Ok(Nil) =
    ctl.run_control_args_with_deps_and_env(
      ["ps", "--json", "--timeout", "500ms", "--control-file", path],
      deps,
      output(output_subject),
      fn(_) { Nil },
      path.env,
    )

  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "\"code\":\"timeout\"")
  assert string.contains(transcript, "\"phase\":\"daemon_connect\"")
  assert string.contains(transcript, "\"accepted\":false")
  assert string.contains(transcript, "\"retryable\":true")
  assert !string.contains(transcript, "SECRET")
}

pub fn operator_json_bad_response_returns_single_json_error_test() {
  let path = "test/tmp/ctl-operator/json-bad-response-control.json"
  write_control_file(path)
  let output_subject = process.new_subject()
  let deps =
    ctl.ControlClient(..ps_deps([], ps_now_ms, ""), raw_request: fn(_, _) {
      Ok("not-json token=SECRET")
    })

  let assert Ok(Nil) =
    ctl.run_control_args_with_deps_and_env(
      ["ps", "--json", "--timeout", "500ms", "--control-file", path],
      deps,
      output(output_subject),
      fn(_) { Nil },
      path.env,
    )

  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "\"ok\":false")
  assert string.contains(transcript, "\"code\":\"bad_response\"")
  assert string.contains(transcript, "\"phase\":\"request_round_trip\"")
  assert string.contains(transcript, "\"accepted\":\"unknown\"")
  assert !string.contains(transcript, "not-json")
  assert !string.contains(transcript, "SECRET")
}

pub fn operator_json_queued_admission_includes_accepted_and_operation_id_test() {
  let path = "test/tmp/ctl-operator/json-queued-admission-control.json"
  write_control_file(path)
  let output_subject = process.new_subject()
  let raw_response =
    protocol.success_response(
      "1",
      protocol.command_result_data(command.queued_operation(
        command.RetryIssue(command.IssueIdentifier("LIV-1342")),
        "op-retry-1342",
        Some("retry queued"),
      )),
    )
    |> protocol.response_to_string
  let deps =
    ctl.ControlClient(
      ..ps_deps([], ps_now_ms, raw_response),
      raw_request: fn(_, _) { Ok(raw_response) },
    )

  let assert Ok(Nil) =
    ctl.run_control_args_with_deps_and_env(
      [
        "task",
        "retry",
        "LIV-1342",
        "--json",
        "--timeout",
        "500ms",
        "--control-file",
        path,
      ],
      deps,
      output(output_subject),
      fn(_) { Nil },
      path.env,
    )

  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "\"ok\":true")
  assert string.contains(transcript, "\"accepted\":true")
  assert string.contains(transcript, "\"operation_id\":\"op-retry-1342\"")
}

pub fn mutating_admission_commands_accept_timeout_and_wait_options_test() {
  let assert Ok(_) =
    ctl.parse(["task", "retry", "LIV-1342", "--timeout", "500ms", "--wait"])
  let assert Ok(_) =
    ctl.parse([
      "publication",
      "retry",
      "run-1",
      "--publication",
      "pub-1",
      "--timeout",
      "500ms",
      "--wait",
    ])
  let assert Ok(_) =
    ctl.parse([
      "schedules",
      "run",
      "workspace-cleanup",
      "--now",
      "--timeout",
      "500ms",
      "--wait",
    ])
  Nil
}

pub fn task_list_human_executes_daemon_query_and_formats_page_test() {
  let path = "test/tmp/ctl-task/list-control.json"
  write_control_file(path)
  let output_subject = process.new_subject()
  let query_calls = process.new_subject()
  let deps =
    ctl.ControlClient(
      ..ps_deps([], ps_now_ms, ""),
      query: fn(control_file, query) {
        process.send(query_calls, #(control_file, query))
        Ok(task_list_response())
      },
    )

  let result =
    ctl.run_with_deps(
      ctl.TaskList(Some(path), False, [task.Ready], 1, Some("cursor:0")),
      deps,
      output(output_subject),
    )

  assert result == Ok(Nil)
  let assert Ok(#(called_control_file, called_query)) =
    process.receive(query_calls, within: 1000)
  assert called_control_file.token == "token"
  assert called_query
    == query_types.TaskList(query_types.TaskListQuery(
      states: [task.Ready],
      limit: 1,
      cursor: Some("cursor:0"),
    ))
  assert output_lines(drain_output(output_subject))
    == ["LIV-770 ready [High] Implement task queries", "next_cursor: cursor:1"]
}

pub fn task_show_json_prints_detail_without_raw_state_name_test() {
  let path = "test/tmp/ctl-task/show-control.json"
  write_control_file(path)
  let output_subject = process.new_subject()
  let query_calls = process.new_subject()
  let deps =
    ctl.ControlClient(..ps_deps([], ps_now_ms, ""), query: fn(_, query) {
      process.send(query_calls, query)
      Ok(task_detail_response())
    })

  let result =
    ctl.run_with_deps(
      ctl.TaskShow(Some(path), True, query_types.TaskDisplayId("LIV-770")),
      deps,
      output(output_subject),
    )

  assert result == Ok(Nil)
  let assert Ok(called_query) = process.receive(query_calls, within: 1000)
  assert called_query
    == query_types.TaskShow(
      query_types.TaskShowQuery(ref: query_types.TaskDisplayId("LIV-770")),
    )
  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "\"id\":\"linear:issue-770\"")
  assert string.contains(transcript, "\"state\":\"ready\"")
  assert string.contains(transcript, "\"description\"")
  assert !string.contains(transcript, "Todo")
}

pub fn task_show_human_sanitizes_untrusted_task_text_test() {
  let path = "test/tmp/ctl-task/sanitize-control.json"
  write_control_file(path)
  let output_subject = process.new_subject()
  let deps =
    ctl.ControlClient(..ps_deps([], ps_now_ms, ""), query: fn(_, _) {
      Ok(task_detail_response_with_terminal_controls())
    })

  let result =
    ctl.run_with_deps(
      ctl.TaskShow(Some(path), False, query_types.TaskDisplayId("LIV-770")),
      deps,
      output(output_subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(output_subject)
  assert !string.contains(transcript, "\u{1b}")
  assert !string.contains(transcript, "\u{7}")
  assert !string.contains(transcript, "\u{9b}")
  assert string.contains(
    transcript,
    "LIV-770␛[31m ready [High␛[31m] Implement␛[31m task",
  )
  assert string.contains(transcript, "labels: workflow␛[31m")
  assert string.contains(transcript, "description (markdown␛[31m):")
  assert string.contains(transcript, "body␛]0;owned␇")
  assert string.contains(transcript, "line\\u{9B}31m")
}

pub fn task_show_not_found_returns_failed_error_test() {
  let path = "test/tmp/ctl-task/not-found-control.json"
  write_control_file(path)
  let output_subject = process.new_subject()
  let deps =
    ctl.ControlClient(..ps_deps([], ps_now_ms, ""), query: fn(_, _) {
      Error(control_client.RequestFailed("not_found", "task not found"))
    })

  let result =
    ctl.run_with_deps(
      ctl.TaskShow(Some(path), False, query_types.TaskDisplayId("LIV-999")),
      deps,
      output(output_subject),
    )

  assert result == Error(ctl.Failed("not_found", "task not found"))
  assert drain_output(output_subject) == ""
}

pub fn outbox_list_human_executes_daemon_query_and_redacts_payload_test() {
  let path = "test/tmp/ctl-outbox/list-control.json"
  write_control_file(path)
  let output_subject = process.new_subject()
  let query_calls = process.new_subject()
  let deps =
    ctl.ControlClient(
      ..ps_deps([], ps_now_ms, ""),
      query: fn(control_file, query) {
        process.send(query_calls, #(control_file, query))
        Ok(outbox_list_response())
      },
    )

  let result =
    ctl.run_with_deps(
      ctl.Outbox(
        Some(path),
        False,
        None,
        [query_types.OutboxRetryableStatus],
        ["linear_comment"],
        1,
        Some("cursor:0"),
      ),
      deps,
      output(output_subject),
    )

  assert result == Ok(Nil)
  let assert Ok(#(called_control_file, called_query)) =
    process.receive(query_calls, within: 1000)
  assert called_control_file.token == "token"
  assert called_query
    == query_types.OutboxList(query_types.OutboxListQuery(
      statuses: [query_types.OutboxRetryableStatus],
      kinds: ["linear_comment"],
      limit: 1,
      cursor: Some("cursor:0"),
    ))
  let transcript = drain_output(output_subject)
  assert string.contains(
    transcript,
    "outbox-1 retryable linear_comment LIV-1087 attempts=2 next_attempt_at_ms=1234 error=rate_limited",
  )
  assert string.contains(transcript, "next_cursor: cursor:1")
  assert !string.contains(transcript, "payload_json")
  assert !string.contains(transcript, "raw-secret")
}

pub fn outbox_show_json_prints_safe_record_test() {
  let path = "test/tmp/ctl-outbox/show-control.json"
  write_control_file(path)
  let output_subject = process.new_subject()
  let query_calls = process.new_subject()
  let deps =
    ctl.ControlClient(..ps_deps([], ps_now_ms, ""), query: fn(_, query) {
      process.send(query_calls, query)
      Ok(outbox_show_response())
    })

  let result =
    ctl.run_with_deps(
      ctl.Outbox(Some(path), True, Some("outbox-1"), [], [], 50, None),
      deps,
      output(output_subject),
    )

  assert result == Ok(Nil)
  let assert Ok(called_query) = process.receive(query_calls, within: 1000)
  assert called_query
    == query_types.OutboxShow(query_types.OutboxShowQuery(outbox_id: "outbox-1"))
  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "\"outbox_id\":\"outbox-1\"")
  assert string.contains(transcript, "\"status\":\"retryable\"")
  assert string.contains(transcript, "\"has_payload\":true")
  assert !string.contains(transcript, "payload_json")
  assert !string.contains(transcript, "raw-secret")
}

pub fn parse_ping_ps_session_events_and_attach_test() {
  assert ctl.parse(["ping"]) == Ok(ctl.Ping(None, False))
  assert ctl.parse(["ps", "--json"]) == Ok(ctl.Ps(None, True))
  assert ctl.parse(["query", "status", "--json"])
    == Ok(ctl.Query(None, True, query_types.Status))
  assert ctl.parse(["query", "metrics"])
    == Ok(ctl.Query(None, False, query_types.Metrics))
  assert ctl.parse(["query", "metrics", "--json"])
    == Ok(ctl.Query(None, True, query_types.Metrics))
  assert ctl.parse(["query", "operation-status", "op-123"])
    == Ok(ctl.Query(
      None,
      False,
      query_types.OperationStatus(query_types.OperationStatusQuery(
        operation_id: "op-123",
      )),
    ))
  assert ctl.parse(["query", "operation-status", "op-123", "--json"])
    == Ok(ctl.Query(
      None,
      True,
      query_types.OperationStatus(query_types.OperationStatusQuery(
        operation_id: "op-123",
      )),
    ))
  assert ctl.parse([
      "task",
      "list",
      "--state",
      "ready",
      "--state",
      "active",
      "--limit",
      "50",
      "--cursor",
      "cursor:50",
      "--json",
    ])
    == Ok(ctl.TaskList(
      None,
      True,
      [task.Ready, task.Active],
      50,
      Some("cursor:50"),
    ))
  assert ctl.parse(["task", "show", "LIV-770"])
    == Ok(ctl.TaskShow(None, False, query_types.TaskDisplayId("LIV-770")))
  assert ctl.parse(["task", "show", "id:issue-770", "--json"])
    == Ok(ctl.TaskShow(
      None,
      True,
      query_types.TaskRemoteId(provider: None, id: "issue-770"),
    ))
  assert ctl.parse([
      "outbox",
      "--status",
      "retryable",
      "--kind",
      "linear_comment",
      "--limit",
      "25",
      "--cursor",
      "cursor:25",
      "--json",
    ])
    == Ok(ctl.Outbox(
      None,
      True,
      None,
      [query_types.OutboxRetryableStatus],
      ["linear_comment"],
      25,
      Some("cursor:25"),
    ))
  assert ctl.parse(["outbox", "outbox-1", "--json"])
    == Ok(ctl.Outbox(None, True, Some("outbox-1"), [], [], 50, None))
  assert ctl.parse(["session", "ABC-1", "--control-file", "state/control.json"])
    == Ok(ctl.Session(Some("state/control.json"), False, "ABC-1"))
  assert ctl.parse(["events", "ABC-1"])
    == Ok(ctl.Events(None, ctl.Raw, style.ColorNever, 0, False, "ABC-1"))
  assert ctl.parse(["events", "ABC-1", "--json"])
    == Ok(ctl.Events(None, ctl.Json, style.ColorNever, 0, False, "ABC-1"))
  assert ctl.parse(["events", "--pretty", "ABC-1"])
    == Ok(ctl.Events(None, ctl.Pretty, style.ColorAuto, 0, False, "ABC-1"))
  assert ctl.parse(["events", "--pretty", "--verbose", "ABC-1"])
    == Ok(ctl.Events(None, ctl.Pretty, style.ColorAuto, 0, True, "ABC-1"))
  assert ctl.parse(["attach", "ABC-1"])
    == Ok(ctl.Attach(
      None,
      ctl.Pretty,
      style.ColorAuto,
      ctl.Follow,
      0,
      False,
      "ABC-1",
    ))
  assert ctl.parse(["attach", "--verbose", "ABC-1"])
    == Ok(ctl.Attach(
      None,
      ctl.Pretty,
      style.ColorAuto,
      ctl.Follow,
      0,
      True,
      "ABC-1",
    ))
  assert ctl.parse(["attach", "--raw", "ABC-1"])
    == Ok(ctl.Attach(
      None,
      ctl.Raw,
      style.ColorNever,
      ctl.Follow,
      0,
      False,
      "ABC-1",
    ))
  assert ctl.parse(["attach", "--json", "--verbose", "ABC-1"])
    == Ok(ctl.Attach(
      None,
      ctl.Json,
      style.ColorNever,
      ctl.Follow,
      0,
      True,
      "ABC-1",
    ))
  assert ctl.parse(["attach", "--no-follow", "ABC-1"])
    == Ok(ctl.Attach(
      None,
      ctl.Pretty,
      style.ColorAuto,
      ctl.NoFollow,
      0,
      False,
      "ABC-1",
    ))
  assert ctl.parse(["attach", "--since-cursor", "40", "ABC-1"])
    == Ok(ctl.Attach(
      None,
      ctl.Pretty,
      style.ColorAuto,
      ctl.Follow,
      40,
      False,
      "ABC-1",
    ))
  assert ctl.parse(["attach", "--color=never", "ABC-1"])
    == Ok(ctl.Attach(
      None,
      ctl.Pretty,
      style.ColorNever,
      ctl.Follow,
      0,
      False,
      "ABC-1",
    ))
  assert ctl.parse(["cleanup"])
    == Ok(ctl.Cleanup(
      None,
      None,
      False,
      True,
      False,
      None,
      None,
      None,
      cleanup.AllProviders,
    ))
  assert ctl.parse([
      "cleanup",
      "--dry-run",
      "--json",
      "--root",
      "work",
      "--limit",
      "25",
      "--cursor",
      "cursor-1",
      "--max-runtime-ms",
      "240000",
    ])
    == Ok(ctl.Cleanup(
      None,
      Some("work"),
      True,
      True,
      False,
      Some(25),
      Some("cursor-1"),
      Some(240_000),
      cleanup.AllProviders,
    ))
  assert ctl.parse(["cleanup", "--dry-run", "--json", "--root", "work"])
    == Ok(ctl.Cleanup(
      None,
      Some("work"),
      True,
      True,
      False,
      None,
      None,
      None,
      cleanup.AllProviders,
    ))
  assert ctl.parse(["cleanup", "--yes", "--root", "work"])
    == Ok(ctl.Cleanup(
      None,
      Some("work"),
      False,
      False,
      True,
      None,
      None,
      None,
      cleanup.AllProviders,
    ))
  assert ctl.parse(["cleanup", "--provider", "local-state"])
    == Ok(ctl.Cleanup(
      None,
      None,
      False,
      True,
      False,
      None,
      None,
      None,
      cleanup.SelectedProvider(cleanup.LocalState),
    ))
  assert ctl.parse(["cleanup", "--provider", "all"])
    == Ok(ctl.Cleanup(
      None,
      None,
      False,
      True,
      False,
      None,
      None,
      None,
      cleanup.AllProviders,
    ))
  assert ctl.parse(["schedules", "status", "--root", "work", "--json"])
    == Ok(ctl.SchedulesStatus(None, Some("work"), True, None))
  assert ctl.parse(["schedules", "status", "nightly", "--root", "work"])
    == Ok(ctl.SchedulesStatus(None, Some("work"), False, Some("nightly")))
  assert ctl.parse(["schedules", "history", "nightly", "--root", "work"])
    == Ok(ctl.SchedulesHistory(None, Some("work"), False, "nightly"))
  assert ctl.parse(["schedules", "logs", "nightly", "--last", "--root", "work"])
    == Ok(ctl.SchedulesLogs(
      None,
      Some("work"),
      False,
      style.ColorAuto,
      False,
      "nightly",
    ))
  assert ctl.parse(["schedules", "doctor", "nightly", "--root", "work"])
    == Ok(ctl.SchedulesDoctor(None, Some("work"), False, "nightly"))
  assert ctl.parse(["run-schedule", "nightly", "--now"])
    == Ok(ctl.Operator(None, False, command.RunScheduleNow("nightly")))
  let assert Ok(ctl.Workstream(_)) = ctl.parse(["workstream", "list", "LIV-1"])
  assert ctl.parse(["workstream", "show", "workstream-1", "--json"])
    == Ok(
      ctl.Workstream(ctl_workstream.Show(
        control_path: None,
        root: None,
        json_output: True,
        workstream_ref: "workstream-1",
      )),
    )
  assert ctl.parse(["artifact", "publication", "list", "--run", "run-1"])
    == Ok(ctl.ArtifactPublicationList(None, None, False, "run-1"))
  assert ctl.parse([
      "artifact",
      "publication",
      "show",
      "--run",
      "run-1",
      "--publication",
      "review_doc",
      "--json",
      "--root",
      "work",
    ])
    == Ok(ctl.ArtifactPublicationShow(
      None,
      Some("work"),
      True,
      "run-1",
      "review_doc",
    ))
  assert ctl.parse([
      "artifact",
      "publication",
      "retry",
      "--run",
      "run-1",
      "--publication",
      "review_doc",
    ])
    == Ok(ctl.ArtifactPublicationRetry(
      None,
      None,
      False,
      "run-1",
      Some("review_doc"),
    ))
  assert ctl.parse(["artifact", "publication", "retry", "--run", "run-1"])
    == Ok(ctl.ArtifactPublicationRetry(None, None, False, "run-1", None))
  assert ctl.parse([
      "artifact",
      "publication",
      "abandon",
      "--run",
      "run-1",
      "--publication",
      "review_doc",
      "--reason",
      "operator chose not to publish",
      "--yes",
    ])
    == Ok(ctl.ArtifactPublicationAbandon(
      None,
      None,
      False,
      "run-1",
      "review_doc",
      "operator chose not to publish",
    ))
  assert ctl.parse(["state", "status", "--root", "work", "--json"])
    == Ok(ctl.StateStatus("work", True))
  assert ctl.parse(["state", "archive-old", "--root", "work", "--yes"])
    == Ok(ctl.StateArchiveOld("work", False, True))
  assert ctl.parse(["state", "compact", "--root", "work", "--dry-run"])
    == Ok(ctl.StateCompact("work", False, True, False))
  assert ctl.parse(["state", "compact", "--root", "work", "--yes", "--json"])
    == Ok(ctl.StateCompact("work", True, False, True))
  assert ctl.parse([
      "state", "compact", "--root", "work", "--rebuild-from-archives", "--yes",
    ])
    == Ok(ctl.StateCompactRebuild("work", False))
  assert ctl.parse([
      "state", "compact", "--root", "work", "--rebuild-from-archives",
      "--dry-run",
    ])
    == Error(ctl.UsageError(
      "state compact --rebuild-from-archives requires --yes",
    ))
  assert ctl.parse(["state", "compact", "--root", "work"])
    == Error(ctl.UsageError("state compact requires --dry-run or --yes"))
  assert ctl.parse([
      "state",
      "compact",
      "--root",
      "work",
      "--dry-run",
      "--yes",
    ])
    == Error(ctl.UsageError(
      "state compact requires exactly one of --dry-run or --yes",
    ))
  assert ctl.parse([
      "state",
      "repair-run-provenance",
      "run:run-1",
      "--root",
      "work",
      "--dry-run",
      "--json",
    ])
    == Ok(ctl.StateRepairRunProvenance("work", True, "run-1", True, False))
}

pub fn parse_offline_accepts_canonical_top_level_commands_and_rejects_daemon_controls_test() {
  assert ctl.parse_offline(["cleanup", "--root", "work"])
    == Ok(ctl.Cleanup(
      None,
      Some("work"),
      False,
      True,
      False,
      None,
      None,
      None,
      cleanup.AllProviders,
    ))
  assert ctl.parse_offline(["schedules", "status", "nightly", "--root", "work"])
    == Ok(ctl.SchedulesStatus(None, Some("work"), False, Some("nightly")))
  let assert Ok(ctl.Workstream(_)) = ctl.parse_offline(["workstream", "list"])
  assert ctl.parse_offline(["state", "status", "--root", "work"])
    == Ok(ctl.StateStatus("work", False))
  let assert Error(ctl.UsageError(run_schedule_error)) =
    ctl.parse_offline(["run-schedule", "nightly", "--now"])
  assert string.contains(run_schedule_error, "unknown")
  let assert Error(ctl.UsageError(schedule_run_error)) =
    ctl.parse_offline(["schedules", "run", "nightly", "--now"])
  assert string.contains(schedule_run_error, "unknown")
}

pub fn parse_operator_commands_test() {
  assert ctl.parse(["pause"])
    == Ok(ctl.Operator(None, False, command.PauseDispatch))
  assert ctl.parse(["resume"])
    == Ok(ctl.Operator(None, False, command.ResumeDispatch))
  assert ctl.parse(["reload"])
    == Ok(ctl.Operator(None, False, command.ReloadWorkflow))
  assert ctl.parse(["task", "retry", "ABC-123"])
    == Ok(ctl.Operator(
      None,
      False,
      command.RetryIssue(command.IssueIdentifier("ABC-123")),
    ))
  assert ctl.parse(["task", "retry", "id:issue-123"])
    == Ok(ctl.Operator(
      None,
      False,
      command.RetryIssue(command.IssueId("issue-123")),
    ))
  assert ctl.parse([
      "task",
      "retry",
      "ABC-123",
      "--start-fresh",
      "--reason",
      "workflow drift",
    ])
    == Ok(ctl.TaskRetryStartFresh(
      None,
      False,
      command.IssueIdentifier("ABC-123"),
      "workflow drift",
    ))
  assert ctl.parse(["retry", "ABC-123"])
    == Ok(ctl.Operator(
      None,
      False,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepAutoTarget("ABC-123"),
        None,
      ),
    ))
  assert ctl.parse(["retry", "run:run-1", "--dry-run", "--json"])
    == Ok(ctl.Operator(
      None,
      True,
      command.RetryWorkflowStepDryRun(
        command.RetryWorkflowStepRunId("run-1"),
        None,
      ),
    ))
  assert ctl.parse(["retry", "all", "ABC-123"])
    == Ok(ctl.TaskRetryStartFresh(
      None,
      False,
      command.IssueIdentifier("ABC-123"),
      "operator_forced_from_scratch",
    ))
  assert ctl.parse([
      "task",
      "retry",
      "ABC-123",
      "--from-scratch",
      "--reason",
      "workflow drift",
    ])
    == Ok(ctl.TaskRetryStartFresh(
      None,
      False,
      command.IssueIdentifier("ABC-123"),
      "workflow drift",
    ))
  assert ctl.parse(["run", "retry-step", "run-1", "--step", "build"])
    == Ok(ctl.Operator(
      None,
      False,
      command.RetryWorkflowStepExact(
        command.RetryWorkflowStepRunId("run-1"),
        Some("build"),
      ),
    ))
  assert ctl.parse(["retry-step", "ABC-123", "--step", "build"])
    == Ok(ctl.Operator(
      None,
      False,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepAutoTarget("ABC-123"),
        Some("build"),
      ),
    ))
  assert ctl.parse(["retry", "step", "ABC-123", "--step", "build"])
    == Ok(ctl.Operator(
      None,
      False,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepAutoTarget("ABC-123"),
        Some("build"),
      ),
    ))
  assert ctl.parse(["retry-step", "run:run-1"])
    == Ok(ctl.Operator(
      None,
      False,
      command.RetryWorkflowStep(command.RetryWorkflowStepRunId("run-1"), None),
    ))
  assert ctl.parse(["run", "recollect-outputs", "run-1"])
    == Ok(ctl.Operator(None, False, command.RecollectWorkflowOutputs("run-1")))
  assert ctl.parse(["recollect-outputs", "run:run-1"])
    == Ok(ctl.Operator(None, False, command.RecollectWorkflowOutputs("run-1")))
  assert ctl.parse(["recollect-outputs", "run:run-1", "--json"])
    == Ok(ctl.Operator(None, True, command.RecollectWorkflowOutputs("run-1")))
  assert ctl.parse([
      "run",
      "finalize",
      "run-1",
      "--validate",
      "--outputs",
      "auto",
      "--publish",
      "--update-tracker",
      "--reason",
      "operator salvage",
      "--dry-run",
    ])
    == Ok(ctl.RunFinalize(None, False, "run-1", "operator salvage", True, False))
  assert ctl.parse([
      "run",
      "finalize",
      "run-1",
      "--validate",
      "--outputs",
      "auto",
      "--publish",
      "--update-tracker",
      "--reason",
      "operator salvage",
      "--yes",
      "--json",
    ])
    == Ok(ctl.RunFinalize(None, True, "run-1", "operator salvage", False, False))
  assert ctl.parse([
      "run",
      "finalize",
      "run-1",
      "--validate",
      "--outputs",
      "auto",
      "--publish",
      "--update-tracker",
      "--reason",
      "operator exception",
      "--allow-unpublished",
      "--yes",
    ])
    == Ok(ctl.RunFinalize(
      None,
      False,
      "run-1",
      "operator exception",
      False,
      True,
    ))
  assert ctl.parse([
      "publication",
      "retry",
      "run-1",
      "--publication",
      "review_doc",
    ])
    == Ok(ctl.Operator(
      None,
      False,
      command.RetryArtifactPublication("run-1", Some("review_doc")),
    ))
  assert ctl.parse(["recovery", "cleanup-orphan-steps", "run:run-1"])
    == Ok(ctl.Operator(None, False, command.CleanupOrphanSteps("run-1", True)))
  assert ctl.parse([
      "recovery",
      "cleanup-orphan-steps",
      "run:run-1",
      "--yes",
    ])
    == Ok(ctl.Operator(None, False, command.CleanupOrphanSteps("run-1", False)))
  assert ctl.parse(["park", "ABC-123", "--reason", "manual", "--yes"])
    == Ok(ctl.Operator(
      None,
      False,
      command.ParkIssue(command.IssueIdentifier("ABC-123"), "manual"),
    ))
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["park", "ABC-123", "--reason", "manual"])
  assert ctl.parse(["unpark", "ABC-123"])
    == Ok(ctl.Operator(
      None,
      False,
      command.UnparkIssue(command.IssueIdentifier("ABC-123")),
    ))
  assert ctl.parse(["abort", "session-1", "--yes"])
    == Ok(ctl.Operator(None, False, command.AbortSession("session-1")))
  let assert Error(ctl.UsageError(_)) = ctl.parse(["abort", "session-1"])
  assert ctl.parse(["stop-after-turn", "session-1", "--yes"])
    == Ok(ctl.Operator(None, False, command.StopAfterCurrentTurn("session-1")))
  assert ctl.parse(["prompt", "session-1", "continue"])
    == Ok(ctl.Operator(
      None,
      False,
      command.PromptSession("session-1", "continue"),
    ))
  assert ctl.parse(["ui", "respond", "session-1", "ui-1", "--cancel"])
    == Ok(ctl.Operator(
      None,
      False,
      command.RespondUi("session-1", "ui-1", command.UiCancel),
    ))
  assert ctl.parse(["ui", "respond", "session-1", "ui-1", "--value", "ok"])
    == Ok(ctl.Operator(
      None,
      False,
      command.RespondUi("session-1", "ui-1", command.UiValue("ok")),
    ))
  assert ctl.parse(["schedules", "run", "nightly", "--now"])
    == Ok(ctl.Operator(None, False, command.RunScheduleNow("nightly")))
  let assert Error(ctl.UsageError(_)) = ctl.parse(["run-schedule", "nightly"])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["schedules", "run", "nightly"])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["schedules", "logs", "nightly"])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["cleanup", "--yes", "--dry-run"])
  let assert Error(ctl.UsageError(_)) = ctl.parse(["cleanup", "--limit", "0"])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["cleanup", "--max-runtime-ms", "0"])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["cleanup", "--cursor", "   "])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse([
      "recovery",
      "cleanup-orphan-steps",
      "run:run-1",
      "--yes",
      "--dry-run",
    ])
}

pub fn deprecated_ctl_offline_alias_prints_hint_and_top_level_offline_does_not_test() {
  let root = "test/tmp/ctl-cleanup/deprecated-alias"
  test_helpers.reset_dir(root)
  let stdout_subject = process.new_subject()
  let stderr_subject = process.new_subject()

  assert ctl.run_control_args_with_deps_and_env(
      ["cleanup", "--root", root],
      ps_deps([], ps_now_ms, ""),
      output(stdout_subject),
      subject_line(stderr_subject),
      path.env,
    )
    == Ok(Nil)
  let hint = drain_output(stderr_subject)
  assert string.contains(hint, "Deprecated: scherzo ctl cleanup")
  assert string.contains(hint, "use scherzo cleanup")

  let offline_stderr = process.new_subject()
  assert ctl.run_offline_args_with_deps_and_env(
      ["cleanup", "--root", root],
      ps_deps([], ps_now_ms, ""),
      output(process.new_subject()),
      subject_line(offline_stderr),
      path.env,
    )
    == Ok(Nil)
  assert drain_output(offline_stderr) == ""
}

pub fn deprecated_ctl_json_alias_keeps_stdout_machine_readable_test() {
  let root = "test/tmp/ctl-cleanup/deprecated-alias-json"
  test_helpers.reset_dir(root)
  let stdout_subject = process.new_subject()
  let stderr_subject = process.new_subject()

  assert ctl.run_control_args_with_deps_and_env(
      ["cleanup", "--root", root, "--json"],
      ps_deps([], ps_now_ms, ""),
      output(stdout_subject),
      subject_line(stderr_subject),
      path.env,
    )
    == Ok(Nil)

  let stdout = drain_output(stdout_subject)
  let stderr = drain_output(stderr_subject)
  assert output_lines(stdout) |> list.length == 1
  assert string.starts_with(stdout, "{")
  assert string.ends_with(string.trim(stdout), "}")
  assert !string.contains(stdout, "Deprecated: scherzo ctl cleanup")
  assert string.contains(stderr, "Deprecated: scherzo ctl cleanup")
  assert string.contains(stderr, "use scherzo cleanup")
}

pub fn deprecated_recovery_aliases_print_resource_first_hints_test() {
  assert command_registry.deprecated_alias_hint(["retry", "ABC-123"]) == None
  assert command_registry.deprecated_alias_hint(["retry-step", "run:run-1"])
    == Some(
      "Deprecated: scherzo ctl retry-step run:run-1 will be removed after one release; use scherzo ctl retry <target> [--step <step-id>] for the common retry path, or scherzo ctl run retry-step <run-id> --step <step-id> for the exact expert override.",
    )
  assert command_registry.deprecated_alias_hint([
      "recollect-outputs",
      "run:run-1",
    ])
    == Some(
      "Deprecated: scherzo ctl recollect-outputs run:run-1 will be removed after one release; use scherzo ctl run recollect-outputs <run-id>.",
    )
}

pub fn cleanup_json_output_uses_provider_report_test() {
  let root = "test/tmp/ctl-cleanup/json"
  test_helpers.reset_dir(root)
  let output_subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Cleanup(
        None,
        Some(root),
        True,
        False,
        False,
        None,
        None,
        None,
        cleanup.AllProviders,
      ),
      ps_deps([], ps_now_ms, ""),
      output(output_subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "\"mode\":\"dry_run\"")
  assert string.contains(transcript, "\"provider_id\":\"local_state\"")
  assert string.contains(transcript, "\"provider_id\":\"workspaces\"")
  assert !string.contains(transcript, "\"truncated\"")
  assert string.contains(transcript, "\"elapsed_ms\":")
}

pub fn cleanup_json_output_filters_provider_test() {
  let root = "test/tmp/ctl-cleanup/json-filter"
  test_helpers.reset_dir(root)
  let output_subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Cleanup(
        None,
        Some(root),
        True,
        False,
        False,
        None,
        None,
        None,
        cleanup.SelectedProvider(cleanup.Workspaces),
      ),
      ps_deps([], ps_now_ms, ""),
      output(output_subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "\"provider_id\":\"workspaces\"")
  assert !string.contains(transcript, "\"provider_id\":\"local_state\"")
}

pub fn cleanup_apply_output_filters_provider_test() {
  let root = "test/tmp/ctl-cleanup/apply-filter"
  test_helpers.reset_dir(root)
  let output_subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Cleanup(
        None,
        Some(root),
        False,
        False,
        True,
        None,
        None,
        None,
        cleanup.SelectedProvider(cleanup.Workspaces),
      ),
      ps_deps([], ps_now_ms, ""),
      output(output_subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "cleanup apply")
  assert string.contains(transcript, "provider: workspaces")
  assert !string.contains(transcript, "provider: local_state")
}

pub fn cleanup_text_output_uses_provider_report_test() {
  let root = "test/tmp/ctl-cleanup/text"
  test_helpers.reset_dir(root)
  let output_subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Cleanup(
        None,
        Some(root),
        False,
        True,
        False,
        None,
        None,
        None,
        cleanup.AllProviders,
      ),
      ps_deps([], ps_now_ms, ""),
      output(output_subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "cleanup dry_run")
  assert string.contains(transcript, "provider: local_state")
  assert string.contains(transcript, "provider: workspaces")
  assert string.contains(transcript, "available: true")
  assert !string.contains(transcript, "page:")
}

pub fn cleanup_bounded_output_includes_page_metadata_test() {
  let root = "test/tmp/ctl-cleanup/bounded"
  test_helpers.reset_dir(root)
  let output_subject = process.new_subject()
  let cursor =
    cleanup_cursor.encode(
      root,
      cleanup_cursor.Cursor("local_state", "cursor-1"),
    )

  let result =
    ctl.run_with_deps(
      ctl.Cleanup(
        None,
        Some(root),
        False,
        True,
        False,
        Some(25),
        Some(cursor),
        Some(240_000),
        cleanup.AllProviders,
      ),
      ps_deps([], ps_now_ms, ""),
      output(output_subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(output_subject)
  assert string.contains(transcript, "page:")
  assert string.contains(transcript, "limit: 25")
  assert string.contains(transcript, "cursor: " <> cursor)
  assert string.contains(transcript, "max_runtime_ms: 240000")
  assert string.contains(transcript, "elapsed_ms:")
}

pub fn parse_rejects_usage_errors_test() {
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["attach", "--raw", "--pretty", "ABC-1"])
  assert ctl.parse(["attach", "--raw", "ABC-1", "--json"])
    == Error(ctl.UsageError("choose only one of --pretty, --raw, or --json"))
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["workstream", "inspect", "workstream-1"])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["attach", "--since-cursor", "-1", "ABC-1"])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["attach", "--since-cursor", "wat", "ABC-1"])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["attach", "--color=bad", "ABC-1"])
  assert ctl.parse(["recollect-outputs", "run:"])
    == Error(ctl.UsageError("recollect-outputs requires run:<run-id>"))
  assert ctl.parse(["recollect-outputs", "ABC-123"])
    == Error(ctl.UsageError("recollect-outputs requires run:<run-id>"))
  assert ctl.parse(["task", "retry", "ABC-123", "--start-fresh"])
    == Error(ctl.UsageError(
      "task retry --start-fresh/--from-scratch requires --reason <text>",
    ))
  assert ctl.parse(["task", "retry", "ABC-123", "--from-scratch"])
    == Error(ctl.UsageError(
      "task retry --start-fresh/--from-scratch requires --reason <text>",
    ))
  assert ctl.parse(["task", "retry", "ABC-123", "--reason", "manual"])
    == Error(ctl.UsageError(
      "task retry --reason <text> requires --start-fresh or --from-scratch",
    ))
  assert ctl.parse(["run", "retry-step", "run-1"])
    == Error(ctl.UsageError("run retry-step requires --step <step-id>"))
  assert ctl.parse(["run", "finalize", "run-1"])
    == Error(ctl.UsageError("run finalize requires --dry-run or --yes"))
  assert ctl.parse([
      "run",
      "finalize",
      "run-1",
      "--validate",
      "--outputs",
      "auto",
      "--publish",
      "--update-tracker",
      "--reason",
      "manual",
      "--dry-run",
      "--yes",
    ])
    == Error(ctl.UsageError(
      "run finalize requires exactly one of --dry-run or --yes",
    ))
  assert ctl.parse(["run", "finalize", "run-1", "--dry-run"])
    == Error(ctl.UsageError("run finalize requires --validate"))
  assert ctl.parse([
      "run",
      "finalize",
      "run-1",
      "--validate",
      "--publish",
      "--update-tracker",
      "--reason",
      "manual",
      "--dry-run",
    ])
    == Error(ctl.UsageError("run finalize requires --outputs auto"))
  let assert Error(ctl.UsageError(_)) = ctl.parse(["ps", "--control-file"])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["task", "list", "--state", "linear-todo"])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["task", "list", "--limit", "0"])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["task", "list", "--cursor", ""])
  let assert Error(ctl.UsageError(_)) = ctl.parse(["task", "show", "id:"])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["outbox", "--status", "unknown"])
  let assert Error(ctl.UsageError(_)) = ctl.parse(["outbox", "--kind", ""])
  let assert Error(ctl.UsageError(provider_error)) =
    ctl.parse(["cleanup", "--provider", "unknown"])
  assert string.contains(provider_error, "invalid cleanup provider 'unknown'")
  assert string.contains(provider_error, "local-state")
  assert string.contains(provider_error, "diagnostic-only unavailable")
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["artifact", "publication", "list"])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["artifact", "publication", "show", "--run", "run-1"])
  let assert Error(ctl.UsageError(_)) = ctl.parse(["unknown"])
}

pub fn parse_rejects_irrelevant_command_options_test() {
  assert ctl.parse(["ping", "--yes"])
    == Error(ctl.UsageError("unsupported option for ping: --yes"))
  assert ctl.parse(["task", "show", "LIV-1", "--state", "ready"])
    == Error(ctl.UsageError("unsupported option for task show: --state"))
  assert ctl.parse(["outbox", "--state", "ready"])
    == Error(ctl.UsageError("unsupported option for outbox: --state"))
  assert ctl.parse(["outbox", "outbox-1", "--status", "retryable"])
    == Error(ctl.UsageError(
      "unsupported option for outbox <outbox-id>: --status",
    ))
  assert ctl.parse(["outbox", "outbox-1", "--kind", "linear_comment"])
    == Error(ctl.UsageError("unsupported option for outbox <outbox-id>: --kind"))
  assert ctl.parse(["outbox", "outbox-1", "--limit", "1"])
    == Error(ctl.UsageError(
      "unsupported option for outbox <outbox-id>: --limit",
    ))
  assert ctl.parse(["outbox", "outbox-1", "--cursor", "cursor:1"])
    == Error(ctl.UsageError(
      "unsupported option for outbox <outbox-id>: --cursor",
    ))
  assert ctl.parse(["schedules", "logs", "nightly", "--now"])
    == Error(ctl.UsageError("unsupported option for schedules logs: --now"))
  assert ctl.parse(["task", "retry", "ABC-123", "--root", "work"])
    == Error(ctl.UsageError("unsupported option for task retry: --root"))
  assert ctl.parse([
      "run",
      "retry-step",
      "run-1",
      "--step",
      "build",
      "--root",
      "work",
    ])
    == Error(ctl.UsageError("unsupported option for run retry-step: --root"))
  assert ctl.parse(["run", "recollect-outputs", "run-1", "--root", "work"])
    == Error(ctl.UsageError(
      "unsupported option for run recollect-outputs: --root",
    ))
  assert ctl.parse(["run", "finalize", "run-1", "--root", "work"])
    == Error(ctl.UsageError("unsupported option for run finalize: --root"))
  assert ctl.parse([
      "publication",
      "retry",
      "run-1",
      "--publication",
      "review_doc",
      "--root",
      "work",
    ])
    == Error(ctl.UsageError("unsupported option for publication retry: --root"))
  assert ctl.parse([
      "artifact",
      "publication",
      "list",
      "--run",
      "run-1",
      "--publication",
      "extra",
    ])
    == Error(ctl.UsageError(
      "unsupported option for artifact publication list: --publication",
    ))
}

pub fn parse_rejects_duplicate_singleton_options_test() {
  assert ctl.parse([
      "artifact",
      "publication",
      "list",
      "--run",
      "run-1",
      "--run",
      "run-2",
    ])
    == Error(ctl.UsageError("option may only be supplied once: --run"))
}

pub fn usage_mentions_daemon_only_commands_and_options_test() {
  let usage = ctl.usage()
  assert string.contains(usage, "ping")
  assert string.contains(usage, "ps")
  assert string.contains(usage, "LAST EVENT is daemon-relative age")
  assert string.contains(usage, "session <session-ref>")
  assert string.contains(usage, "events <session-ref>")
  assert string.contains(usage, "events --pretty <session-ref>")
  assert string.contains(usage, "events --pretty --verbose <session-ref>")
  assert string.contains(usage, "task list")
  assert string.contains(usage, "task show <task|id:<id>>")
  assert string.contains(usage, "outbox <outbox-id>")
  assert string.contains(usage, "outbox --status retryable")
  assert string.contains(usage, "attach <session-ref>")
  assert string.contains(usage, "attach --verbose <session-ref>")
  assert string.contains(usage, "attach --raw <session-ref>")
  assert !string.contains(usage, "attach --raw --json <session-ref>")
  assert string.contains(usage, "pause")
  assert string.contains(
    usage,
    "task retry <task|id:<id>> [--start-fresh|--from-scratch --reason <text>]",
  )
  assert string.contains(
    usage,
    "retry <task|id:<id>|run:<run-id>> [--step <step-id>] [--dry-run]",
  )
  assert string.contains(usage, "run retry-step <run-id> --step <step-id>")
  assert string.contains(
    usage,
    "Queue durable retry-step work without redispatching the whole task.",
  )
  assert string.contains(usage, "run recollect-outputs <run-id>")
  assert string.contains(
    usage,
    "run finalize <run-id> --validate --outputs auto --publish --update-tracker --reason <text> [--allow-unpublished] (--dry-run|--yes)",
  )
  assert string.contains(
    usage,
    "publication retry <run-id> [--publication <publication-id>]",
  )
  assert string.contains(
    usage,
    "Recollect workflow contract outputs without rerunning completed steps.",
  )
  assert string.contains(
    usage,
    "Successful acknowledgement returns queued plus an operation_id; poll query operation-status for completion.",
  )
  assert string.contains(usage, "abort <session-ref> --yes")
  assert string.contains(usage, "ui respond")
  assert string.contains(usage, "run-schedule <job> --now")
  assert string.contains(usage, "recovery cleanup-orphan-steps run:<run-id>")
  assert string.contains(usage, "--control-file <path>")
  assert string.contains(usage, "--json")
  assert string.contains(usage, "--verbose")
  assert string.contains(usage, "--since-cursor <n>")
  assert string.contains(usage, "--state <state>")
  assert string.contains(usage, "--status <status>")
  assert string.contains(usage, "--kind <kind>")
  assert string.contains(usage, "--limit <n>")
  assert string.contains(usage, "--cursor <cursor>")
  assert string.contains(usage, "--dry-run")
  assert string.contains(usage, "--step <step-id>")
  assert string.contains(usage, "--start-fresh")
  assert string.contains(usage, "--validate")
  assert string.contains(usage, "--outputs auto")
  assert string.contains(usage, "--publish")
  assert string.contains(usage, "--update-tracker")
  assert !string.contains(usage, "retry-step <target>")
  assert !string.contains(usage, "recollect-outputs run:<run-id>")
  assert !string.contains(usage, "cleanup --yes")
  assert !string.contains(usage, "schedules status")
  assert !string.contains(usage, "artifact publication list --run <run-id>")
  assert !string.contains(usage, "workstream list [task]")
  assert !string.contains(usage, "state status --root <workspace-root>")
  assert !string.contains(usage, "--root <workspace-root>")
  assert !string.contains(usage, "--run <run-id>")
  assert !string.contains(usage, "--publication <publication>")
}

pub fn usage_contains_every_registered_control_command_line_test() {
  let usage = ctl.usage()
  let lines = command_registry.control_usage_lines()
  assert list.all(lines, fn(line) { string.contains(usage, line) })
}

pub fn offline_usage_mentions_offline_commands_and_options_test() {
  let usage = ctl.offline_usage()
  assert string.contains(usage, "Usage: scherzo <offline-command> [options]")
  assert string.contains(usage, "cleanup --yes")
  assert string.contains(usage, "schedules status [job]")
  assert string.contains(usage, "artifact publication list --run <run-id>")
  assert string.contains(usage, "workstream list [task]")
  assert string.contains(usage, "state status --root <workspace-root>")
  assert string.contains(usage, "--root <workspace-root>")
  assert string.contains(usage, "--run <run-id>")
  assert string.contains(usage, "--publication <publication>")
  assert !string.contains(usage, "run-schedule <job> --now")
  assert !string.contains(usage, "--control-file <path>")
}

pub fn schedules_logs_last_replays_retained_session_events_test() {
  let base = "test/tmp/ctl-schedules/logs-retained"
  let root = base <> "/workspaces"
  let control_path = base <> "/control.json"
  test_helpers.reset_dir(base)
  write_scheduled_history(root, "nightly-session")
  write_control_file_for_root(control_path, root)
  let summary =
    event.SessionSummary(
      ..session_summary("nightly-session", ps_now_ms - 1000),
      display_name: "nightly-session",
      issue_id: "scheduled-nightly",
      issue_identifier: "scheduled",
      issue_title: "Scheduled nightly",
    )
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.SchedulesLogs(
        Some(control_path),
        Some(root),
        False,
        style.ColorNever,
        False,
        "nightly",
      ),
      session_ref_deps([summary]),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "nightly-session")
  assert string.contains(transcript, "scheduled")
  assert !string.contains(transcript, "transcript is not available")
}

pub fn schedules_logs_last_reports_expired_transcript_test() {
  let base = "test/tmp/ctl-schedules/logs-expired"
  let root = base <> "/workspaces"
  test_helpers.reset_dir(base)
  write_scheduled_history(root, "expired-session")
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.SchedulesLogs(
        Some(base <> "/missing-control.json"),
        Some(root),
        False,
        style.ColorNever,
        False,
        "nightly",
      ),
      ps_deps([], ps_now_ms, ""),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "job: nightly")
  assert string.contains(
    transcript,
    "run_id: schedule-nightly-20260505T120000Z",
  )
  assert string.contains(transcript, "session_id: expired-session")
  assert string.contains(
    transcript,
    "logs: latest scheduled session transcript is not available",
  )
}

pub fn schedules_doctor_reports_config_and_linear_label_checks_test() {
  let base = "test/tmp/ctl-schedules/doctor-valid"
  let _ =
    write_schedule_doctor_config(base, "Scheduled job {{ scheduled_job.id }}")
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.SchedulesDoctor(None, Some(base), False, "nightly"),
      ps_deps([], ps_now_ms, ""),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "schedule doctor: nightly")
  assert string.contains(transcript, "config_load")
  assert string.contains(transcript, "workflow_exists")
  assert string.contains(transcript, "failure_task_config")
  assert string.contains(
    transcript,
    "failure task reporting has a configured triage state and open_task_per_schedule dedupe",
  )
  assert string.contains(transcript, "dedupe=open_task_per_schedule")
  assert string.contains(transcript, "linear_reserved_labels")
  assert string.contains(transcript, "scherzo:scheduled-job:nightly")
  assert string.contains(transcript, "scheduled_template_context")
  assert string.contains(transcript, "local ledger projection is readable")
}

pub fn schedules_doctor_reports_issue_context_template_failure_test() {
  let base = "test/tmp/ctl-schedules/doctor-issue-context"
  let _ = write_schedule_doctor_config(base, "Issue title: {{ issue.title }}")
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.SchedulesDoctor(None, Some(base), False, "nightly"),
      ps_deps([], ps_now_ms, ""),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(
    transcript,
    "scheduled_workflow_requires_issue_context",
  )
  assert string.contains(transcript, "issue.title")
  assert string.contains(transcript, "workflow nightly")
}

pub fn schedules_doctor_root_resolves_config_path_from_caller_cwd_test() {
  let base = "test/tmp/ctl-schedules/doctor-caller-root"
  test_helpers.reset_dir(base)
  let assert Ok(caller_abs) = path.absolute(base <> "/consumer")
  let config_path =
    write_schedule_doctor_config(
      caller_abs <> "/.scherzo",
      "Scheduled job {{ scheduled_job.id }}",
    )
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps_and_env(
      ctl.SchedulesDoctor(None, Some(".scherzo/workspaces"), False, "nightly"),
      ps_deps([], ps_now_ms, ""),
      output(subject),
      caller_cwd_env(caller_abs),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "config: " <> config_path)
  assert string.contains(transcript, "config_load")
}

pub fn ps_human_table_uses_display_name_and_matches_header_order_test() {
  let path = "test/tmp/ctl-ps/table-order-control.json"
  write_control_file(path)
  let canonical_session_id = "canonical-session-id-should-stay-hidden"
  let display_name = "LIV-43-fancy-narwhal-finger"
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Ps(Some(path), False),
      ps_deps(
        [
          event.SessionSummary(
            ..session_summary(canonical_session_id, ps_now_ms - 12_000),
            display_name: display_name,
            issue_identifier: "LIV-43",
            current_turn: 7,
          ),
        ],
        ps_now_ms,
        "",
      ),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  let assert [header, row] = output_lines(transcript)
  assert table_columns(header)
    == ["SESSION", "ISSUE", "TURN", "STATUS", "RECOVERY", "LAST", "EVENT"]
  assert string.contains(header, "LAST EVENT")
  assert !string.contains(transcript, "LAST_EVENT")
  assert string.contains(transcript, "LIV-43")
  assert string.contains(transcript, "…")
  assert !string.contains(transcript, canonical_session_id)

  let assert [
    session_col,
    issue_col,
    turn_label,
    turn_col,
    status_col,
    recovery_col,
    age_value,
    age_unit,
  ] = table_columns(row)
  assert string.contains(session_col, "…")
  assert issue_col == "LIV-43"
  assert turn_label == "turn"
  assert turn_col == "7"
  assert status_col == "running"
  assert recovery_col == "-"
  assert age_value == "12s"
  assert age_unit == "ago"
}

pub fn ps_human_table_shortens_long_session_names_and_formats_last_event_age_test() {
  let path = "test/tmp/ctl-ps/table-control.json"
  write_control_file(path)
  let top_level_session_name = "LONGISSUE-12345--576460690849-123456789"
  let step_session_name =
    "LONGISSUE-12345--576460690849-123456789-validate_draft"
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Ps(Some(path), False),
      ps_deps(
        [
          session_summary(top_level_session_name, -576_460_690_330),
          session_summary(step_session_name, ps_now_ms - 180_000),
        ],
        ps_now_ms,
        "",
      ),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "SESSION")
  assert string.contains(transcript, "LAST EVENT")
  assert !string.contains(transcript, "LAST_EVENT")
  assert string.contains(transcript, "12s ago")
  assert string.contains(transcript, "3m ago")
  assert string.contains(transcript, "…")
  assert string.contains(transcript, "123456789")
  assert string.contains(transcript, "date_draft")
  assert !string.contains(transcript, top_level_session_name)
  assert !string.contains(transcript, step_session_name)
  assert !string.contains(transcript, "-576460690330")

  let rows = output_lines(transcript)
  assert list.all(rows, fn(row) { string.length(row) <= 80 })
}

pub fn ps_and_session_human_output_show_recovery_metadata_test() {
  let path = "test/tmp/ctl-ps/recovery-control.json"
  write_control_file(path)
  let interrupted =
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
  let parked =
    event.RecoveryInfo(
      ..interrupted,
      status: event.Parked,
      source: "projection.parked_issue",
      message: Some("operator hold"),
      safe_actions: [event.Inspect, event.ViewEvents, event.Unpark],
      workflow_run_id: None,
      current_pi_session_id: None,
      park_reason: Some("operator hold"),
      park_release_policy: Some("explicit_unpark_only"),
      parked_at_ms: Some(1234),
    )
  let quarantined =
    event.RecoveryInfo(
      ..parked,
      message: Some("worker_failure"),
      park_reason: Some("worker_failure"),
      park_release_policy: Some("auto_unpark_on_issue_change"),
    )
  let auto_operator_parked =
    event.RecoveryInfo(
      ..parked,
      message: Some("dispatch_recovery_rejected"),
      park_reason: Some("dispatch_recovery_rejected"),
      park_release_policy: Some("auto_unpark_on_issue_change"),
    )
  let cleanup =
    event.RecoveryInfo(
      ..interrupted,
      status: event.Cleanup,
      source: "retention.classifier",
      message: Some("retention expired"),
      safe_actions: [event.Inspect, event.CleanupDryRunAction],
      cleanup_phase: Some(event.Eligible),
      retention_until_ms: Some(2000),
      cleanup_eligible_at_ms: Some(2000),
    )
  let sessions = [
    event.SessionSummary(
      ..session_summary("session-1", ps_now_ms - 1000),
      recovery: Some(interrupted),
    ),
    event.SessionSummary(
      ..session_summary("session-2", ps_now_ms - 1000),
      recovery: Some(parked),
    ),
    event.SessionSummary(
      ..session_summary("session-3", ps_now_ms - 1000),
      recovery: Some(cleanup),
    ),
    event.SessionSummary(
      ..session_summary("session-4", ps_now_ms - 1000),
      recovery: Some(quarantined),
    ),
    event.SessionSummary(
      ..session_summary("session-5", ps_now_ms - 1000),
      recovery: Some(auto_operator_parked),
    ),
    session_summary("session-6", ps_now_ms - 1000),
  ]
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Ps(Some(path), False),
      ps_deps(sessions, ps_now_ms, ""),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "RECOVERY")
  assert string.contains(transcript, "interrupted")
  assert string.contains(transcript, "parked (operator)")
  assert string.contains(transcript, "quarantined (failure backoff)")
  assert string.contains(transcript, "cleanup")
  assert string.contains(transcript, "-")

  let subject = process.new_subject()
  let result =
    ctl.run_with_deps(
      ctl.Session(Some(path), False, "session-1"),
      session_ref_deps(sessions),
      output(subject),
    )
  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "recovery:")
  assert string.contains(transcript, "status: interrupted")
  assert string.contains(transcript, "source: projection.run_interrupted")
  assert string.contains(transcript, "reason: daemon_restart")
  assert string.contains(
    transcript,
    "safe_actions: inspect, view_events, retry, park",
  )
  assert string.contains(transcript, "current_pi_session_id: pi-current")
  assert string.contains(transcript, "workflow_run_id: run-1")

  let subject = process.new_subject()
  let result =
    ctl.run_with_deps(
      ctl.Session(Some(path), False, "session-2"),
      session_ref_deps(sessions),
      output(subject),
    )
  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "status: parked (operator)")

  let subject = process.new_subject()
  let result =
    ctl.run_with_deps(
      ctl.Session(Some(path), False, "session-4"),
      session_ref_deps(sessions),
      output(subject),
    )
  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "status: quarantined (failure backoff)")

  let subject = process.new_subject()
  let result =
    ctl.run_with_deps(
      ctl.Session(Some(path), False, "session-5"),
      session_ref_deps(sessions),
      output(subject),
    )
  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "status: parked")
  assert !string.contains(transcript, "status: quarantined (failure backoff)")
}

pub fn ps_human_table_shows_exit_outcomes_test() {
  let path = "test/tmp/ctl-ps/exits-control.json"
  write_control_file(path)
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Ps(Some(path), False),
      ps_deps(
        [
          session_summary_with_status(
            "sid-1",
            ps_now_ms,
            event.Exited(reason.Normal),
          ),
          session_summary_with_status(
            "sid-2",
            ps_now_ms,
            event.Exited(reason.Failed),
          ),
          session_summary_with_status(
            "sid-3",
            ps_now_ms,
            event.Exited(reason.WorkerDown),
          ),
          session_summary_with_status(
            "sid-4",
            ps_now_ms,
            event.Exited(reason.OperatorAbort),
          ),
          session_summary_with_status(
            "sid-5",
            ps_now_ms,
            event.Exited(reason.OperatorStopAfterCurrentTurn),
          ),
          session_summary_with_status("sid-6", ps_now_ms, event.WaitingUi),
        ],
        ps_now_ms,
        "",
      ),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "success")
  assert string.contains(transcript, "failed")
  assert string.contains(transcript, "worker_down")
  assert string.contains(transcript, "operator_abort")
  assert string.contains(transcript, "op_stop_after")
  assert string.contains(transcript, "waiting_ui")
  assert !string.contains(transcript, "exited")

  let rows = string.trim(transcript) |> string.split(on: "\n")
  assert list.all(rows, fn(row) { string.length(row) <= 80 })
}

pub fn ps_human_table_ellipsizes_long_display_name_without_shifting_columns_test() {
  let path = "test/tmp/ctl-ps/table-long-name-control.json"
  write_control_file(path)
  let display_name =
    "LIV-44-this-is-a-very-long-session-display-name-that-keeps-going"
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Ps(Some(path), False),
      ps_deps(
        [
          event.SessionSummary(
            ..session_summary("canonical-session-id", ps_now_ms - 180_000),
            display_name: display_name,
            issue_identifier: "LIV-44",
            current_turn: 42,
          ),
        ],
        ps_now_ms,
        "",
      ),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  let assert [_, row] = output_lines(transcript)
  let assert [
    session_col,
    issue_col,
    turn_label,
    turn_col,
    status_col,
    recovery_col,
    age_value,
    age_unit,
  ] = table_columns(row)
  assert string.contains(session_col, "…")
  assert !string.contains(transcript, display_name)
  assert issue_col == "LIV-44"
  assert turn_label == "turn"
  assert turn_col == "42"
  assert status_col == "running"
  assert recovery_col == "-"
  assert age_value == "3m"
  assert age_unit == "ago"
  assert string.length(row) <= 80
}

pub fn ctl_turn_telemetry_human_and_raw_outputs_test() {
  let path = "test/tmp/ctl-ps/turn-control.json"
  write_control_file(path)
  let summary =
    event.SessionSummary(
      ..session_summary("session-turn", ps_now_ms - 1000),
      current_turn: 3,
      current_turn_status: Some(turn_telemetry.StatusRunning),
      current_turn_started_at_ms: Some(ps_now_ms - 2000),
    )
  let deps = turn_deps(summary)

  let ps_subject = process.new_subject()
  assert ctl.run_with_deps(ctl.Ps(Some(path), False), deps, output(ps_subject))
    == Ok(Nil)
  let ps_transcript = drain_output(ps_subject)
  assert string.contains(ps_transcript, "turn 3 running")

  let session_subject = process.new_subject()
  assert ctl.run_with_deps(
      ctl.Session(Some(path), False, "session-turn"),
      deps,
      output(session_subject),
    )
    == Ok(Nil)
  let session_transcript = drain_output(session_subject)
  assert string.contains(session_transcript, "turn: turn 3 running")
  assert string.contains(session_transcript, "turn_started_at_ms:")

  let events_subject = process.new_subject()
  assert ctl.run_with_deps(
      ctl.Events(
        Some(path),
        ctl.Pretty,
        style.ColorNever,
        0,
        False,
        "session-turn",
      ),
      deps,
      output(events_subject),
    )
    == Ok(Nil)
  let events_transcript = drain_output(events_subject)
  assert string.contains(events_transcript, "turn 3 finished")
  assert string.contains(events_transcript, "+15 tok")

  let attach_subject = process.new_subject()
  assert ctl.run_with_deps(
      ctl.Attach(
        Some(path),
        ctl.Raw,
        style.ColorNever,
        ctl.NoFollow,
        0,
        False,
        "session-turn",
      ),
      deps,
      output(attach_subject),
    )
    == Ok(Nil)
  let attach_transcript = drain_output(attach_subject)
  assert string.contains(attach_transcript, "kind=turn")
  assert string.contains(attach_transcript, "name=turn_finished")
  assert string.contains(attach_transcript, "turn=3")
  assert string.contains(attach_transcript, "turn_status=finished")
}

pub fn ps_json_includes_target_context_without_control_token_test() {
  let path = "test/tmp/ctl-ps/json-target-control.json"
  write_control_file(path)
  let raw_response =
    protocol.success_response(
      "1",
      protocol.list_sessions_data(event.SessionList([], ps_now_ms)),
    )
    |> protocol.response_to_string
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Ps(Some(path), True),
      ps_deps([], ps_now_ms, raw_response),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "\"target\":")
  assert string.contains(transcript, "\"control_file_path\":\"" <> path <> "\"")
  assert string.contains(
    transcript,
    "\"workspace_root\":\"test/tmp/ctl-ps/workspaces\"",
  )
  assert string.contains(transcript, "\"host\":\"127.0.0.1\"")
  assert string.contains(transcript, "\"port\":1")
  assert !string.contains(transcript, "token")
}

pub fn control_file_option_resolves_relative_to_caller_cwd_test() {
  let base = "test/tmp/ctl-path-options/control-file"
  let core_root = base <> "/core"
  let caller_root = base <> "/consumer"
  test_helpers.reset_dir(base)
  let assert Ok(core_abs) = path.absolute(core_root)
  let assert Ok(caller_abs) = path.absolute(caller_root)
  let control_rel = file.default_discovery_path
  write_control_file_for_root(core_abs <> "/" <> control_rel, core_abs)
  write_control_file_for_root(caller_abs <> "/" <> control_rel, caller_abs)
  let raw_response =
    protocol.success_response(
      "1",
      protocol.list_sessions_data(event.SessionList([], ps_now_ms)),
    )
    |> protocol.response_to_string
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps_and_env(
      ctl.Ps(Some(control_rel), True),
      ps_deps([], ps_now_ms, raw_response),
      output(subject),
      caller_cwd_env(caller_abs),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(
    transcript,
    "\"control_file_path\":\"" <> caller_abs <> "/" <> control_rel <> "\"",
  )
  assert string.contains(
    transcript,
    "\"workspace_root\":\"" <> caller_abs <> "\"",
  )
  assert !string.contains(transcript, core_abs <> "\"")
}

pub fn root_option_resolves_relative_to_caller_cwd_test() {
  let base = "test/tmp/ctl-path-options/root"
  test_helpers.reset_dir(base)
  let assert Ok(caller_abs) = path.absolute(base <> "/consumer")
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps_and_env(
      ctl.StateStatus(".scherzo/workspaces", True),
      ps_deps([], ps_now_ms, ""),
      output(subject),
      caller_cwd_env(caller_abs),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(
    transcript,
    "\"workspace_root\":\"" <> caller_abs <> "/.scherzo/workspaces\"",
  )
}

pub fn artifact_publication_list_and_show_offline_state_test() {
  let root = "test/tmp/ctl-artifact-publication/workspaces"
  test_helpers.reset_dir("test/tmp/ctl-artifact-publication")
  seed_publication_state(root)
  let subject = process.new_subject()

  assert ctl.run_with_deps(
      ctl.ArtifactPublicationList(None, Some(root), False, "run-1"),
      ps_deps([], ps_now_ms, ""),
      output(subject),
    )
    == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "run_id: run-1")
  assert string.contains(transcript, "execplan_review_doc")
  assert string.contains(transcript, "failed")
  assert string.contains(transcript, "branch:")
  assert string.contains(transcript, "commit_sha:")
  assert string.contains(transcript, "pr_url:")

  let json_subject = process.new_subject()
  assert ctl.run_with_deps(
      ctl.ArtifactPublicationShow(
        None,
        Some(root),
        True,
        "run-1",
        "execplan_review_doc",
      ),
      ps_deps([], ps_now_ms, ""),
      output(json_subject),
    )
    == Ok(Nil)
  let json_transcript = drain_output(json_subject)
  assert string.contains(
    json_transcript,
    "\"publication_id\":\"execplan_review_doc\"",
  )
  assert string.contains(json_transcript, "\"attempt_count\":2")
  assert string.contains(json_transcript, "\"retry_execution_available\":true")
  assert string.contains(
    json_transcript,
    "\"branch\":\"scherzo/workflow.execplan/LIV-739/execplan_review_doc\"",
  )
  assert string.contains(json_transcript, "\"commit_sha\":\"deadbeef\"")
  assert string.contains(
    json_transcript,
    "\"pr_url\":\"https://example.test/pr/1\"",
  )
  assert string.contains(json_transcript, "\"cleanup_diagnostics\":{")
  assert string.contains(
    json_transcript,
    "\"checkout_path\":\""
      <> root
      <> "/.scherzo-state/artifact-repositories/github/example\"",
  )
}

pub fn artifact_publication_retry_reports_file_publication_unsupported_test() {
  let root = "test/tmp/ctl-artifact-publication-retry/workspaces"
  test_helpers.reset_dir("test/tmp/ctl-artifact-publication-retry")
  seed_failed_retry_publication_state(root)
  let command_subject = process.new_subject()

  let assert Error(#(code, message)) =
    ctl_artifact_publication_retry.retry_with_runner(
      root,
      False,
      "run-1",
      Some("execplan_review_doc"),
      retry_publish_runner(command_subject),
      subject_line(process.new_subject()),
    )
  assert code == "publication_retry_attempt_failed"
  assert string.contains(message, "execplan_review_doc")
  assert string.contains(message, "file_publication_unsupported")
  assert drain_output(command_subject) == ""
}

pub fn artifact_publication_retry_uses_retained_workspace_driver_for_commit_stack_test() {
  let base = "test/tmp/ctl-artifact-publication-retry-commit-stack"
  let root = base <> "/workspaces"
  test_helpers.reset_dir(base)
  seed_failed_commit_stack_retry_publication_state(root)
  let subject = process.new_subject()
  let command_subject = process.new_subject()

  assert ctl_artifact_publication_retry.retry_with_runner(
      root,
      True,
      "run-1",
      Some("publish_stack"),
      retained_workspace_publish_runner(command_subject),
      subject_line(subject),
    )
    == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "\"publication_id\":\"publish_stack\"")
  assert string.contains(transcript, "\"status\":\"published\"")
  assert string.contains(
    transcript,
    "\"branch\":\"scherzo/implementation/LIV-917\"",
  )
  assert string.contains(
    transcript,
    "\"pr_url\":\"https://example.test/pr/42\"",
  )
  assert !string.contains(transcript, "\"recorded_at_ms\":0")

  let commands = drain_output(command_subject)
  assert string.contains(commands, "retained-driver publish-commit-stack")
  assert string.contains(
    commands,
    "(cwd="
      <> path.absolute_or_original(root <> "/runs/run-1/workspaces/main")
      <> ")",
  )
  assert !string.contains(commands, ".scherzo-state/artifact-repositories")
}

pub fn artifact_publication_retry_publishes_declared_commit_stack_without_attempt_test() {
  let base = "test/tmp/ctl-artifact-publication-retry-declared-commit-stack"
  let root = base <> "/workspaces"
  test_helpers.reset_dir(base)
  seed_declared_commit_stack_publication_state_without_attempt(root)
  let subject = process.new_subject()
  let command_subject = process.new_subject()

  assert ctl_artifact_publication_retry.retry_with_runner(
      root,
      True,
      "run-1",
      None,
      retained_workspace_publish_runner(command_subject),
      subject_line(subject),
    )
    == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "\"publication_id\":\"publish_stack\"")
  assert string.contains(transcript, "\"status\":\"published\"")
  assert string.contains(
    drain_output(command_subject),
    "retained-driver publish-commit-stack",
  )
}

pub fn artifact_publication_retry_rejects_declared_route_without_snapshot_test() {
  let base = "test/tmp/ctl-artifact-publication-retry-declared-no-snapshot"
  let root = base <> "/workspaces"
  test_helpers.reset_dir(base)
  seed_declared_commit_stack_publication_state(root, include_snapshot: False)
  let command_subject = process.new_subject()

  let assert Error(#(code, message)) =
    ctl_artifact_publication_retry.retry_with_runner(
      root,
      True,
      "run-1",
      None,
      retained_workspace_publish_runner(command_subject),
      subject_line(process.new_subject()),
    )
  assert code == "publication_route_discovery_unsafe"
  assert string.contains(message, "no run-pinned workflow interface snapshot")
  assert drain_output(command_subject) == ""
}

pub fn artifact_publication_retry_rejects_declared_route_drift_test() {
  let base = "test/tmp/ctl-artifact-publication-retry-declared-route-drift"
  let root = base <> "/workspaces"
  test_helpers.reset_dir(base)
  seed_declared_commit_stack_publication_state_without_attempt(root)
  drift_commit_stack_publication_route(root)
  let command_subject = process.new_subject()

  let assert Error(#(code, message)) =
    ctl_artifact_publication_retry.retry_with_runner(
      root,
      True,
      "run-1",
      None,
      retained_workspace_publish_runner(command_subject),
      subject_line(process.new_subject()),
    )
  assert code == "publication_route_discovery_unsafe"
  assert string.contains(message, "publication routes differ")
  assert drain_output(command_subject) == ""
}

pub fn artifact_publication_retry_rejects_corrupt_route_snapshot_test() {
  let base = "test/tmp/ctl-artifact-publication-retry-declared-snapshot-hash"
  let root = base <> "/workspaces"
  test_helpers.reset_dir(base)
  seed_declared_commit_stack_publication_state_without_attempt(root)
  write_seed_artifact(
    root,
    artifact_store.workflow_interface_snapshot_ref("run-1"),
    "{}",
  )
  let command_subject = process.new_subject()

  let assert Error(#(code, message)) =
    ctl_artifact_publication_retry.retry_with_runner(
      root,
      True,
      "run-1",
      None,
      retained_workspace_publish_runner(command_subject),
      subject_line(process.new_subject()),
    )
  assert code == "publication_route_discovery_snapshot_hash_mismatch"
  assert string.contains(message, "workflow-interface.v1.json")
  assert drain_output(command_subject) == ""
}

pub fn artifact_publication_retry_replans_pre_execution_file_failure_to_unsupported_test() {
  let root = "test/tmp/ctl-artifact-publication-retry-replans/workspaces"
  test_helpers.reset_dir("test/tmp/ctl-artifact-publication-retry-replans")
  seed_pre_execution_failed_publication_state(root)
  let command_subject = process.new_subject()

  let assert Error(#(code, message)) =
    ctl_artifact_publication_retry.retry_with_runner(
      root,
      False,
      "run-1",
      Some("execplan_review_doc"),
      retry_publish_runner(command_subject),
      subject_line(process.new_subject()),
    )
  assert code == "publication_retry_attempt_failed"
  assert string.contains(message, "file_publication_unsupported")
  assert drain_output(command_subject) == ""
}

pub fn artifact_publication_retry_rejects_tampered_output_manifest_test() {
  let root = "test/tmp/ctl-artifact-publication-retry-output-hash/workspaces"
  test_helpers.reset_dir("test/tmp/ctl-artifact-publication-retry-output-hash")
  seed_pre_execution_failed_publication_state(root)
  let assert Ok(Nil) =
    simplifile.write(
      root <> "/.scherzo-state/artifacts/runs/run-1/contract/outputs.json",
      "{}",
    )
  let command_subject = process.new_subject()

  let assert Error(#(code, message)) =
    ctl_artifact_publication_retry.retry_with_runner(
      root,
      False,
      "run-1",
      Some("execplan_review_doc"),
      retry_publish_runner(command_subject),
      subject_line(process.new_subject()),
    )
  assert code == "publication_retry_output_manifest_hash_mismatch"
  assert string.contains(message, "runs/run-1/contract/outputs.json")
  assert drain_output(command_subject) == ""
}

pub fn artifact_publication_retry_rejects_retryable_attempt_without_replan_evidence_test() {
  let root =
    "test/tmp/ctl-artifact-publication-retry-replan-unavailable/workspaces"
  test_helpers.reset_dir(
    "test/tmp/ctl-artifact-publication-retry-replan-unavailable",
  )
  seed_latest_publication_state(root, "failed", True, False)
  let command_subject = process.new_subject()

  let assert Error(#(code, message)) =
    ctl_artifact_publication_retry.retry_with_runner(
      root,
      False,
      "run-1",
      Some("execplan_review_doc"),
      retry_publish_runner(command_subject),
      subject_line(process.new_subject()),
    )
  assert code == "publication_retry_replan_unavailable"
  assert string.contains(message, "cannot be safely re-planned")
  assert string.contains(message, "execplan_review_doc")
  assert drain_output(command_subject) == ""
}

pub fn artifact_publication_retry_all_rejects_retryable_attempt_without_replan_evidence_test() {
  let root =
    "test/tmp/ctl-artifact-publication-retry-all-replan-unavailable/workspaces"
  test_helpers.reset_dir(
    "test/tmp/ctl-artifact-publication-retry-all-replan-unavailable",
  )
  seed_latest_publication_state(root, "failed", True, False)
  let command_subject = process.new_subject()

  let assert Error(#(code, message)) =
    ctl_artifact_publication_retry.retry_with_runner(
      root,
      False,
      "run-1",
      None,
      retry_publish_runner(command_subject),
      subject_line(process.new_subject()),
    )
  assert code == "publication_retry_replan_unavailable"
  assert string.contains(message, "cannot be safely re-planned")
  assert string.contains(message, "execplan_review_doc")
  assert drain_output(command_subject) == ""
}

pub fn artifact_publication_retry_rejects_unknown_publication_id_test() {
  let root =
    "test/tmp/ctl-artifact-publication-retry-missing-publication/workspaces"
  test_helpers.reset_dir(
    "test/tmp/ctl-artifact-publication-retry-missing-publication",
  )
  seed_failed_retry_publication_state(root)
  let command_subject = process.new_subject()

  let assert Error(#(code, message)) =
    ctl_artifact_publication_retry.retry_with_runner(
      root,
      False,
      "run-1",
      Some("missing_doc"),
      retry_publish_runner(command_subject),
      subject_line(process.new_subject()),
    )
  assert code == "publication_not_found"
  assert string.contains(message, "missing_doc")
  assert drain_output(command_subject) == ""
}

pub fn artifact_publication_retry_rejects_non_retryable_latest_statuses_test() {
  assert_retry_rejects_latest_status("planned", "planned", False, False)
  assert_retry_rejects_latest_status("published", "published", False, True)
  assert_retry_rejects_latest_status(
    "failed-nonretryable",
    "failed",
    False,
    True,
  )
}

pub fn artifact_publication_retry_without_publication_reports_file_publication_unsupported_test() {
  let root = "test/tmp/ctl-artifact-publication-retry-all/workspaces"
  test_helpers.reset_dir("test/tmp/ctl-artifact-publication-retry-all")
  seed_failed_retry_publication_state(root)
  let command_subject = process.new_subject()

  let assert Error(#(code, message)) =
    ctl_artifact_publication_retry.retry_with_runner(
      root,
      False,
      "run-1",
      None,
      retry_publish_runner(command_subject),
      subject_line(process.new_subject()),
    )
  assert code == "publication_retry_attempt_failed"
  assert string.contains(message, "file_publication_unsupported")
  assert drain_output(command_subject) == ""
}

pub fn artifact_publication_retry_requires_output_manifest_test() {
  let root = "test/tmp/ctl-artifact-publication-retry-missing-output/workspaces"
  test_helpers.reset_dir(
    "test/tmp/ctl-artifact-publication-retry-missing-output",
  )
  seed_failed_retry_publication_state_without_output_manifest(root)

  let assert Error(#(code, message)) =
    ctl_artifact_publication_retry.retry_with_runner(
      root,
      False,
      "run-1",
      Some("execplan_review_doc"),
      retry_publish_runner(process.new_subject()),
      subject_line(process.new_subject()),
    )
  assert code == "publication_retry_output_manifest_missing"
  assert string.contains(message, "run-1")
}

pub fn artifact_publication_retry_rejects_config_drift_test() {
  let base = "test/tmp/ctl-artifact-publication-retry-config-drift"
  let root = base <> "/workspaces"
  test_helpers.reset_dir(base)
  seed_failed_retry_publication_state(root)
  let assert Ok(Nil) =
    simplifile.write(
      base <> "/scherzo.yaml",
      string.replace(
        in: read_file_or_panic(base <> "/scherzo.yaml"),
        each: "/{{ publication.id }}",
        with: "/{{ publication.version_id }}",
      ),
    )

  let assert Error(#(code, message)) =
    ctl_artifact_publication_retry.retry_with_runner(
      root,
      False,
      "run-1",
      Some("execplan_review_doc"),
      retry_publish_runner(process.new_subject()),
      subject_line(process.new_subject()),
    )
  assert code == "publication_retry_config_drift"
  assert string.contains(message, "execplan_review_doc")
}

pub fn artifact_publication_retry_rejects_required_config_drift_test() {
  let base = "test/tmp/ctl-artifact-publication-retry-required-drift"
  let root = base <> "/workspaces"
  test_helpers.reset_dir(base)
  seed_pre_execution_failed_publication_state(root)
  let workflow_path = base <> "/workflows/execplan.yaml"
  let assert Ok(Nil) =
    simplifile.write(
      workflow_path,
      string.replace(
        in: read_file_or_panic(workflow_path),
        each: "      required: true\n",
        with: "      required: false\n",
      ),
    )
  let command_subject = process.new_subject()

  let assert Error(#(code, message)) =
    ctl_artifact_publication_retry.retry_with_runner(
      root,
      False,
      "run-1",
      Some("execplan_review_doc"),
      retry_publish_runner(command_subject),
      subject_line(process.new_subject()),
    )
  assert code == "publication_retry_config_drift"
  assert string.contains(message, "execplan_review_doc")
  assert drain_output(command_subject) == ""
}

pub fn artifact_publication_uses_control_file_root_and_reports_not_found_test() {
  let base = "test/tmp/ctl-artifact-publication-control"
  let root = base <> "/workspaces"
  let control_path = base <> "/control.json"
  test_helpers.reset_dir(base)
  write_control_file_for_root(control_path, root)
  seed_publication_state(root)
  let subject = process.new_subject()

  assert ctl.run_with_deps(
      ctl.ArtifactPublicationList(Some(control_path), None, True, "run-1"),
      ps_deps([], ps_now_ms, ""),
      output(subject),
    )
    == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "\"workspace_root\":\"" <> root <> "\"")

  let assert Error(missing_run) =
    ctl.run_with_deps(
      ctl.ArtifactPublicationList(None, Some(root), False, "missing-run"),
      ps_deps([], ps_now_ms, ""),
      output(process.new_subject()),
    )
  assert ctl.error_code(missing_run) == "publication_run_not_found"

  let assert Error(missing_publication) =
    ctl.run_with_deps(
      ctl.ArtifactPublicationShow(
        None,
        Some(root),
        False,
        "run-1",
        "missing_publication",
      ),
      ps_deps([], ps_now_ms, ""),
      output(process.new_subject()),
    )
  assert ctl.error_code(missing_publication) == "publication_not_found"
}

pub fn artifact_publication_retry_without_root_requires_explicit_workspace_root_test() {
  let subject = process.new_subject()

  assert ctl.run_with_deps(
      ctl.ArtifactPublicationRetry(
        None,
        None,
        False,
        "run-1",
        Some("execplan_review_doc"),
      ),
      ps_deps([], ps_now_ms, ""),
      output(subject),
    )
    == Error(ctl.UsageError(
      "artifact publication retry requires --root <workspace-root>",
    ))
  assert drain_output(subject) == ""
}

pub fn artifact_publication_retry_refuses_when_instance_lock_is_held_test() {
  let base = "test/tmp/ctl-artifact-publication-retry-lock-held"
  let root = base <> "/workspaces"
  test_helpers.reset_dir(base)
  seed_publication_state(root)
  let assert Ok(lock) = instance_lock.acquire(root)

  let assert Error(error) =
    ctl.run_with_deps(
      ctl.ArtifactPublicationRetry(None, Some(root), False, "run-1", None),
      ps_deps([], ps_now_ms, ""),
      output(process.new_subject()),
    )
  assert ctl.error_code(error) == "instance_lock_failed"
  assert string.contains(
    ctl.error_message(error),
    "instance lock already exists",
  )

  instance_lock.release(lock)
}

pub fn artifact_publication_retry_releases_instance_lock_after_failure_test() {
  let base = "test/tmp/ctl-artifact-publication-retry-lock-release"
  let root = base <> "/workspaces"
  let config_path = base <> "/scherzo.yaml"
  test_helpers.reset_dir(base)
  seed_publication_state(root)
  let assert Ok(Nil) = simplifile.write(config_path, "version: [")

  let assert Error(error) =
    ctl.run_with_deps(
      ctl.ArtifactPublicationRetry(None, Some(root), False, "run-1", None),
      ps_deps([], ps_now_ms, ""),
      output(process.new_subject()),
    )
  assert ctl.error_code(error) != "instance_lock_failed"

  let assert Ok(lock) = instance_lock.acquire(root)
  instance_lock.release(lock)
}

pub fn artifact_publication_retry_invalid_root_does_not_create_lock_state_test() {
  let base = "test/tmp/ctl-artifact-publication-retry-invalid-root"
  let root = base <> "/workspaces"
  test_helpers.reset_dir(base)

  let assert Error(error) =
    ctl.run_with_deps(
      ctl.ArtifactPublicationRetry(None, Some(root), False, "run-1", None),
      ps_deps([], ps_now_ms, ""),
      output(process.new_subject()),
    )
  assert ctl.error_code(error) != "instance_lock_failed"
  assert simplifile.is_directory(root <> "/.scherzo-state") == Ok(False)
}

pub fn state_repair_run_provenance_dry_run_yes_and_idempotent_test() {
  let root = "test/tmp/ctl-state-repair-provenance/workspaces"
  test_helpers.reset_dir("test/tmp/ctl-state-repair-provenance")
  seed_missing_provenance_state(root)
  let subject = process.new_subject()

  assert ctl.run_with_deps(
      ctl.StateRepairRunProvenance(root, True, "run-1", True, False),
      ps_deps([], ps_now_ms, ""),
      output(subject),
    )
    == Ok(Nil)
  let dry_run = drain_output(subject)
  assert string.contains(dry_run, "\"status\":\"dry_run\"")
  assert string.contains(dry_run, "\"repair_status\":\"would_repair\"")
  assert !has_provenance_repair_record(root)

  assert ctl.run_with_deps(
      ctl.StateRepairRunProvenance(root, True, "run-1", False, True),
      ps_deps([], ps_now_ms, ""),
      output(subject),
    )
    == Ok(Nil)
  let repaired = drain_output(subject)
  assert string.contains(repaired, "\"status\":\"repaired\"")
  assert has_provenance_repair_record(root)

  assert ctl.run_with_deps(
      ctl.StateRepairRunProvenance(root, True, "run-1", False, True),
      ps_deps([], ps_now_ms, ""),
      output(subject),
    )
    == Ok(Nil)
  let already = drain_output(subject)
  assert string.contains(already, "\"status\":\"already_repaired\"")
  assert provenance_repair_record_count(root) == 1
}

pub fn state_repair_run_provenance_rejects_live_control_file_test() {
  let root = "test/tmp/ctl-state-repair-provenance-control-file/workspaces"
  test_helpers.reset_dir("test/tmp/ctl-state-repair-provenance-control-file")
  seed_missing_provenance_state(root)
  let control_path = file.path_for_workspace(root)
  let assert Ok(Nil) =
    file.write(
      control_path,
      file.ControlFile(
        host: "127.0.0.1",
        port: 4010,
        token: "token",
        workspace_root: root,
        started_at_ms: 1000,
        command_timeout_ms: 1000,
      ),
    )
  let subject = process.new_subject()

  assert ctl.run_with_deps(
      ctl.StateRepairRunProvenance(root, True, "run-1", False, True),
      ps_deps([], ps_now_ms, ""),
      output(subject),
    )
    == Ok(Nil)
  let rejected = drain_output(subject)
  assert string.contains(rejected, "\"status\":\"rejected\"")
  assert string.contains(rejected, "\"reason\":\"daemon_control_file_present\"")
  assert string.contains(rejected, ".scherzo-state/control.json")
  assert !has_provenance_repair_record(root)
}

pub fn state_repair_run_provenance_rejects_parent_traversal_run_root_test() {
  let root = "test/tmp/ctl-state-repair-provenance-traversal/workspaces"
  test_helpers.reset_dir("test/tmp/ctl-state-repair-provenance-traversal")
  seed_missing_provenance_state_with(
    root,
    root <> "/runs/../../outside",
    "implementation",
    "wf-1",
    True,
  )
  let subject = process.new_subject()

  assert ctl.run_with_deps(
      ctl.StateRepairRunProvenance(root, True, "run-1", False, True),
      ps_deps([], ps_now_ms, ""),
      output(subject),
    )
    == Ok(Nil)
  let rejected = drain_output(subject)
  assert string.contains(rejected, "\"status\":\"rejected\"")
  assert string.contains(rejected, "\"reason\":\"workspace_recovery_failed\"")
  assert !has_provenance_repair_record(root)
}

pub fn state_repair_run_provenance_rejects_incomplete_evidence_test() {
  let root = "test/tmp/ctl-state-repair-provenance-incomplete/workspaces"
  let run_root = root <> "/runs/run-1"
  test_helpers.reset_dir("test/tmp/ctl-state-repair-provenance-incomplete")
  let assert Ok(Nil) = simplifile.create_directory_all(run_root)
  seed_missing_provenance_state_with(
    root,
    run_root,
    "implementation",
    "wf-1",
    False,
  )
  let subject = process.new_subject()

  assert ctl.run_with_deps(
      ctl.StateRepairRunProvenance(root, True, "run-1", False, True),
      ps_deps([], ps_now_ms, ""),
      output(subject),
    )
    == Ok(Nil)
  let rejected = drain_output(subject)
  assert string.contains(rejected, "\"status\":\"rejected\"")
  assert string.contains(
    rejected,
    "\"reason\":\"workflow_provenance_incomplete\"",
  )
  assert string.contains(rejected, "issue_identifier")
  assert !has_provenance_repair_record(root)
}

pub fn state_repair_run_provenance_rejects_ambiguous_evidence_test() {
  let root = "test/tmp/ctl-state-repair-provenance-ambiguous/workspaces"
  let run_root = root <> "/runs/run-1"
  test_helpers.reset_dir("test/tmp/ctl-state-repair-provenance-ambiguous")
  let assert Ok(Nil) = simplifile.create_directory_all(run_root)
  seed_missing_provenance_state_with(
    root,
    run_root,
    "other-workflow",
    "wf-1",
    True,
  )
  let subject = process.new_subject()

  assert ctl.run_with_deps(
      ctl.StateRepairRunProvenance(root, True, "run-1", False, True),
      ps_deps([], ps_now_ms, ""),
      output(subject),
    )
    == Ok(Nil)
  let rejected = drain_output(subject)
  assert string.contains(rejected, "\"status\":\"rejected\"")
  assert string.contains(
    rejected,
    "\"reason\":\"workflow_provenance_ambiguous\"",
  )
  assert !has_provenance_repair_record(root)
}

pub fn ps_json_preserves_full_session_ids_and_raw_fields_test() {
  let path = "test/tmp/ctl-ps/json-control.json"
  write_control_file(path)
  let session_id = "LONGISSUE-12345--576460690849-123456789-validate_draft"
  let raw_response =
    "{\"session_id\":\""
    <> session_id
    <> "\",\"status\":\"exited\",\"exit_reason\":\"failed\",\"last_event_at_ms\":-576460690330}"
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Ps(Some(path), True),
      ps_deps([], ps_now_ms, raw_response),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, session_id)
  assert string.contains(transcript, "\"status\":\"exited\"")
  assert string.contains(transcript, "\"exit_reason\":\"failed\"")
  assert string.contains(transcript, "-576460690330")
  assert !string.contains(transcript, "…")
}

pub fn session_display_ref_resolves_to_canonical_and_prints_both_names_test() {
  let path = "test/tmp/ctl-ps/session-ref-control.json"
  write_control_file(path)
  let canonical_session_id = "LIV-43--576460751551-1"
  let display_name = "liv-43-fancy-narwhal-finger"
  let summary =
    event.SessionSummary(
      ..session_summary(canonical_session_id, ps_now_ms - 1000),
      display_name: display_name,
      issue_identifier: "LIV-43",
    )
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Session(Some(path), False, display_name),
      session_ref_deps([summary]),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "display_name: " <> display_name)
  assert string.contains(transcript, "session_id: " <> canonical_session_id)
}

pub fn session_human_output_appends_workflow_recovery_history_test() {
  let base = "test/tmp/ctl-session-recovery-history"
  let root = base <> "/workspaces"
  let path = base <> "/control.json"
  test_helpers.reset_dir(base)
  write_control_file_for_root(path, root)
  write_workflow_recovery_history(root)
  let summary =
    event.SessionSummary(
      ..session_summary("session-1", ps_now_ms - 1000),
      issue_identifier: "LIV-490",
      issue_title: "Operator history for workflow step recovery",
    )
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Session(Some(path), False, "session-1"),
      session_ref_deps([summary]),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "display_name: session-1")
  assert string.contains(transcript, "workflow_step_recovery_history:")
  assert string.contains(transcript, "decision: recheck")
  assert string.contains(transcript, "recheck_attempt_index: 2")
  assert string.contains(transcript, "recheck_result: succeeded")
  assert string.contains(
    transcript,
    "final_workflow_outcome: succeeded_after_recovery",
  )
}

pub fn session_json_output_remains_raw_when_recovery_history_exists_test() {
  let base = "test/tmp/ctl-session-recovery-history-json"
  let root = base <> "/workspaces"
  let path = base <> "/control.json"
  test_helpers.reset_dir(base)
  write_control_file_for_root(path, root)
  write_workflow_recovery_history(root)
  let raw_response =
    "{\"session_id\":\"session-1\",\"display_name\":\"session-1\"}"
  let summary = session_summary("session-1", ps_now_ms - 1000)
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Session(Some(path), True, "session-1"),
      ps_deps([summary], ps_now_ms, raw_response),
      output(subject),
    )

  assert result == Ok(Nil)
  assert drain_output(subject) == raw_response <> "\n"
}

pub fn session_human_output_preserves_base_fields_when_history_unavailable_test() {
  let base = "test/tmp/ctl-session-recovery-history-unavailable"
  let path = base <> "/control.json"
  test_helpers.reset_dir(base)
  write_control_file_for_root(path, "")
  let summary = session_summary("session-unavailable", ps_now_ms - 1000)
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Session(Some(path), False, "session-unavailable"),
      session_ref_deps([summary]),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "display_name: session-unavailable")
  assert string.contains(
    transcript,
    "workflow_step_recovery_history: unavailable (workspace root must not be empty)",
  )
}

pub fn events_json_ref_prefers_exact_session_id_over_display_name_test() {
  let path = "test/tmp/ctl-ps/exact-ref-control.json"
  write_control_file(path)
  let canonical_session_id = "canonical-session-id"
  let sessions = [
    event.SessionSummary(
      ..session_summary(canonical_session_id, ps_now_ms - 1000),
      display_name: "readable-name",
    ),
    event.SessionSummary(
      ..session_summary("other-session-id", ps_now_ms - 1000),
      display_name: canonical_session_id,
    ),
  ]
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Events(
        Some(path),
        ctl.Json,
        style.ColorNever,
        0,
        False,
        canonical_session_id,
      ),
      session_ref_deps(sessions),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(
    transcript,
    "\"session_id\":\"" <> canonical_session_id <> "\"",
  )
  assert !string.contains(transcript, "other-session-id")
}

pub fn attach_display_ref_replays_canonical_session_events_test() {
  let path = "test/tmp/ctl-ps/attach-ref-control.json"
  write_control_file(path)
  let canonical_session_id = "LIV-43--576460751551-1"
  let display_name = "liv-43-fancy-narwhal-finger"
  let summary =
    event.SessionSummary(
      ..session_summary(canonical_session_id, ps_now_ms - 1000),
      display_name: display_name,
    )
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Attach(
        Some(path),
        ctl.Raw,
        style.ColorNever,
        ctl.NoFollow,
        0,
        False,
        display_name,
      ),
      session_ref_deps([summary]),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(transcript, "1 10 " <> canonical_session_id)
}

pub fn operator_command_by_display_ref_routes_to_canonical_session_test() {
  let path = "test/tmp/ctl-ps/operator-ref-control.json"
  write_control_file(path)
  let canonical_session_id = "LIV-43--576460751551-1"
  let display_name = "liv-43-fancy-narwhal-finger"
  let summary =
    event.SessionSummary(
      ..session_summary(canonical_session_id, ps_now_ms - 1000),
      display_name: display_name,
    )
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Operator(Some(path), False, command.AbortSession(display_name)),
      session_ref_deps([summary]),
      output(subject),
    )

  assert result == Ok(Nil)
  let transcript = drain_output(subject)
  assert string.contains(
    transcript,
    "abort applied target=" <> canonical_session_id,
  )
  assert !string.contains(transcript, "target=" <> display_name)
}

pub fn ambiguous_display_ref_returns_clear_error_test() {
  let path = "test/tmp/ctl-ps/ambiguous-ref-control.json"
  write_control_file(path)
  let display_name = "liv-43-fancy-narwhal-finger"
  let sessions = [
    event.SessionSummary(
      ..session_summary("session-a", ps_now_ms - 1000),
      display_name: display_name,
    ),
    event.SessionSummary(
      ..session_summary("session-b", ps_now_ms - 1000),
      display_name: display_name,
    ),
  ]
  let subject = process.new_subject()

  let result =
    ctl.run_with_deps(
      ctl.Session(Some(path), False, display_name),
      session_ref_deps(sessions),
      output(subject),
    )

  let assert Error(err) = result
  assert ctl.error_code(err) == "ambiguous_session_ref"
  assert string.contains(ctl.error_message(err), "ambiguous")
  assert string.contains(ctl.error_message(err), "canonical session_id")
}

fn seed_publication_state(root: String) -> Nil {
  let planned = seeded_publication_plan(root)
  let planned_ref = "runs/run-1/publications/execplan_review_doc/version-1.json"
  let failed_ref = "runs/run-1/publications/execplan_review_doc/failed-1.json"
  let planned_manifest =
    artifact_publication_manifest.planned_manifest(planned, "version-1", 1010)
  let failed_manifest =
    artifact_publication_manifest.failed_from_planned_manifest(
      planned,
      "failed-1",
      1020,
      True,
      Some(planned.branch),
      Some("deadbeef"),
      Some("https://example.test/pr/1"),
      ["docs/plans/LIV-739.md"],
      [],
      artifact_publication_manifest.PublicationErrorInfo(
        code: "unknown_output",
        message: "missing output",
      ),
    )
    |> artifact_publication_manifest.with_cleanup_diagnostics(
      artifact_publication_manifest.CleanupDiagnostics(
        checkout_path: root
          <> "/.scherzo-state/artifact-repositories/github/example",
        pre_cleanup_status: Some("M docs/plans/LIV-739.md"),
        reset_summary: Some("exit=0"),
        clean_summary: Some("exit=0"),
        post_cleanup_status: Some(""),
        cleanup_succeeded: True,
      ),
    )
  let #(planned_sha, planned_bytes) =
    write_publication_manifest(root, planned_ref, planned_manifest)
  let #(failed_sha, failed_bytes) =
    write_publication_manifest(root, failed_ref, failed_manifest)
  write_seed_artifact(root, "runs/run-1/outputs/review_doc.md", "# Review\n")
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.with_id(
          "workflow-started",
          1000,
          record.WorkflowRunStarted(
            run_id: "run-1",
            workflow_id: "execplan",
            workflow_fingerprint: "wf-1",
            issue_id: "issue-1",
            issue_identifier: "LIV-739",
            issue_fingerprint: "issue-fingerprint",
            observed_updated_at_ms: 999,
            run_root: root <> "/runs/run-1",
          ),
        ),
        record.with_id(
          "publication-planned",
          1010,
          record.PublicationAttemptRecorded(
            run_id: "run-1",
            workflow_id: "execplan",
            publication_id: "execplan_review_doc",
            series_id: planned.series_id,
            attempt_id: "version-1",
            status: "planned",
            required: True,
            retryable: False,
            retry_execution_available: False,
            version_id: Some(planned.version_id),
            manifest_ref: Some(planned_ref),
            manifest_sha256: Some(planned_sha),
            manifest_bytes: Some(planned_bytes),
            error_code: None,
            error_message: None,
          ),
        ),
        record.with_id(
          "publication-failed",
          1020,
          record.PublicationAttemptRecorded(
            run_id: "run-1",
            workflow_id: "execplan",
            publication_id: "execplan_review_doc",
            series_id: planned.series_id,
            attempt_id: "failed-1",
            status: "failed",
            required: True,
            retryable: True,
            retry_execution_available: True,
            version_id: Some(planned.version_id),
            manifest_ref: Some(failed_ref),
            manifest_sha256: Some(failed_sha),
            manifest_bytes: Some(failed_bytes),
            error_code: Some("unknown_output"),
            error_message: Some("missing output"),
          ),
        ),
      ],
      True,
    )
  Nil
}

fn assert_retry_rejects_latest_status(
  suffix: String,
  status: String,
  retryable: Bool,
  retry_execution_available: Bool,
) -> Nil {
  let base = "test/tmp/ctl-artifact-publication-retry-nonretryable-" <> suffix
  let root = base <> "/workspaces"
  test_helpers.reset_dir(base)
  seed_latest_publication_state(
    root,
    status,
    retryable,
    retry_execution_available,
  )
  let command_subject = process.new_subject()

  let assert Error(#(code, message)) =
    ctl_artifact_publication_retry.retry_with_runner(
      root,
      False,
      "run-1",
      Some("execplan_review_doc"),
      retry_publish_runner(command_subject),
      subject_line(process.new_subject()),
    )
  assert code == "publication_not_retryable"
  assert string.contains(message, "execplan_review_doc")
  assert string.contains(message, "status=" <> status)
  assert drain_output(command_subject) == ""
}

fn seed_latest_publication_state(
  root: String,
  status: String,
  retryable: Bool,
  retry_execution_available: Bool,
) -> Nil {
  let planned = seeded_publication_plan(root)
  let #(manifest_ref, manifest_sha, manifest_bytes) =
    write_seeded_publication_manifest(root, planned, "attempt-1", 1020, status)
  append_publication_seed_records(root, [
    publication_attempt_record(
      planned,
      "attempt-1",
      1020,
      status,
      retryable,
      retry_execution_available,
      Some(manifest_ref),
      Some(manifest_sha),
      Some(manifest_bytes),
    ),
  ])
}

fn seed_failed_retry_publication_state(root: String) -> Nil {
  seed_failed_retry_publication_state_with_error(
    root,
    "git_push_failed",
    "previous push failed",
    None,
  )
}

fn seed_failed_commit_stack_retry_publication_state(root: String) -> Nil {
  let config_path = write_commit_stack_retry_publication_config(root)
  write_commit_stack_retained_workspace_manifest(root)
  let output_manifest = seeded_commit_stack_output_manifest(root)
  let assert Ok(bundle) = runtime_bundle.load(Some(config_path))
  let assert Ok(#(_, workflow)) =
    runtime_bundle.workflow_by_id(bundle, "implementation")
  let assert [route] = workflow_dag.publication_routes(workflow)
  let work =
    artifact_publication_planner.PublicationWork(
      kind: artifact_publication_planner.TaskWork,
      id: "issue-1",
      identifier: "LIV-917",
      slug: "LIV-917",
      title: Some("Retry commit stack"),
      url: Some("https://linear.example/LIV-917"),
    )
  let assert Ok(planned) =
    artifact_publication_planner.plan_publication(
      output_manifest,
      bundle.orchestrator.artifact_repositories,
      route,
      artifact_store.new(root),
      work,
      "run-1",
      dict.new(),
    )
  let failed_ref = "runs/run-1/publications/publish_stack/failed-1.json"
  let failed_manifest =
    artifact_publication_manifest.failed_from_planned_manifest(
      planned,
      "failed-1",
      1020,
      True,
      Some(planned.branch),
      None,
      Some("https://example.test/pr/42"),
      [],
      [],
      artifact_publication_manifest.PublicationErrorInfo(
        code: "workspace_driver_publish_failed",
        message: "previous driver publication failed",
      ),
    )
  let #(failed_sha, failed_bytes) =
    write_publication_manifest(root, failed_ref, failed_manifest)
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.with_id(
          "workflow-started",
          1000,
          record.WorkflowRunStarted(
            run_id: "run-1",
            workflow_id: "implementation",
            workflow_fingerprint: "wf-1",
            issue_id: "issue-1",
            issue_identifier: "LIV-917",
            issue_fingerprint: "issue-fingerprint",
            observed_updated_at_ms: 999,
            run_root: root <> "/runs/run-1",
          ),
        ),
        commit_stack_output_manifest_record(root, output_manifest),
        record.with_id(
          "publication-failed",
          1020,
          record.PublicationAttemptRecorded(
            run_id: "run-1",
            workflow_id: "implementation",
            publication_id: "publish_stack",
            series_id: planned.series_id,
            attempt_id: "failed-1",
            status: "failed",
            required: True,
            retryable: True,
            retry_execution_available: True,
            version_id: Some(planned.version_id),
            manifest_ref: Some(failed_ref),
            manifest_sha256: Some(failed_sha),
            manifest_bytes: Some(failed_bytes),
            error_code: Some("workspace_driver_publish_failed"),
            error_message: Some("previous driver publication failed"),
          ),
        ),
      ],
      True,
    )
  Nil
}

fn seed_declared_commit_stack_publication_state_without_attempt(
  root: String,
) -> Nil {
  seed_declared_commit_stack_publication_state(root, include_snapshot: True)
}

fn seed_declared_commit_stack_publication_state(
  root: String,
  include_snapshot include_snapshot: Bool,
) -> Nil {
  let config_path = write_commit_stack_retry_publication_config(root)
  write_commit_stack_retained_workspace_manifest(root)
  let output_manifest = seeded_commit_stack_output_manifest(root)
  let assert Ok(bundle) = runtime_bundle.load(Some(config_path))
  let assert Ok(#(_, workflow)) =
    runtime_bundle.workflow_by_id(bundle, "implementation")
  let fingerprint =
    workflow_attempt.workflow_fingerprint(workflow, bundle.orchestrator)
  let snapshot_records = case include_snapshot {
    True -> [workflow_interface_snapshot_record(root, workflow, fingerprint)]
    False -> []
  }
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      list.append(
        [
          record.with_id(
            "workflow-started",
            1000,
            record.WorkflowRunStarted(
              run_id: "run-1",
              workflow_id: "implementation",
              workflow_fingerprint: fingerprint,
              issue_id: "issue-1",
              issue_identifier: "LIV-917",
              issue_fingerprint: "issue-fingerprint",
              observed_updated_at_ms: 999,
              run_root: root <> "/runs/run-1",
            ),
          ),
          ..snapshot_records
        ],
        [
          commit_stack_output_manifest_record(root, output_manifest),
          record.with_id(
            "workflow-finished",
            1020,
            record.WorkflowRunFinished(
              run_id: "run-1",
              workflow_id: "implementation",
              issue_id: "issue-1",
              outcome: "completed",
              token_total: 0,
              turns: 1,
            ),
          ),
        ],
      ),
      True,
    )
  Nil
}

fn workflow_interface_snapshot_record(
  root: String,
  workflow: workflow_dag.WorkflowDag,
  fingerprint: String,
) -> record.LedgerRecord {
  let ref = artifact_store.workflow_interface_snapshot_ref("run-1")
  let contents =
    workflow_interface_snapshot.from_dag(workflow, fingerprint)
    |> workflow_interface_snapshot.to_string
  write_seed_artifact(root, ref, contents)
  record.with_id(
    "workflow-interface-snapshot",
    1005,
    record.WorkflowInterfaceSnapshotRecorded(
      run_id: "run-1",
      workflow_id: "implementation",
      workflow_fingerprint: fingerprint,
      artifact_ref: ref,
      artifact_sha256: hash.sha256_hex(contents),
      artifact_bytes: bit_array.byte_size(bit_array.from_string(contents)),
    ),
  )
}

fn drift_commit_stack_publication_route(root: String) -> Nil {
  let assert Ok(base) = path.dirname(root)
  let assert Ok(Nil) =
    simplifile.write(
      base <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    commit_stack:\n      type: commit_stack\n      source:\n        step: main\n        field: stdout\n    merge_target:\n      type: code_change\n      source:\n        step: main\n        field: stdout\nartifacts:\n  publications:\n    - id: publish_stack_after_run\n      repository: github.code\n      required: true\n      mode: commit_stack\n      commit_stack:\n        select:\n          output: commit_stack\n      target:\n        kind: existing_pr_branch\n        source:\n          output: merge_target\nsteps:\n  - id: main\n    kind: command\n    run: ignored\n",
    )
  Nil
}

fn seed_failed_retry_publication_state_with_error(
  root: String,
  error_code: String,
  error_message: String,
  cleanup_diagnostics: Option(artifact_publication_manifest.CleanupDiagnostics),
) -> Nil {
  let planned = seeded_publication_plan(root)
  let failed_ref = "runs/run-1/publications/execplan_review_doc/failed-1.json"
  let failed_manifest =
    artifact_publication_manifest.failed_from_planned_manifest(
      planned,
      "failed-1",
      1020,
      True,
      Some(planned.branch),
      None,
      None,
      ["docs/plans/LIV-739.md"],
      [],
      artifact_publication_manifest.PublicationErrorInfo(
        code: error_code,
        message: error_message,
      ),
    )
    |> add_cleanup_diagnostics(cleanup_diagnostics)
  let #(failed_sha, failed_bytes) =
    write_publication_manifest(root, failed_ref, failed_manifest)
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.with_id(
          "workflow-started",
          1000,
          record.WorkflowRunStarted(
            run_id: "run-1",
            workflow_id: "execplan",
            workflow_fingerprint: "wf-1",
            issue_id: "issue-1",
            issue_identifier: "LIV-739",
            issue_fingerprint: "issue-fingerprint",
            observed_updated_at_ms: 999,
            run_root: root <> "/runs/run-1",
          ),
        ),
        seeded_output_manifest_record(root),
        record.with_id(
          "publication-failed",
          1020,
          record.PublicationAttemptRecorded(
            run_id: "run-1",
            workflow_id: "execplan",
            publication_id: "execplan_review_doc",
            series_id: planned.series_id,
            attempt_id: "failed-1",
            status: "failed",
            required: True,
            retryable: True,
            retry_execution_available: True,
            version_id: Some(planned.version_id),
            manifest_ref: Some(failed_ref),
            manifest_sha256: Some(failed_sha),
            manifest_bytes: Some(failed_bytes),
            error_code: Some(error_code),
            error_message: Some(error_message),
          ),
        ),
      ],
      True,
    )
  Nil
}

fn add_cleanup_diagnostics(
  manifest: artifact_publication_manifest.PublicationManifest,
  cleanup_diagnostics: Option(artifact_publication_manifest.CleanupDiagnostics),
) -> artifact_publication_manifest.PublicationManifest {
  case cleanup_diagnostics {
    Some(cleanup) ->
      artifact_publication_manifest.with_cleanup_diagnostics(manifest, cleanup)
    None -> manifest
  }
}

fn seed_pre_execution_failed_publication_state(root: String) -> Nil {
  let planned = seeded_publication_plan(root)
  let series_id = "issue-1:execplan:" <> planned.publication_id
  let failed_ref =
    "runs/run-1/publications/execplan_review_doc/failed-preplan.json"
  let error =
    artifact_publication_manifest.PublicationErrorInfo(
      code: "invalid_artifact_set_descriptor",
      message: "artifact descriptor is missing required field: name",
    )
  let failed_manifest =
    artifact_publication_manifest.failed_manifest(
      "run-1",
      "execplan",
      planned.publication_id,
      series_id,
      planned.required,
      "failed-preplan",
      1020,
      error,
    )
  let #(failed_sha, failed_bytes) =
    write_publication_manifest(root, failed_ref, failed_manifest)
  append_publication_seed_records(root, [
    record.with_id(
      "publication-preplan-failed",
      1020,
      record.PublicationAttemptRecorded(
        run_id: "run-1",
        workflow_id: "execplan",
        publication_id: planned.publication_id,
        series_id: series_id,
        attempt_id: "failed-preplan",
        status: "failed",
        required: planned.required,
        retryable: True,
        retry_execution_available: False,
        version_id: None,
        manifest_ref: Some(failed_ref),
        manifest_sha256: Some(failed_sha),
        manifest_bytes: Some(failed_bytes),
        error_code: Some(error.code),
        error_message: Some(error.message),
      ),
    ),
  ])
}

fn seed_failed_retry_publication_state_without_output_manifest(
  root: String,
) -> Nil {
  let planned = seeded_publication_plan(root)
  let failed_ref = "runs/run-1/publications/execplan_review_doc/failed-1.json"
  let failed_manifest =
    artifact_publication_manifest.failed_from_planned_manifest(
      planned,
      "failed-1",
      1020,
      True,
      Some(planned.branch),
      None,
      None,
      ["docs/plans/LIV-739.md"],
      [],
      artifact_publication_manifest.PublicationErrorInfo(
        code: "git_push_failed",
        message: "previous push failed",
      ),
    )
  let #(failed_sha, failed_bytes) =
    write_publication_manifest(root, failed_ref, failed_manifest)
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.with_id(
          "workflow-started",
          1000,
          record.WorkflowRunStarted(
            run_id: "run-1",
            workflow_id: "execplan",
            workflow_fingerprint: "wf-1",
            issue_id: "issue-1",
            issue_identifier: "LIV-739",
            issue_fingerprint: "issue-fingerprint",
            observed_updated_at_ms: 999,
            run_root: root <> "/runs/run-1",
          ),
        ),
        record.with_id(
          "publication-failed",
          1020,
          record.PublicationAttemptRecorded(
            run_id: "run-1",
            workflow_id: "execplan",
            publication_id: "execplan_review_doc",
            series_id: planned.series_id,
            attempt_id: "failed-1",
            status: "failed",
            required: True,
            retryable: True,
            retry_execution_available: True,
            version_id: Some(planned.version_id),
            manifest_ref: Some(failed_ref),
            manifest_sha256: Some(failed_sha),
            manifest_bytes: Some(failed_bytes),
            error_code: Some("git_push_failed"),
            error_message: Some("previous push failed"),
          ),
        ),
      ],
      True,
    )
  Nil
}

fn write_seeded_publication_manifest(
  root: String,
  planned: artifact_publication_planner.DryRunPublicationManifest,
  attempt_id: String,
  at_ms: Int,
  status: String,
) -> #(String, String, Int) {
  let manifest = case status {
    "published" ->
      artifact_publication_manifest.published_manifest(
        planned,
        attempt_id,
        at_ms,
        "deadbeef",
        Some("https://example.test/pr/" <> planned.publication_id),
        ["docs/plans/LIV-739.md"],
        [],
      )
    "failed" ->
      artifact_publication_manifest.failed_from_planned_manifest(
        planned,
        attempt_id,
        at_ms,
        True,
        Some(planned.branch),
        None,
        None,
        ["docs/plans/LIV-739.md"],
        [],
        artifact_publication_manifest.PublicationErrorInfo(
          code: "git_push_failed",
          message: "previous push failed",
        ),
      )
    _ ->
      artifact_publication_manifest.planned_manifest(planned, attempt_id, at_ms)
  }
  let ref =
    "runs/run-1/publications/"
    <> planned.publication_id
    <> "/"
    <> attempt_id
    <> ".json"
  let #(sha, bytes) = write_publication_manifest(root, ref, manifest)
  #(ref, sha, bytes)
}

fn append_publication_seed_records(
  root: String,
  attempts: List(record.LedgerRecord),
) -> Nil {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      list.append(
        [
          record.with_id(
            "workflow-started",
            1000,
            record.WorkflowRunStarted(
              run_id: "run-1",
              workflow_id: "execplan",
              workflow_fingerprint: "wf-1",
              issue_id: "issue-1",
              issue_identifier: "LIV-739",
              issue_fingerprint: "issue-fingerprint",
              observed_updated_at_ms: 999,
              run_root: root <> "/runs/run-1",
            ),
          ),
          seeded_output_manifest_record(root),
        ],
        attempts,
      ),
      True,
    )
  Nil
}

fn publication_attempt_record(
  planned: artifact_publication_planner.DryRunPublicationManifest,
  attempt_id: String,
  at_ms: Int,
  status: String,
  retryable: Bool,
  retry_execution_available: Bool,
  manifest_ref: Option(String),
  manifest_sha256: Option(String),
  manifest_bytes: Option(Int),
) -> record.LedgerRecord {
  let #(error_code, error_message) = case status {
    "failed" -> #(Some("git_push_failed"), Some("previous push failed"))
    _ -> #(None, None)
  }
  record.with_id(
    "publication-" <> planned.publication_id <> "-" <> attempt_id,
    at_ms,
    record.PublicationAttemptRecorded(
      run_id: "run-1",
      workflow_id: "execplan",
      publication_id: planned.publication_id,
      series_id: planned.series_id,
      attempt_id: attempt_id,
      status: status,
      required: planned.required,
      retryable: retryable,
      retry_execution_available: retry_execution_available,
      version_id: Some(planned.version_id),
      manifest_ref: manifest_ref,
      manifest_sha256: manifest_sha256,
      manifest_bytes: manifest_bytes,
      error_code: error_code,
      error_message: error_message,
    ),
  )
}

fn seeded_publication_plan(
  root: String,
) -> artifact_publication_planner.DryRunPublicationManifest {
  let config_path = write_retry_publication_config(root)
  let assert [planned] = seeded_publication_plans_from_config(root, config_path)
  planned
}

fn seeded_publication_plans_from_config(
  root: String,
  config_path: String,
) -> List(artifact_publication_planner.DryRunPublicationManifest) {
  write_seed_artifact(root, "runs/run-1/outputs/review_doc.md", "# Review\n")
  let output_manifest = seeded_output_manifest()
  let assert Ok(bundle) = runtime_bundle.load(Some(config_path))
  let assert Ok(#(_, workflow)) =
    runtime_bundle.workflow_by_id(bundle, "execplan")
  let assert Ok(body_templates) =
    artifact_publication_recording.load_body_templates(
      workflow_dag.publication_routes(workflow),
      bundle.orchestrator.artifact_repositories,
      bundle.orchestrator.config_dir,
      runtime_bundle.workflow_bundle_dir(bundle, workflow_dag.id(workflow)),
    )
  workflow_dag.publication_routes(workflow)
  |> list.map(fn(route) {
    let assert Ok(planned) =
      artifact_publication_planner.plan_publication(
        output_manifest,
        bundle.orchestrator.artifact_repositories,
        route,
        artifact_store.new(root),
        artifact_publication_planner.PublicationWork(
          kind: artifact_publication_planner.TaskWork,
          id: "issue-1",
          identifier: "LIV-739",
          slug: "LIV-739",
          title: Some("Seeded publication"),
          url: Some("https://linear.example/LIV-739"),
        ),
        "run-1",
        body_templates,
      )
    planned
  })
}

fn seeded_output_manifest() -> workflow_contract_manifest.ContractOutputManifest {
  let body = "# Review\n"
  let written =
    workflow_contract_manifest.ArtifactWritten(
      ref: "runs/run-1/outputs/review_doc.md",
      sha256: hash.sha256_hex(body),
      bytes: 9,
    )
  workflow_contract_manifest.ContractOutputManifest(
    run_id: "run-1",
    workflow_id: "execplan",
    workflow_fingerprint: "wf-1",
    outputs: [
      workflow_contract_manifest.NamedManifestValue(
        name: "review_doc",
        value: workflow_contract_manifest.present_run_artifact(
          workflow_contract.DocumentMarkdown,
          written,
          "text/markdown",
          None,
        ),
      ),
    ],
    diagnostics: [],
  )
}

fn seeded_output_manifest_record(root: String) -> record.LedgerRecord {
  let payload =
    seeded_output_manifest()
    |> workflow_contract_manifest.output_manifest_to_string
  let ref = "runs/run-1/contract/outputs.json"
  write_seed_artifact(root, ref, payload)
  record.with_id(
    "workflow-outputs-recorded",
    1015,
    record.WorkflowRunOutputsRecorded(
      run_id: "run-1",
      workflow_id: "execplan",
      workflow_fingerprint: "wf-1",
      artifact_ref: ref,
      artifact_sha256: hash.sha256_hex(payload),
      artifact_bytes: bit_array.byte_size(bit_array.from_string(payload)),
    ),
  )
}

fn seeded_commit_stack_output_manifest(
  root: String,
) -> workflow_contract_manifest.ContractOutputManifest {
  let stack_written =
    write_seed_contract_artifact(
      root,
      "runs/run-1/outputs/commit-stack.json",
      commit_stack_payload(),
    )
  let target_written =
    write_seed_contract_artifact(
      root,
      "runs/run-1/outputs/merge-target.json",
      existing_pr_branch_target_payload(),
    )
  workflow_contract_manifest.ContractOutputManifest(
    run_id: "run-1",
    workflow_id: "implementation",
    workflow_fingerprint: "wf-1",
    outputs: [
      workflow_contract_manifest.NamedManifestValue(
        name: "commit_stack",
        value: workflow_contract_manifest.present_run_artifact(
          workflow_contract.CommitStack,
          stack_written,
          commit_stack_artifact.commit_stack_media_type,
          None,
        ),
      ),
      workflow_contract_manifest.NamedManifestValue(
        name: "merge_target",
        value: workflow_contract_manifest.present_run_artifact(
          workflow_contract.GenericFile,
          target_written,
          "application/json",
          None,
        ),
      ),
    ],
    diagnostics: [],
  )
}

fn commit_stack_output_manifest_record(
  root: String,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
) -> record.LedgerRecord {
  let payload =
    output_manifest
    |> workflow_contract_manifest.output_manifest_to_string
  let ref = "runs/run-1/contract/outputs.json"
  write_seed_artifact(root, ref, payload)
  record.with_id(
    "workflow-outputs-recorded",
    1015,
    record.WorkflowRunOutputsRecorded(
      run_id: "run-1",
      workflow_id: "implementation",
      workflow_fingerprint: "wf-1",
      artifact_ref: ref,
      artifact_sha256: hash.sha256_hex(payload),
      artifact_bytes: bit_array.byte_size(bit_array.from_string(payload)),
    ),
  )
}

fn write_seed_contract_artifact(
  root: String,
  ref: String,
  contents: String,
) -> workflow_contract_manifest.ArtifactWritten {
  write_seed_artifact(root, ref, contents)
  workflow_contract_manifest.ArtifactWritten(
    ref: ref,
    sha256: hash.sha256_hex(contents),
    bytes: bit_array.byte_size(bit_array.from_string(contents)),
  )
}

fn commit_stack_payload() -> String {
  "{\"artifact_type\":\"scherzo.git_commit_stack.v1\",\"repository\":\"scherzo-systems/scherzo\",\"base\":{\"ref\":\"main\",\"sha\":\""
  <> retry_commit_stack_base_sha()
  <> "\"},\"head\":{\"sha\":\""
  <> retry_commit_stack_head_sha()
  <> "\",\"tree\":\""
  <> retry_commit_stack_tree_sha()
  <> "\"},\"carrier\":{\"ref\":\"runs/run-1/outputs/commit-stack.bundle\",\"sha256\":\""
  <> hash.sha256_hex("bundle")
  <> "\",\"bytes\":6,\"media_type\":\"application/vnd.git.bundle\"}}"
}

fn existing_pr_branch_target_payload() -> String {
  "{\"artifact_type\":\"scherzo.github_existing_pr_branch_target.v1\",\"repository\":\"scherzo-systems/scherzo\",\"head\":{\"repo\":\"scherzo-systems/scherzo\",\"branch\":\"scherzo/implementation/LIV-917\",\"sha\":\""
  <> retry_commit_stack_base_sha()
  <> "\"},\"base\":{\"branch\":\"main\",\"sha\":\""
  <> retry_commit_stack_base_sha()
  <> "\"},\"pull_request\":{\"number\":42,\"url\":\"https://example.test/pr/42\"}}"
}

fn write_commit_stack_retry_publication_config(root: String) -> String {
  let assert Ok(base) = path.dirname(root)
  let workflow_dir = base <> "/workflows"
  let script_dir = base <> "/scripts"
  let config_path = base <> "/scherzo.yaml"
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_dir)
  let assert Ok(Nil) = simplifile.create_directory_all(script_dir)
  let driver_path = script_dir <> "/retained-driver"
  let assert Ok(Nil) =
    simplifile.write(
      driver_path,
      "#!/bin/sh\nif [ \"$1\" = describe ] && [ \"$2\" = --json ]; then\n  printf '%s\\n' '{\"version\":1,\"capabilities\":[\"publish-commit-stack\"]}'\n  exit 0\nfi\nexit 1\n",
    )
  test_helpers.chmod_executable(driver_path)
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    commit_stack:\n      type: commit_stack\n      source:\n        step: main\n        field: stdout\n    merge_target:\n      type: code_change\n      source:\n        step: main\n        field: stdout\nartifacts:\n  publications:\n    - id: publish_stack\n      repository: github.code\n      required: true\n      mode: commit_stack\n      commit_stack:\n        select:\n          output: commit_stack\n      target:\n        kind: existing_pr_branch\n        source:\n          output: merge_target\nsteps:\n  - id: main\n    kind: command\n    run: ignored\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      "version: 1\ntracker:\n  linear:\n    api_key_env: HOME\n    project: TEST\n  states:\n    ready: [Todo]\n    active: [Todo]\n    terminal: [Done]\nworkspace:\n  root: "
        <> root
        <> "\n  driver: retained\n  drivers:\n    retained:\n      type: custom\n      command: scripts/retained-driver\n      timeout: 1234ms\nagents:\n  concurrency: 1\n  sessions_per_task: 1\n  runtime:\n    type: pi\n    pi:\n      executable: fake\ntask_routing:\n  labels:\n    require_exactly_one: false\n    default_workflow: implementation\nartifacts:\n  repositories:\n    github:\n      code:\n        repo: scherzo-systems/scherzo\n        base: main\nworkflows:\n  implementation: workflows/implementation.yaml\n",
    )
  config_path
}

fn write_commit_stack_retained_workspace_manifest(root: String) -> Nil {
  let run_root = root <> "/runs/run-1"
  let workspace = run_root <> "/workspaces/main"
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  let assert Ok(Nil) = simplifile.create_directory_all(run_root <> "/.scherzo")
  let assert Ok(Nil) =
    simplifile.write(
      workspace_manifest.manifest_path(run_root),
      workspace_manifest.encode_manifest(
        [
          workspace_manifest.Entry(
            run_id: "run-1",
            workflow_id: "implementation",
            step_id: "main",
            attempt_index: 1,
            workspace_name: "main",
            relative_path: "workspaces/main",
            workspace_profile: "retained",
            driver_command: "retained-driver",
            driver_capabilities: ["publish-commit-stack"],
            source: workspace.FreshWorkspace,
            state: workspace_manifest.Ready,
          ),
        ],
        "run-1",
        "implementation",
      ),
    )
  Nil
}

fn write_retry_publication_config(root: String) -> String {
  write_retry_publication_config_with_body_template(
    root,
    "templates/publication.md",
  )
}

fn write_retry_publication_config_with_body_template(
  root: String,
  body_template: String,
) -> String {
  write_retry_publication_config_with_workflow(
    root,
    "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    review_doc:\n      type: document.markdown\n      source:\n        step: materialize\n        path: tmp/review_doc.md\nartifacts:\n  publications:\n    - id: execplan_review_doc\n      repository: github.docs\n      required: true\n      pull_request:\n        title: '{{ work.identifier }} publication'\n        body_template: "
      <> body_template
      <> "\n      files:\n        - select:\n            output: review_doc\n          path: docs/plans/{{ work.identifier }}{{ artifact.default_extension }}\nsteps:\n  - id: materialize\n    kind: command\n    run: ignored\n",
  )
}

fn write_retry_publication_config_with_workflow(
  root: String,
  workflow_yaml: String,
) -> String {
  let assert Ok(base) = path.dirname(root)
  let workflow_dir = base <> "/workflows"
  let workflow_template_dir = workflow_dir <> "/templates"
  let config_path = base <> "/scherzo.yaml"
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_dir)
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_template_dir)
  let assert Ok(Nil) =
    simplifile.write(workflow_dir <> "/execplan.yaml", workflow_yaml)
  let assert Ok(Nil) =
    simplifile.write(
      workflow_template_dir <> "/publication.md",
      "Published by Scherzo.",
    )
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      "version: 1\ntracker:\n  linear:\n    api_key_env: HOME\n    project: TEST\n  states:\n    ready: [Todo]\n    active: [Todo]\n    terminal: [Done]\nworkspace:\n  root: "
        <> root
        <> "\nagents:\n  concurrency: 1\n  sessions_per_task: 1\n  runtime:\n    type: pi\n    pi:\n      executable: fake\ntask_routing:\n  labels:\n    require_exactly_one: false\n    default_workflow: execplan\nartifacts:\n  repositories:\n    github:\n      docs:\n        repo: scherzo-systems/scherzo\n        base: main\n        branch:\n          strategy: stable_per_work\n          template: scherzo/workflow.{{ workflow.id }}/{{ work.identifier }}/{{ publication.id }}\n        pull_request:\n          enabled: true\n          strategy: update_existing\n          draft: true\n          title: '{{ work.identifier }} publication'\n          body_template: templates/publication.md\nworkflows:\n  execplan: workflows/execplan.yaml\n",
    )
  config_path
}

fn write_publication_manifest(
  root: String,
  ref: String,
  manifest: artifact_publication_manifest.PublicationManifest,
) -> #(String, Int) {
  let payload = artifact_publication_manifest.to_string(manifest)
  write_seed_artifact(root, ref, payload)
  #(
    hash.sha256_hex(payload),
    bit_array.byte_size(bit_array.from_string(payload)),
  )
}

fn write_seed_artifact(root: String, ref: String, contents: String) -> Nil {
  let absolute = root <> "/.scherzo-state/artifacts/" <> ref
  let assert Ok(dir) = path.dirname(absolute)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  let assert Ok(Nil) = simplifile.write(absolute, contents)
  Nil
}

fn read_file_or_panic(path: String) -> String {
  let assert Ok(contents) = simplifile.read(path)
  contents
}

fn retry_publish_runner(
  subject: process.Subject(OutMsg),
) -> command_runner.Runner {
  retry_runner(subject, False)
}

fn retained_workspace_publish_runner(
  subject: process.Subject(OutMsg),
) -> command_runner.Runner {
  command_runner.Runner(run: fn(spec) {
    process.send(subject, OutLine(command_runner.describe(spec)))
    let command_runner.CommandSpec(args: args, ..) = spec
    case args {
      ["publish-commit-stack", ..] ->
        Ok(command_runner.CommandOutput(0, retained_driver_success_json(), ""))
      _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn retained_driver_success_json() -> String {
  "{\"version\":1,\"status\":\"published\",\"branch\":\"scherzo/implementation/LIV-917\",\"base_ref\":\"main\",\"base_revision\":\""
  <> retry_commit_stack_base_sha()
  <> "\",\"head_revision\":\""
  <> retry_commit_stack_head_sha()
  <> "\",\"created\":false,\"updated\":true,\"url\":\"https://example.test/pr/42\",\"change_id\":\"42\"}"
}

fn retry_commit_stack_base_sha() -> String {
  "1111111111111111111111111111111111111111"
}

fn retry_commit_stack_head_sha() -> String {
  "2222222222222222222222222222222222222222"
}

fn retry_commit_stack_tree_sha() -> String {
  "3333333333333333333333333333333333333333"
}

fn retry_runner(
  subject: process.Subject(OutMsg),
  fail_push: Bool,
) -> command_runner.Runner {
  command_runner.Runner(run: fn(spec) {
    process.send(subject, OutLine(command_runner.describe(spec)))
    let command_runner.CommandSpec(
      executable: executable,
      args: args,
      cwd: cwd,
      ..,
    ) = spec
    let _ = simplifile.create_directory_all(cwd)
    case executable, args {
      "git", ["clone", _, target] -> {
        let _ = simplifile.create_directory_all(target)
        Ok(command_runner.CommandOutput(0, "", ""))
      }
      "git", ["fetch", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["ls-remote", ..] -> Ok(command_runner.CommandOutput(2, "", ""))
      "git", ["rev-parse", "--verify", ..] ->
        Ok(command_runner.CommandOutput(1, "", ""))
      "git", ["checkout", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["status", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["add", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["diff", ..] -> Ok(command_runner.CommandOutput(1, "", ""))
      "git", ["commit", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["rev-parse", "HEAD"] ->
        Ok(command_runner.CommandOutput(0, "deadbeef", ""))
      "git", ["push", ..] ->
        case fail_push {
          True -> Ok(command_runner.CommandOutput(1, "", "push rejected"))
          False -> Ok(command_runner.CommandOutput(0, "", ""))
        }
      "gh", ["pr", "list", ..] -> Ok(command_runner.CommandOutput(0, "[]", ""))
      "gh", ["pr", "create", ..] ->
        Ok(command_runner.CommandOutput(0, "https://example.test/pr/1", ""))
      _, _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn turn_deps(summary: event.SessionSummary) -> ctl.ControlClient {
  ctl.ControlClient(
    list_sessions: fn(_) {
      Ok(event.SessionList(sessions: [summary], now_ms: ps_now_ms))
    },
    get_session: fn(_, session_id) {
      case session_id == summary.session_id {
        True -> Ok(Some(summary))
        False -> Ok(None)
      }
    },
    get_events: fn(_, _, cursor, _) {
      case cursor {
        0 ->
          Ok(event.EventPage(
            events: [turn_finished_event(summary.session_id)],
            next_cursor: 1,
            truncated: False,
          ))
        _ ->
          Ok(event.EventPage(events: [], next_cursor: cursor, truncated: False))
      }
    },
    stream_events: fn(_, _, _, _) { Ok(Nil) },
    query: fn(_, _) {
      Ok(
        query_types.StatusResponse(
          query_types.StatusDto(
            daemon_id: "daemon-1",
            boot_id: "boot-1",
            dispatch_paused: False,
            ui_server_enabled: False,
            supported_queries: ["status"],
          ),
        ),
      )
    },
    apply_command: fn(_, operator_command) {
      Ok(command.applied(operator_command, None))
    },
    raw_request: fn(_, request) { Ok(protocol.request_to_string(request)) },
  )
}

fn turn_finished_event(session_id: String) -> event.SessionEvent {
  event.SessionEvent(
    cursor: 1,
    at_ms: 10,
    session_id: session_id,
    issue_id: "issue-1",
    payload: event.turn_payload(
      turn_telemetry.EventFinished,
      3,
      session_tokens.zero_token_totals(),
      None,
    )
      |> event.with_turn_terminal_details(
        3,
        Some(turn_telemetry.StatusFinished),
        10,
        Some(1500),
        session_tokens.TokenTotals(
          input: 10,
          output: 5,
          cache_read: 0,
          cache_write: 0,
          total: 15,
        ),
      ),
  )
}

fn session_ref_deps(sessions: List(event.SessionSummary)) -> ctl.ControlClient {
  ctl.ControlClient(
    list_sessions: fn(_) {
      Ok(event.SessionList(sessions: sessions, now_ms: ps_now_ms))
    },
    get_session: fn(_, session_id) {
      Ok(summary_by_session_id(sessions, session_id))
    },
    get_events: fn(_, session_id, cursor, _) {
      case summary_by_session_id(sessions, session_id), cursor {
        Some(_), 0 ->
          Ok(event.EventPage(
            events: [replay_event(session_id)],
            next_cursor: 1,
            truncated: False,
          ))
        _, _ ->
          Ok(event.EventPage(events: [], next_cursor: cursor, truncated: False))
      }
    },
    stream_events: fn(_, _, _, _) { Ok(Nil) },
    query: fn(_, _) {
      Ok(
        query_types.StatusResponse(
          query_types.StatusDto(
            daemon_id: "daemon-1",
            boot_id: "boot-1",
            dispatch_paused: False,
            ui_server_enabled: False,
            supported_queries: ["status"],
          ),
        ),
      )
    },
    apply_command: fn(_, operator_command) {
      Ok(command.applied(operator_command, None))
    },
    raw_request: fn(_, request) { Ok(protocol.request_to_string(request)) },
  )
}

fn summary_by_session_id(
  sessions: List(event.SessionSummary),
  session_id: String,
) -> Option(event.SessionSummary) {
  case list.filter(sessions, fn(summary) { summary.session_id == session_id }) {
    [summary, ..] -> Some(summary)
    [] -> None
  }
}

fn replay_event(session_id: String) -> event.SessionEvent {
  event.SessionEvent(
    cursor: 1,
    at_ms: 10,
    session_id: session_id,
    issue_id: "issue-1",
    payload: event.lifecycle_payload(event.WorkerStarted, None, None),
  )
}

fn seed_missing_provenance_state(root: String) -> Nil {
  let run_root = root <> "/runs/run-1"
  let assert Ok(Nil) =
    simplifile.create_directory_all(run_root <> "/workspaces/main")
  seed_missing_provenance_state_with(
    root,
    run_root,
    "implementation",
    "wf-1",
    True,
  )
}

fn seed_missing_provenance_state_with(
  root: String,
  run_root: String,
  attempt_workflow_id: String,
  workflow_fingerprint: String,
  include_known_workspace: Bool,
) -> Nil {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let records =
    list.append(
      [
        record.with_id(
          "inputs",
          1,
          record.WorkflowRunInputsRecorded(
            run_id: "run-1",
            workflow_id: "implementation",
            workflow_fingerprint: workflow_fingerprint,
            artifact_ref: "runs/run-1/inputs.json",
            artifact_sha256: "sha-inputs",
            artifact_bytes: 10,
          ),
        ),
      ],
      list.append(known_workspace_records(root, include_known_workspace), [
        record.with_id(
          "prepared",
          3,
          record.StepAttemptPrepared(
            run_id: "run-1",
            workflow_id: attempt_workflow_id,
            step_id: "apply_feedback",
            attempt_index: 1,
            workspace_name: "main",
            workspace_path: run_root <> "/workspaces/main",
            run_root: run_root,
            source_workspace_name: None,
            source_workspace_path: None,
          ),
        ),
        record.with_id(
          "interrupted-attempt",
          4,
          record.StepAttemptInterrupted(
            run_id: "run-1",
            workflow_id: attempt_workflow_id,
            step_id: "apply_feedback",
            attempt_index: 1,
            reason: "daemon_shutdown",
          ),
        ),
        record.with_id(
          "interrupted-run",
          5,
          record.WorkflowRunInterrupted(
            run_id: "run-1",
            workflow_id: "implementation",
            issue_id: "issue-1",
            reason: "daemon_shutdown",
          ),
        ),
      ]),
    )
  let assert Ok(Nil) = simplifile.create_directory_all(ledger_path.ledger_dir)
  let assert Ok(Nil) =
    simplifile.write(
      ledger_path.current_path,
      records
        |> list.map(record.to_string)
        |> string.join(with: "\n")
        |> fn(contents) { contents <> "\n" },
    )
  Nil
}

fn known_workspace_records(
  root: String,
  include_known_workspace: Bool,
) -> List(record.LedgerRecord) {
  case include_known_workspace {
    True -> [
      record.with_id(
        "known-workspace",
        2,
        record.KnownWorkspace(
          issue_id: "issue-1",
          issue_identifier: "LIV-695",
          workspace_path: root <> "/LIV-695",
        ),
      ),
    ]
    False -> []
  }
}

fn has_provenance_repair_record(root: String) -> Bool {
  provenance_repair_record_count(root) > 0
}

fn provenance_repair_record_count(root: String) -> Int {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(read) = ledger.read_records(ledger_path)
  read.records
  |> list.filter(fn(ledger_record) {
    case ledger_record.body {
      record.WorkflowRunProvenanceRepaired(..) -> True
      _ -> False
    }
  })
  |> list.length
}

fn write_workflow_recovery_history(root: String) -> Nil {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.new(
          900,
          1,
          record.WorkflowRunStarted(
            run_id: "run-1",
            workflow_id: "implementation",
            workflow_fingerprint: "wf-1",
            issue_id: "issue-1",
            issue_identifier: "LIV-490",
            issue_fingerprint: "issue-fp-1",
            observed_updated_at_ms: 800,
            run_root: root,
          ),
        ),
        record.new(
          1000,
          2,
          record.StepAttemptStarted(
            run_id: "run-1",
            workflow_id: "implementation",
            step_id: "implement",
            attempt_index: 1,
            operator_session_id: "session-1",
            external_session_ref: None,
            continuation_capable: True,
          ),
        ),
        record.new(
          1010,
          3,
          record.WorkflowStepRecoveryStarted(
            run_id: "run-1",
            workflow_id: "implementation",
            step_id: "implement",
            failed_attempt_index: 1,
            recovery_attempt_number: 1,
            recovery_session_id: "recover-1",
            model: Some("gpt-5"),
            prompt_ref: ".scherzo/workflows/prompts/recover_failed_step.md",
          ),
        ),
        record.new(
          1020,
          4,
          record.WorkflowStepRecoveryFinished(
            run_id: "run-1",
            workflow_id: "implementation",
            step_id: "implement",
            failed_attempt_index: 1,
            recovery_attempt_number: 1,
            recovery_session_id: "recover-1",
            result: "recheck",
            summary: "Fixed tests",
            reason: "Ready for recheck",
            retry_attempt_index: Some(2),
          ),
        ),
        record.new(
          1030,
          5,
          record.StepAttemptStarted(
            run_id: "run-1",
            workflow_id: "implementation",
            step_id: "implement",
            attempt_index: 2,
            operator_session_id: "session-2",
            external_session_ref: None,
            continuation_capable: True,
          ),
        ),
        record.new(
          1031,
          6,
          record.StepAttemptContinuationStarted(
            run_id: "run-1",
            workflow_id: "implementation",
            step_id: "implement",
            attempt_index: 2,
            session_id: "continue-2",
          ),
        ),
        record.new(
          1040,
          7,
          record.StepAttemptFinished(
            run_id: "run-1",
            workflow_id: "implementation",
            step_id: "implement",
            attempt_index: 2,
            outcome: "succeeded",
            artifact_ref: "runs/run-1/implement/attempt-2.json",
            artifact_sha256: "sha-2",
            workspace_name: "main",
            workspace_path: root <> "/main",
            token_total: 0,
            turns: 0,
          ),
        ),
        record.new(
          1050,
          8,
          record.WorkflowRunFinished(
            run_id: "run-1",
            workflow_id: "implementation",
            issue_id: "issue-1",
            outcome: "succeeded_after_recovery",
            token_total: 0,
            turns: 0,
          ),
        ),
      ],
      True,
    )
  Nil
}

fn write_scheduled_history(root: String, session_id: String) -> Nil {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.new(
          100,
          1,
          record.ScheduledJobDue(
            "nightly",
            "nightly",
            1_746_447_200_000,
            "schedule-nightly-20260505T120000Z",
            "automatic",
          ),
        ),
        record.new(
          101,
          2,
          record.ScheduledRunPending(
            "nightly",
            "nightly",
            1_746_447_200_000,
            "schedule-nightly-20260505T120000Z",
            "automatic",
            101,
          ),
        ),
        record.new(
          102,
          3,
          record.ScheduledRunStarted(
            "nightly",
            "nightly",
            1_746_447_200_000,
            102,
            "schedule-nightly-20260505T120000Z",
            1,
            session_id,
            root
              <> "/nightly/scheduled/nightly/schedule-nightly-20260505T120000Z",
          ),
        ),
        record.new(
          103,
          4,
          record.ScheduledRunSucceeded(
            "nightly",
            "nightly",
            1_746_447_200_000,
            "schedule-nightly-20260505T120000Z",
            1,
            103,
            0,
            0,
          ),
        ),
      ],
      True,
    )
  Nil
}

fn write_schedule_doctor_config(dir: String, prompt: String) -> String {
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/workflows/prompts")
  let config_path = dir <> "/scherzo.yaml"
  let assert Ok(driver_command) =
    path.absolute("scripts/scherzo-workspace-noop")
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      "version: 1\ntracker:\n  linear:\n    api_key_env: HOME\n    project: TEST\n  states:\n    ready: [Todo]\n    active: [Todo]\n    terminal: [Done]\nworkspace:\n  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: custom\n      command: "
        <> driver_command
        <> "\n      timeout: 60s\nworkflows:\n    nightly: workflows/nightly.yaml\nagents:\n  concurrency: 1\n  max_turns: 1\nschedules:\n  - id: nightly\n    workflow: nightly\n    enabled: true\n    every: 15m\n    overlap: skip\n    catch_up: false\n    on_failure:\n      task:\n        enabled: true\n        state: Triage\n        labels:\n          - job:nightly\n        dedupe: open_task_per_schedule\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/nightly.yaml",
      "version: 1\nid: nightly\nsteps:\n  - id: inspect\n    kind: agent\n    prompt: prompts/nightly.md\n    run_in: main\n",
    )
  let assert Ok(Nil) =
    simplifile.write(dir <> "/workflows/prompts/nightly.md", prompt)
  config_path
}
