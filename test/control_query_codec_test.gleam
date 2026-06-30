import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/control/query/codec
import scherzo/control/query/cursor
import scherzo/control/query/dto
import scherzo/control/query/types
import scherzo/session/tokens as session_tokens
import scherzo/task
import scherzo/work_item
import scherzo/work_item/action

pub fn status_query_request_roundtrip_test() {
  let encoded = codec.request_to_string(types.Status)

  assert string.contains(encoded, "\"type\":\"status\"")
  let assert Ok(decoded) = codec.decode_request(encoded)
  assert decoded == types.Status
}

pub fn status_query_response_roundtrip_test() {
  let response =
    types.default_status_source(daemon_id: "daemon-1", boot_id: "boot-1")
    |> dto.status_from_source
    |> types.StatusResponse

  let encoded = codec.response_to_string(response)

  assert string.contains(encoded, "\"ok\":true")
  let assert Ok(decoded) = codec.decode_response(encoded)
  assert decoded == response
}

pub fn metrics_query_request_roundtrip_test() {
  let encoded = codec.request_to_string(types.Metrics)

  assert string.contains(encoded, "\"type\":\"metrics\"")
  let assert Ok(decoded) = codec.decode_request(encoded)
  assert decoded == types.Metrics
}

pub fn metrics_query_response_roundtrip_test() {
  let response =
    types.default_operational_metrics_source(
      daemon_id: "daemon-1",
      boot_id: "boot-1",
    )
    |> dto.operational_metrics_from_source
    |> types.MetricsResponse

  let encoded = codec.response_to_string(response)

  assert string.contains(encoded, "\"ok\":true")
  assert string.contains(encoded, "\"type\":\"metrics\"")
  assert string.contains(encoded, "\"schema_version\":1")
  assert string.contains(encoded, "\"lifecycle_projection_failed\":false")
  let assert Ok(decoded) = codec.decode_response(encoded)
  assert decoded == response
}

pub fn metrics_query_response_rejects_unknown_schema_version_test() {
  let response =
    types.default_operational_metrics_source(
      daemon_id: "daemon-1",
      boot_id: "boot-1",
    )
    |> dto.operational_metrics_from_source
    |> types.MetricsResponse
  let encoded =
    codec.response_to_string(response)
    |> string.replace(
      each: "\"schema_version\":1",
      with: "\"schema_version\":2",
    )

  let assert Error(types.QueryError(code: code, message: message)) =
    codec.decode_response(encoded)
  assert code == types.QueryBackendFailed
  assert message == "unsupported metrics schema version"
}

pub fn operation_status_query_request_response_roundtrip_test() {
  let request =
    types.OperationStatus(types.OperationStatusQuery(operation_id: "op-123"))
  let response =
    types.OperationStatusResponse(types.OperationStatusDto(
      operation_id: "op-123",
      kind: "retry_step",
      command: "retry_step",
      target: "run:run-1",
      run_id: Some("run-1"),
      issue_id: Some("issue-1"),
      issue_identifier: Some("LIV-1"),
      requested_step_id: Some("apply_feedback"),
      publication_id: Some("execplan_review_doc"),
      status: "completed",
      reason: None,
      message: Some("retry-step completed"),
      queued_at_ms: 1000,
      started_at_ms: Some(1001),
      finished_at_ms: Some(1002),
    ))

  let encoded_request = codec.request_to_string(request)
  assert string.contains(encoded_request, "\"type\":\"operation_status\"")
  assert string.contains(encoded_request, "\"operation_id\":\"op-123\"")
  assert codec.decode_request(encoded_request) == Ok(request)

  let encoded_response = codec.response_to_string(response)
  assert string.contains(encoded_response, "\"type\":\"operation_status\"")
  assert string.contains(encoded_response, "\"status\":\"completed\"")
  assert string.contains(encoded_response, "\"operation_id\":\"op-123\"")
  assert string.contains(
    encoded_response,
    "\"publication_id\":\"execplan_review_doc\"",
  )
  assert codec.decode_response(encoded_response) == Ok(response)
}

pub fn shared_query_codec_decodes_nested_local_and_remote_payloads_test() {
  let request = codec.request_to_string(types.Status)
  let local_line = "{\"query\":" <> request <> "}"
  let remote_line = "{\"payload\":" <> request <> "}"

  let assert Ok(local_dynamic) = nested_field(local_line, "query")
  let assert Ok(remote_dynamic) = nested_field(remote_line, "payload")

  assert codec.decode_request_dynamic(local_dynamic) == Ok(types.Status)
  assert codec.decode_request_dynamic(remote_dynamic) == Ok(types.Status)

  let metrics_request = codec.request_to_string(types.Metrics)
  let metrics_line = "{\"payload\":" <> metrics_request <> "}"
  let assert Ok(metrics_dynamic) = nested_field(metrics_line, "payload")
  assert codec.decode_request_dynamic(metrics_dynamic) == Ok(types.Metrics)
}

pub fn outbox_query_request_response_roundtrip_redacts_payload_test() {
  let list_request =
    types.OutboxList(types.OutboxListQuery(
      statuses: [types.OutboxRetryableStatus, types.OutboxPermanentStatus],
      kinds: ["linear_comment"],
      limit: 25,
      cursor: Some("cursor:25"),
    ))
  let show_request =
    types.OutboxShow(types.OutboxShowQuery(outbox_id: "outbox-1"))

  assert codec.decode_request(codec.request_to_string(list_request))
    == Ok(list_request)
  assert codec.decode_request(codec.request_to_string(show_request))
    == Ok(show_request)

  let list_response =
    types.OutboxListResponse(types.OutboxListDto(
      items: [outbox_record()],
      page: types.PageDto(next_cursor: Some("cursor:1"), has_more: True),
    ))
  let show_response = types.OutboxShowResponse(outbox_record())

  let encoded_list = codec.response_to_string(list_response)
  assert string.contains(encoded_list, "\"type\":\"outbox_list\"")
  assert string.contains(encoded_list, "\"status\":\"retryable\"")
  assert string.contains(encoded_list, "\"has_payload\":true")
  assert !string.contains(encoded_list, "payload_json")
  assert !string.contains(encoded_list, "raw-secret")
  assert codec.decode_response(encoded_list) == Ok(list_response)

  let encoded_show = codec.response_to_string(show_response)
  assert string.contains(encoded_show, "\"outbox_record\"")
  assert codec.decode_response(encoded_show) == Ok(show_response)
}

pub fn task_query_request_response_roundtrip_test() {
  let list_request =
    types.TaskList(types.TaskListQuery(
      states: [task.Ready, task.Active],
      limit: 25,
      cursor: Some("cursor:25"),
    ))
  let show_request =
    types.TaskShow(
      types.TaskShowQuery(ref: types.TaskRemoteId(
        provider: Some("linear"),
        id: "issue-1",
      )),
    )

  assert codec.decode_request(codec.request_to_string(list_request))
    == Ok(list_request)
  assert codec.decode_request(codec.request_to_string(show_request))
    == Ok(show_request)

  let list_response =
    types.TaskListResponse(types.TaskListDto(
      items: [task_summary()],
      page: types.PageDto(next_cursor: Some("cursor:1"), has_more: True),
    ))
  let detail_response =
    types.TaskShowResponse(types.TaskDetailDto(
      summary: task_summary(),
      description: types.TaskDescriptionDto(
        format: "markdown",
        body: "task detail body",
      ),
    ))

  let encoded_list = codec.response_to_string(list_response)
  assert string.contains(encoded_list, "\"state\":\"ready\"")
  assert !string.contains(encoded_list, "Todo")
  assert codec.decode_response(encoded_list) == Ok(list_response)

  let encoded_detail = codec.response_to_string(detail_response)
  assert string.contains(encoded_detail, "\"description\"")
  assert codec.decode_response(encoded_detail) == Ok(detail_response)
}

pub fn work_item_query_request_response_roundtrip_test() {
  let list_request =
    types.WorkItemList(types.WorkItemListQuery(
      state_filter: work_item.CategoryWorkItems([task.Ready, task.Active]),
      search: Some("workflow"),
      sort: work_item.UpdatedDescWorkItems,
      limit: 25,
      cursor: Some(
        "work-item:25:categories:active,ready|search:workflow|updated_desc",
      ),
    ))
  let show_request =
    types.WorkItemShow(
      types.WorkItemShowQuery(ref: types.TaskRemoteId(
        provider: Some("linear"),
        id: "issue-1",
      )),
    )

  assert codec.decode_request(codec.request_to_string(list_request))
    == Ok(list_request)
  assert codec.decode_request(codec.request_to_string(show_request))
    == Ok(show_request)

  let list_response =
    types.WorkItemListResponse(work_item.WorkItemPage(
      items: [work_item_summary(labels_truncated: False)],
      next_cursor: Some("work-item:1:active|search:|updated_desc"),
      has_more: True,
    ))
  let detail_response =
    types.WorkItemShowResponse(work_item.WorkItemDetail(
      summary: work_item_summary(labels_truncated: True),
      subtasks: [work_item_summary(labels_truncated: False)],
      subtasks_truncated: False,
    ))

  let encoded_list = codec.response_to_string(list_response)
  assert string.contains(encoded_list, "\"type\":\"work_item_list\"")
  assert string.contains(
    encoded_list,
    "\"state\":{\"id\":\"todo\",\"name\":\"Todo\",\"category\":\"ready\"}",
  )
  assert string.contains(encoded_list, "\"labels_truncated\":false")
  assert string.contains(
    encoded_list,
    "\"action_id\":\"work_item.run_workflow\"",
  )
  assert codec.decode_response(encoded_list) == Ok(list_response)

  let encoded_detail = codec.response_to_string(detail_response)
  assert string.contains(encoded_detail, "\"type\":\"work_item_show\"")
  assert string.contains(encoded_detail, "\"subtasks_truncated\":false")
  assert string.contains(
    encoded_detail,
    "\"action_id\":\"work_item.run_workflow\"",
  )
  assert !string.contains(encoded_detail, "description")
  assert codec.decode_response(encoded_detail) == Ok(detail_response)
}

pub fn work_item_query_decodes_defaults_and_legacy_states_test() {
  let assert Ok(types.WorkItemList(active_default)) =
    codec.decode_request(
      "{\"version\":1,\"type\":\"work_item_list\",\"limit\":5,\"cursor\":null}",
    )
  assert active_default.state_filter == work_item.ActiveWorkItems
  assert active_default.search == None
  assert active_default.sort == work_item.UpdatedDescWorkItems

  let assert Ok(types.WorkItemList(legacy_states)) =
    codec.decode_request(
      "{\"version\":1,\"type\":\"work_item_list\",\"states\":[\"ready\",\"active\"],\"search\":\"   \",\"limit\":5,\"cursor\":null}",
    )
  assert legacy_states.state_filter
    == work_item.CategoryWorkItems([task.Ready, task.Active])
  assert legacy_states.search == None
  assert legacy_states.sort == work_item.UpdatedDescWorkItems
}

pub fn work_item_query_decodes_active_archive_and_categories_test() {
  let assert Ok(types.WorkItemList(active_query)) =
    codec.decode_request(
      "{\"version\":1,\"type\":\"work_item_list\",\"state_filter\":\"active\",\"search\":\"Workflow\",\"sort\":\"updated_desc\",\"limit\":5,\"cursor\":null}",
    )
  assert active_query.state_filter == work_item.ActiveWorkItems
  assert active_query.search == Some("Workflow")

  let assert Ok(types.WorkItemList(archive_query)) =
    codec.decode_request(
      "{\"version\":1,\"type\":\"work_item_list\",\"state_filter\":\"archive\",\"limit\":5,\"cursor\":null}",
    )
  assert archive_query.state_filter == work_item.ArchiveWorkItems

  let assert Ok(types.WorkItemList(category_query)) =
    codec.decode_request(
      "{\"version\":1,\"type\":\"work_item_list\",\"state_filter\":\"categories\",\"states\":[\"ready\",\"active\"],\"limit\":5,\"cursor\":null}",
    )
  assert category_query.state_filter
    == work_item.CategoryWorkItems([task.Ready, task.Active])
}

pub fn work_item_query_rejects_invalid_filter_sort_and_categories_test() {
  let assert Error(types.QueryError(code: code_1, message: message_1)) =
    codec.decode_request(
      "{\"version\":1,\"type\":\"work_item_list\",\"state_filter\":\"bogus\",\"limit\":5,\"cursor\":null}",
    )
  assert code_1 == types.QueryBackendFailed
  assert message_1 == "invalid work item state_filter: bogus"

  let assert Error(types.QueryError(code: code_2, message: message_2)) =
    codec.decode_request(
      "{\"version\":1,\"type\":\"work_item_list\",\"state_filter\":\"categories\",\"states\":[],\"limit\":5,\"cursor\":null}",
    )
  assert code_2 == types.QueryBackendFailed
  assert message_2
    == "work item state_filter categories requires non-empty states"

  let assert Error(types.QueryError(code: code_3, message: message_3)) =
    codec.decode_request(
      "{\"version\":1,\"type\":\"work_item_list\",\"state_filter\":\"categories\",\"states\":[\"bogus\"],\"limit\":5,\"cursor\":null}",
    )
  assert code_3 == types.QueryBackendFailed
  assert message_3 == "invalid state_filter categories: bogus"

  let assert Error(types.QueryError(code: code_4, message: message_4)) =
    codec.decode_request(
      "{\"version\":1,\"type\":\"work_item_list\",\"state_filter\":\"active\",\"sort\":\"bogus\",\"limit\":5,\"cursor\":null}",
    )
  assert code_4 == types.QueryBackendFailed
  assert message_4 == "invalid work item sort: bogus"
}

pub fn work_item_query_decoder_accepts_missing_parent_field_test() {
  let response =
    types.WorkItemShowResponse(work_item.WorkItemDetail(
      summary: work_item_summary(labels_truncated: False),
      subtasks: [work_item_summary(labels_truncated: False)],
      subtasks_truncated: False,
    ))
  let encoded =
    codec.response_to_string(response)
    |> string.replace(each: "\"parent\":null,", with: "")

  assert !string.contains(encoded, "\"parent\"")
  assert codec.decode_response(encoded) == Ok(response)
}

pub fn work_item_query_response_roundtrips_subtask_parent_test() {
  let parent = work_item_summary(labels_truncated: True)
  let child_source =
    work_item.WorkItemSource(
      provider: "linear",
      id: "issue-child-1",
      display_id: Some("LIV-771"),
      url: Some("https://linear.app/living-systems/issue/LIV-771"),
    )
  let child =
    work_item.WorkItemSummary(
      ..work_item_summary(labels_truncated: False),
      id: "linear:issue-child-1",
      source: child_source,
      parent: Some(parent.source),
      title: "Implement child work item",
    )
  let detail =
    work_item.WorkItemDetail(
      summary: parent,
      subtasks: [child],
      subtasks_truncated: False,
    )
  let response = types.WorkItemShowResponse(detail)
  let encoded = codec.response_to_string(response)

  assert string.contains(encoded, "\"parent\":{")
  let assert Ok(types.WorkItemShowResponse(decoded)) =
    codec.decode_response(encoded)
  assert decoded == detail
  let assert [decoded_child] = decoded.subtasks
  assert decoded_child.parent == Some(parent.source)
}

pub fn workflow_query_request_response_roundtrip_test() {
  let list_request = types.WorkflowList
  let detail_request =
    types.WorkflowDetail(types.WorkflowDetailQuery(
      workflow_id: "implementation",
    ))

  assert codec.decode_request(codec.request_to_string(list_request))
    == Ok(list_request)
  assert codec.decode_request(codec.request_to_string(detail_request))
    == Ok(detail_request)

  let list_response =
    types.WorkflowListResponse(
      types.WorkflowListDto(
        schema_version: types.workflow_query_schema_version,
        freshness: workflow_freshness(),
        diagnostics: [],
        workflows: [workflow_summary()],
      ),
    )
  let detail_response =
    types.WorkflowDetailResponse(types.WorkflowDetailDto(
      schema_version: types.workflow_query_schema_version,
      summary: workflow_summary(),
      yaml_sources: [workflow_yaml_source()],
      diagnostics: [],
      freshness: workflow_freshness(),
      graph: types.WorkflowGraphDto(
        nodes: [
          types.WorkflowGraphNodeDto(
            id: "implement",
            label: "implement",
            kind: "agent",
          ),
        ],
        edges: [],
      ),
    ))

  let encoded_list = codec.response_to_string(list_response)
  assert string.contains(encoded_list, "\"type\":\"workflow_list\"")
  assert string.contains(encoded_list, "\"schema_version\":1")
  assert codec.decode_response(encoded_list) == Ok(list_response)

  let encoded_detail = codec.response_to_string(detail_response)
  assert string.contains(encoded_detail, "\"yaml_sources\"")
  assert string.contains(encoded_detail, "workflows/implementation.yaml")
  assert string.contains(encoded_detail, "\"contents_truncated\":false")
  assert codec.decode_response(encoded_detail) == Ok(detail_response)
}

pub fn malformed_workflow_response_payloads_are_rejected_test() {
  let malformed_list =
    codec.response_to_string(workflow_list_response())
    |> string.replace(
      each: "\"workflows\":[",
      with: "\"workflows\":\"not-a-list\",\"ignored\":[",
    )

  let assert Error(types.QueryError(code: list_code, message: list_message)) =
    codec.decode_response(malformed_list)
  assert list_code == types.QueryBackendFailed
  assert list_message == "invalid workflow list query payload"

  let malformed_detail =
    codec.response_to_string(workflow_detail_response())
    |> string.replace(
      each: "\"contents_truncated\":false",
      with: "\"contents_truncated\":\"no\"",
    )

  let assert Error(types.QueryError(code: detail_code, message: detail_message)) =
    codec.decode_response(malformed_detail)
  assert detail_code == types.QueryBackendFailed
  assert detail_message == "invalid workflow detail query payload"
}

pub fn workflow_response_rejects_unsupported_schema_versions_test() {
  let invalid_list_schema =
    codec.response_to_string(workflow_list_response())
    |> string.replace(
      each: "\"schema_version\":1",
      with: "\"schema_version\":2",
    )

  let assert Error(types.QueryError(code: list_code, message: list_message)) =
    codec.decode_response(invalid_list_schema)
  assert list_code == types.QueryBackendFailed
  assert list_message == "unsupported workflow query schema version"

  let invalid_detail_schema =
    codec.response_to_string(workflow_detail_response())
    |> string.replace(
      each: "\"schema_version\":1",
      with: "\"schema_version\":2",
    )

  let assert Error(types.QueryError(code: detail_code, message: detail_message)) =
    codec.decode_response(invalid_detail_schema)
  assert detail_code == types.QueryBackendFailed
  assert detail_message == "unsupported workflow query schema version"
}

pub fn supported_queries_include_work_item_and_workflow_queries_test() {
  let queries = types.supported_queries()
  assert list.contains(queries, "work_item_list")
  assert list.contains(queries, "work_item_show")
  assert list.contains(queries, "workflow_list")
  assert list.contains(queries, "workflow_detail")
}

pub fn malformed_work_item_response_payload_is_rejected_test() {
  let response =
    types.WorkItemShowResponse(work_item.WorkItemDetail(
      summary: work_item_summary(labels_truncated: False),
      subtasks: [],
      subtasks_truncated: False,
    ))
  let encoded =
    codec.response_to_string(response)
    |> string.replace(
      each: "\"subtasks_truncated\":false",
      with: "\"subtasks_truncated\":\"no\"",
    )

  let assert Error(types.QueryError(code: code, message: message)) =
    codec.decode_response(encoded)
  assert code == types.QueryBackendFailed
  assert message == "invalid work item detail query payload"
}

pub fn cursor_encode_decode_roundtrip_test() {
  let encoded = cursor.encode_offset(42)

  assert encoded == "cursor:42"
  assert cursor.decode_offset(encoded) == Ok(42)
}

pub fn work_item_cursor_encode_decode_roundtrip_test() {
  let fingerprint = "active|search:workflow|updated_desc"
  let encoded = cursor.encode_work_item_offset(42, fingerprint)

  assert encoded == "work-item:42:active|search:workflow|updated_desc"
  assert cursor.decode_work_item_offset(encoded, fingerprint) == Ok(42)
}

pub fn invalid_cursor_maps_to_safe_query_error_test() {
  let assert Error(types.QueryError(code: code, message: message)) =
    cursor.decode_offset("cursor:-1")

  assert code == types.InvalidCursor
  assert message == "invalid query cursor"
}

pub fn invalid_work_item_cursor_maps_to_safe_query_error_test() {
  let assert Error(types.QueryError(code: code, message: message)) =
    cursor.decode_work_item_offset(
      "work-item:1:archive|search:workflow|updated_desc",
      "active|search:workflow|updated_desc",
    )

  assert code == types.InvalidCursor
  assert message == "invalid query cursor"
}

pub fn query_error_json_uses_stable_codes_test() {
  let encoded =
    codec.error_to_string(types.QueryError(
      types.QueryOverloaded,
      "query workers are busy",
    ))

  assert string.contains(encoded, "\"ok\":false")
  assert string.contains(encoded, "\"code\":\"query_overloaded\"")

  let assert Error(types.QueryError(code: code, message: message)) =
    codec.decode_response(encoded)
  assert code == types.QueryOverloaded
  assert message == "query workers are busy"
}

pub fn status_dto_redacts_secret_and_raw_payload_inputs_test() {
  let encoded =
    types.StatusSource(
      daemon_id: "daemon-1",
      boot_id: "boot-1",
      dispatch_paused: True,
      ui_server_enabled: True,
      supported_queries: ["status"],
      local_control_token: "local-secret-token",
      enrollment_token: "remote-secret-token",
      tracker_payload: "{\"raw\":true}",
      workflow_internals: ["provider:linear", "workflow:execplan"],
    )
    |> dto.status_from_source
    |> dto.status_to_json
    |> json.to_string

  assert string.contains(encoded, "\"daemon_id\":\"daemon-1\"")
  assert string.contains(encoded, "\"ui_server_enabled\":true")
  assert !string.contains(encoded, "local_control_token")
  assert !string.contains(encoded, "enrollment_token")
  assert !string.contains(encoded, "tracker_payload")
  assert !string.contains(encoded, "workflow_internals")
  assert !string.contains(encoded, "local-secret-token")
  assert !string.contains(encoded, "remote-secret-token")
  assert !string.contains(encoded, "provider:linear")
}

pub fn metrics_dto_uses_narrow_non_secret_source_test() {
  let encoded =
    types.OperationalMetricsSource(
      ..types.default_operational_metrics_source(
        daemon_id: "daemon-1",
        boot_id: "boot-1",
      ),
      sampled_at_ms: 123,
      dispatch_paused: True,
      ui_server_enabled: True,
      remote_client_status: "connected",
      workflow_count: 2,
      active_sessions: 1,
      pending_dispatch_validations: 3,
      pending_review_lane_preflights: 4,
      aggregate_tokens: session_tokens.TokenTotals(
        input: 10,
        output: 20,
        cache_read: 3,
        cache_write: 4,
        total: 37,
      ),
    )
    |> dto.operational_metrics_from_source
    |> dto.operational_metrics_to_json
    |> json.to_string

  assert string.contains(encoded, "\"daemon_id\":\"daemon-1\"")
  assert string.contains(encoded, "\"schema_version\":1")
  assert string.contains(encoded, "\"active_sessions\":1")
  assert string.contains(encoded, "\"pending_dispatch_validations\":3")
  assert string.contains(encoded, "\"pending_review_lane_preflights\":4")
  assert string.contains(encoded, "\"scheduled_next_due_count\":0")
  assert string.contains(encoded, "\"lifecycle_projection_failed\":false")
  assert string.contains(encoded, "\"total\":37")
  assert !string.contains(encoded, "local_control_token")
  assert !string.contains(encoded, "enrollment_token")
  assert !string.contains(encoded, "tracker_payload")
  assert !string.contains(encoded, "workflow_internals")
  assert !string.contains(encoded, "api_key")
  assert !string.contains(encoded, "provider:linear")
  assert !string.contains(encoded, "raw failure payload")
}

fn work_item_summary(
  labels_truncated labels_truncated: Bool,
) -> work_item.WorkItemSummary {
  work_item.WorkItemSummary(
    id: "linear:issue-1",
    source: work_item.WorkItemSource(
      provider: "linear",
      id: "issue-1",
      display_id: Some("LIV-770"),
      url: Some("https://linear.app/living-systems/issue/LIV-770"),
    ),
    parent: None,
    title: "Implement work item queries",
    state: task.TaskState(id: Some("todo"), name: "Todo", category: task.Ready),
    labels: [
      task.TaskLabel(
        id: Some("label-workflow"),
        name: "workflow:implementation",
      ),
    ],
    labels_truncated: labels_truncated,
    created_at: None,
    updated_at: None,
    actions: [
      action.mutating(
        action.run_workflow_action_id,
        "Run workflow",
        False,
        Some(action.ActionDisabledReason(
          code: "run_workflow_not_enabled",
          message: "Run workflow is not enabled yet",
        )),
        action.ActionTargetSummary(
          kind: "work_item",
          provider: "linear",
          id: "issue-1",
          display_id: Some("LIV-770"),
          workflow_id: Some("workflow:implementation"),
          run_id: None,
        ),
      ),
    ],
  )
}

fn workflow_list_response() -> types.QueryResponse {
  types.WorkflowListResponse(
    types.WorkflowListDto(
      schema_version: types.workflow_query_schema_version,
      freshness: workflow_freshness(),
      diagnostics: [],
      workflows: [workflow_summary()],
    ),
  )
}

fn workflow_detail_response() -> types.QueryResponse {
  types.WorkflowDetailResponse(types.WorkflowDetailDto(
    schema_version: types.workflow_query_schema_version,
    summary: workflow_summary(),
    yaml_sources: [workflow_yaml_source()],
    diagnostics: [],
    freshness: workflow_freshness(),
    graph: types.WorkflowGraphDto(
      nodes: [
        types.WorkflowGraphNodeDto(
          id: "implement",
          label: "implement",
          kind: "agent",
        ),
      ],
      edges: [],
    ),
  ))
}

fn workflow_summary() -> types.WorkflowSummaryDto {
  types.WorkflowSummaryDto(
    id: "implementation",
    name: "implementation",
    route: Some("implementation"),
    label: Some("workflow:implementation"),
    yaml_paths: ["scherzo.yaml", "workflows/implementation.yaml"],
    step_count: 1,
    status: "valid",
  )
}

fn workflow_yaml_source() -> types.WorkflowYamlSourceDto {
  types.WorkflowYamlSourceDto(
    path: "workflows/implementation.yaml",
    contents: "version: 1\nid: implementation\n",
    contents_sha256: "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef",
    contents_truncated: False,
  )
}

fn workflow_freshness() -> types.WorkflowFreshnessDto {
  types.WorkflowFreshnessDto(
    source_hash: "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef",
    reload_status: "valid",
  )
}

fn outbox_record() -> types.OutboxRecordDto {
  types.OutboxRecordDto(
    outbox_id: "outbox-1",
    kind: "linear_comment",
    status: types.OutboxRetryableStatus,
    task_ref: types.OutboxTaskRefDto(
      provider: "linear",
      id: "issue-1",
      display_id: Some("LIV-1087"),
      url: Some("https://linear.app/living-systems/issue/LIV-1087"),
    ),
    dedupe_key: Some("dedupe-1"),
    attempt_count: Some(3),
    next_attempt_at_ms: Some(1234),
    last_error_code: Some("rate_limited"),
    pending_at_ms: None,
    attempted_at_ms: None,
    failed_at_ms: Some(1200),
    completed_at_ms: None,
    has_payload: True,
  )
}

fn task_summary() -> types.TaskSummaryDto {
  types.TaskSummaryDto(
    id: "linear:issue-1",
    source: types.TaskSourceDto(
      provider: "linear",
      id: "issue-1",
      display_id: Some("LIV-770"),
      url: Some("https://linear.app/living-systems/issue/LIV-770"),
    ),
    title: "Implement task queries",
    state: task.Ready,
    priority: Some(types.TaskPriorityDto(value: 2, label: "High")),
    labels: [
      types.TaskLabelDto(
        id: Some("label-workflow"),
        name: "workflow:implementation",
      ),
    ],
    created_at: Some("2026-04-28T10:00:00Z"),
    updated_at: Some("2026-04-28T11:00:00Z"),
  )
}

fn nested_field(line: String, field_name: String) {
  case json.parse(line, decode.dynamic) {
    Ok(value) -> decode.run(value, decode.at([field_name], decode.dynamic))
    Error(_) -> Error([])
  }
}
