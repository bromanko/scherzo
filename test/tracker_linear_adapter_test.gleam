import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/config
import scherzo/config/types as config_types
import scherzo/control/query/dto as query_dto
import scherzo/error
import scherzo/handoff
import scherzo/linear
import scherzo/result_artifact
import scherzo/scheduled_failure_reporter as reporter
import scherzo/session/tokens as session_tokens
import scherzo/task
import scherzo/tracker/adapter as tracker_adapter
import scherzo/tracker/idempotency
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/linear_adapter
import scherzo/tracker/state as issue_state
import scherzo/work_item
import scherzo/workflow_completion_policy
import simplifile
import test_async

type CapturedRequest {
  CapturedRequest(String)
}

type SearchPhase {
  FirstSearch
  SecondSearch
}

fn tracker_config() -> config_types.TrackerConfig {
  config_types.TrackerConfig(
    kind: tracker_kind.LinearTracker,
    endpoint: "https://api.linear.test/graphql",
    api_key: Some("secret-key"),
    project_slug: Some("PROJ"),
    task_scope: None,
    active_states: issue_state.list_from_strings(["Todo", "In Progress"]),
    dispatch_states: issue_state.list_from_strings(["Todo"]),
    terminal_states: issue_state.list_from_strings(["Done"]),
  )
}

fn effective_config() -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    tracker: tracker_config(),
    polling: config.default_polling_config(),
    workspace: config_types.WorkspaceConfig(root: "."),
    control: config.default_control_config(),
    ledger_compaction: config.default_ledger_compaction_config(),
    hooks: config.default_hooks_config(),
    agent: config.default_agent_config(),
    pi: config.default_pi_config(),
    handoff: config.default_handoff_config(),
    linear_contract: config.default_linear_contract_config(),
    linear_commands: config.default_linear_command_config(),
    ui_server: config.default_ui_server_config(),
  )
}

fn handoff_effective_config() -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    ..effective_config(),
    handoff: config_types.HandoffConfig(
      enabled: True,
      comment_on_claim: True,
      comment_on_success: True,
      comment_on_failure: True,
      comment_on_park: True,
      claim_state_id: Some(workflow_completion_policy.StateById("claim-state")),
      success_state_id: Some(workflow_completion_policy.StateById(
        "success-state",
      )),
      failure_state_id: Some(workflow_completion_policy.StateById(
        "failure-state",
      )),
      include_result_on_success: True,
      attach_result_on_success: False,
      attachment_fallback_to_markdown_link: True,
      result_max_chars: 8000,
      completion_states: None,
    ),
  )
}

fn task_search_request() -> tracker_adapter.TaskSearchRequest {
  tracker_adapter.TaskSearchRequest(
    active_states: ["Todo", "In Progress"],
    dispatch_states: ["Todo"],
    terminal_states: ["Done"],
    workflow_labels: ["workflow:execplan"],
    limit: 10,
  )
}

fn linear_task_ref() -> task.TaskRef {
  task.TaskRef(
    backend_kind: "linear",
    remote_id: "issue-1",
    key: Some("LIV-266"),
    url: Some("https://linear.app/living-systems/issue/LIV-266"),
  )
}

fn linear_task() -> task.Task {
  task.Task(
    ref: linear_task_ref(),
    title: "Refresh architecture",
    description: Some("body"),
    priority: Some(2),
    state: task.TaskState(id: None, name: "Todo", category: task.Unknown),
    branch_hint: Some("liv-266-refresh"),
    labels: [task.TaskLabel(id: None, name: "workflow:implementation")],
    blockers: [],
    blockers_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn non_linear_task() -> task.Task {
  task.Task(
    ..linear_task(),
    ref: task.TaskRef(
      backend_kind: "github",
      remote_id: "issue-1",
      key: Some("GH-1"),
      url: None,
    ),
  )
}

fn linear_task_without_remote_id() -> task.Task {
  task.Task(
    ..linear_task(),
    ref: task.TaskRef(
      backend_kind: "linear",
      remote_id: "",
      key: Some("LIV-blank"),
      url: None,
    ),
  )
}

fn worker_success(task_context: task.Task) -> agent_types.WorkerSuccess {
  agent_types.WorkerSuccess(
    final_issue: Some(task.to_runtime_issue(task_context)),
    final_classification: agent_types.FinalTerminal,
    workspace_path: "workspace/main",
    tokens: session_tokens.zero_token_totals(),
    turns: 1,
    result: result_artifact.from_final_response(
      Some("completed generic handoff"),
      False,
      "agent_end_messages",
    ),
  )
}

fn worker_failure() -> agent_types.WorkerFailure {
  agent_types.WorkerFailure(
    reason: error.PiFailed(error.PiProtocolError("generic handoff failure")),
    workspace_path: Some("workspace/main"),
    tokens: session_tokens.zero_token_totals(),
    final_issue: None,
  )
}

fn receive_request(subject: process.Subject(CapturedRequest)) -> String {
  let assert Ok(CapturedRequest(body)) = process.receive(subject, within: 1000)
  body
}

pub fn linear_adapter_does_not_expose_remote_commands_test() {
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(_) {
      Error(error.LinearApiRequest("unexpected Linear transport call"))
    })

  let assert None = linear_tracker.remote_commands
}

pub fn linear_adapter_fetch_candidates_matches_linear_issue_decoder_test() {
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      assert string.contains(request.body, "CandidateIssues")
      Ok(linear.Response(status: 200, body: candidate_response()))
    })

  let assert Ok([candidate]) =
    linear_tracker.task_source.fetch_candidates(task_search_request())

  assert candidate.ref == linear_task_ref()
  assert candidate.title == "Refresh architecture"
  assert candidate.description == Some("body")
  assert candidate.priority == Some(2)
  assert candidate.state
    == task.TaskState(id: None, name: "Todo", category: task.Unknown)
  assert candidate.branch_hint == Some("liv-266-refresh")
  assert task.label_names(candidate) == ["workflow:execplan"]
  assert candidate.blockers == []
  assert candidate.blockers_complete == True
}

pub fn linear_adapter_comment_create_only_with_marker_updates_existing_comment_test() {
  let marker = "claim:linear:issue-1:run-1"
  let requests = process.new_subject()
  let responses = process.new_subject()
  process.send(
    responses,
    linear.Response(
      status: 200,
      body: issue_comments_response([
        comment_node(
          "comment-existing",
          "old body\n\n" <> idempotency.marker(marker),
        ),
      ]),
    ),
  )
  process.send(
    responses,
    linear.Response(
      status: 200,
      body: comment_update_response(
        "comment-existing",
        "updated body\n\n" <> idempotency.marker(marker),
      ),
    ),
  )
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      process.send(requests, CapturedRequest(request.body))
      let assert Ok(response) = process.receive(responses, within: 1000)
      Ok(response)
    })
  let assert Some(comments) = linear_tracker.comments

  let assert Ok(receipt) =
    comments.post_or_update(tracker_adapter.CommentRequest(
      task: linear_task_ref(),
      body: "updated body\n\n" <> idempotency.marker(marker),
      mode: tracker_adapter.CreateOnly,
    ))

  let first_request = receive_request(requests)
  assert string.contains(first_request, "ScherzoIssueComments")
  let second_request = receive_request(requests)
  assert string.contains(second_request, "ScherzoCommentUpdate")
  assert string.contains(second_request, "comment-existing")
  assert receipt.id == "comment-existing"
  assert receipt.created == False
}

pub fn linear_adapter_comment_find_by_marker_returns_existing_receipt_test() {
  let marker = "park:linear:issue-1:run-1"
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      assert string.contains(request.body, "ScherzoIssueComments")
      Ok(linear.Response(
        status: 200,
        body: issue_comments_response([
          comment_node(
            "comment-park",
            "parked\n\n" <> idempotency.marker(marker),
          ),
        ]),
      ))
    })
  let assert Some(comments) = linear_tracker.comments

  let assert Ok(Some(receipt)) =
    comments.find_by_marker(tracker_adapter.CommentLookup(
      task: linear_task_ref(),
      marker: marker,
    ))

  assert receipt.id == "comment-park"
  assert receipt.created == False
}

pub fn linear_adapter_lookup_operator_ref_falls_back_to_candidate_identifier_test() {
  let phases = process.new_subject()
  process.send(phases, FirstSearch)
  process.send(phases, SecondSearch)
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      let assert Ok(phase) = process.receive(phases, within: 1000)
      case phase {
        FirstSearch -> {
          assert string.contains(request.body, "IssueStates")
          Ok(linear.Response(status: 200, body: empty_issues_response()))
        }
        SecondSearch -> {
          assert string.contains(request.body, "CandidateIssues")
          Ok(linear.Response(status: 200, body: candidate_response()))
        }
      }
    })

  let assert Ok(Some(found)) =
    linear_tracker.task_source.lookup_by_operator_ref("LIV-266")
  assert found.ref == linear_task_ref()
}

pub fn linear_adapter_task_list_maps_public_fields_and_paginates_test() {
  let captured = process.new_subject()
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      process.send(captured, CapturedRequest(request.body))
      assert string.contains(request.body, "ScherzoTaskList")
      Ok(linear.Response(status: 200, body: task_query_list_response()))
    })

  let assert Ok(first_page) =
    linear_tracker.task_source.list_tasks(tracker_adapter.TaskListRequest(
      state_categories: [task.Ready],
      limit: 1,
      offset: 0,
    ))
  let assert [first] = first_page.items
  assert first.ref.remote_id == "issue-ready-1"
  assert first.priority == Some(2)
  assert first.state.category == task.Ready
  assert first.labels
    == [
      task.TaskLabel(
        id: Some("label-workflow"),
        name: "workflow:implementation",
      ),
    ]
  let first_summary = query_dto.task_summary_from_task(first)
  assert first_summary.created_at == Some("2026-04-28T10:00:00.000Z")
  assert first_summary.updated_at == Some("2026-04-28T11:00:00.000Z")
  assert first_page.has_more == True

  let assert Ok(second_page) =
    linear_tracker.task_source.list_tasks(tracker_adapter.TaskListRequest(
      state_categories: [task.Ready],
      limit: 1,
      offset: 1,
    ))
  let assert [second] = second_page.items
  assert second.ref.remote_id == "issue-ready-2"
  assert second_page.has_more == False

  let assert Ok(active_page) =
    linear_tracker.task_source.list_tasks(tracker_adapter.TaskListRequest(
      state_categories: [task.Active],
      limit: 10,
      offset: 0,
    ))
  let assert [active] = active_page.items
  assert active.ref.remote_id == "issue-active-1"
  assert active.state.category == task.Active

  let first_body = receive_request(captured)
  assert string.contains(first_body, "Todo")
  assert !string.contains(first_body, "secret-key")
}

pub fn linear_adapter_task_detail_lookup_by_identifier_and_remote_id_test() {
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      case string.contains(request.body, "ScherzoTaskDetailByIdentifier") {
        True -> {
          assert string.contains(request.body, "ScherzoTaskDetailByIdentifier")
          assert string.contains(request.body, "issue(id: $issueId)")
          assert string.contains(request.body, "issueId")
          assert string.contains(request.body, "LIV-770")
          assert !string.contains(request.body, "issueRemoteId")
          assert !string.contains(request.body, "taskFilter")
          Ok(linear.Response(
            status: 200,
            body: task_detail_by_identifier_response(),
          ))
        }
        False -> {
          assert string.contains(request.body, "ScherzoTaskDetailById")
          assert string.contains(
            request.body,
            "\"taskFilter\":{\"project\":{\"slugId\":{\"eq\":\"PROJ\"}}}",
          )
          assert string.contains(request.body, "\"ids\":[\"issue-ready-1\"]")
          assert !string.contains(request.body, "issue(id")
          Ok(linear.Response(status: 200, body: task_detail_by_id_response()))
        }
      }
    })

  let assert Ok(Some(by_identifier)) =
    linear_tracker.task_source.lookup_task_detail(
      tracker_adapter.TaskLookupByDisplayId(" LIV-770 "),
    )
  assert by_identifier.ref.remote_id == "issue-ready-1"
  assert by_identifier.description == Some("Detail body from identifier")
  assert by_identifier.state.category == task.Ready

  let assert Ok(Some(by_id)) =
    linear_tracker.task_source.lookup_task_detail(
      tracker_adapter.TaskLookupByRemoteId(
        provider: Some("linear"),
        id: "issue-ready-1",
      ),
    )
  assert by_id.ref.key == Some("LIV-770")
  assert by_id.description == Some("Detail body from id")

  assert linear_tracker.task_source.lookup_task_detail(
      tracker_adapter.TaskLookupByRemoteId(
        provider: Some("github"),
        id: "issue-ready-1",
      ),
    )
    == Ok(None)
}

pub fn linear_adapter_task_detail_display_id_contract_fixture_decodes_test() {
  // This fixture is a captured read-only Linear issue(id: "LIV-864") response.
  // The request uses the same supported direct lookup. Project and label scope
  // are enforced after decode. Linear may return only the normalized project
  // slug suffix while configuration includes a human-readable prefix.
  let assert Ok(body) =
    simplifile.read(
      "test/fixtures/linear/display_id_issue_lookup_response.json",
    )
  let linear_tracker =
    linear_adapter.from_tracker_config(
      config_types.TrackerConfig(
        ..tracker_config(),
        project_slug: Some("scherzo-f6f4bc92d6d7"),
      ),
      fn(request) {
        assert string.contains(request.body, "ScherzoTaskDetailByIdentifier")
        assert string.contains(request.body, "issue(id: $issueId)")
        assert string.contains(request.body, "issueId")
        assert string.contains(request.body, "LIV-864")
        assert !string.contains(request.body, "issueRemoteId")
        assert !string.contains(request.body, "taskFilter")
        Ok(linear.Response(status: 200, body: body))
      },
    )

  let assert Ok(Some(found)) =
    linear_tracker.task_source.lookup_task_detail(
      tracker_adapter.TaskLookupByDisplayId("LIV-864"),
    )
  assert found.ref.remote_id == "ff2bd557-36a1-4753-986e-d9a1c3afa330"
  assert found.ref.key == Some("LIV-864")
  assert found.title
    == "Fix scherzoctl task query display IDs and unfiltered list timeouts"
  assert found.state.category == task.Active
}

pub fn linear_adapter_task_detail_identifier_not_found_or_wrong_project_test() {
  let missing_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      assert string.contains(request.body, "ScherzoTaskDetailByIdentifier")
      Ok(linear.Response(
        status: 200,
        body: task_detail_by_identifier_not_found_response(),
      ))
    })

  assert missing_tracker.task_source.lookup_task_detail(
      tracker_adapter.TaskLookupByDisplayId("LIV-999"),
    )
    == Ok(None)

  let wrong_project_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      assert string.contains(request.body, "ScherzoTaskDetailByIdentifier")
      Ok(linear.Response(
        status: 200,
        body: task_detail_by_identifier_wrong_project_response(),
      ))
    })

  assert wrong_project_tracker.task_source.lookup_task_detail(
      tracker_adapter.TaskLookupByDisplayId("LIV-770"),
    )
    == Ok(None)

  let missing_error_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      assert string.contains(request.body, "ScherzoTaskDetailByIdentifier")
      Ok(linear.Response(
        status: 200,
        body: task_detail_by_identifier_not_found_error_response(),
      ))
    })

  assert missing_error_tracker.task_source.lookup_task_detail(
      tracker_adapter.TaskLookupByDisplayId("LIV-999"),
    )
    == Ok(None)

  let mismatched_identifier_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      assert string.contains(request.body, "ScherzoTaskDetailByIdentifier")
      Ok(linear.Response(
        status: 200,
        body: task_detail_by_identifier_mismatched_identifier_response(),
      ))
    })

  assert mismatched_identifier_tracker.task_source.lookup_task_detail(
      tracker_adapter.TaskLookupByDisplayId("LIV-770"),
    )
    == Ok(None)
}

pub fn linear_adapter_task_detail_blank_display_id_returns_none_without_transport_test() {
  let captured = process.new_subject()
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      process.send(captured, CapturedRequest(request.body))
      Ok(linear.Response(
        status: 200,
        body: task_detail_by_identifier_response(),
      ))
    })

  assert linear_tracker.task_source.lookup_task_detail(
      tracker_adapter.TaskLookupByDisplayId("   "),
    )
    == Ok(None)
  test_async.assert_no_extra_message_within(captured, 20)
}

pub fn linear_adapter_task_detail_identifier_backend_errors_stay_backend_errors_test() {
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      assert string.contains(request.body, "ScherzoTaskDetailByIdentifier")
      Ok(linear.Response(status: 200, body: task_query_graphql_error_response()))
    })

  let assert Error(tracker_adapter.Permanent(message)) =
    linear_tracker.task_source.lookup_task_detail(
      tracker_adapter.TaskLookupByDisplayId("LIV-770"),
    )
  assert string.contains(message, "Linear GraphQL errors: denied")
}

pub fn linear_adapter_work_item_list_and_show_wiring_test() {
  let list_body =
    "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\"issue-parent-1\",\"identifier\":\"LIV-1168\",\"title\":\"Implement work items\",\"url\":\"https://linear.app/living-systems/issue/LIV-1168\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"state\":{\"id\":\"state-todo\",\"name\":\"Todo\",\"type\":\"unstarted\"},\"labels\":{\"nodes\":[{\"id\":\"label-workflow\",\"name\":\"workflow:implementation\"}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}},\"children\":{\"nodes\":[{\"id\":\"issue-child-1\",\"identifier\":\"LIV-1169\",\"title\":\"Child one\",\"url\":\"https://linear.app/living-systems/issue/LIV-1169\",\"createdAt\":\"2026-04-28T12:00:00Z\",\"updatedAt\":\"2026-04-28T13:00:00Z\",\"state\":{\"id\":\"state-progress\",\"name\":\"In Progress\",\"type\":\"started\"},\"labels\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"
  let detail_body =
    "{\"data\":{\"issue\":{\"project\":{\"slugId\":\"PROJ\"},\"id\":\"issue-parent-1\",\"identifier\":\"LIV-1168\",\"title\":\"Implement work items\",\"url\":\"https://linear.app/living-systems/issue/LIV-1168\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"state\":{\"id\":\"state-todo\",\"name\":\"Todo\",\"type\":\"unstarted\"},\"labels\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}},\"children\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}}"
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      case string.contains(request.body, "ScherzoWorkItemList") {
        True -> Ok(linear.Response(status: 200, body: list_body))
        False -> Ok(linear.Response(status: 200, body: detail_body))
      }
    })
  let assert Some(work_items) = linear_tracker.work_items

  let assert Ok(page) =
    work_items.list_work_items(work_item.WorkItemListRequest(
      state_categories: [task.Ready],
      search: None,
      sort: work_item.UpdatedDescWorkItems,
      limit: 10,
      offset: 0,
      subtask_limit: 10,
      label_limit: 50,
    ))
  let assert [summary] = page.items
  assert summary.source.display_id == Some("LIV-1168")

  let assert Ok(Some(detail)) =
    work_items.lookup_work_item(work_item.WorkItemShowRequest(
      ref: work_item.WorkItemLookupByDisplayId("LIV-1168"),
      subtask_limit: 50,
      label_limit: 50,
    ))
  assert detail.summary.source.id == "issue-parent-1"
}

pub fn linear_adapter_work_item_unfiltered_scan_searches_and_sorts_test() {
  let list_body =
    "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\"issue-ready-1\",\"identifier\":\"LIV-2000\",\"title\":\"Ready work item\",\"url\":\"https://linear.app/living-systems/issue/LIV-2000\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"state\":{\"id\":\"state-todo\",\"name\":\"Todo\",\"type\":\"unstarted\"},\"labels\":{\"nodes\":[{\"id\":\"label-1\",\"name\":\"workflow:execplan\"}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}},{\"id\":\"issue-active-1\",\"identifier\":\"LIV-2001\",\"title\":\"Active work item\",\"url\":\"https://linear.app/living-systems/issue/LIV-2001\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"state\":{\"id\":\"state-progress\",\"name\":\"In Progress\",\"type\":\"started\"},\"labels\":{\"nodes\":[{\"id\":\"label-1\",\"name\":\"workflow:execplan\"}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}},{\"id\":\"issue-done-1\",\"identifier\":\"LIV-2002\",\"title\":\"Done work item\",\"url\":\"https://linear.app/living-systems/issue/LIV-2002\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T09:00:00Z\",\"state\":{\"id\":\"state-done\",\"name\":\"Done\",\"type\":\"completed\"},\"labels\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      assert string.contains(request.body, "ScherzoWorkItemList")
      assert !string.contains(request.body, "stateNames")
      Ok(linear.Response(status: 200, body: list_body))
    })
  let assert Some(work_items) = linear_tracker.work_items

  let assert Ok(page) =
    work_items.list_work_items(work_item.WorkItemListRequest(
      state_categories: [task.Backlog, task.Ready, task.Active, task.Unknown],
      search: Some("workflow:execplan"),
      sort: work_item.UpdatedDescWorkItems,
      limit: 10,
      offset: 0,
      subtask_limit: 10,
      label_limit: 50,
    ))
  let assert [first, second] = page.items
  assert first.source.display_id == Some("LIV-2001")
  assert second.source.display_id == Some("LIV-2000")
  assert page.has_more == False
}

pub fn linear_adapter_work_item_scan_limit_is_terminal_test() {
  let calls = process.new_subject()
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      process.send(calls, Nil)
      assert string.contains(request.body, "ScherzoWorkItemList")
      Ok(linear.Response(
        status: 200,
        body: work_item_list_response(
          work_item_scan_nodes(251),
          has_next_page: True,
          end_cursor: Some("after-cap"),
        ),
      ))
    })
  let assert Some(work_items) = linear_tracker.work_items

  let assert Ok(page) =
    work_items.list_work_items(work_item.WorkItemListRequest(
      state_categories: [task.Backlog, task.Ready, task.Active, task.Unknown],
      search: Some("only-after-cap"),
      sort: work_item.UpdatedDescWorkItems,
      limit: 10,
      offset: 0,
      subtask_limit: 10,
      label_limit: 50,
    ))

  assert page.items == []
  assert page.has_more == False
  let assert Ok(Nil) = process.receive(calls, within: 1000)
  test_async.assert_no_extra_message_within(calls, 20)
}

pub fn linear_adapter_work_item_remote_id_lookup_uses_by_id_query_test() {
  let detail_body =
    "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\"issue-parent-1\",\"identifier\":\"LIV-1168\",\"title\":\"Implement work items\",\"url\":\"https://linear.app/living-systems/issue/LIV-1168\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"state\":{\"id\":\"state-todo\",\"name\":\"Todo\",\"type\":\"unstarted\"},\"labels\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}},\"children\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      assert string.contains(request.body, "ScherzoWorkItemDetailById")
      assert string.contains(request.body, "\"ids\":[\"issue-parent-1\"]")
      assert !string.contains(request.body, "issue(id: $issueId)")
      Ok(linear.Response(status: 200, body: detail_body))
    })
  let assert Some(work_items) = linear_tracker.work_items

  let assert Ok(Some(detail)) =
    work_items.lookup_work_item(work_item.WorkItemShowRequest(
      ref: work_item.WorkItemLookupByRemoteId(
        provider: Some("linear"),
        id: "linear:issue-parent-1",
      ),
      subtask_limit: 50,
      label_limit: 50,
    ))
  assert detail.summary.source.display_id == Some("LIV-1168")
}

pub fn linear_adapter_work_item_uses_configured_state_categories_test() {
  let config =
    config_types.TrackerConfig(
      ..tracker_config(),
      active_states: issue_state.list_from_strings(["Queued", "In Progress"]),
      dispatch_states: issue_state.list_from_strings(["Queued"]),
    )
  let list_body =
    "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\"issue-queued-1\",\"identifier\":\"LIV-1200\",\"title\":\"Queued work item\",\"url\":\"https://linear.app/living-systems/issue/LIV-1200\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"state\":{\"id\":\"state-queued\",\"name\":\"Queued\",\"type\":\"started\"},\"labels\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"
  let detail_body =
    "{\"data\":{\"issue\":{\"project\":{\"slugId\":\"PROJ\"},\"id\":\"issue-queued-1\",\"identifier\":\"LIV-1200\",\"title\":\"Queued work item\",\"url\":\"https://linear.app/living-systems/issue/LIV-1200\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"state\":{\"id\":\"state-queued\",\"name\":\"Queued\",\"type\":\"started\"},\"labels\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}},\"children\":{\"nodes\":[{\"id\":\"issue-queued-child\",\"identifier\":\"LIV-1201\",\"title\":\"Queued child\",\"url\":\"https://linear.app/living-systems/issue/LIV-1201\",\"createdAt\":\"2026-04-28T12:00:00Z\",\"updatedAt\":\"2026-04-28T13:00:00Z\",\"state\":{\"id\":\"state-queued\",\"name\":\"Queued\",\"type\":\"started\"},\"labels\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}}"
  let linear_tracker =
    linear_adapter.from_tracker_config(config, fn(request) {
      case string.contains(request.body, "ScherzoWorkItemList") {
        True -> Ok(linear.Response(status: 200, body: list_body))
        False -> Ok(linear.Response(status: 200, body: detail_body))
      }
    })
  let assert Some(work_items) = linear_tracker.work_items

  let assert Ok(page) =
    work_items.list_work_items(work_item.WorkItemListRequest(
      state_categories: [task.Ready],
      search: None,
      sort: work_item.UpdatedDescWorkItems,
      limit: 10,
      offset: 0,
      subtask_limit: 10,
      label_limit: 50,
    ))
  let assert [summary] = page.items
  assert summary.state.category == task.Ready

  let assert Ok(Some(detail)) =
    work_items.lookup_work_item(work_item.WorkItemShowRequest(
      ref: work_item.WorkItemLookupByDisplayId("LIV-1200"),
      subtask_limit: 50,
      label_limit: 50,
    ))
  assert detail.summary.state.category == task.Ready
  let assert [child] = detail.subtasks
  assert child.state.category == task.Ready
}

pub fn linear_adapter_task_list_rejects_unfiltered_requests_without_transport_test() {
  let captured = process.new_subject()
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      process.send(captured, CapturedRequest(request.body))
      Ok(linear.Response(status: 200, body: task_query_list_response()))
    })

  let assert Error(tracker_adapter.UnsupportedCapability(capability)) =
    linear_tracker.task_source.list_tasks(tracker_adapter.TaskListRequest(
      state_categories: [],
      limit: 5,
      offset: 0,
    ))
  assert string.contains(capability, "unfiltered Linear task list")
  assert string.contains(capability, "--state")
  test_async.assert_no_extra_message_within(captured, 20)
}

pub fn linear_adapter_task_list_maps_graphql_and_json_errors_test() {
  let graphql_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(_) {
      Ok(linear.Response(status: 200, body: task_query_graphql_error_response()))
    })
  let assert Error(tracker_adapter.Permanent(graphql_message)) =
    graphql_tracker.task_source.list_tasks(tracker_adapter.TaskListRequest(
      state_categories: [task.Ready],
      limit: 10,
      offset: 0,
    ))
  assert string.contains(graphql_message, "Linear GraphQL errors: denied")

  let invalid_json_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(_) {
      Ok(linear.Response(status: 200, body: "not json"))
    })
  assert invalid_json_tracker.task_source.list_tasks(
      tracker_adapter.TaskListRequest(
        state_categories: [task.Ready],
        limit: 10,
        offset: 0,
      ),
    )
    == Error(tracker_adapter.DecodeFailed("invalid JSON payload"))
}

pub fn linear_adapter_task_list_rejects_missing_end_cursor_test() {
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(_) {
      Ok(linear.Response(
        status: 200,
        body: task_query_missing_end_cursor_response(),
      ))
    })

  assert linear_tracker.task_source.list_tasks(tracker_adapter.TaskListRequest(
      state_categories: [task.Ready],
      limit: 10,
      offset: 0,
    ))
    == Error(tracker_adapter.DecodeFailed(
      "Linear response missing pagination endCursor",
    ))
}

pub fn linear_adapter_posts_comment_with_existing_linear_body_test() {
  let captured = process.new_subject()
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      process.send(captured, CapturedRequest(request.body))
      Ok(linear.Response(
        status: 200,
        body: comment_create_response("comment-1"),
      ))
    })
  let assert Some(tracker_adapter.CommentCapability(
    post_or_update: post_or_update,
    ..,
  )) = linear_tracker.comments

  let assert Ok(receipt) =
    post_or_update(tracker_adapter.CommentRequest(
      task: linear_task_ref(),
      body: "hello from adapter",
      mode: tracker_adapter.CreateOnly,
    ))
  let assert Ok(expected_request) =
    linear.build_comment_create_request(
      tracker_config(),
      "issue-1",
      "hello from adapter",
    )

  assert receipt
    == tracker_adapter.CommentReceipt(
      id: "comment-1",
      task: linear_task_ref(),
      url: Some("https://linear.app/living-systems/issue/LIV-266"),
      created: True,
    )
  assert receive_request(captured) == expected_request.body
}

pub fn linear_adapter_generic_handoff_events_preserve_linear_behavior_test() {
  let captured = process.new_subject()
  let linear_tracker =
    linear_adapter.from_effective_config(
      handoff_effective_config(),
      fn(request) {
        case string.contains(request.body, "ScherzoIssueComments") {
          True ->
            Ok(linear.Response(
              status: 200,
              body: issue_comments_empty_response(),
            ))
          False -> {
            process.send(captured, CapturedRequest(request.body))
            case string.contains(request.body, "issueUpdate") {
              True ->
                Ok(linear.Response(
                  status: 200,
                  body: mutation_success_response("issueUpdate"),
                ))
              False ->
                Ok(linear.Response(
                  status: 200,
                  body: comment_create_response("comment-handoff"),
                ))
            }
          }
        }
      },
    )
  let assert Some(tracker_adapter.HandoffCapability(report: report)) =
    linear_tracker.handoff
  let task_context = linear_task()

  assert report(tracker_adapter.HandoffClaim(
      task_context,
      "workspace/main",
      "run-claim",
    ))
    == Ok(Nil)
  let claim_comment = receive_request(captured)
  let claim_state = receive_request(captured)
  assert string.contains(claim_comment, "Scherzo claimed this issue")
  assert string.contains(claim_comment, "LIV-266")
  assert string.contains(claim_comment, "run-claim")
  assert string.contains(claim_state, "claim-state")

  assert report(tracker_adapter.HandoffSuccess(
      task_context,
      worker_success(task_context),
      "run-success",
      "workflow:implementation",
    ))
    == Ok(Nil)
  let success_comment = receive_request(captured)
  let success_state = receive_request(captured)
  assert string.contains(success_comment, "Scherzo completed the run")
  assert string.contains(success_comment, "completed generic handoff")
  assert string.contains(success_comment, "run-success")
  assert string.contains(success_state, "success-state")

  assert report(tracker_adapter.HandoffFailure(
      task_context,
      worker_failure(),
      "run-failure",
      "workflow:implementation",
    ))
    == Ok(Nil)
  let failure_comment = receive_request(captured)
  let failure_state = receive_request(captured)
  assert string.contains(failure_comment, "Failure diagnostics")
  assert string.contains(failure_comment, "generic handoff failure")
  assert string.contains(failure_comment, "run-failure")
  assert string.contains(failure_state, "failure-state")

  assert report(
      tracker_adapter.HandoffPark(tracker_adapter.ParkReport(
        task: linear_task_ref(),
        issue_identifier: "LIV-266",
        reason: "needs operator input",
        release_policy: Some("explicit_unpark_only"),
        run_id: Some("run-park"),
      )),
    )
    == Ok(Nil)
  let park_comment = receive_request(captured)
  assert string.contains(park_comment, "Scherzo parked this issue")
  assert string.contains(park_comment, "needs operator input")
  assert string.contains(park_comment, "explicit_unpark_only")
  assert string.contains(park_comment, "run-park")
}

pub fn linear_adapter_rejects_generic_handoff_events_for_invalid_tasks_test() {
  let captured = process.new_subject()
  let linear_tracker =
    linear_adapter.from_effective_config(
      handoff_effective_config(),
      fn(request) {
        process.send(captured, CapturedRequest(request.body))
        Error(error.LinearApiRequest("unexpected Linear transport call"))
      },
    )
  let assert Some(tracker_adapter.HandoffCapability(report: report)) =
    linear_tracker.handoff
  let non_linear_task = non_linear_task()
  let blank_linear_task = linear_task_without_remote_id()

  assert report(tracker_adapter.HandoffClaim(
      non_linear_task,
      "workspace/main",
      "run-claim",
    ))
    == Error(tracker_adapter.NotFound(non_linear_task.ref))
  assert report(tracker_adapter.HandoffSuccess(
      non_linear_task,
      worker_success(non_linear_task),
      "run-success",
      "workflow:implementation",
    ))
    == Error(tracker_adapter.NotFound(non_linear_task.ref))
  assert report(tracker_adapter.HandoffFailure(
      non_linear_task,
      worker_failure(),
      "run-failure",
      "workflow:implementation",
    ))
    == Error(tracker_adapter.NotFound(non_linear_task.ref))
  assert report(tracker_adapter.HandoffClaim(
      blank_linear_task,
      "workspace/main",
      "run-blank",
    ))
    == Error(tracker_adapter.NotFound(blank_linear_task.ref))
  test_async.assert_no_extra_message_within(captured, 20)
}

pub fn linear_adapter_scheduled_failure_preserves_dedupe_marker_test() {
  let phases = process.new_subject()
  let linear_tracker =
    linear_adapter.from_dependencies(
      effective_config(),
      linear_adapter.Dependencies(
        transport: fn(_) {
          Error(error.LinearApiRequest("unexpected Linear transport call"))
        },
        handoff_client: handoff.disabled_client(),
        scheduled_failure_client: reporter.client(scheduled_failure_backend(
          phases,
        )),
      ),
    )
  let assert Some(tracker_adapter.ScheduledFailureCapability(publish: publish)) =
    linear_tracker.scheduled_failures

  process.send(phases, FirstSearch)
  let assert Ok(created) = publish(scheduled_publication("schedule-nightly-1"))
  assert created
    == tracker_adapter.ScheduledFailureReceipt(
      task: task.TaskRef(
        backend_kind: "linear",
        remote_id: "lin-nightly",
        key: None,
        url: None,
      ),
      created: True,
      comment_id: None,
    )

  process.send(phases, SecondSearch)
  let assert Ok(updated) = publish(scheduled_publication("schedule-nightly-2"))
  assert updated
    == tracker_adapter.ScheduledFailureReceipt(
      task: task.TaskRef(
        backend_kind: "linear",
        remote_id: "lin-nightly",
        key: None,
        url: None,
      ),
      created: False,
      comment_id: None,
    )
}

pub fn linear_adapter_scheduled_failure_rate_limit_is_transient_test() {
  let linear_tracker =
    linear_adapter.from_dependencies(
      effective_config(),
      linear_adapter.Dependencies(
        transport: fn(_) {
          Error(error.LinearApiRequest("unexpected Linear transport call"))
        },
        handoff_client: handoff.disabled_client(),
        scheduled_failure_client: reporter.client(
          status_error_scheduled_failure_backend(429),
        ),
      ),
    )
  let assert Some(tracker_adapter.ScheduledFailureCapability(publish: publish)) =
    linear_tracker.scheduled_failures

  let assert Error(tracker_adapter.Transient(message)) =
    publish(scheduled_publication("schedule-nightly-429"))
  assert string.contains(message, "429")
}

fn scheduled_publication(
  run_id: String,
) -> tracker_adapter.ScheduledFailurePublication {
  tracker_adapter.ScheduledFailurePublication(
    job_id: "nightly",
    workflow_id: "nightly",
    due_at_ms: 0,
    run_id: run_id,
    attempt: 1,
    max_attempts: 1,
    reason: "workflow_command_failed:inspect: command exited 1",
    run_root: None,
    session_id: None,
    dedupe_key: reporter.dedupe_key("nightly"),
    title: "Nightly failed",
    body: "workflow_command_failed:inspect: command exited 1",
    labels: ["job:nightly"],
    target_state_name: Some("Triage"),
    previous_task_remote_id: None,
  )
}

fn status_error_scheduled_failure_backend(status: Int) -> reporter.Backend {
  reporter.Backend(
    ensure_label: fn(_) { Error(error.LinearApiStatus(status)) },
    find_open_issue_by_id: fn(_) { Error(error.LinearApiStatus(status)) },
    find_open_issues_by_labels: fn(_) { Error(error.LinearApiStatus(status)) },
    create_issue: fn(_, _, _, _) { Error(error.LinearApiStatus(status)) },
    comment_issue: fn(_, _) { Error(error.LinearApiStatus(status)) },
    move_issue_to_state: fn(_, _) { Error(error.LinearApiStatus(status)) },
  )
}

fn scheduled_failure_backend(
  phases: process.Subject(SearchPhase),
) -> reporter.Backend {
  reporter.Backend(
    ensure_label: fn(name) { Ok("id:" <> name) },
    find_open_issue_by_id: fn(_) { Ok(None) },
    find_open_issues_by_labels: fn(labels) {
      assert labels == ["scherzo:scheduled", "scherzo:scheduled-job:nightly"]
      let assert Ok(phase) = process.receive(phases, within: 1000)
      case phase {
        FirstSearch -> Ok([])
        SecondSearch ->
          Ok([
            reporter.ExistingFailureIssue(id: "lin-nightly", updated_at_ms: 2),
          ])
      }
    },
    create_issue: fn(title, body, state, label_ids) {
      assert title == "Scherzo scheduled job failed: nightly"
      assert state == "Triage"
      assert list.contains(label_ids, "id:scherzo:scheduled")
      assert list.contains(label_ids, "id:scherzo:scheduled-job:nightly")
      assert list.contains(label_ids, "id:job:nightly")
      assert string.contains(
        body,
        "<!-- scherzo-dedupe: scheduled-job:nightly -->",
      )
      assert string.contains(body, "Dedupe key: scheduled-job:nightly")
      assert string.contains(
        body,
        "Failure: workflow_command_failed:inspect: command exited 1",
      )
      Ok("lin-nightly")
    },
    comment_issue: fn(issue_id, body) {
      assert issue_id == "lin-nightly"
      assert string.contains(
        body,
        "<!-- scherzo-dedupe: scheduled-job:nightly -->",
      )
      assert string.contains(body, "Run ID: schedule-nightly-2")
      assert string.contains(body, "Current failure issue ID: lin-nightly")
      Ok(Nil)
    },
    move_issue_to_state: fn(issue_id, state) {
      assert issue_id == "lin-nightly"
      assert state == "Triage"
      Ok(Nil)
    },
  )
}

fn empty_issues_response() -> String {
  "{\"data\":{\"issues\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"
}

fn work_item_list_response(
  nodes: List(json.Json),
  has_next_page has_next_page: Bool,
  end_cursor end_cursor: Option(String),
) -> String {
  json.to_string(
    json.object([
      #(
        "data",
        json.object([
          #(
            "issues",
            json.object([
              #("nodes", json.array(nodes, of: fn(node) { node })),
              #(
                "pageInfo",
                json.object([
                  #("hasNextPage", json.bool(has_next_page)),
                  #("endCursor", json.nullable(end_cursor, of: json.string)),
                ]),
              ),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

fn work_item_scan_nodes(count: Int) -> List(json.Json) {
  work_item_scan_nodes_loop(1, count, [])
}

fn work_item_scan_nodes_loop(
  index: Int,
  count: Int,
  acc: List(json.Json),
) -> List(json.Json) {
  case index > count {
    True -> list.reverse(acc)
    False ->
      work_item_scan_nodes_loop(index + 1, count, [
        work_item_scan_node(index),
        ..acc
      ])
  }
}

fn work_item_scan_node(index: Int) -> json.Json {
  let index_string = int.to_string(index)
  let label_name = case index == 251 {
    True -> "only-after-cap"
    False -> "other-label"
  }

  json.object([
    #("id", json.string("scan-issue-" <> index_string)),
    #("identifier", json.string("LIV-" <> int.to_string(3000 + index))),
    #("title", json.string("Scanned work item " <> index_string)),
    #(
      "url",
      json.string(
        "https://linear.app/living-systems/issue/LIV-" <> index_string,
      ),
    ),
    #("createdAt", json.string("2026-04-28T10:00:00Z")),
    #("updatedAt", json.string("2026-04-28T11:00:00Z")),
    #(
      "state",
      json.object([
        #("id", json.string("state-todo")),
        #("name", json.string("Todo")),
        #("type", json.string("unstarted")),
      ]),
    ),
    #(
      "labels",
      json.object([
        #(
          "nodes",
          json.array([#("scan-label", label_name)], of: fn(label) {
            let #(id, name) = label
            json.object([#("id", json.string(id)), #("name", json.string(name))])
          }),
        ),
        #(
          "pageInfo",
          json.object([
            #("hasNextPage", json.bool(False)),
            #("endCursor", json.null()),
          ]),
        ),
      ]),
    ),
  ])
}

fn candidate_response() -> String {
  "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\"issue-1\",\"identifier\":\"LIV-266\",\"title\":\"Refresh architecture\",\"description\":\"body\",\"priority\":2,\"branchName\":\"liv-266-refresh\",\"url\":\"https://linear.app/living-systems/issue/LIV-266\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"state\":{\"name\":\"Todo\"},\"labels\":{\"nodes\":[{\"name\":\"workflow:execplan\"}]},\"inverseRelations\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"
}

fn task_query_list_response() -> String {
  "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\"issue-ready-1\",\"identifier\":\"LIV-770\",\"title\":\"Implement task queries\",\"priority\":2,\"branchName\":\"liv-770-task-queries\",\"url\":\"https://linear.app/living-systems/issue/LIV-770\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"state\":{\"id\":\"state-todo\",\"name\":\"Todo\",\"type\":\"unstarted\"},\"labels\":{\"nodes\":[{\"id\":\"label-workflow\",\"name\":\"workflow:implementation\"}]}},{\"id\":\"issue-active-1\",\"identifier\":\"LIV-771\",\"title\":\"Active task\",\"priority\":3,\"branchName\":\"liv-771-active\",\"url\":\"https://linear.app/living-systems/issue/LIV-771\",\"createdAt\":\"2026-04-28T12:00:00Z\",\"updatedAt\":\"2026-04-28T13:00:00Z\",\"state\":{\"id\":\"state-progress\",\"name\":\"In Progress\",\"type\":\"started\"},\"labels\":{\"nodes\":[]}},{\"id\":\"issue-ready-2\",\"identifier\":\"LIV-772\",\"title\":\"Second ready task\",\"priority\":4,\"branchName\":\"liv-772-ready\",\"url\":\"https://linear.app/living-systems/issue/LIV-772\",\"createdAt\":\"2026-04-28T14:00:00Z\",\"updatedAt\":\"2026-04-28T15:00:00Z\",\"state\":{\"id\":\"state-todo\",\"name\":\"Todo\",\"type\":\"unstarted\"},\"labels\":{\"nodes\":[]}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"
}

fn task_detail_by_identifier_response() -> String {
  "{\"data\":{\"issue\":{\"id\":\"issue-ready-1\",\"identifier\":\"LIV-770\",\"title\":\"Implement task queries\",\"description\":\"Detail body from identifier\",\"priority\":2,\"branchName\":\"liv-770-task-queries\",\"url\":\"https://linear.app/living-systems/issue/LIV-770\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"project\":{\"slugId\":\"PROJ\"},\"state\":{\"id\":\"state-todo\",\"name\":\"Todo\",\"type\":\"unstarted\"},\"labels\":{\"nodes\":[{\"id\":\"label-workflow\",\"name\":\"workflow:implementation\"}]}}}}"
}

fn task_detail_by_identifier_not_found_response() -> String {
  "{\"data\":{\"issue\":null}}"
}

fn task_detail_by_identifier_not_found_error_response() -> String {
  "{\"errors\":[{\"message\":\"Issue not found\"}],\"data\":{\"issue\":null}}"
}

fn task_detail_by_identifier_wrong_project_response() -> String {
  "{\"data\":{\"issue\":{\"id\":\"issue-ready-1\",\"identifier\":\"LIV-770\",\"title\":\"Implement task queries\",\"description\":\"Detail body from identifier\",\"priority\":2,\"branchName\":\"liv-770-task-queries\",\"url\":\"https://linear.app/living-systems/issue/LIV-770\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"project\":{\"slugId\":\"OTHER\"},\"state\":{\"id\":\"state-todo\",\"name\":\"Todo\",\"type\":\"unstarted\"},\"labels\":{\"nodes\":[]}}}}"
}

fn task_detail_by_identifier_mismatched_identifier_response() -> String {
  "{\"data\":{\"issue\":{\"id\":\"issue-ready-1\",\"identifier\":\"LIV-771\",\"title\":\"Implement task queries\",\"description\":\"Detail body from identifier\",\"priority\":2,\"branchName\":\"liv-770-task-queries\",\"url\":\"https://linear.app/living-systems/issue/LIV-771\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"project\":{\"slugId\":\"PROJ\"},\"state\":{\"id\":\"state-todo\",\"name\":\"Todo\",\"type\":\"unstarted\"},\"labels\":{\"nodes\":[]}}}}"
}

fn task_detail_by_id_response() -> String {
  "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\"issue-ready-1\",\"identifier\":\"LIV-770\",\"title\":\"Implement task queries\",\"description\":\"Detail body from id\",\"priority\":2,\"branchName\":\"liv-770-task-queries\",\"url\":\"https://linear.app/living-systems/issue/LIV-770\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"state\":{\"id\":\"state-todo\",\"name\":\"Todo\",\"type\":\"unstarted\"},\"labels\":{\"nodes\":[{\"id\":\"label-workflow\",\"name\":\"workflow:implementation\"}]}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"
}

fn task_query_graphql_error_response() -> String {
  "{\"errors\":[{\"message\":\"denied\"}],\"data\":null}"
}

fn task_query_missing_end_cursor_response() -> String {
  "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\"issue-ready-1\",\"identifier\":\"LIV-770\",\"title\":\"Implement task queries\",\"priority\":2,\"branchName\":\"liv-770-task-queries\",\"url\":\"https://linear.app/living-systems/issue/LIV-770\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"state\":{\"id\":\"state-todo\",\"name\":\"Todo\",\"type\":\"unstarted\"},\"labels\":{\"nodes\":[]}}],\"pageInfo\":{\"hasNextPage\":true,\"endCursor\":null}}}}"
}

fn issue_comments_empty_response() -> String {
  issue_comments_response([])
}

fn issue_comments_response(comments: List(json.Json)) -> String {
  json.to_string(
    json.object([
      #(
        "data",
        json.object([
          #(
            "issue",
            json.object([
              #(
                "comments",
                json.object([
                  #("nodes", json.array(comments, of: fn(comment) { comment })),
                  #(
                    "pageInfo",
                    json.object([
                      #("hasNextPage", json.bool(False)),
                      #("endCursor", json.null()),
                    ]),
                  ),
                ]),
              ),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

fn comment_node(comment_id: String, body: String) -> json.Json {
  json.object([
    #("id", json.string(comment_id)),
    #("body", json.string(body)),
    #("bodyData", json.string(empty_body_data())),
  ])
}

fn mutation_success_response(field: String) -> String {
  json.to_string(
    json.object([
      #(
        "data",
        json.object([#(field, json.object([#("success", json.bool(True))]))]),
      ),
    ]),
  )
}

fn comment_create_response(comment_id: String) -> String {
  comment_payload_response("commentCreate", comment_id, "hello from adapter")
}

fn comment_update_response(comment_id: String, body: String) -> String {
  comment_payload_response("commentUpdate", comment_id, body)
}

fn comment_payload_response(
  field: String,
  comment_id: String,
  body: String,
) -> String {
  json.to_string(
    json.object([
      #(
        "data",
        json.object([
          #(
            field,
            json.object([
              #("success", json.bool(True)),
              #("comment", comment_node(comment_id, body)),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

fn empty_body_data() -> String {
  json.to_string(
    json.object([
      #("type", json.string("doc")),
      #("content", json.preprocessed_array([])),
    ]),
  )
}
