import birl
import gleam/dict
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/config
import scherzo/config/types as config_types
import scherzo/control/query/backend
import scherzo/control/query/cursor
import scherzo/control/query/types
import scherzo/daemon_identity
import scherzo/state/projection
import scherzo/state/record
import scherzo/task
import scherzo/tracker/adapter
import scherzo/work_item
import support/fake_tracker_adapter
import test_async

fn effective_config() -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    tracker: config.default_tracker_config(),
    polling: config.default_polling_config(),
    workspace: config_types.WorkspaceConfig(root: "."),
    control: config.default_control_config(),
    hooks: config.default_hooks_config(),
    agent: config.default_agent_config(),
    pi: config.default_pi_config(),
    handoff: config.default_handoff_config(),
    linear_contract: config.default_linear_contract_config(),
    linear_commands: config.default_linear_command_config(),
    ui_server: config.default_ui_server_config(),
  )
}

fn identity() -> daemon_identity.DaemonIdentity {
  daemon_identity.DaemonIdentity(
    daemon_id: "daemon-1",
    boot_id: "boot-1",
    path: "test/tmp/query-backend/daemon_identity.json",
  )
}

fn second_task() -> task.Task {
  task.Task(
    ..fake_tracker_adapter.task(),
    ref: task.TaskRef(
      backend_kind: fake_tracker_adapter.backend_kind,
      remote_id: "card-2",
      key: Some("CARD-2"),
      url: Some("https://tracker.test/cards/CARD-2"),
    ),
    title: "Second fake card",
  )
}

pub fn backend_task_list_paginates_with_scherzo_owned_cursors_test() {
  let tracker_adapter =
    fake_tracker_adapter.read_only_adapter_with_tasks([
      fake_tracker_adapter.task(),
      second_task(),
    ])

  let assert Ok(types.TaskListResponse(first_page)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.TaskList(types.TaskListQuery(
        states: [task.Ready],
        limit: 1,
        cursor: None,
      )),
    )
  let assert [first] = first_page.items
  assert first.source.display_id == Some("CARD-1")
  assert first_page.page.has_more == True
  let assert Some(next_cursor) = first_page.page.next_cursor
  assert next_cursor == "cursor:1"

  let assert Ok(types.TaskListResponse(second_page)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.TaskList(types.TaskListQuery(
        states: [task.Ready],
        limit: 1,
        cursor: Some(next_cursor),
      )),
    )
  let assert [second] = second_page.items
  assert second.source.display_id == Some("CARD-2")
  assert second_page.page.has_more == False
  assert second_page.page.next_cursor == None
}

pub fn backend_task_show_resolves_display_and_remote_refs_test() {
  let tracker_adapter = fake_tracker_adapter.read_only_adapter()

  let assert Ok(types.TaskShowResponse(by_display)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.TaskShow(types.TaskShowQuery(ref: types.TaskDisplayId("CARD-1"))),
    )
  assert by_display.summary.source.id == "card-1"
  assert by_display.description.body
    == "Prove the adapter seam without Linear types"

  let assert Ok(types.TaskShowResponse(by_remote_id)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.TaskShow(
        types.TaskShowQuery(ref: types.TaskRemoteId(
          provider: Some(fake_tracker_adapter.backend_kind),
          id: "card-1",
        )),
      ),
    )
  assert by_remote_id.summary.source.display_id == Some("CARD-1")
}

pub fn backend_task_list_maps_unsupported_tracker_request_test() {
  let base = fake_tracker_adapter.read_only_adapter()
  let task_source = base.task_source
  let tracker_adapter =
    adapter.TrackerAdapter(
      ..base,
      task_source: adapter.TaskSourceCapability(
        ..task_source,
        list_tasks: fn(_) {
          Error(adapter.UnsupportedCapability(
            "unfiltered Linear task list; pass --state",
          ))
        },
      ),
    )

  let assert Error(types.QueryError(code: code, message: message)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.TaskList(types.TaskListQuery(states: [], limit: 5, cursor: None)),
    )

  assert code == types.UnsupportedQuery
  assert message
    == "tracker adapter does not support unfiltered Linear task list; pass --state"
}

pub fn backend_work_item_list_paginates_and_invokes_provider_test() {
  let calls = process.new_subject()
  let tracker_adapter = instrumented_work_item_adapter(calls)

  let assert Ok(types.WorkItemListResponse(first_page)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.WorkItemList(types.WorkItemListQuery(
        state_filter: work_item.CategoryWorkItems([task.Ready]),
        search: None,
        sort: work_item.UpdatedDescWorkItems,
        limit: 1,
        cursor: None,
      )),
    )
  let assert [first] = first_page.items
  assert first.source.display_id == Some("CARD-1")
  let assert [run_workflow] = first.actions
  assert run_workflow.action_id == "work_item.run_workflow"
  assert first_page.has_more == True
  let assert Some(first_cursor) = first_page.next_cursor
  assert string.starts_with(first_cursor, "work-item:1:")
  let assert Ok("list") = process.receive(calls, within: 1000)

  let assert Ok(types.WorkItemListResponse(second_page)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.WorkItemList(types.WorkItemListQuery(
        state_filter: work_item.CategoryWorkItems([task.Ready]),
        search: None,
        sort: work_item.UpdatedDescWorkItems,
        limit: 1,
        cursor: Some(first_cursor),
      )),
    )
  assert second_page.has_more == False
  let assert Ok("list") = process.receive(calls, within: 1000)
}

pub fn backend_work_item_list_does_not_require_projection_snapshot_test() {
  let tracker_adapter = fake_tracker_adapter.read_only_adapter()

  let assert Ok(types.WorkItemListResponse(page)) =
    backend.run_with_projection(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      fn(_) { Error(Nil) },
      types.WorkItemList(types.WorkItemListQuery(
        state_filter: work_item.CategoryWorkItems([task.Ready]),
        search: None,
        sort: work_item.UpdatedDescWorkItems,
        limit: 1,
        cursor: None,
      )),
    )

  let assert [first] = page.items
  let assert [run_workflow] = first.actions
  assert run_workflow.action_id == "work_item.run_workflow"
}

pub fn backend_work_item_show_resolves_display_and_remote_refs_test() {
  let tracker_adapter = fake_tracker_adapter.read_only_adapter()

  let assert Ok(types.WorkItemShowResponse(by_display)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.WorkItemShow(
        types.WorkItemShowQuery(ref: types.TaskDisplayId("CARD-1")),
      ),
    )
  assert by_display.summary.source.id == "card-1"
  let assert [summary_action] = by_display.summary.actions
  assert summary_action.action_id == "work_item.run_workflow"
  assert list.length(by_display.subtasks) == 2
  let assert [first_subtask, ..] = by_display.subtasks
  assert list.map(first_subtask.actions, fn(item) { item.action_id })
    == [
      "work_subtask.cancel",
      "work_subtask.review_artifacts",
      "work_subtask.fix_retry",
    ]

  let assert Ok(types.WorkItemShowResponse(by_remote_id)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.WorkItemShow(
        types.WorkItemShowQuery(ref: types.TaskRemoteId(
          provider: Some(fake_tracker_adapter.backend_kind),
          id: "card-1",
        )),
      ),
    )
  assert by_remote_id.summary.source.display_id == Some("CARD-1")
}

pub fn backend_work_item_show_uses_projection_for_review_artifact_availability_test() {
  let tracker_adapter = fake_tracker_adapter.read_only_adapter()

  let assert Ok(types.WorkItemShowResponse(detail)) =
    backend.run_with_projection(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      fn(_) {
        Ok(projection_with_retained_artifacts(
          issue_id: "card-1-child-1",
          issue_identifier: "CARD-1.1",
        ))
      },
      types.WorkItemShow(
        types.WorkItemShowQuery(ref: types.TaskDisplayId("CARD-1")),
      ),
    )

  let assert [first_subtask, ..] = detail.subtasks
  let assert [_, review_action, _] = first_subtask.actions
  assert review_action.enabled
  let assert [artifact, ..] = review_action.artifacts
  assert artifact.ref == "artifact://run-1/output.json"
}

pub fn backend_work_item_show_projects_zero_one_many_child_summaries_test() {
  let tracker_adapter =
    fake_tracker_adapter.read_only_adapter_with_work_item_details([
      fake_work_item_detail(
        remote_id: "card-zero",
        display_id: "CARD-ZERO",
        child_count: 0,
      ),
      fake_work_item_detail(
        remote_id: "card-one",
        display_id: "CARD-ONE",
        child_count: 1,
      ),
      fake_work_item_detail(
        remote_id: "card-many",
        display_id: "CARD-MANY",
        child_count: 3,
      ),
    ])

  let assert Ok(types.WorkItemShowResponse(zero)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.WorkItemShow(
        types.WorkItemShowQuery(ref: types.TaskDisplayId("CARD-ZERO")),
      ),
    )
  assert zero.subtasks == []

  let assert Ok(types.WorkItemShowResponse(one)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.WorkItemShow(
        types.WorkItemShowQuery(ref: types.TaskDisplayId("CARD-ONE")),
      ),
    )
  let assert [only_child] = one.subtasks
  assert only_child.parent == Some(one.summary.source)
  assert only_child.source.display_id == Some("CARD-ONE.1")
  assert only_child.labels
    == [
      task.TaskLabel(id: Some("label-child"), name: "workflow:implementation"),
    ]

  let assert Ok(types.WorkItemShowResponse(many)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.WorkItemShow(
        types.WorkItemShowQuery(ref: types.TaskDisplayId("CARD-MANY")),
      ),
    )
  assert list.length(many.subtasks) == 3
}

pub fn backend_work_item_query_maps_unsupported_capability_test() {
  let tracker_adapter =
    adapter.TrackerAdapter(
      ..fake_tracker_adapter.read_only_adapter(),
      work_items: None,
    )

  let assert Error(types.QueryError(code: code, message: message)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.WorkItemList(types.WorkItemListQuery(
        state_filter: work_item.default_state_filter(),
        search: None,
        sort: work_item.default_sort(),
        limit: 5,
        cursor: None,
      )),
    )

  assert code == types.UnsupportedQuery
  assert message == "tracker adapter does not support work_items"
}

pub fn backend_work_item_redacts_backend_failure_and_bounds_test() {
  let requests = process.new_subject()
  let tracker_adapter =
    adapter.TrackerAdapter(
      ..fake_tracker_adapter.read_only_adapter(),
      work_items: Some(
        adapter.WorkItemReadCapability(
          list_work_items: fn(request) {
            process.send(requests, request)
            Error(adapter.Permanent(
              "invalid JSON payload without RAW_PROVIDER_BODY_SECRET",
            ))
          },
          lookup_work_item: fn(_) { Ok(None) },
        ),
      ),
    )

  let assert Error(types.QueryError(code: code, message: message)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.WorkItemList(types.WorkItemListQuery(
        state_filter: work_item.CategoryWorkItems([task.Ready]),
        search: None,
        sort: work_item.UpdatedDescWorkItems,
        limit: 999,
        cursor: None,
      )),
    )

  let assert Ok(forwarded) = process.receive(requests, within: 1000)
  assert forwarded.limit == work_item.max_page_limit
  assert forwarded.search == None
  assert forwarded.sort == work_item.UpdatedDescWorkItems
  assert forwarded.subtask_limit == work_item.default_list_subtask_limit
  assert forwarded.label_limit == work_item.default_label_limit
  assert code == types.QueryBackendFailed
  assert !string.contains(message, "RAW_PROVIDER_BODY_SECRET")
}

pub fn backend_rejects_invalid_task_cursor_before_querying_adapter_test() {
  let assert Error(types.QueryError(code: code, message: message)) =
    backend.run(
      effective_config(),
      identity(),
      fake_tracker_adapter.read_only_adapter(),
      fn(_) { Ok(False) },
      types.TaskList(types.TaskListQuery(
        states: [],
        limit: 10,
        cursor: Some("linear-raw-cursor"),
      )),
    )

  assert code == types.InvalidCursor
  assert message == "invalid query cursor"
}

pub fn backend_rejects_invalid_work_item_cursor_before_querying_adapter_test() {
  let assert Error(types.QueryError(code: code, message: message)) =
    backend.run(
      effective_config(),
      identity(),
      fake_tracker_adapter.read_only_adapter(),
      fn(_) { Ok(False) },
      types.WorkItemList(types.WorkItemListQuery(
        state_filter: work_item.default_state_filter(),
        search: None,
        sort: work_item.default_sort(),
        limit: 10,
        cursor: Some("linear-raw-cursor"),
      )),
    )

  assert code == types.InvalidCursor
  assert message == "invalid query cursor"
}

fn projection_with_retained_artifacts(
  issue_id issue_id: String,
  issue_identifier issue_identifier: String,
) -> projection.Projection {
  projection.Projection(
    ..projection.new(),
    workflow_runs: dict.from_list([
      #(
        "run-1",
        projection.WorkflowRunFinished(
          workflow_id: "workflow:execplan",
          issue_id: issue_id,
          outcome: "completed",
          token_total: 0,
          turns: 0,
          finished_at_ms: 100,
          run_root: "runs/run-1",
        ),
      ),
    ]),
    workflow_run_provenances: dict.from_list([
      #(
        "run-1",
        projection.WorkflowRunProvenance(
          workflow_id: "workflow:execplan",
          workflow_fingerprint: "wf-1",
          issue_id: issue_id,
          issue_identifier: issue_identifier,
          issue_fingerprint: "issue-fingerprint",
          observed_updated_at_ms: 90,
          run_root: "runs/run-1",
          task_ref: record.linear_task_ref_fields(
            issue_id,
            Some(issue_identifier),
            None,
          ),
        ),
      ),
    ]),
    workflow_output_manifests: dict.from_list([
      #(
        "run-1",
        projection.WorkflowContractManifestRef(
          workflow_id: "workflow:execplan",
          workflow_fingerprint: "wf-1",
          artifact_ref: "artifact://run-1/output.json",
          artifact_sha256: "sha-output",
          artifact_bytes: 128,
          recorded_at_ms: 101,
        ),
      ),
    ]),
  )
}

pub fn backend_work_item_list_defaults_to_active_filter_test() {
  let requests = process.new_subject()
  let tracker_adapter =
    adapter.TrackerAdapter(
      ..fake_tracker_adapter.read_only_adapter(),
      work_items: Some(
        adapter.WorkItemReadCapability(
          list_work_items: fn(request) {
            process.send(requests, request)
            Ok(work_item.WorkItemProviderPage(items: [], has_more: False))
          },
          lookup_work_item: fn(_) { Ok(None) },
        ),
      ),
    )

  let assert Ok(types.WorkItemListResponse(_)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.WorkItemList(types.default_work_item_list_query()),
    )

  let assert Ok(forwarded) = process.receive(requests, within: 1000)
  assert forwarded.state_categories
    == [task.Backlog, task.Ready, task.Active, task.Unknown]
  assert forwarded.search == None
  assert forwarded.sort == work_item.UpdatedDescWorkItems
}

pub fn backend_rejects_mismatched_work_item_cursor_before_querying_adapter_test() {
  let requests = process.new_subject()
  let tracker_adapter =
    adapter.TrackerAdapter(
      ..fake_tracker_adapter.read_only_adapter(),
      work_items: Some(
        adapter.WorkItemReadCapability(
          list_work_items: fn(request) {
            process.send(requests, request)
            Ok(work_item.WorkItemProviderPage(items: [], has_more: False))
          },
          lookup_work_item: fn(_) { Ok(None) },
        ),
      ),
    )

  let bad_cursor =
    cursor.encode_work_item_offset(1, "archive|search:|updated_desc")
  let assert Error(types.QueryError(code: code, message: message)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.WorkItemList(types.WorkItemListQuery(
        state_filter: work_item.ActiveWorkItems,
        search: None,
        sort: work_item.UpdatedDescWorkItems,
        limit: 10,
        cursor: Some(bad_cursor),
      )),
    )

  assert code == types.InvalidCursor
  assert message == "invalid query cursor"
  test_async.assert_no_extra_message(requests)
}

pub fn backend_work_item_list_applies_search_archive_sort_and_cursor_test() {
  let tracker_adapter =
    fake_tracker_adapter.read_only_adapter_with_tasks_and_work_item_details(
      [fake_tracker_adapter.task()],
      searchable_work_item_details(),
    )

  let assert Ok(types.WorkItemListResponse(active_page)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.WorkItemList(types.WorkItemListQuery(
        state_filter: work_item.ActiveWorkItems,
        search: Some("workflow:execplan"),
        sort: work_item.UpdatedDescWorkItems,
        limit: 1,
        cursor: None,
      )),
    )
  let assert [first] = active_page.items
  assert first.source.display_id == Some("CARD-ACTIVE")
  assert active_page.has_more == True
  let assert Some(next_cursor) = active_page.next_cursor

  let assert Ok(types.WorkItemListResponse(second_page)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.WorkItemList(types.WorkItemListQuery(
        state_filter: work_item.ActiveWorkItems,
        search: Some("workflow:execplan"),
        sort: work_item.UpdatedDescWorkItems,
        limit: 1,
        cursor: Some(next_cursor),
      )),
    )
  let assert [second] = second_page.items
  assert second.source.display_id == Some("CARD-READY")
  assert second_page.has_more == False

  let assert Ok(types.WorkItemListResponse(archive_page)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.WorkItemList(types.WorkItemListQuery(
        state_filter: work_item.ArchiveWorkItems,
        search: Some("done"),
        sort: work_item.UpdatedDescWorkItems,
        limit: 10,
        cursor: None,
      )),
    )
  let assert [archived] = archive_page.items
  assert archived.source.display_id == Some("CARD-DONE")

  let assert Ok(types.WorkItemListResponse(miss_page)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.WorkItemList(types.WorkItemListQuery(
        state_filter: work_item.ActiveWorkItems,
        search: Some("missing"),
        sort: work_item.UpdatedDescWorkItems,
        limit: 10,
        cursor: None,
      )),
    )
  assert miss_page.items == []

  assert_active_search_returns_display_id(
    tracker_adapter,
    fake_tracker_adapter.backend_kind <> ":provider-unique",
    "DISPLAY-UNIQUE",
  )
  assert_active_search_returns_display_id(
    tracker_adapter,
    "DISPLAY-UNIQUE",
    "DISPLAY-UNIQUE",
  )
  assert_active_search_returns_display_id(
    tracker_adapter,
    "provider-unique",
    "DISPLAY-UNIQUE",
  )
}

fn assert_active_search_returns_display_id(
  tracker_adapter: adapter.TrackerAdapter,
  search: String,
  expected_display_id: String,
) -> Nil {
  let assert Ok(types.WorkItemListResponse(page)) =
    backend.run(
      effective_config(),
      identity(),
      tracker_adapter,
      fn(_) { Ok(False) },
      types.WorkItemList(types.WorkItemListQuery(
        state_filter: work_item.ActiveWorkItems,
        search: Some(search),
        sort: work_item.UpdatedDescWorkItems,
        limit: 10,
        cursor: None,
      )),
    )
  let assert [item] = page.items
  assert item.source.display_id == Some(expected_display_id)
}

fn searchable_work_item_details() -> List(work_item.WorkItemDetail) {
  [
    work_item.WorkItemDetail(
      summary: build_work_item_summary(
        remote_id: "card-ready",
        display_id: "CARD-READY",
        title: "Ready work item",
        category: task.Ready,
        label_name: "workflow:execplan",
        updated_at_ms: 2000,
      ),
      subtasks: [],
      subtasks_truncated: False,
    ),
    work_item.WorkItemDetail(
      summary: build_work_item_summary(
        remote_id: "card-active",
        display_id: "CARD-ACTIVE",
        title: "Active work item",
        category: task.Active,
        label_name: "workflow:execplan",
        updated_at_ms: 2000,
      ),
      subtasks: [],
      subtasks_truncated: False,
    ),
    work_item.WorkItemDetail(
      summary: build_work_item_summary(
        remote_id: "provider-unique",
        display_id: "DISPLAY-UNIQUE",
        title: "Identifier search item",
        category: task.Active,
        label_name: "identifier-only",
        updated_at_ms: 3000,
      ),
      subtasks: [],
      subtasks_truncated: False,
    ),
    work_item.WorkItemDetail(
      summary: build_work_item_summary(
        remote_id: "card-done",
        display_id: "CARD-DONE",
        title: "Done archive item",
        category: task.Done,
        label_name: "archive",
        updated_at_ms: 1000,
      ),
      subtasks: [],
      subtasks_truncated: False,
    ),
  ]
}

fn build_work_item_summary(
  remote_id remote_id: String,
  display_id display_id: String,
  title title: String,
  category category: task.TaskStateCategory,
  label_name label_name: String,
  updated_at_ms updated_at_ms: Int,
) -> work_item.WorkItemSummary {
  work_item.WorkItemSummary(
    id: fake_tracker_adapter.backend_kind <> ":" <> remote_id,
    source: work_item.WorkItemSource(
      provider: fake_tracker_adapter.backend_kind,
      id: remote_id,
      display_id: Some(display_id),
      url: None,
    ),
    parent: None,
    title: title,
    state: task.TaskState(id: Some(remote_id), name: title, category: category),
    labels: [task.TaskLabel(id: None, name: label_name)],
    labels_truncated: False,
    created_at: None,
    updated_at: Some(birl.from_unix_milli(updated_at_ms)),
    actions: [],
  )
}

fn fake_work_item_detail(
  remote_id remote_id: String,
  display_id display_id: String,
  child_count child_count: Int,
) -> work_item.WorkItemDetail {
  let parent = fake_task(remote_id: remote_id, display_id: display_id)
  work_item.detail_from_task_and_subtasks(
    parent,
    fake_children(display_id, child_count, []),
    work_item.default_label_limit,
    work_item.default_show_subtask_limit,
  )
}

fn fake_task(
  remote_id remote_id: String,
  display_id display_id: String,
) -> task.Task {
  task.Task(
    ..fake_tracker_adapter.task(),
    ref: task.TaskRef(
      backend_kind: fake_tracker_adapter.backend_kind,
      remote_id: remote_id,
      key: Some(display_id),
      url: Some("https://tracker.test/cards/" <> display_id),
    ),
    title: "Fake work item " <> display_id,
  )
}

fn fake_children(
  parent_display_id: String,
  remaining: Int,
  acc: List(task.Task),
) -> List(task.Task) {
  case remaining <= 0 {
    True -> list.reverse(acc)
    False -> {
      let index = int.to_string(remaining)
      let display_id = parent_display_id <> "." <> index
      fake_children(parent_display_id, remaining - 1, [
        task.Task(
          ..fake_task(
            remote_id: parent_display_id <> "-child-" <> index,
            display_id: display_id,
          ),
          labels: [
            task.TaskLabel(
              id: Some("label-child"),
              name: "workflow:implementation",
            ),
          ],
        ),
        ..acc
      ])
    }
  }
}

fn instrumented_work_item_adapter(
  calls: process.Subject(String),
) -> adapter.TrackerAdapter {
  let first =
    work_item.WorkItemSummary(
      id: "test-memory:card-1",
      source: work_item.WorkItemSource(
        provider: fake_tracker_adapter.backend_kind,
        id: "card-1",
        display_id: Some("CARD-1"),
        url: None,
      ),
      parent: None,
      title: "First work item",
      state: task.TaskState(
        id: Some("todo"),
        name: "Todo",
        category: task.Ready,
      ),
      labels: [],
      labels_truncated: False,
      created_at: None,
      updated_at: None,
      actions: [],
    )
  let second =
    work_item.WorkItemSummary(
      ..first,
      id: "test-memory:card-2",
      source: work_item.WorkItemSource(
        provider: fake_tracker_adapter.backend_kind,
        id: "card-2",
        display_id: Some("CARD-2"),
        url: None,
      ),
      title: "Second work item",
    )
  adapter.TrackerAdapter(
    ..fake_tracker_adapter.read_only_adapter(),
    work_items: Some(
      adapter.WorkItemReadCapability(
        list_work_items: fn(request) {
          process.send(calls, "list")
          let items = case request.offset {
            0 -> [first]
            _ -> [second]
          }
          Ok(work_item.WorkItemProviderPage(
            items: items,
            has_more: request.offset == 0,
          ))
        },
        lookup_work_item: fn(_) { Ok(None) },
      ),
    ),
  )
}
