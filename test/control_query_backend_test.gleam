import gleam/option.{None, Some}
import scherzo/config
import scherzo/config/types as config_types
import scherzo/control/query/backend
import scherzo/control/query/types
import scherzo/daemon_identity
import scherzo/task
import scherzo/tracker/adapter
import support/fake_tracker_adapter

fn effective_config() -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    tracker: config.default_tracker_config(),
    polling: config.default_polling_config(),
    workspace: config_types.WorkspaceConfig(root: "."),
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
