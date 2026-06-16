import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/task
import scherzo/tracker/adapter
import scherzo/work_item

pub const backend_kind = "test-memory"

pub fn task_ref() -> task.TaskRef {
  task.TaskRef(
    backend_kind: backend_kind,
    remote_id: "card-1",
    key: Some("CARD-1"),
    url: Some("https://tracker.test/cards/CARD-1"),
  )
}

pub fn task() -> task.Task {
  task.Task(
    ref: task_ref(),
    title: "Fake non-Linear card",
    description: Some("Prove the adapter seam without Linear types"),
    priority: Some(3),
    state: task.TaskState(id: Some("todo"), name: "Todo", category: task.Ready),
    branch_hint: Some("card-1-fake-seam"),
    labels: [
      task.TaskLabel(id: Some("label-workflow"), name: "workflow:execplan"),
      task.TaskLabel(id: Some("label-kind"), name: "kind:test"),
    ],
    blockers: [],
    blockers_complete: True,
    created_at: None,
    updated_at: None,
  )
}

pub fn read_only_adapter() -> adapter.TrackerAdapter {
  read_only_adapter_with_tasks([task()])
}

pub fn read_only_adapter_with_tasks(
  tasks: List(task.Task),
) -> adapter.TrackerAdapter {
  read_only_adapter_with_work_item_details(tasks, default_work_item_details())
}

pub fn read_only_adapter_with_work_item_details(
  tasks: List(task.Task),
  details: List(work_item.WorkItemDetail),
) -> adapter.TrackerAdapter {
  adapter.TrackerAdapter(
    kind: backend_kind,
    display_name: "Test memory tracker",
    task_source: task_source_capability(tasks),
    work_items: Some(work_item_read_capability(details)),
    comments: None,
    remote_commands: None,
    state_transitions: None,
    routing_metadata: None,
    links: None,
    handoff: None,
    scheduled_failures: None,
    readiness: None,
    smoke: None,
    attachments: None,
  )
}

pub fn seam_adapter() -> adapter.TrackerAdapter {
  adapter.TrackerAdapter(
    ..read_only_adapter(),
    comments: Some(comment_capability()),
    remote_commands: Some(remote_command_capability()),
    state_transitions: Some(state_transition_capability()),
    routing_metadata: Some(routing_metadata_capability()),
    handoff: Some(handoff_capability()),
    scheduled_failures: Some(scheduled_failure_capability()),
  )
}

fn task_source_capability(
  tasks: List(task.Task),
) -> adapter.TaskSourceCapability {
  adapter.TaskSourceCapability(
    fetch_candidates: fn(request) {
      Ok(
        list.filter(tasks, fn(candidate) { matches_search(candidate, request) }),
      )
    },
    refresh_by_refs: fn(refs) {
      Ok(
        list.filter(tasks, fn(candidate) {
          list.any(refs, fn(ref) { same_ref(candidate.ref, ref) })
        }),
      )
    },
    lookup_by_operator_ref: fn(operator_ref) {
      let operator_ref = string.trim(operator_ref)
      case operator_ref == "" {
        True -> Ok(None)
        False ->
          case
            list.filter(tasks, fn(candidate) {
              matches_operator_ref(candidate.ref, operator_ref)
            })
          {
            [] -> Ok(None)
            [first, ..] -> Ok(Some(first))
          }
      }
    },
    list_tasks: fn(request) {
      let matching =
        list.filter(tasks, fn(candidate) {
          matches_state_categories(candidate, request.state_categories)
        })
      let remaining = drop_first(matching, request.offset)
      Ok(adapter.TaskPage(
        items: take_first(remaining, request.limit),
        has_more: list.length(remaining) > request.limit,
      ))
    },
    lookup_task_detail: fn(ref) {
      let matches =
        list.filter(tasks, fn(candidate) {
          matches_lookup_ref(candidate.ref, ref)
        })
      case matches {
        [] -> Ok(None)
        [first, ..] -> Ok(Some(first))
      }
    },
  )
}

fn matches_search(
  candidate: task.Task,
  request: adapter.TaskSearchRequest,
) -> Bool {
  let state_matches = case request.dispatch_states {
    [] -> True
    states -> list.contains(states, candidate.state.name)
  }
  let label_matches = case request.workflow_labels {
    [] -> True
    labels ->
      task.label_names(candidate)
      |> list.any(fn(label) { list.contains(labels, label) })
  }

  state_matches && label_matches
}

fn matches_state_categories(
  candidate: task.Task,
  categories: List(task.TaskStateCategory),
) -> Bool {
  case categories {
    [] -> True
    categories -> list.contains(categories, candidate.state.category)
  }
}

fn matches_lookup_ref(
  ref: task.TaskRef,
  lookup: adapter.TaskLookupRef,
) -> Bool {
  case lookup {
    adapter.TaskLookupByDisplayId(value) -> option_equals(ref.key, value)
    adapter.TaskLookupByRemoteId(provider: provider, id: value) ->
      provider_matches(ref.backend_kind, provider) && ref.remote_id == value
  }
}

fn matches_operator_ref(ref: task.TaskRef, operator_ref: String) -> Bool {
  let task.TaskRef(remote_id: remote_id, key: key, ..) = ref
  remote_id == operator_ref || option_equals(key, operator_ref)
}

fn provider_matches(provider: String, expected: Option(String)) -> Bool {
  case expected {
    Some(expected) -> provider == expected
    None -> True
  }
}

fn same_ref(left: task.TaskRef, right: task.TaskRef) -> Bool {
  task.identity(left) == task.identity(right)
}

fn drop_first(values: List(a), count: Int) -> List(a) {
  case count <= 0, values {
    True, _ -> values
    _, [] -> []
    False, [_, ..rest] -> drop_first(rest, count - 1)
  }
}

fn take_first(values: List(a), count: Int) -> List(a) {
  case count <= 0, values {
    True, _ -> []
    _, [] -> []
    False, [first, ..rest] -> [first, ..take_first(rest, count - 1)]
  }
}

fn option_equals(value: Option(String), expected: String) -> Bool {
  case value {
    Some(value) -> value == expected
    None -> False
  }
}

pub fn parent_task() -> task.Task {
  task()
}

pub fn child_task(
  remote_id remote_id: String,
  display_id display_id: String,
) -> task.Task {
  task.Task(
    ..task(),
    ref: task.TaskRef(
      backend_kind: backend_kind,
      remote_id: remote_id,
      key: Some(display_id),
      url: Some("https://tracker.test/cards/" <> display_id),
    ),
    title: "Child card " <> display_id,
  )
}

pub fn default_work_item_details() -> List(work_item.WorkItemDetail) {
  [
    work_item.detail_from_task_and_subtasks(
      parent_task(),
      [
        child_task(remote_id: "card-1-child-1", display_id: "CARD-1.1"),
        child_task(remote_id: "card-1-child-2", display_id: "CARD-1.2"),
      ],
      work_item.default_label_limit,
      work_item.default_show_subtask_limit,
    ),
  ]
}

fn work_item_read_capability(
  details: List(work_item.WorkItemDetail),
) -> adapter.WorkItemReadCapability {
  adapter.WorkItemReadCapability(
    list_work_items: fn(request) { list_work_items(details, request) },
    lookup_work_item: fn(request) { lookup_work_item(details, request) },
  )
}

fn list_work_items(
  details: List(work_item.WorkItemDetail),
  request: work_item.WorkItemListRequest,
) -> Result(work_item.WorkItemProviderPage, adapter.TrackerError) {
  details
  |> list.map(fn(detail) { detail.summary })
  |> work_item.apply_list_request(request)
  |> Ok
}

fn lookup_work_item(
  details: List(work_item.WorkItemDetail),
  request: work_item.WorkItemShowRequest,
) -> Result(Option(work_item.WorkItemDetail), adapter.TrackerError) {
  let matches =
    list.filter(details, fn(detail) {
      matches_work_item_lookup_ref(detail.summary.source, request.ref)
    })
  case matches {
    [] -> Ok(None)
    [first, ..] -> Ok(Some(first))
  }
}

fn matches_work_item_lookup_ref(
  source: work_item.WorkItemSource,
  lookup: work_item.WorkItemLookupRef,
) -> Bool {
  case lookup {
    work_item.WorkItemLookupByDisplayId(value) ->
      option_equals(source.display_id, value)
    work_item.WorkItemLookupByRemoteId(provider: provider, id: value) ->
      provider_matches(source.provider, provider) && source.id == value
  }
}

fn comment_capability() -> adapter.CommentCapability {
  adapter.CommentCapability(
    post_or_update: fn(request) {
      let adapter.CommentRequest(task: requested_task, mode: mode, ..) = request
      case mode {
        adapter.CreateOnly ->
          Ok(adapter.CommentReceipt(
            id: "fake-comment-1",
            task: requested_task,
            url: requested_task.url,
            created: True,
          ))
        adapter.UpdateExisting(comment_id: comment_id, ..) ->
          Ok(adapter.CommentReceipt(
            id: comment_id,
            task: requested_task,
            url: requested_task.url,
            created: False,
          ))
      }
    },
    find_by_marker: fn(request) {
      let adapter.CommentLookup(task: requested_task, ..) = request
      Ok(
        Some(adapter.CommentReceipt(
          id: "fake-comment-1",
          task: requested_task,
          url: requested_task.url,
          created: False,
        )),
      )
    },
  )
}

fn state_transition_capability() -> adapter.StateTransitionCapability {
  adapter.StateTransitionCapability(transition: fn(request) {
    let adapter.StateTransitionRequest(
      task: requested_task,
      target_state_id: target_state_id,
      target_state_name: target_state_name,
      ..,
    ) = request

    Ok(adapter.StateTransitionReceipt(
      task: requested_task,
      state: task.TaskState(
        id: target_state_id,
        name: target_state_name,
        category: state_category(target_state_name),
      ),
    ))
  })
}

fn state_category(name: String) -> task.TaskStateCategory {
  case name {
    "Backlog" -> task.Backlog
    "Ready" -> task.Ready
    "Todo" -> task.Ready
    "In Progress" -> task.Active
    "Done" -> task.Done
    "Canceled" -> task.Canceled
    "Duplicate" -> task.Duplicate
    _ -> task.Unknown
  }
}

fn remote_command_capability() -> adapter.RemoteCommandCapability {
  adapter.RemoteCommandCapability(
    fetch_events: fn(fetch) {
      let event =
        adapter.RemoteCommandEvent(
          event_id: "fake-command-1",
          task: task_ref(),
          author_id: "fake-user",
          body: "/scherzo status",
          command_name: "status",
          excerpt: "status",
          observed_at_ms: 42,
        )
      case list.any(fetch.task_refs, fn(ref) { same_ref(ref, task_ref()) }) {
        False -> Ok([])
        True ->
          case list.contains(fetch.since_event_ids, event.event_id) {
            True -> Ok([])
            False -> Ok([event])
          }
      }
    },
    post_ack: fn(ack) {
      Ok(adapter.CommentReceipt(
        id: "fake-ack-" <> ack.event.event_id,
        task: ack.event.task,
        url: ack.event.task.url,
        created: True,
      ))
    },
  )
}

fn handoff_capability() -> adapter.HandoffCapability {
  adapter.HandoffCapability(report: fn(event) {
    let ref = handoff_task_ref(event)
    case ref.backend_kind == backend_kind {
      True -> Ok(Nil)
      False ->
        Error(adapter.Permanent(
          "handoff event used unexpected backend " <> ref.backend_kind,
        ))
    }
  })
}

fn handoff_task_ref(event: adapter.HandoffEvent) -> task.TaskRef {
  case event {
    adapter.HandoffClaim(task: item, ..)
    | adapter.HandoffSuccess(task: item, ..)
    | adapter.HandoffFailure(task: item, ..) -> item.ref
    adapter.HandoffPark(report) -> report.task
  }
}

fn routing_metadata_capability() -> adapter.RoutingMetadataCapability {
  adapter.RoutingMetadataCapability(
    workflow_labels: fn(value) { task.label_names(value) },
    blocker_refs: fn(value) { value.blockers },
  )
}

fn scheduled_failure_capability() -> adapter.ScheduledFailureCapability {
  adapter.ScheduledFailureCapability(publish: fn(publication) {
    Ok(adapter.ScheduledFailureReceipt(
      task: task.TaskRef(
        backend_kind: backend_kind,
        remote_id: "scheduled-" <> publication.dedupe_key,
        key: Some(publication.job_id),
        url: None,
      ),
      created: True,
      comment_id: Some("scheduled-comment-" <> publication.run_id),
    ))
  })
}
