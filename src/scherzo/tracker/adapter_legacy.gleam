import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/error
import scherzo/task
import scherzo/tracker
import scherzo/tracker/adapter
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

pub fn workflow_compat_client(
  tracker_adapter: adapter.TrackerAdapter,
) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() {
      fetch_runtime_candidate_issues(tracker_adapter)
      |> map_runtime_adapter_error
    },
    fetch_issues_by_states: fn(states) {
      fetch_runtime_issues_by_states(tracker_adapter, states)
      |> map_runtime_adapter_error
    },
    fetch_issue_states_by_ids: fn(ids) {
      refresh_runtime_issues_by_ids(tracker_adapter, ids)
      |> map_runtime_adapter_error
    },
  )
}

pub fn fetch_runtime_candidate_issues(
  tracker_adapter: adapter.TrackerAdapter,
) -> Result(List(tracker_issue.Issue), adapter.TrackerError) {
  let request =
    adapter.TaskSearchRequest(
      active_states: [],
      dispatch_states: [],
      terminal_states: [],
      workflow_labels: [],
      limit: 100,
    )
  case tracker_adapter.task_source.fetch_candidates(request) {
    Ok(tasks) -> tasks_to_runtime_issues(tracker_adapter.kind, tasks)
    Error(err) -> Error(err)
  }
}

pub fn fetch_runtime_issues_by_states(
  tracker_adapter: adapter.TrackerAdapter,
  states: List(issue_state.IssueState),
) -> Result(List(tracker_issue.Issue), adapter.TrackerError) {
  let state_names = issue_state.to_strings(states)
  let request =
    adapter.TaskSearchRequest(
      active_states: state_names,
      dispatch_states: state_names,
      terminal_states: [],
      workflow_labels: [],
      limit: 100,
    )
  case tracker_adapter.task_source.fetch_candidates(request) {
    Ok(tasks) -> tasks_to_runtime_issues(tracker_adapter.kind, tasks)
    Error(err) -> Error(err)
  }
}

pub fn refresh_runtime_issues_by_ids(
  tracker_adapter: adapter.TrackerAdapter,
  ids: List(String),
) -> Result(List(tracker_issue.Issue), adapter.TrackerError) {
  let refs =
    list.map(ids, fn(id) {
      task.TaskRef(
        backend_kind: tracker_adapter.kind,
        remote_id: id,
        key: None,
        url: None,
      )
    })
  refresh_runtime_issues_by_refs(tracker_adapter, refs)
}

pub fn refresh_runtime_issues_by_refs(
  tracker_adapter: adapter.TrackerAdapter,
  refs: List(task.TaskRef),
) -> Result(List(tracker_issue.Issue), adapter.TrackerError) {
  case tracker_adapter.task_source.refresh_by_refs(refs) {
    Ok(tasks) -> tasks_to_runtime_issues(tracker_adapter.kind, tasks)
    Error(err) -> Error(err)
  }
}

pub fn lookup_runtime_issue(
  tracker_adapter: adapter.TrackerAdapter,
  operator_ref: String,
) -> Result(Option(tracker_issue.Issue), adapter.TrackerError) {
  case tracker_adapter.task_source.lookup_by_operator_ref(operator_ref) {
    Ok(Some(item)) -> {
      use issue <- try_task_to_runtime_issue(tracker_adapter.kind, item)
      Ok(Some(issue))
    }
    Ok(None) -> Ok(None)
    Error(err) -> Error(err)
  }
}

fn tasks_to_runtime_issues(
  backend_kind: String,
  tasks: List(task.Task),
) -> Result(List(tracker_issue.Issue), adapter.TrackerError) {
  tasks_to_runtime_issues_loop(backend_kind, tasks, [])
}

fn tasks_to_runtime_issues_loop(
  backend_kind: String,
  tasks: List(task.Task),
  acc: List(tracker_issue.Issue),
) -> Result(List(tracker_issue.Issue), adapter.TrackerError) {
  case tasks {
    [] -> Ok(list.reverse(acc))
    [item, ..rest] -> {
      use issue <- try_task_to_runtime_issue(backend_kind, item)
      tasks_to_runtime_issues_loop(backend_kind, rest, [issue, ..acc])
    }
  }
}

fn try_task_to_runtime_issue(
  backend_kind: String,
  item: task.Task,
  next: fn(tracker_issue.Issue) -> Result(a, adapter.TrackerError),
) -> Result(a, adapter.TrackerError) {
  let task.Task(ref: ref, ..) = item
  case ref.backend_kind == backend_kind {
    True -> next(task.to_runtime_issue(item))
    False ->
      Error(adapter.Permanent(
        "tracker adapter returned task for backend "
        <> ref.backend_kind
        <> " while "
        <> backend_kind
        <> " was expected",
      ))
  }
}

pub fn legacy_client(
  tracker_adapter: adapter.TrackerAdapter,
) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() {
      let request =
        adapter.TaskSearchRequest(
          active_states: [],
          dispatch_states: [],
          terminal_states: [],
          workflow_labels: [],
          limit: 100,
        )
      use tasks <- try_tracker_adapter(
        tracker_adapter.task_source.fetch_candidates(request),
      )
      tasks_to_legacy_issues(tasks)
    },
    fetch_issues_by_states: fn(states) {
      let request =
        adapter.TaskSearchRequest(
          active_states: issue_state.to_strings(states),
          dispatch_states: issue_state.to_strings(states),
          terminal_states: [],
          workflow_labels: [],
          limit: 100,
        )
      use tasks <- try_tracker_adapter(
        tracker_adapter.task_source.fetch_candidates(request),
      )
      tasks_to_legacy_issues(tasks)
    },
    fetch_issue_states_by_ids: fn(ids) {
      let refs =
        list.map(ids, fn(id) {
          task.TaskRef(
            backend_kind: tracker_adapter.kind,
            remote_id: id,
            key: None,
            url: None,
          )
        })
      use tasks <- try_tracker_adapter(
        tracker_adapter.task_source.refresh_by_refs(refs),
      )
      tasks_to_legacy_issues(tasks)
    },
  )
}

pub fn adapter_from_legacy_client(
  client: tracker.Client,
  kind: String,
) -> adapter.TrackerAdapter {
  adapter.TrackerAdapter(
    kind: kind,
    display_name: kind,
    task_source: task_source_from_legacy_client(client),
    comments: None,
    remote_commands: None,
    state_transitions: None,
    routing_metadata: Some(
      adapter.RoutingMetadataCapability(
        workflow_labels: fn(value) { task.label_names(value) },
        blocker_refs: fn(value) { value.blockers },
      ),
    ),
    links: None,
    handoff: None,
    scheduled_failures: None,
    readiness: None,
    smoke: None,
    attachments: None,
  )
}

pub fn task_source_from_legacy_client(
  client: tracker.Client,
) -> adapter.TaskSourceCapability {
  adapter.TaskSourceCapability(
    fetch_candidates: fn(_) {
      use issues <- try_legacy(client.fetch_candidate_issues())
      Ok(list.map(issues, task.from_legacy_issue))
    },
    refresh_by_refs: fn(refs) {
      let ids = list.map(refs, fn(ref) { ref.remote_id })
      use issues <- try_legacy(client.fetch_issue_states_by_ids(ids))
      Ok(list.map(issues, task.from_legacy_issue))
    },
    lookup_by_operator_ref: fn(operator_ref) {
      use issues <- try_legacy(client.fetch_issue_states_by_ids([operator_ref]))
      case issues {
        [] -> Ok(None)
        [issue, ..] -> Ok(Some(task.from_legacy_issue(issue)))
      }
    },
    list_tasks: fn(request) { legacy_list_tasks(client, request) },
    lookup_task_detail: fn(ref) { legacy_lookup_task_detail(client, ref) },
  )
}

fn legacy_list_tasks(
  client: tracker.Client,
  request: adapter.TaskListRequest,
) -> Result(adapter.TaskPage, adapter.TrackerError) {
  use issues <- try_legacy(client.fetch_candidate_issues())
  let tasks =
    issues
    |> list.map(task.from_legacy_issue)
    |> list.map(categorize_legacy_task)
    |> filter_state_categories(request.state_categories)
  let remaining = drop_first(tasks, request.offset)
  Ok(adapter.TaskPage(
    items: take_first(remaining, request.limit),
    has_more: list.length(remaining) > request.limit,
  ))
}

fn legacy_lookup_task_detail(
  client: tracker.Client,
  ref: adapter.TaskLookupRef,
) -> Result(Option(task.Task), adapter.TrackerError) {
  let operator_ref = case ref {
    adapter.TaskLookupByDisplayId(value) -> value
    adapter.TaskLookupByRemoteId(id: value, ..) -> value
  }
  use issues <- try_legacy(client.fetch_issue_states_by_ids([operator_ref]))
  case issues {
    [] -> Ok(None)
    [issue, ..] ->
      Ok(Some(categorize_legacy_task(task.from_legacy_issue(issue))))
  }
}

fn categorize_legacy_task(item: task.Task) -> task.Task {
  let state = item.state
  task.Task(
    ..item,
    state: task.TaskState(
      id: state.id,
      name: state.name,
      category: legacy_state_category(state.name),
    ),
  )
}

fn filter_state_categories(
  tasks: List(task.Task),
  categories: List(task.TaskStateCategory),
) -> List(task.Task) {
  case categories {
    [] -> tasks
    categories ->
      list.filter(tasks, fn(item) {
        list.contains(categories, item.state.category)
      })
  }
}

fn legacy_state_category(name: String) -> task.TaskStateCategory {
  let name = name |> string.trim |> string.lowercase
  case name {
    "backlog" -> task.Backlog
    "todo" | "to do" | "ready" | "triage" -> task.Ready
    "in progress" | "doing" | "started" -> task.Active
    "done" | "complete" | "completed" -> task.Done
    "canceled" | "cancelled" -> task.Canceled
    "duplicate" -> task.Duplicate
    _ -> task.Unknown
  }
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

fn tasks_to_legacy_issues(
  tasks: List(task.Task),
) -> Result(List(tracker_issue.Issue), error.TrackerError) {
  tasks_to_legacy_issues_loop(tasks, [])
}

fn tasks_to_legacy_issues_loop(
  tasks: List(task.Task),
  acc: List(tracker_issue.Issue),
) -> Result(List(tracker_issue.Issue), error.TrackerError) {
  case tasks {
    [] -> Ok(list.reverse(acc))
    [item, ..rest] ->
      case task.to_legacy_issue(item) {
        Ok(issue) -> tasks_to_legacy_issues_loop(rest, [issue, ..acc])
        Error(_) ->
          Error(error.LinearUnknownPayload(
            "tracker adapter returned non-Linear task for legacy issue path",
          ))
      }
  }
}

fn try_tracker_adapter(
  result: Result(a, adapter.TrackerError),
  next: fn(a) -> Result(b, error.TrackerError),
) -> Result(b, error.TrackerError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(map_adapter_error(err))
  }
}

fn try_legacy(
  result: Result(a, error.TrackerError),
  next: fn(a) -> Result(b, adapter.TrackerError),
) -> Result(b, adapter.TrackerError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(map_legacy_error(err))
  }
}

fn map_runtime_adapter_error(
  result: Result(a, adapter.TrackerError),
) -> Result(a, error.TrackerError) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) -> Error(map_adapter_error(err))
  }
}

fn map_adapter_error(err: adapter.TrackerError) -> error.TrackerError {
  error.LinearApiRequest(adapter_error_message(err))
}

fn map_legacy_error(err: error.TrackerError) -> adapter.TrackerError {
  case err {
    error.LinearApiRequest(message) -> adapter.Permanent(message)
    error.LinearApiStatus(status) ->
      adapter.Permanent("Linear API returned status " <> int.to_string(status))
    error.LinearGraphqlErrors(message) -> adapter.Permanent(message)
    error.LinearUnknownPayload(message) -> adapter.DecodeFailed(message)
    error.LinearMissingEndCursor -> adapter.DecodeFailed("missing cursor")
    error.LinearUploadStatus(status) ->
      adapter.Permanent(
        "Linear upload returned status " <> int.to_string(status),
      )
    error.LinearAttachmentError(message) -> adapter.Permanent(message)
  }
}

pub fn adapter_error_message(err: adapter.TrackerError) -> String {
  case err {
    adapter.Unauthorized(message) -> message
    adapter.NotFound(ref) -> "task not found: " <> ref.remote_id
    adapter.Transient(message) -> message
    adapter.Permanent(message) -> message
    adapter.UnsupportedCapability(capability) ->
      "unsupported tracker capability: " <> capability
    adapter.DecodeFailed(message) -> message
  }
}
