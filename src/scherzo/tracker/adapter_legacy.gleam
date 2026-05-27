import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/error
import scherzo/task
import scherzo/tracker
import scherzo/tracker/adapter
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

pub fn workflow_compat_client(
  tracker_adapter: adapter.TrackerAdapter,
) -> tracker.Client {
  legacy_client(tracker_adapter)
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
  )
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
