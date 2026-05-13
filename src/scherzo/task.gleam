import birl.{type Time}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

pub type TaskRef {
  TaskRef(
    backend_kind: String,
    remote_id: String,
    key: Option(String),
    url: Option(String),
  )
}

pub type TaskStateCategory {
  Backlog
  Ready
  Active
  Done
  Canceled
  Duplicate
  Unknown
}

pub type TaskState {
  TaskState(id: Option(String), name: String, category: TaskStateCategory)
}

pub type TaskLabel {
  TaskLabel(id: Option(String), name: String)
}

pub type TaskComment {
  TaskComment(
    id: String,
    task: TaskRef,
    author_id: Option(String),
    body: String,
    created_at: Option(Time),
    updated_at: Option(Time),
  )
}

pub type TaskAttachment {
  TaskAttachment(id: String, task: TaskRef, name: String, url: String)
}

pub type TaskLink {
  TaskLink(id: Option(String), task: TaskRef, title: String, url: String)
}

pub type Task {
  Task(
    ref: TaskRef,
    title: String,
    description: Option(String),
    priority: Option(Int),
    state: TaskState,
    branch_hint: Option(String),
    labels: List(TaskLabel),
    blockers: List(TaskRef),
    blockers_complete: Bool,
    created_at: Option(Time),
    updated_at: Option(Time),
  )
}

pub type TaskConversionError {
  RequiresLinearTask
  MissingTaskKey
}

pub fn display_key(ref: TaskRef) -> String {
  let TaskRef(remote_id: remote_id, key: key, ..) = ref
  case non_empty(key) {
    Some(key) -> key
    None -> remote_id
  }
}

pub fn label_names(task: Task) -> List(String) {
  let Task(labels: labels, ..) = task
  list.map(labels, fn(label) {
    let TaskLabel(name: name, ..) = label
    name
  })
}

pub fn from_legacy_issue(issue: tracker_issue.Issue) -> Task {
  let tracker_issue.Issue(
    id: id,
    identifier: identifier,
    title: title,
    description: description,
    priority: priority,
    state: state,
    branch_name: branch_name,
    url: url,
    labels: labels,
    blocked_by: blocked_by,
    blocked_by_complete: blocked_by_complete,
    created_at: created_at,
    updated_at: updated_at,
  ) = issue

  Task(
    ref: TaskRef(
      backend_kind: "linear",
      remote_id: id,
      key: Some(identifier),
      url: url,
    ),
    title: title,
    description: description,
    priority: priority,
    state: TaskState(
      id: None,
      name: issue_state.to_string(state),
      category: Unknown,
    ),
    branch_hint: branch_name,
    labels: list.map(labels, fn(name) { TaskLabel(id: None, name: name) }),
    blockers: list.filter_map(blocked_by, legacy_blocker_to_task_ref),
    blockers_complete: blocked_by_complete,
    created_at: created_at,
    updated_at: updated_at,
  )
}

pub fn to_legacy_issue(
  task: Task,
) -> Result(tracker_issue.Issue, TaskConversionError) {
  let Task(
    ref: ref,
    title: title,
    description: description,
    priority: priority,
    state: state,
    branch_hint: branch_hint,
    labels: labels,
    blockers: blockers,
    blockers_complete: blockers_complete,
    created_at: created_at,
    updated_at: updated_at,
  ) = task
  let TaskRef(
    backend_kind: backend_kind,
    remote_id: remote_id,
    key: key,
    url: url,
  ) = ref

  case backend_kind == "linear", non_empty(key) {
    False, _ -> Error(RequiresLinearTask)
    True, None -> Error(MissingTaskKey)
    True, Some(identifier) ->
      Ok(tracker_issue.Issue(
        id: remote_id,
        identifier: identifier,
        title: title,
        description: description,
        priority: priority,
        state: issue_state.from_string_unchecked(state.name),
        branch_name: branch_hint,
        url: url,
        labels: list.map(labels, fn(label) {
          let TaskLabel(name: name, ..) = label
          name
        }),
        blocked_by: list.filter_map(blockers, task_ref_to_legacy_blocker),
        blocked_by_complete: blockers_complete,
        created_at: created_at,
        updated_at: updated_at,
      ))
  }
}

fn legacy_blocker_to_task_ref(
  blocker: tracker_issue.BlockerRef,
) -> Result(TaskRef, Nil) {
  let tracker_issue.BlockerRef(id: id, identifier: identifier, ..) = blocker
  case blocker_remote_id(id, identifier) {
    Some(remote_id) ->
      Ok(TaskRef(
        backend_kind: "linear",
        remote_id: remote_id,
        key: non_empty(identifier),
        url: None,
      ))
    None -> Error(Nil)
  }
}

fn task_ref_to_legacy_blocker(
  ref: TaskRef,
) -> Result(tracker_issue.BlockerRef, Nil) {
  let TaskRef(remote_id: remote_id, key: key, ..) = ref
  case non_empty(Some(remote_id)), non_empty(key) {
    Some(remote_id), key ->
      Ok(tracker_issue.BlockerRef(
        id: Some(remote_id),
        identifier: key,
        state: None,
      ))
    None, Some(key) ->
      Ok(tracker_issue.BlockerRef(id: None, identifier: Some(key), state: None))
    None, None -> Error(Nil)
  }
}

fn blocker_remote_id(
  id: Option(String),
  identifier: Option(String),
) -> Option(String) {
  case non_empty(id), non_empty(identifier) {
    Some(id), _ -> Some(id)
    None, Some(identifier) -> Some(identifier)
    None, None -> None
  }
}

fn non_empty(value: Option(String)) -> Option(String) {
  case value {
    Some(value) -> {
      let trimmed = string.trim(value)
      case trimmed == "" {
        True -> None
        False -> Some(trimmed)
      }
    }
    None -> None
  }
}
