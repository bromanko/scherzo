import gleam/int
import gleam/option.{None, Some}
import gleam/string
import scherzo/task
import scherzo/tracker/issue as tracker_issue

pub opaque type TaskIdentity {
  TaskIdentity(String)
}

pub opaque type IssueId {
  IssueId(String)
}

pub opaque type RunId {
  RunId(String)
}

pub opaque type SessionId {
  SessionId(String)
}

pub fn task_ref(ref: task.TaskRef) -> TaskIdentity {
  let #(backend_kind, remote_id) = task.identity(ref)
  TaskIdentity(
    encode_identity_component(backend_kind)
    <> "|"
    <> encode_identity_component(remote_id),
  )
}

pub fn task(item: task.Task) -> TaskIdentity {
  let task.Task(ref: ref, ..) = item
  task_ref(ref)
}

pub fn issue(issue: tracker_issue.Issue) -> TaskIdentity {
  issue_ref(issue) |> task_ref
}

pub fn issue_for_backend(
  issue: tracker_issue.Issue,
  backend_kind: String,
) -> TaskIdentity {
  issue_ref_for_backend(issue, backend_kind) |> task_ref
}

pub fn linear_issue_id(issue_id: String) -> TaskIdentity {
  issue_id_for_backend(issue_id, "linear")
}

pub fn issue_id_for_backend(
  issue_id: String,
  backend_kind: String,
) -> TaskIdentity {
  issue_id_ref_for_backend(issue_id, backend_kind) |> task_ref
}

pub fn issue_ref(issue: tracker_issue.Issue) -> task.TaskRef {
  issue_ref_for_backend(issue, "linear")
}

pub fn issue_ref_for_backend(
  issue: tracker_issue.Issue,
  backend_kind: String,
) -> task.TaskRef {
  task.TaskRef(
    backend_kind: backend_kind,
    remote_id: issue.id,
    key: Some(issue.identifier),
    url: issue.url,
  )
}

pub fn linear_issue_id_ref(issue_id: String) -> task.TaskRef {
  issue_id_ref_for_backend(issue_id, "linear")
}

pub fn issue_id_ref_for_backend(
  issue_id: String,
  backend_kind: String,
) -> task.TaskRef {
  task.TaskRef(
    backend_kind: backend_kind,
    remote_id: issue_id,
    key: None,
    url: None,
  )
}

pub fn issue_id_from_string(value: String) -> IssueId {
  IssueId(value)
}

pub fn issue_id_to_string(value: IssueId) -> String {
  let IssueId(raw) = value
  raw
}

pub fn run_id_from_string(value: String) -> RunId {
  RunId(value)
}

pub fn run_id_to_string(value: RunId) -> String {
  let RunId(raw) = value
  raw
}

pub fn session_id_from_string(value: String) -> SessionId {
  SessionId(value)
}

pub fn session_id_to_string(value: SessionId) -> String {
  let SessionId(raw) = value
  raw
}

pub fn to_string(value: TaskIdentity) -> String {
  let TaskIdentity(raw) = value
  raw
}

fn encode_identity_component(value: String) -> String {
  int.to_string(string.length(value)) <> ":" <> value
}
