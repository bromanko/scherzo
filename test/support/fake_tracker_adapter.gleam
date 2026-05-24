import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/task
import scherzo/tracker/adapter

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
  adapter.TrackerAdapter(
    kind: backend_kind,
    display_name: "Test memory tracker",
    task_source: task_source_capability([task()]),
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
    state_transitions: Some(state_transition_capability()),
    routing_metadata: Some(routing_metadata_capability()),
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

fn matches_operator_ref(ref: task.TaskRef, operator_ref: String) -> Bool {
  let task.TaskRef(remote_id: remote_id, key: key, ..) = ref
  remote_id == operator_ref || option_equals(key, operator_ref)
}

fn same_ref(left: task.TaskRef, right: task.TaskRef) -> Bool {
  task.identity(left) == task.identity(right)
}

fn option_equals(value: Option(String), expected: String) -> Bool {
  case value {
    Some(value) -> value == expected
    None -> False
  }
}

fn comment_capability() -> adapter.CommentCapability {
  adapter.CommentCapability(post_or_update: fn(request) {
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
  })
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
