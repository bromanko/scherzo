import gleam/dict.{type Dict}
import gleam/list
import scherzo/runtime/identity
import scherzo/task
import scherzo/tracker/issue as tracker_issue

pub type TaskLifecycle {
  Idle(task_ref: task.TaskRef)
  Validating(
    task_ref: task.TaskRef,
    issue: tracker_issue.Issue,
    generation: Int,
  )
  Claiming(
    task_ref: task.TaskRef,
    issue: tracker_issue.Issue,
    run_id: String,
    session_id: String,
  )
  Starting(
    task_ref: task.TaskRef,
    issue: tracker_issue.Issue,
    run_id: String,
    session_id: String,
    workspace_path: String,
  )
  Running(
    task_ref: task.TaskRef,
    issue: tracker_issue.Issue,
    run_id: String,
    session_id: String,
    workspace_path: String,
  )
  RetryWaiting(
    task_ref: task.TaskRef,
    issue_id: String,
    generation: Int,
    delay_ms: Int,
  )
  RetryRefreshing(
    task_ref: task.TaskRef,
    issue_id: String,
    generation: Int,
    delay_ms: Int,
  )
  Stopping(
    task_ref: task.TaskRef,
    issue: tracker_issue.Issue,
    run_id: String,
    session_id: String,
    workspace_path: String,
    reason: String,
  )
  Parked(task_ref: task.TaskRef, issue_id: String, identifier: String)
  Completed(task_ref: task.TaskRef, issue: tracker_issue.Issue)
}

pub type LifecycleError {
  ConflictingLifecycleSources(
    task_identity: identity.TaskIdentity,
    existing: TaskLifecycle,
    incoming: TaskLifecycle,
  )
  MissingClaimedLifecycle(task_identity: identity.TaskIdentity)
  MissingRetryWaitingForRefresh(
    task_identity: identity.TaskIdentity,
    generation: Int,
  )
  RunningWorkerMismatch(task_identity: identity.TaskIdentity)
}

pub type Counts {
  Counts(
    validating: Int,
    claiming: Int,
    starting: Int,
    running: Int,
    retry_waiting: Int,
    retry_refreshing: Int,
    stopping: Int,
    parked: Int,
    completed: Int,
  )
}

pub opaque type TaskDirectory {
  TaskDirectory(entries: Dict(identity.TaskIdentity, TaskLifecycle))
}

pub fn new() -> TaskDirectory {
  TaskDirectory(dict.new())
}

pub fn put(
  directory: TaskDirectory,
  lifecycle: TaskLifecycle,
) -> Result(TaskDirectory, LifecycleError) {
  let task_identity = lifecycle_identity(lifecycle)
  case dict.get(directory.entries, task_identity) {
    Error(Nil) ->
      Ok(
        TaskDirectory(dict.insert(directory.entries, task_identity, lifecycle)),
      )
    Ok(existing) ->
      case existing == lifecycle {
        True -> Ok(directory)
        False ->
          Error(ConflictingLifecycleSources(task_identity, existing, lifecycle))
      }
  }
}

pub fn replace(
  directory: TaskDirectory,
  lifecycle: TaskLifecycle,
) -> TaskDirectory {
  let task_identity = lifecycle_identity(lifecycle)
  TaskDirectory(dict.insert(directory.entries, task_identity, lifecycle))
}

pub fn delete(
  directory: TaskDirectory,
  task_identity: identity.TaskIdentity,
) -> TaskDirectory {
  TaskDirectory(dict.delete(directory.entries, task_identity))
}

pub fn get(
  directory: TaskDirectory,
  task_identity: identity.TaskIdentity,
) -> Result(TaskLifecycle, Nil) {
  dict.get(directory.entries, task_identity)
}

pub fn entries(
  directory: TaskDirectory,
) -> List(#(identity.TaskIdentity, TaskLifecycle)) {
  dict.to_list(directory.entries)
}

pub fn size(directory: TaskDirectory) -> Int {
  dict.size(directory.entries)
}

pub fn counts(directory: TaskDirectory) -> Counts {
  list.fold(entries(directory), empty_counts(), fn(counts, entry) {
    let #(_, lifecycle) = entry
    count_lifecycle(counts, lifecycle)
  })
}

pub fn empty_counts() -> Counts {
  Counts(
    validating: 0,
    claiming: 0,
    starting: 0,
    running: 0,
    retry_waiting: 0,
    retry_refreshing: 0,
    stopping: 0,
    parked: 0,
    completed: 0,
  )
}

pub fn is_active_or_pending(lifecycle: TaskLifecycle) -> Bool {
  case lifecycle {
    Idle(_) | Completed(_, _) -> False
    Validating(_, _, _)
    | Claiming(_, _, _, _)
    | Starting(_, _, _, _, _)
    | Running(_, _, _, _, _)
    | RetryWaiting(_, _, _, _)
    | RetryRefreshing(_, _, _, _)
    | Stopping(_, _, _, _, _, _)
    | Parked(_, _, _) -> True
  }
}

pub fn consumes_dispatch_slot(lifecycle: TaskLifecycle) -> Bool {
  case lifecycle {
    Validating(_, _, _)
    | Claiming(_, _, _, _)
    | Starting(_, _, _, _, _)
    | Running(_, _, _, _, _)
    | Stopping(_, _, _, _, _, _) -> True
    RetryWaiting(_, _, _, _)
    | RetryRefreshing(_, _, _, _)
    | Parked(_, _, _)
    | Completed(_, _)
    | Idle(_) -> False
  }
}

pub fn holds_tracker_claim(lifecycle: TaskLifecycle) -> Bool {
  case lifecycle {
    Claiming(_, _, _, _)
    | Starting(_, _, _, _, _)
    | Running(_, _, _, _, _)
    | RetryWaiting(_, _, _, _)
    | RetryRefreshing(_, _, _, _)
    | Stopping(_, _, _, _, _, _) -> True
    Validating(_, _, _) | Parked(_, _, _) | Completed(_, _) | Idle(_) -> False
  }
}

pub fn has_live_worker(lifecycle: TaskLifecycle) -> Bool {
  case lifecycle {
    Starting(_, _, _, _, _)
    | Running(_, _, _, _, _)
    | Stopping(_, _, _, _, _, _) -> True
    Validating(_, _, _)
    | Claiming(_, _, _, _)
    | RetryWaiting(_, _, _, _)
    | RetryRefreshing(_, _, _, _)
    | Parked(_, _, _)
    | Completed(_, _)
    | Idle(_) -> False
  }
}

pub fn is_retry_waiting(lifecycle: TaskLifecycle) -> Bool {
  case lifecycle {
    RetryWaiting(_, _, _, _) -> True
    _ -> False
  }
}

pub fn is_retry_refreshing(lifecycle: TaskLifecycle) -> Bool {
  case lifecycle {
    RetryRefreshing(_, _, _, _) -> True
    _ -> False
  }
}

pub fn is_parked(lifecycle: TaskLifecycle) -> Bool {
  case lifecycle {
    Parked(_, _, _) -> True
    _ -> False
  }
}

pub fn is_completed(lifecycle: TaskLifecycle) -> Bool {
  case lifecycle {
    Completed(_, _) -> True
    _ -> False
  }
}

pub fn has_active_or_pending(
  directory: TaskDirectory,
  task_identity: identity.TaskIdentity,
) -> Bool {
  case get(directory, task_identity) {
    Ok(lifecycle) -> is_active_or_pending(lifecycle)
    Error(Nil) -> False
  }
}

pub fn has_dispatch_slot_consumer(
  directory: TaskDirectory,
  task_identity: identity.TaskIdentity,
) -> Bool {
  case get(directory, task_identity) {
    Ok(lifecycle) -> consumes_dispatch_slot(lifecycle)
    Error(Nil) -> False
  }
}

pub fn blocks_new_dispatch(lifecycle: TaskLifecycle) -> Bool {
  case lifecycle {
    Validating(_, _, _)
    | Claiming(_, _, _, _)
    | Starting(_, _, _, _, _)
    | Running(_, _, _, _, _)
    | Stopping(_, _, _, _, _, _) -> True
    RetryWaiting(_, _, _, _)
    | RetryRefreshing(_, _, _, _)
    | Parked(_, _, _)
    | Completed(_, _)
    | Idle(_) -> False
  }
}

pub fn has_dispatch_blocker(
  directory: TaskDirectory,
  task_identity: identity.TaskIdentity,
) -> Bool {
  case get(directory, task_identity) {
    Ok(lifecycle) -> blocks_new_dispatch(lifecycle)
    Error(Nil) -> False
  }
}

pub fn has_tracker_claim(
  directory: TaskDirectory,
  task_identity: identity.TaskIdentity,
) -> Bool {
  case get(directory, task_identity) {
    Ok(lifecycle) -> holds_tracker_claim(lifecycle)
    Error(Nil) -> False
  }
}

pub fn dispatch_slot_count(directory: TaskDirectory) -> Int {
  entries(directory)
  |> list.filter(fn(entry) {
    let #(_, lifecycle) = entry
    consumes_dispatch_slot(lifecycle)
  })
  |> list.length
}

pub fn task_ref(lifecycle: TaskLifecycle) -> task.TaskRef {
  case lifecycle {
    Idle(task_ref: task_ref)
    | Validating(task_ref: task_ref, ..)
    | Claiming(task_ref: task_ref, ..)
    | Starting(task_ref: task_ref, ..)
    | Running(task_ref: task_ref, ..)
    | RetryWaiting(task_ref: task_ref, ..)
    | RetryRefreshing(task_ref: task_ref, ..)
    | Stopping(task_ref: task_ref, ..)
    | Parked(task_ref: task_ref, ..)
    | Completed(task_ref: task_ref, ..) -> task_ref
  }
}

pub fn lifecycle_identity(lifecycle: TaskLifecycle) -> identity.TaskIdentity {
  task_ref(lifecycle) |> identity.task_ref
}

pub fn error_code(error: LifecycleError) -> String {
  case error {
    ConflictingLifecycleSources(_, _, _) -> "conflicting_lifecycle_sources"
    MissingClaimedLifecycle(_) -> "missing_claimed_lifecycle"
    MissingRetryWaitingForRefresh(_, _) -> "missing_retry_waiting_for_refresh"
    RunningWorkerMismatch(_) -> "running_worker_mismatch"
  }
}

fn count_lifecycle(counts: Counts, lifecycle: TaskLifecycle) -> Counts {
  case lifecycle {
    Idle(_) -> counts
    Validating(_, _, _) -> Counts(..counts, validating: counts.validating + 1)
    Claiming(_, _, _, _) -> Counts(..counts, claiming: counts.claiming + 1)
    Starting(_, _, _, _, _) -> Counts(..counts, starting: counts.starting + 1)
    Running(_, _, _, _, _) -> Counts(..counts, running: counts.running + 1)
    RetryWaiting(_, _, _, _) ->
      Counts(..counts, retry_waiting: counts.retry_waiting + 1)
    RetryRefreshing(_, _, _, _) ->
      Counts(..counts, retry_refreshing: counts.retry_refreshing + 1)
    Stopping(_, _, _, _, _, _) ->
      Counts(..counts, stopping: counts.stopping + 1)
    Parked(_, _, _) -> Counts(..counts, parked: counts.parked + 1)
    Completed(_, _) -> Counts(..counts, completed: counts.completed + 1)
  }
}
