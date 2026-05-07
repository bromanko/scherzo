import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, Some}
import scherzo/agent/worker_command
import scherzo/session/reason as session_reason
import scherzo/tracker/issue as tracker_issue

pub type WorkerHandle {
  WorkerHandle(
    issue_id: String,
    issue: tracker_issue.Issue,
    run_id: String,
    pid: process.Pid,
    monitor: process.Monitor,
    workspace_path: String,
    session_id: String,
    command_subject: Option(process.Subject(worker_command.Command)),
  )
}

pub type ScheduledWorkerHandle {
  ScheduledWorkerHandle(
    job_id: String,
    workflow_id: String,
    run_id: String,
    pid: process.Pid,
    monitor: process.Monitor,
    run_root: String,
    session_id: String,
    attempt: Int,
    command_subject: Option(process.Subject(worker_command.Command)),
  )
}

pub type StepCommandSubjectLookupError {
  NoActiveStepCommandSubject
  MultipleActiveStepCommandSubjects
}

pub type DownResolution {
  WorkerDown(registry: Registry, issue_id: String, handle: WorkerHandle)
  WorkerDownStale(registry: Registry, issue_id: String)
  ScheduledWorkerDown(
    registry: Registry,
    run_id: String,
    handle: ScheduledWorkerHandle,
  )
  ScheduledWorkerDownStale(registry: Registry, run_id: String)
  StepCommandDown(registry: Registry, session_id: String)
  UnknownDown(registry: Registry)
}

pub opaque type Registry {
  Registry(
    workers: Dict(String, WorkerHandle),
    worker_monitors: Dict(process.Monitor, String),
    issue_sessions: Dict(String, String),
    scheduled_workers: Dict(String, ScheduledWorkerHandle),
    scheduled_worker_monitors: Dict(process.Monitor, String),
    scheduled_sessions: Dict(String, String),
    step_command_subjects: Dict(String, process.Subject(worker_command.Command)),
    step_command_monitors: Dict(process.Monitor, String),
    step_command_subject_monitors: Dict(String, process.Monitor),
    yaml_step_runs: Dict(String, String),
    stopped_yaml_runs: Dict(String, session_reason.WorkerExitReason),
    next_session_sequence: Int,
  )
}

pub fn new() -> Registry {
  Registry(
    workers: dict.new(),
    worker_monitors: dict.new(),
    issue_sessions: dict.new(),
    scheduled_workers: dict.new(),
    scheduled_worker_monitors: dict.new(),
    scheduled_sessions: dict.new(),
    step_command_subjects: dict.new(),
    step_command_monitors: dict.new(),
    step_command_subject_monitors: dict.new(),
    yaml_step_runs: dict.new(),
    stopped_yaml_runs: dict.new(),
    next_session_sequence: 1,
  )
}

pub fn reserve_session_sequence(registry: Registry) -> #(Registry, Int) {
  #(
    Registry(
      ..registry,
      next_session_sequence: registry.next_session_sequence + 1,
    ),
    registry.next_session_sequence,
  )
}

pub fn register_worker(registry: Registry, handle: WorkerHandle) -> Registry {
  Registry(
    ..registry,
    workers: dict.insert(registry.workers, handle.issue_id, handle),
    worker_monitors: dict.insert(
      registry.worker_monitors,
      handle.monitor,
      handle.issue_id,
    ),
    issue_sessions: dict.insert(
      registry.issue_sessions,
      handle.issue_id,
      handle.session_id,
    ),
  )
}

pub fn register_scheduled_worker(
  registry: Registry,
  handle: ScheduledWorkerHandle,
) -> Registry {
  Registry(
    ..registry,
    scheduled_workers: dict.insert(
      registry.scheduled_workers,
      handle.run_id,
      handle,
    ),
    scheduled_worker_monitors: dict.insert(
      registry.scheduled_worker_monitors,
      handle.monitor,
      handle.run_id,
    ),
    scheduled_sessions: dict.insert(
      registry.scheduled_sessions,
      handle.run_id,
      handle.session_id,
    ),
  )
}

pub fn register_worker_command_subject(
  registry: Registry,
  issue_id: String,
  run_id: String,
  command_subject: process.Subject(worker_command.Command),
) -> Registry {
  case dict.get(registry.workers, issue_id) {
    Error(_) -> registry
    Ok(handle) ->
      case handle.run_id == run_id {
        False -> registry
        True ->
          Registry(
            ..registry,
            workers: dict.insert(
              registry.workers,
              issue_id,
              WorkerHandle(..handle, command_subject: Some(command_subject)),
            ),
          )
      }
  }
}

pub fn register_scheduled_worker_command_subject(
  registry: Registry,
  run_id: String,
  command_subject: process.Subject(worker_command.Command),
) -> Registry {
  case dict.get(registry.scheduled_workers, run_id) {
    Error(_) -> registry
    Ok(handle) ->
      Registry(
        ..registry,
        scheduled_workers: dict.insert(
          registry.scheduled_workers,
          run_id,
          ScheduledWorkerHandle(
            ..handle,
            command_subject: Some(command_subject),
          ),
        ),
      )
  }
}

pub fn worker_for_session(
  registry: Registry,
  session_id: String,
) -> Result(WorkerHandle, Nil) {
  registry.workers
  |> dict.values
  |> list.filter(fn(handle) { handle.session_id == session_id })
  |> first_worker
}

pub fn worker_for_run(
  registry: Registry,
  run_id: String,
) -> Result(WorkerHandle, Nil) {
  registry.workers
  |> dict.values
  |> list.filter(fn(handle) { handle.run_id == run_id })
  |> first_worker
}

pub fn scheduled_worker_for_session(
  registry: Registry,
  session_id: String,
) -> Result(ScheduledWorkerHandle, Nil) {
  registry.scheduled_workers
  |> dict.values
  |> list.filter(fn(handle) { handle.session_id == session_id })
  |> first_scheduled_worker
}

pub fn scheduled_worker_for_run(
  registry: Registry,
  run_id: String,
) -> Result(ScheduledWorkerHandle, Nil) {
  dict.get(registry.scheduled_workers, run_id)
}

pub fn worker_for_issue(
  registry: Registry,
  issue_id: String,
) -> Result(WorkerHandle, Nil) {
  dict.get(registry.workers, issue_id)
}

pub fn worker_handles(registry: Registry) -> List(WorkerHandle) {
  dict.values(registry.workers)
}

pub fn scheduled_worker_handles(
  registry: Registry,
) -> List(ScheduledWorkerHandle) {
  dict.values(registry.scheduled_workers)
}

pub fn worker_issue_ids(registry: Registry) -> List(String) {
  dict.keys(registry.workers)
}

pub fn worker_issues(registry: Registry) -> List(tracker_issue.Issue) {
  registry.workers |> dict.values |> list.map(fn(handle) { handle.issue })
}

pub fn has_active_run(registry: Registry, issue_id: String) -> Bool {
  dict.has_key(registry.workers, issue_id)
}

pub fn issue_sessions(registry: Registry) -> Dict(String, String) {
  registry.issue_sessions
}

pub fn issue_session(
  registry: Registry,
  issue_id: String,
) -> Result(String, Nil) {
  dict.get(registry.issue_sessions, issue_id)
}

pub fn remove_worker(
  registry: Registry,
  issue_id: String,
) -> #(Registry, Result(WorkerHandle, Nil)) {
  case dict.get(registry.workers, issue_id) {
    Error(_) -> #(
      Registry(
        ..registry,
        issue_sessions: dict.delete(registry.issue_sessions, issue_id),
      ),
      Error(Nil),
    )
    Ok(handle) -> #(remove_worker_handle(registry, handle), Ok(handle))
  }
}

pub fn remove_worker_handle(
  registry: Registry,
  handle: WorkerHandle,
) -> Registry {
  Registry(
    ..registry,
    workers: dict.delete(registry.workers, handle.issue_id),
    worker_monitors: dict.delete(registry.worker_monitors, handle.monitor),
    issue_sessions: dict.delete(registry.issue_sessions, handle.issue_id),
  )
}

pub fn remove_scheduled_worker(
  registry: Registry,
  run_id: String,
) -> #(Registry, Result(ScheduledWorkerHandle, Nil)) {
  case dict.get(registry.scheduled_workers, run_id) {
    Error(_) -> #(
      Registry(
        ..registry,
        scheduled_sessions: dict.delete(registry.scheduled_sessions, run_id),
      ),
      Error(Nil),
    )
    Ok(handle) -> #(
      remove_scheduled_worker_handle(registry, handle),
      Ok(handle),
    )
  }
}

pub fn remove_scheduled_worker_handle(
  registry: Registry,
  handle: ScheduledWorkerHandle,
) -> Registry {
  Registry(
    ..registry,
    scheduled_workers: dict.delete(registry.scheduled_workers, handle.run_id),
    scheduled_worker_monitors: dict.delete(
      registry.scheduled_worker_monitors,
      handle.monitor,
    ),
    scheduled_sessions: dict.delete(registry.scheduled_sessions, handle.run_id),
  )
}

pub fn forget_issue_session(registry: Registry, issue_id: String) -> Registry {
  Registry(
    ..registry,
    issue_sessions: dict.delete(registry.issue_sessions, issue_id),
  )
}

pub fn register_yaml_step_started(
  registry: Registry,
  session_id: String,
  run_id: String,
) -> Registry {
  Registry(
    ..registry,
    yaml_step_runs: dict.insert(registry.yaml_step_runs, session_id, run_id),
  )
}

pub fn finish_yaml_step(registry: Registry, session_id: String) -> Registry {
  registry
  |> clear_yaml_step_command_route(session_id)
  |> delete_yaml_step_session(session_id)
}

pub fn active_yaml_step_sessions_for_run(
  registry: Registry,
  run_id: String,
) -> List(String) {
  registry.yaml_step_runs
  |> dict.to_list
  |> list.filter(fn(entry) {
    let #(_, step_run_id) = entry
    step_run_id == run_id
  })
  |> list.map(fn(entry) {
    let #(session_id, _) = entry
    session_id
  })
}

pub fn delete_yaml_step_sessions(
  registry: Registry,
  session_ids: List(String),
) -> Registry {
  list.fold(session_ids, registry, fn(acc, session_id) {
    delete_yaml_step_session(acc, session_id)
  })
}

pub fn mark_yaml_run_stopping(
  registry: Registry,
  run_id: String,
  reason: session_reason.WorkerExitReason,
) -> Registry {
  Registry(
    ..registry,
    stopped_yaml_runs: dict.insert(registry.stopped_yaml_runs, run_id, reason),
  )
}

pub fn stopped_yaml_run_reason(
  registry: Registry,
  run_id: String,
) -> Result(session_reason.WorkerExitReason, Nil) {
  dict.get(registry.stopped_yaml_runs, run_id)
}

pub fn register_yaml_step_command_subject(
  registry: Registry,
  session_id: String,
  command_subject: process.Subject(worker_command.Command),
) -> Registry {
  let registry = clear_yaml_step_command_route(registry, session_id)
  case process.subject_owner(command_subject) {
    Error(_) ->
      Registry(
        ..registry,
        step_command_subjects: dict.insert(
          registry.step_command_subjects,
          session_id,
          command_subject,
        ),
      )
    Ok(pid) -> {
      let monitor = process.monitor(pid)
      case process.is_alive(pid) {
        False -> {
          process.demonitor_process(monitor)
          registry
        }
        True ->
          Registry(
            ..registry,
            step_command_subjects: dict.insert(
              registry.step_command_subjects,
              session_id,
              command_subject,
            ),
            step_command_monitors: dict.insert(
              registry.step_command_monitors,
              monitor,
              session_id,
            ),
            step_command_subject_monitors: dict.insert(
              registry.step_command_subject_monitors,
              session_id,
              monitor,
            ),
          )
      }
    }
  }
}

pub fn clear_yaml_step_command_route(
  registry: Registry,
  session_id: String,
) -> Registry {
  case dict.get(registry.step_command_subject_monitors, session_id) {
    Error(_) ->
      Registry(
        ..registry,
        step_command_subjects: dict.delete(
          registry.step_command_subjects,
          session_id,
        ),
      )
    Ok(monitor) -> {
      process.demonitor_process(monitor)
      delete_step_command_route(registry, session_id, monitor)
    }
  }
}

pub fn clear_yaml_step_command_routes_for_run(
  registry: Registry,
  run_id: String,
) -> Registry {
  registry.step_command_subjects
  |> dict.keys
  |> list.filter(fn(session_id) {
    dict.get(registry.yaml_step_runs, session_id) == Ok(run_id)
  })
  |> list.fold(registry, fn(acc, session_id) {
    clear_yaml_step_command_route(acc, session_id)
  })
}

pub fn step_command_subject_for_session(
  registry: Registry,
  session_id: String,
) -> Result(process.Subject(worker_command.Command), Nil) {
  dict.get(registry.step_command_subjects, session_id)
}

pub fn step_command_monitor_for_session(
  registry: Registry,
  session_id: String,
) -> Result(process.Monitor, Nil) {
  dict.get(registry.step_command_subject_monitors, session_id)
}

pub fn step_command_subject_for_run(
  registry: Registry,
  run_id: String,
) -> Result(
  process.Subject(worker_command.Command),
  StepCommandSubjectLookupError,
) {
  registry.step_command_subjects
  |> dict.to_list
  |> list.filter(fn(entry) {
    let #(session_id, _) = entry
    dict.get(registry.yaml_step_runs, session_id) == Ok(run_id)
  })
  |> single_step_command_subject
}

pub fn resolve_down(
  registry: Registry,
  monitor: process.Monitor,
) -> DownResolution {
  case dict.get(registry.worker_monitors, monitor) {
    Ok(issue_id) ->
      case dict.get(registry.workers, issue_id) {
        Ok(handle) ->
          WorkerDown(remove_worker_handle(registry, handle), issue_id, handle)
        Error(_) ->
          WorkerDownStale(
            Registry(
              ..registry,
              worker_monitors: dict.delete(registry.worker_monitors, monitor),
              issue_sessions: dict.delete(registry.issue_sessions, issue_id),
            ),
            issue_id,
          )
      }
    Error(_) ->
      case dict.get(registry.scheduled_worker_monitors, monitor) {
        Ok(run_id) ->
          case dict.get(registry.scheduled_workers, run_id) {
            Ok(handle) ->
              ScheduledWorkerDown(
                remove_scheduled_worker_handle(registry, handle),
                run_id,
                handle,
              )
            Error(_) ->
              ScheduledWorkerDownStale(
                Registry(
                  ..registry,
                  scheduled_worker_monitors: dict.delete(
                    registry.scheduled_worker_monitors,
                    monitor,
                  ),
                  scheduled_sessions: dict.delete(
                    registry.scheduled_sessions,
                    run_id,
                  ),
                ),
                run_id,
              )
          }
        Error(_) ->
          case dict.get(registry.step_command_monitors, monitor) {
            Error(_) -> UnknownDown(registry)
            Ok(session_id) ->
              StepCommandDown(
                delete_step_command_route(registry, session_id, monitor),
                session_id,
              )
          }
      }
  }
}

pub fn remove_all(registry: Registry) -> Registry {
  dict.each(registry.worker_monitors, fn(monitor, _) {
    process.demonitor_process(monitor)
  })
  dict.each(registry.scheduled_worker_monitors, fn(monitor, _) {
    process.demonitor_process(monitor)
  })
  dict.each(registry.step_command_subject_monitors, fn(_, monitor) {
    process.demonitor_process(monitor)
  })
  Registry(..new(), next_session_sequence: registry.next_session_sequence)
}

fn delete_yaml_step_session(
  registry: Registry,
  session_id: String,
) -> Registry {
  Registry(
    ..registry,
    yaml_step_runs: dict.delete(registry.yaml_step_runs, session_id),
  )
}

fn delete_step_command_route(
  registry: Registry,
  session_id: String,
  monitor: process.Monitor,
) -> Registry {
  Registry(
    ..registry,
    step_command_subjects: dict.delete(
      registry.step_command_subjects,
      session_id,
    ),
    step_command_monitors: dict.delete(registry.step_command_monitors, monitor),
    step_command_subject_monitors: dict.delete(
      registry.step_command_subject_monitors,
      session_id,
    ),
  )
}

fn first_worker(handles: List(WorkerHandle)) -> Result(WorkerHandle, Nil) {
  case handles {
    [handle, ..] -> Ok(handle)
    [] -> Error(Nil)
  }
}

fn first_scheduled_worker(
  handles: List(ScheduledWorkerHandle),
) -> Result(ScheduledWorkerHandle, Nil) {
  case handles {
    [handle, ..] -> Ok(handle)
    [] -> Error(Nil)
  }
}

fn single_step_command_subject(
  entries: List(#(String, process.Subject(worker_command.Command))),
) -> Result(
  process.Subject(worker_command.Command),
  StepCommandSubjectLookupError,
) {
  case entries {
    [] -> Error(NoActiveStepCommandSubject)
    [#(_, subject)] -> Ok(subject)
    [_, _, ..] -> Error(MultipleActiveStepCommandSubjects)
  }
}
