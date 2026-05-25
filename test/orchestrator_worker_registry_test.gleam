import birl
import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import scherzo/orchestrator/worker_registry
import scherzo/session/reason
import scherzo/task
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

fn issue(id: String, identifier: String) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: id,
    identifier: identifier,
    title: "Title " <> identifier,
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked("Todo"),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

fn worker_handle(
  issue_id: String,
  run_id: String,
  session_id: String,
) -> worker_registry.WorkerHandle {
  let monitor = process.monitor(process.self())
  let item = issue(issue_id, "ABC-" <> issue_id)
  worker_registry.WorkerHandle(
    task_ref: task.from_legacy_issue(item).ref,
    issue_id: issue_id,
    issue: item,
    run_id: run_id,
    pid: process.self(),
    monitor: monitor,
    workspace_path: "workspace/" <> issue_id,
    session_id: session_id,
    command_subject: None,
  )
}

fn worker_handle_with_ref(
  task_ref: task.TaskRef,
  identifier: String,
  run_id: String,
  session_id: String,
) -> worker_registry.WorkerHandle {
  let monitor = process.monitor(process.self())
  let item = issue(task_ref.remote_id, identifier)
  worker_registry.WorkerHandle(
    task_ref: task_ref,
    issue_id: task_ref.remote_id,
    issue: item,
    run_id: run_id,
    pid: process.self(),
    monitor: monitor,
    workspace_path: "workspace/" <> identifier,
    session_id: session_id,
    command_subject: None,
  )
}

fn scheduled_handle(
  run_id: String,
  session_id: String,
) -> worker_registry.ScheduledWorkerHandle {
  let monitor = process.monitor(process.self())
  worker_registry.ScheduledWorkerHandle(
    job_id: "repair",
    workflow_id: "repair",
    due_at_ms: 1_746_101_600_000,
    run_id: run_id,
    pid: process.self(),
    monitor: monitor,
    run_root: "workspace/repair/scheduled/repair/" <> run_id,
    session_id: session_id,
    attempt: 1,
    command_subject: None,
  )
}

pub fn worker_registry_registers_worker_and_session_lookup_test() {
  let handle = worker_handle("1", "run-1", "session-1")
  let registry =
    worker_registry.new()
    |> worker_registry.register_worker(handle)

  let assert Ok(found) =
    worker_registry.worker_for_session(registry, "session-1")
  assert found.run_id == "run-1"
  assert worker_registry.has_active_run(registry, "1")
  process.demonitor_process(handle.monitor)
}

pub fn worker_registry_distinguishes_duplicate_remote_ids_by_backend_test() {
  let linear_ref =
    task.TaskRef(
      backend_kind: "linear",
      remote_id: "shared",
      key: Some("ABC-1"),
      url: None,
    )
  let memory_ref =
    task.TaskRef(
      backend_kind: "test-memory",
      remote_id: "shared",
      key: Some("MEM-1"),
      url: None,
    )
  let linear_handle =
    worker_handle_with_ref(linear_ref, "ABC-1", "run-linear", "session-linear")
  let memory_handle =
    worker_handle_with_ref(memory_ref, "MEM-1", "run-memory", "session-memory")
  let registry =
    worker_registry.new()
    |> worker_registry.register_worker(linear_handle)
    |> worker_registry.register_worker(memory_handle)

  assert list.length(worker_registry.worker_handles(registry)) == 2
  assert worker_registry.has_active_task_ref(registry, linear_ref)
  assert worker_registry.has_active_task_ref(registry, memory_ref)
  let assert Ok(found_linear) =
    worker_registry.worker_for_task_ref(registry, linear_ref)
  let assert Ok(found_memory) =
    worker_registry.worker_for_task_ref(registry, memory_ref)
  assert found_linear.run_id == "run-linear"
  assert found_memory.run_id == "run-memory"

  let registry = worker_registry.remove_worker_handle(registry, linear_handle)
  assert !worker_registry.has_active_task_ref(registry, linear_ref)
  assert worker_registry.has_active_task_ref(registry, memory_ref)
  process.demonitor_process(linear_handle.monitor)
  process.demonitor_process(memory_handle.monitor)
}

pub fn worker_registry_registers_scheduled_worker_and_session_lookup_test() {
  let handle =
    scheduled_handle("schedule-repair-20260505T120000Z", "session-scheduled")
  let registry =
    worker_registry.new()
    |> worker_registry.register_scheduled_worker(handle)

  let assert Ok(found_by_run) =
    worker_registry.scheduled_worker_for_run(registry, handle.run_id)
  assert found_by_run.job_id == "repair"
  let assert Ok(found_by_session) =
    worker_registry.scheduled_worker_for_session(registry, "session-scheduled")
  assert found_by_session.run_id == handle.run_id
  process.demonitor_process(handle.monitor)
}

pub fn worker_registry_tracks_yaml_steps_and_stopped_runs_test() {
  let registry =
    worker_registry.new()
    |> worker_registry.register_yaml_step_started("run-1-step-a", "run-1")
    |> worker_registry.register_yaml_step_started("run-1-step-b", "run-1")
    |> worker_registry.register_yaml_step_started("run-2-step-a", "run-2")
    |> worker_registry.mark_yaml_run_stopping("run-1", reason.OperatorAbort)

  let sessions =
    worker_registry.active_yaml_step_sessions_for_run(registry, "run-1")
  assert list.contains(sessions, "run-1-step-a")
  assert list.contains(sessions, "run-1-step-b")
  assert !list.contains(sessions, "run-2-step-a")
  assert worker_registry.stopped_yaml_run_reason(registry, "run-1")
    == Ok(reason.OperatorAbort)
}

pub fn worker_registry_registers_and_clears_step_command_route_test() {
  let command_subject = process.new_subject()
  let registry =
    worker_registry.new()
    |> worker_registry.register_yaml_step_command_subject(
      "run-1-step-a",
      command_subject,
    )

  let assert Ok(_) =
    worker_registry.step_command_subject_for_session(registry, "run-1-step-a")
  let registry =
    worker_registry.clear_yaml_step_command_route(registry, "run-1-step-a")
  assert worker_registry.step_command_subject_for_session(
      registry,
      "run-1-step-a",
    )
    == Error(Nil)
}

pub fn worker_registry_resolves_worker_and_step_command_downs_test() {
  let handle = worker_handle("1", "run-1", "session-1")
  let registry =
    worker_registry.new()
    |> worker_registry.register_worker(handle)
  let assert worker_registry.WorkerDown(registry, "1", down_handle) =
    worker_registry.resolve_down(registry, handle.monitor)
  assert down_handle.session_id == "session-1"
  assert !worker_registry.has_active_run(registry, "1")
  process.demonitor_process(handle.monitor)

  let command_subject = process.new_subject()
  let registry =
    registry
    |> worker_registry.register_yaml_step_command_subject(
      "run-1-step-a",
      command_subject,
    )
  let assert Ok(step_monitor) =
    worker_registry.step_command_monitor_for_session(registry, "run-1-step-a")
  let assert worker_registry.StepCommandDown(registry, "run-1-step-a") =
    worker_registry.resolve_down(registry, step_monitor)
  assert worker_registry.step_command_subject_for_session(
      registry,
      "run-1-step-a",
    )
    == Error(Nil)
  process.demonitor_process(step_monitor)
}

pub fn worker_registry_resolves_scheduled_worker_down_test() {
  let handle =
    scheduled_handle("schedule-repair-20260505T120000Z", "session-scheduled")
  let registry =
    worker_registry.new()
    |> worker_registry.register_scheduled_worker(handle)
  let assert worker_registry.ScheduledWorkerDown(registry, run_id, down_handle) =
    worker_registry.resolve_down(registry, handle.monitor)
  assert run_id == handle.run_id
  assert down_handle.session_id == "session-scheduled"
  assert worker_registry.scheduled_worker_for_run(registry, handle.run_id)
    == Error(Nil)
  process.demonitor_process(handle.monitor)
}

pub fn worker_registry_remove_all_clears_registry_test() {
  let handle = worker_handle("1", "run-1", "session-1")
  let command_subject = process.new_subject()
  let registry =
    worker_registry.new()
    |> worker_registry.register_worker(handle)
    |> worker_registry.register_yaml_step_started("run-1-step-a", "run-1")
    |> worker_registry.register_yaml_step_command_subject(
      "run-1-step-a",
      command_subject,
    )
    |> worker_registry.remove_all

  assert worker_registry.worker_handles(registry) == []
  assert worker_registry.scheduled_worker_handles(registry) == []
  assert worker_registry.active_yaml_step_sessions_for_run(registry, "run-1")
    == []
  assert worker_registry.step_command_subject_for_session(
      registry,
      "run-1-step-a",
    )
    == Error(Nil)
}
