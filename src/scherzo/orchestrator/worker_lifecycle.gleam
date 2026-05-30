import gleam/erlang/process
import gleam/int
import gleam/option.{type Option, None, Some}
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/orchestrator/identity
import scherzo/orchestrator/scheduled_runtime
import scherzo/orchestrator/transition_types
import scherzo/orchestrator/worker_registry
import scherzo/session/event as session_event
import scherzo/session/reason as session_reason
import scherzo/session/tokens as session_tokens
import scherzo/task
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_run

pub type WorkerSpawnContext(state) {
  WorkerSpawnContext(
    state: state,
    now_ms: fn() -> Int,
    register_session: fn(
      String,
      tracker_issue.Issue,
      String,
      Option(session_event.RecoveryInfo),
      Int,
    ) -> Nil,
    publish_recovery_lifecycle: fn(String, Option(session_event.RecoveryInfo)) ->
      Nil,
    publish_dispatch_started: fn(String) -> Nil,
    log_dispatch_started: fn(tracker_issue.Issue, String, String) -> Nil,
    apply_task_ref_start: fn(state, task.TaskRef, tracker_issue.Issue, String) ->
      state,
    spawn: fn(tracker_issue.Issue, String, String) -> process.Pid,
    publish_worker_started: fn(String) -> Nil,
    update_running_status: fn(String) -> Nil,
    register_worker: fn(state, worker_registry.WorkerHandle) -> state,
    clear_recovery: fn(state, String) -> state,
  )
}

pub fn spawn_worker(
  context: WorkerSpawnContext(state),
  ref: task.TaskRef,
  issue: tracker_issue.Issue,
  workspace_path: String,
  run_id: String,
  session_id: String,
  recovery: Option(session_event.RecoveryInfo),
) -> state {
  let started_at_ms = context.now_ms()
  context.register_session(
    session_id,
    issue,
    workspace_path,
    recovery,
    started_at_ms,
  )
  context.publish_recovery_lifecycle(session_id, recovery)
  context.publish_dispatch_started(session_id)
  context.log_dispatch_started(issue, run_id, workspace_path)
  let state =
    context.apply_task_ref_start(context.state, ref, issue, workspace_path)
  let pid = context.spawn(issue, run_id, session_id)
  let monitor = process.monitor(pid)
  context.publish_worker_started(session_id)
  context.update_running_status(session_id)
  let handle =
    worker_registry.WorkerHandle(
      task_ref: ref,
      issue_id: issue.id,
      issue: issue,
      run_id: run_id,
      pid: pid,
      monitor: monitor,
      workspace_path: workspace_path,
      session_id: session_id,
      command_subject: None,
    )
  state
  |> context.register_worker(handle)
  |> context.clear_recovery(issue.id)
}

pub type ScheduledWorkerSpawnContext(state) {
  ScheduledWorkerSpawnContext(
    state: state,
    now_ms: fn() -> Int,
    reserve_session_sequence: fn(state) -> state,
    register_session: fn(String, String, String, Int) -> Nil,
    publish_dispatch_started: fn(String) -> Nil,
    append_started_ledger: fn(
      state,
      scheduled_runtime.PendingStart,
      Int,
      String,
      String,
    ) -> Nil,
    log_dispatch_started: fn(String, String, String) -> Nil,
    spawn: fn(Int, String) -> process.Pid,
    publish_worker_started: fn(String) -> Nil,
    update_running_status: fn(String) -> Nil,
    register_scheduled_worker: fn(state, worker_registry.ScheduledWorkerHandle) ->
      state,
    remove_pending_start: fn(state, String) -> state,
  )
}

pub fn spawn_scheduled_worker(
  context: ScheduledWorkerSpawnContext(state),
  pending: scheduled_runtime.PendingStart,
  run_root: String,
) -> state {
  let state = context.reserve_session_sequence(context.state)
  let session_id = scheduled_session_id(pending.run_id, pending.attempt)
  let started_at_ms = context.now_ms()
  let display_ref = "scheduled-" <> pending.job_id
  context.register_session(session_id, display_ref, run_root, started_at_ms)
  context.publish_dispatch_started(session_id)
  context.append_started_ledger(
    state,
    pending,
    started_at_ms,
    session_id,
    run_root,
  )
  context.log_dispatch_started(
    pending.job_id,
    pending.run_id,
    pending.workflow_id,
  )
  let pid = context.spawn(started_at_ms, session_id)
  let monitor = process.monitor(pid)
  context.publish_worker_started(session_id)
  context.update_running_status(session_id)
  let handle =
    worker_registry.ScheduledWorkerHandle(
      job_id: pending.job_id,
      workflow_id: pending.workflow_id,
      due_at_ms: pending.due_at_ms,
      run_id: pending.run_id,
      pid: pid,
      monitor: monitor,
      run_root: run_root,
      session_id: session_id,
      attempt: pending.attempt,
      command_subject: None,
    )
  state
  |> context.register_scheduled_worker(handle)
  |> context.remove_pending_start(pending.job_id)
}

pub type WorkerCommandReadyContext(state) {
  WorkerCommandReadyContext(
    state: state,
    run_transition_messages: fn(state, List(transition_types.Message)) -> state,
    registry: fn(state) -> worker_registry.Registry,
    set_registry: fn(state, worker_registry.Registry) -> state,
  )
}

pub fn handle_worker_command_ready(
  context: WorkerCommandReadyContext(state),
  issue_id: String,
  run_id: String,
  command_subject: process.Subject(worker_command.Command),
) -> state {
  let state =
    context.run_transition_messages(context.state, [
      transition_types.WorkerCommandReady(
        identity.issue_id_from_string(issue_id),
        identity.run_id_from_string(run_id),
      ),
    ])
  context.set_registry(
    state,
    worker_registry.register_worker_command_subject(
      context.registry(state),
      issue_id,
      run_id,
      command_subject,
    ),
  )
}

pub type WorkerUpdateContext(state) {
  WorkerUpdateContext(
    state: state,
    registry: fn(state) -> worker_registry.Registry,
    publish_worker_update: fn(String, agent_types.RunnerUpdate) -> Nil,
    log_worker_update: fn(state, String, agent_types.RunnerUpdate) -> state,
  )
}

pub fn handle_worker_update(
  context: WorkerUpdateContext(state),
  issue_id: String,
  update: agent_types.RunnerUpdate,
) -> state {
  case
    worker_registry.worker_for_issue(context.registry(context.state), issue_id)
  {
    Ok(handle) -> context.publish_worker_update(handle.session_id, update)
    Error(Nil) -> Nil
  }
  context.log_worker_update(context.state, issue_id, update)
}

pub type ScheduledWorkerFinishedContext(state) {
  ScheduledWorkerFinishedContext(
    state: state,
    evaluate_scheduled_jobs: fn(state) -> state,
    scheduled_worker_for_run: fn(state, String) ->
      Result(worker_registry.ScheduledWorkerHandle, Nil),
    log_stale: fn(state, String) -> Nil,
    demonitor: fn(process.Monitor) -> Nil,
    remove_scheduled_worker_handle: fn(
      state,
      worker_registry.ScheduledWorkerHandle,
    ) -> state,
    finish_success: fn(
      state,
      worker_registry.ScheduledWorkerHandle,
      workflow_run.WorkflowRunSuccess,
    ) -> state,
    finish_failure: fn(
      state,
      worker_registry.ScheduledWorkerHandle,
      workflow_run.WorkflowRunFailure,
    ) -> state,
    start_pending_scheduled_runs: fn(state) -> state,
  )
}

pub fn handle_scheduled_worker_finished(
  context: ScheduledWorkerFinishedContext(state),
  run_id: String,
  result: Result(
    workflow_run.WorkflowRunSuccess,
    workflow_run.WorkflowRunFailure,
  ),
) -> state {
  let state = context.evaluate_scheduled_jobs(context.state)
  case context.scheduled_worker_for_run(state, run_id) {
    Error(Nil) -> {
      context.log_stale(state, run_id)
      state
    }
    Ok(handle) -> {
      context.demonitor(handle.monitor)
      let state = context.remove_scheduled_worker_handle(state, handle)
      let state = case result {
        Ok(success) -> context.finish_success(state, handle, success)
        Error(failure) -> context.finish_failure(state, handle, failure)
      }
      context.start_pending_scheduled_runs(state)
    }
  }
}

pub type ScheduledWorkerSuccessContext(state) {
  ScheduledWorkerSuccessContext(
    state: state,
    log_worker_exited: fn(state, String, String, String) -> Nil,
    update_tokens: fn(String, session_tokens.TokenTotals) -> Nil,
    publish_worker_exited: fn(String, String) -> Nil,
    finish_session: fn(String, session_reason.WorkerExitReason) -> Nil,
    append_success_ledger: fn(
      state,
      worker_registry.ScheduledWorkerHandle,
      workflow_run.WorkflowRunSuccess,
    ) -> Nil,
    needs_human: fn(
      state,
      worker_registry.ScheduledWorkerHandle,
      workflow_run.WorkflowRunSuccess,
    ) -> state,
  )
}

pub fn finish_scheduled_worker_success(
  context: ScheduledWorkerSuccessContext(state),
  handle: worker_registry.ScheduledWorkerHandle,
  success: workflow_run.WorkflowRunSuccess,
) -> state {
  case success.worker_success.final_classification {
    agent_types.FinalTerminal -> {
      context.log_worker_exited(
        context.state,
        handle.job_id,
        handle.run_id,
        "normal",
      )
      context.update_tokens(handle.session_id, success.worker_success.tokens)
      context.publish_worker_exited(handle.session_id, "normal")
      context.finish_session(handle.session_id, session_reason.Normal)
      context.append_success_ledger(context.state, handle, success)
      context.state
    }
    agent_types.FinalActive | agent_types.FinalNonActive ->
      context.needs_human(context.state, handle, success)
  }
}

pub type ScheduledWorkerNeedsHumanContext(state) {
  ScheduledWorkerNeedsHumanContext(
    state: state,
    log_needs_human: fn(state, String, String) -> Nil,
    update_tokens: fn(String, session_tokens.TokenTotals) -> Nil,
    publish_worker_exited: fn(String, String) -> Nil,
    finish_failed_session: fn(String) -> Nil,
    append_failure_ledger: fn(
      state,
      worker_registry.ScheduledWorkerHandle,
      String,
      Bool,
      Option(String),
    ) -> Nil,
    begin_failure_report_request: fn(
      state,
      scheduled_runtime.FailureReportRequest,
    ) -> state,
  )
}

pub fn finish_scheduled_worker_needs_human(
  context: ScheduledWorkerNeedsHumanContext(state),
  handle: worker_registry.ScheduledWorkerHandle,
  success: workflow_run.WorkflowRunSuccess,
) -> state {
  context.log_needs_human(context.state, handle.job_id, handle.run_id)
  context.update_tokens(handle.session_id, success.worker_success.tokens)
  context.publish_worker_exited(handle.session_id, "needs_human")
  context.finish_failed_session(handle.session_id)
  context.append_failure_ledger(
    context.state,
    handle,
    "needs_human",
    True,
    Some(handle.run_root),
  )
  context.begin_failure_report_request(
    context.state,
    scheduled_runtime.needs_human_follow_up(
      handle.job_id,
      handle.workflow_id,
      handle.due_at_ms,
      handle.run_id,
      handle.attempt,
      Some(handle.run_root),
      Some(handle.session_id),
    ),
  )
}

pub type ScheduledWorkerFailureContext(state) {
  ScheduledWorkerFailureContext(
    state: state,
    log_worker_exited: fn(state, String, String, String) -> Nil,
    publish_worker_exited: fn(String, String) -> Nil,
    finish_failed_session: fn(String) -> Nil,
    worker_failure_follow_up: fn(
      state,
      worker_registry.ScheduledWorkerHandle,
      String,
      Option(String),
    ) -> #(state, scheduled_runtime.WorkerFailureFollowUp),
    append_failure_ledger: fn(
      state,
      worker_registry.ScheduledWorkerHandle,
      String,
      Bool,
      Option(String),
    ) -> Nil,
    begin_failure_report_request: fn(
      state,
      scheduled_runtime.FailureReportRequest,
    ) -> state,
    apply_scheduled_runtime_actions: fn(state, List(scheduled_runtime.Action)) ->
      state,
  )
}

pub fn finish_scheduled_worker_failure(
  context: ScheduledWorkerFailureContext(state),
  handle: worker_registry.ScheduledWorkerHandle,
  failure: workflow_run.WorkflowRunFailure,
) -> state {
  let reason = workflow_run.failure_report(failure)
  context.log_worker_exited(context.state, handle.job_id, handle.run_id, reason)
  context.publish_worker_exited(handle.session_id, reason)
  context.finish_failed_session(handle.session_id)
  let run_root = case failure.run_root {
    Some(root) -> Some(root)
    None -> Some(handle.run_root)
  }
  let #(state, follow_up) =
    context.worker_failure_follow_up(context.state, handle, reason, run_root)
  let retry_exhausted = case follow_up {
    scheduled_runtime.WorkerFailureReport(_) -> True
    scheduled_runtime.WorkerFailureRetry(_) -> False
  }
  context.append_failure_ledger(
    state,
    handle,
    reason,
    retry_exhausted,
    run_root,
  )
  continue_scheduled_failure_follow_up(
    state,
    follow_up,
    context.begin_failure_report_request,
    context.apply_scheduled_runtime_actions,
  )
}

pub type WorkerFinishedContext(state) {
  WorkerFinishedContext(
    state: state,
    evaluate_scheduled_jobs: fn(state) -> state,
    run_transition_messages: fn(state, List(transition_types.Message)) -> state,
    lifecycle_context: fn(state) -> transition_types.WorkerLifecycleContext,
    start_pending_scheduled_runs: fn(state) -> state,
  )
}

pub fn worker_finished_to_transition(
  context: WorkerFinishedContext(state),
  issue_id: String,
  run_id: String,
  result: Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
) -> state {
  let state = context.evaluate_scheduled_jobs(context.state)
  let state =
    context.run_transition_messages(state, [
      transition_types.WorkerFinished(
        identity.issue_id_from_string(issue_id),
        identity.run_id_from_string(run_id),
        result,
        context.lifecycle_context(state),
      ),
    ])
  context.start_pending_scheduled_runs(state)
}

pub type WorkerDownContext(state) {
  WorkerDownContext(
    state: state,
    remote_client_monitor: Option(process.Monitor),
    log_remote_client_down: fn(state) -> Nil,
    clear_remote_client: fn(state) -> state,
    restart_remote_client_if_enabled: fn(state) -> state,
    resolve_down: fn(state, process.Monitor) -> worker_registry.DownResolution,
    handle_registry_down_resolution: fn(state, worker_registry.DownResolution) ->
      state,
  )
}

pub fn worker_down_to_transition(
  context: WorkerDownContext(state),
  down: process.Down,
) -> state {
  case down {
    process.ProcessDown(monitor, _, _) ->
      case context.remote_client_monitor {
        Some(remote_client_monitor) if monitor == remote_client_monitor -> {
          context.log_remote_client_down(context.state)
          context.state
          |> context.clear_remote_client
          |> context.restart_remote_client_if_enabled
        }
        _ ->
          context.handle_registry_down_resolution(
            context.state,
            context.resolve_down(context.state, monitor),
          )
      }
    process.PortDown(_, _, _) -> context.state
  }
}

pub type RegistryDownResolutionContext(state) {
  RegistryDownResolutionContext(
    state: state,
    unknown_down: fn(state, worker_registry.Registry) -> state,
    step_command_down: fn(state, worker_registry.Registry, String) -> state,
    worker_down: fn(
      state,
      worker_registry.Registry,
      String,
      worker_registry.WorkerHandle,
    ) -> state,
    worker_down_stale: fn(state, worker_registry.Registry, String) -> state,
    scheduled_worker_down: fn(
      state,
      worker_registry.Registry,
      String,
      worker_registry.ScheduledWorkerHandle,
    ) -> state,
    scheduled_worker_down_stale: fn(state, worker_registry.Registry, String) ->
      state,
  )
}

pub fn handle_registry_down_resolution(
  context: RegistryDownResolutionContext(state),
  resolution: worker_registry.DownResolution,
) -> state {
  case resolution {
    worker_registry.UnknownDown(registry) ->
      context.unknown_down(context.state, registry)
    worker_registry.StepCommandDown(registry, session_id) ->
      context.step_command_down(context.state, registry, session_id)
    worker_registry.WorkerDown(registry, issue_id, handle) ->
      context.worker_down(context.state, registry, issue_id, handle)
    worker_registry.WorkerDownStale(registry, issue_id) ->
      context.worker_down_stale(context.state, registry, issue_id)
    worker_registry.ScheduledWorkerDown(registry, run_id, handle) ->
      context.scheduled_worker_down(context.state, registry, run_id, handle)
    worker_registry.ScheduledWorkerDownStale(registry, run_id) ->
      context.scheduled_worker_down_stale(context.state, registry, run_id)
  }
}

pub type ScheduledWorkerDownContext(state) {
  ScheduledWorkerDownContext(
    state: state,
    set_registry: fn(state, worker_registry.Registry) -> state,
    log_worker_down: fn(state, String, String) -> Nil,
    publish_worker_down: fn(String) -> Nil,
    finish_failed_session: fn(String) -> Nil,
    worker_failure_follow_up: fn(
      state,
      worker_registry.ScheduledWorkerHandle,
      String,
      Option(String),
    ) -> #(state, scheduled_runtime.WorkerFailureFollowUp),
    append_failure_ledger: fn(
      state,
      worker_registry.ScheduledWorkerHandle,
      String,
      Bool,
      Option(String),
    ) -> Nil,
    begin_failure_report_request: fn(
      state,
      scheduled_runtime.FailureReportRequest,
    ) -> state,
    apply_scheduled_runtime_actions: fn(state, List(scheduled_runtime.Action)) ->
      state,
    start_pending_scheduled_runs: fn(state) -> state,
  )
}

pub fn scheduled_worker_down(
  context: ScheduledWorkerDownContext(state),
  registry: worker_registry.Registry,
  run_id: String,
  handle: worker_registry.ScheduledWorkerHandle,
) -> state {
  let state = context.set_registry(context.state, registry)
  context.log_worker_down(state, handle.job_id, run_id)
  context.publish_worker_down(handle.session_id)
  context.finish_failed_session(handle.session_id)
  let #(state, follow_up) =
    context.worker_failure_follow_up(
      state,
      handle,
      "worker_down",
      Some(handle.run_root),
    )
  let retry_exhausted = case follow_up {
    scheduled_runtime.WorkerFailureReport(_) -> True
    scheduled_runtime.WorkerFailureRetry(_) -> False
  }
  context.append_failure_ledger(
    state,
    handle,
    "worker_down",
    retry_exhausted,
    Some(handle.run_root),
  )
  let state =
    continue_scheduled_failure_follow_up(
      state,
      follow_up,
      context.begin_failure_report_request,
      context.apply_scheduled_runtime_actions,
    )
  context.start_pending_scheduled_runs(state)
}

fn continue_scheduled_failure_follow_up(
  state: state,
  follow_up: scheduled_runtime.WorkerFailureFollowUp,
  begin_failure_report_request: fn(
    state,
    scheduled_runtime.FailureReportRequest,
  ) -> state,
  apply_scheduled_runtime_actions: fn(state, List(scheduled_runtime.Action)) ->
    state,
) -> state {
  case follow_up {
    scheduled_runtime.WorkerFailureReport(request) ->
      begin_failure_report_request(state, request)
    scheduled_runtime.WorkerFailureRetry(actions) ->
      apply_scheduled_runtime_actions(state, actions)
  }
}

fn scheduled_session_id(run_id: String, attempt: Int) -> String {
  run_id <> "-a" <> int.to_string(attempt)
}
