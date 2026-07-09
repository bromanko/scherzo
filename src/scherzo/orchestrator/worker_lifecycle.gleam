import gleam/dict
import gleam/erlang/process
import gleam/int
import gleam/option.{type Option, None, Some}
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/config/types as config_types
import scherzo/log
import scherzo/orchestrator/daemon_capabilities
import scherzo/orchestrator/scheduled_runtime
import scherzo/orchestrator/transition_types
import scherzo/orchestrator/worker_registry
import scherzo/orchestrator/workflow_snapshot
import scherzo/runtime/identity
import scherzo/runtime_bundle
import scherzo/session/event as session_event
import scherzo/session/hub
import scherzo/session/reason as session_reason
import scherzo/session/tokens as session_tokens
import scherzo/state/record
import scherzo/task
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_dag
import scherzo/workflow_run

pub type WorkflowSnapshot =
  workflow_snapshot.Snapshot

pub type WorkflowSnapshotError =
  workflow_snapshot.SnapshotError

pub type WorkerSpawnContext(state, message, timer) {
  WorkerSpawnContext(
    state: state,
    capabilities: daemon_capabilities.DaemonCapabilities(state, message, timer),
    register_session: fn(
      String,
      tracker_issue.Issue,
      String,
      Option(session_event.RecoveryInfo),
      Int,
    ) -> Nil,
    apply_task_ref_start: fn(state, task.TaskRef, tracker_issue.Issue, String) ->
      state,
    spawn: fn(tracker_issue.Issue, String, String) -> process.Pid,
    register_worker: fn(state, worker_registry.WorkerHandle) -> state,
    clear_recovery: fn(state, String) -> state,
  )
}

pub fn workflow_snapshot_for_start(
  snapshot: Option(WorkflowSnapshot),
  bundle: runtime_bundle.RuntimeBundle,
  issue: tracker_issue.Issue,
  workflow_id: String,
  run_id: String,
) -> Result(WorkflowSnapshot, WorkflowSnapshotError) {
  workflow_snapshot.for_worker_start(
    snapshot,
    bundle,
    issue,
    workflow_id,
    run_id,
  )
}

pub fn snapshot_reason(error: WorkflowSnapshotError) -> String {
  workflow_snapshot.worker_start_error_reason(error)
}

pub fn workflow_snapshot_workspace_root(snapshot: WorkflowSnapshot) -> String {
  snapshot.orchestrator.effective.workspace.root
}

pub fn workflow_snapshot_dag(
  snapshot: WorkflowSnapshot,
) -> workflow_dag.WorkflowDag {
  snapshot.dag
}

pub fn workflow_snapshot_orchestrator(
  snapshot: WorkflowSnapshot,
) -> config_types.OrchestratorConfig {
  snapshot.orchestrator
}

pub fn workflow_snapshot_fingerprint(snapshot: WorkflowSnapshot) -> String {
  snapshot.fingerprint
}

pub fn workflow_snapshot_run_root(snapshot: WorkflowSnapshot) -> String {
  snapshot.run_root
}

pub fn spawn_worker(
  context: WorkerSpawnContext(state, message, timer),
  ref: task.TaskRef,
  issue: tracker_issue.Issue,
  workspace_path: String,
  run_id: String,
  session_id: String,
  recovery: Option(session_event.RecoveryInfo),
) -> state {
  let capabilities = context.capabilities
  let started_at_ms = daemon_capabilities.now_ms(cap_clock(capabilities))
  context.register_session(
    session_id,
    issue,
    workspace_path,
    recovery,
    started_at_ms,
  )
  daemon_capabilities.recovery_lifecycle(
    cap_events(capabilities),
    session_id,
    recovery,
  )
  daemon_capabilities.lifecycle(
    cap_events(capabilities),
    session_id,
    session_event.DispatchStarted,
    None,
  )
  log_dispatch_started(cap_logger(capabilities), issue, run_id, workspace_path)
  let state =
    context.apply_task_ref_start(context.state, ref, issue, workspace_path)
  let pid = context.spawn(issue, run_id, session_id)
  let monitor = process.monitor(pid)
  daemon_capabilities.lifecycle(
    cap_events(capabilities),
    session_id,
    session_event.WorkerStarted,
    None,
  )
  daemon_capabilities.update_status(
    cap_events(capabilities),
    session_id,
    session_event.Running,
  )
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

pub type ScheduledWorkerSpawnContext(state, message, timer) {
  ScheduledWorkerSpawnContext(
    state: state,
    capabilities: daemon_capabilities.DaemonCapabilities(state, message, timer),
    reserve_session_sequence: fn(state) -> state,
    register_session: fn(String, String, String, Int) -> Nil,
    spawn: fn(Int, String) -> process.Pid,
    register_scheduled_worker: fn(state, worker_registry.ScheduledWorkerHandle) ->
      state,
    remove_pending_start: fn(state, String) -> state,
  )
}

pub fn spawn_scheduled_worker(
  context: ScheduledWorkerSpawnContext(state, message, timer),
  pending: scheduled_runtime.PendingStart,
  run_root: String,
) -> state {
  let capabilities = context.capabilities
  let state = context.reserve_session_sequence(context.state)
  let session_id = scheduled_session_id(pending.run_id, pending.attempt)
  let started_at_ms = daemon_capabilities.now_ms(cap_clock(capabilities))
  let display_ref = "scheduled-" <> pending.job_id
  context.register_session(session_id, display_ref, run_root, started_at_ms)
  daemon_capabilities.lifecycle(
    cap_events(capabilities),
    session_id,
    session_event.DispatchStarted,
    Some("scheduled"),
  )
  let state =
    daemon_capabilities.append_bodies_best_effort(
      cap_ledger(capabilities),
      state,
      [
        record.ScheduledRunStarted(
          pending.job_id,
          pending.workflow_id,
          pending.due_at_ms,
          started_at_ms,
          pending.run_id,
          pending.attempt,
          session_id,
          run_root,
        ),
      ],
      "scheduled_started_append_failed",
    )
  log_scheduled_dispatch_started(
    cap_logger(capabilities),
    pending.job_id,
    pending.run_id,
    pending.workflow_id,
  )
  let pid = context.spawn(started_at_ms, session_id)
  let monitor = process.monitor(pid)
  daemon_capabilities.lifecycle(
    cap_events(capabilities),
    session_id,
    session_event.WorkerStarted,
    Some("scheduled"),
  )
  daemon_capabilities.update_status(
    cap_events(capabilities),
    session_id,
    session_event.Running,
  )
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

pub type WorkerUpdateContext(state, message, timer) {
  WorkerUpdateContext(
    state: state,
    capabilities: daemon_capabilities.DaemonCapabilities(state, message, timer),
    registry: fn(state) -> worker_registry.Registry,
    log_worker_update: fn(state, String, agent_types.RunnerUpdate) -> state,
  )
}

pub fn handle_worker_update(
  context: WorkerUpdateContext(state, message, timer),
  issue_id: String,
  update: agent_types.RunnerUpdate,
) -> state {
  let capabilities = context.capabilities
  case
    worker_registry.worker_for_issue(context.registry(context.state), issue_id)
  {
    Ok(handle) ->
      daemon_capabilities.worker_update(
        cap_events(capabilities),
        handle.session_id,
        update,
      )
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

pub type ScheduledWorkerSuccessContext(state, message, timer) {
  ScheduledWorkerSuccessContext(
    state: state,
    capabilities: daemon_capabilities.DaemonCapabilities(state, message, timer),
    needs_human: fn(
      state,
      worker_registry.ScheduledWorkerHandle,
      workflow_run.WorkflowRunSuccess,
    ) -> state,
  )
}

pub fn finish_scheduled_worker_success(
  context: ScheduledWorkerSuccessContext(state, message, timer),
  handle: worker_registry.ScheduledWorkerHandle,
  success: workflow_run.WorkflowRunSuccess,
) -> state {
  let capabilities = context.capabilities
  case success.worker_success.final_classification {
    agent_types.FinalTerminal -> {
      log_scheduled_worker_exited(
        cap_logger(capabilities),
        "info",
        handle.job_id,
        handle.run_id,
        "normal",
      )
      update_tokens(
        cap_events(capabilities),
        handle.session_id,
        success.worker_success.tokens,
      )
      publish_worker_exited(
        cap_events(capabilities),
        handle.session_id,
        "normal",
      )
      finish_session(
        cap_events(capabilities),
        handle.session_id,
        session_reason.Normal,
      )
      daemon_capabilities.append_bodies_best_effort(
        cap_ledger(capabilities),
        context.state,
        [
          record.ScheduledRunSucceeded(
            handle.job_id,
            handle.workflow_id,
            handle.due_at_ms,
            handle.run_id,
            handle.attempt,
            daemon_capabilities.now_ms(cap_clock(capabilities)),
            success.worker_success.tokens.total,
            success.worker_success.turns,
          ),
        ],
        "scheduled_success_append_failed",
      )
    }
    agent_types.FinalActive | agent_types.FinalNonActive ->
      context.needs_human(context.state, handle, success)
  }
}

pub type ScheduledWorkerNeedsHumanContext(state, message, timer) {
  ScheduledWorkerNeedsHumanContext(
    state: state,
    capabilities: daemon_capabilities.DaemonCapabilities(state, message, timer),
    append_failure_ledger: fn(
      state,
      worker_registry.ScheduledWorkerHandle,
      String,
      Bool,
      Option(String),
    ) -> state,
    begin_failure_report_request: fn(
      state,
      scheduled_runtime.FailureReportRequest,
    ) -> state,
  )
}

pub fn finish_scheduled_worker_needs_human(
  context: ScheduledWorkerNeedsHumanContext(state, message, timer),
  handle: worker_registry.ScheduledWorkerHandle,
  success: workflow_run.WorkflowRunSuccess,
) -> state {
  let capabilities = context.capabilities
  log_scheduled_worker_needs_human(
    cap_logger(capabilities),
    handle.job_id,
    handle.run_id,
  )
  update_tokens(
    cap_events(capabilities),
    handle.session_id,
    success.worker_success.tokens,
  )
  publish_worker_exited(
    cap_events(capabilities),
    handle.session_id,
    "needs_human",
  )
  finish_failed_session(cap_events(capabilities), handle.session_id)
  let state =
    context.append_failure_ledger(
      context.state,
      handle,
      "needs_human",
      True,
      Some(handle.run_root),
    )
  context.begin_failure_report_request(
    state,
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

pub type ScheduledWorkerFailureContext(state, message, timer) {
  ScheduledWorkerFailureContext(
    state: state,
    capabilities: daemon_capabilities.DaemonCapabilities(state, message, timer),
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
    ) -> state,
    begin_failure_report_request: fn(
      state,
      scheduled_runtime.FailureReportRequest,
    ) -> state,
  )
}

pub fn finish_scheduled_worker_failure(
  context: ScheduledWorkerFailureContext(state, message, timer),
  handle: worker_registry.ScheduledWorkerHandle,
  failure: workflow_run.WorkflowRunFailure,
) -> state {
  let capabilities = context.capabilities
  let reason = workflow_run.failure_report(failure)
  log_scheduled_worker_exited(
    cap_logger(capabilities),
    "warn",
    handle.job_id,
    handle.run_id,
    reason,
  )
  publish_worker_exited(cap_events(capabilities), handle.session_id, reason)
  finish_failed_session(cap_events(capabilities), handle.session_id)
  let run_root = case failure.run_root {
    Some(root) -> Some(root)
    None -> Some(handle.run_root)
  }
  let #(state, follow_up) =
    context.worker_failure_follow_up(context.state, handle, reason, run_root)
  let state =
    context.append_failure_ledger(state, handle, reason, True, run_root)
  continue_scheduled_failure_follow_up(
    state,
    follow_up,
    context.begin_failure_report_request,
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

pub fn worker_down_matches(
  workers: transition_types.WorkerDirectory,
  issue_id: String,
  handle: worker_registry.WorkerHandle,
) -> Bool {
  case dict.get(workers.by_session, handle.session_id) {
    Ok(worker_identity) ->
      case dict.get(workers.by_issue, worker_identity) {
        Ok(entry) ->
          entry.issue_id == issue_id
          && entry.run_id == handle.run_id
          && entry.session_id == handle.session_id
        Error(Nil) -> False
      }
    Error(Nil) -> False
  }
}

pub fn worker_down_message(
  issue_id: String,
  handle: worker_registry.WorkerHandle,
  context: transition_types.WorkerLifecycleContext,
) -> transition_types.Message {
  transition_types.WorkerDown(
    transition_types.KnownWorkerDown(
      identity.issue_id_from_string(issue_id),
      identity.run_id_from_string(handle.run_id),
      identity.session_id_from_string(handle.session_id),
    ),
    context,
  )
}

pub fn publish_worker_down(
  events: daemon_capabilities.EventPublisher,
  session_id: String,
) -> Nil {
  daemon_capabilities.lifecycle(
    events,
    session_id,
    session_event.WorkerDown,
    None,
  )
}

pub type ScheduledWorkerDownContext(state, message, timer) {
  ScheduledWorkerDownContext(
    state: state,
    capabilities: daemon_capabilities.DaemonCapabilities(state, message, timer),
    set_registry: fn(state, worker_registry.Registry) -> state,
    worker_failure_follow_up: fn(
      state,
      worker_registry.ScheduledWorkerHandle,
      String,
      Option(String),
    ) -> #(state, scheduled_runtime.WorkerFailureFollowUp),
    begin_failure_report_request: fn(
      state,
      scheduled_runtime.FailureReportRequest,
    ) -> state,
    start_pending_scheduled_runs: fn(state) -> state,
  )
}

pub fn scheduled_worker_down(
  context: ScheduledWorkerDownContext(state, message, timer),
  registry: worker_registry.Registry,
  run_id: String,
  handle: worker_registry.ScheduledWorkerHandle,
) -> state {
  let capabilities = context.capabilities
  let state = context.set_registry(context.state, registry)
  log_scheduled_worker_down(cap_logger(capabilities), handle.job_id, run_id)
  publish_worker_down(cap_events(capabilities), handle.session_id)
  finish_failed_session(cap_events(capabilities), handle.session_id)
  let #(state, follow_up) =
    context.worker_failure_follow_up(
      state,
      handle,
      "worker_down",
      Some(handle.run_root),
    )
  let state =
    daemon_capabilities.append_bodies_best_effort(
      cap_ledger(capabilities),
      state,
      [
        record.ScheduledRunFailed(
          handle.job_id,
          handle.workflow_id,
          handle.due_at_ms,
          handle.run_id,
          handle.attempt,
          daemon_capabilities.now_ms(cap_clock(capabilities)),
          "worker_down",
          True,
          Some(handle.run_root),
        ),
      ],
      "scheduled_worker_down_append_failed",
    )
  let state =
    continue_scheduled_failure_follow_up(
      state,
      follow_up,
      context.begin_failure_report_request,
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
) -> state {
  case follow_up {
    scheduled_runtime.WorkerFailureReport(request) ->
      begin_failure_report_request(state, request)
  }
}

fn cap_clock(
  capabilities: daemon_capabilities.DaemonCapabilities(state, message, timer),
) -> daemon_capabilities.Clock {
  daemon_capabilities.daemon_clock(capabilities)
}

fn cap_logger(
  capabilities: daemon_capabilities.DaemonCapabilities(state, message, timer),
) -> daemon_capabilities.Logger {
  daemon_capabilities.daemon_logger(capabilities)
}

fn cap_events(
  capabilities: daemon_capabilities.DaemonCapabilities(state, message, timer),
) -> daemon_capabilities.EventPublisher {
  daemon_capabilities.daemon_events(capabilities)
}

fn cap_ledger(
  capabilities: daemon_capabilities.DaemonCapabilities(state, message, timer),
) -> daemon_capabilities.LedgerWriter(state) {
  daemon_capabilities.daemon_ledger(capabilities)
}

fn log_dispatch_started(
  logger: daemon_capabilities.Logger,
  issue: tracker_issue.Issue,
  run_id: String,
  workspace_path: String,
) -> Nil {
  write_log(logger, "info", "dispatch_started", [
    #("issue_id", issue.id),
    #("issue_identifier", issue.identifier),
    #("run_id", run_id),
    #("workspace_path", workspace_path),
  ])
}

fn log_scheduled_dispatch_started(
  logger: daemon_capabilities.Logger,
  job_id: String,
  run_id: String,
  workflow_id: String,
) -> Nil {
  write_log(logger, "info", "scheduled_dispatch_started", [
    #("job_id", job_id),
    #("run_id", run_id),
    #("workflow_id", workflow_id),
  ])
}

fn log_scheduled_worker_exited(
  logger: daemon_capabilities.Logger,
  level: String,
  job_id: String,
  run_id: String,
  reason: String,
) -> Nil {
  write_log(logger, level, "scheduled_worker_exited", [
    #("job_id", job_id),
    #("run_id", run_id),
    #("reason", log.truncate(reason, 200)),
  ])
}

fn log_scheduled_worker_needs_human(
  logger: daemon_capabilities.Logger,
  job_id: String,
  run_id: String,
) -> Nil {
  write_log(logger, "warn", "scheduled_worker_needs_human", [
    #("job_id", job_id),
    #("run_id", run_id),
  ])
}

fn log_scheduled_worker_down(
  logger: daemon_capabilities.Logger,
  job_id: String,
  run_id: String,
) -> Nil {
  write_log(logger, "warn", "scheduled_worker_down", [
    #("job_id", job_id),
    #("run_id", run_id),
  ])
}

fn write_log(
  logger: daemon_capabilities.Logger,
  level: String,
  event: String,
  fields: List(log.Field),
) -> Nil {
  case daemon_capabilities.write(logger, level, event, fields, []) {
    Ok(Nil) -> Nil
    Error(Nil) -> Nil
  }
}

fn update_tokens(
  events: daemon_capabilities.EventPublisher,
  session_id: String,
  tokens: session_tokens.TokenTotals,
) -> Nil {
  hub.update_tokens(daemon_capabilities.event_hub(events), session_id, tokens)
}

fn publish_worker_exited(
  events: daemon_capabilities.EventPublisher,
  session_id: String,
  reason: String,
) -> Nil {
  daemon_capabilities.lifecycle(
    events,
    session_id,
    session_event.WorkerExited,
    Some(log.truncate(reason, 200)),
  )
}

fn finish_session(
  events: daemon_capabilities.EventPublisher,
  session_id: String,
  reason: session_reason.WorkerExitReason,
) -> Nil {
  hub.finish_session(daemon_capabilities.event_hub(events), session_id, reason)
}

fn finish_failed_session(
  events: daemon_capabilities.EventPublisher,
  session_id: String,
) -> Nil {
  finish_session(events, session_id, session_reason.Failed)
}

fn scheduled_session_id(run_id: String, attempt: Int) -> String {
  run_id <> "-a" <> int.to_string(attempt)
}
