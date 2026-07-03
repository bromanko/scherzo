import birl
import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import scherzo/agent/pi_event
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/artifact_repository/command_runner
import scherzo/config
import scherzo/config/types.{ui_server_enabled} as config_types
import scherzo/control/command
import scherzo/control/file as control_file
import scherzo/control/query/backend as query_backend
import scherzo/control/query/service as query_service
import scherzo/control/query/types as query_types
import scherzo/control/server as control_server
import scherzo/daemon_identity
import scherzo/error
import scherzo/log
import scherzo/managed_launch/grant as managed_launch_grant
import scherzo/orchestrator/abandoned_claim
import scherzo/orchestrator/artifact_publication_retry_control
import scherzo/orchestrator/control_command_handler
import scherzo/orchestrator/core
import scherzo/orchestrator/daemon_transition_shell
import scherzo/orchestrator/dispatch_recovery
import scherzo/orchestrator/effect_completion_handler
import scherzo/orchestrator/effect_runner
import scherzo/orchestrator/effects/types as transition_effects
import scherzo/orchestrator/event_publisher
import scherzo/orchestrator/operator_runtime
import scherzo/orchestrator/operator_worker_command
import scherzo/orchestrator/outbox_effects
import scherzo/orchestrator/poll_scheduler
import scherzo/orchestrator/query_runtime
import scherzo/orchestrator/read_model
import scherzo/orchestrator/recollect_outputs_control
import scherzo/orchestrator/remote_command_runtime as remote
import scherzo/orchestrator/retry_scheduler
import scherzo/orchestrator/retry_step_operation
import scherzo/orchestrator/retry_step_resumption
import scherzo/orchestrator/run_finalize_control
import scherzo/orchestrator/schedule_core
import scherzo/orchestrator/scheduled_runtime
import scherzo/orchestrator/session_metrics
import scherzo/orchestrator/startup_recovery
import scherzo/orchestrator/transition_types
import scherzo/orchestrator/worker_lifecycle
import scherzo/orchestrator/worker_registry
import scherzo/orchestrator/workflow_reloader
import scherzo/orchestrator/yaml_step_orphans
import scherzo/orchestrator/yaml_workflow_lifecycle
import scherzo/review_lane_preflight
import scherzo/review_lane_preflight_policy
import scherzo/runtime/identity
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
import scherzo/runtime_bundle
import scherzo/session/event as session_event
import scherzo/session/hub
import scherzo/session/name as session_name
import scherzo/session/reason as session_reason
import scherzo/session/recovery as session_recovery
import scherzo/session/tokens as session_tokens
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/ledger_batch
import scherzo/state/outbox
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/task
import scherzo/tracker
import scherzo/tracker/adapter
import scherzo/tracker/adapter_legacy
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/linear_adapter
import scherzo/tracker/state as issue_state
import scherzo/work_item
import scherzo/work_item/action_derivation
import scherzo/work_item/action_executor
import scherzo/work_item/action_receipts
import scherzo/work_item_invalidation
import scherzo/workflow_checkpoint
import scherzo/workflow_completion_policy.{type LinearStateRef}
import scherzo/workflow_dag
import scherzo/workflow_repair
import scherzo/workflow_run
import scherzo/workspace
import scherzo/workspace_run

pub type StartupError {
  StartupError(code: String, message: String)
}

type StartupPhaseMessage {
  SetStartupPhase(String)
  GetStartupPhase(process.Subject(String))
  StopStartupPhaseTracker
}

pub type Message {
  PollTick(Int)
  RetryTick(String, Int)
  ContinueStartupRecovery
  DispatchRecoveryContinue(List(tracker_issue.Issue))
  WorkerFinished(
    String,
    String,
    Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
  )
  ScheduledWorkerFinished(
    String,
    Result(workflow_run.WorkflowRunSuccess, workflow_run.WorkflowRunFailure),
  )
  ScheduledRetryTick(String, Int)
  ScheduledReportRetryTick(String, Int)
  WorkerUpdate(String, agent_types.RunnerUpdate)
  WorkerCommandReady(String, String, process.Subject(worker_command.Command))
  YamlStepStarted(String, String, String, String, Int)
  YamlStepUpdate(String, agent_types.RunnerUpdate)
  YamlStepCommandReady(String, process.Subject(worker_command.Command))
  YamlStepFinished(String, session_tokens.TokenTotals)
  WorkerCommandCompleted(
    command.OperatorCommand,
    worker_command.Reply,
    process.Subject(command.CommandResult),
  )
  WorkerCommandTimedOut(
    command.OperatorCommand,
    process.Subject(command.CommandResult),
  )
  AbortWorkerCommandTimedOut(
    command.OperatorCommand,
    String,
    process.Subject(command.CommandResult),
  )
  WorkerDown(process.Down)
  EffectRunnerDown(process.Down)
  ControlServerDown(process.Down)
  SideEffectCompleted(effect_runner.Completion)
  Shutdown(process.Subject(Nil))
  GetSnapshot(process.Subject(orchestrator_state.RuntimeState))
  GetReadModelSnapshot(process.Subject(read_model.Snapshot))
  GetProjectionSnapshot(process.Subject(projection.Projection))
  GetOutboxSnapshot(process.Subject(List(#(String, projection.OutboxStatus))))
  GetWorkflowSnapshot(process.Subject(workflow_reloader.State))
  GetRemoteDispatchPaused(process.Subject(Bool))
  AwaitStartupRecoveryReady(process.Subject(Result(Nil, Nil)), Int)
  StartupRecoveryWaiterTimedOut(Int)
  StartRemoteClient
  RunQueuedControlOperation(String)
  QueuedControlOperationFinished(String, QueuedControlOperationResult)
  ApplyOperatorCommand(
    command.OperatorCommand,
    Int,
    process.Subject(command.CommandResult),
  )
  ExecuteQuery(
    query_types.QueryRequest,
    Int,
    process.Subject(Result(query_types.QueryResponse, query_types.QueryError)),
  )
}

pub type QueuedControlOperationResult {
  QueuedControlOperationNoop
  QueuedControlOperationSucceeded(
    List(record.RecordBody),
    recovery.RecoveredWorkflowRun,
  )
  QueuedControlOperationCompleted(List(record.RecordBody))
  QueuedControlOperationRejected(List(record.RecordBody))
  QueuedControlOperationFailed(String, Option(String))
}

pub type TimerHandle {
  RealTimer(process.Timer)
  TestTimer(Int)
}

pub type ControlServerHandle {
  NoControlServer
  RealControlServer(control_server.Server)
}

type ControlPlane {
  ControlPlane(handle: ControlServerHandle, control_file_path: Option(String))
}

type StartupRecoveryPhase {
  StartupRecoveryPending(startup_recovery.StartupRecovery)
  StartupRecoveryRunning(StartupRecoveryStep, startup_recovery.StartupRecovery)
  StartupRecoveryReady
  StartupRecoveryFailed(String)
}

type StartupRecoveryStep {
  StartupRecoveryStageApplyRecovery
  StartupRecoveryStageApplyScheduledRecovery
  StartupRecoveryStageResumeWorkflows
  StartupRecoveryStageCheckInvariants
  StartupRecoveryStageFinish
}

pub type RuntimeDependencies {
  RuntimeDependencies(
    make_tracker_adapter: fn(config_types.EffectiveConfig) ->
      adapter.TrackerAdapter,
    workflow_run_dependencies: workflow_run.Dependencies,
    publication_command_runner: command_runner.Runner,
    cleanup: fn(String, String, config_types.HooksConfig) ->
      Result(Nil, error.WorkspaceError),
    logger: fn(String, String, List(log.Field), List(String)) ->
      Result(Nil, Nil),
    now_ms: fn() -> Int,
    send_after: fn(process.Subject(Message), Int, Message) -> TimerHandle,
    cancel_timer: fn(TimerHandle) -> Nil,
    start_event_hub: fn() -> Result(process.Subject(hub.Message), hub.HubError),
    make_control_token: fn() -> Result(String, StartupError),
    start_control_server: fn(control_server.Settings, control_server.Backend) ->
      Result(ControlServerHandle, StartupError),
    stop_control_server: fn(ControlServerHandle) -> Nil,
    start_remote_client: fn(
      config_types.EffectiveConfig,
      Option(managed_launch_grant.Grant),
      process.Subject(hub.Message),
      process.Subject(Message),
      List(String),
      fn(String, String, List(log.Field), List(String)) -> Result(Nil, Nil),
    ) -> Result(remote.Handle, StartupError),
    stop_remote_client: fn(remote.Handle, Int) -> Result(Nil, Nil),
    monitor_remote_client: fn(remote.Handle) -> process.Monitor,
    enqueue_startup_recovery_message: fn(process.Subject(Message), Message) ->
      Nil,
    observe_startup_recovery_stage: fn(String) -> Nil,
    emit_work_item_invalidation: fn(
      Option(remote.Handle),
      work_item_invalidation.Event,
    ) -> Nil,
    check_transition_invariants: daemon_transition_shell.InvariantChecker,
  )
}

type State {
  State(
    subject: process.Subject(Message),
    workflow: workflow_reloader.State,
    tracker_client: tracker.Client,
    tracker_adapter: adapter.TrackerAdapter,
    scheduled_runtime: scheduled_runtime.Runtime,
    scheduled_retry_timers: Dict(String, TimerHandle),
    scheduled_report_retry_timers: Dict(String, TimerHandle),
    runtime: orchestrator_state.RuntimeState,
    workers: transition_types.WorkerDirectory,
    poll: poll_scheduler.State(TimerHandle),
    retry: retry_scheduler.State(TimerHandle),
    registry: worker_registry.Registry,
    yaml_step_tokens: session_metrics.StepTokenEntries,
    pending_claims: Dict(identity.TaskIdentity, transition_types.PendingClaim),
    dispatch_recovery_cleared_pending_claims: List(identity.TaskIdentity),
    pending_dispatch_validations: Dict(
      identity.TaskIdentity,
      transition_types.PendingDispatchValidation,
    ),
    pending_review_lane_preflights: Dict(
      identity.TaskIdentity,
      transition_types.PendingReviewLanePreflight,
    ),
    next_dispatch_validation_generation: Int,
    recovery_by_issue: Dict(String, session_event.RecoveryInfo),
    effect_runner: effect_runner.Handle,
    effect_runner_monitor: process.Monitor,
    event_hub: process.Subject(hub.Message),
    control_server: ControlServerHandle,
    control_server_monitor: Option(process.Monitor),
    control_file_path: Option(String),
    query_service: query_service.Handle,
    read_model: read_model.ReadModel,
    ledger_projection: projection.Projection,
    remote_client: Option(remote.Handle),
    remote_client_monitor: Option(process.Monitor),
    managed_launch: Option(managed_launch_grant.Grant),
    operator_paused: Bool,
    pending_operator_command_replies: Dict(
      String,
      process.Subject(command.CommandResult),
    ),
    completed_operator_command_results: Dict(String, command.CommandResult),
    active_control_operations: Dict(String, Bool),
    work_item_action_receipts: Dict(String, action_receipts.Receipt),
    next_operator_command_correlation_id: Int,
    startup_recovery: StartupRecoveryPhase,
    next_startup_recovery_waiter_id: Int,
    pending_startup_recovery_waiters: Dict(
      Int,
      process.Subject(Result(Nil, Nil)),
    ),
    transition_invariant_violation_pending: Bool,
    dependencies: RuntimeDependencies,
  )
}

pub fn default_dependencies() -> RuntimeDependencies {
  RuntimeDependencies(
    make_tracker_adapter: linear_adapter.real,
    workflow_run_dependencies: workflow_run.default_dependencies(),
    publication_command_runner: command_runner.production(),
    cleanup: workspace.cleanup_stored_path,
    logger: fn(_level, _event, _fields, _secrets) { Ok(Nil) },
    now_ms: wall_clock_ms,
    send_after: fn(subject, delay_ms, message) {
      RealTimer(process.send_after(subject, delay_ms, message))
    },
    cancel_timer: fn(timer) {
      case timer {
        RealTimer(timer) -> {
          let _timer_cancelled = process.cancel_timer(timer)
          Nil
        }
        TestTimer(_) -> Nil
      }
    },
    start_event_hub: fn() {
      hub.start(hub.default_max_events_per_session, wall_clock_ms)
    },
    make_control_token: fn() {
      control_file.generate_token() |> map_control_file_error
    },
    start_control_server: fn(settings, store) {
      case control_server.start(settings, store) {
        Ok(server) -> Ok(RealControlServer(server))
        Error(control_server.ServerStartFailed(message)) ->
          Error(StartupError("control_server_start_failed", message))
      }
    },
    stop_control_server: fn(handle) {
      case handle {
        NoControlServer -> Nil
        RealControlServer(server) -> control_server.stop(server)
      }
    },
    start_remote_client: fn(
      effective,
      managed_launch,
      event_hub,
      daemon_subject,
      secrets,
      logger,
    ) {
      remote.start_remote_client(
        effective,
        managed_launch,
        event_hub,
        daemon_subject,
        secrets,
        logger,
        remote.control_dependencies(
          apply_operator_command: apply_operator_command,
          get_remote_dispatch_paused: get_remote_dispatch_paused,
          execute_query: execute_query,
        ),
      )
      |> result.map_error(fn(err) {
        let #(code, message) = remote.start_error_fields(err)
        StartupError(code, message)
      })
    },
    stop_remote_client: remote.stop,
    monitor_remote_client: remote.monitor,
    enqueue_startup_recovery_message: process.send,
    observe_startup_recovery_stage: fn(_) { Nil },
    emit_work_item_invalidation: fn(remote_client, event) {
      case remote_client {
        Some(handle) -> remote.notify_work_item_invalidation(handle, event)
        None -> Nil
      }
    },
    check_transition_invariants: daemon_transition_shell.default_invariant_checker,
  )
}

fn emit_runtime_log(
  dependencies: RuntimeDependencies,
  level: String,
  event: String,
  fields: List(log.Field),
  secrets: List(String),
) -> Nil {
  case dependencies.logger(level, event, fields, secrets) {
    Ok(Nil) -> Nil
    Error(Nil) -> Nil
  }
}

fn start_startup_phase_tracker(
  initial_phase: String,
) -> process.Subject(StartupPhaseMessage) {
  let ready = process.new_subject()
  let _pid =
    process.spawn_unlinked(fn() {
      let subject = process.new_subject()
      process.send(ready, subject)
      startup_phase_tracker_loop(subject, initial_phase)
    })
  let assert Ok(subject) = process.receive(ready, within: 1000)
  subject
}

fn startup_phase_tracker_loop(
  subject: process.Subject(StartupPhaseMessage),
  current_phase: String,
) -> Nil {
  case process.receive(subject, within: 60_000) {
    Ok(SetStartupPhase(phase)) -> startup_phase_tracker_loop(subject, phase)
    Ok(GetStartupPhase(reply)) -> {
      process.send(reply, current_phase)
      startup_phase_tracker_loop(subject, current_phase)
    }
    Ok(StopStartupPhaseTracker) -> Nil
    Error(Nil) -> startup_phase_tracker_loop(subject, current_phase)
  }
}

fn set_startup_phase(
  tracker: process.Subject(StartupPhaseMessage),
  phase: String,
) -> Nil {
  process.send(tracker, SetStartupPhase(phase))
}

fn current_startup_phase(
  tracker: process.Subject(StartupPhaseMessage),
) -> String {
  let reply = process.new_subject()
  process.send(tracker, GetStartupPhase(reply))
  case process.receive(reply, within: 1000) {
    Ok(phase) -> phase
    Error(Nil) -> "unknown"
  }
}

fn stop_startup_phase_tracker(
  tracker: process.Subject(StartupPhaseMessage),
) -> Nil {
  process.send(tracker, StopStartupPhaseTracker)
}

fn validate_tracker_capabilities(
  tracker_adapter: adapter.TrackerAdapter,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(Nil, StartupError) {
  let requirements = tracker_requirements(orchestrator)
  case adapter.validate_required_capabilities(tracker_adapter, requirements) {
    Ok(Nil) -> Ok(Nil)
    Error(errors) ->
      Error(StartupError(
        "tracker_capability_missing",
        errors
          |> list.map(adapter.capability_validation_error_message)
          |> string.join(with: "\n"),
      ))
  }
}

fn tracker_requirements(
  orchestrator: config_types.OrchestratorConfig,
) -> adapter.TrackerRequirements {
  let effective = orchestrator.effective
  adapter.TrackerRequirements(
    remote_commands_enabled: False,
    remote_commands_config_path: None,
    handoff_comments_enabled: handoff_comments_enabled(effective.handoff),
    handoff_state_moves_enabled: handoff_state_moves_enabled(effective.handoff),
    handoff_config_path: Some("task_updates"),
    workflow_label_paths: workflow_label_paths(orchestrator.routing),
    scheduled_failure_paths: scheduled_failure_paths(
      orchestrator.scheduled_jobs,
    ),
    readiness_checks_enabled: False,
    smoke_checks_enabled: False,
  )
}

fn handoff_comments_enabled(handoff: config_types.HandoffConfig) -> Bool {
  handoff.enabled
  && {
    handoff.comment_on_claim
    || handoff.comment_on_success
    || handoff.comment_on_failure
    || handoff.comment_on_park
    || handoff.attach_result_on_success
  }
}

fn handoff_state_moves_enabled(handoff: config_types.HandoffConfig) -> Bool {
  handoff.enabled
  && {
    handoff.claim_state_id != None
    || handoff.success_state_id != None
    || handoff.failure_state_id != None
    || handoff.completion_states != None
  }
}

fn workflow_label_paths(routing: config_types.RoutingConfig) -> List(String) {
  case routing.workflow_label_prefix == "" {
    True -> []
    False ->
      routing.workflows
      |> dict.keys
      |> list.map(fn(id) { "workflows." <> id <> ".label" })
  }
}

fn scheduled_failure_paths(
  jobs: List(config_types.ScheduledJobConfig),
) -> List(String) {
  jobs
  |> list.filter_map(fn(job) {
    case job.on_failure.task.enabled {
      True -> Ok("schedules." <> job.id <> ".on_failure.task")
      False -> Error(Nil)
    }
  })
}

fn start_query_service(
  effective: config_types.EffectiveConfig,
  daemon_subject: process.Subject(Message),
  identity: daemon_identity.DaemonIdentity,
  _now_ms: fn() -> Int,
  tracker_adapter: adapter.TrackerAdapter,
) -> Result(query_service.Handle, StartupError) {
  query_runtime.start(
    effective,
    identity,
    tracker_adapter,
    get_dispatch_paused: fn(timeout_ms) {
      get_remote_dispatch_paused(daemon_subject, timeout_ms)
    },
    get_read_model_snapshot: fn(timeout_ms) {
      get_read_model_snapshot(daemon_subject, timeout_ms)
    },
    get_projection_snapshot: fn(timeout_ms) {
      get_projection_snapshot(daemon_subject, timeout_ms)
    },
    get_outbox_snapshot: fn(timeout_ms) {
      get_outbox_snapshot(daemon_subject, timeout_ms)
    },
    get_workflow_snapshot: fn(timeout_ms) {
      get_workflow_snapshot(daemon_subject, timeout_ms)
    },
  )
  |> result.map_error(fn(error) {
    let query_service.StartError(code, message) = error
    StartupError(code, message)
  })
}

fn runtime_counts_from_state(state: State) -> read_model.RuntimeCounts {
  let running_workers = dict.size(state.runtime.running)
  let running_scheduled_workers =
    worker_registry.scheduled_worker_count(state.registry)
  let now_ms = state.dependencies.now_ms()
  let #(
    pending_outbox_count,
    in_flight_outbox_count,
    retryable_outbox_count,
    permanent_outbox_count,
  ) = outbox_counts_for_metrics(state)
  read_model.RuntimeCounts(
    workflow_count: dict.size(state.workflow.bundle.workflows),
    scheduled_job_count: list.length(
      state.workflow.bundle.orchestrator.scheduled_jobs,
    ),
    active_sessions: running_workers
      + running_scheduled_workers
      + worker_registry.active_yaml_step_session_count(state.registry),
    running_workers: running_workers,
    running_scheduled_workers: running_scheduled_workers,
    queued_claims: dict.size(state.pending_claims),
    pending_dispatch_validations: dict.size(state.pending_dispatch_validations),
    pending_review_lane_preflights: dict.size(
      state.pending_review_lane_preflights,
    ),
    claimed_tasks: dict.size(state.runtime.claimed),
    retry_tasks: dict.size(state.runtime.retry_attempts),
    parked_tasks: dict.size(state.runtime.parked),
    completed_tasks: dict.size(state.runtime.completed),
    pending_outbox_count: pending_outbox_count,
    in_flight_outbox_count: in_flight_outbox_count,
    retryable_outbox_count: retryable_outbox_count,
    permanent_outbox_count: permanent_outbox_count,
    poll_generation: poll_scheduler.generation(state.poll),
    poll_in_flight: poll_scheduler.in_flight(state.poll) != None,
    poll_timer_active: poll_scheduler.timer(state.poll) != None,
    retry_timer_count: retry_scheduler.timer_count(state.retry),
    retry_refresh_in_flight_count: retry_scheduler.refresh_in_flight_count(
      state.retry,
    ),
    lifecycle_projection_failed: daemon_transition_shell.lifecycle_projection_failed(
      transition_state_from_daemon(state),
    ),
    scheduled_due_count: state.scheduled_runtime
      |> scheduled_runtime.due_count(now_ms),
    scheduled_next_due_count: scheduled_runtime.next_due_count(
      state.scheduled_runtime,
    ),
    scheduled_pending_count: dict.size(state.scheduled_runtime.pending_starts),
    scheduled_retry_count: dict.size(state.scheduled_runtime.scheduled_retries),
    scheduled_report_retry_count: dict.size(
      state.scheduled_runtime.scheduled_report_retries,
    ),
    scheduled_retry_timer_count: dict.size(state.scheduled_retry_timers),
    scheduled_report_retry_timer_count: dict.size(
      state.scheduled_report_retry_timers,
    ),
  )
}

fn outbox_counts_for_metrics(state: State) -> #(Int, Int, Int, Int) {
  state.ledger_projection.outbox
  |> dict.values
  |> list.fold(#(0, 0, 0, 0), fn(counts, status) {
    let #(pending, in_flight, retryable, permanent) = counts
    case status {
      projection.OutboxPendingV2(_, _, _, _, _)
      | projection.OutboxPendingV2WithTask(_, _, _, _, _) -> #(
        pending + 1,
        in_flight,
        retryable,
        permanent,
      )
      projection.OutboxAttempted(_, _, _, _, _, _)
      | projection.OutboxAttemptedWithTask(_, _, _, _, _, _) -> #(
        pending,
        in_flight + 1,
        retryable,
        permanent,
      )
      projection.OutboxRetryScheduled(_, _, _, _, _, _, _, _)
      | projection.OutboxRetryScheduledWithTask(_, _, _, _, _, _, _, _) -> #(
        pending,
        in_flight,
        retryable + 1,
        permanent,
      )
      projection.OutboxPermanentlyFailed(_, _, _, _, _)
      | projection.OutboxPermanentlyFailedWithTask(_, _, _, _, _) -> #(
        pending,
        in_flight,
        retryable,
        permanent + 1,
      )
      _ -> counts
    }
  })
}

fn metrics_token_totals(state: State) -> session_tokens.TokenTotals {
  session_metrics.total(state.yaml_step_tokens)
  |> session_tokens.add(state.runtime.aggregate_pi_totals)
}

fn refresh_read_model(state: State) -> State {
  let refreshed =
    state.read_model
    |> read_model.update_counts(runtime_counts_from_state(state))
    |> read_model.update_dispatch_paused(dispatch_paused: state.operator_paused)
    |> read_model.update_token_totals(metrics_token_totals(state))

  State(..state, read_model: refreshed)
}

fn read_model_snapshot_from_state(state: State) -> read_model.Snapshot {
  read_model.snapshot(
    state.read_model,
    sampled_at_ms: state.dependencies.now_ms(),
  )
}

fn start_control_plane(
  dependencies: RuntimeDependencies,
  effective: config_types.EffectiveConfig,
  event_hub: process.Subject(hub.Message),
  daemon_subject: process.Subject(Message),
  query_handle: query_service.Handle,
  secrets: List(String),
) -> Result(ControlPlane, StartupError) {
  use token <- try_startup(dependencies.make_control_token())
  let settings = control_server.settings_for_control(token, effective.control)
  use handle <- try_startup(dependencies.start_control_server(
    settings,
    control_backend(event_hub, daemon_subject, query_handle),
  ))
  case handle {
    NoControlServer -> Ok(ControlPlane(handle: handle, control_file_path: None))
    RealControlServer(server) -> {
      let port = control_server.bound_port(server)
      let path = control_file.path_for_workspace(effective.workspace.root)
      let control =
        control_file.ControlFile(
          host: "127.0.0.1",
          port: port,
          token: token,
          workspace_root: effective.workspace.root,
          started_at_ms: dependencies.now_ms(),
          command_timeout_ms: settings.command_timeout_ms,
        )
      case control_file.write(path, control) {
        Ok(Nil) -> {
          emit_runtime_log(
            dependencies,
            "info",
            "control_server_started",
            [
              #("control_file", path),
              #("host", "127.0.0.1"),
              #("port", int.to_string(port)),
            ],
            secrets,
          )
          Ok(ControlPlane(handle: handle, control_file_path: Some(path)))
        }
        Error(err) -> {
          dependencies.stop_control_server(handle)
          control_file.remove(path)
          map_control_file_error(Error(err))
        }
      }
    }
  }
}

fn stop_control_plane(
  dependencies: RuntimeDependencies,
  control_plane: ControlPlane,
) -> Nil {
  dependencies.stop_control_server(control_plane.handle)
  case control_plane.control_file_path {
    Some(path) -> control_file.remove(path)
    None -> Nil
  }
}

fn monitor_control_server(
  handle: ControlServerHandle,
) -> Option(process.Monitor) {
  case handle {
    NoControlServer -> None
    RealControlServer(server) -> Some(control_server.monitor(server))
  }
}

fn start_remote_client_now(state: State) -> State {
  case state.remote_client {
    Some(_) -> state
    None -> restart_remote_client_if_enabled(state)
  }
}

fn update_read_model_remote_client_status(
  state: State,
  status: read_model.RemoteClientStatus,
) -> State {
  State(
    ..state,
    read_model: read_model.update_remote_client_status(state.read_model, status),
  )
}

fn restart_remote_client_if_enabled(state: State) -> State {
  case
    ui_server_enabled(state.workflow.effective.ui_server),
    state.managed_launch
  {
    False, None -> stop_remote_client_and_clear(state, read_model.Disabled)
    _, _ ->
      case
        state.dependencies.start_remote_client(
          state.workflow.effective,
          state.managed_launch,
          state.event_hub,
          state.subject,
          state.workflow.secrets,
          state.dependencies.logger,
        )
      {
        Ok(handle) ->
          state
          |> update_read_model_remote_client_status(read_model.Connected)
          |> fn(state) {
            State(
              ..state,
              remote_client: Some(handle),
              remote_client_monitor: Some(
                state.dependencies.monitor_remote_client(handle),
              ),
            )
          }
        Error(StartupError(code, message)) -> {
          log_state(state, "warn", "remote_client_restart_failed", [
            #("code", code),
            #("message", message),
          ])
          state
          |> stop_remote_client_and_clear(read_model.Retrying(code))
        }
      }
  }
}

fn stop_remote_client_and_clear(
  state: State,
  status: read_model.RemoteClientStatus,
) -> State {
  case state.remote_client_monitor {
    Some(monitor) -> process.demonitor_process(monitor)
    None -> Nil
  }
  case state.remote_client {
    Some(handle) ->
      case state.dependencies.stop_remote_client(handle, 1000) {
        Ok(Nil) -> Nil
        Error(Nil) -> {
          remote.kill(handle)
          log_state(state, "warn", "remote_client_shutdown_timeout", [
            #("timeout_ms", "1000"),
          ])
        }
      }
    None -> Nil
  }
  state
  |> update_read_model_remote_client_status(status)
  |> fn(state) {
    State(..state, remote_client: None, remote_client_monitor: None)
  }
}

fn control_backend(
  event_hub: process.Subject(hub.Message),
  daemon_subject: process.Subject(Message),
  query_handle: query_service.Handle,
) -> control_server.Backend {
  let read_backend = control_server.event_hub_store(event_hub)
  control_server.Backend(
    ..read_backend,
    query: fn(query) { query_service.query(query_handle, query) },
    apply_command: fn(operator_command, timeout_ms) {
      apply_operator_command(daemon_subject, operator_command, timeout_ms)
    },
  )
}

pub fn apply_operator_command(
  daemon_subject: process.Subject(Message),
  operator_command: command.OperatorCommand,
  timeout_ms: Int,
) -> Result(command.CommandResult, Nil) {
  let reply =
    apply_operator_command_async(daemon_subject, operator_command, timeout_ms)
  process.receive(reply, within: timeout_ms)
}

pub fn apply_operator_command_async(
  daemon_subject: process.Subject(Message),
  operator_command: command.OperatorCommand,
  timeout_ms: Int,
) -> process.Subject(command.CommandResult) {
  let reply = process.new_subject()
  process.send(
    daemon_subject,
    ApplyOperatorCommand(operator_command, timeout_ms, reply),
  )
  reply
}

pub fn execute_query(
  daemon_subject: process.Subject(Message),
  query: query_types.QueryRequest,
  timeout_ms: Int,
) -> Result(query_types.QueryResponse, query_types.QueryError) {
  remote.call_without_late_reply(
    send_request: fn(reply) {
      process.send(daemon_subject, ExecuteQuery(query, timeout_ms, reply))
    },
    timeout_ms: timeout_ms,
    timeout_value: Error(query_types.QueryError(
      query_types.QueryTimeout,
      "daemon query timed out",
    )),
  )
}

pub fn get_remote_dispatch_paused(
  daemon_subject: process.Subject(Message),
  timeout_ms: Int,
) -> Result(Bool, Nil) {
  remote.call_result_without_late_reply(
    fn(reply) { process.send(daemon_subject, GetRemoteDispatchPaused(reply)) },
    timeout_ms,
    Nil,
  )
}

pub fn start(
  workflow_path: Option(String),
  dependencies: RuntimeDependencies,
) -> Result(actor.Started(process.Subject(Message)), StartupError) {
  start_with_initialiser_timeout(workflow_path, dependencies, 60_000)
}

pub fn start_with_initialiser_timeout(
  workflow_path: Option(String),
  dependencies: RuntimeDependencies,
  initialiser_timeout_ms: Int,
) -> Result(actor.Started(process.Subject(Message)), StartupError) {
  start_with_managed_launch_and_initialiser_timeout(
    workflow_path,
    None,
    dependencies,
    initialiser_timeout_ms,
  )
}

pub fn start_with_managed_launch(
  workflow_path: Option(String),
  managed_launch: Option(managed_launch_grant.Grant),
  dependencies: RuntimeDependencies,
) -> Result(actor.Started(process.Subject(Message)), StartupError) {
  start_with_managed_launch_and_initialiser_timeout(
    workflow_path,
    managed_launch,
    dependencies,
    60_000,
  )
}

fn start_with_managed_launch_and_initialiser_timeout(
  workflow_path: Option(String),
  managed_launch: Option(managed_launch_grant.Grant),
  dependencies: RuntimeDependencies,
  initialiser_timeout_ms: Int,
) -> Result(actor.Started(process.Subject(Message)), StartupError) {
  use bundle <- try_startup(
    runtime_bundle.load(workflow_path)
    |> map_bundle_error,
  )
  let workflow = workflow_reloader.from_bundle(workflow_path, bundle)
  let effective = bundle.effective
  let tracker_adapter = dependencies.make_tracker_adapter(effective)
  use Nil <- try_startup(validate_tracker_capabilities(
    tracker_adapter,
    bundle.orchestrator,
  ))
  let tracker_client = adapter_legacy.workflow_compat_client(tracker_adapter)
  let secrets = config.resolved_secrets(effective)
  use startup_recovery <- try_startup(
    startup_recovery.load(
      bundle,
      tracker_adapter,
      startup_recovery.Dependencies(
        logger: dependencies.logger,
        now_ms: dependencies.now_ms,
        sleep_ms: process.sleep,
      ),
      secrets,
    )
    |> map_startup_recovery_error,
  )
  let runtime = startup_recovery.runtime
  use event_hub <- try_startup(dependencies.start_event_hub() |> map_hub_error)
  use daemon_identity <- try_startup(
    daemon_identity.load_or_create(effective.workspace.root)
    |> result.map_error(fn(err) {
      StartupError("daemon_identity_failed", daemon_identity.error_message(err))
    }),
  )
  let startup_phase = start_startup_phase_tracker("pre_actor_startup_complete")
  let builder =
    actor.new_with_initialiser(initialiser_timeout_ms, fn(subject) {
      set_startup_phase(startup_phase, "query_service_starting")
      case
        start_query_service(
          effective,
          subject,
          daemon_identity,
          dependencies.now_ms,
          tracker_adapter,
        )
      {
        Error(err) -> Error(encode_startup_error(err))
        Ok(query_handle) -> {
          set_startup_phase(startup_phase, "control_plane_starting")
          case
            start_control_plane(
              dependencies,
              effective,
              event_hub,
              subject,
              query_handle,
              secrets,
            )
          {
            Error(err) -> {
              let _best_effort = query_service.stop(query_handle, 1000)
              Error(encode_startup_error(err))
            }
            Ok(control_plane) -> {
              set_startup_phase(startup_phase, "effect_runner_starting")
              case
                effect_runner.start(
                  effect_runner.Dependencies(
                    max_concurrent: 4,
                    notify: fn(completion) {
                      process.send(subject, SideEffectCompleted(completion))
                    },
                  ),
                )
              {
                Error(_) -> {
                  stop_control_plane(dependencies, control_plane)
                  let _best_effort = query_service.stop(query_handle, 1000)
                  Error(
                    encode_startup_error(StartupError(
                      "effect_runner_start_failed",
                      "effect runner start failed",
                    )),
                  )
                }
                Ok(effect_runner_handle) -> {
                  let effect_runner_monitor =
                    effect_runner.monitor(effect_runner_handle)
                  case effect_runner.is_alive(effect_runner_handle) {
                    False -> {
                      process.demonitor_process(effect_runner_monitor)
                      stop_control_plane(dependencies, control_plane)
                      let _best_effort = query_service.stop(query_handle, 1000)
                      Error(
                        encode_startup_error(StartupError(
                          "effect_runner_start_failed",
                          "effect runner exited during startup",
                        )),
                      )
                    }
                    True -> {
                      set_startup_phase(
                        startup_phase,
                        "constructing_startup_state",
                      )
                      let control_server_monitor =
                        monitor_control_server(control_plane.handle)
                      let state =
                        State(
                          subject: subject,
                          workflow: workflow,
                          tracker_client: tracker_client,
                          tracker_adapter: tracker_adapter,
                          scheduled_runtime: startup_recovery.scheduled.runtime,
                          scheduled_retry_timers: dict.new(),
                          scheduled_report_retry_timers: dict.new(),
                          runtime: runtime,
                          workers: transition_types.new_worker_directory(),
                          poll: poll_scheduler.idle(),
                          retry: retry_scheduler.new(),
                          registry: worker_registry.new(),
                          yaml_step_tokens: session_metrics.new(),
                          pending_claims: dict.new(),
                          dispatch_recovery_cleared_pending_claims: [],
                          pending_dispatch_validations: dict.new(),
                          pending_review_lane_preflights: dict.new(),
                          next_dispatch_validation_generation: 1,
                          recovery_by_issue: startup_recovery.recovery_by_issue,
                          effect_runner: effect_runner_handle,
                          effect_runner_monitor: effect_runner_monitor,
                          event_hub: event_hub,
                          control_server: control_plane.handle,
                          control_server_monitor: control_server_monitor,
                          control_file_path: control_plane.control_file_path,
                          query_service: query_handle,
                          read_model: read_model.new(
                            daemon_id: daemon_identity.daemon_id,
                            boot_id: daemon_identity.boot_id,
                            ui_server_enabled: ui_server_enabled(
                              effective.ui_server,
                            ),
                          ),
                          ledger_projection: startup_recovery.projection,
                          remote_client: None,
                          remote_client_monitor: None,
                          managed_launch: managed_launch,
                          operator_paused: startup_recovery.projection.dispatch_paused,
                          pending_operator_command_replies: dict.new(),
                          completed_operator_command_results: dict.new(),
                          active_control_operations: dict.new(),
                          work_item_action_receipts: action_receipts.empty(),
                          next_operator_command_correlation_id: 1,
                          startup_recovery: StartupRecoveryPending(
                            startup_recovery,
                          ),
                          next_startup_recovery_waiter_id: 1,
                          pending_startup_recovery_waiters: dict.new(),
                          transition_invariant_violation_pending: False,
                          dependencies: dependencies,
                        )
                        |> log_startup_invariant_warn_mode
                      dependencies.enqueue_startup_recovery_message(
                        subject,
                        ContinueStartupRecovery,
                      )
                      let selector =
                        process.new_selector()
                        |> process.select(subject)
                        |> process.select_specific_monitor(
                          effect_runner_monitor,
                          fn(down) { EffectRunnerDown(down) },
                        )
                      let selector = case control_server_monitor {
                        Some(monitor) ->
                          process.select_specific_monitor(
                            selector,
                            monitor,
                            fn(down) { ControlServerDown(down) },
                          )
                        None -> selector
                      }
                      let selector =
                        process.select_monitors(selector, WorkerDown)
                      set_startup_phase(startup_phase, "actor_initialised")
                      actor.initialised(state)
                      |> actor.selecting(selector)
                      |> actor.returning(subject)
                      |> Ok
                    }
                  }
                }
              }
            }
          }
        }
      }
    })
    |> actor.on_message(handle_message)
  let result = case actor.start(builder) {
    Ok(started) -> Ok(started)
    Error(actor.InitFailed(reason)) -> Error(decode_startup_error(reason))
    Error(actor.InitTimeout) -> {
      let last_phase = current_startup_phase(startup_phase)
      emit_runtime_log(
        dependencies,
        "error",
        "daemon_startup_timeout",
        [
          #("initialiser_timeout_ms", int.to_string(initialiser_timeout_ms)),
          #("last_startup_phase", last_phase),
        ],
        secrets,
      )
      Error(StartupError(
        "daemon_actor_init_timeout",
        "daemon actor initializer timed out during " <> last_phase,
      ))
    }
    Error(_) -> Error(StartupError("daemon_start_failed", "actor start failed"))
  }
  stop_startup_phase_tracker(startup_phase)
  result
}

fn encode_startup_error(error: StartupError) -> String {
  error.code <> "\t" <> error.message
}

fn decode_startup_error(reason: String) -> StartupError {
  case string.split_once(reason, on: "\t") {
    Ok(#(code, message)) -> StartupError(code, message)
    Error(Nil) -> StartupError("daemon_start_failed", reason)
  }
}

pub fn shutdown(
  subject: process.Subject(Message),
  timeout_ms: Int,
) -> Result(Nil, Nil) {
  let reply = process.new_subject()
  process.send(subject, Shutdown(reply))
  process.receive(reply, within: timeout_ms)
}

pub fn get_snapshot(
  subject: process.Subject(Message),
  timeout_ms: Int,
) -> Result(orchestrator_state.RuntimeState, Nil) {
  let reply = process.new_subject()
  process.send(subject, GetSnapshot(reply))
  process.receive(reply, within: timeout_ms)
}

pub fn get_read_model_snapshot(
  subject: process.Subject(Message),
  timeout_ms: Int,
) -> Result(read_model.Snapshot, Nil) {
  let reply = process.new_subject()
  process.send(subject, GetReadModelSnapshot(reply))
  process.receive(reply, within: timeout_ms)
}

pub fn get_projection_snapshot(
  subject: process.Subject(Message),
  timeout_ms: Int,
) -> Result(projection.Projection, Nil) {
  let reply = process.new_subject()
  process.send(subject, GetProjectionSnapshot(reply))
  process.receive(reply, within: timeout_ms)
}

pub fn get_outbox_snapshot(
  subject: process.Subject(Message),
  timeout_ms: Int,
) -> Result(List(#(String, projection.OutboxStatus)), Nil) {
  let reply = process.new_subject()
  process.send(subject, GetOutboxSnapshot(reply))
  process.receive(reply, within: timeout_ms)
}

pub fn await_startup_recovery_ready(
  subject: process.Subject(Message),
  timeout_ms: Int,
) -> Result(Nil, Nil) {
  let reply = process.new_subject()
  process.send(subject, AwaitStartupRecoveryReady(reply, timeout_ms))
  case process.receive(reply, within: timeout_ms) {
    Ok(result) -> result
    Error(Nil) -> Error(Nil)
  }
}

pub fn get_workflow_snapshot(
  subject: process.Subject(Message),
  timeout_ms: Int,
) -> Result(workflow_reloader.State, Nil) {
  let reply = process.new_subject()
  process.send(subject, GetWorkflowSnapshot(reply))
  process.receive(reply, within: timeout_ms)
}

fn apply_scheduled_startup_recovery(
  state: State,
  scheduled: startup_recovery.ScheduledRecovery,
) -> State {
  case state.transition_invariant_violation_pending {
    True -> state
    False ->
      list.fold(scheduled.effects, state, fn(state, effect) {
        apply_scheduled_startup_effect(state, effect)
      })
  }
}

fn apply_scheduled_startup_effect(
  state: State,
  effect: startup_recovery.ScheduledRecoveryEffect,
) -> State {
  case effect {
    startup_recovery.AppendLedger(record_bodies, failure_event) ->
      append_ledger_bodies_best_effort(state, record_bodies, failure_event)
    startup_recovery.ApplyScheduledRuntimeActions(actions, append_retry_record) ->
      apply_scheduled_runtime_actions(
        state,
        actions,
        append_retry_record: append_retry_record,
      )
    startup_recovery.ScheduleReportRetryTimer(run_id, generation, delay_ms) ->
      schedule_scheduled_report_retry_timer(state, run_id, generation, delay_ms)
    startup_recovery.BeginFailureReport(request) ->
      begin_scheduled_failure_report_request(state, request)
  }
}

fn scheduled_job_by_id(
  state: State,
  job_id: String,
) -> Result(config_types.ScheduledJobConfig, Nil) {
  state.workflow.bundle.orchestrator.scheduled_jobs
  |> list.find(fn(job) { job.id == job_id })
}

fn apply_startup_recovery(
  state: State,
  startup_recovery: startup_recovery.StartupRecovery,
) -> State {
  run_transition_messages(state, [
    transition_types.StartupRecoveryApplied(
      retry_timers: startup_recovery.retry_timers,
      cleanup_workspaces: startup_recovery.cleanup_workspaces,
      outbox_to_replay: startup_recovery.outbox_to_replay,
      park_reports: startup_recovery.park_reports,
      warnings: startup_recovery.warnings,
      secrets: state.workflow.secrets,
    ),
  ])
}

fn log_startup_invariant_warn_mode(state: State) -> State {
  case transition_invariant_mode_from_env() {
    daemon_transition_shell.WarnOnInvariantViolation -> {
      log_state(state, "warn", "transition_invariants_warn_mode_enabled", [
        #("env", "SCHERZO_INVARIANTS=warn"),
      ])
      state
    }
    daemon_transition_shell.FailOnInvariantViolation -> state
  }
}

fn check_startup_transition_invariants(state: State) -> State {
  case state.transition_invariant_violation_pending {
    True -> state
    False ->
      daemon_transition_shell.check_invariants(transition_shell_context(state))
  }
}

fn spawn_recovered_workflow_resumptions(
  state: State,
  resumptions: List(recovery.RecoveredWorkflowRun),
) -> State {
  case state.transition_invariant_violation_pending {
    True -> state
    False ->
      list.fold(resumptions, state, fn(state, resumption) {
        spawn_recovered_workflow_resumption(state, resumption)
      })
  }
}

fn replay_incomplete_control_operations(
  subject: process.Subject(Message),
  state: State,
) -> Nil {
  [
    "retry_step",
    "artifact_publication_retry",
    "recollect_outputs",
    "run_finalize",
  ]
  |> list.each(fn(operation_kind) {
    projection.replayable_control_operation_ids(
      state.ledger_projection,
      operation_kind,
    )
    |> list.each(fn(operation_id) {
      process.send(subject, RunQueuedControlOperation(operation_id))
    })
  })
}

fn startup_recovery_ready(state: State) -> Bool {
  case state.startup_recovery {
    StartupRecoveryReady -> True
    StartupRecoveryPending(_)
    | StartupRecoveryRunning(_, _)
    | StartupRecoveryFailed(_) -> False
  }
}

fn queue_startup_recovery_continuation(state: State) -> State {
  state.dependencies.enqueue_startup_recovery_message(
    state.subject,
    ContinueStartupRecovery,
  )
  state
}

fn start_initial_poll(state: State) -> State {
  let poll =
    poll_scheduler.start(fn(generation) {
      state.dependencies.send_after(state.subject, 0, PollTick(generation))
    })
  State(..state, poll: poll)
}

fn notify_startup_recovery_waiters(
  waiters: Dict(Int, process.Subject(Result(Nil, Nil))),
  result: Result(Nil, Nil),
) -> Nil {
  waiters
  |> dict.values
  |> list.each(fn(reply) { process.send(reply, result) })
}

fn fail_pending_startup_recovery_waiters(state: State) -> State {
  notify_startup_recovery_waiters(
    state.pending_startup_recovery_waiters,
    Error(Nil),
  )
  State(..state, pending_startup_recovery_waiters: dict.new())
}

fn defer_until_startup_recovery_ready(
  state: State,
  message: Message,
) -> actor.Next(State, Message) {
  process.send(state.subject, message)
  actor.continue(state)
}

fn startup_recovery_should_defer(state: State, message: Message) -> Bool {
  case startup_recovery_ready(state) {
    True -> False
    False ->
      case message {
        ContinueStartupRecovery
        | PollTick(_)
        | AwaitStartupRecoveryReady(_, _)
        | StartRemoteClient
        | RunQueuedControlOperation(_) -> False
        RetryTick(_, _)
        | DispatchRecoveryContinue(_)
        | WorkerFinished(_, _, _)
        | ScheduledWorkerFinished(_, _)
        | ScheduledRetryTick(_, _)
        | ScheduledReportRetryTick(_, _)
        | WorkerUpdate(_, _)
        | WorkerCommandReady(_, _, _)
        | YamlStepStarted(_, _, _, _, _)
        | YamlStepUpdate(_, _)
        | YamlStepCommandReady(_, _)
        | YamlStepFinished(_, _)
        | SideEffectCompleted(_)
        | QueuedControlOperationFinished(_, _) -> True
        WorkerCommandCompleted(_, _, _)
        | WorkerCommandTimedOut(_, _)
        | AbortWorkerCommandTimedOut(_, _, _)
        | WorkerDown(_)
        | EffectRunnerDown(_)
        | ControlServerDown(_)
        | Shutdown(_)
        | GetSnapshot(_)
        | GetReadModelSnapshot(_)
        | GetProjectionSnapshot(_)
        | GetOutboxSnapshot(_)
        | GetWorkflowSnapshot(_)
        | GetRemoteDispatchPaused(_)
        | StartupRecoveryWaiterTimedOut(_)
        | ApplyOperatorCommand(_, _, _)
        | ExecuteQuery(_, _, _) -> False
      }
  }
}

fn complete_startup_recovery(state: State) -> State {
  let state = refresh_read_model(state)
  let waiters = state.pending_startup_recovery_waiters
  let state =
    State(
      ..state,
      startup_recovery: StartupRecoveryReady,
      pending_startup_recovery_waiters: dict.new(),
    )
  replay_incomplete_control_operations(state.subject, state)
  let state = start_initial_poll(state)
  process.send(state.subject, StartRemoteClient)
  notify_startup_recovery_waiters(waiters, Ok(Nil))
  state
}

fn advance_startup_recovery(state: State) -> State {
  case state.startup_recovery {
    StartupRecoveryReady | StartupRecoveryFailed(_) -> state
    StartupRecoveryPending(plan) -> {
      state.dependencies.observe_startup_recovery_stage("startup_recovery")
      apply_startup_recovery(state, plan)
      |> fn(state) {
        State(
          ..state,
          startup_recovery: StartupRecoveryRunning(
            StartupRecoveryStageApplyScheduledRecovery,
            plan,
          ),
        )
      }
      |> queue_startup_recovery_continuation
    }
    StartupRecoveryRunning(stage, plan) ->
      case stage {
        StartupRecoveryStageApplyRecovery -> {
          state.dependencies.observe_startup_recovery_stage("startup_recovery")
          apply_startup_recovery(state, plan)
          |> fn(state) {
            State(
              ..state,
              startup_recovery: StartupRecoveryRunning(
                StartupRecoveryStageApplyScheduledRecovery,
                plan,
              ),
            )
          }
          |> queue_startup_recovery_continuation
        }
        StartupRecoveryStageApplyScheduledRecovery -> {
          state.dependencies.observe_startup_recovery_stage(
            "scheduled_startup_recovery",
          )
          apply_scheduled_startup_recovery(state, plan.scheduled)
          |> fn(state) {
            State(
              ..state,
              startup_recovery: StartupRecoveryRunning(
                StartupRecoveryStageResumeWorkflows,
                plan,
              ),
            )
          }
          |> queue_startup_recovery_continuation
        }
        StartupRecoveryStageResumeWorkflows -> {
          state.dependencies.observe_startup_recovery_stage(
            "workflow_resumptions",
          )
          spawn_recovered_workflow_resumptions(state, plan.workflow_resumptions)
          |> fn(state) {
            State(
              ..state,
              startup_recovery: StartupRecoveryRunning(
                StartupRecoveryStageCheckInvariants,
                plan,
              ),
            )
          }
          |> queue_startup_recovery_continuation
        }
        StartupRecoveryStageCheckInvariants -> {
          state.dependencies.observe_startup_recovery_stage(
            "startup_transition_invariants",
          )
          let state = check_startup_transition_invariants(state)
          case state.transition_invariant_violation_pending {
            True ->
              State(
                ..fail_pending_startup_recovery_waiters(state),
                startup_recovery: StartupRecoveryFailed(
                  "transition_invariant_violation",
                ),
              )
            False ->
              State(
                ..state,
                startup_recovery: StartupRecoveryRunning(
                  StartupRecoveryStageFinish,
                  plan,
                ),
              )
              |> queue_startup_recovery_continuation
          }
        }
        StartupRecoveryStageFinish -> {
          state.dependencies.observe_startup_recovery_stage(
            "startup_recovery_ready",
          )
          complete_startup_recovery(state)
        }
      }
  }
}

fn spawn_recovered_workflow_resumption(
  state: State,
  recovered: recovery.RecoveredWorkflowRun,
) -> State {
  case has_active_run(state, recovered.issue.id) {
    True -> state
    False -> {
      let #(registry, session_sequence) =
        worker_registry.reserve_session_sequence(state.registry)
      let state = State(..state, registry: registry)
      let session_id =
        make_recovered_session_id(recovered.run_id, session_sequence)
      let started_at_ms = state.dependencies.now_ms()
      let recovery =
        Some(
          session_recovery.base_info(
            session_event.Resumed,
            "workflow_recovery.resumed",
            Some("workflow run resumed after daemon restart"),
            [],
          ),
        )
      hub.register_session(
        state.event_hub,
        session_event.SessionSummary(
          session_id: session_id,
          display_name: session_name.generate(
            recovered.issue.identifier,
            session_id,
          ),
          issue_id: recovered.issue.id,
          issue_identifier: recovered.issue.identifier,
          issue_title: recovered.issue.title,
          workspace_path: recovered.run_root,
          pi_session_id: None,
          status: session_event.Preparing,
          recovery: recovery,
          current_turn: 0,
          current_turn_status: None,
          current_turn_started_at_ms: None,
          last_turn_finished_at_ms: None,
          last_turn_duration_ms: None,
          last_turn_token_delta: session_tokens.zero_token_totals(),
          last_turn_reason: None,
          started_at_ms: started_at_ms,
          last_event_at_ms: started_at_ms,
          token_totals: session_tokens.zero_token_totals(),
        ),
      )
      publish_recovery_lifecycle(state.event_hub, session_id, recovery)
      event_publisher.lifecycle(
        state.event_hub,
        session_id,
        session_event.DispatchStarted,
        Some("recovered_workflow"),
      )
      log_state(state, "info", "workflow_recovery_resumed", [
        #("issue_id", recovered.issue.id),
        #("run_id", recovered.run_id),
        #("workflow_id", recovered.workflow_id),
      ])
      let runtime =
        core.apply_worker_start(
          state.runtime,
          recovered.issue,
          recovered.run_root,
        )
      let subject = state.subject
      let dependencies = state.dependencies
      let tracker_client = state.tracker_client
      let bundle = state.workflow.bundle
      let secrets = state.workflow.secrets
      let event_hub = state.event_hub
      let pid =
        process.spawn_unlinked(fn() {
          let result =
            run_recovered_workflow_worker(
              recovered,
              bundle,
              tracker_client,
              secrets,
              dependencies.workflow_run_dependencies,
              subject,
              event_hub,
              session_id,
              dependencies.now_ms,
            )
          process.send(
            subject,
            WorkerFinished(recovered.issue.id, recovered.run_id, result),
          )
        })
      let monitor = process.monitor(pid)
      event_publisher.lifecycle(
        state.event_hub,
        session_id,
        session_event.WorkerStarted,
        Some("recovered_workflow"),
      )
      hub.update_status(state.event_hub, session_id, session_event.Running)
      let task_ref = task.from_legacy_issue(recovered.issue).ref
      let task_identity = orchestrator_state.task_ref_identity(task_ref)
      let handle =
        worker_registry.WorkerHandle(
          task_ref: task_ref,
          issue_id: recovered.issue.id,
          issue: recovered.issue,
          run_id: recovered.run_id,
          pid: pid,
          monitor: monitor,
          workspace_path: recovered.run_root,
          session_id: session_id,
          command_subject: None,
        )
      let command_route_id = "worker:" <> recovered.run_id <> ":recovered"
      let worker_entry =
        transition_types.WorkerEntry(
          task_ref: task_ref,
          issue_id: recovered.issue.id,
          run_id: recovered.run_id,
          session_id: session_id,
          issue: recovered.issue,
          workspace_path: recovered.run_root,
          workflow_id: recovered.workflow_id,
          workflow_snapshot: None,
          command_route_id: command_route_id,
          status: transition_types.WorkerRunning,
          recovery: recovery,
        )
      let workers =
        transition_types.WorkerDirectory(
          ..state.workers,
          by_issue: dict.insert(
            state.workers.by_issue,
            task_identity,
            worker_entry,
          ),
          by_session: dict.insert(
            state.workers.by_session,
            session_id,
            task_identity,
          ),
          route_to_session: dict.insert(
            state.workers.route_to_session,
            command_route_id,
            session_id,
          ),
        )
      State(
        ..state,
        runtime: runtime,
        workers: workers,
        registry: worker_registry.register_worker(state.registry, handle),
      )
    }
  }
}

fn run_recovered_workflow_worker(
  recovered: recovery.RecoveredWorkflowRun,
  bundle: runtime_bundle.RuntimeBundle,
  tracker_client: tracker.Client,
  secrets: List(String),
  workflow_dependencies: workflow_run.Dependencies,
  daemon_subject: process.Subject(Message),
  event_hub: process.Subject(hub.Message),
  session_id: String,
  now_ms: fn() -> Int,
) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
  case retry_step_resumption.validate(bundle, recovered) {
    Error(failure) ->
      Error(yaml_worker_failure(
        "workflow_recovery_invalid:" <> failure.reason,
        Some(recovered.run_root),
        recovered.issue,
      ))
    Ok(validation) -> {
      let workflow_dependencies =
        workflow_run.Dependencies(
          ..workflow_dependencies,
          checkpoint: workflow_checkpoint.ledger_writer(
            bundle.effective.workspace.root,
            now_ms,
          ),
        )
      let resume =
        workflow_run.ResumeState(
          artifacts: recovered.completed_artifacts,
          workspaces: recovered_workspaces_to_prepared(
            recovered.completed_workspaces,
            validation.profile.name,
            bundle.orchestrator,
          ),
          next_attempt_indexes: recovered.next_attempt_indexes,
          run_root: Some(recovered.run_root),
          recovery_evidence: recovered.recovery_evidence,
          pi_session_continuations: recovered.pi_session_continuations,
          contract_inputs_recorded: recovered_contract_manifest(
            recovered.contract_input_manifest,
          ),
          contract_outputs_recorded: recovered_contract_manifest(
            recovered.contract_output_manifest,
          ),
        )
      case
        workflow_run.execute_with_resume(
          recovered.issue,
          validation.dag,
          bundle.orchestrator,
          tracker_client,
          secrets,
          recovered.run_id,
          yaml_workflow_dependencies(
            workflow_dependencies,
            recovered.issue,
            recovered.run_id,
            session_id,
            daemon_subject,
            event_hub,
            now_ms,
          ),
          resume,
        )
      {
        Ok(success) -> {
          publish_post_success_cleanup_warning(
            event_hub,
            session_id,
            success.cleanup_warning,
          )
          Ok(success.worker_success)
        }
        Error(failure) -> Error(yaml_workflow_failure(failure, recovered.issue))
      }
    }
  }
}

fn recovered_contract_manifest(
  manifest: Option(recovery.RecoveredContractManifest),
) -> Option(workflow_checkpoint.ArtifactWritten) {
  manifest
  |> option.map(fn(manifest) {
    workflow_checkpoint.ArtifactWritten(
      ref: manifest.ref,
      sha256: manifest.sha256,
      bytes: manifest.bytes,
    )
  })
}

fn recovered_workspaces_to_prepared(
  workspaces: Dict(String, recovery.RecoveredWorkspaceSummary),
  profile_name: String,
  _orchestrator: config_types.OrchestratorConfig,
) -> Dict(String, workspace_run.PreparedStepWorkspace) {
  workspaces
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(workspace_name, w) = entry
    #(
      workspace_name,
      workspace_run.PreparedStepWorkspace(
        workflow_id: w.workflow_id,
        run_id: w.run_id,
        run_root: w.run_root,
        workflow_bundle_dir: "",
        attempt_index: w.attempt_index,
        workspace_name: w.workspace_name,
        path: w.path,
        source: w.source,
        workspace_profile: profile_name,
      ),
    )
  })
  |> dict.from_list
}

fn map_startup_recovery_error(
  result: Result(a, startup_recovery.StartupError),
) -> Result(a, StartupError) {
  case result {
    Ok(value) -> Ok(value)
    Error(startup_recovery.StartupError(code, message)) ->
      Error(StartupError(code, message))
  }
}

fn continue_with_refreshed_state(state: State) -> actor.Next(State, Message) {
  case state.transition_invariant_violation_pending {
    True -> {
      let state = fail_pending_startup_recovery_waiters(state)
      let _shutdown_state = shutdown_runtime_shell(state, True)
      actor.stop_abnormal("transition_invariant_violation")
    }
    False -> actor.continue(refresh_read_model(state))
  }
}

fn handle_issue_worker_finished(
  state: State,
  issue_id: String,
  run_id: String,
  result: Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
) -> State {
  let state =
    worker_lifecycle.worker_finished_to_transition(
      worker_finished_context(state, run_id, result),
      issue_id,
      run_id,
      result,
    )
  finalize_yaml_step_tokens_for_issue_result(state, run_id, result)
}

fn handle_scheduled_worker_finished(
  state: State,
  run_id: String,
  result: Result(
    workflow_run.WorkflowRunSuccess,
    workflow_run.WorkflowRunFailure,
  ),
) -> State {
  let state =
    worker_lifecycle.handle_scheduled_worker_finished(
      scheduled_worker_finished_context(state),
      run_id,
      result,
    )
  let state =
    finalize_yaml_step_tokens_for_scheduled_result(state, run_id, result)
  case result {
    Ok(_) -> state
    Error(failure) ->
      cleanup_orphaned_yaml_children_after_parent_stop(
        state,
        run_id,
        workflow_run.failure_report(failure),
        None,
      )
  }
}

fn finalize_yaml_step_tokens_for_issue_result(
  state: State,
  run_id: String,
  result: Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
) -> State {
  let direct_tokens = session_metrics.worker_result_tokens(result)
  let child_tokens =
    session_metrics.total_for_run(state.yaml_step_tokens, run_id)
  let tokens = case session_tokens.nonzero(direct_tokens) {
    True -> direct_tokens
    False -> child_tokens
  }
  state
  |> add_runtime_aggregate_tokens_via_transition(tokens)
  |> remove_yaml_step_tokens_for_run(run_id)
}

fn finalize_yaml_step_tokens_for_scheduled_result(
  state: State,
  run_id: String,
  result: Result(
    workflow_run.WorkflowRunSuccess,
    workflow_run.WorkflowRunFailure,
  ),
) -> State {
  let direct_tokens = session_metrics.workflow_run_result_tokens(result)
  let child_tokens =
    session_metrics.total_for_run(state.yaml_step_tokens, run_id)
  let tokens = case session_tokens.nonzero(direct_tokens) {
    True -> direct_tokens
    False -> child_tokens
  }
  state
  |> add_runtime_aggregate_tokens_via_transition(tokens)
  |> remove_yaml_step_tokens_for_run(run_id)
}

fn add_runtime_aggregate_tokens_via_transition(
  state: State,
  tokens: session_tokens.TokenTotals,
) -> State {
  case session_tokens.nonzero(tokens) {
    False -> state
    True ->
      run_transition_messages(state, [
        transition_types.AggregatePiTokensAdded(tokens),
      ])
  }
}

fn remove_yaml_step_tokens_for_run(state: State, run_id: String) -> State {
  State(
    ..state,
    yaml_step_tokens: session_metrics.remove_run(state.yaml_step_tokens, run_id),
  )
}

fn handle_message(
  state: State,
  message: Message,
) -> actor.Next(State, Message) {
  case startup_recovery_should_defer(state, message) {
    True -> defer_until_startup_recovery_ready(state, message)
    False ->
      case message {
        PollTick(generation) ->
          case startup_recovery_ready(state) {
            True ->
              continue_with_refreshed_state(poll_tick_shell(state, generation))
            False -> actor.continue(state)
          }
        RetryTick(issue_id, generation) ->
          continue_with_refreshed_state(
            run_transition_messages(state, [
              transition_types.RetryTick(
                issue_id,
                generation,
                transition_dispatch_context(state),
              ),
            ]),
          )
        ContinueStartupRecovery ->
          continue_with_refreshed_state(advance_startup_recovery(state))
        DispatchRecoveryContinue(remaining_candidates) ->
          continue_with_refreshed_state(
            run_transition_messages(state, [
              transition_types.DispatchCandidates(
                remaining_candidates,
                transition_dispatch_context(state),
              ),
            ]),
          )
        WorkerFinished(issue_id, run_id, result) ->
          continue_with_refreshed_state(handle_issue_worker_finished(
            state,
            issue_id,
            run_id,
            result,
          ))
        ScheduledWorkerFinished(run_id, result) ->
          continue_with_refreshed_state(handle_scheduled_worker_finished(
            state,
            run_id,
            result,
          ))
        ScheduledRetryTick(run_id, generation) ->
          continue_with_refreshed_state(handle_scheduled_retry_tick(
            state,
            run_id,
            generation,
          ))
        ScheduledReportRetryTick(run_id, generation) ->
          continue_with_refreshed_state(handle_scheduled_report_retry_tick(
            state,
            run_id,
            generation,
          ))
        WorkerUpdate(issue_id, update) ->
          continue_with_refreshed_state(worker_lifecycle.handle_worker_update(
            worker_update_context(state),
            issue_id,
            update,
          ))
        WorkerCommandReady(issue_id, run_id, command_subject) ->
          continue_with_refreshed_state(
            worker_lifecycle.handle_worker_command_ready(
              worker_command_ready_context(state),
              issue_id,
              run_id,
              command_subject,
            ),
          )
        YamlStepStarted(session_id, run_id, workflow_id, step_id, attempt_index) ->
          continue_with_refreshed_state(handle_yaml_step_started(
            state,
            session_id,
            run_id,
            workflow_id,
            step_id,
            attempt_index,
          ))
        YamlStepUpdate(session_id, update) -> {
          event_publisher.worker_update(state.event_hub, session_id, update)
          log_yaml_step_update(state, session_id, update)
          actor.continue(
            State(
              ..state,
              yaml_step_tokens: session_metrics.update_from_runner(
                state.yaml_step_tokens,
                session_id,
                update,
              ),
            ),
          )
        }
        YamlStepCommandReady(session_id, command_subject) ->
          continue_with_refreshed_state(handle_yaml_step_command_ready(
            state,
            session_id,
            command_subject,
          ))
        YamlStepFinished(session_id, tokens) ->
          continue_with_refreshed_state(handle_yaml_step_finished(
            state,
            session_id,
            tokens,
          ))
        WorkerCommandCompleted(operator_command, worker_reply, reply) -> {
          let result =
            operator_worker_command.reply_result(operator_command, worker_reply)
          process.send(reply, result)
          log_operator_result(state, result, [])
          actor.continue(state)
        }
        WorkerCommandTimedOut(operator_command, reply) -> {
          let result = operator_worker_command.timeout_result(operator_command)
          process.send(reply, result)
          log_operator_result(state, result, [])
          actor.continue(state)
        }
        AbortWorkerCommandTimedOut(operator_command, session_id, reply) -> {
          let #(state, result, follow_ups) =
            stop_session_for_operator(
              state,
              operator_command,
              session_id,
              session_reason.OperatorAbort,
            )
          let state = run_transition_messages(state, follow_ups)
          process.send(reply, result)
          log_operator_result(state, result, [])
          continue_with_refreshed_state(state)
        }
        WorkerDown(down) ->
          continue_with_refreshed_state(
            worker_lifecycle.worker_down_to_transition(
              worker_down_context(state),
              down,
            ),
          )
        EffectRunnerDown(down) -> {
          let _shutdown_state = handle_effect_runner_down(state, down)
          actor.stop_abnormal("effect_runner_down")
        }
        ControlServerDown(down) -> {
          let _shutdown_state = handle_control_server_down(state, down)
          actor.stop_abnormal("control_server_down")
        }
        SideEffectCompleted(completion) ->
          continue_with_refreshed_state(handle_side_effect_completed(
            state,
            completion,
          ))
        GetSnapshot(reply) -> {
          effect_runner.reply_snapshot(state.runtime, reply)
          actor.continue(state)
        }
        GetReadModelSnapshot(reply) -> {
          let refreshed = refresh_read_model(state)
          process.send(reply, read_model_snapshot_from_state(refreshed))
          actor.continue(refreshed)
        }
        GetProjectionSnapshot(reply) -> {
          process.send(reply, state.ledger_projection)
          actor.continue(state)
        }
        GetOutboxSnapshot(reply) -> {
          process.send(reply, dict.to_list(state.ledger_projection.outbox))
          actor.continue(state)
        }
        GetWorkflowSnapshot(reply) -> {
          process.send(reply, state.workflow)
          actor.continue(state)
        }
        GetRemoteDispatchPaused(reply) -> {
          process.send(reply, state.operator_paused)
          actor.continue(state)
        }
        AwaitStartupRecoveryReady(reply, timeout_ms) ->
          case startup_recovery_ready(state) {
            True -> {
              process.send(reply, Ok(Nil))
              actor.continue(state)
            }
            False ->
              case timeout_ms <= 0 {
                True -> {
                  process.send(reply, Error(Nil))
                  actor.continue(state)
                }
                False -> {
                  let waiter_id = state.next_startup_recovery_waiter_id
                  let _timer =
                    state.dependencies.send_after(
                      state.subject,
                      timeout_ms,
                      StartupRecoveryWaiterTimedOut(waiter_id),
                    )
                  actor.continue(
                    State(
                      ..state,
                      next_startup_recovery_waiter_id: waiter_id + 1,
                      pending_startup_recovery_waiters: dict.insert(
                        state.pending_startup_recovery_waiters,
                        waiter_id,
                        reply,
                      ),
                    ),
                  )
                }
              }
          }
        StartupRecoveryWaiterTimedOut(waiter_id) -> {
          let pending_waiters =
            dict.delete(state.pending_startup_recovery_waiters, waiter_id)
          case dict.get(state.pending_startup_recovery_waiters, waiter_id) {
            Ok(reply) -> process.send(reply, Error(Nil))
            Error(Nil) -> Nil
          }
          actor.continue(
            State(..state, pending_startup_recovery_waiters: pending_waiters),
          )
        }
        StartRemoteClient ->
          case
            state.transition_invariant_violation_pending
            || !startup_recovery_ready(state)
          {
            True -> continue_with_refreshed_state(state)
            False ->
              continue_with_refreshed_state(start_remote_client_now(state))
          }
        RunQueuedControlOperation(operation_id) ->
          case startup_recovery_ready(state) {
            True ->
              continue_with_refreshed_state(run_queued_control_operation(
                state,
                operation_id,
              ))
            False -> actor.continue(state)
          }
        QueuedControlOperationFinished(operation_id, execution_result) ->
          continue_with_refreshed_state(finish_queued_control_operation(
            state,
            operation_id,
            execution_result,
          ))
        ApplyOperatorCommand(operator_command, timeout_ms, reply) ->
          continue_with_refreshed_state(operator_command_reply(
            state,
            operator_command,
            timeout_ms,
            reply,
          ))
        ExecuteQuery(query, _timeout_ms, reply) -> {
          let _query_worker_pid =
            process.spawn_unlinked(fn() {
              process.send(
                reply,
                query_service.query(state.query_service, query),
              )
              Nil
            })
          actor.continue(state)
        }
        Shutdown(reply) -> {
          let state =
            run_transition_messages(state, [
              transition_types.ShutdownRequested(True),
            ])
            |> fail_pending_startup_recovery_waiters
          case state.transition_invariant_violation_pending {
            True -> actor.stop_abnormal("transition_invariant_violation")
            False -> {
              log_state(state, "info", "daemon_shutdown", [])
              process.send(reply, Nil)
              actor.stop()
            }
          }
        }
      }
  }
}

fn handle_yaml_step_command_ready(
  state: State,
  session_id: String,
  command_subject: process.Subject(worker_command.Command),
) -> State {
  State(
    ..state,
    registry: worker_registry.register_yaml_step_command_subject(
      state.registry,
      session_id,
      command_subject,
    ),
  )
}

fn clear_yaml_step_command_routes_for_run(
  state: State,
  run_id: String,
) -> State {
  State(
    ..state,
    registry: worker_registry.clear_yaml_step_command_routes_for_run(
      state.registry,
      run_id,
    ),
  )
}

fn handle_yaml_step_started(
  state: State,
  session_id: String,
  run_id: String,
  workflow_id: String,
  step_id: String,
  attempt_index: Int,
) -> State {
  emit_work_item_invalidation_for_run(state, run_id)
  let parent_session_id = parent_session_id_for_run(state, run_id)
  let registry =
    worker_registry.register_active_yaml_step_started(
      state.registry,
      session_id,
      run_id,
      workflow_id,
      step_id,
      attempt_index,
    )
  let registered =
    list.contains(
      worker_registry.active_yaml_step_sessions_for_run(registry, run_id),
      session_id,
    )
  let yaml_step_tokens = case registered {
    True ->
      session_metrics.register_step(
        state.yaml_step_tokens,
        session_id,
        run_id,
        parent_session_id,
      )
    False -> state.yaml_step_tokens
  }
  let state =
    State(..state, registry: registry, yaml_step_tokens: yaml_step_tokens)
  case registered {
    True ->
      run_transition_messages(state, [
        transition_types.YamlStepStarted(
          identity.session_id_from_string(session_id),
          identity.run_id_from_string(run_id),
        ),
      ])
    False ->
      transition_finish_yaml_step_session(
        state,
        identity.session_id_from_string(session_id),
        session_reason.Stopped,
      )
  }
}

fn handle_yaml_step_finished(
  state: State,
  session_id: String,
  tokens: session_tokens.TokenTotals,
) -> State {
  case worker_registry.active_yaml_step_handle(state.registry, session_id) {
    Ok(handle) -> emit_work_item_invalidation_for_run(state, handle.run_id)
    Error(Nil) -> Nil
  }
  let state =
    State(
      ..state,
      yaml_step_tokens: session_metrics.update_tokens(
        state.yaml_step_tokens,
        session_id,
        tokens,
      ),
    )
  run_transition_messages(state, [
    transition_types.YamlStepFinished(identity.session_id_from_string(
      session_id,
    )),
  ])
}

fn parent_session_id_for_run(state: State, run_id: String) -> String {
  case worker_registry.worker_for_run(state.registry, run_id) {
    Ok(handle) -> handle.session_id
    Error(Nil) ->
      case worker_registry.scheduled_worker_for_run(state.registry, run_id) {
        Ok(handle) -> handle.session_id
        Error(Nil) -> run_id
      }
  }
}

fn finish_yaml_step_sessions_for_run(
  state: State,
  run_id: String,
  reason: session_reason.WorkerExitReason,
) -> State {
  let reason_text = session_reason.to_string(reason)
  let session_ids =
    worker_registry.active_yaml_step_sessions_for_run(state.registry, run_id)
  list.each(session_ids, fn(session_id) {
    hub.update_status(state.event_hub, session_id, session_event.Stopping)
    event_publisher.lifecycle(
      state.event_hub,
      session_id,
      session_event.OperatorCommand,
      Some(reason_text),
    )
    event_publisher.lifecycle(
      state.event_hub,
      session_id,
      session_event.WorkerExited,
      Some(reason_text),
    )
    hub.finish_session(state.event_hub, session_id, reason)
  })
  let registry =
    state.registry
    |> worker_registry.delete_yaml_step_sessions(session_ids)
    |> worker_registry.clear_yaml_run_stopping(run_id)
  State(..state, registry: registry)
}

fn registry_down_resolution_context(
  state: State,
) -> worker_lifecycle.RegistryDownResolutionContext(State) {
  worker_lifecycle.RegistryDownResolutionContext(
    state: state,
    unknown_down: fn(state, registry) {
      run_transition_messages(State(..state, registry: registry), [
        transition_types.WorkerDown(
          transition_types.UnknownWorkerDown,
          transition_lifecycle_context(state),
        ),
      ])
    },
    step_command_down: fn(state, registry, session_id) {
      log_state(state, "warn", "yaml_step_command_down", [
        #("session_id", session_id),
      ])
      State(..state, registry: registry)
    },
    worker_down: handle_known_worker_down,
    worker_down_stale: fn(state, registry, issue_id) {
      run_transition_messages(State(..state, registry: registry), [
        transition_types.WorkerDown(
          transition_types.WorkerDownStale(identity.issue_id_from_string(
            issue_id,
          )),
          transition_lifecycle_context(state),
        ),
      ])
    },
    scheduled_worker_down: fn(state, registry, run_id, handle) {
      worker_lifecycle.scheduled_worker_down(
        scheduled_worker_down_context(state),
        registry,
        run_id,
        handle,
      )
    },
    scheduled_worker_down_stale: fn(state, registry, run_id) {
      log_state(state, "warn", "scheduled_worker_down_stale", [
        #("run_id", run_id),
      ])
      State(..state, registry: registry)
    },
  )
}

fn handle_known_worker_down(
  state: State,
  registry: worker_registry.Registry,
  issue_id: String,
  handle: worker_registry.WorkerHandle,
) -> State {
  let state = State(..state, registry: registry)
  let state = case
    worker_lifecycle.worker_down_matches(state.workers, issue_id, handle)
  {
    False -> state
    True -> {
      let state =
        append_workflow_interrupted_terminal(state, handle, "worker_down")
      worker_lifecycle.publish_worker_down(state.event_hub, handle.session_id)
      state
    }
  }
  run_transition_messages(state, [
    worker_lifecycle.worker_down_message(
      issue_id,
      handle,
      transition_lifecycle_context(state),
    ),
  ])
}

fn scheduled_worker_down_context(
  state: State,
) -> worker_lifecycle.ScheduledWorkerDownContext(State) {
  worker_lifecycle.ScheduledWorkerDownContext(
    state: state,
    set_registry: fn(state, registry) { State(..state, registry: registry) },
    log_worker_down: fn(state, job_id, run_id) {
      log_state(state, "warn", "scheduled_worker_down", [
        #("job_id", job_id),
        #("run_id", run_id),
      ])
    },
    publish_worker_down: fn(session_id) {
      event_publisher.lifecycle(
        state.event_hub,
        session_id,
        session_event.WorkerDown,
        None,
      )
    },
    finish_failed_session: fn(session_id) {
      hub.finish_session(state.event_hub, session_id, session_reason.Failed)
    },
    worker_failure_follow_up: scheduled_worker_failure_follow_up,
    append_failure_ledger: fn(state, handle, reason, retry_exhausted, run_root) {
      append_ledger_bodies_best_effort(
        state,
        [
          record.ScheduledRunFailed(
            handle.job_id,
            handle.workflow_id,
            handle.due_at_ms,
            handle.run_id,
            handle.attempt,
            state.dependencies.now_ms(),
            reason,
            retry_exhausted,
            run_root,
          ),
        ],
        "scheduled_worker_down_append_failed",
      )
    },
    begin_failure_report_request: begin_scheduled_failure_report_request,
    start_pending_scheduled_runs: start_pending_scheduled_runs,
  )
}

type OperatorCommandReplyState {
  OperatorCommandImmediate(State, command.CommandResult)
  OperatorCommandPending(State)
}

fn startup_recovery_rejection(
  operator_command: command.OperatorCommand,
) -> command.CommandResult {
  command.rejected(
    operator_command,
    "startup_recovery_in_progress",
    Some("startup recovery is still in progress"),
  )
}

fn operator_command_reply(
  state: State,
  operator_command: command.OperatorCommand,
  timeout_ms: Int,
  reply: process.Subject(command.CommandResult),
) -> State {
  case startup_recovery_ready(state) {
    False -> {
      let result = startup_recovery_rejection(operator_command)
      process.send(reply, result)
      log_operator_result(state, result, [])
      state
    }
    True ->
      case operator_command {
        command.WorkItemAction(request) ->
          reply_for_work_item_action(state, request, reply)
        command.AbortSession(_)
        | command.StopAfterCurrentTurn(_)
        | command.PromptSession(_, _)
        | command.RespondUi(_, _, _) ->
          reply_for_worker_operator_command(
            state,
            operator_command,
            timeout_ms,
            reply,
          )
        _ ->
          transition_operator_command_reply(
            state,
            operator_command,
            timeout_ms,
            reply,
          )
      }
  }
}

fn reply_for_work_item_action(
  state: State,
  request: command.WorkItemActionRequest,
  reply: process.Subject(command.CommandResult),
) -> State {
  let action_executor.Outcome(result: result, receipts: receipts) =
    action_executor.execute(
      state.work_item_action_receipts,
      request,
      fn(request) { live_work_item_detail(state, request) },
    )
  process.send(reply, result)
  log_operator_result(state, result, [])
  State(..state, work_item_action_receipts: receipts)
}

fn live_work_item_detail(
  state: State,
  request: command.WorkItemActionRequest,
) -> Result(Option(work_item.WorkItemDetail), query_types.QueryError) {
  case
    query_backend.load_work_item_detail(
      state.tracker_adapter,
      ref: query_types.TaskRemoteId(
        provider: request.target_provider,
        id: request.target_id,
      ),
    )
  {
    Ok(Some(detail)) ->
      Ok(
        Some(action_derivation.detail_for_target_kind_in_projection(
          detail,
          target_kind: request.target_kind,
          dispatch_paused: state.operator_paused,
          projection_state: state.ledger_projection,
        )),
      )
    Ok(None) -> Ok(None)
    Error(error) -> Error(error)
  }
}

fn reply_for_worker_operator_command(
  state: State,
  operator_command: command.OperatorCommand,
  timeout_ms: Int,
  reply: process.Subject(command.CommandResult),
) -> State {
  case
    apply_async_worker_operator_command(
      state,
      operator_command,
      timeout_ms,
      reply,
    )
  {
    OperatorCommandImmediate(state, result) -> {
      process.send(reply, result)
      log_operator_result(state, result, [])
      state
    }
    OperatorCommandPending(state) -> state
  }
}

fn transition_operator_command_reply(
  state: State,
  operator_command: command.OperatorCommand,
  timeout_ms: Int,
  reply: process.Subject(command.CommandResult),
) -> State {
  let correlation_id = next_operator_command_correlation_id(state)
  let state =
    State(
      ..state,
      pending_operator_command_replies: dict.insert(
        state.pending_operator_command_replies,
        correlation_id,
        reply,
      ),
      completed_operator_command_results: dict.delete(
        state.completed_operator_command_results,
        correlation_id,
      ),
      next_operator_command_correlation_id: state.next_operator_command_correlation_id
        + 1,
    )
  let request =
    transition_effects.OperatorCommandRequest(
      correlation_id: correlation_id,
      source: transition_effects.LocalOperatorCommand,
      operator_command: operator_command,
      timeout_ms: timeout_ms,
    )
  let state =
    run_transition_messages(state, [
      transition_types.OperatorCommandSubmitted(
        request: request,
        context: transition_dispatch_context(state),
        issue_resolution: operator_issue_resolution(state, operator_command),
        parked_issue_resolution: parked_issue_resolution(
          state,
          operator_command,
        ),
      ),
    ])
  case dict.get(state.completed_operator_command_results, correlation_id) {
    Ok(result) ->
      send_completed_operator_command_reply(state, correlation_id, result)
    Error(Nil) -> {
      let result =
        command.rejected(
          operator_command,
          "operator_command_result_missing",
          Some("operator command did not produce a result"),
        )
      send_completed_operator_command_reply(state, correlation_id, result)
    }
  }
}

fn send_completed_operator_command_reply(
  state: State,
  correlation_id: String,
  result: command.CommandResult,
) -> State {
  case dict.get(state.pending_operator_command_replies, correlation_id) {
    Ok(reply) -> {
      process.send(reply, result)
      log_operator_result(state, result, [#("correlation_id", correlation_id)])
      State(
        ..state,
        pending_operator_command_replies: dict.delete(
          state.pending_operator_command_replies,
          correlation_id,
        ),
        completed_operator_command_results: dict.delete(
          state.completed_operator_command_results,
          correlation_id,
        ),
      )
    }
    Error(Nil) -> {
      log_state(state, "warn", "operator_command_reply_missing", [
        #("correlation_id", correlation_id),
        #("command", result.command),
      ])
      State(
        ..state,
        completed_operator_command_results: dict.delete(
          state.completed_operator_command_results,
          correlation_id,
        ),
      )
    }
  }
}

fn next_operator_command_correlation_id(state: State) -> String {
  "operator-command-"
  <> int.to_string(state.next_operator_command_correlation_id)
}

fn operator_issue_resolution(
  state: State,
  operator_command: command.OperatorCommand,
) -> transition_types.OperatorIssueResolution {
  operator_runtime.operator_issue_resolution(
    operator_runtime.lookup(
      issue_for_ref: fn(issue_ref) { issue_for_ref(state, issue_ref) },
      parked_issue_id_for_ref: fn(issue_ref) {
        parked_issue_id_for_ref(state, issue_ref)
      },
    ),
    operator_command,
  )
}

fn parked_issue_resolution(
  state: State,
  operator_command: command.OperatorCommand,
) -> transition_types.ParkedIssueResolution {
  operator_runtime.parked_issue_resolution(
    operator_runtime.lookup(
      issue_for_ref: fn(issue_ref) { issue_for_ref(state, issue_ref) },
      parked_issue_id_for_ref: fn(issue_ref) {
        parked_issue_id_for_ref(state, issue_ref)
      },
    ),
    operator_command,
  )
}

fn apply_async_worker_operator_command(
  state: State,
  operator_command: command.OperatorCommand,
  timeout_ms: Int,
  reply: process.Subject(command.CommandResult),
) -> OperatorCommandReplyState {
  let context =
    operator_worker_command.context(
      state: state,
      daemon_subject: state.subject,
      route_session_id: route_worker_command_session_id,
      worker_for_session: fn(state, session_id) {
        case worker_for_session(state, session_id) {
          Error(Nil) -> operator_worker_command.NoWorker
          Ok(handle) ->
            case handle.command_subject {
              Some(subject) ->
                operator_worker_command.WorkerWithCommandSubject(subject)
              None ->
                operator_worker_command.WorkerWithoutCommandSubject(
                  handle.run_id,
                )
            }
        }
      },
      step_subject_for_run: fn(state, run_id) {
        case
          worker_registry.step_command_subject_for_run(state.registry, run_id)
        {
          Error(worker_registry.NoActiveStepCommandSubject) ->
            operator_worker_command.NoActiveStepCommandSubject
          Error(worker_registry.MultipleActiveStepCommandSubjects) ->
            operator_worker_command.MultipleActiveStepCommandSubjects
          Ok(subject) ->
            operator_worker_command.StepRunCommandSubjectFound(subject)
        }
      },
      step_subject_for_session: fn(state, session_id) {
        worker_registry.step_command_subject_for_session(
          state.registry,
          session_id,
        )
      },
      stop_for_abort: fn(state, operator_command, session_id) {
        let #(state, result, follow_ups) =
          stop_session_for_operator(
            state,
            operator_command,
            session_id,
            session_reason.OperatorAbort,
          )
        #(run_transition_messages(state, follow_ups), result)
      },
      completion_message: fn(operator_command, worker_reply, operator_reply) {
        WorkerCommandCompleted(operator_command, worker_reply, operator_reply)
      },
      timeout_message: fn(operator_command, operator_reply) {
        WorkerCommandTimedOut(operator_command, operator_reply)
      },
      abort_timeout_message: fn(operator_command, session_id, operator_reply) {
        AbortWorkerCommandTimedOut(operator_command, session_id, operator_reply)
      },
    )
  case
    operator_worker_command.apply(context, operator_command, timeout_ms, reply)
  {
    operator_worker_command.Immediate(state, result) ->
      OperatorCommandImmediate(state, result)
    operator_worker_command.Pending(state) -> OperatorCommandPending(state)
  }
}

fn apply_shell_operator_command(
  state: State,
  request: transition_effects.OperatorCommandRequest,
) -> #(State, command.CommandResult, List(transition_types.Message)) {
  operator_runtime.apply_shell_operator_command(
    state,
    request,
    operator_runtime.shell_handlers(
      reload_workflow_for_operator: reload_workflow_for_operator,
      retry_workflow_step_for_operator: fn(
        state,
        operator_command,
        target,
        step_id,
      ) {
        let #(state, result) =
          retry_workflow_step_for_operator(
            state,
            operator_command,
            target,
            step_id,
          )
        #(state, result, [])
      },
      recollect_workflow_outputs_for_operator: fn(
        state,
        operator_command,
        run_id,
      ) {
        let #(state, result) =
          recollect_workflow_outputs_for_operator(
            state,
            operator_command,
            run_id,
          )
        #(state, result, [])
      },
      run_finalize_for_operator: fn(
        state,
        operator_command,
        run_id,
        validate,
        outputs,
        publish,
        update_tracker,
        dry_run,
        reason,
      ) {
        let #(state, result) =
          run_finalize_for_operator(
            state,
            operator_command,
            run_id,
            validate,
            outputs,
            publish,
            update_tracker,
            dry_run,
            reason,
          )
        #(state, result, [])
      },
      retry_artifact_publication_for_operator: fn(
        state,
        operator_command,
        run_id,
        publication_id,
      ) {
        let #(state, result) =
          retry_artifact_publication_for_operator(
            state,
            operator_command,
            run_id,
            publication_id,
          )
        #(state, result, [])
      },
      schedule_run_now_for_operator: fn(state, operator_command, job_id) {
        let #(state, result) =
          schedule_run_now_for_operator(state, operator_command, job_id)
        #(state, result, [])
      },
      abort_session_for_operator_sync: abort_session_for_operator_sync,
      route_worker_command_sync: route_worker_command_sync,
      cleanup_orphan_steps_for_operator: fn(
        state,
        operator_command,
        run_id,
        dry_run,
      ) {
        let #(state, result) =
          cleanup_orphan_steps_for_operator(
            state,
            operator_command,
            run_id,
            dry_run,
          )
        #(state, result, [])
      },
    ),
  )
}

fn retry_workflow_step_for_operator(
  state: State,
  operator_command: command.OperatorCommand,
  target: command.RetryWorkflowStepTarget,
  step_id: Option(String),
) -> #(State, command.CommandResult) {
  case replay_projection_for_operator(state) {
    Error(reason) -> #(
      state,
      command.rejected(operator_command, "ledger_read_failed", Some(reason)),
    )
    Ok(projection_state) ->
      case workflow_repair.resolve_target_run(projection_state, target) {
        Error(error) -> #(
          state,
          command.rejected(
            operator_command,
            workflow_repair.describe_error(error),
            workflow_repair.error_message(error),
          ),
        )
        Ok(#(run_id, issue_id, issue_identifier)) ->
          case
            retry_step_issue_preflight(
              state,
              projection_state,
              operator_command,
              target,
              run_id,
              issue_id,
            )
          {
            Error(result) -> #(state, result)
            Ok(_) -> {
              let operation_id =
                make_retry_step_operation_id(state, run_id, step_id)
              let queued_body =
                record.ControlOperationQueued(
                  operation_id: operation_id,
                  operation_kind: "retry_step",
                  command_name: command.command_name(operator_command),
                  target: option.unwrap(
                    command.command_target(operator_command),
                    "",
                  ),
                  run_id: Some(run_id),
                  issue_id: Some(issue_id),
                  issue_identifier: Some(issue_identifier),
                  requested_step_id: step_id,
                  publication_id: None,
                )
              queue_control_operation(
                state,
                operator_command,
                operation_id,
                queued_body,
                "retry_step_queue_append_failed",
                "failed to append retry-step operation",
                "retry-step accepted; poll query operation-status for completion",
              )
            }
          }
      }
  }
}

fn queue_control_operation(
  state: State,
  operator_command: command.OperatorCommand,
  operation_id: String,
  queued_body: record.RecordBody,
  append_event: String,
  append_failure_message: String,
  queued_message: String,
) -> #(State, command.CommandResult) {
  let #(state, appended) =
    append_ledger_bodies(state, [queued_body], append_event)
  case appended {
    False -> #(
      state,
      command.rejected(
        operator_command,
        "ledger_append_failed",
        Some(append_failure_message),
      ),
    )
    True -> {
      process.send(state.subject, RunQueuedControlOperation(operation_id))
      #(
        state,
        command.queued_operation(
          operator_command,
          operation_id,
          Some(queued_message),
        ),
      )
    }
  }
}

fn recollect_workflow_outputs_for_operator(
  state: State,
  operator_command: command.OperatorCommand,
  run_id: String,
) -> #(State, command.CommandResult) {
  case replay_projection_for_operator(state) {
    Error(reason) -> #(
      state,
      command.rejected(operator_command, "ledger_read_failed", Some(reason)),
    )
    Ok(projection_state) ->
      case
        recollect_outputs_control.queue_decision(
          projection_state,
          operator_command,
          run_id,
          state.dependencies.now_ms(),
        )
      {
        Error(error) -> #(
          state,
          recollect_outputs_control.queue_error_result(operator_command, error),
        )
        Ok(decision) ->
          queue_recollect_outputs_operation(
            state,
            operator_command,
            run_id,
            decision,
          )
      }
  }
}

fn queue_recollect_outputs_operation(
  state: State,
  operator_command: command.OperatorCommand,
  run_id: String,
  decision: recollect_outputs_control.QueueDecision,
) -> #(State, command.CommandResult) {
  case
    recollect_outputs_control.parked_preflight_for_run(
      state.runtime,
      operator_command,
      run_id,
      decision.issue_id,
    )
  {
    Error(result) -> #(state, result)
    Ok(Nil) ->
      case decision.body {
        None -> #(
          state,
          command.queued_operation(
            operator_command,
            decision.operation_id,
            Some(
              "recollect-outputs already queued/running; poll query operation-status for completion",
            ),
          ),
        )
        Some(queued_body) ->
          queue_control_operation(
            state,
            operator_command,
            decision.operation_id,
            queued_body,
            "recollect_outputs_queue_append_failed",
            "failed to append recollect-outputs operation",
            "recollect-outputs accepted; poll query operation-status for completion",
          )
      }
  }
}

fn run_finalize_for_operator(
  state: State,
  operator_command: command.OperatorCommand,
  run_id: String,
  validate: Bool,
  outputs: command.RunFinalizeOutputs,
  publish: Bool,
  update_tracker: Bool,
  dry_run: Bool,
  reason: String,
) -> #(State, command.CommandResult) {
  let _ = validate
  let _ = outputs
  let _ = publish
  let _ = update_tracker
  case replay_projection_for_operator(state) {
    Error(message) -> #(
      state,
      command.rejected(operator_command, "ledger_read_failed", Some(message)),
    )
    Ok(projected) ->
      case run_finalize_control.dry_run(projected, run_id) {
        Error(#(code, message)) ->
          case code {
            "run_not_found" -> #(
              state,
              command.not_found(operator_command, Some(message)),
            )
            _ -> #(
              state,
              command.rejected(operator_command, code, Some(message)),
            )
          }
        Ok(plan) ->
          case plan.already_finalized {
            True ->
              case dry_run {
                True -> #(
                  state,
                  command.applied(
                    operator_command,
                    Some(run_finalize_control.dry_run_message(plan)),
                  ),
                )
                False -> #(
                  state,
                  command.applied(
                    operator_command,
                    Some(run_finalize_control.already_finalized_message(run_id)),
                  ),
                )
              }
            False ->
              case
                recollect_outputs_issue_preflight_for_run(
                  state,
                  operator_command,
                  run_id,
                  plan.issue_id,
                )
              {
                Error(result) -> #(state, result)
                Ok(issue) -> {
                  let current =
                    startup_recovery.current_workflow_observation(
                      state.workflow.bundle,
                      issue,
                    )
                  case
                    run_finalize_control.validated_dry_run(
                      projected,
                      run_id,
                      current,
                      state.workflow.effective.workspace.root,
                    )
                  {
                    Error(#(code, message)) ->
                      case code {
                        "run_not_found" -> #(
                          state,
                          command.not_found(operator_command, Some(message)),
                        )
                        _ -> #(
                          state,
                          command.rejected(
                            operator_command,
                            code,
                            Some(message),
                          ),
                        )
                      }
                    Ok(plan) ->
                      case dry_run {
                        True -> #(
                          state,
                          command.applied(
                            operator_command,
                            Some(run_finalize_control.dry_run_message(plan)),
                          ),
                        )
                        False ->
                          case
                            run_finalize_control.queue_decision(
                              projected,
                              operator_command,
                              run_id,
                              state.dependencies.now_ms(),
                            )
                          {
                            Error(#(code, message)) ->
                              case code {
                                "run_not_found" -> #(
                                  state,
                                  command.not_found(
                                    operator_command,
                                    Some(message),
                                  ),
                                )
                                _ -> #(
                                  state,
                                  command.rejected(
                                    operator_command,
                                    code,
                                    Some(message),
                                  ),
                                )
                              }
                            Ok(run_finalize_control.AlreadyFinalized(message)) -> #(
                              state,
                              command.applied(operator_command, Some(message)),
                            )
                            Ok(run_finalize_control.ExistingOperation(
                              operation_id,
                            )) -> #(
                              state,
                              command.queued_operation(
                                operator_command,
                                operation_id,
                                Some(
                                  "run finalize already queued/running; poll query operation-status for completion",
                                ),
                              ),
                            )
                            Ok(run_finalize_control.ConflictingOperation(
                              _,
                              _,
                              message,
                            )) -> #(
                              state,
                              command.rejected(
                                operator_command,
                                "control_operation_already_running",
                                Some(message),
                              ),
                            )
                            Ok(run_finalize_control.NewOperation(
                              operation_id,
                              queued_body,
                            )) -> {
                              let plan_bodies = [
                                record.WorkflowRunDiagnostic(
                                  run_id,
                                  plan.workflow_id,
                                  plan.issue_id,
                                  "run_finalize_requested:" <> reason,
                                ),
                                queued_body,
                              ]
                              let #(state, appended) =
                                append_ledger_bodies(
                                  state,
                                  plan_bodies,
                                  "run_finalize_queue_append_failed",
                                )
                              case appended {
                                False -> #(
                                  state,
                                  command.rejected(
                                    operator_command,
                                    "ledger_append_failed",
                                    Some(
                                      "failed to append run finalize operation",
                                    ),
                                  ),
                                )
                                True -> {
                                  process.send(
                                    state.subject,
                                    RunQueuedControlOperation(operation_id),
                                  )
                                  #(
                                    state,
                                    command.queued_operation(
                                      operator_command,
                                      operation_id,
                                      Some(
                                        "run finalize accepted; poll query operation-status for completion",
                                      ),
                                    ),
                                  )
                                }
                              }
                            }
                          }
                      }
                  }
                }
              }
          }
      }
  }
}

fn recollect_outputs_issue_preflight_for_run(
  state: State,
  operator_command: command.OperatorCommand,
  run_id: String,
  issue_id: String,
) -> Result(tracker_issue.Issue, command.CommandResult) {
  use Nil <- result.try(recollect_outputs_control.parked_preflight_for_run(
    state.runtime,
    operator_command,
    run_id,
    issue_id,
  ))
  case fetch_issue_by_id(state, issue_id) {
    Error(status) -> Error(command.result_for(operator_command, status, None))
    Ok(issue) ->
      recollect_outputs_control.validate_issue_state(
        state.workflow.effective,
        operator_command,
        run_id,
        issue,
      )
  }
}

fn retry_step_issue_preflight(
  state: State,
  projection_state: projection.Projection,
  operator_command: command.OperatorCommand,
  target: command.RetryWorkflowStepTarget,
  run_id: String,
  issue_id: String,
) -> Result(tracker_issue.Issue, command.CommandResult) {
  case issue_is_active_or_pending_except_parked(state, issue_id) {
    True ->
      Error(command.rejected(
        operator_command,
        "issue_already_active",
        Some("issue already has an active or pending workflow"),
      ))
    False ->
      case
        retry_step_operation.parked_issue(
          state.runtime,
          projection_state,
          operator_command,
          run_id,
          issue_id,
        )
      {
        Error(result) -> Error(result)
        Ok(Nil) ->
          case fetch_issue_by_id(state, issue_id) {
            Error(status) ->
              Error(command.result_for(operator_command, status, None))
            Ok(issue) ->
              case core.is_terminal(state.workflow.effective, issue.state) {
                True ->
                  Error(command.rejected(
                    operator_command,
                    "issue_state_drift:terminal_state",
                    Some(
                      "run "
                      <> command.retry_workflow_step_target_to_string(target)
                      <> " for issue "
                      <> issue.identifier
                      <> " is currently in terminal state "
                      <> issue_state.to_string(issue.state),
                    ),
                  ))
                False -> Ok(issue)
              }
          }
      }
  }
}

fn run_queued_control_operation(state: State, operation_id: String) -> State {
  case dict.get(state.active_control_operations, operation_id) {
    Ok(True) -> state
    _ ->
      case projection.control_operation(state.ledger_projection, operation_id) {
        Error(Nil) -> state
        Ok(operation) ->
          case operation.status {
            "completed" | "failed" -> state
            _ -> start_queued_control_operation(state, operation)
          }
      }
  }
}

fn start_queued_control_operation(
  state: State,
  operation: projection.ControlOperationStatus,
) -> State {
  case
    recollect_outputs_control.control_operation_running_conflict(
      state.ledger_projection,
      operation,
    )
  {
    Ok(conflict) ->
      append_control_operation_failure(
        state,
        operation.operation_id,
        conflict.reason,
        conflict.message,
      )
    Error(Nil) -> {
      let state =
        State(
          ..state,
          active_control_operations: dict.insert(
            state.active_control_operations,
            operation.operation_id,
            True,
          ),
        )
      let state = case operation.status {
        "queued" ->
          append_ledger_bodies_best_effort(
            state,
            [record.ControlOperationStarted(operation.operation_id)],
            "control_operation_start_append_failed",
          )
        _ -> state
      }
      let subject = state.subject
      let _queued_control_operation_worker =
        process.spawn_unlinked(fn() {
          process.send(
            subject,
            QueuedControlOperationFinished(
              operation.operation_id,
              execute_queued_control_operation(state, operation),
            ),
          )
          Nil
        })
      state
    }
  }
}

fn finish_queued_control_operation(
  state: State,
  operation_id: String,
  execution_result: QueuedControlOperationResult,
) -> State {
  let state =
    State(
      ..state,
      active_control_operations: dict.delete(
        state.active_control_operations,
        operation_id,
      ),
    )
  case execution_result {
    QueuedControlOperationNoop -> state
    QueuedControlOperationSucceeded(bodies, resumption) ->
      case retry_step_resumption.validate(state.workflow.bundle, resumption) {
        Error(failure) ->
          append_control_operation_failure(
            state,
            operation_id,
            failure.reason,
            Some(retry_step_operation.validation_rejection_message(
              failure,
              resumption.run_id,
              operation_requested_step_id(state, operation_id),
            )),
          )
        Ok(_) -> {
          let #(state, appended) =
            append_ledger_bodies(state, bodies, "retry_step_append_failed")
          case appended {
            False ->
              append_control_operation_failure(
                state,
                operation_id,
                "ledger_append_failed",
                Some("failed to append retry-step repair records"),
              )
            True -> spawn_recovered_workflow_resumption(state, resumption)
          }
        }
      }
    QueuedControlOperationCompleted(bodies) -> {
      let #(state, appended) =
        append_ledger_bodies(
          state,
          bodies,
          "control_operation_completion_append_failed",
        )
      case appended {
        True -> state
        False ->
          append_control_operation_failure(
            state,
            operation_id,
            "ledger_append_failed",
            Some("failed to append control operation completion records"),
          )
      }
    }
    QueuedControlOperationRejected(bodies) ->
      append_ledger_bodies_best_effort(
        state,
        bodies,
        "retry_step_rejection_diagnostic_append_failed",
      )
    QueuedControlOperationFailed(reason, message) ->
      append_control_operation_failure(state, operation_id, reason, message)
  }
}

fn make_retry_step_operation_id(
  state: State,
  run_id: String,
  step_id: Option(String),
) -> String {
  "retry-step:"
  <> run_id
  <> ":"
  <> option.unwrap(step_id, "auto")
  <> ":"
  <> int.to_string(state.dependencies.now_ms())
}

fn execute_queued_control_operation(
  state: State,
  operation: projection.ControlOperationStatus,
) -> QueuedControlOperationResult {
  case operation.operation_kind {
    "retry_step" -> execute_retry_step_operation(state, operation)
    "recollect_outputs" -> execute_recollect_outputs_operation(state, operation)
    "run_finalize" -> execute_run_finalize_operation(state, operation)
    "artifact_publication_retry" -> {
      let root = state.workflow.effective.workspace.root
      case
        artifact_publication_retry_control.execute_operation(
          root,
          operation,
          state.workflow.bundle,
          state.dependencies.publication_command_runner,
        )
      {
        artifact_publication_retry_control.ExecutionCompleted(bodies) ->
          QueuedControlOperationCompleted(bodies)
        artifact_publication_retry_control.ExecutionFailed(reason, message) ->
          QueuedControlOperationFailed(reason, message)
      }
    }
    _ ->
      QueuedControlOperationFailed(
        "unsupported_control_operation",
        Some(
          "unsupported queued control operation: " <> operation.operation_kind,
        ),
      )
  }
}

fn execute_retry_step_operation(
  state: State,
  operation: projection.ControlOperationStatus,
) -> QueuedControlOperationResult {
  let operator_command =
    command.RetryWorkflowStep(
      command.RetryWorkflowStepRunId(option.unwrap(operation.run_id, "")),
      operation.requested_step_id,
    )
  case option.unwrap(operation.issue_id, "") {
    "" ->
      QueuedControlOperationFailed(
        "operation_missing_issue_id",
        Some("retry-step operation is missing issue metadata"),
      )
    issue_id ->
      case
        retry_step_issue_preflight(
          state,
          state.ledger_projection,
          operator_command,
          command.RetryWorkflowStepRunId(option.unwrap(operation.run_id, "")),
          option.unwrap(operation.run_id, ""),
          issue_id,
        )
      {
        Error(result) -> queued_control_operation_failure_result(result)
        Ok(issue) -> continue_retry_step_operation(state, operation, issue)
      }
  }
}

fn continue_retry_step_operation(
  state: State,
  operation: projection.ControlOperationStatus,
  issue: tracker_issue.Issue,
) -> QueuedControlOperationResult {
  let projection_state = state.ledger_projection
  let target =
    command.RetryWorkflowStepRunId(option.unwrap(operation.run_id, ""))
  let step_id = operation.requested_step_id
  let observation =
    startup_recovery.current_workflow_observation(state.workflow.bundle, issue)
  case workflow_repair.plan(projection_state, target, step_id, observation) {
    Error(error) -> {
      let reason = workflow_repair.describe_error(error)
      QueuedControlOperationFailed(
        reason,
        Some(retry_step_operation.failure_message(
          reason,
          workflow_repair.error_message(error),
          option.unwrap(operation.run_id, ""),
          step_id,
        )),
      )
    }
    Ok(plan) ->
      case
        recovery.finalize_retry_step_candidates_with_config(
          projection_state,
          [plan.candidate],
          dict.from_list([#(plan.run_id, observation)]),
          artifact_store.new(state.workflow.effective.workspace.root),
          state.dependencies.now_ms(),
          state.workflow.effective,
        )
      {
        Error(error) ->
          QueuedControlOperationFailed(
            recovery.describe_error(error),
            Some(recovery.describe_error(error)),
          )
        Ok(finalization) ->
          case finalization.resumptions {
            [resumption] -> {
              let message = retry_step_applied_message(plan)
              let bodies =
                list.append(
                  plan.records_to_append,
                  list.append(
                    ledger_record_bodies(finalization.records_to_append),
                    [
                      record.ControlOperationCompleted(
                        operation.operation_id,
                        Some(message),
                      ),
                    ],
                  ),
                )
              QueuedControlOperationSucceeded(bodies, resumption)
            }
            _ ->
              QueuedControlOperationRejected(
                list.append(
                  retry_step_operation.diagnostic_bodies(finalization),
                  [
                    record.ControlOperationFailed(
                      operation.operation_id,
                      retry_step_operation.rejection_reason(finalization),
                      retry_step_operation.rejection_message(
                        finalization,
                        plan.run_id,
                        operation.requested_step_id,
                      ),
                    ),
                  ],
                ),
              )
          }
      }
  }
}

fn execute_recollect_outputs_operation(
  state: State,
  operation: projection.ControlOperationStatus,
) -> QueuedControlOperationResult {
  let run_id = option.unwrap(operation.run_id, "")
  case option.unwrap(operation.issue_id, "") {
    "" ->
      QueuedControlOperationFailed(
        "operation_missing_issue_id",
        Some("recollect-outputs operation is missing issue metadata"),
      )
    issue_id -> {
      let operator_command = command.RecollectWorkflowOutputs(run_id)
      case
        recollect_outputs_issue_preflight_for_run(
          state,
          operator_command,
          run_id,
          issue_id,
        )
      {
        Error(result) -> queued_control_operation_failure_result(result)
        Ok(issue) ->
          execute_recollect_outputs_with_issue(state, operation, issue)
      }
    }
  }
}

fn execute_recollect_outputs_with_issue(
  state: State,
  operation: projection.ControlOperationStatus,
  issue: tracker_issue.Issue,
) -> QueuedControlOperationResult {
  case replay_projection_for_operator(state) {
    Error(reason) ->
      QueuedControlOperationFailed("ledger_read_failed", Some(reason))
    Ok(projection_state) -> {
      let root = state.workflow.effective.workspace.root
      case
        recollect_outputs_control.execute_operation(
          root,
          operation,
          state.workflow.bundle,
          state.dependencies.now_ms,
          issue,
          projection_state,
        )
      {
        recollect_outputs_control.ExecutionCompleted(bodies) ->
          QueuedControlOperationCompleted(bodies)
        recollect_outputs_control.ExecutionFailed(reason, message) ->
          QueuedControlOperationFailed(reason, message)
      }
    }
  }
}

fn queued_control_operation_failure_result(
  result: command.CommandResult,
) -> QueuedControlOperationResult {
  QueuedControlOperationFailed(
    case result.status {
      command.Rejected(reason) | command.NotAllowed(reason) -> reason
      command.NotFound -> "not_found"
      command.Applied | command.Queued -> "rejected"
    },
    result.message,
  )
}

fn execute_run_finalize_operation(
  state: State,
  operation: projection.ControlOperationStatus,
) -> QueuedControlOperationResult {
  let run_id = option.unwrap(operation.run_id, "")
  case replay_projection_for_operator(state) {
    Error(message) ->
      QueuedControlOperationFailed("ledger_read_failed", Some(message))
    Ok(projected) ->
      case run_finalize_control.dry_run(projected, run_id) {
        Error(#(code, message)) ->
          QueuedControlOperationFailed(code, Some(message))
        Ok(plan) ->
          case plan.already_finalized {
            True ->
              QueuedControlOperationCompleted([
                record.ControlOperationCompleted(
                  operation.operation_id,
                  Some(run_finalize_control.already_finalized_message(run_id)),
                ),
              ])
            False ->
              case
                recollect_outputs_issue_preflight_for_run(
                  state,
                  command.RunFinalize(
                    run_id: plan.run_id,
                    validate: True,
                    outputs: command.RunFinalizeOutputsAuto,
                    publish: True,
                    update_tracker: True,
                    dry_run: False,
                    reason: "queued execution",
                  ),
                  run_id,
                  plan.issue_id,
                )
              {
                Error(result) -> queued_control_operation_failure_result(result)
                Ok(issue) -> {
                  let current =
                    startup_recovery.current_workflow_observation(
                      state.workflow.bundle,
                      issue,
                    )
                  case
                    run_finalize_control.validated_dry_run(
                      projected,
                      run_id,
                      current,
                      state.workflow.effective.workspace.root,
                    )
                  {
                    Error(#(code, message)) ->
                      QueuedControlOperationFailed(code, Some(message))
                    Ok(plan) ->
                      continue_run_finalize_operation(
                        state,
                        operation,
                        projected,
                        plan,
                        issue,
                      )
                  }
                }
              }
          }
      }
  }
}

fn continue_run_finalize_operation(
  state: State,
  operation: projection.ControlOperationStatus,
  projected: projection.Projection,
  plan: run_finalize_control.FinalizePlan,
  issue: tracker_issue.Issue,
) -> QueuedControlOperationResult {
  let output_bodies = case plan.output_action {
    "recollect_outputs" ->
      collect_run_finalize_output_bodies(
        recollect_outputs_control.execute_operation(
          state.workflow.effective.workspace.root,
          operation,
          state.workflow.bundle,
          state.dependencies.now_ms,
          issue,
          projected,
        ),
      )
    _ -> Ok([])
  }
  case output_bodies {
    Error(#(reason, message)) ->
      QueuedControlOperationFailed(reason, Some(message))
    Ok(output_bodies) -> {
      let publication_projection =
        projection.fold_from(
          projected,
          ledger_records_for_bodies(state.dependencies.now_ms(), output_bodies),
        )
      case
        artifact_publication_retry_control.retry_all_attempts_with_projection(
          state.workflow.effective.workspace.root,
          publication_projection,
          plan.run_id,
          state.workflow.bundle,
          state.dependencies.publication_command_runner,
        )
      {
        Error(#(reason, message)) ->
          QueuedControlOperationFailed(reason, Some(message))
        Ok(_) ->
          case run_finalize_transition_tracker(state, issue, plan.workflow_id) {
            Error(message) ->
              QueuedControlOperationFailed(
                "tracker_update_failed",
                Some(message),
              )
            Ok(Nil) ->
              QueuedControlOperationCompleted(
                list.append(output_bodies, [
                  record.WorkflowRunDiagnostic(
                    plan.run_id,
                    plan.workflow_id,
                    plan.issue_id,
                    "run_finalize_completed",
                  ),
                  run_finalize_control.finish_record(plan),
                  record.ControlOperationCompleted(
                    operation.operation_id,
                    Some("run finalize completed without starting a worker"),
                  ),
                ]),
              )
          }
      }
    }
  }
}

fn collect_run_finalize_output_bodies(
  outcome: recollect_outputs_control.ExecutionOutcome,
) -> Result(List(record.RecordBody), #(String, String)) {
  case outcome {
    recollect_outputs_control.ExecutionCompleted(bodies) ->
      Ok(
        list.filter(bodies, fn(body) {
          case body {
            record.ControlOperationCompleted(_, _) -> False
            _ -> True
          }
        }),
      )
    recollect_outputs_control.ExecutionFailed(reason, message) ->
      Error(#(reason, option.unwrap(message, reason)))
  }
}

fn run_finalize_transition_tracker(
  state: State,
  issue: tracker_issue.Issue,
  workflow_id: String,
) -> Result(Nil, String) {
  case
    publication_recovery_completion_target(
      state.workflow.effective.handoff,
      workflow_id,
    )
  {
    Error(message) -> Error(message)
    Ok(#(target_state_id, target_state_name)) ->
      case
        transition_issue_state(
          state,
          issue,
          target_state_id,
          target_state_name,
          "run_finalize",
        )
      {
        Ok(_) -> Ok(Nil)
        Error(#(_, reason, message)) -> Error(reason <> ": " <> message)
      }
  }
}

fn append_control_operation_failure(
  state: State,
  operation_id: String,
  reason: String,
  message: Option(String),
) -> State {
  append_ledger_bodies_best_effort(
    state,
    [record.ControlOperationFailed(operation_id, reason, message)],
    "control_operation_failed_append_failed",
  )
}

fn replay_projection_for_operator(
  state: State,
) -> Result(projection.Projection, String) {
  use ledger_path <- result.try(
    ledger.path_for_workspace_root(state.workflow.effective.workspace.root)
    |> result.map_error(ledger.ledger_error_to_string),
  )
  use read <- result.try(
    ledger.read_records(ledger_path)
    |> result.map_error(ledger.ledger_error_to_string),
  )
  Ok(projection.fold(read.records))
}

fn cleanup_orphan_steps_for_operator(
  state: State,
  operator_command: command.OperatorCommand,
  run_id: String,
  dry_run: Bool,
) -> #(State, command.CommandResult) {
  case replay_projection_for_operator(state) {
    Error(reason) -> #(
      state,
      command.rejected(operator_command, "ledger_read_failed", Some(reason)),
    )
    Ok(projected) -> {
      let active_session_ids =
        worker_registry.active_yaml_step_sessions_for_run(
          state.registry,
          run_id,
        )
      let parent_active = case
        worker_registry.worker_for_run(state.registry, run_id)
      {
        Ok(_) -> True
        Error(Nil) -> False
      }
      case
        yaml_step_orphans.plan_cleanup(
          projected,
          run_id,
          active_session_ids,
          parent_active,
        )
      {
        Error(yaml_step_orphans.UnknownRun) -> #(
          state,
          command.not_found(
            operator_command,
            Some("workflow run not found: " <> run_id),
          ),
        )
        Error(yaml_step_orphans.ParentStillActive(parent_state)) -> #(
          state,
          command.rejected(
            operator_command,
            "parent_run_active",
            Some(
              "workflow run "
              <> run_id
              <> " is still active ("
              <> parent_state
              <> ")",
            ),
          ),
        )
        Ok(plan) -> {
          let message = cleanup_orphan_steps_message(plan, dry_run)
          case dry_run {
            True -> #(state, command.applied(operator_command, Some(message)))
            False ->
              case cleanup_orphaned_yaml_children_from_plan(state, plan, None) {
                Ok(state) -> #(
                  remove_yaml_step_tokens_for_run(state, plan.run_id),
                  command.applied(operator_command, Some(message)),
                )
                Error(Nil) -> #(
                  state,
                  command.rejected(
                    operator_command,
                    "ledger_append_failed",
                    Some("failed to append orphan step interruption records"),
                  ),
                )
              }
          }
        }
      }
    }
  }
}

fn cleanup_orphan_steps_message(
  plan: yaml_step_orphans.CleanupPlan,
  dry_run: Bool,
) -> String {
  yaml_step_orphans.describe_cleanup(plan, dry_run: dry_run)
}

fn orphan_cleanup_plan_for_run(
  state: State,
  run_id: String,
  parent_state: String,
) -> Result(yaml_step_orphans.CleanupPlan, Nil) {
  Ok(yaml_step_orphans.CleanupPlan(
    run_id: run_id,
    parent_state: parent_state,
    candidates: active_yaml_cleanup_candidates_for_run(state.registry, run_id),
  ))
}

fn active_yaml_cleanup_candidates_for_run(
  registry: worker_registry.Registry,
  run_id: String,
) -> List(yaml_step_orphans.CleanupCandidate) {
  registry
  |> worker_registry.active_yaml_step_handles_for_run(run_id)
  |> list.map(fn(handle) {
    yaml_step_orphans.CleanupCandidate(
      workflow_id: handle.workflow_id,
      step_id: handle.step_id,
      attempt_index: handle.attempt_index,
      session_id: Some(handle.session_id),
    )
  })
}

fn cleanup_orphaned_yaml_children_after_parent_stop(
  state: State,
  run_id: String,
  _stop_reason: String,
  issue_state_name: Option(String),
) -> State {
  case orphan_cleanup_plan_for_run(state, run_id, "stopped") {
    Error(Nil) -> state
    Ok(plan) ->
      case
        cleanup_orphaned_yaml_children_from_plan(state, plan, issue_state_name)
      {
        Ok(state) -> state
        Error(Nil) -> state
      }
  }
}

fn record_orphaned_yaml_children_from_plan(
  state: State,
  plan: yaml_step_orphans.CleanupPlan,
  issue_state_name: Option(String),
) -> #(State, Bool) {
  let bodies =
    yaml_step_orphans.interruption_records(
      plan.run_id,
      plan.candidates,
      "orphaned_parent_stopped",
    )
  let #(state, appended) = case bodies {
    [] -> #(state, True)
    _ ->
      append_ledger_bodies(
        state,
        bodies,
        "yaml_step_orphan_cleanup_append_failed",
      )
  }
  case appended {
    True ->
      list.each(plan.candidates, fn(candidate) {
        case candidate.session_id {
          Some(session_id) -> {
            let recovery =
              yaml_child_recovery_info(
                plan.run_id,
                candidate.step_id,
                candidate.attempt_index,
                issue_state_name,
                orphan_status: Some("orphaned_parent_stopped"),
                recommended_action: Some("cleanup_orphan_steps"),
              )
            hub.update_recovery(state.event_hub, session_id, Some(recovery))
            publish_recovery_lifecycle(
              state.event_hub,
              session_id,
              Some(recovery),
            )
          }
          None -> Nil
        }
      })
    False -> Nil
  }
  #(state, appended)
}

fn record_orphaned_yaml_children_after_parent_stop(
  state: State,
  run_id: String,
  issue_state_name: Option(String),
) -> State {
  case orphan_cleanup_plan_for_run(state, run_id, "stopping") {
    Error(Nil) -> state
    Ok(plan) -> {
      let #(state, _) =
        record_orphaned_yaml_children_from_plan(state, plan, issue_state_name)
      state
    }
  }
}

fn cleanup_orphaned_yaml_children_from_plan(
  state: State,
  plan: yaml_step_orphans.CleanupPlan,
  issue_state_name: Option(String),
) -> Result(State, Nil) {
  let #(state, appended) =
    record_orphaned_yaml_children_from_plan(state, plan, issue_state_name)
  case appended {
    False -> Error(Nil)
    True -> {
      request_abort_for_orphaned_yaml_children(state, plan.candidates)
      let state =
        finish_yaml_step_sessions_for_run(
          state,
          plan.run_id,
          session_reason.Stopped,
        )
      let state = clear_yaml_step_command_routes_for_run(state, plan.run_id)
      Ok(state)
    }
  }
}

fn request_abort_for_orphaned_yaml_children(
  state: State,
  candidates: List(yaml_step_orphans.CleanupCandidate),
) -> Nil {
  list.each(candidates, fn(candidate) {
    case candidate.session_id {
      Some(session_id) ->
        case
          worker_registry.step_command_subject_for_session(
            state.registry,
            session_id,
          )
        {
          Ok(subject) -> {
            let reply = process.new_subject()
            process.send(subject, worker_command.Abort(reply))
            Nil
          }
          Error(Nil) -> Nil
        }
      None -> Nil
    }
  })
}

fn yaml_child_recovery_info(
  run_id: String,
  step_id: String,
  attempt_index: Int,
  issue_state_name: Option(String),
  orphan_status orphan_status: Option(String),
  recommended_action recommended_action: Option(String),
) -> session_event.RecoveryInfo {
  session_event.RecoveryInfo(
    ..session_recovery.base_info(
      session_event.Cleanup,
      "workflow.yaml_step_orphan_cleanup",
      Some("parent workflow run stopped before child step completed"),
      [],
    ),
    workflow_run_id: Some(run_id),
    workflow_step_id: Some(step_id),
    workflow_attempt_index: Some(attempt_index),
    parent_session_id: Some(run_id),
    orphan_status: orphan_status,
    issue_state: issue_state_name,
    recommended_action: recommended_action,
  )
}

fn worker_issue_state_name(state: State, run_id: String) -> Option(String) {
  case worker_registry.worker_for_run(state.registry, run_id) {
    Ok(handle) -> Some(issue_state.to_string(handle.issue.state))
    Error(Nil) -> None
  }
}

fn worker_run_id_from_resolution(
  resolution: worker_registry.DownResolution,
) -> Option(String) {
  case resolution {
    worker_registry.WorkerDown(_, _, handle) -> Some(handle.run_id)
    worker_registry.ScheduledWorkerDown(_, run_id, _) -> Some(run_id)
    worker_registry.UnknownDown(_)
    | worker_registry.StepCommandDown(_, _)
    | worker_registry.WorkerDownStale(_, _)
    | worker_registry.ScheduledWorkerDownStale(_, _) -> None
  }
}

fn worker_issue_state_name_from_resolution(
  _state: State,
  resolution: worker_registry.DownResolution,
) -> Option(String) {
  case resolution {
    worker_registry.WorkerDown(_, _, handle) ->
      Some(issue_state.to_string(handle.issue.state))
    _ -> None
  }
}

fn issue_is_active_or_pending_except_parked(
  state: State,
  issue_id: String,
) -> Bool {
  let identity =
    orchestrator_state.issue_id_identity_for_backend(
      issue_id,
      state.tracker_adapter.kind,
    )
  has_active_run(state, issue_id)
  || dict.has_key(state.pending_claims, identity)
  || dict.has_key(state.pending_dispatch_validations, identity)
  || dict.has_key(state.pending_review_lane_preflights, identity)
  || dict.has_key(state.runtime.claimed, identity)
  || dict.has_key(state.runtime.retry_attempts, identity)
}

fn ledger_record_bodies(
  records: List(record.LedgerRecord),
) -> List(record.RecordBody) {
  records
  |> list.map(fn(ledger_record) { ledger_record.body })
}

fn retry_step_applied_message(plan: workflow_repair.RepairPlan) -> String {
  let repair_status = case plan.provenance_repair {
    Some(_) -> "provenance_repaired; "
    None -> "provenance_ok; "
  }
  repair_status
  <> "retrying run "
  <> plan.run_id
  <> " step "
  <> plan.selected_step_id
  <> " at attempt "
  <> int.to_string(plan.next_attempt_index)
}

fn operation_requested_step_id(
  state: State,
  operation_id: String,
) -> Option(String) {
  case projection.control_operation(state.ledger_projection, operation_id) {
    Ok(operation) -> operation.requested_step_id
    Error(Nil) -> None
  }
}

fn finish_operator_command_effect(
  state: State,
  request: transition_effects.OperatorCommandRequest,
  result: command.CommandResult,
) -> #(State, List(transition_types.Message)) {
  case
    dict.get(state.pending_operator_command_replies, request.correlation_id)
  {
    Ok(_) -> #(
      State(
        ..state,
        completed_operator_command_results: dict.insert(
          state.completed_operator_command_results,
          request.correlation_id,
          result,
        ),
      ),
      [],
    )
    Error(Nil) -> {
      log_state(state, "warn", "operator_command_reply_missing", [
        #("correlation_id", request.correlation_id),
        #("command", command.command_name(request.operator_command)),
      ])
      #(state, [])
    }
  }
}

fn set_operator_paused(state: State, paused: Bool) -> State {
  case paused {
    True ->
      State(..state, operator_paused: True)
      |> start_pending_scheduled_runs
      |> evaluate_scheduled_jobs
    False -> {
      let state = evaluate_scheduled_jobs(state)
      State(..state, operator_paused: False) |> start_pending_scheduled_runs
    }
  }
}

fn log_operator_result(
  state: State,
  result: command.CommandResult,
  extra_fields: List(log.Field),
) -> Nil {
  log_state(state, "info", "operator_command", [
    #("command", result.command),
    #("status", command.status_to_string(result.status)),
    ..extra_fields
  ])
}

fn reload_workflow_for_operator(
  state: State,
  operator_command: command.OperatorCommand,
) -> #(State, command.CommandResult, List(transition_types.Message)) {
  let outcome = workflow_reloader.reload_now(state.workflow)
  let #(state, follow_ups) = apply_workflow_reload_outcome(state, outcome)
  let reloaded = command.applied(operator_command, Some("workflow reloaded"))
  let failure_message = workflow_reloader.invalid_operator_message(outcome)
  case state.workflow.reload_state.current_status {
    config.CurrentValid -> {
      work_item_invalidation.unknown(work_item_invalidation.ManualRefresh)
      |> emit_work_item_invalidation_event(state, _)
      #(state, reloaded, follow_ups)
    }
    config.CurrentInvalid(reason) -> #(
      state,
      command.rejected(operator_command, reason, failure_message),
      follow_ups,
    )
  }
}

fn retry_artifact_publication_for_operator(
  state: State,
  operator_command: command.OperatorCommand,
  run_id: String,
  publication_id: Option(String),
) -> #(State, command.CommandResult) {
  case replay_projection_for_operator(state) {
    Error(reason) -> #(
      state,
      command.rejected(operator_command, "ledger_read_failed", Some(reason)),
    )
    Ok(projection_state) ->
      case
        artifact_publication_retry_control.queue_decision(
          projection_state,
          operator_command,
          run_id,
          publication_id,
          state.dependencies.now_ms(),
        )
      {
        Error(error) -> #(
          state,
          artifact_publication_retry_control.error_result(
            operator_command,
            error,
          ),
        )
        Ok(artifact_publication_retry_control.ExistingOperation(operation_id)) -> #(
          state,
          command.queued_operation(
            operator_command,
            operation_id,
            Some(
              "artifact publication retry already queued/running; poll query operation-status for completion",
            ),
          ),
        )
        Ok(artifact_publication_retry_control.NewOperation(
          operation_id,
          queued_body,
        )) -> {
          let #(state, appended) =
            append_ledger_bodies(
              state,
              [queued_body],
              "artifact_publication_retry_queue_append_failed",
            )
          case appended {
            False -> #(
              state,
              command.rejected(
                operator_command,
                "ledger_append_failed",
                Some("failed to append artifact publication retry operation"),
              ),
            )
            True -> {
              process.send(
                state.subject,
                RunQueuedControlOperation(operation_id),
              )
              #(
                state,
                command.queued_operation(
                  operator_command,
                  operation_id,
                  Some(
                    "artifact publication retry accepted; poll query operation-status for completion",
                  ),
                ),
              )
            }
          }
        }
      }
  }
}

fn schedule_run_now_for_operator(
  state: State,
  operator_command: command.OperatorCommand,
  job_id: String,
) -> #(State, command.CommandResult) {
  let state = evaluate_scheduled_jobs(state)
  case state.operator_paused {
    True -> #(
      state,
      command.rejected(
        operator_command,
        "dispatch_paused",
        Some("dispatch is paused"),
      ),
    )
    False ->
      case scheduled_job_by_id(state, job_id) {
        Error(Nil) -> #(
          state,
          command.not_found(operator_command, Some("scheduled job not found")),
        )
        Ok(job) ->
          case job.enabled {
            False -> #(
              state,
              command.rejected(
                operator_command,
                "scheduled_job_disabled",
                Some("scheduled job is disabled"),
              ),
            )
            True ->
              schedule_run_now_for_enabled_job(state, operator_command, job)
          }
      }
  }
}

fn schedule_run_now_for_enabled_job(
  state: State,
  operator_command: command.OperatorCommand,
  job: config_types.ScheduledJobConfig,
) -> #(State, command.CommandResult) {
  case
    scheduled_runtime.schedule_mode(
      state.scheduled_runtime,
      job.id,
      scheduled_worker_active_for_job(state, job.id),
    )
  {
    schedule_core.Idle -> {
      let now_ms = state.dependencies.now_ms()
      let run_id = schedule_core.manual_run_id(job.id, now_ms)
      let pending =
        scheduled_runtime.PendingStart(
          job_id: job.id,
          workflow_id: job.workflow,
          due_at_ms: now_ms,
          run_id: run_id,
          trigger: "manual",
          requested_at_ms: now_ms,
          attempt: 1,
          blocking_reason: "",
        )
      let state =
        append_ledger_bodies_best_effort(
          state,
          [
            record.ScheduledJobDue(
              job.id,
              job.workflow,
              now_ms,
              run_id,
              "manual",
            ),
          ],
          "scheduled_due_append_failed",
        )
      let state =
        append_ledger_bodies_best_effort(
          state,
          [
            record.ScheduledRunPending(
              job.id,
              job.workflow,
              now_ms,
              run_id,
              "manual",
              now_ms,
            ),
          ],
          "scheduled_pending_append_failed",
        )
      let state =
        State(
          ..state,
          scheduled_runtime: scheduled_runtime.insert_pending_start(
            state.scheduled_runtime,
            pending,
          ),
        )
        |> start_pending_scheduled_runs
      case
        scheduled_runtime.schedule_mode(
          state.scheduled_runtime,
          job.id,
          scheduled_worker_active_for_job(state, job.id),
        )
      {
        schedule_core.Pending(_) -> #(
          state,
          command.queued(operator_command, Some("scheduled run queued")),
        )
        _ -> #(
          state,
          command.applied(operator_command, Some("scheduled run started")),
        )
      }
    }
    _ -> #(
      state,
      command.rejected(
        operator_command,
        "overlap_running",
        Some("scheduled job already has a pending, active, or retrying run"),
      ),
    )
  }
}

fn route_worker_command_session_id(state: State, session_id: String) -> String {
  case dict.get(state.workers.route_to_session, session_id) {
    Ok(routed_session_id) -> routed_session_id
    Error(Nil) -> session_id
  }
}

fn abort_session_for_operator_sync(
  state: State,
  operator_command: command.OperatorCommand,
  session_id: String,
  timeout_ms: Int,
) -> #(State, command.CommandResult, List(transition_types.Message)) {
  let session_id = route_worker_command_session_id(state, session_id)
  case worker_for_session(state, session_id) {
    Error(Nil) ->
      case
        worker_registry.step_command_subject_for_session(
          state.registry,
          session_id,
        )
      {
        Ok(subject) ->
          send_sync_worker_command(
            state,
            operator_command,
            timeout_ms,
            subject,
            fn(subject, reply) {
              process.send(subject, worker_command.Abort(reply))
            },
          )
        Error(Nil) -> #(
          state,
          command.not_found(operator_command, Some("session not found")),
          [],
        )
      }
    Ok(handle) ->
      case handle.command_subject {
        Some(subject) ->
          send_sync_worker_command_with_timeout(
            state,
            operator_command,
            timeout_ms,
            subject,
            fn(subject, reply) {
              process.send(subject, worker_command.Abort(reply))
            },
            fn(state) {
              stop_session_for_operator(
                state,
                operator_command,
                session_id,
                session_reason.OperatorAbort,
              )
            },
          )
        None ->
          case
            worker_registry.step_command_subject_for_run(
              state.registry,
              handle.run_id,
            )
          {
            Ok(subject) ->
              send_sync_worker_command_with_timeout(
                state,
                operator_command,
                timeout_ms,
                subject,
                fn(subject, reply) {
                  process.send(subject, worker_command.Abort(reply))
                },
                fn(state) {
                  stop_session_for_operator(
                    state,
                    operator_command,
                    session_id,
                    session_reason.OperatorAbort,
                  )
                },
              )
            Error(_) ->
              stop_session_for_operator(
                state,
                operator_command,
                session_id,
                session_reason.OperatorAbort,
              )
          }
      }
  }
}

fn route_worker_command_sync(
  state: State,
  operator_command: command.OperatorCommand,
  session_id: String,
  timeout_ms: Int,
  send: fn(
    process.Subject(worker_command.Command),
    process.Subject(worker_command.Reply),
  ) -> Nil,
) -> #(State, command.CommandResult, List(transition_types.Message)) {
  let session_id = route_worker_command_session_id(state, session_id)
  case worker_for_session(state, session_id) {
    Error(Nil) ->
      case
        worker_registry.step_command_subject_for_session(
          state.registry,
          session_id,
        )
      {
        Ok(subject) ->
          send_sync_worker_command(
            state,
            operator_command,
            timeout_ms,
            subject,
            send,
          )
        Error(Nil) -> #(
          state,
          command.not_found(operator_command, Some("session not found")),
          [],
        )
      }
    Ok(handle) ->
      case handle.command_subject {
        Some(subject) ->
          send_sync_worker_command(
            state,
            operator_command,
            timeout_ms,
            subject,
            send,
          )
        None ->
          case
            worker_registry.step_command_subject_for_run(
              state.registry,
              handle.run_id,
            )
          {
            Ok(subject) ->
              send_sync_worker_command(
                state,
                operator_command,
                timeout_ms,
                subject,
                send,
              )
            Error(worker_registry.NoActiveStepCommandSubject) -> #(
              state,
              command.not_allowed(
                operator_command,
                "worker_command_subject_unavailable",
                Some("session worker does not accept operator commands"),
              ),
              [],
            )
            Error(worker_registry.MultipleActiveStepCommandSubjects) -> #(
              state,
              command.not_allowed(
                operator_command,
                "multiple_step_command_subjects",
                Some(
                  "multiple active step sessions accept operator commands; target a step session",
                ),
              ),
              [],
            )
          }
      }
  }
}

fn send_sync_worker_command(
  state: State,
  operator_command: command.OperatorCommand,
  timeout_ms: Int,
  subject: process.Subject(worker_command.Command),
  send: fn(
    process.Subject(worker_command.Command),
    process.Subject(worker_command.Reply),
  ) -> Nil,
) -> #(State, command.CommandResult, List(transition_types.Message)) {
  send_sync_worker_command_with_timeout(
    state,
    operator_command,
    timeout_ms,
    subject,
    send,
    fn(state) {
      #(state, operator_worker_command.timeout_result(operator_command), [])
    },
  )
}

fn send_sync_worker_command_with_timeout(
  state: State,
  operator_command: command.OperatorCommand,
  timeout_ms: Int,
  subject: process.Subject(worker_command.Command),
  send: fn(
    process.Subject(worker_command.Command),
    process.Subject(worker_command.Reply),
  ) -> Nil,
  on_timeout: fn(State) ->
    #(State, command.CommandResult, List(transition_types.Message)),
) -> #(State, command.CommandResult, List(transition_types.Message)) {
  let reply = process.new_subject()
  send(subject, reply)
  case
    process.receive(
      reply,
      within: control_command_handler.worker_command_timeout(timeout_ms),
    )
  {
    Ok(worker_reply) -> #(
      state,
      operator_worker_command.reply_result(operator_command, worker_reply),
      [],
    )
    Error(Nil) -> on_timeout(state)
  }
}

fn stop_session_for_operator(
  state: State,
  operator_command: command.OperatorCommand,
  session_id: String,
  reason: session_reason.WorkerExitReason,
) -> #(State, command.CommandResult, List(transition_types.Message)) {
  case worker_for_session(state, session_id) {
    Error(Nil) -> #(
      state,
      command.not_found(operator_command, Some("session not found")),
      [],
    )
    Ok(handle) -> {
      let reason_text = session_reason.to_string(reason)
      let state =
        record_orphaned_yaml_children_after_parent_stop(
          state,
          handle.run_id,
          Some(issue_state.to_string(handle.issue.state)),
        )
      #(state, command.applied(operator_command, Some(reason_text)), [
        transition_types.WorkerStopRequested(
          identity.session_id_from_string(session_id),
          reason,
          transition_lifecycle_context(state),
        ),
      ])
    }
  }
}

fn has_active_run(state: State, issue_id: String) -> Bool {
  worker_registry.has_active_run(state.registry, issue_id)
}

fn active_run_count(state: State) -> Int {
  active_run_issue_ids(state) |> list.length
}

fn active_run_issue_ids(state: State) -> List(String) {
  []
  |> append_unique_list(
    state.runtime.running
    |> dict.values
    |> list.map(fn(entry) { entry.issue.id }),
  )
  |> append_unique_list(worker_registry.worker_issue_ids(state.registry))
}

fn issue_for_ref(
  state: State,
  issue_ref: command.IssueRef,
) -> Result(tracker_issue.Issue, command.CommandStatus) {
  case issue_ref {
    command.IssueId(issue_id) -> issue_for_id(state, issue_id)
    command.IssueIdentifier(identifier) ->
      issue_for_identifier(state, identifier)
  }
}

fn issue_for_id(
  state: State,
  issue_id: String,
) -> Result(tracker_issue.Issue, command.CommandStatus) {
  let identity = orchestrator_state.linear_issue_id_identity(issue_id)
  case dict.get(state.runtime.running, identity) {
    Ok(entry) -> Ok(entry.issue)
    Error(Nil) ->
      case dict.get(state.pending_claims, identity) {
        Ok(pending) -> Ok(pending.issue)
        Error(Nil) ->
          case dict.get(state.pending_dispatch_validations, identity) {
            Ok(pending) -> Ok(pending.issue)
            Error(Nil) ->
              case dict.get(state.pending_review_lane_preflights, identity) {
                Ok(pending) -> Ok(pending.issue)
                Error(Nil) ->
                  case
                    orchestrator_state.completed_for(state.runtime, identity)
                  {
                    Ok(issue) -> Ok(issue)
                    Error(Nil) -> fetch_issue_by_id(state, issue_id)
                  }
              }
          }
      }
  }
}

fn issue_for_identifier(
  state: State,
  identifier: String,
) -> Result(tracker_issue.Issue, command.CommandStatus) {
  let local = local_issues_with_identifier(state, identifier)
  case unique_issue(local) {
    Ok(issue) -> Ok(issue)
    Error(command.NotFound) ->
      case fetch_candidates_with_identifier(state, identifier) {
        Ok(issue) -> Ok(issue)
        Error(command.NotFound) ->
          case parked_issue_id_for_identifier(state, identifier) {
            Ok(issue_id) -> fetch_issue_by_id(state, issue_id)
            Error(err) -> Error(err)
          }
        Error(err) -> Error(err)
      }
    Error(err) -> Error(err)
  }
}

fn local_issues_with_identifier(
  state: State,
  identifier: String,
) -> List(tracker_issue.Issue) {
  let running =
    state.runtime.running
    |> dict.values
    |> list.map(fn(entry) { entry.issue })
  let pending =
    state.pending_claims
    |> dict.values
    |> list.map(fn(entry) { entry.issue })
  let pending_validations =
    state.pending_dispatch_validations
    |> dict.values
    |> list.map(fn(entry) { entry.issue })
  let pending_preflights =
    state.pending_review_lane_preflights
    |> dict.values
    |> list.map(fn(entry) { entry.issue })
  let completed = orchestrator_state.completed_issues(state.runtime)
  list.append(
    running,
    list.append(
      pending,
      list.append(
        pending_validations,
        list.append(pending_preflights, completed),
      ),
    ),
  )
  |> list.filter(fn(issue) { issue.identifier == identifier })
}

fn fetch_candidates_with_identifier(
  state: State,
  identifier: String,
) -> Result(tracker_issue.Issue, command.CommandStatus) {
  case adapter.lookup_runtime_issue(state.tracker_adapter, identifier) {
    Ok(Some(issue)) -> Ok(issue)
    Ok(None) -> Error(command.NotFound)
    Error(_) -> Error(command.Rejected("candidate_fetch_failed"))
  }
}

fn fetch_issue_by_id(
  state: State,
  issue_id: String,
) -> Result(tracker_issue.Issue, command.CommandStatus) {
  case
    adapter.refresh_runtime_issues_by_ids(state.tracker_adapter, [issue_id])
  {
    Ok([issue]) -> Ok(issue)
    Ok([]) -> Error(command.NotFound)
    Ok(_) -> Error(command.Rejected("ambiguous_issue_id"))
    Error(_) -> Error(command.Rejected("issue_fetch_failed"))
  }
}

fn unique_issue(
  issues: List(tracker_issue.Issue),
) -> Result(tracker_issue.Issue, command.CommandStatus) {
  case issues {
    [] -> Error(command.NotFound)
    [issue] -> Ok(issue)
    [_, ..] -> Error(command.Rejected("ambiguous_issue_identifier"))
  }
}

fn parked_issue_id_for_ref(
  state: State,
  issue_ref: command.IssueRef,
) -> Result(String, command.CommandStatus) {
  case issue_ref {
    command.IssueId(issue_id) ->
      case
        dict.has_key(
          state.runtime.parked,
          orchestrator_state.linear_issue_id_identity(issue_id),
        )
      {
        True -> Ok(issue_id)
        False -> Error(command.NotFound)
      }
    command.IssueIdentifier(identifier) ->
      parked_issue_id_for_identifier(state, identifier)
  }
}

fn parked_issue_id_for_identifier(
  state: State,
  identifier: String,
) -> Result(String, command.CommandStatus) {
  let matches =
    state.runtime.parked
    |> dict.values
    |> list.filter(fn(entry) { entry.identifier == identifier })
  case matches {
    [] -> Error(command.NotFound)
    [entry] -> Ok(entry.issue_id)
    [_, ..] -> Error(command.Rejected("ambiguous_issue_identifier"))
  }
}

fn worker_for_session(
  state: State,
  session_id: String,
) -> Result(worker_registry.WorkerHandle, Nil) {
  worker_registry.worker_for_session(state.registry, session_id)
}

fn cancel_retry_timer(state: State, issue_id: String) -> State {
  State(
    ..state,
    retry: retry_scheduler.cancel_timer(
      state.retry,
      issue_id,
      state.dependencies.cancel_timer,
    ),
  )
}

fn poll_tick_shell(state: State, generation: Int) -> State {
  let state =
    run_transition_messages(state, [
      transition_types.PollTick(generation, poll_snapshot(state)),
    ])
  case poll_scheduler.in_flight(state.poll) == Some(generation) {
    False -> state
    True -> {
      let state = reload_if_changed(state)
      let state = evaluate_scheduled_jobs(state)
      begin_running_refresh(state, generation)
    }
  }
}

fn reload_if_changed(state: State) -> State {
  let #(state, follow_ups) =
    apply_workflow_reload_outcome(
      state,
      workflow_reloader.reload_if_changed(state.workflow),
    )
  run_transition_messages(state, follow_ups)
}

fn apply_workflow_reload_outcome(
  state: State,
  outcome: workflow_reloader.Outcome,
) -> #(State, List(transition_types.Message)) {
  case outcome {
    workflow_reloader.Unchanged(workflow) -> #(
      State(..state, workflow: workflow),
      [],
    )
    workflow_reloader.Reloaded(workflow) ->
      apply_reloaded_workflow(state, workflow)
    workflow_reloader.Invalid(workflow, reason, message) -> {
      let state = State(..state, workflow: workflow)
      let fields = workflow_reloader.invalid_log_fields(reason, message)
      log_state(state, "warn", "workflow_reload_failed", fields)
      #(state, [])
    }
  }
}

fn apply_reloaded_workflow(
  state: State,
  workflow: workflow_reloader.State,
) -> #(State, List(transition_types.Message)) {
  let effective = workflow.effective
  let tracker_adapter = state.dependencies.make_tracker_adapter(effective)
  let state =
    State(
      ..state,
      workflow: workflow,
      tracker_client: adapter_legacy.workflow_compat_client(tracker_adapter),
      tracker_adapter: tracker_adapter,
    )
  let state = refresh_scheduled_next_due_after_reload(state)
  let state = reconcile_remote_client_after_reload(state)
  let follow_ups = [
    transition_types.WorkflowRuntimeReloaded(
      poll_interval_ms: effective.polling.interval_ms,
      max_concurrent_agents: effective.agent.max_concurrent_agents,
    ),
  ]
  log_state(state, "info", "workflow_reloaded", [])
  #(state, follow_ups)
}

fn reconcile_remote_client_after_reload(state: State) -> State {
  let ui_server = state.workflow.effective.ui_server
  let state =
    State(
      ..state,
      read_model: state.read_model
        |> read_model.update_ui_server_enabled(ui_server_enabled(ui_server)),
    )
  case ui_server_enabled(ui_server) {
    False -> stop_remote_client_and_clear(state, read_model.Disabled)
    True ->
      case state.remote_client {
        None -> restart_remote_client_if_enabled(state)
        Some(_) ->
          state
          |> stop_remote_client_and_clear(read_model.Starting)
          |> restart_remote_client_if_enabled
      }
  }
}

fn refresh_scheduled_next_due_after_reload(state: State) -> State {
  let now_ms = state.dependencies.now_ms()
  let runtime =
    state.workflow.bundle.orchestrator.scheduled_jobs
    |> list.filter(fn(job) { job.enabled })
    |> list.fold(state.scheduled_runtime, fn(runtime, job) {
      let #(runtime, _) =
        scheduled_runtime.ensure_next_due(runtime, job.id, now_ms, job.every_ms)
      runtime
    })
  State(..state, scheduled_runtime: runtime)
}

fn begin_running_refresh(state: State, generation: Int) -> State {
  let ids = active_run_issue_ids(state)
  case ids {
    [] -> begin_candidate_fetch_or_finish(state, generation)
    _ ->
      enqueue_side_effect(
        state,
        effect_runner.RefreshRunning(
          generation: generation,
          ids: ids,
          tracker_adapter: state.tracker_adapter,
        ),
      )
  }
}

fn begin_candidate_fetch_or_finish(state: State, generation: Int) -> State {
  run_transition_messages(state, [
    transition_types.CandidateFetchStartRequested(
      generation,
      transition_dispatch_context(state),
    ),
  ])
}

fn handle_running_refresh_finished(
  state: State,
  generation: Int,
  result: Result(List(tracker_issue.Issue), error.TrackerError),
) -> State {
  case result {
    Ok(issues) ->
      emit_work_item_invalidation_for_issues(
        state,
        work_item_invalidation.TrackerRefresh,
        issues,
      )
    Error(_tracker_error) -> Nil
  }
  let result = case result {
    Ok(issues) -> Ok(issues)
    Error(err) -> Error(error.tracker_code(err))
  }
  run_transition_messages(state, [
    transition_types.RunningRefreshCompleted(
      generation,
      poll_snapshot(state),
      result,
      transition_dispatch_context(state),
    ),
  ])
}

fn handle_candidate_fetch_finished(
  state: State,
  generation: Int,
  result: Result(List(tracker_issue.Issue), error.TrackerError),
) -> State {
  case result {
    Ok(candidates) ->
      emit_work_item_invalidation_for_issues(
        state,
        work_item_invalidation.PollRefresh,
        candidates,
      )
    Error(_tracker_error) -> Nil
  }
  let result = case result {
    Ok(candidates) -> Ok(candidates)
    Error(err) -> Error(error.tracker_code(err))
  }
  run_transition_messages(state, [
    transition_types.CandidateFetchCompleted(
      generation,
      poll_snapshot(state),
      result,
      transition_dispatch_context(state),
    ),
  ])
}

fn append_unique_list(
  existing: List(String),
  values: List(String),
) -> List(String) {
  list.fold(values, existing, fn(acc, value) { append_unique(acc, value) })
}

fn append_unique(values: List(String), value: String) -> List(String) {
  case list.contains(values, value) {
    True -> values
    False -> list.append(values, [value])
  }
}

fn emit_work_item_invalidation_event(
  state: State,
  event: work_item_invalidation.Event,
) -> Nil {
  state.dependencies.emit_work_item_invalidation(state.remote_client, event)
}

fn emit_work_item_invalidation_for_issues(
  state: State,
  source: work_item_invalidation.Source,
  issues: List(tracker_issue.Issue),
) -> Nil {
  case issues {
    [] -> Nil
    _ ->
      work_item_invalidation.from_issues(
        source,
        state.tracker_adapter.kind,
        issues,
      )
      |> emit_work_item_invalidation_event(state, _)
  }
}

fn emit_work_item_invalidation_for_issue(
  state: State,
  source: work_item_invalidation.Source,
  issue: tracker_issue.Issue,
) -> Nil {
  emit_work_item_invalidation_for_issues(state, source, [issue])
}

fn emit_work_item_invalidation_for_task_ref(
  state: State,
  source: work_item_invalidation.Source,
  ref: task.TaskRef,
) -> Nil {
  work_item_invalidation.from_task_refs(source, [ref])
  |> emit_work_item_invalidation_event(state, _)
}

fn emit_work_item_invalidation_for_outbox_result(
  state: State,
  source: work_item_invalidation.Source,
  outbox: outbox_effects.Intent,
  result: Result(a, error.TrackerError),
) -> Nil {
  case result {
    Ok(_) ->
      emit_work_item_invalidation_for_task_ref(
        state,
        source,
        outbox_effects.task_ref_from_fields(outbox.task_ref),
      )
    Error(_tracker_error) -> Nil
  }
}

fn emit_work_item_invalidation_for_run(state: State, run_id: String) -> Nil {
  case worker_registry.worker_for_run(state.registry, run_id) {
    Ok(handle) ->
      emit_work_item_invalidation_for_task_ref(
        state,
        work_item_invalidation.WorkflowObserved,
        handle.task_ref,
      )
    Error(Nil) -> Nil
  }
}

fn poll_snapshot(state: State) -> transition_types.PollSnapshot {
  transition_types.PollSnapshot(
    generation: poll_scheduler.generation(state.poll),
    in_flight: poll_scheduler.in_flight(state.poll),
  )
}

fn transition_dispatch_context(
  state: State,
) -> transition_types.DispatchContext {
  transition_types.dispatch_context(
    state.workflow.effective,
    state.workflow.bundle.orchestrator,
    state.tracker_adapter.kind,
    state.workflow.bundle.orchestrator.routing,
    runtime_bundle.normalized_workflows(state.workflow.bundle),
    config.can_dispatch(state.workflow.reload_state),
    state.operator_paused,
    worker_registry.worker_issue_ids(state.registry),
    worker_registry.worker_issues(state.registry),
    worker_registry.scheduled_worker_count(state.registry),
    state.workflow.effective.workspace.root,
    state.dependencies.now_ms(),
    state.recovery_by_issue,
    state.workflow.bundle.orchestrator.config_dir,
    review_lane_preflight_policy.from_env(),
  )
}

fn transition_state_from_daemon(state: State) -> transition_types.State {
  transition_types.State(
    runtime: state.runtime,
    workers: state.workers,
    pending_claims: state.pending_claims,
    pending_dispatch_validations: state.pending_dispatch_validations,
    pending_review_lane_preflights: state.pending_review_lane_preflights,
    lifecycle: transition_types.empty_lifecycle(),
    retry_refresh_generations: dict.from_list(
      retry_scheduler.refresh_generations(state.retry),
    ),
    next_dispatch_validation_generation: state.next_dispatch_validation_generation,
    next_session_sequence: worker_registry.next_session_sequence(state.registry),
  )
}

fn merge_transition_state(
  state: State,
  input_transition_state: transition_types.State,
  transition_state: transition_types.State,
) -> State {
  let pending_claims =
    merge_transition_field(
      state.pending_claims,
      input_transition_state.pending_claims,
      transition_state.pending_claims,
    )
    |> clear_dispatch_recovery_pending_claims(
      state.dispatch_recovery_cleared_pending_claims,
    )
  State(
    ..state,
    runtime: merge_transition_field(
      state.runtime,
      input_transition_state.runtime,
      transition_state.runtime,
    ),
    workers: merge_transition_field(
      state.workers,
      input_transition_state.workers,
      transition_state.workers,
    ),
    pending_claims: pending_claims,
    dispatch_recovery_cleared_pending_claims: [],
    pending_dispatch_validations: merge_transition_field(
      state.pending_dispatch_validations,
      input_transition_state.pending_dispatch_validations,
      transition_state.pending_dispatch_validations,
    ),
    pending_review_lane_preflights: merge_transition_field(
      state.pending_review_lane_preflights,
      input_transition_state.pending_review_lane_preflights,
      transition_state.pending_review_lane_preflights,
    ),
    next_dispatch_validation_generation: merge_transition_field(
      state.next_dispatch_validation_generation,
      input_transition_state.next_dispatch_validation_generation,
      transition_state.next_dispatch_validation_generation,
    ),
  )
}

fn clear_dispatch_recovery_pending_claims(
  pending_claims: Dict(identity.TaskIdentity, transition_types.PendingClaim),
  cleared: List(identity.TaskIdentity),
) -> Dict(identity.TaskIdentity, transition_types.PendingClaim) {
  list.fold(cleared, pending_claims, fn(pending_claims, task_identity) {
    dict.delete(pending_claims, task_identity)
  })
}

fn merge_transition_field(
  shell_value: value,
  input_value: value,
  transition_value: value,
) -> value {
  case transition_value == input_value {
    True -> shell_value
    False -> transition_value
  }
}

fn run_transition_messages(
  state: State,
  messages: List(transition_types.Message),
) -> State {
  daemon_transition_shell.run(transition_shell_context(state), messages)
}

fn transition_shell_context(
  state: State,
) -> daemon_transition_shell.Context(State) {
  daemon_transition_shell.context(
    state: state,
    transition_state_from_state: transition_state_from_daemon,
    merge_transition_state: merge_transition_state,
    log_exhausted: fn(state, message_limit) {
      log_state(state, "warn", "transition_runner_exhausted", [
        #("message_limit", int.to_string(message_limit)),
      ])
      state
    },
    mark_invariant_failure: fn(state, _) {
      State(..state, transition_invariant_violation_pending: True)
    },
    invariant_mode: transition_invariant_mode_from_env(),
    invariant_checker: state.dependencies.check_transition_invariants,
    max_messages: daemon_transition_shell.default_message_limit(),
    handlers: transition_shell_handlers(),
  )
}

fn transition_invariant_mode_from_env() -> daemon_transition_shell.InvariantMode {
  case control_file.get_env("SCHERZO_INVARIANTS") {
    Some(value) -> daemon_transition_shell.invariant_mode_from_string(value)
    None -> daemon_transition_shell.FailOnInvariantViolation
  }
}

fn transition_shell_handlers() -> daemon_transition_shell.ShellHandlers(State) {
  daemon_transition_shell.shell_handlers(
    append_ledger: transition_append_ledger,
    now_ms: fn(state) { state.dependencies.now_ms() },
    log_effect: fn(state, level, event, fields) {
      log_state(state, level, event, fields)
      state
    },
    start_worker: transition_start_worker,
    reply_snapshot: fn(state, _) { state },
    mark_poll_in_flight: fn(state, generation) {
      State(
        ..state,
        poll: poll_scheduler.mark_in_flight(state.poll, generation),
      )
    },
    schedule_next_poll: schedule_next_poll,
    fetch_candidates: fn(state, generation) {
      enqueue_side_effect(
        state,
        effect_runner.FetchCandidates(
          generation: generation,
          tracker_adapter: state.tracker_adapter,
        ),
      )
    },
    begin_dispatch_validation: fn(state, issue_id, generation) {
      enqueue_side_effect(
        state,
        effect_runner.ValidateDispatchClaim(
          issue_id: issue_id,
          generation: generation,
          tracker_adapter: state.tracker_adapter,
        ),
      )
    },
    begin_review_lane_preflight: fn(state, request) {
      enqueue_side_effect(state, effect_runner.ReviewLanePreflight(request))
    },
    reserve_session_sequence: transition_reserve_session_sequence,
    claim_issue: dispatch_time_recovery_claim_issue,
    report_invalid_workflow: fn(
      state,
      issue,
      violation,
      violation_fingerprint,
      reporting_policy_fingerprint,
    ) {
      let intent =
        outbox_effects.invalid_workflow_intent(
          issue,
          violation,
          violation_fingerprint,
          reporting_policy_fingerprint,
          state.workflow.effective.linear_contract,
          tracker_secrets(state),
        )
      enqueue_outbox_side_effect(state, intent, fn(intent) {
        effect_runner.ReportInvalidWorkflow(
          outbox: intent,
          issue: issue,
          violation: violation,
          violation_fingerprint: violation_fingerprint,
          reporting_policy_fingerprint: reporting_policy_fingerprint,
          contract_config: state.workflow.effective.linear_contract,
          comments: state.tracker_adapter.comments,
          state_transitions: state.tracker_adapter.state_transitions,
        )
      })
    },
    replay_outbox: fn(state, outbox_replay) {
      let intent = outbox_effects.recovered_intent(outbox_replay)
      case outbox_effects.replay_attempt_count(intent) {
        Error(error) ->
          record_outbox_replay_payload_failure(state, intent, error)
        Ok(attempt_count) -> {
          let #(state, appended) =
            append_outbox_attempt_with_count(state, intent, attempt_count)
          case appended {
            True ->
              enqueue_side_effect(
                state,
                effect_runner.ReplayOutbox(
                  outbox: outbox_replay,
                  comments: state.tracker_adapter.comments,
                  state_transitions: state.tracker_adapter.state_transitions,
                  scheduled_failures: state.tracker_adapter.scheduled_failures,
                ),
              )
            False -> state
          }
        }
      }
    },
    remove_retry_timer: fn(state, issue_id) {
      State(..state, retry: retry_scheduler.remove_timer(state.retry, issue_id))
    },
    finish_retry_refresh: fn(state, issue_id) {
      State(
        ..state,
        retry: retry_scheduler.finish_refresh(state.retry, issue_id),
      )
    },
    defer_retry_timer: transition_defer_retry_timer,
    begin_retry_refresh: transition_begin_retry_refresh,
    schedule_retry_timer: transition_schedule_retry_timer,
    schedule_recovered_retry_timer: transition_schedule_recovered_retry_timer,
    cancel_retry_timer: fn(state, issue_id, _, _) {
      cancel_retry_timer(state, issue_id)
    },
    release_claim: fn(state, issue_id) {
      log_state(state, "info", "claim_released", [#("issue_id", issue_id)])
      state
    },
    clear_recovery: fn(state, issue_id) {
      State(
        ..state,
        recovery_by_issue: dict.delete(state.recovery_by_issue, issue_id),
      )
    },
    worker_start_failed: transition_worker_start_failed,
    remove_worker: transition_remove_worker,
    publish_worker_exited: transition_publish_worker_exited,
    report_worker_success: transition_report_worker_success,
    report_worker_failure: transition_report_worker_failure,
    cleanup_workspace: transition_cleanup_workspace,
    park_issue: transition_park_issue,
    report_park: transition_report_park,
    stop_worker: transition_stop_worker,
    stop_worker_after_issue_refresh: transition_stop_worker_after_issue_refresh,
    register_yaml_step_started: transition_register_yaml_step_started,
    finish_yaml_step_route: transition_finish_yaml_step_route,
    finish_yaml_step_session: transition_finish_yaml_step_session,
    finish_yaml_step_sessions_for_run: transition_finish_yaml_step_sessions_for_run,
    clear_yaml_step_routes_for_run: transition_clear_yaml_step_routes_for_run,
    mark_yaml_run_stopping: transition_mark_yaml_run_stopping,
    shutdown_runtime: shutdown_runtime_shell,
    set_operator_paused: set_operator_paused,
    apply_operator_command: apply_shell_operator_command,
    finish_operator_command: finish_operator_command_effect,
    report_park_effect: fn(
      state,
      issue_id,
      issue_identifier,
      reason,
      release_policy,
      source_run_id,
    ) {
      enqueue_park_report(
        state,
        issue_id,
        issue_identifier,
        reason,
        release_policy,
        source_run_id,
      )
    },
  )
}

fn dispatch_time_recovery_claim_issue(
  state: State,
  task_ref: task.TaskRef,
  issue: tracker_issue.Issue,
  workspace_path: String,
  run_id: String,
  remaining_candidates: List(tracker_issue.Issue),
) -> State {
  case replay_projection_for_operator(state) {
    Error(reason) ->
      park_dispatch_recovery_rejection(
        clear_pending_claim_for_task_ref(state, task_ref),
        remaining_candidates,
        task_ref,
        issue,
        "dispatch_recovery_projection_unavailable",
        reason,
      )
    Ok(projected) -> {
      let observation =
        startup_recovery.current_workflow_observation(
          state.workflow.bundle,
          issue,
        )
      case dispatch_recovery.classify(projected, issue, observation) {
        dispatch_recovery.FreshDispatch ->
          enqueue_tracker_claim_issue(
            state,
            task_ref,
            issue,
            workspace_path,
            run_id,
          )
        dispatch_recovery.StepRecovery(plan) ->
          apply_dispatch_step_recovery(
            state,
            task_ref,
            issue,
            remaining_candidates,
            projected,
            observation,
            plan,
          )
        dispatch_recovery.PublicationRecovery(run_id, workflow_id) ->
          apply_dispatch_publication_recovery(
            state,
            task_ref,
            issue,
            remaining_candidates,
            run_id,
            workflow_id,
          )
        dispatch_recovery.PublicationAlreadyPublished(run_id, workflow_id) -> {
          let state = clear_pending_claim_for_task_ref(state, task_ref)
          log_state(state, "info", "dispatch_recovery_already_published", [
            #("issue_id", issue.id),
            #("run_id", run_id),
            #("workflow_id", workflow_id),
          ])
          case
            complete_dispatch_publication_recovery(
              state,
              issue,
              run_id,
              0,
              workflow_id,
            )
          {
            Ok(state) ->
              continue_dispatching_remaining_candidates(
                state,
                remaining_candidates,
              )
            Error(#(state, reason, message)) ->
              park_dispatch_recovery_rejection(
                state,
                remaining_candidates,
                task_ref,
                issue,
                reason,
                message,
              )
          }
        }
        dispatch_recovery.RejectRecovery(reason, message) ->
          park_dispatch_recovery_rejection(
            clear_pending_claim_for_task_ref(state, task_ref),
            remaining_candidates,
            task_ref,
            issue,
            reason,
            message,
          )
      }
    }
  }
}

fn enqueue_tracker_claim_issue(
  state: State,
  task_ref: task.TaskRef,
  issue: tracker_issue.Issue,
  workspace_path: String,
  run_id: String,
) -> State {
  let intent =
    outbox_effects.claim_intent(
      task_ref,
      issue,
      run_id,
      state.workflow.effective.handoff,
      tracker_secrets(state),
    )
  enqueue_outbox_side_effect(state, intent, fn(intent) {
    effect_runner.ClaimIssue(
      outbox: intent,
      task_ref: task_ref,
      issue: issue,
      workspace_path: workspace_path,
      run_id: run_id,
      capability: require_handoff_capability(state),
    )
  })
}

fn pending_claim_for_task_ref(
  state: State,
  task_ref: task.TaskRef,
) -> Result(transition_types.PendingClaim, Nil) {
  dict.get(state.pending_claims, orchestrator_state.task_ref_identity(task_ref))
}

fn clear_pending_claim_for_task_ref(
  state: State,
  task_ref: task.TaskRef,
) -> State {
  let task_identity = orchestrator_state.task_ref_identity(task_ref)
  State(
    ..state,
    pending_claims: dict.delete(state.pending_claims, task_identity),
    dispatch_recovery_cleared_pending_claims: [
      task_identity,
      ..state.dispatch_recovery_cleared_pending_claims
    ],
  )
}

fn apply_dispatch_step_recovery(
  state: State,
  task_ref: task.TaskRef,
  issue: tracker_issue.Issue,
  remaining_candidates: List(tracker_issue.Issue),
  projection_state: projection.Projection,
  observation: recovery.CurrentWorkflowObservation,
  plan: workflow_repair.RepairPlan,
) -> State {
  case
    recovery.finalize_retry_step_candidates_with_config(
      projection_state,
      [plan.candidate],
      dict.from_list([#(plan.run_id, observation)]),
      artifact_store.new(state.workflow.effective.workspace.root),
      state.dependencies.now_ms(),
      state.workflow.effective,
    )
  {
    Error(error) ->
      park_dispatch_recovery_rejection(
        clear_pending_claim_for_task_ref(state, task_ref),
        remaining_candidates,
        task_ref,
        issue,
        recovery.describe_error(error),
        recovery.describe_error(error),
      )
    Ok(finalization) ->
      case finalization.resumptions {
        [resumption] -> {
          let bodies =
            list.append(
              plan.records_to_append,
              ledger_record_bodies(finalization.records_to_append),
            )
          let #(state, appended) =
            append_ledger_bodies(
              state,
              bodies,
              "dispatch_recovery_append_failed",
            )
          case appended {
            False ->
              park_dispatch_recovery_rejection(
                clear_pending_claim_for_task_ref(state, task_ref),
                remaining_candidates,
                task_ref,
                issue,
                "ledger_append_failed",
                "failed to append dispatch recovery records",
              )
            True ->
              spawn_recovered_workflow_resumption(
                clear_pending_claim_for_task_ref(state, task_ref),
                resumption,
              )
          }
        }
        _ -> {
          let #(state, _diagnostic_appended) =
            append_ledger_bodies(
              state,
              retry_step_operation.diagnostic_bodies(finalization),
              "dispatch_recovery_rejection_diagnostic_append_failed",
            )
          park_dispatch_recovery_rejection(
            clear_pending_claim_for_task_ref(state, task_ref),
            remaining_candidates,
            task_ref,
            issue,
            retry_step_operation.rejection_reason(finalization),
            string.trim(option_with_default(
              retry_step_operation.dispatch_rejection_message(finalization),
              "dispatch recovery rejected",
            )),
          )
        }
      }
  }
}

fn apply_dispatch_publication_recovery(
  state: State,
  task_ref: task.TaskRef,
  issue: tracker_issue.Issue,
  remaining_candidates: List(tracker_issue.Issue),
  run_id: String,
  workflow_id: String,
) -> State {
  case
    artifact_publication_retry_control.retry_all_attempts(
      state.workflow.effective.workspace.root,
      run_id,
      state.workflow.bundle,
      state.dependencies.publication_command_runner,
    )
  {
    Ok(attempts) -> {
      let state = clear_pending_claim_for_task_ref(state, task_ref)
      let attempt_count = list.length(attempts)
      log_state(state, "info", "dispatch_recovery_publication_retry", [
        #("issue_id", issue.id),
        #("run_id", run_id),
        #("attempt_count", int.to_string(attempt_count)),
      ])
      case
        complete_dispatch_publication_recovery(
          state,
          issue,
          run_id,
          attempt_count,
          workflow_id,
        )
      {
        Ok(state) ->
          continue_dispatching_remaining_candidates(state, remaining_candidates)
        Error(#(state, reason, message)) ->
          park_dispatch_recovery_rejection(
            state,
            remaining_candidates,
            task_ref,
            issue,
            reason,
            message,
          )
      }
    }
    Error(#(reason, message)) -> {
      let remaining_candidates = pending_remaining_candidates(state, task_ref)
      park_dispatch_recovery_rejection(
        clear_pending_claim_for_task_ref(state, task_ref),
        remaining_candidates,
        task_ref,
        issue,
        reason,
        message,
      )
    }
  }
}

fn complete_dispatch_publication_recovery(
  state: State,
  issue: tracker_issue.Issue,
  run_id: String,
  attempt_count: Int,
  workflow_id: String,
) -> Result(State, #(State, String, String)) {
  let state = case attempt_count {
    0 -> state
    _ ->
      post_dispatch_publication_recovery_comment(
        state,
        issue,
        run_id,
        attempt_count,
      )
  }
  case
    publication_recovery_completion_target(
      state.workflow.effective.handoff,
      workflow_id,
    )
  {
    Error(message) ->
      Error(#(state, "publication_retry_completion_target_missing", message))
    Ok(#(target_state_id, target_state_name)) ->
      case
        issue_state.equals_normalized(
          issue.state,
          issue_state.from_string_unchecked(target_state_name),
        )
      {
        True -> Ok(state)
        False ->
          transition_issue_state(
            state,
            issue,
            target_state_id,
            target_state_name,
            case attempt_count {
              0 -> "dispatch_recovery:publication_already_published"
              _ -> "dispatch_recovery:publication_retry_recorded"
            },
          )
      }
  }
}

fn post_dispatch_publication_recovery_comment(
  state: State,
  issue: tracker_issue.Issue,
  run_id: String,
  attempt_count: Int,
) -> State {
  case state.tracker_adapter.comments {
    None -> state
    Some(comments) -> {
      let body =
        "Scherzo retried retained publication output for run "
        <> run_id
        <> " without rerunning the workflow and recorded "
        <> int.to_string(attempt_count)
        <> " publication attempt(s)."
      case
        comments.post_or_update(adapter.CommentRequest(
          task: task.from_legacy_issue(issue).ref,
          body: body,
          mode: adapter.CreateOnly,
        ))
      {
        Ok(_) -> state
        Error(error) -> {
          log_state(
            state,
            "warn",
            "dispatch_recovery_publication_comment_failed",
            [
              #("issue_id", issue.id),
              #("run_id", run_id),
              #("reason", adapter_error_message(error)),
            ],
          )
          state
        }
      }
    }
  }
}

fn publication_recovery_completion_target(
  handoff: config_types.HandoffConfig,
  workflow_id: String,
) -> Result(#(Option(String), String), String) {
  let missing =
    "publication retry completed but no success or completion state is configured"
  case handoff.completion_states {
    Some(policy) ->
      policy
      |> workflow_completion_policy.choose_linear_completion_state(
        workflow_id,
        workflow_completion_policy.WorkflowCompletionOutcome(
          workflow_completion_policy.CompletionSucceeded,
          [],
          workflow_completion_policy.ReviewUnknown,
          None,
          False,
        ),
      )
      |> publication_recovery_decision_target
    None ->
      handoff.success_state_id
      |> option.to_result(missing)
      |> result.map(linear_state_target)
  }
}

fn publication_recovery_decision_target(
  decision: workflow_completion_policy.CompletionStateDecision,
) -> Result(#(Option(String), String), String) {
  case decision {
    workflow_completion_policy.MoveToState(state, _) ->
      Ok(linear_state_target(state))
    workflow_completion_policy.LeaveLinearState(reason) ->
      Error("publication retry completed but " <> reason)
  }
}

fn linear_state_target(state_ref: LinearStateRef) -> #(Option(String), String) {
  case state_ref {
    workflow_completion_policy.StateById(value) -> #(Some(value), value)
    workflow_completion_policy.StateByName(value) -> #(None, value)
  }
}

fn transition_issue_state(
  state: State,
  issue: tracker_issue.Issue,
  target_state_id: Option(String),
  target_state_name: String,
  reason: String,
) -> Result(State, #(State, String, String)) {
  case state.tracker_adapter.state_transitions {
    None ->
      Error(#(
        state,
        "dispatch_recovery_state_transition_unsupported",
        "tracker adapter does not support state transitions",
      ))
    Some(state_transitions) ->
      case
        state_transitions.transition(adapter.StateTransitionRequest(
          task: task.from_legacy_issue(issue).ref,
          target_state_id: target_state_id,
          target_state_name: target_state_name,
          reason: reason,
        ))
      {
        Ok(_) -> Ok(state)
        Error(error) ->
          Error(#(
            state,
            "dispatch_recovery_state_transition_failed",
            adapter_error_message(error),
          ))
      }
  }
}

fn park_dispatch_recovery_rejection(
  state: State,
  remaining_candidates: List(tracker_issue.Issue),
  task_ref: task.TaskRef,
  issue: tracker_issue.Issue,
  reason: String,
  message: String,
) -> State {
  let state =
    attempt_dispatch_recovery_rejection_transition(state, issue, reason)
  let parked =
    orchestrator_state.ParkedEntry(
      task_ref: task_ref,
      issue_id: issue.id,
      identifier: issue.identifier,
      reason: orchestrator_reason.ParkOperator(reason),
      release_policy: orchestrator_state.AutoUnparkOnIssueChange(
        core.issue_fingerprint(issue),
      ),
      parked_at_ms: state.dependencies.now_ms(),
    )
  let identity = orchestrator_state.task_ref_identity(task_ref)
  let runtime =
    orchestrator_state.mark_task_parked(state.runtime, identity, parked)
  let state = State(..state, runtime: runtime)
  let state = transition_park_issue(state, parked, None)
  let state = case string.trim(message) == "" {
    True -> state
    False -> {
      log_state(state, "warn", "dispatch_recovery_rejected", [
        #("issue_id", issue.id),
        #("reason", reason),
        #("message", message),
      ])
      state
    }
  }
  continue_dispatching_remaining_candidates(state, remaining_candidates)
}

fn continue_dispatching_remaining_candidates(
  state: State,
  remaining_candidates: List(tracker_issue.Issue),
) -> State {
  case remaining_candidates {
    [] -> state
    _ -> {
      process.send(
        state.subject,
        DispatchRecoveryContinue(remaining_candidates),
      )
      state
    }
  }
}

fn attempt_dispatch_recovery_rejection_transition(
  state: State,
  issue: tracker_issue.Issue,
  reason: String,
) -> State {
  let #(target_state_id, target_state_name) = case
    state.workflow.effective.handoff.failure_state_id
  {
    Some(state_ref) -> linear_state_target(state_ref)
    None -> #(None, "Triage")
  }
  case
    transition_issue_state(
      state,
      issue,
      target_state_id,
      target_state_name,
      "dispatch_recovery:" <> reason,
    )
  {
    Ok(state) -> {
      log_state(state, "info", "dispatch_recovery_rejected_state_transition", [
        #("issue_id", issue.id),
        #("target_state", target_state_name),
      ])
      state
    }
    Error(#(state, transition_reason, transition_message)) -> {
      log_state(
        state,
        "warn",
        "dispatch_recovery_rejected_state_transition_failed",
        [
          #("issue_id", issue.id),
          #("reason", transition_reason),
          #("message", transition_message),
        ],
      )
      state
    }
  }
}

fn pending_remaining_candidates(
  state: State,
  task_ref: task.TaskRef,
) -> List(tracker_issue.Issue) {
  case pending_claim_for_task_ref(state, task_ref) {
    Ok(pending) -> pending.remaining_candidates
    Error(Nil) -> []
  }
}

fn option_with_default(value: Option(String), fallback: String) -> String {
  case value {
    Some(text) -> text
    None -> fallback
  }
}

fn adapter_error_message(err: adapter.TrackerError) -> String {
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

fn transition_append_ledger(
  state: State,
  request: transition_effects.LedgerAppend,
) -> #(State, Result(Nil, ledger.LedgerError)) {
  let bodies = ledger_batch.to_bodies(request.batch)
  let #(state, result) =
    append_ledger_records(
      state,
      ledger_records_for_bodies(state.dependencies.now_ms(), bodies),
      request.failure_event,
    )
  case result != Ok(Nil), request.policy {
    True, transition_effects.StopBatchOnFailure -> #(
      State(..state, transition_invariant_violation_pending: True),
      result,
    )
    _, _ -> #(state, result)
  }
}

fn transition_start_worker(
  state: State,
  request: transition_effects.WorkerStart,
) -> #(State, Result(Nil, String)) {
  let run_id = identity.run_id_to_string(request.run_id)
  let session_id = identity.session_id_to_string(request.session_id)
  case
    worker_lifecycle.workflow_snapshot_for_start(
      request.workflow_snapshot,
      state.workflow.bundle,
      request.issue,
      request.workflow_id,
      run_id,
    )
  {
    Error(error) -> #(state, Error(worker_lifecycle.snapshot_reason(error)))
    Ok(snapshot) -> #(
      worker_lifecycle.spawn_worker(
        worker_spawn_context(state, request.issue, run_id, session_id, snapshot),
        request.task_ref,
        request.issue,
        request.workspace_path,
        run_id,
        session_id,
        request.recovery,
      ),
      Ok(Nil),
    )
  }
}

fn transition_worker_start_failed(
  state: State,
  request: transition_effects.WorkerStart,
  reason: String,
) -> State {
  let issue_id = identity.issue_id_to_string(request.issue_id)
  let run_id = identity.run_id_to_string(request.run_id)
  log_state(state, "warn", "worker_start_failed", [
    #("issue_id", issue_id),
    #("run_id", run_id),
    #("reason", reason),
  ])
  State(
    ..state,
    registry: worker_registry.forget_task_ref_session(
      state.registry,
      request.task_ref,
    ),
    recovery_by_issue: dict.delete(state.recovery_by_issue, issue_id),
  )
}

fn transition_remove_worker(
  state: State,
  identity: transition_effects.WorkerIdentity,
  demonitor: Bool,
) -> State {
  let run_id = identity.run_id_to_string(identity.run_id)
  case
    worker_registry.resolve_worker_run(
      state.registry,
      identity.task_ref,
      run_id,
    )
  {
    worker_registry.WorkerMissing ->
      State(
        ..state,
        registry: worker_registry.forget_task_ref_session(
          state.registry,
          identity.task_ref,
        ),
      )
    worker_registry.WorkerStale(handle) -> {
      log_state(state, "warn", "worker_remove_stale", [
        #("issue_id", handle.issue_id),
        #("expected_run_id", handle.run_id),
        #("remove_run_id", run_id),
      ])
      state
    }
    worker_registry.WorkerCurrent(handle) -> {
      case demonitor {
        True -> process.demonitor_process(handle.monitor)
        False -> Nil
      }
      State(
        ..state,
        registry: worker_registry.remove_worker_handle(state.registry, handle),
      )
    }
  }
}

fn transition_publish_worker_exited(
  state: State,
  request: transition_effects.WorkerExitPublication,
) -> State {
  let session_id = identity.session_id_to_string(request.identity.session_id)
  case
    request.update_tokens && event_publisher.tokens_are_nonzero(request.tokens)
  {
    True -> hub.update_tokens(state.event_hub, session_id, request.tokens)
    False -> Nil
  }
  event_publisher.lifecycle(
    state.event_hub,
    session_id,
    session_event.WorkerExited,
    Some(request.reason_text),
  )
  hub.finish_session(state.event_hub, session_id, request.exit_reason)
  state
}

fn transition_report_worker_success(
  state: State,
  identity: transition_effects.WorkerIdentity,
  success: agent_types.WorkerSuccess,
) -> State {
  let final_issue = case success.final_issue {
    Some(issue) -> issue
    None -> identity.issue
  }
  let run_id = identity.run_id_to_string(identity.run_id)
  let intent =
    outbox_effects.success_intent(
      identity.task_ref,
      final_issue,
      success,
      run_id,
      identity.workflow_id,
      state.workflow.effective.handoff,
      tracker_secrets(state),
    )
  enqueue_outbox_side_effect(state, intent, fn(intent) {
    effect_runner.ReportSuccess(
      outbox: intent,
      task_ref: identity.task_ref,
      issue_id: identity.issue_id_to_string(identity.issue_id),
      issue: final_issue,
      success: success,
      run_id: run_id,
      workflow_id: identity.workflow_id,
      capability: require_handoff_capability(state),
    )
  })
}

fn transition_report_worker_failure(
  state: State,
  identity: transition_effects.WorkerIdentity,
  failure: agent_types.WorkerFailure,
) -> State {
  let run_id = identity.run_id_to_string(identity.run_id)
  let intent =
    outbox_effects.failure_intent(
      identity.task_ref,
      identity.issue,
      failure,
      run_id,
      identity.workflow_id,
      state.workflow.effective.handoff,
      tracker_secrets(state),
    )
  enqueue_outbox_side_effect(state, intent, fn(intent) {
    effect_runner.ReportFailure(
      outbox: intent,
      task_ref: identity.task_ref,
      issue_id: identity.issue_id_to_string(identity.issue_id),
      issue: identity.issue,
      failure: failure,
      run_id: run_id,
      workflow_id: identity.workflow_id,
      capability: require_handoff_capability(state),
    )
  })
}

fn transition_cleanup_workspace(state: State, workspace_path: String) -> State {
  case string.trim(workspace_path) == "" {
    True -> state
    False ->
      enqueue_side_effect(
        state,
        effect_runner.CleanupWorkspace(
          root: state.workflow.effective.workspace.root,
          workspace_path: workspace_path,
          hooks: state.workflow.effective.hooks,
          cleanup: state.dependencies.cleanup,
        ),
      )
  }
}

fn transition_report_park(state: State, report: adapter.ParkReport) -> State {
  let intent = outbox_effects.park_report_intent(report, tracker_secrets(state))
  enqueue_outbox_side_effect(state, intent, fn(intent) {
    effect_runner.ReportPark(intent, report, require_handoff_capability(state))
  })
}

fn transition_park_issue(
  state: State,
  parked: orchestrator_state.ParkedEntry,
  source_run_id: Option(String),
) -> State {
  let reason_text = orchestrator_reason.park_to_string(parked.reason)
  log_state(state, "warn", "issue_parked", [
    #("issue_id", parked.issue_id),
    #("reason", reason_text),
  ])
  let #(state, appended) = append_parked_record(state, parked, reason_text)
  case appended {
    False -> State(..state, transition_invariant_violation_pending: True)
    True ->
      enqueue_parked_entry_report(state, parked, reason_text, source_run_id)
  }
}

fn append_parked_record(
  state: State,
  parked: orchestrator_state.ParkedEntry,
  reason_text: String,
) -> #(State, Bool) {
  let #(release_policy, issue_fingerprint) = case parked.release_policy {
    orchestrator_state.ExplicitUnparkOnly -> #("explicit_unpark_only", "")
    orchestrator_state.AutoUnparkOnIssueChange(fingerprint) -> #(
      park_release_policy_to_string(parked.release_policy),
      fingerprint,
    )
  }
  append_ledger_bodies(
    state,
    [
      record.IssueParkedV2(
        parked.issue_id,
        parked.identifier,
        reason_text,
        release_policy,
        issue_fingerprint,
        state.dependencies.now_ms(),
      ),
    ],
    "ledger_append_failed",
  )
}

fn enqueue_parked_entry_report(
  state: State,
  parked: orchestrator_state.ParkedEntry,
  reason_text: String,
  source_run_id: Option(String),
) -> State {
  let report =
    adapter.ParkReport(
      task: parked.task_ref,
      issue_identifier: parked.identifier,
      reason: reason_text,
      release_policy: Some(park_release_policy_to_string(parked.release_policy)),
      run_id: source_run_id,
    )
  transition_report_park(state, report)
}

fn transition_stop_worker(
  state: State,
  identity: transition_effects.WorkerIdentity,
  reason: session_reason.WorkerExitReason,
) -> State {
  let reason_text = session_reason.to_string(reason)
  let run_id = identity.run_id_to_string(identity.run_id)
  case
    worker_registry.resolve_worker_run(
      state.registry,
      identity.task_ref,
      run_id,
    )
  {
    worker_registry.WorkerMissing -> state
    worker_registry.WorkerStale(handle) -> {
      log_state(state, "warn", "worker_stop_stale", [
        #("issue_id", handle.issue_id),
        #("expected_run_id", handle.run_id),
        #("stop_run_id", run_id),
      ])
      state
    }
    worker_registry.WorkerCurrent(handle) -> {
      hub.update_status(
        state.event_hub,
        handle.session_id,
        session_event.Stopping,
      )
      event_publisher.lifecycle(
        state.event_hub,
        handle.session_id,
        session_event.OperatorCommand,
        Some(reason_text),
      )
      event_publisher.lifecycle(
        state.event_hub,
        handle.session_id,
        session_event.WorkerExited,
        Some(reason_text),
      )
      hub.finish_session(state.event_hub, handle.session_id, reason)
      process.demonitor_process(handle.monitor)
      kill_worker(handle)
      State(
        ..state,
        registry: worker_registry.remove_worker_handle(state.registry, handle),
      )
    }
  }
}

fn transition_stop_worker_after_issue_refresh(
  state: State,
  identity: transition_effects.WorkerIdentity,
  reason: orchestrator_reason.StopReason,
) -> State {
  let reason_text = orchestrator_reason.stop_to_string(reason)
  let run_id = identity.run_id_to_string(identity.run_id)
  case
    worker_registry.resolve_worker_run(
      state.registry,
      identity.task_ref,
      run_id,
    )
  {
    worker_registry.WorkerMissing -> state
    worker_registry.WorkerStale(handle) -> {
      log_state(state, "warn", "worker_stop_stale", [
        #("issue_id", handle.issue_id),
        #("expected_run_id", handle.run_id),
        #("stop_run_id", run_id),
      ])
      state
    }
    worker_registry.WorkerCurrent(handle) -> {
      let state =
        cleanup_orphaned_yaml_children_after_parent_stop(
          state,
          run_id,
          reason_text,
          Some(issue_state.to_string(identity.issue.state)),
        )
      let state = remove_yaml_step_tokens_for_run(state, run_id)
      hub.update_status(
        state.event_hub,
        handle.session_id,
        session_event.Stopping,
      )
      event_publisher.lifecycle(
        state.event_hub,
        handle.session_id,
        session_event.StopRequested,
        Some(reason_text),
      )
      process.demonitor_process(handle.monitor)
      kill_worker(handle)
      event_publisher.lifecycle(
        state.event_hub,
        handle.session_id,
        session_event.WorkerExited,
        Some(session_reason.to_string(session_reason.Stopped)),
      )
      hub.finish_session(
        state.event_hub,
        handle.session_id,
        session_reason.Stopped,
      )
      log_state(state, "warn", "worker_stop_requested", [
        #("issue_id", identity.issue_id_to_string(identity.issue_id)),
        #("reason", reason_text),
      ])
      State(
        ..state,
        registry: worker_registry.remove_worker_handle(state.registry, handle),
      )
    }
  }
}

fn transition_register_yaml_step_started(
  state: State,
  session_id: identity.SessionId,
  run_id: identity.RunId,
) -> State {
  State(
    ..state,
    registry: worker_registry.register_yaml_step_started(
      state.registry,
      identity.session_id_to_string(session_id),
      identity.run_id_to_string(run_id),
    ),
  )
}

fn transition_finish_yaml_step_route(
  state: State,
  session_id: identity.SessionId,
) -> State {
  State(
    ..state,
    registry: worker_registry.finish_yaml_step(
      state.registry,
      identity.session_id_to_string(session_id),
    ),
  )
}

fn transition_finish_yaml_step_session(
  state: State,
  session_id: identity.SessionId,
  reason: session_reason.WorkerExitReason,
) -> State {
  let session_id_text = identity.session_id_to_string(session_id)
  let reason_text = session_reason.to_string(reason)
  hub.update_status(state.event_hub, session_id_text, session_event.Stopping)
  event_publisher.lifecycle(
    state.event_hub,
    session_id_text,
    session_event.OperatorCommand,
    Some(reason_text),
  )
  event_publisher.lifecycle(
    state.event_hub,
    session_id_text,
    session_event.WorkerExited,
    Some(reason_text),
  )
  hub.finish_session(state.event_hub, session_id_text, reason)
  state
}

fn transition_finish_yaml_step_sessions_for_run(
  state: State,
  run_id: identity.RunId,
  reason: session_reason.WorkerExitReason,
) -> State {
  let run_id = identity.run_id_to_string(run_id)
  finish_yaml_step_sessions_for_run(state, run_id, reason)
  |> remove_yaml_step_tokens_for_run(run_id)
}

fn transition_clear_yaml_step_routes_for_run(
  state: State,
  run_id: identity.RunId,
) -> State {
  clear_yaml_step_command_routes_for_run(
    state,
    identity.run_id_to_string(run_id),
  )
}

fn transition_mark_yaml_run_stopping(
  state: State,
  run_id: identity.RunId,
  reason: session_reason.WorkerExitReason,
) -> State {
  State(
    ..state,
    registry: worker_registry.mark_yaml_run_stopping(
      state.registry,
      identity.run_id_to_string(run_id),
      reason,
    ),
  )
}

fn transition_reserve_session_sequence(state: State, sequence: Int) -> State {
  let #(registry, reserved) =
    worker_registry.reserve_session_sequence(state.registry)
  let state = State(..state, registry: registry)
  case reserved == sequence {
    True -> state
    False -> {
      log_state(state, "warn", "session_sequence_mismatch", [
        #("expected", int.to_string(sequence)),
        #("reserved", int.to_string(reserved)),
      ])
      state
    }
  }
}

fn transition_defer_retry_timer(
  state: State,
  issue_id: String,
  generation: Int,
  delay_ms: Int,
) -> State {
  let timer =
    state.dependencies.send_after(
      state.subject,
      delay_ms,
      RetryTick(issue_id, generation),
    )
  State(
    ..state,
    retry: retry_scheduler.schedule_timer(
      state.retry,
      issue_id,
      timer,
      state.dependencies.cancel_timer,
    ),
  )
}

fn transition_begin_retry_refresh(
  state: State,
  issue_id: String,
  generation: Int,
) -> State {
  case retry_scheduler.begin_refresh(state.retry, issue_id, generation) {
    Error(Nil) -> {
      log_state(state, "info", "retry_timer_stale", [#("issue_id", issue_id)])
      state
    }
    Ok(retry) ->
      enqueue_side_effect(
        State(..state, retry: retry),
        effect_runner.RefreshRetry(
          issue_id: issue_id,
          generation: generation,
          tracker_adapter: state.tracker_adapter,
        ),
      )
  }
}

fn transition_schedule_retry_timer(
  state: State,
  issue_id: String,
  delay_ms: Int,
  generation: Int,
  reason: orchestrator_reason.RetryReason,
) -> State {
  let reason_text = orchestrator_reason.retry_to_string(reason)
  case worker_registry.issue_session(state.registry, issue_id) {
    Ok(session_id) ->
      event_publisher.lifecycle(
        state.event_hub,
        session_id,
        session_event.RetryScheduled,
        Some(reason_text),
      )
    Error(Nil) -> Nil
  }
  log_state(state, "info", "retry_scheduled", [
    #("issue_id", issue_id),
    #("delay_ms", int.to_string(delay_ms)),
    #("generation", int.to_string(generation)),
    #("reason", reason_text),
  ])
  let timer =
    state.dependencies.send_after(
      state.subject,
      delay_ms,
      RetryTick(issue_id, generation),
    )
  State(
    ..state,
    retry: retry_scheduler.schedule_timer(
      state.retry,
      issue_id,
      timer,
      state.dependencies.cancel_timer,
    ),
  )
}

fn transition_schedule_recovered_retry_timer(
  state: State,
  issue_id: String,
  delay_ms: Int,
  generation: Int,
) -> State {
  let timer =
    state.dependencies.send_after(
      state.subject,
      delay_ms,
      RetryTick(issue_id, generation),
    )
  State(
    ..state,
    retry: retry_scheduler.schedule_timer(
      state.retry,
      issue_id,
      timer,
      state.dependencies.cancel_timer,
    ),
  )
}

fn schedule_next_poll(state: State) -> State {
  let #(poll, delay) =
    poll_scheduler.schedule_next_jittered_message(
      state.poll,
      state.workflow.effective.polling.interval_ms,
      state.workflow.effective.workspace.root,
      state.subject,
      PollTick,
      state.dependencies.send_after,
      state.dependencies.cancel_timer,
    )
  let state = State(..state, poll: poll)
  let fields = poll_scheduler.jitter_log_fields(delay)
  log_state(state, "info", "next_poll_scheduled", fields)
  state
}

fn evaluate_scheduled_jobs(state: State) -> State {
  case config.can_dispatch(state.workflow.reload_state) {
    False -> state
    True -> {
      let now_ms = state.dependencies.now_ms()
      state.workflow.bundle.orchestrator.scheduled_jobs
      |> list.fold(state, fn(state, job) {
        case job.enabled {
          False -> state
          True -> evaluate_scheduled_job(state, job, now_ms)
        }
      })
      |> start_pending_scheduled_runs
    }
  }
}

fn evaluate_scheduled_job(
  state: State,
  job: config_types.ScheduledJobConfig,
  now_ms: Int,
) -> State {
  let #(runtime, actions) =
    scheduled_runtime.admit_due(
      state.scheduled_runtime,
      job.id,
      job.workflow,
      job.every_ms,
      now_ms,
      scheduled_worker_active_for_job(state, job.id),
    )
  let state = State(..state, scheduled_runtime: runtime)
  apply_scheduled_runtime_actions(state, actions, append_retry_record: True)
}

fn scheduled_worker_active_for_job(state: State, job_id: String) -> Bool {
  state.registry
  |> worker_registry.scheduled_worker_handles
  |> list.any(fn(handle) { handle.job_id == job_id })
}

fn apply_scheduled_runtime_actions(
  state: State,
  actions: List(scheduled_runtime.Action),
  append_retry_record append_retry_record: Bool,
) -> State {
  list.fold(actions, state, fn(state, action) {
    apply_scheduled_runtime_action(
      state,
      action,
      append_retry_record: append_retry_record,
    )
  })
}

fn apply_scheduled_runtime_action(
  state: State,
  action: scheduled_runtime.Action,
  append_retry_record append_retry_record: Bool,
) -> State {
  case action {
    scheduled_runtime.RecordScheduledDue(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      trigger,
    ) ->
      append_ledger_bodies_best_effort(
        state,
        [
          record.ScheduledJobDue(
            job_id,
            workflow_id,
            due_at_ms,
            run_id,
            trigger,
          ),
        ],
        "scheduled_due_append_failed",
      )
    scheduled_runtime.RecordScheduledPending(pending) ->
      append_ledger_bodies_best_effort(
        state,
        [
          record.ScheduledRunPending(
            pending.job_id,
            pending.workflow_id,
            pending.due_at_ms,
            pending.run_id,
            pending.trigger,
            pending.requested_at_ms,
          ),
        ],
        "scheduled_pending_append_failed",
      )
    scheduled_runtime.RecordScheduledSkipped(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      reason,
      skipped_count,
    ) ->
      append_ledger_bodies_best_effort(
        state,
        [
          record.ScheduledJobSkipped(
            job_id,
            workflow_id,
            due_at_ms,
            run_id,
            reason,
            skipped_count,
          ),
        ],
        "scheduled_skip_append_failed",
      )
    scheduled_runtime.RecordScheduledPendingBlocked(pending, blocked_at_ms) ->
      append_ledger_bodies_best_effort(
        state,
        [
          record.ScheduledRunPendingBlocked(
            pending.job_id,
            pending.workflow_id,
            pending.due_at_ms,
            pending.run_id,
            pending.blocking_reason,
            blocked_at_ms,
          ),
        ],
        "scheduled_pending_blocked_append_failed",
      )
    scheduled_runtime.UpdateNextDue(_, _) -> state
    scheduled_runtime.ScheduleRetryTimer(run_id, generation, delay_ms) ->
      schedule_scheduled_retry_timer(state, run_id, generation, delay_ms)
    scheduled_runtime.ScheduleReportRetryTimer(run_id, generation, delay_ms) ->
      schedule_scheduled_report_retry_timer(state, run_id, generation, delay_ms)
    scheduled_runtime.RecordScheduledRetry(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      next_attempt,
      delay_ms,
      generation,
      reason,
    ) -> {
      case append_retry_record {
        True ->
          append_ledger_bodies_best_effort(
            state,
            [
              record.ScheduledRunRetryScheduled(
                job_id,
                workflow_id,
                due_at_ms,
                run_id,
                next_attempt,
                delay_ms,
                generation,
                reason,
              ),
            ],
            "scheduled_retry_append_failed",
          )
        False -> state
      }
    }
    scheduled_runtime.PromoteRetryToPending(_) -> state
    scheduled_runtime.RetryReport(job_id, run_id, report_attempt_index) ->
      retry_scheduled_failure_report_by_identity(
        state,
        job_id,
        run_id,
        report_attempt_index,
      )
  }
}

fn schedule_scheduled_retry_timer(
  state: State,
  run_id: String,
  generation: Int,
  delay_ms: Int,
) -> State {
  let timer =
    state.dependencies.send_after(
      state.subject,
      delay_ms,
      ScheduledRetryTick(run_id, generation),
    )
  State(
    ..state,
    scheduled_retry_timers: scheduled_runtime.insert_timer_cancelling_existing(
      state.scheduled_retry_timers,
      run_id,
      timer,
      state.dependencies.cancel_timer,
    ),
  )
}

fn schedule_scheduled_report_retry_timer(
  state: State,
  run_id: String,
  generation: Int,
  delay_ms: Int,
) -> State {
  let timer =
    state.dependencies.send_after(
      state.subject,
      delay_ms,
      ScheduledReportRetryTick(run_id, generation),
    )
  State(
    ..state,
    scheduled_report_retry_timers: scheduled_runtime.insert_timer_cancelling_existing(
      state.scheduled_report_retry_timers,
      run_id,
      timer,
      state.dependencies.cancel_timer,
    ),
  )
}

fn start_pending_scheduled_runs(state: State) -> State {
  state.scheduled_runtime
  |> scheduled_runtime.pending_starts
  |> list.fold(state, fn(state, pending) {
    start_pending_scheduled_run(state, pending)
  })
}

fn start_pending_scheduled_run(
  state: State,
  pending: scheduled_runtime.PendingStart,
) -> State {
  case state.operator_paused {
    True -> block_pending_scheduled_run(state, pending, "paused")
    False ->
      case scheduled_slot_available_for_start(state) {
        False ->
          block_pending_scheduled_run(state, pending, "waiting_for_global_slot")
        True ->
          case
            runtime_bundle.workflow_by_id(
              state.workflow.bundle,
              pending.workflow_id,
            )
          {
            Error(runtime_bundle.BundleError(_, _)) -> {
              let state =
                append_ledger_bodies_best_effort(
                  state,
                  [
                    record.ScheduledRunPendingCancelled(
                      pending.job_id,
                      pending.workflow_id,
                      pending.due_at_ms,
                      pending.run_id,
                      "workflow_missing",
                      state.dependencies.now_ms(),
                    ),
                  ],
                  "scheduled_pending_cancel_append_failed",
                )
              State(
                ..state,
                scheduled_runtime: scheduled_runtime.remove_pending_start(
                  state.scheduled_runtime,
                  pending.job_id,
                ),
              )
            }
            Ok(#(_, dag)) ->
              case
                workspace_run.scheduled_run_root_for(
                  pending.job_id,
                  pending.workflow_id,
                  pending.run_id,
                  state.workflow.bundle.orchestrator,
                )
              {
                Error(err) -> {
                  let state =
                    append_ledger_bodies_best_effort(
                      state,
                      [
                        record.ScheduledRunFailed(
                          pending.job_id,
                          pending.workflow_id,
                          pending.due_at_ms,
                          pending.run_id,
                          pending.attempt,
                          state.dependencies.now_ms(),
                          "workspace_failed:" <> error.workspace_code(err),
                          True,
                          None,
                        ),
                      ],
                      "scheduled_start_failed_append_failed",
                    )
                  State(
                    ..state,
                    scheduled_runtime: scheduled_runtime.remove_pending_start(
                      state.scheduled_runtime,
                      pending.job_id,
                    ),
                  )
                }
                Ok(run_root) ->
                  worker_lifecycle.spawn_scheduled_worker(
                    scheduled_worker_spawn_context(state, pending, dag),
                    pending,
                    run_root,
                  )
              }
          }
      }
  }
}

fn block_pending_scheduled_run(
  state: State,
  pending: scheduled_runtime.PendingStart,
  reason: String,
) -> State {
  let #(runtime, actions) =
    scheduled_runtime.block_pending_start(
      state.scheduled_runtime,
      pending.job_id,
      reason,
      state.dependencies.now_ms(),
    )
  let state = State(..state, scheduled_runtime: runtime)
  apply_scheduled_runtime_actions(state, actions, append_retry_record: True)
}

fn scheduled_slot_available_for_start(state: State) -> Bool {
  active_run_count(state)
  + dict.size(state.pending_claims)
  + dict.size(state.pending_dispatch_validations)
  + dict.size(state.pending_review_lane_preflights)
  + list.length(worker_registry.scheduled_worker_handles(state.registry))
  + pending_issue_retry_headroom(state)
  < state.workflow.effective.agent.max_concurrent_agents
}

fn pending_issue_retry_headroom(state: State) -> Int {
  case has_pending_issue_retry(state) {
    True -> 1
    False -> 0
  }
}

fn has_pending_issue_retry(state: State) -> Bool {
  state.runtime.retry_attempts
  |> dict.values
  |> list.any(fn(retry) {
    !list.contains(active_run_issue_ids(state), retry.issue_id)
  })
}

fn handle_retry_refresh_finished(
  state: State,
  issue_id: String,
  generation: Int,
  result: Result(List(tracker_issue.Issue), error.TrackerError),
) -> State {
  case result {
    Ok(issues) ->
      emit_work_item_invalidation_for_issues(
        state,
        work_item_invalidation.TrackerRefresh,
        issues,
      )
    Error(_tracker_error) -> Nil
  }
  let result = case result {
    Ok(issues) -> Ok(issues)
    Error(err) -> Error(error.tracker_code(err))
  }
  run_transition_messages(state, [
    transition_types.RetryRefreshCompleted(
      issue_id,
      generation,
      result,
      transition_dispatch_context(state),
    ),
  ])
}

fn workflow_run_started_body_for_claim(
  pending: transition_types.PendingClaim,
) -> record.RecordBody {
  let snapshot = pending.workflow_snapshot
  record.WorkflowRunStartedWithTask(
    pending.run_id,
    workflow_dag.id(snapshot.dag),
    snapshot.fingerprint,
    pending.issue.id,
    pending.issue.identifier,
    record.TaskRefFields(
      pending.task_ref.backend_kind,
      pending.task_ref.remote_id,
      pending.task_ref.key,
      pending.task_ref.url,
    ),
    core.issue_fingerprint(pending.issue),
    observed_updated_at_ms(pending.issue),
    snapshot.run_root,
  )
}

fn publish_recovery_lifecycle(
  event_hub: process.Subject(hub.Message),
  session_id: String,
  recovery: Option(session_event.RecoveryInfo),
) -> Nil {
  case recovery {
    None -> Nil
    Some(info) ->
      event_publisher.lifecycle_with_recovery(
        event_hub,
        session_id,
        lifecycle_name_for_recovery(info.status),
        info.message,
        Some(info),
      )
  }
}

fn publish_post_success_cleanup_warning(
  event_hub: process.Subject(hub.Message),
  session_id: String,
  warning: Option(workflow_run.PostSuccessCleanupWarning),
) -> Nil {
  case warning {
    None -> Nil
    Some(workflow_run.PostSuccessCleanupWarning(message: message, ..)) -> {
      let recovery =
        session_recovery.cleanup_metadata(
          "workflow.post_success_cleanup",
          message,
          session_event.Retained,
          None,
          None,
        )
      hub.update_recovery(event_hub, session_id, Some(recovery))
      event_publisher.lifecycle_with_recovery(
        event_hub,
        session_id,
        session_event.RecoveryCleanup,
        Some(message),
        Some(recovery),
      )
    }
  }
}

fn lifecycle_name_for_recovery(
  status: session_event.RecoveryStatus,
) -> session_event.LifecycleEventName {
  case status {
    session_event.Interrupted -> session_event.RecoveryInterrupted
    session_event.Parked -> session_event.RecoveryParked
    session_event.Cleanup -> session_event.RecoveryCleanup
    session_event.OldStateResetRequired ->
      session_event.OldStateResetRequiredEvent
    session_event.Recovered
    | session_event.Resumed
    | session_event.InspectionNeeded
    | session_event.Blocked
    | session_event.DriftDetected -> session_event.RecoveryDetected
  }
}

fn worker_spawn_context(
  state: State,
  issue: tracker_issue.Issue,
  run_id: String,
  session_id: String,
  snapshot: worker_lifecycle.WorkflowSnapshot,
) -> worker_lifecycle.WorkerSpawnContext(State) {
  let subject = state.subject
  let dependencies = state.dependencies
  let tracker_client = state.tracker_client
  let secrets = state.workflow.secrets
  let event_hub = state.event_hub
  worker_lifecycle.WorkerSpawnContext(
    state: state,
    now_ms: state.dependencies.now_ms,
    register_session: fn(
      session_id,
      issue,
      workspace_path,
      recovery,
      started_at_ms,
    ) {
      hub.register_session(
        state.event_hub,
        session_event.SessionSummary(
          session_id: session_id,
          display_name: session_name.generate(issue.identifier, session_id),
          issue_id: issue.id,
          issue_identifier: issue.identifier,
          issue_title: issue.title,
          workspace_path: workspace_path,
          pi_session_id: None,
          status: session_event.Preparing,
          recovery: recovery,
          current_turn: 0,
          current_turn_status: None,
          current_turn_started_at_ms: None,
          last_turn_finished_at_ms: None,
          last_turn_duration_ms: None,
          last_turn_token_delta: session_tokens.zero_token_totals(),
          last_turn_reason: None,
          started_at_ms: started_at_ms,
          last_event_at_ms: started_at_ms,
          token_totals: session_tokens.zero_token_totals(),
        ),
      )
    },
    publish_recovery_lifecycle: fn(session_id, recovery) {
      publish_recovery_lifecycle(state.event_hub, session_id, recovery)
    },
    publish_dispatch_started: fn(session_id) {
      event_publisher.lifecycle(
        state.event_hub,
        session_id,
        session_event.DispatchStarted,
        None,
      )
    },
    log_dispatch_started: fn(issue, run_id, workspace_path) {
      log_state(state, "info", "dispatch_started", [
        #("issue_id", issue.id),
        #("issue_identifier", issue.identifier),
        #("run_id", run_id),
        #("workspace_path", workspace_path),
      ])
    },
    apply_task_ref_start: fn(state, ref, issue, workspace_path) {
      State(
        ..state,
        runtime: core.apply_task_ref_start(
          state.runtime,
          ref,
          issue,
          workspace_path,
        ),
      )
    },
    spawn: fn(_issue, _run_id, _session_id) {
      process.spawn_unlinked(fn() {
        let result =
          run_workflow_worker(
            issue,
            run_id,
            snapshot,
            tracker_client,
            secrets,
            dependencies.workflow_run_dependencies,
            subject,
            event_hub,
            session_id,
            dependencies.now_ms,
          )
        process.send(subject, WorkerFinished(issue.id, run_id, result))
      })
    },
    publish_worker_started: fn(session_id) {
      event_publisher.lifecycle(
        state.event_hub,
        session_id,
        session_event.WorkerStarted,
        None,
      )
    },
    update_running_status: fn(session_id) {
      hub.update_status(state.event_hub, session_id, session_event.Running)
    },
    register_worker: fn(state, handle) {
      State(
        ..state,
        registry: worker_registry.register_worker(state.registry, handle),
      )
    },
    clear_recovery: fn(state, issue_id) {
      State(
        ..state,
        recovery_by_issue: dict.delete(state.recovery_by_issue, issue_id),
      )
    },
  )
}

fn scheduled_worker_spawn_context(
  state: State,
  pending: scheduled_runtime.PendingStart,
  dag: workflow_dag.WorkflowDag,
) -> worker_lifecycle.ScheduledWorkerSpawnContext(State) {
  let subject = state.subject
  let dependencies = state.dependencies
  let tracker_client = state.tracker_client
  let bundle = state.workflow.bundle
  let secrets = state.workflow.secrets
  let event_hub = state.event_hub
  worker_lifecycle.ScheduledWorkerSpawnContext(
    state: state,
    now_ms: state.dependencies.now_ms,
    reserve_session_sequence: fn(state) {
      let #(registry, _session_sequence) =
        worker_registry.reserve_session_sequence(state.registry)
      State(..state, registry: registry)
    },
    register_session: fn(session_id, display_ref, run_root, started_at_ms) {
      hub.register_session(
        state.event_hub,
        session_event.SessionSummary(
          session_id: session_id,
          display_name: session_name.generate(display_ref, session_id),
          issue_id: "",
          issue_identifier: display_ref,
          issue_title: "Scheduled job " <> pending.job_id,
          workspace_path: run_root,
          pi_session_id: None,
          status: session_event.Preparing,
          recovery: None,
          current_turn: 0,
          current_turn_status: None,
          current_turn_started_at_ms: None,
          last_turn_finished_at_ms: None,
          last_turn_duration_ms: None,
          last_turn_token_delta: session_tokens.zero_token_totals(),
          last_turn_reason: None,
          started_at_ms: started_at_ms,
          last_event_at_ms: started_at_ms,
          token_totals: session_tokens.zero_token_totals(),
        ),
      )
    },
    publish_dispatch_started: fn(session_id) {
      event_publisher.lifecycle(
        state.event_hub,
        session_id,
        session_event.DispatchStarted,
        Some("scheduled"),
      )
    },
    append_started_ledger: fn(
      state,
      pending,
      started_at_ms,
      session_id,
      run_root,
    ) {
      append_ledger_bodies_best_effort(
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
    },
    log_dispatch_started: fn(job_id, run_id, workflow_id) {
      log_state(state, "info", "scheduled_dispatch_started", [
        #("job_id", job_id),
        #("run_id", run_id),
        #("workflow_id", workflow_id),
      ])
    },
    spawn: fn(started_at_ms, session_id) {
      let scheduled =
        schedule_core.ScheduledRunContext(
          job_id: pending.job_id,
          workflow_id: pending.workflow_id,
          due_at_ms: pending.due_at_ms,
          started_at_ms: started_at_ms,
          run_id: pending.run_id,
          attempt: pending.attempt,
          trigger: pending.trigger,
        )
      process.spawn_unlinked(fn() {
        let result =
          run_scheduled_workflow_worker(
            scheduled,
            dag,
            bundle,
            tracker_client,
            secrets,
            dependencies.workflow_run_dependencies,
            subject,
            event_hub,
            session_id,
            dependencies.now_ms,
          )
        process.send(subject, ScheduledWorkerFinished(pending.run_id, result))
      })
    },
    publish_worker_started: fn(session_id) {
      event_publisher.lifecycle(
        state.event_hub,
        session_id,
        session_event.WorkerStarted,
        Some("scheduled"),
      )
    },
    update_running_status: fn(session_id) {
      hub.update_status(state.event_hub, session_id, session_event.Running)
    },
    register_scheduled_worker: fn(state, handle) {
      State(
        ..state,
        registry: worker_registry.register_scheduled_worker(
          state.registry,
          handle,
        ),
      )
    },
    remove_pending_start: fn(state, job_id) {
      State(
        ..state,
        scheduled_runtime: scheduled_runtime.remove_pending_start(
          state.scheduled_runtime,
          job_id,
        ),
      )
    },
  )
}

fn run_scheduled_workflow_worker(
  scheduled: schedule_core.ScheduledRunContext,
  dag: workflow_dag.WorkflowDag,
  bundle: runtime_bundle.RuntimeBundle,
  tracker_client: tracker.Client,
  secrets: List(String),
  workflow_dependencies: workflow_run.Dependencies,
  daemon_subject: process.Subject(Message),
  event_hub: process.Subject(hub.Message),
  session_id: String,
  now_ms: fn() -> Int,
) -> Result(workflow_run.WorkflowRunSuccess, workflow_run.WorkflowRunFailure) {
  let workflow_dependencies =
    workflow_run.Dependencies(
      ..workflow_dependencies,
      checkpoint: workflow_checkpoint.corrupt_tolerant_ledger_writer(
        bundle.effective.workspace.root,
        now_ms,
      ),
    )
  case
    workflow_run.execute_scheduled(
      scheduled,
      dag,
      bundle.orchestrator,
      tracker_client,
      secrets,
      yaml_scheduled_workflow_dependencies(
        workflow_dependencies,
        scheduled,
        daemon_subject,
        event_hub,
        now_ms,
      ),
    )
  {
    Ok(success) -> {
      publish_post_success_cleanup_warning(
        event_hub,
        session_id,
        success.cleanup_warning,
      )
      Ok(success)
    }
    Error(failure) -> Error(failure)
  }
}

fn run_workflow_worker(
  issue: tracker_issue.Issue,
  run_id: String,
  snapshot: worker_lifecycle.WorkflowSnapshot,
  tracker_client: tracker.Client,
  secrets: List(String),
  workflow_dependencies: workflow_run.Dependencies,
  daemon_subject: process.Subject(Message),
  event_hub: process.Subject(hub.Message),
  session_id: String,
  now_ms: fn() -> Int,
) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
  let workflow_dependencies =
    workflow_run.Dependencies(
      ..workflow_dependencies,
      checkpoint: workflow_checkpoint.ledger_writer(
        worker_lifecycle.workflow_snapshot_workspace_root(snapshot),
        now_ms,
      ),
    )
  case
    workflow_run.execute_with_context(
      issue,
      worker_lifecycle.workflow_snapshot_dag(snapshot),
      worker_lifecycle.workflow_snapshot_orchestrator(snapshot),
      tracker_client,
      secrets,
      workflow_run.FreshRun(workflow_run.RunInvocation(
        run_id: run_id,
        workflow_fingerprint: worker_lifecycle.workflow_snapshot_fingerprint(
          snapshot,
        ),
        supplied_contract_values: workflow_run.empty_contract_run_values(),
        scheduled_context: None,
      )),
      yaml_workflow_dependencies(
        workflow_dependencies,
        issue,
        run_id,
        session_id,
        daemon_subject,
        event_hub,
        now_ms,
      ),
    )
  {
    Ok(success) -> {
      publish_post_success_cleanup_warning(
        event_hub,
        session_id,
        success.cleanup_warning,
      )
      Ok(success.worker_success)
    }
    Error(failure) -> Error(yaml_workflow_failure(failure, issue))
  }
}

fn yaml_step_callbacks(
  daemon_subject: process.Subject(Message),
) -> yaml_workflow_lifecycle.LifecycleCallbacks {
  yaml_workflow_lifecycle.LifecycleCallbacks(
    step_started: fn(session_id, run_id, workflow_id, step_id, attempt_index) {
      process.send(
        daemon_subject,
        YamlStepStarted(session_id, run_id, workflow_id, step_id, attempt_index),
      )
    },
    step_update: fn(session_id, update) {
      process.send(daemon_subject, YamlStepUpdate(session_id, update))
    },
    step_command_ready: fn(session_id, command_subject) {
      process.send(
        daemon_subject,
        YamlStepCommandReady(session_id, command_subject),
      )
    },
    step_finished: fn(session_id, tokens) {
      process.send(daemon_subject, YamlStepFinished(session_id, tokens))
    },
  )
}

fn yaml_scheduled_workflow_dependencies(
  base: workflow_run.Dependencies,
  scheduled: schedule_core.ScheduledRunContext,
  daemon_subject: process.Subject(Message),
  event_hub: process.Subject(hub.Message),
  now_ms: fn() -> Int,
) -> workflow_run.Dependencies {
  yaml_workflow_lifecycle.scheduled_workflow_dependencies(
    base,
    scheduled,
    yaml_step_callbacks(daemon_subject),
    event_hub,
    now_ms,
  )
}

fn yaml_workflow_dependencies(
  base: workflow_run.Dependencies,
  issue: tracker_issue.Issue,
  run_id: String,
  parent_session_id: String,
  daemon_subject: process.Subject(Message),
  event_hub: process.Subject(hub.Message),
  now_ms: fn() -> Int,
) -> workflow_run.Dependencies {
  yaml_workflow_lifecycle.workflow_dependencies(
    base,
    issue,
    run_id,
    parent_session_id,
    yaml_step_callbacks(daemon_subject),
    event_hub,
    now_ms,
  )
}

fn yaml_worker_failure(
  reason: String,
  workspace_path: Option(String),
  issue: tracker_issue.Issue,
) -> agent_types.WorkerFailure {
  yaml_workflow_lifecycle.worker_failure(reason, workspace_path, issue)
}

fn yaml_workflow_failure(
  failure: workflow_run.WorkflowRunFailure,
  issue: tracker_issue.Issue,
) -> agent_types.WorkerFailure {
  yaml_workflow_lifecycle.workflow_failure(failure, issue)
}

fn worker_command_ready_context(
  state: State,
) -> worker_lifecycle.WorkerCommandReadyContext(State) {
  worker_lifecycle.WorkerCommandReadyContext(
    state: state,
    run_transition_messages: run_transition_messages,
    registry: fn(state) { state.registry },
    set_registry: fn(state, registry) { State(..state, registry: registry) },
  )
}

fn log_yaml_step_update(
  state: State,
  session_id: String,
  update: agent_types.RunnerUpdate,
) -> Nil {
  case update {
    agent_types.RunnerPiUpdate(update) ->
      log_pi_update(state, Some(session_id), None, update)
    agent_types.RunnerTurnUpdate(_) -> Nil
  }
}

fn log_worker_update(
  state: State,
  issue_id: String,
  update: agent_types.RunnerUpdate,
) -> State {
  case update {
    agent_types.RunnerPiUpdate(update) ->
      case log_pi_update(state, None, Some(issue_id), update) {
        Nil -> state
      }
    agent_types.RunnerTurnUpdate(_) -> state
  }
}

fn log_pi_update(
  state: State,
  session_id: Option(String),
  issue_id: Option(String),
  update: agent_types.PiUpdate,
) -> Nil {
  case pi_event.is_message_update(update.event) {
    True -> Nil
    False -> {
      let message = case update.message {
        Some(message) -> log.truncate(message, 200)
        None -> ""
      }
      let base = [
        #("event_name", pi_event.to_string(update.event)),
        #("message", message),
      ]
      let fields = case session_id {
        Some(session_id) -> [#("session_id", session_id), ..base]
        None -> base
      }
      let fields = case issue_id {
        Some(issue_id) -> [#("issue_id", issue_id), ..fields]
        None -> fields
      }
      log_state(state, "info", "pi_event", fields)
    }
  }
}

fn worker_update_context(
  state: State,
) -> worker_lifecycle.WorkerUpdateContext(State) {
  worker_lifecycle.WorkerUpdateContext(
    state: state,
    registry: fn(state) { state.registry },
    publish_worker_update: fn(session_id, update) {
      event_publisher.worker_update(state.event_hub, session_id, update)
    },
    log_worker_update: log_worker_update,
  )
}

fn scheduled_worker_finished_context(
  state: State,
) -> worker_lifecycle.ScheduledWorkerFinishedContext(State) {
  worker_lifecycle.ScheduledWorkerFinishedContext(
    state: state,
    evaluate_scheduled_jobs: evaluate_scheduled_jobs,
    scheduled_worker_for_run: fn(state, run_id) {
      worker_registry.scheduled_worker_for_run(state.registry, run_id)
    },
    log_stale: fn(state, run_id) {
      log_state(state, "warn", "scheduled_worker_finished_stale", [
        #("run_id", run_id),
      ])
    },
    demonitor: process.demonitor_process,
    remove_scheduled_worker_handle: fn(state, handle) {
      State(
        ..state,
        registry: worker_registry.remove_scheduled_worker_handle(
          state.registry,
          handle,
        ),
      )
    },
    finish_success: fn(state, handle, success) {
      worker_lifecycle.finish_scheduled_worker_success(
        scheduled_worker_success_context(state),
        handle,
        success,
      )
    },
    finish_failure: fn(state, handle, failure) {
      worker_lifecycle.finish_scheduled_worker_failure(
        scheduled_worker_failure_context(state),
        handle,
        failure,
      )
    },
    start_pending_scheduled_runs: start_pending_scheduled_runs,
  )
}

fn scheduled_worker_success_context(
  state: State,
) -> worker_lifecycle.ScheduledWorkerSuccessContext(State) {
  worker_lifecycle.ScheduledWorkerSuccessContext(
    state: state,
    log_worker_exited: fn(state, job_id, run_id, reason) {
      log_state(state, "info", "scheduled_worker_exited", [
        #("job_id", job_id),
        #("run_id", run_id),
        #("reason", reason),
      ])
    },
    update_tokens: fn(session_id, tokens) {
      hub.update_tokens(state.event_hub, session_id, tokens)
    },
    publish_worker_exited: fn(session_id, reason) {
      event_publisher.lifecycle(
        state.event_hub,
        session_id,
        session_event.WorkerExited,
        Some(log.truncate(reason, 200)),
      )
    },
    finish_session: fn(session_id, reason) {
      hub.finish_session(state.event_hub, session_id, reason)
    },
    append_success_ledger: fn(state, handle, success) {
      append_ledger_bodies_best_effort(
        state,
        [
          record.ScheduledRunSucceeded(
            handle.job_id,
            handle.workflow_id,
            handle.due_at_ms,
            handle.run_id,
            handle.attempt,
            state.dependencies.now_ms(),
            success.worker_success.tokens.total,
            success.worker_success.turns,
          ),
        ],
        "scheduled_success_append_failed",
      )
    },
    needs_human: fn(state, handle, success) {
      worker_lifecycle.finish_scheduled_worker_needs_human(
        scheduled_worker_needs_human_context(state),
        handle,
        success,
      )
    },
  )
}

fn scheduled_worker_needs_human_context(
  state: State,
) -> worker_lifecycle.ScheduledWorkerNeedsHumanContext(State) {
  worker_lifecycle.ScheduledWorkerNeedsHumanContext(
    state: state,
    log_needs_human: fn(state, job_id, run_id) {
      log_state(state, "warn", "scheduled_worker_needs_human", [
        #("job_id", job_id),
        #("run_id", run_id),
      ])
    },
    update_tokens: fn(session_id, tokens) {
      hub.update_tokens(state.event_hub, session_id, tokens)
    },
    publish_worker_exited: fn(session_id, reason) {
      event_publisher.lifecycle(
        state.event_hub,
        session_id,
        session_event.WorkerExited,
        Some(reason),
      )
    },
    finish_failed_session: fn(session_id) {
      hub.finish_session(state.event_hub, session_id, session_reason.Failed)
    },
    append_failure_ledger: scheduled_failure_ledger_append,
    begin_failure_report_request: begin_scheduled_failure_report_request,
  )
}

fn scheduled_worker_failure_context(
  state: State,
) -> worker_lifecycle.ScheduledWorkerFailureContext(State) {
  worker_lifecycle.ScheduledWorkerFailureContext(
    state: state,
    log_worker_exited: fn(state, job_id, run_id, reason) {
      log_state(state, "warn", "scheduled_worker_exited", [
        #("job_id", job_id),
        #("run_id", run_id),
        #("reason", log.truncate(reason, 200)),
      ])
    },
    publish_worker_exited: fn(session_id, reason) {
      event_publisher.lifecycle(
        state.event_hub,
        session_id,
        session_event.WorkerExited,
        Some(log.truncate(reason, 200)),
      )
    },
    finish_failed_session: fn(session_id) {
      hub.finish_session(state.event_hub, session_id, session_reason.Failed)
    },
    worker_failure_follow_up: scheduled_worker_failure_follow_up,
    append_failure_ledger: scheduled_failure_ledger_append,
    begin_failure_report_request: begin_scheduled_failure_report_request,
  )
}

fn scheduled_worker_failure_follow_up(
  state: State,
  handle: worker_registry.ScheduledWorkerHandle,
  reason: String,
  run_root: Option(String),
) -> #(State, scheduled_runtime.WorkerFailureFollowUp) {
  let #(runtime, follow_up) =
    scheduled_runtime.worker_failure_follow_up(
      state.scheduled_runtime,
      handle.job_id,
      handle.workflow_id,
      handle.due_at_ms,
      handle.run_id,
      handle.attempt,
      reason,
      run_root,
      Some(handle.session_id),
    )
  #(State(..state, scheduled_runtime: runtime), follow_up)
}

fn scheduled_failure_ledger_append(
  state: State,
  handle: worker_registry.ScheduledWorkerHandle,
  reason: String,
  retry_exhausted: Bool,
  run_root: Option(String),
) -> State {
  append_ledger_bodies_best_effort(
    state,
    [
      record.ScheduledRunFailed(
        handle.job_id,
        handle.workflow_id,
        handle.due_at_ms,
        handle.run_id,
        handle.attempt,
        state.dependencies.now_ms(),
        reason,
        retry_exhausted,
        run_root,
      ),
    ],
    "scheduled_failure_append_failed",
  )
}

fn begin_scheduled_failure_report_request(
  state: State,
  request: scheduled_runtime.FailureReportRequest,
) -> State {
  let scheduled_runtime.FailureReportRequest(
    job_id: job_id,
    workflow_id: workflow_id,
    due_at_ms: due_at_ms,
    run_id: run_id,
    attempt: attempt,
    reason: reason,
    run_root: run_root,
    session_id: session_id,
  ) = request
  case scheduled_job_by_id(state, job_id) {
    Error(Nil) -> state
    Ok(job) -> {
      let #(state, _) =
        begin_scheduled_failure_report_for_job(
          state,
          job,
          workflow_id,
          due_at_ms,
          run_id,
          attempt,
          reason,
          run_root,
          scheduled_runtime.initial_report_attempt_index(),
          session_id,
        )
      state
    }
  }
}

fn begin_scheduled_failure_report_for_job(
  state: State,
  job: config_types.ScheduledJobConfig,
  workflow_id: String,
  due_at_ms: Int,
  run_id: String,
  attempt: Int,
  reason: String,
  run_root: Option(String),
  report_attempt_index: Int,
  session_id: Option(String),
) -> #(State, Bool) {
  let task_config = job.on_failure.task
  case task_config.enabled, task_config.state {
    False, _ -> #(state, True)
    True, None -> {
      log_state(state, "warn", "scheduled_failure_report_skipped", [
        #("job_id", job.id),
        #("run_id", run_id),
        #("reason", "missing_triage_state"),
      ])
      #(state, True)
    }
    True, Some(triage_state) -> {
      let generation =
        scheduled_runtime.normalize_report_attempt_index(report_attempt_index)
      let previous_task_remote_id = case
        projection.scheduled_status_for(state.ledger_projection, job.id)
      {
        Ok(status) -> status.failure_issue_id
        Error(Nil) -> None
      }
      let publication =
        adapter.ScheduledFailurePublication(
          job_id: job.id,
          workflow_id: workflow_id,
          due_at_ms: due_at_ms,
          run_id: run_id,
          attempt: attempt,
          max_attempts: attempt,
          reason: reason,
          run_root: run_root,
          session_id: session_id,
          dedupe_key: outbox_effects.scheduled_failure_dedupe_key(job.id),
          title: "Scheduled workflow failure: " <> job.id,
          body: reason,
          labels: task_config.labels,
          target_state_name: Some(triage_state),
          previous_task_remote_id: previous_task_remote_id,
        )
      case state.tracker_adapter.scheduled_failures {
        Some(capability) -> {
          let intent =
            outbox_effects.scheduled_failure_intent(
              publication,
              generation,
              tracker_secrets(state),
            )
          enqueue_outbox_side_effect_with_attempt_count_result(
            state,
            intent,
            generation,
            fn(intent) {
              effect_runner.ReportScheduledFailure(
                outbox: intent,
                generation: generation,
                publication: publication,
                capability: capability,
              )
            },
          )
        }
        None -> #(state, True)
      }
    }
  }
}

fn handle_scheduled_retry_tick(
  state: State,
  run_id: String,
  generation: Int,
) -> State {
  let state = evaluate_scheduled_jobs(state)
  let clear_timer =
    scheduled_runtime.retry_tick_matches(
      state.scheduled_runtime,
      run_id,
      generation,
    )
  let #(runtime, actions) =
    scheduled_runtime.handle_retry_tick(
      state.scheduled_runtime,
      run_id,
      generation,
      state.dependencies.now_ms(),
      scheduled_slot_available_for_start(state),
      state.operator_paused,
    )
  let scheduled_retry_timers = case clear_timer {
    True -> dict.delete(state.scheduled_retry_timers, run_id)
    False -> state.scheduled_retry_timers
  }
  let state =
    State(
      ..state,
      scheduled_runtime: runtime,
      scheduled_retry_timers: scheduled_retry_timers,
    )
  apply_scheduled_runtime_actions(state, actions, append_retry_record: True)
  |> start_pending_scheduled_runs
}

fn handle_scheduled_failure_report_finished(
  state: State,
  outbox: outbox_effects.Intent,
  generation: Int,
  publication: adapter.ScheduledFailurePublication,
  result: Result(adapter.ScheduledFailureReceipt, adapter.TrackerError),
) -> State {
  case result {
    Ok(receipt) ->
      handle_scheduled_failure_report_success(
        state,
        outbox,
        publication,
        receipt,
      )
    Error(err) ->
      handle_scheduled_failure_report_failure(
        state,
        outbox,
        generation,
        publication,
        err,
      )
  }
}

fn handle_scheduled_failure_report_success(
  state: State,
  outbox: outbox_effects.Intent,
  publication: adapter.ScheduledFailurePublication,
  receipt: adapter.ScheduledFailureReceipt,
) -> State {
  emit_work_item_invalidation_for_task_ref(
    state,
    work_item_invalidation.WorkflowObserved,
    receipt.task,
  )
  let issue_id = receipt.task.remote_id
  let action = case receipt.created {
    True -> "created"
    False -> "updated"
  }
  log_state(state, "info", "scheduled_failure_reported", [
    #("job_id", publication.job_id),
    #("run_id", publication.run_id),
    #("linear_issue_id", issue_id),
    #("action", action),
  ])
  let state =
    State(
      ..state,
      scheduled_runtime: scheduled_runtime.clear_report_retry(
        state.scheduled_runtime,
        publication.run_id,
      ),
      scheduled_report_retry_timers: scheduled_runtime.delete_timer_cancelling_existing(
        state.scheduled_report_retry_timers,
        publication.run_id,
        state.dependencies.cancel_timer,
      ),
    )
  append_ledger_bodies_best_effort(
    state,
    [
      outbox_effects.completed_body(outbox),
      record.ScheduledFailureReported(
        publication.job_id,
        publication.workflow_id,
        publication.due_at_ms,
        publication.run_id,
        publication.attempt,
        publication.dedupe_key,
        issue_id,
        action,
      ),
    ],
    "scheduled_failure_report_append_failed",
  )
}

fn handle_scheduled_failure_report_failure(
  state: State,
  outbox: outbox_effects.Intent,
  generation: Int,
  publication: adapter.ScheduledFailurePublication,
  err: adapter.TrackerError,
) -> State {
  let decision =
    scheduled_runtime.decide_report_failure(
      state.scheduled_runtime,
      publication.job_id,
      publication.run_id,
      generation,
      err,
      state.dependencies.now_ms(),
      scheduled_runtime.default_max_backoff_ms(),
    )
  let state =
    State(
      ..state,
      scheduled_runtime: scheduled_runtime.report_failure_decision_runtime(
        decision,
      ),
      scheduled_report_retry_timers: case decision {
        scheduled_runtime.ReportFailureTerminal(..) ->
          scheduled_runtime.delete_timer_cancelling_existing(
            state.scheduled_report_retry_timers,
            publication.run_id,
            state.dependencies.cancel_timer,
          )
        scheduled_runtime.ReportFailureRetry(..) ->
          state.scheduled_report_retry_timers
      },
    )
  log_state(
    state,
    "warn",
    "scheduled_failure_report_failed",
    scheduled_runtime.report_failure_log_fields(publication, decision),
  )
  let state =
    append_ledger_bodies_best_effort(
      state,
      [
        scheduled_runtime.report_failure_outbox_failed_record(
          outbox,
          publication,
          decision,
          tracker_secrets(state),
        ),
        scheduled_runtime.report_failure_failed_record(publication, decision),
      ],
      "scheduled_failure_report_failed_append_failed",
    )
  case decision {
    scheduled_runtime.ReportFailureRetry(actions:, ..) ->
      apply_scheduled_runtime_actions(state, actions, append_retry_record: True)
    scheduled_runtime.ReportFailureTerminal(..) -> state
  }
}

fn handle_scheduled_report_retry_tick(
  state: State,
  run_id: String,
  generation: Int,
) -> State {
  let clear_timer =
    scheduled_runtime.report_retry_tick_matches(
      state.scheduled_runtime,
      run_id,
      generation,
    )
  let #(runtime, actions) =
    scheduled_runtime.handle_report_retry_tick(
      state.scheduled_runtime,
      run_id,
      generation,
    )
  let scheduled_report_retry_timers = case clear_timer {
    True -> dict.delete(state.scheduled_report_retry_timers, run_id)
    False -> state.scheduled_report_retry_timers
  }
  let state =
    State(
      ..state,
      scheduled_runtime: runtime,
      scheduled_report_retry_timers: scheduled_report_retry_timers,
    )
  apply_scheduled_runtime_actions(state, actions, append_retry_record: True)
}

fn retry_scheduled_failure_report_by_identity(
  state: State,
  job_id: String,
  run_id: String,
  report_attempt_index: Int,
) -> State {
  case projection.scheduled_status_for(state.ledger_projection, job_id) {
    Error(Nil) -> state
    Ok(status) ->
      case scheduled_job_by_id(state, job_id), status.current_run {
        Ok(job), Some(run) -> {
          let #(state, appended) =
            begin_scheduled_failure_report_for_job(
              state,
              job,
              status.workflow_id,
              run.due_at_ms,
              run_id,
              case run.attempt <= 0 {
                True -> 1
                False -> run.attempt
              },
              case status.last_failure_reason {
                Some(reason) -> reason
                None -> "scheduled failure"
              },
              run.run_root,
              report_attempt_index,
              run.session_id,
            )
          case appended {
            True -> state
            False ->
              retain_scheduled_report_retry_after_outbox_append_failure(
                state,
                job.id,
                run_id,
                report_attempt_index,
              )
          }
        }
        _, _ -> state
      }
  }
}

fn retain_scheduled_report_retry_after_outbox_append_failure(
  state: State,
  job_id: String,
  run_id: String,
  report_attempt_index: Int,
) -> State {
  let generation = previous_report_retry_generation(report_attempt_index)
  let state =
    State(
      ..state,
      scheduled_runtime: scheduled_runtime.insert_report_retry(
        state.scheduled_runtime,
        scheduled_runtime.ReportRetryStart(
          job_id: job_id,
          run_id: run_id,
          generation: generation,
        ),
      ),
    )
  log_state(state, "warn", "scheduled_report_retry_retained", [
    #("job_id", job_id),
    #("run_id", run_id),
    #("report_attempt", int.to_string(report_attempt_index)),
    #("reason", "outbox_append_failed"),
  ])
  schedule_scheduled_report_retry_timer(state, run_id, generation, 1000)
}

fn previous_report_retry_generation(report_attempt_index: Int) -> Int {
  let generation =
    scheduled_runtime.normalize_report_attempt_index(report_attempt_index) - 1
  case generation <= 0 {
    True -> scheduled_runtime.initial_report_attempt_index()
    False -> generation
  }
}

fn worker_finished_context(
  state: State,
  run_id: String,
  result: Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
) -> worker_lifecycle.WorkerFinishedContext(State) {
  let issue_state_name = worker_issue_state_name(state, run_id)
  let cleanup_reason = case result {
    Ok(_) -> None
    Error(failure) -> Some(error.agent_code(failure.reason))
  }
  worker_lifecycle.WorkerFinishedContext(
    state: state,
    evaluate_scheduled_jobs: evaluate_scheduled_jobs,
    run_transition_messages: run_transition_messages,
    lifecycle_context: transition_lifecycle_context,
    start_pending_scheduled_runs: fn(state) {
      let state = case cleanup_reason {
        Some(reason) ->
          cleanup_orphaned_yaml_children_after_parent_stop(
            state,
            run_id,
            reason,
            issue_state_name,
          )
        None -> state
      }
      start_pending_scheduled_runs(state)
    },
  )
}

fn transition_lifecycle_context(
  state: State,
) -> transition_types.WorkerLifecycleContext {
  transition_types.WorkerLifecycleContext(
    effective: state.workflow.effective,
    now_ms: state.dependencies.now_ms(),
    secrets: state.workflow.secrets,
  )
}

fn append_workflow_interrupted_terminal(
  state: State,
  handle: worker_registry.WorkerHandle,
  reason: String,
) -> State {
  case workflow_id_for_handle(state, handle) {
    Error(Nil) -> {
      log_state(state, "warn", "workflow_terminal_append_skipped", [
        #("issue_id", handle.issue_id),
        #("run_id", handle.run_id),
        #("reason", "workflow_id_unavailable"),
      ])
      state
    }
    Ok(workflow_id) ->
      append_ledger_bodies_best_effort(
        state,
        [
          record.WorkflowRunInterrupted(
            handle.run_id,
            workflow_id,
            handle.issue_id,
            reason,
          ),
        ],
        "workflow_terminal_append_failed",
      )
  }
}

fn workflow_id_for_handle(
  state: State,
  handle: worker_registry.WorkerHandle,
) -> Result(String, Nil) {
  case dict.get(state.workers.by_session, handle.session_id) {
    Ok(worker_identity) ->
      case dict.get(state.workers.by_issue, worker_identity) {
        Ok(entry) -> Ok(entry.workflow_id)
        Error(Nil) -> workflow_id_for_issue_from_bundle(state, handle.issue)
      }
    Error(Nil) -> workflow_id_for_issue_from_bundle(state, handle.issue)
  }
}

fn workflow_id_for_issue_from_bundle(
  state: State,
  issue: tracker_issue.Issue,
) -> Result(String, Nil) {
  case runtime_bundle.select_workflow(state.workflow.bundle, issue) {
    Ok(#(_, dag)) -> Ok(workflow_dag.id(dag))
    Error(runtime_bundle.BundleError(_, _)) -> Error(Nil)
  }
}

fn worker_down_context(
  state: State,
) -> worker_lifecycle.WorkerDownContext(State) {
  worker_lifecycle.WorkerDownContext(
    state: state,
    remote_client_monitor: state.remote_client_monitor,
    log_remote_client_down: fn(state) {
      log_state(state, "warn", "remote_client_down", [
        #("monitor", "remote_client"),
      ])
    },
    clear_remote_client: fn(state) {
      state
      |> update_read_model_remote_client_status(read_model.Stopped)
      |> fn(state) {
        State(..state, remote_client: None, remote_client_monitor: None)
      }
    },
    restart_remote_client_if_enabled: restart_remote_client_if_enabled,
    resolve_down: fn(state, monitor) {
      worker_registry.resolve_down(state.registry, monitor)
    },
    handle_registry_down_resolution: fn(state, resolution) {
      let issue_state_name =
        worker_issue_state_name_from_resolution(state, resolution)
      let cleanup_run_id = worker_run_id_from_resolution(resolution)
      let state =
        worker_lifecycle.handle_registry_down_resolution(
          registry_down_resolution_context(state),
          resolution,
        )
      case cleanup_run_id {
        Some(run_id) -> {
          let state =
            cleanup_orphaned_yaml_children_after_parent_stop(
              state,
              run_id,
              "worker_down",
              issue_state_name,
            )
          remove_yaml_step_tokens_for_run(state, run_id)
        }
        None -> state
      }
    },
  )
}

fn handle_effect_runner_down(state: State, down: process.Down) -> State {
  log_state(
    state,
    "error",
    "effect_runner_down",
    effect_runner_down_fields(down),
  )
  run_transition_messages(state, [transition_types.ShutdownRequested(False)])
}

fn handle_control_server_down(state: State, down: process.Down) -> State {
  log_state(
    state,
    "error",
    "control_server_down",
    control_server_down_fields(down),
  )
  run_transition_messages(state, [transition_types.ShutdownRequested(True)])
}

fn effect_runner_down_fields(down: process.Down) -> List(log.Field) {
  process_down_fields(down)
}

fn control_server_down_fields(down: process.Down) -> List(log.Field) {
  [#("monitor", "control_server_accept_loop"), ..process_down_fields(down)]
}

fn process_down_fields(down: process.Down) -> List(log.Field) {
  case down {
    process.ProcessDown(_, _, reason) -> [
      #("reason", process_exit_reason_to_string(reason)),
    ]
    process.PortDown(_, _, reason) -> [
      #("reason", process_exit_reason_to_string(reason)),
    ]
  }
}

fn process_exit_reason_to_string(reason: process.ExitReason) -> String {
  case reason {
    process.Normal -> "normal"
    process.Killed -> "killed"
    process.Abnormal(_) -> "abnormal"
  }
}

fn handle_side_effect_completed(
  state: State,
  completion: effect_runner.Completion,
) -> State {
  effect_completion_handler.handle_completed(
    effect_completion_context(state),
    completion,
  )
}

fn effect_completion_context(
  state: State,
) -> effect_completion_handler.Context(State) {
  effect_completion_handler.context(
    state: state,
    log_side_effect_crashed: fn(state, effect, reason) {
      log_state(state, "warn", "side_effect_crashed", [
        #("effect", effect_runner.effect_kind(effect)),
        #("reason", reason),
      ])
      state
    },
    result_handlers: effect_completion_handler.result_handlers(
      candidate_fetch_finished: handle_candidate_fetch_finished,
      running_refresh_finished: handle_running_refresh_finished,
      retry_refresh_finished: handle_retry_refresh_finished,
      dispatch_claim_validation_finished: handle_dispatch_claim_validation_finished,
      review_lane_preflight_finished: handle_review_lane_preflight_finished,
      handoff_claim_finished: handle_handoff_claim_finished,
      handoff_success_finished: handle_handoff_success_finished,
      handoff_failure_finished: handle_handoff_failure_finished,
      handoff_park_finished: handle_handoff_park_finished,
      invalid_workflow_report_finished: handle_invalid_workflow_report_finished,
      outbox_replay_finished: handle_outbox_replay_finished,
      scheduled_failure_report_finished: handle_scheduled_failure_report_finished,
      cleanup_finished: handle_cleanup_finished,
    ),
  )
}

fn handle_dispatch_claim_validation_finished(
  state: State,
  issue_id: String,
  generation: Int,
  result: Result(
    tracker_issue.Issue,
    effect_runner.DispatchClaimValidationError,
  ),
) -> State {
  case result {
    Ok(issue) ->
      emit_work_item_invalidation_for_issue(
        state,
        work_item_invalidation.TrackerRefresh,
        issue,
      )
    Error(_validation_error) -> Nil
  }
  let result = case result {
    Ok(issue) -> Ok(issue)
    Error(err) -> Error(dispatch_validation_error_to_transition(err))
  }
  run_transition_messages(state, [
    transition_types.DispatchValidationCompleted(
      issue_id,
      generation,
      result,
      transition_dispatch_context(state),
    ),
  ])
}

fn handle_review_lane_preflight_finished(
  state: State,
  task_identity: identity.TaskIdentity,
  issue_id: String,
  generation: Int,
  workflow_id: String,
  result: review_lane_preflight.PreflightResult,
) -> State {
  run_transition_messages(state, [
    transition_types.ReviewLanePreflightCompleted(
      task_identity,
      issue_id,
      generation,
      workflow_id,
      transition_dispatch_context(state),
      result,
    ),
  ])
}

fn dispatch_validation_error_to_transition(
  err: effect_runner.DispatchClaimValidationError,
) -> transition_types.DispatchValidationError {
  case err {
    effect_runner.DispatchValidationTrackerError(tracker_error) ->
      transition_types.DispatchValidationTrackerError(error.tracker_code(
        tracker_error,
      ))
    effect_runner.DispatchValidationMissingIssue ->
      transition_types.DispatchValidationMissingIssue
    effect_runner.DispatchValidationDuplicateIssue ->
      transition_types.DispatchValidationDuplicateIssue
    effect_runner.DispatchValidationIdMismatch(expected, actual) ->
      transition_types.DispatchValidationIdMismatch(expected, actual)
  }
}

fn handle_handoff_claim_finished(
  state: State,
  outbox: outbox_effects.Intent,
  issue_id: String,
  run_id: String,
  result: Result(Nil, error.TrackerError),
) -> State {
  emit_work_item_invalidation_for_outbox_result(
    state,
    work_item_invalidation.WorkflowObserved,
    outbox,
    result,
  )
  let task_identity =
    orchestrator_state.issue_id_identity_for_backend(
      issue_id,
      state.tracker_adapter.kind,
    )
  let active_run_present = dict.has_key(state.runtime.running, task_identity)
  let pending_claim_superseded = case
    dict.get(state.pending_claims, task_identity)
  {
    Ok(pending) -> pending.run_id != run_id
    Error(Nil) -> False
  }
  let compensation_reason = case result {
    Ok(Nil) ->
      case
        abandoned_claim.claim_success_abandoned(
          state.pending_claims,
          state.tracker_adapter.kind,
          issue_id,
        )
        && !active_run_present
      {
        True -> Some("stale_claim_success")
        False -> None
      }
    Error(err) ->
      case
        tracker_error_retryable(err)
        || active_run_present
        || pending_claim_superseded
      {
        True -> None
        False -> Some("permanent_failure:" <> error.tracker_code(err))
      }
  }
  let state = case result {
    Ok(Nil) -> state
    Error(err) -> append_outbox_failure(state, outbox, err)
  }
  case compensation_reason {
    Some(reason) -> {
      let state =
        State(
          ..state,
          pending_claims: dict.delete(state.pending_claims, task_identity),
        )
      compensate_abandoned_claim(state, outbox, run_id, reason)
    }
    None -> {
      let claim_result =
        handoff_claim_result_for_transition(
          state,
          outbox,
          issue_id,
          run_id,
          result,
        )
      run_transition_messages(state, [
        transition_types.HandoffClaimCompleted(
          task_identity,
          identity.issue_id_from_string(issue_id),
          identity.run_id_from_string(run_id),
          claim_result,
        ),
      ])
    }
  }
}

fn handoff_claim_result_for_transition(
  state: State,
  outbox: outbox_effects.Intent,
  issue_id: String,
  run_id: String,
  result: Result(Nil, error.TrackerError),
) -> transition_types.HandoffClaimResult {
  case result {
    Error(err) -> transition_types.HandoffClaimFailed(error.tracker_code(err))
    Ok(Nil) ->
      case
        dict.get(
          state.pending_claims,
          orchestrator_state.issue_id_identity_for_backend(
            issue_id,
            state.tracker_adapter.kind,
          ),
        )
      {
        Error(Nil) -> transition_types.HandoffClaimFailed("stale")
        Ok(pending) ->
          case pending.run_id == run_id {
            False -> transition_types.HandoffClaimFailed("stale")
            True -> claim_ledger_batch_for_pending(state, pending, outbox)
          }
      }
  }
}

fn compensate_abandoned_claim(
  state: State,
  outbox: outbox_effects.Intent,
  run_id: String,
  abandonment_reason: String,
) -> State {
  let compensation =
    abandoned_claim.compensate(
      state.runtime,
      outbox_effects.task_ref_from_fields(outbox.task_ref),
      run_id,
      abandonment_reason,
      state.dependencies.now_ms(),
      tracker_secrets(state),
    )
  let state = State(..state, runtime: compensation.runtime)
  log_state(state, "warn", "abandoned_claim_compensated", [
    #("issue_id", compensation.parked.issue_id),
    #("issue_identifier", compensation.parked.identifier),
    #("run_id", run_id),
    #("reason", abandonment_reason),
  ])
  let #(state, appended) =
    append_parked_record(state, compensation.parked, compensation.reason_text)
  case appended {
    False -> state
    True -> enqueue_release_claim_intent(state, compensation.release_intent)
  }
}

fn enqueue_release_claim_intent(
  state: State,
  intent: outbox_effects.Intent,
) -> State {
  enqueue_outbox_side_effect(state, intent, fn(intent) {
    effect_runner.ReplayOutbox(
      outbox: recovery.OutboxReplay(
        intent.outbox_id,
        intent.task_ref,
        intent.outbox_kind,
        intent.dedupe_key,
        intent.payload_json,
      ),
      comments: state.tracker_adapter.comments,
      state_transitions: state.tracker_adapter.state_transitions,
      scheduled_failures: state.tracker_adapter.scheduled_failures,
    )
  })
}

fn claim_ledger_batch_for_pending(
  state: State,
  pending: transition_types.PendingClaim,
  outbox: outbox_effects.Intent,
) -> transition_types.HandoffClaimResult {
  let post_spawn_runtime =
    core.apply_task_ref_start(
      state.runtime,
      pending.task_ref,
      pending.issue,
      pending.workspace_path,
    )
  let counter = counter_for_runtime(post_spawn_runtime, pending.issue.id)
  let workflow_started_body = workflow_run_started_body_for_claim(pending)
  let batch =
    ledger_batch.claim_started(
      workflow_started_body,
      pending.issue.id,
      pending.issue.identifier,
      pending.workspace_path,
      counter.failure_attempts,
      counter.worker_sessions,
      state.dependencies.now_ms(),
    )
  let batch = case pending.retry_cancellation {
    Some(transition_types.RetryCancellation(
      issue_id: retry_issue_id,
      generation: generation,
      reason: reason,
      ..,
    )) ->
      ledger_batch.append_retry_cancelled(
        batch,
        retry_issue_id,
        generation,
        reason,
      )
    None -> batch
  }
  let batch =
    ledger_batch.append_body(batch, outbox_effects.completed_body(outbox))
  transition_types.HandoffClaimSucceeded(batch)
}

fn handle_handoff_success_finished(
  state: State,
  outbox: outbox_effects.Intent,
  issue_id: String,
  result: Result(Nil, error.TrackerError),
) -> State {
  emit_work_item_invalidation_for_outbox_result(
    state,
    work_item_invalidation.WorkflowObserved,
    outbox,
    result,
  )
  let state = append_outbox_result(state, outbox, result)
  case result {
    Ok(Nil) -> state
    Error(err) -> {
      log_state(state, "warn", "handoff_success_failed", [
        #("issue_id", issue_id),
        #("error", error.tracker_code(err)),
      ])
      state
    }
  }
}

fn handle_handoff_failure_finished(
  state: State,
  outbox: outbox_effects.Intent,
  issue_id: String,
  result: Result(Nil, error.TrackerError),
) -> State {
  emit_work_item_invalidation_for_outbox_result(
    state,
    work_item_invalidation.WorkflowObserved,
    outbox,
    result,
  )
  let state = append_outbox_result(state, outbox, result)
  case result {
    Ok(Nil) -> state
    Error(err) -> {
      log_state(state, "warn", "handoff_failure_failed", [
        #("issue_id", issue_id),
        #("error", error.tracker_code(err)),
      ])
      state
    }
  }
}

fn handle_handoff_park_finished(
  state: State,
  outbox: outbox_effects.Intent,
  issue_id: String,
  result: Result(Nil, error.TrackerError),
) -> State {
  emit_work_item_invalidation_for_outbox_result(
    state,
    work_item_invalidation.WorkflowObserved,
    outbox,
    result,
  )
  let state = append_outbox_result(state, outbox, result)
  case result {
    Ok(Nil) -> state
    Error(err) -> {
      log_state(state, "warn", "handoff_park_failed", [
        #("issue_id", issue_id),
        #("error", error.tracker_code(err)),
      ])
      state
    }
  }
}

fn handle_invalid_workflow_report_finished(
  state: State,
  outbox: outbox_effects.Intent,
  issue_id: String,
  violation_fingerprint: String,
  reporting_policy_fingerprint: String,
  result: Result(effect_runner.InvalidWorkflowReportOutcome, error.TrackerError),
) -> State {
  case result {
    Ok(effect_runner.InvalidWorkflowReportNoop) | Error(_) -> Nil
    Ok(_) ->
      emit_work_item_invalidation_for_task_ref(
        state,
        work_item_invalidation.WorkflowObserved,
        outbox_effects.task_ref_from_fields(outbox.task_ref),
      )
  }
  let state = append_outbox_result(state, outbox, result)
  case result {
    Ok(effect_runner.InvalidWorkflowReportNoop) -> {
      log_state(state, "info", "invalid_workflow_report_noop", [
        #("issue_id", issue_id),
        #("violation_fingerprint", violation_fingerprint),
      ])
      run_transition_messages(state, [
        transition_types.InvalidWorkflowReportResultRecorded(
          issue_id,
          violation_fingerprint,
          reporting_policy_fingerprint,
          "noop",
        ),
      ])
    }
    Ok(outcome) -> {
      log_state(state, "info", "invalid_workflow_reported", [
        #("issue_id", issue_id),
        #("violation_fingerprint", violation_fingerprint),
        #("outcome", invalid_workflow_outcome_to_string(outcome)),
      ])
      run_transition_messages(state, [
        transition_types.InvalidWorkflowReportResultRecorded(
          issue_id,
          violation_fingerprint,
          reporting_policy_fingerprint,
          "reported",
        ),
      ])
    }
    Error(err) -> {
      log_state(state, "warn", "invalid_workflow_report_failed", [
        #("issue_id", issue_id),
        #("violation_fingerprint", violation_fingerprint),
        #("error", error.tracker_code(err)),
      ])
      run_transition_messages(state, [
        transition_types.InvalidWorkflowReportResultRecorded(
          issue_id,
          violation_fingerprint,
          reporting_policy_fingerprint,
          "failed",
        ),
      ])
    }
  }
}

fn invalid_workflow_outcome_to_string(
  outcome: effect_runner.InvalidWorkflowReportOutcome,
) -> String {
  case outcome {
    effect_runner.InvalidWorkflowReportNoop -> "noop"
    effect_runner.InvalidWorkflowReportComment -> "comment"
    effect_runner.InvalidWorkflowReportState -> "state"
    effect_runner.InvalidWorkflowReportCommentAndState -> "comment_and_state"
  }
}

fn record_outbox_replay_payload_failure(
  state: State,
  intent: outbox_effects.Intent,
  replay_error: outbox.ReplayError,
) -> State {
  let error_code = outbox_effects.replay_error_code(replay_error)
  let state =
    append_ledger_bodies_best_effort(
      state,
      [outbox_effects.replay_failed_body(intent, replay_error)],
      "outbox_replay_payload_append_failed",
    )
  let _ =
    log_state(state, "warn", "outbox_replay_failed", [
      #("outbox_id", intent.outbox_id),
      #("outbox_kind", intent.outbox_kind),
      #("task_remote_id", intent.task_ref.task_remote_id),
      #("error", error_code),
    ])
  state
}

fn handle_outbox_replay_finished(
  state: State,
  outbox_replay: recovery.OutboxReplay,
  result: Result(Nil, error.TrackerError),
) -> State {
  let recovery.OutboxReplay(outbox_id, task_ref, outbox_kind, _, _) =
    outbox_replay
  let intent = outbox_effects.recovered_intent(outbox_replay)
  emit_work_item_invalidation_for_outbox_result(
    state,
    work_item_invalidation.WorkflowObserved,
    intent,
    result,
  )
  case result {
    Ok(Nil) -> {
      let state =
        append_ledger_bodies_best_effort(
          state,
          [outbox_effects.completed_body(intent)],
          "outbox_replay_completion_append_failed",
        )
      log_state(state, "info", "outbox_replay_completed", [
        #("outbox_id", outbox_id),
        #("outbox_kind", outbox_kind),
        #("task_remote_id", task_ref.task_remote_id),
      ])
      state
    }
    Error(err) -> {
      let state = append_outbox_failure(state, intent, err)
      log_state(state, "warn", "outbox_replay_failed", [
        #("outbox_id", outbox_id),
        #("outbox_kind", outbox_kind),
        #("task_remote_id", task_ref.task_remote_id),
        #("error", error.tracker_code(err)),
      ])
      state
    }
  }
}

fn handle_cleanup_finished(
  state: State,
  workspace_path: String,
  result: Result(Nil, error.WorkspaceError),
) -> State {
  case result {
    Ok(Nil) -> {
      log_state(state, "info", "workflow_cleanup_completed", [
        #("workspace_path", workspace_path),
        #("status", "deleted"),
      ])
      log_state(state, "info", "workspace_cleaned", [
        #("workspace_path", workspace_path),
      ])
      state
    }
    Error(err) -> {
      log_state(state, "warn", "workflow_cleanup_completed", [
        #("workspace_path", workspace_path),
        #("status", "failed"),
        #("error", error.workspace_code(err)),
      ])
      log_state(state, "warn", "workspace_cleanup_failed", [
        #("workspace_path", workspace_path),
        #("error", error.workspace_code(err)),
      ])
      state
    }
  }
}

fn require_handoff_capability(state: State) -> adapter.HandoffCapability {
  case state.tracker_adapter.handoff {
    Some(capability) -> capability
    None -> adapter.HandoffCapability(report: fn(_) { Ok(Nil) })
  }
}

fn enqueue_side_effect(state: State, effect: effect_runner.Effect) -> State {
  effect_runner.enqueue(state.effect_runner, effect)
  state
}

fn enqueue_outbox_side_effect(
  state: State,
  intent: outbox_effects.Intent,
  make_effect: fn(outbox_effects.Intent) -> effect_runner.Effect,
) -> State {
  enqueue_outbox_side_effect_with_attempt_count(state, intent, 1, make_effect)
}

fn enqueue_outbox_side_effect_with_attempt_count(
  state: State,
  intent: outbox_effects.Intent,
  attempt_count: Int,
  make_effect: fn(outbox_effects.Intent) -> effect_runner.Effect,
) -> State {
  let #(state, _) =
    enqueue_outbox_side_effect_with_attempt_count_result(
      state,
      intent,
      attempt_count,
      make_effect,
    )
  state
}

fn enqueue_outbox_side_effect_with_attempt_count_result(
  state: State,
  intent: outbox_effects.Intent,
  attempt_count: Int,
  make_effect: fn(outbox_effects.Intent) -> effect_runner.Effect,
) -> #(State, Bool) {
  let #(state, appended) =
    append_outbox_attempt_with_count(state, intent, attempt_count)
  case appended {
    True -> #(enqueue_side_effect(state, make_effect(intent)), True)
    False -> #(state, False)
  }
}

fn append_outbox_attempt_with_count(
  state: State,
  intent: outbox_effects.Intent,
  attempt_count: Int,
) -> #(State, Bool) {
  append_ledger_bodies(
    state,
    [
      outbox_effects.pending_body(intent),
      outbox_effects.attempted_body(intent, attempt_count),
    ],
    "outbox_ledger_append_failed",
  )
}

fn append_outbox_result(
  state: State,
  intent: outbox_effects.Intent,
  result: Result(a, error.TrackerError),
) -> State {
  case result {
    Ok(_) ->
      append_ledger_bodies_best_effort(
        state,
        [outbox_effects.completed_body(intent)],
        "outbox_ledger_append_failed",
      )
    Error(err) -> append_outbox_failure(state, intent, err)
  }
}

fn append_outbox_failure(
  state: State,
  intent: outbox_effects.Intent,
  err: error.TrackerError,
) -> State {
  let error_code = error.tracker_code(err)
  let body = case tracker_error_retryable(err) {
    True -> {
      log_state(state, "warn", "outbox_retry_scheduled", [
        #("outbox_id", intent.outbox_id),
        #("outbox_kind", intent.outbox_kind),
        #("error", error_code),
      ])
      outbox_effects.retry_scheduled_body(
        intent,
        error_code,
        1,
        state.dependencies.now_ms() + 60_000,
      )
    }
    False -> {
      log_state(state, "error", "outbox_permanently_failed", [
        #("outbox_id", intent.outbox_id),
        #("outbox_kind", intent.outbox_kind),
        #("error", error_code),
      ])
      outbox_effects.permanently_failed_body(intent, error_code, 1)
    }
  }
  append_ledger_bodies_best_effort(state, [body], "outbox_ledger_append_failed")
}

fn tracker_error_retryable(err: error.TrackerError) -> Bool {
  case err {
    error.LinearApiStatus(status) -> status >= 500
    error.LinearApiRequest(_) -> True
    error.LinearUploadStatus(status) -> status >= 500
    error.LinearGraphqlErrors(_)
    | error.LinearUnknownPayload(_)
    | error.LinearMissingEndCursor
    | error.LinearAttachmentError(_) -> False
  }
}

fn tracker_secrets(state: State) -> List(String) {
  case state.workflow.effective.tracker.api_key {
    Some(value) -> [value]
    None -> []
  }
}

fn append_ledger_bodies_best_effort(
  state: State,
  bodies: List(record.RecordBody),
  event: String,
) -> State {
  let #(state, _) = append_ledger_bodies(state, bodies, event)
  state
}

fn append_ledger_bodies(
  state: State,
  bodies: List(record.RecordBody),
  event: String,
) -> #(State, Bool) {
  case bodies {
    [] -> #(state, True)
    _ -> {
      let #(state, append_result) =
        append_ledger_records(
          state,
          ledger_records_for_bodies(state.dependencies.now_ms(), bodies),
          event,
        )
      #(state, append_result == Ok(Nil))
    }
  }
}

fn append_ledger_records(
  state: State,
  records: List(record.LedgerRecord),
  event: String,
) -> #(State, Result(Nil, ledger.LedgerError)) {
  case records {
    [] -> #(state, Ok(Nil))
    _ ->
      case
        ledger.path_for_workspace_root(state.workflow.effective.workspace.root)
      {
        Error(err) -> {
          log_state(state, "error", event, [
            #("error", ledger.ledger_error_to_string(err)),
          ])
          #(state, Error(err))
        }
        Ok(ledger_path) ->
          case ledger.append_many(ledger_path, records, True) {
            Ok(Nil) -> #(
              State(
                ..state,
                ledger_projection: projection.fold_from(
                  state.ledger_projection,
                  records,
                ),
              ),
              Ok(Nil),
            )
            Error(err) -> {
              log_state(state, "error", event, [
                #("error", ledger.ledger_error_to_string(err)),
              ])
              #(state, Error(err))
            }
          }
      }
  }
}

fn observed_updated_at_ms(issue: tracker_issue.Issue) -> Int {
  case issue.updated_at {
    Some(time) -> birl.to_unix_milli(time)
    None -> 0
  }
}

fn ledger_records_for_bodies(
  now_ms: Int,
  bodies: List(record.RecordBody),
) -> List(record.LedgerRecord) {
  ledger_records_for_bodies_loop(bodies, now_ms, 1, [])
}

fn ledger_records_for_bodies_loop(
  bodies: List(record.RecordBody),
  now_ms: Int,
  sequence: Int,
  acc: List(record.LedgerRecord),
) -> List(record.LedgerRecord) {
  case bodies {
    [] -> list.reverse(acc)
    [body, ..rest] ->
      ledger_records_for_bodies_loop(rest, now_ms, sequence + 1, [
        record.new(now_ms, sequence, body),
        ..acc
      ])
  }
}

fn counter_for_runtime(
  runtime: orchestrator_state.RuntimeState,
  issue_id: String,
) -> orchestrator_state.IssueCounter {
  case
    dict.get(
      runtime.issue_counters,
      orchestrator_state.linear_issue_id_identity(issue_id),
    )
  {
    Ok(counter) -> counter
    Error(Nil) -> orchestrator_state.new_issue_counter()
  }
}

fn enqueue_park_report(
  state: State,
  issue_id: String,
  issue_identifier: String,
  reason: String,
  release_policy: String,
  source_run_id: Option(String),
) -> State {
  let report =
    adapter.ParkReport(
      task: task.TaskRef(
        backend_kind: state.tracker_adapter.kind,
        remote_id: issue_id,
        key: Some(issue_identifier),
        url: None,
      ),
      issue_identifier: issue_identifier,
      reason: reason,
      release_policy: Some(release_policy),
      run_id: source_run_id,
    )
  transition_report_park(state, report)
}

fn park_release_policy_to_string(
  release_policy: orchestrator_state.ParkReleasePolicy,
) -> String {
  case release_policy {
    orchestrator_state.ExplicitUnparkOnly -> "explicit_unpark_only"
    orchestrator_state.AutoUnparkOnIssueChange(_) ->
      "auto_unpark_on_issue_change"
  }
}

fn kill_worker(handle: worker_registry.WorkerHandle) -> Nil {
  process.kill(handle.pid)
}

fn kill_scheduled_worker(handle: worker_registry.ScheduledWorkerHandle) -> Nil {
  process.kill(handle.pid)
}

fn append_shutdown_step_attempt_interruptions(state: State) -> Nil {
  let bodies =
    worker_registry.worker_handles(state.registry)
    |> list.fold([], fn(bodies, handle) {
      list.append(
        shutdown_step_attempt_interruption_bodies(state.registry, handle.run_id),
        bodies,
      )
    })
  case bodies {
    [] -> Nil
    _ -> {
      let _state =
        append_ledger_bodies_best_effort(
          state,
          bodies,
          "workflow_shutdown_interrupt_append_failed",
        )
      Nil
    }
  }
}

fn shutdown_step_attempt_interruption_bodies(
  registry: worker_registry.Registry,
  run_id: String,
) -> List(record.RecordBody) {
  active_yaml_cleanup_candidates_for_run(registry, run_id)
  |> list.map(fn(candidate) {
    record.StepAttemptInterrupted(
      run_id,
      candidate.workflow_id,
      candidate.step_id,
      candidate.attempt_index,
      "daemon_shutdown",
    )
  })
}

fn shutdown_runtime_shell(state: State, stop_effect_runner: Bool) -> State {
  process.demonitor_process(state.effect_runner_monitor)
  case state.control_server_monitor {
    Some(monitor) -> process.demonitor_process(monitor)
    None -> Nil
  }
  case stop_effect_runner {
    True ->
      case effect_runner.shutdown(state.effect_runner, 1000) {
        Ok(Nil) -> Nil
        Error(Nil) ->
          log_state(state, "warn", "effect_runner_shutdown_timeout", [
            #("timeout_ms", "1000"),
          ])
      }
    False -> Nil
  }
  case state.remote_client {
    Some(handle) ->
      case state.dependencies.stop_remote_client(handle, 1000) {
        Ok(Nil) -> Nil
        Error(Nil) -> {
          remote.kill(handle)
          log_state(state, "warn", "remote_client_shutdown_timeout", [
            #("timeout_ms", "1000"),
          ])
        }
      }
    None -> Nil
  }
  let _best_effort = query_service.stop(state.query_service, 1000)
  state.dependencies.stop_control_server(state.control_server)
  case state.control_file_path {
    Some(path) -> control_file.remove(path)
    None -> Nil
  }
  let poll =
    poll_scheduler.cancel_all(state.poll, state.dependencies.cancel_timer)
  let retry =
    retry_scheduler.cancel_all(state.retry, state.dependencies.cancel_timer)
  append_shutdown_step_attempt_interruptions(state)
  worker_registry.worker_handles(state.registry)
  |> list.each(fn(handle) { kill_worker(handle) })
  worker_registry.scheduled_worker_handles(state.registry)
  |> list.each(fn(handle) { kill_scheduled_worker(handle) })
  state.scheduled_retry_timers
  |> dict.values
  |> list.each(state.dependencies.cancel_timer)
  state.scheduled_report_retry_timers
  |> dict.values
  |> list.each(state.dependencies.cancel_timer)
  let event_hub_shutdown_timeout_ms = 1000
  case hub.stop_and_wait(state.event_hub, event_hub_shutdown_timeout_ms) {
    Ok(Nil) -> Nil
    Error(Nil) ->
      log_state(state, "warn", "event_hub_shutdown_timeout", [
        #("timeout_ms", int.to_string(event_hub_shutdown_timeout_ms)),
      ])
  }
  let registry = worker_registry.remove_all(state.registry)
  State(
    ..state,
    poll: poll,
    retry: retry,
    registry: registry,
    workers: transition_types.new_worker_directory(),
    pending_claims: dict.new(),
    pending_dispatch_validations: dict.new(),
    pending_review_lane_preflights: dict.new(),
    scheduled_runtime: scheduled_runtime.new(),
    scheduled_retry_timers: dict.new(),
    scheduled_report_retry_timers: dict.new(),
    control_server: NoControlServer,
    control_server_monitor: None,
    control_file_path: None,
    query_service: state.query_service,
    remote_client: None,
    remote_client_monitor: None,
  )
}

fn make_recovered_session_id(run_id: String, sequence: Int) -> String {
  run_id <> "-resume-" <> int.to_string(sequence)
}

fn log_state(
  state: State,
  level: String,
  event: String,
  fields: List(log.Field),
) -> Nil {
  case state.dependencies.logger(level, event, fields, state.workflow.secrets) {
    Ok(Nil) -> Nil
    Error(Nil) -> Nil
  }
}

fn map_bundle_error(
  result: Result(a, runtime_bundle.BundleError),
) -> Result(a, StartupError) {
  case result {
    Ok(value) -> Ok(value)
    Error(runtime_bundle.BundleError(code, message)) ->
      Error(StartupError(code, message))
  }
}

fn map_hub_error(result: Result(a, hub.HubError)) -> Result(a, StartupError) {
  case result {
    Ok(value) -> Ok(value)
    Error(_) ->
      Error(StartupError("event_hub_start_failed", "event hub start failed"))
  }
}

fn map_control_file_error(
  result: Result(a, control_file.ControlFileError),
) -> Result(a, StartupError) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) ->
      case err {
        control_file.TokenGenerationFailed(message) ->
          Error(StartupError("control_token_failed", message))
        control_file.ControlFileWriteFailed(_, message)
        | control_file.ControlFilePermissionFailed(_, message) ->
          Error(StartupError("control_file_write_failed", message))
        control_file.ControlFileReadFailed(_, message)
        | control_file.ControlFileInvalid(_, message)
        | control_file.ControlFileNotFound(message) ->
          Error(StartupError("control_file_write_failed", message))
      }
  }
}

fn try_startup(
  result: Result(a, StartupError),
  next: fn(a) -> Result(b, StartupError),
) -> Result(b, StartupError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}

@external(erlang, "scherzo_time_ffi", "wall_clock_ms")
fn wall_clock_ms() -> Int
