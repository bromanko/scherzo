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
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/control/file as control_file
import scherzo/control/query/backend as query_backend
import scherzo/control/query/metrics as query_metrics
import scherzo/control/query/service as query_service
import scherzo/control/query/types as query_types
import scherzo/control/server as control_server
import scherzo/ctl/artifact_publication_retry as ctl_artifact_publication_retry
import scherzo/daemon_identity
import scherzo/error
import scherzo/log
import scherzo/orchestrator/control_command_handler
import scherzo/orchestrator/core
import scherzo/orchestrator/daemon_transition_shell
import scherzo/orchestrator/effect_completion_handler
import scherzo/orchestrator/effect_runner
import scherzo/orchestrator/effects/types as transition_effects
import scherzo/orchestrator/event_publisher
import scherzo/orchestrator/operator_runtime
import scherzo/orchestrator/outbox_effects
import scherzo/orchestrator/poll_scheduler
import scherzo/orchestrator/read_model
import scherzo/orchestrator/remote_command_runtime
import scherzo/orchestrator/retry_scheduler
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
import scherzo/workflow_checkpoint
import scherzo/workflow_dag
import scherzo/workflow_fingerprint
import scherzo/workflow_repair
import scherzo/workflow_run
import scherzo/workspace
import scherzo/workspace_profile
import scherzo/workspace_run

pub type StartupError {
  StartupError(code: String, message: String)
}

pub type Message {
  PollTick(Int)
  RetryTick(String, Int)
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
  YamlStepStarted(String, String)
  YamlStepUpdate(String, agent_types.RunnerUpdate)
  YamlStepCommandReady(String, process.Subject(worker_command.Command))
  YamlStepFinished(String, session_tokens.TokenTotals)
  AbortWorkerCommandTimedOut(
    command.OperatorCommand,
    String,
    process.Subject(command.CommandResult),
  )
  WorkerDown(process.Down)
  EffectRunnerDown(process.Down)
  SideEffectCompleted(effect_runner.Completion)
  Shutdown(process.Subject(Nil))
  GetSnapshot(process.Subject(orchestrator_state.RuntimeState))
  GetReadModelSnapshot(process.Subject(read_model.Snapshot))
  GetRemoteDispatchPaused(process.Subject(Bool))
  StartRemoteClient
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

pub type RuntimeDependencies {
  RuntimeDependencies(
    make_tracker_adapter: fn(config_types.EffectiveConfig) ->
      adapter.TrackerAdapter,
    workflow_run_dependencies: workflow_run.Dependencies,
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
      process.Subject(hub.Message),
      process.Subject(Message),
      List(String),
      fn(String, String, List(log.Field), List(String)) -> Result(Nil, Nil),
    ) -> Result(remote_command_runtime.Handle, StartupError),
    stop_remote_client: fn(remote_command_runtime.Handle, Int) ->
      Result(Nil, Nil),
    monitor_remote_client: fn(remote_command_runtime.Handle) -> process.Monitor,
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
    control_file_path: Option(String),
    query_service: query_service.Handle,
    read_model: read_model.ReadModel,
    remote_client: Option(remote_command_runtime.Handle),
    remote_client_monitor: Option(process.Monitor),
    operator_paused: Bool,
    last_operator_command_result: Option(command.CommandResult),
    shell_state_overrides_transition: Bool,
    dependencies: RuntimeDependencies,
  )
}

pub fn default_dependencies() -> RuntimeDependencies {
  RuntimeDependencies(
    make_tracker_adapter: linear_adapter.real,
    workflow_run_dependencies: workflow_run.default_dependencies(),
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
      event_hub,
      daemon_subject,
      secrets,
      logger,
    ) {
      remote_command_runtime.start_remote_client(
        effective,
        event_hub,
        daemon_subject,
        secrets,
        logger,
        remote_command_runtime.control_dependencies(
          apply_operator_command: apply_operator_command,
          execute_query: execute_query,
          get_remote_dispatch_paused: get_remote_dispatch_paused,
        ),
      )
      |> result.map_error(fn(err) {
        let #(code, message) = remote_command_runtime.start_error_fields(err)
        StartupError(code, message)
      })
    },
    stop_remote_client: remote_command_runtime.stop,
    monitor_remote_client: remote_command_runtime.monitor,
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
  query_service.start(
    query_service.default_settings(),
    query_service.Backend(run: fn(query) {
      let get_dispatch_paused = fn(timeout_ms) {
        get_remote_dispatch_paused(daemon_subject, timeout_ms)
      }
      let request_read_model_snapshot = fn(timeout_ms) {
        get_read_model_snapshot(daemon_subject, timeout_ms)
      }
      case query {
        query_types.Status ->
          query_metrics.execute_status(
            get_snapshot: request_read_model_snapshot,
          )
        query_types.Metrics ->
          query_metrics.execute_metrics(
            get_snapshot: request_read_model_snapshot,
          )
        query_types.TaskList(_) | query_types.TaskShow(_) ->
          query_backend.run(
            effective,
            identity,
            tracker_adapter,
            get_dispatch_paused,
            query,
          )
      }
    }),
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
  case ledger.path_for_workspace_root(state.workflow.effective.workspace.root) {
    Error(_) -> #(0, 0, 0, 0)
    Ok(ledger_path) ->
      case ledger.load_projection(ledger_path) {
        Error(_) -> #(0, 0, 0, 0)
        Ok(loaded) ->
          loaded.outbox
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
  }
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
  let settings = control_server.default_settings(token)
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
  case state.workflow.effective.ui_server.enabled {
    False -> stop_remote_client_and_clear(state, read_model.Disabled)
    True ->
      case
        state.dependencies.start_remote_client(
          state.workflow.effective,
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
          remote_command_runtime.kill(handle)
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
  let reply = process.new_subject()
  process.send(
    daemon_subject,
    ApplyOperatorCommand(operator_command, timeout_ms, reply),
  )
  process.receive(reply, within: timeout_ms)
}

pub fn execute_query(
  daemon_subject: process.Subject(Message),
  query: query_types.QueryRequest,
  timeout_ms: Int,
) -> Result(query_types.QueryResponse, query_types.QueryError) {
  let reply = process.new_subject()
  process.send(daemon_subject, ExecuteQuery(query, timeout_ms, reply))
  case process.receive(reply, within: timeout_ms) {
    Ok(result) -> result
    Error(Nil) ->
      Error(query_types.QueryError(
        query_types.QueryTimeout,
        "daemon query timed out",
      ))
  }
}

pub fn get_remote_dispatch_paused(
  daemon_subject: process.Subject(Message),
  timeout_ms: Int,
) -> Result(Bool, Nil) {
  let reply = process.new_subject()
  process.send(daemon_subject, GetRemoteDispatchPaused(reply))
  process.receive(reply, within: timeout_ms)
}

pub fn start(
  workflow_path: Option(String),
  dependencies: RuntimeDependencies,
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
  let builder =
    actor.new_with_initialiser(60_000, fn(subject) {
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
        Ok(query_handle) ->
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
            Ok(control_plane) ->
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
                      let poll =
                        poll_scheduler.start(fn(generation) {
                          dependencies.send_after(
                            subject,
                            0,
                            PollTick(generation),
                          )
                        })
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
                          poll: poll,
                          retry: retry_scheduler.new(),
                          registry: worker_registry.new(),
                          yaml_step_tokens: session_metrics.new(),
                          pending_claims: dict.new(),
                          pending_dispatch_validations: dict.new(),
                          pending_review_lane_preflights: dict.new(),
                          next_dispatch_validation_generation: 1,
                          recovery_by_issue: startup_recovery.recovery_by_issue,
                          effect_runner: effect_runner_handle,
                          effect_runner_monitor: effect_runner_monitor,
                          event_hub: event_hub,
                          control_server: control_plane.handle,
                          control_file_path: control_plane.control_file_path,
                          query_service: query_handle,
                          read_model: read_model.new(
                            daemon_id: daemon_identity.daemon_id,
                            boot_id: daemon_identity.boot_id,
                            ui_server_enabled: effective.ui_server.enabled,
                          ),
                          remote_client: None,
                          remote_client_monitor: None,
                          operator_paused: False,
                          last_operator_command_result: None,
                          shell_state_overrides_transition: False,
                          dependencies: dependencies,
                        )
                        |> apply_startup_recovery(startup_recovery)
                        |> apply_scheduled_startup_recovery(
                          startup_recovery.scheduled,
                        )
                        |> spawn_recovered_workflow_resumptions(
                          startup_recovery.workflow_resumptions,
                        )
                        |> refresh_read_model
                      process.send(subject, StartRemoteClient)
                      let selector =
                        process.new_selector()
                        |> process.select(subject)
                        |> process.select_specific_monitor(
                          effect_runner_monitor,
                          fn(down) { EffectRunnerDown(down) },
                        )
                        |> process.select_monitors(WorkerDown)
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
    })
    |> actor.on_message(handle_message)
  case actor.start(builder) {
    Ok(started) -> Ok(started)
    Error(actor.InitFailed(reason)) -> Error(decode_startup_error(reason))
    Error(_) -> Error(StartupError("daemon_start_failed", "actor start failed"))
  }
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

fn scheduled_projection_for_root(
  workspace_root: String,
) -> Result(projection.Projection, ledger.LedgerError) {
  use ledger_path <- result.try(ledger.path_for_workspace_root(workspace_root))
  ledger.load_projection(ledger_path)
}

fn apply_scheduled_startup_recovery(
  state: State,
  scheduled: startup_recovery.ScheduledRecovery,
) -> State {
  list.fold(scheduled.effects, state, fn(state, effect) {
    apply_scheduled_startup_effect(state, effect)
  })
}

fn apply_scheduled_startup_effect(
  state: State,
  effect: startup_recovery.ScheduledRecoveryEffect,
) -> State {
  case effect {
    startup_recovery.AppendLedger(record_bodies, failure_event) -> {
      append_ledger_bodies_best_effort(state, record_bodies, failure_event)
      state
    }
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

fn normalized_scheduled_attempt(attempt: Int) -> Int {
  case attempt <= 0 {
    True -> 1
    False -> attempt
  }
}

fn optional_string_or_default(
  value: Option(String),
  default: String,
) -> String {
  case value {
    Some(value) -> value
    None -> default
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

fn spawn_recovered_workflow_resumptions(
  state: State,
  resumptions: List(recovery.RecoveredWorkflowRun),
) -> State {
  list.fold(resumptions, state, fn(state, resumption) {
    spawn_recovered_workflow_resumption(state, resumption)
  })
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
  case runtime_bundle.select_workflow(bundle, recovered.issue) {
    Error(runtime_bundle.BundleError(code, _)) ->
      Error(yaml_worker_failure(code, Some(recovered.run_root), recovered.issue))
    Ok(#(_, dag)) ->
      case
        recovered_workflow_identity_matches(dag, bundle.orchestrator, recovered)
      {
        Error(err) ->
          Error(yaml_worker_failure(
            "workflow_recovery_invalid:workflow_fingerprint_failed:"
              <> startup_recovery.fingerprint_error_message(err),
            Some(recovered.run_root),
            recovered.issue,
          ))
        Ok(False) ->
          Error(yaml_worker_failure(
            "workflow_recovery_invalid:workflow_drift",
            Some(recovered.run_root),
            recovered.issue,
          ))
        Ok(True) -> {
          case workspace_profile.resolve(dag, bundle.orchestrator) {
            Error(_) ->
              Error(yaml_worker_failure(
                "workflow_recovery_invalid:workspace_profile_unavailable",
                Some(recovered.run_root),
                recovered.issue,
              ))
            Ok(profile) -> {
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
                    profile.name,
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
                  dag,
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
                Error(failure) ->
                  Error(yaml_workflow_failure(failure, recovered.issue))
              }
            }
          }
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

fn recovered_workflow_identity_matches(
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  recovered: recovery.RecoveredWorkflowRun,
) -> Result(Bool, workflow_fingerprint.FingerprintError) {
  case dag.id == recovered.workflow_id {
    False -> Ok(False)
    True ->
      case workflow_fingerprint.fingerprint_for_execution(dag, orchestrator) {
        Error(err) -> Error(err)
        Ok(fingerprint) -> Ok(fingerprint == recovered.workflow_fingerprint)
      }
  }
}

fn recovered_workspaces_to_prepared(
  workspaces: Dict(String, recovery.RecoveredWorkspaceSummary),
  profile_name: String,
  _orchestrator: config_types.OrchestratorConfig,
) -> Dict(String, workspace_run.PreparedStepWorkspace) {
  workspaces
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(workspace_name, workspace) = entry
    #(
      workspace_name,
      workspace_run.PreparedStepWorkspace(
        workflow_id: workspace.workflow_id,
        run_id: workspace.run_id,
        run_root: workspace.run_root,
        workflow_bundle_dir: "",
        attempt_index: workspace.attempt_index,
        workspace_name: workspace.workspace_name,
        path: workspace.path,
        source_workspace_name: workspace.source_workspace_name,
        source_workspace_path: workspace.source_workspace_path,
        workspace_profile: profile_name,
      ),
    )
  })
  |> dict.from_list
}

fn ledger_error_message(error: ledger.LedgerError) -> String {
  ledger.ledger_error_to_string(error)
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
  actor.continue(refresh_read_model(state))
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
  let state = case session_tokens.nonzero(direct_tokens) {
    True -> state
    False -> add_runtime_aggregate_tokens(state, child_tokens)
  }
  remove_yaml_step_tokens_for_run(state, run_id)
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
  |> add_runtime_aggregate_tokens(tokens)
  |> remove_yaml_step_tokens_for_run(run_id)
}

fn add_runtime_aggregate_tokens(
  state: State,
  tokens: session_tokens.TokenTotals,
) -> State {
  case session_tokens.nonzero(tokens) {
    False -> state
    True ->
      State(
        ..state,
        runtime: orchestrator_state.RuntimeState(
          ..state.runtime,
          aggregate_pi_totals: session_tokens.add(
            state.runtime.aggregate_pi_totals,
            tokens,
          ),
        ),
      )
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
  case message {
    PollTick(generation) ->
      continue_with_refreshed_state(poll_tick_shell(state, generation))
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
    YamlStepStarted(session_id, run_id) ->
      continue_with_refreshed_state(handle_yaml_step_started(
        state,
        session_id,
        run_id,
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
    AbortWorkerCommandTimedOut(operator_command, session_id, reply) -> {
      let #(state, result) =
        stop_session_for_operator(
          state,
          operator_command,
          session_id,
          session_reason.OperatorAbort,
        )
      process.send(reply, result)
      continue_with_refreshed_state(state)
    }
    WorkerDown(down) ->
      continue_with_refreshed_state(worker_lifecycle.worker_down_to_transition(
        worker_down_context(state),
        down,
      ))
    EffectRunnerDown(down) -> {
      let _shutdown_state = handle_effect_runner_down(state, down)
      actor.stop_abnormal("effect_runner_down")
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
    GetRemoteDispatchPaused(reply) -> {
      process.send(reply, state.operator_paused)
      actor.continue(state)
    }
    StartRemoteClient ->
      continue_with_refreshed_state(start_remote_client_now(state))
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
          process.send(reply, query_service.query(state.query_service, query))
          Nil
        })
      actor.continue(state)
    }
    Shutdown(reply) -> {
      let state =
        run_transition_messages(state, [
          transition_types.ShutdownRequested(True),
        ])
      log_state(state, "info", "daemon_shutdown", [])
      process.send(reply, Nil)
      actor.stop()
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
) -> State {
  let parent_session_id = parent_session_id_for_run(state, run_id)
  let registry =
    worker_registry.register_active_yaml_step_started(
      state.registry,
      session_id,
      run_id,
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
  case worker_lifecycle.worker_down_matches(state.workers, issue_id, handle) {
    False -> Nil
    True -> {
      append_workflow_interrupted_terminal(state, handle, "worker_down")
      worker_lifecycle.publish_worker_down(state.event_hub, handle.session_id)
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
      let _worker_down_failure_appended =
        append_ledger_bodies(
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
      Nil
    },
    begin_failure_report_request: begin_scheduled_failure_report_request,
    start_pending_scheduled_runs: start_pending_scheduled_runs,
  )
}

fn operator_command_reply(
  state: State,
  operator_command: command.OperatorCommand,
  timeout_ms: Int,
  reply: process.Subject(command.CommandResult),
) -> State {
  let #(state, result) =
    apply_operator_command_to_state(state, operator_command, timeout_ms)
  process.send(reply, result)
  state
}

fn apply_operator_command_to_state(
  state: State,
  operator_command: command.OperatorCommand,
  timeout_ms: Int,
) -> #(State, command.CommandResult) {
  let state = State(..state, last_operator_command_result: None)
  let request =
    transition_effects.OperatorCommandRequest(
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
  let result = case state.last_operator_command_result {
    Some(result) -> result
    None ->
      command.rejected(
        operator_command,
        "operator_command_result_missing",
        Some("operator command did not produce a result"),
      )
  }
  #(State(..state, last_operator_command_result: None), result)
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

fn apply_shell_operator_command(
  state: State,
  request: transition_effects.OperatorCommandRequest,
) -> #(State, command.CommandResult) {
  let #(state, result) =
    operator_runtime.apply_shell_operator_command(
      state,
      request,
      operator_runtime.shell_handlers(
        reload_workflow_for_operator: reload_workflow_for_operator,
        retry_workflow_step_for_operator: retry_workflow_step_for_operator,
        retry_artifact_publication_for_operator: retry_artifact_publication_for_operator,
        schedule_run_now_for_operator: schedule_run_now_for_operator,
        abort_session_for_operator_sync: abort_session_for_operator_sync,
        route_worker_command_sync: route_worker_command_sync,
        cleanup_orphan_steps_for_operator: cleanup_orphan_steps_for_operator,
      ),
    )
  #(State(..state, shell_state_overrides_transition: True), result)
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
        Ok(#(_run_id, issue_id, _issue_identifier)) ->
          case
            retry_step_issue_preflight(
              state,
              operator_command,
              target,
              issue_id,
            )
          {
            Error(result) -> #(state, result)
            Ok(issue) ->
              continue_retry_workflow_step_for_operator(
                state,
                operator_command,
                projection_state,
                target,
                step_id,
                issue,
              )
          }
      }
  }
}

fn retry_step_issue_preflight(
  state: State,
  operator_command: command.OperatorCommand,
  target: command.RetryWorkflowStepTarget,
  issue_id: String,
) -> Result(tracker_issue.Issue, command.CommandResult) {
  case
    dict.get(
      state.runtime.parked,
      orchestrator_state.linear_issue_id_identity(issue_id),
    )
  {
    Ok(parked) ->
      Error(command.rejected(
        operator_command,
        "issue_parked",
        Some(
          "issue is parked for "
          <> orchestrator_reason.park_to_string(parked.reason)
          <> "; unpark before retry-step",
        ),
      ))
    Error(Nil) ->
      case issue_is_active_or_pending(state, issue_id) {
        True ->
          Error(command.rejected(
            operator_command,
            "issue_already_active",
            Some("issue already has an active or pending workflow"),
          ))
        False ->
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
                False ->
                  case core.is_active(state.workflow.effective, issue.state) {
                    True -> Ok(issue)
                    False ->
                      Error(command.rejected(
                        operator_command,
                        "issue_state_drift:non_active_state",
                        Some(
                          "run "
                          <> command.retry_workflow_step_target_to_string(
                            target,
                          )
                          <> " for issue "
                          <> issue.identifier
                          <> " is currently in non-active state "
                          <> issue_state.to_string(issue.state)
                          <> "; move the issue to a configured active state before retry-step",
                        ),
                      ))
                  }
              }
          }
      }
  }
}

fn continue_retry_workflow_step_for_operator(
  state: State,
  operator_command: command.OperatorCommand,
  projection_state: projection.Projection,
  target: command.RetryWorkflowStepTarget,
  step_id: Option(String),
  issue: tracker_issue.Issue,
) -> #(State, command.CommandResult) {
  let observation =
    startup_recovery.current_workflow_observation(state.workflow.bundle, issue)
  case workflow_repair.plan(projection_state, target, step_id, observation) {
    Error(error) -> #(
      state,
      command.rejected(
        operator_command,
        workflow_repair.describe_error(error),
        workflow_repair.error_message(error),
      ),
    )
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
        Error(error) -> #(
          state,
          command.rejected(
            operator_command,
            recovery.describe_error(error),
            Some(recovery.describe_error(error)),
          ),
        )
        Ok(finalization) ->
          case finalization.resumptions {
            [resumption] -> {
              let bodies =
                list.append(
                  plan.records_to_append,
                  ledger_record_bodies(finalization.records_to_append),
                )
              case
                append_ledger_bodies(state, bodies, "retry_step_append_failed")
              {
                False -> #(
                  state,
                  command.rejected(
                    operator_command,
                    "ledger_append_failed",
                    Some("failed to append retry-step repair records"),
                  ),
                )
                True -> {
                  let state =
                    spawn_recovered_workflow_resumption(state, resumption)
                  #(
                    state,
                    command.applied(
                      operator_command,
                      Some(retry_step_applied_message(plan)),
                    ),
                  )
                }
              }
            }
            _ -> {
              let _best_effort_retry_step_rejection_diagnostic_appended =
                append_ledger_bodies(
                  state,
                  retry_step_rejection_diagnostic_bodies(finalization),
                  "retry_step_rejection_diagnostic_append_failed",
                )
              #(
                state,
                command.rejected(
                  operator_command,
                  rejection_reason_from_finalization(finalization),
                  rejection_message_from_finalization(finalization),
                ),
              )
            }
          }
      }
  }
}

fn replay_projection_for_operator(
  state: State,
) -> Result(projection.Projection, String) {
  use ledger_path <- result.try(
    ledger.path_for_workspace_root(state.workflow.effective.workspace.root)
    |> result.map_error(ledger_error_message),
  )
  use read <- result.try(
    ledger.read_records(ledger_path) |> result.map_error(ledger_error_message),
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
  let active_session_ids =
    worker_registry.active_yaml_step_sessions_for_run(state.registry, run_id)
  case active_session_ids {
    [] ->
      Ok(
        yaml_step_orphans.CleanupPlan(
          run_id: run_id,
          parent_state: parent_state,
          candidates: [],
        ),
      )
    _ ->
      case replay_projection_for_operator(state) {
        Error(_) -> Error(Nil)
        Ok(projected) ->
          Ok(yaml_step_orphans.CleanupPlan(
            run_id: run_id,
            parent_state: parent_state,
            candidates: yaml_step_orphans.unfinished_candidates(
              projected,
              run_id,
              active_session_ids,
            ),
          ))
      }
  }
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
) -> Bool {
  let bodies =
    yaml_step_orphans.interruption_records(
      plan.run_id,
      plan.candidates,
      "orphaned_parent_stopped",
    )
  let appended = case bodies {
    [] -> True
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
  appended
}

fn record_orphaned_yaml_children_after_parent_stop(
  state: State,
  run_id: String,
  issue_state_name: Option(String),
) -> State {
  case orphan_cleanup_plan_for_run(state, run_id, "stopping") {
    Error(Nil) -> state
    Ok(plan) -> {
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
  case record_orphaned_yaml_children_from_plan(state, plan, issue_state_name) {
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

fn issue_is_active_or_pending(state: State, issue_id: String) -> Bool {
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
  || dict.has_key(state.runtime.parked, identity)
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

fn rejection_message_from_finalization(
  finalization: recovery.WorkflowFinalization,
) -> Option(String) {
  case finalization.diagnostics {
    [diagnostic, ..] ->
      Some(
        "retry-step repair was rejected by recovery validation: "
        <> recovery.workflow_recovery_diagnostic_message(diagnostic),
      )
    [] -> Some("retry-step repair was rejected by recovery validation")
  }
}

fn retry_step_rejection_diagnostic_bodies(
  finalization: recovery.WorkflowFinalization,
) -> List(record.RecordBody) {
  finalization.diagnostics
  |> list.map(recovery.workflow_recovery_diagnostic_record_body)
}

fn rejection_reason_from_finalization(
  finalization: recovery.WorkflowFinalization,
) -> String {
  case finalization.diagnostics {
    [diagnostic, ..] -> recovery.workflow_recovery_diagnostic_reason(diagnostic)
    [] ->
      case finalization.records_to_append {
        [
          record.LedgerRecord(
            body: record.IssueParkedV2(reason: reason, ..),
            ..,
          ),
          ..
        ] -> reason
        [
          record.LedgerRecord(
            body: record.WorkflowRunInterrupted(reason: reason, ..),
            ..,
          ),
          ..
        ] -> reason
        _ -> "artifact_recovery_failed"
      }
  }
}

fn finish_operator_command_effect(
  state: State,
  _request: transition_effects.OperatorCommandRequest,
  result: command.CommandResult,
) -> #(State, List(transition_types.Message)) {
  let state = State(..state, last_operator_command_result: Some(result))
  log_operator_result(state, result, [])
  #(state, [])
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
) -> #(State, command.CommandResult) {
  let outcome = workflow_reloader.reload_now(state.workflow)
  let state = apply_workflow_reload_outcome(state, outcome)
  let reloaded = command.applied(operator_command, Some("workflow reloaded"))
  let failure_message = workflow_reloader.invalid_operator_message(outcome)
  case state.workflow.reload_state.current_status {
    config.CurrentValid -> #(state, reloaded)
    config.CurrentInvalid(reason) -> #(
      state,
      command.rejected(operator_command, reason, failure_message),
    )
  }
}

fn retry_artifact_publication_for_operator(
  state: State,
  operator_command: command.OperatorCommand,
  run_id: String,
  publication_id: Option(String),
) -> #(State, command.CommandResult) {
  let root = state.workflow.effective.workspace.root
  case
    ctl_artifact_publication_retry.retry_attempts_with_bundle_runner(
      root,
      run_id,
      publication_id,
      state.workflow.bundle,
      command_runner.production(),
    )
  {
    Ok(attempts) -> {
      let message = case attempts {
        [attempt] ->
          Some(
            "publication retry recorded "
            <> attempt.publication_id
            <> " as "
            <> attempt.status,
          )
        _ ->
          Some(
            "publication retry recorded "
            <> int.to_string(list.length(attempts))
            <> " attempt(s)",
          )
      }
      #(state, command.applied(operator_command, message))
    }
    Error(#(code, message)) -> {
      let result = case code {
        "publication_run_not_found" | "publication_not_found" ->
          command.not_found(operator_command, Some(message))
        _ -> command.rejected(operator_command, code, Some(message))
      }
      #(state, result)
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
      append_ledger_bodies_best_effort(
        state,
        [record.ScheduledJobDue(job.id, job.workflow, now_ms, run_id, "manual")],
        "scheduled_due_append_failed",
      )
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

fn route_worker_command_sync(
  state: State,
  operator_command: command.OperatorCommand,
  session_id: String,
  timeout_ms: Int,
  send: fn(
    process.Subject(worker_command.Command),
    process.Subject(worker_command.Reply),
  ) -> Nil,
) -> #(State, command.CommandResult) {
  let session_id = route_worker_command_session_id(state, session_id)
  case worker_for_session(state, session_id) {
    Error(Nil) ->
      route_step_command_sync(
        state,
        operator_command,
        session_id,
        timeout_ms,
        send,
      )
    Ok(handle) ->
      case handle.command_subject {
        None ->
          case
            worker_registry.step_command_subject_for_run(
              state.registry,
              handle.run_id,
            )
          {
            Error(worker_registry.NoActiveStepCommandSubject) -> #(
              state,
              command.not_allowed(
                operator_command,
                "worker_command_subject_unavailable",
                Some("session worker does not accept operator commands"),
              ),
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
            )
            Ok(subject) ->
              send_worker_command_sync(
                state,
                operator_command,
                timeout_ms,
                send,
                subject,
              )
          }
        Some(subject) ->
          send_worker_command_sync(
            state,
            operator_command,
            timeout_ms,
            send,
            subject,
          )
      }
  }
}

fn route_step_command_sync(
  state: State,
  operator_command: command.OperatorCommand,
  session_id: String,
  timeout_ms: Int,
  send: fn(
    process.Subject(worker_command.Command),
    process.Subject(worker_command.Reply),
  ) -> Nil,
) -> #(State, command.CommandResult) {
  case
    worker_registry.step_command_subject_for_session(state.registry, session_id)
  {
    Error(Nil) -> #(
      state,
      command.not_found(operator_command, Some("session not found")),
    )
    Ok(subject) ->
      send_worker_command_sync(
        state,
        operator_command,
        timeout_ms,
        send,
        subject,
      )
  }
}

fn send_worker_command_sync(
  state: State,
  operator_command: command.OperatorCommand,
  timeout_ms: Int,
  send: fn(
    process.Subject(worker_command.Command),
    process.Subject(worker_command.Reply),
  ) -> Nil,
  subject: process.Subject(worker_command.Command),
) -> #(State, command.CommandResult) {
  let worker_reply = process.new_subject()
  send(subject, worker_reply)
  case
    process.receive(
      worker_reply,
      within: control_command_handler.worker_command_timeout(timeout_ms),
    )
  {
    Ok(reply) -> #(
      state,
      control_command_handler.worker_reply_to_command_result(
        operator_command,
        reply,
      ),
    )
    Error(Nil) -> #(
      state,
      command.rejected(
        operator_command,
        "worker_command_timeout",
        Some("worker command timed out"),
      ),
    )
  }
}

fn abort_session_for_operator_sync(
  state: State,
  operator_command: command.OperatorCommand,
  session_id: String,
  timeout_ms: Int,
) -> #(State, command.CommandResult) {
  let session_id = route_worker_command_session_id(state, session_id)
  case worker_for_session(state, session_id) {
    Error(Nil) ->
      abort_step_session_for_operator_sync(
        state,
        operator_command,
        session_id,
        timeout_ms,
      )
    Ok(handle) ->
      case handle.command_subject {
        None ->
          stop_session_for_operator(
            state,
            operator_command,
            session_id,
            session_reason.OperatorAbort,
          )
        Some(subject) -> {
          let worker_reply = process.new_subject()
          process.send(subject, worker_command.Abort(worker_reply))
          case
            process.receive(
              worker_reply,
              within: control_command_handler.worker_command_timeout(timeout_ms),
            )
          {
            Ok(reply) -> #(
              state,
              control_command_handler.worker_reply_to_command_result(
                operator_command,
                reply,
              ),
            )
            Error(Nil) ->
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
}

fn abort_step_session_for_operator_sync(
  state: State,
  operator_command: command.OperatorCommand,
  session_id: String,
  timeout_ms: Int,
) -> #(State, command.CommandResult) {
  case
    worker_registry.step_command_subject_for_session(state.registry, session_id)
  {
    Error(Nil) -> #(
      state,
      command.not_found(operator_command, Some("session not found")),
    )
    Ok(subject) -> {
      let worker_reply = process.new_subject()
      process.send(subject, worker_command.Abort(worker_reply))
      case
        process.receive(
          worker_reply,
          within: control_command_handler.worker_command_timeout(timeout_ms),
        )
      {
        Ok(reply) -> #(
          state,
          control_command_handler.worker_reply_to_command_result(
            operator_command,
            reply,
          ),
        )
        Error(Nil) -> #(
          state,
          command.rejected(
            operator_command,
            "worker_command_timeout",
            Some("worker command timed out"),
          ),
        )
      }
    }
  }
}

fn stop_session_for_operator(
  state: State,
  operator_command: command.OperatorCommand,
  session_id: String,
  reason: session_reason.WorkerExitReason,
) -> #(State, command.CommandResult) {
  case worker_for_session(state, session_id) {
    Error(Nil) -> #(
      state,
      command.not_found(operator_command, Some("session not found")),
    )
    Ok(handle) -> {
      let reason_text = session_reason.to_string(reason)
      let state =
        record_orphaned_yaml_children_after_parent_stop(
          state,
          handle.run_id,
          Some(issue_state.to_string(handle.issue.state)),
        )
      let state =
        run_transition_messages(state, [
          transition_types.WorkerStopRequested(
            identity.session_id_from_string(session_id),
            reason,
            transition_lifecycle_context(state),
          ),
        ])
      #(state, command.applied(operator_command, Some(reason_text)))
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
                  case dict.get(state.runtime.completed, identity) {
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
  let completed = state.runtime.completed |> dict.values
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
  apply_workflow_reload_outcome(
    state,
    workflow_reloader.reload_if_changed(state.workflow),
  )
}

fn apply_workflow_reload_outcome(
  state: State,
  outcome: workflow_reloader.Outcome,
) -> State {
  case outcome {
    workflow_reloader.Unchanged(workflow) -> State(..state, workflow: workflow)
    workflow_reloader.Reloaded(workflow) ->
      apply_reloaded_workflow(state, workflow)
    workflow_reloader.Invalid(workflow, reason, message) -> {
      let state = State(..state, workflow: workflow)
      let fields = workflow_reloader.invalid_log_fields(reason, message)
      log_state(state, "warn", "workflow_reload_failed", fields)
      state
    }
  }
}

fn apply_reloaded_workflow(
  state: State,
  workflow: workflow_reloader.State,
) -> State {
  let effective = workflow.effective
  let runtime =
    orchestrator_state.RuntimeState(
      ..state.runtime,
      poll_interval_ms: effective.polling.interval_ms,
      max_concurrent_agents: effective.agent.max_concurrent_agents,
    )
  let tracker_adapter = state.dependencies.make_tracker_adapter(effective)
  let state =
    State(
      ..state,
      workflow: workflow,
      tracker_client: adapter_legacy.workflow_compat_client(tracker_adapter),
      tracker_adapter: tracker_adapter,
      runtime: runtime,
    )
  let state = refresh_scheduled_next_due_after_reload(state)
  let state = reconcile_remote_client_after_reload(state)
  log_state(state, "info", "workflow_reloaded", [])
  state
}

fn reconcile_remote_client_after_reload(state: State) -> State {
  let ui_server = state.workflow.effective.ui_server
  let state =
    State(
      ..state,
      read_model: read_model.update_ui_server_enabled(
        state.read_model,
        ui_server.enabled,
      ),
    )
  case ui_server.enabled {
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
  transition_state: transition_types.State,
) -> State {
  let state = case state.shell_state_overrides_transition {
    True -> State(..state, shell_state_overrides_transition: False)
    False ->
      State(
        ..state,
        runtime: transition_state.runtime,
        workers: transition_state.workers,
        pending_claims: transition_state.pending_claims,
        pending_dispatch_validations: transition_state.pending_dispatch_validations,
        pending_review_lane_preflights: transition_state.pending_review_lane_preflights,
        next_dispatch_validation_generation: transition_state.next_dispatch_validation_generation,
        shell_state_overrides_transition: False,
      )
  }
  state
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
    max_messages: daemon_transition_shell.default_message_limit(),
    handlers: transition_shell_handlers(),
  )
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
    claim_issue: fn(state, task_ref, issue, workspace_path, run_id) {
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
    },
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
      case append_outbox_attempt(state, intent) {
        True ->
          enqueue_side_effect(
            state,
            effect_runner.ReplayOutbox(
              outbox: outbox_replay,
              comments: state.tracker_adapter.comments,
              state_transitions: state.tracker_adapter.state_transitions,
            ),
          )
        False -> state
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

fn transition_append_ledger(
  state: State,
  request: transition_effects.LedgerAppend,
) -> #(State, Result(Nil, ledger.LedgerError)) {
  let bodies = ledger_batch.to_bodies(request.batch)
  case bodies {
    [] -> #(state, Ok(Nil))
    _ ->
      case
        ledger.path_for_workspace_root(state.workflow.effective.workspace.root)
      {
        Error(err) -> {
          log_state(state, "error", request.failure_event, [
            #("error", ledger_error_message(err)),
          ])
          #(state, Error(err))
        }
        Ok(ledger_path) ->
          case
            ledger.append_many(
              ledger_path,
              ledger_records_for_bodies(state.dependencies.now_ms(), bodies),
              True,
            )
          {
            Ok(Nil) -> #(state, Ok(Nil))
            Error(err) -> {
              log_state(state, "error", request.failure_event, [
                #("error", ledger_error_message(err)),
              ])
              #(state, Error(err))
            }
          }
      }
  }
}

fn transition_start_worker(
  state: State,
  request: transition_effects.WorkerStart,
) -> #(State, Result(Nil, String)) {
  let run_id = identity.run_id_to_string(request.run_id)
  let session_id = identity.session_id_to_string(request.session_id)
  #(
    worker_lifecycle.spawn_worker(
      worker_spawn_context(state, request.issue, run_id, session_id),
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
  case append_parked_record(state, parked, reason_text) {
    False -> state
    True ->
      enqueue_parked_entry_report(state, parked, reason_text, source_run_id)
  }
}

fn append_parked_record(
  state: State,
  parked: orchestrator_state.ParkedEntry,
  reason_text: String,
) -> Bool {
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
    ) -> {
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
      state
    }
    scheduled_runtime.RecordScheduledPending(pending) -> {
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
      state
    }
    scheduled_runtime.RecordScheduledSkipped(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      reason,
      skipped_count,
    ) -> {
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
      state
    }
    scheduled_runtime.RecordScheduledPendingBlocked(pending, blocked_at_ms) -> {
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
      state
    }
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
        True -> {
          let _retry_scheduled_appended =
            append_ledger_bodies(
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
          state
        }
        False -> state
      }
    }
    scheduled_runtime.PromoteRetryToPending(_) -> state
    scheduled_runtime.RetryReport(job_id, run_id) ->
      retry_scheduled_failure_report_by_identity(state, job_id, run_id)
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
    scheduled_retry_timers: dict.insert(
      state.scheduled_retry_timers,
      run_id,
      timer,
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
    scheduled_report_retry_timers: dict.insert(
      state.scheduled_report_retry_timers,
      run_id,
      timer,
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
  state: State,
  pending: transition_types.PendingClaim,
) -> Result(record.RecordBody, String) {
  case runtime_bundle.select_workflow(state.workflow.bundle, pending.issue) {
    Error(runtime_bundle.BundleError(code, message)) ->
      Error(code <> ":" <> message)
    Ok(#(_, dag)) -> {
      use fingerprint <- result_try_string(
        workflow_fingerprint.fingerprint_for_execution(
          dag,
          state.workflow.bundle.orchestrator,
        )
        |> result.replace_error("workflow_fingerprint_failed"),
      )
      use run_root <- result_try_string(
        workspace_run.run_root_for(
          pending.issue,
          dag.id,
          pending.run_id,
          state.workflow.bundle.orchestrator,
        )
        |> result.map_error(fn(err) { error.workspace_code(err) }),
      )
      Ok(record.WorkflowRunStartedWithTask(
        pending.run_id,
        dag.id,
        fingerprint,
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
        run_root,
      ))
    }
  }
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
) -> worker_lifecycle.WorkerSpawnContext(State) {
  let subject = state.subject
  let dependencies = state.dependencies
  let tracker_client = state.tracker_client
  let bundle = state.workflow.bundle
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
            bundle,
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
      checkpoint: checkpoint_writer_with_corrupt_ledger_fallback(
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

fn checkpoint_writer_with_corrupt_ledger_fallback(
  workspace_root: String,
  now_ms: fn() -> Int,
) -> workflow_checkpoint.Writer {
  case ledger.path_for_workspace_root(workspace_root) {
    Ok(ledger_path) ->
      case ledger.load_projection(ledger_path) {
        Ok(_) -> workflow_checkpoint.ledger_writer(workspace_root, now_ms)
        Error(ledger.CorruptRecord(..)) -> workflow_checkpoint.noop_writer()
        Error(_) -> workflow_checkpoint.ledger_writer(workspace_root, now_ms)
      }
    Error(_) -> workflow_checkpoint.ledger_writer(workspace_root, now_ms)
  }
}

fn run_workflow_worker(
  issue: tracker_issue.Issue,
  run_id: String,
  bundle: runtime_bundle.RuntimeBundle,
  tracker_client: tracker.Client,
  secrets: List(String),
  workflow_dependencies: workflow_run.Dependencies,
  daemon_subject: process.Subject(Message),
  event_hub: process.Subject(hub.Message),
  session_id: String,
  now_ms: fn() -> Int,
) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
  case runtime_bundle.select_workflow(bundle, issue) {
    Error(runtime_bundle.BundleError(code, _)) ->
      Error(yaml_worker_failure(code, None, issue))
    Ok(#(_, dag)) -> {
      let workflow_dependencies =
        workflow_run.Dependencies(
          ..workflow_dependencies,
          checkpoint: workflow_checkpoint.ledger_writer(
            bundle.effective.workspace.root,
            now_ms,
          ),
        )
      case
        workflow_run.execute(
          issue,
          dag,
          bundle.orchestrator,
          tracker_client,
          secrets,
          run_id,
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
  }
}

fn yaml_step_callbacks(
  daemon_subject: process.Subject(Message),
) -> yaml_workflow_lifecycle.LifecycleCallbacks {
  yaml_workflow_lifecycle.LifecycleCallbacks(
    step_started: fn(session_id, run_id) {
      process.send(daemon_subject, YamlStepStarted(session_id, run_id))
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
) -> Nil {
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
    Ok(job) ->
      begin_scheduled_failure_report_for_job(
        state,
        job,
        workflow_id,
        due_at_ms,
        run_id,
        attempt,
        reason,
        run_root,
        session_id,
      )
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
  session_id: Option(String),
) -> State {
  let task_config = job.on_failure.task
  case task_config.enabled, task_config.state {
    False, _ -> state
    True, None -> {
      log_state(state, "warn", "scheduled_failure_report_skipped", [
        #("job_id", job.id),
        #("run_id", run_id),
        #("reason", "missing_triage_state"),
      ])
      state
    }
    True, Some(triage_state) -> {
      let #(runtime, generation) =
        scheduled_runtime.reserve_report_generation(state.scheduled_runtime)
      let state = State(..state, scheduled_runtime: runtime)
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
          dedupe_key: scheduled_failure_dedupe_key(job.id),
          title: "Scheduled workflow failure: " <> job.id,
          body: reason,
          labels: task_config.labels,
          target_state_name: Some(triage_state),
          previous_task_remote_id: scheduled_failure_issue_id_for_state(
            state,
            job.id,
          ),
        )
      case state.tracker_adapter.scheduled_failures {
        Some(capability) ->
          enqueue_side_effect(
            state,
            effect_runner.ReportScheduledFailure(
              generation: generation,
              publication: publication,
              capability: capability,
            ),
          )
        None -> state
      }
    }
  }
}

fn scheduled_failure_dedupe_key(job_id: String) -> String {
  "scheduled-job:" <> job_id
}

fn scheduled_failure_issue_id_for_state(
  state: State,
  job_id: String,
) -> Option(String) {
  case scheduled_projection_for_root(state.workflow.effective.workspace.root) {
    Error(err) -> {
      log_state(
        state,
        "warn",
        "scheduled_failure_issue_projection_unavailable",
        [
          #("job_id", job_id),
          #("error", ledger_error_message(err)),
        ],
      )
      None
    }
    Ok(projected) ->
      case projection.scheduled_status_for(projected, job_id) {
        Ok(status) -> status.failure_issue_id
        Error(Nil) -> None
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
  generation: Int,
  publication: adapter.ScheduledFailurePublication,
  result: Result(adapter.ScheduledFailureReceipt, error.TrackerError),
) -> State {
  case result {
    Ok(receipt) ->
      handle_scheduled_failure_report_success(state, publication, receipt)
    Error(err) ->
      handle_scheduled_failure_report_failure(
        state,
        generation,
        publication,
        err,
      )
  }
}

fn handle_scheduled_failure_report_success(
  state: State,
  publication: adapter.ScheduledFailurePublication,
  receipt: adapter.ScheduledFailureReceipt,
) -> State {
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
      scheduled_report_retry_timers: dict.delete(
        state.scheduled_report_retry_timers,
        publication.run_id,
      ),
    )
  append_ledger_bodies_best_effort(
    state,
    [
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
  state
}

fn handle_scheduled_failure_report_failure(
  state: State,
  generation: Int,
  publication: adapter.ScheduledFailurePublication,
  err: error.TrackerError,
) -> State {
  let #(runtime, delay_ms, actions) =
    scheduled_runtime.schedule_report_retry_after_failure(
      state.scheduled_runtime,
      publication.job_id,
      publication.run_id,
      generation,
      scheduled_runtime.default_max_backoff_ms(),
    )
  let next_retry_at_ms = state.dependencies.now_ms() + delay_ms
  let state = State(..state, scheduled_runtime: runtime)
  log_state(state, "warn", "scheduled_failure_report_failed", [
    #("job_id", publication.job_id),
    #("run_id", publication.run_id),
    #("error", error.tracker_code(err)),
  ])
  append_ledger_bodies_best_effort(
    state,
    [
      record.ScheduledFailureReportFailed(
        publication.job_id,
        publication.workflow_id,
        publication.due_at_ms,
        publication.run_id,
        publication.attempt,
        publication.dedupe_key,
        error.tracker_code(err),
        tracker_error_message(err),
        next_retry_at_ms,
        generation,
      ),
    ],
    "scheduled_failure_report_failed_append_failed",
  )
  apply_scheduled_runtime_actions(state, actions, append_retry_record: True)
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
) -> State {
  case scheduled_projection_for_root(state.workflow.effective.workspace.root) {
    Error(err) -> {
      log_state(state, "warn", "scheduled_report_retry_projection_unavailable", [
        #("job_id", job_id),
        #("run_id", run_id),
        #("error", ledger_error_message(err)),
      ])
      state
    }
    Ok(projected) ->
      case projection.scheduled_status_for(projected, job_id) {
        Error(Nil) -> state
        Ok(status) ->
          case scheduled_job_by_id(state, job_id), status.current_run {
            Ok(job), Some(run) ->
              begin_scheduled_failure_report_for_job(
                state,
                job,
                status.workflow_id,
                run.due_at_ms,
                run_id,
                normalized_scheduled_attempt(run.attempt),
                optional_string_or_default(
                  status.last_failure_reason,
                  "scheduled failure",
                ),
                run.run_root,
                run.session_id,
              )
            _, _ -> state
          }
      }
  }
}

fn tracker_error_message(err: error.TrackerError) -> String {
  case err {
    error.LinearApiRequest(message) -> message
    error.LinearApiStatus(status) ->
      "Linear API status " <> int.to_string(status)
    error.LinearGraphqlErrors(message) -> message
    error.LinearUnknownPayload(message) -> message
    error.LinearMissingEndCursor -> "missing Linear pagination cursor"
    error.LinearUploadStatus(status) ->
      "Linear upload status " <> int.to_string(status)
    error.LinearAttachmentError(message) -> message
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
) -> Nil {
  case workflow_id_for_handle(state, handle) {
    Error(Nil) -> {
      log_state(state, "warn", "workflow_terminal_append_skipped", [
        #("issue_id", handle.issue_id),
        #("run_id", handle.run_id),
        #("reason", "workflow_id_unavailable"),
      ])
      Nil
    }
    Ok(workflow_id) -> {
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
}

fn workflow_id_for_handle(
  state: State,
  handle: worker_registry.WorkerHandle,
) -> Result(String, Nil) {
  case workflow_id_from_projection(state, handle.run_id) {
    Ok(workflow_id) -> Ok(workflow_id)
    Error(Nil) ->
      case runtime_bundle.select_workflow(state.workflow.bundle, handle.issue) {
        Ok(#(_, dag)) -> Ok(dag.id)
        Error(runtime_bundle.BundleError(_, _)) -> Error(Nil)
      }
  }
}

fn workflow_id_from_projection(
  state: State,
  run_id: String,
) -> Result(String, Nil) {
  case ledger.path_for_workspace_root(state.workflow.effective.workspace.root) {
    Error(_) -> Error(Nil)
    Ok(ledger_path) ->
      case ledger.load_projection(ledger_path) {
        Error(_) -> Error(Nil)
        Ok(projection) ->
          case dict.get(projection.workflow_runs, run_id) {
            Ok(status) -> Ok(workflow_id_from_status(status))
            Error(Nil) -> Error(Nil)
          }
      }
  }
}

fn workflow_id_from_status(status: projection.WorkflowRunStatus) -> String {
  case status {
    projection.WorkflowRunActive(workflow_id: workflow_id, ..)
    | projection.WorkflowRunFinished(workflow_id: workflow_id, ..)
    | projection.WorkflowRunInterrupted(workflow_id: workflow_id, ..)
    | projection.WorkflowRunSuperseded(workflow_id: workflow_id, ..) ->
      workflow_id
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

fn effect_runner_down_fields(down: process.Down) -> List(log.Field) {
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
  let state = case result {
    Ok(Nil) -> state
    Error(err) -> append_outbox_failure(state, outbox, err)
  }
  let task_identity =
    orchestrator_state.issue_id_identity_for_backend(
      issue_id,
      state.tracker_adapter.kind,
    )
  run_transition_messages(state, [
    transition_types.HandoffClaimCompleted(
      task_identity,
      identity.issue_id_from_string(issue_id),
      identity.run_id_from_string(run_id),
      handoff_claim_result_for_transition(
        state,
        outbox,
        issue_id,
        run_id,
        result,
      ),
    ),
  ])
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
  case workflow_run_started_body_for_claim(state, pending) {
    Error(reason) -> transition_types.HandoffClaimStartRecordFailed(reason)
    Ok(workflow_started_body) -> {
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
  }
}

fn handle_handoff_success_finished(
  state: State,
  outbox: outbox_effects.Intent,
  issue_id: String,
  result: Result(Nil, error.TrackerError),
) -> State {
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
  let state = append_outbox_result(state, outbox, result)
  case result {
    Ok(effect_runner.InvalidWorkflowReportNoop) -> {
      log_state(state, "info", "invalid_workflow_report_noop", [
        #("issue_id", issue_id),
        #("violation_fingerprint", violation_fingerprint),
      ])
      let runtime =
        core.mark_invalid_workflow_report_result(
          state.runtime,
          issue_id,
          violation_fingerprint,
          reporting_policy_fingerprint,
          "noop",
        )
      State(..state, runtime: runtime)
    }
    Ok(outcome) -> {
      log_state(state, "info", "invalid_workflow_reported", [
        #("issue_id", issue_id),
        #("violation_fingerprint", violation_fingerprint),
        #("outcome", invalid_workflow_outcome_to_string(outcome)),
      ])
      let runtime =
        core.mark_invalid_workflow_report_result(
          state.runtime,
          issue_id,
          violation_fingerprint,
          reporting_policy_fingerprint,
          "reported",
        )
      State(..state, runtime: runtime)
    }
    Error(err) -> {
      log_state(state, "warn", "invalid_workflow_report_failed", [
        #("issue_id", issue_id),
        #("violation_fingerprint", violation_fingerprint),
        #("error", error.tracker_code(err)),
      ])
      let runtime =
        core.mark_invalid_workflow_report_result(
          state.runtime,
          issue_id,
          violation_fingerprint,
          reporting_policy_fingerprint,
          "failed",
        )
      State(..state, runtime: runtime)
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

fn handle_outbox_replay_finished(
  state: State,
  outbox_replay: recovery.OutboxReplay,
  result: Result(Nil, error.TrackerError),
) -> State {
  let recovery.OutboxReplay(outbox_id, task_ref, outbox_kind, _, _) =
    outbox_replay
  let intent = outbox_effects.recovered_intent(outbox_replay)
  case result {
    Ok(Nil) -> {
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
  case append_outbox_attempt(state, intent) {
    True -> enqueue_side_effect(state, make_effect(intent))
    False -> state
  }
}

fn append_outbox_attempt(state: State, intent: outbox_effects.Intent) -> Bool {
  append_ledger_bodies(
    state,
    [
      outbox_effects.pending_body(intent),
      outbox_effects.attempted_body(intent, 1),
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
    Ok(_) -> {
      append_ledger_bodies_best_effort(
        state,
        [outbox_effects.completed_body(intent)],
        "outbox_ledger_append_failed",
      )
      state
    }
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
  state
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
) -> Nil {
  case append_ledger_bodies(state, bodies, event) {
    True -> Nil
    False -> Nil
  }
}

fn append_ledger_bodies(
  state: State,
  bodies: List(record.RecordBody),
  event: String,
) -> Bool {
  case bodies {
    [] -> True
    _ ->
      case
        ledger.path_for_workspace_root(state.workflow.effective.workspace.root)
      {
        Error(err) -> {
          log_state(state, "error", event, [
            #("error", ledger_error_message(err)),
          ])
          False
        }
        Ok(ledger_path) ->
          case
            ledger.append_many(
              ledger_path,
              ledger_records_for_bodies(state.dependencies.now_ms(), bodies),
              True,
            )
          {
            Ok(Nil) -> True
            Error(err) -> {
              log_state(state, "error", event, [
                #("error", ledger_error_message(err)),
              ])
              False
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

fn result_try_string(
  result: Result(a, String),
  next: fn(a) -> Result(b, String),
) -> Result(b, String) {
  case result {
    Ok(value) -> next(value)
    Error(reason) -> Error(reason)
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
  case ledger.path_for_workspace_root(state.workflow.effective.workspace.root) {
    Error(err) ->
      log_state(state, "warn", "workflow_shutdown_projection_unavailable", [
        #("error", ledger_error_message(err)),
      ])
    Ok(ledger_path) ->
      case ledger.load_projection(ledger_path) {
        Error(err) ->
          log_state(state, "warn", "workflow_shutdown_projection_unavailable", [
            #("error", ledger_error_message(err)),
          ])
        Ok(projection) -> {
          let bodies =
            worker_registry.worker_handles(state.registry)
            |> list.fold([], fn(bodies, handle) {
              list.append(
                shutdown_step_attempt_interruption_bodies(
                  projection,
                  handle.run_id,
                ),
                bodies,
              )
            })
          case bodies {
            [] -> Nil
            _ ->
              append_ledger_bodies_best_effort(
                state,
                bodies,
                "workflow_shutdown_interrupt_append_failed",
              )
          }
        }
      }
  }
}

fn shutdown_step_attempt_interruption_bodies(
  projection: projection.Projection,
  run_id: String,
) -> List(record.RecordBody) {
  projection.step_attempts
  |> dict.values
  |> list.fold([], fn(bodies, status) {
    case status {
      projection.StepAttemptPending(
        run_id: status_run_id,
        workflow_id: workflow_id,
        step_id: step_id,
        attempt_index: attempt_index,
        ..,
      ) ->
        case status_run_id == run_id {
          True -> [
            record.StepAttemptInterrupted(
              run_id,
              workflow_id,
              step_id,
              attempt_index,
              "daemon_shutdown",
            ),
            ..bodies
          ]
          False -> bodies
        }
      projection.StepAttemptRunning(
        run_id: status_run_id,
        workflow_id: workflow_id,
        step_id: step_id,
        attempt_index: attempt_index,
        ..,
      ) ->
        case status_run_id == run_id {
          True -> [
            record.StepAttemptInterrupted(
              run_id,
              workflow_id,
              step_id,
              attempt_index,
              "daemon_shutdown",
            ),
            ..bodies
          ]
          False -> bodies
        }
      _ -> bodies
    }
  })
}

fn shutdown_runtime_shell(state: State, stop_effect_runner: Bool) -> State {
  process.demonitor_process(state.effect_runner_monitor)
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
          remote_command_runtime.kill(handle)
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
