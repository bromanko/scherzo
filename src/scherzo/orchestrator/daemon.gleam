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
import scherzo/config
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/control/file as control_file
import scherzo/control/linear_parser
import scherzo/control/linear_transport
import scherzo/control/server as control_server
import scherzo/error
import scherzo/log
import scherzo/orchestrator/control_command_handler
import scherzo/orchestrator/core
import scherzo/orchestrator/effect_runner
import scherzo/orchestrator/effects/interpreter as transition_interpreter
import scherzo/orchestrator/effects/types as transition_effects
import scherzo/orchestrator/event_publisher
import scherzo/orchestrator/poll_scheduler
import scherzo/orchestrator/reason as orchestrator_reason
import scherzo/orchestrator/retry_scheduler
import scherzo/orchestrator/schedule_core.{next_due_after_persisted_due}
import scherzo/orchestrator/state as orchestrator_state
import scherzo/orchestrator/transition_runner
import scherzo/orchestrator/transition_types
import scherzo/orchestrator/worker_registry
import scherzo/orchestrator/workflow_reloader
import scherzo/orchestrator/yaml_step_session
import scherzo/runtime_bundle
import scherzo/session/event as session_event
import scherzo/session/hub
import scherzo/session/name as session_name
import scherzo/session/reason as session_reason
import scherzo/session/recovery as session_recovery
import scherzo/session/tokens as session_tokens
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/step_artifact
import scherzo/task
import scherzo/tracker
import scherzo/tracker/adapter
import scherzo/tracker/adapter_legacy
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/linear_adapter
import scherzo/tracker/state as issue_state
import scherzo/workflow_attempt
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
  YamlStepFinished(String)
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
  ApplyOperatorCommand(
    command.OperatorCommand,
    Int,
    process.Subject(command.CommandResult),
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

type ScheduledPendingStart {
  ScheduledPendingStart(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    trigger: String,
    requested_at_ms: Int,
    attempt: Int,
    blocking_reason: String,
  )
}

type ScheduledRetryStart {
  ScheduledRetryStart(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    next_attempt: Int,
    generation: Int,
    timer: TimerHandle,
  )
}

type ScheduledReportRetryStart {
  ScheduledReportRetryStart(
    job_id: String,
    run_id: String,
    generation: Int,
    timer: TimerHandle,
  )
}

type StartupRecovery {
  StartupRecovery(
    runtime: orchestrator_state.RuntimeState,
    retry_timers: List(recovery.RecoveredRetry),
    cleanup_workspaces: List(recovery.CleanupRequest),
    outbox_to_replay: List(recovery.OutboxReplay),
    park_reports: List(adapter.ParkReport),
    command_receipts: Dict(String, projection.CommandReceiptState),
    recovery_by_issue: Dict(String, session_event.RecoveryInfo),
    warnings: List(String),
    workflow_resumptions: List(recovery.RecoveredWorkflowRun),
  )
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
  )
}

type State {
  State(
    subject: process.Subject(Message),
    workflow: workflow_reloader.State,
    tracker_client: tracker.Client,
    tracker_adapter: adapter.TrackerAdapter,
    linear_command_state: linear_transport.TransportState,
    pending_linear_command_acks: Dict(
      String,
      transition_types.PendingLinearCommandAck,
    ),
    in_flight_linear_command_acks: Dict(String, Bool),
    scheduled_next_due: Dict(String, Int),
    pending_scheduled_starts: Dict(String, ScheduledPendingStart),
    scheduled_retries: Dict(String, ScheduledRetryStart),
    next_scheduled_retry_generation: Int,
    scheduled_report_retries: Dict(String, ScheduledReportRetryStart),
    next_scheduled_report_generation: Int,
    runtime: orchestrator_state.RuntimeState,
    workers: transition_types.WorkerDirectory,
    poll: poll_scheduler.State(TimerHandle),
    retry: retry_scheduler.State(TimerHandle),
    registry: worker_registry.Registry,
    pending_claims: Dict(String, transition_types.PendingClaim),
    pending_dispatch_validations: Dict(
      String,
      transition_types.PendingDispatchValidation,
    ),
    next_dispatch_validation_generation: Int,
    recovery_by_issue: Dict(String, session_event.RecoveryInfo),
    effect_runner: effect_runner.Handle,
    effect_runner_monitor: process.Monitor,
    event_hub: process.Subject(hub.Message),
    control_server: ControlServerHandle,
    control_file_path: Option(String),
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
    remote_commands_enabled: effective.linear_commands.enabled,
    remote_commands_config_path: Some("remote_commands.enabled"),
    handoff_comments_enabled: handoff_comments_enabled(effective.handoff),
    handoff_state_moves_enabled: handoff_state_moves_enabled(effective.handoff),
    handoff_config_path: Some("handoff.states"),
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
  }
}

fn handoff_state_moves_enabled(handoff: config_types.HandoffConfig) -> Bool {
  handoff.enabled
  && {
    option_is_some(handoff.claim_state_id)
    || option_is_some(handoff.success_state_id)
    || option_is_some(handoff.failure_state_id)
    || option_is_some(handoff.completion_states)
  }
}

fn option_is_some(value: Option(a)) -> Bool {
  case value {
    Some(_) -> True
    None -> False
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
    case job.on_failure.linear.enabled {
      True -> Ok("scheduled_jobs." <> job.id <> ".on_failure")
      False -> Error(Nil)
    }
  })
}

fn start_control_plane(
  dependencies: RuntimeDependencies,
  effective: config_types.EffectiveConfig,
  event_hub: process.Subject(hub.Message),
  daemon_subject: process.Subject(Message),
  secrets: List(String),
) -> Result(ControlPlane, StartupError) {
  use token <- try_startup(dependencies.make_control_token())
  let settings = control_server.default_settings(token)
  use handle <- try_startup(dependencies.start_control_server(
    settings,
    control_backend(event_hub, daemon_subject),
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

fn control_backend(
  event_hub: process.Subject(hub.Message),
  daemon_subject: process.Subject(Message),
) -> control_server.Backend {
  let read_backend = control_server.event_hub_store(event_hub)
  control_server.Backend(
    ..read_backend,
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
  use startup_recovery <- try_startup(load_startup_recovery(
    bundle,
    tracker_adapter,
    dependencies,
    secrets,
  ))
  let linear_command_state =
    linear_transport.new_state_with_receipts(
      dependencies.now_ms(),
      startup_recovery.command_receipts,
    )
  let runtime = startup_recovery.runtime
  use event_hub <- try_startup(dependencies.start_event_hub() |> map_hub_error)
  let builder =
    actor.new_with_initialiser(10_000, fn(subject) {
      case
        start_control_plane(
          dependencies,
          effective,
          event_hub,
          subject,
          secrets,
        )
      {
        Error(err) -> Error(encode_startup_error(err))
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
                      dependencies.send_after(subject, 0, PollTick(generation))
                    })
                  let state =
                    State(
                      subject: subject,
                      workflow: workflow,
                      tracker_client: tracker_client,
                      tracker_adapter: tracker_adapter,
                      linear_command_state: linear_command_state,
                      pending_linear_command_acks: dict.new(),
                      in_flight_linear_command_acks: dict.new(),
                      scheduled_next_due: initial_scheduled_next_due(
                        workflow.bundle,
                        dependencies.now_ms(),
                        dependencies,
                        workflow.secrets,
                      ),
                      pending_scheduled_starts: dict.new(),
                      scheduled_retries: dict.new(),
                      next_scheduled_retry_generation: 1,
                      scheduled_report_retries: dict.new(),
                      next_scheduled_report_generation: 1,
                      runtime: runtime,
                      workers: transition_types.new_worker_directory(),
                      poll: poll,
                      retry: retry_scheduler.new(),
                      registry: worker_registry.new(),
                      pending_claims: dict.new(),
                      pending_dispatch_validations: dict.new(),
                      next_dispatch_validation_generation: 1,
                      recovery_by_issue: startup_recovery.recovery_by_issue,
                      effect_runner: effect_runner_handle,
                      effect_runner_monitor: effect_runner_monitor,
                      event_hub: event_hub,
                      control_server: control_plane.handle,
                      control_file_path: control_plane.control_file_path,
                      operator_paused: False,
                      last_operator_command_result: None,
                      shell_state_overrides_transition: False,
                      dependencies: dependencies,
                    )
                    |> apply_startup_recovery(startup_recovery)
                    |> recover_scheduled_runtime_state
                    |> spawn_recovered_workflow_resumptions(
                      startup_recovery.workflow_resumptions,
                    )
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

fn load_startup_recovery(
  bundle: runtime_bundle.RuntimeBundle,
  tracker_adapter: adapter.TrackerAdapter,
  dependencies: RuntimeDependencies,
  secrets: List(String),
) -> Result(StartupRecovery, StartupError) {
  let effective = bundle.effective
  use ledger_path <- try_startup(
    ledger.path_for_workspace_root(effective.workspace.root)
    |> map_ledger_error("ledger_path_failed"),
  )
  use replayed <- try_startup(
    ledger.replay(ledger_path)
    |> map_ledger_error("ledger_replay_failed"),
  )
  case replayed.truncated_tail {
    True ->
      emit_runtime_log(
        dependencies,
        "warn",
        "ledger_truncated_tail_ignored",
        [],
        secrets,
      )
    False -> Nil
  }
  use refreshed_issues <- try_startup(fetch_recovery_issue_states(
    tracker_adapter,
    recovery.known_issue_ids(replayed.projection),
  ))
  use recovery_plan <- try_startup(
    recovery.plan(
      replayed.projection,
      effective,
      refreshed_issues,
      dependencies.now_ms(),
    )
    |> map_recovery_error,
  )
  let workflow_candidates = recovery.workflow_candidates(replayed.projection)
  let observations =
    workflow_recovery_observations(
      bundle,
      workflow_candidates,
      refreshed_issues,
    )
  use workflow_finalization <- try_startup(
    recovery.finalize_workflow_candidates_with_config(
      replayed.projection,
      workflow_candidates,
      observations,
      artifact_store.new(effective.workspace.root),
      dependencies.now_ms(),
      effective,
    )
    |> map_recovery_error,
  )
  let records_to_append =
    list.append(
      recovery_plan.records_to_append,
      workflow_finalization.records_to_append,
    )
  use Nil <- try_startup(
    ledger.append_many(ledger_path, records_to_append, True)
    |> map_ledger_error("ledger_recovery_append_failed"),
  )
  Ok(StartupRecovery(
    runtime: recovery_plan.runtime,
    retry_timers: recovery_plan.retry_timers,
    cleanup_workspaces: recovery_plan.cleanup_workspaces,
    outbox_to_replay: recovery_plan.outbox_to_replay,
    park_reports: startup_park_reports(records_to_append),
    command_receipts: replayed.projection.command_receipts,
    recovery_by_issue: startup_recovery_by_issue(
      replayed.projection,
      recovery_plan,
    ),
    warnings: list.append(
      recovery_plan.warnings,
      workflow_finalization.warnings,
    ),
    workflow_resumptions: workflow_finalization.resumptions,
  ))
}

fn initial_scheduled_next_due(
  bundle: runtime_bundle.RuntimeBundle,
  now_ms: Int,
  dependencies: RuntimeDependencies,
  secrets: List(String),
) -> Dict(String, Int) {
  let projection = case
    scheduled_projection_for_root(bundle.effective.workspace.root)
  {
    Ok(projected) -> Some(projected)
    Error(err) -> {
      emit_runtime_log(
        dependencies,
        "warn",
        "scheduled_next_due_projection_unavailable",
        [#("error", ledger_error_message(err))],
        secrets,
      )
      None
    }
  }
  bundle.orchestrator.scheduled_jobs
  |> list.filter(fn(job) { job.enabled })
  |> list.fold(dict.new(), fn(acc, job) {
    let next_due = case projection {
      Some(projected) ->
        case projection.scheduled_status_for(projected, job.id) {
          Ok(status) ->
            case status.last_due_at_ms {
              Some(due_at_ms) ->
                next_due_after_persisted_due(due_at_ms, now_ms, job.every_ms)
              None -> schedule_core.initial_next_due(now_ms, job.every_ms)
            }
          Error(Nil) -> schedule_core.initial_next_due(now_ms, job.every_ms)
        }
      None -> schedule_core.initial_next_due(now_ms, job.every_ms)
    }
    dict.insert(acc, job.id, next_due)
  })
}

fn scheduled_projection_for_root(
  workspace_root: String,
) -> Result(projection.Projection, ledger.LedgerError) {
  use ledger_path <- result.try(ledger.path_for_workspace_root(workspace_root))
  ledger.load_projection(ledger_path)
}

fn recover_scheduled_runtime_state(state: State) -> State {
  case scheduled_projection_for_root(state.workflow.effective.workspace.root) {
    Error(err) -> {
      log_state(
        state,
        "warn",
        "scheduled_runtime_recovery_projection_unavailable",
        [
          #("error", ledger_error_message(err)),
        ],
      )
      state
    }
    Ok(projected) ->
      projected
      |> projection.scheduled_statuses
      |> list.fold(state, fn(state, status) {
        recover_scheduled_status(state, status)
      })
  }
}

fn recover_scheduled_status(
  state: State,
  status: projection.ScheduledJobStatus,
) -> State {
  case scheduled_job_by_id(state, status.job_id), status.current_run {
    Ok(job), Some(run) ->
      case job.enabled {
        False -> recover_disabled_scheduled_run(state, status, run)
        True -> recover_enabled_scheduled_run(state, job, status, run)
      }
    Error(Nil), Some(run) -> recover_disabled_scheduled_run(state, status, run)
    _, None -> state
  }
}

fn recover_enabled_scheduled_run(
  state: State,
  job: config_types.ScheduledJobConfig,
  status: projection.ScheduledJobStatus,
  run: projection.ScheduledRunSummary,
) -> State {
  case status.state {
    projection.ScheduledDuePending
    | projection.ScheduledPaused
    | projection.ScheduledWaitingForGlobalSlot ->
      State(
        ..state,
        pending_scheduled_starts: dict.insert(
          state.pending_scheduled_starts,
          job.id,
          ScheduledPendingStart(
            job_id: job.id,
            workflow_id: job.workflow,
            due_at_ms: run.due_at_ms,
            run_id: run.run_id,
            trigger: run.trigger,
            requested_at_ms: state.dependencies.now_ms(),
            attempt: case run.attempt <= 0 {
              True -> 1
              False -> run.attempt
            },
            blocking_reason: optional_string_or_default(run.reason, ""),
          ),
        ),
      )
    projection.ScheduledActive ->
      recover_interrupted_scheduled_run(state, job, run)
    projection.ScheduledRetryWaiting ->
      recover_scheduled_retry_waiting(state, job, run)
    projection.ScheduledReportRetryWaiting ->
      recover_scheduled_report_retry_waiting(state, job, status)
    projection.ScheduledIdle
    | projection.ScheduledTerminalSuccess
    | projection.ScheduledTerminalFailure -> state
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

fn recover_scheduled_retry_waiting(
  state: State,
  job: config_types.ScheduledJobConfig,
  run: projection.ScheduledRunSummary,
) -> State {
  let generation = state.next_scheduled_retry_generation
  let timer =
    state.dependencies.send_after(
      state.subject,
      0,
      ScheduledRetryTick(run.run_id, generation),
    )
  State(
    ..state,
    scheduled_retries: dict.insert(
      state.scheduled_retries,
      run.run_id,
      ScheduledRetryStart(
        job_id: job.id,
        workflow_id: job.workflow,
        due_at_ms: run.due_at_ms,
        run_id: run.run_id,
        next_attempt: normalized_scheduled_attempt(run.attempt),
        generation: generation,
        timer: timer,
      ),
    ),
    next_scheduled_retry_generation: generation + 1,
  )
}

fn recover_scheduled_report_retry_waiting(
  state: State,
  job: config_types.ScheduledJobConfig,
  status: projection.ScheduledJobStatus,
) -> State {
  case status.report_retry {
    None -> state
    Some(report_retry) -> {
      let delay_ms = case
        report_retry.next_retry_at_ms <= state.dependencies.now_ms()
      {
        True -> 0
        False -> report_retry.next_retry_at_ms - state.dependencies.now_ms()
      }
      let timer =
        state.dependencies.send_after(
          state.subject,
          delay_ms,
          ScheduledReportRetryTick(report_retry.run_id, report_retry.generation),
        )
      State(
        ..state,
        scheduled_report_retries: dict.insert(
          state.scheduled_report_retries,
          report_retry.run_id,
          ScheduledReportRetryStart(
            job_id: job.id,
            run_id: report_retry.run_id,
            generation: report_retry.generation,
            timer: timer,
          ),
        ),
        next_scheduled_report_generation: case
          state.next_scheduled_report_generation > report_retry.generation
        {
          True -> state.next_scheduled_report_generation
          False -> report_retry.generation + 1
        },
      )
    }
  }
}

fn recover_disabled_scheduled_run(
  state: State,
  status: projection.ScheduledJobStatus,
  run: projection.ScheduledRunSummary,
) -> State {
  case status.state {
    projection.ScheduledDuePending
    | projection.ScheduledPaused
    | projection.ScheduledWaitingForGlobalSlot -> {
      append_ledger_bodies_best_effort(
        state,
        [
          record.ScheduledRunPendingCancelled(
            status.job_id,
            status.workflow_id,
            run.due_at_ms,
            run.run_id,
            "job_disabled",
            state.dependencies.now_ms(),
          ),
        ],
        "scheduled_recovery_append_failed",
      )
      state
    }
    projection.ScheduledActive ->
      recover_interrupted_scheduled_run_for_status(state, status, run)
    projection.ScheduledRetryWaiting -> {
      let _retry_cancel_appended =
        append_ledger_bodies(
          state,
          [
            record.ScheduledRunRetryCancelled(
              status.job_id,
              run.run_id,
              0,
              "job_disabled",
            ),
          ],
          "scheduled_recovery_append_failed",
        )
      state
    }
    _ -> state
  }
}

fn recover_interrupted_scheduled_run(
  state: State,
  job: config_types.ScheduledJobConfig,
  run: projection.ScheduledRunSummary,
) -> State {
  let attempt = normalized_scheduled_attempt(run.attempt)
  let next_attempt = attempt + 1
  let retry_exhausted =
    schedule_core.retry_exhausted(
      next_attempt,
      state.workflow.effective.agent.max_retry_attempts,
    )
  let _interrupted_failure_appended =
    append_ledger_bodies(
      state,
      [
        record.ScheduledRunFailed(
          job.id,
          job.workflow,
          run.due_at_ms,
          run.run_id,
          attempt,
          state.dependencies.now_ms(),
          "daemon_restart",
          retry_exhausted,
          run.run_root,
        ),
      ],
      "scheduled_recovery_append_failed",
    )
  case retry_exhausted {
    True ->
      begin_scheduled_failure_report(
        state,
        job.id,
        job.workflow,
        run.due_at_ms,
        run.run_id,
        attempt,
        "daemon_restart",
        run.run_root,
        run.session_id,
      )
    False ->
      schedule_scheduled_retry_for_run(
        state,
        job.id,
        job.workflow,
        run.due_at_ms,
        run.run_id,
        attempt,
        next_attempt,
        "daemon_restart",
      )
  }
}

fn recover_interrupted_scheduled_run_for_status(
  state: State,
  status: projection.ScheduledJobStatus,
  run: projection.ScheduledRunSummary,
) -> State {
  let _disabled_interrupted_failure_appended =
    append_ledger_bodies(
      state,
      [
        record.ScheduledRunFailed(
          status.job_id,
          status.workflow_id,
          run.due_at_ms,
          run.run_id,
          normalized_scheduled_attempt(run.attempt),
          state.dependencies.now_ms(),
          "daemon_restart",
          True,
          run.run_root,
        ),
      ],
      "scheduled_recovery_append_failed",
    )
  state
}

fn scheduled_job_by_id(
  state: State,
  job_id: String,
) -> Result(config_types.ScheduledJobConfig, Nil) {
  state.workflow.bundle.orchestrator.scheduled_jobs
  |> list.find(fn(job) { job.id == job_id })
}

fn startup_park_reports(
  records: List(record.LedgerRecord),
) -> List(adapter.ParkReport) {
  let run_ids = startup_park_report_run_ids(records)
  startup_park_reports_loop(records, run_ids, [], [])
}

fn startup_park_reports_loop(
  records: List(record.LedgerRecord),
  run_ids: Dict(String, String),
  seen_issue_ids: List(String),
  reports: List(adapter.ParkReport),
) -> List(adapter.ParkReport) {
  case records {
    [] -> list.reverse(reports)
    [ledger_record, ..rest] ->
      case ledger_record.body {
        record.IssueParked(issue_id, issue_identifier, reason_text, _) ->
          add_startup_park_report(
            rest,
            run_ids,
            seen_issue_ids,
            reports,
            issue_id,
            issue_identifier,
            reason_text,
            None,
          )
        record.IssueParkedV2(
          issue_id,
          issue_identifier,
          reason_text,
          release_policy,
          _,
          _,
        ) ->
          add_startup_park_report(
            rest,
            run_ids,
            seen_issue_ids,
            reports,
            issue_id,
            issue_identifier,
            reason_text,
            Some(release_policy),
          )
        _ -> startup_park_reports_loop(rest, run_ids, seen_issue_ids, reports)
      }
  }
}

fn add_startup_park_report(
  rest: List(record.LedgerRecord),
  run_ids: Dict(String, String),
  seen_issue_ids: List(String),
  reports: List(adapter.ParkReport),
  issue_id: String,
  issue_identifier: String,
  reason_text: String,
  release_policy: Option(String),
) -> List(adapter.ParkReport) {
  case list.contains(seen_issue_ids, issue_id) {
    True -> startup_park_reports_loop(rest, run_ids, seen_issue_ids, reports)
    False ->
      startup_park_reports_loop(rest, run_ids, [issue_id, ..seen_issue_ids], [
        adapter.ParkReport(
          task: task.TaskRef(
            backend_kind: "linear",
            remote_id: issue_id,
            key: Some(issue_identifier),
            url: None,
          ),
          issue_identifier: issue_identifier,
          reason: reason_text,
          release_policy: release_policy,
          run_id: optional_run_id(run_ids, issue_id),
        ),
        ..reports
      ])
  }
}

fn startup_park_report_run_ids(
  records: List(record.LedgerRecord),
) -> Dict(String, String) {
  list.fold(records, dict.new(), fn(run_ids, ledger_record) {
    case ledger_record.body {
      record.RunInterrupted(run_id, issue_id, _) ->
        insert_run_id_if_missing(run_ids, issue_id, run_id)
      record.WorkflowRunInterrupted(run_id, _, issue_id, _) ->
        insert_run_id_if_missing(run_ids, issue_id, run_id)
      record.IssueCounterUpdated(issue_id, _, _, _, _, Some(run_id)) ->
        insert_run_id_if_missing(run_ids, issue_id, run_id)
      _ -> run_ids
    }
  })
}

fn insert_run_id_if_missing(
  run_ids: Dict(String, String),
  issue_id: String,
  run_id: String,
) -> Dict(String, String) {
  case string.trim(run_id) == "" || dict.has_key(run_ids, issue_id) {
    True -> run_ids
    False -> dict.insert(run_ids, issue_id, run_id)
  }
}

fn optional_run_id(
  run_ids: Dict(String, String),
  issue_id: String,
) -> Option(String) {
  case dict.get(run_ids, issue_id) {
    Ok(run_id) -> Some(run_id)
    Error(Nil) -> None
  }
}

fn workflow_recovery_observations(
  bundle: runtime_bundle.RuntimeBundle,
  candidates: List(recovery.WorkflowRecoveryCandidate),
  refreshed_issues: List(tracker_issue.Issue),
) -> Dict(String, recovery.CurrentWorkflowObservation) {
  let issue_by_id =
    refreshed_issues
    |> list.map(fn(issue) { #(issue.id, issue) })
    |> dict.from_list
  candidates
  |> list.map(fn(candidate) {
    let observation = case dict.get(issue_by_id, candidate.issue_id) {
      Error(Nil) -> recovery.IssueUnavailable
      Ok(issue) -> current_workflow_observation(bundle, issue)
    }
    #(candidate.run_id, observation)
  })
  |> dict.from_list
}

fn current_workflow_observation(
  bundle: runtime_bundle.RuntimeBundle,
  issue: tracker_issue.Issue,
) -> recovery.CurrentWorkflowObservation {
  case runtime_bundle.select_workflow(bundle, issue) {
    Error(runtime_bundle.BundleError(code, message)) ->
      recovery.WorkflowUnavailable(code <> ":" <> message)
    Ok(#(_, dag)) ->
      case
        workflow_fingerprint.fingerprint_for_execution(dag, bundle.orchestrator)
      {
        Error(err) ->
          recovery.WorkflowUnavailable(
            "workflow_fingerprint_failed:" <> fingerprint_error_message(err),
          )
        Ok(fingerprint) ->
          recovery.CurrentWorkflow(
            issue,
            dag.id,
            fingerprint,
            core.issue_fingerprint(issue),
            dag,
            bundle.effective.workspace.root,
          )
      }
  }
}

fn fingerprint_error_message(
  err: workflow_fingerprint.FingerprintError,
) -> String {
  case err {
    workflow_fingerprint.PromptFileReadFailed(path) ->
      "prompt_file_read_failed:" <> path
    workflow_fingerprint.UnsupportedWorkflowShape(reason) ->
      "unsupported_workflow_shape:" <> reason
    workflow_fingerprint.WorkspaceProfileUnavailable(profile_name) ->
      "workspace_profile_unavailable:" <> profile_name
  }
}

fn fetch_recovery_issue_states(
  tracker_adapter: adapter.TrackerAdapter,
  issue_ids: List(String),
) -> Result(List(tracker_issue.Issue), StartupError) {
  fetch_recovery_issue_chunks(tracker_adapter, chunk_strings(issue_ids, 50), [])
}

fn fetch_recovery_issue_chunks(
  tracker_adapter: adapter.TrackerAdapter,
  chunks: List(List(String)),
  acc: List(tracker_issue.Issue),
) -> Result(List(tracker_issue.Issue), StartupError) {
  case chunks {
    [] -> Ok(list.reverse(acc))
    [chunk, ..rest] ->
      case
        adapter_legacy.refresh_runtime_issues_by_ids(tracker_adapter, chunk)
      {
        Ok(issues) ->
          fetch_recovery_issue_chunks(
            tracker_adapter,
            rest,
            list.append(list.reverse(issues), acc),
          )
        Error(err) ->
          Error(StartupError(
            "recovery_issue_fetch_failed",
            adapter_legacy.adapter_error_message(err),
          ))
      }
  }
}

fn chunk_strings(values: List(String), size: Int) -> List(List(String)) {
  case values {
    [] -> []
    _ -> {
      let size = case size <= 0 {
        True -> 1
        False -> size
      }
      let chunk = list.take(values, size)
      let rest = list.drop(values, size)
      [chunk, ..chunk_strings(rest, size)]
    }
  }
}

fn startup_recovery_by_issue(
  projection: projection.Projection,
  recovery_plan: recovery.RecoveryPlan,
) -> Dict(String, session_event.RecoveryInfo) {
  dict.new()
  |> insert_interrupted_recovery(projection)
  |> insert_recovered_retry_recovery(recovery_plan.retry_timers)
  |> insert_parked_recovery(projection)
  |> insert_cleanup_recovery(recovery_plan.cleanup_workspaces)
}

fn insert_interrupted_recovery(
  acc: Dict(String, session_event.RecoveryInfo),
  projection: projection.Projection,
) -> Dict(String, session_event.RecoveryInfo) {
  projection.runs
  |> dict.to_list
  |> list.fold(acc, fn(acc, entry) {
    let #(run_id, status) = entry
    case session_recovery.interrupted_run(run_id, status, None) {
      Some(info) -> dict.insert(acc, issue_id_for_run_status(status), info)
      None -> acc
    }
  })
}

fn insert_recovered_retry_recovery(
  acc: Dict(String, session_event.RecoveryInfo),
  retries: List(recovery.RecoveredRetry),
) -> Dict(String, session_event.RecoveryInfo) {
  list.fold(retries, acc, fn(acc, retry) {
    let recovery.RecoveredRetry(issue_id, _, _, _, reason) = retry
    insert_if_missing(
      acc,
      issue_id,
      session_recovery.recovered("recovery.recovered_retry", Some(reason)),
    )
  })
}

fn insert_parked_recovery(
  acc: Dict(String, session_event.RecoveryInfo),
  projection: projection.Projection,
) -> Dict(String, session_event.RecoveryInfo) {
  projection.parked_issues
  |> dict.to_list
  |> list.fold(acc, fn(acc, entry) {
    let #(issue_id, parked) = entry
    dict.insert(acc, issue_id, session_recovery.parked_issue(parked))
  })
}

fn insert_cleanup_recovery(
  acc: Dict(String, session_event.RecoveryInfo),
  cleanups: List(recovery.CleanupRequest),
) -> Dict(String, session_event.RecoveryInfo) {
  list.fold(cleanups, acc, fn(acc, cleanup) {
    let recovery.CleanupRequest(issue_id, _, _) = cleanup
    dict.insert(acc, issue_id, session_recovery.cleanup_request(cleanup))
  })
}

fn insert_if_missing(
  acc: Dict(String, session_event.RecoveryInfo),
  issue_id: String,
  info: session_event.RecoveryInfo,
) -> Dict(String, session_event.RecoveryInfo) {
  case dict.has_key(acc, issue_id) {
    True -> acc
    False -> dict.insert(acc, issue_id, info)
  }
}

fn issue_id_for_run_status(status: projection.RunStatus) -> String {
  case status {
    projection.RunRunning(issue_id, ..)
    | projection.RunInterrupted(issue_id, ..)
    | projection.RunFinished(issue_id, ..) -> issue_id
  }
}

fn apply_startup_recovery(
  state: State,
  startup_recovery: StartupRecovery,
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
      let handle =
        worker_registry.WorkerHandle(
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
            recovered.issue.id,
            worker_entry,
          ),
          by_session: dict.insert(
            state.workers.by_session,
            session_id,
            recovered.issue.id,
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
              <> fingerprint_error_message(err),
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
                    daemon_subject,
                    event_hub,
                    now_ms,
                  ),
                  resume,
                )
              {
                Ok(success) -> Ok(success.worker_success)
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

fn map_ledger_error(
  result: Result(a, ledger.LedgerError),
  code: String,
) -> Result(a, StartupError) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) -> Error(StartupError(code, ledger_error_message(err)))
  }
}

fn ledger_error_message(error: ledger.LedgerError) -> String {
  case error {
    ledger.Io(message) -> message
    ledger.LedgerFfiFailed(error) -> ledger.ledger_ffi_error_to_string(error)
    ledger.UnsupportedVersion(version) ->
      "unsupported ledger schema version " <> int.to_string(version)
    ledger.CorruptRecord(line, reason) ->
      "corrupt ledger record at line " <> int.to_string(line) <> ": " <> reason
  }
}

fn map_recovery_error(
  result: Result(a, recovery.RecoveryError),
) -> Result(a, StartupError) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) ->
      Error(StartupError(
        "startup_recovery_failed",
        recovery.describe_error(err),
      ))
  }
}

fn handle_message(
  state: State,
  message: Message,
) -> actor.Next(State, Message) {
  case message {
    PollTick(generation) -> actor.continue(poll_tick_shell(state, generation))
    RetryTick(issue_id, generation) ->
      actor.continue(
        run_transition_messages(state, [
          transition_types.RetryTick(
            issue_id,
            generation,
            transition_dispatch_context(state),
          ),
        ]),
      )
    WorkerFinished(issue_id, run_id, result) ->
      actor.continue(worker_finished_to_transition(
        state,
        issue_id,
        run_id,
        result,
      ))
    ScheduledWorkerFinished(run_id, result) ->
      actor.continue(handle_scheduled_worker_finished(state, run_id, result))
    ScheduledRetryTick(run_id, generation) ->
      actor.continue(handle_scheduled_retry_tick(state, run_id, generation))
    ScheduledReportRetryTick(run_id, generation) ->
      actor.continue(handle_scheduled_report_retry_tick(
        state,
        run_id,
        generation,
      ))
    WorkerUpdate(issue_id, update) ->
      actor.continue(handle_worker_update(state, issue_id, update))
    WorkerCommandReady(issue_id, run_id, command_subject) ->
      actor.continue(handle_worker_command_ready(
        state,
        issue_id,
        run_id,
        command_subject,
      ))
    YamlStepStarted(session_id, run_id) ->
      actor.continue(handle_yaml_step_started(state, session_id, run_id))
    YamlStepUpdate(session_id, update) -> {
      event_publisher.worker_update(state.event_hub, session_id, update)
      log_yaml_step_update(state, session_id, update)
      actor.continue(state)
    }
    YamlStepCommandReady(session_id, command_subject) ->
      actor.continue(handle_yaml_step_command_ready(
        state,
        session_id,
        command_subject,
      ))
    YamlStepFinished(session_id) ->
      actor.continue(handle_yaml_step_finished(state, session_id))
    AbortWorkerCommandTimedOut(operator_command, session_id, reply) -> {
      let #(state, result) =
        stop_session_for_operator(
          state,
          operator_command,
          session_id,
          session_reason.OperatorAbort,
        )
      process.send(reply, result)
      actor.continue(state)
    }
    WorkerDown(down) -> actor.continue(worker_down_to_transition(state, down))
    EffectRunnerDown(down) -> {
      let _shutdown_state = handle_effect_runner_down(state, down)
      actor.stop_abnormal("effect_runner_down")
    }
    SideEffectCompleted(completion) ->
      actor.continue(handle_side_effect_completed(state, completion))
    GetSnapshot(reply) -> {
      effect_runner.reply_snapshot(state.runtime, reply)
      actor.continue(state)
    }
    ApplyOperatorCommand(operator_command, timeout_ms, reply) ->
      actor.continue(operator_command_reply(
        state,
        operator_command,
        timeout_ms,
        reply,
      ))
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
  run_transition_messages(state, [
    transition_types.YamlStepStarted(session_id, run_id),
  ])
}

fn handle_yaml_step_finished(state: State, session_id: String) -> State {
  run_transition_messages(state, [transition_types.YamlStepFinished(session_id)])
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
  State(
    ..state,
    registry: worker_registry.delete_yaml_step_sessions(
      state.registry,
      session_ids,
    ),
  )
}

fn handle_registry_down_resolution(
  state: State,
  resolution: worker_registry.DownResolution,
) -> State {
  case resolution {
    worker_registry.UnknownDown(registry) ->
      run_transition_messages(State(..state, registry: registry), [
        transition_types.WorkerDown(
          transition_types.UnknownWorkerDown,
          transition_lifecycle_context(state),
        ),
      ])
    worker_registry.StepCommandDown(registry, session_id) -> {
      log_state(state, "warn", "yaml_step_command_down", [
        #("session_id", session_id),
      ])
      State(..state, registry: registry)
    }
    worker_registry.WorkerDown(registry, issue_id, handle) -> {
      let state = State(..state, registry: registry)
      append_workflow_interrupted_terminal(state, handle, "worker_down")
      event_publisher.lifecycle(
        state.event_hub,
        handle.session_id,
        session_event.WorkerDown,
        None,
      )
      run_transition_messages(state, [
        transition_types.WorkerDown(
          transition_types.KnownWorkerDown(
            issue_id,
            handle.run_id,
            handle.session_id,
          ),
          transition_lifecycle_context(state),
        ),
      ])
    }
    worker_registry.WorkerDownStale(registry, issue_id) ->
      run_transition_messages(State(..state, registry: registry), [
        transition_types.WorkerDown(
          transition_types.WorkerDownStale(issue_id),
          transition_lifecycle_context(state),
        ),
      ])
    worker_registry.ScheduledWorkerDown(registry, run_id, handle) -> {
      let state = State(..state, registry: registry)
      log_state(state, "warn", "scheduled_worker_down", [
        #("job_id", handle.job_id),
        #("run_id", run_id),
      ])
      event_publisher.lifecycle(
        state.event_hub,
        handle.session_id,
        session_event.WorkerDown,
        None,
      )
      hub.finish_session(
        state.event_hub,
        handle.session_id,
        session_reason.Failed,
      )
      let next_attempt = handle.attempt + 1
      let retry_exhausted =
        schedule_core.retry_exhausted(
          next_attempt,
          state.workflow.effective.agent.max_retry_attempts,
        )
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
              "worker_down",
              retry_exhausted,
              Some(handle.run_root),
            ),
          ],
          "scheduled_worker_down_append_failed",
        )
      let state = case retry_exhausted {
        True -> state
        False ->
          schedule_scheduled_retry(
            state,
            handle,
            handle.due_at_ms,
            next_attempt,
            "worker_down",
          )
      }
      start_pending_scheduled_runs(state)
    }
    worker_registry.ScheduledWorkerDownStale(registry, _run_id) -> {
      log_state(state, "warn", "scheduled_worker_down_stale", [])
      State(..state, registry: registry)
    }
  }
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
  case operator_command {
    command.RetryIssue(issue_ref) | command.ParkIssue(issue_ref, _) ->
      case issue_for_ref(state, issue_ref) {
        Ok(issue) -> transition_types.OperatorIssueResolved(issue)
        Error(command.NotFound) -> transition_types.OperatorIssueNotFound
        Error(command.Rejected(reason)) ->
          transition_types.OperatorIssueRejected(reason)
        Error(command.NotAllowed(reason)) ->
          transition_types.OperatorIssueNotAllowed(reason)
        Error(command.Applied) | Error(command.Queued) ->
          transition_types.OperatorIssueResolutionFailed
      }
    command.PauseDispatch
    | command.ResumeDispatch
    | command.ReloadWorkflow
    | command.RetryWorkflowStep(_, _)
    | command.UnparkIssue(_)
    | command.AbortSession(_)
    | command.StopAfterCurrentTurn(_)
    | command.PromptSession(_, _)
    | command.RespondUi(_, _, _)
    | command.RunScheduleNow(_) -> transition_types.OperatorIssueNotResolved
  }
}

fn parked_issue_resolution(
  state: State,
  operator_command: command.OperatorCommand,
) -> transition_types.ParkedIssueResolution {
  case operator_command {
    command.UnparkIssue(issue_ref) ->
      case parked_issue_id_for_ref(state, issue_ref) {
        Ok(issue_id) -> transition_types.ParkedIssueResolved(issue_id)
        Error(command.NotFound) -> transition_types.ParkedIssueNotFound
        Error(command.Rejected(reason)) ->
          transition_types.ParkedIssueRejected(reason)
        Error(command.NotAllowed(reason)) ->
          transition_types.ParkedIssueNotAllowed(reason)
        Error(command.Applied) | Error(command.Queued) ->
          transition_types.ParkedIssueResolutionFailed
      }
    command.PauseDispatch
    | command.ResumeDispatch
    | command.ReloadWorkflow
    | command.RetryIssue(_)
    | command.RetryWorkflowStep(_, _)
    | command.ParkIssue(_, _)
    | command.AbortSession(_)
    | command.StopAfterCurrentTurn(_)
    | command.PromptSession(_, _)
    | command.RespondUi(_, _, _)
    | command.RunScheduleNow(_) -> transition_types.ParkedIssueNotResolved
  }
}

fn apply_shell_operator_command(
  state: State,
  request: transition_effects.OperatorCommandRequest,
) -> #(State, command.CommandResult) {
  let operator_command = request.operator_command
  let #(state, result) = case operator_command {
    command.ReloadWorkflow ->
      reload_workflow_for_operator(state, operator_command)
    command.RetryWorkflowStep(target, step_id) ->
      retry_workflow_step_for_operator(state, operator_command, target, step_id)
    command.RunScheduleNow(job_id) ->
      schedule_run_now_for_operator(state, operator_command, job_id)
    command.AbortSession(session_id) ->
      abort_session_for_operator_sync(
        state,
        operator_command,
        session_id,
        request.timeout_ms,
      )
    command.StopAfterCurrentTurn(session_id) ->
      route_worker_command_sync(
        state,
        operator_command,
        session_id,
        request.timeout_ms,
        fn(subject, reply) {
          process.send(subject, worker_command.StopAfterCurrentTurn(reply))
        },
      )
    command.PromptSession(session_id, message) ->
      route_worker_command_sync(
        state,
        operator_command,
        session_id,
        request.timeout_ms,
        fn(subject, reply) {
          process.send(subject, worker_command.QueuePrompt(message, reply))
        },
      )
    command.RespondUi(session_id, request_id, response) ->
      route_worker_command_sync(
        state,
        operator_command,
        session_id,
        request.timeout_ms,
        fn(subject, reply) {
          process.send(
            subject,
            worker_command.RespondToUi(request_id, response, reply),
          )
        },
      )
    command.PauseDispatch
    | command.ResumeDispatch
    | command.RetryIssue(_)
    | command.ParkIssue(_, _)
    | command.UnparkIssue(_) ->
      case request.source {
        transition_effects.RemoteOperatorCommand(_, _, _, _, _) ->
          apply_operator_command_to_state(
            state,
            operator_command,
            request.timeout_ms,
          )
        transition_effects.LocalOperatorCommand -> #(
          state,
          command.rejected(
            operator_command,
            "operator_command_already_handled",
            None,
          ),
        )
      }
  }
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
          case issue_is_active_or_pending(state, issue_id) {
            True -> #(
              state,
              command.rejected(
                operator_command,
                "issue_already_active",
                Some("issue already has an active or pending workflow"),
              ),
            )
            False ->
              case dict.get(state.runtime.parked, issue_id) {
                Ok(parked) -> #(
                  state,
                  command.rejected(
                    operator_command,
                    "issue_parked",
                    Some(
                      "issue is parked for "
                      <> orchestrator_reason.park_to_string(parked.reason)
                      <> "; unpark before retry-step",
                    ),
                  ),
                )
                Error(Nil) ->
                  case issue_for_id(state, issue_id) {
                    Error(status) -> #(
                      state,
                      command.result_for(operator_command, status, None),
                    )
                    Ok(issue) -> {
                      let observation =
                        current_workflow_observation(
                          state.workflow.bundle,
                          issue,
                        )
                      case
                        workflow_repair.plan(
                          projection_state,
                          target,
                          step_id,
                          observation,
                        )
                      {
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
                            recovery.finalize_workflow_candidates_with_config(
                              projection_state,
                              [plan.candidate],
                              dict.from_list([
                                #(plan.run_id, observation),
                              ]),
                              artifact_store.new(
                                state.workflow.effective.workspace.root,
                              ),
                              state.dependencies.now_ms(),
                              config_types.with_additional_active_state(
                                state.workflow.effective,
                                issue.state,
                              ),
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
                                      ledger_record_bodies(
                                        finalization.records_to_append,
                                      ),
                                    )
                                  case
                                    append_ledger_bodies(
                                      state,
                                      bodies,
                                      "retry_step_append_failed",
                                    )
                                  {
                                    False -> #(
                                      state,
                                      command.rejected(
                                        operator_command,
                                        "ledger_append_failed",
                                        Some(
                                          "failed to append retry-step repair records",
                                        ),
                                      ),
                                    )
                                    True -> {
                                      let state =
                                        spawn_recovered_workflow_resumption(
                                          state,
                                          resumption,
                                        )
                                      #(
                                        state,
                                        command.applied(
                                          operator_command,
                                          Some(
                                            "retrying run "
                                            <> plan.run_id
                                            <> " step "
                                            <> plan.selected_step_id
                                            <> " at attempt "
                                            <> int.to_string(
                                              plan.next_attempt_index,
                                            ),
                                          ),
                                        ),
                                      )
                                    }
                                  }
                                }
                                _ ->
                                  case
                                    append_ledger_bodies(
                                      state,
                                      retry_step_rejection_diagnostic_bodies(
                                        finalization,
                                      ),
                                      "retry_step_rejection_diagnostic_append_failed",
                                    )
                                  {
                                    False | True -> #(
                                      state,
                                      command.rejected(
                                        operator_command,
                                        rejection_reason_from_finalization(
                                          finalization,
                                        ),
                                        rejection_message_from_finalization(
                                          finalization,
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

fn issue_is_active_or_pending(state: State, issue_id: String) -> Bool {
  has_active_run(state, issue_id)
  || dict.has_key(state.runtime.running, issue_id)
  || dict.has_key(state.pending_claims, issue_id)
  || dict.has_key(state.pending_dispatch_validations, issue_id)
}

fn ledger_record_bodies(
  records: List(record.LedgerRecord),
) -> List(record.RecordBody) {
  records
  |> list.map(fn(ledger_record) { ledger_record.body })
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
  request: transition_effects.OperatorCommandRequest,
  result: command.CommandResult,
) -> #(State, List(transition_types.Message)) {
  let state = State(..state, last_operator_command_result: Some(result))
  case request.source {
    transition_effects.LocalOperatorCommand -> {
      log_operator_result(state, result, [])
      #(state, [])
    }
    transition_effects.RemoteOperatorCommand(
      backend_kind,
      event_id,
      task_remote_id,
      command_name,
      excerpt,
    ) -> {
      let completion =
        remote_command_completion_for_result(
          state,
          event_id,
          task_remote_id,
          command_name,
          excerpt,
          request.operator_command,
          result,
        )
      log_state(state, "info", "remote_operator_command", [
        #("event_id", event_id),
        #("command", result.command),
        #("status", command.status_to_string(result.status)),
      ])
      #(state, [
        transition_types.RemoteCommandApplied(
          backend_kind: backend_kind,
          event_id: event_id,
          task_remote_id: task_remote_id,
          command_name: command_name,
          result: completion.result,
          message_excerpt: completion.message_excerpt,
          ack_body: completion.ack_body,
        ),
      ])
    }
  }
}

fn remote_command_completion_for_result(
  state: State,
  event_id: String,
  task_remote_id: String,
  _command_name: String,
  excerpt: String,
  operator_command: command.OperatorCommand,
  result: command.CommandResult,
) -> transition_effects.RemoteCommandCompletion {
  let ack_result =
    command_result_with_display_target(state, operator_command, result)
  let message_excerpt =
    result_message_excerpt(ack_result, state.workflow.secrets)
  let ack_body = case
    linear_transport.should_ack_result(
      state.workflow.effective.linear_commands,
      result,
    )
  {
    True ->
      Some(linear_transport.result_ack_body(
        event_id,
        linear_parser.ParsedLinearCommand(
          source_issue_id: task_remote_id,
          source_comment_id: event_id,
          command: operator_command,
          excerpt: excerpt,
        ),
        ack_result,
        state.workflow.secrets,
      ))
    False -> None
  }
  transition_effects.RemoteCommandCompletion(
    result: ack_result,
    message_excerpt: message_excerpt,
    ack_body: ack_body,
  )
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
  case scheduled_mode_for_job(state, job.id) {
    schedule_core.Idle -> {
      let now_ms = state.dependencies.now_ms()
      let run_id = schedule_core.manual_run_id(job.id, now_ms)
      let state =
        state
        |> apply_scheduled_decision(
          job,
          schedule_core.ScheduledDue(now_ms, run_id, "manual"),
        )
        |> apply_scheduled_decision(
          job,
          schedule_core.ScheduledPending(now_ms, run_id, "manual", now_ms),
        )
        |> start_pending_scheduled_runs
      case dict.has_key(state.pending_scheduled_starts, job.id) {
        True -> #(
          state,
          command.queued(operator_command, Some("scheduled run queued")),
        )
        False -> #(
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
    Ok(_) -> {
      let reason_text = session_reason.to_string(reason)
      let state =
        run_transition_messages(state, [
          transition_types.WorkerStopRequested(
            session_id,
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
  |> append_unique_list(dict.keys(state.runtime.running))
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
  case dict.get(state.runtime.running, issue_id) {
    Ok(entry) -> Ok(entry.issue)
    Error(Nil) ->
      case dict.get(state.pending_claims, issue_id) {
        Ok(pending) -> Ok(pending.issue)
        Error(Nil) ->
          case dict.get(state.pending_dispatch_validations, issue_id) {
            Ok(pending) -> Ok(pending.issue)
            Error(Nil) ->
              case dict.get(state.runtime.completed, issue_id) {
                Ok(issue) -> Ok(issue)
                Error(Nil) -> fetch_issue_by_id(state, issue_id)
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
  let completed = state.runtime.completed |> dict.values
  list.append(
    running,
    list.append(pending, list.append(pending_validations, completed)),
  )
  |> list.filter(fn(issue) { issue.identifier == identifier })
}

fn fetch_candidates_with_identifier(
  state: State,
  identifier: String,
) -> Result(tracker_issue.Issue, command.CommandStatus) {
  case adapter_legacy.lookup_runtime_issue(state.tracker_adapter, identifier) {
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
    adapter_legacy.refresh_runtime_issues_by_ids(state.tracker_adapter, [
      issue_id,
    ])
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
      case dict.has_key(state.runtime.parked, issue_id) {
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
  log_state(state, "info", "workflow_reloaded", [])
  state
}

fn refresh_scheduled_next_due_after_reload(state: State) -> State {
  let now_ms = state.dependencies.now_ms()
  let scheduled_next_due =
    state.workflow.bundle.orchestrator.scheduled_jobs
    |> list.filter(fn(job) { job.enabled })
    |> list.fold(state.scheduled_next_due, fn(acc, job) {
      case dict.get(acc, job.id) {
        Ok(_) -> acc
        Error(Nil) ->
          dict.insert(
            acc,
            job.id,
            schedule_core.initial_next_due(now_ms, job.every_ms),
          )
      }
    })
  State(..state, scheduled_next_due: scheduled_next_due)
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

fn handle_remote_command_fetch_finished(
  state: State,
  generation: Int,
  candidates: List(tracker_issue.Issue),
  dispatch_after: Bool,
  result: Result(List(adapter.RemoteCommandEvent), error.TrackerError),
) -> State {
  case poll_result_is_stale(state, generation) {
    True -> state
    False -> {
      let state = case result {
        Error(err) -> {
          log_state(state, "warn", "remote_command_fetch_failed", [
            #("error", error.tracker_code(err)),
          ])
          state
        }
        Ok(events) -> remote_events_to_transition(state, events)
      }
      finish_remote_command_phase(state, candidates, dispatch_after)
    }
  }
}

fn finish_remote_command_phase(
  state: State,
  candidates: List(tracker_issue.Issue),
  dispatch_after: Bool,
) -> State {
  run_transition_messages(state, [
    transition_types.RetryPendingRemoteCommandAcks,
    transition_types.RemoteCommandPhaseFinished(
      candidates,
      dispatch_after,
      transition_dispatch_context(state),
    ),
  ])
}

fn remote_events_to_transition(
  state: State,
  events: List(adapter.RemoteCommandEvent),
) -> State {
  let #(transport_state, actions) =
    linear_transport.process_remote_events(
      state.linear_command_state,
      state.workflow.effective.linear_commands,
      events,
      worker_registry.issue_sessions(state.registry),
    )
  let state = State(..state, linear_command_state: transport_state)
  fold_remote_transport_actions(state, actions)
}

fn fold_remote_transport_actions(
  state: State,
  actions: List(linear_transport.RemoteTransportAction),
) -> State {
  case actions {
    [] -> state
    [action, ..rest] ->
      fold_remote_transport_actions(
        remote_transport_action_to_transition(state, action),
        rest,
      )
  }
}

fn command_result_with_display_target(
  state: State,
  operator_command: command.OperatorCommand,
  result: command.CommandResult,
) -> command.CommandResult {
  case result.target, operator_command_targets_session(operator_command) {
    Some(target), True ->
      case worker_registry.worker_for_session(state.registry, target) {
        Ok(handle) ->
          command.CommandResult(
            ..result,
            target: Some(session_name.generate(
              handle.issue.identifier,
              handle.session_id,
            )),
          )
        Error(Nil) -> result
      }
    _, _ -> result
  }
}

fn operator_command_targets_session(
  operator_command: command.OperatorCommand,
) -> Bool {
  case operator_command {
    command.AbortSession(_)
    | command.StopAfterCurrentTurn(_)
    | command.PromptSession(_, _)
    | command.RespondUi(_, _, _) -> True
    command.PauseDispatch
    | command.ResumeDispatch
    | command.ReloadWorkflow
    | command.RetryIssue(_)
    | command.RetryWorkflowStep(_, _)
    | command.ParkIssue(_, _)
    | command.UnparkIssue(_)
    | command.RunScheduleNow(_) -> False
  }
}

fn remote_transport_action_to_transition(
  state: State,
  action: linear_transport.RemoteTransportAction,
) -> State {
  case action {
    linear_transport.SubmitRemoteCommand(event, parsed) ->
      remote_submit_to_transition(state, event, parsed)
    linear_transport.PostRemoteAck(backend_kind, task_remote_id, event_id, body) ->
      remote_ack_to_transition(
        state,
        backend_kind,
        task_remote_id,
        event_id,
        body,
        False,
        "remote_command_ack",
      )
    linear_transport.LogRemoteIgnored(reason, event_id) -> {
      log_state(state, "info", "remote_command_ignored", [
        #("event_id", event_id),
        #("reason", reason),
      ])
      state
    }
  }
}

fn remote_submit_to_transition(
  state: State,
  event: adapter.RemoteCommandEvent,
  parsed: linear_parser.ParsedLinearCommand,
) -> State {
  run_transition_messages(state, [
    transition_types.RemoteCommandSubmitted(
      event: event,
      parsed: parsed,
      safe_excerpt: safe_remote_command_excerpt(
        parsed.excerpt,
        state.workflow.secrets,
      ),
    ),
  ])
}

fn remote_ack_to_transition(
  state: State,
  backend_kind: String,
  task_remote_id: String,
  event_id: String,
  body: String,
  outbox_recorded: Bool,
  outbox_kind: String,
) -> State {
  run_transition_messages(state, [
    transition_types.RemoteCommandAckRequested(
      backend_kind: backend_kind,
      task_remote_id: task_remote_id,
      event_id: event_id,
      body: body,
      outbox_recorded: outbox_recorded,
      outbox_kind: outbox_kind,
    ),
  ])
}

fn post_remote_command_ack_shell(
  state: State,
  backend_kind: String,
  task_remote_id: String,
  event_id: String,
  body: String,
  outbox_kind: String,
) -> State {
  case state.tracker_adapter.remote_commands {
    Some(capability) ->
      enqueue_side_effect(
        state,
        effect_runner.PostRemoteCommandAck(
          backend_kind: backend_kind,
          task_remote_id: task_remote_id,
          event_id: event_id,
          body: body,
          outbox_kind: outbox_kind,
          capability: capability,
        ),
      )
    None -> state
  }
}

fn safe_remote_command_excerpt(value: String, secrets: List(String)) -> String {
  log.redact("remote_command_receipt", value, secrets)
  |> log.truncate(record.max_excerpt_chars)
}

fn result_message_excerpt(
  result: command.CommandResult,
  secrets: List(String),
) -> String {
  case result.message {
    Some(message) -> safe_remote_command_excerpt(message, secrets)
    None -> ""
  }
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

fn poll_result_is_stale(state: State, generation: Int) -> Bool {
  poll_scheduler.result_is_stale(state.poll, generation)
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
    state.workflow.bundle.orchestrator.routing,
    runtime_bundle.normalized_workflows(state.workflow.bundle),
    config.can_dispatch(state.workflow.reload_state),
    state.operator_paused,
    worker_registry.worker_issue_ids(state.registry),
    worker_registry.worker_issues(state.registry),
    list.length(worker_registry.scheduled_worker_handles(state.registry)),
    state.workflow.effective.workspace.root,
    state.dependencies.now_ms(),
    state.recovery_by_issue,
    state.workflow.bundle.orchestrator.config_dir,
  )
}

fn transition_state_from_daemon(state: State) -> transition_types.State {
  transition_types.State(
    runtime: state.runtime,
    workers: state.workers,
    pending_claims: state.pending_claims,
    pending_dispatch_validations: state.pending_dispatch_validations,
    next_dispatch_validation_generation: state.next_dispatch_validation_generation,
    next_session_sequence: worker_registry.next_session_sequence(state.registry),
    pending_linear_command_acks: state.pending_linear_command_acks,
    in_flight_linear_command_acks: state.in_flight_linear_command_acks,
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
        next_dispatch_validation_generation: transition_state.next_dispatch_validation_generation,
        shell_state_overrides_transition: False,
      )
  }
  State(
    ..state,
    pending_linear_command_acks: transition_state.pending_linear_command_acks,
    in_flight_linear_command_acks: transition_state.in_flight_linear_command_acks,
  )
}

fn run_transition_messages(
  state: State,
  messages: List(transition_types.Message),
) -> State {
  let transition_state = transition_state_from_daemon(state)
  let shell = transition_shell(state)
  let transition_runner.RunResult(
    state: transition_state,
    shell: shell,
    exhausted: exhausted,
  ) =
    transition_runner.run(
      state: transition_state,
      shell: shell,
      messages: messages,
      max_messages: 128,
    )
  let state =
    merge_transition_state(transition_interpreter.data(shell), transition_state)
  case exhausted {
    True ->
      log_state(state, "warn", "transition_runner_exhausted", [
        #("message_limit", "128"),
      ])
    False -> Nil
  }
  state
}

fn transition_shell(state: State) -> transition_interpreter.ShellState(State) {
  transition_interpreter.new_production_shell_state(
    data: state,
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
    fetch_remote_commands: fn(
      state,
      generation,
      task_refs,
      candidates,
      dispatch_after,
    ) {
      case state.tracker_adapter.remote_commands {
        Some(capability) ->
          enqueue_side_effect(
            state,
            effect_runner.FetchRemoteCommands(
              generation: generation,
              task_refs: task_refs,
              candidates: candidates,
              dispatch_after: dispatch_after,
              capability: capability,
              limit_per_task: state.workflow.effective.linear_commands.poll_limit_per_issue,
            ),
          )
        None -> finish_remote_command_phase(state, candidates, dispatch_after)
      }
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
    reserve_session_sequence: transition_reserve_session_sequence,
    claim_issue: fn(state, issue, workspace_path, run_id) {
      enqueue_side_effect(
        state,
        effect_runner.ClaimIssue(
          issue: issue,
          workspace_path: workspace_path,
          run_id: run_id,
          capability: require_handoff_capability(state),
        ),
      )
    },
    report_invalid_workflow: fn(
      state,
      issue,
      violation,
      violation_fingerprint,
      reporting_policy_fingerprint,
    ) {
      enqueue_side_effect(
        state,
        effect_runner.ReportInvalidWorkflow(
          issue: issue,
          violation: violation,
          violation_fingerprint: violation_fingerprint,
          reporting_policy_fingerprint: reporting_policy_fingerprint,
          contract_config: state.workflow.effective.linear_contract,
          comments: state.tracker_adapter.comments,
          state_transitions: state.tracker_adapter.state_transitions,
        ),
      )
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
    replay_remote_command_ack: transition_replay_remote_command_ack,
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
    post_remote_command_ack: fn(
      state,
      backend_kind,
      task_remote_id,
      event_id,
      body,
      outbox_kind,
    ) {
      post_remote_command_ack_shell(
        state,
        backend_kind,
        task_remote_id,
        event_id,
        body,
        outbox_kind,
      )
    },
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
  case request.bodies {
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
              ledger_records_for_bodies(
                state.dependencies.now_ms(),
                request.bodies,
              ),
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
  #(
    spawn_worker(
      state,
      request.issue,
      request.workspace_path,
      request.run_id,
      request.session_id,
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
  log_state(state, "warn", "worker_start_failed", [
    #("issue_id", request.issue_id),
    #("run_id", request.run_id),
    #("reason", reason),
  ])
  State(
    ..state,
    registry: worker_registry.forget_issue_session(
      state.registry,
      request.issue_id,
    ),
    recovery_by_issue: dict.delete(state.recovery_by_issue, request.issue_id),
  )
}

fn transition_remove_worker(
  state: State,
  identity: transition_effects.WorkerIdentity,
  demonitor: Bool,
) -> State {
  case worker_registry.worker_for_issue(state.registry, identity.issue_id) {
    Error(Nil) ->
      State(
        ..state,
        registry: worker_registry.forget_issue_session(
          state.registry,
          identity.issue_id,
        ),
      )
    Ok(handle) -> {
      case handle.run_id == identity.run_id && demonitor {
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
  case
    request.update_tokens && event_publisher.tokens_are_nonzero(request.tokens)
  {
    True ->
      hub.update_tokens(
        state.event_hub,
        request.identity.session_id,
        request.tokens,
      )
    False -> Nil
  }
  event_publisher.lifecycle(
    state.event_hub,
    request.identity.session_id,
    session_event.WorkerExited,
    Some(request.reason_text),
  )
  hub.finish_session(
    state.event_hub,
    request.identity.session_id,
    request.exit_reason,
  )
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
  enqueue_side_effect(
    state,
    effect_runner.ReportSuccess(
      issue_id: identity.issue_id,
      issue: final_issue,
      success: success,
      run_id: identity.run_id,
      workflow_id: identity.workflow_id,
      capability: require_handoff_capability(state),
    ),
  )
}

fn transition_report_worker_failure(
  state: State,
  identity: transition_effects.WorkerIdentity,
  failure: agent_types.WorkerFailure,
) -> State {
  enqueue_side_effect(
    state,
    effect_runner.ReportFailure(
      issue_id: identity.issue_id,
      issue: identity.issue,
      failure: failure,
      run_id: identity.run_id,
      workflow_id: identity.workflow_id,
      capability: require_handoff_capability(state),
    ),
  )
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

fn transition_replay_remote_command_ack(
  state: State,
  backend_kind: String,
  task_remote_id: String,
  event_id: String,
  body: String,
  outbox_kind: String,
) -> State {
  let state =
    State(
      ..state,
      linear_command_state: linear_transport.mark_processed(
        state.linear_command_state,
        event_id,
      ),
    )
  remote_ack_to_transition(
    state,
    backend_kind,
    task_remote_id,
    event_id,
    body,
    True,
    outbox_kind,
  )
}

fn transition_report_park(state: State, report: adapter.ParkReport) -> State {
  enqueue_side_effect(
    state,
    effect_runner.ReportPark(report, require_handoff_capability(state)),
  )
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
  enqueue_side_effect(
    state,
    effect_runner.ReportPark(
      adapter.ParkReport(
        task: task.TaskRef(
          backend_kind: state.tracker_adapter.kind,
          remote_id: parked.issue_id,
          key: Some(parked.identifier),
          url: None,
        ),
        issue_identifier: parked.identifier,
        reason: reason_text,
        release_policy: Some(park_release_policy_to_string(
          parked.release_policy,
        )),
        run_id: source_run_id,
      ),
      require_handoff_capability(state),
    ),
  )
}

fn transition_stop_worker(
  state: State,
  identity: transition_effects.WorkerIdentity,
  reason: session_reason.WorkerExitReason,
) -> State {
  let reason_text = session_reason.to_string(reason)
  case worker_registry.worker_for_issue(state.registry, identity.issue_id) {
    Error(Nil) -> state
    Ok(handle) -> {
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
      stop_worker(handle)
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
  case worker_registry.worker_for_issue(state.registry, identity.issue_id) {
    Error(Nil) -> state
    Ok(handle) -> {
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
      stop_worker(handle)
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
        #("issue_id", identity.issue_id),
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
  session_id: String,
  run_id: String,
) -> State {
  State(
    ..state,
    registry: worker_registry.register_yaml_step_started(
      state.registry,
      session_id,
      run_id,
    ),
  )
}

fn transition_finish_yaml_step_route(
  state: State,
  session_id: String,
) -> State {
  State(
    ..state,
    registry: worker_registry.finish_yaml_step(state.registry, session_id),
  )
}

fn transition_finish_yaml_step_session(
  state: State,
  session_id: String,
  reason: session_reason.WorkerExitReason,
) -> State {
  let reason_text = session_reason.to_string(reason)
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
  state
}

fn transition_finish_yaml_step_sessions_for_run(
  state: State,
  run_id: String,
  reason: session_reason.WorkerExitReason,
) -> State {
  finish_yaml_step_sessions_for_run(state, run_id, reason)
}

fn transition_clear_yaml_step_routes_for_run(
  state: State,
  run_id: String,
) -> State {
  clear_yaml_step_command_routes_for_run(state, run_id)
}

fn transition_mark_yaml_run_stopping(
  state: State,
  run_id: String,
  reason: session_reason.WorkerExitReason,
) -> State {
  State(
    ..state,
    registry: worker_registry.mark_yaml_run_stopping(
      state.registry,
      run_id,
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
  let #(state, next_due_at_ms) = ensure_scheduled_next_due(state, job, now_ms)
  let schedule_state =
    schedule_core.ScheduleState(
      job_id: job.id,
      workflow_id: job.workflow,
      every_ms: job.every_ms,
      next_due_at_ms: next_due_at_ms,
      mode: scheduled_mode_for_job(state, job.id),
    )
  schedule_core.admit_due_boundaries(schedule_state, now_ms)
  |> apply_scheduled_decisions(state, job)
}

fn ensure_scheduled_next_due(
  state: State,
  job: config_types.ScheduledJobConfig,
  now_ms: Int,
) -> #(State, Int) {
  case dict.get(state.scheduled_next_due, job.id) {
    Ok(value) -> #(state, value)
    Error(Nil) -> {
      let next_due_at_ms = schedule_core.initial_next_due(now_ms, job.every_ms)
      #(
        State(
          ..state,
          scheduled_next_due: dict.insert(
            state.scheduled_next_due,
            job.id,
            next_due_at_ms,
          ),
        ),
        next_due_at_ms,
      )
    }
  }
}

fn scheduled_mode_for_job(
  state: State,
  job_id: String,
) -> schedule_core.ScheduleMode {
  case dict.get(state.pending_scheduled_starts, job_id) {
    Ok(pending) ->
      schedule_core.Pending(scheduled_skip_reason_for_block(
        pending.blocking_reason,
      ))
    Error(Nil) ->
      case scheduled_worker_active_for_job(state, job_id) {
        True -> schedule_core.Active
        False ->
          case scheduled_retry_waiting_for_job(state, job_id) {
            True -> schedule_core.RetryWaiting
            False -> schedule_core.Idle
          }
      }
  }
}

fn scheduled_skip_reason_for_block(reason: String) -> String {
  case reason {
    "paused" -> "schedule_paused"
    _ -> reason
  }
}

fn scheduled_worker_active_for_job(state: State, job_id: String) -> Bool {
  state.registry
  |> worker_registry.scheduled_worker_handles
  |> list.any(fn(handle) { handle.job_id == job_id })
}

fn scheduled_retry_waiting_for_job(state: State, job_id: String) -> Bool {
  list.any(dict.values(state.scheduled_retries), fn(entry) {
    entry.job_id == job_id
  })
  || list.any(dict.values(state.scheduled_report_retries), fn(entry) {
    entry.job_id == job_id
  })
}

fn apply_scheduled_decisions(
  decisions: List(schedule_core.ScheduleDecision),
  state: State,
  job: config_types.ScheduledJobConfig,
) -> State {
  list.fold(decisions, state, fn(state, decision) {
    apply_scheduled_decision(state, job, decision)
  })
}

fn apply_scheduled_decision(
  state: State,
  job: config_types.ScheduledJobConfig,
  decision: schedule_core.ScheduleDecision,
) -> State {
  case decision {
    schedule_core.ScheduledDue(due_at_ms, run_id, trigger) -> {
      append_ledger_bodies_best_effort(
        state,
        [
          record.ScheduledJobDue(
            job.id,
            job.workflow,
            due_at_ms,
            run_id,
            trigger,
          ),
        ],
        "scheduled_due_append_failed",
      )
      state
    }
    schedule_core.ScheduledPending(due_at_ms, run_id, trigger, requested_at_ms) -> {
      append_ledger_bodies_best_effort(
        state,
        [
          record.ScheduledRunPending(
            job.id,
            job.workflow,
            due_at_ms,
            run_id,
            trigger,
            requested_at_ms,
          ),
        ],
        "scheduled_pending_append_failed",
      )
      State(
        ..state,
        pending_scheduled_starts: dict.insert(
          state.pending_scheduled_starts,
          job.id,
          ScheduledPendingStart(
            job_id: job.id,
            workflow_id: job.workflow,
            due_at_ms: due_at_ms,
            run_id: run_id,
            trigger: trigger,
            requested_at_ms: requested_at_ms,
            attempt: 1,
            blocking_reason: "",
          ),
        ),
      )
    }
    schedule_core.ScheduledSkipped(due_at_ms, run_id, reason, skipped_count) -> {
      append_ledger_bodies_best_effort(
        state,
        [
          record.ScheduledJobSkipped(
            job.id,
            job.workflow,
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
    schedule_core.ScheduledNextDue(next_due_at_ms) ->
      State(
        ..state,
        scheduled_next_due: dict.insert(
          state.scheduled_next_due,
          job.id,
          next_due_at_ms,
        ),
      )
  }
}

fn start_pending_scheduled_runs(state: State) -> State {
  state.pending_scheduled_starts
  |> dict.values
  |> list.fold(state, fn(state, pending) {
    start_pending_scheduled_run(state, pending)
  })
}

fn start_pending_scheduled_run(
  state: State,
  pending: ScheduledPendingStart,
) -> State {
  case state.operator_paused {
    True -> block_pending_scheduled_run(state, pending, "paused")
    False ->
      case scheduled_slot_available_for_start(state) {
        False ->
          block_pending_scheduled_run(state, pending, "waiting_for_global_slot")
        True -> spawn_scheduled_worker_for_pending(state, pending)
      }
  }
}

fn block_pending_scheduled_run(
  state: State,
  pending: ScheduledPendingStart,
  reason: String,
) -> State {
  case pending.blocking_reason == reason {
    True -> state
    False -> {
      append_ledger_bodies_best_effort(
        state,
        [
          record.ScheduledRunPendingBlocked(
            pending.job_id,
            pending.workflow_id,
            pending.due_at_ms,
            pending.run_id,
            reason,
            state.dependencies.now_ms(),
          ),
        ],
        "scheduled_pending_blocked_append_failed",
      )
      let pending = ScheduledPendingStart(..pending, blocking_reason: reason)
      State(
        ..state,
        pending_scheduled_starts: dict.insert(
          state.pending_scheduled_starts,
          pending.job_id,
          pending,
        ),
      )
    }
  }
}

fn scheduled_slot_available_for_start(state: State) -> Bool {
  active_run_count(state)
  + dict.size(state.pending_claims)
  + dict.size(state.pending_dispatch_validations)
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
  |> dict.keys
  |> list.any(fn(issue_id) {
    !list.contains(active_run_issue_ids(state), issue_id)
    && !dict.has_key(state.pending_claims, issue_id)
    && !dict.has_key(state.pending_dispatch_validations, issue_id)
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
        record.linear_task_ref_fields(
          pending.issue.id,
          Some(pending.issue.identifier),
          pending.issue.url,
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

fn spawn_worker(
  state: State,
  issue: tracker_issue.Issue,
  workspace_path: String,
  run_id: String,
  session_id: String,
  recovery: Option(session_event.RecoveryInfo),
) -> State {
  let started_at_ms = state.dependencies.now_ms()
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
  publish_recovery_lifecycle(state.event_hub, session_id, recovery)
  event_publisher.lifecycle(
    state.event_hub,
    session_id,
    session_event.DispatchStarted,
    None,
  )
  log_state(state, "info", "dispatch_started", [
    #("issue_id", issue.id),
    #("issue_identifier", issue.identifier),
    #("run_id", run_id),
    #("workspace_path", workspace_path),
  ])
  let runtime = core.apply_worker_start(state.runtime, issue, workspace_path)
  let subject = state.subject
  let dependencies = state.dependencies
  let tracker_client = state.tracker_client
  let bundle = state.workflow.bundle
  let secrets = state.workflow.secrets
  let event_hub = state.event_hub
  let pid =
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
          dependencies.now_ms,
        )
      process.send(subject, WorkerFinished(issue.id, run_id, result))
    })
  let monitor = process.monitor(pid)
  event_publisher.lifecycle(
    state.event_hub,
    session_id,
    session_event.WorkerStarted,
    None,
  )
  hub.update_status(state.event_hub, session_id, session_event.Running)
  let handle =
    worker_registry.WorkerHandle(
      issue_id: issue.id,
      issue: issue,
      run_id: run_id,
      pid: pid,
      monitor: monitor,
      workspace_path: workspace_path,
      session_id: session_id,
      command_subject: None,
    )
  State(
    ..state,
    runtime: runtime,
    registry: worker_registry.register_worker(state.registry, handle),
    recovery_by_issue: dict.delete(state.recovery_by_issue, issue.id),
  )
}

fn spawn_scheduled_worker_for_pending(
  state: State,
  pending: ScheduledPendingStart,
) -> State {
  case
    runtime_bundle.workflow_by_id(state.workflow.bundle, pending.workflow_id)
  {
    Error(_) -> {
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
        pending_scheduled_starts: dict.delete(
          state.pending_scheduled_starts,
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
            pending_scheduled_starts: dict.delete(
              state.pending_scheduled_starts,
              pending.job_id,
            ),
          )
        }
        Ok(run_root) ->
          spawn_scheduled_worker_with_run_root(state, pending, dag, run_root)
      }
  }
}

fn spawn_scheduled_worker_with_run_root(
  state: State,
  pending: ScheduledPendingStart,
  dag: workflow_dag.WorkflowDag,
  run_root: String,
) -> State {
  let #(registry, _session_sequence) =
    worker_registry.reserve_session_sequence(state.registry)
  let state = State(..state, registry: registry)
  let session_id = scheduled_session_id(pending.run_id, pending.attempt)
  let started_at_ms = state.dependencies.now_ms()
  let display_ref = "scheduled-" <> pending.job_id
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
  event_publisher.lifecycle(
    state.event_hub,
    session_id,
    session_event.DispatchStarted,
    Some("scheduled"),
  )
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
  log_state(state, "info", "scheduled_dispatch_started", [
    #("job_id", pending.job_id),
    #("run_id", pending.run_id),
    #("workflow_id", pending.workflow_id),
  ])
  let subject = state.subject
  let dependencies = state.dependencies
  let tracker_client = state.tracker_client
  let bundle = state.workflow.bundle
  let secrets = state.workflow.secrets
  let event_hub = state.event_hub
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
  let pid =
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
          dependencies.now_ms,
        )
      process.send(subject, ScheduledWorkerFinished(pending.run_id, result))
    })
  let monitor = process.monitor(pid)
  event_publisher.lifecycle(
    state.event_hub,
    session_id,
    session_event.WorkerStarted,
    Some("scheduled"),
  )
  hub.update_status(state.event_hub, session_id, session_event.Running)
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
  State(
    ..state,
    registry: worker_registry.register_scheduled_worker(state.registry, handle),
    pending_scheduled_starts: dict.delete(
      state.pending_scheduled_starts,
      pending.job_id,
    ),
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
  now_ms: fn() -> Int,
) -> Result(workflow_run.WorkflowRunSuccess, workflow_run.WorkflowRunFailure) {
  let workflow_dependencies =
    workflow_run.Dependencies(
      ..workflow_dependencies,
      checkpoint: workflow_checkpoint.ledger_writer(
        bundle.effective.workspace.root,
        now_ms,
      ),
    )
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
}

fn scheduled_session_id(run_id: String, attempt: Int) -> String {
  run_id <> "-a" <> int.to_string(attempt)
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
            daemon_subject,
            event_hub,
            now_ms,
          ),
        )
      {
        Ok(success) -> Ok(success.worker_success)
        Error(failure) -> Error(yaml_workflow_failure(failure, issue))
      }
    }
  }
}

fn yaml_scheduled_workflow_dependencies(
  base: workflow_run.Dependencies,
  scheduled: schedule_core.ScheduledRunContext,
  daemon_subject: process.Subject(Message),
  event_hub: process.Subject(hub.Message),
  now_ms: fn() -> Int,
) -> workflow_run.Dependencies {
  yaml_workflow_dependencies(
    base,
    scheduled_session_issue(scheduled),
    scheduled.run_id,
    daemon_subject,
    event_hub,
    now_ms,
  )
}

fn scheduled_session_issue(
  scheduled: schedule_core.ScheduledRunContext,
) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "",
    identifier: "scheduled-" <> scheduled.job_id,
    title: "Scheduled job " <> scheduled.job_id,
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked("scheduled"),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn yaml_workflow_dependencies(
  base: workflow_run.Dependencies,
  issue: tracker_issue.Issue,
  run_id: String,
  daemon_subject: process.Subject(Message),
  event_hub: process.Subject(hub.Message),
  now_ms: fn() -> Int,
) -> workflow_run.Dependencies {
  workflow_run.Dependencies(
    ..base,
    command_step: fn(context, command, timeout_ms, secrets, limits) {
      run_yaml_command_step(
        base,
        issue,
        run_id,
        context,
        command,
        timeout_ms,
        secrets,
        limits,
        daemon_subject,
        event_hub,
        now_ms,
      )
    },
    agent_step: fn(
      issue,
      context,
      prompt_mode,
      attempt_context,
      effective,
      tracker_client,
      _emit_update,
      _command_ready,
      record_pi_session,
    ) {
      run_yaml_agent_step(
        base,
        issue,
        run_id,
        context,
        prompt_mode,
        attempt_context,
        effective,
        tracker_client,
        daemon_subject,
        event_hub,
        now_ms,
        record_pi_session,
      )
    },
  )
}

fn register_yaml_step_session(
  event_hub: process.Subject(hub.Message),
  session_id: String,
  issue: tracker_issue.Issue,
  workspace_path: String,
  step_id: String,
  attempt_index: Int,
  now_ms: fn() -> Int,
) -> Nil {
  let started_at_ms = now_ms()
  hub.register_session(
    event_hub,
    session_event.SessionSummary(
      session_id: session_id,
      display_name: session_name.generate(issue.identifier, session_id),
      issue_id: issue.id,
      issue_identifier: issue.identifier,
      issue_title: issue.title,
      workspace_path: workspace_path,
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
  hub.update_status(event_hub, session_id, session_event.Running)
  hub.publish(
    event_hub,
    session_id,
    session_event.EventPayload(
      ..session_event.empty_payload(
        session_event.Lifecycle,
        session_event.LifecycleName(session_event.StepStarted),
      ),
      message: Some(step_id <> " attempt " <> int.to_string(attempt_index)),
    ),
  )
}

fn run_yaml_command_step(
  base: workflow_run.Dependencies,
  issue: tracker_issue.Issue,
  run_id: String,
  context: workflow_run.StepContext,
  command: String,
  timeout_ms: Int,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
  daemon_subject: process.Subject(Message),
  event_hub: process.Subject(hub.Message),
  now_ms: fn() -> Int,
) -> step_artifact.StepArtifact {
  let session_id =
    yaml_step_session.id(run_id, context.step_id, context.attempt_index)
  register_yaml_step_session(
    event_hub,
    session_id,
    issue,
    context.workspace_path,
    context.step_id,
    context.attempt_index,
    now_ms,
  )
  process.send(daemon_subject, YamlStepStarted(session_id, run_id))
  let artifact =
    base.command_step(context, command, timeout_ms, secrets, limits)
  case step_artifact.succeeded(artifact.status) {
    True -> Nil
    False -> publish_yaml_command_failure(event_hub, session_id, artifact)
  }
  let reason = case step_artifact.succeeded(artifact.status) {
    True -> session_reason.Normal
    False -> session_reason.Failed
  }
  hub.finish_session(event_hub, session_id, reason)
  process.send(daemon_subject, YamlStepFinished(session_id))
  artifact
}

fn publish_yaml_command_failure(
  event_hub: process.Subject(hub.Message),
  session_id: String,
  artifact: step_artifact.StepArtifact,
) -> Nil {
  let summary = case step_artifact.command_failure_summary(artifact) {
    Some(summary) -> summary
    None -> "command step failed: step=" <> artifact.step_id
  }
  hub.publish(
    event_hub,
    session_id,
    session_event.EventPayload(
      ..session_event.empty_payload(
        session_event.Error,
        session_event.PiName(pi_event.UnknownPiEvent("command_failed")),
      ),
      message: Some(summary),
      tool_name: Some("workflow command " <> artifact.step_id),
      tool_input: artifact.command,
      tool_output: Some(step_artifact.command_failure_details(artifact)),
      tool_status: Some("failed"),
    ),
  )
}

fn run_yaml_agent_step(
  base: workflow_run.Dependencies,
  issue: tracker_issue.Issue,
  run_id: String,
  context: workflow_run.StepContext,
  prompt_mode: workflow_attempt.AgentPromptMode,
  attempt_context: workflow_attempt.StepAttemptContext,
  effective: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  daemon_subject: process.Subject(Message),
  event_hub: process.Subject(hub.Message),
  now_ms: fn() -> Int,
  record_pi_session: fn(workflow_attempt.PiSessionObservation) -> Nil,
) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
  let session_step_id = case prompt_mode {
    workflow_attempt.StructuredOutputRetryPrompt(_) ->
      context.step_id <> "_structured_output_retry"
    _ -> context.step_id
  }
  let session_id =
    yaml_step_session.id(run_id, session_step_id, context.attempt_index)
  let started_at_ms = now_ms()
  hub.register_session(
    event_hub,
    session_event.SessionSummary(
      session_id: session_id,
      display_name: session_name.generate(issue.identifier, session_id),
      issue_id: issue.id,
      issue_identifier: issue.identifier,
      issue_title: issue.title,
      workspace_path: context.workspace_path,
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
  hub.update_status(event_hub, session_id, session_event.Running)
  hub.publish(
    event_hub,
    session_id,
    session_event.EventPayload(
      ..session_event.empty_payload(
        session_event.Lifecycle,
        session_event.LifecycleName(session_event.StepStarted),
      ),
      message: Some(
        context.step_id <> " attempt " <> int.to_string(context.attempt_index),
      ),
    ),
  )
  process.send(daemon_subject, YamlStepStarted(session_id, run_id))
  let result =
    base.agent_step(
      issue,
      context,
      prompt_mode,
      attempt_context,
      effective,
      tracker_client,
      fn(update) {
        process.send(daemon_subject, YamlStepUpdate(session_id, update))
      },
      fn(command_subject) {
        process.send(
          daemon_subject,
          YamlStepCommandReady(session_id, command_subject),
        )
      },
      record_pi_session,
    )
  case result {
    Ok(success) -> {
      hub.update_tokens(event_hub, session_id, success.tokens)
      hub.finish_session(event_hub, session_id, session_reason.Normal)
    }
    Error(failure) -> {
      case event_publisher.tokens_are_nonzero(failure.tokens) {
        True -> hub.update_tokens(event_hub, session_id, failure.tokens)
        False -> Nil
      }
      hub.finish_session(event_hub, session_id, session_reason.Failed)
    }
  }
  process.send(daemon_subject, YamlStepFinished(session_id))
  result
}

fn yaml_worker_failure(
  reason: String,
  workspace_path: Option(String),
  issue: tracker_issue.Issue,
) -> agent_types.WorkerFailure {
  agent_types.WorkerFailure(
    reason: error.PiFailed(error.PiProtocolError(reason)),
    workspace_path: workspace_path,
    tokens: session_tokens.zero_token_totals(),
    final_issue: Some(issue),
  )
}

fn yaml_workflow_failure(
  failure: workflow_run.WorkflowRunFailure,
  issue: tracker_issue.Issue,
) -> agent_types.WorkerFailure {
  let report = workflow_run.failure_report(failure)
  case workflow_run.failed_command_failure(failure) {
    Some(#(code, step_id)) ->
      agent_types.WorkerFailure(
        reason: error.WorkflowCommandFailed(
          code: code,
          step_id: step_id,
          detail: report,
        ),
        workspace_path: failure.run_root,
        tokens: session_tokens.zero_token_totals(),
        final_issue: Some(issue),
      )
    None ->
      case failure.agent_reason {
        Some(reason) ->
          agent_types.WorkerFailure(
            reason: reason,
            workspace_path: failure.run_root,
            tokens: session_tokens.zero_token_totals(),
            final_issue: Some(issue),
          )
        None -> yaml_worker_failure(report, failure.run_root, issue)
      }
  }
}

fn handle_worker_command_ready(
  state: State,
  issue_id: String,
  run_id: String,
  command_subject: process.Subject(worker_command.Command),
) -> State {
  let state =
    run_transition_messages(state, [
      transition_types.WorkerCommandReady(issue_id, run_id),
    ])
  State(
    ..state,
    registry: worker_registry.register_worker_command_subject(
      state.registry,
      issue_id,
      run_id,
      command_subject,
    ),
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

fn handle_worker_update(
  state: State,
  issue_id: String,
  update: agent_types.RunnerUpdate,
) -> State {
  case worker_registry.worker_for_issue(state.registry, issue_id) {
    Ok(handle) ->
      event_publisher.worker_update(state.event_hub, handle.session_id, update)
    Error(Nil) -> Nil
  }
  log_worker_update(state, issue_id, update)
}

fn handle_scheduled_worker_finished(
  state: State,
  run_id: String,
  result: Result(
    workflow_run.WorkflowRunSuccess,
    workflow_run.WorkflowRunFailure,
  ),
) -> State {
  let state = evaluate_scheduled_jobs(state)
  case worker_registry.scheduled_worker_for_run(state.registry, run_id) {
    Error(Nil) -> {
      log_state(state, "warn", "scheduled_worker_finished_stale", [
        #("run_id", run_id),
      ])
      state
    }
    Ok(handle) -> {
      process.demonitor_process(handle.monitor)
      let state =
        State(
          ..state,
          registry: worker_registry.remove_scheduled_worker_handle(
            state.registry,
            handle,
          ),
        )
      case result {
        Ok(success) -> finish_scheduled_worker_success(state, handle, success)
        Error(failure) ->
          finish_scheduled_worker_failure(state, handle, failure)
      }
      |> start_pending_scheduled_runs
    }
  }
}

fn finish_scheduled_worker_success(
  state: State,
  handle: worker_registry.ScheduledWorkerHandle,
  success: workflow_run.WorkflowRunSuccess,
) -> State {
  case success.worker_success.final_classification {
    agent_types.FinalTerminal -> {
      log_state(state, "info", "scheduled_worker_exited", [
        #("job_id", handle.job_id),
        #("run_id", handle.run_id),
        #("reason", "normal"),
      ])
      hub.update_tokens(
        state.event_hub,
        handle.session_id,
        success.worker_success.tokens,
      )
      event_publisher.lifecycle(
        state.event_hub,
        handle.session_id,
        session_event.WorkerExited,
        Some("normal"),
      )
      hub.finish_session(
        state.event_hub,
        handle.session_id,
        session_reason.Normal,
      )
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
      state
    }
    agent_types.FinalActive | agent_types.FinalNonActive ->
      finish_scheduled_worker_needs_human(state, handle, success)
  }
}

fn finish_scheduled_worker_needs_human(
  state: State,
  handle: worker_registry.ScheduledWorkerHandle,
  success: workflow_run.WorkflowRunSuccess,
) -> State {
  log_state(state, "warn", "scheduled_worker_needs_human", [
    #("job_id", handle.job_id),
    #("run_id", handle.run_id),
  ])
  hub.update_tokens(
    state.event_hub,
    handle.session_id,
    success.worker_success.tokens,
  )
  event_publisher.lifecycle(
    state.event_hub,
    handle.session_id,
    session_event.WorkerExited,
    Some("needs_human"),
  )
  hub.finish_session(state.event_hub, handle.session_id, session_reason.Failed)
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
        "needs_human",
        True,
        Some(handle.run_root),
      ),
    ],
    "scheduled_failure_append_failed",
  )
  begin_scheduled_failure_report(
    state,
    handle.job_id,
    handle.workflow_id,
    handle.due_at_ms,
    handle.run_id,
    handle.attempt,
    "needs_human",
    Some(handle.run_root),
    Some(handle.session_id),
  )
}

fn finish_scheduled_worker_failure(
  state: State,
  handle: worker_registry.ScheduledWorkerHandle,
  failure: workflow_run.WorkflowRunFailure,
) -> State {
  let reason = workflow_run.failure_report(failure)
  log_state(state, "warn", "scheduled_worker_exited", [
    #("job_id", handle.job_id),
    #("run_id", handle.run_id),
    #("reason", log.truncate(reason, 200)),
  ])
  event_publisher.lifecycle(
    state.event_hub,
    handle.session_id,
    session_event.WorkerExited,
    Some(log.truncate(reason, 200)),
  )
  hub.finish_session(state.event_hub, handle.session_id, session_reason.Failed)
  let next_attempt = handle.attempt + 1
  let retry_exhausted =
    schedule_core.retry_exhausted(
      next_attempt,
      state.workflow.effective.agent.max_retry_attempts,
    )
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
        option.or(failure.run_root, Some(handle.run_root)),
      ),
    ],
    "scheduled_failure_append_failed",
  )
  case retry_exhausted {
    True ->
      begin_scheduled_failure_report(
        state,
        handle.job_id,
        handle.workflow_id,
        handle.due_at_ms,
        handle.run_id,
        handle.attempt,
        reason,
        option.or(failure.run_root, Some(handle.run_root)),
        Some(handle.session_id),
      )
    False ->
      schedule_scheduled_retry(
        state,
        handle,
        handle.due_at_ms,
        next_attempt,
        reason,
      )
  }
}

fn begin_scheduled_failure_report(
  state: State,
  job_id: String,
  workflow_id: String,
  due_at_ms: Int,
  run_id: String,
  attempt: Int,
  reason: String,
  run_root: Option(String),
  session_id: Option(String),
) -> State {
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
  let linear_config = job.on_failure.linear
  case linear_config.enabled, linear_config.state {
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
      let generation = state.next_scheduled_report_generation
      let publication =
        adapter.ScheduledFailurePublication(
          job_id: job.id,
          workflow_id: workflow_id,
          due_at_ms: due_at_ms,
          run_id: run_id,
          attempt: attempt,
          max_attempts: state.workflow.effective.agent.max_retry_attempts,
          reason: reason,
          run_root: run_root,
          session_id: session_id,
          dedupe_key: scheduled_failure_dedupe_key(job.id),
          title: "Scheduled workflow failure: " <> job.id,
          body: reason,
          labels: linear_config.labels,
          target_state_name: Some(triage_state),
          previous_task_remote_id: scheduled_failure_issue_id_for_state(
            state,
            job.id,
          ),
        )
      case state.tracker_adapter.scheduled_failures {
        Some(capability) ->
          enqueue_side_effect(
            State(..state, next_scheduled_report_generation: generation + 1),
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

fn schedule_scheduled_retry(
  state: State,
  handle: worker_registry.ScheduledWorkerHandle,
  due_at_ms: Int,
  next_attempt: Int,
  reason: String,
) -> State {
  schedule_scheduled_retry_for_run(
    state,
    handle.job_id,
    handle.workflow_id,
    due_at_ms,
    handle.run_id,
    handle.attempt,
    next_attempt,
    reason,
  )
}

fn schedule_scheduled_retry_for_run(
  state: State,
  job_id: String,
  workflow_id: String,
  due_at_ms: Int,
  run_id: String,
  current_attempt: Int,
  next_attempt: Int,
  reason: String,
) -> State {
  let generation = state.next_scheduled_retry_generation
  let delay_ms =
    schedule_core.retry_delay(
      current_attempt,
      state.workflow.effective.agent.max_retry_backoff_ms,
    )
  let timer =
    state.dependencies.send_after(
      state.subject,
      delay_ms,
      ScheduledRetryTick(run_id, generation),
    )
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
  State(
    ..state,
    scheduled_retries: dict.insert(
      state.scheduled_retries,
      run_id,
      ScheduledRetryStart(
        job_id: job_id,
        workflow_id: workflow_id,
        due_at_ms: due_at_ms,
        run_id: run_id,
        next_attempt: next_attempt,
        generation: generation,
        timer: timer,
      ),
    ),
    next_scheduled_retry_generation: generation + 1,
  )
}

fn handle_scheduled_retry_tick(
  state: State,
  run_id: String,
  generation: Int,
) -> State {
  let state = evaluate_scheduled_jobs(state)
  case dict.get(state.scheduled_retries, run_id) {
    Error(Nil) -> state
    Ok(entry) ->
      case entry.generation != generation {
        True -> state
        False ->
          case
            state.operator_paused || !scheduled_slot_available_for_start(state)
          {
            True -> defer_scheduled_retry(state, entry)
            False -> start_scheduled_retry_now(state, entry)
          }
      }
  }
}

fn defer_scheduled_retry(state: State, entry: ScheduledRetryStart) -> State {
  let timer =
    state.dependencies.send_after(
      state.subject,
      1000,
      ScheduledRetryTick(entry.run_id, entry.generation),
    )
  State(
    ..state,
    scheduled_retries: dict.insert(
      state.scheduled_retries,
      entry.run_id,
      ScheduledRetryStart(..entry, timer: timer),
    ),
  )
}

fn start_scheduled_retry_now(
  state: State,
  entry: ScheduledRetryStart,
) -> State {
  let state =
    State(
      ..state,
      scheduled_retries: dict.delete(state.scheduled_retries, entry.run_id),
      pending_scheduled_starts: dict.insert(
        state.pending_scheduled_starts,
        entry.job_id,
        ScheduledPendingStart(
          job_id: entry.job_id,
          workflow_id: entry.workflow_id,
          due_at_ms: entry.due_at_ms,
          run_id: entry.run_id,
          trigger: "automatic",
          requested_at_ms: state.dependencies.now_ms(),
          attempt: entry.next_attempt,
          blocking_reason: "",
        ),
      ),
    )
  start_pending_scheduled_runs(state)
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
  append_ledger_bodies_best_effort(
    State(
      ..state,
      scheduled_report_retries: dict.delete(
        state.scheduled_report_retries,
        publication.run_id,
      ),
    ),
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
  State(
    ..state,
    scheduled_report_retries: dict.delete(
      state.scheduled_report_retries,
      publication.run_id,
    ),
  )
}

fn handle_scheduled_failure_report_failure(
  state: State,
  generation: Int,
  publication: adapter.ScheduledFailurePublication,
  err: error.TrackerError,
) -> State {
  let delay_ms =
    schedule_core.retry_delay(
      generation,
      state.workflow.effective.agent.max_retry_backoff_ms,
    )
  let next_retry_at_ms = state.dependencies.now_ms() + delay_ms
  let timer =
    state.dependencies.send_after(
      state.subject,
      delay_ms,
      ScheduledReportRetryTick(publication.run_id, generation),
    )
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
  State(
    ..state,
    scheduled_report_retries: dict.insert(
      state.scheduled_report_retries,
      publication.run_id,
      ScheduledReportRetryStart(
        job_id: publication.job_id,
        run_id: publication.run_id,
        generation: generation,
        timer: timer,
      ),
    ),
  )
}

fn handle_scheduled_report_retry_tick(
  state: State,
  run_id: String,
  generation: Int,
) -> State {
  case dict.get(state.scheduled_report_retries, run_id) {
    Error(Nil) -> state
    Ok(entry) ->
      case entry.generation != generation {
        True -> state
        False ->
          retry_scheduled_failure_report(
            State(
              ..state,
              scheduled_report_retries: dict.delete(
                state.scheduled_report_retries,
                run_id,
              ),
            ),
            entry,
          )
      }
  }
}

fn retry_scheduled_failure_report(
  state: State,
  entry: ScheduledReportRetryStart,
) -> State {
  case scheduled_projection_for_root(state.workflow.effective.workspace.root) {
    Error(err) -> {
      log_state(state, "warn", "scheduled_report_retry_projection_unavailable", [
        #("job_id", entry.job_id),
        #("run_id", entry.run_id),
        #("error", ledger_error_message(err)),
      ])
      state
    }
    Ok(projected) ->
      case projection.scheduled_status_for(projected, entry.job_id) {
        Error(Nil) -> state
        Ok(status) ->
          case scheduled_job_by_id(state, entry.job_id), status.current_run {
            Ok(job), Some(run) ->
              begin_scheduled_failure_report_for_job(
                state,
                job,
                status.workflow_id,
                run.due_at_ms,
                entry.run_id,
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

fn worker_finished_to_transition(
  state: State,
  issue_id: String,
  run_id: String,
  result: Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
) -> State {
  let state = evaluate_scheduled_jobs(state)
  let state =
    run_transition_messages(state, [
      transition_types.WorkerFinished(
        issue_id,
        run_id,
        result,
        transition_lifecycle_context(state),
      ),
    ])
  start_pending_scheduled_runs(state)
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

fn worker_down_to_transition(state: State, down: process.Down) -> State {
  case down {
    process.ProcessDown(monitor, _, _) ->
      handle_registry_down_resolution(
        state,
        worker_registry.resolve_down(state.registry, monitor),
      )
    process.PortDown(_, _, _) -> state
  }
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
  case completion {
    effect_runner.Finished(_, result) ->
      handle_side_effect_result(state, result)
    effect_runner.Crashed(_, effect, reason) -> {
      log_state(state, "warn", "side_effect_crashed", [
        #("effect", effect_runner.effect_kind(effect)),
        #("reason", reason),
      ])
      handle_side_effect_result(state, crash_result_for_effect(effect, reason))
    }
  }
}

fn handle_side_effect_result(
  state: State,
  result: effect_runner.EffectResult,
) -> State {
  case result {
    effect_runner.CandidateFetchFinished(generation, result) ->
      handle_candidate_fetch_finished(state, generation, result)
    effect_runner.RemoteCommandFetchFinished(
      generation,
      candidates,
      dispatch_after,
      result,
    ) ->
      handle_remote_command_fetch_finished(
        state,
        generation,
        candidates,
        dispatch_after,
        result,
      )
    effect_runner.RunningRefreshFinished(generation, result) ->
      handle_running_refresh_finished(state, generation, result)
    effect_runner.RetryRefreshFinished(issue_id, generation, result) ->
      handle_retry_refresh_finished(state, issue_id, generation, result)
    effect_runner.DispatchClaimValidationFinished(issue_id, generation, result) ->
      handle_dispatch_claim_validation_finished(
        state,
        issue_id,
        generation,
        result,
      )
    effect_runner.HandoffClaimFinished(issue_id, run_id, result) ->
      handle_handoff_claim_finished(state, issue_id, run_id, result)
    effect_runner.HandoffSuccessFinished(issue_id, _run_id, result) ->
      handle_handoff_success_finished(state, issue_id, result)
    effect_runner.HandoffFailureFinished(issue_id, _run_id, result) ->
      handle_handoff_failure_finished(state, issue_id, result)
    effect_runner.HandoffParkFinished(issue_id, result) ->
      handle_handoff_park_finished(state, issue_id, result)
    effect_runner.RemoteCommandAckFinished(
      backend_kind,
      task_remote_id,
      event_id,
      outbox_kind,
      result,
    ) ->
      handle_remote_command_ack_finished(
        state,
        backend_kind,
        task_remote_id,
        event_id,
        outbox_kind,
        result,
      )
    effect_runner.InvalidWorkflowReportFinished(
      issue_id,
      violation_fingerprint,
      reporting_policy_fingerprint,
      result,
    ) ->
      handle_invalid_workflow_report_finished(
        state,
        issue_id,
        violation_fingerprint,
        reporting_policy_fingerprint,
        result,
      )
    effect_runner.ScheduledFailureReportFinished(generation, request, result) ->
      handle_scheduled_failure_report_finished(
        state,
        generation,
        request,
        result,
      )
    effect_runner.CleanupFinished(workspace_path, result) ->
      handle_cleanup_finished(state, workspace_path, result)
  }
}

fn crash_result_for_effect(
  effect: effect_runner.Effect,
  reason: String,
) -> effect_runner.EffectResult {
  case effect {
    effect_runner.FetchCandidates(generation, _) ->
      effect_runner.CandidateFetchFinished(
        generation,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.FetchRemoteCommands(
      generation,
      _,
      candidates,
      dispatch_after,
      _,
      _,
    ) ->
      effect_runner.RemoteCommandFetchFinished(
        generation,
        candidates,
        dispatch_after,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.RefreshRunning(generation, _, _) ->
      effect_runner.RunningRefreshFinished(
        generation,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.RefreshRetry(issue_id, generation, _) ->
      effect_runner.RetryRefreshFinished(
        issue_id,
        generation,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.ValidateDispatchClaim(issue_id, generation, _) ->
      effect_runner.DispatchClaimValidationFinished(
        issue_id: issue_id,
        generation: generation,
        result: Error(
          effect_runner.DispatchValidationTrackerError(error.LinearApiRequest(
            reason,
          )),
        ),
      )
    effect_runner.ClaimIssue(issue, _, run_id, _) ->
      effect_runner.HandoffClaimFinished(
        issue.id,
        run_id,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.ReportSuccess(issue_id, _, _, run_id, _, _) ->
      effect_runner.HandoffSuccessFinished(
        issue_id,
        run_id,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.ReportFailure(issue_id, _, _, run_id, _, _) ->
      effect_runner.HandoffFailureFinished(
        issue_id,
        run_id,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.ReportPark(report, _) ->
      effect_runner.HandoffParkFinished(
        report.task.remote_id,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.PostRemoteCommandAck(
      backend_kind,
      task_remote_id,
      event_id,
      _,
      outbox_kind,
      _,
    ) ->
      effect_runner.RemoteCommandAckFinished(
        backend_kind,
        task_remote_id,
        event_id,
        outbox_kind,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.ReportInvalidWorkflow(
      issue,
      _,
      violation_fingerprint,
      reporting_policy_fingerprint,
      _,
      _,
      _,
    ) ->
      effect_runner.InvalidWorkflowReportFinished(
        issue.id,
        violation_fingerprint,
        reporting_policy_fingerprint,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.ReportScheduledFailure(generation, publication, _) ->
      effect_runner.ScheduledFailureReportFinished(
        generation,
        publication,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.CleanupWorkspace(_, workspace_path, _, _) ->
      effect_runner.CleanupFinished(
        workspace_path,
        Error(error.WorkspaceIo(reason)),
      )
  }
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
  issue_id: String,
  run_id: String,
  result: Result(Nil, error.TrackerError),
) -> State {
  run_transition_messages(state, [
    transition_types.HandoffClaimCompleted(
      issue_id,
      run_id,
      handoff_claim_result_for_transition(state, issue_id, run_id, result),
    ),
  ])
}

fn handoff_claim_result_for_transition(
  state: State,
  issue_id: String,
  run_id: String,
  result: Result(Nil, error.TrackerError),
) -> transition_types.HandoffClaimResult {
  case result {
    Error(err) -> transition_types.HandoffClaimFailed(error.tracker_code(err))
    Ok(Nil) ->
      case dict.get(state.pending_claims, issue_id) {
        Error(Nil) -> transition_types.HandoffClaimSucceeded([])
        Ok(pending) ->
          case pending.run_id == run_id {
            False -> transition_types.HandoffClaimSucceeded([])
            True -> claim_ledger_bodies_for_pending(state, pending)
          }
      }
  }
}

fn claim_ledger_bodies_for_pending(
  state: State,
  pending: transition_types.PendingClaim,
) -> transition_types.HandoffClaimResult {
  let post_spawn_runtime =
    core.apply_worker_start(
      state.runtime,
      pending.issue,
      pending.workspace_path,
    )
  let counter = counter_for_runtime(post_spawn_runtime, pending.issue.id)
  case workflow_run_started_body_for_claim(state, pending) {
    Error(reason) -> transition_types.HandoffClaimStartRecordFailed(reason)
    Ok(workflow_started_body) ->
      transition_types.HandoffClaimSucceeded([
        workflow_started_body,
        record.KnownWorkspace(
          pending.issue.id,
          pending.issue.identifier,
          pending.workspace_path,
        ),
        record.RunStarted(
          pending.run_id,
          pending.issue.id,
          pending.issue.identifier,
          pending.workspace_path,
        ),
        record.IssueCounterUpdated(
          pending.issue.id,
          pending.issue.identifier,
          counter.failure_attempts,
          counter.worker_sessions,
          state.dependencies.now_ms(),
          None,
        ),
      ])
  }
}

fn handle_handoff_success_finished(
  state: State,
  issue_id: String,
  result: Result(Nil, error.TrackerError),
) -> State {
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
  issue_id: String,
  result: Result(Nil, error.TrackerError),
) -> State {
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
  issue_id: String,
  result: Result(Nil, error.TrackerError),
) -> State {
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

fn handle_remote_command_ack_finished(
  state: State,
  backend_kind: String,
  task_remote_id: String,
  event_id: String,
  outbox_kind: String,
  result: Result(Nil, error.TrackerError),
) -> State {
  let result = case result {
    Ok(Nil) -> Ok(Nil)
    Error(err) -> Error(error.tracker_code(err))
  }
  run_transition_messages(state, [
    transition_types.RemoteCommandAckFinished(
      backend_kind: backend_kind,
      task_remote_id: task_remote_id,
      event_id: event_id,
      outbox_kind: outbox_kind,
      result: result,
    ),
  ])
}

fn handle_invalid_workflow_report_finished(
  state: State,
  issue_id: String,
  violation_fingerprint: String,
  reporting_policy_fingerprint: String,
  result: Result(effect_runner.InvalidWorkflowReportOutcome, error.TrackerError),
) -> State {
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
  case dict.get(runtime.issue_counters, issue_id) {
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
  enqueue_side_effect(
    state,
    effect_runner.ReportPark(
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
      ),
      require_handoff_capability(state),
    ),
  )
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

fn stop_worker(handle: worker_registry.WorkerHandle) -> Nil {
  case handle.command_subject {
    Some(subject) -> {
      let reply = process.new_subject()
      process.send(subject, worker_command.Abort(reply))
    }
    None -> Nil
  }
  process.kill(handle.pid)
}

fn stop_scheduled_worker(handle: worker_registry.ScheduledWorkerHandle) -> Nil {
  case handle.command_subject {
    Some(subject) -> {
      let reply = process.new_subject()
      process.send(subject, worker_command.Abort(reply))
    }
    None -> Nil
  }
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
  |> list.each(fn(handle) { stop_worker(handle) })
  worker_registry.scheduled_worker_handles(state.registry)
  |> list.each(fn(handle) { stop_scheduled_worker(handle) })
  state.scheduled_retries
  |> dict.values
  |> list.each(fn(entry) { state.dependencies.cancel_timer(entry.timer) })
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
    pending_scheduled_starts: dict.new(),
    scheduled_retries: dict.new(),
    control_server: NoControlServer,
    control_file_path: None,
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
