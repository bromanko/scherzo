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
import scherzo/handoff
import scherzo/linear
import scherzo/linear_triage
import scherzo/log
import scherzo/orchestrator/control_command_handler
import scherzo/orchestrator/core
import scherzo/orchestrator/effect_runner
import scherzo/orchestrator/event_publisher
import scherzo/orchestrator/poll_scheduler
import scherzo/orchestrator/reason as orchestrator_reason
import scherzo/orchestrator/retry_scheduler
import scherzo/orchestrator/schedule_core
import scherzo/orchestrator/state as orchestrator_state
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
import scherzo/state/outbox
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/step_artifact
import scherzo/tracker
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_attempt
import scherzo/workflow_checkpoint
import scherzo/workflow_dag
import scherzo/workflow_fingerprint
import scherzo/workflow_policy
import scherzo/workflow_run
import scherzo/workspace
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

type PendingClaim {
  PendingClaim(
    issue: tracker_issue.Issue,
    workspace_path: String,
    run_id: String,
    session_sequence: Int,
    recovery: Option(session_event.RecoveryInfo),
    remaining_candidates: List(tracker_issue.Issue),
  )
}

type PendingDispatchValidation {
  PendingDispatchValidation(
    issue: tracker_issue.Issue,
    remaining_candidates: List(tracker_issue.Issue),
    generation: Int,
    requested_at_ms: Int,
  )
}

type PendingLinearCommandAck {
  PendingLinearCommandAck(issue_id: String, body: String)
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

type StartupRecovery {
  StartupRecovery(
    runtime: orchestrator_state.RuntimeState,
    retry_timers: List(recovery.RecoveredRetry),
    cleanup_workspaces: List(recovery.CleanupRequest),
    outbox_to_replay: List(recovery.OutboxReplay),
    park_reports: List(handoff.ParkReport),
    command_receipts: Dict(String, projection.CommandReceiptState),
    recovery_by_issue: Dict(String, session_event.RecoveryInfo),
    warnings: List(String),
    workflow_resumptions: List(recovery.RecoveredWorkflowRun),
  )
}

pub type RuntimeDependencies {
  RuntimeDependencies(
    make_tracker: fn(config_types.TrackerConfig) -> tracker.Client,
    make_handoff: fn(config_types.TrackerConfig, config_types.HandoffConfig) ->
      handoff.Client,
    make_linear_commands: fn(config_types.TrackerConfig) -> linear.CommandClient,
    make_triage: fn(
      config_types.TrackerConfig,
      config_types.LinearContractConfig,
    ) -> linear_triage.TriageClient,
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
    handoff_client: handoff.Client,
    linear_command_client: linear.CommandClient,
    triage_client: linear_triage.TriageClient,
    linear_command_state: linear_transport.TransportState,
    pending_linear_command_acks: Dict(String, PendingLinearCommandAck),
    in_flight_linear_command_acks: Dict(String, Bool),
    scheduled_next_due: Dict(String, Int),
    pending_scheduled_starts: Dict(String, ScheduledPendingStart),
    scheduled_retries: Dict(String, ScheduledRetryStart),
    next_scheduled_retry_generation: Int,
    runtime: orchestrator_state.RuntimeState,
    poll: poll_scheduler.State(TimerHandle),
    retry: retry_scheduler.State(TimerHandle),
    registry: worker_registry.Registry,
    pending_claims: Dict(String, PendingClaim),
    pending_dispatch_validations: Dict(String, PendingDispatchValidation),
    next_dispatch_validation_generation: Int,
    recovery_by_issue: Dict(String, session_event.RecoveryInfo),
    effect_runner: effect_runner.Handle,
    effect_runner_monitor: process.Monitor,
    event_hub: process.Subject(hub.Message),
    control_server: ControlServerHandle,
    control_file_path: Option(String),
    operator_paused: Bool,
    dependencies: RuntimeDependencies,
  )
}

pub fn default_dependencies() -> RuntimeDependencies {
  RuntimeDependencies(
    make_tracker: linear.real_client,
    make_handoff: fn(tracker_config, handoff_config) {
      handoff.linear_client(
        tracker_config,
        handoff_config,
        linear.http_transport,
      )
    },
    make_linear_commands: linear.real_command_client,
    make_triage: linear_triage.real_triage_client,
    workflow_run_dependencies: workflow_run.default_dependencies(),
    cleanup: workspace.cleanup_stored_path,
    logger: fn(_level, _event, _fields, _secrets) { Ok(Nil) },
    now_ms: monotonic_ms,
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
      hub.start(hub.default_max_events_per_session, monotonic_ms)
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

fn start_control_plane(
  dependencies: RuntimeDependencies,
  effective: config_types.EffectiveConfig,
  event_hub: process.Subject(hub.Message),
  daemon_subject: process.Subject(Message),
  secrets: List(String),
) -> Result(ControlPlane, StartupError) {
  use token <- try_startup(dependencies.make_control_token())
  let settings =
    control_server.Settings(
      host: "127.0.0.1",
      port: 0,
      token: token,
      event_timeout_ms: 500,
      stream_poll_ms: 100,
      command_timeout_ms: 500,
    )
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
  let tracker_client = dependencies.make_tracker(effective.tracker)
  let handoff_client =
    dependencies.make_handoff(effective.tracker, effective.handoff)
  let linear_command_client =
    dependencies.make_linear_commands(effective.tracker)
  let triage_client =
    dependencies.make_triage(effective.tracker, effective.linear_contract)
  let secrets = config.resolved_secrets(effective)
  use startup_recovery <- try_startup(load_startup_recovery(
    bundle,
    tracker_client,
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
    actor.new_with_initialiser(1000, fn(subject) {
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
                      handoff_client: handoff_client,
                      linear_command_client: linear_command_client,
                      triage_client: triage_client,
                      linear_command_state: linear_command_state,
                      pending_linear_command_acks: dict.new(),
                      in_flight_linear_command_acks: dict.new(),
                      scheduled_next_due: initial_scheduled_next_due(
                        workflow.bundle,
                        dependencies.now_ms(),
                      ),
                      pending_scheduled_starts: dict.new(),
                      scheduled_retries: dict.new(),
                      next_scheduled_retry_generation: 1,
                      runtime: runtime,
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
                      dependencies: dependencies,
                    )
                    |> schedule_recovered_retry_timers(
                      startup_recovery.retry_timers,
                    )
                    |> enqueue_recovered_cleanups(
                      startup_recovery.cleanup_workspaces,
                    )
                    |> enqueue_startup_recovery_outbox(
                      startup_recovery.outbox_to_replay,
                    )
                    |> enqueue_startup_park_reports(
                      startup_recovery.park_reports,
                    )
                    |> recover_scheduled_runtime_state
                    |> spawn_recovered_workflow_resumptions(
                      startup_recovery.workflow_resumptions,
                    )
                    |> log_startup_recovery_warnings(startup_recovery.warnings)
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
  tracker_client: tracker.Client,
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
    tracker_client,
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
) -> Dict(String, Int) {
  let projection =
    scheduled_projection_for_root(bundle.effective.workspace.root)
  bundle.orchestrator.scheduled_jobs
  |> list.filter(fn(job) { job.enabled })
  |> list.fold(dict.new(), fn(acc, job) {
    let next_due = case projection {
      Some(projected) ->
        case projection.scheduled_status_for(projected, job.id) {
          Ok(status) ->
            case status.last_due_at_ms {
              Some(due_at_ms) ->
                schedule_core.next_due_after(due_at_ms, job.every_ms)
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
) -> Option(projection.Projection) {
  case ledger.path_for_workspace_root(workspace_root) {
    Error(_) -> None
    Ok(ledger_path) ->
      case ledger.load_projection(ledger_path) {
        Ok(projected) -> Some(projected)
        Error(_) -> None
      }
  }
}

fn recover_scheduled_runtime_state(state: State) -> State {
  case scheduled_projection_for_root(state.workflow.effective.workspace.root) {
    None -> state
    Some(projected) ->
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
            blocking_reason: option.unwrap(run.reason, ""),
          ),
        ),
      )
    projection.ScheduledActive ->
      recover_interrupted_scheduled_run(state, job, run)
    projection.ScheduledRetryWaiting ->
      recover_scheduled_retry_waiting(state, job, run)
    projection.ScheduledReportRetryWaiting
    | projection.ScheduledIdle
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

fn recover_disabled_scheduled_run(
  state: State,
  status: projection.ScheduledJobStatus,
  run: projection.ScheduledRunSummary,
) -> State {
  case status.state {
    projection.ScheduledDuePending
    | projection.ScheduledPaused
    | projection.ScheduledWaitingForGlobalSlot -> {
      let _ =
        append_ledger_bodies(
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
    True -> state
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
) -> List(handoff.ParkReport) {
  let run_ids = startup_park_report_run_ids(records)
  startup_park_reports_loop(records, run_ids, [], [])
}

fn startup_park_reports_loop(
  records: List(record.LedgerRecord),
  run_ids: Dict(String, String),
  seen_issue_ids: List(String),
  reports: List(handoff.ParkReport),
) -> List(handoff.ParkReport) {
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
  reports: List(handoff.ParkReport),
  issue_id: String,
  issue_identifier: String,
  reason_text: String,
  release_policy: Option(String),
) -> List(handoff.ParkReport) {
  case list.contains(seen_issue_ids, issue_id) {
    True -> startup_park_reports_loop(rest, run_ids, seen_issue_ids, reports)
    False ->
      startup_park_reports_loop(rest, run_ids, [issue_id, ..seen_issue_ids], [
        handoff.ParkReport(
          issue_id: issue_id,
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
        Error(_) -> recovery.WorkflowUnavailable("workflow_fingerprint_failed")
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

fn fetch_recovery_issue_states(
  tracker_client: tracker.Client,
  issue_ids: List(String),
) -> Result(List(tracker_issue.Issue), StartupError) {
  fetch_recovery_issue_chunks(tracker_client, chunk_strings(issue_ids, 50), [])
}

fn fetch_recovery_issue_chunks(
  tracker_client: tracker.Client,
  chunks: List(List(String)),
  acc: List(tracker_issue.Issue),
) -> Result(List(tracker_issue.Issue), StartupError) {
  case chunks {
    [] -> Ok(list.reverse(acc))
    [chunk, ..rest] ->
      case tracker_client.fetch_issue_states_by_ids(chunk) {
        Ok(issues) ->
          fetch_recovery_issue_chunks(
            tracker_client,
            rest,
            list.append(list.reverse(issues), acc),
          )
        Error(err) ->
          Error(StartupError(
            "recovery_issue_fetch_failed",
            error.tracker_code(err),
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

fn schedule_recovered_retry_timers(
  state: State,
  retries: List(recovery.RecoveredRetry),
) -> State {
  list.fold(retries, state, fn(state, retry) {
    let recovery.RecoveredRetry(
      issue_id,
      issue_identifier,
      delay_ms,
      generation,
      reason_text,
    ) = retry
    let safe_reason =
      log.truncate(
        log.redact("recovery_reason", reason_text, state.workflow.secrets),
        200,
      )
    log_state(state, "info", "workflow_recovery_status", [
      #("issue_id", issue_id),
      #("issue_identifier", issue_identifier),
      #("status", "recovered"),
      #("source", "recovery.recovered_retry"),
      #("delay_ms", int.to_string(delay_ms)),
      #("generation", int.to_string(generation)),
      #("reason", safe_reason),
    ])
    log_state(state, "info", "recovered_retry_scheduled", [
      #("issue_id", issue_id),
      #("delay_ms", int.to_string(delay_ms)),
      #("generation", int.to_string(generation)),
      #("reason", safe_reason),
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
  })
}

fn enqueue_recovered_cleanups(
  state: State,
  cleanups: List(recovery.CleanupRequest),
) -> State {
  list.fold(cleanups, state, fn(state, cleanup) {
    let recovery.CleanupRequest(issue_id, issue_identifier, workspace_path) =
      cleanup
    log_state(state, "info", "workflow_recovery_status", [
      #("issue_id", issue_id),
      #("issue_identifier", issue_identifier),
      #("status", "cleanup"),
      #("source", "recovery.cleanup_request"),
      #("reason", "terminal interrupted run cleanup queued"),
    ])
    log_state(state, "info", "recovered_workspace_cleanup", [
      #("issue_id", issue_id),
      #("workspace_path", workspace_path),
    ])
    enqueue_side_effect(
      state,
      effect_runner.CleanupWorkspace(
        root: state.workflow.effective.workspace.root,
        workspace_path: workspace_path,
        hooks: state.workflow.effective.hooks,
        cleanup: state.dependencies.cleanup,
      ),
    )
  })
}

fn enqueue_startup_recovery_outbox(
  state: State,
  outbox_entries: List(recovery.OutboxReplay),
) -> State {
  list.fold(outbox_entries, state, fn(state, entry) {
    let recovery.OutboxReplay(outbox_id, issue_id, outbox_kind, _, payload_json) =
      entry
    case outbox.decode_payload(payload_json) {
      Error(error) ->
        fail_startup_recovery_outbox(
          state,
          outbox_id,
          issue_id,
          outbox_kind,
          error,
        )
      Ok(payload) ->
        case outbox.recovery_replay_error(outbox_kind, payload.kind) {
          Error(error) ->
            fail_startup_recovery_outbox(
              state,
              outbox_id,
              issue_id,
              outbox_kind,
              error,
            )
          Ok(Nil) -> {
            log_state(state, "info", "outbox_replay_enqueued", [
              #("outbox_id", outbox_id),
              #("issue_id", issue_id),
              #("kind", outbox_kind),
            ])
            let source_comment_id = case payload.source_comment_id {
              Some(source_comment_id) -> source_comment_id
              None -> outbox_id
            }
            let state =
              State(
                ..state,
                linear_command_state: linear_transport.mark_processed(
                  state.linear_command_state,
                  source_comment_id,
                ),
              )
            state
            |> remember_pending_linear_command_ack(
              issue_id,
              source_comment_id,
              payload.body,
            )
            |> enqueue_pending_linear_command_ack_effect(
              issue_id,
              source_comment_id,
              payload.body,
            )
          }
        }
    }
  })
}

fn enqueue_startup_park_reports(
  state: State,
  reports: List(handoff.ParkReport),
) -> State {
  list.fold(reports, state, fn(state, report) {
    enqueue_side_effect(
      state,
      effect_runner.ReportPark(report, state.handoff_client),
    )
  })
}

fn fail_startup_recovery_outbox(
  state: State,
  outbox_id: String,
  issue_id: String,
  outbox_kind: String,
  error: outbox.ReplayError,
) -> State {
  let error_code = outbox.replay_error_code(error)
  let body = record.OutboxFailed(outbox_id, issue_id, outbox_kind, error_code)
  log_state(state, "warn", "outbox_replay_failed", [
    #("outbox_id", outbox_id),
    #("issue_id", issue_id),
    #("kind", outbox_kind),
    #("error", error_code),
    #("reason", outbox.describe_replay_error(error)),
  ])
  let _ = append_ledger_bodies(state, [body], "ledger_append_failed")
  state
}

fn log_startup_recovery_warnings(
  state: State,
  warnings: List(String),
) -> State {
  list.each(warnings, fn(warning) {
    let safe_warning =
      log.truncate(
        log.redact("recovery_warning", warning, state.workflow.secrets),
        200,
      )
    log_state(state, "warn", "workflow_recovery_status", [
      #("status", "recovered"),
      #("source", "recovery.warning"),
      #("reason", safe_warning),
    ])
    log_state(state, "warn", "startup_recovery_warning", [
      #("warning", safe_warning),
    ])
  })
  state
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
      State(
        ..state,
        runtime: runtime,
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
        False ->
          Error(yaml_worker_failure(
            "workflow_recovery_invalid:workflow_drift",
            Some(recovered.run_root),
            recovered.issue,
          ))
        True -> {
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
              ),
              next_attempt_indexes: recovered.next_attempt_indexes,
              run_root: Some(recovered.run_root),
              pi_session_continuations: recovered.pi_session_continuations,
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

fn recovered_workflow_identity_matches(
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  recovered: recovery.RecoveredWorkflowRun,
) -> Bool {
  case dag.id == recovered.workflow_id {
    False -> False
    True ->
      case workflow_fingerprint.fingerprint_for_execution(dag, orchestrator) {
        Error(_) -> False
        Ok(fingerprint) -> fingerprint == recovered.workflow_fingerprint
      }
  }
}

fn recovered_workspaces_to_prepared(
  workspaces: Dict(String, recovery.RecoveredWorkspaceSummary),
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
        attempt_index: workspace.attempt_index,
        workspace_name: workspace.workspace_name,
        path: workspace.path,
        source_workspace_name: workspace.source_workspace_name,
        source_workspace_path: workspace.source_workspace_path,
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
    PollTick(generation) -> actor.continue(handle_poll_tick(state, generation))
    RetryTick(issue_id, generation) ->
      actor.continue(handle_retry_tick(state, issue_id, generation))
    WorkerFinished(issue_id, run_id, result) ->
      actor.continue(handle_worker_finished(state, issue_id, run_id, result))
    ScheduledWorkerFinished(run_id, result) ->
      actor.continue(handle_scheduled_worker_finished(state, run_id, result))
    ScheduledRetryTick(run_id, generation) ->
      actor.continue(handle_scheduled_retry_tick(state, run_id, generation))
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
    WorkerDown(down) -> actor.continue(handle_worker_down(state, down))
    EffectRunnerDown(down) -> {
      let _ = handle_effect_runner_down(state, down)
      actor.stop_abnormal("effect_runner_down")
    }
    SideEffectCompleted(completion) ->
      actor.continue(handle_side_effect_completed(state, completion))
    GetSnapshot(reply) -> {
      process.send(reply, state.runtime)
      actor.continue(state)
    }
    ApplyOperatorCommand(operator_command, timeout_ms, reply) ->
      actor.continue(handle_operator_command(
        state,
        operator_command,
        timeout_ms,
        reply,
      ))
    Shutdown(reply) -> {
      let state = shutdown_state(state)
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
  case worker_registry.stopped_yaml_run_reason(state.registry, run_id) {
    Ok(reason) -> {
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
    Error(Nil) ->
      case worker_registry.worker_for_run(state.registry, run_id) {
        Ok(_) ->
          State(
            ..state,
            registry: worker_registry.register_yaml_step_started(
              state.registry,
              session_id,
              run_id,
            ),
          )
        Error(Nil) -> state
      }
  }
}

fn handle_yaml_step_finished(state: State, session_id: String) -> State {
  State(
    ..state,
    registry: worker_registry.finish_yaml_step(state.registry, session_id),
  )
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
    worker_registry.UnknownDown(registry) -> {
      log_state(state, "warn", "worker_down_stale", [])
      State(..state, registry: registry)
    }
    worker_registry.StepCommandDown(registry, session_id) -> {
      log_state(state, "warn", "yaml_step_command_down", [
        #("session_id", session_id),
      ])
      State(..state, registry: registry)
    }
    worker_registry.WorkerDown(registry, issue_id, handle) -> {
      let state = State(..state, registry: registry)
      log_state(state, "warn", "worker_down", [#("issue_id", issue_id)])
      append_workflow_interrupted_terminal(state, handle, "worker_down")
      event_publisher.lifecycle(
        state.event_hub,
        handle.session_id,
        session_event.WorkerDown,
        None,
      )
      let failure =
        agent_types.WorkerFailure(
          reason: error.PiFailed(
            error.PiProtocolError(session_reason.to_string(
              session_reason.WorkerDown,
            )),
          ),
          workspace_path: Some(handle.workspace_path),
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        )
      finish_worker_failure(state, handle, failure)
    }
    worker_registry.WorkerDownStale(registry, _issue_id) -> {
      log_state(state, "warn", "worker_down_stale", [])
      State(..state, registry: registry)
    }
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

fn handle_operator_command(
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
  control_command_handler.apply(
    control_command_handler.Context(
      state: state,
      pending_claim_count: fn(state) { dict.size(state.pending_claims) },
      set_paused: set_operator_paused,
      reload_workflow: reload_workflow_for_operator,
      retry_issue: retry_issue_for_operator,
      park_issue: park_issue_for_operator,
      unpark_issue: unpark_issue_for_operator,
      run_schedule_now: schedule_run_now_for_operator,
      abort_session: abort_session_for_operator_sync,
      route_worker_command: route_worker_command_sync,
      log_result: log_operator_result,
    ),
    operator_command,
    timeout_ms,
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
  case state.workflow.reload_state.current_status {
    config.CurrentValid -> #(
      state,
      command.applied(operator_command, Some("workflow reloaded")),
    )
    config.CurrentInvalid(reason) -> #(
      state,
      command.rejected(operator_command, reason, Some("workflow reload failed")),
    )
  }
}

fn retry_issue_for_operator(
  state: State,
  operator_command: command.OperatorCommand,
  issue_ref: command.IssueRef,
) -> #(State, command.CommandResult) {
  case issue_for_ref(state, issue_ref) {
    Error(command.NotFound) -> #(
      state,
      command.not_found(operator_command, Some("issue not found")),
    )
    Error(command.Rejected(reason)) -> #(
      state,
      command.rejected(operator_command, reason, Some(reason)),
    )
    Error(command.NotAllowed(reason)) -> #(
      state,
      command.not_allowed(operator_command, reason, Some(reason)),
    )
    Error(_) -> #(
      state,
      command.rejected(operator_command, "issue_resolution_failed", None),
    )
    Ok(issue) ->
      case issue_is_running_claimed_or_pending(state, issue.id) {
        True -> #(
          state,
          command.rejected(
            operator_command,
            "issue_already_active",
            Some("issue is running, claimed, or pending claim"),
          ),
        )
        False ->
          case state.operator_paused {
            True -> #(
              state,
              command.rejected(
                operator_command,
                "dispatch_paused",
                Some("dispatch is paused"),
              ),
            )
            False -> retry_resolved_issue(state, operator_command, issue)
          }
      }
  }
}

fn retry_resolved_issue(
  state: State,
  operator_command: command.OperatorCommand,
  issue: tracker_issue.Issue,
) -> #(State, command.CommandResult) {
  let _ =
    append_ledger_bodies(
      state,
      [
        record.IssueUnparked(issue.id, issue.identifier, "operator_retry"),
        record.IssueCounterUpdated(
          issue.id,
          issue.identifier,
          0,
          0,
          state.dependencies.now_ms(),
          None,
        ),
      ],
      "ledger_append_failed",
    )
  let runtime =
    orchestrator_state.RuntimeState(
      ..state.runtime,
      parked: dict.delete(state.runtime.parked, issue.id),
      retry_attempts: dict.delete(state.runtime.retry_attempts, issue.id),
      issue_counters: dict.delete(state.runtime.issue_counters, issue.id),
    )
  let state =
    State(
      ..state,
      runtime: runtime,
      recovery_by_issue: dict.delete(state.recovery_by_issue, issue.id),
    )
    |> cancel_retry_timer(issue.id)
  case
    config.can_dispatch(state.workflow.reload_state)
    && core.should_dispatch(state.runtime, state.workflow.effective, issue)
    && can_reserve_dispatch_slot(state, issue)
  {
    True -> {
      let state = dispatch_issue(state, issue)
      #(state, command.applied(operator_command, Some("retry dispatched")))
    }
    False -> #(
      state,
      command.rejected(
        operator_command,
        "not_dispatchable",
        Some("issue is not currently dispatchable"),
      ),
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
        Error(_) -> #(
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

fn park_issue_for_operator(
  state: State,
  operator_command: command.OperatorCommand,
  issue_ref: command.IssueRef,
  reason: String,
) -> #(State, command.CommandResult) {
  case issue_for_ref(state, issue_ref) {
    Error(command.NotFound) -> #(
      state,
      command.not_found(operator_command, Some("issue not found")),
    )
    Error(command.Rejected(reason)) -> #(
      state,
      command.rejected(operator_command, reason, Some(reason)),
    )
    Error(command.NotAllowed(reason)) -> #(
      state,
      command.not_allowed(operator_command, reason, Some(reason)),
    )
    Error(_) -> #(
      state,
      command.rejected(operator_command, "issue_resolution_failed", None),
    )
    Ok(issue) ->
      case issue_is_running_claimed_or_pending(state, issue.id) {
        True -> #(
          state,
          command.rejected(
            operator_command,
            "issue_active",
            Some(
              "running, claimed, or pending issues must be stopped before parking",
            ),
          ),
        )
        False -> {
          let state =
            park_issue_state(
              state,
              issue,
              orchestrator_reason.ParkOperator(reason),
            )
          #(state, command.applied(operator_command, Some("issue parked")))
        }
      }
  }
}

fn unpark_issue_for_operator(
  state: State,
  operator_command: command.OperatorCommand,
  issue_ref: command.IssueRef,
) -> #(State, command.CommandResult) {
  case parked_issue_id_for_ref(state, issue_ref) {
    Ok(issue_id) -> {
      let state = unpark_issue_state(state, issue_id)
      #(state, command.applied(operator_command, Some("issue unparked")))
    }
    Error(command.NotFound) -> #(
      state,
      command.not_found(operator_command, Some("parked issue not found")),
    )
    Error(command.Rejected(reason)) -> #(
      state,
      command.rejected(operator_command, reason, Some(reason)),
    )
    Error(command.NotAllowed(reason)) -> #(
      state,
      command.not_allowed(operator_command, reason, Some(reason)),
    )
    Error(_) -> #(
      state,
      command.rejected(operator_command, "issue_resolution_failed", None),
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
    Error(_) -> #(
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
    Error(_) -> #(
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
    Error(_) -> #(
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
        Error(_) -> #(
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
      append_workflow_cancelled_terminal(state, handle, reason_text, 0, 0)
      process.demonitor_process(handle.monitor)
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
      let state =
        State(
          ..state,
          registry: worker_registry.mark_yaml_run_stopping(
            state.registry,
            handle.run_id,
            reason,
          ),
        )
      process.kill(handle.pid)
      let state =
        finish_yaml_step_sessions_for_run(state, handle.run_id, reason)
      let state = clear_yaml_step_command_routes_for_run(state, handle.run_id)
      let registry =
        worker_registry.remove_worker_handle(state.registry, handle)
      let state =
        State(..state, registry: registry)
        |> park_issue_state_with_run(
          handle.issue,
          orchestrator_reason.ParkOperator(reason_text),
          Some(handle.run_id),
        )
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

fn active_run_issues(state: State) -> List(tracker_issue.Issue) {
  let runtime =
    state.runtime.running
    |> dict.values
    |> list.map(fn(entry) { entry.issue })
  let active_workers = worker_registry.worker_issues(state.registry)
  []
  |> append_unique_issues(runtime)
  |> append_unique_issues(active_workers)
}

fn append_unique_issues(
  existing: List(tracker_issue.Issue),
  values: List(tracker_issue.Issue),
) -> List(tracker_issue.Issue) {
  list.fold(values, existing, fn(acc, issue) {
    case list.contains(list.map(acc, fn(item) { item.id }), issue.id) {
      True -> acc
      False -> list.append(acc, [issue])
    }
  })
}

fn issue_is_running_claimed_or_pending(state: State, issue_id: String) -> Bool {
  has_active_run(state, issue_id)
  || dict.has_key(state.runtime.running, issue_id)
  || dict.has_key(state.runtime.claimed, issue_id)
  || dict.has_key(state.pending_claims, issue_id)
  || dict.has_key(state.pending_dispatch_validations, issue_id)
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
  case state.tracker_client.fetch_candidate_issues() {
    Error(_) -> Error(command.Rejected("candidate_fetch_failed"))
    Ok(issues) ->
      issues
      |> list.filter(fn(issue) { issue.identifier == identifier })
      |> unique_issue
  }
}

fn fetch_issue_by_id(
  state: State,
  issue_id: String,
) -> Result(tracker_issue.Issue, command.CommandStatus) {
  case state.tracker_client.fetch_issue_states_by_ids([issue_id]) {
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

fn park_issue_state(
  state: State,
  issue: tracker_issue.Issue,
  reason: orchestrator_reason.ParkReason,
) -> State {
  park_issue_state_with_run(state, issue, reason, None)
}

fn park_issue_state_with_run(
  state: State,
  issue: tracker_issue.Issue,
  reason: orchestrator_reason.ParkReason,
  source_run_id: Option(String),
) -> State {
  let parked =
    orchestrator_state.ParkedEntry(
      issue_id: issue.id,
      identifier: issue.identifier,
      reason: reason,
      release_policy: orchestrator_state.ExplicitUnparkOnly,
      parked_at_ms: state.dependencies.now_ms(),
    )
  let runtime =
    orchestrator_state.RuntimeState(
      ..state.runtime,
      running: dict.delete(state.runtime.running, issue.id),
      claimed: dict.delete(state.runtime.claimed, issue.id),
      retry_attempts: dict.delete(state.runtime.retry_attempts, issue.id),
      issue_counters: dict.delete(state.runtime.issue_counters, issue.id),
      parked: dict.insert(state.runtime.parked, issue.id, parked),
    )
  let state = State(..state, runtime: runtime) |> cancel_retry_timer(issue.id)
  let reason_text = orchestrator_reason.park_to_string(reason)
  let appended = append_parked_record_for_runtime(state, issue.id, reason_text)
  log_state(state, "warn", "issue_parked", [
    #("issue_id", issue.id),
    #("reason", reason_text),
  ])
  case appended {
    False -> state
    True -> enqueue_park_report_for_runtime(state, issue.id, source_run_id)
  }
}

fn unpark_issue_state(state: State, issue_id: String) -> State {
  let issue_identifier = identifier_for_runtime(state.runtime, issue_id)
  let runtime =
    orchestrator_state.RuntimeState(
      ..state.runtime,
      parked: dict.delete(state.runtime.parked, issue_id),
      retry_attempts: dict.delete(state.runtime.retry_attempts, issue_id),
      issue_counters: dict.delete(state.runtime.issue_counters, issue_id),
    )
  let state =
    State(
      ..state,
      runtime: runtime,
      recovery_by_issue: dict.delete(state.recovery_by_issue, issue_id),
    )
    |> cancel_retry_timer(issue_id)
  let _ =
    append_ledger_bodies(
      state,
      [
        record.IssueUnparked(issue_id, issue_identifier, "operator"),
        record.IssueCounterUpdated(
          issue_id,
          issue_identifier,
          0,
          0,
          state.dependencies.now_ms(),
          None,
        ),
      ],
      "ledger_append_failed",
    )
  log_state(state, "info", "issue_unparked", [#("issue_id", issue_id)])
  state
}

fn unpark_if_issue_changed_state(
  state: State,
  issue: tracker_issue.Issue,
) -> State {
  let had_retry = dict.has_key(state.runtime.retry_attempts, issue.id)
  let was_parked = dict.has_key(state.runtime.parked, issue.id)
  let runtime = core.unpark_if_issue_changed(state.runtime, issue)
  let state = case was_parked && !dict.has_key(runtime.parked, issue.id) {
    True ->
      State(
        ..state,
        runtime: runtime,
        recovery_by_issue: dict.delete(state.recovery_by_issue, issue.id),
      )
    False -> State(..state, runtime: runtime)
  }
  let state = case was_parked && !dict.has_key(runtime.parked, issue.id) {
    True -> {
      let _ =
        append_ledger_bodies(
          state,
          [
            record.IssueUnparked(issue.id, issue.identifier, "issue_changed"),
            record.IssueCounterUpdated(
              issue.id,
              issue.identifier,
              0,
              0,
              state.dependencies.now_ms(),
              None,
            ),
          ],
          "ledger_append_failed",
        )
      state
    }
    False -> state
  }
  case had_retry && !dict.has_key(runtime.retry_attempts, issue.id) {
    True ->
      State(
        ..state,
        recovery_by_issue: dict.delete(state.recovery_by_issue, issue.id),
      )
      |> cancel_retry_timer(issue.id)
    False -> state
  }
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

fn handle_poll_tick(state: State, generation: Int) -> State {
  case poll_scheduler.accept_tick(state.poll, generation) {
    Error(Nil) -> state
    Ok(poll) -> {
      let state = State(..state, poll: poll)
      log_state(state, "info", "tick_started", [
        #("generation", int.to_string(generation)),
      ])
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
    workflow_reloader.Invalid(workflow, reason) -> {
      let state = State(..state, workflow: workflow)
      log_state(state, "warn", "workflow_reload_failed", [#("error", reason)])
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
  let state =
    State(
      ..state,
      workflow: workflow,
      tracker_client: state.dependencies.make_tracker(effective.tracker),
      handoff_client: state.dependencies.make_handoff(
        effective.tracker,
        effective.handoff,
      ),
      linear_command_client: state.dependencies.make_linear_commands(
        effective.tracker,
      ),
      triage_client: state.dependencies.make_triage(
        effective.tracker,
        effective.linear_contract,
      ),
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
          client: state.tracker_client,
        ),
      )
  }
}

fn begin_candidate_fetch_or_finish(state: State, generation: Int) -> State {
  case
    !state.operator_paused
    && config.can_dispatch(state.workflow.reload_state)
    && state.workflow.effective.agent.max_concurrent_agents != 0
    && slots_remain(state)
  {
    False -> begin_linear_command_fetch_or_finish(state, generation, [], False)
    True ->
      enqueue_side_effect(
        state,
        effect_runner.FetchCandidates(
          generation: generation,
          client: state.tracker_client,
        ),
      )
  }
}

fn handle_running_refresh_finished(
  state: State,
  generation: Int,
  result: Result(List(tracker_issue.Issue), error.TrackerError),
) -> State {
  case poll_result_is_stale(state, generation) {
    True -> state
    False -> {
      let state = case result {
        Error(err) -> {
          log_state(state, "warn", "running_refresh_failed", [
            #("error", error.tracker_code(err)),
          ])
          state
        }
        Ok(issues) ->
          list.fold(issues, state, fn(acc, issue) {
            let transition =
              core.reconcile_issue(acc.runtime, acc.workflow.effective, issue)
            let acc = State(..acc, runtime: transition.state)
            apply_effects(acc, transition.effects)
          })
      }
      begin_candidate_fetch_or_finish(state, generation)
    }
  }
}

fn handle_candidate_fetch_finished(
  state: State,
  generation: Int,
  result: Result(List(tracker_issue.Issue), error.TrackerError),
) -> State {
  case poll_result_is_stale(state, generation) {
    True -> state
    False ->
      case result {
        Error(err) -> {
          log_state(state, "warn", "candidate_fetch_failed", [
            #("error", error.tracker_code(err)),
          ])
          begin_linear_command_fetch_or_finish(state, generation, [], False)
        }
        Ok(candidates) -> {
          log_state(state, "info", "candidates_fetched", [
            #("count", int.to_string(list.length(candidates))),
          ])
          begin_linear_command_fetch_or_finish(
            state,
            generation,
            core.sort_candidates(candidates),
            True,
          )
        }
      }
  }
}

fn begin_linear_command_fetch_or_finish(
  state: State,
  generation: Int,
  candidates: List(tracker_issue.Issue),
  dispatch_after: Bool,
) -> State {
  case state.workflow.effective.linear_commands.enabled {
    False -> finish_linear_command_phase(state, candidates, dispatch_after)
    True -> {
      let issue_ids = observed_issue_ids(state, candidates)
      case issue_ids {
        [] -> finish_linear_command_phase(state, candidates, dispatch_after)
        _ ->
          enqueue_side_effect(
            state,
            effect_runner.FetchLinearCommands(
              generation: generation,
              issue_ids: issue_ids,
              candidates: candidates,
              dispatch_after: dispatch_after,
              client: state.linear_command_client,
              limit_per_issue: state.workflow.effective.linear_commands.poll_limit_per_issue,
            ),
          )
      }
    }
  }
}

fn handle_linear_command_fetch_finished(
  state: State,
  generation: Int,
  candidates: List(tracker_issue.Issue),
  dispatch_after: Bool,
  result: Result(List(linear.LinearComment), error.TrackerError),
) -> State {
  case poll_result_is_stale(state, generation) {
    True -> state
    False -> {
      let state = case result {
        Error(err) -> {
          log_state(state, "warn", "linear_command_fetch_failed", [
            #("error", error.tracker_code(err)),
          ])
          state
        }
        Ok(comments) -> process_linear_command_comments(state, comments)
      }
      finish_linear_command_phase(state, candidates, dispatch_after)
    }
  }
}

fn finish_linear_command_phase(
  state: State,
  candidates: List(tracker_issue.Issue),
  dispatch_after: Bool,
) -> State {
  let state = retry_pending_linear_command_acks(state)
  let state = case dispatch_after {
    True -> dispatch_candidates(candidates, state)
    False -> state
  }
  schedule_next_poll(state)
}

fn process_linear_command_comments(
  state: State,
  comments: List(linear.LinearComment),
) -> State {
  let #(transport_state, actions) =
    linear_transport.process_comments(
      state.linear_command_state,
      state.workflow.effective.linear_commands,
      comments,
      worker_registry.issue_sessions(state.registry),
    )
  let state = State(..state, linear_command_state: transport_state)
  apply_linear_transport_actions(state, actions)
}

fn apply_linear_transport_actions(
  state: State,
  actions: List(linear_transport.TransportAction),
) -> State {
  case actions {
    [] -> state
    [action, ..rest] ->
      apply_linear_transport_actions(
        apply_linear_transport_action(state, action),
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
    | command.ParkIssue(_, _)
    | command.UnparkIssue(_)
    | command.RunScheduleNow(_) -> False
  }
}

fn apply_linear_transport_action(
  state: State,
  action: linear_transport.TransportAction,
) -> State {
  case action {
    linear_transport.SubmitCommand(comment, parsed) ->
      apply_linear_submit_command(state, comment, parsed)
    linear_transport.PostAck(issue_id, source_comment_id, body) ->
      enqueue_linear_command_ack(state, issue_id, source_comment_id, body)
    linear_transport.LogIgnored(reason, comment_id) -> {
      log_state(state, "info", "linear_command_ignored", [
        #("comment_id", comment_id),
        #("reason", reason),
      ])
      state
    }
  }
}

fn apply_linear_submit_command(
  state: State,
  comment: linear.LinearComment,
  parsed: linear_parser.ParsedLinearCommand,
) -> State {
  let command_name = command.command_name(parsed.command)
  case
    append_ledger_bodies(
      state,
      [
        record.LinearCommandSeen(
          comment.id,
          comment.issue_id,
          comment.author.id,
          command_name,
          safe_linear_command_excerpt(parsed.excerpt, state.workflow.secrets),
        ),
        record.LinearCommandStarted(comment.id, comment.issue_id, command_name),
      ],
      "linear_command_start_record_failed",
    )
  {
    False -> state
    True -> {
      let state_before_command = state
      let #(state, result) =
        apply_operator_command_to_state(state, parsed.command, 1000)
      let ack_result =
        command_result_with_display_target(
          state_before_command,
          parsed.command,
          result,
        )
      log_state(state, "info", "linear_operator_command", [
        #("comment_id", comment.id),
        #("command", result.command),
        #("status", command.status_to_string(result.status)),
      ])
      case
        append_ledger_bodies(
          state,
          [
            record.LinearCommandCompleted(
              comment.id,
              comment.issue_id,
              command.status_to_string(result.status),
              result_message_excerpt(ack_result, state.workflow.secrets),
            ),
          ],
          "linear_command_completion_record_failed",
        )
      {
        False -> state
        True ->
          case
            linear_transport.should_ack_result(
              state.workflow.effective.linear_commands,
              result,
            )
          {
            True -> {
              let body =
                linear_transport.result_ack_body(
                  comment.id,
                  parsed,
                  ack_result,
                  state.workflow.secrets,
                )
              enqueue_linear_command_ack(
                state,
                comment.issue_id,
                comment.id,
                body,
              )
            }
            False -> state
          }
      }
    }
  }
}

fn enqueue_linear_command_ack(
  state: State,
  issue_id: String,
  source_comment_id: String,
  body: String,
) -> State {
  case
    append_ledger_bodies(
      state,
      [
        record.OutboxPendingV2(
          source_comment_id,
          issue_id,
          "linear_command_ack",
          "linear_command_ack:" <> source_comment_id,
          outbox.linear_command_ack_payload(
            source_comment_id,
            body,
            state.workflow.secrets,
          ),
        ),
      ],
      "linear_command_ack_outbox_record_failed",
    )
  {
    False -> state
    True ->
      state
      |> remember_pending_linear_command_ack(issue_id, source_comment_id, body)
      |> enqueue_pending_linear_command_ack_effect(
        issue_id,
        source_comment_id,
        body,
      )
  }
}

fn remember_pending_linear_command_ack(
  state: State,
  issue_id: String,
  source_comment_id: String,
  body: String,
) -> State {
  State(
    ..state,
    pending_linear_command_acks: dict.insert(
      state.pending_linear_command_acks,
      source_comment_id,
      PendingLinearCommandAck(issue_id, body),
    ),
  )
}

fn enqueue_pending_linear_command_ack_effect(
  state: State,
  issue_id: String,
  source_comment_id: String,
  body: String,
) -> State {
  case dict.has_key(state.in_flight_linear_command_acks, source_comment_id) {
    True -> state
    False -> {
      let state =
        State(
          ..state,
          in_flight_linear_command_acks: dict.insert(
            state.in_flight_linear_command_acks,
            source_comment_id,
            True,
          ),
        )
      enqueue_side_effect(
        state,
        effect_runner.PostLinearCommandAck(
          issue_id: issue_id,
          source_comment_id: source_comment_id,
          body: body,
          client: state.linear_command_client,
        ),
      )
    }
  }
}

fn retry_pending_linear_command_acks(state: State) -> State {
  state.pending_linear_command_acks
  |> dict.to_list
  |> list.fold(state, fn(state, entry) {
    let #(source_comment_id, pending) = entry
    let PendingLinearCommandAck(issue_id, body) = pending
    case dict.has_key(state.in_flight_linear_command_acks, source_comment_id) {
      True -> state
      False -> {
        log_state(state, "info", "linear_command_ack_retry_enqueued", [
          #("issue_id", issue_id),
          #("comment_id", source_comment_id),
        ])
        enqueue_pending_linear_command_ack_effect(
          state,
          issue_id,
          source_comment_id,
          body,
        )
      }
    }
  })
}

fn safe_linear_command_excerpt(value: String, secrets: List(String)) -> String {
  log.redact("linear_command_receipt", value, secrets)
  |> log.truncate(record.max_excerpt_chars)
}

fn result_message_excerpt(
  result: command.CommandResult,
  secrets: List(String),
) -> String {
  case result.message {
    Some(message) -> safe_linear_command_excerpt(message, secrets)
    None -> ""
  }
}

fn observed_issue_ids(
  state: State,
  candidates: List(tracker_issue.Issue),
) -> List(String) {
  active_run_issue_ids(state)
  |> append_unique_list(dict.keys(state.runtime.retry_attempts))
  |> append_unique_list(dict.keys(state.runtime.parked))
  |> append_unique_list(list.map(candidates, fn(issue) { issue.id }))
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

fn dispatch_candidates(
  issues: List(tracker_issue.Issue),
  state: State,
) -> State {
  case
    !state.operator_paused && config.can_dispatch(state.workflow.reload_state)
  {
    False -> state
    True ->
      case issues {
        [] -> state
        [issue, ..rest] -> {
          let state = unpark_if_issue_changed_state(state, issue)
          case core.blocker_decision(state.workflow.effective, issue) {
            core.BlockedByDependency(_, _) -> {
              let state =
                report_blocked_dependency(
                  state,
                  issue,
                  "candidate",
                  "linear_dependency_blocked_candidate",
                  core.blocker_decision(state.workflow.effective, issue),
                )
              dispatch_candidates(rest, state)
            }
            core.BlockersSatisfied -> {
              let runtime =
                core.clear_blocked_dependency_report(
                  state.runtime,
                  issue.id,
                  "candidate",
                )
              let state = State(..state, runtime: runtime)
              case
                core.dispatch_preconditions_satisfied_without_slot_capacity(
                  state.runtime,
                  state.workflow.effective,
                  issue,
                )
                && !dict.has_key(state.pending_claims, issue.id)
                && !dict.has_key(state.pending_dispatch_validations, issue.id)
              {
                False -> dispatch_candidates(rest, state)
                True ->
                  case
                    workflow_policy.classify_issue(
                      state.workflow.effective.linear_contract,
                      issue,
                    )
                  {
                    workflow_policy.WorkflowInvalid(violation) ->
                      handle_invalid_workflow_candidate(
                        state,
                        issue,
                        violation,
                        rest,
                      )
                    workflow_policy.WorkflowPolicyDisabled
                    | workflow_policy.WorkflowSelected(_, _) ->
                      handle_valid_workflow_candidate(state, issue, rest)
                  }
              }
            }
          }
        }
      }
  }
}

fn handle_valid_workflow_candidate(
  state: State,
  issue: tracker_issue.Issue,
  remaining_candidates: List(tracker_issue.Issue),
) -> State {
  let runtime = core.clear_invalid_workflow_report(state.runtime, issue.id)
  let state = State(..state, runtime: runtime)
  case can_reserve_dispatch_slot(state, issue) {
    False -> dispatch_candidates(remaining_candidates, state)
    True -> begin_dispatch_validation(state, issue, remaining_candidates)
  }
}

fn report_blocked_dependency(
  state: State,
  issue: tracker_issue.Issue,
  phase: String,
  event: String,
  decision: core.BlockerDecision,
) -> State {
  case
    core.already_reported_blocked_dependency(
      state.runtime,
      state.workflow.effective,
      issue,
      phase,
      decision,
    )
  {
    True -> state
    False -> {
      log_state(state, "warn", event, [
        #("issue_id", issue.id),
        #("issue_identifier", issue.identifier),
        #("phase", phase),
        #(
          "blocker_fingerprint",
          core.blocked_dependency_fingerprint(
            state.workflow.effective,
            issue,
            phase,
            decision,
          ),
        ),
        #("blockers", blocker_summary(issue, decision)),
        #("incomplete", bool_field(core.blocker_decision_incomplete(decision))),
      ])
      let runtime =
        core.mark_blocked_dependency_reported(
          state.runtime,
          state.workflow.effective,
          issue,
          phase,
          decision,
          state.dependencies.now_ms(),
        )
      State(..state, runtime: runtime)
    }
  }
}

fn blocker_summary(
  issue: tracker_issue.Issue,
  decision: core.BlockerDecision,
) -> String {
  let blockers = case decision {
    core.BlockersSatisfied -> issue.blocked_by
    core.BlockedByDependency(open_blockers, _) ->
      case open_blockers {
        [] -> issue.blocked_by
        _ -> open_blockers
      }
  }
  blockers
  |> list.map(blocker_to_summary)
  |> string.join(with: ",")
}

fn blocker_to_summary(blocker: tracker_issue.BlockerRef) -> String {
  let name = case blocker.identifier {
    Some(identifier) -> identifier
    None ->
      case blocker.id {
        Some(id) -> id
        None -> "unknown"
      }
  }
  let state = case blocker.state {
    Some(state) -> issue_state.to_string(state)
    None -> "unknown"
  }
  name <> ":" <> state
}

fn bool_field(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn handle_invalid_workflow_candidate(
  state: State,
  issue: tracker_issue.Issue,
  violation: workflow_policy.IssueWorkflowViolation,
  remaining_candidates: List(tracker_issue.Issue),
) -> State {
  case
    core.already_attempted_invalid_workflow(
      state.runtime,
      issue,
      violation,
      state.workflow.effective.linear_contract,
    )
  {
    True -> dispatch_candidates(remaining_candidates, state)
    False -> {
      let fingerprint = workflow_policy.violation_fingerprint(violation)
      let reporting_policy_fingerprint =
        workflow_policy.reporting_policy_fingerprint(
          state.workflow.effective.linear_contract,
        )
      let runtime =
        core.mark_invalid_workflow_report_pending(
          state.runtime,
          issue,
          violation,
          state.workflow.effective.linear_contract,
          state.dependencies.now_ms(),
        )
      let state = State(..state, runtime: runtime)
      log_state(state, "warn", "invalid_workflow_candidate", [
        #("issue_id", issue.id),
        #("issue_identifier", issue.identifier),
        #("violation", workflow_policy.violation_code(violation)),
        #("violation_fingerprint", fingerprint),
      ])
      let state =
        enqueue_side_effect(
          state,
          effect_runner.ReportInvalidWorkflow(
            issue: issue,
            violation: violation,
            violation_fingerprint: fingerprint,
            reporting_policy_fingerprint: reporting_policy_fingerprint,
            client: state.triage_client,
          ),
        )
      dispatch_candidates(remaining_candidates, state)
    }
  }
}

fn schedule_next_poll(state: State) -> State {
  let poll =
    poll_scheduler.schedule_next(
      state.poll,
      fn(generation) {
        state.dependencies.send_after(
          state.subject,
          state.workflow.effective.polling.interval_ms,
          PollTick(generation),
        )
      },
      state.dependencies.cancel_timer,
    )
  State(..state, poll: poll)
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
  state.scheduled_retries
  |> dict.values
  |> list.any(fn(entry) { entry.job_id == job_id })
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
      let _ =
        append_ledger_bodies(
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
      let _ =
        append_ledger_bodies(
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
      let _ =
        append_ledger_bodies(
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
      case scheduled_slot_available_for_start(state, pending.job_id) {
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
      let _ =
        append_ledger_bodies(
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

fn scheduled_slot_available_for_start(state: State, job_id: String) -> Bool {
  scheduled_dispatch_slots_used_excluding(state, job_id)
  < state.workflow.effective.agent.max_concurrent_agents
}

fn scheduled_dispatch_slots_used_excluding(
  state: State,
  job_id: String,
) -> Int {
  active_run_count(state)
  + dict.size(state.pending_claims)
  + dict.size(state.pending_dispatch_validations)
  + list.length(worker_registry.scheduled_worker_handles(state.registry))
  + pending_scheduled_count_excluding(state.pending_scheduled_starts, job_id)
}

fn pending_scheduled_count_excluding(
  pending: Dict(String, ScheduledPendingStart),
  job_id: String,
) -> Int {
  pending
  |> dict.keys
  |> list.filter(fn(id) { id != job_id })
  |> list.length
}

fn handle_retry_tick(state: State, issue_id: String, generation: Int) -> State {
  case dict.get(state.runtime.retry_attempts, issue_id) {
    Error(_) -> {
      log_state(state, "info", "retry_timer_stale", [#("issue_id", issue_id)])
      state
    }
    Ok(entry) ->
      case entry.timer_generation != generation {
        True -> {
          log_state(state, "info", "retry_timer_stale", [
            #("issue_id", issue_id),
            #("generation", int.to_string(generation)),
          ])
          state
        }
        False -> {
          let state =
            State(
              ..state,
              retry: retry_scheduler.remove_timer(state.retry, issue_id),
            )
          case
            state.operator_paused,
            config.can_dispatch(state.workflow.reload_state),
            slots_remain(state)
          {
            True, _, _ ->
              defer_retry_until_dispatch_available(state, issue_id, generation)
            _, False, _ ->
              defer_retry_until_dispatch_available(state, issue_id, generation)
            False, True, False ->
              defer_retry_until_dispatch_available(state, issue_id, generation)
            False, True, True ->
              begin_retry_refresh(state, issue_id, generation)
          }
        }
      }
  }
}

fn defer_retry_until_dispatch_available(
  state: State,
  issue_id: String,
  generation: Int,
) -> State {
  log_state(state, "warn", "retry_deferred_dispatch_unavailable", [
    #("issue_id", issue_id),
  ])
  let timer =
    state.dependencies.send_after(
      state.subject,
      1000,
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

fn begin_retry_refresh(
  state: State,
  issue_id: String,
  generation: Int,
) -> State {
  case retry_scheduler.begin_refresh(state.retry, issue_id, generation) {
    Error(_) -> {
      log_state(state, "info", "retry_timer_stale", [#("issue_id", issue_id)])
      state
    }
    Ok(retry) -> {
      let state = State(..state, retry: retry)
      enqueue_side_effect(
        state,
        effect_runner.RefreshRetry(
          issue_id: issue_id,
          generation: generation,
          client: state.tracker_client,
        ),
      )
    }
  }
}

fn handle_retry_refresh_finished(
  state: State,
  issue_id: String,
  generation: Int,
  result: Result(List(tracker_issue.Issue), error.TrackerError),
) -> State {
  let state =
    State(..state, retry: retry_scheduler.finish_refresh(state.retry, issue_id))
  case dict.get(state.runtime.retry_attempts, issue_id) {
    Error(_) -> {
      log_state(state, "info", "retry_timer_stale", [#("issue_id", issue_id)])
      state
    }
    Ok(entry) ->
      case entry.timer_generation != generation {
        True -> {
          log_state(state, "info", "retry_timer_stale", [
            #("issue_id", issue_id),
            #("generation", int.to_string(generation)),
          ])
          state
        }
        False ->
          case config.can_dispatch(state.workflow.reload_state) {
            False ->
              defer_retry_until_dispatch_available(state, issue_id, generation)
            True -> {
              let candidate = case result {
                Error(err) -> Error(error.tracker_code(err))
                Ok([issue]) -> Ok(Some(issue))
                Ok(_) -> Ok(None)
              }
              handle_retry_candidate_after_refresh(state, issue_id, candidate)
            }
          }
      }
  }
}

fn handle_retry_candidate_after_refresh(
  state: State,
  issue_id: String,
  candidate: Result(Option(tracker_issue.Issue), String),
) -> State {
  let state = case candidate {
    Ok(Some(issue)) -> unpark_if_issue_changed_state(state, issue)
    _ -> state
  }
  case candidate {
    Ok(Some(issue)) ->
      case core.blocker_decision(state.workflow.effective, issue) {
        core.BlockedByDependency(_, _) -> {
          let decision = core.blocker_decision(state.workflow.effective, issue)
          let state =
            report_blocked_dependency(
              state,
              issue,
              "retry",
              "linear_dependency_retry_blocked",
              decision,
            )
          let transition =
            core.stop_retry_for_dependency_blocked(state.runtime, issue_id)
          let state =
            State(
              ..state,
              runtime: transition.state,
              recovery_by_issue: dict.delete(state.recovery_by_issue, issue_id),
            )
          apply_effects(state, transition.effects)
        }
        core.BlockersSatisfied -> {
          let runtime =
            core.clear_blocked_dependency_report(
              state.runtime,
              issue.id,
              "retry",
            )
          let state = State(..state, runtime: runtime)
          case
            core.retry_candidate_preconditions_satisfied_without_slot_capacity(
              state.runtime,
              state.workflow.effective,
              issue_id,
              issue,
            )
          {
            False ->
              handle_retry_candidate_with_slots(state, issue_id, candidate)
            True ->
              case
                workflow_policy.classify_issue(
                  state.workflow.effective.linear_contract,
                  issue,
                )
              {
                workflow_policy.WorkflowInvalid(violation) -> {
                  let transition =
                    core.stop_retry_for_policy_invalid(state.runtime, issue_id)
                  let state = State(..state, runtime: transition.state)
                  let state = apply_effects(state, transition.effects)
                  handle_invalid_workflow_candidate(state, issue, violation, [])
                }
                workflow_policy.WorkflowPolicyDisabled
                | workflow_policy.WorkflowSelected(_, _) ->
                  handle_retry_candidate_with_slots(state, issue_id, candidate)
              }
          }
        }
      }
    _ -> handle_retry_candidate_with_slots(state, issue_id, candidate)
  }
}

fn handle_retry_candidate_with_slots(
  state: State,
  issue_id: String,
  candidate: Result(Option(tracker_issue.Issue), String),
) -> State {
  case retry_candidate_needs_slot_retry(state, issue_id, candidate) {
    True -> {
      let transition =
        core.schedule_retry_with_backoff(
          state.runtime,
          state.workflow.effective,
          issue_id,
          orchestrator_reason.RetryNoSlots,
        )
      let state = State(..state, runtime: transition.state)
      apply_effects(state, transition.effects)
    }
    False -> {
      let transition =
        core.handle_retry_candidate(
          state.runtime,
          state.workflow.effective,
          issue_id,
          candidate,
        )
      let state = State(..state, runtime: transition.state)
      apply_effects(state, transition.effects)
    }
  }
}

fn retry_candidate_needs_slot_retry(
  state: State,
  issue_id: String,
  candidate: Result(Option(tracker_issue.Issue), String),
) -> Bool {
  case candidate {
    Ok(Some(issue)) ->
      core.retry_candidate_preconditions_satisfied_without_slot_capacity(
        state.runtime,
        state.workflow.effective,
        issue_id,
        issue,
      )
      && core.workflow_policy_satisfied(state.workflow.effective, issue)
      && !can_reserve_dispatch_slot(state, issue)
    _ -> False
  }
}

fn slots_remain(state: State) -> Bool {
  state.workflow.effective.agent.max_concurrent_agents != 0
  && dispatch_slots_used(state)
  < state.workflow.effective.agent.max_concurrent_agents
}

fn can_reserve_dispatch_slot(state: State, issue: tracker_issue.Issue) -> Bool {
  !has_active_run(state, issue.id)
  && !dict.has_key(state.pending_claims, issue.id)
  && !dict.has_key(state.pending_dispatch_validations, issue.id)
  && slots_remain(state)
  && per_state_dispatch_slot_available(state, issue.state)
}

fn dispatch_slots_used(state: State) -> Int {
  active_run_count(state)
  + dict.size(state.pending_claims)
  + dict.size(state.pending_dispatch_validations)
  + list.length(worker_registry.scheduled_worker_handles(state.registry))
  + dict.size(state.pending_scheduled_starts)
}

fn per_state_dispatch_slot_available(
  state: State,
  issue_state_value: issue_state.IssueState,
) -> Bool {
  let key = issue_state.key(issue_state_value)
  case
    dict.get(state.workflow.effective.agent.max_concurrent_agents_by_state, key)
  {
    Error(_) -> True
    Ok(limit) -> dispatch_count_for_state(state, key) < limit
  }
}

fn dispatch_count_for_state(
  state: State,
  normalized_state: issue_state.IssueStateKey,
) -> Int {
  running_count_for_state(state, normalized_state)
  + pending_claim_count_for_state(state, normalized_state)
}

fn running_count_for_state(
  state: State,
  normalized_state: issue_state.IssueStateKey,
) -> Int {
  state
  |> active_run_issues
  |> list.filter(fn(issue) { issue_state.key(issue.state) == normalized_state })
  |> list.length
}

fn pending_claim_count_for_state(
  state: State,
  normalized_state: issue_state.IssueStateKey,
) -> Int {
  state.pending_claims
  |> dict.to_list
  |> list.filter(fn(entry) {
    let #(_, pending) = entry
    issue_state.key(pending.issue.state) == normalized_state
  })
  |> list.length
}

fn dispatch_issue(state: State, issue: tracker_issue.Issue) -> State {
  dispatch_issue_with_continuation(state, issue, [])
}

fn begin_dispatch_validation(
  state: State,
  issue: tracker_issue.Issue,
  remaining_candidates: List(tracker_issue.Issue),
) -> State {
  case issue_is_running_claimed_or_pending(state, issue.id) {
    True -> dispatch_candidates(remaining_candidates, state)
    False -> {
      let generation = state.next_dispatch_validation_generation
      let pending =
        PendingDispatchValidation(
          issue: issue,
          remaining_candidates: remaining_candidates,
          generation: generation,
          requested_at_ms: state.dependencies.now_ms(),
        )
      let state =
        State(
          ..state,
          pending_dispatch_validations: dict.insert(
            state.pending_dispatch_validations,
            issue.id,
            pending,
          ),
          next_dispatch_validation_generation: generation + 1,
        )
      log_state(state, "info", "linear_dependency_claim_validation_started", [
        #("issue_id", issue.id),
        #("issue_identifier", issue.identifier),
        #("generation", int.to_string(generation)),
      ])
      enqueue_side_effect(
        state,
        effect_runner.ValidateDispatchClaim(
          issue_id: issue.id,
          generation: generation,
          client: state.tracker_client,
        ),
      )
    }
  }
}

fn dispatch_issue_with_continuation(
  state: State,
  issue: tracker_issue.Issue,
  remaining_candidates: List(tracker_issue.Issue),
) -> State {
  case can_reserve_dispatch_slot(state, issue) {
    False -> retry_dispatch_later_if_needed(state, issue)
    True ->
      case can_route_issue_for_dispatch(state, issue) {
        False -> dispatch_candidates(remaining_candidates, state)
        True ->
          case
            workspace.workspace_path(
              state.workflow.effective.workspace.root,
              issue.identifier,
            )
          {
            Error(err) -> {
              log_state(state, "warn", "dispatch_workspace_path_failed", [
                #("issue_id", issue.id),
                #("error", error.workspace_code(err)),
              ])
              dispatch_candidates(remaining_candidates, state)
            }
            Ok(#(_, workspace_path)) -> {
              let #(registry, session_sequence) =
                worker_registry.reserve_session_sequence(state.registry)
              let state = State(..state, registry: registry)
              let run_id =
                make_run_id(
                  issue,
                  state.dependencies.now_ms(),
                  session_sequence,
                )
              let pending =
                PendingClaim(
                  issue: issue,
                  workspace_path: workspace_path,
                  run_id: run_id,
                  session_sequence: session_sequence,
                  recovery: recovery_for_issue(state, issue.id),
                  remaining_candidates: remaining_candidates,
                )
              let state =
                State(
                  ..state,
                  pending_claims: dict.insert(
                    state.pending_claims,
                    issue.id,
                    pending,
                  ),
                )
              enqueue_side_effect(
                state,
                effect_runner.ClaimIssue(
                  issue: issue,
                  workspace_path: workspace_path,
                  run_id: run_id,
                  client: state.handoff_client,
                ),
              )
            }
          }
      }
  }
}

fn workflow_run_started_body_for_claim(
  state: State,
  pending: PendingClaim,
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
      Ok(record.WorkflowRunStarted(
        pending.run_id,
        dag.id,
        fingerprint,
        pending.issue.id,
        pending.issue.identifier,
        core.issue_fingerprint(pending.issue),
        observed_updated_at_ms(pending.issue),
        run_root,
      ))
    }
  }
}

fn can_route_issue_for_dispatch(
  state: State,
  issue: tracker_issue.Issue,
) -> Bool {
  case workflow_reloader.select_workflow(state.workflow, issue) {
    Ok(_) -> True
    Error(runtime_bundle.BundleError(code, message)) -> {
      log_state(state, "warn", "workflow_route_failed", [
        #("issue_id", issue.id),
        #("error", code),
        #("message", message),
      ])
      False
    }
  }
}

fn retry_dispatch_later_if_needed(
  state: State,
  issue: tracker_issue.Issue,
) -> State {
  case dict.has_key(state.runtime.retry_attempts, issue.id) {
    False -> state
    True -> {
      let transition =
        core.schedule_retry_with_backoff(
          state.runtime,
          state.workflow.effective,
          issue.id,
          orchestrator_reason.RetryNoSlots,
        )
      let state = State(..state, runtime: transition.state)
      apply_effects(state, transition.effects)
    }
  }
}

fn recovery_for_issue(
  state: State,
  issue_id: String,
) -> Option(session_event.RecoveryInfo) {
  case dict.get(state.recovery_by_issue, issue_id) {
    Ok(recovery) -> Some(recovery)
    Error(_) -> None
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
  session_sequence: Int,
  recovery: Option(session_event.RecoveryInfo),
) -> State {
  let session_id = make_session_id(issue.identifier, run_id, session_sequence)
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
  case dict.get(state.workflow.bundle.workflows, pending.workflow_id) {
    Error(_) -> {
      let _ =
        append_ledger_bodies(
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
    Ok(dag) ->
      case
        workspace_run.scheduled_run_root_for(
          pending.job_id,
          pending.workflow_id,
          pending.run_id,
          state.workflow.bundle.orchestrator,
        )
      {
        Error(err) -> {
          let _ =
            append_ledger_bodies(
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
  let _ =
    append_ledger_bodies(
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
  let session_id =
    yaml_step_session.id(run_id, context.step_id, context.attempt_index)
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
    Error(_) -> Nil
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
    Error(_) -> {
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
  hub.finish_session(state.event_hub, handle.session_id, session_reason.Normal)
  let _ =
    append_ledger_bodies(
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
  let _ =
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
          option.or(failure.run_root, Some(handle.run_root)),
        ),
      ],
      "scheduled_failure_append_failed",
    )
  case retry_exhausted {
    True -> state
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
    Error(_) -> state
    Ok(entry) ->
      case entry.generation != generation {
        True -> state
        False ->
          case
            state.operator_paused
            || !scheduled_slot_available_for_start(state, entry.job_id)
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

fn handle_worker_finished(
  state: State,
  issue_id: String,
  run_id: String,
  result: Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
) -> State {
  let state = evaluate_scheduled_jobs(state)
  let state = case worker_registry.worker_for_issue(state.registry, issue_id) {
    Error(_) -> {
      log_state(state, "warn", "worker_finished_stale", [
        #("issue_id", issue_id),
      ])
      State(
        ..state,
        registry: worker_registry.forget_issue_session(state.registry, issue_id),
      )
    }
    Ok(handle) ->
      case handle.run_id == run_id {
        False -> {
          log_state(state, "warn", "worker_finished_stale", [
            #("issue_id", issue_id),
            #("run_id", run_id),
          ])
          state
        }
        True -> {
          process.demonitor_process(handle.monitor)
          let state =
            State(
              ..state,
              registry: worker_registry.remove_worker_handle(
                state.registry,
                handle,
              ),
            )
          case result {
            Ok(success) -> finish_worker_success(state, handle, success)
            Error(failure) -> finish_worker_failure(state, handle, failure)
          }
        }
      }
  }
  start_pending_scheduled_runs(state)
}

fn finish_worker_success(
  state: State,
  handle: worker_registry.WorkerHandle,
  success: agent_types.WorkerSuccess,
) -> State {
  log_state(state, "info", "worker_exited", [
    #("issue_id", handle.issue_id),
    #("run_id", handle.run_id),
    #("reason", "normal"),
  ])
  hub.update_tokens(state.event_hub, handle.session_id, success.tokens)
  event_publisher.lifecycle(
    state.event_hub,
    handle.session_id,
    session_event.WorkerExited,
    Some("normal"),
  )
  hub.finish_session(state.event_hub, handle.session_id, session_reason.Normal)
  let final_issue = case success.final_issue {
    Some(issue) -> issue
    None -> handle.issue
  }
  let transition =
    core.apply_workflow_success(
      state.runtime,
      state.workflow.effective,
      handle.issue_id,
      final_issue,
      success.tokens,
      state.dependencies.now_ms(),
      core.AlreadyCleaned,
    )
  let state = State(..state, runtime: transition.state)
  case
    append_ledger_bodies(
      state,
      [
        record.RunFinished(
          handle.run_id,
          handle.issue_id,
          classification_to_string(success.final_classification),
          success.tokens.total,
          success.turns,
        ),
        counter_record_for_state(
          state,
          handle.issue_id,
          final_issue.identifier,
          Some(handle.run_id),
        ),
      ],
      "ledger_append_failed",
    )
  {
    False -> state
    True -> {
      let state =
        enqueue_side_effect(
          state,
          effect_runner.ReportSuccess(
            issue_id: handle.issue_id,
            issue: final_issue,
            success: success,
            run_id: handle.run_id,
            client: state.handoff_client,
          ),
        )
      apply_effects_with_run(state, transition.effects, Some(handle.run_id))
    }
  }
}

fn worker_failure_message(
  failure: agent_types.WorkerFailure,
  secrets: List(String),
) -> String {
  let code = error.agent_code(failure.reason)
  case failure.reason {
    error.PiFailed(error.PiProtocolError(reason)) ->
      code <> ":pi_protocol_error:" <> log.redact("failure", reason, secrets)
    error.PiFailed(pi_error) -> code <> ":" <> error.pi_rpc_code(pi_error)
    error.WorkflowCommandFailed(step_id: step_id, detail: detail, ..) ->
      code
      <> ":workflow_command_failed:"
      <> step_id
      <> ":"
      <> log.redact("failure", detail, secrets)
    error.ProbeFailed(pi_error) -> code <> ":" <> error.pi_rpc_code(pi_error)
    error.PromptFailed(template_error) ->
      code <> ":" <> error.template_code(template_error)
    error.WorkspaceFailed(workspace_error) ->
      code <> ":" <> error.workspace_code(workspace_error)
    error.HookFailedError(hook_error) ->
      code <> ":" <> hook_failure_message(hook_error, secrets)
    error.WorkflowHookFailed(hook_error) ->
      code <> ":" <> hook_failure_message(hook_error, secrets)
    error.StateRefreshFailed(tracker_error) ->
      code <> ":" <> error.tracker_code(tracker_error)
    error.OperatorAbort | error.OperatorStopAfterCurrentTurn -> code
  }
}

fn hook_failure_message(
  hook_error: error.HookError,
  secrets: List(String),
) -> String {
  let detail = case hook_error {
    error.HookFailed(command, status, output) ->
      error.hook_code(hook_error)
      <> ":"
      <> command
      <> " exited "
      <> int.to_string(status)
      <> ": "
      <> output
    error.HookTimedOut(command) ->
      error.hook_code(hook_error) <> ":" <> command <> " timed out"
    error.HookIo(message) -> error.hook_code(hook_error) <> ":" <> message
  }
  log.redact("failure", detail, secrets)
  |> log.truncate(4000)
}

fn finish_worker_failure(
  state: State,
  handle: worker_registry.WorkerHandle,
  failure: agent_types.WorkerFailure,
) -> State {
  case is_recovery_resume_validation_worker_failure(failure) {
    True -> finish_recovery_resume_validation_failure(state, handle, failure)
    False ->
      case failure.reason {
        error.OperatorAbort ->
          finish_operator_worker_exit(
            state,
            handle,
            failure,
            session_reason.OperatorAbort,
          )
        error.OperatorStopAfterCurrentTurn ->
          finish_operator_worker_exit(
            state,
            handle,
            failure,
            session_reason.OperatorStopAfterCurrentTurn,
          )
        _ -> finish_standard_worker_failure(state, handle, failure)
      }
  }
}

fn is_recovery_resume_validation_worker_failure(
  failure: agent_types.WorkerFailure,
) -> Bool {
  case failure.reason {
    error.PiFailed(error.PiProtocolError(reason)) ->
      reason == workflow_attempt.recovery_pi_resume_validation_failed
    _ -> False
  }
}

fn finish_standard_worker_failure(
  state: State,
  handle: worker_registry.WorkerHandle,
  failure: agent_types.WorkerFailure,
) -> State {
  let failure_message = worker_failure_message(failure, state.workflow.secrets)
  log_state(state, "warn", "worker_exited", [
    #("issue_id", handle.issue_id),
    #("run_id", handle.run_id),
    #("reason", failure_message),
  ])
  event_publisher.lifecycle(
    state.event_hub,
    handle.session_id,
    session_event.WorkerExited,
    Some(failure_message),
  )
  hub.finish_session(state.event_hub, handle.session_id, session_reason.Failed)
  let baseline_issue = baseline_issue_for_failure(handle, failure)
  let transition =
    core.apply_worker_failure(
      state.runtime,
      state.workflow.effective,
      handle.issue_id,
      baseline_issue,
      state.dependencies.now_ms(),
    )
  let state = State(..state, runtime: transition.state)
  case append_worker_failure_records(state, handle, failure, baseline_issue) {
    False -> state
    True -> {
      let state =
        enqueue_side_effect(
          state,
          effect_runner.ReportFailure(
            issue_id: handle.issue_id,
            issue: handle.issue,
            failure: failure,
            run_id: handle.run_id,
            client: state.handoff_client,
          ),
        )
      apply_effects_with_run(state, transition.effects, Some(handle.run_id))
    }
  }
}

fn finish_recovery_resume_validation_failure(
  state: State,
  handle: worker_registry.WorkerHandle,
  failure: agent_types.WorkerFailure,
) -> State {
  let reason = workflow_attempt.recovery_pi_resume_validation_failed
  log_state(state, "warn", "worker_exited", [
    #("issue_id", handle.issue_id),
    #("run_id", handle.run_id),
    #("reason", reason),
  ])
  event_publisher.lifecycle(
    state.event_hub,
    handle.session_id,
    session_event.WorkerExited,
    Some(reason),
  )
  hub.finish_session(state.event_hub, handle.session_id, session_reason.Failed)
  let baseline_issue = baseline_issue_for_failure(handle, failure)
  let runtime =
    orchestrator_state.RuntimeState(
      ..state.runtime,
      running: dict.delete(state.runtime.running, handle.issue_id),
      retry_attempts: dict.delete(state.runtime.retry_attempts, handle.issue_id),
    )
  let state = State(..state, runtime: runtime)
  case append_worker_failure_records(state, handle, failure, baseline_issue) {
    False -> state
    True ->
      park_issue_state_with_run(
        state,
        baseline_issue,
        orchestrator_reason.ParkOperator(reason),
        Some(handle.run_id),
      )
  }
}

fn baseline_issue_for_failure(
  handle: worker_registry.WorkerHandle,
  failure: agent_types.WorkerFailure,
) -> tracker_issue.Issue {
  case failure.final_issue {
    Some(issue) ->
      case issue.id == handle.issue_id {
        True -> issue
        False -> handle.issue
      }
    None -> handle.issue
  }
}

fn append_worker_failure_records(
  state: State,
  handle: worker_registry.WorkerHandle,
  failure: agent_types.WorkerFailure,
  baseline_issue: tracker_issue.Issue,
) -> Bool {
  append_ledger_bodies(
    state,
    [
      record.RunFinished(
        handle.run_id,
        handle.issue_id,
        "failure",
        failure.tokens.total,
        0,
      ),
      counter_record_for_state(
        state,
        handle.issue_id,
        baseline_issue.identifier,
        Some(handle.run_id),
      ),
    ],
    "ledger_append_failed",
  )
}

fn finish_operator_worker_exit(
  state: State,
  handle: worker_registry.WorkerHandle,
  failure: agent_types.WorkerFailure,
  reason: session_reason.WorkerExitReason,
) -> State {
  let reason_text = session_reason.to_string(reason)
  log_state(state, "warn", "worker_exited", [
    #("issue_id", handle.issue_id),
    #("run_id", handle.run_id),
    #("reason", reason_text),
  ])
  case event_publisher.tokens_are_nonzero(failure.tokens) {
    True ->
      hub.update_tokens(state.event_hub, handle.session_id, failure.tokens)
    False -> Nil
  }
  event_publisher.lifecycle(
    state.event_hub,
    handle.session_id,
    session_event.WorkerExited,
    Some(reason_text),
  )
  hub.finish_session(state.event_hub, handle.session_id, reason)
  append_workflow_cancelled_terminal(
    state,
    handle,
    reason_text,
    failure.tokens.total,
    0,
  )
  let final_issue = case failure.final_issue {
    Some(issue) ->
      case issue.id == handle.issue_id {
        True -> issue
        False -> handle.issue
      }
    None -> handle.issue
  }
  let runtime =
    orchestrator_state.RuntimeState(
      ..state.runtime,
      completed: dict.insert(
        state.runtime.completed,
        handle.issue_id,
        final_issue,
      ),
      aggregate_pi_totals: add_tokens(
        state.runtime.aggregate_pi_totals,
        failure.tokens,
      ),
    )
  State(..state, runtime: runtime)
  |> park_issue_state_with_run(
    final_issue,
    orchestrator_reason.ParkOperator(reason_text),
    Some(handle.run_id),
  )
}

fn append_workflow_cancelled_terminal(
  state: State,
  handle: worker_registry.WorkerHandle,
  _reason: String,
  token_total: Int,
  turns: Int,
) -> Nil {
  case workflow_id_for_handle(state, handle) {
    Error(_) -> Nil
    Ok(workflow_id) -> {
      let _ =
        append_ledger_bodies(
          state,
          [
            record.WorkflowRunFinished(
              handle.run_id,
              workflow_id,
              handle.issue_id,
              "cancelled",
              token_total,
              turns,
            ),
          ],
          "workflow_terminal_append_failed",
        )
      Nil
    }
  }
}

fn append_workflow_interrupted_terminal(
  state: State,
  handle: worker_registry.WorkerHandle,
  reason: String,
) -> Nil {
  case workflow_id_for_handle(state, handle) {
    Error(_) -> Nil
    Ok(workflow_id) -> {
      let _ =
        append_ledger_bodies(
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
      Nil
    }
  }
}

fn workflow_id_for_handle(
  state: State,
  handle: worker_registry.WorkerHandle,
) -> Result(String, Nil) {
  case workflow_id_from_projection(state, handle.run_id) {
    Ok(workflow_id) -> Ok(workflow_id)
    Error(_) ->
      case runtime_bundle.select_workflow(state.workflow.bundle, handle.issue) {
        Ok(#(_, dag)) -> Ok(dag.id)
        Error(_) -> Error(Nil)
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
            Error(_) -> Error(Nil)
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

fn handle_worker_down(state: State, down: process.Down) -> State {
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
  shutdown_state_after_effect_runner_down(state)
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
    effect_runner.LinearCommandFetchFinished(
      generation,
      candidates,
      dispatch_after,
      result,
    ) ->
      handle_linear_command_fetch_finished(
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
    effect_runner.LinearCommandAckFinished(issue_id, comment_id, result) ->
      handle_linear_command_ack_finished(state, issue_id, comment_id, result)
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
    effect_runner.FetchLinearCommands(
      generation,
      _,
      candidates,
      dispatch_after,
      _,
      _,
    ) ->
      effect_runner.LinearCommandFetchFinished(
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
    effect_runner.ReportSuccess(issue_id, _, _, run_id, _) ->
      effect_runner.HandoffSuccessFinished(
        issue_id,
        run_id,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.ReportFailure(issue_id, _, _, run_id, _) ->
      effect_runner.HandoffFailureFinished(
        issue_id,
        run_id,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.ReportPark(report, _) ->
      effect_runner.HandoffParkFinished(
        report.issue_id,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.PostLinearCommandAck(issue_id, source_comment_id, _, _) ->
      effect_runner.LinearCommandAckFinished(
        issue_id,
        source_comment_id,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.ReportInvalidWorkflow(
      issue,
      _,
      violation_fingerprint,
      reporting_policy_fingerprint,
      _,
    ) ->
      effect_runner.InvalidWorkflowReportFinished(
        issue.id,
        violation_fingerprint,
        reporting_policy_fingerprint,
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
  case dict.get(state.pending_dispatch_validations, issue_id) {
    Error(_) -> {
      log_state(state, "info", "dispatch_validation_stale", [
        #("issue_id", issue_id),
        #("generation", int.to_string(generation)),
      ])
      state
    }
    Ok(pending) ->
      case pending.generation != generation {
        True -> {
          log_state(state, "info", "dispatch_validation_stale", [
            #("issue_id", issue_id),
            #("generation", int.to_string(generation)),
          ])
          state
        }
        False -> {
          let state =
            State(
              ..state,
              pending_dispatch_validations: dict.delete(
                state.pending_dispatch_validations,
                issue_id,
              ),
            )
          case result {
            Error(err) -> {
              log_state(
                state,
                "warn",
                "linear_dependency_claim_validation_failed",
                [
                  #("issue_id", issue_id),
                  #("generation", int.to_string(generation)),
                  #("reason", dispatch_validation_error_reason(err)),
                ],
              )
              dispatch_candidates(pending.remaining_candidates, state)
            }
            Ok(refreshed_issue) ->
              handle_successful_dispatch_validation(
                state,
                pending,
                refreshed_issue,
              )
          }
        }
      }
  }
}

fn handle_successful_dispatch_validation(
  state: State,
  pending: PendingDispatchValidation,
  refreshed_issue: tracker_issue.Issue,
) -> State {
  let state = unpark_if_issue_changed_state(state, refreshed_issue)
  let decision =
    core.blocker_decision(state.workflow.effective, refreshed_issue)
  case decision {
    core.BlockedByDependency(_, _) -> {
      let state =
        report_blocked_dependency(
          state,
          refreshed_issue,
          "claim_validation",
          "linear_dependency_claim_validation_blocked",
          decision,
        )
      dispatch_candidates(pending.remaining_candidates, state)
    }
    core.BlockersSatisfied -> {
      let runtime =
        core.clear_blocked_dependency_report(
          state.runtime,
          refreshed_issue.id,
          "claim_validation",
        )
      let state = State(..state, runtime: runtime)
      case dispatch_validation_precondition_failure(state, refreshed_issue) {
        Some(reason) -> {
          log_state(state, "info", "dispatch_validation_precondition_failed", [
            #("issue_id", refreshed_issue.id),
            #("generation", int.to_string(pending.generation)),
            #("reason", reason),
          ])
          dispatch_candidates(pending.remaining_candidates, state)
        }
        None ->
          case
            workflow_policy.classify_issue(
              state.workflow.effective.linear_contract,
              refreshed_issue,
            )
          {
            workflow_policy.WorkflowInvalid(violation) ->
              handle_invalid_workflow_candidate(
                state,
                refreshed_issue,
                violation,
                pending.remaining_candidates,
              )
            workflow_policy.WorkflowPolicyDisabled
            | workflow_policy.WorkflowSelected(_, _) ->
              case can_reserve_dispatch_slot(state, refreshed_issue) {
                False -> {
                  log_state(
                    state,
                    "info",
                    "dispatch_validation_slot_unavailable",
                    [
                      #("issue_id", refreshed_issue.id),
                      #("generation", int.to_string(pending.generation)),
                    ],
                  )
                  dispatch_candidates(pending.remaining_candidates, state)
                }
                True ->
                  dispatch_issue_with_continuation(
                    state,
                    refreshed_issue,
                    pending.remaining_candidates,
                  )
              }
          }
      }
    }
  }
}

fn dispatch_validation_precondition_failure(
  state: State,
  issue: tracker_issue.Issue,
) -> Option(String) {
  case
    string.trim(issue.id) == ""
    || string.trim(issue.identifier) == ""
    || string.trim(issue.title) == ""
    || string.trim(issue_state.to_string(issue.state)) == ""
  {
    True -> Some("missing_required_fields")
    False ->
      case core.is_active(state.workflow.effective, issue.state) {
        False -> Some("inactive_state")
        True ->
          case core.is_terminal(state.workflow.effective, issue.state) {
            True -> Some("terminal_state")
            False ->
              case
                has_active_run(state, issue.id)
                || dict.has_key(state.runtime.running, issue.id)
              {
                True -> Some("already_running")
                False ->
                  case
                    dict.has_key(state.runtime.claimed, issue.id)
                    || dict.has_key(state.pending_claims, issue.id)
                  {
                    True -> Some("already_claimed")
                    False ->
                      case
                        core.dispatch_preconditions_satisfied_without_slot_capacity(
                          state.runtime,
                          state.workflow.effective,
                          issue,
                        )
                      {
                        True -> None
                        False -> Some("parked")
                      }
                  }
              }
          }
      }
  }
}

fn dispatch_validation_error_reason(
  err: effect_runner.DispatchClaimValidationError,
) -> String {
  case err {
    effect_runner.DispatchValidationTrackerError(tracker_error) ->
      "tracker_error:" <> error.tracker_code(tracker_error)
    effect_runner.DispatchValidationMissingIssue -> "missing_issue"
    effect_runner.DispatchValidationDuplicateIssue -> "duplicate_issue"
    effect_runner.DispatchValidationIdMismatch(_, _) -> "id_mismatch"
  }
}

fn handle_handoff_claim_finished(
  state: State,
  issue_id: String,
  run_id: String,
  result: Result(Nil, error.TrackerError),
) -> State {
  case dict.get(state.pending_claims, issue_id) {
    Error(_) -> {
      log_state(state, "warn", "handoff_claim_stale", [#("issue_id", issue_id)])
      state
    }
    Ok(pending) ->
      case pending.run_id != run_id {
        True -> {
          log_state(state, "warn", "handoff_claim_stale", [
            #("issue_id", issue_id),
            #("run_id", run_id),
          ])
          state
        }
        False -> {
          let state =
            State(
              ..state,
              pending_claims: dict.delete(state.pending_claims, issue_id),
            )
          case result {
            Error(err) -> {
              log_state(state, "warn", "handoff_claim_failed", [
                #("issue_id", issue_id),
                #("error", error.tracker_code(err)),
              ])
              dispatch_candidates(pending.remaining_candidates, state)
            }
            Ok(Nil) -> {
              let post_spawn_runtime =
                core.apply_worker_start(
                  state.runtime,
                  pending.issue,
                  pending.workspace_path,
                )
              let counter =
                counter_for_runtime(post_spawn_runtime, pending.issue.id)
              case workflow_run_started_body_for_claim(state, pending) {
                Error(reason) -> {
                  log_state(state, "warn", "workflow_checkpoint_start_failed", [
                    #("issue_id", pending.issue.id),
                    #("error", reason),
                  ])
                  dispatch_candidates(pending.remaining_candidates, state)
                }
                Ok(workflow_started_body) ->
                  case
                    append_ledger_bodies(
                      state,
                      [
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
                      ],
                      "ledger_append_failed",
                    )
                  {
                    False -> state
                    True -> {
                      let state =
                        spawn_worker(
                          state,
                          pending.issue,
                          pending.workspace_path,
                          pending.run_id,
                          pending.session_sequence,
                          pending.recovery,
                        )
                      dispatch_candidates(pending.remaining_candidates, state)
                    }
                  }
              }
            }
          }
        }
      }
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

fn handle_linear_command_ack_finished(
  state: State,
  issue_id: String,
  comment_id: String,
  result: Result(Nil, error.TrackerError),
) -> State {
  let state =
    State(
      ..state,
      in_flight_linear_command_acks: dict.delete(
        state.in_flight_linear_command_acks,
        comment_id,
      ),
    )
  case result {
    Ok(Nil) -> {
      let _ =
        append_ledger_bodies(
          state,
          [
            record.OutboxCompleted(comment_id, issue_id, "linear_command_ack"),
            record.LinearCommandAcked(comment_id, issue_id),
          ],
          "ledger_append_failed",
        )
      State(
        ..state,
        pending_linear_command_acks: dict.delete(
          state.pending_linear_command_acks,
          comment_id,
        ),
      )
    }
    Error(err) -> {
      log_state(state, "warn", "linear_command_ack_failed", [
        #("issue_id", issue_id),
        #("comment_id", comment_id),
        #("error", error.tracker_code(err)),
      ])
      state
    }
  }
}

fn handle_invalid_workflow_report_finished(
  state: State,
  issue_id: String,
  violation_fingerprint: String,
  reporting_policy_fingerprint: String,
  result: Result(linear_triage.InvalidWorkflowReportOutcome, error.TrackerError),
) -> State {
  case result {
    Ok(linear_triage.InvalidWorkflowReportNoop) -> {
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
  outcome: linear_triage.InvalidWorkflowReportOutcome,
) -> String {
  case outcome {
    linear_triage.InvalidWorkflowReportNoop -> "noop"
    linear_triage.InvalidWorkflowReportComment -> "comment"
    linear_triage.InvalidWorkflowReportState -> "state"
    linear_triage.InvalidWorkflowReportCommentAndState -> "comment_and_state"
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

fn enqueue_side_effect(state: State, effect: effect_runner.Effect) -> State {
  effect_runner.enqueue(state.effect_runner, effect)
  state
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

fn apply_effects(state: State, effects: List(core.Effect)) -> State {
  apply_effects_with_run(state, effects, None)
}

fn apply_effects_with_run(
  state: State,
  effects: List(core.Effect),
  source_run_id: Option(String),
) -> State {
  case effects {
    [] -> state
    [effect, ..rest] ->
      apply_effects_with_run(
        apply_effect(state, effect, source_run_id),
        rest,
        source_run_id,
      )
  }
}

fn apply_effect(
  state: State,
  effect: core.Effect,
  source_run_id: Option(String),
) -> State {
  case effect {
    core.Dispatch(issue) -> dispatch_issue(state, issue)
    core.ScheduleRetry(issue_id, delay_ms, generation, reason) -> {
      let reason_text = orchestrator_reason.retry_to_string(reason)
      case worker_registry.issue_session(state.registry, issue_id) {
        Ok(session_id) ->
          event_publisher.lifecycle(
            state.event_hub,
            session_id,
            session_event.RetryScheduled,
            Some(reason_text),
          )
        Error(_) -> Nil
      }
      log_state(state, "info", "retry_scheduled", [
        #("issue_id", issue_id),
        #("delay_ms", int.to_string(delay_ms)),
        #("generation", int.to_string(generation)),
        #("reason", reason_text),
      ])
      let _ =
        append_ledger_bodies(
          state,
          [
            record.RetryScheduled(
              issue_id,
              identifier_for_runtime(state.runtime, issue_id),
              delay_ms,
              generation,
              reason_text,
            ),
          ],
          "ledger_append_failed",
        )
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
    core.CancelRetry(issue_id, generation, reason) -> {
      let _ =
        append_ledger_bodies(
          state,
          [
            record.RetryCancelled(issue_id, generation, reason),
          ],
          "ledger_append_failed",
        )
      cancel_retry_timer(state, issue_id)
    }
    core.CleanupWorkspace(workspace_path) -> {
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
    core.StopWorker(issue_id, reason) -> {
      let reason_text = orchestrator_reason.stop_to_string(reason)
      case worker_registry.worker_for_issue(state.registry, issue_id) {
        Error(_) -> state
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
            #("issue_id", issue_id),
            #("reason", reason_text),
          ])
          State(
            ..state,
            registry: worker_registry.remove_worker_handle(
              state.registry,
              handle,
            ),
          )
        }
      }
    }
    core.ReleaseClaim(issue_id) -> {
      log_state(state, "info", "claim_released", [#("issue_id", issue_id)])
      state
    }
    core.ParkIssue(issue_id, reason) -> {
      let reason_text = orchestrator_reason.park_to_string(reason)
      log_state(state, "warn", "issue_parked", [
        #("issue_id", issue_id),
        #("reason", reason_text),
      ])
      case append_parked_record_for_runtime(state, issue_id, reason_text) {
        False -> state
        True -> enqueue_park_report_for_runtime(state, issue_id, source_run_id)
      }
    }
  }
}

fn counter_for_runtime(
  runtime: orchestrator_state.RuntimeState,
  issue_id: String,
) -> orchestrator_state.IssueCounter {
  case dict.get(runtime.issue_counters, issue_id) {
    Ok(counter) -> counter
    Error(_) -> orchestrator_state.new_issue_counter()
  }
}

fn identifier_for_runtime(
  runtime: orchestrator_state.RuntimeState,
  issue_id: String,
) -> String {
  case dict.get(runtime.claimed, issue_id) {
    Ok(identifier) -> identifier
    Error(_) ->
      case dict.get(runtime.completed, issue_id) {
        Ok(issue) -> issue.identifier
        Error(_) ->
          case dict.get(runtime.parked, issue_id) {
            Ok(parked) -> parked.identifier
            Error(_) -> issue_id
          }
      }
  }
}

fn enqueue_park_report_for_runtime(
  state: State,
  issue_id: String,
  source_run_id: Option(String),
) -> State {
  case dict.get(state.runtime.parked, issue_id) {
    Error(_) -> state
    Ok(parked) ->
      enqueue_side_effect(
        state,
        effect_runner.ReportPark(
          handoff.ParkReport(
            issue_id: parked.issue_id,
            issue_identifier: parked.identifier,
            reason: orchestrator_reason.park_to_string(parked.reason),
            release_policy: Some(park_release_policy_to_string(
              parked.release_policy,
            )),
            run_id: source_run_id,
          ),
          state.handoff_client,
        ),
      )
  }
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

fn append_parked_record_for_runtime(
  state: State,
  issue_id: String,
  reason_text: String,
) -> Bool {
  case dict.get(state.runtime.parked, issue_id) {
    Error(_) -> False
    Ok(parked) -> {
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
            issue_id,
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
  }
}

fn counter_record_for_state(
  state: State,
  issue_id: String,
  issue_identifier: String,
  source_run_id: Option(String),
) -> record.RecordBody {
  let counter = counter_for_runtime(state.runtime, issue_id)
  record.IssueCounterUpdated(
    issue_id,
    issue_identifier,
    counter.failure_attempts,
    counter.worker_sessions,
    state.dependencies.now_ms(),
    source_run_id,
  )
}

fn classification_to_string(
  classification: agent_types.FinalClassification,
) -> String {
  case classification {
    agent_types.FinalActive -> "active"
    agent_types.FinalTerminal -> "terminal"
    agent_types.FinalNonActive -> "non_active"
  }
}

fn add_tokens(
  a: session_tokens.TokenTotals,
  b: session_tokens.TokenTotals,
) -> session_tokens.TokenTotals {
  session_tokens.TokenTotals(
    input: a.input + b.input,
    output: a.output + b.output,
    cache_read: a.cache_read + b.cache_read,
    cache_write: a.cache_write + b.cache_write,
    total: a.total + b.total,
  )
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

fn shutdown_state(state: State) -> State {
  shutdown_state_internal(state, True)
}

fn shutdown_state_after_effect_runner_down(state: State) -> State {
  shutdown_state_internal(state, False)
}

fn append_shutdown_step_attempt_interruptions(state: State) -> Nil {
  case ledger.path_for_workspace_root(state.workflow.effective.workspace.root) {
    Error(_) -> Nil
    Ok(ledger_path) ->
      case ledger.load_projection(ledger_path) {
        Error(_) -> Nil
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
            _ -> {
              let _ =
                append_ledger_bodies(
                  state,
                  bodies,
                  "workflow_shutdown_interrupt_append_failed",
                )
              Nil
            }
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

fn shutdown_state_internal(state: State, stop_effect_runner: Bool) -> State {
  process.demonitor_process(state.effect_runner_monitor)
  case stop_effect_runner {
    True -> {
      let _ = effect_runner.shutdown(state.effect_runner, 1000)
      Nil
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
    pending_claims: dict.new(),
    pending_dispatch_validations: dict.new(),
    pending_scheduled_starts: dict.new(),
    scheduled_retries: dict.new(),
    control_server: NoControlServer,
    control_file_path: None,
  )
}

fn make_run_id(
  issue: tracker_issue.Issue,
  now_ms: Int,
  sequence: Int,
) -> String {
  issue.identifier
  <> "-"
  <> int.to_string(now_ms)
  <> "-"
  <> int.to_string(sequence)
}

fn make_session_id(
  _issue_identifier: String,
  run_id: String,
  _sequence: Int,
) -> String {
  run_id
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
  let _ =
    state.dependencies.logger(level, event, fields, state.workflow.secrets)
  Nil
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

@external(erlang, "scherzo_time_ffi", "monotonic_ms")
fn monotonic_ms() -> Int
