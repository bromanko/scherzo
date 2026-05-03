import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/string
import scherzo/agent/pi_event
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/config
import scherzo/control/command
import scherzo/control/file as control_file
import scherzo/control/linear_transport
import scherzo/control/server as control_server
import scherzo/domain
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
import scherzo/orchestrator/worker_registry
import scherzo/orchestrator/workflow_reloader
import scherzo/runtime_bundle
import scherzo/session/event as session_event
import scherzo/session/hub
import scherzo/session/name as session_name
import scherzo/session/reason as session_reason
import scherzo/state/ledger
import scherzo/state/outbox
import scherzo/state/record
import scherzo/state/recovery
import scherzo/step_artifact
import scherzo/tracker
import scherzo/tracker/state as issue_state
import scherzo/workflow_policy
import scherzo/workflow_run
import scherzo/workspace

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
  WorkerUpdate(String, agent_types.PiUpdate)
  WorkerCommandReady(String, String, process.Subject(worker_command.Command))
  YamlStepStarted(String, String)
  YamlStepUpdate(String, agent_types.PiUpdate)
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
  GetSnapshot(process.Subject(domain.RuntimeState))
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
    issue: domain.Issue,
    workspace_path: String,
    run_id: String,
    session_sequence: Int,
    remaining_candidates: List(domain.Issue),
  )
}

type StartupRecovery {
  StartupRecovery(
    runtime: domain.RuntimeState,
    retry_timers: List(recovery.RecoveredRetry),
    cleanup_workspaces: List(recovery.CleanupRequest),
    outbox_to_replay: List(recovery.OutboxReplay),
    warnings: List(String),
  )
}

pub type RuntimeDependencies {
  RuntimeDependencies(
    make_tracker: fn(domain.TrackerConfig) -> tracker.Client,
    make_handoff: fn(domain.TrackerConfig, domain.HandoffConfig) ->
      handoff.Client,
    make_linear_commands: fn(domain.TrackerConfig) -> linear.CommandClient,
    make_triage: fn(domain.TrackerConfig, domain.LinearContractConfig) ->
      linear_triage.TriageClient,
    workflow_run_dependencies: workflow_run.Dependencies,
    cleanup: fn(String, String, domain.HooksConfig) ->
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
    runtime: domain.RuntimeState,
    poll: poll_scheduler.State(TimerHandle),
    retry: retry_scheduler.State(TimerHandle),
    registry: worker_registry.Registry,
    pending_claims: Dict(String, PendingClaim),
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
    logger: fn(level, event, fields, secrets) {
      let _ = level
      let _ = event
      let _ = fields
      let _ = secrets
      Ok(Nil)
    },
    now_ms: monotonic_ms,
    send_after: fn(subject, delay_ms, message) {
      RealTimer(process.send_after(subject, delay_ms, message))
    },
    cancel_timer: fn(timer) {
      case timer {
        RealTimer(timer) -> {
          let _ = process.cancel_timer(timer)
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

fn start_control_plane(
  dependencies: RuntimeDependencies,
  effective: domain.EffectiveConfig,
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
          let _ =
            dependencies.logger(
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
  let linear_command_state = linear_transport.new_state(dependencies.now_ms())
  let secrets = config.resolved_secrets(effective)
  use startup_recovery <- try_startup(load_startup_recovery(
    effective,
    tracker_client,
    dependencies,
    secrets,
  ))
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
                      runtime: runtime,
                      poll: poll,
                      retry: retry_scheduler.new(),
                      registry: worker_registry.new(),
                      pending_claims: dict.new(),
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
) -> Result(domain.RuntimeState, Nil) {
  let reply = process.new_subject()
  process.send(subject, GetSnapshot(reply))
  process.receive(reply, within: timeout_ms)
}

fn load_startup_recovery(
  effective: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  dependencies: RuntimeDependencies,
  secrets: List(String),
) -> Result(StartupRecovery, StartupError) {
  use ledger_path <- try_startup(
    ledger.path_for_workspace_root(effective.workspace.root)
    |> map_ledger_error("ledger_path_failed"),
  )
  use replayed <- try_startup(
    ledger.replay(ledger_path)
    |> map_ledger_error("ledger_replay_failed"),
  )
  let _ = list.length(replayed.records)
  let _ = case replayed.truncated_tail {
    True ->
      dependencies.logger("warn", "ledger_truncated_tail_ignored", [], secrets)
    False -> Ok(Nil)
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
  use Nil <- try_startup(
    ledger.append_many(ledger_path, recovery_plan.records_to_append, True)
    |> map_ledger_error("ledger_recovery_append_failed"),
  )
  Ok(StartupRecovery(
    runtime: recovery_plan.runtime,
    retry_timers: recovery_plan.retry_timers,
    cleanup_workspaces: recovery_plan.cleanup_workspaces,
    outbox_to_replay: recovery_plan.outbox_to_replay,
    warnings: recovery_plan.warnings,
  ))
}

fn fetch_recovery_issue_states(
  tracker_client: tracker.Client,
  issue_ids: List(String),
) -> Result(List(domain.Issue), StartupError) {
  fetch_recovery_issue_chunks(tracker_client, chunk_strings(issue_ids, 50), [])
}

fn fetch_recovery_issue_chunks(
  tracker_client: tracker.Client,
  chunks: List(List(String)),
  acc: List(domain.Issue),
) -> Result(List(domain.Issue), StartupError) {
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

fn schedule_recovered_retry_timers(
  state: State,
  retries: List(recovery.RecoveredRetry),
) -> State {
  list.fold(retries, state, fn(state, retry) {
    let recovery.RecoveredRetry(issue_id, _, delay_ms, generation, reason_text) =
      retry
    log_state(state, "info", "recovered_retry_scheduled", [
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
  })
}

fn enqueue_recovered_cleanups(
  state: State,
  cleanups: List(recovery.CleanupRequest),
) -> State {
  list.fold(cleanups, state, fn(state, cleanup) {
    let recovery.CleanupRequest(issue_id, _, workspace_path) = cleanup
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
      Error(error_code) ->
        fail_startup_recovery_outbox(
          state,
          outbox_id,
          issue_id,
          outbox_kind,
          error_code,
        )
      Ok(payload) ->
        case outbox.recovery_replay_error(outbox_kind, payload.kind) {
          Error(error_code) ->
            fail_startup_recovery_outbox(
              state,
              outbox_id,
              issue_id,
              outbox_kind,
              error_code,
            )
          Ok(Nil) -> {
            log_state(state, "info", "outbox_replay_enqueued", [
              #("outbox_id", outbox_id),
              #("issue_id", issue_id),
              #("kind", outbox_kind),
            ])
            enqueue_side_effect(
              state,
              effect_runner.PostLinearCommandAck(
                issue_id: issue_id,
                source_comment_id: outbox_id,
                body: payload.body,
                client: state.linear_command_client,
              ),
            )
          }
        }
    }
  })
}

fn fail_startup_recovery_outbox(
  state: State,
  outbox_id: String,
  issue_id: String,
  outbox_kind: String,
  error_code: String,
) -> State {
  log_state(state, "warn", "outbox_replay_failed", [
    #("outbox_id", outbox_id),
    #("issue_id", issue_id),
    #("kind", outbox_kind),
    #("error", error_code),
  ])
  let _ =
    append_ledger_bodies(
      state,
      [record.OutboxFailed(outbox_id, issue_id, outbox_kind, error_code)],
      "ledger_append_failed",
    )
  state
}

fn log_startup_recovery_warnings(
  state: State,
  warnings: List(String),
) -> State {
  list.each(warnings, fn(warning) {
    log_state(state, "warn", "startup_recovery_warning", [#("warning", warning)])
  })
  state
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
      case pi_event.is_message_update(update.event) {
        True -> Nil
        False -> {
          let message = case update.message {
            Some(message) -> log.truncate(message, 200)
            None -> ""
          }
          log_state(state, "info", "pi_event", [
            #("session_id", session_id),
            #("event_name", pi_event.to_string(update.event)),
            #("message", message),
          ])
        }
      }
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
    Error(_) ->
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
        Error(_) -> state
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
          tokens: domain.zero_token_totals(),
          final_issue: None,
        )
      finish_worker_failure(state, handle, failure)
    }
    worker_registry.WorkerDownStale(registry, _issue_id) -> {
      log_state(state, "warn", "worker_down_stale", [])
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
      set_paused: fn(state, paused) { State(..state, operator_paused: paused) },
      reload_workflow: reload_workflow_for_operator,
      retry_issue: retry_issue_for_operator,
      park_issue: park_issue_for_operator,
      unpark_issue: unpark_issue_for_operator,
      abort_session: abort_session_for_operator_sync,
      route_worker_command: route_worker_command_sync,
      log_result: log_operator_result,
    ),
    operator_command,
    timeout_ms,
  )
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
  issue: domain.Issue,
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
    domain.RuntimeState(
      ..state.runtime,
      parked: dict.delete(state.runtime.parked, issue.id),
      retry_attempts: dict.delete(state.runtime.retry_attempts, issue.id),
      issue_counters: dict.delete(state.runtime.issue_counters, issue.id),
    )
  let state = State(..state, runtime: runtime) |> cancel_retry_timer(issue.id)
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
        |> park_issue_state(
          handle.issue,
          orchestrator_reason.ParkOperator(reason_text),
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

fn active_run_issues(state: State) -> List(domain.Issue) {
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
  existing: List(domain.Issue),
  values: List(domain.Issue),
) -> List(domain.Issue) {
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
}

fn issue_for_ref(
  state: State,
  issue_ref: command.IssueRef,
) -> Result(domain.Issue, command.CommandStatus) {
  case issue_ref {
    command.IssueId(issue_id) -> issue_for_id(state, issue_id)
    command.IssueIdentifier(identifier) ->
      issue_for_identifier(state, identifier)
  }
}

fn issue_for_id(
  state: State,
  issue_id: String,
) -> Result(domain.Issue, command.CommandStatus) {
  case dict.get(state.runtime.running, issue_id) {
    Ok(entry) -> Ok(entry.issue)
    Error(_) ->
      case dict.get(state.pending_claims, issue_id) {
        Ok(pending) -> Ok(pending.issue)
        Error(_) ->
          case dict.get(state.runtime.completed, issue_id) {
            Ok(issue) -> Ok(issue)
            Error(_) -> fetch_issue_by_id(state, issue_id)
          }
      }
  }
}

fn issue_for_identifier(
  state: State,
  identifier: String,
) -> Result(domain.Issue, command.CommandStatus) {
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
) -> List(domain.Issue) {
  let running =
    state.runtime.running
    |> dict.values
    |> list.map(fn(entry) { entry.issue })
  let pending =
    state.pending_claims
    |> dict.values
    |> list.map(fn(entry) { entry.issue })
  let completed = state.runtime.completed |> dict.values
  list.append(running, list.append(pending, completed))
  |> list.filter(fn(issue) { issue.identifier == identifier })
}

fn fetch_candidates_with_identifier(
  state: State,
  identifier: String,
) -> Result(domain.Issue, command.CommandStatus) {
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
) -> Result(domain.Issue, command.CommandStatus) {
  case state.tracker_client.fetch_issue_states_by_ids([issue_id]) {
    Ok([issue]) -> Ok(issue)
    Ok([]) -> Error(command.NotFound)
    Ok(_) -> Error(command.Rejected("ambiguous_issue_id"))
    Error(_) -> Error(command.Rejected("issue_fetch_failed"))
  }
}

fn unique_issue(
  issues: List(domain.Issue),
) -> Result(domain.Issue, command.CommandStatus) {
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
  issue: domain.Issue,
  reason: orchestrator_reason.ParkReason,
) -> State {
  let parked =
    domain.ParkedEntry(
      issue_id: issue.id,
      identifier: issue.identifier,
      reason: reason,
      release_policy: domain.ExplicitUnparkOnly,
      parked_at_ms: state.dependencies.now_ms(),
    )
  let runtime =
    domain.RuntimeState(
      ..state.runtime,
      running: dict.delete(state.runtime.running, issue.id),
      claimed: dict.delete(state.runtime.claimed, issue.id),
      retry_attempts: dict.delete(state.runtime.retry_attempts, issue.id),
      issue_counters: dict.delete(state.runtime.issue_counters, issue.id),
      parked: dict.insert(state.runtime.parked, issue.id, parked),
    )
  let state = State(..state, runtime: runtime) |> cancel_retry_timer(issue.id)
  let reason_text = orchestrator_reason.park_to_string(reason)
  let _ = append_parked_record_for_runtime(state, issue.id, reason_text)
  log_state(state, "warn", "issue_parked", [
    #("issue_id", issue.id),
    #("reason", reason_text),
  ])
  state
}

fn unpark_issue_state(state: State, issue_id: String) -> State {
  let issue_identifier = identifier_for_runtime(state.runtime, issue_id)
  let runtime =
    domain.RuntimeState(
      ..state.runtime,
      parked: dict.delete(state.runtime.parked, issue_id),
      retry_attempts: dict.delete(state.runtime.retry_attempts, issue_id),
      issue_counters: dict.delete(state.runtime.issue_counters, issue_id),
    )
  let state = State(..state, runtime: runtime) |> cancel_retry_timer(issue_id)
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

fn unpark_if_issue_changed_state(state: State, issue: domain.Issue) -> State {
  let had_retry = dict.has_key(state.runtime.retry_attempts, issue.id)
  let was_parked = dict.has_key(state.runtime.parked, issue.id)
  let runtime = core.unpark_if_issue_changed(state.runtime, issue)
  let state = State(..state, runtime: runtime)
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
    True -> cancel_retry_timer(state, issue.id)
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
    Error(_) -> state
    Ok(poll) -> {
      let state = State(..state, poll: poll)
      log_state(state, "info", "tick_started", [
        #("generation", int.to_string(generation)),
      ])
      let state = reload_if_changed(state)
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
    domain.RuntimeState(
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
  log_state(state, "info", "workflow_reloaded", [])
  state
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
  result: Result(List(domain.Issue), error.TrackerError),
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
  result: Result(List(domain.Issue), error.TrackerError),
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
  candidates: List(domain.Issue),
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
  candidates: List(domain.Issue),
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
  candidates: List(domain.Issue),
  dispatch_after: Bool,
) -> State {
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
        Error(_) -> result
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
    | command.UnparkIssue(_) -> False
  }
}

fn apply_linear_transport_action(
  state: State,
  action: linear_transport.TransportAction,
) -> State {
  case action {
    linear_transport.SubmitCommand(comment, parsed) -> {
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
          let _ =
            append_ledger_bodies(
              state,
              [
                record.OutboxPendingV2(
                  comment.id,
                  comment.issue_id,
                  "linear_command_ack",
                  "linear_command_ack:" <> comment.id,
                  outbox.linear_command_ack_payload(
                    comment.id,
                    body,
                    state.workflow.secrets,
                  ),
                ),
              ],
              "ledger_append_failed",
            )
          enqueue_side_effect(
            state,
            effect_runner.PostLinearCommandAck(
              issue_id: comment.issue_id,
              source_comment_id: comment.id,
              body: body,
              client: state.linear_command_client,
            ),
          )
        }
        False -> state
      }
    }
    linear_transport.PostAck(issue_id, body) -> {
      let outbox_id = "linear-command-ack-" <> issue_id
      let _ =
        append_ledger_bodies(
          state,
          [
            record.OutboxPendingV2(
              outbox_id,
              issue_id,
              "linear_command_ack",
              "linear_command_ack:" <> issue_id,
              outbox.linear_command_ack_payload(
                outbox_id,
                body,
                state.workflow.secrets,
              ),
            ),
          ],
          "ledger_append_failed",
        )
      enqueue_side_effect(
        state,
        effect_runner.PostLinearCommandAck(
          issue_id: issue_id,
          source_comment_id: outbox_id,
          body: body,
          client: state.linear_command_client,
        ),
      )
    }
    linear_transport.LogIgnored(reason, comment_id) -> {
      log_state(state, "info", "linear_command_ignored", [
        #("comment_id", comment_id),
        #("reason", reason),
      ])
      state
    }
  }
}

fn observed_issue_ids(
  state: State,
  candidates: List(domain.Issue),
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

fn dispatch_candidates(issues: List(domain.Issue), state: State) -> State {
  case
    !state.operator_paused && config.can_dispatch(state.workflow.reload_state)
  {
    False -> state
    True ->
      case issues {
        [] -> state
        [issue, ..rest] -> {
          let state = unpark_if_issue_changed_state(state, issue)
          case
            core.dispatch_preconditions_satisfied_without_slot_capacity(
              state.runtime,
              state.workflow.effective,
              issue,
            )
            && !dict.has_key(state.pending_claims, issue.id)
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

fn handle_valid_workflow_candidate(
  state: State,
  issue: domain.Issue,
  remaining_candidates: List(domain.Issue),
) -> State {
  let runtime = core.clear_invalid_workflow_report(state.runtime, issue.id)
  let state = State(..state, runtime: runtime)
  case can_reserve_dispatch_slot(state, issue) {
    False -> dispatch_candidates(remaining_candidates, state)
    True -> dispatch_issue_with_continuation(state, issue, remaining_candidates)
  }
}

fn handle_invalid_workflow_candidate(
  state: State,
  issue: domain.Issue,
  violation: workflow_policy.IssueWorkflowViolation,
  remaining_candidates: List(domain.Issue),
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
  result: Result(List(domain.Issue), error.TrackerError),
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
  candidate: Result(Option(domain.Issue), String),
) -> State {
  let state = case candidate {
    Ok(Some(issue)) -> unpark_if_issue_changed_state(state, issue)
    _ -> state
  }
  case candidate {
    Ok(Some(issue)) ->
      case
        core.retry_candidate_preconditions_satisfied_without_slot_capacity(
          state.runtime,
          state.workflow.effective,
          issue_id,
          issue,
        )
      {
        False -> handle_retry_candidate_with_slots(state, issue_id, candidate)
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
    _ -> handle_retry_candidate_with_slots(state, issue_id, candidate)
  }
}

fn handle_retry_candidate_with_slots(
  state: State,
  issue_id: String,
  candidate: Result(Option(domain.Issue), String),
) -> State {
  case retry_candidate_needs_slot_retry(state, candidate) {
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
  candidate: Result(Option(domain.Issue), String),
) -> Bool {
  case candidate {
    Ok(Some(issue)) -> !can_reserve_dispatch_slot(state, issue)
    _ -> False
  }
}

fn slots_remain(state: State) -> Bool {
  state.workflow.effective.agent.max_concurrent_agents != 0
  && dispatch_slots_used(state)
  < state.workflow.effective.agent.max_concurrent_agents
}

fn can_reserve_dispatch_slot(state: State, issue: domain.Issue) -> Bool {
  !has_active_run(state, issue.id)
  && !dict.has_key(state.pending_claims, issue.id)
  && slots_remain(state)
  && per_state_dispatch_slot_available(state, issue.state)
}

fn dispatch_slots_used(state: State) -> Int {
  active_run_count(state) + dict.size(state.pending_claims)
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

fn dispatch_issue(state: State, issue: domain.Issue) -> State {
  dispatch_issue_with_continuation(state, issue, [])
}

fn dispatch_issue_with_continuation(
  state: State,
  issue: domain.Issue,
  remaining_candidates: List(domain.Issue),
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

fn can_route_issue_for_dispatch(state: State, issue: domain.Issue) -> Bool {
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

fn retry_dispatch_later_if_needed(state: State, issue: domain.Issue) -> State {
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

fn spawn_worker(
  state: State,
  issue: domain.Issue,
  workspace_path: String,
  run_id: String,
  session_sequence: Int,
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
      current_turn: 0,
      started_at_ms: started_at_ms,
      last_event_at_ms: started_at_ms,
      token_totals: domain.zero_token_totals(),
    ),
  )
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
  )
}

fn run_workflow_worker(
  issue: domain.Issue,
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
    Ok(#(_, dag)) ->
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
        Error(failure) ->
          Error(yaml_worker_failure(failure.reason, failure.run_root, issue))
      }
  }
}

fn yaml_workflow_dependencies(
  base: workflow_run.Dependencies,
  issue: domain.Issue,
  run_id: String,
  daemon_subject: process.Subject(Message),
  event_hub: process.Subject(hub.Message),
  now_ms: fn() -> Int,
) -> workflow_run.Dependencies {
  workflow_run.Dependencies(
    ..base,
    command_step: fn(
      step_id,
      command,
      workspace_path,
      timeout_ms,
      secrets,
      limits,
    ) {
      run_yaml_command_step(
        base,
        issue,
        run_id,
        step_id,
        command,
        workspace_path,
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
      step_id,
      prompt,
      effective,
      tracker_client,
      workspace_path,
      _emit_update,
      _command_ready,
    ) {
      run_yaml_agent_step(
        base,
        issue,
        run_id,
        step_id,
        prompt,
        effective,
        tracker_client,
        workspace_path,
        daemon_subject,
        event_hub,
        now_ms,
      )
    },
  )
}

fn register_yaml_step_session(
  event_hub: process.Subject(hub.Message),
  session_id: String,
  issue: domain.Issue,
  workspace_path: String,
  step_id: String,
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
      current_turn: 0,
      started_at_ms: started_at_ms,
      last_event_at_ms: started_at_ms,
      token_totals: domain.zero_token_totals(),
    ),
  )
  hub.update_status(event_hub, session_id, session_event.Running)
  hub.publish(
    event_hub,
    session_id,
    session_event.EventPayload(
      kind: session_event.Lifecycle,
      name: session_event.LifecycleName(session_event.StepStarted),
      turn: None,
      pi_type: None,
      message: Some(step_id),
      request_id: None,
      method: None,
      tool_name: None,
      tool_input: None,
      tool_output: None,
      tool_status: None,
      tokens: domain.zero_token_totals(),
      raw_json: None,
    ),
  )
}

fn run_yaml_command_step(
  base: workflow_run.Dependencies,
  issue: domain.Issue,
  run_id: String,
  step_id: String,
  command: String,
  workspace_path: String,
  timeout_ms: Int,
  secrets: List(String),
  limits: domain.ArtifactLimits,
  daemon_subject: process.Subject(Message),
  event_hub: process.Subject(hub.Message),
  now_ms: fn() -> Int,
) -> step_artifact.StepArtifact {
  let session_id = run_id <> "-" <> step_id
  register_yaml_step_session(
    event_hub,
    session_id,
    issue,
    workspace_path,
    step_id,
    now_ms,
  )
  process.send(daemon_subject, YamlStepStarted(session_id, run_id))
  let artifact =
    base.command_step(
      step_id,
      command,
      workspace_path,
      timeout_ms,
      secrets,
      limits,
    )
  let reason = case step_artifact.succeeded(artifact.status) {
    True -> session_reason.Normal
    False -> session_reason.Failed
  }
  hub.finish_session(event_hub, session_id, reason)
  process.send(daemon_subject, YamlStepFinished(session_id))
  artifact
}

fn run_yaml_agent_step(
  base: workflow_run.Dependencies,
  issue: domain.Issue,
  run_id: String,
  step_id: String,
  prompt: String,
  effective: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  workspace_path: String,
  daemon_subject: process.Subject(Message),
  event_hub: process.Subject(hub.Message),
  now_ms: fn() -> Int,
) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
  let session_id = run_id <> "-" <> step_id
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
      current_turn: 0,
      started_at_ms: started_at_ms,
      last_event_at_ms: started_at_ms,
      token_totals: domain.zero_token_totals(),
    ),
  )
  hub.update_status(event_hub, session_id, session_event.Running)
  hub.publish(
    event_hub,
    session_id,
    session_event.EventPayload(
      kind: session_event.Lifecycle,
      name: session_event.LifecycleName(session_event.StepStarted),
      turn: None,
      pi_type: None,
      message: Some(step_id),
      request_id: None,
      method: None,
      tool_name: None,
      tool_input: None,
      tool_output: None,
      tool_status: None,
      tokens: domain.zero_token_totals(),
      raw_json: None,
    ),
  )
  process.send(daemon_subject, YamlStepStarted(session_id, run_id))
  let result =
    base.agent_step(
      issue,
      step_id,
      prompt,
      effective,
      tracker_client,
      workspace_path,
      fn(update) {
        process.send(daemon_subject, YamlStepUpdate(session_id, update))
      },
      fn(command_subject) {
        process.send(
          daemon_subject,
          YamlStepCommandReady(session_id, command_subject),
        )
      },
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
  issue: domain.Issue,
) -> agent_types.WorkerFailure {
  agent_types.WorkerFailure(
    reason: error.PiFailed(error.PiProtocolError(reason)),
    workspace_path: workspace_path,
    tokens: domain.zero_token_totals(),
    final_issue: Some(issue),
  )
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

fn handle_worker_update(
  state: State,
  issue_id: String,
  update: agent_types.PiUpdate,
) -> State {
  case worker_registry.worker_for_issue(state.registry, issue_id) {
    Ok(handle) ->
      event_publisher.worker_update(state.event_hub, handle.session_id, update)
    Error(_) -> Nil
  }
  case pi_event.is_message_update(update.event) {
    True -> state
    False -> {
      let message = case update.message {
        Some(message) -> log.truncate(message, 200)
        None -> ""
      }
      log_state(state, "info", "pi_event", [
        #("issue_id", issue_id),
        #("event_name", pi_event.to_string(update.event)),
        #("message", message),
      ])
      state
    }
  }
}

fn handle_worker_finished(
  state: State,
  issue_id: String,
  run_id: String,
  result: Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
) -> State {
  case worker_registry.worker_for_issue(state.registry, issue_id) {
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
      apply_effects(state, transition.effects)
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
    error.ProbeFailed(pi_error) -> code <> ":" <> error.pi_rpc_code(pi_error)
    error.PromptFailed(template_error) ->
      code <> ":" <> error.template_code(template_error)
    error.WorkspaceFailed(workspace_error) ->
      code <> ":" <> error.workspace_code(workspace_error)
    error.HookFailedError(hook_error) ->
      code <> ":" <> error.hook_code(hook_error)
    error.StateRefreshFailed(tracker_error) ->
      code <> ":" <> error.tracker_code(tracker_error)
    error.OperatorAbort | error.OperatorStopAfterCurrentTurn -> code
  }
}

fn finish_worker_failure(
  state: State,
  handle: worker_registry.WorkerHandle,
  failure: agent_types.WorkerFailure,
) -> State {
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
    _ -> {
      let failure_message =
        worker_failure_message(failure, state.workflow.secrets)
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
      hub.finish_session(
        state.event_hub,
        handle.session_id,
        session_reason.Failed,
      )
      let baseline_issue = case failure.final_issue {
        Some(issue) ->
          case issue.id == handle.issue_id {
            True -> issue
            False -> handle.issue
          }
        None -> handle.issue
      }
      let transition =
        core.apply_worker_failure(
          state.runtime,
          state.workflow.effective,
          handle.issue_id,
          baseline_issue,
          state.dependencies.now_ms(),
        )
      let state = State(..state, runtime: transition.state)
      case
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
      {
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
          apply_effects(state, transition.effects)
        }
      }
    }
  }
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
  let final_issue = case failure.final_issue {
    Some(issue) ->
      case issue.id == handle.issue_id {
        True -> issue
        False -> handle.issue
      }
    None -> handle.issue
  }
  let runtime =
    domain.RuntimeState(
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
  |> park_issue_state(
    final_issue,
    orchestrator_reason.ParkOperator(reason_text),
  )
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
    effect_runner.HandoffClaimFinished(issue_id, run_id, result) ->
      handle_handoff_claim_finished(state, issue_id, run_id, result)
    effect_runner.HandoffSuccessFinished(issue_id, _run_id, result) ->
      handle_handoff_success_finished(state, issue_id, result)
    effect_runner.HandoffFailureFinished(issue_id, _run_id, result) ->
      handle_handoff_failure_finished(state, issue_id, result)
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
              case
                append_ledger_bodies(
                  state,
                  [
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

fn handle_linear_command_ack_finished(
  state: State,
  issue_id: String,
  comment_id: String,
  result: Result(Nil, error.TrackerError),
) -> State {
  case result {
    Ok(Nil) -> {
      let _ =
        append_ledger_bodies(
          state,
          [
            record.OutboxCompleted(comment_id, issue_id, "linear_command_ack"),
          ],
          "ledger_append_failed",
        )
      state
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
      log_state(state, "info", "workspace_cleaned", [
        #("workspace_path", workspace_path),
      ])
      state
    }
    Error(err) -> {
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
  case effects {
    [] -> state
    [effect, ..rest] -> apply_effects(apply_effect(state, effect), rest)
  }
}

fn apply_effect(state: State, effect: core.Effect) -> State {
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
    core.CancelRetry(issue_id) -> {
      let _ =
        append_ledger_bodies(
          state,
          [
            record.RetryCancelled(issue_id, 0, "cancel_retry"),
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
      let _ = append_parked_record_for_runtime(state, issue_id, reason_text)
      state
    }
  }
}

fn counter_for_runtime(
  runtime: domain.RuntimeState,
  issue_id: String,
) -> domain.IssueCounter {
  case dict.get(runtime.issue_counters, issue_id) {
    Ok(counter) -> counter
    Error(_) -> domain.new_issue_counter()
  }
}

fn identifier_for_runtime(
  runtime: domain.RuntimeState,
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

fn append_parked_record_for_runtime(
  state: State,
  issue_id: String,
  reason_text: String,
) -> Bool {
  case dict.get(state.runtime.parked, issue_id) {
    Error(_) -> True
    Ok(parked) -> {
      let #(release_policy, issue_fingerprint) = case parked.release_policy {
        domain.ExplicitUnparkOnly -> #("explicit_unpark_only", "")
        domain.AutoUnparkOnIssueChange(fingerprint) -> #(
          "auto_unpark_on_issue_change",
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
  a: domain.TokenTotals,
  b: domain.TokenTotals,
) -> domain.TokenTotals {
  domain.TokenTotals(
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

fn shutdown_state(state: State) -> State {
  shutdown_state_internal(state, True)
}

fn shutdown_state_after_effect_runner_down(state: State) -> State {
  shutdown_state_internal(state, False)
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
  worker_registry.worker_handles(state.registry)
  |> list.each(fn(handle) { stop_worker(handle) })
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
    control_server: NoControlServer,
    control_file_path: None,
  )
}

fn make_run_id(issue: domain.Issue, now_ms: Int, sequence: Int) -> String {
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
