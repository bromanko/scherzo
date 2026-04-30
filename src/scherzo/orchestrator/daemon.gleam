import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/string
import scherzo/agent/runner
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
import scherzo/orchestrator/core
import scherzo/runtime_bundle
import scherzo/session/event as session_event
import scherzo/session/hub
import scherzo/step_artifact
import scherzo/tracker
import scherzo/workflow_policy
import scherzo/workflow_run
import scherzo/workspace
import simplifile
import yay

pub type StartupError {
  StartupError(code: String, message: String)
}

pub type Message {
  PollTick(Int)
  RetryTick(String, Int)
  WorkerFinished(
    String,
    String,
    Result(runner.WorkerSuccess, runner.WorkerFailure),
  )
  WorkerUpdate(String, runner.PiUpdate)
  WorkerCommandReady(String, String, process.Subject(worker_command.Command))
  YamlStepUpdate(String, runner.PiUpdate)
  YamlStepCommandReady(String, process.Subject(worker_command.Command))
  YamlStepFinished(String)
  AbortWorkerCommandTimedOut(
    command.OperatorCommand,
    String,
    process.Subject(command.CommandResult),
  )
  WorkerDown(process.Down)
  SideEffectFinished(SideEffectResult)
  Shutdown(process.Subject(Nil))
  GetSnapshot(process.Subject(domain.RuntimeState))
  ApplyOperatorCommand(
    command.OperatorCommand,
    Int,
    process.Subject(command.CommandResult),
  )
}

pub type WorkerHandle {
  WorkerHandle(
    issue_id: String,
    issue: domain.Issue,
    run_id: String,
    pid: process.Pid,
    monitor: process.Monitor,
    workspace_path: String,
    session_id: String,
    command_subject: Option(process.Subject(worker_command.Command)),
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

const max_worker_command_wait_ms = 500

type PendingClaim {
  PendingClaim(
    issue: domain.Issue,
    workspace_path: String,
    run_id: String,
    session_sequence: Int,
    remaining_candidates: List(domain.Issue),
  )
}

type SideEffect {
  FetchCandidates(generation: Int, client: tracker.Client)
  FetchLinearCommands(
    generation: Int,
    issue_ids: List(String),
    candidates: List(domain.Issue),
    dispatch_after: Bool,
    client: linear.CommandClient,
    limit_per_issue: Int,
  )
  RefreshRunning(generation: Int, ids: List(String), client: tracker.Client)
  RefreshRetry(issue_id: String, generation: Int, client: tracker.Client)
  ClaimIssue(
    issue: domain.Issue,
    workspace_path: String,
    run_id: String,
    client: handoff.Client,
  )
  ReportSuccess(
    issue_id: String,
    issue: domain.Issue,
    success: runner.WorkerSuccess,
    run_id: String,
    client: handoff.Client,
  )
  ReportFailure(
    issue_id: String,
    issue: domain.Issue,
    failure: runner.WorkerFailure,
    run_id: String,
    client: handoff.Client,
  )
  PostLinearCommandAck(
    issue_id: String,
    source_comment_id: String,
    body: String,
    client: linear.CommandClient,
  )
  ReportInvalidWorkflow(
    issue: domain.Issue,
    violation: workflow_policy.IssueWorkflowViolation,
    violation_fingerprint: String,
    reporting_policy_fingerprint: String,
    client: linear_triage.TriageClient,
  )
  CleanupWorkspace(
    root: String,
    workspace_path: String,
    hooks: domain.HooksConfig,
    cleanup: fn(String, String, domain.HooksConfig) ->
      Result(Nil, error.WorkspaceError),
  )
}

pub type SideEffectResult {
  CandidateFetchFinished(Int, Result(List(domain.Issue), error.TrackerError))
  LinearCommandFetchFinished(
    Int,
    List(domain.Issue),
    Bool,
    Result(List(linear.LinearComment), error.TrackerError),
  )
  RunningRefreshFinished(Int, Result(List(domain.Issue), error.TrackerError))
  RetryRefreshFinished(
    String,
    Int,
    Result(List(domain.Issue), error.TrackerError),
  )
  HandoffClaimFinished(String, String, Result(Nil, error.TrackerError))
  HandoffSuccessFinished(String, String, Result(Nil, error.TrackerError))
  HandoffFailureFinished(String, String, Result(Nil, error.TrackerError))
  LinearCommandAckFinished(String, String, Result(Nil, error.TrackerError))
  InvalidWorkflowReportFinished(
    issue_id: String,
    violation_fingerprint: String,
    reporting_policy_fingerprint: String,
    result: Result(
      linear_triage.InvalidWorkflowReportOutcome,
      error.TrackerError,
    ),
  )
  CleanupFinished(String, Result(Nil, error.WorkspaceError))
}

pub type RuntimeDependencies {
  RuntimeDependencies(
    make_tracker: fn(domain.TrackerConfig) -> tracker.Client,
    make_handoff: fn(domain.TrackerConfig, domain.HandoffConfig) ->
      handoff.Client,
    make_linear_commands: fn(domain.TrackerConfig) -> linear.CommandClient,
    make_triage: fn(domain.TrackerConfig, domain.LinearContractConfig) ->
      linear_triage.TriageClient,
    agent_runner: fn(
      domain.Issue,
      Option(Int),
      domain.WorkflowDefinition,
      domain.EffectiveConfig,
      tracker.Client,
      fn(String, runner.PiUpdate) -> Nil,
      process.Subject(worker_command.Command),
      fn() -> Nil,
    ) ->
      Result(runner.WorkerSuccess, runner.WorkerFailure),
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
    workflow_path: Option(String),
    chosen_path: String,
    last_contents: String,
    definition: domain.WorkflowDefinition,
    bundle: runtime_bundle.RuntimeBundle,
    reload_state: config.ReloadState,
    effective: domain.EffectiveConfig,
    tracker_client: tracker.Client,
    handoff_client: handoff.Client,
    linear_command_client: linear.CommandClient,
    triage_client: linear_triage.TriageClient,
    linear_command_state: linear_transport.TransportState,
    runtime: domain.RuntimeState,
    poll_generation: Int,
    poll_in_flight: Option(Int),
    poll_timer: Option(TimerHandle),
    retry_timers: Dict(String, TimerHandle),
    retry_refreshes_in_flight: Dict(String, Int),
    workers: Dict(String, WorkerHandle),
    worker_monitors: Dict(process.Monitor, String),
    issue_sessions: Dict(String, String),
    step_command_subjects: Dict(String, process.Subject(worker_command.Command)),
    step_command_monitors: Dict(process.Monitor, String),
    step_command_subject_monitors: Dict(String, process.Monitor),
    next_session_sequence: Int,
    pending_claims: Dict(String, PendingClaim),
    side_effects_in_flight: Int,
    side_effect_queue: List(SideEffect),
    secrets: List(String),
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
    agent_runner: runner.run_attempt_with_command_ready,
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

fn workflow_definition_from_bundle(
  bundle: runtime_bundle.RuntimeBundle,
) -> domain.WorkflowDefinition {
  case bundle.legacy_workflow {
    Some(definition) -> definition
    None ->
      domain.WorkflowDefinition(config: yay.NodeMap([]), prompt_template: "")
  }
}

pub fn start(
  workflow_path: Option(String),
  dependencies: RuntimeDependencies,
) -> Result(actor.Started(process.Subject(Message)), StartupError) {
  use bundle <- try_startup(
    runtime_bundle.load(workflow_path)
    |> map_bundle_error,
  )
  let chosen_path = bundle.config_path
  let contents = bundle.config_contents
  let definition = workflow_definition_from_bundle(bundle)
  let effective = bundle.effective
  use _ <- try_startup(case bundle.mode {
    runtime_bundle.LegacyMarkdown ->
      config.validate_dispatch(effective) |> map_config_error
    runtime_bundle.OrchestratorYaml -> Ok(Nil)
  })
  let tracker_client = dependencies.make_tracker(effective.tracker)
  let handoff_client =
    dependencies.make_handoff(effective.tracker, effective.handoff)
  let linear_command_client =
    dependencies.make_linear_commands(effective.tracker)
  let triage_client =
    dependencies.make_triage(effective.tracker, effective.linear_contract)
  let linear_command_state = linear_transport.new_state(dependencies.now_ms())
  let reload_state =
    config.ReloadState(
      last_known_good: Some(effective),
      current_status: config.CurrentValid,
    )
  let runtime = core.new_state(effective)
  let secrets = config.resolved_secrets(effective)
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
        Ok(control_plane) -> {
          let poll_generation = 1
          let poll_timer =
            dependencies.send_after(subject, 0, PollTick(poll_generation))
          let state =
            State(
              subject: subject,
              workflow_path: workflow_path,
              chosen_path: chosen_path,
              last_contents: contents,
              definition: definition,
              bundle: bundle,
              reload_state: reload_state,
              effective: effective,
              tracker_client: tracker_client,
              handoff_client: handoff_client,
              linear_command_client: linear_command_client,
              triage_client: triage_client,
              linear_command_state: linear_command_state,
              runtime: runtime,
              poll_generation: poll_generation,
              poll_in_flight: None,
              poll_timer: Some(poll_timer),
              retry_timers: dict.new(),
              retry_refreshes_in_flight: dict.new(),
              workers: dict.new(),
              worker_monitors: dict.new(),
              issue_sessions: dict.new(),
              step_command_subjects: dict.new(),
              step_command_monitors: dict.new(),
              step_command_subject_monitors: dict.new(),
              next_session_sequence: 1,
              pending_claims: dict.new(),
              side_effects_in_flight: 0,
              side_effect_queue: [],
              secrets: secrets,
              event_hub: event_hub,
              control_server: control_plane.handle,
              control_file_path: control_plane.control_file_path,
              operator_paused: False,
              dependencies: dependencies,
            )
          let selector =
            process.new_selector()
            |> process.select(subject)
            |> process.select_monitors(WorkerDown)
          actor.initialised(state)
          |> actor.selecting(selector)
          |> actor.returning(subject)
          |> Ok
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

fn handle_message(state: State, message: Message) -> actor.Next(State, Message) {
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
    YamlStepUpdate(session_id, update) -> {
      publish_worker_update(state, session_id, update)
      actor.continue(state)
    }
    YamlStepCommandReady(session_id, command_subject) ->
      actor.continue(handle_yaml_step_command_ready(
        state,
        session_id,
        command_subject,
      ))
    YamlStepFinished(session_id) ->
      actor.continue(clear_yaml_step_command_route(state, session_id))
    AbortWorkerCommandTimedOut(operator_command, session_id, reply) -> {
      let #(state, result) =
        stop_session_for_operator(
          state,
          operator_command,
          session_id,
          "operator_abort",
        )
      process.send(reply, result)
      actor.continue(state)
    }
    WorkerDown(down) -> actor.continue(handle_worker_down(state, down))
    SideEffectFinished(result) ->
      actor.continue(handle_side_effect_finished(state, result))
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
  let state = clear_yaml_step_command_route(state, session_id)
  case process.subject_owner(command_subject) {
    Error(_) ->
      State(
        ..state,
        step_command_subjects: dict.insert(
          state.step_command_subjects,
          session_id,
          command_subject,
        ),
      )
    Ok(pid) -> {
      let monitor = process.monitor(pid)
      case process.is_alive(pid) {
        False -> {
          process.demonitor_process(monitor)
          state
        }
        True ->
          State(
            ..state,
            step_command_subjects: dict.insert(
              state.step_command_subjects,
              session_id,
              command_subject,
            ),
            step_command_monitors: dict.insert(
              state.step_command_monitors,
              monitor,
              session_id,
            ),
            step_command_subject_monitors: dict.insert(
              state.step_command_subject_monitors,
              session_id,
              monitor,
            ),
          )
      }
    }
  }
}

fn clear_yaml_step_command_route(state: State, session_id: String) -> State {
  case dict.get(state.step_command_subject_monitors, session_id) {
    Error(_) ->
      State(
        ..state,
        step_command_subjects: dict.delete(
          state.step_command_subjects,
          session_id,
        ),
      )
    Ok(monitor) -> {
      process.demonitor_process(monitor)
      State(
        ..state,
        step_command_subjects: dict.delete(
          state.step_command_subjects,
          session_id,
        ),
        step_command_monitors: dict.delete(state.step_command_monitors, monitor),
        step_command_subject_monitors: dict.delete(
          state.step_command_subject_monitors,
          session_id,
        ),
      )
    }
  }
}

fn handle_step_command_down(state: State, monitor: process.Monitor) -> State {
  case dict.get(state.step_command_monitors, monitor) {
    Error(_) -> {
      log_state(state, "warn", "worker_down_stale", [])
      state
    }
    Ok(session_id) -> {
      log_state(state, "warn", "yaml_step_command_down", [
        #("session_id", session_id),
      ])
      State(
        ..state,
        step_command_subjects: dict.delete(
          state.step_command_subjects,
          session_id,
        ),
        step_command_monitors: dict.delete(state.step_command_monitors, monitor),
        step_command_subject_monitors: dict.delete(
          state.step_command_subject_monitors,
          session_id,
        ),
      )
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
  case operator_command {
    command.PauseDispatch -> {
      let pending = dict.size(state.pending_claims)
      let state = State(..state, operator_paused: True)
      let result =
        command.applied(
          operator_command,
          Some("dispatch paused; pending_claims=" <> int.to_string(pending)),
        )
      log_operator_result(state, result, [
        #("pending_claims", int.to_string(pending)),
      ])
      #(state, result)
    }
    command.ResumeDispatch -> {
      let state = State(..state, operator_paused: False)
      let result = command.applied(operator_command, Some("dispatch resumed"))
      log_operator_result(state, result, [])
      #(state, result)
    }
    command.ReloadWorkflow ->
      log_operator_transition(reload_workflow_for_operator(
        state,
        operator_command,
      ))
    command.RetryIssue(issue_ref) ->
      log_operator_transition(retry_issue_for_operator(
        state,
        operator_command,
        issue_ref,
      ))
    command.ParkIssue(issue_ref, reason) ->
      log_operator_transition(park_issue_for_operator(
        state,
        operator_command,
        issue_ref,
        reason,
      ))
    command.UnparkIssue(issue_ref) ->
      log_operator_transition(unpark_issue_for_operator(
        state,
        operator_command,
        issue_ref,
      ))
    command.AbortSession(session_id) ->
      abort_session_for_operator_sync(
        state,
        operator_command,
        session_id,
        timeout_ms,
      )
    command.StopAfterCurrentTurn(session_id) ->
      route_worker_command_sync(
        state,
        operator_command,
        session_id,
        timeout_ms,
        fn(subject, reply) {
          process.send(subject, worker_command.StopAfterCurrentTurn(reply))
        },
      )
    command.PromptSession(session_id, message) ->
      case operator_prompt_too_large(message) {
        True -> #(
          state,
          command.rejected(
            operator_command,
            "prompt_too_large",
            Some("operator prompt is too large"),
          ),
        )
        False ->
          route_worker_command_sync(
            state,
            operator_command,
            session_id,
            timeout_ms,
            fn(subject, reply) {
              process.send(subject, worker_command.QueuePrompt(message, reply))
            },
          )
      }
    command.RespondUi(session_id, request_id, response) ->
      case ui_response_too_large(response) {
        True -> #(
          state,
          command.rejected(
            operator_command,
            "ui_response_too_large",
            Some("operator UI response value is too large"),
          ),
        )
        False ->
          route_worker_command_sync(
            state,
            operator_command,
            session_id,
            timeout_ms,
            fn(subject, reply) {
              process.send(
                subject,
                worker_command.RespondToUi(request_id, response, reply),
              )
            },
          )
      }
  }
}

fn log_operator_transition(
  transition: #(State, command.CommandResult),
) -> #(State, command.CommandResult) {
  let #(state, result) = transition
  log_operator_result(state, result, [])
  #(state, result)
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
  case simplifile.read(state.chosen_path) {
    Error(_) -> {
      let state = mark_reload_invalid(state, "missing_workflow_file")
      #(
        state,
        command.rejected(
          operator_command,
          "missing_workflow_file",
          Some("workflow file could not be read"),
        ),
      )
    }
    Ok(contents) -> {
      let state = case contents == state.last_contents {
        True -> state
        False -> apply_new_contents(state, contents)
      }
      case state.reload_state.current_status {
        config.CurrentValid -> #(
          state,
          command.applied(operator_command, Some("workflow reloaded")),
        )
        config.CurrentInvalid(reason) -> #(
          state,
          command.rejected(
            operator_command,
            reason,
            Some("workflow reload failed"),
          ),
        )
      }
    }
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
  let runtime =
    domain.RuntimeState(
      ..state.runtime,
      parked: dict.delete(state.runtime.parked, issue.id),
      retry_attempts: dict.delete(state.runtime.retry_attempts, issue.id),
      issue_counters: dict.delete(state.runtime.issue_counters, issue.id),
    )
  let state = State(..state, runtime: runtime) |> cancel_retry_timer(issue.id)
  case
    config.can_dispatch(state.reload_state)
    && core.should_dispatch(state.runtime, state.effective, issue)
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
      case
        dict.has_key(state.runtime.running, issue.id)
        || dict.has_key(state.runtime.claimed, issue.id)
        || dict.has_key(state.pending_claims, issue.id)
      {
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
          let state = park_issue_state(state, issue, reason)
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
  ) ->
    Nil,
) -> #(State, command.CommandResult) {
  case worker_for_session(state, session_id) {
    Error(Nil) ->
      case yaml_run_for_session(state, session_id) {
        Ok(_) -> #(
          state,
          command.not_allowed(
            operator_command,
            "worker_command_subject_unavailable",
            Some("session worker does not accept operator commands"),
          ),
        )
        Error(Nil) ->
          route_step_command_sync(
            state,
            operator_command,
            session_id,
            timeout_ms,
            send,
          )
      }
    Ok(handle) ->
      case handle.command_subject {
        None -> #(
          state,
          command.not_allowed(
            operator_command,
            "worker_command_subject_unavailable",
            Some("session worker does not accept operator commands"),
          ),
        )
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
  ) ->
    Nil,
) -> #(State, command.CommandResult) {
  case dict.get(state.step_command_subjects, session_id) {
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
  ) ->
    Nil,
  subject: process.Subject(worker_command.Command),
) -> #(State, command.CommandResult) {
  let worker_reply = process.new_subject()
  send(subject, worker_reply)
  case
    process.receive(worker_reply, within: worker_command_timeout(timeout_ms))
  {
    Ok(reply) -> #(
      state,
      worker_reply_to_command_result(operator_command, reply),
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

fn worker_command_timeout(timeout_ms: Int) -> Int {
  let client_timeout = case timeout_ms > 25 {
    True -> timeout_ms - 25
    False ->
      case timeout_ms > 1 {
        True -> timeout_ms - 1
        False -> timeout_ms
      }
  }
  min_int(client_timeout, max_worker_command_wait_ms)
}

fn min_int(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}

fn operator_prompt_too_large(message: String) -> Bool {
  string.length(message) > worker_command.max_operator_prompt_chars
}

fn ui_response_too_large(response: command.UiResponse) -> Bool {
  case response {
    command.UiCancel -> False
    command.UiValue(value) ->
      string.length(value) > worker_command.max_operator_ui_value_chars
  }
}

fn worker_reply_to_command_result(
  operator_command: command.OperatorCommand,
  reply: worker_command.Reply,
) -> command.CommandResult {
  case reply {
    worker_command.Applied(message) ->
      command.applied(operator_command, message)
    worker_command.Queued(message) -> command.queued(operator_command, message)
    worker_command.Rejected(reason, message) ->
      command.rejected(operator_command, reason, message)
    worker_command.NotFound(message) ->
      command.not_found(operator_command, message)
    worker_command.NotAllowed(reason, message) ->
      command.not_allowed(operator_command, reason, message)
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
      case yaml_run_for_session(state, session_id) {
        Ok(handle) ->
          stop_yaml_session_for_operator(
            state,
            operator_command,
            handle,
            "operator_abort",
          )
        Error(Nil) ->
          abort_step_session_for_operator_sync(
            state,
            operator_command,
            session_id,
            timeout_ms,
          )
      }
    Ok(handle) ->
      case handle.command_subject {
        None ->
          stop_session_for_operator(
            state,
            operator_command,
            session_id,
            "operator_abort",
          )
        Some(subject) -> {
          let worker_reply = process.new_subject()
          process.send(subject, worker_command.Abort(worker_reply))
          case
            process.receive(
              worker_reply,
              within: worker_command_timeout(timeout_ms),
            )
          {
            Ok(reply) -> #(
              state,
              worker_reply_to_command_result(operator_command, reply),
            )
            Error(_) ->
              stop_session_for_operator(
                state,
                operator_command,
                session_id,
                "operator_abort",
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
  case dict.get(state.step_command_subjects, session_id) {
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
          within: worker_command_timeout(timeout_ms),
        )
      {
        Ok(reply) -> #(
          state,
          worker_reply_to_command_result(operator_command, reply),
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
  reason: String,
) -> #(State, command.CommandResult) {
  case worker_for_session(state, session_id) {
    Error(Nil) -> #(
      state,
      command.not_found(operator_command, Some("session not found")),
    )
    Ok(handle) -> {
      process.demonitor_process(handle.monitor)
      hub.update_status(
        state.event_hub,
        handle.session_id,
        session_event.Stopping,
      )
      publish_lifecycle(
        state,
        handle.session_id,
        "operator_command",
        Some(reason),
      )
      publish_lifecycle(state, handle.session_id, "worker_exited", Some(reason))
      hub.finish_session(state.event_hub, handle.session_id, reason)
      process.kill(handle.pid)
      let state =
        State(
          ..state,
          workers: dict.delete(state.workers, handle.issue_id),
          worker_monitors: dict.delete(state.worker_monitors, handle.monitor),
          issue_sessions: dict.delete(state.issue_sessions, handle.issue_id),
        )
        |> park_issue_state(handle.issue, reason)
      #(state, command.applied(operator_command, Some(reason)))
    }
  }
}

fn issue_is_running_claimed_or_pending(state: State, issue_id: String) -> Bool {
  dict.has_key(state.runtime.running, issue_id)
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
) -> Result(WorkerHandle, Nil) {
  state.workers
  |> dict.values
  |> list.filter(fn(handle) { handle.session_id == session_id })
  |> first_worker
}

fn first_worker(handles: List(WorkerHandle)) -> Result(WorkerHandle, Nil) {
  case handles {
    [handle, ..] -> Ok(handle)
    [] -> Error(Nil)
  }
}

fn park_issue_state(state: State, issue: domain.Issue, reason: String) -> State {
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
  log_state(state, "warn", "issue_parked", [
    #("issue_id", issue.id),
    #("reason", reason),
  ])
  state
}

fn unpark_issue_state(state: State, issue_id: String) -> State {
  let runtime =
    domain.RuntimeState(
      ..state.runtime,
      parked: dict.delete(state.runtime.parked, issue_id),
      retry_attempts: dict.delete(state.runtime.retry_attempts, issue_id),
      issue_counters: dict.delete(state.runtime.issue_counters, issue_id),
    )
  let state = State(..state, runtime: runtime) |> cancel_retry_timer(issue_id)
  log_state(state, "info", "issue_unparked", [#("issue_id", issue_id)])
  state
}

fn unpark_if_issue_changed_state(state: State, issue: domain.Issue) -> State {
  let had_retry = dict.has_key(state.runtime.retry_attempts, issue.id)
  let runtime = core.unpark_if_issue_changed(state.runtime, issue)
  let state = State(..state, runtime: runtime)
  case had_retry && !dict.has_key(runtime.retry_attempts, issue.id) {
    True -> cancel_retry_timer(state, issue.id)
    False -> state
  }
}

fn cancel_retry_timer(state: State, issue_id: String) -> State {
  case dict.get(state.retry_timers, issue_id) {
    Ok(timer) -> state.dependencies.cancel_timer(timer)
    Error(_) -> Nil
  }
  State(..state, retry_timers: dict.delete(state.retry_timers, issue_id))
}

fn handle_poll_tick(state: State, generation: Int) -> State {
  case generation != state.poll_generation || state.poll_in_flight != None {
    True -> state
    False -> {
      log_state(state, "info", "tick_started", [
        #("generation", int.to_string(generation)),
      ])
      let state = reload_if_changed(state)
      let state = State(..state, poll_in_flight: Some(generation))
      begin_running_refresh(state, generation)
    }
  }
}

fn reload_if_changed(state: State) -> State {
  case simplifile.read(state.chosen_path) {
    Error(_) -> mark_reload_invalid(state, "missing_workflow_file")
    Ok(contents) ->
      case contents == state.last_contents {
        True -> state
        False -> apply_new_contents(state, contents)
      }
  }
}

fn apply_new_contents(state: State, contents: String) -> State {
  case runtime_bundle.load(Some(state.chosen_path)) {
    Error(runtime_bundle.BundleError(code, _)) -> {
      let state =
        State(
          ..state,
          last_contents: contents,
          reload_state: config.ReloadState(
            last_known_good: Some(state.effective),
            current_status: config.CurrentInvalid(code),
          ),
        )
      log_state(state, "warn", "workflow_reload_failed", [#("error", code)])
      state
    }
    Ok(bundle) -> {
      let effective = bundle.effective
      case validate_reloaded_bundle(bundle, effective) {
        Error(err) -> {
          let state =
            State(
              ..state,
              last_contents: contents,
              reload_state: config.ReloadState(
                last_known_good: Some(state.effective),
                current_status: config.CurrentInvalid(error.config_code(err)),
              ),
            )
          log_state(state, "warn", "workflow_reload_failed", [
            #("error", error.config_code(err)),
          ])
          state
        }
        Ok(Nil) -> apply_reloaded_bundle(state, contents, bundle, effective)
      }
    }
  }
}

fn validate_reloaded_bundle(
  bundle: runtime_bundle.RuntimeBundle,
  effective: domain.EffectiveConfig,
) -> Result(Nil, error.ConfigError) {
  case bundle.mode {
    runtime_bundle.LegacyMarkdown -> config.validate_dispatch(effective)
    runtime_bundle.OrchestratorYaml -> Ok(Nil)
  }
}

fn apply_reloaded_bundle(
  state: State,
  contents: String,
  bundle: runtime_bundle.RuntimeBundle,
  effective: domain.EffectiveConfig,
) -> State {
  let secrets = config.resolved_secrets(effective)
  let runtime =
    domain.RuntimeState(
      ..state.runtime,
      poll_interval_ms: effective.polling.interval_ms,
      max_concurrent_agents: effective.agent.max_concurrent_agents,
    )
  let state =
    State(
      ..state,
      last_contents: contents,
      definition: workflow_definition_from_bundle(bundle),
      bundle: bundle,
      effective: effective,
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
      reload_state: config.ReloadState(
        last_known_good: Some(effective),
        current_status: config.CurrentValid,
      ),
      secrets: secrets,
    )
  log_state(state, "info", "workflow_reloaded", [])
  state
}

fn mark_reload_invalid(state: State, reason: String) -> State {
  let state =
    State(
      ..state,
      reload_state: config.ReloadState(
        last_known_good: Some(state.effective),
        current_status: config.CurrentInvalid(reason),
      ),
    )
  log_state(state, "warn", "workflow_reload_failed", [#("error", reason)])
  state
}

fn begin_running_refresh(state: State, generation: Int) -> State {
  let ids = dict.keys(state.runtime.running)
  case ids {
    [] -> begin_candidate_fetch_or_finish(state, generation)
    _ ->
      enqueue_side_effect(
        state,
        RefreshRunning(
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
    && config.can_dispatch(state.reload_state)
    && state.effective.agent.max_concurrent_agents != 0
    && slots_remain(state)
  {
    False -> begin_linear_command_fetch_or_finish(state, generation, [], False)
    True ->
      enqueue_side_effect(
        state,
        FetchCandidates(generation: generation, client: state.tracker_client),
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
              core.reconcile_issue(acc.runtime, acc.effective, issue)
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
  case state.effective.linear_commands.enabled {
    False -> finish_linear_command_phase(state, candidates, dispatch_after)
    True -> {
      let issue_ids = observed_issue_ids(state, candidates)
      case issue_ids {
        [] -> finish_linear_command_phase(state, candidates, dispatch_after)
        _ ->
          enqueue_side_effect(
            state,
            FetchLinearCommands(
              generation: generation,
              issue_ids: issue_ids,
              candidates: candidates,
              dispatch_after: dispatch_after,
              client: state.linear_command_client,
              limit_per_issue: state.effective.linear_commands.poll_limit_per_issue,
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
      state.effective.linear_commands,
      comments,
      state.issue_sessions,
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

fn apply_linear_transport_action(
  state: State,
  action: linear_transport.TransportAction,
) -> State {
  case action {
    linear_transport.SubmitCommand(comment, parsed) -> {
      let #(state, result) =
        apply_operator_command_to_state(state, parsed.command, 1000)
      log_state(state, "info", "linear_operator_command", [
        #("comment_id", comment.id),
        #("command", result.command),
        #("status", command.status_to_string(result.status)),
      ])
      case
        linear_transport.should_ack_result(
          state.effective.linear_commands,
          result,
        )
      {
        True ->
          enqueue_side_effect(
            state,
            PostLinearCommandAck(
              issue_id: comment.issue_id,
              source_comment_id: comment.id,
              body: linear_transport.result_ack_body(
                comment.id,
                parsed,
                result,
                state.secrets,
              ),
              client: state.linear_command_client,
            ),
          )
        False -> state
      }
    }
    linear_transport.PostAck(issue_id, body) ->
      enqueue_side_effect(
        state,
        PostLinearCommandAck(
          issue_id: issue_id,
          source_comment_id: "",
          body: body,
          client: state.linear_command_client,
        ),
      )
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
  []
  |> append_unique_list(dict.keys(state.runtime.running))
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
  generation != state.poll_generation
  || state.poll_in_flight != Some(generation)
}

fn dispatch_candidates(issues: List(domain.Issue), state: State) -> State {
  case !state.operator_paused && config.can_dispatch(state.reload_state) {
    False -> state
    True ->
      case issues {
        [] -> state
        [issue, ..rest] -> {
          let state = unpark_if_issue_changed_state(state, issue)
          case
            core.dispatch_preconditions_satisfied_without_slot_capacity(
              state.runtime,
              state.effective,
              issue,
            )
            && !dict.has_key(state.pending_claims, issue.id)
          {
            False -> dispatch_candidates(rest, state)
            True ->
              case
                workflow_policy.classify_issue(
                  state.effective.linear_contract,
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
      state.effective.linear_contract,
    )
  {
    True -> dispatch_candidates(remaining_candidates, state)
    False -> {
      let fingerprint = workflow_policy.violation_fingerprint(violation)
      let reporting_policy_fingerprint =
        workflow_policy.reporting_policy_fingerprint(
          state.effective.linear_contract,
        )
      let runtime =
        core.mark_invalid_workflow_report_pending(
          state.runtime,
          issue,
          violation,
          state.effective.linear_contract,
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
          ReportInvalidWorkflow(
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
  case state.poll_timer {
    Some(timer) -> state.dependencies.cancel_timer(timer)
    None -> Nil
  }
  let generation = state.poll_generation + 1
  let timer =
    state.dependencies.send_after(
      state.subject,
      state.effective.polling.interval_ms,
      PollTick(generation),
    )
  State(
    ..state,
    poll_generation: generation,
    poll_in_flight: None,
    poll_timer: Some(timer),
  )
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
              retry_timers: dict.delete(state.retry_timers, issue_id),
            )
          case state.operator_paused, config.can_dispatch(state.reload_state) {
            True, _ ->
              defer_retry_until_dispatch_available(state, issue_id, generation)
            _, False ->
              defer_retry_until_dispatch_available(state, issue_id, generation)
            False, True -> begin_retry_refresh(state, issue_id, generation)
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
  State(..state, retry_timers: dict.insert(state.retry_timers, issue_id, timer))
}

fn begin_retry_refresh(state: State, issue_id: String, generation: Int) -> State {
  case dict.get(state.retry_refreshes_in_flight, issue_id) {
    Ok(_) -> {
      log_state(state, "info", "retry_timer_stale", [#("issue_id", issue_id)])
      state
    }
    Error(_) -> {
      let state =
        State(
          ..state,
          retry_refreshes_in_flight: dict.insert(
            state.retry_refreshes_in_flight,
            issue_id,
            generation,
          ),
        )
      enqueue_side_effect(
        state,
        RefreshRetry(
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
    State(
      ..state,
      retry_refreshes_in_flight: dict.delete(
        state.retry_refreshes_in_flight,
        issue_id,
      ),
    )
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
          case config.can_dispatch(state.reload_state) {
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
          state.effective,
          issue_id,
          issue,
        )
      {
        False -> handle_retry_candidate_with_slots(state, issue_id, candidate)
        True ->
          case
            workflow_policy.classify_issue(
              state.effective.linear_contract,
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
        core.schedule_retry(
          state.runtime,
          issue_id,
          1000,
          "no available orchestrator slots",
        )
      let state = State(..state, runtime: transition.state)
      apply_effects(state, transition.effects)
    }
    False -> {
      let transition =
        core.handle_retry_candidate(
          state.runtime,
          state.effective,
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
  state.effective.agent.max_concurrent_agents != 0
  && dispatch_slots_used(state) < state.effective.agent.max_concurrent_agents
}

fn can_reserve_dispatch_slot(state: State, issue: domain.Issue) -> Bool {
  !dict.has_key(state.pending_claims, issue.id)
  && slots_remain(state)
  && per_state_dispatch_slot_available(state, issue.state)
}

fn dispatch_slots_used(state: State) -> Int {
  dict.size(state.runtime.running) + dict.size(state.pending_claims)
}

fn per_state_dispatch_slot_available(state: State, issue_state: String) -> Bool {
  let key = normalize_state(issue_state)
  case dict.get(state.effective.agent.max_concurrent_agents_by_state, key) {
    Error(_) -> True
    Ok(limit) -> dispatch_count_for_state(state, key) < limit
  }
}

fn dispatch_count_for_state(state: State, normalized_state: String) -> Int {
  running_count_for_state(state, normalized_state)
  + pending_claim_count_for_state(state, normalized_state)
}

fn running_count_for_state(state: State, normalized_state: String) -> Int {
  state.runtime.running
  |> dict.to_list
  |> list.filter(fn(entry) {
    let #(_, running) = entry
    normalize_state(running.issue.state) == normalized_state
  })
  |> list.length
}

fn pending_claim_count_for_state(state: State, normalized_state: String) -> Int {
  state.pending_claims
  |> dict.to_list
  |> list.filter(fn(entry) {
    let #(_, pending) = entry
    normalize_state(pending.issue.state) == normalized_state
  })
  |> list.length
}

fn normalize_state(value: String) -> String {
  value |> string.trim |> string.lowercase
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
              state.effective.workspace.root,
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
              let session_sequence = state.next_session_sequence
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
                  next_session_sequence: session_sequence + 1,
                  pending_claims: dict.insert(
                    state.pending_claims,
                    issue.id,
                    pending,
                  ),
                )
              enqueue_side_effect(
                state,
                ClaimIssue(
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
  case state.bundle.mode {
    runtime_bundle.LegacyMarkdown -> True
    runtime_bundle.OrchestratorYaml ->
      case runtime_bundle.select_workflow(state.bundle, issue) {
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
}

fn retry_dispatch_later_if_needed(state: State, issue: domain.Issue) -> State {
  case dict.has_key(state.runtime.retry_attempts, issue.id) {
    False -> state
    True -> {
      let transition =
        core.schedule_retry(
          state.runtime,
          issue.id,
          1000,
          "no available orchestrator slots",
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
  publish_lifecycle(state, session_id, "dispatch_started", None)
  log_state(state, "info", "dispatch_started", [
    #("issue_id", issue.id),
    #("issue_identifier", issue.identifier),
    #("run_id", run_id),
    #("workspace_path", workspace_path),
  ])
  let runtime = core.apply_worker_start(state.runtime, issue, workspace_path)
  let subject = state.subject
  let dependencies = state.dependencies
  let definition = state.definition
  let effective = state.effective
  let tracker_client = state.tracker_client
  let bundle = state.bundle
  let secrets = state.secrets
  let pid =
    process.spawn_unlinked(fn() {
      let result = case bundle.mode {
        runtime_bundle.LegacyMarkdown -> {
          let command_subject = process.new_subject()
          dependencies.agent_runner(
            issue,
            None,
            definition,
            effective,
            tracker_client,
            fn(_, update) {
              process.send(subject, WorkerUpdate(issue.id, update))
            },
            command_subject,
            fn() {
              process.send(
                subject,
                WorkerCommandReady(issue.id, run_id, command_subject),
              )
            },
          )
        }
        runtime_bundle.OrchestratorYaml ->
          run_yaml_worker(
            issue,
            run_id,
            bundle,
            tracker_client,
            secrets,
            dependencies.workflow_run_dependencies,
            subject,
            state.event_hub,
            dependencies.now_ms,
          )
      }
      process.send(subject, WorkerFinished(issue.id, run_id, result))
    })
  let monitor = process.monitor(pid)
  publish_lifecycle(state, session_id, "worker_started", None)
  hub.update_status(state.event_hub, session_id, session_event.Running)
  let handle =
    WorkerHandle(
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
    workers: dict.insert(state.workers, issue.id, handle),
    worker_monitors: dict.insert(state.worker_monitors, monitor, issue.id),
    issue_sessions: dict.insert(state.issue_sessions, issue.id, session_id),
  )
}

fn run_yaml_worker(
  issue: domain.Issue,
  run_id: String,
  bundle: runtime_bundle.RuntimeBundle,
  tracker_client: tracker.Client,
  secrets: List(String),
  workflow_dependencies: workflow_run.Dependencies,
  daemon_subject: process.Subject(Message),
  event_hub: process.Subject(hub.Message),
  now_ms: fn() -> Int,
) -> Result(runner.WorkerSuccess, runner.WorkerFailure) {
  case bundle.orchestrator {
    None ->
      Error(yaml_worker_failure("missing_orchestrator_config", None, issue))
    Some(orchestrator) ->
      case runtime_bundle.select_workflow(bundle, issue) {
        Error(runtime_bundle.BundleError(code, _)) ->
          Error(yaml_worker_failure(code, None, issue))
        Ok(#(_, dag)) ->
          case
            workflow_run.execute(
              issue,
              dag,
              orchestrator,
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
      name: "step_started",
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
  let artifact =
    base.command_step(
      step_id,
      command,
      workspace_path,
      timeout_ms,
      secrets,
      limits,
    )
  let reason = case artifact.status == "success" {
    True -> "normal"
    False -> "failed"
  }
  hub.finish_session(event_hub, session_id, reason)
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
) -> Result(runner.WorkerSuccess, runner.WorkerFailure) {
  let session_id = run_id <> "-" <> step_id
  let started_at_ms = now_ms()
  hub.register_session(
    event_hub,
    session_event.SessionSummary(
      session_id: session_id,
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
      name: "step_started",
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
      hub.finish_session(event_hub, session_id, "normal")
    }
    Error(failure) -> {
      case tokens_are_nonzero(failure.tokens) {
        True -> hub.update_tokens(event_hub, session_id, failure.tokens)
        False -> Nil
      }
      hub.finish_session(event_hub, session_id, "failed")
    }
  }
  process.send(daemon_subject, YamlStepFinished(session_id))
  result
}

fn yaml_worker_failure(
  reason: String,
  workspace_path: Option(String),
  issue: domain.Issue,
) -> runner.WorkerFailure {
  runner.WorkerFailure(
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
  case dict.get(state.workers, issue_id) {
    Error(_) -> state
    Ok(handle) ->
      case handle.run_id == run_id {
        False -> state
        True ->
          State(
            ..state,
            workers: dict.insert(
              state.workers,
              issue_id,
              WorkerHandle(..handle, command_subject: Some(command_subject)),
            ),
          )
      }
  }
}

fn handle_worker_update(
  state: State,
  issue_id: String,
  update: runner.PiUpdate,
) -> State {
  case dict.get(state.workers, issue_id) {
    Ok(handle) -> publish_worker_update(state, handle.session_id, update)
    Error(_) -> Nil
  }
  case update.event {
    "message_update" -> state
    _ -> {
      let message = case update.message {
        Some(message) -> log.truncate(message, 200)
        None -> ""
      }
      log_state(state, "info", "pi_event", [
        #("issue_id", issue_id),
        #("event_name", update.event),
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
  result: Result(runner.WorkerSuccess, runner.WorkerFailure),
) -> State {
  case dict.get(state.workers, issue_id) {
    Error(_) -> {
      log_state(state, "warn", "worker_finished_stale", [
        #("issue_id", issue_id),
      ])
      State(
        ..state,
        issue_sessions: dict.delete(state.issue_sessions, issue_id),
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
              workers: dict.delete(state.workers, issue_id),
              worker_monitors: dict.delete(
                state.worker_monitors,
                handle.monitor,
              ),
              issue_sessions: dict.delete(state.issue_sessions, issue_id),
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
  handle: WorkerHandle,
  success: runner.WorkerSuccess,
) -> State {
  log_state(state, "info", "worker_exited", [
    #("issue_id", handle.issue_id),
    #("run_id", handle.run_id),
    #("reason", "normal"),
  ])
  hub.update_tokens(state.event_hub, handle.session_id, success.tokens)
  publish_lifecycle(state, handle.session_id, "worker_exited", Some("normal"))
  hub.finish_session(state.event_hub, handle.session_id, "normal")
  let final_issue = case success.final_issue {
    Some(issue) -> issue
    None -> handle.issue
  }
  case state.bundle.mode {
    runtime_bundle.LegacyMarkdown -> {
      let transition =
        core.apply_worker_success_with_workspace_path(
          state.runtime,
          state.effective,
          handle.issue_id,
          final_issue,
          success.workspace_path,
          success.tokens,
          state.dependencies.now_ms(),
        )
      let state = State(..state, runtime: transition.state)
      let state =
        enqueue_side_effect(
          state,
          ReportSuccess(
            issue_id: handle.issue_id,
            issue: final_issue,
            success: success,
            run_id: handle.run_id,
            client: state.handoff_client,
          ),
        )
      apply_effects(state, transition.effects)
    }
    runtime_bundle.OrchestratorYaml ->
      finish_yaml_worker_success(state, handle, final_issue, success)
  }
}

fn finish_yaml_worker_success(
  state: State,
  handle: WorkerHandle,
  final_issue: domain.Issue,
  success: runner.WorkerSuccess,
) -> State {
  let runtime =
    domain.RuntimeState(
      ..state.runtime,
      running: dict.delete(state.runtime.running, handle.issue_id),
      claimed: dict.delete(state.runtime.claimed, handle.issue_id),
      completed: dict.insert(
        state.runtime.completed,
        handle.issue_id,
        final_issue,
      ),
      aggregate_pi_totals: add_tokens(
        state.runtime.aggregate_pi_totals,
        success.tokens,
      ),
    )
  State(..state, runtime: runtime)
  |> enqueue_side_effect(ReportSuccess(
    issue_id: handle.issue_id,
    issue: final_issue,
    success: success,
    run_id: handle.run_id,
    client: state.handoff_client,
  ))
}

fn finish_worker_failure(
  state: State,
  handle: WorkerHandle,
  failure: runner.WorkerFailure,
) -> State {
  case failure.reason {
    error.OperatorAbort ->
      finish_operator_worker_exit(state, handle, failure, "operator_abort")
    error.OperatorStopAfterCurrentTurn ->
      finish_operator_worker_exit(
        state,
        handle,
        failure,
        "operator_stop_after_current_turn",
      )
    _ -> {
      log_state(state, "warn", "worker_exited", [
        #("issue_id", handle.issue_id),
        #("run_id", handle.run_id),
        #("reason", "failed"),
      ])
      publish_lifecycle(
        state,
        handle.session_id,
        "worker_exited",
        Some("failed"),
      )
      hub.finish_session(state.event_hub, handle.session_id, "failed")
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
          state.effective,
          handle.issue_id,
          baseline_issue,
          state.dependencies.now_ms(),
        )
      let state = State(..state, runtime: transition.state)
      let state =
        enqueue_side_effect(
          state,
          ReportFailure(
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

fn finish_operator_worker_exit(
  state: State,
  handle: WorkerHandle,
  failure: runner.WorkerFailure,
  reason: String,
) -> State {
  log_state(state, "warn", "worker_exited", [
    #("issue_id", handle.issue_id),
    #("run_id", handle.run_id),
    #("reason", reason),
  ])
  case tokens_are_nonzero(failure.tokens) {
    True ->
      hub.update_tokens(state.event_hub, handle.session_id, failure.tokens)
    False -> Nil
  }
  publish_lifecycle(state, handle.session_id, "worker_exited", Some(reason))
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
  |> park_issue_state(final_issue, reason)
}

fn handle_worker_down(state: State, down: process.Down) -> State {
  case down {
    process.ProcessDown(monitor, _, _) ->
      case dict.get(state.worker_monitors, monitor) {
        Error(_) -> handle_yaml_workflow_down(state, monitor)
        Ok(issue_id) ->
          case dict.get(state.workers, issue_id) {
            Error(_) -> {
              log_state(state, "warn", "worker_down_stale", [])
              State(
                ..state,
                worker_monitors: dict.delete(state.worker_monitors, monitor),
                issue_sessions: dict.delete(state.issue_sessions, issue_id),
              )
            }
            Ok(handle) -> {
              let state =
                State(
                  ..state,
                  workers: dict.delete(state.workers, issue_id),
                  worker_monitors: dict.delete(state.worker_monitors, monitor),
                  issue_sessions: dict.delete(state.issue_sessions, issue_id),
                )
              log_state(state, "warn", "worker_down", [#("issue_id", issue_id)])
              publish_lifecycle(state, handle.session_id, "worker_down", None)
              let failure =
                runner.WorkerFailure(
                  reason: error.PiFailed(error.PiProtocolError("worker_down")),
                  workspace_path: Some(handle.workspace_path),
                  tokens: domain.zero_token_totals(),
                  final_issue: None,
                )
              finish_worker_failure(state, handle, failure)
            }
          }
      }
    process.PortDown(_, _, _) -> state
  }
}

fn handle_side_effect_finished(state: State, result: SideEffectResult) -> State {
  let in_flight = case state.side_effects_in_flight <= 0 {
    True -> 0
    False -> state.side_effects_in_flight - 1
  }
  let state = State(..state, side_effects_in_flight: in_flight)
  let state = case result {
    CandidateFetchFinished(generation, result) ->
      handle_candidate_fetch_finished(state, generation, result)
    LinearCommandFetchFinished(generation, candidates, dispatch_after, result) ->
      handle_linear_command_fetch_finished(
        state,
        generation,
        candidates,
        dispatch_after,
        result,
      )
    RunningRefreshFinished(generation, result) ->
      handle_running_refresh_finished(state, generation, result)
    RetryRefreshFinished(issue_id, generation, result) ->
      handle_retry_refresh_finished(state, issue_id, generation, result)
    HandoffClaimFinished(issue_id, run_id, result) ->
      handle_handoff_claim_finished(state, issue_id, run_id, result)
    HandoffSuccessFinished(issue_id, _run_id, result) ->
      handle_handoff_success_finished(state, issue_id, result)
    HandoffFailureFinished(issue_id, _run_id, result) ->
      handle_handoff_failure_finished(state, issue_id, result)
    LinearCommandAckFinished(issue_id, comment_id, result) ->
      handle_linear_command_ack_finished(state, issue_id, comment_id, result)
    InvalidWorkflowReportFinished(
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
    CleanupFinished(workspace_path, result) ->
      handle_cleanup_finished(state, workspace_path, result)
  }
  drain_side_effects(state)
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
    Ok(Nil) -> state
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

fn enqueue_side_effect(state: State, effect: SideEffect) -> State {
  let state =
    State(
      ..state,
      side_effect_queue: list.append(state.side_effect_queue, [effect]),
    )
  drain_side_effects(state)
}

fn drain_side_effects(state: State) -> State {
  case state.side_effects_in_flight >= max_side_effects() {
    True -> state
    False ->
      case state.side_effect_queue {
        [] -> state
        [effect, ..rest] -> {
          spawn_side_effect(state.subject, effect)
          let state =
            State(
              ..state,
              side_effects_in_flight: state.side_effects_in_flight + 1,
              side_effect_queue: rest,
            )
          drain_side_effects(state)
        }
      }
  }
}

fn max_side_effects() -> Int {
  4
}

fn spawn_side_effect(
  subject: process.Subject(Message),
  effect: SideEffect,
) -> Nil {
  let _ =
    process.spawn_unlinked(fn() {
      process.send(subject, SideEffectFinished(run_side_effect(effect)))
    })
  Nil
}

fn run_side_effect(effect: SideEffect) -> SideEffectResult {
  case effect {
    FetchCandidates(generation, client) ->
      CandidateFetchFinished(generation, client.fetch_candidate_issues())
    FetchLinearCommands(
      generation,
      issue_ids,
      candidates,
      dispatch_after,
      client,
      limit_per_issue,
    ) ->
      LinearCommandFetchFinished(
        generation,
        candidates,
        dispatch_after,
        client.fetch_comments(issue_ids, limit_per_issue),
      )
    RefreshRunning(generation, ids, client) ->
      RunningRefreshFinished(generation, client.fetch_issue_states_by_ids(ids))
    RefreshRetry(issue_id, generation, client) ->
      RetryRefreshFinished(
        issue_id,
        generation,
        client.fetch_issue_states_by_ids([issue_id]),
      )
    ClaimIssue(issue, _workspace_path, run_id, client) ->
      HandoffClaimFinished(issue.id, run_id, client.claim_issue(issue, run_id))
    ReportSuccess(issue_id, issue, success, run_id, client) ->
      HandoffSuccessFinished(
        issue_id,
        run_id,
        client.report_success(issue, success, run_id),
      )
    ReportFailure(issue_id, issue, failure, run_id, client) ->
      HandoffFailureFinished(
        issue_id,
        run_id,
        client.report_failure(issue, failure, run_id),
      )
    PostLinearCommandAck(issue_id, source_comment_id, body, client) ->
      LinearCommandAckFinished(
        issue_id,
        source_comment_id,
        client.post_ack(issue_id, body),
      )
    ReportInvalidWorkflow(
      issue,
      violation,
      violation_fingerprint,
      reporting_policy_fingerprint,
      client,
    ) ->
      InvalidWorkflowReportFinished(
        issue.id,
        violation_fingerprint,
        reporting_policy_fingerprint,
        client.report_invalid_workflow(issue, violation),
      )
    CleanupWorkspace(root, workspace_path, hooks, cleanup) ->
      CleanupFinished(workspace_path, cleanup(root, workspace_path, hooks))
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
      case dict.get(state.issue_sessions, issue_id) {
        Ok(session_id) ->
          publish_lifecycle(state, session_id, "retry_scheduled", Some(reason))
        Error(_) -> Nil
      }
      log_state(state, "info", "retry_scheduled", [
        #("issue_id", issue_id),
        #("delay_ms", int.to_string(delay_ms)),
        #("generation", int.to_string(generation)),
        #("reason", reason),
      ])
      let timer =
        state.dependencies.send_after(
          state.subject,
          delay_ms,
          RetryTick(issue_id, generation),
        )
      State(
        ..state,
        retry_timers: dict.insert(state.retry_timers, issue_id, timer),
      )
    }
    core.CancelRetry(issue_id) -> {
      case dict.get(state.retry_timers, issue_id) {
        Ok(timer) -> state.dependencies.cancel_timer(timer)
        Error(_) -> Nil
      }
      State(..state, retry_timers: dict.delete(state.retry_timers, issue_id))
    }
    core.CleanupWorkspace(workspace_path) -> {
      case string.trim(workspace_path) == "" {
        True -> state
        False ->
          enqueue_side_effect(
            state,
            CleanupWorkspace(
              root: state.effective.workspace.root,
              workspace_path: workspace_path,
              hooks: state.effective.hooks,
              cleanup: state.dependencies.cleanup,
            ),
          )
      }
    }
    core.StopWorker(issue_id, reason) -> {
      case dict.get(state.workers, issue_id) {
        Error(_) -> state
        Ok(handle) -> {
          hub.update_status(
            state.event_hub,
            handle.session_id,
            session_event.Stopping,
          )
          publish_lifecycle(
            state,
            handle.session_id,
            "stop_requested",
            Some(reason),
          )
          stop_worker(handle)
          publish_lifecycle(
            state,
            handle.session_id,
            "worker_exited",
            Some("stopped"),
          )
          hub.finish_session(state.event_hub, handle.session_id, "stopped")
          log_state(state, "warn", "worker_stop_requested", [
            #("issue_id", issue_id),
            #("reason", reason),
          ])
          State(
            ..state,
            workers: dict.delete(state.workers, issue_id),
            worker_monitors: dict.delete(state.worker_monitors, handle.monitor),
            issue_sessions: dict.delete(state.issue_sessions, issue_id),
          )
        }
      }
    }
    core.ReleaseClaim(issue_id) -> {
      log_state(state, "info", "claim_released", [#("issue_id", issue_id)])
      state
    }
    core.ParkIssue(issue_id, reason) -> {
      log_state(state, "warn", "issue_parked", [
        #("issue_id", issue_id),
        #("reason", reason),
      ])
      state
    }
  }
}

fn publish_worker_update(
  state: State,
  session_id: String,
  update: runner.PiUpdate,
) -> Nil {
  case status_for_update(update) {
    Some(status) -> hub.update_status(state.event_hub, session_id, status)
    None -> Nil
  }
  case update.pi_session_id {
    Some(pi_session_id) ->
      hub.update_pi_session(state.event_hub, session_id, pi_session_id)
    None -> Nil
  }
  case tokens_are_nonzero(update.tokens) {
    True -> hub.update_tokens(state.event_hub, session_id, update.tokens)
    False -> Nil
  }
  hub.publish(state.event_hub, session_id, update_payload(update))
}

fn publish_lifecycle(
  state: State,
  session_id: String,
  name: String,
  message: Option(String),
) -> Nil {
  hub.publish(
    state.event_hub,
    session_id,
    session_event.EventPayload(
      kind: session_event.Lifecycle,
      name: name,
      turn: None,
      pi_type: None,
      message: message,
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

fn update_payload(update: runner.PiUpdate) -> session_event.EventPayload {
  session_event.EventPayload(
    kind: kind_for_update(update),
    name: update.event,
    turn: update.turn,
    pi_type: pi_type_for_update(update),
    message: update.message,
    request_id: update.request_id,
    method: update.method,
    tool_name: update.tool_name,
    tool_input: update.tool_input,
    tool_output: update.tool_output,
    tool_status: update.tool_status,
    tokens: update.tokens,
    raw_json: update.raw_json,
  )
}

fn kind_for_update(update: runner.PiUpdate) -> session_event.EventKind {
  case update.event {
    "probe_started" | "probe_finished" | "pi_session_started" ->
      session_event.Lifecycle
    "turn_finished" -> session_event.TokenStats
    "message_start" | "message_update" | "message_end" ->
      session_event.AssistantMessage
    "tool_execution_start" | "tool_execution_update" | "tool_execution_end" ->
      session_event.Tool
    "message" ->
      case
        update.tool_name,
        update.tool_input,
        update.tool_output,
        update.tool_status
      {
        Some(_), _, _, _
        | _, Some(_), _, _
        | _, _, Some(_), _
        | _, _, _, Some(_)
        -> session_event.Tool
        _, _, _, _ -> session_event.Pi
      }
    "extension_ui_request" ->
      case is_blocking_ui_method(update.method) {
        True -> session_event.UiRequest
        False -> session_event.Pi
      }
    "extension_ui_response" -> session_event.UiResponse
    "agent_start" | "turn_start" | "turn_end" | "agent_end" -> session_event.Pi
    _ ->
      case update.raw_json {
        Some(_) -> session_event.PiRaw
        None -> session_event.Lifecycle
      }
  }
}

fn pi_type_for_update(update: runner.PiUpdate) -> Option(String) {
  case update.raw_json {
    Some(_) -> Some(update.event)
    None -> None
  }
}

fn status_for_update(
  update: runner.PiUpdate,
) -> Option(session_event.SessionStatus) {
  case update.event {
    "probe_started" | "probe_finished" -> Some(session_event.Probing)
    "pi_session_started" -> Some(session_event.Running)
    "extension_ui_request" ->
      case is_blocking_ui_method(update.method) {
        True -> Some(session_event.WaitingUi)
        False -> Some(session_event.Running)
      }
    "extension_ui_response" | "turn_finished" -> Some(session_event.Running)
    _ ->
      case update.raw_json {
        Some(_) -> Some(session_event.Running)
        None -> None
      }
  }
}

fn is_blocking_ui_method(method: Option(String)) -> Bool {
  case method {
    Some("select") | Some("confirm") | Some("input") | Some("editor") -> True
    _ -> False
  }
}

fn tokens_are_nonzero(tokens: domain.TokenTotals) -> Bool {
  tokens.input > 0
  || tokens.output > 0
  || tokens.cache_read > 0
  || tokens.cache_write > 0
  || tokens.total > 0
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

fn stop_worker(handle: WorkerHandle) -> Nil {
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
  state.dependencies.stop_control_server(state.control_server)
  case state.control_file_path {
    Some(path) -> control_file.remove(path)
    None -> Nil
  }
  case state.poll_timer {
    Some(timer) -> state.dependencies.cancel_timer(timer)
    None -> Nil
  }
  dict.each(state.retry_timers, fn(_, timer) {
    state.dependencies.cancel_timer(timer)
  })
  dict.each(state.worker_monitors, fn(monitor, _) {
    process.demonitor_process(monitor)
  })
  dict.each(state.yaml_run_monitors, fn(monitor, _) {
    process.demonitor_process(monitor)
  })
  dict.each(state.step_command_subject_monitors, fn(_, monitor) {
    process.demonitor_process(monitor)
  })
  dict.each(state.workers, fn(_, handle) { stop_worker(handle) })
  State(
    ..state,
    poll_in_flight: None,
    poll_timer: None,
    retry_timers: dict.new(),
    retry_refreshes_in_flight: dict.new(),
    workers: dict.new(),
    worker_monitors: dict.new(),
    issue_sessions: dict.new(),
    step_command_subjects: dict.new(),
    step_command_monitors: dict.new(),
    step_command_subject_monitors: dict.new(),
    pending_claims: dict.new(),
    side_effects_in_flight: 0,
    side_effect_queue: [],
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
  let _ = state.dependencies.logger(level, event, fields, state.secrets)
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

fn map_config_error(
  result: Result(a, error.ConfigError),
) -> Result(a, StartupError) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) -> Error(StartupError(error.config_code(err), "config error"))
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
