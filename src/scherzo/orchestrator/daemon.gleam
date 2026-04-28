import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/string
import scherzo/agent/runner
import scherzo/config
import scherzo/control/file as control_file
import scherzo/control/server as control_server
import scherzo/domain
import scherzo/error
import scherzo/handoff
import scherzo/linear
import scherzo/log
import scherzo/orchestrator/core
import scherzo/session/event as session_event
import scherzo/session/hub
import scherzo/tracker
import scherzo/workflow
import scherzo/workspace
import simplifile

pub type StartupError {
  StartupError(code: String, message: String)
}

pub type WorkerCommand {
  Abort
  StopAfterCurrentTurn
  QueuePrompt(String)
  RespondToUi(String, String)
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
  WorkerDown(process.Down)
  SideEffectFinished(SideEffectResult)
  Shutdown(process.Subject(Nil))
  GetSnapshot(process.Subject(domain.RuntimeState))
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
    command_subject: Option(process.Subject(WorkerCommand)),
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

type SideEffect {
  FetchCandidates(generation: Int, client: tracker.Client)
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
  RunningRefreshFinished(Int, Result(List(domain.Issue), error.TrackerError))
  RetryRefreshFinished(
    String,
    Int,
    Result(List(domain.Issue), error.TrackerError),
  )
  HandoffClaimFinished(String, String, Result(Nil, error.TrackerError))
  HandoffSuccessFinished(String, String, Result(Nil, error.TrackerError))
  HandoffFailureFinished(String, String, Result(Nil, error.TrackerError))
  CleanupFinished(String, Result(Nil, error.WorkspaceError))
}

pub type RuntimeDependencies {
  RuntimeDependencies(
    make_tracker: fn(domain.TrackerConfig) -> tracker.Client,
    make_handoff: fn(domain.TrackerConfig, domain.HandoffConfig) ->
      handoff.Client,
    agent_runner: fn(
      domain.Issue,
      Option(Int),
      domain.WorkflowDefinition,
      domain.EffectiveConfig,
      tracker.Client,
      fn(String, runner.PiUpdate) -> Nil,
    ) ->
      Result(runner.WorkerSuccess, runner.WorkerFailure),
    cleanup: fn(String, String, domain.HooksConfig) ->
      Result(Nil, error.WorkspaceError),
    logger: fn(String, String, List(log.Field), List(String)) ->
      Result(Nil, Nil),
    now_ms: fn() -> Int,
    send_after: fn(process.Subject(Message), Int, Message) -> TimerHandle,
    cancel_timer: fn(TimerHandle) -> Nil,
    start_event_hub: fn() -> Result(process.Subject(hub.Message), hub.HubError),
    make_control_token: fn() -> Result(String, StartupError),
    start_control_server: fn(control_server.Settings, control_server.EventStore) ->
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
    reload_state: config.ReloadState,
    effective: domain.EffectiveConfig,
    tracker_client: tracker.Client,
    handoff_client: handoff.Client,
    runtime: domain.RuntimeState,
    poll_generation: Int,
    poll_in_flight: Option(Int),
    poll_timer: Option(TimerHandle),
    retry_timers: Dict(String, TimerHandle),
    retry_refreshes_in_flight: Dict(String, Int),
    workers: Dict(String, WorkerHandle),
    worker_monitors: Dict(process.Monitor, String),
    issue_sessions: Dict(String, String),
    next_session_sequence: Int,
    pending_claims: Dict(String, PendingClaim),
    side_effects_in_flight: Int,
    side_effect_queue: List(SideEffect),
    secrets: List(String),
    event_hub: process.Subject(hub.Message),
    control_server: ControlServerHandle,
    control_file_path: Option(String),
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
    agent_runner: runner.run_attempt,
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
    )
  use handle <- try_startup(dependencies.start_control_server(
    settings,
    control_server.event_hub_store(event_hub),
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

pub fn start(
  workflow_path: Option(String),
  dependencies: RuntimeDependencies,
) -> Result(actor.Started(process.Subject(Message)), StartupError) {
  let chosen_path = workflow.choose_path(workflow_path)
  use contents <- try_startup(read_workflow_contents(chosen_path))
  use definition <- try_startup(workflow.parse(contents) |> map_workflow_error)
  use effective <- try_startup(
    config.resolve(definition, chosen_path) |> map_config_error,
  )
  use _ <- try_startup(config.validate_dispatch(effective) |> map_config_error)
  let tracker_client = dependencies.make_tracker(effective.tracker)
  let handoff_client =
    dependencies.make_handoff(effective.tracker, effective.handoff)
  let reload_state =
    config.ReloadState(
      last_known_good: Some(effective),
      current_status: config.CurrentValid,
    )
  let runtime = core.new_state(effective)
  let secrets = config.resolved_secrets(effective)
  use event_hub <- try_startup(dependencies.start_event_hub() |> map_hub_error)
  use control_plane <- try_startup(start_control_plane(
    dependencies,
    effective,
    event_hub,
    secrets,
  ))
  let builder =
    actor.new_with_initialiser(1000, fn(subject) {
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
          reload_state: reload_state,
          effective: effective,
          tracker_client: tracker_client,
          handoff_client: handoff_client,
          runtime: runtime,
          poll_generation: poll_generation,
          poll_in_flight: None,
          poll_timer: Some(poll_timer),
          retry_timers: dict.new(),
          retry_refreshes_in_flight: dict.new(),
          workers: dict.new(),
          worker_monitors: dict.new(),
          issue_sessions: dict.new(),
          next_session_sequence: 1,
          pending_claims: dict.new(),
          side_effects_in_flight: 0,
          side_effect_queue: [],
          secrets: secrets,
          event_hub: event_hub,
          control_server: control_plane.handle,
          control_file_path: control_plane.control_file_path,
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
    })
    |> actor.on_message(handle_message)
  case actor.start(builder) {
    Ok(started) -> Ok(started)
    Error(_) -> {
      dependencies.stop_control_server(control_plane.handle)
      case control_plane.control_file_path {
        Some(path) -> control_file.remove(path)
        None -> Nil
      }
      Error(StartupError("daemon_start_failed", "actor start failed"))
    }
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
    WorkerDown(down) -> actor.continue(handle_worker_down(state, down))
    SideEffectFinished(result) ->
      actor.continue(handle_side_effect_finished(state, result))
    GetSnapshot(reply) -> {
      process.send(reply, state.runtime)
      actor.continue(state)
    }
    Shutdown(reply) -> {
      let state = shutdown_state(state)
      log_state(state, "info", "daemon_shutdown", [])
      process.send(reply, Nil)
      actor.stop()
    }
  }
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
  case workflow.parse(contents) {
    Error(err) -> {
      let state =
        State(
          ..state,
          last_contents: contents,
          reload_state: config.ReloadState(
            last_known_good: Some(state.effective),
            current_status: config.CurrentInvalid(error.workflow_code(err)),
          ),
        )
      log_state(state, "warn", "workflow_reload_failed", [
        #("error", error.workflow_code(err)),
      ])
      state
    }
    Ok(definition) ->
      case config.resolve(definition, state.chosen_path) {
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
        Ok(effective) ->
          case config.validate_dispatch(effective) {
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
            Ok(Nil) -> {
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
                  definition: definition,
                  effective: effective,
                  tracker_client: state.dependencies.make_tracker(
                    effective.tracker,
                  ),
                  handoff_client: state.dependencies.make_handoff(
                    effective.tracker,
                    effective.handoff,
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
          }
      }
  }
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
    config.can_dispatch(state.reload_state)
    && state.effective.agent.max_concurrent_agents != 0
    && slots_remain(state)
  {
    False -> schedule_next_poll(state)
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
    False -> {
      let state = case result {
        Error(err) -> {
          log_state(state, "warn", "candidate_fetch_failed", [
            #("error", error.tracker_code(err)),
          ])
          state
        }
        Ok(candidates) -> {
          log_state(state, "info", "candidates_fetched", [
            #("count", int.to_string(list.length(candidates))),
          ])
          dispatch_candidates(core.sort_candidates(candidates), state)
        }
      }
      schedule_next_poll(state)
    }
  }
}

fn poll_result_is_stale(state: State, generation: Int) -> Bool {
  generation != state.poll_generation
  || state.poll_in_flight != Some(generation)
}

fn dispatch_candidates(issues: List(domain.Issue), state: State) -> State {
  case config.can_dispatch(state.reload_state) && slots_remain(state) {
    False -> state
    True ->
      case issues {
        [] -> state
        [issue, ..rest] ->
          case
            core.should_dispatch(state.runtime, state.effective, issue)
            && can_reserve_dispatch_slot(state, issue)
          {
            True -> dispatch_issue_with_continuation(state, issue, rest)
            False -> dispatch_candidates(rest, state)
          }
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
          case config.can_dispatch(state.reload_state) {
            False ->
              defer_retry_for_invalid_workflow(state, issue_id, generation)
            True -> begin_retry_refresh(state, issue_id, generation)
          }
        }
      }
  }
}

fn defer_retry_for_invalid_workflow(
  state: State,
  issue_id: String,
  generation: Int,
) -> State {
  log_state(state, "warn", "retry_deferred_invalid_workflow", [
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
              defer_retry_for_invalid_workflow(state, issue_id, generation)
            True -> {
              let candidate = case result {
                Error(err) -> Error(error.tracker_code(err))
                Ok([issue]) -> Ok(Some(issue))
                Ok(_) -> Ok(None)
              }
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
          }
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
            make_run_id(issue, state.dependencies.now_ms(), session_sequence)
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
  let pid =
    process.spawn_unlinked(fn() {
      let result =
        dependencies.agent_runner(
          issue,
          None,
          definition,
          effective,
          tracker_client,
          fn(_, update) {
            process.send(subject, WorkerUpdate(issue.id, update))
          },
        )
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

fn finish_worker_failure(
  state: State,
  handle: WorkerHandle,
  failure: runner.WorkerFailure,
) -> State {
  log_state(state, "warn", "worker_exited", [
    #("issue_id", handle.issue_id),
    #("run_id", handle.run_id),
    #("reason", "failed"),
  ])
  publish_lifecycle(state, handle.session_id, "worker_exited", Some("failed"))
  hub.finish_session(state.event_hub, handle.session_id, "failed")
  let transition =
    core.apply_worker_failure(
      state.runtime,
      state.effective,
      handle.issue_id,
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

fn handle_worker_down(state: State, down: process.Down) -> State {
  case down {
    process.ProcessDown(monitor, _, _) ->
      case dict.get(state.worker_monitors, monitor) {
        Error(_) -> {
          log_state(state, "warn", "worker_down_stale", [])
          state
        }
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
    tokens: update.tokens,
    raw_json: update.raw_json,
  )
}

fn kind_for_update(update: runner.PiUpdate) -> session_event.EventKind {
  case update.event {
    "probe_started" | "probe_finished" | "pi_session_started" ->
      session_event.Lifecycle
    "turn_finished" -> session_event.TokenStats
    "extension_ui_request" ->
      case is_blocking_ui_method(update.method) {
        True -> session_event.UiRequest
        False -> session_event.Pi
      }
    "extension_ui_response" -> session_event.UiResponse
    "message_update" | "agent_start" | "turn_start" | "turn_end" | "agent_end" ->
      session_event.Pi
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

fn stop_worker(handle: WorkerHandle) -> Nil {
  case handle.command_subject {
    Some(subject) -> process.send(subject, Abort)
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

fn read_workflow_contents(path: String) -> Result(String, StartupError) {
  case simplifile.read(path) {
    Ok(contents) -> Ok(contents)
    Error(_) -> Error(StartupError("missing_workflow_file", "workflow error"))
  }
}

fn map_workflow_error(
  result: Result(a, error.WorkflowError),
) -> Result(a, StartupError) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) ->
      Error(StartupError(error.workflow_code(err), "workflow error"))
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
