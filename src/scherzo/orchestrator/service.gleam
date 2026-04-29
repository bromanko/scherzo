import gleam/erlang/process
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/probe
import scherzo/agent/runner
import scherzo/config
import scherzo/domain
import scherzo/error
import scherzo/instance_lock
import scherzo/linear
import scherzo/log
import scherzo/orchestrator/core
import scherzo/orchestrator/daemon
import scherzo/smoke
import scherzo/tracker
import scherzo/workflow
import scherzo/workspace

pub type StartupError {
  StartupError(code: String, message: String)
}

pub type Dependencies {
  Dependencies(
    tracker: fn(domain.TrackerConfig) -> tracker.Client,
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
    logger: fn(String) -> Result(Nil, Nil),
    now_ms: fn() -> Int,
  )
}

pub type ServiceResult {
  ServiceResult(logs: List(String), dispatched: Int, state: domain.RuntimeState)
}

pub fn default_dependencies() -> Dependencies {
  Dependencies(
    tracker: linear.real_client,
    agent_runner: runner.run_attempt,
    cleanup: workspace.cleanup_stored_path,
    logger: fn(line) {
      io.println_error(line)
      Ok(Nil)
    },
    now_ms: monotonic_ms,
  )
}

fn daemon_dependencies() -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    ..daemon.default_dependencies(),
    logger: log_stderr,
  )
}

pub fn start(workflow_path: Option(String)) -> Result(Nil, StartupError) {
  start_daemon(workflow_path)
}

pub fn start_once(workflow_path: Option(String)) -> Result(Nil, StartupError) {
  use lock <- try_startup(acquire_lock_for_workflow(workflow_path, True))
  let result = start_with_dependencies(workflow_path, default_dependencies())
  instance_lock.release(lock)
  result
}

pub fn start_with_dependencies(
  workflow_path: Option(String),
  dependencies: Dependencies,
) -> Result(Nil, StartupError) {
  use _ <- try_startup(run_once_with_dependencies(workflow_path, dependencies))
  Ok(Nil)
}

pub fn start_linear_smoke(
  workflow_path: Option(String),
) -> Result(Nil, StartupError) {
  let path = workflow.choose_path(workflow_path)
  use definition <- try_startup(
    workflow.load(workflow_path)
    |> map_workflow_error,
  )
  use effective <- try_startup(
    config.resolve(definition, path)
    |> map_config_error,
  )
  let secrets = config.resolved_secrets(effective)
  let reader = smoke.real_linear_reader(effective.tracker)
  use result <- try_startup(
    smoke.linear_read_smoke(reader, effective.tracker.terminal_states)
    |> map_tracker_error,
  )
  let _ =
    log_stderr(
      "info",
      "linear_smoke_ok",
      [
        #("candidate_count", int_to_string(result.candidate_count)),
        #("terminal_count", int_to_string(result.terminal_count)),
        #("refreshed_count", int_to_string(result.refreshed_count)),
      ],
      secrets,
    )
  Ok(Nil)
}

pub fn start_daemon(workflow_path: Option(String)) -> Result(Nil, StartupError) {
  use lock <- try_startup(acquire_lock_for_workflow(workflow_path, True))
  case daemon.start(workflow_path, daemon_dependencies()) |> map_daemon_error {
    Error(err) -> {
      instance_lock.release(lock)
      Error(err)
    }
    Ok(_) -> {
      process.sleep_forever()
      instance_lock.release(lock)
      Ok(Nil)
    }
  }
}

pub fn start_pi_probe(
  workflow_path: Option(String),
) -> Result(Nil, StartupError) {
  let path = workflow.choose_path(workflow_path)
  use definition <- try_startup(
    workflow.load(workflow_path)
    |> map_workflow_error,
  )
  use effective <- try_startup(
    config.resolve(definition, path)
    |> map_config_error,
  )
  use _ <- try_startup(config.validate_dispatch(effective) |> map_config_error)
  use lock <- try_startup(acquire_lock(effective.workspace.root))
  let probe_result = run_pi_probe(effective)
  instance_lock.release(lock)
  probe_result
}

pub fn log_stderr(
  level: String,
  event: String,
  fields: List(log.Field),
  secrets: List(String),
) -> Result(Nil, Nil) {
  io.println_error(log.format(level, event, fields, secrets))
  Ok(Nil)
}

fn acquire_lock_for_workflow(
  workflow_path: Option(String),
  require_dispatch: Bool,
) -> Result(instance_lock.Lock, StartupError) {
  let path = workflow.choose_path(workflow_path)
  use definition <- try_startup(
    workflow.load(workflow_path)
    |> map_workflow_error,
  )
  use effective <- try_startup(
    config.resolve(definition, path)
    |> map_config_error,
  )
  case require_dispatch {
    True -> {
      use _ <- try_startup(
        config.validate_dispatch(effective) |> map_config_error,
      )
      acquire_lock(effective.workspace.root)
    }
    False -> acquire_lock(effective.workspace.root)
  }
}

fn acquire_lock(
  workspace_root: String,
) -> Result(instance_lock.Lock, StartupError) {
  instance_lock.acquire(workspace_root) |> map_lock_error
}

fn run_pi_probe(effective: domain.EffectiveConfig) -> Result(Nil, StartupError) {
  let secrets = config.resolved_secrets(effective)
  case
    workspace.prepare("SCHERZO-PROBE", effective.workspace, effective.hooks)
  {
    Error(workspace.WorkspaceFailure(err)) ->
      Error(StartupError(error.workspace_code(err), "workspace error"))
    Error(workspace.HookFailure(err)) ->
      Error(StartupError(error.hook_code(err), "hook error"))
    Ok(prepared) -> {
      let probe_result =
        probe.probe(
          effective.pi.command,
          prepared.path,
          effective.pi.read_timeout_ms,
        )
      cleanup_probe_workspace(effective, prepared.path, secrets)
      case probe_result {
        Ok(Nil) -> {
          let _ =
            log_stderr(
              "info",
              "pi_probe_ok",
              [
                #("workspace_path", prepared.path),
              ],
              secrets,
            )
          Ok(Nil)
        }
        Error(err) ->
          Error(StartupError(error.pi_rpc_code(err), "pi probe error"))
      }
    }
  }
}

fn cleanup_probe_workspace(
  effective: domain.EffectiveConfig,
  workspace_path: String,
  secrets: List(String),
) -> Nil {
  case
    workspace.cleanup_stored_path(
      effective.workspace.root,
      workspace_path,
      effective.hooks,
    )
  {
    Ok(Nil) -> Nil
    Error(err) -> {
      let _ =
        log_stderr(
          "warn",
          "pi_probe_cleanup_failed",
          [
            #("workspace_path", workspace_path),
            #("error", error.workspace_code(err)),
          ],
          secrets,
        )
      Nil
    }
  }
}

pub fn run_once_with_dependencies(
  workflow_path: Option(String),
  dependencies: Dependencies,
) -> Result(ServiceResult, StartupError) {
  let path = workflow.choose_path(workflow_path)
  use definition <- try_startup(
    workflow.load(workflow_path)
    |> map_workflow_error,
  )
  use effective <- try_startup(
    config.resolve(definition, path)
    |> map_config_error,
  )
  use _ <- try_startup(config.validate_dispatch(effective) |> map_config_error)
  let tracker_client = dependencies.tracker(effective.tracker)
  let initial_state = core.new_state(effective)
  let logs = [
    log.info("workflow_loaded", [
      #("workflow_path", path),
      #("polling_interval_ms", int_to_string(effective.polling.interval_ms)),
      #(
        "max_concurrent_agents",
        int_to_string(effective.agent.max_concurrent_agents),
      ),
    ]),
  ]
  list.each(logs, fn(line) {
    let _ = dependencies.logger(line)
  })
  case effective.agent.max_concurrent_agents == 0 {
    True ->
      Ok(ServiceResult(
        logs: [log.info("dispatch_paused", []), ..logs],
        dispatched: 0,
        state: initial_state,
      ))
    False ->
      run_tick(
        definition,
        effective,
        initial_state,
        tracker_client,
        dependencies,
        logs,
      )
  }
}

fn run_tick(
  definition: domain.WorkflowDefinition,
  effective: domain.EffectiveConfig,
  state: domain.RuntimeState,
  tracker_client: tracker.Client,
  dependencies: Dependencies,
  logs: List(String),
) -> Result(ServiceResult, StartupError) {
  let tick_log = log.info("tick_started", [])
  let _ = dependencies.logger(tick_log)
  case tracker_client.fetch_candidate_issues() {
    Error(err) -> {
      let line =
        log.warn("candidate_fetch_failed", [#("error", error.tracker_code(err))])
      let _ = dependencies.logger(line)
      Ok(ServiceResult(
        logs: [line, tick_log, ..logs],
        dispatched: 0,
        state: state,
      ))
    }
    Ok(candidates) -> {
      let fetched =
        log.info("candidates_fetched", [
          #("count", int_to_string(list.length(candidates))),
        ])
      let _ = dependencies.logger(fetched)
      dispatch_candidates(
        core.sort_candidates(candidates),
        definition,
        effective,
        state,
        tracker_client,
        dependencies,
        [fetched, tick_log, ..logs],
        0,
      )
    }
  }
}

fn dispatch_candidates(
  candidates: List(domain.Issue),
  definition: domain.WorkflowDefinition,
  effective: domain.EffectiveConfig,
  state: domain.RuntimeState,
  tracker_client: tracker.Client,
  dependencies: Dependencies,
  logs: List(String),
  dispatched: Int,
) -> Result(ServiceResult, StartupError) {
  case candidates {
    [] -> Ok(ServiceResult(logs: logs, dispatched: dispatched, state: state))
    [issue, ..rest] -> {
      let state = core.unpark_if_issue_changed(state, issue)
      case core.should_dispatch(state, effective, issue) {
        False ->
          dispatch_candidates(
            rest,
            definition,
            effective,
            state,
            tracker_client,
            dependencies,
            logs,
            dispatched,
          )
        True -> {
          let started =
            log.info("dispatch_started", [
              #("issue_id", issue.id),
              #("issue_identifier", issue.identifier),
            ])
          let _ = dependencies.logger(started)
          let state = core.apply_worker_start(state, issue, "")
          case
            dependencies.agent_runner(
              issue,
              None,
              definition,
              effective,
              tracker_client,
              fn(_, _) { Nil },
            )
          {
            Ok(success) -> {
              let exited =
                log.info("worker_exited", [
                  #("issue_id", issue.id),
                  #("issue_identifier", issue.identifier),
                  #("reason", "normal"),
                  #("workspace_path", success.workspace_path),
                ])
              let _ = dependencies.logger(exited)
              let final_issue = case success.final_issue {
                Some(i) -> i
                None -> issue
              }
              let transition =
                core.apply_worker_success_with_workspace_path(
                  state,
                  effective,
                  issue.id,
                  final_issue,
                  success.workspace_path,
                  success.tokens,
                  dependencies.now_ms(),
                )
              let logs =
                interpret_effects(transition.effects, effective, dependencies, [
                  exited,
                  started,
                  ..logs
                ])
              dispatch_candidates(
                rest,
                definition,
                effective,
                transition.state,
                tracker_client,
                dependencies,
                logs,
                dispatched + 1,
              )
            }
            Error(failure) -> {
              let failed =
                log.warn("worker_exited", [
                  #("issue_id", issue.id),
                  #("issue_identifier", issue.identifier),
                  #("reason", "failed"),
                ])
              let _ = dependencies.logger(failed)
              let baseline_issue = case failure.final_issue {
                Some(final_issue) ->
                  case final_issue.id == issue.id {
                    True -> final_issue
                    False -> issue
                  }
                None -> issue
              }
              let transition =
                core.apply_worker_failure(
                  state,
                  effective,
                  issue.id,
                  baseline_issue,
                  dependencies.now_ms(),
                )
              let logs =
                interpret_effects(transition.effects, effective, dependencies, [
                  failed,
                  started,
                  ..logs
                ])
              dispatch_candidates(
                rest,
                definition,
                effective,
                transition.state,
                tracker_client,
                dependencies,
                logs,
                dispatched + 1,
              )
            }
          }
        }
      }
    }
  }
}

fn interpret_effects(
  effects: List(core.Effect),
  effective: domain.EffectiveConfig,
  dependencies: Dependencies,
  logs: List(String),
) -> List(String) {
  case effects {
    [] -> logs
    [effect, ..rest] -> {
      let line = interpret_effect(effect, effective, dependencies)
      let _ = dependencies.logger(line)
      interpret_effects(rest, effective, dependencies, [line, ..logs])
    }
  }
}

fn interpret_effect(
  effect: core.Effect,
  effective: domain.EffectiveConfig,
  dependencies: Dependencies,
) -> String {
  case effect {
    core.CleanupWorkspace(workspace_path) ->
      case string.trim(workspace_path) == "" {
        True ->
          log.warn("workspace_cleanup_skipped", [#("reason", "empty_path")])
        False ->
          case
            dependencies.cleanup(
              effective.workspace.root,
              workspace_path,
              effective.hooks,
            )
          {
            Ok(Nil) ->
              log.info("workspace_cleaned", [
                #("workspace_path", workspace_path),
              ])
            Error(err) ->
              log.warn("workspace_cleanup_failed", [
                #("workspace_path", workspace_path),
                #("error", error.workspace_code(err)),
              ])
          }
      }
    core.Dispatch(issue) ->
      log.info("dispatch_effect", [
        #("issue_id", issue.id),
        #("issue_identifier", issue.identifier),
      ])
    core.ScheduleRetry(issue_id, delay_ms, generation, reason) ->
      log.info("retry_scheduled", [
        #("issue_id", issue_id),
        #("delay_ms", int_to_string(delay_ms)),
        #("generation", int_to_string(generation)),
        #("reason", reason),
      ])
    core.CancelRetry(issue_id) ->
      log.info("retry_cancelled", [#("issue_id", issue_id)])
    core.ReleaseClaim(issue_id) ->
      log.info("claim_released", [#("issue_id", issue_id)])
    core.StopWorker(issue_id, reason) ->
      log.warn("worker_stop_requested", [
        #("issue_id", issue_id),
        #("reason", reason),
      ])
    core.ParkIssue(issue_id, reason) ->
      log.warn("issue_parked", [#("issue_id", issue_id), #("reason", reason)])
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

fn map_tracker_error(
  result: Result(a, error.TrackerError),
) -> Result(a, StartupError) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) -> Error(StartupError(error.tracker_code(err), "tracker error"))
  }
}

fn map_lock_error(
  result: Result(a, instance_lock.LockError),
) -> Result(a, StartupError) {
  case result {
    Ok(value) -> Ok(value)
    Error(instance_lock.LockAlreadyHeld(message)) ->
      Error(StartupError("instance_lock_held", message))
    Error(instance_lock.LockIo(message)) ->
      Error(StartupError("instance_lock_io", message))
  }
}

fn map_daemon_error(
  result: Result(a, daemon.StartupError),
) -> Result(a, StartupError) {
  case result {
    Ok(value) -> Ok(value)
    Error(daemon.StartupError(code, message)) ->
      Error(StartupError(code, message))
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

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String

@external(erlang, "scherzo_time_ffi", "monotonic_ms")
fn monotonic_ms() -> Int
