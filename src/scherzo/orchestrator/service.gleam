import gleam/dict
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
import scherzo/lifecycle
import scherzo/linear
import scherzo/linear_contract
import scherzo/log
import scherzo/orchestrator/core
import scherzo/orchestrator/daemon
import scherzo/runtime_bundle
import scherzo/signal
import scherzo/smoke
import scherzo/tracker
import scherzo/workflow_dag
import scherzo/workflow_run
import scherzo/workspace
import scherzo/workspace_run

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
    workflow_run_dependencies: workflow_run.Dependencies,
    cleanup: fn(String, String, domain.HooksConfig) ->
      Result(Nil, error.WorkspaceError),
    logger: fn(String) -> Result(Nil, Nil),
    now_ms: fn() -> Int,
  )
}

pub type DaemonLifecycleDependencies {
  DaemonLifecycleDependencies(
    daemon_dependencies: daemon.RuntimeDependencies,
    install_stop_source: fn(process.Subject(lifecycle.StopReason)) ->
      Result(signal.Installation, String),
    shutdown_timeout_ms: Int,
    lifecycle_logger: fn(String, String, List(log.Field)) -> Nil,
  )
}

pub type ContractCheckDependencies {
  ContractCheckDependencies(
    make_contract_client: fn(domain.TrackerConfig) -> linear.ContractClient,
    logger: fn(String, String, List(log.Field), List(String)) ->
      Result(Nil, Nil),
  )
}

pub type ServiceResult {
  ServiceResult(logs: List(String), dispatched: Int, state: domain.RuntimeState)
}

pub fn default_dependencies() -> Dependencies {
  Dependencies(
    tracker: linear.real_client,
    agent_runner: runner.run_attempt,
    workflow_run_dependencies: workflow_run.default_dependencies(),
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
  use bundle <- try_startup(
    runtime_bundle.load(workflow_path)
    |> map_bundle_error,
  )
  let effective = bundle.effective
  let secrets = bundle.secrets
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

pub fn start_linear_contract_check(
  workflow_path: Option(String),
) -> Result(Nil, StartupError) {
  start_linear_contract_check_with_dependencies(
    workflow_path,
    ContractCheckDependencies(
      make_contract_client: linear.real_contract_client,
      logger: log_stderr,
    ),
  )
}

pub fn start_linear_contract_check_with_dependencies(
  workflow_path: Option(String),
  dependencies: ContractCheckDependencies,
) -> Result(Nil, StartupError) {
  use bundle <- try_startup(
    runtime_bundle.load(workflow_path)
    |> map_bundle_error,
  )
  let effective = bundle.effective
  let secrets = bundle.secrets
  let client = dependencies.make_contract_client(effective.tracker)
  use remote <- try_startup(
    client.fetch_remote_contract()
    |> map_tracker_error,
  )
  let diagnostics = linear_contract.check(effective, remote)
  case linear_contract.is_ok(diagnostics) {
    True -> {
      let _ =
        dependencies.logger(
          "info",
          "linear_contract_ok",
          [
            #("project_slug", remote.project_slug),
            #("project_id", remote.project_id),
            #("team_count", int_to_string(list.length(remote.teams))),
            #("state_count", int_to_string(total_state_count(remote.teams))),
            #("label_count", int_to_string(total_label_count(remote))),
          ],
          secrets,
        )
      Ok(Nil)
    }
    False -> {
      let _ =
        dependencies.logger(
          "error",
          "linear_contract_mismatch",
          [#("diagnostic_count", int_to_string(list.length(diagnostics)))],
          secrets,
        )
      log_contract_diagnostics(diagnostics, dependencies, secrets)
      Error(StartupError(
        "linear_contract_mismatch",
        "Linear board contract mismatch",
      ))
    }
  }
}

pub fn start_daemon(workflow_path: Option(String)) -> Result(Nil, StartupError) {
  start_daemon_with_lifecycle(
    workflow_path,
    DaemonLifecycleDependencies(
      daemon_dependencies: daemon_dependencies(),
      install_stop_source: signal.install,
      shutdown_timeout_ms: 10_000,
      lifecycle_logger: fn(level, event, fields) {
        let _ = log_stderr(level, event, fields, [])
        Nil
      },
    ),
  )
}

pub fn start_daemon_with_lifecycle(
  workflow_path: Option(String),
  dependencies: DaemonLifecycleDependencies,
) -> Result(Nil, StartupError) {
  use lock <- try_startup(acquire_lock_for_workflow(workflow_path, True))
  let stop_subject = process.new_subject()
  case dependencies.install_stop_source(stop_subject) {
    Error(message) -> {
      instance_lock.release(lock)
      Error(StartupError("signal_handler_failed", message))
    }
    Ok(installation) -> {
      dependencies.lifecycle_logger("info", "signal_handler_installed", [
        #("signal", "sigterm"),
        #("os_pid", installation.os_pid),
      ])
      case
        daemon.start(workflow_path, dependencies.daemon_dependencies)
        |> map_daemon_error
      {
        Error(err) -> {
          installation.cleanup()
          instance_lock.release(lock)
          Error(err)
        }
        Ok(started) -> {
          let result =
            lifecycle.run_until_stop(
              stop_subject,
              fn(_) {
                daemon.shutdown(started.data, dependencies.shutdown_timeout_ms)
              },
              installation.cleanup,
              fn() { instance_lock.release(lock) },
              dependencies.lifecycle_logger,
            )
          case result {
            lifecycle.ShutdownComplete -> Ok(Nil)
            lifecycle.ShutdownTimedOut ->
              Error(StartupError(
                "daemon_shutdown_timeout",
                "daemon shutdown timed out",
              ))
          }
        }
      }
    }
  }
}

fn total_state_count(teams: List(linear_contract.RemoteTeam)) -> Int {
  list.fold(teams, 0, fn(acc, team) { acc + list.length(team.states) })
}

fn total_label_count(remote: linear_contract.RemoteBoard) -> Int {
  list.fold(remote.teams, list.length(remote.workspace_labels), fn(acc, team) {
    acc + list.length(team.labels)
  })
}

fn log_contract_diagnostics(
  diagnostics: List(linear_contract.ContractDiagnostic),
  dependencies: ContractCheckDependencies,
  secrets: List(String),
) -> Nil {
  case diagnostics {
    [] -> Nil
    [diagnostic, ..rest] -> {
      let _ =
        dependencies.logger(
          "error",
          "linear_contract_diagnostic",
          diagnostic_log_fields(diagnostic),
          secrets,
        )
      log_contract_diagnostics(rest, dependencies, secrets)
    }
  }
}

fn diagnostic_log_fields(
  diagnostic: linear_contract.ContractDiagnostic,
) -> List(log.Field) {
  let code = linear_contract.diagnostic_code(diagnostic)
  case diagnostic {
    linear_contract.MissingState(team_key, name, source) -> [
      #("code", code),
      #("team", team_key),
      #("source", source),
      #("name", name),
    ]
    linear_contract.MissingLabel(team_key, name, source) -> [
      #("code", code),
      #("team", team_key),
      #("source", source),
      #("name", name),
    ]
    linear_contract.MissingHandoffStateId(field, id) -> [
      #("code", code),
      #("field", field),
      #("id", id),
    ]
    linear_contract.MultiTeamHandoffStateUnsupported(field, id, team_keys) -> [
      #("code", code),
      #("field", field),
      #("id", id),
      #("teams", string.join(team_keys, with: ",")),
    ]
    linear_contract.HandoffStateNameMismatch(
      field,
      id,
      expected,
      actual,
      actual_team_key,
    ) -> [
      #("code", code),
      #("field", field),
      #("id", id),
      #("expected", expected),
      #("actual", actual),
      #("actual_team", actual_team_key),
    ]
    linear_contract.MissingInvalidWorkflowStateId(id) -> [
      #("code", code),
      #("id", id),
    ]
    linear_contract.MultiTeamInvalidWorkflowStateUnsupported(id, team_keys) -> [
      #("code", code),
      #("id", id),
      #("teams", string.join(team_keys, with: ",")),
    ]
    linear_contract.InvalidWorkflowStateNameMismatch(
      id,
      expected,
      actual,
      actual_team_key,
    ) -> [
      #("code", code),
      #("id", id),
      #("expected", expected),
      #("actual", actual),
      #("actual_team", actual_team_key),
    ]
  }
}

pub fn start_pi_probe(
  workflow_path: Option(String),
) -> Result(Nil, StartupError) {
  use bundle <- try_startup(
    runtime_bundle.load(workflow_path)
    |> map_bundle_error,
  )
  case bundle.mode {
    runtime_bundle.LegacyMarkdown -> {
      use _ <- try_startup(
        config.validate_dispatch(bundle.effective) |> map_config_error,
      )
      use lock <- try_startup(acquire_lock(bundle.effective.workspace.root))
      let probe_result = run_pi_probe(bundle.effective)
      instance_lock.release(lock)
      probe_result
    }
    runtime_bundle.OrchestratorYaml -> {
      use lock <- try_startup(acquire_lock(bundle.effective.workspace.root))
      let probe_result = case bundle.orchestrator {
        Some(orchestrator) ->
          run_pi_probe_orchestrator(orchestrator, bundle.secrets)
        None ->
          Error(StartupError(
            "missing_orchestrator_config",
            "missing orchestrator config",
          ))
      }
      instance_lock.release(lock)
      probe_result
    }
  }
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
  use bundle <- try_startup(
    runtime_bundle.load(workflow_path)
    |> map_bundle_error,
  )
  case require_dispatch, bundle.mode {
    True, runtime_bundle.LegacyMarkdown -> {
      use _ <- try_startup(
        config.validate_dispatch(bundle.effective) |> map_config_error,
      )
      acquire_lock(bundle.effective.workspace.root)
    }
    _, _ -> acquire_lock(bundle.effective.workspace.root)
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

fn run_pi_probe_orchestrator(
  orchestrator: domain.OrchestratorConfig,
  secrets: List(String),
) -> Result(Nil, StartupError) {
  let issue =
    domain.Issue(
      id: "SCHERZO-PROBE",
      identifier: "SCHERZO-PROBE",
      title: "Scherzo probe",
      description: None,
      priority: None,
      state: "",
      branch_name: None,
      url: None,
      labels: [],
      blocked_by: [],
      created_at: None,
      updated_at: None,
    )
  case
    workspace_run.prepare_step(
      issue,
      "probe",
      "probe",
      "probe",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      dict.new(),
    )
  {
    Error(workspace_run.WorkspaceFailure(err)) ->
      Error(StartupError(error.workspace_code(err), "workspace error"))
    Error(workspace_run.HookFailure(err)) ->
      Error(StartupError(error.hook_code(err), "hook error"))
    Ok(prepared) -> {
      let probe_result =
        probe.probe(
          orchestrator.effective.pi.command,
          prepared.path,
          orchestrator.effective.pi.read_timeout_ms,
        )
      let _ = workspace_run.cleanup_run(prepared.run_root, orchestrator)
      case probe_result {
        Ok(Nil) -> {
          let _ =
            log_stderr(
              "info",
              "pi_probe_ok",
              [#("workspace_path", prepared.path)],
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
  use bundle <- try_startup(
    runtime_bundle.load(workflow_path)
    |> map_bundle_error,
  )
  case bundle.mode {
    runtime_bundle.LegacyMarkdown -> {
      use _ <- try_startup(
        config.validate_dispatch(bundle.effective) |> map_config_error,
      )
      run_once_loaded(bundle.config_path, bundle, dependencies)
    }
    runtime_bundle.OrchestratorYaml ->
      run_once_loaded(bundle.config_path, bundle, dependencies)
  }
}

fn run_once_loaded(
  path: String,
  bundle: runtime_bundle.RuntimeBundle,
  dependencies: Dependencies,
) -> Result(ServiceResult, StartupError) {
  let effective = bundle.effective
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
      case bundle.mode, bundle.legacy_workflow {
        runtime_bundle.LegacyMarkdown, Some(definition) ->
          run_tick(
            definition,
            effective,
            initial_state,
            tracker_client,
            dependencies,
            logs,
          )
        runtime_bundle.LegacyMarkdown, None ->
          Error(StartupError("missing_workflow", "legacy workflow is missing"))
        runtime_bundle.OrchestratorYaml, _ ->
          run_tick_yaml(
            bundle,
            initial_state,
            tracker_client,
            dependencies,
            logs,
          )
      }
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

fn run_tick_yaml(
  bundle: runtime_bundle.RuntimeBundle,
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
      dispatch_candidates_yaml(
        core.sort_candidates(candidates),
        bundle,
        state,
        tracker_client,
        dependencies,
        [fetched, tick_log, ..logs],
        0,
      )
    }
  }
}

fn dispatch_candidates_yaml(
  candidates: List(domain.Issue),
  bundle: runtime_bundle.RuntimeBundle,
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
      case core.should_dispatch(state, bundle.effective, issue) {
        False ->
          dispatch_candidates_yaml(
            rest,
            bundle,
            state,
            tracker_client,
            dependencies,
            logs,
            dispatched,
          )
        True ->
          case runtime_bundle.select_workflow(bundle, issue) {
            Error(runtime_bundle.BundleError(code, _)) -> {
              let skipped =
                log.warn("workflow_route_failed", [
                  #("issue_id", issue.id),
                  #("issue_identifier", issue.identifier),
                  #("error", code),
                ])
              let _ = dependencies.logger(skipped)
              dispatch_candidates_yaml(
                rest,
                bundle,
                state,
                tracker_client,
                dependencies,
                [skipped, ..logs],
                dispatched,
              )
            }
            Ok(#(workflow_id, dag)) ->
              dispatch_yaml_issue(
                rest,
                issue,
                workflow_id,
                dag,
                bundle,
                state,
                tracker_client,
                dependencies,
                logs,
                dispatched,
              )
          }
      }
    }
  }
}

fn dispatch_yaml_issue(
  remaining: List(domain.Issue),
  issue: domain.Issue,
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
  bundle: runtime_bundle.RuntimeBundle,
  state: domain.RuntimeState,
  tracker_client: tracker.Client,
  dependencies: Dependencies,
  logs: List(String),
  dispatched: Int,
) -> Result(ServiceResult, StartupError) {
  case bundle.orchestrator {
    None ->
      Error(StartupError(
        "missing_orchestrator_config",
        "missing orchestrator config",
      ))
    Some(orchestrator) -> {
      let started =
        log.info("dispatch_started", [
          #("issue_id", issue.id),
          #("issue_identifier", issue.identifier),
          #("workflow_id", workflow_id),
        ])
      let _ = dependencies.logger(started)
      let state = core.apply_worker_start(state, issue, "")
      let run_id = issue.identifier <> "-once"
      case
        workflow_run.execute(
          issue,
          dag,
          orchestrator,
          tracker_client,
          bundle.secrets,
          run_id,
          dependencies.workflow_run_dependencies,
        )
      {
        Ok(success) -> {
          let exited =
            log.info("worker_exited", [
              #("issue_id", issue.id),
              #("issue_identifier", issue.identifier),
              #("reason", "normal"),
              #("workspace_path", success.run_root),
            ])
          let cleaned =
            log.info("workspace_cleaned", [
              #("workspace_path", success.run_root),
            ])
          let _ = dependencies.logger(exited)
          let _ = dependencies.logger(cleaned)
          let final_issue = case success.worker_success.final_issue {
            Some(i) -> i
            None -> issue
          }
          let state =
            apply_dag_success_state(
              state,
              issue.id,
              final_issue,
              success.worker_success.tokens,
            )
          dispatch_candidates_yaml(
            remaining,
            bundle,
            state,
            tracker_client,
            dependencies,
            [cleaned, exited, started, ..logs],
            dispatched + 1,
          )
        }
        Error(failure) -> {
          let failed =
            log.warn("worker_exited", [
              #("issue_id", issue.id),
              #("issue_identifier", issue.identifier),
              #("reason", "failed"),
              #("error", failure.reason),
            ])
          let _ = dependencies.logger(failed)
          let state = apply_dag_failure_state(state, issue.id)
          dispatch_candidates_yaml(
            remaining,
            bundle,
            state,
            tracker_client,
            dependencies,
            [failed, started, ..logs],
            dispatched + 1,
          )
        }
      }
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

fn apply_dag_success_state(
  state: domain.RuntimeState,
  issue_id: String,
  final_issue: domain.Issue,
  tokens: domain.TokenTotals,
) -> domain.RuntimeState {
  domain.RuntimeState(
    ..state,
    running: dict.delete(state.running, issue_id),
    claimed: dict.delete(state.claimed, issue_id),
    completed: dict.insert(state.completed, issue_id, final_issue),
    aggregate_pi_totals: add_tokens(state.aggregate_pi_totals, tokens),
  )
}

fn apply_dag_failure_state(
  state: domain.RuntimeState,
  issue_id: String,
) -> domain.RuntimeState {
  domain.RuntimeState(
    ..state,
    running: dict.delete(state.running, issue_id),
    claimed: dict.delete(state.claimed, issue_id),
  )
}

fn add_tokens(
  left: domain.TokenTotals,
  right: domain.TokenTotals,
) -> domain.TokenTotals {
  domain.TokenTotals(
    input: left.input + right.input,
    output: left.output + right.output,
    cache_read: left.cache_read + right.cache_read,
    cache_write: left.cache_write + right.cache_write,
    total: left.total + right.total,
  )
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
