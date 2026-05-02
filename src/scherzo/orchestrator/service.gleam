import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/probe
import scherzo/doctor
import scherzo/domain
import scherzo/error
import scherzo/instance_lock
import scherzo/lifecycle
import scherzo/linear
import scherzo/linear_attachment
import scherzo/linear_contract
import scherzo/log
import scherzo/orchestrator/core
import scherzo/orchestrator/daemon
import scherzo/orchestrator/reason as orchestrator_reason
import scherzo/runtime_bundle
import scherzo/signal
import scherzo/smoke
import scherzo/tracker
import scherzo/tracker/state as issue_state
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

pub type DoctorLock {
  DoctorLock(release: fn() -> Nil)
}

pub type DoctorDependencies {
  DoctorDependencies(
    load_bundle: fn(Option(String)) ->
      Result(runtime_bundle.RuntimeBundle, runtime_bundle.BundleError),
    make_linear_smoke_reader: fn(domain.TrackerConfig) ->
      smoke.LinearSmokeReader,
    make_contract_client: fn(domain.TrackerConfig) -> linear.ContractClient,
    acquire_lock: fn(String) -> Result(DoctorLock, instance_lock.LockError),
    prepare_step: fn(
      domain.Issue,
      String,
      String,
      String,
      workflow_dag.WorkspaceRef,
      domain.OrchestratorConfig,
      Dict(String, workspace_run.PreparedStepWorkspace),
    ) ->
      Result(workspace_run.PreparedStepWorkspace, workspace_run.PrepareError),
    cleanup_run: fn(String, domain.OrchestratorConfig) ->
      Result(Nil, error.WorkspaceError),
    pi_probe: fn(String, String, Int) -> Result(Nil, error.PiRpcError),
    logger: fn(String, String, List(log.Field), List(String)) ->
      Result(Nil, Nil),
    list_writer: fn(String) -> Result(Nil, Nil),
  )
}

pub type ServiceResult {
  ServiceResult(logs: List(String), dispatched: Int, state: domain.RuntimeState)
}

pub fn default_dependencies() -> Dependencies {
  Dependencies(
    tracker: linear.real_client,
    workflow_run_dependencies: workflow_run.default_dependencies(),
    cleanup: workspace.cleanup_stored_path,
    logger: fn(line) {
      io.println_error(line)
      Ok(Nil)
    },
    now_ms: monotonic_ms,
  )
}

pub fn default_doctor_dependencies() -> DoctorDependencies {
  DoctorDependencies(
    load_bundle: runtime_bundle.load,
    make_linear_smoke_reader: smoke.real_linear_reader,
    make_contract_client: linear.real_contract_client,
    acquire_lock: acquire_doctor_lock,
    prepare_step: workspace_run.prepare_step,
    cleanup_run: workspace_run.cleanup_run,
    pi_probe: probe.probe,
    logger: log_stderr,
    list_writer: fn(line) {
      io.println(line)
      Ok(Nil)
    },
  )
}

fn acquire_doctor_lock(
  workspace_root: String,
) -> Result(DoctorLock, instance_lock.LockError) {
  case instance_lock.acquire(workspace_root) {
    Error(err) -> Error(err)
    Ok(lock) -> Ok(DoctorLock(release: fn() { instance_lock.release(lock) }))
  }
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

pub fn start_linear_attach_comment_file(
  workflow_path: Option(String),
  comment_id: String,
  file_path: String,
) -> Result(Nil, StartupError) {
  use bundle <- try_startup(
    runtime_bundle.load(workflow_path)
    |> map_bundle_error,
  )
  use outcome <- try_startup(
    linear_attachment.attach_markdown_file_to_comment(
      bundle.effective.tracker,
      comment_id,
      file_path,
      linear_attachment.AttachOptions(
        fallback_to_markdown_link: True,
        dedupe_by_filename: False,
      ),
      linear_attachment.real_dependencies(linear.http_transport),
    )
    |> map_attachment_tracker_error,
  )
  let _ =
    log_stderr(
      "info",
      "linear_comment_attachment_ok",
      attachment_log_fields(outcome),
      bundle.secrets,
    )
  Ok(Nil)
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

pub fn start_doctor(options: doctor.Options) -> Result(Nil, StartupError) {
  start_doctor_with_dependencies(options, default_doctor_dependencies())
}

pub fn start_doctor_with_dependencies(
  options: doctor.Options,
  dependencies: DoctorDependencies,
) -> Result(Nil, StartupError) {
  case options.list_checks {
    True -> {
      list.each(doctor.list_check_names(), fn(name) {
        let _ = dependencies.list_writer(name)
        Nil
      })
      Ok(Nil)
    }
    False -> {
      use report_and_secrets <- try_startup(build_doctor_report_and_secrets(
        options,
        dependencies,
      ))
      let #(report, secrets) = report_and_secrets
      write_doctor_report(report, options, dependencies, secrets)
      case doctor.has_failures(report) {
        True ->
          Error(StartupError(
            "doctor_failed",
            "one or more doctor checks failed",
          ))
        False -> Ok(Nil)
      }
    }
  }
}

pub fn build_doctor_report_with_dependencies(
  options: doctor.Options,
  dependencies: DoctorDependencies,
) -> Result(doctor.Report, StartupError) {
  use report_and_secrets <- try_startup(build_doctor_report_and_secrets(
    options,
    dependencies,
  ))
  let #(report, _secrets) = report_and_secrets
  Ok(report)
}

fn build_doctor_report_and_secrets(
  options: doctor.Options,
  dependencies: DoctorDependencies,
) -> Result(#(doctor.Report, List(String)), StartupError) {
  use selected <- try_startup(resolve_doctor_checks(options.checks))
  case dependencies.load_bundle(options.path) {
    Error(err) -> Ok(#(doctor_bundle_failure_report(selected, err), []))
    Ok(bundle) ->
      Ok(#(
        run_loaded_doctor_checks(selected, bundle, dependencies),
        bundle.secrets,
      ))
  }
}

fn resolve_doctor_checks(
  raw_checks: List(String),
) -> Result(List(doctor.CheckName), StartupError) {
  case doctor.selected_checks(raw_checks) {
    Ok(checks) -> Ok(checks)
    Error(name) ->
      Error(StartupError(
        "unknown_doctor_check",
        "unknown doctor check: " <> name,
      ))
  }
}

fn doctor_bundle_failure_report(
  selected: List(doctor.CheckName),
  error: runtime_bundle.BundleError,
) -> doctor.Report {
  let runtime_bundle.BundleError(code, message) = error
  let results = [
    doctor.CheckResult(
      check: doctor.WorkflowConfig,
      status: doctor.Fail,
      code: code,
      message: message,
      fields: [],
    ),
  ]
  doctor.Report(skip_after_workflow_failure(
    doctor.canonical_checks(selected),
    results,
  ))
}

fn skip_after_workflow_failure(
  checks: List(doctor.CheckName),
  results: List(doctor.CheckResult),
) -> List(doctor.CheckResult) {
  case checks {
    [] -> results
    [doctor.WorkflowConfig, ..rest] ->
      skip_after_workflow_failure(rest, results)
    [check, ..rest] ->
      skip_after_workflow_failure(
        rest,
        list.append(results, [
          doctor.CheckResult(
            check: check,
            status: doctor.Skip,
            code: "workflow_config_failed",
            message: "workflow config did not load",
            fields: [],
          ),
        ]),
      )
  }
}

fn run_loaded_doctor_checks(
  selected: List(doctor.CheckName),
  bundle: runtime_bundle.RuntimeBundle,
  dependencies: DoctorDependencies,
) -> doctor.Report {
  []
  |> maybe_workflow_config_result(selected, bundle)
  |> maybe_linear_contract_result(selected, bundle, dependencies)
  |> maybe_linear_smoke_result(selected, bundle, dependencies)
  |> maybe_local_probe_results(selected, bundle, dependencies)
  |> doctor.Report
}

fn maybe_workflow_config_result(
  results: List(doctor.CheckResult),
  selected: List(doctor.CheckName),
  bundle: runtime_bundle.RuntimeBundle,
) -> List(doctor.CheckResult) {
  case doctor.contains_check(selected, doctor.WorkflowConfig) {
    False -> results
    True ->
      list.append(results, [
        doctor.CheckResult(
          check: doctor.WorkflowConfig,
          status: doctor.Pass,
          code: "ok",
          message: "YAML orchestrator config and workflow DAGs are valid",
          fields: [
            #("config_path", bundle.config_path),
            #(
              "workflow_count",
              int_to_string(list.length(dict.to_list(bundle.workflows))),
            ),
          ],
        ),
      ])
  }
}

fn maybe_linear_contract_result(
  results: List(doctor.CheckResult),
  selected: List(doctor.CheckName),
  bundle: runtime_bundle.RuntimeBundle,
  dependencies: DoctorDependencies,
) -> List(doctor.CheckResult) {
  case doctor.contains_check(selected, doctor.LinearContract) {
    False -> results
    True ->
      list.append(results, [
        run_linear_contract_doctor_check(bundle, dependencies),
      ])
  }
}

fn run_linear_contract_doctor_check(
  bundle: runtime_bundle.RuntimeBundle,
  dependencies: DoctorDependencies,
) -> doctor.CheckResult {
  let client = dependencies.make_contract_client(bundle.effective.tracker)
  case client.fetch_remote_contract() {
    Error(err) ->
      doctor.CheckResult(
        check: doctor.LinearContract,
        status: doctor.Fail,
        code: error.tracker_code(err),
        message: "Linear contract fetch failed",
        fields: [],
      )
    Ok(remote) -> {
      let diagnostics = linear_contract.check(bundle.effective, remote)
      case linear_contract.is_ok(diagnostics) {
        True ->
          doctor.CheckResult(
            check: doctor.LinearContract,
            status: doctor.Pass,
            code: "ok",
            message: "Linear board contract matches configured states and labels",
            fields: [
              #("project_slug", remote.project_slug),
              #("project_id", remote.project_id),
              #("team_count", int_to_string(list.length(remote.teams))),
              #("state_count", int_to_string(total_state_count(remote.teams))),
              #("label_count", int_to_string(total_label_count(remote))),
            ],
          )
        False ->
          doctor.CheckResult(
            check: doctor.LinearContract,
            status: doctor.Fail,
            code: "linear_contract_mismatch",
            message: "Linear board contract mismatch",
            fields: contract_mismatch_fields(diagnostics),
          )
      }
    }
  }
}

fn contract_mismatch_fields(
  diagnostics: List(linear_contract.ContractDiagnostic),
) -> List(log.Field) {
  let base = [#("diagnostic_count", int_to_string(list.length(diagnostics)))]
  case diagnostics {
    [] -> base
    [first, ..] ->
      list.append(base, [
        #("first_diagnostic_code", linear_contract.diagnostic_code(first)),
        #("first_diagnostic_message", linear_contract.diagnostic_message(first)),
      ])
  }
}

fn maybe_linear_smoke_result(
  results: List(doctor.CheckResult),
  selected: List(doctor.CheckName),
  bundle: runtime_bundle.RuntimeBundle,
  dependencies: DoctorDependencies,
) -> List(doctor.CheckResult) {
  case doctor.contains_check(selected, doctor.LinearSmoke) {
    False -> results
    True ->
      list.append(results, [run_linear_smoke_doctor_check(bundle, dependencies)])
  }
}

fn run_linear_smoke_doctor_check(
  bundle: runtime_bundle.RuntimeBundle,
  dependencies: DoctorDependencies,
) -> doctor.CheckResult {
  let reader = dependencies.make_linear_smoke_reader(bundle.effective.tracker)
  case
    smoke.linear_read_smoke(reader, bundle.effective.tracker.terminal_states)
  {
    Error(err) ->
      doctor.CheckResult(
        check: doctor.LinearSmoke,
        status: doctor.Fail,
        code: error.tracker_code(err),
        message: "Linear read smoke failed",
        fields: [],
      )
    Ok(result) ->
      doctor.CheckResult(
        check: doctor.LinearSmoke,
        status: doctor.Pass,
        code: "ok",
        message: "Linear read smoke succeeded",
        fields: [
          #("candidate_count", int_to_string(result.candidate_count)),
          #("terminal_count", int_to_string(result.terminal_count)),
          #("refreshed_count", int_to_string(result.refreshed_count)),
        ],
      )
  }
}

fn maybe_local_probe_results(
  results: List(doctor.CheckResult),
  selected: List(doctor.CheckName),
  bundle: runtime_bundle.RuntimeBundle,
  dependencies: DoctorDependencies,
) -> List(doctor.CheckResult) {
  case needs_doctor_lock(selected) {
    False -> results
    True ->
      case dependencies.acquire_lock(bundle.effective.workspace.root) {
        Error(err) -> local_probe_lock_failure_results(results, selected, err)
        Ok(lock) -> {
          let results =
            results
            |> append_instance_lock_pass(selected, bundle)
            |> run_workspace_and_pi_checks(selected, bundle, dependencies)
          lock.release()
          results
        }
      }
  }
}

fn needs_doctor_lock(selected: List(doctor.CheckName)) -> Bool {
  doctor.contains_check(selected, doctor.InstanceLock)
  || doctor.contains_check(selected, doctor.WorkspaceHooks)
  || doctor.contains_check(selected, doctor.PiProbe)
}

fn local_probe_lock_failure_results(
  results: List(doctor.CheckResult),
  selected: List(doctor.CheckName),
  err: instance_lock.LockError,
) -> List(doctor.CheckResult) {
  let startup = map_lock_error(Error(err))
  let #(code, message) = case startup {
    Error(err) -> #(err.code, err.message)
    Ok(_) -> #("", "")
  }
  let instance_lock_selected =
    doctor.contains_check(selected, doctor.InstanceLock)
  let workspace_hooks_selected =
    doctor.contains_check(selected, doctor.WorkspaceHooks)
  let results = case instance_lock_selected {
    False -> results
    True ->
      list.append(results, [
        doctor.CheckResult(
          check: doctor.InstanceLock,
          status: doctor.Fail,
          code: code,
          message: message,
          fields: [],
        ),
      ])
  }
  let results = case workspace_hooks_selected {
    False -> results
    True -> {
      let status = case instance_lock_selected {
        True -> doctor.Skip
        False -> doctor.Fail
      }
      list.append(results, [
        doctor.CheckResult(
          check: doctor.WorkspaceHooks,
          status: status,
          code: "instance_lock_failed",
          message: "instance lock was unavailable",
          fields: [],
        ),
      ])
    }
  }
  case doctor.contains_check(selected, doctor.PiProbe) {
    False -> results
    True -> {
      let status = case instance_lock_selected || workspace_hooks_selected {
        True -> doctor.Skip
        False -> doctor.Fail
      }
      list.append(results, [
        doctor.CheckResult(
          check: doctor.PiProbe,
          status: status,
          code: "instance_lock_failed",
          message: "instance lock was unavailable",
          fields: [],
        ),
      ])
    }
  }
}

fn append_instance_lock_pass(
  results: List(doctor.CheckResult),
  selected: List(doctor.CheckName),
  bundle: runtime_bundle.RuntimeBundle,
) -> List(doctor.CheckResult) {
  case doctor.contains_check(selected, doctor.InstanceLock) {
    False -> results
    True ->
      list.append(results, [
        doctor.CheckResult(
          check: doctor.InstanceLock,
          status: doctor.Pass,
          code: "ok",
          message: "instance lock acquired",
          fields: [#("workspace_root", bundle.effective.workspace.root)],
        ),
      ])
  }
}

fn run_workspace_and_pi_checks(
  results: List(doctor.CheckResult),
  selected: List(doctor.CheckName),
  bundle: runtime_bundle.RuntimeBundle,
  dependencies: DoctorDependencies,
) -> List(doctor.CheckResult) {
  case
    doctor.contains_check(selected, doctor.WorkspaceHooks)
    || doctor.contains_check(selected, doctor.PiProbe)
  {
    False -> results
    True ->
      case prepare_doctor_workspace(bundle, dependencies) {
        Error(err) -> workspace_prepare_failure_results(results, selected, err)
        Ok(prepared) -> {
          let results =
            maybe_workspace_hooks_pass(results, selected, bundle, prepared)
          let results =
            maybe_pi_probe_result(
              results,
              selected,
              bundle,
              dependencies,
              prepared,
            )
          append_cleanup_warning_if_needed(
            results,
            selected,
            bundle,
            dependencies,
            prepared,
          )
        }
      }
  }
}

fn prepare_doctor_workspace(
  bundle: runtime_bundle.RuntimeBundle,
  dependencies: DoctorDependencies,
) -> Result(workspace_run.PreparedStepWorkspace, workspace_run.PrepareError) {
  dependencies.prepare_step(
    doctor_issue(),
    "doctor",
    "doctor",
    "doctor",
    workflow_dag.WorkspaceRef(name: "main", from: None),
    bundle.orchestrator,
    dict.new(),
  )
}

fn doctor_issue() -> domain.Issue {
  domain.Issue(
    id: "SCHERZO-DOCTOR",
    identifier: "SCHERZO-DOCTOR",
    title: "Scherzo doctor",
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked(""),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    created_at: None,
    updated_at: None,
  )
}

fn workspace_prepare_failure_results(
  results: List(doctor.CheckResult),
  selected: List(doctor.CheckName),
  err: workspace_run.PrepareError,
) -> List(doctor.CheckResult) {
  let #(code, message) = prepare_error_details(err)
  let workspace_hooks_selected =
    doctor.contains_check(selected, doctor.WorkspaceHooks)
  let results = case workspace_hooks_selected {
    False -> results
    True ->
      list.append(results, [
        doctor.CheckResult(
          check: doctor.WorkspaceHooks,
          status: doctor.Fail,
          code: code,
          message: message,
          fields: [],
        ),
      ])
  }
  case doctor.contains_check(selected, doctor.PiProbe) {
    False -> results
    True -> {
      let status = case workspace_hooks_selected {
        True -> doctor.Skip
        False -> doctor.Fail
      }
      list.append(results, [
        doctor.CheckResult(
          check: doctor.PiProbe,
          status: status,
          code: "workspace_prepare_failed",
          message: "doctor workspace was not prepared",
          fields: [],
        ),
      ])
    }
  }
}

fn prepare_error_details(err: workspace_run.PrepareError) -> #(String, String) {
  case err {
    workspace_run.WorkspaceFailure(err) -> #(
      error.workspace_code(err),
      "workspace error",
    )
    workspace_run.HookFailure(err) -> #(error.hook_code(err), "hook error")
  }
}

fn maybe_workspace_hooks_pass(
  results: List(doctor.CheckResult),
  selected: List(doctor.CheckName),
  bundle: runtime_bundle.RuntimeBundle,
  prepared: workspace_run.PreparedStepWorkspace,
) -> List(doctor.CheckResult) {
  case doctor.contains_check(selected, doctor.WorkspaceHooks) {
    False -> results
    True ->
      list.append(results, [
        doctor.CheckResult(
          check: doctor.WorkspaceHooks,
          status: doctor.Pass,
          code: "ok",
          message: "workspace hooks prepared a scratch step workspace",
          fields: [
            #("workspace_path", prepared.path),
            #("run_root", prepared.run_root),
            #(
              "hooks",
              configured_workspace_hooks(bundle.orchestrator.dag_hooks),
            ),
          ],
        ),
      ])
  }
}

fn configured_workspace_hooks(hooks: domain.DagHooksConfig) -> String {
  []
  |> append_hook_name(hooks.create, "create")
  |> append_hook_name(hooks.before_step, "before_step")
  |> append_hook_name(hooks.remove, "remove")
  |> list.reverse
  |> hook_names_to_string
}

fn append_hook_name(
  names: List(String),
  script: Option(String),
  name: String,
) -> List(String) {
  case script {
    None -> names
    Some(_) -> [name, ..names]
  }
}

fn hook_names_to_string(names: List(String)) -> String {
  case names {
    [] -> "none"
    _ -> string.join(names, with: ",")
  }
}

fn maybe_pi_probe_result(
  results: List(doctor.CheckResult),
  selected: List(doctor.CheckName),
  bundle: runtime_bundle.RuntimeBundle,
  dependencies: DoctorDependencies,
  prepared: workspace_run.PreparedStepWorkspace,
) -> List(doctor.CheckResult) {
  case doctor.contains_check(selected, doctor.PiProbe) {
    False -> results
    True ->
      list.append(results, [
        run_pi_probe_doctor_check(bundle, dependencies, prepared),
      ])
  }
}

fn run_pi_probe_doctor_check(
  bundle: runtime_bundle.RuntimeBundle,
  dependencies: DoctorDependencies,
  prepared: workspace_run.PreparedStepWorkspace,
) -> doctor.CheckResult {
  case
    dependencies.pi_probe(
      bundle.effective.pi.command,
      prepared.path,
      bundle.effective.pi.read_timeout_ms,
    )
  {
    Ok(Nil) ->
      doctor.CheckResult(
        check: doctor.PiProbe,
        status: doctor.Pass,
        code: "ok",
        message: "pi RPC probe succeeded without sending a prompt",
        fields: [#("workspace_path", prepared.path)],
      )
    Error(err) ->
      doctor.CheckResult(
        check: doctor.PiProbe,
        status: doctor.Fail,
        code: error.pi_rpc_code(err),
        message: "pi probe error",
        fields: [#("workspace_path", prepared.path)],
      )
  }
}

fn append_cleanup_warning_if_needed(
  results: List(doctor.CheckResult),
  selected: List(doctor.CheckName),
  bundle: runtime_bundle.RuntimeBundle,
  dependencies: DoctorDependencies,
  prepared: workspace_run.PreparedStepWorkspace,
) -> List(doctor.CheckResult) {
  case dependencies.cleanup_run(prepared.run_root, bundle.orchestrator) {
    Ok(Nil) -> results
    Error(err) ->
      case doctor.contains_check(selected, doctor.WorkspaceHooks) {
        False -> results
        True ->
          list.append(results, [
            doctor.CheckResult(
              check: doctor.WorkspaceHooks,
              status: doctor.Warn,
              code: "workspace_cleanup_failed",
              message: "doctor workspace cleanup failed",
              fields: [
                #("run_root", prepared.run_root),
                #("workspace_path", prepared.path),
                #("error", error.workspace_code(err)),
              ],
            ),
          ])
      }
  }
}

fn write_doctor_report(
  report: doctor.Report,
  options: doctor.Options,
  dependencies: DoctorDependencies,
  secrets: List(String),
) -> Nil {
  case options.output {
    doctor.Human -> {
      let _ =
        dependencies.list_writer(doctor.human_report(report, options.path))
      Nil
    }
    doctor.Logfmt -> log_doctor_report(report, dependencies, secrets)
  }
}

fn log_doctor_report(
  report: doctor.Report,
  dependencies: DoctorDependencies,
  secrets: List(String),
) -> Nil {
  list.each(report.results, fn(result) {
    let _ =
      dependencies.logger(
        doctor_result_level(result),
        doctor.result_event(result),
        doctor.result_log_fields(result),
        secrets,
      )
    Nil
  })
  let _ =
    dependencies.logger(
      "info",
      "doctor_summary",
      doctor.summary_log_fields(doctor.summary(report)),
      secrets,
    )
  Nil
}

fn doctor_result_level(result: doctor.CheckResult) -> String {
  case result.status {
    doctor.Pass -> "info"
    doctor.Warn -> "warn"
    doctor.Fail -> "error"
    doctor.Skip -> "warn"
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
  use lock <- try_startup(acquire_lock(bundle.effective.workspace.root))
  let probe_result =
    run_pi_probe_orchestrator(bundle.orchestrator, bundle.secrets)
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
  _require_dispatch: Bool,
) -> Result(instance_lock.Lock, StartupError) {
  use bundle <- try_startup(
    runtime_bundle.load(workflow_path)
    |> map_bundle_error,
  )
  acquire_lock(bundle.effective.workspace.root)
}

fn acquire_lock(
  workspace_root: String,
) -> Result(instance_lock.Lock, StartupError) {
  instance_lock.acquire(workspace_root) |> map_lock_error
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
      state: issue_state.from_string_unchecked(""),
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

pub fn run_once_with_dependencies(
  workflow_path: Option(String),
  dependencies: Dependencies,
) -> Result(ServiceResult, StartupError) {
  use bundle <- try_startup(
    runtime_bundle.load(workflow_path)
    |> map_bundle_error,
  )
  run_once_loaded(bundle.config_path, bundle, dependencies)
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
    False -> run_tick(bundle, initial_state, tracker_client, dependencies, logs)
  }
}

fn run_tick(
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
      dispatch_candidates(
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

fn dispatch_candidates(
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
          dispatch_candidates(
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
              dispatch_candidates(
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
              dispatch_issue(
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

fn dispatch_issue(
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
  let orchestrator = bundle.orchestrator
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
      let transition =
        core.apply_workflow_success(
          state,
          bundle.effective,
          issue.id,
          final_issue,
          success.worker_success.tokens,
          dependencies.now_ms(),
          core.AlreadyCleaned,
        )
      let logs =
        interpret_effects(transition.effects, bundle.effective, dependencies, [
          cleaned,
          exited,
          started,
          ..logs
        ])
      dispatch_candidates(
        remaining,
        bundle,
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
          #("error", failure.reason),
        ])
      let _ = dependencies.logger(failed)
      let transition =
        core.apply_worker_failure(
          state,
          bundle.effective,
          issue.id,
          issue,
          dependencies.now_ms(),
        )
      let logs =
        interpret_effects(transition.effects, bundle.effective, dependencies, [
          failed,
          started,
          ..logs
        ])
      dispatch_candidates(
        remaining,
        bundle,
        transition.state,
        tracker_client,
        dependencies,
        logs,
        dispatched + 1,
      )
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
        #("reason", orchestrator_reason.retry_to_string(reason)),
      ])
    core.CancelRetry(issue_id) ->
      log.info("retry_cancelled", [#("issue_id", issue_id)])
    core.ReleaseClaim(issue_id) ->
      log.info("claim_released", [#("issue_id", issue_id)])
    core.StopWorker(issue_id, reason) ->
      log.warn("worker_stop_requested", [
        #("issue_id", issue_id),
        #("reason", orchestrator_reason.stop_to_string(reason)),
      ])
    core.ParkIssue(issue_id, reason) ->
      log.warn("issue_parked", [
        #("issue_id", issue_id),
        #("reason", orchestrator_reason.park_to_string(reason)),
      ])
  }
}

fn attachment_log_fields(
  outcome: linear_attachment.AttachmentOutcome,
) -> List(log.Field) {
  case outcome {
    linear_attachment.AttachedNative(comment_id, filename, _) -> [
      #("comment_id", comment_id),
      #("filename", filename),
      #("mode", linear_attachment.outcome_mode(outcome)),
    ]
    linear_attachment.AttachedMarkdownLink(comment_id, filename, _) -> [
      #("comment_id", comment_id),
      #("filename", filename),
      #("mode", linear_attachment.outcome_mode(outcome)),
    ]
    linear_attachment.AlreadyAttached(comment_id, filename) -> [
      #("comment_id", comment_id),
      #("filename", filename),
      #("mode", linear_attachment.outcome_mode(outcome)),
    ]
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

fn map_tracker_error(
  result: Result(a, error.TrackerError),
) -> Result(a, StartupError) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) -> Error(StartupError(error.tracker_code(err), "tracker error"))
  }
}

fn map_attachment_tracker_error(
  result: Result(a, error.TrackerError),
) -> Result(a, StartupError) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) ->
      Error(StartupError(error.tracker_code(err), tracker_error_message(err)))
  }
}

fn tracker_error_message(err: error.TrackerError) -> String {
  case err {
    error.LinearApiRequest(message) -> message
    error.LinearApiStatus(status) ->
      "Linear API returned status " <> int_to_string(status)
    error.LinearGraphqlErrors(message) -> message
    error.LinearUnknownPayload(message) -> message
    error.LinearMissingEndCursor -> "Linear response was missing endCursor"
    error.LinearUploadStatus(status) ->
      "Linear upload returned status " <> int_to_string(status)
    error.LinearAttachmentError(message) -> message
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
