import gleam/erlang/process
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/probe
import scherzo/doctor
import scherzo/error
import scherzo/instance_lock
import scherzo/linear
import scherzo/linear_contract
import scherzo/orchestrator/service
import scherzo/path
import scherzo/runtime_bundle
import scherzo/smoke
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workspace_run
import simplifile
import test_async

pub type DoctorAction {
  LockAcquired(String)
  LockReleased
  PrepareCalled(run_root: String, workspace_path: String, profile: String)
  PiCalled(cwd: String)
  CleanupCalled(run_root: String, profile: String)
  LogCaptured(
    level: String,
    event: String,
    fields: List(#(String, String)),
    secrets: List(String),
  )
  ListWritten(String)
}

fn reset_dir(dir: String) -> Nil {
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  Nil
}

fn fake_pi() -> String {
  let assert Ok(abs) = path.absolute("test/fixtures/fake_pi_rpc.sh")
  abs
}

fn issue(id: String) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: id,
    identifier: id,
    title: "Issue " <> id,
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked("Todo"),
    branch_name: None,
    url: None,
    labels: ["workflow:implementation"],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn write_config(dir: String, extra: String) -> String {
  reset_dir(dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/workflows/prompts")
  let config_path = dir <> "/scherzo.yaml"
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      "version: 1\ntracker:\n  kind: linear\n  api_key: test-key\n  project_slug: TEST\n  active_states: [Todo]\n  dispatch_states: [Todo]\n  terminal_states: [Done]\nworkspace:\n  root: workspaces\n  hooks:\n    create: |\n      mkdir -p \"$SCHERZO_WORKSPACE_PATH\"\n    before_step: |\n      test -d \"$SCHERZO_WORKSPACE_PATH\"\n    remove: |\n      rm -rf \"$SCHERZO_WORKSPACE_PATH\"\n    timeout_ms: 60000\nrouting:\n  workflow_label_prefix: \"workflow:\"\n  require_exactly_one_workflow_label: true\n  workflows:\n    implementation: workflows/implementation.yaml\nagent:\n  max_concurrent_agents: 1\n  max_turns: 1\n"
        <> extra,
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/implementation.md\n    workspace: main\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/prompts/implementation.md",
      "Implement the issue.",
    )
  config_path
}

fn write_profile_hooks_config(dir: String) -> String {
  reset_dir(dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/workflows/prompts")
  let config_path = dir <> "/scherzo.yaml"
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      "version: 1\ntracker:\n  kind: linear\n  api_key: test-key\n  project_slug: TEST\n  active_states: [Todo]\n  dispatch_states: [Todo]\n  terminal_states: [Done]\nworkspace:\n  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      hooks:\n        create: |\n          mkdir -p \"$SCHERZO_WORKSPACE_PATH\"\n        remove: |\n          rm -rf \"$SCHERZO_WORKSPACE_PATH\"\nrouting:\n  workflow_label_prefix: \"workflow:\"\n  require_exactly_one_workflow_label: true\n  workflows:\n    implementation: workflows/implementation.yaml\nagent:\n  max_concurrent_agents: 1\n  max_turns: 1\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\nworkspace_profile: noop\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/implementation.md\n    workspace: main\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/prompts/implementation.md",
      "Implement the issue.",
    )
  config_path
}

fn write_invalid_dispatch_config(
  dir: String,
  tracker_fields: String,
) -> String {
  reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      "version: 1\ntracker:\n  kind: linear\n  api_key: test-key\n  project_slug: TEST\n"
        <> tracker_fields,
    )
  config_path
}

fn successful_deps(
  subject: process.Subject(DoctorAction),
) -> service.DoctorDependencies {
  service.DoctorDependencies(
    load_bundle: runtime_bundle.load,
    make_linear_smoke_reader: fn(_) {
      smoke_reader_success([issue("candidate")], [], [issue("candidate")])
    },
    make_contract_client: fn(_) {
      contract_client(Ok(contract_board(["Todo", "Done"])))
    },
    acquire_lock: fn(root) {
      process.send(subject, LockAcquired(root))
      Ok(
        service.DoctorLock(release: fn() { process.send(subject, LockReleased) }),
      )
    },
    prepare_step: fn(
      issue,
      workflow_id,
      run_id,
      _step_id,
      workspace_ref,
      orchestrator,
      profile,
      _known,
    ) {
      let run_root =
        orchestrator.effective.workspace.root
        <> "/"
        <> workflow_id
        <> "/"
        <> issue.identifier
        <> "/"
        <> run_id
      let workspace_path = run_root <> "/" <> workspace_ref.name
      process.send(
        subject,
        PrepareCalled(run_root, workspace_path, profile.name),
      )
      Ok(workspace_run.PreparedStepWorkspace(
        workflow_id: workflow_id,
        run_id: run_id,
        run_root: run_root,
        attempt_index: 1,
        workspace_name: workspace_ref.name,
        path: workspace_path,
        source_workspace_name: workspace_ref.from,
        source_workspace_path: None,
        workspace_profile: profile.name,
      ))
    },
    cleanup_run: fn(run_root, _orchestrator, profile) {
      process.send(subject, CleanupCalled(run_root, profile.name))
      Ok(Nil)
    },
    pi_probe: fn(_command, cwd, _timeout) {
      process.send(subject, PiCalled(cwd))
      Ok(Nil)
    },
    logger: fn(level, event, fields, secrets) {
      process.send(subject, LogCaptured(level, event, fields, secrets))
      Ok(Nil)
    },
    list_writer: fn(line) {
      process.send(subject, ListWritten(line))
      Ok(Nil)
    },
  )
}

fn smoke_reader_success(
  candidates: List(tracker_issue.Issue),
  terminals: List(tracker_issue.Issue),
  refreshed: List(tracker_issue.Issue),
) -> smoke.LinearSmokeReader {
  smoke.LinearSmokeReader(
    fetch_candidate_sample: fn() { Ok(candidates) },
    fetch_terminal_sample: fn(_) { Ok(terminals) },
    refresh_issue_states_by_ids: fn(_) { Ok(refreshed) },
  )
}

fn smoke_reader_error(err: error.TrackerError) -> smoke.LinearSmokeReader {
  smoke.LinearSmokeReader(
    fetch_candidate_sample: fn() { Error(err) },
    fetch_terminal_sample: fn(_) { Error(err) },
    refresh_issue_states_by_ids: fn(_) { Error(err) },
  )
}

fn contract_client(
  result: Result(linear_contract.RemoteBoard, error.TrackerError),
) -> linear.ContractClient {
  linear.ContractClient(fetch_remote_contract: fn() { result })
}

fn contract_board(states: List(String)) -> linear_contract.RemoteBoard {
  linear_contract.RemoteBoard(
    project_id: "project-id",
    project_slug: "TEST",
    project_name: "Test Project",
    teams: [
      linear_contract.RemoteTeam(
        id: "team-eng",
        key: "ENG",
        name: "Engineering",
        states: list_states(states),
        labels: [],
      ),
    ],
    workspace_labels: [],
  )
}

fn list_states(names: List(String)) -> List(linear_contract.RemoteState) {
  case names {
    [] -> []
    [name, ..rest] -> [
      linear_contract.RemoteState(
        id: "state-" <> name,
        name: name,
        type_: "started",
      ),
      ..list_states(rest)
    ]
  }
}

fn result_for(
  report: doctor.Report,
  check: doctor.CheckName,
) -> Option(doctor.CheckResult) {
  let doctor.Report(results) = report
  find_result(results, check)
}

fn find_result(
  results: List(doctor.CheckResult),
  check: doctor.CheckName,
) -> Option(doctor.CheckResult) {
  case results {
    [] -> None
    [result, ..rest] ->
      case result.check == check {
        True -> Some(result)
        False -> find_result(rest, check)
      }
  }
}

fn field_value(fields: List(#(String, String)), key: String) -> Option(String) {
  case fields {
    [] -> None
    [#(field_key, value), ..rest] ->
      case field_key == key {
        True -> Some(value)
        False -> field_value(rest, key)
      }
  }
}

pub fn doctor_workflow_config_success_prints_human_summary_test() {
  let config_path = write_config("test/tmp/doctor-workflow-config", "")
  let subject = process.new_subject()
  let deps = successful_deps(subject)
  let options =
    doctor.Options(
      path: Some(config_path),
      checks: ["workflow-config"],
      list_checks: False,
      output: doctor.Human,
    )
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(options, deps)
  let assert Some(result) = result_for(report, doctor.WorkflowConfig)
  assert result.status == doctor.Pass
  assert result.code == "ok"
  assert field_value(result.fields, "workflow_count") == Some("1")

  assert service.start_doctor_with_dependencies(options, deps) == Ok(Nil)
  let assert Ok(ListWritten(output)) = process.receive(subject, within: 1000)
  assert string.contains(output, "Scherzo doctor")
  assert string.contains(output, "Config: " <> config_path)
  assert string.contains(output, "✓ Workflow config")
  assert string.contains(
    output,
    "Summary: 1 passed, 0 warnings, 0 failed, 0 skipped",
  )
  assert string.contains(output, "Selected checks passed.")
}

pub fn doctor_missing_dispatch_states_failure_is_actionable_test() {
  let config_path =
    write_invalid_dispatch_config(
      "test/tmp/doctor-missing-dispatch-states",
      "  active_states: [Todo]\n  terminal_states: [Done]\n",
    )
  let subject = process.new_subject()
  let deps = successful_deps(subject)
  let options =
    doctor.Options(
      path: Some(config_path),
      checks: ["workflow-config"],
      list_checks: False,
      output: doctor.Human,
    )
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(options, deps)
  let assert Some(result) = result_for(report, doctor.WorkflowConfig)
  assert result.status == doctor.Fail
  assert string.contains(result.message, "tracker.dispatch_states")
  assert string.contains(result.message, "required")
  assert string.contains(result.message, "dispatch_states: [Todo]")

  assert service.start_doctor_with_dependencies(options, deps)
    == Error(service.StartupError(
      "doctor_failed",
      "one or more doctor checks failed",
    ))
  let assert Ok(ListWritten(output)) = process.receive(subject, within: 1000)
  assert string.contains(output, "Workflow config")
  assert string.contains(output, "tracker.dispatch_states")
  assert string.contains(output, "required")
  assert string.contains(output, "dispatch_states: [Todo]")
}

pub fn doctor_wrong_type_dispatch_states_failure_is_actionable_test() {
  assert_doctor_dispatch_state_config_failure(
    "test/tmp/doctor-wrong-dispatch-states",
    "  active_states: [Todo]\n  dispatch_states: Todo\n  terminal_states: [Done]\n",
    "tracker.dispatch_states must be a string list",
  )
}

pub fn doctor_non_string_dispatch_states_failure_is_actionable_test() {
  assert_doctor_dispatch_state_config_failure(
    "test/tmp/doctor-non-string-dispatch-states",
    "  active_states: [Todo]\n  dispatch_states: [Todo, 123]\n  terminal_states: [Done]\n",
    "tracker.dispatch_states entries must be strings",
  )
}

pub fn doctor_empty_dispatch_states_failure_is_actionable_test() {
  assert_doctor_dispatch_state_config_failure(
    "test/tmp/doctor-empty-dispatch-states",
    "  active_states: [Todo]\n  dispatch_states: []\n  terminal_states: [Done]\n",
    "tracker.dispatch_states must contain at least one state",
  )
}

pub fn doctor_out_of_subset_dispatch_states_failure_is_actionable_test() {
  assert_doctor_dispatch_state_config_failure(
    "test/tmp/doctor-subset-dispatch-states",
    "  active_states: [Todo]\n  dispatch_states: [In Progress]\n  terminal_states: [Done]\n",
    "tracker.dispatch_states must be a subset of tracker.active_states",
  )
}

fn assert_doctor_dispatch_state_config_failure(
  dir: String,
  tracker_fields: String,
  expected_message: String,
) -> Nil {
  let config_path = write_invalid_dispatch_config(dir, tracker_fields)
  let subject = process.new_subject()
  let deps = successful_deps(subject)
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(
      doctor.Options(
        path: Some(config_path),
        checks: ["workflow-config"],
        list_checks: False,
        output: doctor.Human,
      ),
      deps,
    )
  let assert Some(result) = result_for(report, doctor.WorkflowConfig)
  assert result.status == doctor.Fail
  assert string.contains(result.message, expected_message)
  assert string.contains(result.message, "tracker.dispatch_states")
}

pub fn doctor_unknown_check_name_fails_before_loading_workflow_test() {
  let subject = process.new_subject()
  let deps =
    service.DoctorDependencies(..successful_deps(subject), load_bundle: fn(_) {
      panic as "load_bundle should not run for an unknown doctor check"
    })
  let assert Error(err) =
    service.build_doctor_report_with_dependencies(
      doctor.Options(
        path: Some("test/tmp/no-such-scherzo.yaml"),
        checks: ["no-such-check"],
        list_checks: False,
        output: doctor.Human,
      ),
      deps,
    )
  assert err.code == "unknown_doctor_check"
}

pub fn doctor_linear_smoke_success_reports_counts_test() {
  let config_path = write_config("test/tmp/doctor-linear-smoke", "")
  let subject = process.new_subject()
  let deps =
    service.DoctorDependencies(
      ..successful_deps(subject),
      make_linear_smoke_reader: fn(_) {
        smoke_reader_success([issue("candidate")], [], [issue("candidate")])
      },
    )
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(
      doctor.Options(
        path: Some(config_path),
        checks: ["tracker-smoke"],
        list_checks: False,
        output: doctor.Human,
      ),
      deps,
    )
  let assert Some(result) = result_for(report, doctor.LinearSmoke)
  assert result.status == doctor.Pass
  assert field_value(result.fields, "candidate_count") == Some("1")
  assert field_value(result.fields, "terminal_count") == Some("0")
  assert field_value(result.fields, "refreshed_count") == Some("1")
}

pub fn doctor_linear_smoke_failure_does_not_skip_workspace_probe_test() {
  let config_path = write_config("test/tmp/doctor-linear-smoke-failure", "")
  let subject = process.new_subject()
  let deps =
    service.DoctorDependencies(
      ..successful_deps(subject),
      make_linear_smoke_reader: fn(_) {
        smoke_reader_error(error.LinearApiStatus(500))
      },
    )
  let options =
    doctor.Options(
      path: Some(config_path),
      checks: ["linear-smoke", "workspace-hooks", "pi-probe"],
      list_checks: False,
      output: doctor.Logfmt,
    )
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(options, deps)
  let assert Some(smoke_result) = result_for(report, doctor.LinearSmoke)
  let assert Some(workspace_result) = result_for(report, doctor.WorkspaceHooks)
  let assert Some(pi_result) = result_for(report, doctor.PiProbe)
  assert smoke_result.status == doctor.Fail
  assert smoke_result.code == "linear_api_status"
  assert workspace_result.status == doctor.Warn
  assert pi_result.status == doctor.Pass

  let assert Error(err) = service.start_doctor_with_dependencies(options, deps)
  assert err.code == "doctor_failed"
  assert receive_log_event(subject, "doctor_check_fail") == True
  assert receive_log_event(subject, "doctor_check_pass") == True
}

pub fn doctor_contract_mismatch_reports_failure_test() {
  let config_path = write_config("test/tmp/doctor-contract-mismatch", "")
  let subject = process.new_subject()
  let deps =
    service.DoctorDependencies(
      ..successful_deps(subject),
      make_contract_client: fn(_) {
        contract_client(Ok(contract_board(["Done"])))
      },
    )
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(
      doctor.Options(
        path: Some(config_path),
        checks: ["tracker-contract"],
        list_checks: False,
        output: doctor.Human,
      ),
      deps,
    )
  let assert Some(result) = result_for(report, doctor.LinearContract)
  assert result.status == doctor.Fail
  assert result.code == "linear_contract_mismatch"
  assert field_value(result.fields, "diagnostic_count") == Some("2")
  assert doctor.has_failures(report) == True
}

pub fn doctor_lock_failure_reports_only_selected_checks_test() {
  let config_path = write_config("test/tmp/doctor-lock-failure", "")
  let subject = process.new_subject()
  let deps =
    service.DoctorDependencies(
      ..successful_deps(subject),
      acquire_lock: fn(_) { Error(instance_lock.LockAlreadyHeld("held")) },
      prepare_step: fn(_, _, _, _, _, _, _, _) {
        panic as "prepare_step should not run when the doctor lock fails"
      },
      pi_probe: fn(_, _, _) {
        panic as "pi_probe should not run when the doctor lock fails"
      },
    )
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(
      doctor.Options(
        path: Some(config_path),
        checks: ["workspace-hooks", "pi-probe"],
        list_checks: False,
        output: doctor.Human,
      ),
      deps,
    )
  assert result_for(report, doctor.InstanceLock) == None
  let assert Some(workspace_result) = result_for(report, doctor.WorkspaceHooks)
  let assert Some(pi_result) = result_for(report, doctor.PiProbe)
  assert workspace_result.status == doctor.Fail
  assert pi_result.status == doctor.Skip
  assert doctor.has_failures(report) == True
  test_async.assert_no_extra_message_within(subject, 50)
}

pub fn doctor_pi_probe_lock_failure_reports_only_pi_probe_test() {
  let config_path = write_config("test/tmp/doctor-pi-lock-failure", "")
  let subject = process.new_subject()
  let deps =
    service.DoctorDependencies(
      ..successful_deps(subject),
      acquire_lock: fn(_) { Error(instance_lock.LockAlreadyHeld("held")) },
      prepare_step: fn(_, _, _, _, _, _, _, _) {
        panic as "prepare_step should not run when the doctor lock fails"
      },
      pi_probe: fn(_, _, _) {
        panic as "pi_probe should not run when the doctor lock fails"
      },
    )
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(
      doctor.Options(
        path: Some(config_path),
        checks: ["pi-probe"],
        list_checks: False,
        output: doctor.Human,
      ),
      deps,
    )
  assert result_for(report, doctor.InstanceLock) == None
  assert result_for(report, doctor.WorkspaceHooks) == None
  let assert Some(pi_result) = result_for(report, doctor.PiProbe)
  assert pi_result.status == doctor.Fail
  assert pi_result.code == "instance_lock_failed"
  assert doctor.has_failures(report) == True
  test_async.assert_no_extra_message_within(subject, 50)
}

pub fn doctor_workspace_and_pi_share_one_prepared_workspace_test() {
  let config_path = write_config("test/tmp/doctor-share-workspace", "")
  let subject = process.new_subject()
  let deps = successful_deps(subject)
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(
      doctor.Options(
        path: Some(config_path),
        checks: ["workspace-hooks", "pi-probe"],
        list_checks: False,
        output: doctor.Human,
      ),
      deps,
    )
  let assert Some(workspace_result) = result_for(report, doctor.WorkspaceHooks)
  let assert Some(pi_result) = result_for(report, doctor.PiProbe)
  let assert Some(workspace_path) =
    field_value(workspace_result.fields, "workspace_path")
  assert field_value(pi_result.fields, "workspace_path") == Some(workspace_path)

  let assert Ok(LockAcquired(_)) = process.receive(subject, within: 1000)
  let assert Ok(PrepareCalled(run_root, prepared_path, "default")) =
    process.receive(subject, within: 1000)
  let assert Ok(PiCalled(pi_path)) = process.receive(subject, within: 1000)
  let assert Ok(CleanupCalled(cleaned_root, "default")) =
    process.receive(subject, within: 1000)
  let assert Ok(LockReleased) = process.receive(subject, within: 1000)
  assert pi_path == prepared_path
  assert cleaned_root == run_root
}

pub fn doctor_workspace_hooks_warns_for_top_level_legacy_hooks_test() {
  let config_path = write_config("test/tmp/doctor-top-level-hook-warning", "")
  let subject = process.new_subject()
  let deps = successful_deps(subject)
  let options =
    doctor.Options(
      path: Some(config_path),
      checks: ["workspace-hooks"],
      list_checks: False,
      output: doctor.Human,
    )
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(options, deps)
  let assert Some(result) = result_for(report, doctor.WorkspaceHooks)
  assert result.status == doctor.Warn
  assert result.code == "legacy_workspace_hooks"
  assert string.contains(result.message, "workspace.hooks")
  assert string.contains(result.message, "workspace.profiles.<name>.driver")
  assert string.contains(
    result.message,
    "docs/runbooks/workspace-driver-migration.md",
  )
  assert field_value(result.fields, "legacy_key") == Some("workspace.hooks")

  assert service.start_doctor_with_dependencies(options, deps) == Ok(Nil)
  let assert Some(output) = receive_list_written(subject)
  assert string.contains(output, "docs/runbooks/workspace-driver-migration.md")
  assert string.contains(output, "workspace.hooks")
}

pub fn doctor_workspace_hooks_warns_for_profile_local_legacy_hooks_test() {
  let config_path =
    write_profile_hooks_config("test/tmp/doctor-profile-hook-warning")
  let subject = process.new_subject()
  let deps = successful_deps(subject)
  let options =
    doctor.Options(
      path: Some(config_path),
      checks: ["workspace-hooks"],
      list_checks: False,
      output: doctor.Human,
    )
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(options, deps)
  let assert Some(result) = result_for(report, doctor.WorkspaceHooks)
  assert result.status == doctor.Warn
  assert string.contains(result.message, "workspace.profiles.noop.hooks")
  assert string.contains(result.message, "workspace.profiles.noop.driver")
  assert string.contains(
    result.message,
    "docs/runbooks/workspace-driver-migration.md",
  )
  assert field_value(result.fields, "legacy_key")
    == Some("workspace.profiles.noop.hooks")
}

pub fn doctor_cleanup_failure_warns_test() {
  let config_path = write_config("test/tmp/doctor-cleanup-warning", "")
  let subject = process.new_subject()
  let deps =
    service.DoctorDependencies(
      ..successful_deps(subject),
      cleanup_run: fn(run_root, _orchestrator, _profile) {
        process.send(subject, CleanupCalled(run_root, "default"))
        Error(error.WorkspaceIo("delete failed"))
      },
    )
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(
      doctor.Options(
        path: Some(config_path),
        checks: ["workspace-hooks", "pi-probe"],
        list_checks: False,
        output: doctor.Human,
      ),
      deps,
    )
  let summary = doctor.summary(report)
  assert summary.warned == 2
  let doctor.Report(results) = report
  assert has_warning(results, "workspace_cleanup_failed") == True
}

pub fn doctor_pi_probe_prepare_failure_reports_only_pi_probe_test() {
  let config_path = write_config("test/tmp/doctor-pi-prepare-failure", "")
  let subject = process.new_subject()
  let deps =
    service.DoctorDependencies(
      ..successful_deps(subject),
      prepare_step: fn(_, _, _, _, _, _, _, _) {
        Error(
          workspace_run.WorkspaceFailure(error.WorkspaceIo("prepare failed")),
        )
      },
      cleanup_run: fn(_, _, _) {
        panic as "cleanup_run should not run when prepare_step fails"
      },
      pi_probe: fn(_, _, _) {
        panic as "pi_probe should not run when prepare_step fails"
      },
    )
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(
      doctor.Options(
        path: Some(config_path),
        checks: ["pi-probe"],
        list_checks: False,
        output: doctor.Human,
      ),
      deps,
    )
  assert result_for(report, doctor.InstanceLock) == None
  assert result_for(report, doctor.WorkspaceHooks) == None
  let assert Some(pi_result) = result_for(report, doctor.PiProbe)
  assert pi_result.status == doctor.Fail
  assert pi_result.code == "workspace_prepare_failed"
  assert doctor.has_failures(report) == True
}

pub fn doctor_pi_probe_cleanup_failure_does_not_report_workspace_hooks_test() {
  let config_path = write_config("test/tmp/doctor-pi-cleanup-warning", "")
  let subject = process.new_subject()
  let deps =
    service.DoctorDependencies(
      ..successful_deps(subject),
      cleanup_run: fn(run_root, _orchestrator, _profile) {
        process.send(subject, CleanupCalled(run_root, "default"))
        Error(error.WorkspaceIo("delete failed"))
      },
    )
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(
      doctor.Options(
        path: Some(config_path),
        checks: ["pi-probe"],
        list_checks: False,
        output: doctor.Human,
      ),
      deps,
    )
  let summary = doctor.summary(report)
  assert summary.warned == 0
  assert summary.failed == 0
  assert result_for(report, doctor.InstanceLock) == None
  assert result_for(report, doctor.WorkspaceHooks) == None
  let assert Some(pi_result) = result_for(report, doctor.PiProbe)
  assert pi_result.status == doctor.Pass
}

pub fn doctor_pi_probe_does_not_prompt_test() {
  let dir = "test/tmp/doctor-pi-probe"
  let transcript_path = dir <> "/transcript.jsonl"
  let assert Ok(transcript) = path.absolute(transcript_path)
  let command = "FAKE_PI_TRANSCRIPT=" <> transcript <> " " <> fake_pi()
  let config_path =
    write_config(
      dir,
      "pi:\n  command: \"" <> command <> "\"\n  compatibility_probe: true\n",
    )
  let subject = process.new_subject()
  let deps =
    service.DoctorDependencies(
      ..successful_deps(subject),
      prepare_step: workspace_run.prepare_step,
      cleanup_run: workspace_run.cleanup_run,
      pi_probe: probe.probe,
    )
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(
      doctor.Options(
        path: Some(config_path),
        checks: ["pi-probe"],
        list_checks: False,
        output: doctor.Human,
      ),
      deps,
    )
  assert result_for(report, doctor.InstanceLock) == None
  assert result_for(report, doctor.WorkspaceHooks) == None
  let assert Some(pi_result) = result_for(report, doctor.PiProbe)
  assert pi_result.status == doctor.Pass
  let assert Ok(contents) = simplifile.read(transcript)
  assert string.contains(contents, "get_state")
  assert string.contains(contents, "get_session_stats")
  assert !string.contains(contents, "prompt")
}

pub fn doctor_list_checks_writes_names_without_loading_config_test() {
  let subject = process.new_subject()
  let deps =
    service.DoctorDependencies(..successful_deps(subject), load_bundle: fn(_) {
      panic as "load_bundle should not run for --list-checks"
    })
  assert service.start_doctor_with_dependencies(
      doctor.Options(
        path: None,
        checks: [],
        list_checks: True,
        output: doctor.Human,
      ),
      deps,
    )
    == Ok(Nil)
  let assert Ok(ListWritten("workflow-config")) =
    process.receive(subject, within: 1000)
  let assert Ok(ListWritten("scheduled-jobs")) =
    process.receive(subject, within: 1000)
  let assert Ok(ListWritten("tracker-contract")) =
    process.receive(subject, within: 1000)
}

fn receive_list_written(
  subject: process.Subject(DoctorAction),
) -> Option(String) {
  case process.receive(subject, within: 1000) {
    Error(_) -> None
    Ok(ListWritten(output)) -> Some(output)
    Ok(_) -> receive_list_written(subject)
  }
}

fn receive_log_event(
  subject: process.Subject(DoctorAction),
  wanted: String,
) -> Bool {
  case process.receive(subject, within: 1000) {
    Error(_) -> False
    Ok(LogCaptured(event: event, ..)) ->
      case event == wanted {
        True -> True
        False -> receive_log_event(subject, wanted)
      }
    Ok(_) -> receive_log_event(subject, wanted)
  }
}

fn has_warning(results: List(doctor.CheckResult), code: String) -> Bool {
  case results {
    [] -> False
    [result, ..rest] ->
      case result.status == doctor.Warn && result.code == code {
        True -> True
        False -> has_warning(rest, code)
      }
  }
}
