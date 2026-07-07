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
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/record
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_attempt
import scherzo/workflow_interface_snapshot
import scherzo/workspace
import scherzo/workspace_run
import simplifile
import support/test_helpers
import test_async

fn workspace_source(
  from: Option(String),
  run_root: String,
) -> workspace.WorkspaceSource {
  case from {
    None -> workspace.FreshWorkspace
    Some(name) -> workspace.DerivedWorkspace(name, run_root <> "/" <> name)
  }
}

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
  write_config_with_linear_fields(
    dir,
    "    api_key_env: HOME\n    project: TEST\n",
    extra,
  )
}

fn write_config_with_linear_fields(
  dir: String,
  linear_fields: String,
  extra: String,
) -> String {
  write_config_with_tracker_and_linear_fields(dir, "", linear_fields, extra)
}

fn write_config_with_tracker_and_linear_fields(
  dir: String,
  tracker_fields: String,
  linear_fields: String,
  extra: String,
) -> String {
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/workflows/prompts")
  let config_path = dir <> "/scherzo.yaml"
  let assert Ok(driver_command) =
    path.absolute("scripts/scherzo-workspace-noop")
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      "version: 1\ntracker:\n"
        <> tracker_fields
        <> "  linear:\n"
        <> linear_fields
        <> "  states:\n    ready: [Todo]\n    active: [Todo]\n    terminal: [Done]\nworkspace:\n  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: custom\n      command: "
        <> driver_command
        <> "\n      timeout: 60s\nworkflows:\n    implementation: workflows/implementation.yaml\nagents:\n  concurrency: 1\n  max_turns: 1\n"
        <> extra,
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/implementation.md\n    run_in: main\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/prompts/implementation.md",
      "Implement the issue.",
    )
  config_path
}

fn write_retained_publication_config(dir: String) -> String {
  let config_path =
    write_config(
      dir,
      "artifacts:\n  repositories:\n    github:\n      docs:\n        repo: scherzo-systems/scherzo\n        base: main\n        branch:\n          strategy: stable_per_work\n          template: scherzo/workflow.{{ workflow.id }}/{{ work.identifier }}/{{ publication.id }}\n        pull_request:\n          enabled: true\n          strategy: update_existing\n          draft: true\n          title: '{{ work.identifier }} publication'\n          body_template: templates/publication.md\n",
    )
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/workflows/templates")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/templates/publication.md",
      "Published by Scherzo.",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    review_doc:\n      type: document.markdown\n      source:\n        step: main\n        field: stdout\nartifacts:\n  publications:\n    - id: review_doc_pub\n      repository: github.docs\n      required: true\n      pull_request:\n        title: '{{ work.identifier }} publication'\n        body_template: templates/publication.md\n      files:\n        - select:\n            output: review_doc\n          path: docs/{{ work.identifier }}.md\nsteps:\n  - id: main\n    kind: command\n    run: ignored\n",
    )
  config_path
}

fn write_top_level_hooks_config(dir: String) -> String {
  test_helpers.reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      "version: 1\ntracker:\n  linear:\n    api_key_env: HOME\n    project: TEST\n  states:\n    ready: [Todo]\n    active: [Todo]\n    terminal: [Done]\nworkspace:\n  root: workspaces\n  hooks:\n    create: mkdir -p \"$SCHERZO_WORKSPACE_PATH\"\nworkflows:\n    implementation: workflows/implementation.yaml\n",
    )
  config_path
}

fn write_profile_hooks_config(dir: String) -> String {
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/workflows/prompts")
  let config_path = dir <> "/scherzo.yaml"
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      "version: 1\ntracker:\n  linear:\n    api_key_env: HOME\n    project: TEST\n  states:\n    ready: [Todo]\n    active: [Todo]\n    terminal: [Done]\nworkspace:\n  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: custom\n      command: scripts/noop\n      hooks:\n        create: |\n          mkdir -p \"$SCHERZO_WORKSPACE_PATH\"\n        remove: |\n          rm -rf \"$SCHERZO_WORKSPACE_PATH\"\nworkflows:\n    implementation: workflows/implementation.yaml\nagents:\n  concurrency: 1\n  max_turns: 1\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\nworkspace:\n  driver: noop\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/implementation.md\n    run_in: main\n",
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
  test_helpers.reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      "version: 1\ntracker:\n  linear:\n    api_key_env: HOME\n    project: TEST\n"
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
        workflow_bundle_dir: "",
        attempt_index: 1,
        workspace_name: workspace_ref.name,
        path: workspace_path,
        source: workspace_source(workspace_ref.from, run_root),
        workspace_profile: profile.name,
      ))
    },
    cleanup_run: fn(run_root, _orchestrator, profile) {
      process.send(subject, CleanupCalled(run_root, profile.name))
      Ok(Nil)
    },
    pi_probe: fn(_launch, cwd, _timeout) {
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

fn seed_retained_materialized_unpublished_run(
  bundle: runtime_bundle.RuntimeBundle,
) -> Nil {
  let root = bundle.effective.workspace.root
  let run_root = root <> "/runs/run-1"
  let assert Ok(Nil) = simplifile.create_directory_all(run_root)
  let assert Ok(Nil) =
    simplifile.write(
      workspace_run.cleanup_retention_marker(run_root),
      "manual recovery retained workspace",
    )
  let assert Ok(#(_, workflow)) =
    runtime_bundle.workflow_by_id(bundle, "implementation")
  let fingerprint =
    workflow_attempt.workflow_fingerprint(workflow, bundle.orchestrator)
  let snapshot_contents =
    workflow_interface_snapshot.from_dag(workflow, fingerprint)
    |> workflow_interface_snapshot.to_string
  let assert Ok(snapshot_ref) =
    artifact_store.write_workflow_interface_snapshot(
      artifact_store.new(root),
      "run-1",
      snapshot_contents,
    )
  let artifact_store.ArtifactRef(
    ref: artifact_ref,
    sha256: artifact_sha256,
    bytes: artifact_bytes,
  ) = snapshot_ref
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.with_id(
          "workflow-started-run-1",
          10,
          record.WorkflowRunStarted(
            run_id: "run-1",
            workflow_id: "implementation",
            workflow_fingerprint: fingerprint,
            issue_id: "issue-1",
            issue_identifier: "LIV-1407",
            issue_fingerprint: "issue-fingerprint",
            observed_updated_at_ms: 9,
            run_root: run_root,
          ),
        ),
        record.with_id(
          "workflow-interface-snapshot-run-1",
          15,
          record.WorkflowInterfaceSnapshotRecorded(
            run_id: "run-1",
            workflow_id: "implementation",
            workflow_fingerprint: fingerprint,
            artifact_ref: artifact_ref,
            artifact_sha256: artifact_sha256,
            artifact_bytes: artifact_bytes,
          ),
        ),
        record.with_id(
          "outputs-recorded-run-1",
          20,
          record.WorkflowRunOutputsRecorded(
            run_id: "run-1",
            workflow_id: "implementation",
            workflow_fingerprint: fingerprint,
            artifact_ref: "runs/run-1/outputs.v1.json",
            artifact_sha256: "sha256",
            artifact_bytes: 123,
          ),
        ),
        record.with_id(
          "workflow-interrupted-run-1",
          30,
          record.WorkflowRunInterrupted(
            run_id: "run-1",
            workflow_id: "implementation",
            issue_id: "issue-1",
            reason: "manual_recovery_after_materialization",
          ),
        ),
      ],
      True,
    )
  Nil
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
        labels: [
          linear_contract.RemoteLabel(
            "label-workflow-implementation",
            "workflow:implementation",
          ),
        ],
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

pub fn doctor_retained_publications_warns_with_pending_route_test() {
  let config_path =
    write_retained_publication_config("test/tmp/doctor-retained-publications")
  let assert Ok(bundle) = runtime_bundle.load(Some(config_path))
  seed_retained_materialized_unpublished_run(bundle)
  let subject = process.new_subject()
  let deps = successful_deps(subject)
  let options =
    doctor.Options(
      path: Some(config_path),
      checks: ["retained-publications"],
      list_checks: False,
      output: doctor.Human,
    )

  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(options, deps)
  let assert Some(result) = result_for(report, doctor.RetainedPublications)
  assert result.status == doctor.Warn
  assert result.code == "retained_publications_unpublished"
  assert string.contains(result.message, "run-1 review_doc_pub=pending")
  assert field_value(result.fields, "unpublished_run_count") == Some("1")
  assert field_value(result.fields, "unpublished_route_count") == Some("1")
  assert field_value(result.fields, "unpublished_route_1_run_id")
    == Some("run-1")
  assert field_value(result.fields, "unpublished_route_1_publication_id")
    == Some("review_doc_pub")
  assert field_value(result.fields, "unpublished_route_1_status")
    == Some("pending")

  assert service.start_doctor_with_dependencies(options, deps) == Ok(Nil)
  let assert Ok(ListWritten(output)) = process.receive(subject, within: 1000)
  assert string.contains(output, "! Retained publications")
  assert string.contains(output, "run-1 review_doc_pub=pending")
}

pub fn doctor_tracker_scope_reports_explicit_canonical_summary_test() {
  let config_path =
    write_config_with_linear_fields(
      "test/tmp/doctor-tracker-scope-explicit",
      "    api_key_env: HOME\n    tasks_from:\n      project: product-platform\n",
      "",
    )
  let subject = process.new_subject()
  let deps = successful_deps(subject)
  let options =
    doctor.Options(
      path: Some(config_path),
      checks: ["tracker-scope"],
      list_checks: False,
      output: doctor.Human,
    )
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(options, deps)
  let assert Some(result) = result_for(report, doctor.LinearTaskScope)
  assert result.status == doctor.Pass
  assert result.code == "ok"
  assert field_value(result.fields, "task_scope_summary")
    == Some("project(product-platform)")
  assert field_value(result.fields, "task_scope_source")
    == Some("tracker.linear.tasks_from")

  assert service.start_doctor_with_dependencies(options, deps) == Ok(Nil)
  let assert Ok(ListWritten(output)) = process.receive(subject, within: 1000)
  assert string.contains(output, "✓ Tracker task scope")
  assert string.contains(output, "Linear task scope: project(product-platform)")
}

pub fn doctor_tracker_scope_reports_legacy_desugaring_test() {
  let config_path = write_config("test/tmp/doctor-tracker-scope-legacy", "")
  assert_tracker_scope_legacy_desugaring(
    config_path,
    "tracker.linear.project",
    "project(TEST)",
  )
}

pub fn doctor_tracker_scope_reports_linear_project_slug_desugaring_test() {
  let config_path =
    write_config_with_linear_fields(
      "test/tmp/doctor-tracker-scope-linear-project-slug",
      "    api_key_env: HOME\n    project_slug: product-platform\n",
      "",
    )
  assert_tracker_scope_legacy_desugaring(
    config_path,
    "tracker.linear.project_slug",
    "project(product-platform)",
  )
}

pub fn doctor_tracker_scope_reports_top_level_project_slug_desugaring_test() {
  let config_path =
    write_config_with_tracker_and_linear_fields(
      "test/tmp/doctor-tracker-scope-top-level-project-slug",
      "  project_slug: customer-success\n",
      "    api_key_env: HOME\n",
      "",
    )
  assert_tracker_scope_legacy_desugaring(
    config_path,
    "tracker.project_slug",
    "project(customer-success)",
  )
}

fn assert_tracker_scope_legacy_desugaring(
  config_path: String,
  expected_path: String,
  expected_summary: String,
) {
  let subject = process.new_subject()
  let deps = successful_deps(subject)
  let options =
    doctor.Options(
      path: Some(config_path),
      checks: ["tracker-scope"],
      list_checks: False,
      output: doctor.Human,
    )
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(options, deps)
  let assert Some(result) = result_for(report, doctor.LinearTaskScope)
  assert result.status == doctor.Pass
  assert field_value(result.fields, "task_scope_summary")
    == Some(expected_summary)
  assert field_value(result.fields, "task_scope_source") == Some(expected_path)
  assert field_value(result.fields, "legacy_task_scope_path")
    == Some(expected_path)

  assert service.start_doctor_with_dependencies(options, deps) == Ok(Nil)
  let assert Ok(ListWritten(output)) = process.receive(subject, within: 1000)
  assert string.contains(
    output,
    "Legacy "
      <> expected_path
      <> " desugars to tracker.linear.tasks_from: "
      <> expected_summary,
  )
}

pub fn doctor_tracker_scope_warns_for_static_overlap_risk_test() {
  let config_path =
    write_config_with_linear_fields(
      "test/tmp/doctor-tracker-scope-overlap",
      "    api_key_env: HOME\n    tasks_from:\n      or:\n        - and:\n            - project: product-platform\n            - any_label: [workflow:implementation]\n        - projects: [customer-success, customer-support]\n",
      "",
    )
  let subject = process.new_subject()
  let deps = successful_deps(subject)
  let options =
    doctor.Options(
      path: Some(config_path),
      checks: ["tracker-scope"],
      list_checks: False,
      output: doctor.Human,
    )
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(options, deps)
  let assert Some(result) = result_for(report, doctor.LinearTaskScope)
  assert result.status == doctor.Warn
  assert result.code == "linear_task_scope_overlap"
  assert field_value(result.fields, "task_scope_summary")
    == Some(
      "or(and(project(product-platform), any_label([workflow:implementation])), projects([customer-success, customer-support]))",
    )
  assert field_value(result.fields, "overlap_warning_count") == Some("3")
  let assert Some(first_warning) =
    field_value(result.fields, "first_overlap_warning")
  let assert Some(second_warning) =
    field_value(result.fields, "overlap_warning_2")
  let assert Some(third_warning) =
    field_value(result.fields, "overlap_warning_3")
  assert string.contains(first_warning, "or scope")
  assert string.contains(second_warning, "label-narrowed scope")
  assert string.contains(third_warning, "multi-project scope")

  assert service.start_doctor_with_dependencies(options, deps) == Ok(Nil)
  let assert Ok(ListWritten(output)) = process.receive(subject, within: 1000)
  assert string.contains(output, "! Tracker task scope")
  assert string.contains(
    output,
    "Linear task scope: or(and(project(product-platform), any_label([workflow:implementation])), projects([customer-success, customer-support]))",
  )
  assert string.contains(output, "Overlap warning: " <> first_warning)
  assert string.contains(output, "Overlap warning: " <> second_warning)
  assert string.contains(output, "Overlap warning: " <> third_warning)
}

pub fn doctor_invalid_tasks_from_failure_is_pathful_test() {
  let config_path =
    write_config_with_linear_fields(
      "test/tmp/doctor-invalid-tasks-from",
      "    api_key_env: HOME\n    tasks_from:\n      any_label: [workflow:implementation]\n",
      "",
    )
  let subject = process.new_subject()
  let deps = successful_deps(subject)
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(
      doctor.Options(
        path: Some(config_path),
        checks: ["workflow-config", "tracker-scope"],
        list_checks: False,
        output: doctor.Human,
      ),
      deps,
    )
  let assert Some(config_result) = result_for(report, doctor.WorkflowConfig)
  assert config_result.status == doctor.Fail
  assert string.contains(
    config_result.message,
    "tracker.linear.tasks_from.any_label would select labels across all projects",
  )
  let assert Some(scope_result) = result_for(report, doctor.LinearTaskScope)
  assert scope_result.status == doctor.Skip
}

pub fn doctor_legacy_publish_change_requirement_failure_is_actionable_test() {
  let config_path = write_config("test/tmp/doctor-legacy-publish-change", "")
  let assert Ok(Nil) =
    simplifile.write(
      "test/tmp/doctor-legacy-publish-change/workflows/implementation.yaml",
      "version: 1\nid: implementation\nworkspace:\n  requires: [publish-change]\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/implementation.md\n    run_in: main\n",
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
  assert string.contains(result.message, "publish-change was removed")
  assert string.contains(result.message, "publish-commit-stack")
  assert string.contains(
    result.message,
    "docs/runbooks/workspace-driver-migration.md",
  )

  assert service.start_doctor_with_dependencies(options, deps)
    == Error(service.StartupError(
      "doctor_failed",
      "one or more doctor checks failed",
    ))
  let assert Ok(ListWritten(output)) = process.receive(subject, within: 1000)
  assert string.contains(output, "Workflow config")
  assert string.contains(output, "publish-change was removed")
  assert string.contains(output, "publish-commit-stack")
  assert string.contains(output, "docs/runbooks/workspace-driver-migration.md")
}

pub fn doctor_old_tracker_state_key_failure_is_actionable_test() {
  let config_path =
    write_invalid_dispatch_config(
      "test/tmp/doctor-old-tracker-state-key",
      "  active_states: [Todo]\n",
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
  assert string.contains(result.message, "tracker.active_states")
  assert string.contains(result.message, "tracker.states.active")
  assert string.contains(result.message, "SCHERZO_YAML_SIMPLIFIED_V1")

  assert service.start_doctor_with_dependencies(options, deps)
    == Error(service.StartupError(
      "doctor_failed",
      "one or more doctor checks failed",
    ))
  let assert Ok(ListWritten(output)) = process.receive(subject, within: 1000)
  assert string.contains(output, "Workflow config")
  assert string.contains(output, "tracker.active_states")
  assert string.contains(output, "tracker.states.active")
  assert string.contains(output, "SCHERZO_YAML_SIMPLIFIED_V1")
}

pub fn doctor_wrong_type_ready_states_failure_is_actionable_test() {
  assert_doctor_dispatch_state_config_failure(
    "test/tmp/doctor-wrong-ready-states",
    "  states:\n    active: [Todo]\n    ready: Todo\n    terminal: [Done]\n",
    "tracker.states.ready must be a string list",
  )
}

pub fn doctor_non_string_ready_states_failure_is_actionable_test() {
  assert_doctor_dispatch_state_config_failure(
    "test/tmp/doctor-non-string-ready-states",
    "  states:\n    active: [Todo]\n    ready: [Todo, 123]\n    terminal: [Done]\n",
    "tracker.states.ready entries must be strings",
  )
}

pub fn doctor_empty_ready_states_failure_is_actionable_test() {
  assert_doctor_dispatch_state_config_failure(
    "test/tmp/doctor-empty-ready-states",
    "  states:\n    active: [Todo]\n    ready: []\n    terminal: [Done]\n",
    "tracker.states.ready must contain at least one state",
  )
}

pub fn doctor_out_of_subset_ready_states_failure_is_actionable_test() {
  assert_doctor_dispatch_state_config_failure(
    "test/tmp/doctor-subset-ready-states",
    "  states:\n    active: [Todo]\n    ready: [In Progress]\n    terminal: [Done]\n",
    "tracker.states.ready must be a subset of tracker.states.active",
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
  assert string.contains(result.message, "tracker.states.ready")
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

  let assert Error(retired_alias_err) =
    service.build_doctor_report_with_dependencies(
      doctor.Options(
        path: Some("test/tmp/no-such-scherzo.yaml"),
        checks: ["linear-smoke"],
        list_checks: False,
        output: doctor.Human,
      ),
      deps,
    )
  assert retired_alias_err.code == "unknown_doctor_check"
  assert string.contains(retired_alias_err.message, "linear-smoke")
  assert string.contains(retired_alias_err.message, "tracker-smoke")

  let assert Error(retired_contract_err) =
    service.build_doctor_report_with_dependencies(
      doctor.Options(
        path: Some("test/tmp/no-such-scherzo.yaml"),
        checks: ["linear-contract"],
        list_checks: False,
        output: doctor.Human,
      ),
      deps,
    )
  assert retired_contract_err.code == "unknown_doctor_check"
  assert string.contains(retired_contract_err.message, "linear-contract")
  assert string.contains(retired_contract_err.message, "tracker-contract")
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
      checks: ["tracker-smoke", "workspace-hooks", "pi-probe"],
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
  assert workspace_result.status == doctor.Pass
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
  let assert Ok(PrepareCalled(run_root, prepared_path, "noop")) =
    process.receive(subject, within: 1000)
  let assert Ok(PiCalled(pi_path)) = process.receive(subject, within: 1000)
  let assert Ok(CleanupCalled(cleaned_root, "noop")) =
    process.receive(subject, within: 1000)
  let assert Ok(LockReleased) = process.receive(subject, within: 1000)
  assert pi_path == prepared_path
  assert cleaned_root == run_root
}

pub fn doctor_reports_unsupported_top_level_legacy_hooks_test() {
  let config_path =
    write_top_level_hooks_config("test/tmp/doctor-top-level-hook-warning")
  let subject = process.new_subject()
  let deps = successful_deps(subject)
  let options =
    doctor.Options(
      path: Some(config_path),
      checks: ["workflow-config", "workspace-hooks"],
      list_checks: False,
      output: doctor.Human,
    )
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(options, deps)
  let assert Some(result) = result_for(report, doctor.WorkflowConfig)
  assert result.status == doctor.Fail
  assert result.code == "invalid_config"
  assert string.contains(result.message, "workspace.hooks")
  assert string.contains(result.message, "was removed")
  assert string.contains(
    result.message,
    "workspace.drivers.<name>.type: custom",
  )
  let assert Some(skipped) = result_for(report, doctor.WorkspaceHooks)
  assert skipped.status == doctor.Skip

  let assert Error(_) = service.start_doctor_with_dependencies(options, deps)
  let assert Some(output) = receive_list_written(subject)
  assert string.contains(output, "workspace.hooks")
  assert string.contains(output, "was removed")
}

pub fn doctor_reports_unsupported_profile_local_legacy_hooks_test() {
  let config_path =
    write_profile_hooks_config("test/tmp/doctor-profile-hook-warning")
  let subject = process.new_subject()
  let deps = successful_deps(subject)
  let options =
    doctor.Options(
      path: Some(config_path),
      checks: ["workflow-config", "workspace-hooks"],
      list_checks: False,
      output: doctor.Human,
    )
  let assert Ok(report) =
    service.build_doctor_report_with_dependencies(options, deps)
  let assert Some(result) = result_for(report, doctor.WorkflowConfig)
  assert result.status == doctor.Fail
  assert result.code == "invalid_config"
  assert string.contains(result.message, "workspace.drivers.noop.hooks")
  assert string.contains(result.message, "was removed")
  assert string.contains(result.message, "workspace.drivers.noop.type: custom")
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
  assert summary.warned == 1
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

pub fn doctor_pi_probe_cleanup_failure_reports_pi_probe_failure_test() {
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
  assert summary.failed == 1
  assert result_for(report, doctor.InstanceLock) == None
  assert result_for(report, doctor.WorkspaceHooks) == None
  let assert Some(pi_result) = result_for(report, doctor.PiProbe)
  assert pi_result.status == doctor.Fail
  assert pi_result.code == "workspace_cleanup_failed"
  assert field_value(pi_result.fields, "error") == Some("workspace_io")
  assert doctor.has_failures(report) == True
}

pub fn doctor_pi_probe_replaces_retired_mode_without_prompt_test() {
  let dir = "test/tmp/doctor-pi-probe"
  let transcript_path = dir <> "/transcript.jsonl"
  let assert Ok(transcript) = path.absolute(transcript_path)
  let config_path =
    write_config(
      dir,
      "  runtime:\n    type: pi\n    compatibility_check: true\n    pi:\n      executable: \""
        <> fake_pi()
        <> "\"\n      env:\n        FAKE_PI_TRANSCRIPT: \""
        <> transcript
        <> "\"\n",
    )
  let subject = process.new_subject()
  let deps =
    service.DoctorDependencies(
      ..successful_deps(subject),
      prepare_step: workspace_run.prepare_step,
      cleanup_run: workspace_run.cleanup_run,
      pi_probe: probe.probe_config,
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
  assert pi_result.code == "ok"
  let assert Some(workspace_path) =
    field_value(pi_result.fields, "workspace_path")
  assert string.contains(workspace_path, "SCHERZO-DOCTOR")
  let assert Ok(LockAcquired(_)) = process.receive(subject, within: 1000)
  let assert Ok(LockReleased) = process.receive(subject, within: 1000)
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
  let assert Ok(ListWritten("tracker-scope")) =
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
