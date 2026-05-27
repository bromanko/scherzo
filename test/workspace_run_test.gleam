import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/command_step
import scherzo/config
import scherzo/config/types as config_types
import scherzo/error
import scherzo/path
import scherzo/step_artifact
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_dag
import scherzo/workspace_driver_discovery
import scherzo/workspace_manifest
import scherzo/workspace_run
import simplifile
import support/test_helpers
import yay

fn env(_name: String) -> Option(String) {
  None
}

fn issue() -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "issue-id",
    identifier: "ABC-123",
    title: "Implement DAGs",
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked("Todo"),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn root(source: String) -> yay.Node {
  let assert Ok([document]) = yay.parse_string(source)
  yay.document_root(document)
}

fn chmod_path(path: String, mode: String) -> Nil {
  let artifact =
    command_step.run(
      "chmod",
      "chmod " <> mode <> " " <> path,
      ".",
      5000,
      [],
      test_helpers.default_artifact_limits(),
    )
  assert artifact.status == step_artifact.StepSucceeded
}

fn log_line_before(log: String, first: String, second: String) -> Bool {
  log_line_before_loop(string.split(log, on: "\n"), first, second, False)
}

fn log_line_before_loop(
  lines: List(String),
  first: String,
  second: String,
  seen_first: Bool,
) -> Bool {
  case lines {
    [] -> False
    [line, ..rest] -> {
      case string.contains(line, second) {
        True -> seen_first
        False ->
          log_line_before_loop(
            rest,
            first,
            second,
            seen_first || string.contains(line, first),
          )
      }
    }
  }
}

fn orchestrator(
  dir: String,
  _create_hook: String,
  _before_hook: String,
) -> config_types.OrchestratorConfig {
  write_lifecycle_driver(dir)
  let source =
    "version: 1\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\n  driver: default\n  drivers:\n    default:\n      type: custom\n      command: ./driver.sh\n      timeout: 5s\nworkflows:\n    implementation: workflows/implementation.yaml\n"
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(root(source), dir <> "/scherzo.yaml", env)
  let assert Ok(orchestrator) =
    workspace_driver_discovery.enrich_orchestrator(orchestrator)
  orchestrator
}

fn default_profile(
  orchestrator: config_types.OrchestratorConfig,
) -> config_types.WorkspaceHookProfile {
  let assert Ok(profile) =
    dict.get(orchestrator.workspace_profiles.profiles, "default")
  profile
}

fn named_profile(
  orchestrator: config_types.OrchestratorConfig,
  name: String,
) -> config_types.WorkspaceHookProfile {
  let assert Ok(profile) =
    dict.get(orchestrator.workspace_profiles.profiles, name)
  profile
}

fn no_driver_profile() -> config_types.WorkspaceHookProfile {
  config_types.WorkspaceHookProfile(
    name: "default",
    driver: None,
    source: config_types.SyntheticDefaultWorkspace,
  )
}

pub fn prepares_logical_workspace_paths_under_run_root_test() {
  let dir = "test/tmp/workspace-run-layout"
  test_helpers.reset_dir(dir)
  let orchestrator =
    orchestrator(
      dir,
      "mkdir -p \"$SCHERZO_WORKSPACE_PATH\"",
      "test -d \"$SCHERZO_WORKSPACE_PATH\"",
    )
  let assert Ok(main) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-1",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      default_profile(orchestrator),
      dict.new(),
    )
  let assert Ok(code_review_path) =
    workspace_run.workspace_path_for(
      issue(),
      "implementation",
      "run-1",
      "code-review",
      orchestrator,
    )
  let assert Ok(other_run) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-2",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      default_profile(orchestrator),
      dict.new(),
    )
  assert string.ends_with(
    main.path,
    "/workspaces/implementation/ABC-123/run-1/workspaces/main",
  )
  assert string.ends_with(
    code_review_path,
    "/workspaces/implementation/ABC-123/run-1/workspaces/code-review",
  )
  assert string.ends_with(
    other_run.path,
    "/workspaces/implementation/ABC-123/run-2/workspaces/main",
  )
  assert main.path != code_review_path
  assert main.path != other_run.path
}

fn driver_profile_orchestrator(dir: String) -> config_types.OrchestratorConfig {
  let source =
    "version: 1\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\n  driver: dogfood-jj\n  drivers:\n    dogfood-jj:\n      type: custom\n      command: ./driver.sh\n      timeout: 5s\nworkflows:\n    implementation: workflows/implementation.yaml\n"
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(root(source), dir <> "/scherzo.yaml", env)
  let assert Ok(orchestrator) =
    workspace_driver_discovery.enrich_orchestrator(orchestrator)
  orchestrator
}

fn write_lifecycle_driver(dir: String) -> Nil {
  let driver = dir <> "/driver.sh"
  let assert Ok(Nil) =
    simplifile.write(
      driver,
      "#!/bin/sh\nset -eu\nif [ \"$1 $2\" = 'describe --json' ]; then\n  printf '%s\\n' '{\"version\":1,\"capabilities\":[\"status\",\"assert-only\"]}'\n  exit 0\nfi\nop=\"$1 $2\"\nprintf '%s|pwd=%s|workspace=%s|run=%s|profile=%s|driver=%s|caps=%s\\n' \"$op\" \"$PWD\" \"$SCHERZO_WORKSPACE_PATH\" \"$SCHERZO_RUN_ROOT\" \"$SCHERZO_WORKSPACE_PROFILE\" \"$SCHERZO_WORKSPACE_DRIVER\" \"$SCHERZO_WORKSPACE_CAPABILITIES\" >> \"$SCHERZO_CONFIG_DIR/driver.log\"\ncase \"$op\" in\n  'lifecycle create') if [ -f \"$SCHERZO_CONFIG_DIR/create-fail-workspace\" ] && [ \"$(cat \"$SCHERZO_CONFIG_DIR/create-fail-workspace\")\" = \"$SCHERZO_WORKSPACE_NAME\" ]; then exit 17; fi; mkdir -p \"$SCHERZO_WORKSPACE_PATH\"; printf created > \"$SCHERZO_WORKSPACE_PATH/created\" ;;\n  'lifecycle before-step') if [ -f \"$SCHERZO_CONFIG_DIR/before-step-fail\" ]; then exit 19; fi; test -f \"$SCHERZO_WORKSPACE_PATH/created\" ;;\n  'lifecycle after-step') test -d \"$SCHERZO_WORKSPACE_PATH\" ;;\n  'lifecycle remove') test -d \"$SCHERZO_RUN_ROOT\"; if [ -f \"$SCHERZO_CONFIG_DIR/remove-fail-workspace\" ] && [ \"$(cat \"$SCHERZO_CONFIG_DIR/remove-fail-workspace\")\" = \"$SCHERZO_WORKSPACE_NAME\" ]; then exit 23; fi; rm -rf \"$SCHERZO_WORKSPACE_PATH\" ;;\n  *) exit 2 ;;\nesac\n",
    )
  test_helpers.chmod_executable(driver)
}

pub fn driver_profile_invokes_lifecycle_create_before_after_and_remove_test() {
  let dir = "test/tmp/workspace-run-driver-lifecycle"
  test_helpers.reset_dir(dir)
  write_lifecycle_driver(dir)
  let orchestrator = driver_profile_orchestrator(dir)
  let profile = named_profile(orchestrator, "dogfood-jj")
  let assert Ok(main) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-driver",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      profile,
      dict.new(),
    )
  let assert Ok(True) = simplifile.is_file(main.path <> "/created")

  workspace_run.after_step(issue(), "implement", main, orchestrator, profile)
  let assert Ok(Nil) =
    workspace_run.cleanup_run(main.run_root, orchestrator, profile)
  let assert Ok(False) = simplifile.is_directory(main.run_root)
  let assert Ok(log) = simplifile.read(orchestrator.config_dir <> "/driver.log")

  assert string.contains(log, "lifecycle create|")
  assert string.contains(log, "lifecycle before-step|")
  assert string.contains(log, "lifecycle after-step|")
  assert string.contains(log, "lifecycle remove|")
  assert string.contains(
    log,
    "|profile=dogfood-jj|driver=./driver.sh|caps=status assert-only",
  )
}

pub fn cleanup_invokes_remove_for_each_run_workspace_before_delete_test() {
  let dir = "test/tmp/workspace-run-cleanup-multiple-driver-workspaces"
  test_helpers.reset_dir(dir)
  write_lifecycle_driver(dir)
  let orchestrator = driver_profile_orchestrator(dir)
  let profile = named_profile(orchestrator, "dogfood-jj")
  let assert Ok(main) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-cleanup-multiple",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      profile,
      dict.new(),
    )
  let assert Ok(review) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-cleanup-multiple",
      "review",
      workflow_dag.WorkspaceRef(name: "review", from: Some("main")),
      orchestrator,
      profile,
      dict.from_list([#("main", main)]),
    )

  let assert Ok(Nil) =
    workspace_run.cleanup_run(main.run_root, orchestrator, profile)

  assert main.run_root == review.run_root
  let assert Ok(False) = simplifile.is_directory(main.run_root)
  let assert Ok(log) = simplifile.read(orchestrator.config_dir <> "/driver.log")
  assert string.contains(
    log,
    "lifecycle remove|pwd="
      <> orchestrator.config_dir
      <> "|workspace="
      <> main.path,
  )
  assert string.contains(
    log,
    "lifecycle remove|pwd="
      <> orchestrator.config_dir
      <> "|workspace="
      <> review.path,
  )
  assert log_line_before(
    log,
    "lifecycle remove|pwd="
      <> orchestrator.config_dir
      <> "|workspace="
      <> review.path,
    "lifecycle remove|pwd="
      <> orchestrator.config_dir
      <> "|workspace="
      <> main.path,
  )
}

pub fn cleanup_remove_failure_returns_error_and_keeps_run_root_test() {
  let dir = "test/tmp/workspace-run-cleanup-remove-failure"
  test_helpers.reset_dir(dir)
  write_lifecycle_driver(dir)
  let orchestrator = driver_profile_orchestrator(dir)
  let profile = named_profile(orchestrator, "dogfood-jj")
  let assert Ok(main) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-cleanup-failure",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      profile,
      dict.new(),
    )
  let assert Ok(review) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-cleanup-failure",
      "review",
      workflow_dag.WorkspaceRef(name: "review", from: Some("main")),
      orchestrator,
      profile,
      dict.from_list([#("main", main)]),
    )
  let assert Ok(Nil) =
    simplifile.write(
      orchestrator.config_dir <> "/remove-fail-workspace",
      "review\n",
    )

  let assert Error(error.WorkspaceIo(cleanup_error)) =
    workspace_run.cleanup_run(main.run_root, orchestrator, profile)

  assert string.contains(cleanup_error, "workspace review at " <> review.path)
  assert string.contains(cleanup_error, "hook_failed")
  assert string.contains(cleanup_error, "exited 23")
  assert main.run_root == review.run_root
  let assert Ok(True) = simplifile.is_directory(main.run_root)
  let assert Ok(True) = simplifile.is_directory(review.path)
  let assert Ok(log) = simplifile.read(orchestrator.config_dir <> "/driver.log")
  assert string.contains(log, "lifecycle remove|")
}

pub fn prepare_cleanup_failure_returns_cleanup_error_and_keeps_run_root_test() {
  let dir = "test/tmp/workspace-run-prepare-cleanup-failure"
  test_helpers.reset_dir(dir)
  write_lifecycle_driver(dir)
  let orchestrator = driver_profile_orchestrator(dir)
  let profile = named_profile(orchestrator, "dogfood-jj")
  let assert Ok(Nil) =
    simplifile.write(orchestrator.config_dir <> "/before-step-fail", "1")
  let assert Ok(Nil) =
    simplifile.write(
      orchestrator.config_dir <> "/remove-fail-workspace",
      "main\n",
    )

  let assert Error(workspace_run.WorkspaceFailure(error.WorkspaceIo(message))) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-prepare-cleanup-failure",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      profile,
      dict.new(),
    )

  assert string.contains(
    message,
    "cleanup after workspace prepare failure failed",
  )
  assert string.contains(message, "driver_lifecycle_before_step")
  assert string.contains(
    message,
    "driver lifecycle remove failed for workspace main",
  )
  let assert Ok(run_root) =
    workspace_run.run_root_for(
      issue(),
      "implementation",
      "run-prepare-cleanup-failure",
      orchestrator,
    )
  let assert Ok(workspace_path) =
    workspace_run.workspace_path_for(
      issue(),
      "implementation",
      "run-prepare-cleanup-failure",
      "main",
      orchestrator,
    )
  let assert Ok(True) = simplifile.is_directory(run_root)
  let assert Ok(True) = simplifile.is_directory(workspace_path)
}

pub fn scheduled_run_paths_and_hook_env_are_issue_free_test() {
  let dir = "test/tmp/workspace-run-scheduled"
  test_helpers.reset_dir(dir)
  let orchestrator =
    orchestrator(dir, "mkdir -p \"$SCHERZO_WORKSPACE_PATH\"", "")
  let run_id = "schedule-pr-conflict-repair-20260505T120000Z"
  let assert Ok(run_root) =
    workspace_run.scheduled_run_root_for(
      "pr-conflict-repair",
      "pr-conflict-repair",
      run_id,
      orchestrator,
    )
  let assert Ok(workspace_path) =
    workspace_run.scheduled_workspace_path_for_attempt(
      "pr-conflict-repair",
      "pr-conflict-repair",
      run_id,
      "inspect",
      1,
      "main",
      orchestrator,
    )
  assert string.ends_with(
    run_root,
    "/workspaces/pr-conflict-repair/scheduled/pr-conflict-repair/" <> run_id,
  )
  assert string.ends_with(workspace_path, "/workspaces/main")

  let prepared =
    workspace_run.PreparedStepWorkspace(
      workflow_id: "pr-conflict-repair",
      run_id: run_id,
      run_root: run_root,
      workflow_bundle_dir: ".scherzo/workflows",
      attempt_index: 1,
      workspace_name: "main",
      path: workspace_path,
      source_workspace_name: None,
      source_workspace_path: None,
      workspace_profile: "default",
    )
  let env =
    workspace_run.scheduled_hook_env(
      "pr-conflict-repair",
      "2026-05-05T12:00:00Z",
      "2026-05-05T12:00:03Z",
      1,
      "inspect",
      prepared,
      orchestrator,
    )
  assert dict.get(dict.from_list(env), "SCHERZO_RUN_KIND") == Ok("scheduled")
  assert dict.get(dict.from_list(env), "SCHERZO_SCHEDULED_JOB_ID")
    == Ok("pr-conflict-repair")
  assert dict.get(dict.from_list(env), "SCHERZO_ISSUE_ID") == Ok("")
  assert dict.get(dict.from_list(env), "SCHERZO_ISSUE_IDENTIFIER") == Ok("")
}

pub fn recovered_workspace_validation_rejects_paths_outside_run_root_test() {
  let dir = "test/tmp/workspace-run-recovered-validation"
  test_helpers.reset_dir(dir)
  let orchestrator =
    orchestrator(dir, "mkdir -p \"$SCHERZO_WORKSPACE_PATH\"", "")
  let assert Ok(expected_run_root) =
    workspace_run.run_root_for(issue(), "implementation", "run-1", orchestrator)
  let assert Ok(other_run_workspace) =
    workspace_run.workspace_path_for(
      issue(),
      "implementation",
      "run-2",
      "main",
      orchestrator,
    )
  let assert Ok(Nil) = simplifile.create_directory_all(other_run_workspace)
  let recovered_workspace =
    workspace_run.PreparedStepWorkspace(
      workflow_id: "implementation",
      run_id: "run-1",
      run_root: expected_run_root,
      workflow_bundle_dir: ".scherzo/workflows",
      attempt_index: 1,
      workspace_name: "main",
      path: other_run_workspace,
      source_workspace_name: None,
      source_workspace_path: None,
      workspace_profile: "default",
    )
  let known = dict.from_list([#("main", recovered_workspace)])

  let assert Error(workspace_run.WorkspaceFailure(error.WorkspaceIo(
    "invalid recovered workspace",
  ))) =
    workspace_run.prepare_recovered_step_attempt(
      issue(),
      "implementation",
      "run-1",
      expected_run_root,
      "reuse_main",
      2,
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      default_profile(orchestrator),
      known,
    )
  let assert Error(workspace_run.WorkspaceFailure(error.WorkspaceIo(
    "invalid recovered workspace",
  ))) =
    workspace_run.prepare_recovered_step_attempt(
      issue(),
      "implementation",
      "run-1",
      expected_run_root,
      "copy_main",
      1,
      workflow_dag.WorkspaceRef(name: "review", from: Some("main")),
      orchestrator,
      default_profile(orchestrator),
      known,
    )
}

pub fn cleanup_rejects_paths_outside_workspace_root_test() {
  let dir = "test/tmp/workspace-run-cleanup"
  test_helpers.reset_dir(dir)
  let orchestrator =
    orchestrator(dir, "mkdir -p \"$SCHERZO_WORKSPACE_PATH\"", "")
  let assert Error(error.WorkspaceOutsideRoot(_)) =
    workspace_run.cleanup_run(
      "/tmp/not-under-scherzo-root",
      orchestrator,
      default_profile(orchestrator),
    )
}

pub fn cleanup_retention_marker_inspect_failure_returns_error_test() {
  let dir = "test/tmp/workspace-run-retention-inspect-failure"
  test_helpers.reset_dir(dir)
  let orchestrator =
    orchestrator(dir, "mkdir -p \"$SCHERZO_WORKSPACE_PATH\"", "")
  let profile = default_profile(orchestrator)
  let assert Ok(main) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-retention-inspect-failure",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      profile,
      dict.new(),
    )
  let assert Ok(Nil) = simplifile.delete(main.run_root)
  let assert Ok(Nil) = simplifile.write(main.run_root, "not a directory")

  let assert Error(error.WorkspaceIo(message)) =
    workspace_run.cleanup_run(main.run_root, orchestrator, profile)

  assert string.contains(message, "inspect workspace retention marker failed")
  let assert Ok(True) = simplifile.is_file(main.run_root)
}

pub fn cleanup_delete_run_root_failure_returns_error_test() {
  let dir = "test/tmp/workspace-run-delete-failure"
  test_helpers.reset_dir(dir)
  let orchestrator =
    orchestrator(dir, "mkdir -p \"$SCHERZO_WORKSPACE_PATH\"", "")
  let profile = no_driver_profile()
  let assert Ok(main) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-delete-failure",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      profile,
      dict.new(),
    )

  let assert Ok(run_parent) = path.dirname(main.run_root)
  chmod_path(run_parent, "u-w")
  let cleanup_result =
    workspace_run.cleanup_run(main.run_root, orchestrator, profile)
  chmod_path(run_parent, "u+w")

  let assert Error(error.WorkspaceIo(message)) = cleanup_result
  assert string.contains(message, "delete workspace run root failed")
  let assert Ok(True) = simplifile.is_directory(main.run_root)
  let assert Ok(Nil) = simplifile.delete(main.run_root)
}

pub fn cleanup_retention_marker_skips_delete_until_removed_test() {
  let dir = "test/tmp/workspace-run-retained-cleanup"
  test_helpers.reset_dir(dir)
  let orchestrator =
    orchestrator(dir, "mkdir -p \"$SCHERZO_WORKSPACE_PATH\"", "")
  let assert Ok(main) =
    workspace_run.prepare_step(
      issue(),
      "execplan-implementation",
      "run-retained",
      "prepare_plan",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      default_profile(orchestrator),
      dict.new(),
    )
  let marker = workspace_run.cleanup_retention_marker(main.run_root)
  let assert Ok(Nil) = simplifile.write(marker, "keep until PR publish\n")
  let assert Ok(Nil) = simplifile.write(main.path <> "/work", "saved")

  let assert Ok(Nil) =
    workspace_run.cleanup_run(
      main.run_root,
      orchestrator,
      default_profile(orchestrator),
    )
  let assert Ok(True) = simplifile.is_directory(main.run_root)
  let assert Ok(True) = simplifile.is_file(main.path <> "/work")

  let assert Ok(Nil) = simplifile.delete(marker)
  let assert Ok(Nil) =
    workspace_run.cleanup_run(
      main.run_root,
      orchestrator,
      default_profile(orchestrator),
    )
  let assert Ok(False) = simplifile.is_directory(main.run_root)
}

pub fn managed_workspace_manifest_round_trip_and_upsert_test() {
  let contents =
    workspace_manifest.encode_manifest(
      [
        workspace_manifest.Entry(
          run_id: "run-1",
          workflow_id: "implementation",
          step_id: "review",
          attempt_index: 2,
          workspace_name: "main",
          relative_path: "workspaces/main",
          workspace_profile: "dogfood-jj",
          driver_command: "driver.sh",
          driver_capabilities: ["status", "assert-only"],
          source_workspace_name: Some("main"),
          source_workspace_relative_path: Some("workspaces/main"),
          state: workspace_manifest.Ready,
        ),
      ],
      "run-1",
      "implementation",
    )
  let assert Ok([entry]) = workspace_manifest.decode_manifest(contents)
  assert entry.step_id == "review"
  assert entry.attempt_index == 2
  assert entry.source_workspace_relative_path == Some("workspaces/main")

  let dir = "test/tmp/workspace-run-manifest-upsert"
  test_helpers.reset_dir(dir)
  let run_root = dir <> "/run"
  let assert Ok(Nil) = simplifile.create_directory_all(run_root)
  let planned =
    workspace_manifest.Entry(
      run_id: "run-1",
      workflow_id: "implementation",
      step_id: "prepare",
      attempt_index: 1,
      workspace_name: "main",
      relative_path: "workspaces/main",
      workspace_profile: "dogfood-jj",
      driver_command: "driver.sh",
      driver_capabilities: ["status"],
      source_workspace_name: None,
      source_workspace_relative_path: None,
      state: workspace_manifest.Planned,
    )
  let ready =
    workspace_manifest.Entry(
      run_id: planned.run_id,
      workflow_id: planned.workflow_id,
      step_id: planned.step_id,
      attempt_index: planned.attempt_index,
      workspace_name: planned.workspace_name,
      relative_path: planned.relative_path,
      workspace_profile: planned.workspace_profile,
      driver_command: planned.driver_command,
      driver_capabilities: planned.driver_capabilities,
      source_workspace_name: planned.source_workspace_name,
      source_workspace_relative_path: planned.source_workspace_relative_path,
      state: workspace_manifest.Ready,
    )
  let assert Ok(Nil) =
    workspace_manifest.write_entry(run_root, "run-1", "implementation", planned)
  let assert Ok(Nil) =
    workspace_manifest.write_entry(run_root, "run-1", "implementation", ready)
  let assert Ok(contents) =
    simplifile.read(workspace_manifest.manifest_path(run_root))
  let assert Ok([written]) = workspace_manifest.decode_manifest(contents)
  assert written.state == workspace_manifest.Ready
}

pub fn prepare_create_failure_leaves_planned_manifest_entry_test() {
  let dir = "test/tmp/workspace-run-create-failure-manifest"
  test_helpers.reset_dir(dir)
  write_lifecycle_driver(dir)
  let orchestrator = driver_profile_orchestrator(dir)
  let profile = named_profile(orchestrator, "dogfood-jj")
  let assert Ok(Nil) =
    simplifile.write(
      orchestrator.config_dir <> "/create-fail-workspace",
      "main",
    )
  let assert Ok(run_root) =
    workspace_run.run_root_for(
      issue(),
      "implementation",
      "run-create-failure",
      orchestrator,
    )

  let assert Error(workspace_run.HookFailure(error.HookFailed(_, 17, _))) =
    workspace_run.prepare_recovered_step_attempt(
      issue(),
      "implementation",
      "run-create-failure",
      run_root,
      "implement",
      1,
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      profile,
      dict.new(),
    )
  let assert Ok(contents) =
    simplifile.read(workspace_manifest.manifest_path(run_root))
  let assert Ok([entry]) = workspace_manifest.decode_manifest(contents)
  assert entry.state == workspace_manifest.Planned
  assert entry.relative_path == "workspaces/main"
}

pub fn cleanup_uses_manifest_entries_only_test() {
  let dir = "test/tmp/workspace-run-manifest-cleanup-only"
  test_helpers.reset_dir(dir)
  write_lifecycle_driver(dir)
  let orchestrator = driver_profile_orchestrator(dir)
  let profile = named_profile(orchestrator, "dogfood-jj")
  let assert Ok(main) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-cleanup-manifest",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      profile,
      dict.new(),
    )
  let assert Ok(review) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-cleanup-manifest",
      "review",
      workflow_dag.WorkspaceRef(name: "review", from: Some("main")),
      orchestrator,
      profile,
      dict.from_list([#("main", main)]),
    )
  let outside_source = dir <> "/outside-source"
  let other_outside = dir <> "/other-outside"
  let assert Ok(Nil) = simplifile.create_directory_all(outside_source)
  let assert Ok(Nil) = simplifile.create_directory_all(other_outside)
  let assert Ok(Nil) = simplifile.write(outside_source <> "/sentinel", "keep")
  let assert Ok(Nil) = simplifile.write(other_outside <> "/sentinel", "keep")
  let assert Ok(Nil) =
    path.symlink(outside_source, main.run_root <> "/workspaces/scherzo")
  let assert Ok(Nil) =
    path.symlink(other_outside, main.run_root <> "/workspaces/outside-symlink")
  let assert Ok(Nil) =
    simplifile.create_directory_all(main.run_root <> "/workspaces/unmanaged")

  let assert Ok(Nil) =
    workspace_run.cleanup_run(main.run_root, orchestrator, profile)

  let assert Ok(False) = simplifile.is_directory(main.run_root)
  let assert Ok(True) = simplifile.is_file(outside_source <> "/sentinel")
  let assert Ok(True) = simplifile.is_file(other_outside <> "/sentinel")
  let assert Ok(log) = simplifile.read(orchestrator.config_dir <> "/driver.log")
  assert string.contains(
    log,
    "lifecycle remove|pwd="
      <> orchestrator.config_dir
      <> "|workspace="
      <> main.path,
  )
  assert string.contains(
    log,
    "lifecycle remove|pwd="
      <> orchestrator.config_dir
      <> "|workspace="
      <> review.path,
  )
  assert !string.contains(log, "workspaces/scherzo")
  assert !string.contains(log, "workspaces/outside-symlink")
  assert !string.contains(log, "workspaces/unmanaged")
}

pub fn cleanup_missing_manifest_returns_error_and_keeps_run_root_test() {
  let dir = "test/tmp/workspace-run-missing-manifest"
  test_helpers.reset_dir(dir)
  write_lifecycle_driver(dir)
  let orchestrator = driver_profile_orchestrator(dir)
  let profile = named_profile(orchestrator, "dogfood-jj")
  let assert Ok(main) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-missing-manifest",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      profile,
      dict.new(),
    )
  let assert Ok(Nil) =
    simplifile.delete(workspace_manifest.manifest_path(main.run_root))

  let assert Error(error.WorkspaceIo("managed workspace manifest missing")) =
    workspace_run.cleanup_run(main.run_root, orchestrator, profile)
  let assert Ok(True) = simplifile.is_directory(main.run_root)
}

pub fn cleanup_invalid_manifest_path_returns_error_and_keeps_run_root_test() {
  let dir = "test/tmp/workspace-run-invalid-manifest"
  test_helpers.reset_dir(dir)
  write_lifecycle_driver(dir)
  let orchestrator = driver_profile_orchestrator(dir)
  let profile = named_profile(orchestrator, "dogfood-jj")
  let assert Ok(main) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-invalid-manifest",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      profile,
      dict.new(),
    )
  let assert Ok(Nil) =
    simplifile.write(
      workspace_manifest.manifest_path(main.run_root),
      workspace_manifest.encode_manifest(
        [
          workspace_manifest.Entry(
            run_id: "run-invalid-manifest",
            workflow_id: "implementation",
            step_id: "implement",
            attempt_index: 1,
            workspace_name: "main",
            relative_path: "../escape",
            workspace_profile: "dogfood-jj",
            driver_command: "./driver.sh",
            driver_capabilities: ["status", "assert-only"],
            source_workspace_name: None,
            source_workspace_relative_path: None,
            state: workspace_manifest.Ready,
          ),
        ],
        "run-invalid-manifest",
        "implementation",
      ),
    )

  let assert Error(error.WorkspaceIo("managed workspace path is unsafe")) =
    workspace_run.cleanup_run(main.run_root, orchestrator, profile)
  let assert Ok(True) = simplifile.is_directory(main.run_root)
}

fn ready_manifest_entry(
  run_id: String,
  workspace_name: String,
  relative_path: String,
) -> workspace_manifest.Entry {
  workspace_manifest.Entry(
    run_id: run_id,
    workflow_id: "implementation",
    step_id: "implement",
    attempt_index: 1,
    workspace_name: workspace_name,
    relative_path: relative_path,
    workspace_profile: "dogfood-jj",
    driver_command: "./driver.sh",
    driver_capabilities: ["status", "assert-only"],
    source_workspace_name: None,
    source_workspace_relative_path: None,
    state: workspace_manifest.Ready,
  )
}

fn write_manifest_entries(
  run_root: String,
  run_id: String,
  entries: List(workspace_manifest.Entry),
) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      workspace_manifest.manifest_path(run_root),
      workspace_manifest.encode_manifest(entries, run_id, "implementation"),
    )
  Nil
}

pub fn cleanup_rejects_manifest_driver_context_mismatch_test() {
  let dir = "test/tmp/workspace-run-driver-context-mismatch"
  test_helpers.reset_dir(dir)
  write_lifecycle_driver(dir)
  let orchestrator = driver_profile_orchestrator(dir)
  let profile = named_profile(orchestrator, "dogfood-jj")
  let assert Ok(main) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-driver-context-mismatch",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      profile,
      dict.new(),
    )
  write_manifest_entries(main.run_root, "run-driver-context-mismatch", [
    workspace_manifest.Entry(
      ..ready_manifest_entry(
        "run-driver-context-mismatch",
        "main",
        "workspaces/main",
      ),
      driver_command: "./other-driver.sh",
    ),
  ])

  let assert Error(error.WorkspaceIo(
    "managed workspace manifest driver context mismatch",
  )) = workspace_run.cleanup_run(main.run_root, orchestrator, profile)
  let assert Ok(True) = simplifile.is_directory(main.run_root)
  let assert Ok(log) = simplifile.read(orchestrator.config_dir <> "/driver.log")
  assert !string.contains(log, "lifecycle remove|")
}

pub fn cleanup_rejects_non_workspace_manifest_path_before_remove_hook_test() {
  let dir = "test/tmp/workspace-run-non-workspace-manifest"
  test_helpers.reset_dir(dir)
  write_lifecycle_driver(dir)
  let orchestrator = driver_profile_orchestrator(dir)
  let profile = named_profile(orchestrator, "dogfood-jj")
  let assert Ok(main) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-non-workspace-manifest",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      profile,
      dict.new(),
    )
  write_manifest_entries(main.run_root, "run-non-workspace-manifest", [
    ready_manifest_entry("run-non-workspace-manifest", "main", ".scherzo"),
  ])

  let assert Error(error.WorkspaceIo(
    "managed workspace path does not match workspace name",
  )) = workspace_run.cleanup_run(main.run_root, orchestrator, profile)
  write_manifest_entries(main.run_root, "run-non-workspace-manifest", [
    ready_manifest_entry(
      "run-non-workspace-manifest",
      "review",
      "workspaces/main",
    ),
  ])
  let assert Error(error.WorkspaceIo(
    "managed workspace path does not match workspace name",
  )) = workspace_run.cleanup_run(main.run_root, orchestrator, profile)
  let assert Ok(True) = simplifile.is_directory(main.run_root)
  let assert Ok(log) = simplifile.read(orchestrator.config_dir <> "/driver.log")
  assert !string.contains(log, "lifecycle remove|")
}

pub fn cleanup_rejects_manifest_realpath_escape_and_keeps_run_root_test() {
  let dir = "test/tmp/workspace-run-realpath-escape"
  test_helpers.reset_dir(dir)
  write_lifecycle_driver(dir)
  let orchestrator = driver_profile_orchestrator(dir)
  let profile = named_profile(orchestrator, "dogfood-jj")
  let assert Ok(main) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-realpath-escape",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      profile,
      dict.new(),
    )
  let outside = dir <> "/outside-target"
  let assert Ok(Nil) = simplifile.create_directory_all(outside)
  let assert Ok(Nil) = simplifile.write(outside <> "/sentinel", "keep")
  let assert Ok(Nil) = simplifile.delete(main.path)
  let assert Ok(outside_abs) = path.absolute(outside)
  let assert Ok(Nil) = path.symlink(outside_abs, main.path)

  let assert Error(error.WorkspaceIo(
    "managed workspace realpath escapes run root",
  )) = workspace_run.cleanup_run(main.run_root, orchestrator, profile)
  let assert Ok(True) = simplifile.is_directory(main.run_root)
  let assert Ok(True) = simplifile.is_file(outside <> "/sentinel")
  let assert Ok(log) = simplifile.read(orchestrator.config_dir <> "/driver.log")
  assert !string.contains(log, "lifecycle remove|")
}

pub fn cleanup_rejects_oversized_manifest_and_keeps_run_root_test() {
  let dir = "test/tmp/workspace-run-oversized-manifest"
  test_helpers.reset_dir(dir)
  write_lifecycle_driver(dir)
  let orchestrator = driver_profile_orchestrator(dir)
  let profile = named_profile(orchestrator, "dogfood-jj")
  let assert Ok(main) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-oversized-manifest",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      profile,
      dict.new(),
    )
  let assert Ok(Nil) =
    simplifile.write(
      workspace_manifest.manifest_path(main.run_root),
      string.repeat("x", times: workspace_manifest.max_manifest_bytes + 1),
    )

  let assert Error(error.WorkspaceIo("managed workspace manifest too large")) =
    workspace_run.cleanup_run(main.run_root, orchestrator, profile)
  let assert Ok(True) = simplifile.is_directory(main.run_root)
  let assert Ok(log) = simplifile.read(orchestrator.config_dir <> "/driver.log")
  assert !string.contains(log, "lifecycle remove|")
}
