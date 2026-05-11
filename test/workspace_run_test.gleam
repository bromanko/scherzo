import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/command_step
import scherzo/config
import scherzo/config/types as config_types
import scherzo/error
import scherzo/step_artifact
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_dag
import scherzo/workspace_run
import simplifile
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

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 4000,
    template_field_max_chars: 4000,
    workflow_summary_max_chars: 4000,
  )
}

fn chmod_executable(path: String) -> Nil {
  let artifact =
    command_step.run("chmod", "chmod +x " <> path, ".", 5000, [], limits())
  assert artifact.status == step_artifact.StepSucceeded
}

fn orchestrator(
  dir: String,
  create_hook: String,
  before_hook: String,
) -> config_types.OrchestratorConfig {
  let source =
    "version: 1\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\n  dispatch_states: [Todo]\nworkspace:\n  root: workspaces\n  hooks:\n    create: |\n"
    <> indent(create_hook)
    <> "    before_step: |\n"
    <> indent(before_hook)
    <> "    timeout_ms: 5000\nrouting:\n  workflows:\n    implementation: workflows/implementation.yaml\n"
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(root(source), dir <> "/scherzo.yaml", env)
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

fn profile_orchestrator(dir: String) -> config_types.OrchestratorConfig {
  let source =
    "version: 1\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\n  dispatch_states: [Todo]\nworkspace:\n  root: workspaces\n  default_profile: isolated\n  profiles:\n    isolated:\n      hooks:\n        create: |\n          mkdir -p \"$SCHERZO_WORKSPACE_PATH\"\n          touch \"$SCHERZO_WORKSPACE_PATH/isolated\"\n        remove: |\n          printf '%s' \"$SCHERZO_WORKSPACE_PROFILE\" > remove-profile.log\n        timeout_ms: 5000\n    noop:\n      hooks:\n        create: |\n          mkdir -p \"$SCHERZO_WORKSPACE_PATH\"\n          touch \"$SCHERZO_WORKSPACE_PATH/noop\"\n        remove: |\n          printf '%s' \"$SCHERZO_WORKSPACE_PROFILE\" > remove-profile.log\n        timeout_ms: 5000\nrouting:\n  workflows:\n    implementation: workflows/implementation.yaml\n"
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(root(source), dir <> "/scherzo.yaml", env)
  orchestrator
}

fn indent(script: String) -> String {
  script
  |> string.split(on: "\n")
  |> list_map(fn(line) { "      " <> line <> "\n" })
  |> string.join(with: "")
}

pub fn prepares_logical_workspace_paths_under_run_root_test() {
  let dir = "test/tmp/workspace-run-layout"
  reset_dir(dir)
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

pub fn hook_receives_step_environment_and_config_cwd_test() {
  let dir = "test/tmp/workspace-run-hooks"
  reset_dir(dir)
  let create_hook =
    "mkdir -p \"$SCHERZO_WORKSPACE_PATH\"\nprintf '%s|%s|%s|%s|%s|%s|%s|%s|%s\\n' \"$PWD\" \"$SCHERZO_CONFIG_DIR\" \"$SCHERZO_RUN_ROOT\" \"$SCHERZO_STEP_ID\" \"$SCHERZO_WORKSPACE_PROFILE\" \"$SCHERZO_WORKSPACE_NAME\" \"$SCHERZO_WORKSPACE_PATH\" \"$SCHERZO_SOURCE_WORKSPACE_NAME\" \"$SCHERZO_SOURCE_WORKSPACE_PATH\" >> hook.log\nif [ -n \"$SCHERZO_SOURCE_WORKSPACE_PATH\" ]; then cp \"$SCHERZO_SOURCE_WORKSPACE_PATH/marker\" \"$SCHERZO_WORKSPACE_PATH/from-source\"; fi"
  let before_hook = "pwd > \"$SCHERZO_WORKSPACE_PATH/before-cwd\""
  let orchestrator = orchestrator(dir, create_hook, before_hook)
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
  let assert Ok(Nil) = simplifile.write(main.path <> "/marker", "copied")
  let known = dict.from_list([#("main", main)])
  let assert Ok(review) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-1",
      "code_review",
      workflow_dag.WorkspaceRef(name: "code-review", from: Some("main")),
      orchestrator,
      default_profile(orchestrator),
      known,
    )
  let assert Ok(hook_log) =
    simplifile.read(orchestrator.config_dir <> "/hook.log")
  assert string.contains(hook_log, "|code_review|default|code-review|")
  assert string.contains(hook_log, "|main|")
  assert string.contains(hook_log, main.path)
  let assert Ok(copied) = simplifile.read(review.path <> "/from-source")
  assert copied == "copied"
  let assert Ok(before_cwd) = simplifile.read(review.path <> "/before-cwd")
  assert string.trim(before_cwd) == orchestrator.config_dir

  let assert Ok(next_main) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-1",
      "test_after_implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      default_profile(orchestrator),
      known,
    )
  assert next_main.path == main.path
  assert next_main.source_workspace_name == Some("main")
  assert next_main.source_workspace_path == Some(main.path)
}

pub fn selected_profile_uses_selected_hook_bodies_test() {
  let dir = "test/tmp/workspace-run-selected-profile"
  reset_dir(dir)
  let orchestrator = profile_orchestrator(dir)
  let noop = named_profile(orchestrator, "noop")
  let isolated = named_profile(orchestrator, "isolated")
  let assert Ok(noop_workspace) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-noop",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      noop,
      dict.new(),
    )
  let assert Ok(True) = simplifile.is_file(noop_workspace.path <> "/noop")
  let assert Ok(False) = simplifile.is_file(noop_workspace.path <> "/isolated")

  let assert Ok(isolated_workspace) =
    workspace_run.prepare_step(
      issue(),
      "implementation",
      "run-isolated",
      "implement",
      workflow_dag.WorkspaceRef(name: "main", from: None),
      orchestrator,
      isolated,
      dict.new(),
    )
  let assert Ok(True) =
    simplifile.is_file(isolated_workspace.path <> "/isolated")
  let assert Ok(False) = simplifile.is_file(isolated_workspace.path <> "/noop")

  let assert Ok(Nil) =
    workspace_run.cleanup_run(noop_workspace.run_root, orchestrator, noop)
  let assert Ok(remove_profile) =
    simplifile.read(orchestrator.config_dir <> "/remove-profile.log")
  assert remove_profile == "noop"
}

fn driver_profile_orchestrator(dir: String) -> config_types.OrchestratorConfig {
  let source =
    "version: 1\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\n  dispatch_states: [Todo]\nworkspace:\n  root: workspaces\n  default_profile: dogfood-jj\n  profiles:\n    dogfood-jj:\n      driver:\n        command: ./driver.sh\n        lifecycle: [create, before-step, after-step, remove]\n        capabilities: [status, assert-only]\n        timeout_ms: 5000\nrouting:\n  workflows:\n    implementation: workflows/implementation.yaml\n"
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(root(source), dir <> "/scherzo.yaml", env)
  orchestrator
}

fn write_lifecycle_driver(dir: String) -> Nil {
  let driver = dir <> "/driver.sh"
  let assert Ok(Nil) =
    simplifile.write(
      driver,
      "#!/bin/sh\nset -eu\nop=\"$1 $2\"\nprintf '%s|pwd=%s|workspace=%s|run=%s|profile=%s|driver=%s|caps=%s\\n' \"$op\" \"$PWD\" \"$SCHERZO_WORKSPACE_PATH\" \"$SCHERZO_RUN_ROOT\" \"$SCHERZO_WORKSPACE_PROFILE\" \"$SCHERZO_WORKSPACE_DRIVER\" \"$SCHERZO_WORKSPACE_CAPABILITIES\" >> \"$SCHERZO_CONFIG_DIR/driver.log\"\ncase \"$op\" in\n  'lifecycle create') mkdir -p \"$SCHERZO_WORKSPACE_PATH\"; printf created > \"$SCHERZO_WORKSPACE_PATH/created\" ;;\n  'lifecycle before-step') test -f \"$SCHERZO_WORKSPACE_PATH/created\" ;;\n  'lifecycle after-step') test -d \"$SCHERZO_WORKSPACE_PATH\" ;;\n  'lifecycle remove') rm -rf \"$SCHERZO_WORKSPACE_PATH\" ;;\n  *) exit 2 ;;\nesac\n",
    )
  chmod_executable(driver)
}

pub fn driver_profile_invokes_lifecycle_create_before_after_and_remove_test() {
  let dir = "test/tmp/workspace-run-driver-lifecycle"
  reset_dir(dir)
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

pub fn scheduled_run_paths_and_hook_env_are_issue_free_test() {
  let dir = "test/tmp/workspace-run-scheduled"
  reset_dir(dir)
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
  reset_dir(dir)
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
  reset_dir(dir)
  let orchestrator =
    orchestrator(dir, "mkdir -p \"$SCHERZO_WORKSPACE_PATH\"", "")
  let assert Error(error.WorkspaceOutsideRoot(_)) =
    workspace_run.cleanup_run(
      "/tmp/not-under-scherzo-root",
      orchestrator,
      default_profile(orchestrator),
    )
}

pub fn cleanup_retention_marker_skips_delete_until_removed_test() {
  let dir = "test/tmp/workspace-run-retained-cleanup"
  reset_dir(dir)
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

fn list_map(values: List(a), mapper: fn(a) -> b) -> List(b) {
  case values {
    [] -> []
    [value, ..rest] -> [mapper(value), ..list_map(rest, mapper)]
  }
}
