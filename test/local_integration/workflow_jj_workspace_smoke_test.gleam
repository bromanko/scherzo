import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import scherzo/artifact_publication_config
import scherzo/command_step
import scherzo/config
import scherzo/config/types as config_types
import scherzo/model_config
import scherzo/path
import scherzo/step_artifact
import scherzo/tracker
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import scherzo/workflow_dag
import scherzo/workflow_run
import simplifile
import support/test_helpers

fn issue() -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "issue-id",
    identifier: "ABC-123",
    title: "JJ smoke",
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked("Todo"),
    branch_name: None,
    url: None,
    labels: ["workflow:smoke"],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn effective(workspace_root: String) -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    tracker: config_types.TrackerConfig(
      kind: tracker_kind.LinearTracker,
      endpoint: "https://api.linear.app/graphql",
      api_key: Some("test-key"),
      project_slug: Some("TEST"),
      task_scope: None,
      active_states: issue_state.list_from_strings(["Todo"]),
      dispatch_states: issue_state.list_from_strings(["Todo"]),
      terminal_states: issue_state.list_from_strings(["Done"]),
    ),
    polling: config.default_polling_config(),
    workspace: config_types.WorkspaceConfig(root: workspace_root),
    control: config.default_control_config(),
    ledger_compaction: config.default_ledger_compaction_config(),
    hooks: config.default_hooks_config(),
    agent: config.default_agent_config(),
    pi: config_types.PiConfig(
      ..config.default_pi_config(),
      compatibility_probe: False,
    ),
    handoff: config.default_handoff_config(),
    linear_contract: config.default_linear_contract_config(),
    linear_commands: config.default_linear_command_config(),
    ui_server: config.default_ui_server_config(),
  )
}

fn driver_orchestrator(
  root: String,
  repo: String,
  script: String,
) -> config_types.OrchestratorConfig {
  let workspace_root = root <> "/workspaces"
  let config_dir = repo <> "/.scherzo"
  let assert Ok(Nil) = simplifile.create_directory_all(config_dir)
  let driver =
    config_types.WorkspaceDriverConfig(
      command: script,
      lifecycle: [
        config_types.LifecycleCreate,
        config_types.LifecycleBeforeStep,
        config_types.LifecycleAfterStep,
        config_types.LifecycleRemove,
      ],
      capabilities: [
        config_types.WorkspaceStatus,
        config_types.WorkspaceDiff,
        config_types.WorkspaceChangedFiles,
        config_types.WorkspaceAssertOnly,
      ],
      timeout_ms: 20_000,
      env: [],
    )
  config_types.OrchestratorConfig(
    effective: effective(workspace_root),
    config_dir: config_dir,
    routing: config_types.RoutingConfig(
      workflow_label_prefix: "workflow:",
      require_exactly_one_workflow_label: True,
      default_workflow: None,
      workflows: dict.from_list([#("smoke", "smoke.yaml")]),
    ),
    dag_hooks: config_types.empty_dag_hooks(),
    workspace_profiles: config_types.WorkspaceHookProfiles(
      default_profile: "dogfood-jj",
      profiles: dict.from_list([
        #(
          "dogfood-jj",
          config_types.WorkspaceHookProfile(
            name: "dogfood-jj",
            driver: Some(driver),
            source: config_types.ConfiguredWorkspaceDriver,
          ),
        ),
      ]),
    ),
    artifact_limits: test_helpers.default_artifact_limits(),
    artifact_repositories: artifact_publication_config.empty_repositories(),
    model_settings: model_config.default_settings(),
    scheduled_jobs: [],
  )
}

fn empty_tracker() -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([]) },
  )
}

fn driver_dag() -> workflow_dag.WorkflowDag {
  dag_source("workspace:\n  driver: dogfood-jj\n")
}

fn dag_source(profile_line: String) -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: smoke\n"
      <> profile_line
      <> "steps:\n  - id: first\n    kind: command\n    run: printf '%s\\n' \"$PWD\" > smoke.log; printf '%s|%s|%s\\n' \"$PWD\" \"$SCHERZO_RUN_ROOT\" \"$SCHERZO_STEP_ID\"\n    run_in: main\n  - id: second\n    kind: command\n    depends_on: [first]\n    run: read first_pwd < smoke.log; test \"$first_pwd\" = \"$PWD\"; printf '%s|%s|%s\\n' \"$PWD\" \"$SCHERZO_RUN_ROOT\" \"$SCHERZO_STEP_ID\"\n    run_in: main\n",
    )
  dag
}

fn reset_dir(root: String) -> Nil {
  let _ = simplifile.delete(root)
  let assert Ok(Nil) = simplifile.create_directory_all(root <> "/config")
  Nil
}

fn setup_jj_repo(root: String) -> String {
  let repo = absolute(root <> "/repo")
  let setup =
    command_step.run(
      "setup_jj_repo",
      "mkdir -p repo && cd repo && jj git init --colocate . && printf 'hello\\n' > file.txt && jj file track file.txt && jj describe -m initial && jj bookmark set main -r @ && jj git export && jj git remote add origin . && jj git remote add scherzo-agent .",
      root,
      20_000,
      [],
      test_helpers.default_artifact_limits(),
    )
  assert setup.status == step_artifact.StepSucceeded
  repo
}

pub fn workflow_jj_workspace_driver_lifecycle_reuses_main_and_cleans_up_smoke_test() {
  let root = "test/tmp/workflow-jj-workspace-driver-smoke"
  reset_dir(root)
  let repo = setup_jj_repo(root)
  let script = absolute("scripts/scherzo-workspace-jj")
  let orch = driver_orchestrator(root, repo, script)
  let assert Ok(success) =
    workflow_run.execute(
      issue(),
      driver_dag(),
      orch,
      empty_tracker(),
      [],
      "run-1",
      workflow_run.default_dependencies(),
    )

  let assert Ok(first) = dict.get(success.artifacts, "first")
  let assert Ok(second) = dict.get(success.artifacts, "second")
  let first_fields = string.split(string.trim(first.stdout), on: "|")
  let second_fields = string.split(string.trim(second.stdout), on: "|")
  let assert [first_workspace, first_run_root, "first"] = first_fields
  let assert [second_workspace, second_run_root, "second"] = second_fields
  assert first_workspace == second_workspace
  assert string.ends_with(
    first_workspace,
    "/workspaces/smoke/ABC-123/run-1/workspaces/main",
  )
  assert first_run_root == second_run_root
  assert first_run_root == success.run_root
  assert simplifile.is_directory(success.run_root) == Ok(False)

  let list =
    command_step.run(
      "jj_workspace_driver_list",
      "jj --repository \"" <> repo <> "\" workspace list --color=never",
      ".",
      20_000,
      [],
      test_helpers.default_artifact_limits(),
    )
  assert list.status == step_artifact.StepSucceeded
  assert !string.contains(list.stdout, "scherzo-smoke-ABC-123-run-1-main")
}

fn absolute(value: String) -> String {
  path.absolute(value) |> result_unwrap(value)
}

fn result_unwrap(result: Result(a, b), default: a) -> a {
  case result {
    Ok(value) -> value
    Error(_) -> default
  }
}
