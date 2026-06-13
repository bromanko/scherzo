import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/artifact_publication_config
import scherzo/artifact_publication_planner
import scherzo/artifact_publication_runtime
import scherzo/config
import scherzo/config/types as config_types
import scherzo/model_config
import scherzo/path
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import scherzo/workflow_dag
import scherzo/workspace_manifest
import simplifile
import support/test_helpers

pub fn retained_run_driver_uses_manifest_workspace_and_configured_profile_test() {
  let root = "test/tmp/artifact-publication-runtime/retained-driver"
  let run_root = root <> "/runs/run-1"
  let workspace = run_root <> "/workspaces/main"
  let source_workspace = run_root <> "/workspaces/source"
  test_helpers.reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  let assert Ok(Nil) = simplifile.create_directory_all(source_workspace)
  write_retained_workspace_manifest(run_root)

  let assert Ok(driver) =
    artifact_publication_runtime.driver_for_retained_run(
      workflow(),
      orchestrator(root),
      root <> "/workflows",
      "run-1",
      run_root,
      artifact_publication_planner.PublicationWork(
        kind: artifact_publication_planner.TaskWork,
        id: "issue-1",
        identifier: "LIV-917",
        slug: "LIV-917",
        title: Some("Retained driver"),
        url: Some("https://linear.example/LIV-917"),
      ),
    )

  let workspace_abs = path.absolute_or_original(workspace)
  assert driver.workspace_path == workspace_abs
  assert driver.command == "fake-retained-driver"
  assert driver.timeout_ms == 777
  assert list.contains(driver.capabilities, config_types.WorkspacePublishChange)
  assert env_value(driver.env, "SCHERZO_WORKSPACE_PATH") == Some(workspace_abs)
  assert env_value(driver.env, "SCHERZO_WORKSPACE_NAME") == Some("main")
  assert env_value(driver.env, "SCHERZO_RUN_ROOT") == Some(run_root)
  assert env_value(driver.env, "STATIC_DRIVER_ENV") == Some("kept")
}

fn write_retained_workspace_manifest(run_root: String) -> Nil {
  let assert Ok(Nil) = simplifile.create_directory_all(run_root <> "/.scherzo")
  let assert Ok(Nil) =
    simplifile.write(
      workspace_manifest.manifest_path(run_root),
      workspace_manifest.encode_manifest(
        [
          workspace_manifest.Entry(
            run_id: "run-1",
            workflow_id: "implementation",
            step_id: "implement",
            attempt_index: 1,
            workspace_name: "main",
            relative_path: "workspaces/main",
            workspace_profile: "dogfood-jj",
            driver_command: "fake-retained-driver",
            driver_capabilities: ["publish-change"],
            source_workspace_name: Some("source"),
            source_workspace_relative_path: Some("workspaces/source"),
            state: workspace_manifest.Ready,
          ),
        ],
        "run-1",
        "implementation",
      ),
    )
  Nil
}

fn workflow() -> workflow_dag.WorkflowDag {
  workflow_dag.WorkflowDag(
    id: "implementation",
    description: None,
    workspace_profile: None,
    workspace_capabilities: [],
    max_parallel_steps: 1,
    recover: None,
    steps: [],
    contract: None,
    publication_routes: [],
    workstream_phase: None,
  )
}

fn orchestrator(root: String) -> config_types.OrchestratorConfig {
  let driver =
    config_types.WorkspaceDriverConfig(
      command: "configured-driver-command",
      lifecycle: [],
      capabilities: [],
      timeout_ms: 777,
      env: [#("STATIC_DRIVER_ENV", "kept")],
    )
  config_types.OrchestratorConfig(
    effective: effective(root),
    config_dir: root,
    routing: config_types.RoutingConfig(
      workflow_label_prefix: "workflow:",
      require_exactly_one_workflow_label: True,
      default_workflow: None,
      workflows: dict.new(),
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
    artifact_limits: config_types.ArtifactLimits(
      command_stream_max_chars: 1000,
      template_field_max_chars: 1000,
      workflow_summary_max_chars: 4000,
    ),
    artifact_repositories: artifact_publication_config.empty_repositories(),
    model_settings: model_config.default_settings(),
    scheduled_jobs: [],
  )
}

fn effective(root: String) -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    tracker: config_types.TrackerConfig(
      kind: tracker_kind.LinearTracker,
      endpoint: "https://api.linear.app/graphql",
      api_key: Some("test-key"),
      project_slug: Some("TEST"),
      active_states: issue_state.list_from_strings(["Todo"]),
      dispatch_states: issue_state.list_from_strings(["Todo"]),
      terminal_states: issue_state.list_from_strings(["Done"]),
    ),
    polling: config.default_polling_config(),
    workspace: config_types.WorkspaceConfig(root: root <> "/workspaces"),
    control: config.default_control_config(),
    hooks: config.default_hooks_config(),
    agent: config.default_agent_config(),
    pi: config.default_pi_config(),
    handoff: config.default_handoff_config(),
    linear_contract: config.default_linear_contract_config(),
    linear_commands: config.default_linear_command_config(),
    ui_server: config.default_ui_server_config(),
  )
}

fn env_value(env: List(#(String, String)), key: String) -> Option(String) {
  case list.key_find(env, key) {
    Ok(value) -> Some(value)
    Error(Nil) -> None
  }
}
