import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import scherzo/artifact_publication_config
import scherzo/config
import scherzo/config/types as config_types
import scherzo/error
import scherzo/hooks
import scherzo/model_config
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import scherzo/workspace_driver_lifecycle
import simplifile
import support/test_helpers

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
    hooks: config.default_hooks_config(),
    agent: config.default_agent_config(),
    pi: config.default_pi_config(),
    handoff: config.default_handoff_config(),
    linear_contract: config.default_linear_contract_config(),
    linear_commands: config.default_linear_command_config(),
    ui_server: config.default_ui_server_config(),
  )
}

fn orchestrator(dir: String) -> config_types.OrchestratorConfig {
  config_types.OrchestratorConfig(
    effective: effective(dir),
    config_dir: dir,
    routing: config_types.RoutingConfig(
      workflow_label_prefix: "workflow:",
      require_exactly_one_workflow_label: True,
      default_workflow: None,
      workflows: dict.new(),
    ),
    dag_hooks: config_types.empty_dag_hooks(),
    workspace_profiles: config_types.WorkspaceHookProfiles(
      default_profile: "noop",
      profiles: dict.new(),
    ),
    artifact_limits: config_types.ArtifactLimits(
      command_stream_max_chars: 4000,
      template_field_max_chars: 4000,
      workflow_summary_max_chars: 4000,
    ),
    artifact_repositories: artifact_publication_config.empty_repositories(),
    model_settings: model_config.default_settings(),
    scheduled_jobs: [],
  )
}

fn driver(env: List(#(String, String))) -> config_types.WorkspaceDriverConfig {
  config_types.WorkspaceDriverConfig(
    command: "./driver.sh",
    lifecycle: [
      config_types.LifecycleCreate,
      config_types.LifecycleBeforeStep,
      config_types.LifecycleAfterStep,
      config_types.LifecycleRemove,
    ],
    capabilities: [config_types.WorkspaceStatus],
    timeout_ms: 1000,
    env: env,
  )
}

fn env(dir: String) -> List(#(String, String)) {
  [
    #("SCHERZO_RUN_KIND", "issue"),
    #("SCHERZO_CONFIG_DIR", dir),
    #("SCHERZO_WORKFLOW_ID", "workflow"),
    #("SCHERZO_RUN_ID", "run"),
    #("SCHERZO_RUN_ROOT", dir <> "/run"),
    #("SCHERZO_ISSUE_ID", "issue-id"),
    #("SCHERZO_ISSUE_IDENTIFIER", "ABC-123"),
    #("SCHERZO_STEP_ID", "step"),
    #("SCHERZO_ATTEMPT_INDEX", "0"),
    #("SCHERZO_WORKSPACE_ROOT", dir <> "/workspaces"),
    #("SCHERZO_WORKSPACE_PROFILE", "profile"),
    #("SCHERZO_WORKSPACE_NAME", "main"),
    #("SCHERZO_WORKSPACE_PATH", dir <> "/workspace"),
    #("SCHERZO_SOURCE_WORKSPACE_NAME", ""),
    #("SCHERZO_SOURCE_WORKSPACE_PATH", ""),
  ]
}

fn write_driver(dir: String, body: String) -> Nil {
  test_helpers.reset_dir(dir)
  let path = dir <> "/driver.sh"
  let assert Ok(Nil) = simplifile.write(path, body)
  test_helpers.chmod_executable(path)
}

pub fn lifecycle_invocations_receive_profile_env_test() {
  let dir = "test/tmp/workspace-driver-lifecycle-env"
  let log_path = dir <> "/driver.log"
  write_driver(
    dir,
    "#!/bin/sh\n"
      <> "echo $2:SCHERZO_JJ_WORKSPACE_BASE=$SCHERZO_JJ_WORKSPACE_BASE:SCHERZO_WORKSPACE_DRIVER=$SCHERZO_WORKSPACE_DRIVER:SCHERZO_WORKSPACE_PATH=$SCHERZO_WORKSPACE_PATH >> "
      <> test_helpers.shell_quote("driver.log")
      <> "\n",
  )
  let orchestrator = orchestrator(dir)
  let driver = driver([#("SCHERZO_JJ_WORKSPACE_BASE", "profile-base")])

  let assert Ok(Nil) =
    workspace_driver_lifecycle.run(
      "driver_lifecycle_create",
      config_types.LifecycleCreate,
      driver,
      orchestrator,
      env(dir),
    )
  let assert Ok(Nil) =
    workspace_driver_lifecycle.run(
      "driver_lifecycle_before_step",
      config_types.LifecycleBeforeStep,
      driver,
      orchestrator,
      env(dir),
    )
  let assert Ok(Nil) =
    workspace_driver_lifecycle.run(
      "driver_lifecycle_after_step",
      config_types.LifecycleAfterStep,
      driver,
      orchestrator,
      env(dir),
    )
  let assert Ok(Nil) =
    workspace_driver_lifecycle.run(
      "driver_lifecycle_remove",
      config_types.LifecycleRemove,
      driver,
      orchestrator,
      env(dir),
    )

  let assert Ok(log) = simplifile.read(log_path)
  assert string.contains(log, "create:SCHERZO_JJ_WORKSPACE_BASE=profile-base")
  assert string.contains(
    log,
    "before-step:SCHERZO_JJ_WORKSPACE_BASE=profile-base",
  )
  assert string.contains(
    log,
    "after-step:SCHERZO_JJ_WORKSPACE_BASE=profile-base",
  )
  assert string.contains(log, "remove:SCHERZO_JJ_WORKSPACE_BASE=profile-base")
  assert string.contains(log, "SCHERZO_WORKSPACE_DRIVER=./driver.sh")
  assert string.contains(log, "SCHERZO_WORKSPACE_PATH=" <> dir <> "/workspace")
}

pub fn lifecycle_invokes_path_installed_packaged_jj_command_name_test() {
  let dir = "test/tmp/workspace-driver-lifecycle-packaged-jj"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let driver_path = dir <> "/bin/scherzo-workspace-jj"
  let log_path = dir <> "/driver.log"
  let assert Ok(Nil) =
    simplifile.write(
      driver_path,
      "#!/bin/sh\nname=${0##*/}\nprintf '%s %s %s\\n' \"$name\" \"$1\" \"$2\" >> driver.log\n",
    )
  test_helpers.chmod_executable(driver_path)
  let orchestrator = orchestrator(dir)
  let driver =
    config_types.WorkspaceDriverConfig(
      command: "scherzo-workspace-jj",
      lifecycle: [config_types.LifecycleCreate, config_types.LifecycleRemove],
      capabilities: [],
      timeout_ms: 1000,
      env: [#("PATH", "./bin")],
    )

  let assert Ok(Nil) =
    workspace_driver_lifecycle.run(
      "driver_lifecycle_create",
      config_types.LifecycleCreate,
      driver,
      orchestrator,
      env(dir),
    )
  let assert Ok(Nil) =
    workspace_driver_lifecycle.run(
      "driver_lifecycle_remove",
      config_types.LifecycleRemove,
      driver,
      orchestrator,
      env(dir),
    )

  let assert Ok(log) = simplifile.read(log_path)
  assert string.contains(log, "scherzo-workspace-jj lifecycle create")
  assert string.contains(log, "scherzo-workspace-jj lifecycle remove")
}

pub fn lifecycle_run_discards_driver_stdout_test() {
  let dir = "test/tmp/workspace-driver-lifecycle-stdout"
  write_driver(
    dir,
    "#!/bin/sh\nprintf 'driver stdout before exit\\n'\nsleep 0.1\n",
  )
  let orchestrator = orchestrator(dir)
  let driver = driver([])
  let _ = drain_any_port_data_messages()

  let assert Ok(Nil) =
    workspace_driver_lifecycle.run(
      "driver_lifecycle_create",
      config_types.LifecycleCreate,
      driver,
      orchestrator,
      env(dir),
    )

  assert drain_any_port_data_messages() == 0
}

pub fn lifecycle_failures_redact_sensitive_profile_env_test() {
  let dir = "test/tmp/workspace-driver-lifecycle-redaction"
  write_driver(dir, "#!/bin/sh\necho token=$DRIVER_SECRET_TOKEN >&2\nexit 9\n")
  let orchestrator = orchestrator(dir)
  let driver = driver([#("DRIVER_SECRET_TOKEN", "driver-env-redaction-token")])

  let assert Error(error.HookFailed(_, _, diagnostics)) =
    workspace_driver_lifecycle.run(
      "driver_lifecycle_create",
      config_types.LifecycleCreate,
      driver,
      orchestrator,
      env(dir),
    )
  assert string.contains(diagnostics, "[REDACTED]")
  assert !string.contains(diagnostics, "driver-env-redaction-token")
}

pub fn lifecycle_best_effort_logs_redact_sensitive_profile_env_test() {
  let dir = "test/tmp/workspace-driver-lifecycle-best-effort-redaction"
  write_driver(dir, "#!/bin/sh\necho token=$DRIVER_SECRET_TOKEN >&2\nexit 9\n")
  let message =
    hooks.run_best_effort_argv_with_env_redacting(
      "driver_lifecycle_remove",
      "./driver.sh",
      ["lifecycle", "remove"],
      dir,
      1000,
      [#("DRIVER_SECRET_TOKEN", "driver-env-redaction-token"), ..env(dir)],
      ["driver-env-redaction-token"],
    )
  assert string.contains(message, "[REDACTED]")
  assert !string.contains(message, "driver-env-redaction-token")
}

@external(erlang, "scherzo_test_ffi", "drain_any_port_data_messages")
fn drain_any_port_data_messages() -> Int
