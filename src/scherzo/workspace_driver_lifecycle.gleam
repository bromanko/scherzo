import gleam/list
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/hooks
import scherzo/path
import scherzo/workspace_driver_command
import scherzo/workspace_driver_env
import simplifile

pub fn supports(
  driver: config_types.WorkspaceDriverConfig,
  operation: config_types.WorkspaceLifecycleOperation,
) -> Bool {
  list.contains(driver.lifecycle, operation)
}

pub fn run_if_supported(
  name: String,
  operation: config_types.WorkspaceLifecycleOperation,
  driver: config_types.WorkspaceDriverConfig,
  orchestrator: config_types.OrchestratorConfig,
  env: List(#(String, String)),
) -> Result(Nil, error.HookError) {
  case supports(driver, operation) {
    False -> Ok(Nil)
    True -> run(name, operation, driver, orchestrator, env)
  }
}

pub fn run(
  name: String,
  operation: config_types.WorkspaceLifecycleOperation,
  driver: config_types.WorkspaceDriverConfig,
  orchestrator: config_types.OrchestratorConfig,
  env: List(#(String, String)),
) -> Result(Nil, error.HookError) {
  hooks.run_argv_with_env_redacting(
    name,
    workspace_driver_command.resolve(driver.command, orchestrator),
    lifecycle_args(operation),
    cwd(orchestrator),
    driver.timeout_ms,
    lifecycle_env(env, driver, orchestrator),
    workspace_driver_env.values_for_redaction(driver.env),
  )
}

pub fn run_best_effort(
  name: String,
  operation: config_types.WorkspaceLifecycleOperation,
  driver: config_types.WorkspaceDriverConfig,
  orchestrator: config_types.OrchestratorConfig,
  env: List(#(String, String)),
) -> Nil {
  case supports(driver, operation) {
    False -> Nil
    True -> {
      let _ =
        hooks.run_best_effort_argv_with_env_redacting(
          name,
          workspace_driver_command.resolve(driver.command, orchestrator),
          lifecycle_args(operation),
          cwd(orchestrator),
          driver.timeout_ms,
          lifecycle_env(env, driver, orchestrator),
          workspace_driver_env.values_for_redaction(driver.env),
        )
      Nil
    }
  }
}

pub fn remove_run(
  run_root: String,
  orchestrator: config_types.OrchestratorConfig,
  profile_name: String,
  driver: config_types.WorkspaceDriverConfig,
) -> Nil {
  case supports(driver, config_types.LifecycleRemove) {
    False -> Nil
    True -> {
      let workspaces_dir = path.join(run_root, "workspaces")
      case simplifile.read_directory(workspaces_dir) {
        Ok(entries) ->
          remove_entries(
            entries,
            workspaces_dir,
            run_root,
            orchestrator,
            profile_name,
            driver,
          )
        Error(_) -> Nil
      }
    }
  }
}

fn remove_entries(
  entries: List(String),
  workspaces_dir: String,
  run_root: String,
  orchestrator: config_types.OrchestratorConfig,
  profile_name: String,
  driver: config_types.WorkspaceDriverConfig,
) -> Nil {
  case entries {
    [] -> Nil
    [entry, ..rest] -> {
      let workspace_path = path.join(workspaces_dir, entry)
      case simplifile.is_directory(workspace_path) {
        Ok(True) ->
          run_best_effort(
            "driver_lifecycle_remove",
            config_types.LifecycleRemove,
            driver,
            orchestrator,
            remove_env(
              run_root,
              workspace_path,
              entry,
              profile_name,
              orchestrator,
            ),
          )
        _ -> Nil
      }
      remove_entries(
        rest,
        workspaces_dir,
        run_root,
        orchestrator,
        profile_name,
        driver,
      )
    }
  }
}

fn remove_env(
  run_root: String,
  workspace_path: String,
  workspace_name: String,
  profile_name: String,
  orchestrator: config_types.OrchestratorConfig,
) -> List(#(String, String)) {
  [
    #("SCHERZO_RUN_KIND", "issue"),
    #("SCHERZO_CONFIG_DIR", orchestrator.config_dir),
    #("SCHERZO_WORKFLOW_ID", ""),
    #("SCHERZO_RUN_ID", ""),
    #("SCHERZO_RUN_ROOT", run_root),
    #("SCHERZO_ISSUE_ID", ""),
    #("SCHERZO_ISSUE_IDENTIFIER", ""),
    #("SCHERZO_STEP_ID", ""),
    #("SCHERZO_ATTEMPT_INDEX", "0"),
    #("SCHERZO_WORKSPACE_ROOT", orchestrator.effective.workspace.root),
    #("SCHERZO_WORKSPACE_PROFILE", profile_name),
    #("SCHERZO_WORKSPACE_NAME", workspace_name),
    #("SCHERZO_WORKSPACE_PATH", workspace_path),
    #("SCHERZO_SOURCE_WORKSPACE_NAME", ""),
    #("SCHERZO_SOURCE_WORKSPACE_PATH", ""),
  ]
}

fn lifecycle_args(
  operation: config_types.WorkspaceLifecycleOperation,
) -> List(String) {
  ["lifecycle", config_types.workspace_lifecycle_operation_to_string(operation)]
}

fn lifecycle_env(
  env: List(#(String, String)),
  driver: config_types.WorkspaceDriverConfig,
  orchestrator: config_types.OrchestratorConfig,
) -> List(#(String, String)) {
  workspace_driver_env.merge(driver.env, [
    #(
      "SCHERZO_WORKSPACE_DRIVER",
      workspace_driver_command.resolve(driver.command, orchestrator),
    ),
    #(
      "SCHERZO_WORKSPACE_CAPABILITIES",
      config_types.workspace_capability_names(driver.capabilities)
        |> string.join(with: " "),
    ),
    #(
      "SCHERZO_REPO_ROOT",
      workspace_driver_command.default_repo_root(orchestrator),
    ),
    ..env
  ])
}

fn cwd(orchestrator: config_types.OrchestratorConfig) -> String {
  workspace_driver_command.inferred_repo_root(orchestrator.config_dir)
}
