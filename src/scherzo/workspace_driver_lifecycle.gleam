import gleam/int
import gleam/list
import gleam/result
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
) -> Result(Nil, error.WorkspaceError) {
  case supports(driver, config_types.LifecycleRemove) {
    False -> Ok(Nil)
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
        Error(simplifile.Enoent) -> Ok(Nil)
        Error(file_error) ->
          Error(error.WorkspaceIo(
            "read workspaces failed: " <> simplifile.describe_error(file_error),
          ))
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
) -> Result(Nil, error.WorkspaceError) {
  case entries {
    [] -> Ok(Nil)
    [entry, ..rest] -> {
      let workspace_path = path.join(workspaces_dir, entry)
      use _ <- result.try(remove_entry(
        workspace_path,
        entry,
        run_root,
        orchestrator,
        profile_name,
        driver,
      ))
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

fn remove_entry(
  workspace_path: String,
  workspace_name: String,
  run_root: String,
  orchestrator: config_types.OrchestratorConfig,
  profile_name: String,
  driver: config_types.WorkspaceDriverConfig,
) -> Result(Nil, error.WorkspaceError) {
  case simplifile.is_directory(workspace_path) {
    Ok(True) ->
      run(
        "driver_lifecycle_remove",
        config_types.LifecycleRemove,
        driver,
        orchestrator,
        remove_env(
          run_root,
          workspace_path,
          workspace_name,
          profile_name,
          orchestrator,
        ),
      )
      |> result.map_error(fn(err) {
        driver_remove_error(workspace_name, workspace_path, err)
      })
    Ok(False) -> Ok(Nil)
    Error(simplifile.Enoent) -> Ok(Nil)
    Error(file_error) ->
      Error(error.WorkspaceIo(
        "inspect workspace failed: " <> simplifile.describe_error(file_error),
      ))
  }
}

fn driver_remove_error(
  workspace_name: String,
  workspace_path: String,
  err: error.HookError,
) -> error.WorkspaceError {
  error.WorkspaceIo(
    "driver lifecycle remove failed for workspace "
    <> workspace_name
    <> " at "
    <> workspace_path
    <> ": "
    <> hook_error_detail(err),
  )
}

fn hook_error_detail(err: error.HookError) -> String {
  case err {
    error.HookFailed(name, status, diagnostics) -> {
      let detail = name <> " exited " <> int.to_string(status)
      case string.trim(diagnostics) == "" {
        True -> error.hook_code(err) <> ": " <> detail
        False -> error.hook_code(err) <> ": " <> detail <> ": " <> diagnostics
      }
    }
    error.HookTimedOut(name) ->
      error.hook_code(err) <> ": " <> name <> " timed out"
    error.HookIo(message) -> error.hook_code(err) <> ": " <> message
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
    #("SCHERZO_WORKFLOW_BUNDLE_DIR", ""),
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
