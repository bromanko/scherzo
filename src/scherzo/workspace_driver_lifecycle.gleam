import gleam/int
import gleam/list
import gleam/option.{type Option, unwrap}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/hooks
import scherzo/workspace as workspace_core
import scherzo/workspace_driver_command
import scherzo/workspace_driver_env
import scherzo/workspace_manifest

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
      let operator_visible_log =
        hooks.run_best_effort_argv_with_env_redacting(
          name,
          workspace_driver_command.resolve(driver.command, orchestrator),
          lifecycle_args(operation),
          cwd(orchestrator),
          driver.timeout_ms,
          lifecycle_env(env, driver, orchestrator),
          workspace_driver_env.values_for_redaction(driver.env),
        )
      acknowledge_best_effort_log(operator_visible_log)
    }
  }
}

fn acknowledge_best_effort_log(_operator_visible_log: String) -> Nil {
  Nil
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
      use entries <- result.try(workspace_manifest.cleanup_entries(
        run_root,
        profile_name,
        workspace_driver_command.resolve(driver.command, orchestrator),
        config_types.workspace_capability_names(driver.capabilities),
      ))
      remove_entries(list.reverse(entries), run_root, orchestrator, driver)
    }
  }
}

fn remove_entries(
  entries: List(workspace_manifest.CleanupEntry),
  run_root: String,
  orchestrator: config_types.OrchestratorConfig,
  driver: config_types.WorkspaceDriverConfig,
) -> Result(Nil, error.WorkspaceError) {
  case entries {
    [] -> Ok(Nil)
    [entry, ..rest] -> {
      use _ <- result.try(remove_entry(entry, run_root, orchestrator, driver))
      remove_entries(rest, run_root, orchestrator, driver)
    }
  }
}

fn remove_entry(
  cleanup_entry: workspace_manifest.CleanupEntry,
  run_root: String,
  orchestrator: config_types.OrchestratorConfig,
  driver: config_types.WorkspaceDriverConfig,
) -> Result(Nil, error.WorkspaceError) {
  let workspace_manifest.CleanupEntry(
    entry: entry,
    workspace_path: workspace_path,
    source_workspace_path: source_workspace_path,
    exists: exists,
  ) = cleanup_entry
  case exists {
    False -> Ok(Nil)
    True ->
      run(
        "driver_lifecycle_remove",
        config_types.LifecycleRemove,
        driver,
        orchestrator,
        remove_env(
          run_root,
          workspace_path,
          source_workspace_path,
          entry,
          orchestrator,
        ),
      )
      |> result.map_error(fn(err) {
        driver_remove_error(entry.workspace_name, workspace_path, err)
      })
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
  source_workspace_path: Option(String),
  entry: workspace_manifest.Entry,
  orchestrator: config_types.OrchestratorConfig,
) -> List(#(String, String)) {
  [
    #("SCHERZO_RUN_KIND", "issue"),
    #("SCHERZO_CONFIG_DIR", orchestrator.config_dir),
    #("SCHERZO_WORKFLOW_ID", entry.workflow_id),
    #("SCHERZO_WORKFLOW_BUNDLE_DIR", ""),
    #("SCHERZO_RUN_ID", entry.run_id),
    #("SCHERZO_RUN_ROOT", run_root),
    #("SCHERZO_ISSUE_ID", ""),
    #("SCHERZO_ISSUE_IDENTIFIER", ""),
    #("SCHERZO_STEP_ID", entry.step_id),
    #("SCHERZO_ATTEMPT_INDEX", int.to_string(entry.attempt_index)),
    #("SCHERZO_WORKSPACE_ROOT", orchestrator.effective.workspace.root),
    #("SCHERZO_WORKSPACE_PROFILE", entry.workspace_profile),
    #("SCHERZO_WORKSPACE_NAME", entry.workspace_name),
    #("SCHERZO_WORKSPACE_PATH", workspace_path),
    #(
      "SCHERZO_SOURCE_WORKSPACE_NAME",
      unwrap(workspace_core.source_name(entry.source), ""),
    ),
    #("SCHERZO_SOURCE_WORKSPACE_PATH", unwrap(source_workspace_path, "")),
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
