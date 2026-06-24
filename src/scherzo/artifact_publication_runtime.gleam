import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/artifact_publication_config
import scherzo/artifact_publication_driver
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_planner
import scherzo/artifact_publication_recording
import scherzo/config/types as config_types
import scherzo/error
import scherzo/path
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_dag
import scherzo/workspace as workspace_core
import scherzo/workspace_driver_context
import scherzo/workspace_manifest
import scherzo/workspace_profile
import scherzo/workspace_run

pub fn driver_for_run(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
  profile: config_types.WorkspaceHookProfile,
) -> Option(artifact_publication_driver.WorkspacePublicationDriver) {
  case profile.driver, publication_workspace(prepared_workspaces) {
    Some(driver_config), Some(workspace) -> {
      let context = workspace_profile.driver_context(profile, orchestrator)
      Some(artifact_publication_driver.WorkspacePublicationDriver(
        workspace_path: workspace.path,
        command: context.driver,
        capabilities: context.capabilities,
        env: workspace_profile.driver_context_env_vars_with_generated(
          context,
          publication_driver_generated_env(issue, dag, orchestrator, workspace),
        ),
        redaction_values: workspace_profile.driver_context_redaction_values(
          context,
        ),
        timeout_ms: driver_config.timeout_ms,
      ))
    }
    _, _ -> None
  }
}

pub fn driver_for_retained_run(
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  workflow_bundle_dir: String,
  run_id: String,
  run_root: String,
  work: artifact_publication_planner.PublicationWork,
) -> Result(artifact_publication_driver.WorkspacePublicationDriver, String) {
  use entry <- result.try(retained_publication_workspace_entry(run_root))
  use capabilities <- result.try(manifest_capabilities(
    entry.driver_capabilities,
  ))
  use driver <- result.try(retained_workspace_driver_config(
    orchestrator,
    entry.workspace_profile,
  ))
  let workspace_path =
    path.absolute_or_original(path.join(run_root, entry.relative_path))
  let context =
    workspace_driver_context.Context(
      profile: entry.workspace_profile,
      driver: entry.driver_command,
      capabilities: capabilities,
      env: driver.env,
    )
  Ok(artifact_publication_driver.WorkspacePublicationDriver(
    workspace_path: workspace_path,
    command: entry.driver_command,
    capabilities: capabilities,
    env: workspace_profile.driver_context_env_vars_with_generated(
      context,
      retained_publication_driver_generated_env(
        work,
        dag,
        orchestrator,
        workflow_bundle_dir,
        run_id,
        run_root,
        entry,
        workspace_path,
      ),
    ),
    redaction_values: workspace_profile.driver_context_redaction_values(context),
    timeout_ms: driver.timeout_ms,
  ))
}

pub fn failures_require_workspace_retention(
  routes: List(artifact_publication_config.PublicationRoute),
  failures: List(artifact_publication_recording.PublicationFailure),
) -> Bool {
  list.any(failures, fn(failure) {
    failure_requires_workspace_retention(routes, failure)
  })
}

pub fn retention_reason_suffix(
  routes: List(artifact_publication_config.PublicationRoute),
  failures: List(artifact_publication_recording.PublicationFailure),
) -> String {
  case failures_require_workspace_retention(routes, failures) {
    True -> "; workspace_retained_for_commit_stack_publication_failure"
    False -> ""
  }
}

fn failure_requires_workspace_retention(
  routes: List(artifact_publication_config.PublicationRoute),
  failure: artifact_publication_recording.PublicationFailure,
) -> Bool {
  list.any(routes, fn(route) {
    route.id == failure.publication_id
    && route.mode == artifact_publication_config.CommitStackPublication
  })
}

fn retained_publication_workspace_entry(
  run_root: String,
) -> Result(workspace_manifest.Entry, String) {
  case workspace_manifest.read_entries(run_root) {
    Error(err) ->
      Error(
        "retained workspace manifest unavailable for commit_stack retry: "
        <> workspace_error_message(err),
      )
    Ok(entries) ->
      case preferred_workspace_manifest_entry(entries) {
        Some(entry) -> Ok(entry)
        None ->
          Error(
            "retained workspace manifest has no workspace entries for commit_stack retry",
          )
      }
  }
}

pub fn preferred_workspace_manifest_entry(
  entries: List(workspace_manifest.Entry),
) -> Option(workspace_manifest.Entry) {
  case list.find(entries, fn(entry) { entry.workspace_name == "main" }) {
    Ok(entry) -> Some(entry)
    Error(Nil) ->
      case entries {
        [entry, ..] -> Some(entry)
        [] -> None
      }
  }
}

pub fn retained_workspace_path_from_run_root(
  run_root: String,
) -> Option(String) {
  case workspace_manifest.read_entries(run_root) {
    Ok(entries) ->
      case preferred_workspace_manifest_entry(entries) {
        Some(entry) ->
          Some(
            path.absolute_or_original(path.join(run_root, entry.relative_path)),
          )
        None -> None
      }
    Error(_) -> None
  }
}

pub fn publication_manifest_is_commit_stack(
  manifest: artifact_publication_manifest.PublicationManifest,
) -> Bool {
  case manifest.publication_mode {
    Some("commit_stack") -> True
    _ ->
      case manifest.dry_run_manifest {
        Some(planned) ->
          case planned.commit_stack {
            Some(_) -> True
            None -> False
          }
        None -> False
      }
  }
}

fn manifest_capabilities(
  names: List(String),
) -> Result(List(config_types.WorkspaceCapability), String) {
  manifest_capabilities_loop(names, [])
}

fn manifest_capabilities_loop(
  names: List(String),
  acc: List(config_types.WorkspaceCapability),
) -> Result(List(config_types.WorkspaceCapability), String) {
  case names {
    [] -> Ok(config_types.canonical_workspace_capabilities(list.reverse(acc)))
    [name, ..rest] ->
      case config_types.workspace_capability_from_string(name) {
        Ok(capability) -> manifest_capabilities_loop(rest, [capability, ..acc])
        Error(Nil) ->
          Error(
            "retained workspace manifest has unknown driver capability: "
            <> name,
          )
      }
  }
}

fn retained_workspace_driver_config(
  orchestrator: config_types.OrchestratorConfig,
  profile_name: String,
) -> Result(config_types.WorkspaceDriverConfig, String) {
  case dict.get(orchestrator.workspace_profiles.profiles, profile_name) {
    Error(Nil) ->
      Error(
        "retained workspace profile is not configured for commit_stack retry: "
        <> profile_name,
      )
    Ok(profile) ->
      case profile.driver {
        Some(driver) -> Ok(driver)
        None ->
          Error(
            "retained workspace profile has no driver for commit_stack retry: "
            <> profile_name,
          )
      }
  }
}

fn workspace_error_message(err: error.WorkspaceError) -> String {
  case err {
    error.WorkspaceOutsideRoot(path_) ->
      "workspace path is outside the workspace root: " <> path_
    error.WorkspaceIo(message) -> message
    error.PartialWorkspace(path_) ->
      "workspace is partially prepared: " <> path_
    error.UnsafeWorkspaceKey(key) -> "unsafe workspace key: " <> key
    error.WorkspaceCollision(message) -> message
  }
}

fn publication_workspace(
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
) -> Option(workspace_run.PreparedStepWorkspace) {
  case dict.get(prepared_workspaces, "main") {
    Ok(workspace) -> Some(workspace)
    Error(Nil) ->
      case dict.values(prepared_workspaces) {
        [workspace] -> Some(workspace)
        _ -> None
      }
  }
}

fn publication_driver_generated_env(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  workspace: workspace_run.PreparedStepWorkspace,
) -> List(#(String, String)) {
  [
    #("SCHERZO_CONFIG_DIR", orchestrator.config_dir),
    #("SCHERZO_WORKFLOW_ID", dag.id),
    #("SCHERZO_WORKFLOW_BUNDLE_DIR", workspace.workflow_bundle_dir),
    #("SCHERZO_RUN_ID", workspace.run_id),
    #("SCHERZO_RUN_ROOT", workspace.run_root),
    #("SCHERZO_RUN_KIND", "issue"),
    #("SCHERZO_ISSUE_ID", issue.id),
    #("SCHERZO_ISSUE_IDENTIFIER", issue.identifier),
    #("SCHERZO_WORKSPACE_ROOT", orchestrator.effective.workspace.root),
    #("SCHERZO_WORKSPACE_NAME", workspace.workspace_name),
    #("SCHERZO_WORKSPACE_PATH", workspace.path),
    #(
      "SCHERZO_SOURCE_WORKSPACE_NAME",
      optional_publication_env_value(workspace_core.source_name(
        workspace.source,
      )),
    ),
    #(
      "SCHERZO_SOURCE_WORKSPACE_PATH",
      optional_publication_env_value(workspace_core.source_path(
        workspace.source,
      )),
    ),
  ]
}

fn retained_publication_driver_generated_env(
  work: artifact_publication_planner.PublicationWork,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  workflow_bundle_dir: String,
  run_id: String,
  run_root: String,
  entry: workspace_manifest.Entry,
  workspace_path: String,
) -> List(#(String, String)) {
  [
    #("SCHERZO_CONFIG_DIR", orchestrator.config_dir),
    #("SCHERZO_WORKFLOW_ID", dag.id),
    #("SCHERZO_WORKFLOW_BUNDLE_DIR", workflow_bundle_dir),
    #("SCHERZO_RUN_ID", run_id),
    #("SCHERZO_RUN_ROOT", run_root),
    #("SCHERZO_RUN_KIND", "issue"),
    #("SCHERZO_ISSUE_ID", work.id),
    #("SCHERZO_ISSUE_IDENTIFIER", work.identifier),
    #("SCHERZO_WORKSPACE_ROOT", orchestrator.effective.workspace.root),
    #("SCHERZO_WORKSPACE_NAME", entry.workspace_name),
    #("SCHERZO_WORKSPACE_PATH", workspace_path),
    #(
      "SCHERZO_SOURCE_WORKSPACE_NAME",
      optional_publication_env_value(workspace_core.source_name(entry.source)),
    ),
    #(
      "SCHERZO_SOURCE_WORKSPACE_PATH",
      optional_publication_env_value(optional_source_workspace_path(
        run_root,
        workspace_core.source_path(entry.source),
      )),
    ),
  ]
}

fn optional_source_workspace_path(
  run_root: String,
  relative_path: Option(String),
) -> Option(String) {
  case relative_path {
    Some(relative_path) ->
      Some(path.absolute_or_original(path.join(run_root, relative_path)))
    None -> None
  }
}

fn optional_publication_env_value(value: Option(String)) -> String {
  case value {
    Some(value) -> value
    None -> ""
  }
}
