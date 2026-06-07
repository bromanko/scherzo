import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/artifact_publication_config
import scherzo/artifact_publication_driver
import scherzo/artifact_publication_recording
import scherzo/config/types as config_types
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_dag
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
      optional_publication_env_value(workspace.source_workspace_name),
    ),
    #(
      "SCHERZO_SOURCE_WORKSPACE_PATH",
      optional_publication_env_value(workspace.source_workspace_path),
    ),
  ]
}

fn optional_publication_env_value(value: Option(String)) -> String {
  case value {
    Some(value) -> value
    None -> ""
  }
}
