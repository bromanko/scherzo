import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/artifact_publication_config
import scherzo/config/types as config_types
import scherzo/workflow_dag
import scherzo/workspace_profile

pub type Diagnostic {
  Diagnostic(
    workflow_id: String,
    publication_id: String,
    required: Bool,
    profile_name: String,
    provided: List(config_types.WorkspaceCapability),
  )
}

pub type PreflightError {
  PreflightError(Diagnostic)
}

pub fn validate_required(
  orchestrator: config_types.OrchestratorConfig,
  workflows: List(#(String, workflow_dag.WorkflowDag)),
) -> Result(Nil, PreflightError) {
  case first_required_diagnostic(orchestrator, workflows) {
    None -> Ok(Nil)
    Some(diagnostic) -> Error(PreflightError(diagnostic))
  }
}

pub fn optional_diagnostics(
  orchestrator: config_types.OrchestratorConfig,
  workflows: List(#(String, workflow_dag.WorkflowDag)),
) -> List(Diagnostic) {
  diagnostics_for_workflows(orchestrator, workflows, False, [])
}

pub fn error_code(error: PreflightError) -> String {
  case error {
    PreflightError(diagnostic) -> diagnostic_code(diagnostic)
  }
}

pub fn error_message(error: PreflightError) -> String {
  case error {
    PreflightError(diagnostic) -> diagnostic_message(diagnostic)
  }
}

pub fn diagnostic_code(diagnostic: Diagnostic) -> String {
  case diagnostic.required {
    True -> "commit_stack_publication_driver_unsupported"
    False -> "optional_commit_stack_publication_driver_unsupported"
  }
}

pub fn diagnostic_message(diagnostic: Diagnostic) -> String {
  let required_text = case diagnostic.required {
    True -> "requires"
    False -> "declares optional"
  }
  let action_text = case diagnostic.required {
    True ->
      "select a workspace driver that advertises publish-change or publish-commit-stack before running this workflow, or mark the publication required: false if it is optional"
    False ->
      "optional publication will not be publishable until the selected workspace driver advertises publish-change or publish-commit-stack"
  }
  "workflow "
  <> diagnostic.workflow_id
  <> " publication "
  <> diagnostic.publication_id
  <> " "
  <> required_text
  <> " same-repo commit_stack publication but workspace.driver "
  <> diagnostic.profile_name
  <> " provides "
  <> config_types.workspace_capabilities_to_string(diagnostic.provided)
  <> "; "
  <> action_text
}

fn first_required_diagnostic(
  orchestrator: config_types.OrchestratorConfig,
  workflows: List(#(String, workflow_dag.WorkflowDag)),
) -> Option(Diagnostic) {
  case diagnostics_for_workflows(orchestrator, workflows, True, []) {
    [] -> None
    [diagnostic, ..] -> Some(diagnostic)
  }
}

fn diagnostics_for_workflows(
  orchestrator: config_types.OrchestratorConfig,
  workflows: List(#(String, workflow_dag.WorkflowDag)),
  required: Bool,
  acc: List(Diagnostic),
) -> List(Diagnostic) {
  case workflows {
    [] -> list.reverse(acc)
    [#(workflow_id, dag), ..rest] -> {
      let diagnostics =
        diagnostics_for_dag(orchestrator, workflow_id, dag, required)
        |> list.reverse
      diagnostics_for_workflows(
        orchestrator,
        rest,
        required,
        list.append(diagnostics, acc),
      )
    }
  }
}

fn diagnostics_for_dag(
  orchestrator: config_types.OrchestratorConfig,
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
  required: Bool,
) -> List(Diagnostic) {
  diagnostics_for_routes(
    orchestrator,
    workflow_id,
    dag,
    dag.publication_routes,
    required,
    [],
  )
}

fn diagnostics_for_routes(
  orchestrator: config_types.OrchestratorConfig,
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
  routes: List(artifact_publication_config.PublicationRoute),
  required: Bool,
  acc: List(Diagnostic),
) -> List(Diagnostic) {
  case routes {
    [] -> list.reverse(acc)
    [route, ..rest] -> {
      let acc = case route_requires_commit_stack_publication(route, required) {
        False -> acc
        True ->
          case diagnostic_for_route(orchestrator, workflow_id, dag, route) {
            None -> acc
            Some(diagnostic) -> [diagnostic, ..acc]
          }
      }
      diagnostics_for_routes(
        orchestrator,
        workflow_id,
        dag,
        rest,
        required,
        acc,
      )
    }
  }
}

fn route_requires_commit_stack_publication(
  route: artifact_publication_config.PublicationRoute,
  required: Bool,
) -> Bool {
  route.required == required
  && route.mode == artifact_publication_config.CommitStackPublication
}

fn diagnostic_for_route(
  orchestrator: config_types.OrchestratorConfig,
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
  route: artifact_publication_config.PublicationRoute,
) -> Option(Diagnostic) {
  let profile_name = workspace_profile.selected_name(dag, orchestrator)
  case dict.get(orchestrator.workspace_profiles.profiles, profile_name) {
    Error(Nil) -> None
    Ok(profile) -> {
      let provided = workspace_profile_capabilities(profile)
      case supports_commit_stack_publication(provided) {
        True -> None
        False ->
          Some(Diagnostic(
            workflow_id: workflow_id,
            publication_id: route.id,
            required: route.required,
            profile_name: profile_name,
            provided: provided,
          ))
      }
    }
  }
}

fn supports_commit_stack_publication(
  provided: List(config_types.WorkspaceCapability),
) -> Bool {
  list.contains(provided, config_types.WorkspacePublishChange)
  || list.contains(provided, config_types.WorkspacePublishCommitStack)
}

fn workspace_profile_capabilities(
  profile: config_types.WorkspaceHookProfile,
) -> List(config_types.WorkspaceCapability) {
  case profile.driver {
    Some(driver) -> driver.capabilities
    None -> []
  }
}
