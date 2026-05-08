import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/workflow_dag

pub type ProfileResolutionError {
  UnknownWorkspaceProfile(
    workflow_id: String,
    profile_name: String,
    available: List(String),
  )
}

pub fn selected_name(
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
) -> String {
  case dag.workspace_profile {
    Some(profile) -> profile
    None -> orchestrator.workspace_profiles.default_profile
  }
}

pub fn resolve(
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(config_types.WorkspaceHookProfile, ProfileResolutionError) {
  let profile_name = selected_name(dag, orchestrator)
  case dict.get(orchestrator.workspace_profiles.profiles, profile_name) {
    Ok(profile) -> Ok(profile)
    Error(_) ->
      Error(UnknownWorkspaceProfile(
        workflow_id: dag.id,
        profile_name: profile_name,
        available: available_names(orchestrator),
      ))
  }
}

pub fn error_message(error: ProfileResolutionError) -> String {
  case error {
    UnknownWorkspaceProfile(workflow_id, profile_name, available) ->
      "workflow "
      <> workflow_id
      <> " selects unknown workspace_profile "
      <> profile_name
      <> "; available profiles: "
      <> available_names_to_string(available)
  }
}

fn available_names(
  orchestrator: config_types.OrchestratorConfig,
) -> List(String) {
  orchestrator.workspace_profiles.profiles
  |> dict.keys
  |> list.sort(by: string.compare)
}

fn available_names_to_string(available: List(String)) -> String {
  case available {
    [] -> "none"
    _ -> string.join(available, with: ", ")
  }
}
