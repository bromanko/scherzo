import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/template
import scherzo/workflow_dag
import scherzo/workspace_driver_context

pub type WorkspaceDriverContext =
  workspace_driver_context.Context

pub fn driver_context_from_profile(
  profile: config_types.WorkspaceHookProfile,
) -> WorkspaceDriverContext {
  workspace_driver_context.from_profile(profile)
}

pub fn driver_context_env_vars(
  context: WorkspaceDriverContext,
) -> List(#(String, String)) {
  workspace_driver_context.env_vars(context)
}

pub fn driver_context_template_locals(
  context: WorkspaceDriverContext,
) -> List(#(String, template.Value)) {
  workspace_driver_context.template_locals(context)
}

pub type ProfileResolutionError {
  UnknownWorkspaceProfile(
    workflow_id: String,
    profile_name: String,
    available: List(String),
  )
  WorkspaceCapabilitiesUnavailable(
    workflow_id: String,
    profile_name: String,
    required: List(config_types.WorkspaceCapability),
    provided: List(config_types.WorkspaceCapability),
    missing: List(config_types.WorkspaceCapability),
  )
  WorkspaceDriverInvocationUnavailable(
    workflow_id: String,
    profile_name: String,
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

pub fn validate_capabilities(
  dag: workflow_dag.WorkflowDag,
  profile: config_types.WorkspaceHookProfile,
) -> Result(Nil, ProfileResolutionError) {
  let provided = provided_capabilities(profile)
  let missing = missing_capabilities(dag.workspace_capabilities, provided, [])
  case missing {
    [] -> Ok(Nil)
    _ ->
      Error(WorkspaceCapabilitiesUnavailable(
        workflow_id: dag.id,
        profile_name: profile.name,
        required: dag.workspace_capabilities,
        provided: provided,
        missing: missing,
      ))
  }
}

pub fn validate_dispatchable_profile(
  dag: workflow_dag.WorkflowDag,
  profile: config_types.WorkspaceHookProfile,
) -> Result(Nil, ProfileResolutionError) {
  case profile.driver, profile.hooks {
    Some(_), None ->
      Error(WorkspaceDriverInvocationUnavailable(
        workflow_id: dag.id,
        profile_name: profile.name,
      ))
    _, _ -> Ok(Nil)
  }
}

pub fn error_code(error: ProfileResolutionError) -> String {
  case error {
    UnknownWorkspaceProfile(..) -> "unknown_workspace_profile"
    WorkspaceCapabilitiesUnavailable(..) -> "workspace_capabilities_unavailable"
    WorkspaceDriverInvocationUnavailable(..) ->
      "workspace_driver_invocation_unavailable"
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
    WorkspaceCapabilitiesUnavailable(
      workflow_id,
      profile_name,
      required,
      provided,
      missing,
    ) ->
      "workflow "
      <> workflow_id
      <> " requires workspace capabilities "
      <> config_types.workspace_capabilities_to_string(required)
      <> " but workspace_profile "
      <> profile_name
      <> " provides "
      <> config_types.workspace_capabilities_to_string(provided)
      <> "; missing: "
      <> config_types.workspace_capabilities_to_string(missing)
    WorkspaceDriverInvocationUnavailable(workflow_id, profile_name) ->
      "workflow "
      <> workflow_id
      <> " selects workspace_profile "
      <> profile_name
      <> ", but workspace driver invocation is not implemented in this Scherzo version; use a hook-backed profile or wait for the driver invocation migration. See docs/runbooks/workspace-driver-migration.md"
  }
}

fn provided_capabilities(
  profile: config_types.WorkspaceHookProfile,
) -> List(config_types.WorkspaceCapability) {
  case profile.driver {
    Some(driver) -> driver.capabilities
    None -> []
  }
}

fn missing_capabilities(
  required: List(config_types.WorkspaceCapability),
  provided: List(config_types.WorkspaceCapability),
  acc: List(config_types.WorkspaceCapability),
) -> List(config_types.WorkspaceCapability) {
  case required {
    [] -> config_types.canonical_workspace_capabilities(acc)
    [capability, ..rest] ->
      case
        list.contains(provided, capability) || list.contains(acc, capability)
      {
        True -> missing_capabilities(rest, provided, acc)
        False -> missing_capabilities(rest, provided, [capability, ..acc])
      }
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
