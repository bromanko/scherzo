import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/template
import scherzo/workflow_dag
import scherzo/workspace_driver_context
import scherzo/workspace_driver_env

pub type WorkspaceDriverContext =
  workspace_driver_context.Context

pub fn driver_context(
  profile: config_types.WorkspaceHookProfile,
  orchestrator: config_types.OrchestratorConfig,
) -> WorkspaceDriverContext {
  workspace_driver_context.from_profile_for_orchestrator(profile, orchestrator)
}

pub fn driver_context_env_vars(
  context: WorkspaceDriverContext,
) -> List(#(String, String)) {
  workspace_driver_context.env_vars(context)
}

pub fn driver_context_env_vars_with_generated(
  context: WorkspaceDriverContext,
  generated: List(#(String, String)),
) -> List(#(String, String)) {
  workspace_driver_env.merge(
    context.env,
    list.append(generated, workspace_driver_context.generated_env_vars(context)),
  )
}

pub fn driver_context_redaction_values(
  context: WorkspaceDriverContext,
) -> List(String) {
  workspace_driver_env.values_for_redaction(context.env)
}

pub fn profile_redaction_values(
  profile: config_types.WorkspaceHookProfile,
) -> List(String) {
  case profile.driver {
    Some(driver) -> workspace_driver_env.values_for_redaction(driver.env)
    None -> []
  }
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
    Ok(profile) -> {
      use _ <- result.try(validate_capabilities(dag, profile))
      Ok(profile)
    }
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

pub fn error_code(error: ProfileResolutionError) -> String {
  case error {
    UnknownWorkspaceProfile(..) -> "unknown_workspace_profile"
    WorkspaceCapabilitiesUnavailable(..) -> "workspace_capabilities_unavailable"
  }
}

pub fn error_label(error: ProfileResolutionError) -> String {
  error_code(error) <> ":" <> error_message(error)
}

pub fn error_message(error: ProfileResolutionError) -> String {
  case error {
    UnknownWorkspaceProfile(workflow_id, profile_name, available) ->
      "workflow "
      <> workflow_id
      <> " selects unknown workspace.driver "
      <> profile_name
      <> "; available drivers: "
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
      <> " requires workspace.requires "
      <> config_types.workspace_capabilities_to_string(required)
      <> " but workspace.driver "
      <> profile_name
      <> " provides "
      <> config_types.workspace_capabilities_to_string(provided)
      <> "; missing: "
      <> config_types.workspace_capabilities_to_string(missing)
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
        capability_is_provided(capability, provided)
        || list.contains(acc, capability)
      {
        True -> missing_capabilities(rest, provided, acc)
        False -> missing_capabilities(rest, provided, [capability, ..acc])
      }
  }
}

fn capability_is_provided(
  capability: config_types.WorkspaceCapability,
  provided: List(config_types.WorkspaceCapability),
) -> Bool {
  list.contains(provided, capability)
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
