import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/template
import scherzo/workspace_driver_command
import scherzo/workspace_driver_env

pub type Context {
  Context(
    profile: String,
    driver: String,
    capabilities: List(config_types.WorkspaceCapability),
    env: List(#(String, String)),
  )
}

pub fn from_profile(profile: config_types.WorkspaceHookProfile) -> Context {
  let #(driver, capabilities, env) = case profile.driver {
    Some(driver) -> #(driver.command, driver.capabilities, driver.env)
    None -> #("", [], [])
  }
  Context(
    profile: profile.name,
    driver: driver,
    capabilities: capabilities,
    env: env,
  )
}

pub fn from_profile_for_orchestrator(
  profile: config_types.WorkspaceHookProfile,
  orchestrator: config_types.OrchestratorConfig,
) -> Context {
  let #(driver, capabilities, env) = case profile.driver {
    Some(driver) -> #(
      workspace_driver_command.resolve(driver.command, orchestrator),
      driver.capabilities,
      driver.env,
    )
    None -> #("", [], [])
  }
  Context(
    profile: profile.name,
    driver: driver,
    capabilities: capabilities,
    env: env,
  )
}

pub fn env_vars(context: Context) -> List(#(String, String)) {
  workspace_driver_env.merge(context.env, generated_env_vars(context))
}

pub fn generated_env_vars(context: Context) -> List(#(String, String)) {
  [
    #("SCHERZO_WORKSPACE_PROFILE", context.profile),
    #("SCHERZO_WORKSPACE_DRIVER", context.driver),
    #(
      "SCHERZO_WORKSPACE_CAPABILITIES",
      serialize_capabilities(context.capabilities),
    ),
  ]
}

pub fn template_locals(context: Context) -> List(#(String, template.Value)) {
  [
    #("workspace.profile", template.VString(context.profile)),
    #("workspace.driver", template.VString(context.driver)),
    #(
      "workspace.capabilities",
      template.VList(capability_values(context.capabilities)),
    ),
  ]
}

pub fn serialize_capabilities(
  capabilities: List(config_types.WorkspaceCapability),
) -> String {
  capabilities
  |> list.map(config_types.workspace_capability_to_string)
  |> string.join(with: " ")
}

fn capability_values(
  capabilities: List(config_types.WorkspaceCapability),
) -> List(template.Value) {
  capabilities
  |> list.map(fn(capability) {
    template.VString(config_types.workspace_capability_to_string(capability))
  })
}
