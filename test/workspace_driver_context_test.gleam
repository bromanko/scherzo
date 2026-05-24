import gleam/option.{None, Some}
import scherzo/config/types as config_types
import scherzo/template
import scherzo/workspace_driver_context

fn driver_profile() -> config_types.WorkspaceHookProfile {
  config_types.WorkspaceHookProfile(
    name: "dogfood-jj",
    driver: Some(
      config_types.WorkspaceDriverConfig(
        command: "scripts/scherzo-workspace-jj",
        lifecycle: [],
        capabilities: [
          config_types.WorkspaceAssertOnly,
          config_types.WorkspaceChangedFiles,
        ],
        timeout_ms: 1000,
        env: [#("SCHERZO_JJ_WORKSPACE_BASE", "profile-base")],
      ),
    ),
    source: config_types.ConfiguredWorkspaceDriver,
  )
}

pub fn context_from_profile_uses_profile_driver_metadata_test() {
  let context = workspace_driver_context.from_profile(driver_profile())

  assert context.profile == "dogfood-jj"
  assert context.driver == "scripts/scherzo-workspace-jj"
  assert context.capabilities
    == [
      config_types.WorkspaceAssertOnly,
      config_types.WorkspaceChangedFiles,
    ]
  assert context.env == [#("SCHERZO_JJ_WORKSPACE_BASE", "profile-base")]
}

pub fn env_vars_serialize_workspace_driver_context_test() {
  let context = workspace_driver_context.from_profile(driver_profile())

  assert workspace_driver_context.env_vars(context)
    == [
      #("SCHERZO_JJ_WORKSPACE_BASE", "profile-base"),
      #("SCHERZO_WORKSPACE_PROFILE", "dogfood-jj"),
      #("SCHERZO_WORKSPACE_DRIVER", "scripts/scherzo-workspace-jj"),
      #("SCHERZO_WORKSPACE_CAPABILITIES", "assert-only changed-files"),
    ]
}

pub fn template_locals_expose_workspace_driver_context_test() {
  let context = workspace_driver_context.from_profile(driver_profile())

  assert workspace_driver_context.template_locals(context)
    == [
      #("workspace.profile", template.VString("dogfood-jj")),
      #("workspace.driver", template.VString("scripts/scherzo-workspace-jj")),
      #(
        "workspace.capabilities",
        template.VList([
          template.VString("assert-only"),
          template.VString("changed-files"),
        ]),
      ),
    ]
}

pub fn driverless_profile_uses_empty_driver_context_test() {
  let context =
    workspace_driver_context.from_profile(config_types.WorkspaceHookProfile(
      name: "default",
      driver: None,
      source: config_types.SyntheticDefaultWorkspace,
    ))

  assert workspace_driver_context.env_vars(context)
    == [
      #("SCHERZO_WORKSPACE_PROFILE", "default"),
      #("SCHERZO_WORKSPACE_DRIVER", ""),
      #("SCHERZO_WORKSPACE_CAPABILITIES", ""),
    ]
  assert context.env == []
}
