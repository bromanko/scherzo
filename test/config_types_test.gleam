import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import scherzo/config
import scherzo/config/types as config_types
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state

fn effective(pi: config_types.PiConfig) -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    tracker: config_types.TrackerConfig(
      kind: tracker_kind.LinearTracker,
      endpoint: "https://api.linear.app/graphql",
      api_key: Some("test-key"),
      project_slug: Some("TEST"),
      active_states: issue_state.list_from_strings(["Todo"]),
      dispatch_states: issue_state.list_from_strings(["Todo"]),
      terminal_states: issue_state.list_from_strings(["Done"]),
    ),
    polling: config.default_polling_config(),
    workspace: config_types.WorkspaceConfig(root: "test/tmp/workspaces"),
    hooks: config.default_hooks_config(),
    agent: config.default_agent_config(),
    pi: pi,
    handoff: config.default_handoff_config(),
    linear_contract: config.default_linear_contract_config(),
    linear_commands: config.default_linear_command_config(),
  )
}

fn value_for(env: List(#(String, String)), key: String) -> String {
  let assert Ok(value) = dict.get(dict.from_list(env), key)
  value
}

pub fn with_pi_env_prepends_shell_exports_test() {
  let pi =
    config_types.PiConfig(
      ..config.default_pi_config(),
      command: "pi --mode rpc --no-session",
      argv_command: None,
    )
  let configured =
    config_types.with_pi_env(effective(pi), [
      #("SCHERZO_JJ_WORKSPACE_BASE", "profile-base"),
      #("PATH", "/profile/bin"),
      #("QUOTE", "O'Brien"),
    ])

  assert string.starts_with(
    configured.pi.command,
    "export SCHERZO_JJ_WORKSPACE_BASE='profile-base'\nexport PATH='/profile/bin'\n",
  )
  assert string.contains(configured.pi.command, "export QUOTE='O'\\''Brien'\n")
  assert string.ends_with(configured.pi.command, "pi --mode rpc --no-session")
}

pub fn with_pi_env_merges_argv_env_with_step_precedence_test() {
  let pi =
    config_types.PiConfig(
      ..config.default_pi_config(),
      argv_command: Some(
        config_types.PiArgvCommand(
          executable: "pi",
          args: ["--mode", "rpc"],
          env: [
            #("SCHERZO_JJ_WORKSPACE_BASE", "pi-base"),
            #("PATH", "pi-base-path"),
            #("PI_ONLY", "kept"),
          ],
        ),
      ),
    )
  let configured =
    config_types.with_pi_env(effective(pi), [
      #("SCHERZO_JJ_WORKSPACE_BASE", "profile-base"),
      #("PATH", "/profile/bin"),
      #("SCHERZO_WORKSPACE_DRIVER", "generated-driver"),
    ])
  let assert Some(argv) = configured.pi.argv_command

  assert value_for(argv.env, "SCHERZO_JJ_WORKSPACE_BASE") == "profile-base"
  assert value_for(argv.env, "PATH") == "/profile/bin"
  assert value_for(argv.env, "SCHERZO_WORKSPACE_DRIVER") == "generated-driver"
  assert value_for(argv.env, "PI_ONLY") == "kept"
}
