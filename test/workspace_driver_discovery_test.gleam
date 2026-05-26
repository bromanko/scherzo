import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import scherzo/config
import scherzo/config/types as config_types
import scherzo/model_config
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import scherzo/workspace_driver_discovery
import simplifile
import support/test_helpers

fn effective(root: String) -> config_types.EffectiveConfig {
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
    workspace: config_types.WorkspaceConfig(root: root <> "/workspaces"),
    hooks: config.default_hooks_config(),
    agent: config.default_agent_config(),
    pi: config.default_pi_config(),
    handoff: config.default_handoff_config(),
    linear_contract: config.default_linear_contract_config(),
    linear_commands: config.default_linear_command_config(),
  )
}

fn orchestrator(
  dir: String,
  command: String,
  timeout_ms: Int,
) -> config_types.OrchestratorConfig {
  orchestrator_with_env(dir, command, timeout_ms, [])
}

fn orchestrator_with_env(
  dir: String,
  command: String,
  timeout_ms: Int,
  env: List(#(String, String)),
) -> config_types.OrchestratorConfig {
  let driver =
    config_types.WorkspaceDriverConfig(
      command: command,
      lifecycle: [],
      capabilities: [],
      timeout_ms: timeout_ms,
      env: env,
    )
  config_types.OrchestratorConfig(
    effective: effective(dir),
    config_dir: dir,
    routing: config_types.RoutingConfig(
      workflow_label_prefix: "workflow:",
      require_exactly_one_workflow_label: True,
      default_workflow: None,
      workflows: dict.new(),
    ),
    dag_hooks: config_types.empty_dag_hooks(),
    workspace_profiles: config_types.WorkspaceHookProfiles(
      default_profile: "noop",
      profiles: dict.from_list([
        #(
          "noop",
          config_types.WorkspaceHookProfile(
            name: "noop",
            driver: Some(driver),
            source: config_types.ConfiguredWorkspaceDriver,
          ),
        ),
      ]),
    ),
    artifact_limits: config_types.ArtifactLimits(
      command_stream_max_chars: 1000,
      template_field_max_chars: 1000,
      workflow_summary_max_chars: 4000,
    ),
    model_settings: model_config.default_settings(),
    scheduled_jobs: [],
  )
}

fn discovery_timeout_ms() -> Int {
  5000
}

fn write_driver(dir: String, body: String) -> Nil {
  test_helpers.reset_dir(dir)
  let path = dir <> "/driver.sh"
  let assert Ok(Nil) = simplifile.write(path, body)
  test_helpers.chmod_executable(path)
}

fn describe_driver(payload: String) -> String {
  "#!/bin/sh\n"
  <> "if [ \"${SCHERZO_WORKSPACE_PATH+x}\" = x ]; then echo workspace path leaked >&2; exit 1; fi\n"
  <> "if [ \"${SCHERZO_WORKSPACE_CAPABILITIES+x}\" = x ]; then echo capabilities leaked >&2; exit 1; fi\n"
  <> "if [ \"${SCHERZO_RUN_ID+x}\" = x ]; then echo run id leaked >&2; exit 1; fi\n"
  <> "if [ \"$1\" != describe ] || [ \"$2\" != --json ]; then exit 2; fi\n"
  <> "printf '%s\\n' '"
  <> payload
  <> "'\n"
}

fn selected_driver(
  orchestrator: config_types.OrchestratorConfig,
) -> config_types.WorkspaceDriverConfig {
  let assert Ok(profile) =
    dict.get(orchestrator.workspace_profiles.profiles, "noop")
  let assert Some(driver) = profile.driver
  driver
}

pub fn discovery_receives_profile_env_without_workspace_values_test() {
  let dir = "test/tmp/workspace-driver-discovery-env"
  write_driver(
    dir,
    "#!/bin/sh\n"
      <> "if [ \"$SCHERZO_JJ_WORKSPACE_BASE\" != profile-base ]; then echo missing profile base >&2; exit 1; fi\n"
      <> "if [ \"${SCHERZO_WORKSPACE_PATH+x}\" = x ]; then echo workspace path leaked >&2; exit 1; fi\n"
      <> "if [ \"${SCHERZO_WORKSPACE_CAPABILITIES+x}\" = x ]; then echo capabilities leaked >&2; exit 1; fi\n"
      <> "printf '%s\\n' '{\"version\":1,\"capabilities\":[\"status\"]}'\n",
  )

  let assert Ok(enriched) =
    workspace_driver_discovery.enrich_orchestrator(
      orchestrator_with_env(dir, "./driver.sh", discovery_timeout_ms(), [
        #("SCHERZO_JJ_WORKSPACE_BASE", "profile-base"),
      ]),
    )
  assert selected_driver(enriched).capabilities
    == [config_types.WorkspaceStatus]
}

pub fn discovery_profile_path_overrides_process_path_test() {
  let dir = "test/tmp/workspace-driver-discovery-path"
  write_driver(
    dir,
    "#!/bin/sh\n"
      <> "profile-helper >/dev/null 2>&1 || { echo helper missing >&2; exit 9; }\n"
      <> "printf '%s\\n' '{\"version\":1,\"capabilities\":[]}'\n",
  )
  let bin = dir <> "/bin"
  let assert Ok(Nil) = simplifile.create_directory_all(bin)
  let helper = bin <> "/profile-helper"
  let assert Ok(Nil) = simplifile.write(helper, "#!/bin/sh\necho helper\n")
  test_helpers.chmod_executable(helper)

  let assert Ok(enriched) =
    workspace_driver_discovery.enrich_orchestrator(
      orchestrator_with_env(dir, "./driver.sh", discovery_timeout_ms(), [
        #("PATH", "./bin"),
      ]),
    )
  assert selected_driver(enriched).capabilities == []
}

pub fn discovery_errors_redact_sensitive_profile_env_values_test() {
  let dir = "test/tmp/workspace-driver-discovery-redaction"
  write_driver(
    dir,
    "#!/bin/sh\n"
      <> "echo token=$DRIVER_SECRET_TOKEN base=$SCHERZO_JJ_WORKSPACE_BASE @ marker >&2\n"
      <> "exit 7\n",
  )
  let assert Error(error) =
    workspace_driver_discovery.enrich_orchestrator(
      orchestrator_with_env(dir, "./driver.sh", discovery_timeout_ms(), [
        #("DRIVER_SECRET_TOKEN", "driver-env-redaction-token"),
        #("SCHERZO_JJ_WORKSPACE_BASE", "@"),
      ]),
    )
  let message = workspace_driver_discovery.error_message(error)
  assert string.contains(message, "[REDACTED]")
  assert !string.contains(message, "driver-env-redaction-token")
  assert string.contains(message, "@ marker")
}

fn discovery_error_for(dir: String, body: String) -> #(String, String) {
  discovery_error_for_timeout(dir, body, discovery_timeout_ms())
}

fn discovery_error_for_timeout(
  dir: String,
  body: String,
  timeout_ms: Int,
) -> #(String, String) {
  write_driver(dir, body)
  let assert Error(error) =
    workspace_driver_discovery.enrich_orchestrator(orchestrator(
      dir,
      "./driver.sh",
      timeout_ms,
    ))
  #(
    workspace_driver_discovery.error_code(error),
    workspace_driver_discovery.error_message(error),
  )
}

pub fn enrich_orchestrator_discovers_and_canonicalizes_capabilities_test() {
  let dir = "test/tmp/workspace-driver-discovery-valid"
  write_driver(
    dir,
    describe_driver(
      "{\"version\":1,\"capabilities\":[\"assert-only\",\"changed-files\"]}",
    ),
  )

  let assert Ok(enriched) =
    workspace_driver_discovery.enrich_orchestrator(orchestrator(
      dir,
      "./driver.sh",
      discovery_timeout_ms(),
    ))
  assert selected_driver(enriched).capabilities
    == [
      config_types.WorkspaceChangedFiles,
      config_types.WorkspaceAssertOnly,
    ]
}

pub fn enrich_orchestrator_accepts_empty_capability_list_test() {
  let dir = "test/tmp/workspace-driver-discovery-empty"
  write_driver(dir, describe_driver("{\"version\":1,\"capabilities\":[]}"))

  let assert Ok(enriched) =
    workspace_driver_discovery.enrich_orchestrator(orchestrator(
      dir,
      "./driver.sh",
      discovery_timeout_ms(),
    ))
  assert selected_driver(enriched).capabilities == []
}

pub fn discovery_rejects_malformed_and_invalid_descriptions_test() {
  let cases = [
    #(describe_driver("not json"), "valid JSON"),
    #(describe_driver("{\"capabilities\":[]}"), "missing version"),
    #(
      describe_driver("{\"version\":2,\"capabilities\":[]}"),
      "unsupported describe version",
    ),
    #(describe_driver("{\"version\":1}"), "missing capabilities"),
    #(
      describe_driver("{\"version\":1,\"capabilities\":\"status\"}"),
      "capabilities must be a list",
    ),
    #(
      describe_driver("{\"version\":1,\"capabilities\":[123]}"),
      "capabilities entries must be strings",
    ),
    #(
      describe_driver("{\"version\":1,\"capabilities\":[\"pull-request\"]}"),
      "unknown capability: pull-request",
    ),
    #(
      describe_driver(
        "{\"version\":1,\"capabilities\":[\"status\",\"status\"]}",
      ),
      "duplicate capability: status",
    ),
  ]
  assert_invalid_cases(cases, 0)
}

fn assert_invalid_cases(cases: List(#(String, String)), index: Int) -> Nil {
  case cases {
    [] -> Nil
    [#(body, expected), ..rest] -> {
      let #(code, message) =
        discovery_error_for(
          "test/tmp/workspace-driver-discovery-invalid-" <> int_to_string(index),
          body,
        )
      assert code == "workspace_driver_discovery_failed"
      assert string.contains(message, "profile noop")
      assert string.contains(message, "./driver.sh")
      assert string.contains(message, expected)
      assert_invalid_cases(rest, index + 1)
    }
  }
}

pub fn discovery_rejects_nonzero_exit_and_timeout_test() {
  let #(nonzero_code, nonzero_message) =
    discovery_error_for(
      "test/tmp/workspace-driver-discovery-nonzero",
      "#!/bin/sh\necho boom >&2\nexit 7\n",
    )
  assert nonzero_code == "workspace_driver_discovery_failed"
  assert string.contains(nonzero_message, "exited 7")
  assert string.contains(nonzero_message, "boom")

  let #(timeout_code, timeout_message) =
    discovery_error_for_timeout(
      "test/tmp/workspace-driver-discovery-timeout",
      "#!/bin/sh\nsleep 1\n",
      50,
    )
  assert timeout_code == "workspace_driver_discovery_failed"
  assert string.contains(timeout_message, "timed out")
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
