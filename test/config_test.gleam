import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config
import scherzo/config/types as config_types
import scherzo/config/ui_server as ui_server_config
import scherzo/control/file as control_file
import scherzo/error
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import scherzo/workflow_completion_policy
import simplifile
import yay

fn env(name: String) -> Option(String) {
  case name {
    "LINEAR_API_KEY" -> Some("linearkey")
    "LINEAR_PROJECT_SLUG" -> Some("ENV-PROJECT")
    "OTHER_VAR" -> Some("other-secret")
    "WORKSPACE_ROOT" -> Some("test/tmp/env-workspaces")
    "UI_SERVER_TOKEN" -> Some("ui-server-secret-token")
    "EMPTY_UI_SERVER_TOKEN" -> Some("")
    "SCHERZO_CONTROL_FILE" -> Some(test_control_file_path())
    "EMPTY" -> None
    _ -> None
  }
}

fn definition(front: String) -> yay.Node {
  let assert Ok([document]) = yay.parse_string(front)
  yay.document_root(document)
}

fn minimal_front() -> String {
  "tracker:\n  linear:\n    project: TEST\nhooks:\n  before_run: test -d .git\n"
}

fn tracker_validation_front(tracker_fields: String) -> String {
  "tracker:\n  linear:\n    project: TEST\n"
  <> tracker_fields
  <> "hooks:\n  before_run: test -d .git\n"
}

fn invalid_config_message(front: String) -> String {
  let assert Error(error.InvalidConfig(message)) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
  message
}

fn test_control_file_path() -> String {
  "test/tmp/ui-server-control/control.json"
}

fn ensure_test_control_file() {
  let path = test_control_file_path()
  let _ = simplifile.delete("test/tmp/ui-server-control")
  let assert Ok(Nil) =
    simplifile.create_directory_all("test/tmp/ui-server-control")
  let contents =
    control_file.control_file_to_string(control_file.ControlFile(
      host: "127.0.0.1",
      port: 9999,
      token: "local-control-token",
      workspace_root: "test/tmp/control-workspace",
      started_at_ms: 1,
    ))
  let assert Ok(Nil) = simplifile.write(path, contents)
  Nil
}

pub fn default_values_test() {
  let tracker = config.default_tracker_config()
  assert tracker.endpoint == "https://api.linear.app/graphql"
  assert tracker.kind == tracker_kind.LinearTracker
  assert issue_state.to_strings(tracker.active_states)
    == ["Todo", "In Progress"]
  assert issue_state.to_strings(tracker.dispatch_states) == ["Todo"]
  assert issue_state.to_strings(tracker.terminal_states)
    == ["Done", "Canceled", "Cancelled", "Duplicate"]

  let agent = config.default_agent_config()
  assert agent.max_concurrent_agents == 10
  assert agent.max_turns == 20
  assert agent.max_retry_backoff_ms == 300_000
  assert agent.max_retry_attempts == 5
  assert agent.max_sessions_per_issue == 3
  assert agent.context_recovery_max_attempts == 1
  assert agent.context_recovery_prompt_char_limit == 40_000

  let pi = config.default_pi_config()
  assert pi.command == "pi --mode rpc --no-session --rpc-message-updates off"
  assert pi.turn_timeout_ms == 3_600_000
  assert pi.read_timeout_ms == 5000
  assert pi.stall_timeout_ms == 300_000
  assert pi.auto_retry == True
  assert pi.ui_request_policy == config_types.Cancel
  assert pi.ui_request_timeout_ms == 300_000
  assert pi.compatibility_probe == True

  let ui_server = config.default_ui_server_config()
  assert ui_server.enabled == False
  assert ui_server.endpoint == None
  assert ui_server.enrollment_token_env == None
  assert ui_server.enrollment_token == None
}

pub fn duration_string_fields_parse_to_milliseconds_test() {
  let front =
    "tracker:\n  linear:\n    project: TEST\n  polling:\n    every: 45s\nhooks:\n  before_run: test -d .git\n  timeout: 90s\nagent:\n  max_retry_backoff: 5m\npi:\n  turn_timeout: 1h\n  read_timeout: 5s\n  stall_timeout: 0ms\n  ui_request_timeout: 10m\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)

  assert configured.polling.interval_ms == 45_000
  assert configured.hooks.timeout_ms == 90_000
  assert configured.agent.max_retry_backoff_ms == 300_000
  assert configured.pi.turn_timeout_ms == 3_600_000
  assert configured.pi.read_timeout_ms == 5000
  assert configured.pi.stall_timeout_ms == 0
  assert configured.pi.ui_request_timeout_ms == 600_000
}

pub fn duration_string_fields_reject_invalid_values_test() {
  let bare_number =
    invalid_config_message(minimal_front() <> "pi:\n  read_timeout: 5000\n")
  assert string.contains(bare_number, "duration string")

  let invalid_unit =
    invalid_config_message(minimal_front() <> "pi:\n  turn_timeout: 1d\n")
  assert string.contains(invalid_unit, "unit ms, s, m, or h")

  let negative_nonnegative_field =
    invalid_config_message(minimal_front() <> "pi:\n  stall_timeout: -1ms\n")
  assert string.contains(negative_nonnegative_field, "zero or positive")

  let zero_positive_field =
    invalid_config_message(
      minimal_front() <> "agent:\n  max_retry_backoff: 0s\n",
    )
  assert string.contains(zero_positive_field, "must be positive")
}

pub fn legacy_non_polling_duration_ms_fields_remain_supported_test() {
  let front =
    "tracker:\n  linear:\n    project: TEST\n  polling:\n    every: 45s\nhooks:\n  before_run: test -d .git\n  timeout_ms: 2345\nagent:\n  max_retry_backoff_ms: 3456\npi:\n  turn_timeout_ms: 4567\n  read_timeout_ms: 5678\n  stall_timeout_ms: 0\n  ui_request_timeout_ms: 6789\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)

  assert configured.polling.interval_ms == 45_000
  assert configured.hooks.timeout_ms == 2345
  assert configured.agent.max_retry_backoff_ms == 3456
  assert configured.pi.turn_timeout_ms == 4567
  assert configured.pi.read_timeout_ms == 5678
  assert configured.pi.stall_timeout_ms == 0
  assert configured.pi.ui_request_timeout_ms == 6789
}

pub fn duration_string_fields_take_precedence_over_legacy_ms_test() {
  let front =
    "tracker:\n  linear:\n    project: TEST\n  polling:\n    every: 2s\nhooks:\n  before_run: test -d .git\n  timeout: 3s\n  timeout_ms: 999\nagent:\n  max_retry_backoff: 4s\n  max_retry_backoff_ms: 999\npi:\n  turn_timeout: 5s\n  turn_timeout_ms: 999\n  read_timeout: 6s\n  read_timeout_ms: 999\n  stall_timeout: 0ms\n  stall_timeout_ms: 999\n  ui_request_timeout: 7s\n  ui_request_timeout_ms: 999\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)

  assert configured.polling.interval_ms == 2000
  assert configured.hooks.timeout_ms == 3000
  assert configured.agent.max_retry_backoff_ms == 4000
  assert configured.pi.turn_timeout_ms == 5000
  assert configured.pi.read_timeout_ms == 6000
  assert configured.pi.stall_timeout_ms == 0
  assert configured.pi.ui_request_timeout_ms == 7000
}

pub fn old_polling_keys_fail_with_migration_hint_test() {
  let interval_ms =
    invalid_config_message(minimal_front() <> "polling:\n  interval_ms: 1234\n")
  assert string.contains(interval_ms, "polling.interval_ms")
  assert string.contains(interval_ms, "tracker.polling.every")
  assert string.contains(interval_ms, "SCHERZO_YAML_SIMPLIFIED_V1")

  let interval =
    invalid_config_message(minimal_front() <> "polling:\n  interval: 2s\n")
  assert string.contains(interval, "polling.interval")
  assert string.contains(interval, "tracker.polling.every")
}

pub fn old_routing_keys_fail_with_migration_hint_test() {
  let workflows =
    invalid_config_message(
      minimal_front()
      <> "routing:\n  workflows:\n    research: workflows/research.yaml\n",
    )
  assert string.contains(workflows, "routing.workflows")
  assert string.contains(workflows, "top-level workflows")
  assert string.contains(workflows, "SCHERZO_YAML_SIMPLIFIED_V1")

  let prefix =
    invalid_config_message(
      minimal_front() <> "routing:\n  workflow_label_prefix: \"workflow:\"\n",
    )
  assert string.contains(prefix, "routing.workflow_label_prefix")
  assert string.contains(prefix, "task_routing.labels.prefix")

  let require_exactly_one =
    invalid_config_message(
      minimal_front()
      <> "routing:\n  require_exactly_one_workflow_label: true\n",
    )
  assert string.contains(
    require_exactly_one,
    "routing.require_exactly_one_workflow_label",
  )
  assert string.contains(
    require_exactly_one,
    "task_routing.labels.require_exactly_one",
  )

  let default_workflow =
    invalid_config_message(
      minimal_front() <> "routing:\n  default_workflow: research\n",
    )
  assert string.contains(default_workflow, "routing.default_workflow")
  assert string.contains(
    default_workflow,
    "task_routing.labels.default_workflow",
  )
}

pub fn ui_server_default_and_disabled_config_test() {
  let assert Ok(defaulted) =
    config.resolve_with_env(
      definition(minimal_front()),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert defaulted.ui_server == config.default_ui_server_config()

  let disabled_front =
    minimal_front()
    <> "ui_server:\n  enabled: false\n  endpoint: https://ui.example.test\n  enrollment_token_env: UI_SERVER_TOKEN\n"
  let assert Ok(disabled) =
    config.resolve_with_env(
      definition(disabled_front),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert disabled.ui_server.enabled == False
  assert disabled.ui_server.endpoint == Some("https://ui.example.test")
  assert disabled.ui_server.enrollment_token_env == Some("UI_SERVER_TOKEN")
  assert disabled.ui_server.enrollment_token == None
  assert config.resolved_secrets(disabled) == ["linearkey"]
}

pub fn ui_server_enabled_validation_and_secret_resolution_test() {
  let missing_endpoint =
    invalid_config_message(
      minimal_front()
      <> "ui_server:\n  enabled: true\n  enrollment_token_env: UI_SERVER_TOKEN\n",
    )
  assert missing_endpoint == "ui_server.endpoint is required when enabled"

  let http_endpoint =
    invalid_config_message(
      minimal_front()
      <> "ui_server:\n  enabled: true\n  endpoint: http://ui.example.test\n  enrollment_token_env: UI_SERVER_TOKEN\n",
    )
  assert http_endpoint == "ui_server.endpoint must be an HTTPS URL with a host"

  let empty_host_endpoint =
    invalid_config_message(
      minimal_front()
      <> "ui_server:\n  enabled: true\n  endpoint: https://\n  enrollment_token_env: UI_SERVER_TOKEN\n",
    )
  assert empty_host_endpoint
    == "ui_server.endpoint must be an HTTPS URL with a host"

  let missing_env_name =
    invalid_config_message(
      minimal_front()
      <> "ui_server:\n  enabled: true\n  endpoint: https://ui.example.test\n",
    )
  assert missing_env_name
    == "ui_server.enrollment_token_env is required when enabled"

  let invalid_env_name =
    invalid_config_message(
      minimal_front()
      <> "ui_server:\n  enabled: true\n  endpoint: https://ui.example.test\n  enrollment_token_env: BAD-NAME\n",
    )
  assert string.contains(invalid_env_name, "invalid environment variable name")

  let missing_env_value =
    invalid_config_message(
      minimal_front()
      <> "ui_server:\n  enabled: true\n  endpoint: https://ui.example.test\n  enrollment_token_env: MISSING_UI_SERVER_TOKEN\n",
    )
  assert string.contains(
    missing_env_value,
    "must resolve to a non-empty environment variable",
  )

  let empty_env_value =
    invalid_config_message(
      minimal_front()
      <> "ui_server:\n  enabled: true\n  endpoint: https://ui.example.test\n  enrollment_token_env: EMPTY_UI_SERVER_TOKEN\n",
    )
  assert string.contains(
    empty_env_value,
    "must resolve to a non-empty environment variable",
  )

  let enabled_front =
    minimal_front()
    <> "ui_server:\n  enabled: true\n  endpoint: https://ui.example.test\n  enrollment_token_env: UI_SERVER_TOKEN\n"
  let assert Ok(enabled) =
    config.resolve_with_env(
      definition(enabled_front),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert enabled.ui_server.enabled == True
  assert enabled.ui_server.endpoint == Some("https://ui.example.test")
  assert enabled.ui_server.enrollment_token_env == Some("UI_SERVER_TOKEN")
  assert enabled.ui_server.enrollment_token == Some("ui-server-secret-token")
  assert config.resolved_secrets(enabled)
    == ["linearkey", "ui-server-secret-token"]
}

pub fn ui_server_redaction_and_local_control_separation_test() {
  ensure_test_control_file()

  let missing_env_name =
    invalid_config_message(
      minimal_front()
      <> "ui_server:\n  enabled: true\n  endpoint: https://ui.example.test\n",
    )
  assert missing_env_name
    == "ui_server.enrollment_token_env is required when enabled"

  let enabled_front =
    minimal_front()
    <> "ui_server:\n  enabled: true\n  endpoint: https://ui.example.test\n  enrollment_token_env: UI_SERVER_TOKEN\n"
  let assert Ok(enabled) =
    config.resolve_with_env(
      definition(enabled_front),
      "test/tmp/scherzo.yaml",
      env,
    )
  let summary = ui_server_config.debug_summary(enabled)

  assert string.contains(summary, "event=ui_server_config")
  assert string.contains(summary, "[REDACTED]")
  assert !string.contains(summary, "ui-server-secret-token")
  assert !string.contains(summary, "local-control-token")
  assert !string.contains(summary, test_control_file_path())
  assert !list.contains(config.resolved_secrets(enabled), "local-control-token")
  assert !list.contains(
    config.resolved_secrets(enabled),
    test_control_file_path(),
  )
  assert enabled.ui_server.enrollment_token == Some("ui-server-secret-token")
}

pub fn tracker_states_default_when_absent_test() {
  let assert Ok(configured) =
    config.resolve_with_env(
      definition(minimal_front()),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert issue_state.to_strings(configured.tracker.active_states)
    == ["Todo", "In Progress"]
  assert issue_state.to_strings(configured.tracker.dispatch_states) == ["Todo"]
  assert issue_state.to_strings(configured.tracker.terminal_states)
    == ["Done", "Canceled", "Cancelled", "Duplicate"]
}

pub fn tracker_states_wrong_type_fails_test() {
  let list_message =
    invalid_config_message(tracker_validation_front("  states: [Todo]\n"))
  assert string.contains(list_message, "tracker.states must be a map")

  let scalar_message =
    invalid_config_message(tracker_validation_front("  states: Todo\n"))
  assert string.contains(scalar_message, "tracker.states must be a map")
}

pub fn wrong_type_ready_states_fails_test() {
  let message =
    invalid_config_message(tracker_validation_front(
      "  states:\n    active: [Todo]\n    ready: Todo\n    terminal: [Done]\n",
    ))
  assert string.contains(message, "tracker.states.ready must be a string list")
}

pub fn non_string_ready_states_entry_fails_test() {
  let message =
    invalid_config_message(tracker_validation_front(
      "  states:\n    active: [Todo]\n    ready: [Todo, 123]\n    terminal: [Done]\n",
    ))
  assert string.contains(
    message,
    "tracker.states.ready entries must be strings",
  )
}

pub fn empty_ready_states_fails_test() {
  let message =
    invalid_config_message(tracker_validation_front(
      "  states:\n    active: [Todo]\n    ready: []\n    terminal: [Done]\n",
    ))
  assert string.contains(message, "must contain at least one state")
}

pub fn ready_states_outside_active_states_fails_test() {
  let message =
    invalid_config_message(tracker_validation_front(
      "  states:\n    active: [Todo]\n    ready: [In Progress]\n    terminal: [Done]\n",
    ))
  assert string.contains(message, "tracker.states.ready")
  assert string.contains(message, "subset")
  assert string.contains(message, "tracker.states.active")
  assert string.contains(message, "In Progress")
}

pub fn ready_states_normalized_subset_canonicalizes_test() {
  let assert Ok(configured) =
    config.resolve_with_env(
      definition(tracker_validation_front(
        "  states:\n    active: [Todo, In Progress]\n    ready: [\" todo \"]\n    terminal: [Done]\n",
      )),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert issue_state.to_strings(configured.tracker.dispatch_states) == ["Todo"]
}

pub fn old_tracker_state_keys_fail_with_migration_hint_test() {
  let active =
    invalid_config_message(tracker_validation_front("  active_states: [Todo]\n"))
  assert string.contains(active, "tracker.active_states")
  assert string.contains(active, "tracker.states.active")
  assert string.contains(active, "SCHERZO_YAML_SIMPLIFIED_V1")

  let ready =
    invalid_config_message(tracker_validation_front(
      "  dispatch_states: [Todo]\n",
    ))
  assert string.contains(ready, "tracker.dispatch_states")
  assert string.contains(ready, "tracker.states.ready")

  let terminal =
    invalid_config_message(tracker_validation_front(
      "  terminal_states: [Done]\n",
    ))
  assert string.contains(terminal, "tracker.terminal_states")
  assert string.contains(terminal, "tracker.states.terminal")
}

pub fn tracker_validation_and_env_resolution_test() {
  let assert Ok(defaulted_kind) =
    config.resolve_with_env(
      definition(
        "tracker:\n  linear:\n    project: TEST\nhooks:\n  before_run: test -d .git\n",
      ),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert defaulted_kind.tracker.kind == tracker_kind.LinearTracker
  let assert Error(_) =
    config.resolve_with_env(
      definition(
        "tracker:\n  kind: github\n  project_slug: TEST\nhooks:\n  before_run: test -d .git\n",
      ),
      "test/tmp/scherzo.yaml",
      env,
    )
  let assert Error(_) =
    config.resolve_with_env(
      definition(
        "tracker:\n  kind: linear\nhooks:\n  before_run: test -d .git\n",
      ),
      "test/tmp/scherzo.yaml",
      env,
    )

  let assert Error(_) =
    config.resolve_with_env(
      definition(
        "tracker:\n  kind: linear\n  endpoint: http://api.linear.test/graphql\n  project_slug: TEST\nhooks:\n  before_run: test -d .git\n",
      ),
      "test/tmp/scherzo.yaml",
      env,
    )

  let assert Ok(configured) =
    config.resolve_with_env(
      definition(minimal_front()),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert configured.tracker.api_key == Some("linearkey")

  let env_project =
    "tracker:\n  kind: linear\n  linear:\n    project: \"$LINEAR_PROJECT_SLUG\"\nhooks:\n  before_run: test -d .git\n"
  let assert Ok(configured_env_project) =
    config.resolve_with_env(
      definition(env_project),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert configured_env_project.tracker.project_slug == Some("ENV-PROJECT")

  let explicit =
    "tracker:\n  kind: linear\n  linear:\n    project: TEST\n  api_key: \"$OTHER_VAR\"\nhooks:\n  before_run: test -d .git\n"
  let assert Ok(configured_explicit) =
    config.resolve_with_env(definition(explicit), "test/tmp/scherzo.yaml", env)
  assert configured_explicit.tracker.api_key == Some("other-secret")
}

pub fn flat_linear_tracker_config_aliases_still_parse_test() {
  let front =
    "tracker:\n  kind: linear\n  endpoint: https://api.linear.app/graphql\n  api_key: \"$LINEAR_API_KEY\"\n  project_slug: example-project\n  states:\n    ready: [Todo]\n    active: [Todo, In Progress]\n    terminal: [Done, Canceled]\nhooks:\n  before_run: test -d .git\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)

  assert configured.tracker.kind == tracker_kind.LinearTracker
  assert configured.tracker.endpoint == "https://api.linear.app/graphql"
  assert configured.tracker.api_key == Some("linearkey")
  assert configured.tracker.project_slug == Some("example-project")
  assert issue_state.to_strings(configured.tracker.active_states)
    == ["Todo", "In Progress"]
  assert issue_state.to_strings(configured.tracker.dispatch_states) == ["Todo"]
  assert issue_state.to_strings(configured.tracker.terminal_states)
    == ["Done", "Canceled"]
}

pub fn nested_linear_tracker_config_parses_test() {
  let front =
    "tracker:\n  kind: linear\n  credentials:\n    api_key_env: LINEAR_API_KEY\n  linear:\n    endpoint: https://api.linear.app/graphql\n    project: example-project\n  states:\n    ready: [Todo]\n    active: [Todo, In Progress]\n    terminal: [Done, Canceled]\nhooks:\n  before_run: test -d .git\n"
  let assert Ok(report) =
    config.resolve_with_env_report(
      definition(front),
      "test/tmp/scherzo.yaml",
      env,
    )
  let configured = report.config

  assert report.warnings == []
  assert configured.tracker.kind == tracker_kind.LinearTracker
  assert configured.tracker.endpoint == "https://api.linear.app/graphql"
  assert configured.tracker.api_key == Some("linearkey")
  assert configured.tracker.project_slug == Some("example-project")
  assert issue_state.to_strings(configured.tracker.active_states)
    == ["Todo", "In Progress"]
  assert issue_state.to_strings(configured.tracker.dispatch_states) == ["Todo"]
  assert issue_state.to_strings(configured.tracker.terminal_states)
    == ["Done", "Canceled"]
}

pub fn nested_tracker_config_takes_precedence_over_flat_aliases_test() {
  let front =
    "tracker:\n  kind: linear\n  endpoint: https://flat.linear.test/graphql\n  api_key: \"$OTHER_VAR\"\n  project_slug: flat-project\n  credentials:\n    api_key_env: LINEAR_API_KEY\n  linear:\n    endpoint: https://nested.linear.test/graphql\n    project: nested-project\n  states:\n    ready: [Todo]\n    active: [Todo, In Progress]\n    terminal: [Done, Canceled]\nhooks:\n  before_run: test -d .git\n"
  let assert Ok(report) =
    config.resolve_with_env_report(
      definition(front),
      "test/tmp/scherzo.yaml",
      env,
    )
  let configured = report.config

  assert configured.tracker.endpoint == "https://nested.linear.test/graphql"
  assert configured.tracker.api_key == Some("linearkey")
  assert configured.tracker.project_slug == Some("nested-project")
  assert report.warnings
    == [
      config_types.ConfigWarning(
        event: "legacy_tracker_field_ignored",
        path: "tracker.api_key",
        replacement: "tracker.credentials.api_key_env",
      ),
      config_types.ConfigWarning(
        event: "legacy_tracker_field_ignored",
        path: "tracker.endpoint",
        replacement: "tracker.linear.endpoint",
      ),
      config_types.ConfigWarning(
        event: "legacy_tracker_field_ignored",
        path: "tracker.project_slug",
        replacement: "tracker.linear.project",
      ),
    ]
  let assert [first_warning, ..] = report.warnings
  assert config.config_warning_message(first_warning)
    == "legacy_tracker_field_ignored path=tracker.api_key replacement=tracker.credentials.api_key_env"
}

fn workspace_driver_env_front(env_body: String) -> String {
  minimal_front()
  <> "workspace:\n  root: test/tmp/workspaces\n  default_profile: isolated\n  profiles:\n    isolated:\n      driver:\n        command: scripts/scherzo-workspace-jj\n        lifecycle: [create, before-step]\n        timeout: 60s\n"
  <> env_body
  <> "workflows:\n  implementation: workflows/implementation.yaml\n"
}

fn workspace_driver_env_error(env_body: String) -> String {
  let assert Error(error.InvalidConfig(message)) =
    config.resolve_orchestrator_root(
      definition(workspace_driver_env_front(env_body)),
      "test/tmp/scherzo.yaml",
      env,
    )
  message
}

pub fn workspace_driver_env_parses_literal_sorted_values_test() {
  let front =
    workspace_driver_env_front(
      "        env:\n          SCHERZO_JJ_WORKSPACE_REMOTE: upstream\n          SCHERZO_JJ_WORKSPACE_BASE: \"@\"\n          SCHERZO_JJ_WORKSPACE_BASE_BRANCH: trunk\n          PATH: /profile/bin:/usr/bin\n          EMPTY_VALUE: \"\"\n          LITERAL_REF: \"$LINEAR_API_KEY\"\n",
    )
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      definition(front),
      "test/tmp/scherzo.yaml",
      env,
    )
  let assert Ok(profile) =
    dict.get(orchestrator.workspace_profiles.profiles, "isolated")
  let assert Some(driver) = profile.driver
  assert driver.env
    == [
      #("EMPTY_VALUE", ""),
      #("LITERAL_REF", "$LINEAR_API_KEY"),
      #("PATH", "/profile/bin:/usr/bin"),
      #("SCHERZO_JJ_WORKSPACE_BASE", "@"),
      #("SCHERZO_JJ_WORKSPACE_BASE_BRANCH", "trunk"),
      #("SCHERZO_JJ_WORKSPACE_REMOTE", "upstream"),
    ]
}

pub fn workspace_driver_env_defaults_empty_when_absent_test() {
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      definition(workspace_driver_env_front("")),
      "test/tmp/scherzo.yaml",
      env,
    )
  let assert Ok(profile) =
    dict.get(orchestrator.workspace_profiles.profiles, "isolated")
  let assert Some(driver) = profile.driver
  assert driver.env == []
}

pub fn workspace_driver_env_rejects_invalid_shapes_test() {
  assert workspace_driver_env_error("        env: [A=B]\n")
    == "workspace.profiles.isolated.driver.env must be a map"
  assert workspace_driver_env_error("        env:\n          123: value\n")
    == "workspace.profiles.isolated.driver.env keys must be strings"
  assert workspace_driver_env_error("        env:\n          1BAD: value\n")
    == "workspace.profiles.isolated.driver.env.1BAD has invalid environment variable name; expected [A-Za-z_][A-Za-z0-9_]*"
  assert workspace_driver_env_error("        env:\n          BAD-NAME: value\n")
    == "workspace.profiles.isolated.driver.env.BAD-NAME has invalid environment variable name; expected [A-Za-z_][A-Za-z0-9_]*"
  assert workspace_driver_env_error(
      "        env:\n          SCHERZO_WORKSPACE_DRIVER: value\n",
    )
    == "workspace.profiles.isolated.driver.env.SCHERZO_WORKSPACE_DRIVER is reserved by Scherzo and cannot be configured in driver.env"
  assert workspace_driver_env_error("        env:\n          GOOD: 123\n")
    == "workspace.profiles.isolated.driver.env.GOOD must be a string"
  assert workspace_driver_env_error("        env:\n          GOOD:\n")
    == "workspace.profiles.isolated.driver.env.GOOD must be a string"
  assert workspace_driver_env_error(
      "        env:\n          SCHERZO_JJ_WORKSPACE_BASE: one\n          SCHERZO_JJ_WORKSPACE_BASE: two\n",
    )
    == "workspace.profiles.isolated.driver.env has duplicate key: SCHERZO_JJ_WORKSPACE_BASE"
}

pub fn path_resolution_and_env_indirection_test() {
  let front = minimal_front() <> "workspace:\n  root: relative-workspaces\n"
  let assert Ok(configured) =
    config.resolve_with_env(
      definition(front),
      "test/tmp/workflows/scherzo.yaml",
      env,
    )
  assert string.ends_with(
    configured.workspace.root,
    "/test/tmp/workflows/relative-workspaces",
  )

  let front_env = minimal_front() <> "workspace:\n  root: \"$WORKSPACE_ROOT\"\n"
  let assert Ok(configured_env) =
    config.resolve_with_env(definition(front_env), "test/tmp/scherzo.yaml", env)
  assert string.ends_with(
    configured_env.workspace.root,
    "/test/tmp/env-workspaces",
  )

  let front_inline =
    minimal_front() <> "workspace:\n  root: \"$WORKSPACE_ROOT/sub\"\n"
  let assert Ok(configured_inline) =
    config.resolve_with_env(
      definition(front_inline),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert string.contains(
    configured_inline.workspace.root,
    "$WORKSPACE_ROOT/sub",
  )
}

pub fn hooks_and_agent_limit_validation_test() {
  let assert Ok(no_hooks) =
    config.resolve_with_env(
      definition("tracker:\n  linear:\n    project: TEST\n"),
      "test/tmp/scherzo.yaml",
      env,
    )
  let assert Error(error.DispatchValidationFailed(_)) =
    config.validate_dispatch(no_hooks)
  let assert Ok(prepop) =
    config.resolve_with_env(
      definition(minimal_front()),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert prepop.hooks.before_run == Some("test -d .git")

  let paused_front =
    minimal_front()
    <> "agent:\n  max_concurrent_agents: 0\n  max_turns: 1\n  max_retry_attempts: 1\n  max_sessions_per_issue: 1\n  max_concurrent_agents_by_state:\n    todo: 2\n    bad: 0\n"
  let assert Ok(paused) =
    config.resolve_with_env(
      definition(paused_front),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert paused.agent.max_concurrent_agents == 0
  assert dict.get(
      paused.agent.max_concurrent_agents_by_state,
      issue_state.key_from_string("todo"),
    )
    == Ok(2)
  assert dict.get(
      paused.agent.max_concurrent_agents_by_state,
      issue_state.key_from_string("bad"),
    )
    == Error(Nil)

  let invalid_front = minimal_front() <> "agent:\n  max_concurrent_agents: -1\n"
  let assert Error(_) =
    config.resolve_with_env(
      definition(invalid_front),
      "test/tmp/scherzo.yaml",
      env,
    )
}

pub fn context_recovery_agent_config_validation_test() {
  let disabled_front =
    minimal_front()
    <> "agent:\n  context_recovery_max_attempts: 0\n  context_recovery_prompt_char_limit: 1234\n"
  let assert Ok(disabled) =
    config.resolve_with_env(
      definition(disabled_front),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert disabled.agent.context_recovery_max_attempts == 0
  assert disabled.agent.context_recovery_prompt_char_limit == 1234

  let negative_attempts =
    invalid_config_message(
      minimal_front() <> "agent:\n  context_recovery_max_attempts: -1\n",
    )
  assert string.contains(negative_attempts, "context_recovery_max_attempts")

  let nonpositive_limit =
    invalid_config_message(
      minimal_front() <> "agent:\n  context_recovery_prompt_char_limit: 0\n",
    )
  assert string.contains(nonpositive_limit, "agent limits must be positive")
}

pub fn pi_validation_and_unknown_keys_ignored_test() {
  let front =
    minimal_front()
    <> "pi:\n  command: \"custom pi --mode rpc\"\n  compatibility_probe: false\nunknown: ignored\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
  assert configured.pi.command == "custom pi --mode rpc"
  assert configured.pi.compatibility_probe == False

  let operator_policy =
    minimal_front()
    <> "pi:\n  ui_request_policy: operator\n  ui_request_timeout: 1234ms\n"
  let assert Ok(configured_operator_policy) =
    config.resolve_with_env(
      definition(operator_policy),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert configured_operator_policy.pi.ui_request_policy
    == config_types.Operator
  assert configured_operator_policy.pi.ui_request_timeout_ms == 1234

  let cancel_policy =
    minimal_front() <> "pi:\n  ui_request_policy: \" Cancel \"\n"
  let assert Ok(configured_cancel_policy) =
    config.resolve_with_env(
      definition(cancel_policy),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert configured_cancel_policy.pi.ui_request_policy == config_types.Cancel

  let explicit_timeout =
    minimal_front() <> "pi:\n  ui_request_timeout: 1234ms\n"
  let assert Ok(configured_timeout) =
    config.resolve_with_env(
      definition(explicit_timeout),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert configured_timeout.pi.ui_request_timeout_ms == 1234

  let fail_policy = minimal_front() <> "pi:\n  ui_request_policy: fail\n"
  let assert Ok(configured_fail_policy) =
    config.resolve_with_env(
      definition(fail_policy),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert configured_fail_policy.pi.ui_request_policy == config_types.Fail

  let ignore_policy = minimal_front() <> "pi:\n  ui_request_policy: ignore\n"
  let assert Ok(configured_ignore_policy) =
    config.resolve_with_env(
      definition(ignore_policy),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert configured_ignore_policy.pi.ui_request_policy == config_types.Ignore

  let invalid_policy = minimal_front() <> "pi:\n  ui_request_policy: surprise\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(invalid_policy),
      "test/tmp/scherzo.yaml",
      env,
    )

  let invalid_timeout = minimal_front() <> "pi:\n  ui_request_timeout: 0ms\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(invalid_timeout),
      "test/tmp/scherzo.yaml",
      env,
    )

  let invalid = minimal_front() <> "pi:\n  command: \"\"\n"
  let assert Error(_) =
    config.resolve_with_env(definition(invalid), "test/tmp/scherzo.yaml", env)
}

pub fn handoff_defaults_and_parsing_test() {
  let assert Ok(defaulted) =
    config.resolve_with_env(
      definition(minimal_front()),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert defaulted.handoff.enabled == False
  assert defaulted.handoff.comment_on_claim == False
  assert defaulted.handoff.comment_on_park == False
  assert defaulted.handoff.attach_result_on_success == False
  assert defaulted.handoff.attachment_fallback_to_markdown_link == True

  let comments_only = minimal_front() <> "handoff:\n  enabled: true\n"
  let assert Ok(enabled) =
    config.resolve_with_env(
      definition(comments_only),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert enabled.handoff.enabled == True
  assert enabled.handoff.comment_on_claim == True
  assert enabled.handoff.comment_on_success == True
  assert enabled.handoff.comment_on_failure == True
  assert enabled.handoff.comment_on_park == True

  let with_states =
    minimal_front()
    <> "handoff:\n  enabled: true\n  comment_on_failure: false\n  comment_on_park: false\n  claim_state_id: state-claim\n  success_state_id: state-success\n  failure_state_id: state-fail\n  attach_result_on_success: true\n  attachment_fallback_to_markdown_link: false\n"
  let assert Ok(parsed) =
    config.resolve_with_env(
      definition(with_states),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert parsed.handoff.comment_on_failure == False
  assert parsed.handoff.comment_on_park == False
  assert parsed.handoff.claim_state_id == Some("state-claim")
  assert parsed.handoff.success_state_id == Some("state-success")
  assert parsed.handoff.failure_state_id == Some("state-fail")
  assert parsed.handoff.attach_result_on_success == True
  assert parsed.handoff.attachment_fallback_to_markdown_link == False
}

pub fn handoff_result_defaults_follow_success_comments_test() {
  let front = minimal_front() <> "handoff:\n  enabled: true\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
  assert configured.handoff.comment_on_success == True
  assert configured.handoff.include_result_on_success == True
  assert configured.handoff.result_max_chars == 8000
}

pub fn handoff_can_disable_result_in_success_comment_test() {
  let front =
    minimal_front()
    <> "handoff:\n  enabled: true\n  include_result_on_success: false\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
  assert configured.handoff.comment_on_success == True
  assert configured.handoff.include_result_on_success == False
}

pub fn handoff_result_max_chars_must_be_positive_test() {
  let front =
    minimal_front() <> "handoff:\n  enabled: true\n  result_max_chars: 0\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
}

pub fn handoff_completion_states_parse_display_names_test() {
  let front =
    minimal_front()
    <> "linear_contract:\n  enabled: true\nhandoff:\n  enabled: true\n  completion_states:\n    default_completion_state: In Review\n    no_review_completion_state: Done\n    failure_state: Needs Attention\n    partial_success_state: Needs Attention\n    cancellation_state: Canceled\n    workflows:\n      execplan:\n        produces_reviewable_artifacts: true\n        requires_review: true\n      no-review-maintenance:\n        produces_reviewable_artifacts: false\n        requires_review: false\n        success_state: Done\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
  let assert Some(policy) = configured.handoff.completion_states
  assert policy.default_completion_state
    == workflow_completion_policy.StateByName("In Review")
  assert policy.no_review_completion_state
    == Some(workflow_completion_policy.StateByName("Done"))
  assert policy.failure_state
    == workflow_completion_policy.StateByName("Needs Attention")
  assert policy.partial_success_state
    == workflow_completion_policy.StateByName("Needs Attention")
  assert policy.cancellation_state
    == Some(workflow_completion_policy.StateByName("Canceled"))
  let assert Ok(execplan_v2) = dict.get(policy.workflows, "execplan")
  assert execplan_v2.produces_reviewable_artifacts == Some(True)
  assert execplan_v2.requires_review == Some(True)
  let assert Ok(maintenance) =
    dict.get(policy.workflows, "no-review-maintenance")
  assert maintenance.produces_reviewable_artifacts == Some(False)
  assert maintenance.requires_review == Some(False)
  assert maintenance.success_state
    == Some(workflow_completion_policy.StateByName("Done"))
}

pub fn handoff_completion_states_parse_state_ids_test() {
  let front =
    minimal_front()
    <> "linear_contract:\n  enabled: true\nhandoff:\n  enabled: true\n  completion_states:\n    default_completion_state_id: state-review\n    failure_state_id: state-attention\n    partial_success_state_id: state-attention\n    workflows:\n      execplan:\n        success_state_id: state-custom\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
  let assert Some(policy) = configured.handoff.completion_states
  assert policy.default_completion_state
    == workflow_completion_policy.StateById("state-review")
  assert policy.failure_state
    == workflow_completion_policy.StateById("state-attention")
  assert policy.partial_success_state
    == workflow_completion_policy.StateById("state-attention")
  let assert Ok(execplan_v2) = dict.get(policy.workflows, "execplan")
  assert execplan_v2.success_state
    == Some(workflow_completion_policy.StateById("state-custom"))
}

pub fn handoff_completion_states_reject_invalid_config_test() {
  let duplicate =
    invalid_config_message(
      minimal_front()
      <> "linear_contract:\n  enabled: true\nhandoff:\n  completion_states:\n    default_completion_state: In Review\n    default_completion_state_id: state-review\n    failure_state: Needs Attention\n    partial_success_state: Needs Attention\n",
    )
  assert string.contains(
    duplicate,
    "handoff.completion_states.default_completion_state",
  )
  assert string.contains(
    duplicate,
    "handoff.completion_states.default_completion_state_id",
  )

  let missing_required =
    invalid_config_message(
      minimal_front()
      <> "linear_contract:\n  enabled: true\nhandoff:\n  completion_states:\n    failure_state: Needs Attention\n    partial_success_state: Needs Attention\n",
    )
  assert string.contains(
    missing_required,
    "handoff.completion_states.default_completion_state",
  )

  let empty_value =
    invalid_config_message(
      minimal_front()
      <> "linear_contract:\n  enabled: true\nhandoff:\n  completion_states:\n    default_completion_state: \"  \"\n    failure_state: Needs Attention\n    partial_success_state: Needs Attention\n",
    )
  assert string.contains(empty_value, "must be non-empty")

  let contract_disabled =
    invalid_config_message(
      minimal_front()
      <> "handoff:\n  completion_states:\n    default_completion_state: In Review\n    failure_state: Needs Attention\n    partial_success_state: Needs Attention\n",
    )
  assert string.contains(contract_disabled, "linear_contract.enabled")
  assert string.contains(
    contract_disabled,
    "scherzo doctor --check tracker-contract",
  )

  let unsupported =
    invalid_config_message(
      minimal_front()
      <> "linear_contract:\n  enabled: true\nhandoff:\n  completion_states:\n    default_completion_state: In Review\n    failure_state: Needs Attention\n    partial_success_state: Needs Attention\n    unresolved_state_policy: best_effort\n",
    )
  assert string.contains(unsupported, "unresolved_state_policy")
}

pub fn handoff_attachment_requires_success_comment_test() {
  let front =
    minimal_front()
    <> "handoff:\n  attach_result_on_success: true\n  comment_on_success: false\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
}

pub fn linear_contract_defaults_test() {
  let defaults = config.default_linear_contract_config()
  assert defaults.enabled == False
  assert defaults.workflow_label_prefix == "workflow:"
  assert defaults.workflow_labels == []
  assert defaults.support_labels == []
  assert dict.to_list(defaults.required_states) == []
  assert dict.to_list(defaults.handoff_state_bindings) == []
  assert defaults.enforce_issue_workflow_labels == False
  assert defaults.invalid_workflow_state_id == None
  assert defaults.invalid_workflow_state_target == None
  assert defaults.comment_on_invalid_workflow == False

  let assert Ok(configured) =
    config.resolve_with_env(
      definition(minimal_front()),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert configured.linear_contract == defaults
}

pub fn linear_contract_parses_and_normalizes_test() {
  let front =
    minimal_front()
    <> "linear_contract:\n  enabled: true\n  workflow_label_prefix: \" Workflow: \"\n  workflow_labels: [Bugfix, \" bugfix \", Research, \"\"]\n  support_labels: [Needs-Workflow, \" needs-workflow \", Needs-Clarification]\n  required_states:\n    Ready: \"Ready for Agent\"\n    in_progress: \" In Progress \"\n    done: Done\n  handoff_state_bindings:\n    claim: IN_PROGRESS\n    success: done\n  enforce_issue_workflow_labels: true\n  invalid_workflow_state_id: \" state-needs-workflow \"\n  comment_on_invalid_workflow: true\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
  let contract = configured.linear_contract
  assert contract.enabled == True
  assert contract.workflow_label_prefix == "workflow:"
  assert contract.workflow_labels == ["bugfix", "research"]
  assert contract.support_labels == ["needs-workflow", "needs-clarification"]
  assert dict.get(contract.required_states, "ready") == Ok("Ready for Agent")
  assert dict.get(contract.required_states, "in_progress") == Ok("In Progress")
  assert dict.get(contract.handoff_state_bindings, "claim") == Ok("in_progress")
  assert dict.get(contract.handoff_state_bindings, "success") == Ok("done")
  assert contract.enforce_issue_workflow_labels == True
  assert contract.invalid_workflow_state_id == Some("state-needs-workflow")
  assert contract.invalid_workflow_state_target
    == Some(config_types.InvalidWorkflowStateId("state-needs-workflow"))
  assert contract.comment_on_invalid_workflow == True
}

pub fn linear_contract_optional_dispatch_policy_defaults_test() {
  let front =
    minimal_front()
    <> "linear_contract:\n  workflow_labels: []\n  invalid_workflow_state_id: null\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
  assert configured.linear_contract.enforce_issue_workflow_labels == False
  assert configured.linear_contract.workflow_labels == []
  assert configured.linear_contract.invalid_workflow_state_id == None
  assert configured.linear_contract.invalid_workflow_state_target == None
  assert configured.linear_contract.comment_on_invalid_workflow == False

  let blank_state_id =
    minimal_front()
    <> "linear_contract:\n  invalid_workflow_state_id: \"   \"\n"
  let assert Ok(configured_blank) =
    config.resolve_with_env(
      definition(blank_state_id),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert configured_blank.linear_contract.invalid_workflow_state_id == None
}

pub fn linear_contract_rejects_invalid_values_test() {
  let empty_prefix =
    minimal_front()
    <> "linear_contract:\n  enabled: true\n  workflow_label_prefix: \"  \"\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(empty_prefix),
      "test/tmp/scherzo.yaml",
      env,
    )

  let enforcement_without_labels =
    minimal_front()
    <> "linear_contract:\n  enforce_issue_workflow_labels: true\n  workflow_labels: []\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(enforcement_without_labels),
      "test/tmp/scherzo.yaml",
      env,
    )

  let enforcement_empty_prefix =
    minimal_front()
    <> "linear_contract:\n  enforce_issue_workflow_labels: true\n  workflow_label_prefix: \"  \"\n  workflow_labels: [bugfix]\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(enforcement_empty_prefix),
      "test/tmp/scherzo.yaml",
      env,
    )

  let invalid_bool =
    minimal_front()
    <> "linear_contract:\n  enforce_issue_workflow_labels: yes\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(invalid_bool),
      "test/tmp/scherzo.yaml",
      env,
    )

  let unknown_binding_key =
    minimal_front()
    <> "linear_contract:\n  required_states:\n    done: Done\n  handoff_state_bindings:\n    surprise: done\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(unknown_binding_key),
      "test/tmp/scherzo.yaml",
      env,
    )

  let missing_binding_target =
    minimal_front()
    <> "linear_contract:\n  required_states:\n    done: Done\n  handoff_state_bindings:\n    success: closed\n"
  let assert Error(error.InvalidConfig(missing_binding_message)) =
    config.resolve_with_env(
      definition(missing_binding_target),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert missing_binding_message
    == "linear_contract.handoff_state_bindings.success references unknown required state: closed"

  let non_string_list_entry =
    minimal_front() <> "linear_contract:\n  workflow_labels: [bugfix, 123]\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(non_string_list_entry),
      "test/tmp/scherzo.yaml",
      env,
    )

  let non_string_map_key =
    minimal_front() <> "linear_contract:\n  required_states:\n    123: Done\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(non_string_map_key),
      "test/tmp/scherzo.yaml",
      env,
    )

  let non_string_map_value =
    minimal_front() <> "linear_contract:\n  required_states:\n    ready: 123\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(non_string_map_value),
      "test/tmp/scherzo.yaml",
      env,
    )

  let blank_map_key =
    minimal_front()
    <> "linear_contract:\n  required_states:\n    \"  \": Done\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(blank_map_key),
      "test/tmp/scherzo.yaml",
      env,
    )

  let blank_map_value =
    minimal_front()
    <> "linear_contract:\n  required_states:\n    ready: \"  \"\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(blank_map_value),
      "test/tmp/scherzo.yaml",
      env,
    )

  let non_map_section = minimal_front() <> "linear_contract: true\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(non_map_section),
      "test/tmp/scherzo.yaml",
      env,
    )
}

pub fn scheduled_jobs_parse_defaults_and_linear_failure_config_test() {
  let front =
    minimal_front()
    <> "workflows:\n    pr-conflict-repair: workflows/pr-conflict-repair.yaml\n"
    <> "scheduled_jobs:\n  - id: pr-conflict-repair\n    workflow: pr-conflict-repair\n    every: 15m\n    on_failure:\n      linear:\n        enabled: true\n        state: Triage\n        labels:\n          - job:pr-conflict-repair\n"
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      definition(front),
      "test/tmp/scherzo.yaml",
      env,
    )
  let assert [job] = orchestrator.scheduled_jobs
  assert job.id == "pr-conflict-repair"
  assert job.workflow == "pr-conflict-repair"
  assert job.enabled == True
  assert job.every_ms == 900_000
  assert job.overlap == config_types.SkipOverlap
  assert job.catch_up == False
  let config_types.ScheduledFailureConfig(linear: linear) = job.on_failure
  assert linear.enabled == True
  assert linear.state == Some("Triage")
  assert linear.labels == ["job:pr-conflict-repair"]
  assert linear.dedupe == config_types.OpenIssuePerJob
}

pub fn scheduled_jobs_reject_invalid_duration_and_unsupported_modes_test() {
  let base =
    minimal_front() <> "workflows:\n    repair: workflows/repair.yaml\n"

  let invalid_duration =
    base
    <> "scheduled_jobs:\n  - id: repair\n    workflow: repair\n    every: 500ms\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_orchestrator_root(
      definition(invalid_duration),
      "test/tmp/scherzo.yaml",
      env,
    )

  let catch_up =
    base
    <> "scheduled_jobs:\n  - id: repair\n    workflow: repair\n    every: 15m\n    catch_up: true\n"
  let assert Error(error.ScheduledJobCatchUpUnsupported(_)) =
    config.resolve_orchestrator_root(
      definition(catch_up),
      "test/tmp/scherzo.yaml",
      env,
    )

  let overlap =
    base
    <> "scheduled_jobs:\n  - id: repair\n    workflow: repair\n    every: 15m\n    overlap: queue\n"
  let assert Error(error.InvalidScheduledJobOverlap(_)) =
    config.resolve_orchestrator_root(
      definition(overlap),
      "test/tmp/scherzo.yaml",
      env,
    )
}

pub fn scheduled_jobs_reject_unknown_workflow_and_payload_fields_test() {
  let unknown_workflow =
    minimal_front()
    <> "workflows:\n    repair: workflows/repair.yaml\n"
    <> "scheduled_jobs:\n  - id: nightly\n    workflow: missing\n    every: 15m\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_orchestrator_root(
      definition(unknown_workflow),
      "test/tmp/scherzo.yaml",
      env,
    )

  let payload =
    minimal_front()
    <> "workflows:\n    repair: workflows/repair.yaml\n"
    <> "scheduled_jobs:\n  - id: repair\n    workflow: repair\n    every: 15m\n    vars:\n      key: value\n"
  let assert Error(error.ScheduledJobUnsupportedInputs(message)) =
    config.resolve_orchestrator_root(
      definition(payload),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert string.contains(message, "intentionally deferred")
}

pub fn reload_state_preserves_last_good_and_blocks_dispatch_test() {
  let state = config.initial_reload_state()
  let good = definition(minimal_front())
  let config.ReloadResult(state: loaded, resolved_secrets: secrets) =
    config.apply_reload(state, good, "test/tmp/scherzo.yaml", env)
  assert config.can_dispatch(loaded)
  assert secrets == ["linearkey"]
  let assert Some(_) = loaded.last_known_good

  let bad = definition("tracker:\n  kind: linear\n")
  let config.ReloadResult(state: invalid, resolved_secrets: bad_secrets) =
    config.apply_reload(loaded, bad, "test/tmp/scherzo.yaml", env)
  assert !config.can_dispatch(invalid)
  assert bad_secrets == []
  let assert Some(_) = invalid.last_known_good

  let paused =
    definition(minimal_front() <> "agent:\n  max_concurrent_agents: 0\n")
  let config.ReloadResult(state: reloaded, resolved_secrets: _) =
    config.apply_reload(invalid, paused, "test/tmp/scherzo.yaml", env)
  assert config.can_dispatch(reloaded)
  let assert Some(effective) = reloaded.last_known_good
  assert effective.agent.max_concurrent_agents == 0
}
