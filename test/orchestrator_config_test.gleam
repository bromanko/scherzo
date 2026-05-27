import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config
import scherzo/config/types as config_types
import scherzo/error
import scherzo/model_config
import yay

fn env(name: String) -> Option(String) {
  case name {
    "LINEAR_API_KEY" -> Some("linearkey")
    "LINEAR_PROJECT_SLUG" -> Some("ENV-PROJECT")
    _ -> None
  }
}

fn root(source: String) -> yay.Node {
  let assert Ok([document]) = yay.parse_string(source)
  yay.document_root(document)
}

fn base_config(extra: String) -> String {
  "version: 1\ntracker:\n  kind: linear\n  api_key: \"$LINEAR_API_KEY\"\n  project_slug: \"$LINEAR_PROJECT_SLUG\"\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\nworkflows:\n    implementation: workflows/implementation.yaml\n"
  <> extra
}

fn base_config_with_workspace(workspace: String) -> String {
  "version: 1\ntracker:\n  kind: linear\n  api_key: \"$LINEAR_API_KEY\"\n  project_slug: \"$LINEAR_PROJECT_SLUG\"\n  states:\n    ready: [Todo]\nworkspace:\n"
  <> workspace
  <> "workflows:\n    implementation: workflows/implementation.yaml\n"
}

fn invalid_workspace_error(workspace: String) -> String {
  let assert Error(error.InvalidConfig(message)) =
    config.resolve_orchestrator_root(
      root(base_config_with_workspace(workspace)),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  message
}

pub fn resolve_root_resolves_shared_config_from_standalone_yaml_test() {
  let assert Ok(effective) =
    config.resolve_root(
      root(base_config("")),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert effective.tracker.api_key == Some("linearkey")
  assert effective.tracker.project_slug == Some("ENV-PROJECT")
  assert string.ends_with(
    effective.workspace.root,
    "/test/tmp/config/workspaces",
  )
}

pub fn simplified_minimal_root_config_loads_with_defaults_test() {
  let source =
    "version: 1\ntracker:\n  linear:\n    project: TEST\nworkflows:\n  research: workflows/research.yaml\n"
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(source),
      "test/tmp/config/scherzo.yaml",
      env,
    )

  assert orchestrator.effective.tracker.api_key == Some("linearkey")
  assert orchestrator.effective.tracker.project_slug == Some("TEST")
  assert orchestrator.effective.polling.interval_ms == 30_000
  assert orchestrator.routing.workflow_label_prefix == "workflow:"
  assert orchestrator.routing.require_exactly_one_workflow_label == True
  let assert Ok(path) = dict.get(orchestrator.routing.workflows, "research")
  assert string.ends_with(path, "/test/tmp/config/workflows/research.yaml")
  assert orchestrator.effective.linear_contract.enforce_issue_workflow_labels
    == True
  assert orchestrator.effective.linear_contract.workflow_labels == ["research"]
}

pub fn task_routing_labels_overrides_linear_contract_defaults_test() {
  let source =
    "version: 1\ntracker:\n  linear:\n    project: TEST\ntask_routing:\n  labels:\n    prefix: \"Work:\"\n    require_exactly_one: true\n    on_invalid:\n      state: Triage\n      comment: true\nworkflows:\n  research: workflows/research.yaml\n"
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(source),
      "test/tmp/config/scherzo.yaml",
      env,
    )

  assert orchestrator.routing.workflow_label_prefix == "work:"
  assert orchestrator.routing.require_exactly_one_workflow_label == True
  let contract = orchestrator.effective.linear_contract
  assert contract.workflow_label_prefix == "work:"
  assert contract.workflow_labels == ["research"]
  assert contract.enforce_issue_workflow_labels == True
  assert contract.invalid_workflow_state_id == Some("Triage")
  assert contract.invalid_workflow_state_target
    == Some(config_types.InvalidWorkflowStateName("Triage"))
  assert contract.comment_on_invalid_workflow == True
}

pub fn orchestrator_config_resolves_routing_and_driver_profile_test() {
  let source =
    "version: 1\ntracker:\n  kind: linear\n  api_key: \"$LINEAR_API_KEY\"\n  project_slug: \"$LINEAR_PROJECT_SLUG\"\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      driver:\n        command: scripts/scherzo-workspace-noop\n        lifecycle: [create, before-step, after-step, remove]\n        timeout: 1234ms\nworkflows:\n    implementation: workflows/implementation.yaml\nartifact_limits:\n  command_stream_max_chars: 111\n  template_field_max_chars: 222\n  workflow_summary_max_chars: 333\n"
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(source),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert string.ends_with(orchestrator.config_dir, "/test/tmp/config")
  assert orchestrator.routing.workflow_label_prefix == "workflow:"
  assert orchestrator.routing.require_exactly_one_workflow_label == True
  let assert Ok(path) =
    dict.get(orchestrator.routing.workflows, "implementation")
  assert string.ends_with(
    path,
    "/test/tmp/config/workflows/implementation.yaml",
  )
  assert orchestrator.dag_hooks == config_types.empty_dag_hooks()
  assert orchestrator.artifact_limits.command_stream_max_chars == 111
  assert orchestrator.artifact_limits.template_field_max_chars == 222
  assert orchestrator.artifact_limits.workflow_summary_max_chars == 333
  assert orchestrator.model_settings == model_config.default_settings()
}

pub fn legacy_workspace_hooks_are_rejected_test() {
  let message =
    invalid_workspace_error(
      "  root: workspaces\n  hooks:\n    create: legacy-create\n",
    )
  assert string.contains(message, "workspace.hooks")
  assert string.contains(message, "no longer supported")
  assert string.contains(message, "workspace.profiles.<name>.driver")
}

pub fn workspace_profiles_resolve_default_and_named_drivers_test() {
  let source =
    base_config_with_workspace(
      "  root: workspaces\n  default_profile: isolated\n  profiles:\n    isolated:\n      driver:\n        command: scripts/isolated\n        timeout: 111ms\n    noop:\n      driver:\n        command: scripts/noop\n        lifecycle: [create]\n        timeout: 222ms\n",
    )
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(source),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert orchestrator.workspace_profiles.default_profile == "isolated"
  let assert Ok(isolated) =
    dict.get(orchestrator.workspace_profiles.profiles, "isolated")
  let assert Ok(noop) =
    dict.get(orchestrator.workspace_profiles.profiles, "noop")
  assert isolated.source == config_types.ConfiguredWorkspaceDriver
  assert noop.source == config_types.ConfiguredWorkspaceDriver
  let assert Some(isolated_driver) = isolated.driver
  let assert Some(noop_driver) = noop.driver
  assert isolated_driver.command == "scripts/isolated"
  assert isolated_driver.timeout_ms == 111
  assert noop_driver.command == "scripts/noop"
  assert noop_driver.lifecycle == [config_types.LifecycleCreate]
  assert noop_driver.timeout_ms == 222
  assert orchestrator.dag_hooks == config_types.empty_dag_hooks()
}

pub fn workspace_driver_legacy_timeout_ms_remains_supported_test() {
  let source =
    base_config_with_workspace(
      "  root: workspaces\n  default_profile: legacy\n  profiles:\n    legacy:\n      driver:\n        command: scripts/legacy\n        timeout_ms: 1234\n    canonical:\n      driver:\n        command: scripts/canonical\n        timeout: 2s\n        timeout_ms: 999\n",
    )
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(source),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  let assert Ok(legacy) =
    dict.get(orchestrator.workspace_profiles.profiles, "legacy")
  let assert Ok(canonical) =
    dict.get(orchestrator.workspace_profiles.profiles, "canonical")
  let assert Some(legacy_driver) = legacy.driver
  let assert Some(canonical_driver) = canonical.driver
  assert legacy_driver.timeout_ms == 1234
  assert canonical_driver.timeout_ms == 2000
}

pub fn driver_workspace_profile_parses_schema_test() {
  let source =
    base_config_with_workspace(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      driver:\n        command: \"$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-noop\"\n        lifecycle: [create, remove]\n        timeout: 1234ms\n    default-timeout:\n      driver:\n        command: scripts/default-timeout\n",
    )
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(source),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert orchestrator.workspace_profiles.default_profile == "noop"
  let assert Ok(profile) =
    dict.get(orchestrator.workspace_profiles.profiles, "noop")
  assert profile.name == "noop"
  assert profile.source == config_types.ConfiguredWorkspaceDriver
  let assert Some(driver) = profile.driver
  assert driver.command == "$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-noop"
  assert driver.lifecycle
    == [
      config_types.LifecycleCreate,
      config_types.LifecycleRemove,
    ]
  assert driver.capabilities == []
  assert driver.timeout_ms == 1234
  let assert Ok(default_timeout_profile) =
    dict.get(orchestrator.workspace_profiles.profiles, "default-timeout")
  let assert Some(default_timeout_driver) = default_timeout_profile.driver
  assert default_timeout_driver.timeout_ms == 60_000
  assert orchestrator.dag_hooks == config_types.empty_dag_hooks()
}

pub fn workspace_driver_profiles_resolve_dogfood_jj_shape_test() {
  let source =
    base_config_with_workspace(
      "  root: workspaces\n  default_profile: dogfood-jj\n  profiles:\n    dogfood-jj:\n      driver:\n        command: \"$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-jj\"\n        lifecycle: [create, before-step, after-step, remove]\n        timeout: 60s\n",
    )
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(source),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert orchestrator.workspace_profiles.default_profile == "dogfood-jj"
  let assert Ok(profile) =
    dict.get(orchestrator.workspace_profiles.profiles, "dogfood-jj")
  assert profile.name == "dogfood-jj"
  assert profile.source == config_types.ConfiguredWorkspaceDriver
  let assert Some(driver) = profile.driver
  assert driver.command == "$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-jj"
  assert driver.lifecycle
    == [
      config_types.LifecycleCreate,
      config_types.LifecycleBeforeStep,
      config_types.LifecycleAfterStep,
      config_types.LifecycleRemove,
    ]
  assert driver.capabilities == []
  assert driver.timeout_ms == 60_000
}

pub fn profile_local_hooks_are_rejected_even_with_driver_test() {
  let message =
    invalid_workspace_error(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      hooks:\n        create: mkdir -p \"$SCHERZO_WORKSPACE_PATH\"\n      driver:\n        command: scripts/scherzo-workspace-jj\n",
    )
  assert string.contains(message, "workspace.profiles.noop.hooks")
  assert string.contains(message, "no longer supported")
  assert string.contains(message, "workspace.profiles.<name>.driver")
}

pub fn rejects_removed_driver_capabilities_config_test() {
  let message =
    invalid_workspace_error(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      driver:\n        command: run\n        capabilities: [assert-only]\n",
    )
  assert string.contains(message, "workspace.profiles.noop.driver.capabilities")
  assert string.contains(message, "describe --json")
  assert string.contains(
    message,
    "docs/runbooks/workspace-driver-capabilities.md",
  )
}

pub fn workspace_profiles_reject_invalid_driver_shapes_test() {
  let missing_shape =
    invalid_workspace_error(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop: {}\n",
    )
  assert string.contains(missing_shape, "must define driver")

  let empty_command =
    invalid_workspace_error(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      driver:\n        command: \"\"\n",
    )
  assert string.contains(empty_command, "command must be non-empty")

  let command_with_whitespace =
    invalid_workspace_error(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      driver:\n        command: sh driver.sh\n",
    )
  assert string.contains(command_with_whitespace, "without whitespace")

  let command_with_shell_metacharacter =
    invalid_workspace_error(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      driver:\n        command: scripts/driver;rm\n",
    )
  assert string.contains(
    command_with_shell_metacharacter,
    "shell metacharacters",
  )

  let unsupported_command_env =
    invalid_workspace_error(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      driver:\n        command: $OTHER_ROOT/scripts/driver\n",
    )
  assert string.contains(unsupported_command_env, "$SCHERZO_REPO_ROOT")

  let lifecycle_not_list =
    invalid_workspace_error(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      driver:\n        command: run\n        lifecycle: create\n",
    )
  assert string.contains(lifecycle_not_list, "lifecycle must be a list")

  let unknown_lifecycle =
    invalid_workspace_error(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      driver:\n        command: run\n        lifecycle: [publish]\n",
    )
  assert string.contains(unknown_lifecycle, "unknown lifecycle operation")

  let duplicate_lifecycle =
    invalid_workspace_error(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      driver:\n        command: run\n        lifecycle: [create, create]\n",
    )
  assert string.contains(duplicate_lifecycle, "duplicate lifecycle operation")

  let removed_capabilities =
    invalid_workspace_error(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      driver:\n        command: run\n        capabilities: [assert-only]\n",
    )
  assert string.contains(
    removed_capabilities,
    "workspace.profiles.noop.driver.capabilities",
  )
  assert string.contains(removed_capabilities, "describe --json")
  assert string.contains(
    removed_capabilities,
    "docs/runbooks/workspace-driver-capabilities.md",
  )

  let invalid_timeout =
    invalid_workspace_error(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      driver:\n        command: run\n        timeout: 0ms\n",
    )
  assert string.contains(invalid_timeout, "timeout must be positive")
}

pub fn workspace_hooks_cannot_coexist_with_driver_profiles_test() {
  let message =
    invalid_workspace_error(
      "  root: workspaces\n  hooks:\n    create: legacy-create\n  profiles:\n    noop:\n      driver:\n        command: scripts/noop\n",
    )
  assert string.contains(message, "workspace.hooks")
  assert string.contains(message, "no longer supported")
}

pub fn examples_workspace_driver_profiles_remain_parseable_test() {
  let source =
    base_config_with_workspace(
      "  root: .scherzo/workspaces\n  default_profile: isolated\n  profiles:\n    isolated:\n      driver:\n        command: scripts/scherzo-workspace-jj\n        lifecycle: [create, before-step, after-step, remove]\n        timeout: 60s\n    noop:\n      driver:\n        command: scripts/scherzo-workspace-noop\n        lifecycle: [create, before-step, after-step, remove]\n        timeout: 60s\n",
    )
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(source),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert orchestrator.workspace_profiles.default_profile == "isolated"
  let assert Ok(isolated) =
    dict.get(orchestrator.workspace_profiles.profiles, "isolated")
  let assert Ok(noop) =
    dict.get(orchestrator.workspace_profiles.profiles, "noop")
  assert isolated.source == config_types.ConfiguredWorkspaceDriver
  assert noop.source == config_types.ConfiguredWorkspaceDriver
  let assert Some(isolated_driver) = isolated.driver
  let assert Some(noop_driver) = noop.driver
  assert isolated_driver.timeout_ms == 60_000
  assert noop_driver.timeout_ms == 60_000
}

pub fn workspace_profiles_reject_invalid_config_test() {
  let invalid_name =
    base_config_with_workspace(
      "  root: workspaces\n  profiles:\n    Bad:\n      hooks: {}\n",
    )
  let assert Error(error.InvalidConfig(message)) =
    config.resolve_orchestrator_root(
      root(invalid_name),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert string.contains(message, "workspace.profiles.Bad")

  let missing_default =
    base_config_with_workspace(
      "  root: workspaces\n  profiles:\n    noop:\n      driver:\n        command: scripts/noop\n",
    )
  let assert Error(error.InvalidConfig(message)) =
    config.resolve_orchestrator_root(
      root(missing_default),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert string.contains(message, "workspace.default_profile is required")

  let unknown_default =
    base_config_with_workspace(
      "  root: workspaces\n  default_profile: missing\n  profiles:\n    noop:\n      driver:\n        command: scripts/noop\n",
    )
  let assert Error(error.InvalidConfig(message)) =
    config.resolve_orchestrator_root(
      root(unknown_default),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert string.contains(message, "workspace.default_profile")

  let collision =
    base_config_with_workspace(
      "  root: workspaces\n  hooks:\n    create: legacy\n  profiles:\n    default:\n      driver:\n        command: scripts/noop\n",
    )
  let assert Error(error.InvalidConfig(message)) =
    config.resolve_orchestrator_root(
      root(collision),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert string.contains(message, "workspace.hooks")
  assert string.contains(message, "no longer supported")
}

pub fn orchestrator_config_parses_project_model_defaults_test() {
  let source =
    base_config(
      "agents:\n  model: github-copilot/gpt-5.1-codex\n  thinking: high\n",
    )
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(source),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert orchestrator.model_settings.model
    == Some("github-copilot/gpt-5.1-codex")
  assert orchestrator.model_settings.thinking == Some(model_config.ThinkingHigh)
}

pub fn orchestrator_config_rejects_invalid_project_model_defaults_test() {
  let invalid_thinking = base_config("agents:\n  thinking: extreme\n")
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_orchestrator_root(
      root(invalid_thinking),
      "test/tmp/config/scherzo.yaml",
      env,
    )

  let invalid_model = base_config("agents:\n  model: \"sonnet:high\"\n")
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_orchestrator_root(
      root(invalid_model),
      "test/tmp/config/scherzo.yaml",
      env,
    )

  let separate_provider =
    base_config("agents:\n  provider: openai\n  model: gpt-5\n")
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_orchestrator_root(
      root(separate_provider),
      "test/tmp/config/scherzo.yaml",
      env,
    )
}

pub fn orchestrator_config_validates_default_workflow_test() {
  let with_default =
    "version: 1\ntracker:\n  kind: linear\n  api_key: \"$LINEAR_API_KEY\"\n  project_slug: \"$LINEAR_PROJECT_SLUG\"\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\ntask_routing:\n  labels:\n    default_workflow: Implementation\nworkflows:\n  implementation: workflows/implementation.yaml\n"
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(with_default),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert orchestrator.routing.default_workflow == Some("implementation")

  let invalid_default =
    "version: 1\ntracker:\n  kind: linear\n  api_key: \"$LINEAR_API_KEY\"\n  project_slug: \"$LINEAR_PROJECT_SLUG\"\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\ntask_routing:\n  labels:\n    default_workflow: research\nworkflows:\n  implementation: workflows/implementation.yaml\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_orchestrator_root(
      root(invalid_default),
      "test/tmp/config/scherzo.yaml",
      env,
    )
}

pub fn orchestrator_config_derives_linear_contract_workflow_labels_test() {
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(base_config("linear_contract:\n  enabled: true\n")),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  let contract = orchestrator.effective.linear_contract
  assert contract.workflow_label_prefix == "workflow:"
  assert contract.workflow_labels == ["implementation"]
}

pub fn orchestrator_config_accepts_matching_linear_contract_labels_test() {
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(base_config(
        "linear_contract:\n  workflow_labels: [implementation]\n",
      )),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert orchestrator.effective.linear_contract.workflow_labels
    == ["implementation"]
}

pub fn orchestrator_config_allows_scheduled_only_routes_outside_linear_contract_test() {
  let source =
    "version: 1\ntracker:\n  kind: linear\n  api_key: \"$LINEAR_API_KEY\"\n  project_slug: \"$LINEAR_PROJECT_SLUG\"\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\nworkflows:\n    implementation: workflows/implementation.yaml\n    scheduled-maintenance: workflows/scheduled-maintenance.yaml\nscheduled_jobs:\n  - id: scheduled-maintenance\n    workflow: scheduled-maintenance\n    every: 15m\nlinear_contract:\n  workflow_labels: [implementation]\n"
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(source),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert orchestrator.effective.linear_contract.workflow_labels
    == ["implementation"]
}

pub fn orchestrator_config_rejects_disagreeing_linear_contract_labels_test() {
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_orchestrator_root(
      root(base_config("linear_contract:\n  workflow_labels: [research]\n")),
      "test/tmp/config/scherzo.yaml",
      env,
    )
}

pub fn orchestrator_config_rejects_escaping_routing_paths_test() {
  let source =
    "version: 1\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\n  states:\n    ready: [Todo]\nworkflows:\n    research: ../outside.yaml\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_orchestrator_root(
      root(source),
      "test/tmp/config/scherzo.yaml",
      env,
    )
}

pub fn orchestrator_config_rejects_home_relative_workflow_paths_test() {
  let source =
    "version: 1\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\n  states:\n    ready: [Todo]\nworkflows:\n    research: ~/outside.yaml\n"
  let assert Error(error.InvalidConfig(message)) =
    config.resolve_orchestrator_root(
      root(source),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert string.contains(message, "workflows.research")
  assert string.contains(message, "relative path")
}
