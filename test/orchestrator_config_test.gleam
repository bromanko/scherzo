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
  "version: 1\ntracker:\n  kind: linear\n  api_key: \"$LINEAR_API_KEY\"\n  project_slug: \"$LINEAR_PROJECT_SLUG\"\n  dispatch_states: [Todo]\nworkspace:\n  root: workspaces\nrouting:\n  workflow_label_prefix: \"workflow:\"\n  require_exactly_one_workflow_label: true\n  workflows:\n    implementation: workflows/implementation.yaml\n"
  <> extra
}

fn base_config_with_workspace(workspace: String) -> String {
  "version: 1\ntracker:\n  kind: linear\n  api_key: \"$LINEAR_API_KEY\"\n  project_slug: \"$LINEAR_PROJECT_SLUG\"\n  dispatch_states: [Todo]\nworkspace:\n"
  <> workspace
  <> "routing:\n  workflow_label_prefix: \"workflow:\"\n  require_exactly_one_workflow_label: true\n  workflows:\n    implementation: workflows/implementation.yaml\n"
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

pub fn orchestrator_config_resolves_routing_and_dag_hooks_test() {
  let source =
    "version: 1\ntracker:\n  kind: linear\n  api_key: \"$LINEAR_API_KEY\"\n  project_slug: \"$LINEAR_PROJECT_SLUG\"\n  dispatch_states: [Todo]\nworkspace:\n  root: workspaces\n  hooks:\n    create: mkdir -p \"$SCHERZO_WORKSPACE_PATH\"\n    before_step: test -d \"$SCHERZO_WORKSPACE_PATH\"\n    after_step: echo done\n    remove: rm -rf \"$SCHERZO_WORKSPACE_PATH\"\n    timeout_ms: 1234\nrouting:\n  workflow_label_prefix: \"workflow:\"\n  require_exactly_one_workflow_label: true\n  workflows:\n    implementation: workflows/implementation.yaml\nartifact_limits:\n  command_stream_max_chars: 111\n  template_field_max_chars: 222\n  workflow_summary_max_chars: 333\n"
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
  assert orchestrator.dag_hooks.create
    == Some("mkdir -p \"$SCHERZO_WORKSPACE_PATH\"")
  assert orchestrator.dag_hooks.before_step
    == Some("test -d \"$SCHERZO_WORKSPACE_PATH\"")
  assert orchestrator.dag_hooks.after_step == Some("echo done")
  assert orchestrator.dag_hooks.remove
    == Some("rm -rf \"$SCHERZO_WORKSPACE_PATH\"")
  assert orchestrator.dag_hooks.timeout_ms == 1234
  assert orchestrator.artifact_limits.command_stream_max_chars == 111
  assert orchestrator.artifact_limits.template_field_max_chars == 222
  assert orchestrator.artifact_limits.workflow_summary_max_chars == 333
  assert orchestrator.model_settings == model_config.default_settings()
}

pub fn legacy_workspace_hooks_synthesize_default_profile_test() {
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(base_config_with_workspace(
        "  root: workspaces\n  hooks:\n    create: legacy-create\n    before_step: legacy-before\n    after_step: legacy-after\n    remove: legacy-remove\n    timeout_ms: 123\n",
      )),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert orchestrator.workspace_profiles.default_profile == "default"
  let assert Ok(profile) =
    dict.get(orchestrator.workspace_profiles.profiles, "default")
  assert profile.name == "default"
  assert profile.source == config_types.LegacyWorkspaceHooks
  let assert Some(hooks) = profile.hooks
  assert profile.driver == None
  assert hooks.create == Some("legacy-create")
  assert hooks.before_step == Some("legacy-before")
  assert hooks.after_step == Some("legacy-after")
  assert hooks.remove == Some("legacy-remove")
  assert hooks.timeout_ms == 123
  assert orchestrator.dag_hooks == hooks
}

pub fn workspace_profiles_resolve_default_and_named_hooks_test() {
  let source =
    base_config_with_workspace(
      "  root: workspaces\n  default_profile: isolated\n  profiles:\n    isolated:\n      hooks:\n        create: isolated-create\n        timeout_ms: 111\n    noop:\n      hooks:\n        create: noop-create\n        before_step: noop-before\n        timeout_ms: 222\n",
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
  assert isolated.source == config_types.ConfiguredWorkspaceHooks
  assert noop.source == config_types.ConfiguredWorkspaceHooks
  let assert Some(isolated_hooks) = isolated.hooks
  let assert Some(noop_hooks) = noop.hooks
  assert isolated.driver == None
  assert noop.driver == None
  assert isolated_hooks.create == Some("isolated-create")
  assert isolated_hooks.timeout_ms == 111
  assert noop_hooks.create == Some("noop-create")
  assert noop_hooks.before_step == Some("noop-before")
  assert noop_hooks.timeout_ms == 222
  assert orchestrator.dag_hooks == isolated_hooks
}

pub fn driver_workspace_profile_parses_schema_test() {
  let source =
    base_config_with_workspace(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      driver:\n        command: \"$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-noop\"\n        lifecycle: [create, remove]\n        capabilities: [assert-only]\n        timeout_ms: 1234\n    default-timeout:\n      driver:\n        command: scripts/default-timeout\n",
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
  assert profile.hooks == None
  let assert Some(driver) = profile.driver
  assert driver.command == "$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-noop"
  assert driver.lifecycle
    == [
      config_types.LifecycleCreate,
      config_types.LifecycleRemove,
    ]
  assert driver.capabilities == [config_types.WorkspaceAssertOnly]
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
      "  root: workspaces\n  default_profile: dogfood-jj\n  profiles:\n    dogfood-jj:\n      driver:\n        command: \"$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-jj\"\n        lifecycle: [create, before-step, after-step, remove]\n        capabilities: [status, diff, changed-files, assert-only]\n        timeout_ms: 60000\n",
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
  assert profile.hooks == None
  let assert Some(driver) = profile.driver
  assert driver.command == "$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-jj"
  assert driver.lifecycle
    == [
      config_types.LifecycleCreate,
      config_types.LifecycleBeforeStep,
      config_types.LifecycleAfterStep,
      config_types.LifecycleRemove,
    ]
  assert driver.capabilities
    == [
      config_types.WorkspaceStatus,
      config_types.WorkspaceDiff,
      config_types.WorkspaceChangedFiles,
      config_types.WorkspaceAssertOnly,
    ]
  assert driver.timeout_ms == 60_000
}

pub fn hook_workspace_profile_can_include_driver_context_test() {
  let source =
    base_config_with_workspace(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      hooks:\n        create: mkdir -p \"$SCHERZO_WORKSPACE_PATH\"\n        timeout_ms: 111\n      driver:\n        command: scripts/scherzo-workspace-jj\n        capabilities: [assert-only, changed-files]\n",
    )
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(source),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  let assert Ok(profile) =
    dict.get(orchestrator.workspace_profiles.profiles, "noop")
  assert profile.source == config_types.ConfiguredWorkspaceHooks
  let assert Some(hooks) = profile.hooks
  let assert Some(driver) = profile.driver
  assert hooks.timeout_ms == 111
  assert driver.command == "scripts/scherzo-workspace-jj"
  assert driver.capabilities
    == [
      config_types.WorkspaceAssertOnly,
      config_types.WorkspaceChangedFiles,
    ]
  assert orchestrator.dag_hooks == hooks
}

pub fn workspace_profiles_reject_invalid_driver_shapes_test() {
  let missing_shape =
    invalid_workspace_error(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop: {}\n",
    )
  assert string.contains(missing_shape, "must define hooks")

  let empty_command =
    invalid_workspace_error(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      driver:\n        command: \"\"\n",
    )
  assert string.contains(empty_command, "command must be non-empty")

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

  let capabilities_not_list =
    invalid_workspace_error(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      driver:\n        command: run\n        capabilities: assert-only\n",
    )
  assert string.contains(capabilities_not_list, "capabilities must be a list")

  let unknown_capability =
    invalid_workspace_error(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      driver:\n        command: run\n        capabilities: [pull-request]\n",
    )
  assert string.contains(unknown_capability, "unknown workspace capability")

  let duplicate_capability =
    invalid_workspace_error(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      driver:\n        command: run\n        capabilities: [assert-only, assert-only]\n",
    )
  assert string.contains(duplicate_capability, "duplicate workspace capability")

  let invalid_timeout =
    invalid_workspace_error(
      "  root: workspaces\n  default_profile: noop\n  profiles:\n    noop:\n      driver:\n        command: run\n        timeout_ms: 0\n",
    )
  assert string.contains(invalid_timeout, "timeout_ms must be positive")
}

pub fn workspace_hooks_can_coexist_with_extra_profiles_test() {
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(base_config_with_workspace(
        "  root: workspaces\n  hooks:\n    create: legacy-create\n  profiles:\n    noop:\n      hooks:\n        create: noop-create\n",
      )),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert orchestrator.workspace_profiles.default_profile == "default"
  let assert Ok(default_profile) =
    dict.get(orchestrator.workspace_profiles.profiles, "default")
  let assert Ok(noop) =
    dict.get(orchestrator.workspace_profiles.profiles, "noop")
  assert default_profile.source == config_types.LegacyWorkspaceHooks
  assert noop.source == config_types.ConfiguredWorkspaceHooks
  let assert Some(default_hooks) = default_profile.hooks
  let assert Some(noop_hooks) = noop.hooks
  assert default_profile.driver == None
  assert noop.driver == None
  assert default_hooks.create == Some("legacy-create")
  assert noop_hooks.create == Some("noop-create")
  assert orchestrator.dag_hooks.create == Some("legacy-create")
}

pub fn examples_workspace_hook_profiles_remain_parseable_test() {
  let source =
    base_config_with_workspace(
      "  root: .scherzo/workspaces\n  default_profile: isolated\n  profiles:\n    isolated:\n      hooks:\n        create: |\n          mkdir -p \"$SCHERZO_WORKSPACE_PATH\"\n        before_step: |\n          test -d \"$SCHERZO_WORKSPACE_PATH\"\n        after_step: |\n          true\n        remove: |\n          rm -rf \"$SCHERZO_WORKSPACE_PATH\"\n        timeout_ms: 60000\n    noop:\n      hooks:\n        create: |\n          mkdir -p \"$SCHERZO_WORKSPACE_PATH\"\n        before_step: |\n          true\n        after_step: |\n          true\n        remove: |\n          rm -rf \"$SCHERZO_WORKSPACE_PATH\"\n        timeout_ms: 60000\n",
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
  assert isolated.source == config_types.ConfiguredWorkspaceHooks
  assert noop.source == config_types.ConfiguredWorkspaceHooks
  let assert Some(isolated_hooks) = isolated.hooks
  let assert Some(noop_hooks) = noop.hooks
  assert isolated_hooks.timeout_ms == 60_000
  assert noop_hooks.timeout_ms == 60_000
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
      "  root: workspaces\n  profiles:\n    noop:\n      hooks: {}\n",
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
      "  root: workspaces\n  default_profile: missing\n  profiles:\n    noop:\n      hooks: {}\n",
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
      "  root: workspaces\n  hooks:\n    create: legacy\n  profiles:\n    default:\n      hooks: {}\n",
    )
  let assert Error(error.InvalidConfig(message)) =
    config.resolve_orchestrator_root(
      root(collision),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert string.contains(message, "conflicts with legacy workspace.hooks")
}

pub fn orchestrator_config_parses_project_model_defaults_test() {
  let source =
    base_config(
      "pi:\n  model: github-copilot/gpt-5.1-codex\n  thinking: high\n",
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
  let invalid_thinking = base_config("pi:\n  thinking: extreme\n")
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_orchestrator_root(
      root(invalid_thinking),
      "test/tmp/config/scherzo.yaml",
      env,
    )

  let invalid_model = base_config("pi:\n  model: \"sonnet:high\"\n")
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_orchestrator_root(
      root(invalid_model),
      "test/tmp/config/scherzo.yaml",
      env,
    )

  let separate_provider =
    base_config("pi:\n  provider: openai\n  model: gpt-5\n")
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_orchestrator_root(
      root(separate_provider),
      "test/tmp/config/scherzo.yaml",
      env,
    )
}

pub fn orchestrator_config_validates_default_workflow_test() {
  let with_default =
    "version: 1\ntracker:\n  kind: linear\n  api_key: \"$LINEAR_API_KEY\"\n  project_slug: \"$LINEAR_PROJECT_SLUG\"\n  dispatch_states: [Todo]\nworkspace:\n  root: workspaces\nrouting:\n  workflow_label_prefix: \"workflow:\"\n  require_exactly_one_workflow_label: true\n  default_workflow: Implementation\n  workflows:\n    implementation: workflows/implementation.yaml\n"
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(with_default),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert orchestrator.routing.default_workflow == Some("implementation")

  let invalid_default =
    "version: 1\ntracker:\n  kind: linear\n  api_key: \"$LINEAR_API_KEY\"\n  project_slug: \"$LINEAR_PROJECT_SLUG\"\n  dispatch_states: [Todo]\nworkspace:\n  root: workspaces\nrouting:\n  workflow_label_prefix: \"workflow:\"\n  require_exactly_one_workflow_label: true\n  default_workflow: research\n  workflows:\n    implementation: workflows/implementation.yaml\n"
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
    "version: 1\ntracker:\n  kind: linear\n  api_key: \"$LINEAR_API_KEY\"\n  project_slug: \"$LINEAR_PROJECT_SLUG\"\n  dispatch_states: [Todo]\nworkspace:\n  root: workspaces\nrouting:\n  workflow_label_prefix: \"workflow:\"\n  require_exactly_one_workflow_label: true\n  workflows:\n    implementation: workflows/implementation.yaml\n    scheduled-maintenance: workflows/scheduled-maintenance.yaml\nscheduled_jobs:\n  - id: scheduled-maintenance\n    workflow: scheduled-maintenance\n    every: 15m\nlinear_contract:\n  workflow_labels: [implementation]\n"
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
    "version: 1\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\n  dispatch_states: [Todo]\nrouting:\n  workflows:\n    research: ../outside.yaml\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_orchestrator_root(
      root(source),
      "test/tmp/config/scherzo.yaml",
      env,
    )
}
