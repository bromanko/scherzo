import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config
import scherzo/config/types as config_types
import scherzo/error
import scherzo/linear_contract
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
    "version: 1\ntracker:\n  kind: linear\n  api_key: \"$LINEAR_API_KEY\"\n  project_slug: \"$LINEAR_PROJECT_SLUG\"\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: custom\n      command: scripts/scherzo-workspace-noop\n      timeout: 1234ms\nworkflows:\n    implementation: workflows/implementation.yaml\nartifact_limits:\n  command_stream_max_chars: 111\n  template_field_max_chars: 222\n  workflow_summary_max_chars: 333\n"
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
  assert string.contains(message, "was removed")
  assert string.contains(message, "workspace.drivers.<name>.type: custom")
}

pub fn workspace_drivers_resolve_default_and_named_drivers_test() {
  let source =
    base_config_with_workspace(
      "  root: workspaces\n  driver: isolated\n  drivers:\n    isolated:\n      type: custom\n      command: scripts/isolated\n      timeout: 111ms\n    noop:\n      type: custom\n      command: scripts/noop\n      timeout: 222ms\n",
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
  assert noop_driver.lifecycle
    == [
      config_types.LifecycleCreate,
      config_types.LifecycleBeforeStep,
      config_types.LifecycleAfterStep,
      config_types.LifecycleRemove,
    ]
  assert noop_driver.timeout_ms == 222
  assert orchestrator.dag_hooks == config_types.empty_dag_hooks()
}

pub fn workspace_driver_timeout_ms_is_rejected_test() {
  let message =
    invalid_workspace_error(
      "  root: workspaces\n  driver: legacy\n  drivers:\n    legacy:\n      type: custom\n      command: scripts/legacy\n      timeout_ms: 1234\n",
    )
  assert string.contains(message, "workspace.drivers.legacy.timeout_ms")
  assert string.contains(message, "workspace.drivers.legacy.timeout")
  assert string.contains(message, "SCHERZO_YAML_SIMPLIFIED_V1")
}

pub fn driver_workspace_profile_parses_schema_test() {
  let source =
    base_config_with_workspace(
      "  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: custom\n      command: \"$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-noop\"\n      timeout: 1234ms\n    default-timeout:\n      type: custom\n      command: scripts/default-timeout\n",
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
      config_types.LifecycleBeforeStep,
      config_types.LifecycleAfterStep,
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
      "  root: workspaces\n  driver: dogfood-jj\n  drivers:\n    dogfood-jj:\n      type: jj\n      remote: scherzo-agent\n      base_branch: main\n      fetch_base: true\n      publish_remote: scherzo-agent\n      github_repo: scherzo-systems/scherzo\n      timeout: 60s\n",
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
  assert driver.capabilities
    == [
      config_types.WorkspaceStatus,
      config_types.WorkspaceDiff,
      config_types.WorkspaceChangedFiles,
      config_types.WorkspaceAssertOnly,
      config_types.WorkspaceBaseline,
      config_types.WorkspaceRefreshBase,
      config_types.WorkspacePublishChange,
    ]
  assert driver.env
    == [
      #("SCHERZO_GITHUB_REPO", "scherzo-systems/scherzo"),
      #("SCHERZO_JJ_WORKSPACE_BASE_BRANCH", "main"),
      #("SCHERZO_JJ_WORKSPACE_FETCH_BASE", "true"),
      #("SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE", "scherzo-agent"),
      #("SCHERZO_JJ_WORKSPACE_REMOTE", "scherzo-agent"),
    ]
  assert driver.timeout_ms == 60_000
}

pub fn profile_local_hooks_are_rejected_even_with_driver_test() {
  let message =
    invalid_workspace_error(
      "  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: custom\n      command: scripts/scherzo-workspace-jj\n      hooks:\n        create: mkdir -p \"$SCHERZO_WORKSPACE_PATH\"\n",
    )
  assert string.contains(message, "workspace.drivers.noop.hooks")
  assert string.contains(message, "was removed")
  assert string.contains(message, "workspace.drivers.noop.type: custom")
}

pub fn rejects_removed_driver_capabilities_config_test() {
  let message =
    invalid_workspace_error(
      "  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: custom\n      command: run\n      capabilities: [assert-only]\n",
    )
  assert string.contains(message, "workspace.drivers.noop.capabilities")
  assert string.contains(message, "driver describe --json")
  assert string.contains(message, "SCHERZO_YAML_SIMPLIFIED_V1")
}

pub fn workspace_drivers_reject_invalid_driver_shapes_test() {
  let missing_type =
    invalid_workspace_error(
      "  root: workspaces\n  driver: noop\n  drivers:\n    noop: {}\n",
    )
  assert string.contains(
    missing_type,
    "workspace.drivers.noop.type is required",
  )

  let empty_command =
    invalid_workspace_error(
      "  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: custom\n      command: \"\"\n",
    )
  assert string.contains(empty_command, "command must be non-empty")

  let command_with_whitespace =
    invalid_workspace_error(
      "  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: custom\n      command: sh driver.sh\n",
    )
  assert string.contains(command_with_whitespace, "without whitespace")

  let command_with_shell_metacharacter =
    invalid_workspace_error(
      "  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: custom\n      command: scripts/driver;rm\n",
    )
  assert string.contains(
    command_with_shell_metacharacter,
    "shell metacharacters",
  )

  let unsupported_command_env =
    invalid_workspace_error(
      "  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: custom\n      command: $OTHER_ROOT/scripts/driver\n",
    )
  assert string.contains(unsupported_command_env, "$SCHERZO_REPO_ROOT")

  let command_on_builtin =
    invalid_workspace_error(
      "  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: noop\n      command: scripts/noop\n",
    )
  assert string.contains(command_on_builtin, "only valid for type: custom")

  let lifecycle_config =
    invalid_workspace_error(
      "  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: custom\n      command: run\n      lifecycle: [create]\n",
    )
  assert string.contains(lifecycle_config, "workspace.drivers.noop.lifecycle")
  assert string.contains(lifecycle_config, "Lifecycle selection was removed")

  let removed_capabilities =
    invalid_workspace_error(
      "  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: custom\n      command: run\n      capabilities: [assert-only]\n",
    )
  assert string.contains(
    removed_capabilities,
    "workspace.drivers.noop.capabilities",
  )
  assert string.contains(removed_capabilities, "describe --json")

  let invalid_timeout =
    invalid_workspace_error(
      "  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: custom\n      command: run\n      timeout: 0ms\n",
    )
  assert string.contains(invalid_timeout, "timeout must be positive")
}

pub fn workspace_hooks_cannot_coexist_with_driver_profiles_test() {
  let message =
    invalid_workspace_error(
      "  root: workspaces\n  hooks:\n    create: legacy-create\n  drivers:\n    noop:\n      type: custom\n      command: scripts/noop\n",
    )
  assert string.contains(message, "workspace.hooks")
  assert string.contains(message, "was removed")
}

pub fn examples_workspace_driver_profiles_remain_parseable_test() {
  let source =
    base_config_with_workspace(
      "  root: .scherzo/workspaces\n  driver: isolated\n  drivers:\n    isolated:\n      type: custom\n      command: scripts/scherzo-workspace-jj\n      timeout: 60s\n    noop:\n      type: custom\n      command: scripts/scherzo-workspace-noop\n      timeout: 60s\n",
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

pub fn workspace_drivers_reject_invalid_config_test() {
  let invalid_name =
    base_config_with_workspace(
      "  root: workspaces\n  drivers:\n    Bad:\n      type: custom\n      command: scripts/noop\n",
    )
  let assert Error(error.InvalidConfig(message)) =
    config.resolve_orchestrator_root(
      root(invalid_name),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert string.contains(message, "workspace.drivers.Bad")

  let unknown_driver =
    base_config_with_workspace(
      "  root: workspaces\n  driver: missing\n  drivers:\n    noop:\n      type: custom\n      command: scripts/noop\n",
    )
  let assert Error(error.InvalidConfig(message)) =
    config.resolve_orchestrator_root(
      root(unknown_driver),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert string.contains(message, "workspace.driver references unknown driver")

  let collision =
    base_config_with_workspace(
      "  root: workspaces\n  hooks:\n    create: legacy\n  drivers:\n    default:\n      type: custom\n      command: scripts/noop\n",
    )
  let assert Error(error.InvalidConfig(message)) =
    config.resolve_orchestrator_root(
      root(collision),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert string.contains(message, "workspace.hooks")
  assert string.contains(message, "was removed")
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

pub fn orchestrator_config_derives_linear_setup_workflow_labels_test() {
  let source =
    "version: 1\ntracker:\n  linear:\n    project: TEST\n    check_setup: true\nworkspace:\n  root: workspaces\ntask_routing:\n  labels:\n    require_exactly_one: false\n    default_workflow: implementation\nworkflows:\n    implementation: workflows/implementation.yaml\n"
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(source),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  let contract = orchestrator.effective.linear_contract
  assert contract.enabled == True
  assert contract.workflow_label_prefix == "workflow:"
  assert contract.workflow_labels == ["implementation"]
  assert contract.enforce_issue_workflow_labels == False
}

pub fn orchestrator_config_reads_linear_support_labels_test() {
  let source =
    "version: 1\ntracker:\n  linear:\n    project: TEST\n    check_setup: true\n    labels:\n      support: [Needs-Workflow, needs-workflow, Needs-Clarification]\nworkspace:\n  root: workspaces\nworkflows:\n    implementation: workflows/implementation.yaml\n"
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(source),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert orchestrator.effective.linear_contract.support_labels
    == ["needs-workflow", "needs-clarification"]
}

pub fn orchestrator_config_derives_linear_setup_state_requirements_test() {
  let source =
    "version: 1\ntracker:\n  linear:\n    project: TEST\n    check_setup: true\n  states:\n    ready: [Todo]\n    active: [Todo, In Progress]\n    terminal: [Done]\nworkspace:\n  root: workspaces\ntask_updates:\n  enabled: true\n  states:\n    claim: In Progress\n    success: In Review\n    failure: Triage\nworkflows:\n    implementation: workflows/implementation.yaml\n"
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(source),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  let diagnostics =
    linear_contract.check(
      orchestrator.effective,
      linear_contract.RemoteBoard(
        project_id: "project-id",
        project_slug: "TEST",
        project_name: "Project",
        teams: [
          linear_contract.RemoteTeam(
            id: "team-eng",
            key: "ENG",
            name: "Engineering",
            states: [
              linear_contract.RemoteState(
                id: "state-todo",
                name: "Todo",
                type_: "unstarted",
              ),
              linear_contract.RemoteState(
                id: "state-progress",
                name: "In Progress",
                type_: "started",
              ),
              linear_contract.RemoteState(
                id: "state-done",
                name: "Done",
                type_: "completed",
              ),
            ],
            labels: [
              linear_contract.RemoteLabel(
                id: "label-implementation",
                name: "workflow:implementation",
              ),
            ],
          ),
        ],
        workspace_labels: [],
      ),
    )
  assert diagnostics
    == [
      linear_contract.MissingState(
        team_key: "ENG",
        name: "In Review",
        source: "task_updates.states.success",
      ),
      linear_contract.MissingState(
        team_key: "ENG",
        name: "Triage",
        source: "task_updates.states.failure",
      ),
    ]
}

pub fn orchestrator_config_allows_scheduled_only_routes_outside_linear_setup_labels_test() {
  let source =
    "version: 1\ntracker:\n  kind: linear\n  api_key: \"$LINEAR_API_KEY\"\n  project_slug: \"$LINEAR_PROJECT_SLUG\"\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\nworkflows:\n    implementation: workflows/implementation.yaml\n    scheduled-maintenance: workflows/scheduled-maintenance.yaml\nscheduled_jobs:\n  - id: scheduled-maintenance\n    workflow: scheduled-maintenance\n    every: 15m\n"
  let assert Ok(orchestrator) =
    config.resolve_orchestrator_root(
      root(source),
      "test/tmp/config/scherzo.yaml",
      env,
    )
  assert orchestrator.effective.linear_contract.workflow_labels
    == ["implementation"]
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
