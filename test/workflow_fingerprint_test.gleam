import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config
import scherzo/config/types as config_types
import scherzo/model_config
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import scherzo/workflow_dag
import scherzo/workflow_fingerprint

fn parse(content: String) -> workflow_dag.WorkflowDag {
  let assert Ok(dag) = workflow_dag.parse(content)
  dag
}

fn hooks(create: Option(String)) -> config_types.DagHooksConfig {
  config_types.DagHooksConfig(
    create: create,
    before_step: None,
    after_step: None,
    remove: None,
    timeout_ms: 1000,
  )
}

fn profile(
  name: String,
  hooks: config_types.DagHooksConfig,
) -> config_types.WorkspaceHookProfile {
  config_types.WorkspaceHookProfile(
    name: name,
    hooks: Some(hooks),
    driver: None,
    source: config_types.ConfiguredWorkspaceHooks,
  )
}

fn driver_profile(
  command: String,
  lifecycle: List(config_types.WorkspaceLifecycleOperation),
  capabilities: List(config_types.WorkspaceCapability),
) -> config_types.WorkspaceHookProfile {
  driver_profile_with_timeout(command, lifecycle, capabilities, 1000)
}

fn hook_profile_with_driver(
  name: String,
  hooks: config_types.DagHooksConfig,
  command: String,
  capabilities: List(config_types.WorkspaceCapability),
) -> config_types.WorkspaceHookProfile {
  config_types.WorkspaceHookProfile(
    name: name,
    hooks: Some(hooks),
    driver: Some(config_types.WorkspaceDriverConfig(
      command: command,
      lifecycle: [],
      capabilities: capabilities,
      timeout_ms: hooks.timeout_ms,
    )),
    source: config_types.ConfiguredWorkspaceHooks,
  )
}

fn driver_profile_with_timeout(
  command: String,
  lifecycle: List(config_types.WorkspaceLifecycleOperation),
  capabilities: List(config_types.WorkspaceCapability),
  timeout_ms: Int,
) -> config_types.WorkspaceHookProfile {
  config_types.WorkspaceHookProfile(
    name: "noop",
    hooks: None,
    driver: Some(config_types.WorkspaceDriverConfig(
      command: command,
      lifecycle: lifecycle,
      capabilities: capabilities,
      timeout_ms: timeout_ms,
    )),
    source: config_types.ConfiguredWorkspaceDriver,
  )
}

fn limits(command_stream_max_chars: Int) -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: command_stream_max_chars,
    template_field_max_chars: 1000,
    workflow_summary_max_chars: 4000,
  )
}

fn orchestrator_with_profiles(
  profiles: List(#(String, config_types.WorkspaceHookProfile)),
) -> config_types.OrchestratorConfig {
  config_types.OrchestratorConfig(
    effective: config_types.EffectiveConfig(
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
      workspace: config_types.WorkspaceConfig(root: "workspaces"),
      hooks: config.default_hooks_config(),
      agent: config.default_agent_config(),
      pi: config.default_pi_config(),
      handoff: config.default_handoff_config(),
      linear_contract: config.default_linear_contract_config(),
      linear_commands: config.default_linear_command_config(),
    ),
    config_dir: ".",
    routing: config_types.RoutingConfig(
      workflow_label_prefix: "workflow:",
      require_exactly_one_workflow_label: True,
      default_workflow: None,
      workflows: dict.new(),
    ),
    dag_hooks: hooks(None),
    workspace_profiles: config_types.WorkspaceHookProfiles(
      default_profile: "noop",
      profiles: dict.from_list(profiles),
    ),
    artifact_limits: limits(1000),
    model_settings: model_config.default_settings(),
    scheduled_jobs: [],
  )
}

pub fn workflow_fingerprint_ignores_yaml_comments_and_step_order_test() {
  let first =
    parse(
      "version: 1\nid: implementation\n# comment\nmax_parallel_steps: 2\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n  - id: summarize\n    kind: command\n    depends_on: [collect]\n    run: summarize\n    workspace: main\n",
    )
  let second =
    parse(
      "version: 1\nid: implementation\nmax_parallel_steps: 2\nsteps:\n  - id: summarize\n    workspace: main\n    run: summarize\n    depends_on: [collect]\n    kind: command\n  - id: collect\n    workspace: main\n    run: collect\n    kind: command\n",
    )

  assert workflow_fingerprint.for_dag("implementation", first)
    == workflow_fingerprint.for_dag("implementation", second)
}

pub fn workflow_fingerprint_changes_for_semantic_fields_test() {
  let base =
    parse(
      "version: 1\nid: implementation\nmax_parallel_steps: 1\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n  - id: summarize\n    kind: command\n    depends_on: [collect]\n    run: summarize\n    workspace: main\n",
    )
  let changed_command =
    parse(
      "version: 1\nid: implementation\nmax_parallel_steps: 1\nsteps:\n  - id: collect\n    kind: command\n    run: collect changed\n    workspace: main\n  - id: summarize\n    kind: command\n    depends_on: [collect]\n    run: summarize\n    workspace: main\n",
    )
  let changed_parallelism =
    parse(
      "version: 1\nid: implementation\nmax_parallel_steps: 2\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n  - id: summarize\n    kind: command\n    depends_on: [collect]\n    run: summarize\n    workspace: main\n",
    )

  let base_fingerprint = workflow_fingerprint.for_dag("implementation", base)
  assert base_fingerprint
    != workflow_fingerprint.for_dag("implementation", changed_command)
  assert base_fingerprint
    != workflow_fingerprint.for_dag("implementation", changed_parallelism)
}

pub fn workflow_fingerprint_changes_for_structured_output_contract_test() {
  let unstructured =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    workspace: main\n",
    )
  let structured =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    workspace: main\n    structured_output:\n      artifact_name: review_result\n      required: true\n      schema:\n        required: [summary, findings]\n",
    )

  assert workflow_fingerprint.for_dag("implementation", unstructured)
    != workflow_fingerprint.for_dag("implementation", structured)
  assert string.contains(
    workflow_fingerprint.canonical_input(structured),
    "structured_output",
  )
}

pub fn workflow_dag_fingerprint_includes_explicit_workspace_profile_test() {
  let noop =
    parse(
      "version: 1\nid: implementation\nworkspace_profile: noop\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )
  let isolated =
    parse(
      "version: 1\nid: implementation\nworkspace_profile: isolated\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )
  let omitted =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )
  assert workflow_fingerprint.for_dag("implementation", noop)
    != workflow_fingerprint.for_dag("implementation", isolated)
  assert !string.contains(
    workflow_fingerprint.canonical_input(omitted),
    "workspace_profile",
  )
}

pub fn workflow_dag_fingerprint_includes_workspace_capabilities_test() {
  let omitted =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )
  let required =
    parse(
      "version: 1\nid: implementation\nworkspace_capabilities: [assert-only]\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )
  let reordered =
    parse(
      "version: 1\nid: implementation\nworkspace_capabilities: [changed-files, assert-only]\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )
  let canonical =
    parse(
      "version: 1\nid: implementation\nworkspace_capabilities: [assert-only, changed-files]\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )

  assert workflow_fingerprint.for_dag("implementation", omitted)
    != workflow_fingerprint.for_dag("implementation", required)
  assert workflow_fingerprint.for_dag("implementation", reordered)
    == workflow_fingerprint.for_dag("implementation", canonical)
  assert string.contains(
    workflow_fingerprint.canonical_input(required),
    "workspace_capabilities",
  )
}

pub fn execution_fingerprint_uses_selected_workspace_profile_test() {
  let dag =
    parse(
      "version: 1\nid: implementation\nworkspace_profile: noop\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )
  let settings = model_config.default_settings()
  let noop = profile("noop", hooks(Some("create")))
  let renamed = profile("isolated", hooks(Some("create")))
  let changed = profile("noop", hooks(Some("changed")))
  let base =
    workflow_fingerprint.for_execution_profile_options(
      "implementation",
      dag,
      noop,
      limits(1000),
      settings,
    )
  assert base
    != workflow_fingerprint.for_execution_profile_options(
      "implementation",
      dag,
      renamed,
      limits(1000),
      settings,
    )
  assert base
    != workflow_fingerprint.for_execution_profile_options(
      "implementation",
      dag,
      changed,
      limits(1000),
      settings,
    )
  assert string.contains(
    workflow_fingerprint.canonical_execution_input_for_profile(
      "implementation",
      dag,
      noop,
      limits(1000),
      settings,
    ),
    "workspace_profile",
  )
  assert !string.contains(
    workflow_fingerprint.canonical_execution_input_for(
      "implementation",
      dag,
      hooks(Some("create")),
      limits(1000),
      settings,
    ),
    "\"workspace_profile\":{",
  )
}

pub fn execution_fingerprint_includes_selected_driver_metadata_test() {
  let dag =
    parse(
      "version: 1\nid: implementation\nworkspace_profile: noop\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )
  let settings = model_config.default_settings()
  let base =
    driver_profile(
      "scripts/noop",
      [config_types.LifecycleCreate, config_types.LifecycleRemove],
      [config_types.WorkspaceAssertOnly],
    )
  let changed_command =
    driver_profile(
      "scripts/changed",
      [config_types.LifecycleCreate, config_types.LifecycleRemove],
      [config_types.WorkspaceAssertOnly],
    )
  let changed_lifecycle =
    driver_profile("scripts/noop", [config_types.LifecycleCreate], [
      config_types.WorkspaceAssertOnly,
    ])
  let changed_capabilities =
    driver_profile(
      "scripts/noop",
      [config_types.LifecycleCreate, config_types.LifecycleRemove],
      [config_types.WorkspaceStatus],
    )
  let changed_timeout =
    driver_profile_with_timeout(
      "scripts/noop",
      [config_types.LifecycleCreate, config_types.LifecycleRemove],
      [config_types.WorkspaceAssertOnly],
      2000,
    )
  let fingerprint =
    workflow_fingerprint.for_execution_profile_options(
      "implementation",
      dag,
      base,
      limits(1000),
      settings,
    )
  assert fingerprint
    != workflow_fingerprint.for_execution_profile_options(
      "implementation",
      dag,
      changed_command,
      limits(1000),
      settings,
    )
  assert fingerprint
    != workflow_fingerprint.for_execution_profile_options(
      "implementation",
      dag,
      changed_lifecycle,
      limits(1000),
      settings,
    )
  assert fingerprint
    != workflow_fingerprint.for_execution_profile_options(
      "implementation",
      dag,
      changed_capabilities,
      limits(1000),
      settings,
    )
  assert fingerprint
    != workflow_fingerprint.for_execution_profile_options(
      "implementation",
      dag,
      changed_timeout,
      limits(1000),
      settings,
    )
  assert string.contains(
    workflow_fingerprint.canonical_execution_input_for_profile(
      "implementation",
      dag,
      base,
      limits(1000),
      settings,
    ),
    "workspace_driver",
  )
}

pub fn execution_fingerprint_changes_for_hook_profile_driver_context_test() {
  let dag =
    parse(
      "version: 1\nid: implementation\nworkspace_profile: noop\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )
  let settings = model_config.default_settings()
  let base =
    hook_profile_with_driver("noop", hooks(Some("create")), "scripts/noop", [
      config_types.WorkspaceAssertOnly,
    ])
  let changed_command =
    hook_profile_with_driver("noop", hooks(Some("create")), "scripts/changed", [
      config_types.WorkspaceAssertOnly,
    ])
  let changed_capabilities =
    hook_profile_with_driver("noop", hooks(Some("create")), "scripts/noop", [
      config_types.WorkspaceChangedFiles,
    ])
  let fingerprint =
    workflow_fingerprint.for_execution_profile_options(
      "implementation",
      dag,
      base,
      limits(1000),
      settings,
    )

  assert fingerprint
    != workflow_fingerprint.for_execution_profile_options(
      "implementation",
      dag,
      changed_command,
      limits(1000),
      settings,
    )
  assert fingerprint
    != workflow_fingerprint.for_execution_profile_options(
      "implementation",
      dag,
      changed_capabilities,
      limits(1000),
      settings,
    )
}

pub fn execution_fingerprint_canonicalizes_driver_list_order_test() {
  let dag =
    parse(
      "version: 1\nid: implementation\nworkspace_profile: noop\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )
  let settings = model_config.default_settings()
  let first =
    driver_profile(
      "scripts/noop",
      [config_types.LifecycleRemove, config_types.LifecycleCreate],
      [config_types.WorkspaceChangedFiles, config_types.WorkspaceAssertOnly],
    )
  let second =
    driver_profile(
      "scripts/noop",
      [config_types.LifecycleCreate, config_types.LifecycleRemove],
      [config_types.WorkspaceAssertOnly, config_types.WorkspaceChangedFiles],
    )

  assert workflow_fingerprint.for_execution_profile_options(
      "implementation",
      dag,
      first,
      limits(1000),
      settings,
    )
    == workflow_fingerprint.for_execution_profile_options(
      "implementation",
      dag,
      second,
      limits(1000),
      settings,
    )
}

pub fn execution_fingerprint_ignores_unselected_driver_profiles_test() {
  let dag =
    parse(
      "version: 1\nid: implementation\nworkspace_profile: noop\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )
  let selected = profile("noop", hooks(Some("create")))
  let first_orchestrator =
    orchestrator_with_profiles([
      #("noop", selected),
      #(
        "unused",
        driver_profile("scripts/unused-one", [config_types.LifecycleCreate], [
          config_types.WorkspaceAssertOnly,
        ]),
      ),
    ])
  let second_orchestrator =
    orchestrator_with_profiles([
      #("noop", selected),
      #(
        "unused",
        driver_profile("scripts/unused-two", [config_types.LifecycleRemove], [
          config_types.WorkspaceStatus,
        ]),
      ),
    ])

  let assert Ok(first) =
    workflow_fingerprint.fingerprint_for_execution(dag, first_orchestrator)
  let assert Ok(second) =
    workflow_fingerprint.fingerprint_for_execution(dag, second_orchestrator)
  assert first == second
}

pub fn workflow_execution_fingerprint_changes_for_hooks_and_artifact_limits_test() {
  let dag =
    parse(
      "version: 1\nid: implementation\nmax_parallel_steps: 1\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )
  let settings = model_config.default_settings()
  let base =
    workflow_fingerprint.for_execution_options(
      "implementation",
      dag,
      hooks(None),
      limits(1000),
      settings,
    )

  assert base
    != workflow_fingerprint.for_execution_options(
      "implementation",
      dag,
      hooks(Some("create-workspace")),
      limits(1000),
      settings,
    )
  assert base
    != workflow_fingerprint.for_execution_options(
      "implementation",
      dag,
      hooks(None),
      limits(2000),
      settings,
    )
}
