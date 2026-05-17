import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/command_step
import scherzo/config
import scherzo/config/types as config_types
import scherzo/model_config
import scherzo/step_artifact
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import scherzo/workflow_dag
import scherzo/workflow_fingerprint
import scherzo/workspace_driver_discovery
import simplifile

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
    driver: Some(
      config_types.WorkspaceDriverConfig(
        command: command,
        lifecycle: [],
        capabilities: capabilities,
        timeout_ms: hooks.timeout_ms,
        env: [],
      ),
    ),
    source: config_types.ConfiguredWorkspaceHooks,
  )
}

fn driver_profile_with_timeout(
  command: String,
  lifecycle: List(config_types.WorkspaceLifecycleOperation),
  capabilities: List(config_types.WorkspaceCapability),
  timeout_ms: Int,
) -> config_types.WorkspaceHookProfile {
  driver_profile_with_env(command, lifecycle, capabilities, timeout_ms, [])
}

fn driver_profile_with_env(
  command: String,
  lifecycle: List(config_types.WorkspaceLifecycleOperation),
  capabilities: List(config_types.WorkspaceCapability),
  timeout_ms: Int,
  env: List(#(String, String)),
) -> config_types.WorkspaceHookProfile {
  config_types.WorkspaceHookProfile(
    name: "noop",
    hooks: None,
    driver: Some(config_types.WorkspaceDriverConfig(
      command: command,
      lifecycle: lifecycle,
      capabilities: capabilities,
      timeout_ms: timeout_ms,
      env: env,
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

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

fn shell_quote(value: String) -> String {
  "'" <> string.replace(value, each: "'", with: "'\\''") <> "'"
}

fn chmod_executable(path: String) -> Nil {
  let artifact =
    command_step.run(
      "chmod_fingerprint_driver",
      "chmod +x " <> shell_quote(path),
      ".",
      5000,
      [],
      limits(4000),
    )
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
}

fn write_describe_driver(path: String, capabilities_json: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "if [ \"$1\" = describe ] && [ \"$2\" = --json ]; then\n"
        <> "  printf '%s\\n' '{\"version\":1,\"capabilities\":"
        <> capabilities_json
        <> "}'\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "exit 2\n",
    )
  chmod_executable(path)
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
      "version: 1\nid: implementation\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    workspace: main\n    structured_output:\n      artifact_name: review_result\n      required: true\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_result\n      schema:\n        required: [summary, findings]\n",
    )

  assert workflow_fingerprint.for_dag("implementation", unstructured)
    != workflow_fingerprint.for_dag("implementation", structured)
  assert string.contains(
    workflow_fingerprint.canonical_input(structured),
    "structured_output",
  )
}

pub fn workflow_fingerprint_changes_for_structured_output_source_test() {
  let first_tool_call =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: example_json\n    kind: agent\n    prompt: prompts/example.md\n    workspace: main\n    structured_output:\n      artifact_name: example_artifact\n      source:\n        type: pi_tool_call\n        tool_name: submit_example_artifact\n        require_single: true\n        reject_sibling_tool_calls: true\n      schema:\n        required: [schema_version, artifact_type]\n",
    )
  let second_tool_call =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: example_json\n    kind: agent\n    prompt: prompts/example.md\n    workspace: main\n    structured_output:\n      artifact_name: example_artifact\n      source:\n        type: pi_tool_call\n        tool_name: submit_other_artifact\n        require_single: true\n        reject_sibling_tool_calls: true\n      schema:\n        required: [schema_version, artifact_type]\n",
    )

  assert workflow_fingerprint.for_dag("implementation", first_tool_call)
    != workflow_fingerprint.for_dag("implementation", second_tool_call)
  assert string.contains(
    workflow_fingerprint.canonical_input(first_tool_call),
    "submit_example_artifact",
  )
}

pub fn workflow_fingerprint_changes_for_structured_output_validators_test() {
  let without_validator =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    workspace: main\n    structured_output:\n      artifact_name: review_result\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_result\n      schema:\n        required: [summary, findings]\n",
    )
  let with_schema_validator =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    workspace: main\n    structured_output:\n      artifact_name: review_result\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_result\n      schema:\n        required: [summary, findings]\n      validators:\n        - name: shape\n          type: json_schema\n          path: schemas/review.schema.json\n",
    )
  let with_command_validator =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    workspace: main\n    structured_output:\n      artifact_name: review_result\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_result\n      schema:\n        required: [summary, findings]\n      validators:\n        - name: semantics\n          type: command\n          argv: [python3, scripts/validate]\n          timeout_ms: 30000\n          env:\n            CHECK_MODE: strict\n",
    )
  let with_changed_env =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    workspace: main\n    structured_output:\n      artifact_name: review_result\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_result\n      schema:\n        required: [summary, findings]\n      validators:\n        - name: semantics\n          type: command\n          argv: [python3, scripts/validate]\n          timeout_ms: 30000\n          env:\n            CHECK_MODE: relaxed\n",
    )

  let base = workflow_fingerprint.for_dag("implementation", without_validator)
  assert base
    != workflow_fingerprint.for_dag("implementation", with_schema_validator)
  assert base
    != workflow_fingerprint.for_dag("implementation", with_command_validator)
  assert workflow_fingerprint.for_dag("implementation", with_command_validator)
    != workflow_fingerprint.for_dag("implementation", with_changed_env)
  assert string.contains(
    workflow_fingerprint.canonical_input(with_schema_validator),
    "validator_contract_version",
  )
}

pub fn execution_fingerprint_changes_for_json_schema_content_hash_test() {
  let dir = "test/tmp/workflow-fingerprint-schema-hash"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/.scherzo")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/schemas")
  let schema_path = dir <> "/schemas/review.schema.json"
  let dag =
    parse(
      "version: 1\nid: implementation\nworkspace_profile: noop\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    workspace: main\n    structured_output:\n      artifact_name: review_result\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_result\n      validators:\n        - name: shape\n          type: json_schema\n          path: schemas/review.schema.json\n",
    )
  let orchestrator =
    orchestrator_with_profiles([#("noop", profile("noop", hooks(None)))])
  let orchestrator =
    config_types.OrchestratorConfig(
      ..orchestrator,
      config_dir: dir <> "/.scherzo",
    )

  let assert Ok(Nil) = simplifile.write(schema_path, "{\"type\":\"object\"}\n")
  let assert Ok(first) =
    workflow_fingerprint.fingerprint_for_execution(dag, orchestrator)
  let assert Ok(Nil) =
    simplifile.write(
      schema_path,
      "{\"type\":\"object\",\"required\":[\"summary\"]}\n",
    )
  let assert Ok(second) =
    workflow_fingerprint.fingerprint_for_execution(dag, orchestrator)

  assert first != second
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

pub fn execution_fingerprint_includes_profile_driver_env_digests_test() {
  let dag =
    parse(
      "version: 1\nid: implementation\nworkspace_profile: noop\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )
  let settings = model_config.default_settings()
  let base =
    driver_profile_with_env(
      "scripts/noop",
      [config_types.LifecycleCreate],
      [config_types.WorkspaceAssertOnly],
      1000,
      [#("DRIVER_SECRET_TOKEN", "driver-env-redaction-token")],
    )
  let changed_value =
    driver_profile_with_env(
      "scripts/noop",
      [config_types.LifecycleCreate],
      [config_types.WorkspaceAssertOnly],
      1000,
      [#("DRIVER_SECRET_TOKEN", "different-token")],
    )
  let reordered =
    driver_profile_with_env(
      "scripts/noop",
      [config_types.LifecycleCreate],
      [config_types.WorkspaceAssertOnly],
      1000,
      [
        #("SCHERZO_JJ_WORKSPACE_BASE", "profile-base"),
        #("DRIVER_SECRET_TOKEN", "driver-env-redaction-token"),
      ],
    )
  let reordered_same =
    driver_profile_with_env(
      "scripts/noop",
      [config_types.LifecycleCreate],
      [config_types.WorkspaceAssertOnly],
      1000,
      [
        #("DRIVER_SECRET_TOKEN", "driver-env-redaction-token"),
        #("SCHERZO_JJ_WORKSPACE_BASE", "profile-base"),
      ],
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
      changed_value,
      limits(1000),
      settings,
    )
  assert workflow_fingerprint.for_execution_profile_options(
      "implementation",
      dag,
      reordered,
      limits(1000),
      settings,
    )
    == workflow_fingerprint.for_execution_profile_options(
      "implementation",
      dag,
      reordered_same,
      limits(1000),
      settings,
    )

  let canonical =
    workflow_fingerprint.canonical_execution_input_for_profile(
      "implementation",
      dag,
      base,
      limits(1000),
      settings,
    )
  assert string.contains(canonical, "DRIVER_SECRET_TOKEN")
  assert string.contains(canonical, "value_sha256")
  assert !string.contains(canonical, "driver-env-redaction-token")
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

pub fn execution_fingerprint_uses_discovered_driver_capabilities_test() {
  let dir = "test/tmp/workflow-fingerprint-discovered-driver"
  reset_dir(dir)
  let driver = dir <> "/driver.sh"
  let dag =
    parse(
      "version: 1\nid: implementation\nworkspace_profile: noop\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )
  let base_orchestrator =
    orchestrator_with_profiles([
      #("noop", driver_profile(driver, [], [])),
    ])

  write_describe_driver(driver, "[\"changed-files\",\"assert-only\"]")
  let assert Ok(first_orchestrator) =
    workspace_driver_discovery.enrich_orchestrator(base_orchestrator)
  write_describe_driver(driver, "[\"assert-only\",\"changed-files\"]")
  let assert Ok(reordered_orchestrator) =
    workspace_driver_discovery.enrich_orchestrator(base_orchestrator)
  write_describe_driver(driver, "[\"assert-only\"]")
  let assert Ok(changed_orchestrator) =
    workspace_driver_discovery.enrich_orchestrator(base_orchestrator)

  let assert Ok(first) =
    workflow_fingerprint.fingerprint_for_execution(dag, first_orchestrator)
  let assert Ok(reordered) =
    workflow_fingerprint.fingerprint_for_execution(dag, reordered_orchestrator)
  let assert Ok(changed) =
    workflow_fingerprint.fingerprint_for_execution(dag, changed_orchestrator)

  assert first == reordered
  assert first != changed
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

pub fn workflow_fingerprint_changes_for_generic_pi_tool_call_schema_contract_test() {
  let base =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: example_json\n    kind: agent\n    prompt: prompts/example.md\n    structured_output:\n      artifact_name: example_artifact\n      source:\n        type: pi_tool_call\n        tool_name: submit_structured_output\n        parameters_schema_path: .scherzo/workflows/schemas/review-lane-draft.correctness.v1.schema.json\n      validators:\n        - name: shape\n          type: json_schema\n          path: .scherzo/workflows/schemas/review-lane-draft.correctness.v1.schema.json\n      schema:\n        required: [schema_version, artifact_type]\n",
    )
  let changed_tool =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: example_json\n    kind: agent\n    prompt: prompts/example.md\n    structured_output:\n      artifact_name: example_artifact\n      source:\n        type: pi_tool_call\n        tool_name: submit_other_output\n        parameters_schema_path: .scherzo/workflows/schemas/review-lane-draft.correctness.v1.schema.json\n      validators:\n        - name: shape\n          type: json_schema\n          path: .scherzo/workflows/schemas/review-lane-draft.correctness.v1.schema.json\n      schema:\n        required: [schema_version, artifact_type]\n",
    )
  let changed_schema =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: example_json\n    kind: agent\n    prompt: prompts/example.md\n    structured_output:\n      artifact_name: example_artifact\n      source:\n        type: pi_tool_call\n        tool_name: submit_structured_output\n        parameters_schema_path: .scherzo/workflows/schemas/review-lane-draft.test-quality.v1.schema.json\n      validators:\n        - name: shape\n          type: json_schema\n          path: .scherzo/workflows/schemas/review-lane-draft.test-quality.v1.schema.json\n      schema:\n        required: [schema_version, artifact_type]\n",
    )
  let changed_validator =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: example_json\n    kind: agent\n    prompt: prompts/example.md\n    structured_output:\n      artifact_name: example_artifact\n      source:\n        type: pi_tool_call\n        tool_name: submit_structured_output\n        parameters_schema_path: .scherzo/workflows/schemas/review-lane-draft.correctness.v1.schema.json\n      validators:\n        - name: shape_changed\n          type: json_schema\n          path: .scherzo/workflows/schemas/review-lane-draft.correctness.v1.schema.json\n      schema:\n        required: [schema_version, artifact_type]\n",
    )

  let fingerprint = workflow_fingerprint.for_dag("implementation", base)
  assert fingerprint
    != workflow_fingerprint.for_dag("implementation", changed_tool)
  assert fingerprint
    != workflow_fingerprint.for_dag("implementation", changed_schema)
  assert fingerprint
    != workflow_fingerprint.for_dag("implementation", changed_validator)
  assert string.contains(
    workflow_fingerprint.canonical_input(base),
    "parameters_schema_path",
  )
}

pub fn workflow_fingerprint_changes_for_contract_type_test() {
  let exec_plan =
    parse(
      "version: 1\nid: draft\ncontract:\n  version: 1\n  outputs:\n    exec_plan:\n      type: exec_plan\n      source:\n        step: draft_execplan\n        field: final_response\nsteps:\n  - id: draft_execplan\n    kind: agent\n    prompt: prompts/draft.md\n",
    )
  let markdown =
    parse(
      "version: 1\nid: draft\ncontract:\n  version: 1\n  outputs:\n    exec_plan:\n      type: document.markdown\n      source:\n        step: draft_execplan\n        field: final_response\nsteps:\n  - id: draft_execplan\n    kind: agent\n    prompt: prompts/draft.md\n",
    )
  assert workflow_fingerprint.for_dag("draft", exec_plan)
    != workflow_fingerprint.for_dag("draft", markdown)
}

pub fn workflow_fingerprint_contract_map_order_is_stable_test() {
  let first =
    parse(
      "version: 1\nid: research\ncontract:\n  version: 1\n  inputs:\n    prompt:\n      type: text\n      source: issue_context\n    attachments: artifact[]\n  outputs:\n    findings:\n      type: document.markdown\n      source:\n        step: collect_findings\n        field: stdout\nsteps:\n  - id: collect_findings\n    kind: command\n    run: echo findings\n",
    )
  let reordered =
    parse(
      "version: 1\nid: research\ncontract:\n  outputs:\n    findings:\n      source:\n        field: stdout\n        step: collect_findings\n      type: document.markdown\n  inputs:\n    attachments: artifact[]\n    prompt:\n      source: issue_context\n      type: text\n  version: 1\nsteps:\n  - run: echo findings\n    kind: command\n    id: collect_findings\n",
    )
  assert workflow_fingerprint.for_dag("research", first)
    == workflow_fingerprint.for_dag("research", reordered)
}

pub fn workflow_fingerprint_changes_for_contract_source_kind_test() {
  let issue_context =
    parse(
      "version: 1\nid: research\ncontract:\n  version: 1\n  inputs:\n    prompt:\n      type: text\n      source: issue_context\nsteps:\n  - id: collect_findings\n    kind: command\n    run: echo findings\n",
    )
  let scheduled_context =
    parse(
      "version: 1\nid: research\ncontract:\n  version: 1\n  inputs:\n    prompt:\n      type: text\n      source: scheduled_context\nsteps:\n  - id: collect_findings\n    kind: command\n    run: echo findings\n",
    )
  assert workflow_fingerprint.for_dag("research", issue_context)
    != workflow_fingerprint.for_dag("research", scheduled_context)
  assert string.contains(
    workflow_fingerprint.canonical_input(issue_context),
    "issue_context",
  )
}

pub fn v2_workflow_fingerprint_includes_structured_output_and_contract_types_test() {
  let base =
    parse(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: materialize_bundle\n        field: stdout\nsteps:\n  - id: draft\n    kind: agent\n    prompt: prompts/execplan-draft.md\n    workspace: main\n    structured_output:\n      artifact_name: implementation_pack_submission\n      source:\n        type: pi_tool_call\n        tool_name: submit_implementation_pack_submission\n        parameters_schema_path: .scherzo/workflows/schemas/implementation-pack-submission.v2.schema.json\n      validators:\n        - name: shape\n          type: json_schema\n          path: .scherzo/workflows/schemas/implementation-pack-submission.v2.schema.json\n  - id: materialize_bundle\n    kind: command\n    depends_on: [draft]\n    run: scripts/scherzo-execplan materialize-bundle\n",
    )
  let changed_schema =
    parse(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: materialize_bundle\n        field: stdout\nsteps:\n  - id: draft\n    kind: agent\n    prompt: prompts/execplan-draft.md\n    workspace: main\n    structured_output:\n      artifact_name: implementation_pack_submission\n      source:\n        type: pi_tool_call\n        tool_name: submit_implementation_pack_submission\n        parameters_schema_path: .scherzo/workflows/schemas/exec-plan-revision-submission.v2.schema.json\n      validators:\n        - name: shape\n          type: json_schema\n          path: .scherzo/workflows/schemas/exec-plan-revision-submission.v2.schema.json\n  - id: materialize_bundle\n    kind: command\n    depends_on: [draft]\n    run: scripts/scherzo-execplan materialize-bundle\n",
    )
  let changed_tool =
    parse(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: materialize_bundle\n        field: stdout\nsteps:\n  - id: draft\n    kind: agent\n    prompt: prompts/execplan-draft.md\n    workspace: main\n    structured_output:\n      artifact_name: implementation_pack_submission\n      source:\n        type: pi_tool_call\n        tool_name: submit_other_pack_submission\n        parameters_schema_path: .scherzo/workflows/schemas/implementation-pack-submission.v2.schema.json\n      validators:\n        - name: shape\n          type: json_schema\n          path: .scherzo/workflows/schemas/implementation-pack-submission.v2.schema.json\n  - id: materialize_bundle\n    kind: command\n    depends_on: [draft]\n    run: scripts/scherzo-execplan materialize-bundle\n",
    )
  let changed_command =
    parse(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: materialize_bundle\n        field: stdout\nsteps:\n  - id: draft\n    kind: agent\n    prompt: prompts/execplan-draft.md\n    workspace: main\n    structured_output:\n      artifact_name: implementation_pack_submission\n      source:\n        type: pi_tool_call\n        tool_name: submit_implementation_pack_submission\n        parameters_schema_path: .scherzo/workflows/schemas/implementation-pack-submission.v2.schema.json\n      validators:\n        - name: shape\n          type: json_schema\n          path: .scherzo/workflows/schemas/implementation-pack-submission.v2.schema.json\n  - id: materialize_bundle\n    kind: command\n    depends_on: [draft]\n    run: scripts/scherzo-execplan materialize-bundle --changed\n",
    )
  let changed_output_type =
    parse(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: code_change_bundle\n      source:\n        step: materialize_bundle\n        field: stdout\nsteps:\n  - id: draft\n    kind: agent\n    prompt: prompts/execplan-draft.md\n    workspace: main\n    structured_output:\n      artifact_name: implementation_pack_submission\n      source:\n        type: pi_tool_call\n        tool_name: submit_implementation_pack_submission\n        parameters_schema_path: .scherzo/workflows/schemas/implementation-pack-submission.v2.schema.json\n      validators:\n        - name: shape\n          type: json_schema\n          path: .scherzo/workflows/schemas/implementation-pack-submission.v2.schema.json\n  - id: materialize_bundle\n    kind: command\n    depends_on: [draft]\n    run: scripts/scherzo-execplan materialize-bundle\n",
    )

  let fingerprint = workflow_fingerprint.for_dag("execplan", base)
  assert fingerprint != workflow_fingerprint.for_dag("execplan", changed_schema)
  assert fingerprint != workflow_fingerprint.for_dag("execplan", changed_tool)
  assert fingerprint
    != workflow_fingerprint.for_dag("execplan", changed_command)
  assert fingerprint
    != workflow_fingerprint.for_dag("execplan", changed_output_type)
  assert string.contains(
    workflow_fingerprint.canonical_input(base),
    "exec_plan_bundle",
  )
  assert string.contains(
    workflow_fingerprint.canonical_input(base),
    "submit_implementation_pack_submission",
  )
}
