import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config
import scherzo/config/types as config_types
import scherzo/model_config
import scherzo/runtime_bundle
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import scherzo/workflow_dag
import scherzo/workflow_fingerprint
import scherzo/workspace_driver_discovery
import simplifile
import support/test_helpers

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
  _hooks: config_types.DagHooksConfig,
) -> config_types.WorkspaceHookProfile {
  config_types.WorkspaceHookProfile(
    name: name,
    driver: Some(
      config_types.WorkspaceDriverConfig(
        command: "scripts/" <> name,
        lifecycle: [],
        capabilities: [],
        timeout_ms: 1000,
        env: [],
      ),
    ),
    source: config_types.ConfiguredWorkspaceDriver,
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
    driver: Some(
      config_types.WorkspaceDriverConfig(
        command: command,
        lifecycle: [],
        capabilities: capabilities,
        timeout_ms: hooks.timeout_ms,
        env: [],
      ),
    ),
    source: config_types.ConfiguredWorkspaceDriver,
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
  test_helpers.chmod_executable(path)
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
      ui_server: config.default_ui_server_config(),
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
      "version: 1\nid: implementation\n# comment\nconcurrency: 2\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n  - id: summarize\n    kind: command\n    depends_on: [collect]\n    run: summarize\n    run_in: main\n",
    )
  let second =
    parse(
      "version: 1\nid: implementation\nconcurrency: 2\nsteps:\n  - id: summarize\n    run_in: main\n    run: summarize\n    depends_on: [collect]\n    kind: command\n  - id: collect\n    run_in: main\n    run: collect\n    kind: command\n",
    )

  assert workflow_fingerprint.for_dag("implementation", first)
    == workflow_fingerprint.for_dag("implementation", second)
}

pub fn workflow_fingerprint_changes_for_semantic_fields_test() {
  let base =
    parse(
      "version: 1\nid: implementation\nconcurrency: 1\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n  - id: summarize\n    kind: command\n    depends_on: [collect]\n    run: summarize\n    run_in: main\n",
    )
  let changed_command =
    parse(
      "version: 1\nid: implementation\nconcurrency: 1\nsteps:\n  - id: collect\n    kind: command\n    run: collect changed\n    run_in: main\n  - id: summarize\n    kind: command\n    depends_on: [collect]\n    run: summarize\n    run_in: main\n",
    )
  let changed_parallelism =
    parse(
      "version: 1\nid: implementation\nconcurrency: 2\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n  - id: summarize\n    kind: command\n    depends_on: [collect]\n    run: summarize\n    run_in: main\n",
    )

  let base_fingerprint = workflow_fingerprint.for_dag("implementation", base)
  assert base_fingerprint
    != workflow_fingerprint.for_dag("implementation", changed_command)
  assert base_fingerprint
    != workflow_fingerprint.for_dag("implementation", changed_parallelism)
}

pub fn workflow_fingerprint_changes_for_recover_config_test() {
  let base =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n",
    )
  let recovered =
    parse(
      "version: 1\nid: implementation\nrecovery:\n  attempts: 2\n  prompt: prompts/recover.md\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n",
    )

  assert workflow_fingerprint.for_dag("implementation", base)
    != workflow_fingerprint.for_dag("implementation", recovered)
  assert string.contains(
    workflow_fingerprint.canonical_input(recovered),
    "recover",
  )
}

pub fn workflow_fingerprint_changes_for_resolved_recover_prompt_contents_test() {
  let dir = "test/tmp/workflow-fingerprint-recover-prompts"
  let workflow_path = dir <> "/workflow.yaml"
  let prompt_path = dir <> "/prompts/recover.md"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/prompts")
  let assert Ok(Nil) =
    simplifile.write(
      workflow_path,
      "version: 1\nid: implementation\nrecovery:\n  prompt: prompts/recover.md\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n",
    )
  let assert Ok(Nil) = simplifile.write(prompt_path, "Recover v1")
  let assert Ok(first) = runtime_bundle.load_workflow_file(workflow_path)
  let first_fingerprint = workflow_fingerprint.for_dag("implementation", first)

  let assert Ok(Nil) = simplifile.write(prompt_path, "Recover v2")
  let assert Ok(second) = runtime_bundle.load_workflow_file(workflow_path)
  let second_fingerprint =
    workflow_fingerprint.for_dag("implementation", second)

  assert first_fingerprint != second_fingerprint
}

pub fn workflow_fingerprint_changes_for_structured_output_contract_test() {
  let unstructured =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    run_in: main\n",
    )
  let structured =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    run_in: main\n    structured_output:\n      artifact_name: review_result\n      required: true\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_result\n      schema:\n        required: [summary, findings]\n",
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
      "version: 1\nid: implementation\nsteps:\n  - id: example_json\n    kind: agent\n    prompt: prompts/example.md\n    run_in: main\n    structured_output:\n      artifact_name: example_artifact\n      source:\n        type: pi_tool_call\n        tool_name: submit_example_artifact\n        require_single: true\n        reject_sibling_tool_calls: true\n      schema:\n        required: [schema_version, artifact_type]\n",
    )
  let second_tool_call =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: example_json\n    kind: agent\n    prompt: prompts/example.md\n    run_in: main\n    structured_output:\n      artifact_name: example_artifact\n      source:\n        type: pi_tool_call\n        tool_name: submit_other_artifact\n        require_single: true\n        reject_sibling_tool_calls: true\n      schema:\n        required: [schema_version, artifact_type]\n",
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
      "version: 1\nid: implementation\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    run_in: main\n    structured_output:\n      artifact_name: review_result\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_result\n      schema:\n        required: [summary, findings]\n",
    )
  let with_schema_validator =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    run_in: main\n    structured_output:\n      artifact_name: review_result\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_result\n      schema:\n        required: [summary, findings]\n      validators:\n        - name: shape\n          type: json_schema\n          path: schemas/review.schema.json\n",
    )
  let with_command_validator =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    run_in: main\n    structured_output:\n      artifact_name: review_result\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_result\n      schema:\n        required: [summary, findings]\n      validators:\n        - name: semantics\n          type: command\n          argv: [python3, scripts/validate]\n          timeout: 30s\n          env:\n            CHECK_MODE: strict\n",
    )
  let with_changed_env =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    run_in: main\n    structured_output:\n      artifact_name: review_result\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_result\n      schema:\n        required: [summary, findings]\n      validators:\n        - name: semantics\n          type: command\n          argv: [python3, scripts/validate]\n          timeout: 30s\n          env:\n            CHECK_MODE: relaxed\n",
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
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/.scherzo")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/schemas")
  let schema_path = dir <> "/schemas/review.schema.json"
  let dag =
    parse(
      "version: 1\nid: implementation\nworkspace:\n  driver: noop\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    run_in: main\n    structured_output:\n      artifact_name: review_result\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_result\n      validators:\n        - name: shape\n          type: json_schema\n          path: schemas/review.schema.json\n",
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
      "version: 1\nid: implementation\nworkspace:\n  driver: noop\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n",
    )
  let isolated =
    parse(
      "version: 1\nid: implementation\nworkspace:\n  driver: isolated\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n",
    )
  let omitted =
    parse(
      "version: 1\nid: implementation\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n",
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
      "version: 1\nid: implementation\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n",
    )
  let required =
    parse(
      "version: 1\nid: implementation\nworkspace:\n  requires: [assert-only]\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n",
    )
  let reordered =
    parse(
      "version: 1\nid: implementation\nworkspace:\n  requires: [changed-files, assert-only]\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n",
    )
  let canonical =
    parse(
      "version: 1\nid: implementation\nworkspace:\n  requires: [assert-only, changed-files]\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n",
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
      "version: 1\nid: implementation\nworkspace:\n  driver: noop\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n",
    )
  let settings = model_config.default_settings()
  let noop = profile("noop", hooks(Some("create")))
  let renamed = profile("isolated", hooks(Some("create")))
  let changed = driver_profile_with_timeout("scripts/changed", [], [], 1000)
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
      "version: 1\nid: implementation\nworkspace:\n  driver: noop\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n",
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
      "version: 1\nid: implementation\nworkspace:\n  driver: noop\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n",
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
      "version: 1\nid: implementation\nworkspace:\n  driver: noop\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n",
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
      "version: 1\nid: implementation\nworkspace:\n  driver: noop\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n",
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
  test_helpers.reset_dir(dir)
  let driver = dir <> "/driver.sh"
  let dag =
    parse(
      "version: 1\nid: implementation\nworkspace:\n  driver: noop\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n",
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
      "version: 1\nid: implementation\nworkspace:\n  driver: noop\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n",
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

pub fn workflow_execution_fingerprint_ignores_legacy_hooks_but_changes_for_artifact_limits_test() {
  let dag =
    parse(
      "version: 1\nid: implementation\nconcurrency: 1\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    run_in: main\n",
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
    == workflow_fingerprint.for_execution_options(
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

pub fn workflow_fingerprint_omits_absent_workstream_phase_field_test() {
  let dag =
    parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    code_change_bundle:\n      type: code_change_bundle\n      source:\n        step: implement\n        path: tmp/code-change-bundle.json\nsteps:\n  - id: implement\n    kind: command\n    run: echo ok\n",
    )

  let canonical = workflow_fingerprint.canonical_input(dag)
  assert string.contains(canonical, "workstream_phase") == False
}

pub fn workflow_fingerprint_changes_for_present_workstream_phase_metadata_test() {
  let base =
    parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    code_change_bundle:\n      type: code_change_bundle\n      source:\n        step: implement\n        path: tmp/code-change-bundle.json\nsteps:\n  - id: implement\n    kind: command\n    run: echo ok\nworkstream_phase:\n  phase_id: artifact_specs\n  handoff:\n    output: code_change_bundle\n    artifact_type: scherzo.handoff.v1\n    snapshot: required\n  gates: [human_review]\n  next_actions:\n    - action_id: revise_plan\n      workflow_id: execplan-revision\n      inputs: [code_change_bundle]\n      auto_enqueue: false\n",
    )
  let changed_phase =
    parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    code_change_bundle:\n      type: code_change_bundle\n      source:\n        step: implement\n        path: tmp/code-change-bundle.json\nsteps:\n  - id: implement\n    kind: command\n    run: echo ok\nworkstream_phase:\n  phase_id: artifact_specs_v2\n  handoff:\n    output: code_change_bundle\n    artifact_type: scherzo.handoff.v1\n    snapshot: required\n  gates: [human_review]\n  next_actions:\n    - action_id: revise_plan\n      workflow_id: execplan-revision\n      inputs: [code_change_bundle]\n      auto_enqueue: false\n",
    )
  let changed_output =
    parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    code_change_bundle:\n      type: code_change_bundle\n      source:\n        step: implement\n        path: tmp/code-change-bundle.json\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: implement\n        path: tmp/execplan-bundle.json\nsteps:\n  - id: implement\n    kind: command\n    run: echo ok\nworkstream_phase:\n  phase_id: artifact_specs\n  handoff:\n    output: exec_plan_bundle\n    artifact_type: scherzo.handoff.v1\n    snapshot: required\n  gates: [human_review]\n  next_actions:\n    - action_id: revise_plan\n      workflow_id: execplan-revision\n      inputs: [code_change_bundle]\n      auto_enqueue: false\n",
    )
  let changed_gate =
    parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    code_change_bundle:\n      type: code_change_bundle\n      source:\n        step: implement\n        path: tmp/code-change-bundle.json\nsteps:\n  - id: implement\n    kind: command\n    run: echo ok\nworkstream_phase:\n  phase_id: artifact_specs\n  handoff:\n    output: code_change_bundle\n    artifact_type: scherzo.handoff.v1\n    snapshot: required\n  gates: [human_gate]\n  next_actions:\n    - action_id: revise_plan\n      workflow_id: execplan-revision\n      inputs: [code_change_bundle]\n      auto_enqueue: false\n",
    )
  let changed_next_action_input =
    parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    code_change_bundle:\n      type: code_change_bundle\n      source:\n        step: implement\n        path: tmp/code-change-bundle.json\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: implement\n        path: tmp/execplan-bundle.json\nsteps:\n  - id: implement\n    kind: command\n    run: echo ok\nworkstream_phase:\n  phase_id: artifact_specs\n  handoff:\n    output: code_change_bundle\n    artifact_type: scherzo.handoff.v1\n    snapshot: required\n  gates: [human_review]\n  next_actions:\n    - action_id: revise_plan\n      workflow_id: execplan-revision\n      inputs: [exec_plan_bundle]\n      auto_enqueue: false\n",
    )
  let changed_requires_gate =
    parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    code_change_bundle:\n      type: code_change_bundle\n      source:\n        step: implement\n        path: tmp/code-change-bundle.json\nsteps:\n  - id: implement\n    kind: command\n    run: echo ok\nworkstream_phase:\n  phase_id: artifact_specs\n  handoff:\n    output: code_change_bundle\n    artifact_type: scherzo.handoff.v1\n    snapshot: required\n  gates: [human_review]\n  next_actions:\n    - action_id: revise_plan\n      workflow_id: execplan-revision\n      inputs: [code_change_bundle]\n      requires_gate: human_review\n      auto_enqueue: false\n",
    )
  let changed_state =
    parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    code_change_bundle:\n      type: code_change_bundle\n      source:\n        step: implement\n        path: tmp/code-change-bundle.json\nsteps:\n  - id: implement\n    kind: command\n    run: echo ok\nworkstream_phase:\n  phase_id: artifact_specs\n  handoff:\n    output: code_change_bundle\n    artifact_type: scherzo.handoff.v1\n    snapshot: required\n  gates: [human_review]\n  next_actions:\n    - action_id: revise_plan\n      workflow_id: execplan-revision\n      state: available\n      inputs: [code_change_bundle]\n      auto_enqueue: false\n",
    )
  let changed_priority =
    parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    code_change_bundle:\n      type: code_change_bundle\n      source:\n        step: implement\n        path: tmp/code-change-bundle.json\nsteps:\n  - id: implement\n    kind: command\n    run: echo ok\nworkstream_phase:\n  phase_id: artifact_specs\n  handoff:\n    output: code_change_bundle\n    artifact_type: scherzo.handoff.v1\n    snapshot: required\n  gates: [human_review]\n  next_actions:\n    - action_id: revise_plan\n      workflow_id: execplan-revision\n      priority: 1\n      inputs: [code_change_bundle]\n      auto_enqueue: false\n",
    )
  let changed_auto_enqueue =
    parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    code_change_bundle:\n      type: code_change_bundle\n      source:\n        step: implement\n        path: tmp/code-change-bundle.json\nsteps:\n  - id: implement\n    kind: command\n    run: echo ok\nworkstream_phase:\n  phase_id: artifact_specs\n  handoff:\n    output: code_change_bundle\n    artifact_type: scherzo.handoff.v1\n    snapshot: required\n  gates: [human_review]\n  next_actions:\n    - action_id: revise_plan\n      workflow_id: execplan-revision\n      inputs: [code_change_bundle]\n      auto_enqueue: true\n",
    )
  let changed_final_phase =
    parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    code_change_bundle:\n      type: code_change_bundle\n      source:\n        step: implement\n        path: tmp/code-change-bundle.json\nsteps:\n  - id: implement\n    kind: command\n    run: echo ok\nworkstream_phase:\n  phase_id: artifact_specs\n  handoff:\n    output: code_change_bundle\n    artifact_type: scherzo.handoff.v1\n    snapshot: required\n  gates: [human_review]\n  next_actions:\n    - action_id: revise_plan\n      workflow_id: execplan-revision\n      inputs: [code_change_bundle]\n      auto_enqueue: false\n  final_phase: true\n",
    )

  let fingerprint = workflow_fingerprint.for_dag("implementation", base)
  assert string.contains(
    workflow_fingerprint.canonical_input(base),
    "workstream_phase",
  )
  assert fingerprint
    != workflow_fingerprint.for_dag("implementation", changed_phase)
  assert fingerprint
    != workflow_fingerprint.for_dag("implementation", changed_output)
  assert fingerprint
    != workflow_fingerprint.for_dag("implementation", changed_gate)
  assert fingerprint
    != workflow_fingerprint.for_dag("implementation", changed_next_action_input)
  assert fingerprint
    != workflow_fingerprint.for_dag("implementation", changed_requires_gate)
  assert fingerprint
    != workflow_fingerprint.for_dag("implementation", changed_state)
  assert fingerprint
    != workflow_fingerprint.for_dag("implementation", changed_priority)
  assert fingerprint
    != workflow_fingerprint.for_dag("implementation", changed_auto_enqueue)
  assert fingerprint
    != workflow_fingerprint.for_dag("implementation", changed_final_phase)
}

pub fn v2_workflow_fingerprint_includes_structured_output_and_contract_types_test() {
  let base =
    parse(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: materialize_bundle\n        field: stdout\nsteps:\n  - id: draft\n    kind: agent\n    prompt: prompts/execplan-draft.md\n    run_in: main\n    structured_output:\n      artifact_name: implementation_pack_submission\n      source:\n        type: pi_tool_call\n        tool_name: submit_implementation_pack_submission\n        parameters_schema_path: .scherzo/workflows/schemas/implementation-pack-submission.v2.schema.json\n      validators:\n        - name: shape\n          type: json_schema\n          path: .scherzo/workflows/schemas/implementation-pack-submission.v2.schema.json\n  - id: materialize_bundle\n    kind: command\n    depends_on: [draft]\n    run: .scherzo/workflows/scripts/scherzo-execplan materialize-bundle\n",
    )
  let changed_schema =
    parse(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: materialize_bundle\n        field: stdout\nsteps:\n  - id: draft\n    kind: agent\n    prompt: prompts/execplan-draft.md\n    run_in: main\n    structured_output:\n      artifact_name: implementation_pack_submission\n      source:\n        type: pi_tool_call\n        tool_name: submit_implementation_pack_submission\n        parameters_schema_path: .scherzo/workflows/schemas/exec-plan-revision-submission.v2.schema.json\n      validators:\n        - name: shape\n          type: json_schema\n          path: .scherzo/workflows/schemas/exec-plan-revision-submission.v2.schema.json\n  - id: materialize_bundle\n    kind: command\n    depends_on: [draft]\n    run: .scherzo/workflows/scripts/scherzo-execplan materialize-bundle\n",
    )
  let changed_tool =
    parse(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: materialize_bundle\n        field: stdout\nsteps:\n  - id: draft\n    kind: agent\n    prompt: prompts/execplan-draft.md\n    run_in: main\n    structured_output:\n      artifact_name: implementation_pack_submission\n      source:\n        type: pi_tool_call\n        tool_name: submit_other_pack_submission\n        parameters_schema_path: .scherzo/workflows/schemas/implementation-pack-submission.v2.schema.json\n      validators:\n        - name: shape\n          type: json_schema\n          path: .scherzo/workflows/schemas/implementation-pack-submission.v2.schema.json\n  - id: materialize_bundle\n    kind: command\n    depends_on: [draft]\n    run: .scherzo/workflows/scripts/scherzo-execplan materialize-bundle\n",
    )
  let changed_command =
    parse(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: materialize_bundle\n        field: stdout\nsteps:\n  - id: draft\n    kind: agent\n    prompt: prompts/execplan-draft.md\n    run_in: main\n    structured_output:\n      artifact_name: implementation_pack_submission\n      source:\n        type: pi_tool_call\n        tool_name: submit_implementation_pack_submission\n        parameters_schema_path: .scherzo/workflows/schemas/implementation-pack-submission.v2.schema.json\n      validators:\n        - name: shape\n          type: json_schema\n          path: .scherzo/workflows/schemas/implementation-pack-submission.v2.schema.json\n  - id: materialize_bundle\n    kind: command\n    depends_on: [draft]\n    run: .scherzo/workflows/scripts/scherzo-execplan materialize-bundle --changed\n",
    )
  let changed_output_type =
    parse(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: code_change_bundle\n      source:\n        step: materialize_bundle\n        field: stdout\nsteps:\n  - id: draft\n    kind: agent\n    prompt: prompts/execplan-draft.md\n    run_in: main\n    structured_output:\n      artifact_name: implementation_pack_submission\n      source:\n        type: pi_tool_call\n        tool_name: submit_implementation_pack_submission\n        parameters_schema_path: .scherzo/workflows/schemas/implementation-pack-submission.v2.schema.json\n      validators:\n        - name: shape\n          type: json_schema\n          path: .scherzo/workflows/schemas/implementation-pack-submission.v2.schema.json\n  - id: materialize_bundle\n    kind: command\n    depends_on: [draft]\n    run: .scherzo/workflows/scripts/scherzo-execplan materialize-bundle\n",
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
