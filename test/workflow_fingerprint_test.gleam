import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/model_config
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
    hooks: hooks,
    source: config_types.ConfiguredWorkspaceProfile,
  )
}

fn limits(command_stream_max_chars: Int) -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: command_stream_max_chars,
    template_field_max_chars: 1000,
    workflow_summary_max_chars: 4000,
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
