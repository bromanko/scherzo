import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/commit_stack_publication_preflight
import scherzo/config/types as config_types
import scherzo/path
import scherzo/result_artifact
import scherzo/runtime_bundle
import scherzo/session/tokens as session_tokens
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_completion_policy
import scherzo/workflow_dag
import simplifile
import support/test_helpers

@external(erlang, "scherzo_test_ffi", "set_cwd")
fn set_cwd(path: String) -> Result(Nil, simplifile.FileError)

fn env(name: String) -> Option(String) {
  case name {
    "LINEAR_API_KEY" -> Some("linearkey")
    "LINEAR_PROJECT_SLUG" -> Some("TEST")
    _ -> None
  }
}

fn issue(labels: List(String)) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "issue-id",
    identifier: "ABC-123",
    title: "Implement DAGs",
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked("Todo"),
    branch_name: None,
    url: None,
    labels: labels,
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn worker_success(
  result: result_artifact.ResultArtifact,
) -> agent_types.WorkerSuccess {
  agent_types.WorkerSuccess(
    final_issue: None,
    final_classification: agent_types.FinalTerminal,
    workspace_path: "test/tmp/workspace",
    tokens: session_tokens.zero_token_totals(),
    turns: 1,
    result: result,
  )
}

fn write_driver_script(dir: String, name: String, body: String) -> Nil {
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/scripts")
  let path = dir <> "/scripts/" <> name
  let assert Ok(Nil) = simplifile.write(path, body)
  test_helpers.chmod_executable(path)
}

fn write_describe_driver(
  dir: String,
  name: String,
  capabilities_json: String,
) -> Nil {
  write_driver_script(
    dir,
    name,
    "#!/bin/sh\n"
      <> "if [ \"$1\" = describe ] && [ \"$2\" = --json ]; then\n"
      <> "  printf '%s\\n' '{\"version\":1,\"capabilities\":"
      <> capabilities_json
      <> "}'\n"
      <> "  exit 0\n"
      <> "fi\n"
      <> "exit 2\n",
  )
}

fn write_malformed_describe_driver(dir: String, name: String) -> Nil {
  write_driver_script(
    dir,
    name,
    "#!/bin/sh\nif [ \"$1\" = describe ]; then echo not-json; exit 0; fi\nexit 2\n",
  )
}

fn load_default_from_dir(
  dir: String,
) -> Result(runtime_bundle.RuntimeBundle, runtime_bundle.BundleError) {
  let assert Ok(original) = simplifile.current_directory()
  let assert Ok(Nil) = set_cwd(dir)
  let result = runtime_bundle.load_with_env(None, env)
  let assert Ok(Nil) = set_cwd(original)
  result
}

fn write_publication_yaml_project(
  dir: String,
  repository_ref: String,
  root_extra: String,
) -> Nil {
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: implement\n        field: final_response\nsteps:\n  - id: implement\n    kind: agent\n    prompt: prompts/implement.md\nartifacts:\n  publications:\n    - id: review_doc\n      repository: "
        <> repository_ref
        <> "\n      files:\n        - select:\n            output: exec_plan_bundle\n            entry: plan\n          path: docs/plans/{{ work.identifier }}.md\n",
    )
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/workflows/prompts")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/workflows/prompts/implement.md", "Implement")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nartifacts:\n  repositories:\n    github:\n      docs:\n        repo: scherzo-systems/scherzo\n        base: main\n"
        <> root_extra
        <> "workflows:\n    implementation: workflows/implementation.yaml\n",
    )
  Nil
}

fn write_commit_stack_publication_yaml_project(
  dir: String,
  workspace_config: String,
  workflow_workspace: String,
  required: Bool,
) -> Nil {
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\n"
        <> workflow_workspace
        <> "contract:\n  version: 1\n  outputs:\n    commit_stack:\n      type: commit_stack\n      source:\n        step: main\n        field: stdout\n    merge_target:\n      type: code_change\n      source:\n        step: main\n        field: stdout\nsteps:\n  - id: main\n    kind: command\n    run: echo ok\nartifacts:\n  publications:\n    - id: publish_stack\n      repository: github.code\n      required: "
        <> yaml_bool(required)
        <> "\n      mode: commit_stack\n      commit_stack:\n        select:\n          output: commit_stack\n      target:\n        kind: existing_pr_branch\n        source:\n          output: merge_target\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\n"
        <> workspace_config
        <> "artifacts:\n  repositories:\n    github:\n      code:\n        repo: scherzo-systems/scherzo\n        base: main\nworkflows:\n    implementation: workflows/implementation.yaml\n",
    )
  Nil
}

fn yaml_bool(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn write_default_yaml_project(dir: String) -> Nil {
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/.scherzo/workflows/prompts")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/.scherzo/workflows/prompts/implement.md",
      "Implement",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/.scherzo/workflows/implementation.yaml",
      "version: 1\nid: implementation\nsteps:\n  - id: implement\n    kind: agent\n    prompt: prompts/implement.md\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/.scherzo/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\nworkflows:\n    implementation: workflows/implementation.yaml\n",
    )
  Nil
}

pub fn loads_configured_publication_repository_test() {
  let dir = "test/tmp/runtime-bundle-publication-configured"
  write_publication_yaml_project(dir, "github.docs", "")

  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  let assert Ok(dag) = dict.get(bundle.workflows, "implementation")
  let assert [route] = dag.publication_routes
  assert route.repository == "github.docs"
}

pub fn rejects_missing_publication_repository_test() {
  let dir = "test/tmp/runtime-bundle-publication-missing"
  write_publication_yaml_project(dir, "github.missing", "")

  let assert Error(runtime_bundle.BundleError(code, message)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "publication_repository_missing"
  assert string.contains(message, "github.missing")
}

pub fn rejects_unsupported_publication_repository_backend_test() {
  let dir = "test/tmp/runtime-bundle-publication-backend"
  write_publication_yaml_project(dir, "gitlab.docs", "")

  let assert Error(runtime_bundle.BundleError(code, message)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "unsupported_publication_repository_backend"
  assert string.contains(message, "gitlab")
}

pub fn rejects_required_commit_stack_publication_with_noop_driver_test() {
  let dir = "test/tmp/runtime-bundle-commit-stack-noop-driver"
  write_commit_stack_publication_yaml_project(
    dir,
    "workspace:\n  root: workspaces\n  driver: noop\n",
    "",
    True,
  )

  let assert Error(runtime_bundle.BundleError(code, message)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "commit_stack_publication_driver_unsupported"
  assert string.contains(message, "workflow implementation")
  assert string.contains(message, "publication publish_stack")
  assert string.contains(message, "workspace.driver noop")
  assert string.contains(message, "publish-commit-stack")
  assert !string.contains(message, "publish-change or publish-commit-stack")
  assert string.contains(message, "required: false")
}

pub fn rejects_first_required_commit_stack_publication_in_declaration_order_test() {
  let dir = "test/tmp/runtime-bundle-commit-stack-diagnostic-order"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    commit_stack:\n      type: commit_stack\n      source:\n        step: main\n        field: stdout\n    merge_target:\n      type: code_change\n      source:\n        step: main\n        field: stdout\nsteps:\n  - id: main\n    kind: command\n    run: echo ok\nartifacts:\n  publications:\n    - id: first_stack\n      repository: github.code\n      required: true\n      mode: commit_stack\n      commit_stack:\n        select:\n          output: commit_stack\n      target:\n        kind: existing_pr_branch\n        source:\n          output: merge_target\n    - id: second_stack\n      repository: github.code\n      required: true\n      mode: commit_stack\n      commit_stack:\n        select:\n          output: commit_stack\n      target:\n        kind: existing_pr_branch\n        source:\n          output: merge_target\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\n  driver: noop\nartifacts:\n  repositories:\n    github:\n      code:\n        repo: scherzo-systems/scherzo\n        base: main\nworkflows:\n    implementation: workflows/implementation.yaml\n",
    )

  let assert Error(runtime_bundle.BundleError(code, message)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "commit_stack_publication_driver_unsupported"
  assert string.contains(message, "publication first_stack")
  assert !string.contains(message, "publication second_stack")
}

pub fn loads_required_commit_stack_publication_with_jj_driver_test() {
  let dir = "test/tmp/runtime-bundle-commit-stack-jj-driver"
  write_commit_stack_publication_yaml_project(
    dir,
    "workspace:\n  root: workspaces\n  driver: jj\n",
    "",
    True,
  )

  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert dict.has_key(bundle.workflows, "implementation")
}

pub fn loads_required_commit_stack_publication_with_successor_capability_test() {
  let dir = "test/tmp/runtime-bundle-commit-stack-successor-driver"
  write_commit_stack_publication_yaml_project(
    dir,
    "workspace:\n  root: workspaces\n  driver: custom\n  drivers:\n    custom:\n      type: custom\n      command: scripts/driver\n",
    "",
    True,
  )
  write_describe_driver(dir, "driver", "[\"publish-commit-stack\"]")

  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert dict.has_key(bundle.workflows, "implementation")
}

pub fn preflight_rejects_commit_stack_publication_with_legacy_and_successor_capabilities_test() {
  let dir = "test/tmp/runtime-bundle-commit-stack-legacy-both"
  write_commit_stack_publication_yaml_project(
    dir,
    "workspace:\n  root: workspaces\n  driver: custom\n  drivers:\n    custom:\n      type: custom\n      command: scripts/driver\n",
    "",
    True,
  )
  write_describe_driver(dir, "driver", "[\"publish-commit-stack\"]")

  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  let assert Ok(profile) =
    dict.get(bundle.orchestrator.workspace_profiles.profiles, "custom")
  let assert Some(driver) = profile.driver
  let legacy_profile =
    config_types.WorkspaceHookProfile(
      ..profile,
      driver: Some(
        config_types.WorkspaceDriverConfig(..driver, capabilities: [
          config_types.WorkspacePublishChange,
          config_types.WorkspacePublishCommitStack,
        ]),
      ),
    )
  let profiles =
    dict.insert(
      bundle.orchestrator.workspace_profiles.profiles,
      "custom",
      legacy_profile,
    )
  let orchestrator =
    config_types.OrchestratorConfig(
      ..bundle.orchestrator,
      workspace_profiles: config_types.WorkspaceHookProfiles(
        ..bundle.orchestrator.workspace_profiles,
        profiles: profiles,
      ),
    )

  let assert Error(error) =
    commit_stack_publication_preflight.validate_required(
      orchestrator,
      dict.to_list(bundle.workflows),
    )
  assert commit_stack_publication_preflight.error_code(error)
    == "commit_stack_publication_driver_unsupported"
  let message = commit_stack_publication_preflight.error_message(error)
  assert string.contains(message, "publish-change was removed")
  assert string.contains(message, "publish-commit-stack")
  assert string.contains(message, "docs/runbooks/workspace-driver-migration.md")
}

pub fn rejects_commit_stack_publication_with_legacy_driver_capability_test() {
  let dir = "test/tmp/runtime-bundle-commit-stack-legacy-driver"
  write_commit_stack_publication_yaml_project(
    dir,
    "workspace:\n  root: workspaces\n  driver: custom\n  drivers:\n    custom:\n      type: custom\n      command: scripts/driver\n",
    "",
    True,
  )
  write_describe_driver(dir, "driver", "[\"publish-change\"]")

  let assert Error(runtime_bundle.BundleError(code, message)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "workspace_driver_discovery_failed"
  assert string.contains(message, "publish-change was removed")
  assert string.contains(message, "publish-commit-stack")
  assert string.contains(message, "docs/runbooks/workspace-driver-migration.md")
}

pub fn loads_optional_commit_stack_publication_with_unsupported_driver_test() {
  let dir = "test/tmp/runtime-bundle-commit-stack-optional-noop"
  write_commit_stack_publication_yaml_project(
    dir,
    "workspace:\n  root: workspaces\n  driver: noop\n",
    "",
    False,
  )

  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  let diagnostics =
    commit_stack_publication_preflight.optional_diagnostics(
      bundle.orchestrator,
      dict.to_list(bundle.workflows),
    )
  let assert [diagnostic] = diagnostics
  assert commit_stack_publication_preflight.diagnostic_code(diagnostic)
    == "optional_commit_stack_publication_driver_unsupported"
  assert string.contains(
    commit_stack_publication_preflight.diagnostic_message(diagnostic),
    "optional publication",
  )
}

pub fn rejects_required_commit_stack_publication_with_custom_status_driver_test() {
  let dir = "test/tmp/runtime-bundle-commit-stack-custom-status"
  write_commit_stack_publication_yaml_project(
    dir,
    "workspace:\n  root: workspaces\n  driver: custom\n  drivers:\n    custom:\n      type: custom\n      command: scripts/driver\n",
    "",
    True,
  )
  write_describe_driver(dir, "driver", "[\"status\",\"changed-files\"]")

  let assert Error(runtime_bundle.BundleError(code, message)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "commit_stack_publication_driver_unsupported"
  assert string.contains(message, "workspace.driver custom")
  assert string.contains(message, "status, changed-files")
}

pub fn rejects_commit_stack_publication_when_custom_driver_metadata_missing_test() {
  let dir = "test/tmp/runtime-bundle-commit-stack-missing-metadata"
  write_commit_stack_publication_yaml_project(
    dir,
    "workspace:\n  root: workspaces\n  driver: custom\n  drivers:\n    custom:\n      type: custom\n      command: scripts/driver\n",
    "",
    True,
  )
  write_driver_script(
    dir,
    "driver",
    "#!/bin/sh\necho missing describe >&2\nexit 2\n",
  )

  let assert Error(runtime_bundle.BundleError(code, message)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "workspace_driver_discovery_failed"
  assert string.contains(message, "profile custom")
  assert string.contains(message, "describe --json exited")
}

pub fn rejects_commit_stack_publication_when_driver_reports_unknown_capability_test() {
  let dir = "test/tmp/runtime-bundle-commit-stack-unknown-capability"
  write_commit_stack_publication_yaml_project(
    dir,
    "workspace:\n  root: workspaces\n  driver: custom\n  drivers:\n    custom:\n      type: custom\n      command: scripts/driver\n",
    "",
    True,
  )
  write_describe_driver(dir, "driver", "[\"status\",\"publish-stack\"]")

  let assert Error(runtime_bundle.BundleError(code, message)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "workspace_driver_discovery_failed"
  assert string.contains(message, "unknown capability: publish-stack")
}

pub fn rejects_markdown_paths_as_unsupported_config_path_test() {
  let dir = "test/tmp/runtime-bundle-legacy"
  test_helpers.reset_dir(dir)
  let workflow_path = dir <> "/WORKFLOW.md"
  let assert Ok(Nil) = simplifile.write(workflow_path, "Legacy prompt\n")
  let assert Error(runtime_bundle.BundleError(code, _)) =
    runtime_bundle.load_with_env(Some(workflow_path), env)
  assert code == "unsupported_config_path"
}

pub fn default_path_prefers_scherzo_yaml_test() {
  let dir = "test/tmp/runtime-bundle-default-yaml"
  write_default_yaml_project(dir)
  let assert Ok(bundle) = load_default_from_dir(dir)
  assert bundle.config_path == ".scherzo/scherzo.yaml"
  assert dict.has_key(bundle.workflows, "implementation")
}

pub fn default_path_ignores_workflow_md_and_reports_missing_yaml_test() {
  let dir = "test/tmp/runtime-bundle-default-legacy"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.write(dir <> "/WORKFLOW.md", "Legacy\n")
  let assert Error(runtime_bundle.BundleError(code, _)) =
    load_default_from_dir(dir)
  assert code == "missing_config_file"
}

pub fn loads_yaml_orchestrator_and_prompt_files_test() {
  let dir = "test/tmp/runtime-bundle-yaml"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/workflows/prompts")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/prompts/implement.md",
      "Implement {{ issue.identifier }}",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\nsteps:\n  - id: implement\n    kind: agent\n    prompt: prompts/implement.md\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\nworkflows:\n    implementation: workflows/implementation.yaml\n",
    )
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert bundle.config_path == dir <> "/scherzo.yaml"
  let assert Ok(dag) = dict.get(bundle.workflows, "implementation")
  let assert [step] = dag.steps
  let assert workflow_dag.AgentStep(workflow_dag.PromptInline(prompt), None) =
    step.kind
  assert prompt == "Implement {{ issue.identifier }}"
}

pub fn task_updates_completion_policy_uses_loaded_workflow_review_metadata_test() {
  let dir = "test/tmp/runtime-bundle-task-updates-review-policy"
  test_helpers.reset_dir(dir)
  write_describe_driver(dir, "driver", "[\"publish-commit-stack\"]")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\nworkspace:\n  driver: driver\n  requires: [publish-commit-stack]\nsteps:\n  - id: run\n    kind: command\n    run: echo reviewable\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/maintenance.yaml",
      "version: 1\nid: maintenance\nworkspace:\n  driver: driver\nsteps:\n  - id: run\n    kind: command\n    run: echo maintenance\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\n  driver: driver\n  drivers:\n    driver:\n      type: custom\n      command: scripts/driver\ntask_updates:\n  enabled: true\n  states:\n    success: In Review\n    no_review_success: Done\n    failure: Triage\n    partial_success: Triage\nworkflows:\n    implementation: workflows/implementation.yaml\n    maintenance: workflows/maintenance.yaml\n",
    )

  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  let assert Some(policy) = bundle.effective.handoff.completion_states
  let assert Ok(implementation) = dict.get(policy.workflows, "implementation")
  let assert Ok(maintenance) = dict.get(policy.workflows, "maintenance")

  assert implementation.produces_reviewable_artifacts == Some(True)
  assert implementation.requires_review == Some(True)
  assert maintenance.produces_reviewable_artifacts == Some(False)
  assert maintenance.requires_review == Some(False)

  let empty_success = worker_success(result_artifact.empty())
  let implementation_outcome =
    workflow_completion_policy.success_outcome(
      Some(policy),
      "implementation",
      empty_success,
    )
  assert implementation_outcome.expected_artifacts_missing == True
  assert implementation_outcome.requires_review
    == workflow_completion_policy.ReviewRequired
  assert workflow_completion_policy.choose_linear_completion_state(
      policy,
      "implementation",
      implementation_outcome,
    )
    == workflow_completion_policy.MoveToState(
      workflow_completion_policy.StateByName("Triage"),
      "expected reviewable artifacts were missing",
    )

  let maintenance_success =
    worker_success(result_artifact.from_final_response(
      Some("maintenance completed"),
      False,
      "test",
    ))
  let maintenance_outcome =
    workflow_completion_policy.success_outcome(
      Some(policy),
      "maintenance",
      maintenance_success,
    )
  assert maintenance_outcome.requires_review
    == workflow_completion_policy.ReviewNotRequired
  assert workflow_completion_policy.choose_linear_completion_state(
      policy,
      "maintenance",
      maintenance_outcome,
    )
    == workflow_completion_policy.MoveToState(
      workflow_completion_policy.StateByName("Done"),
      "no review is required",
    )
}

pub fn task_updates_workflow_override_can_make_publish_commit_stack_no_review_test() {
  let dir = "test/tmp/runtime-bundle-task-updates-workflow-override"
  test_helpers.reset_dir(dir)
  write_describe_driver(dir, "driver", "[\"publish-commit-stack\"]")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let publish_stack_workflow =
    "version: 1\nworkspace:\n  driver: driver\n  requires: [publish-commit-stack]\nsteps:\n  - id: run\n    kind: command\n    run: echo published\n"
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/merge-conflict-resolution.yaml",
      "id: merge-conflict-resolution\n" <> publish_stack_workflow,
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "id: implementation\n" <> publish_stack_workflow,
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/execplan.yaml",
      "id: execplan\n" <> publish_stack_workflow,
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\n  driver: driver\n  drivers:\n    driver:\n      type: custom\n      command: scripts/driver\ntask_updates:\n  enabled: true\n  states:\n    success: In Review\n    no_review_success: Done\n    failure: Triage\n    partial_success: Triage\n  workflows:\n    merge-conflict-resolution:\n      requires_review: false\n      states:\n        no_review_success: Done\n    execplan:\n      states:\n        success: Done\nworkflows:\n    merge-conflict-resolution: workflows/merge-conflict-resolution.yaml\n    implementation: workflows/implementation.yaml\n    execplan: workflows/execplan.yaml\n",
    )

  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  let assert Some(policy) = bundle.effective.handoff.completion_states
  let assert Ok(merge_override) =
    dict.get(policy.workflows, "merge-conflict-resolution")
  let assert Ok(implementation_override) =
    dict.get(policy.workflows, "implementation")
  let assert Ok(execplan_override) = dict.get(policy.workflows, "execplan")

  assert merge_override.produces_reviewable_artifacts == Some(False)
  assert merge_override.requires_review == Some(False)
  assert merge_override.no_review_completion_state
    == Some(workflow_completion_policy.StateByName("Done"))
  assert implementation_override.produces_reviewable_artifacts == Some(True)
  assert implementation_override.requires_review == Some(True)
  assert execplan_override.produces_reviewable_artifacts == Some(True)
  assert execplan_override.requires_review == Some(True)
  assert execplan_override.success_state
    == Some(workflow_completion_policy.StateByName("Done"))

  let success =
    worker_success(result_artifact.from_final_response(
      Some("published commit stack"),
      False,
      "test",
    ))
  let merge_outcome =
    workflow_completion_policy.success_outcome(
      Some(policy),
      "merge-conflict-resolution",
      success,
    )
  assert merge_outcome.requires_review
    == workflow_completion_policy.ReviewNotRequired
  assert workflow_completion_policy.choose_linear_completion_state(
      policy,
      "merge-conflict-resolution",
      merge_outcome,
    )
    == workflow_completion_policy.MoveToState(
      workflow_completion_policy.StateByName("Done"),
      "no review is required",
    )

  let implementation_outcome =
    workflow_completion_policy.success_outcome(
      Some(policy),
      "implementation",
      success,
    )
  assert implementation_outcome.requires_review
    == workflow_completion_policy.ReviewRequired
  assert workflow_completion_policy.choose_linear_completion_state(
      policy,
      "implementation",
      implementation_outcome,
    )
    == workflow_completion_policy.MoveToState(
      workflow_completion_policy.StateByName("In Review"),
      "reviewable artifacts were produced",
    )

  let execplan_outcome =
    workflow_completion_policy.success_outcome(
      Some(policy),
      "execplan",
      success,
    )
  assert execplan_outcome.requires_review
    == workflow_completion_policy.ReviewRequired
  assert workflow_completion_policy.choose_linear_completion_state(
      policy,
      "execplan",
      execplan_outcome,
    )
    == workflow_completion_policy.MoveToState(
      workflow_completion_policy.StateByName("Done"),
      "workflow-specific success state",
    )

  let empty_execplan_outcome =
    workflow_completion_policy.success_outcome(
      Some(policy),
      "execplan",
      worker_success(result_artifact.empty()),
    )
  assert empty_execplan_outcome.expected_artifacts_missing == True
  assert workflow_completion_policy.choose_linear_completion_state(
      policy,
      "execplan",
      empty_execplan_outcome,
    )
    == workflow_completion_policy.MoveToState(
      workflow_completion_policy.StateByName("Triage"),
      "expected reviewable artifacts were missing",
    )
}

pub fn task_updates_workflow_override_rejects_unknown_workflow_test() {
  let dir = "test/tmp/runtime-bundle-task-updates-unknown-workflow"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\nsteps:\n  - id: run\n    kind: command\n    run: echo implementation\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\ntask_updates:\n  enabled: true\n  workflows:\n    implmentation:\n      states:\n        success: Done\nworkflows:\n    implementation: workflows/implementation.yaml\n",
    )

  let assert Error(runtime_bundle.BundleError(code, message)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "unknown_task_update_workflow"
  assert string.contains(message, "task_updates.workflows.implmentation")
}

pub fn task_updates_completion_policy_accepts_publish_commit_stack_with_jj_test() {
  let dir = "test/tmp/runtime-bundle-task-updates-publish-commit-stack"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\nworkspace:\n  driver: jj\n  requires: [publish-commit-stack]\nsteps:\n  - id: run\n    kind: command\n    run: echo reviewable\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\n  driver: jj\ntask_updates:\n  enabled: true\n  states:\n    success: In Review\n    no_review_success: Done\n    failure: Triage\n    partial_success: Triage\nworkflows:\n    implementation: workflows/implementation.yaml\n",
    )

  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  let assert Ok(dag) = dict.get(bundle.workflows, "implementation")
  let assert Ok(profile) =
    dict.get(bundle.orchestrator.workspace_profiles.profiles, "jj")
  let assert Some(driver) = profile.driver
  let assert Some(policy) = bundle.effective.handoff.completion_states
  let assert Ok(implementation) = dict.get(policy.workflows, "implementation")

  assert dag.workspace_capabilities
    == [config_types.WorkspacePublishCommitStack]
  assert !list.contains(
    driver.capabilities,
    config_types.WorkspacePublishChange,
  )
  assert list.contains(
    driver.capabilities,
    config_types.WorkspacePublishCommitStack,
  )
  assert implementation.produces_reviewable_artifacts == Some(True)
  assert implementation.requires_review == Some(True)
}

pub fn loads_workflow_yaml_without_recover_fields_through_current_parser_test() {
  let dir = "test/tmp/runtime-bundle-current-dag-shape"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/legacy.yaml",
      "version: 1\nid: legacy\ncontract:\n  version: 1\n  outputs:\n    summary:\n      type: document.markdown\n      source:\n        step: summarize\n        field: stdout\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n  - id: summarize\n    kind: command\n    depends_on: [collect]\n    run: summarize\n",
    )

  let assert Ok(dag) =
    runtime_bundle.load_workflow_file(dir <> "/workflows/legacy.yaml")

  assert dag.id == "legacy"
  assert dag.recover == None
  let assert Some(_) = dag.contract
  let assert [collect, summarize] = dag.steps
  assert collect.id == "collect"
  assert collect.recover == None
  assert summarize.id == "summarize"
  assert summarize.depends_on == ["collect"]
  assert summarize.recover == None
}

pub fn loads_recovery_prompt_files_test() {
  let dir = "test/tmp/runtime-bundle-recover-prompts"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/workflows/prompts")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/workflows/prompts/implement.md", "Implement")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/workflows/prompts/recover.md", "Recover workflow")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/prompts/recover-step.md",
      "Recover step",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\nrecovery:\n  prompt: prompts/recover.md\nsteps:\n  - id: implement\n    kind: agent\n    prompt: prompts/implement.md\n    recovery:\n      prompt: prompts/recover-step.md\n",
    )
  let assert Ok(dag) =
    runtime_bundle.load_workflow_file(dir <> "/workflows/implementation.yaml")
  let assert Some(workflow_dag.RecoveryConfigPatch(
    prompt: Some(workflow_dag.PromptInline(workflow_prompt)),
    ..,
  )) = dag.recover
  let assert [step] = dag.steps
  let assert Some(workflow_dag.RecoveryConfigPatch(
    prompt: Some(workflow_dag.PromptInline(step_prompt)),
    ..,
  )) = step.recover
  assert workflow_prompt == "Recover workflow"
  assert step_prompt == "Recover step"
}

pub fn runtime_bundle_records_config_workflow_and_prompt_dependencies_test() {
  let dir = "test/tmp/runtime-bundle-dependencies"
  let config_path = dir <> "/scherzo.yaml"
  let workflow_path = dir <> "/workflows/implementation.yaml"
  let prompt_path = dir <> "/workflows/prompts/implement.md"
  let prompt_text = "Implement dependency manifest"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/workflows/prompts")
  let assert Ok(Nil) = simplifile.write(prompt_path, prompt_text)
  let assert Ok(Nil) =
    simplifile.write(
      workflow_path,
      "version: 1\nid: implementation\nsteps:\n  - id: implement\n    kind: agent\n    prompt: prompts/implement.md\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\nworkflows:\n    implementation: workflows/implementation.yaml\n",
    )

  let assert Ok(bundle) = runtime_bundle.load_with_env(Some(config_path), env)
  let assert Ok(resolved_workflow_path) = path.absolute(workflow_path)
  let assert Ok(resolved_prompt_path) = path.absolute(prompt_path)
  let dependency_paths =
    bundle.dependencies
    |> list.map(fn(dependency) { dependency.path })
  let assert Ok(prompt_dependency) =
    list.find(bundle.dependencies, fn(dependency) {
      dependency.path == resolved_prompt_path
    })

  assert list.contains(dependency_paths, config_path)
  assert list.contains(dependency_paths, resolved_workflow_path)
  assert list.contains(dependency_paths, resolved_prompt_path)
  assert prompt_dependency.contents == prompt_text
}

pub fn loads_workflows_with_workspace_profiles_test() {
  let dir = "test/tmp/runtime-bundle-workspace-profiles"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/noop.yaml",
      "version: 1\nid: noop\nworkspace:\n  driver: noop\nsteps:\n  - id: run\n    kind: command\n    run: echo noop\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/defaulted.yaml",
      "version: 1\nid: defaulted\nsteps:\n  - id: run\n    kind: command\n    run: echo default\n",
    )
  write_describe_driver(dir, "isolated", "[]")
  write_describe_driver(dir, "noop", "[]")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\n  driver: isolated\n  drivers:\n    isolated:\n      type: custom\n      command: scripts/isolated\n    noop:\n      type: custom\n      command: scripts/noop\nworkflows:\n    noop: workflows/noop.yaml\n    defaulted: workflows/defaulted.yaml\n",
    )
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  let assert Ok(noop) = dict.get(bundle.workflows, "noop")
  let assert Ok(defaulted) = dict.get(bundle.workflows, "defaulted")
  assert noop.workspace_profile == Some("noop")
  assert defaulted.workspace_profile == None
}

pub fn rejects_hook_backed_profile_with_no_workspace_capabilities_test() {
  let dir = "test/tmp/runtime-bundle-hook-no-capabilities"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/noop.yaml",
      "version: 1\nid: noop\nworkspace:\n  driver: noop\nsteps:\n  - id: run\n    kind: command\n    run: echo noop\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: custom\n      command: scripts/noop\n      lifecycle: [create]\nworkflows:\n    noop: workflows/noop.yaml\n",
    )
  let assert Error(runtime_bundle.BundleError(code, message)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "invalid_config"
  assert string.contains(message, "workspace.drivers.noop.lifecycle")
  assert string.contains(message, "was removed")
}

pub fn rejects_missing_selected_workspace_capabilities_test() {
  let dir = "test/tmp/runtime-bundle-missing-capabilities"
  test_helpers.reset_dir(dir)
  write_describe_driver(dir, "noop", "[\"status\"]")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/noop.yaml",
      "version: 1\nid: noop\nworkspace:\n  driver: noop\n  requires: [assert-only]\nsteps:\n  - id: run\n    kind: command\n    run: echo noop\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: custom\n      command: scripts/noop\nworkflows:\n    noop: workflows/noop.yaml\n",
    )
  let assert Error(runtime_bundle.BundleError(code, message)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "workspace_capabilities_unavailable"
  assert string.contains(message, "workflow noop")
  assert string.contains(message, "workspace.driver noop")
  assert string.contains(message, "missing: assert-only")
}

pub fn loads_driver_profile_with_driver_capabilities_test() {
  let dir = "test/tmp/runtime-bundle-hook-driver-capabilities"
  test_helpers.reset_dir(dir)
  write_describe_driver(
    dir,
    "scherzo-workspace-jj",
    "[\"assert-only\",\"changed-files\"]",
  )
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/noop.yaml",
      "version: 1\nid: noop\nworkspace:\n  driver: noop\n  requires: [assert-only]\nsteps:\n  - id: run\n    kind: command\n    run: echo noop\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: custom\n      command: scripts/scherzo-workspace-jj\nworkflows:\n    noop: workflows/noop.yaml\n",
    )
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert dict.has_key(bundle.workflows, "noop")
}

pub fn loads_selected_driver_profile_after_capability_match_test() {
  let dir = "test/tmp/runtime-bundle-driver-dispatchable"
  test_helpers.reset_dir(dir)
  write_describe_driver(dir, "noop", "[\"assert-only\"]")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/noop.yaml",
      "version: 1\nid: noop\nworkspace:\n  driver: noop\n  requires: [assert-only]\nsteps:\n  - id: run\n    kind: command\n    run: echo noop\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: custom\n      command: scripts/noop\nworkflows:\n    noop: workflows/noop.yaml\n",
    )
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert dict.has_key(bundle.workflows, "noop")
}

pub fn rejects_malformed_workspace_driver_discovery_before_dispatch_test() {
  let dir = "test/tmp/runtime-bundle-driver-discovery-malformed"
  test_helpers.reset_dir(dir)
  write_malformed_describe_driver(dir, "noop")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/noop.yaml",
      "version: 1\nid: noop\nworkspace:\n  driver: noop\nsteps:\n  - id: run\n    kind: command\n    run: echo noop\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\n  driver: noop\n  drivers:\n    noop:\n      type: custom\n      command: scripts/noop\nworkflows:\n    noop: workflows/noop.yaml\n",
    )
  let assert Error(runtime_bundle.BundleError(code, message)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "workspace_driver_discovery_failed"
  assert string.contains(message, "profile noop")
  assert string.contains(message, "scripts/noop")
  assert string.contains(message, "valid JSON")
}

pub fn dogfood_workflows_select_existing_driver_profile_test() {
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(".scherzo/scherzo.yaml"), env)
  let assert Ok(profile) =
    dict.get(bundle.orchestrator.workspace_profiles.profiles, "dogfood-jj")
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
      config_types.WorkspacePublishCommitStack,
    ]

  assert_dogfood_workflows_select_profile(
    [
      "research",
      "implementation",
      "execplan",
      "execplan-revision",
      "execplan-implementation",
      "merge-conflict-resolution",
    ],
    bundle.workflows,
  )
  let assert Ok(conflict_scout) =
    dict.get(bundle.workflows, "github-pr-conflict-scout")
  assert conflict_scout.workspace_profile == Some("noop")
  assert conflict_scout.workspace_capabilities == []
}

pub fn checked_in_merge_conflict_completion_policy_completes_without_review_test() {
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(".scherzo/scherzo.yaml"), env)
  let assert Some(policy) = bundle.effective.handoff.completion_states

  assert policy.default_completion_state
    == Some(workflow_completion_policy.StateByName("In Review"))
  let assert Ok(merge_conflict) =
    dict.get(policy.workflows, "merge-conflict-resolution")
  let assert Ok(implementation) = dict.get(policy.workflows, "implementation")
  let assert Ok(execplan) = dict.get(policy.workflows, "execplan")

  assert merge_conflict.produces_reviewable_artifacts == Some(False)
  assert merge_conflict.requires_review == Some(False)
  assert merge_conflict.no_review_completion_state
    == Some(workflow_completion_policy.StateByName("Done"))
  assert implementation.requires_review == Some(True)
  assert implementation.success_state == None
  assert execplan.requires_review == Some(True)
  assert execplan.success_state == None

  let success =
    worker_success(result_artifact.from_final_response(
      Some("published commit stack"),
      False,
      "test",
    ))
  let merge_outcome =
    workflow_completion_policy.success_outcome(
      Some(policy),
      "merge-conflict-resolution",
      success,
    )
  assert merge_outcome.requires_review
    == workflow_completion_policy.ReviewNotRequired
  assert workflow_completion_policy.choose_linear_completion_state(
      policy,
      "merge-conflict-resolution",
      merge_outcome,
    )
    == workflow_completion_policy.MoveToState(
      workflow_completion_policy.StateByName("Done"),
      "no review is required",
    )

  let empty_merge_outcome =
    workflow_completion_policy.success_outcome(
      Some(policy),
      "merge-conflict-resolution",
      worker_success(result_artifact.empty()),
    )
  assert empty_merge_outcome.expected_artifacts_missing == False
  assert empty_merge_outcome.requires_review
    == workflow_completion_policy.ReviewNotRequired
  assert workflow_completion_policy.choose_linear_completion_state(
      policy,
      "merge-conflict-resolution",
      empty_merge_outcome,
    )
    == workflow_completion_policy.MoveToState(
      workflow_completion_policy.StateByName("Done"),
      "no review is required",
    )

  let implementation_outcome =
    workflow_completion_policy.success_outcome(
      Some(policy),
      "implementation",
      success,
    )
  assert implementation_outcome.requires_review
    == workflow_completion_policy.ReviewRequired
  assert workflow_completion_policy.choose_linear_completion_state(
      policy,
      "implementation",
      implementation_outcome,
    )
    == workflow_completion_policy.MoveToState(
      workflow_completion_policy.StateByName("In Review"),
      "reviewable artifacts were produced",
    )

  let execplan_outcome =
    workflow_completion_policy.success_outcome(
      Some(policy),
      "execplan",
      success,
    )
  assert execplan_outcome.requires_review
    == workflow_completion_policy.ReviewRequired
  assert workflow_completion_policy.choose_linear_completion_state(
      policy,
      "execplan",
      execplan_outcome,
    )
    == workflow_completion_policy.MoveToState(
      workflow_completion_policy.StateByName("In Review"),
      "reviewable artifacts were produced",
    )
}

fn assert_dogfood_workflows_select_profile(
  ids: List(String),
  workflows: dict.Dict(String, workflow_dag.WorkflowDag),
) -> Nil {
  case ids {
    [] -> Nil
    [id, ..rest] -> {
      let assert Ok(dag) = dict.get(workflows, id)
      assert dag.workspace_profile == Some("dogfood-jj")
      let expected_capabilities = case id {
        "implementation" | "execplan-implementation" -> [
          config_types.WorkspaceStatus,
          config_types.WorkspaceDiff,
          config_types.WorkspaceChangedFiles,
          config_types.WorkspaceBaseline,
          config_types.WorkspaceRefreshBase,
          config_types.WorkspacePublishCommitStack,
        ]
        "execplan" -> [
          config_types.WorkspaceStatus,
          config_types.WorkspaceDiff,
          config_types.WorkspaceChangedFiles,
          config_types.WorkspacePublishCommitStack,
        ]
        "execplan-revision" -> [
          config_types.WorkspaceStatus,
          config_types.WorkspaceDiff,
          config_types.WorkspaceChangedFiles,
          config_types.WorkspaceRefreshBase,
          config_types.WorkspacePublishCommitStack,
        ]
        "merge-conflict-resolution" -> [
          config_types.WorkspaceStatus,
          config_types.WorkspaceDiff,
          config_types.WorkspaceChangedFiles,
          config_types.WorkspacePublishCommitStack,
        ]
        _ -> []
      }
      assert dag.workspace_capabilities == expected_capabilities
      assert_dogfood_workflows_select_profile(rest, workflows)
    }
  }
}

pub fn default_workspace_driver_is_builtin_noop_test() {
  let dir = "test/tmp/runtime-bundle-default-noop-driver"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/noop.yaml",
      "version: 1\nid: noop\nworkspace:\n  requires: [assert-only]\nsteps:\n  - id: run\n    kind: command\n    run: echo noop\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\nworkflows:\n    noop: workflows/noop.yaml\n",
    )
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert bundle.orchestrator.workspace_profiles.default_profile == "noop"
  let assert Ok(profile) =
    dict.get(bundle.orchestrator.workspace_profiles.profiles, "noop")
  let assert Some(driver) = profile.driver
  assert driver.command == "$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-noop"
}

pub fn rejects_workflow_with_unknown_workspace_profile_test() {
  let dir = "test/tmp/runtime-bundle-unknown-workspace-profile"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/noop.yaml",
      "version: 1\nid: noop\nworkspace:\n  driver: missing\nsteps:\n  - id: run\n    kind: command\n    run: echo noop\n",
    )
  write_describe_driver(dir, "isolated", "[]")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\n  driver: isolated\n  drivers:\n    isolated:\n      type: custom\n      command: scripts/isolated\nworkflows:\n    noop: workflows/noop.yaml\n",
    )
  let assert Error(runtime_bundle.BundleError(code, message)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "unknown_workspace_profile"
  assert string.contains(message, "noop")
  assert string.contains(message, "missing")
  assert string.contains(message, "isolated")
}

pub fn scheduled_workflow_rejects_issue_context_references_test() {
  let dir = "test/tmp/runtime-bundle-scheduled-issue-context"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/repair.yaml",
      "version: 1\nid: repair\nsteps:\n  - id: inspect\n    kind: command\n    run: echo {{ issue.identifier }}\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkflows:\n    repair: workflows/repair.yaml\nschedules:\n  - id: repair\n    workflow: repair\n    every: 15m\n",
    )
  let assert Error(runtime_bundle.BundleError(code, message)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "scheduled_workflow_requires_issue_context"
  assert string.contains(message, "repair")
  assert string.contains(message, "inspect")
  assert string.contains(message, "issue.identifier")
}

pub fn rejects_absolute_prompt_paths_test() {
  let dir = "test/tmp/runtime-bundle-absolute-prompt"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\nsteps:\n  - id: implement\n    kind: agent\n    prompt: /tmp/prompt.md\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkflows:\n    implementation: workflows/implementation.yaml\n",
    )
  let assert Error(runtime_bundle.BundleError(code, _)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "invalid_prompt_path"
}

pub fn rejects_invalid_project_model_thinking_combination_test() {
  let dir = "test/tmp/runtime-bundle-invalid-model-combo"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/workflows/prompts")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/workflows/prompts/implement.md", "Implement")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\nsteps:\n  - id: implement\n    kind: agent\n    prompt: prompts/implement.md\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nagents:\n  model: openai/gpt-4o\n  thinking: high\nworkflows:\n    implementation: workflows/implementation.yaml\n",
    )
  let assert Error(runtime_bundle.BundleError(code, message)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "invalid_model_thinking"
  assert string.contains(message, "thinking=high")
}

pub fn rejects_invalid_step_model_thinking_combination_after_default_resolution_test() {
  let dir = "test/tmp/runtime-bundle-invalid-step-model-combo"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/workflows/prompts")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/workflows/prompts/implement.md", "Implement")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\nsteps:\n  - id: implement\n    kind: agent\n    prompt: prompts/implement.md\n    model: openai/gpt-4o\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nagents:\n  thinking: high\nworkflows:\n    implementation: workflows/implementation.yaml\n",
    )
  let assert Error(runtime_bundle.BundleError(code, message)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "invalid_model_thinking"
  assert string.contains(message, "workflow implementation step implement")
}

pub fn selects_yaml_workflow_from_issue_label_test() {
  let dir = "test/tmp/runtime-bundle-routing"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/workflows/prompts")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/workflows/prompts/implement.md", "Implement")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\nsteps:\n  - id: implement\n    kind: agent\n    prompt: prompts/implement.md\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkflows:\n  implementation: workflows/implementation.yaml\n",
    )
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  let assert Ok(#("implementation", dag)) =
    runtime_bundle.select_workflow(bundle, issue(["workflow:implementation"]))
  assert dag.id == "implementation"
}

pub fn select_workflow_rejects_scheduled_only_label_when_policy_enforced_test() {
  let dir = "test/tmp/runtime-bundle-scheduled-only-routing"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\nsteps:\n  - id: run\n    kind: command\n    run: echo implementation\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/scheduled-maintenance.yaml",
      "version: 1\nid: scheduled-maintenance\nsteps:\n  - id: run\n    kind: command\n    run: echo scheduled\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    project: TEST\n    check_setup: true\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\nworkflows:\n  implementation: workflows/implementation.yaml\n  scheduled-maintenance: workflows/scheduled-maintenance.yaml\nschedules:\n  - id: scheduled-maintenance\n    workflow: scheduled-maintenance\n    every: 15m\n",
    )

  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert bundle.effective.linear_contract.workflow_labels == ["implementation"]
  let assert Ok(#("implementation", _)) =
    runtime_bundle.select_workflow(bundle, issue(["workflow:implementation"]))
  let assert Error(runtime_bundle.BundleError("unknown_workflow_label", _)) =
    runtime_bundle.select_workflow(
      bundle,
      issue(["workflow:scheduled-maintenance"]),
    )
}

pub fn loads_checked_in_execplan_implementation_workflow_test() {
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(".scherzo/scherzo.yaml"), env)
  assert !dict.has_key(bundle.workflows, "execplan-implementation-v2")
  assert dict.has_key(bundle.workflows, "execplan-implementation")
  let assert Ok(#("execplan-implementation", dag)) =
    runtime_bundle.select_workflow(
      bundle,
      issue(["workflow:execplan-implementation"]),
    )
  assert dag.id == "execplan-implementation"
  let assert Some(terminal) = workflow_dag.terminal_step(dag)
  assert terminal.id == "materialize_code_change_bundle"
}

pub fn checked_in_dogfood_workflows_select_named_jj_profile_test() {
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(".scherzo/scherzo.yaml"), env)
  assert bundle.orchestrator.workspace_profiles.default_profile == "dogfood-jj"
  assert !dict.has_key(
    bundle.orchestrator.workspace_profiles.profiles,
    "default",
  )
  let assert Ok(profile) =
    dict.get(bundle.orchestrator.workspace_profiles.profiles, "dogfood-jj")
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
      config_types.WorkspacePublishCommitStack,
    ]
  assert driver.timeout_ms == 60_000
  assert driver.env
    == [
      #("SCHERZO_GITHUB_REPO", "scherzo-systems/scherzo"),
      #("SCHERZO_JJ_WORKSPACE_BASE_BRANCH", "main"),
      #("SCHERZO_JJ_WORKSPACE_FETCH_BASE", "true"),
      #("SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE", "scherzo-agent"),
      #("SCHERZO_JJ_WORKSPACE_REMOTE", "scherzo-agent"),
    ]

  list.each(
    [
      "research",
      "implementation",
      "execplan",
      "execplan-revision",
      "execplan-implementation",
      "merge-conflict-resolution",
    ],
    fn(workflow_id) {
      let assert Ok(dag) = dict.get(bundle.workflows, workflow_id)
      assert dag.workspace_profile == Some("dogfood-jj")
    },
  )

  let assert Ok(conflict_scout) =
    dict.get(bundle.workflows, "github-pr-conflict-scout")
  assert conflict_scout.workspace_profile == Some("noop")
}

pub fn routing_rejects_missing_unknown_and_multiple_labels_test() {
  let dir = "test/tmp/runtime-bundle-routing-errors"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/workflows/prompts")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/workflows/prompts/research.md", "Research")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/research.yaml",
      "version: 1\nid: research\nsteps:\n  - id: research\n    kind: agent\n    prompt: prompts/research.md\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkflows:\n  research: workflows/research.yaml\n",
    )
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  let assert Error(runtime_bundle.BundleError("missing_workflow_label", _)) =
    runtime_bundle.select_workflow(bundle, issue([]))
  let assert Error(runtime_bundle.BundleError("unknown_workflow_label", _)) =
    runtime_bundle.select_workflow(bundle, issue(["workflow:implementation"]))
  let assert Error(runtime_bundle.BundleError("multiple_workflow_labels", _)) =
    runtime_bundle.select_workflow(
      bundle,
      issue(["workflow:research", "workflow:implementation"]),
    )
}

pub fn default_workflow_is_used_only_when_exact_label_not_required_test() {
  let dir = "test/tmp/runtime-bundle-default-routing"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/workflows/prompts")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/workflows/prompts/research.md", "Research")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/research.yaml",
      "version: 1\nid: research\nsteps:\n  - id: research\n    kind: agent\n    prompt: prompts/research.md\n",
    )
  let config =
    "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\ntask_routing:\n  labels:\n    default_workflow: research\n    require_exactly_one: "
  let suffix = "\nworkflows:\n  research: workflows/research.yaml\n"

  let assert Ok(Nil) =
    simplifile.write(dir <> "/scherzo.yaml", config <> "false" <> suffix)
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  let assert Ok(#("research", _)) =
    runtime_bundle.select_workflow(bundle, issue([]))

  let assert Ok(Nil) =
    simplifile.write(dir <> "/scherzo.yaml", config <> "true" <> suffix)
  let assert Ok(strict_bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  let assert Error(runtime_bundle.BundleError("missing_workflow_label", _)) =
    runtime_bundle.select_workflow(strict_bundle, issue([]))
}

pub fn rejects_escaping_prompt_paths_test() {
  let dir = "test/tmp/runtime-bundle-escaping-prompt"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\nsteps:\n  - id: implement\n    kind: agent\n    prompt: ../prompt.md\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkflows:\n    implementation: workflows/implementation.yaml\n",
    )
  let assert Error(runtime_bundle.BundleError(code, _)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "invalid_prompt_path"
}

pub fn invalid_workflow_contract_rejects_bundle_load_test() {
  let dir = "test/tmp/runtime-bundle-invalid-contract"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    findings:\n      type: document.markdown\n      source:\n        step: missing\n        field: stdout\nsteps:\n  - id: collect_findings\n    kind: command\n    run: echo ok\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\nworkflows:\n    implementation: workflows/implementation.yaml\n",
    )

  let assert Error(runtime_bundle.BundleError(code, message)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "contract_output_unknown_step"
  assert string.contains(message, "unknown step missing")
}
