import gleam/dict
import gleam/option.{type Option, None, Some}
import scherzo/domain
import scherzo/runtime_bundle
import scherzo/workflow_dag
import simplifile

@external(erlang, "scherzo_test_ffi", "set_cwd")
fn set_cwd(path: String) -> Result(Nil, simplifile.FileError)

fn env(name: String) -> Option(String) {
  case name {
    "LINEAR_API_KEY" -> Some("linearkey")
    "LINEAR_PROJECT_SLUG" -> Some("TEST")
    _ -> None
  }
}

fn issue(labels: List(String)) -> domain.Issue {
  domain.Issue(
    id: "issue-id",
    identifier: "ABC-123",
    title: "Implement DAGs",
    description: None,
    priority: None,
    state: "Todo",
    branch_name: None,
    url: None,
    labels: labels,
    blocked_by: [],
    created_at: None,
    updated_at: None,
  )
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
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

fn write_default_yaml_project(dir: String) -> Nil {
  reset_dir(dir)
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
      "version: 1\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\nworkspace:\n  root: workspaces\nrouting:\n  workflows:\n    implementation: workflows/implementation.yaml\n",
    )
  Nil
}

pub fn rejects_markdown_paths_as_unsupported_config_path_test() {
  let dir = "test/tmp/runtime-bundle-legacy"
  reset_dir(dir)
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
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.write(dir <> "/WORKFLOW.md", "Legacy\n")
  let assert Error(runtime_bundle.BundleError(code, _)) =
    load_default_from_dir(dir)
  assert code == "missing_config_file"
}

pub fn loads_yaml_orchestrator_and_prompt_files_test() {
  let dir = "test/tmp/runtime-bundle-yaml"
  reset_dir(dir)
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
      "version: 1\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\nworkspace:\n  root: workspaces\nrouting:\n  require_exactly_one_workflow_label: true\n  workflows:\n    implementation: workflows/implementation.yaml\n",
    )
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert bundle.config_path == dir <> "/scherzo.yaml"
  let assert Ok(dag) = dict.get(bundle.workflows, "implementation")
  let assert [step] = dag.steps
  let assert workflow_dag.AgentStep(workflow_dag.PromptInline(prompt)) =
    step.kind
  assert prompt == "Implement {{ issue.identifier }}"
}

pub fn rejects_absolute_prompt_paths_test() {
  let dir = "test/tmp/runtime-bundle-absolute-prompt"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\nsteps:\n  - id: implement\n    kind: agent\n    prompt: /tmp/prompt.md\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\nrouting:\n  workflows:\n    implementation: workflows/implementation.yaml\n",
    )
  let assert Error(runtime_bundle.BundleError(code, _)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "invalid_prompt_path"
}

pub fn selects_yaml_workflow_from_issue_label_test() {
  let dir = "test/tmp/runtime-bundle-routing"
  reset_dir(dir)
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
      "version: 1\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\nrouting:\n  workflow_label_prefix: \"workflow:\"\n  require_exactly_one_workflow_label: true\n  workflows:\n    implementation: workflows/implementation.yaml\n",
    )
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  let assert Ok(#("implementation", dag)) =
    runtime_bundle.select_workflow(bundle, issue(["workflow:implementation"]))
  assert dag.id == "implementation"
}

pub fn routing_rejects_missing_unknown_and_multiple_labels_test() {
  let dir = "test/tmp/runtime-bundle-routing-errors"
  reset_dir(dir)
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
      "version: 1\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\nrouting:\n  workflow_label_prefix: \"workflow:\"\n  require_exactly_one_workflow_label: true\n  workflows:\n    research: workflows/research.yaml\n",
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
  reset_dir(dir)
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
    "version: 1\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\nrouting:\n  workflow_label_prefix: \"workflow:\"\n  default_workflow: research\n  require_exactly_one_workflow_label: "
  let suffix = "\n  workflows:\n    research: workflows/research.yaml\n"

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
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\nsteps:\n  - id: implement\n    kind: agent\n    prompt: ../prompt.md\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\nrouting:\n  workflows:\n    implementation: workflows/implementation.yaml\n",
    )
  let assert Error(runtime_bundle.BundleError(code, _)) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  assert code == "invalid_prompt_path"
}
