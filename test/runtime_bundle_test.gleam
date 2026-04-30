import gleam/dict
import gleam/option.{type Option, None, Some}
import scherzo/runtime_bundle
import scherzo/workflow_dag
import simplifile

fn env(name: String) -> Option(String) {
  case name {
    "LINEAR_API_KEY" -> Some("linearkey")
    _ -> None
  }
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

pub fn loads_legacy_markdown_as_one_step_dag_test() {
  let dir = "test/tmp/runtime-bundle-legacy"
  reset_dir(dir)
  let workflow_path = dir <> "/WORKFLOW.md"
  let assert Ok(Nil) =
    simplifile.write(
      workflow_path,
      "---\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\n---\nLegacy prompt\n",
    )
  let assert Ok(bundle) = runtime_bundle.load_with_env(Some(workflow_path), env)
  assert bundle.mode == runtime_bundle.LegacyMarkdown
  let assert Ok(dag) = dict.get(bundle.workflows, "legacy")
  assert dag.max_parallel_steps == 1
  let assert [step] = dag.steps
  assert step.id == "main"
  let assert workflow_dag.AgentStep(workflow_dag.PromptInline("Legacy prompt")) =
    step.kind
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
  assert bundle.mode == runtime_bundle.OrchestratorYaml
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
