import gleam/dict
import gleam/int
import gleam/option.{None, Some}
import gleam/string
import scherzo/config
import scherzo/orchestrator/workflow_reloader
import scherzo/runtime_bundle
import scherzo/workflow_dag
import simplifile
import support/test_helpers

fn workflow_text(root: String, interval_ms: Int) -> String {
  "version: 1
tracker:
  linear:
    api_key_env: HOME
    project: TEST
  states:
    ready: [Todo]
    active: [Todo]
    terminal: [Done]
  polling:
    every: " <> int.to_string(interval_ms) <> "ms
workspace:
  root: " <> root <> "
agents:
  concurrency: 1
  runtime:
    type: pi
    pi:
      executable: fake
task_routing:
  labels:
    require_exactly_one: false
    default_workflow: implementation
workflows:
  implementation: workflows/implementation.yaml
"
}

fn workflow_file_path(dir: String) -> String {
  dir <> "/workflows/implementation.yaml"
}

fn prompt_file_path(dir: String) -> String {
  dir <> "/workflows/prompts/task.md"
}

fn new_prompt_file_path(dir: String) -> String {
  dir <> "/workflows/prompts/new-task.md"
}

fn implementation_workflow_text() -> String {
  "version: 1
id: implementation
steps:
  - id: implement
    kind: agent
    prompt: prompts/task.md
    run_in: main
"
}

fn implementation_workflow_with_gate_text() -> String {
  "version: 1
id: implementation
steps:
  - id: implement
    kind: agent
    prompt: prompts/task.md
    run_in: main
  - id: gate_plan_completion
    kind: command
    depends_on: [implement]
    run: echo gate
"
}

fn implementation_workflow_with_new_prompt_text() -> String {
  "version: 1
id: implementation
steps:
  - id: implement
    kind: agent
    prompt: prompts/new-task.md
    run_in: main
"
}

fn implementation_workflow_with_invalid_contract_text() -> String {
  "version: 1
id: implementation
contract:
  version: 1
  outputs:
    findings:
      type: document.markdown
      source:
        step: missing_step
        field: stdout
steps:
  - id: implement
    kind: agent
    prompt: prompts/task.md
    run_in: main
"
}

fn write_workflow_file(dir: String, contents: String) -> Nil {
  let assert Ok(Nil) = simplifile.write(workflow_file_path(dir), contents)
  Nil
}

fn write_workflow(dir: String, interval_ms: Int) -> String {
  test_helpers.reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      workflow_text(dir <> "/workspaces", interval_ms),
    )
  let assert Ok(Nil) = simplifile.write(prompt_file_path(dir), "Prompt")
  write_workflow_file(dir, implementation_workflow_text())
  config_path
}

fn load_state(path: String) -> workflow_reloader.State {
  let assert Ok(bundle) = runtime_bundle.load(Some(path))
  workflow_reloader.from_bundle(Some(path), bundle)
}

pub fn workflow_reloader_unchanged_contents_do_not_reload_test() {
  let path = write_workflow("test/tmp/workflow-reloader-unchanged", 1000)
  let state = load_state(path)

  case workflow_reloader.reload_if_changed(state) {
    workflow_reloader.Unchanged(next) -> {
      assert next.last_contents == state.last_contents
      assert next.effective.polling.interval_ms == 1000
    }
    _ -> panic as "expected unchanged reload outcome"
  }
}

pub fn workflow_reloader_valid_changed_contents_reload_interval_test() {
  let dir = "test/tmp/workflow-reloader-changed"
  let path = write_workflow(dir, 1000)
  let state = load_state(path)
  let assert Ok(Nil) =
    simplifile.write(path, workflow_text(dir <> "/workspaces", 2500))

  case workflow_reloader.reload_if_changed(state) {
    workflow_reloader.Reloaded(next) -> {
      assert next.effective.polling.interval_ms == 2500
      assert next.reload_state.current_status == config.CurrentValid
    }
    _ -> panic as "expected reloaded outcome"
  }
}

pub fn workflow_reloader_changed_routed_workflow_reloads_dag_test() {
  let dir = "test/tmp/workflow-reloader-routed-workflow-changed"
  let path = write_workflow(dir, 1000)
  let state = load_state(path)
  write_workflow_file(dir, implementation_workflow_with_gate_text())

  case workflow_reloader.reload_if_changed(state) {
    workflow_reloader.Reloaded(next) -> {
      let assert Ok(dag) = dict.get(next.bundle.workflows, "implementation")
      let assert Ok(gate) = workflow_dag.step_by_id(dag, "gate_plan_completion")
      assert gate.id == "gate_plan_completion"
    }
    _ -> panic as "expected routed workflow change to reload"
  }
}

pub fn workflow_reloader_changed_prompt_reloads_prompt_contents_test() {
  let dir = "test/tmp/workflow-reloader-prompt-changed"
  let path = write_workflow(dir, 1000)
  let state = load_state(path)
  let assert Ok(Nil) = simplifile.write(prompt_file_path(dir), "Updated Prompt")

  case workflow_reloader.reload_if_changed(state) {
    workflow_reloader.Reloaded(next) -> {
      let assert Ok(dag) = dict.get(next.bundle.workflows, "implementation")
      let assert [step] = dag.steps
      assert step.kind
        == workflow_dag.AgentStep(
          workflow_dag.PromptInline("Updated Prompt"),
          None,
        )
    }
    _ -> panic as "expected prompt change to reload"
  }
}

pub fn workflow_reloader_invalid_contents_keep_last_known_good_test() {
  let path = write_workflow("test/tmp/workflow-reloader-invalid", 1000)
  let state = load_state(path)
  let assert Ok(Nil) = simplifile.write(path, "version: [")

  case workflow_reloader.reload_if_changed(state) {
    workflow_reloader.Invalid(next, reason, message) -> {
      assert next.effective.polling.interval_ms == 1000
      assert next.reload_state.current_status == config.CurrentInvalid(reason)
      assert string.contains(message, path)
    }
    _ -> panic as "expected invalid reload outcome"
  }
}

pub fn workflow_reloader_invalid_routed_workflow_keeps_last_known_good_and_names_path_test() {
  let dir = "test/tmp/workflow-reloader-invalid-routed-workflow"
  let path = write_workflow(dir, 1000)
  let state = load_state(path)
  let assert Ok(Nil) = simplifile.write(workflow_file_path(dir), "version: [")

  case workflow_reloader.reload_if_changed(state) {
    workflow_reloader.Invalid(next, reason, message) -> {
      assert next.effective.polling.interval_ms == 1000
      assert next.reload_state.current_status == config.CurrentInvalid(reason)
      assert !config.can_dispatch(next.reload_state)
      assert string.contains(message, "workflows/implementation.yaml")
      let assert Ok(dag) = dict.get(next.bundle.workflows, "implementation")
      let assert [step] = dag.steps
      assert step.id == "implement"
    }
    _ -> panic as "expected invalid routed workflow reload outcome"
  }
}

pub fn workflow_reloader_invalid_contract_keeps_last_known_good_bundle_test() {
  let dir = "test/tmp/workflow-reloader-invalid-contract"
  let path = write_workflow(dir, 1000)
  let state = load_state(path)
  write_workflow_file(dir, implementation_workflow_with_invalid_contract_text())

  case workflow_reloader.reload_if_changed(state) {
    workflow_reloader.Invalid(next, reason, message) -> {
      assert next.effective.polling.interval_ms == 1000
      assert next.reload_state.current_status == config.CurrentInvalid(reason)
      assert !config.can_dispatch(next.reload_state)
      assert string.contains(message, "missing_step")
      let assert Ok(dag) = dict.get(next.bundle.workflows, "implementation")
      let assert [step] = dag.steps
      assert step.id == "implement"
      assert dag.contract == None
    }
    _ -> panic as "expected invalid contract reload outcome"
  }
}

pub fn workflow_reloader_missing_prompt_keeps_last_known_good_and_names_path_test() {
  let dir = "test/tmp/workflow-reloader-missing-prompt"
  let path = write_workflow(dir, 1000)
  let state = load_state(path)
  let _ = simplifile.delete(prompt_file_path(dir))

  case workflow_reloader.reload_if_changed(state) {
    workflow_reloader.Invalid(next, reason, message) -> {
      assert next.effective.polling.interval_ms == 1000
      assert next.reload_state.current_status == config.CurrentInvalid(reason)
      assert !config.can_dispatch(next.reload_state)
      assert string.contains(message, "workflows/prompts/task.md")
      let assert Ok(dag) = dict.get(next.bundle.workflows, "implementation")
      let assert [step] = dag.steps
      assert step.kind
        == workflow_dag.AgentStep(workflow_dag.PromptInline("Prompt"), None)
    }
    _ -> panic as "expected missing prompt reload outcome"
  }
}

pub fn workflow_reloader_recovers_when_created_prompt_was_not_in_last_good_bundle_test() {
  let dir = "test/tmp/workflow-reloader-recovers-created-new-prompt"
  let path = write_workflow(dir, 1000)
  let state = load_state(path)
  write_workflow_file(dir, implementation_workflow_with_new_prompt_text())
  let assert workflow_reloader.Invalid(invalid, reason, message) =
    workflow_reloader.reload_if_changed(state)
  assert reason == "missing_prompt_file"
  assert string.contains(message, "workflows/prompts/new-task.md")

  let still_invalid = case workflow_reloader.reload_if_changed(invalid) {
    workflow_reloader.Unchanged(still_invalid) -> {
      assert still_invalid.reload_state.current_status
        == config.CurrentInvalid(reason)
      still_invalid
    }
    _ -> panic as "expected repeated missing new prompt poll to be unchanged"
  }

  let assert Ok(Nil) = simplifile.write(new_prompt_file_path(dir), "New Prompt")
  case workflow_reloader.reload_if_changed(still_invalid) {
    workflow_reloader.Reloaded(recovered) -> {
      assert recovered.reload_state.current_status == config.CurrentValid
      assert config.can_dispatch(recovered.reload_state)
      assert recovered.last_invalid_dependency_snapshot == None
      let assert Ok(dag) =
        dict.get(recovered.bundle.workflows, "implementation")
      let assert [step] = dag.steps
      assert step.kind
        == workflow_dag.AgentStep(workflow_dag.PromptInline("New Prompt"), None)
    }
    _ -> panic as "expected created missing prompt to recover"
  }
}

pub fn workflow_reloader_repeated_invalid_dependency_poll_is_unchanged_test() {
  let dir = "test/tmp/workflow-reloader-repeated-invalid"
  let path = write_workflow(dir, 1000)
  let state = load_state(path)
  let assert Ok(Nil) = simplifile.write(workflow_file_path(dir), "version: [")
  let assert workflow_reloader.Invalid(invalid, _, _) =
    workflow_reloader.reload_if_changed(state)
  let assert Some(snapshot) = invalid.last_invalid_dependency_snapshot

  case workflow_reloader.reload_if_changed(invalid) {
    workflow_reloader.Unchanged(still_invalid) -> {
      assert still_invalid.reload_state.current_status
        == invalid.reload_state.current_status
      assert still_invalid.last_invalid_dependency_snapshot == Some(snapshot)
      let assert Ok(dag) =
        dict.get(still_invalid.bundle.workflows, "implementation")
      let assert [step] = dag.steps
      assert step.id == "implement"
    }
    _ -> panic as "expected repeated invalid dependency poll to be unchanged"
  }
}

pub fn workflow_reloader_recovers_after_invalid_dependency_is_fixed_test() {
  let dir = "test/tmp/workflow-reloader-recovers-new-valid"
  let path = write_workflow(dir, 1000)
  let state = load_state(path)
  let assert Ok(Nil) = simplifile.write(workflow_file_path(dir), "version: [")
  let assert workflow_reloader.Invalid(invalid, _, _) =
    workflow_reloader.reload_if_changed(state)
  write_workflow_file(dir, implementation_workflow_with_gate_text())

  case workflow_reloader.reload_if_changed(invalid) {
    workflow_reloader.Reloaded(recovered) -> {
      assert recovered.reload_state.current_status == config.CurrentValid
      assert config.can_dispatch(recovered.reload_state)
      assert recovered.last_invalid_dependency_snapshot == None
      let assert Ok(dag) =
        dict.get(recovered.bundle.workflows, "implementation")
      let assert Ok(gate) = workflow_dag.step_by_id(dag, "gate_plan_completion")
      assert gate.id == "gate_plan_completion"
    }
    _ -> panic as "expected fixed workflow dependency to recover"
  }

  let exact_dir = "test/tmp/workflow-reloader-recovers-exact-last-good"
  let exact_path = write_workflow(exact_dir, 1000)
  let exact_state = load_state(exact_path)
  let _ = simplifile.delete(prompt_file_path(exact_dir))
  let assert workflow_reloader.Invalid(exact_invalid, _, _) =
    workflow_reloader.reload_if_changed(exact_state)
  let assert Ok(Nil) = simplifile.write(prompt_file_path(exact_dir), "Prompt")

  case workflow_reloader.reload_if_changed(exact_invalid) {
    workflow_reloader.Reloaded(recovered) -> {
      assert recovered.reload_state.current_status == config.CurrentValid
      assert config.can_dispatch(recovered.reload_state)
      assert recovered.last_invalid_dependency_snapshot == None
      let assert Ok(dag) =
        dict.get(recovered.bundle.workflows, "implementation")
      let assert [step] = dag.steps
      assert step.kind
        == workflow_dag.AgentStep(workflow_dag.PromptInline("Prompt"), None)
    }
    _ -> panic as "expected exact last-good prompt restoration to recover"
  }
}

pub fn workflow_reloader_reload_now_reloads_when_config_contents_unchanged_test() {
  let dir = "test/tmp/workflow-reloader-reload-now-unchanged-config"
  let path = write_workflow(dir, 1000)
  let state = load_state(path)
  let assert Ok(Nil) =
    simplifile.write(dir <> "/workflows/prompts/task.md", "Updated Prompt")

  case workflow_reloader.reload_now(state) {
    workflow_reloader.Reloaded(next) -> {
      assert next.last_contents == state.last_contents
      let assert Ok(dag) = dict.get(next.bundle.workflows, "implementation")
      let assert [step] = dag.steps
      assert step.kind
        == workflow_dag.AgentStep(
          workflow_dag.PromptInline("Updated Prompt"),
          None,
        )
    }
    _ -> panic as "expected reload_now to reload unchanged config contents"
  }
}
