import gleam/dict
import gleam/int
import gleam/option.{Some}
import scherzo/config
import scherzo/orchestrator/workflow_reloader
import scherzo/runtime_bundle
import scherzo/workflow_dag
import simplifile

fn reset_dir(dir: String) -> Nil {
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  Nil
}

fn workflow_text(root: String, interval_ms: Int) -> String {
  "version: 1
tracker:
  kind: linear
  api_key: test-key
  project_slug: TEST
  active_states: [Todo]
  terminal_states: [Done]
workspace:
  root: " <> root <> "
  hooks:
    create: |
      mkdir -p \"$SCHERZO_WORKSPACE_PATH\"
    before_step: |
      test -d \"$SCHERZO_WORKSPACE_PATH\"
    after_step: |
      true
    remove: |
      rm -rf \"$SCHERZO_WORKSPACE_PATH\"
    timeout_ms: 60000
polling:
  interval_ms: " <> int.to_string(interval_ms) <> "
agent:
  max_concurrent_agents: 1
pi:
  command: fake
routing:
  workflow_label_prefix: \"workflow:\"
  require_exactly_one_workflow_label: false
  default_workflow: implementation
  workflows:
    implementation: workflows/implementation.yaml
"
}

fn write_workflow(dir: String, interval_ms: Int) -> String {
  reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      workflow_text(dir <> "/workspaces", interval_ms),
    )
  let assert Ok(Nil) = simplifile.write(prompt_dir <> "/task.md", "Prompt")
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
steps:
  - id: implement
    kind: agent
    prompt: prompts/task.md
    workspace: main
",
    )
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

pub fn workflow_reloader_invalid_contents_keep_last_known_good_test() {
  let path = write_workflow("test/tmp/workflow-reloader-invalid", 1000)
  let state = load_state(path)
  let assert Ok(Nil) = simplifile.write(path, "version: [")

  case workflow_reloader.reload_if_changed(state) {
    workflow_reloader.Invalid(next, reason) -> {
      assert next.effective.polling.interval_ms == 1000
      assert next.reload_state.current_status == config.CurrentInvalid(reason)
    }
    _ -> panic as "expected invalid reload outcome"
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
        == workflow_dag.AgentStep(workflow_dag.PromptInline("Updated Prompt"))
    }
    _ -> panic as "expected reload_now to reload unchanged config contents"
  }
}
