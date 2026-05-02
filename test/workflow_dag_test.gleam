import gleam/option.{None, Some}
import scherzo/model_config
import scherzo/workflow_dag

fn parse_ok(source: String) -> workflow_dag.WorkflowDag {
  let assert Ok(dag) = workflow_dag.parse(source)
  dag
}

fn error_code(source: String) -> String {
  let assert Error(workflow_dag.DagError(code, _)) = workflow_dag.parse(source)
  code
}

fn minimal() -> String {
  "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\n"
}

pub fn parses_minimal_workflow_dag_test() {
  let dag = parse_ok(minimal())
  assert dag.id == "research"
  assert dag.max_parallel_steps == 1
  let assert [step] = dag.steps
  assert step.id == "main"
  assert step.depends_on == []
  assert step.workspace == workflow_dag.WorkspaceRef(name: "main", from: None)
  assert step.on_failure == workflow_dag.FailWorkflow
  assert step.model_settings == model_config.default_settings()
  let assert workflow_dag.AgentStep(workflow_dag.PromptFile(
    "prompts/research.md",
  )) = step.kind
}

pub fn parses_optional_description_test() {
  let dag =
    parse_ok(
      "version: 1\nid: research\ndescription: Test description\nsteps:\n  - id: main\n    kind: command\n    run: echo ok\n",
    )
  assert dag.description == Some("Test description")
}

pub fn parses_per_step_model_settings_test() {
  let dag =
    parse_ok(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\n    model: github-copilot/gpt-5.1-codex\n    thinking: xhigh\n",
    )
  let assert [step] = dag.steps
  assert step.model_settings.model == Some("github-copilot/gpt-5.1-codex")
  assert step.model_settings.thinking == Some(model_config.ThinkingXHigh)
}

pub fn rejects_invalid_per_step_model_settings_test() {
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n    model: \"sonnet:high\"\n",
    )
    == "invalid_model"
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n    thinking: extreme\n",
    )
    == "invalid_thinking"
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n    provider: openai\n    model: gpt-5\n",
    )
    == "unsupported_provider_field"
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: test\n    kind: command\n    run: gleam test\n    thinking: high\n",
    )
    == "model_settings_on_command_step"
}

pub fn rejects_duplicate_step_ids_test() {
  let code =
    error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n  - id: main\n    kind: command\n    run: gleam test\n",
    )
  assert code == "duplicate_step_id"
}

pub fn rejects_missing_dependencies_test() {
  let code =
    error_code(
      "version: 1\nid: research\nsteps:\n  - id: review\n    kind: agent\n    depends_on: [implement]\n    prompt: r.md\n",
    )
  assert code == "missing_dependency"
}

pub fn rejects_cycles_test() {
  let code =
    error_code(
      "version: 1\nid: research\nsteps:\n  - id: a\n    kind: command\n    depends_on: [b]\n    run: one\n  - id: b\n    kind: command\n    depends_on: [a]\n    run: two\n",
    )
  assert code == "cycle"
}

pub fn rejects_multiple_terminal_steps_test() {
  let code =
    error_code(
      "version: 1\nid: research\nsteps:\n  - id: a\n    kind: command\n    run: one\n  - id: b\n    kind: command\n    run: two\n",
    )
  assert code == "multiple_terminal_steps"
}

pub fn terminal_step_uses_dependency_sink_test() {
  let dag =
    parse_ok(
      "version: 1\nid: research\nsteps:\n  - id: final\n    kind: command\n    depends_on: [implement]\n    run: done\n  - id: implement\n    kind: agent\n    prompt: implement.md\n",
    )
  let assert Some(step) = workflow_dag.terminal_step(dag)
  assert step.id == "final"
}

pub fn accepts_string_workspace_test() {
  let dag =
    parse_ok(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: command\n    workspace: main\n    run: gleam test\n",
    )
  let assert [step] = dag.steps
  assert step.workspace == workflow_dag.WorkspaceRef(name: "main", from: None)
}

pub fn accepts_derived_workspace_from_transitive_dependency_test() {
  let dag =
    parse_ok(
      "version: 1\nid: implementation\nsteps:\n  - id: implement\n    kind: agent\n    prompt: implement.md\n    workspace: main\n  - id: code_review\n    kind: agent\n    depends_on: [implement]\n    prompt: review.md\n    workspace:\n      name: code-review\n      from: main\n",
    )
  let assert [_, review] = dag.steps
  assert review.workspace
    == workflow_dag.WorkspaceRef(name: "code-review", from: Some("main"))
}

pub fn rejects_derived_workspace_without_transitive_source_test() {
  let code =
    error_code(
      "version: 1\nid: implementation\nsteps:\n  - id: implement\n    kind: agent\n    prompt: implement.md\n    workspace: main\n  - id: code_review\n    kind: agent\n    prompt: review.md\n    workspace:\n      name: code-review\n      from: main\n",
    )
  assert code == "invalid_workspace_from"
}

pub fn rejects_invalid_identifiers_test() {
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: bad-step\n    kind: agent\n    prompt: a.md\n",
    )
    == "invalid_step_id"
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    workspace: \"\"\n    prompt: a.md\n",
    )
    == "invalid_workspace_name"
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    workspace: ../main\n    prompt: a.md\n",
    )
    == "invalid_workspace_name"
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    workspace: dir/main\n    prompt: a.md\n",
    )
    == "invalid_workspace_name"
}

pub fn rejects_zero_parallelism_test() {
  assert error_code(
      "version: 1\nid: research\nmax_parallel_steps: 0\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n",
    )
    == "invalid_max_parallel_steps"
}

pub fn defaults_depends_on_and_on_failure_test() {
  let dag = parse_ok(minimal())
  let assert [step] = dag.steps
  assert step.depends_on == []
  assert step.on_failure == workflow_dag.FailWorkflow
}

pub fn parses_on_failure_continue_test() {
  let dag =
    parse_ok(
      "version: 1\nid: research\nsteps:\n  - id: test_step\n    kind: command\n    run: gleam test\n    on_failure: continue\n",
    )
  let assert [step] = dag.steps
  assert step.on_failure == workflow_dag.ContinueWorkflow
}
