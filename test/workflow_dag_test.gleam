import gleam/option.{None, Some}
import scherzo/config/types as config_types
import scherzo/model_config
import scherzo/structured_output_source
import scherzo/workflow_contract
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
  assert dag.workspace_profile == None
  assert dag.workspace_capabilities == []
  assert dag.max_parallel_steps == 1
  assert dag.contract == None
  let assert [step] = dag.steps
  assert step.id == "main"
  assert step.depends_on == []
  assert step.workspace == workflow_dag.WorkspaceRef(name: "main", from: None)
  assert step.on_failure == workflow_dag.FailWorkflow
  assert step.model_settings == model_config.default_settings()
  let assert workflow_dag.AgentStep(
    workflow_dag.PromptFile("prompts/research.md"),
    None,
  ) = step.kind
}

pub fn parses_agent_structured_output_defaults_test() {
  let dag =
    parse_ok(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n",
    )
  let assert [step] = dag.steps
  let assert workflow_dag.AgentStep(
    workflow_dag.PromptFile("prompts/review.md"),
    Some(spec),
  ) = step.kind
  assert spec.format == workflow_dag.StructuredJson
  assert spec.artifact_name == "review_json"
  assert spec.required == True
  assert spec.source
    == structured_output_source.PiToolCallSource(
      tool_name: "submit_review_json",
      require_single: True,
      reject_sibling_tool_calls: True,
      parameters_schema_path: None,
    )
  assert spec.schema == workflow_dag.StructuredObjectSchema([])
  assert spec.validators == []
  assert spec.validation_retries == 1
}

pub fn parses_agent_structured_output_json_contract_test() {
  let dag =
    parse_ok(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      format: json\n      artifact_name: review_result\n      required: true\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_result\n      validator: review_lane_draft\n      validation_retries: 0\n      schema:\n        type: object\n        required:\n          - summary\n          - findings\n",
    )
  let assert [step] = dag.steps
  let assert workflow_dag.AgentStep(
    workflow_dag.PromptFile("prompts/review.md"),
    Some(spec),
  ) = step.kind
  assert spec.format == workflow_dag.StructuredJson
  assert spec.artifact_name == "review_result"
  assert spec.required == True
  assert spec.source
    == structured_output_source.PiToolCallSource(
      tool_name: "submit_review_result",
      require_single: True,
      reject_sibling_tool_calls: True,
      parameters_schema_path: None,
    )
  assert spec.schema
    == workflow_dag.StructuredObjectSchema(["summary", "findings"])
  assert spec.validators
    == [
      workflow_dag.CommandValidator(
        name: "review_lane_draft_compat",
        argv: [
          "python3",
          ".scherzo/workflows/scripts/scherzo-review",
          "validate-structured-output",
          "--validator",
          "review_lane_draft",
        ],
        timeout_ms: 30_000,
        working_directory: workflow_dag.ValidatorInRepository,
        env: [],
      ),
    ]
  assert spec.validation_retries == 0
}

pub fn parses_agent_structured_output_pi_tool_call_source_test() {
  let dag =
    parse_ok(
      "version: 1\nid: structured_review\nsteps:\n  - id: example_json\n    kind: agent\n    prompt: prompts/example.md\n    structured_output:\n      artifact_name: example_artifact\n      source:\n        type: pi_tool_call\n        tool_name: submit_example_artifact\n        require_single: true\n        reject_sibling_tool_calls: true\n      schema:\n        required: [schema_version, artifact_type]\n",
    )
  let assert [step] = dag.steps
  let assert workflow_dag.AgentStep(_, Some(spec)) = step.kind
  assert spec.source
    == structured_output_source.PiToolCallSource(
      tool_name: "submit_example_artifact",
      require_single: True,
      reject_sibling_tool_calls: True,
      parameters_schema_path: None,
    )
}

pub fn rejects_invalid_structured_output_source_contracts_test() {
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output: {}\n",
    )
    == "missing_structured_output_source"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: unknown\n",
    )
    == "unsupported_structured_output_source_type"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: final_response\n",
    )
    == "unsupported_structured_output_source_type"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n",
    )
    == "missing_structured_output_source_tool_name"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: bad tool\n",
    )
    == "invalid_structured_output_source_tool_name"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_example_artifact\n        require_single: false\n",
    )
    == "unsupported_structured_output_source_require_single"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_example_artifact\n        reject_sibling_tool_calls: false\n",
    )
    == "unsupported_structured_output_source_reject_sibling_tool_calls"
}

pub fn rejects_invalid_structured_output_contracts_test() {
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: command\n    run: echo ok\n    structured_output: {}\n",
    )
    == "structured_output_on_command_step"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n      format: yaml\n",
    )
    == "unsupported_structured_output_format"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n      artifact_name: bad-name\n",
    )
    == "invalid_structured_artifact_name"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n      schema:\n        type: array\n",
    )
    == "unsupported_structured_output_schema_type"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n      schema:\n        required: summary\n",
    )
    == "structured_output_schema_required_not_list"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n      schema:\n        required:\n          - 123\n",
    )
    == "structured_output_schema_required_entry_not_string"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n      validator: unknown_contract\n",
    )
    == "unknown_structured_output_validator"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n      validator: 123\n",
    )
    == "structured_output_validator_not_string"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n      validation_retries: 2\n",
    )
    == "invalid_structured_output_validation_retries"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n      validation_retries: once\n",
    )
    == "structured_output_validation_retries_not_int"
}

pub fn parses_workspace_capabilities_test() {
  let dag =
    parse_ok(
      "version: 1\nid: research\nworkspace_capabilities: [assert-only, changed-files]\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\n",
    )
  assert dag.workspace_capabilities
    == [
      config_types.WorkspaceAssertOnly,
      config_types.WorkspaceChangedFiles,
    ]
}

pub fn rejects_invalid_workspace_capabilities_test() {
  assert error_code(
      "version: 1\nid: research\nworkspace_capabilities: assert-only\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n",
    )
    == "workspace_capabilities_not_list"
  assert error_code(
      "version: 1\nid: research\nworkspace_capabilities: [123]\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n",
    )
    == "workspace_capabilities_entry_not_string"
  assert error_code(
      "version: 1\nid: research\nworkspace_capabilities: [pull-request]\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n",
    )
    == "unknown_workspace_capability"
  assert error_code(
      "version: 1\nid: research\nworkspace_capabilities: [assert-only, assert-only]\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n",
    )
    == "duplicate_workspace_capability"
}

pub fn parses_top_level_workspace_profile_test() {
  let dag =
    parse_ok(
      "version: 1\nid: research\nworkspace_profile: noop\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\n",
    )
  assert dag.workspace_profile == Some("noop")
}

pub fn rejects_invalid_workspace_profile_test() {
  assert error_code(
      "version: 1\nid: research\nworkspace_profile: 123\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n",
    )
    == "workspace_profile_not_string"
  assert error_code(
      "version: 1\nid: research\nworkspace_profile: ../noop\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n",
    )
    == "invalid_workspace_profile"
  assert error_code(
      "version: 1\nid: research\nworkspace_profile: Noop\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n",
    )
    == "invalid_workspace_profile"
}

pub fn rejects_step_level_workspace_profile_test() {
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    workspace_profile: noop\n    prompt: a.md\n",
    )
    == "step_workspace_profile_not_supported"
}

pub fn rejects_step_level_workspace_capabilities_test() {
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    workspace_capabilities: [assert-only]\n    prompt: a.md\n",
    )
    == "step_workspace_capabilities_not_supported"
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

pub fn rejects_generic_pi_tool_call_without_matching_json_schema_validator_test() {
  let missing =
    "version: 1\nid: implementation\nsteps:\n  - id: example_json\n    kind: agent\n    prompt: prompts/example.md\n    structured_output:\n      artifact_name: example_artifact\n      source:\n        type: pi_tool_call\n        tool_name: submit_structured_output\n        parameters_schema_path: .scherzo/workflows/schemas/review-lane-draft.correctness.v1.schema.json\n      schema:\n        required: [schema_version, artifact_type]\n"
  assert error_code(missing)
    == "structured_output_parameters_schema_missing_json_schema_validator"

  let mismatched =
    "version: 1\nid: implementation\nsteps:\n  - id: example_json\n    kind: agent\n    prompt: prompts/example.md\n    structured_output:\n      artifact_name: example_artifact\n      source:\n        type: pi_tool_call\n        tool_name: submit_structured_output\n        parameters_schema_path: .scherzo/workflows/schemas/review-lane-draft.correctness.v1.schema.json\n      validators:\n        - name: shape\n          type: json_schema\n          path: .scherzo/workflows/schemas/review-lane-draft.v1.schema.json\n      schema:\n        required: [schema_version, artifact_type]\n"
  assert error_code(mismatched)
    == "structured_output_parameters_schema_path_mismatch"
}

pub fn accepts_generic_pi_tool_call_with_matching_json_schema_validator_test() {
  let dag =
    parse_ok(
      "version: 1\nid: implementation\nsteps:\n  - id: example_json\n    kind: agent\n    prompt: prompts/example.md\n    structured_output:\n      artifact_name: example_artifact\n      source:\n        type: pi_tool_call\n        tool_name: submit_structured_output\n        parameters_schema_path: .scherzo/workflows/schemas/review-lane-draft.correctness.v1.schema.json\n      validators:\n        - name: shape\n          type: json_schema\n          path: .scherzo/workflows/schemas/review-lane-draft.correctness.v1.schema.json\n      schema:\n        required: [schema_version, artifact_type]\n",
    )
  let assert [step] = dag.steps
  let assert workflow_dag.AgentStep(_, Some(spec)) = step.kind
  assert structured_output_source.parameters_schema_path(spec.source)
    == Some(
      ".scherzo/workflows/schemas/review-lane-draft.correctness.v1.schema.json",
    )
}

pub fn validates_contract_output_step_sources_test() {
  let dag =
    parse_ok(
      "version: 1\nid: research\ncontract:\n  version: 1\n  outputs:\n    findings:\n      type: document.markdown\n      source:\n        step: collect_findings\n        field: stdout\nsteps:\n  - id: collect_findings\n    kind: command\n    run: printf '# Findings'\n",
    )
  let assert Some(contract) = dag.contract
  let assert [output] = contract.outputs
  assert output.source
    == Some(workflow_contract.StepField(
      "collect_findings",
      workflow_contract.Stdout,
    ))
}

pub fn rejects_contract_output_unknown_step_test() {
  assert error_code(
      "version: 1\nid: research\ncontract:\n  version: 1\n  outputs:\n    findings:\n      type: document.markdown\n      source:\n        step: missing\n        field: stdout\nsteps:\n  - id: collect_findings\n    kind: command\n    run: echo ok\n",
    )
    == "contract_output_unknown_step"
}

pub fn rejects_contract_final_response_on_command_test() {
  assert error_code(
      "version: 1\nid: research\ncontract:\n  version: 1\n  outputs:\n    findings:\n      type: document.markdown\n      source:\n        step: collect_findings\n        field: final_response\nsteps:\n  - id: collect_findings\n    kind: command\n    run: echo ok\n",
    )
    == "contract_output_field_invalid_for_step"
}

pub fn validates_contract_structured_output_sources_test() {
  let dag =
    parse_ok(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    code_change:\n      type: code_change\n      source:\n        step: summarize_change\n        structured_output: code_change\n    inline_change:\n      type: code_change\n      source:\n        step: summarize_change\n        inline_json: code_change\nsteps:\n  - id: summarize_change\n    kind: agent\n    prompt: prompts/summarize.md\n    structured_output:\n      artifact_name: code_change\n      source:\n        type: pi_tool_call\n        tool_name: submit_code_change\n",
    )
  let assert Some(contract) = dag.contract
  let assert [structured, inline] = contract.outputs
  assert structured.source
    == Some(workflow_contract.StructuredOutput(
      "summarize_change",
      "code_change",
    ))
  assert inline.source
    == Some(workflow_contract.InlineJson("summarize_change", "code_change"))
}

pub fn rejects_contract_structured_output_missing_artifact_test() {
  assert error_code(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    code_change:\n      type: code_change\n      source:\n        step: summarize_change\n        structured_output: code_change\nsteps:\n  - id: summarize_change\n    kind: agent\n    prompt: prompts/summarize.md\n    structured_output:\n      artifact_name: other_change\n      source:\n        type: pi_tool_call\n        tool_name: submit_code_change\n",
    )
    == "contract_output_structured_artifact_missing"
}
