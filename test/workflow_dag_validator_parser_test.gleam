import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/path
import scherzo/structured_output_source
import scherzo/workflow_contract
import scherzo/workflow_dag
import simplifile

fn parse_ok(source: String) -> workflow_dag.WorkflowDag {
  let assert Ok(dag) = workflow_dag.parse(source)
  dag
}

fn error(source: String) -> workflow_dag.DagError {
  let assert Error(error) = workflow_dag.parse(source)
  error
}

fn structured_spec(source: String) -> workflow_dag.StructuredOutputSpec {
  let dag = parse_ok(source)
  let assert [step] = dag.steps
  let assert workflow_dag.AgentStep(_, Some(spec)) = step.kind
  spec
}

fn workflow_with_structured_output(body: String) -> String {
  "version: 1\nid: structured_review\nsteps:\n  - id: review\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review\n"
  <> body
}

pub fn parses_json_schema_and_command_validators_test() {
  let spec =
    structured_spec(workflow_with_structured_output(
      "      artifact_name: review_lane_draft\n      required: true\n      format: json\n      schema:\n        type: object\n        required:\n          - schema_version\n          - artifact_type\n          - findings\n      validators:\n        - name: review_lane_shape\n          type: json_schema\n          path: schemas/review_lane_draft.schema.json\n          draft: \"2020-12\"\n        - name: review_lane_semantics\n          type: command\n          argv:\n            - python3\n            - .scherzo/workflows/scripts/scherzo-review\n            - validate-structured-output\n            - --validator\n            - review_lane_draft\n          timeout: 30s\n          working_directory: repository\n      validation_retries: 1\n",
    ))

  assert spec.format == workflow_dag.StructuredJson
  assert spec.artifact_name == "review_lane_draft"
  assert spec.required == True
  assert spec.source
    == structured_output_source.PiToolCallSource(
      tool_name: "submit_review",
      require_single: True,
      reject_sibling_tool_calls: True,
      parameters_schema_path: None,
    )
  assert spec.schema
    == workflow_dag.StructuredObjectSchema([
      "schema_version",
      "artifact_type",
      "findings",
    ])
  assert spec.validation_retries == 1
  assert spec.validators
    == [
      workflow_dag.JsonSchemaValidator(
        name: "review_lane_shape",
        path: "schemas/review_lane_draft.schema.json",
        draft: Some("2020-12"),
      ),
      workflow_dag.CommandValidator(
        name: "review_lane_semantics",
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
}

pub fn parses_validator_defaults_test() {
  let spec =
    structured_spec(workflow_with_structured_output(
      "      validators:\n        - type: json_schema\n          path: schemas/review_lane_draft.schema.json\n        - type: command\n          argv: [python3, scripts/validate]\n",
    ))

  assert spec.artifact_name == "review"
  assert spec.required == True
  assert spec.source
    == structured_output_source.PiToolCallSource(
      tool_name: "submit_review",
      require_single: True,
      reject_sibling_tool_calls: True,
      parameters_schema_path: None,
    )
  assert spec.schema == workflow_dag.StructuredObjectSchema([])
  assert spec.validators
    == [
      workflow_dag.JsonSchemaValidator(
        name: "validator_1",
        path: "schemas/review_lane_draft.schema.json",
        draft: None,
      ),
      workflow_dag.CommandValidator(
        name: "validator_2",
        argv: ["python3", "scripts/validate"],
        timeout_ms: 30_000,
        working_directory: workflow_dag.ValidatorInWorkspace,
        env: [],
      ),
    ]
}

pub fn rejects_invalid_validator_declarations_test() {
  let assert Ok(absolute_schema_path) = path.absolute("schema.json")
  let cases = [
    #(
      workflow_with_structured_output("      validators: command\n"),
      "validators",
    ),
    #(
      workflow_with_structured_output(
        "      validators:\n        - name: missing_type\n",
      ),
      "type",
    ),
    #(
      workflow_with_structured_output(
        "      validators:\n        - type: custom\n",
      ),
      "type",
    ),
    #(
      workflow_with_structured_output(
        "      validators:\n        - type: json_schema\n",
      ),
      "path",
    ),
    #(
      workflow_with_structured_output(
        "      validators:\n        - type: json_schema\n          path: \"\"\n",
      ),
      "path",
    ),
    #(
      workflow_with_structured_output(
        "      validators:\n        - type: json_schema\n          path: ../schema.json\n",
      ),
      "path",
    ),
    #(
      workflow_with_structured_output(
        "      validators:\n        - type: json_schema\n          path: \""
        <> absolute_schema_path
        <> "\"\n",
      ),
      "path",
    ),
    #(
      workflow_with_structured_output(
        "      validators:\n        - type: json_schema\n          path: schemas/review_lane_draft.schema.json\n          draft: 2020\n",
      ),
      "draft",
    ),
    #(
      workflow_with_structured_output(
        "      validators:\n        - type: command\n          argv: []\n",
      ),
      "argv",
    ),
    #(
      workflow_with_structured_output(
        "      validators:\n        - type: command\n          argv: [\"\"]\n",
      ),
      "argv",
    ),
    #(
      workflow_with_structured_output(
        "      validators:\n        - type: command\n          argv: [python3, 123]\n",
      ),
      "argv",
    ),
    #(
      workflow_with_structured_output(
        "      validators:\n        - type: command\n          argv: [python3]\n          timeout: 0ms\n",
      ),
      "timeout",
    ),
    #(
      workflow_with_structured_output(
        "      validators:\n        - type: command\n          argv: [python3]\n          timeout: 30\n",
      ),
      "duration string",
    ),
    #(
      workflow_with_structured_output(
        "      validators:\n        - type: command\n          argv: [python3]\n          timeout: 1d\n",
      ),
      "unit ms, s, m, or h",
    ),
    #(
      workflow_with_structured_output(
        "      validators:\n        - type: command\n          argv: [python3]\n          timeout_ms: 30000\n",
      ),
      "timeout_ms was removed",
    ),
    #(
      workflow_with_structured_output(
        "      validators:\n        - type: command\n          argv: [python3]\n          working_directory: elsewhere\n",
      ),
      "working_directory",
    ),
    #(
      workflow_with_structured_output(
        "      validators:\n        - type: command\n          argv: [python3]\n          env:\n            OK: 123\n",
      ),
      "env",
    ),
    #(
      workflow_with_structured_output(
        "      validators:\n        - type: command\n          argv: [python3]\n          env:\n            BAD-NAME: ok\n",
      ),
      "env",
    ),
    #(
      workflow_with_structured_output(
        "      validators:\n        - type: command\n          argv: [python3]\n          env:\n            PATH: ok\n",
      ),
      "env",
    ),
    #(
      workflow_with_structured_output(
        "      validators:\n        - name: duplicate\n          type: json_schema\n          path: schemas/a.json\n        - name: duplicate\n          type: command\n          argv: [python3]\n",
      ),
      "duplicate",
    ),
    #(
      workflow_with_structured_output(
        "      validator: review_lane_draft\n      validators: []\n",
      ),
      "validator",
    ),
  ]

  list.each(cases, fn(entry) {
    let #(source, field) = entry
    let workflow_dag.DagError(_, message) = error(source)
    assert string.contains(message, field)
  })
}

pub fn removed_validator_timeout_ms_fails_before_missing_type_test() {
  let workflow_dag.DagError(code, message) =
    error(workflow_with_structured_output(
      "      validators:\n        - timeout_ms: 30000\n",
    ))

  assert code == "removed_timeout_ms"
  assert string.contains(message, "timeout_ms")
  assert string.contains(message, "timeout")
  assert string.contains(message, "SCHERZO_YAML_SIMPLIFIED_V1")
}

pub fn legacy_review_validator_lowers_to_command_validator_test() {
  let spec =
    structured_spec(workflow_with_structured_output(
      "      artifact_name: review_lane_draft\n      validator: review_lane_draft\n",
    ))

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
}

pub fn parses_descriptor_first_contracts_in_workflow_yaml_test() {
  let dag =
    parse_ok(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  inputs:\n    exec_plan_bundle:\n      kind: artifact_set\n      media_type: application/json\n      artifact_type: scherzo.exec_plan_bundle.v2\n      required: true\n      source: mapped_output\n  outputs:\n    plan:\n      kind: file\n      media_type: text/markdown\n      artifact_type: scherzo.exec_plan.v1\n      source:\n        step: materialize_bundle\n        path: tmp/execplan-review-doc.md\n    implementation_pack:\n      kind: file\n      media_type: application/json\n      artifact_type: scherzo.implementation_pack.v2\n      source:\n        step: materialize_pack\n        path: tmp/execplan-implementation-pack.json\n    code_change_bundle:\n      kind: artifact_set\n      media_type: application/json\n      artifact_type: scherzo.code_change_bundle.v2\n      source:\n        step: materialize_code_change_bundle\n        path: tmp/execplan-code-change-bundle.json\nsteps:\n  - id: materialize_bundle\n    kind: command\n    run: echo bundle\n  - id: materialize_pack\n    kind: command\n    depends_on: [materialize_bundle]\n    run: echo pack\n  - id: materialize_code_change_bundle\n    kind: command\n    depends_on: [materialize_pack]\n    run: echo change\n",
    )

  let assert Some(contract) = dag.contract
  let assert [exec_plan_bundle] = contract.inputs
  let assert [plan, implementation_pack, code_change_bundle] = contract.outputs
  assert exec_plan_bundle.type_ == workflow_contract.ExecPlanBundle
  assert plan.type_ == workflow_contract.ExecPlan
  assert implementation_pack.type_ == workflow_contract.ImplementationPack
  assert code_change_bundle.type_ == workflow_contract.CodeChangeBundle
}

pub fn canonical_execplan_workflows_parse_before_routing_test() {
  let workflow_paths = [
    #(".scherzo/workflows/execplan.yaml", "execplan"),
    #(".scherzo/workflows/execplan-revision.yaml", "execplan-revision"),
    #(
      ".scherzo/workflows/execplan-implementation.yaml",
      "execplan-implementation",
    ),
  ]

  list.each(workflow_paths, fn(workflow_path) {
    let #(path, expected_id) = workflow_path
    let assert Ok(source) = simplifile.read(path)
    let dag = parse_ok(source)
    assert dag.id == expected_id
    assert !string.contains(source, "docs/schemas/")
    assert string.contains(source, ".scherzo/workflows/schemas/")
      || path == ".scherzo/workflows/execplan-implementation.yaml"
  })

  let assert Ok(implementation_source) =
    simplifile.read(".scherzo/workflows/execplan-implementation.yaml")
  let implementation = parse_ok(implementation_source)
  let implementation_step_ids =
    list.map(implementation.steps, fn(step) { step.id })
  list.each(
    [
      "assert_native_review_cutover",
      "prepare_review",
      "lane_correctness",
      "lane_test_quality",
      "lane_idioms_maintainability",
      "lane_security_performance",
      "synthesize_review",
      "validate_native_review_artifacts",
      "review_changes",
      "apply_review_feedback",
    ],
    fn(step_id) {
      assert list.contains(implementation_step_ids, step_id)
    },
  )

  let assert Ok(drafting_source) =
    simplifile.read(".scherzo/workflows/execplan.yaml")
  let drafting = parse_ok(drafting_source)
  let assert Some(contract) = drafting.contract
  let assert [plan, implementation_pack, exec_plan_bundle] = contract.outputs
  assert plan.type_ == workflow_contract.ExecPlan
  assert implementation_pack.type_ == workflow_contract.ImplementationPack
  assert exec_plan_bundle.type_ == workflow_contract.ExecPlanBundle

  let assert Ok(validate_review_doc) =
    workflow_dag.step_by_id(drafting, "validate_review_doc")
  let assert Ok(Some(recovery_config)) =
    workflow_dag.effective_recovery_config(drafting, validate_review_doc)
  assert recovery_config.attempts == 1
  assert recovery_config.model == None
  assert recovery_config.prompt
    == workflow_dag.PromptFile("prompts/execplan-recover-failed-step.md")

  assert drafting.publication_routes == []

  let assert Error(Nil) =
    workflow_dag.step_by_id(drafting, "publish_review_doc")
  let assert Ok(materialize_bundle) =
    workflow_dag.step_by_id(drafting, "materialize_bundle")
  let assert Ok(None) =
    workflow_dag.effective_recovery_config(drafting, materialize_bundle)

  let assert Ok(recovery_prompt) =
    simplifile.read(
      ".scherzo/workflows/prompts/execplan-recover-failed-step.md",
    )
  assert string.contains(
    recovery_prompt,
    "submit_workflow_step_recovery_result",
  )
}
