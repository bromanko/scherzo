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
      "      artifact_name: review_lane_draft\n      required: true\n      format: json\n      schema:\n        type: object\n        required:\n          - schema_version\n          - artifact_type\n          - findings\n      validators:\n        - name: review_lane_shape\n          type: json_schema\n          path: schemas/review_lane_draft.schema.json\n          draft: \"2020-12\"\n        - name: review_lane_semantics\n          type: command\n          argv:\n            - python3\n            - .scherzo/workflows/scripts/scherzo-review\n            - validate-structured-output\n            - --validator\n            - review_lane_draft\n          timeout_ms: 30000\n          working_directory: repository\n      validation_retries: 1\n",
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
        "      validators:\n        - type: command\n          argv: [python3]\n          timeout_ms: 0\n",
      ),
      "timeout_ms",
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
  let assert [implementation_pack, exec_plan_bundle] = contract.outputs
  assert implementation_pack.type_ == workflow_contract.ImplementationPack
  assert exec_plan_bundle.type_ == workflow_contract.ExecPlanBundle
}
