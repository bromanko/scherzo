import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/result_artifact
import scherzo/structured_output
import scherzo/structured_output_source
import scherzo/workflow_dag
import simplifile

const submit_structured_output_tool = "submit_review_lane_draft"

const submit_dispositions_tool = "submit_review_finding_dispositions"

const disposition_provider_schema_path = ".scherzo/workflows/schemas/provider/review-finding-dispositions.v1.schema.json"

fn implementation_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(contents) =
    simplifile.read(".scherzo/workflows/implementation.yaml")
  let assert Ok(dag) = workflow_dag.parse(contents)
  dag
}

fn execplan_implementation_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(contents) =
    simplifile.read(".scherzo/workflows/execplan-implementation.yaml")
  let assert Ok(dag) = workflow_dag.parse(contents)
  dag
}

fn lane_spec(
  dag: workflow_dag.WorkflowDag,
  step_id: String,
) -> workflow_dag.StructuredOutputSpec {
  let assert Ok(step) = workflow_dag.step_by_id(dag, step_id)
  let assert workflow_dag.AgentStep(_, Some(spec)) = step.kind
  spec
}

fn lane_schema_path(step_id: String) -> String {
  case step_id {
    "lane_test_quality" ->
      ".scherzo/workflows/schemas/provider/review-lane-draft.test-quality.v1.schema.json"
    "lane_idioms_maintainability" ->
      ".scherzo/workflows/schemas/provider/review-lane-draft.idioms-maintainability.v1.schema.json"
    "lane_security_performance" ->
      ".scherzo/workflows/schemas/provider/review-lane-draft.security-performance.v1.schema.json"
    _ ->
      ".scherzo/workflows/schemas/provider/review-lane-draft.correctness.v1.schema.json"
  }
}

fn assert_review_tool_source(
  spec: workflow_dag.StructuredOutputSpec,
  schema_path: String,
) -> Nil {
  assert spec.source
    == structured_output_source.PiToolCallSource(
      tool_name: submit_structured_output_tool,
      parameters_schema_path: Some(schema_path),
    )
}

fn expected_review_lane_validators(
  schema_path: String,
) -> List(workflow_dag.StructuredOutputValidator) {
  [
    workflow_dag.JsonSchemaValidator(
      name: "review_lane_submission_shape",
      path: schema_path,
      draft: Some("2020-12"),
    ),
  ]
}

fn assert_review_lane_validators(
  spec: workflow_dag.StructuredOutputSpec,
  schema_path: String,
) -> Nil {
  assert spec.validators == expected_review_lane_validators(schema_path)
}

fn assert_disposition_tool_source(
  spec: workflow_dag.StructuredOutputSpec,
) -> Nil {
  assert spec.source
    == structured_output_source.PiToolCallSource(
      tool_name: submit_dispositions_tool,
      parameters_schema_path: Some(disposition_provider_schema_path),
    )
}

fn expected_disposition_validators() -> List(
  workflow_dag.StructuredOutputValidator,
) {
  [
    workflow_dag.CommandValidator(
      name: "review_finding_disposition_input_semantics",
      argv: [
        "python3",
        ".scherzo/workflows/scripts/scherzo-review",
        "validate-structured-output",
        "--validator",
        "review_finding_disposition_input",
      ],
      timeout_ms: 30_000,
      working_directory: workflow_dag.ValidatorInRepository,
      env: [],
    ),
    workflow_dag.JsonSchemaValidator(
      name: "review_finding_disposition_provider_shape",
      path: disposition_provider_schema_path,
      draft: Some("2020-12"),
    ),
    workflow_dag.JsonSchemaValidator(
      name: "review_finding_disposition_input_schema",
      path: ".scherzo/workflows/schemas/review-finding-disposition-input.v1.schema.json",
      draft: Some("2020-12"),
    ),
  ]
}

fn assert_disposition_structured_output(
  dag: workflow_dag.WorkflowDag,
  step_id: String,
) -> Nil {
  let spec = lane_spec(dag, step_id)
  assert spec.artifact_name == "review_finding_disposition_input"
  assert spec.required == True
  assert spec.schema
    == workflow_dag.StructuredObjectSchema([
      "schema_version",
      "artifact_type",
      "entries",
    ])
  assert_disposition_tool_source(spec)
  assert spec.validators == expected_disposition_validators()
}

fn assert_lane_workspace_from_main(
  dag: workflow_dag.WorkflowDag,
  step_id: String,
  workspace_name: String,
) -> Nil {
  let assert Ok(step) = workflow_dag.step_by_id(dag, step_id)
  assert step.workspace
    == workflow_dag.WorkspaceRef(name: workspace_name, from: Some("main"))
}

fn assert_native_review_lane_workspaces_are_isolated(
  dag: workflow_dag.WorkflowDag,
) -> Nil {
  assert_lane_workspace_from_main(dag, "lane_correctness", "review-correctness")
  assert_lane_workspace_from_main(
    dag,
    "lane_test_quality",
    "review-test-quality",
  )
  assert_lane_workspace_from_main(
    dag,
    "lane_idioms_maintainability",
    "review-idioms-maintainability",
  )
  assert_lane_workspace_from_main(
    dag,
    "lane_security_performance",
    "review-security-performance",
  )
}

fn assert_contains(contents: String, expected: String) -> Nil {
  case string.contains(contents, expected) {
    True -> Nil
    False -> {
      let message = "expected text not found: " <> expected
      panic as message
    }
  }
}

fn assert_not_contains(contents: String, unexpected: String) -> Nil {
  case string.contains(contents, unexpected) {
    False -> Nil
    True -> {
      let message = "unexpected text still present: " <> unexpected
      panic as message
    }
  }
}

fn assert_list_contains(values: List(String), expected: String) -> Nil {
  case list.contains(values, expected) {
    True -> Nil
    False -> {
      let message = "expected list item not found: " <> expected
      panic as message
    }
  }
}

fn valid_review_lane_draft_json() -> String {
  "{\"draft_findings\":[],\"review_notes\":[],\"evidence_requests\":[],\"self_check\":{\"summary\":\"Inspected the diff and found no concrete findings.\"}}"
}

fn workflow_schema_files() -> List(String) {
  [
    "review-artifacts.v1.schema.json",
    "review-finding-disposition-input.v1.schema.json",
    "review-lane-draft.v1.schema.json",
    "review-lane-draft.correctness.v1.schema.json",
    "review-lane-draft.test-quality.v1.schema.json",
    "review-lane-draft.idioms-maintainability.v1.schema.json",
    "review-lane-draft.security-performance.v1.schema.json",
  ]
}

fn provider_schema_files() -> List(String) {
  [
    "review-finding-dispositions.v1.schema.json",
    "review-lane-draft.correctness.v1.schema.json",
    "review-lane-draft.test-quality.v1.schema.json",
    "review-lane-draft.idioms-maintainability.v1.schema.json",
    "review-lane-draft.security-performance.v1.schema.json",
  ]
}

fn provider_review_workflow_paths() -> List(String) {
  [
    ".scherzo/workflows/implementation.yaml",
    ".scherzo/workflows/execplan-implementation.yaml",
  ]
}

fn routed_review_dags() -> List(workflow_dag.WorkflowDag) {
  [implementation_dag(), execplan_implementation_dag()]
}

fn command_step_run(dag: workflow_dag.WorkflowDag, step_id: String) -> String {
  let assert Ok(step) = workflow_dag.step_by_id(dag, step_id)
  let assert workflow_dag.CommandStep(run, _) = step.kind
  run
}

fn assert_native_review_downstream_steps_resolve_attempts(
  dag: workflow_dag.WorkflowDag,
) -> Nil {
  let finalize_run = command_step_run(dag, "finalize_lanes")
  assert_contains(finalize_run, "finalize-lanes")
  assert_contains(finalize_run, "--artifact-dir")
  assert_contains(finalize_run, "--prepare-dir")
  assert_contains(finalize_run, "artifacts/review/prepare_review")
  assert_contains(finalize_run, "--review-root")
  assert_contains(finalize_run, "$SCHERZO_RUN_ROOT/artifacts/review")
  assert_contains(finalize_run, "--dirty-tree-dir")
  assert_contains(finalize_run, "artifacts/review/dirty_tree")
  assert_contains(finalize_run, "--synthesis-output-dir")
  assert_contains(finalize_run, "artifacts/review/synthesize_review")
  assert_not_contains(finalize_run, "attempt-1/structured")
  assert_not_contains(finalize_run, "attempt-1.json")

  let lane_ids = [
    #("correctness", "correctness"),
    #("test-quality", "test_quality"),
    #("idioms-maintainability", "idioms_maintainability"),
    #("security-performance", "security_performance"),
  ]
  list.each(lane_ids, fn(lane) {
    let #(lane_id, step_suffix) = lane
    assert_contains(finalize_run, "--lane " <> lane_id)
    assert workflow_dag.step_by_id(dag, "materialize_" <> step_suffix)
      == Error(Nil)
    assert workflow_dag.step_by_id(dag, "verify_" <> step_suffix <> "_evidence")
      == Error(Nil)
    assert workflow_dag.step_by_id(dag, "normalize_" <> step_suffix)
      == Error(Nil)
  })
}

fn validate_result(
  spec: workflow_dag.StructuredOutputSpec,
  result: result_artifact.ResultArtifact,
) -> Result(
  structured_output.StructuredOutputValidation,
  structured_output.StructuredOutputError,
) {
  structured_output.validate_agent_result(
    spec,
    result,
    [],
    structured_output.default_validator_runner(
      structured_output.default_validator_context(
        ".scherzo",
        ".",
        "review_native",
        ".scherzo/workflows",
        "test_run",
        "lane_correctness",
        1,
        ".",
        spec.artifact_name,
        "json",
        spec.source,
      ),
      [],
    ),
  )
}

pub fn review_schema_files_are_packaged_with_workflows_test() {
  let assert Ok(True) = simplifile.is_directory(".scherzo/workflows/schemas")
  let assert Ok(True) =
    simplifile.is_directory(".scherzo/workflows/schemas/provider")

  list.each(workflow_schema_files(), fn(name) {
    let assert Ok(True) =
      simplifile.is_file(".scherzo/workflows/schemas/" <> name)
    Nil
  })

  list.each(provider_schema_files(), fn(name) {
    let assert Ok(True) =
      simplifile.is_file(".scherzo/workflows/schemas/provider/" <> name)
    Nil
  })

  list.each(provider_review_workflow_paths(), fn(path) {
    let assert Ok(contents) = simplifile.read(path)
    assert_contains(contents, "submit_review_lane_draft")
    assert_contains(contents, ".scherzo/workflows/schemas/provider/")
    assert_not_contains(contents, "docs/schemas/")
  })
}

pub fn routed_review_lane_steps_use_submit_review_lane_draft_tool_source_test() {
  list.each(routed_review_dags(), fn(dag) {
    assert_review_tool_source(
      lane_spec(dag, "lane_correctness"),
      lane_schema_path("lane_correctness"),
    )
    assert_review_tool_source(
      lane_spec(dag, "lane_test_quality"),
      lane_schema_path("lane_test_quality"),
    )
    assert_review_tool_source(
      lane_spec(dag, "lane_idioms_maintainability"),
      lane_schema_path("lane_idioms_maintainability"),
    )
    assert_review_tool_source(
      lane_spec(dag, "lane_security_performance"),
      lane_schema_path("lane_security_performance"),
    )
  })
}

pub fn routed_review_lane_steps_use_isolated_derived_workspaces_test() {
  list.each(
    routed_review_dags(),
    assert_native_review_lane_workspaces_are_isolated,
  )
}

pub fn routed_review_lane_steps_use_provider_schema_shape_validator_test() {
  list.each(routed_review_dags(), fn(dag) {
    assert_review_lane_validators(
      lane_spec(dag, "lane_correctness"),
      lane_schema_path("lane_correctness"),
    )
    assert_review_lane_validators(
      lane_spec(dag, "lane_test_quality"),
      lane_schema_path("lane_test_quality"),
    )
    assert_review_lane_validators(
      lane_spec(dag, "lane_idioms_maintainability"),
      lane_schema_path("lane_idioms_maintainability"),
    )
    assert_review_lane_validators(
      lane_spec(dag, "lane_security_performance"),
      lane_schema_path("lane_security_performance"),
    )
  })
}

pub fn native_review_downstream_steps_resolve_successful_lane_attempts_test() {
  list.each(
    routed_review_dags(),
    assert_native_review_downstream_steps_resolve_attempts,
  )
}

pub fn native_review_prompts_and_tool_guidance_use_relative_input_ref_examples_test() {
  let prompt_paths = [
    ".scherzo/workflows/prompts/review-native-correctness.md",
    ".scherzo/workflows/prompts/review-native-test-quality.md",
    ".scherzo/workflows/prompts/review-native-idioms-maintainability.md",
    ".scherzo/workflows/prompts/review-native-security-performance.md",
  ]
  list.each(prompt_paths, fn(path) {
    let assert Ok(prompt) = simplifile.read(path)
    assert_contains(prompt, "draft_findings")
    assert_contains(prompt, "Do not include runner-owned metadata fields")
  })

  let assert Ok(extension) =
    simplifile.read(".pi/extensions/scherzo-structured-output/index.ts")
  assert_contains(extension, "remote_mutations: \"none\"")
  assert_contains(extension, "arguments must be a JSON object")
}

pub fn review_lane_draft_tool_is_enabled_for_implementation_lane_steps_test() {
  list.each(provider_review_workflow_paths(), fn(path) {
    let assert Ok(contents) = simplifile.read(path)
    assert_contains(contents, "tool_name: submit_review_lane_draft")
    assert_contains(
      contents,
      "parameters_schema_path: .scherzo/workflows/schemas/provider/",
    )
  })
}

pub fn implementation_workflow_uses_native_agent_lane_steps_test() {
  let dag = implementation_dag()

  assert_review_tool_source(
    lane_spec(dag, "lane_correctness"),
    lane_schema_path("lane_correctness"),
  )
  assert_review_tool_source(
    lane_spec(dag, "lane_test_quality"),
    lane_schema_path("lane_test_quality"),
  )
  assert_review_tool_source(
    lane_spec(dag, "lane_idioms_maintainability"),
    lane_schema_path("lane_idioms_maintainability"),
  )
  assert_review_tool_source(
    lane_spec(dag, "lane_security_performance"),
    lane_schema_path("lane_security_performance"),
  )
  assert_native_review_lane_workspaces_are_isolated(dag)

  let cutover_step_id = "assert_native" <> "_review_cutover"
  assert workflow_dag.step_by_id(dag, cutover_step_id) == Error(Nil)
  assert workflow_dag.step_by_id(dag, "assert_clean_after_lanes") == Error(Nil)
  assert workflow_dag.step_by_id(dag, "synthesize_review") == Error(Nil)
  assert workflow_dag.step_by_id(dag, "validate_native_review_artifacts")
    == Error(Nil)

  let assert Ok(prepare_step) = workflow_dag.step_by_id(dag, "prepare_review")
  assert prepare_step.depends_on == ["validate_before_native_review"]
  let assert workflow_dag.CommandStep(prepare_run, _) = prepare_step.kind
  assert_contains(prepare_run, "prepare-native")
  assert_contains(prepare_run, "--dirty-tree-dir")
  assert_contains(prepare_run, "artifacts/review/dirty_tree")
  assert_contains(prepare_run, "--cutover-contract-dir")
  assert_contains(prepare_run, "artifacts/review/cutover_contract")
  assert_not_contains(prepare_run, "--native" <> "-review-scenario")
  assert_not_contains(prepare_run, "--agent" <> "-backend")

  let assert Ok(finalize_step) = workflow_dag.step_by_id(dag, "finalize_lanes")
  assert finalize_step.on_failure == workflow_dag.FailWorkflow
  let assert workflow_dag.CommandStep(finalize_run, _) = finalize_step.kind
  assert_contains(finalize_run, "finalize-lanes")
  assert_contains(finalize_run, "--synthesis-output-dir")
  assert_not_contains(finalize_run, "repo_root=")

  let assert Error(_) = workflow_dag.step_by_id(dag, "code_review")
  let assert Ok(apply_feedback_step) =
    workflow_dag.step_by_id(dag, "apply_feedback")
  assert apply_feedback_step.depends_on == ["finalize_lanes"]

  assert_disposition_structured_output(dag, "apply_feedback")
  let materialize_run = command_step_run(dag, "materialize_review_dispositions")
  assert_contains(materialize_run, "materialize-disposition-input")
  assert_contains(materialize_run, "--submission-step apply_feedback")
  assert_contains(materialize_run, "tmp/review-finding-dispositions.v1.json")
  let assert Ok(refresh_base) =
    workflow_dag.step_by_id(dag, "refresh_base_before_validation")
  assert refresh_base.depends_on == ["materialize_review_dispositions"]

  let assert Ok(finalize_dispositions) =
    workflow_dag.step_by_id(dag, "finalize_review_dispositions")
  assert finalize_dispositions.depends_on == ["final_validate"]
  let assert Ok(materialize_commit_stack) =
    workflow_dag.step_by_id(dag, "materialize_commit_stack")
  assert materialize_commit_stack.depends_on == ["finalize_review_dispositions"]
}

pub fn execplan_implementation_workflow_finalizes_dispositions_before_publish_test() {
  let dag = execplan_implementation_dag()
  let assert Error(_) = workflow_dag.step_by_id(dag, "review_changes")
  let assert Ok(apply_review_feedback_step) =
    workflow_dag.step_by_id(dag, "apply_review_feedback")
  assert apply_review_feedback_step.depends_on == ["finalize_lanes"]
  assert_disposition_structured_output(dag, "apply_review_feedback")
  let materialize_run = command_step_run(dag, "materialize_review_dispositions")
  assert_contains(materialize_run, "materialize-disposition-input")
  assert_contains(materialize_run, "--submission-step apply_review_feedback")
  assert_contains(materialize_run, "tmp/review-finding-dispositions.v1.json")
  let assert Ok(refresh_base) =
    workflow_dag.step_by_id(dag, "refresh_base_before_validation")
  assert refresh_base.depends_on == ["materialize_review_dispositions"]

  let assert Ok(finalize_dispositions) =
    workflow_dag.step_by_id(dag, "finalize_review_dispositions")
  assert finalize_dispositions.depends_on == ["final_validate"]
  let assert Ok(materialize_commit_stack) =
    workflow_dag.step_by_id(dag, "materialize_commit_stack")
  assert_list_contains(
    materialize_commit_stack.depends_on,
    "finalize_final_plan_completion_gate",
  )
  assert_list_contains(
    materialize_commit_stack.depends_on,
    "finalize_review_dispositions",
  )

  let feedback_prompt_paths = [
    ".scherzo/workflows/prompts/apply-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-apply-feedback.md",
  ]
  list.each(feedback_prompt_paths, fn(path) {
    let assert Ok(prompt) = simplifile.read(path)
    assert_contains(prompt, "review-finding-dispositions.v1.json")
    assert_contains(prompt, submit_dispositions_tool)
    assert_contains(prompt, "REVIEW_FINAL_ARTIFACT_PATH")
    assert_contains(prompt, "targeted remediation")
    assert_contains(prompt, "not a fresh review of the whole diff")
    assert_not_contains(prompt, "steps.code_review")
    assert_not_contains(prompt, "steps.review_changes")
    assert_not_contains(
      prompt,
      "Write `tmp/review-finding-dispositions.v1.json`",
    )
  })
}

pub fn routed_review_rejects_final_response_only_and_accepts_tool_submission_test() {
  let spec = lane_spec(implementation_dag(), "lane_correctness")
  let final_response_only =
    result_artifact.from_final_response_with_tool_calls(
      Some(valid_review_lane_draft_json()),
      False,
      "review_native_workflow_test",
      [],
    )

  let assert Error(missing_tool_call) =
    validate_result(spec, final_response_only)
  assert structured_output.error_code(missing_tool_call)
    == "structured_output_tool_call_missing"
  assert_contains(
    structured_output.error_message(missing_tool_call),
    submit_structured_output_tool,
  )

  let tool_call_result =
    result_artifact.from_final_response_with_tool_calls(
      None,
      False,
      "review_native_workflow_test",
      [
        result_artifact.ToolCallSubmission(
          name: submit_structured_output_tool,
          arguments_json: Some(valid_review_lane_draft_json()),
          status: Some("success"),
          sibling_count: 1,
          receipt_json: Some("{\"remote_mutations\":\"none\"}"),
        ),
      ],
    )

  let assert Ok(structured_output.StructuredOutputPresent(payload)) =
    validate_result(spec, tool_call_result)
  assert_contains(payload, "draft_findings")
}
