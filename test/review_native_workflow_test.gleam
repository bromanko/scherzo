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

const submit_plan_completion_tool = "submit_plan_completion_verdict"

const submit_implementation_completion_tool = "submit_implementation_completion"

const implementation_completion_provider_schema_path = ".scherzo/workflows/schemas/provider/implementation-completion-submission.v1.schema.json"

const implementation_completion_schema_path = ".scherzo/workflows/schemas/implementation-completion-submission.v1.schema.json"

const disposition_provider_schema_path = ".scherzo/workflows/schemas/provider/review-finding-dispositions.v1.schema.json"

const plan_completion_provider_schema_path = ".scherzo/workflows/schemas/provider/plan-completion-verdict-submission.v1.schema.json"

const plan_completion_schema_path = ".scherzo/workflows/schemas/plan-completion-verdict-submission.v1.schema.json"

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

fn assert_plan_completion_tool_source(
  spec: workflow_dag.StructuredOutputSpec,
) -> Nil {
  assert spec.source
    == structured_output_source.PiToolCallSource(
      tool_name: submit_plan_completion_tool,
      parameters_schema_path: Some(plan_completion_provider_schema_path),
    )
}

fn expected_plan_completion_validators() -> List(
  workflow_dag.StructuredOutputValidator,
) {
  [
    workflow_dag.JsonSchemaValidator(
      name: "plan_completion_verdict_submission_provider_shape",
      path: plan_completion_provider_schema_path,
      draft: Some("2020-12"),
    ),
    workflow_dag.JsonSchemaValidator(
      name: "plan_completion_verdict_submission_schema",
      path: plan_completion_schema_path,
      draft: Some("2020-12"),
    ),
    workflow_dag.CommandValidator(
      name: "plan_completion_gate_from_submission",
      argv: [
        "python3",
        ".scherzo/workflows/scripts/scherzo-implementation",
        "gate-plan-completion",
        "--from-submission",
      ],
      timeout_ms: 30_000,
      working_directory: workflow_dag.ValidatorInWorkspace,
      env: [],
    ),
  ]
}

fn expected_implementation_completion_validators() -> List(
  workflow_dag.StructuredOutputValidator,
) {
  [
    workflow_dag.JsonSchemaValidator(
      name: "implementation_completion_submission_provider_shape",
      path: implementation_completion_provider_schema_path,
      draft: Some("2020-12"),
    ),
    workflow_dag.JsonSchemaValidator(
      name: "implementation_completion_submission_schema",
      path: implementation_completion_schema_path,
      draft: Some("2020-12"),
    ),
    workflow_dag.CommandValidator(
      name: "implementation_completion_gate_from_submission",
      argv: [
        "python3",
        ".scherzo/workflows/scripts/scherzo-implementation",
        "gate-implementation-completion",
      ],
      timeout_ms: 30_000,
      working_directory: workflow_dag.ValidatorInWorkspace,
      env: [],
    ),
  ]
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

fn assert_implementation_completion_structured_output(
  dag: workflow_dag.WorkflowDag,
) -> Nil {
  let spec = lane_spec(dag, "implement_plan")
  assert spec.artifact_name == "implementation_completion_submission"
  assert spec.required == True
  assert spec.validation_retries == 0
  assert spec.source
    == structured_output_source.PiToolCallSource(
      tool_name: submit_implementation_completion_tool,
      parameters_schema_path: Some(
        implementation_completion_provider_schema_path,
      ),
    )
  assert spec.schema
    == workflow_dag.StructuredObjectSchema([
      "ready_for_verification",
      "changed_files",
      "remaining_required_work",
      "blockers",
    ])
  assert spec.validators == expected_implementation_completion_validators()
}

fn assert_plan_completion_structured_output(
  dag: workflow_dag.WorkflowDag,
  step_id: String,
) -> Nil {
  let spec = lane_spec(dag, step_id)
  assert spec.artifact_name == "plan_completion_verdict_submission"
  assert spec.required == True
  assert spec.validation_retries == 0
  assert spec.schema
    == workflow_dag.StructuredObjectSchema([
      "verdict",
      "blocking_findings",
      "evidence",
      "checked_acceptance_criteria",
      "deferred_manual_verification",
    ])
  assert_plan_completion_tool_source(spec)
  assert spec.validators == expected_plan_completion_validators()
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

fn valid_review_lane_draft_json() -> String {
  "{\"draft_findings\":[],\"review_notes\":[],\"evidence_requests\":[],\"self_check\":{\"summary\":\"Inspected the diff and found no concrete findings.\"}}"
}

fn plan_completion_submission_json(
  verdict: String,
  blocking_findings_json: String,
) -> String {
  "{\"verdict\":\""
  <> verdict
  <> "\",\"blocking_findings\":"
  <> blocking_findings_json
  <> ",\"evidence\":[\"Required behavior is present.\"],\"checked_acceptance_criteria\":[\"Required work.\"],\"deferred_manual_verification\":[]}"
}

fn workflow_schema_files() -> List(String) {
  [
    "implementation-completion-submission.v1.schema.json",
    "plan-completion-verdict-submission.v1.schema.json",
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
    "implementation-completion-submission.v1.schema.json",
    "plan-completion-verdict-submission.v1.schema.json",
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
  let assert Ok(refresh_and_validate) =
    workflow_dag.step_by_id(dag, "refresh_and_validate_after_review")
  assert refresh_and_validate.depends_on == ["materialize_review_dispositions"]
  let refresh_and_validate_run =
    command_step_run(dag, "refresh_and_validate_after_review")
  assert_contains(
    refresh_and_validate_run,
    "refresh-base-and-validate --stage before-validation",
  )

  let assert Ok(finalize_dispositions) =
    workflow_dag.step_by_id(dag, "finalize_review_dispositions")
  assert finalize_dispositions.depends_on
    == ["refresh_and_validate_after_review"]
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
  let assert Ok(refresh_and_validate) =
    workflow_dag.step_by_id(dag, "refresh_and_validate_after_review")
  assert refresh_and_validate.depends_on == ["materialize_review_dispositions"]
  let refresh_and_validate_run =
    command_step_run(dag, "refresh_and_validate_after_review")
  assert_contains(
    refresh_and_validate_run,
    "refresh-base-and-validate --stage before-validation",
  )

  let assert Ok(finalize_dispositions) =
    workflow_dag.step_by_id(dag, "finalize_review_dispositions")
  assert finalize_dispositions.depends_on == ["final_validate"]
  let assert Ok(materialize_commit_stack) =
    workflow_dag.step_by_id(dag, "materialize_commit_stack")
  assert materialize_commit_stack.depends_on == ["finalize_review_dispositions"]

  let feedback_prompt_paths = [
    ".scherzo/workflows/prompts/apply-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-apply-feedback.md",
  ]
  list.each(feedback_prompt_paths, fn(path) {
    let assert Ok(prompt) = simplifile.read(path)
    assert_contains(prompt, "review-finding-dispositions.v1.json")
    assert_contains(prompt, submit_dispositions_tool)
    assert_contains(prompt, "REVIEW_FINAL_ARTIFACT_PATH")
    assert_contains(prompt, "steps.finalize_lanes.stdout")
    assert_contains(prompt, "targeted remediation")
    assert_contains(prompt, "not a fresh review of the whole diff")
    assert_not_contains(prompt, "steps.code_review")
    assert_not_contains(prompt, "steps.review_changes")
    assert_not_contains(prompt, "steps.verify_correctness_evidence")
    assert_not_contains(prompt, "steps.normalize_correctness")
    assert_not_contains(prompt, "steps.verify_test_quality_evidence")
    assert_not_contains(prompt, "steps.normalize_test_quality")
    assert_not_contains(prompt, "steps.verify_idioms_maintainability_evidence")
    assert_not_contains(prompt, "steps.normalize_idioms_maintainability")
    assert_not_contains(prompt, "steps.verify_security_performance_evidence")
    assert_not_contains(prompt, "steps.normalize_security_performance")
    assert_not_contains(prompt, "steps.synthesize_review")
    assert_not_contains(prompt, "steps.validate_native_review_artifacts")
    assert_not_contains(
      prompt,
      "Write `tmp/review-finding-dispositions.v1.json`",
    )
  })
}

pub fn execplan_implementation_completion_blocks_downstream_and_disables_recovery_test() {
  let dag = execplan_implementation_dag()
  assert_implementation_completion_structured_output(dag)

  let assert Ok(implement_plan) = workflow_dag.step_by_id(dag, "implement_plan")
  let assert Ok(None) =
    workflow_dag.effective_recovery_config(dag, implement_plan)
  let assert Ok(gate_no_conflict) =
    workflow_dag.step_by_id(dag, "gate_no_conflict")
  assert gate_no_conflict.depends_on == ["implement_plan"]
  let assert Ok(analyze_changes) =
    workflow_dag.step_by_id(dag, "analyze_changes")
  assert analyze_changes.depends_on == ["gate_no_conflict"]
  let assert Ok(verify_plan_completion) =
    workflow_dag.step_by_id(dag, "verify_plan_completion")
  assert verify_plan_completion.depends_on == ["analyze_changes"]
}

pub fn liv_1469_final_response_without_completion_submission_fails_test() {
  let spec = lane_spec(execplan_implementation_dag(), "implement_plan")
  let liv_1469_response =
    result_artifact.from_final_response_with_tool_calls(
      Some(
        "Changed files: None\nReady for verify_plan_completion: No\nRemaining required work: implement required milestones and tests.",
      ),
      False,
      "review_native_workflow_test",
      [],
    )

  let assert Error(error) = validate_result(spec, liv_1469_response)
  assert structured_output.error_code(error)
    == "structured_output_tool_call_missing"
  assert_contains(
    structured_output.error_message(error),
    submit_implementation_completion_tool,
  )
}

pub fn execplan_plan_completion_verifiers_use_structured_output_test() {
  let dag = execplan_implementation_dag()
  let verifier_steps = [
    "verify_plan_completion",
    "verify_plan_completion_before_final_validation",
  ]
  list.each(verifier_steps, fn(step_id) {
    assert_plan_completion_structured_output(dag, step_id)
    let assert Ok(step) = workflow_dag.step_by_id(dag, step_id)
    let assert Ok(Some(workflow_dag.EffectiveRecoveryConfig(
      attempts: attempts,
      prompt: prompt,
      ..,
    ))) = workflow_dag.effective_recovery_config(dag, step)
    assert attempts == 2
    assert prompt
      == workflow_dag.PromptFile(
        "prompts/execplan-implementation-recover-plan-completion.md",
      )
  })
}

pub fn plan_completion_structured_output_rejects_inconsistent_verdicts_test() {
  let spec = lane_spec(execplan_implementation_dag(), "verify_plan_completion")
  let pass_with_blockers =
    result_artifact.from_final_response_with_tool_calls(
      None,
      False,
      "review_native_workflow_test",
      [
        result_artifact.ToolCallSubmission(
          name: submit_plan_completion_tool,
          arguments_json: Some(plan_completion_submission_json(
            "pass",
            "[\"Acceptance criterion remains unchecked.\"]",
          )),
          status: Some("success"),
          sibling_count: 1,
          receipt_json: Some("{\"remote_mutations\":\"none\"}"),
        ),
      ],
    )

  let assert Error(pass_error) = validate_result(spec, pass_with_blockers)
  assert structured_output.error_code(pass_error)
    == "structured_output_json_schema_rejected"

  let fail_without_blockers =
    result_artifact.from_final_response_with_tool_calls(
      None,
      False,
      "review_native_workflow_test",
      [
        result_artifact.ToolCallSubmission(
          name: submit_plan_completion_tool,
          arguments_json: Some(plan_completion_submission_json("fail", "[]")),
          status: Some("success"),
          sibling_count: 1,
          receipt_json: Some("{\"remote_mutations\":\"none\"}"),
        ),
      ],
    )

  let assert Error(fail_error) = validate_result(spec, fail_without_blockers)
  assert structured_output.error_code(fail_error)
    == "structured_output_json_schema_rejected"
}

pub fn plan_completion_verifier_prompts_do_not_transcribe_machine_context_test() {
  let prompt_paths = [
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion.md",
  ]
  list.each(prompt_paths, fn(path) {
    let assert Ok(prompt) = simplifile.read(path)
    assert_contains(prompt, submit_plan_completion_tool)
    assert_contains(prompt, "Submit only semantic verdict fields")
    assert_not_contains(prompt, "plan-completion-context")
    assert_not_contains(prompt, "copy the context values exactly")
    assert_not_contains(prompt, "PLAN_COMPLETION_BASE_CHANGE_ID")
    assert_not_contains(prompt, "PLAN_COMPLETION_DIFF_FINGERPRINT")
    assert_not_contains(prompt, "Write valid JSON")
    assert_not_contains(prompt, "scherzo-plan-completion-verdict.json` written")
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
