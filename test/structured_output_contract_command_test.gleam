import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/step_artifact
import simplifile
import support/test_helpers
import workflow_context_test_support

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 8000,
    template_field_max_chars: 8000,
    workflow_summary_max_chars: 8000,
  )
}

fn run_shell(command: String) -> step_artifact.StepArtifact {
  command_step.run(
    "structured_output_contract",
    workflow_context_test_support.without_workflow_context(command),
    ".",
    30_000,
    [],
    limits(),
  )
}

pub fn structured_output_contract_check_schema_rejects_nested_enum_test() {
  let dir = "test/tmp/structured-output-contract"
  test_helpers.reset_dir(dir)
  let schema_path = dir <> "/nested-enum.schema.json"
  let assert Ok(Nil) =
    simplifile.write(
      schema_path,
      "{\"type\":\"object\",\"properties\":{\"severity\":{\"type\":\"string\",\"enum\":[\"low\"]}}}\n",
    )

  let artifact =
    run_shell(
      "scripts/scherzo-structured-output-contract check-schema --schema "
      <> schema_path,
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stderr, "/properties/severity/enum")
}

pub fn structured_output_contract_check_workflows_passes_for_checked_in_workflows_test() {
  let dir = "test/tmp/structured-output-contract/all"
  test_helpers.reset_dir(dir)

  let artifact =
    run_shell(
      "scripts/scherzo-structured-output-contract check-workflows --output-dir "
      <> dir,
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "STRUCTURED_OUTPUT_CONTRACT=passed")
  let assert Ok(report) =
    simplifile.read(dir <> "/structured-output-contract-report.v1.json")
  assert string.contains(
    report,
    "\"artifact_type\":\"structured_output_contract_report\"",
  )
  assert string.contains(report, "\"remote_mutations\":\"none\"")
}

pub fn structured_output_contract_check_workflow_reports_prompt_mismatch_test() {
  let dir = "test/tmp/structured-output-contract/prompt-mismatch"
  test_helpers.reset_dir(dir)
  let workflow_path = dir <> "/workflow.yaml"
  let prompt_path = dir <> "/prompt.md"
  let assert Ok(Nil) =
    simplifile.write(
      prompt_path,
      "Call submit_review_lane_draft, but return JSON directly in your final response.\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      workflow_path,
      "version: 1\nid: prompt-mismatch\nsteps:\n  - id: lane_correctness\n    kind: agent\n    prompt: prompt.md\n    structured_output:\n      artifact_name: correctness_submission\n      required: true\n      format: json\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_lane_draft\n        parameters_schema_path: .scherzo/workflows/schemas/provider/review-lane-draft.correctness.v1.schema.json\n      schema:\n        type: object\n        required: [draft_findings, review_notes, evidence_requests, self_check]\n      validators:\n        - name: provider_schema\n          type: json_schema\n          path: .scherzo/workflows/schemas/provider/review-lane-draft.correctness.v1.schema.json\n          draft: '2020-12'\n",
    )

  let artifact =
    run_shell(
      "scripts/scherzo-structured-output-contract check-workflow --workflow "
      <> workflow_path
      <> " --output-dir "
      <> dir,
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stdout, "STRUCTURED_OUTPUT_CONTRACT=failed")
  let assert Ok(report) =
    simplifile.read(dir <> "/structured-output-contract-report.v1.json")
  assert string.contains(
    report,
    "structured_output_prompt_requests_final_response_json",
  )
}

pub fn structured_output_contract_check_workflow_rejects_incomplete_finalize_lanes_test() {
  let dir = "test/tmp/structured-output-contract/incomplete-finalize-lanes"
  test_helpers.reset_dir(dir)
  let workflow_path = dir <> "/workflow.yaml"
  let prompt_path = dir <> "/prompt.md"
  let assert Ok(Nil) =
    simplifile.write(
      prompt_path,
      "Call submit_review_lane_draft with the review lane draft arguments.\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      workflow_path,
      "version: 1\nid: incomplete-finalize-lanes\nsteps:\n  - id: lane_correctness\n    kind: agent\n    prompt: prompt.md\n    structured_output:\n      artifact_name: correctness_submission\n      required: true\n      format: json\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_lane_draft\n        parameters_schema_path: .scherzo/workflows/schemas/provider/review-lane-draft.correctness.v1.schema.json\n      schema:\n        type: object\n        required: [draft_findings, review_notes, evidence_requests, self_check]\n      validators:\n        - name: provider_schema\n          type: json_schema\n          path: .scherzo/workflows/schemas/provider/review-lane-draft.correctness.v1.schema.json\n          draft: '2020-12'\n  - id: finalize_lanes\n    kind: command\n    depends_on: [lane_correctness]\n    run: '.scherzo/workflows/scripts/scherzo-review finalize-lanes --prepare-dir \"$SCHERZO_RUN_ROOT/artifacts/review/prepare_review\" --review-root \"$SCHERZO_RUN_ROOT/artifacts/review\" --lane correctness'\n",
    )

  let artifact =
    run_shell(
      "scripts/scherzo-structured-output-contract check-workflow --workflow "
      <> workflow_path
      <> " --output-dir "
      <> dir,
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stdout, "STRUCTURED_OUTPUT_CONTRACT=failed")
  let assert Ok(report) =
    simplifile.read(dir <> "/structured-output-contract-report.v1.json")
  assert string.contains(report, "structured_output_missing_materialization")
  assert string.contains(report, "missing_materialization")
}
