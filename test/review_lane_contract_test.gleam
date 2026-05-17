import gleam/list
import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/step_artifact
import simplifile
import workflow_context_test_support

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 8000,
    template_field_max_chars: 8000,
    workflow_summary_max_chars: 8000,
  )
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

fn run_contract(command: String) -> step_artifact.StepArtifact {
  run_shell("scripts/scherzo-review-lane-contract " <> command)
}

fn run_shell(command: String) -> step_artifact.StepArtifact {
  command_step.run(
    "review_lane_contract",
    workflow_context_test_support.without_workflow_context(command),
    ".",
    300_000,
    [],
    limits(),
  )
}

fn valid_correctness_payload() -> String {
  "{"
  <> "\"draft_findings\":[],"
  <> "\"evidence_requests\":[],"
  <> "\"review_notes\":[],"
  <> "\"self_check\":{\"summary\":\"Inspected the diff.\"}"
  <> "}"
}

fn correctness_structured_output_artifact(payload: String) -> String {
  "{"
  <> "\"schema_version\":1,"
  <> "\"artifact_type\":\"structured_output\","
  <> "\"run_id\":\"run-1\","
  <> "\"workflow_id\":\"implementation\","
  <> "\"step_id\":\"lane_correctness\","
  <> "\"attempt_index\":1,"
  <> "\"artifact_name\":\"correctness_submission\","
  <> "\"format\":\"json\","
  <> "\"source_type\":\"pi_tool_call\","
  <> "\"source_tool_name\":\"submit_review_lane_draft\","
  <> "\"schema\":{\"type\":\"object\",\"required\":[\"draft_findings\",\"review_notes\",\"evidence_requests\",\"self_check\"]},"
  <> "\"validation\":{\"source_type\":\"pi_tool_call\",\"source_tool_name\":\"submit_review_lane_draft\",\"baseline\":{\"schema_type\":\"object\",\"required_keys\":[\"draft_findings\",\"review_notes\",\"evidence_requests\",\"self_check\"]},\"validators\":[]},"
  <> "\"payload\":"
  <> payload
  <> "}"
}

pub fn review_lane_contract_materializes_structured_output_artifact_test() {
  let dir = "test/tmp/review-lane-contract-structured-output"
  reset_dir(dir)
  let submission_path = dir <> "/correctness_submission.json"
  let output_path = dir <> "/review-lane-draft.v1.json"
  let assert Ok(Nil) =
    simplifile.write(
      submission_path,
      correctness_structured_output_artifact(valid_correctness_payload()),
    )

  let artifact =
    run_contract(
      "materialize --lane correctness --submission "
      <> submission_path
      <> " --prepare-dir test/fixtures/review-lane-contract/prepared-review --output "
      <> output_path,
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "REVIEW_LANE_MATERIALIZE=ok")
  let assert Ok(draft) = simplifile.read(output_path)
  assert string.contains(draft, "\"artifact_type\": \"review_lane_draft\"")
  assert string.contains(draft, "\"remote_mutations\": \"none\"")
}

pub fn review_lane_contract_still_rejects_metadata_inside_payload_test() {
  let dir = "test/tmp/review-lane-contract-structured-output-invalid-payload"
  reset_dir(dir)
  let submission_path = dir <> "/correctness_submission.json"
  let output_path = dir <> "/review-lane-draft.v1.json"
  let invalid_payload =
    "{"
    <> "\"schema_version\":1,"
    <> "\"artifact_type\":\"review_lane_draft\","
    <> "\"draft_findings\":[],"
    <> "\"evidence_requests\":[],"
    <> "\"review_notes\":[],"
    <> "\"self_check\":{\"summary\":\"Inspected the diff.\"}"
    <> "}"
  let assert Ok(Nil) =
    simplifile.write(
      submission_path,
      correctness_structured_output_artifact(invalid_payload),
    )

  let artifact =
    run_contract(
      "materialize --lane correctness --submission "
      <> submission_path
      <> " --prepare-dir test/fixtures/review-lane-contract/prepared-review --output "
      <> output_path,
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "review_lane_submission_unexpected_runner_metadata",
  )
}

pub fn review_lane_contract_offline_accepts_routed_review_workflows_test() {
  let workflows = [
    ".scherzo/workflows/implementation.yaml",
    ".scherzo/workflows/execplan-implementation.yaml",
  ]

  list.each(workflows, fn(path) {
    let assert Ok(workflow) = simplifile.read(path)
    assert string.contains(workflow, "tool_name: submit_review_lane_draft")
    assert string.contains(
      workflow,
      ".scherzo/workflows/schemas/provider/review-lane-draft.correctness.v1.schema.json",
    )
    assert string.contains(workflow, "materialize_correctness")
    assert string.contains(workflow, "artifacts/review/lanes/correctness")
  })
}

pub fn review_lane_contract_live_skips_without_credentials_test() {
  let dir = "test/tmp/review-lane-contract-live"
  reset_dir(dir)

  let artifact =
    run_shell(
      "env -u ANTHROPIC_API_KEY -u OPENAI_API_KEY -u GEMINI_API_KEY -u GOOGLE_API_KEY scripts/scherzo-review-lane-contract live --workflow .scherzo/workflows/implementation.yaml --output-dir "
      <> dir
      <> " --skip-if-missing-credentials",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(report) = simplifile.read(dir <> "/live-probe-report.v1.json")
  assert string.contains(report, "skipped_missing_credentials")
  assert string.contains(report, "\"remote_mutations\": \"none\"")
}
