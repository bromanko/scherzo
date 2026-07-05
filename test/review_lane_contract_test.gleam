import gleam/int
import gleam/list
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

fn run_contract(command: String) -> step_artifact.StepArtifact {
  run_shell(
    ".scherzo/workflows/scripts/scherzo-review-lane-contract " <> command,
  )
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

fn valid_payload_with_summary(summary: String) -> String {
  "{"
  <> "\"draft_findings\":[],"
  <> "\"evidence_requests\":[],"
  <> "\"review_notes\":[],"
  <> "\"self_check\":{\"summary\":\""
  <> summary
  <> "\"}"
  <> "}"
}

fn valid_correctness_payload() -> String {
  valid_payload_with_summary("Inspected the diff.")
}

fn unsupported_evidence_key_payload() -> String {
  "{"
  <> "\"draft_findings\":[{"
  <> "\"draft_finding_id\":\"F1\","
  <> "\"title\":\"Create modal success path may be untested\","
  <> "\"claim\":\"The create modal success path needs review context.\","
  <> "\"category\":\"correctness\","
  <> "\"severity\":\"medium\","
  <> "\"proposed_blocking\":false,"
  <> "\"locations\":[{\"path\":\"src/create_modal.gleam\"}],"
  <> "\"evidence_request_ids\":[\"E1\"],"
  <> "\"suggested_fix\":\"Inspect the create modal success path.\""
  <> "}],"
  <> "\"review_notes\":[],"
  <> "\"evidence_requests\":[{"
  <> "\"request_id\":\"E1\","
  <> "\"draft_finding_id\":\"F1\","
  <> "\"evidence_key\":\"create-modal-success-heuristic\","
  <> "\"claim\":\"The success heuristic should be checked.\","
  <> "\"expected_observation\":\"The diff shows the success path context.\","
  <> "\"target\":{\"changed_file_path\":\"src/create_modal.gleam\"}"
  <> "}],"
  <> "\"self_check\":{\"summary\":\"Inspected unsupported evidence key fixture.\"}"
  <> "}"
}

fn review_lane_structured_output_artifact(
  step_id: String,
  artifact_name: String,
  attempt_index: Int,
  payload: String,
) -> String {
  "{"
  <> "\"schema_version\":1,"
  <> "\"artifact_type\":\"structured_output\","
  <> "\"run_id\":\"run-1\","
  <> "\"workflow_id\":\"implementation\","
  <> "\"step_id\":\""
  <> step_id
  <> "\","
  <> "\"attempt_index\":"
  <> int.to_string(attempt_index)
  <> ","
  <> "\"artifact_name\":\""
  <> artifact_name
  <> "\","
  <> "\"format\":\"json\","
  <> "\"source_type\":\"pi_tool_call\","
  <> "\"source_tool_name\":\"submit_review_lane_draft\","
  <> "\"schema\":{\"type\":\"object\",\"required\":[\"draft_findings\",\"review_notes\",\"evidence_requests\",\"self_check\"]},"
  <> "\"validation\":{\"source_type\":\"pi_tool_call\",\"source_tool_name\":\"submit_review_lane_draft\",\"baseline\":{\"schema_type\":\"object\",\"required_keys\":[\"draft_findings\",\"review_notes\",\"evidence_requests\",\"self_check\"]},\"validators\":[]},"
  <> "\"payload\":"
  <> payload
  <> "}"
}

fn correctness_structured_output_artifact(payload: String) -> String {
  review_lane_structured_output_artifact(
    "lane_correctness",
    "correctness_submission",
    1,
    payload,
  )
}

pub fn review_lane_contract_materializes_structured_output_artifact_test() {
  let dir = "test/tmp/review-lane-contract-structured-output"
  test_helpers.reset_dir(dir)
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

pub fn unsupported_evidence_key_is_materialized_as_context_only_test() {
  let dir = "test/tmp/review-lane-contract-unsupported-evidence"
  test_helpers.reset_dir(dir)
  let prepare_dir = "test/fixtures/review-lane-contract/prepared-review"
  let submission_path = dir <> "/correctness_submission.json"
  let draft_path = dir <> "/review-lane-draft.v1.json"
  let lane_dir = dir <> "/lane"
  let assert Ok(Nil) =
    simplifile.write(
      submission_path,
      correctness_structured_output_artifact(unsupported_evidence_key_payload()),
    )

  let materialized =
    run_contract(
      "materialize --lane correctness --submission "
      <> submission_path
      <> " --prepare-dir "
      <> prepare_dir
      <> " --output "
      <> draft_path,
    )
  assert materialized.status == step_artifact.StepSucceeded
  assert materialized.exit_code == Some(0)

  let assert Ok(draft) = simplifile.read(draft_path)
  assert string.contains(draft, "\"evidence_key\": \"context_only\"")
  assert string.contains(draft, "\"target\": {}")
  assert !string.contains(draft, "\"changed_file_path\"")
  assert string.contains(draft, "\"normalization_diagnostic\"")
  assert string.contains(draft, "create-modal-success-heuristic")
  assert !string.contains(draft, "unsupported_evidence_key_normalized")

  let verified =
    run_shell(
      ".scherzo/workflows/scripts/scherzo-review verify-evidence --lane correctness --draft "
      <> draft_path
      <> " --brief "
      <> prepare_dir
      <> "/review-brief.v1.json --diff-file "
      <> prepare_dir
      <> "/diff.patch --changed-files "
      <> prepare_dir
      <> "/changed-files.v1.json --validation-status "
      <> prepare_dir
      <> "/validation-status.v1.json --context-manifest "
      <> prepare_dir
      <> "/context-manifest.v1.json --output-dir "
      <> lane_dir,
    )
  assert verified.status == step_artifact.StepSucceeded
  assert verified.exit_code == Some(0)

  let assert Ok(ledger) =
    simplifile.read(lane_dir <> "/evidence-ledger.v1.json")
  assert string.contains(ledger, "\"evidence_key\": \"context_only\"")
  assert string.contains(ledger, "\"verdict\": \"context_only\"")
  assert !string.contains(ledger, "\"verdict\": \"rejected\"")
  assert !string.contains(
    ledger,
    "\"evidence_key\": \"create-modal-success-heuristic\"",
  )
  assert string.contains(
    ledger,
    "\"original_evidence_key\": \"create-modal-success-heuristic\"",
  )
}

fn native_lane_step_metadata(
  step_id: String,
  artifact_name: String,
  attempt_index: Int,
  status: String,
  structured_output_path: String,
) -> String {
  let structured_output = case status {
    "success" ->
      "{\"status\":\"valid\",\"artifact_name\":\""
      <> artifact_name
      <> "\",\"format\":\"json\",\"path\":\""
      <> structured_output_path
      <> "\"}"
    _ -> "null"
  }
  "{"
  <> "\"schema_version\":2,"
  <> "\"run_id\":\"run-1\","
  <> "\"workflow_id\":\"implementation\","
  <> "\"step_id\":\""
  <> step_id
  <> "\","
  <> "\"attempt_index\":"
  <> int.to_string(attempt_index)
  <> ","
  <> "\"artifact\":{"
  <> "\"step_id\":\""
  <> step_id
  <> "\","
  <> "\"status\":\""
  <> status
  <> "\","
  <> "\"failure_code\":"
  <> case status {
    "success" -> "null"
    _ -> "\"operator_recovery_lane_interrupted\""
  }
  <> ","
  <> "\"structured_output\":"
  <> structured_output
  <> "}"
  <> "}"
}

pub fn review_lane_contract_resolves_successful_retry_attempt_test() {
  let dir = "test/tmp/review-lane-contract-retry-attempt"
  test_helpers.reset_dir(dir)
  let artifact_dir = dir <> "/artifact-runs/run-1"
  let stable_structured_dir =
    artifact_dir <> "/lane_security_performance/attempt-2/structured"
  let metadata_dir = artifact_dir <> "/lane_security_performance-abc123def456"
  let lane_dir = dir <> "/lane"
  let prepare_dir = "test/fixtures/review-lane-contract/prepared-review"
  let submission_path =
    stable_structured_dir <> "/security_performance_submission.json"
  let draft_path = lane_dir <> "/review-lane-draft.v1.json"
  let assert Ok(Nil) = simplifile.create_directory_all(stable_structured_dir)
  let assert Ok(Nil) = simplifile.create_directory_all(metadata_dir)
  let assert Ok(Nil) = simplifile.create_directory_all(lane_dir)
  let assert Ok(Nil) =
    simplifile.write(
      submission_path,
      review_lane_structured_output_artifact(
        "lane_security_performance",
        "security_performance_submission",
        2,
        valid_correctness_payload(),
      ),
    )
  let assert Ok(Nil) =
    simplifile.write(
      metadata_dir <> "/attempt-1.json",
      native_lane_step_metadata(
        "lane_security_performance",
        "security_performance_submission",
        1,
        "failure",
        "",
      ),
    )
  let assert Ok(Nil) =
    simplifile.write(
      metadata_dir <> "/attempt-2.json",
      native_lane_step_metadata(
        "lane_security_performance",
        "security_performance_submission",
        2,
        "success",
        submission_path,
      ),
    )

  let materialized =
    run_contract(
      "materialize --lane security-performance --artifact-dir "
      <> artifact_dir
      <> " --prepare-dir "
      <> prepare_dir
      <> " --output "
      <> draft_path,
    )
  assert materialized.status == step_artifact.StepSucceeded
  assert materialized.exit_code == Some(0)
  assert string.contains(materialized.stdout, "REVIEW_LANE_MATERIALIZE=ok")
  assert string.contains(materialized.stdout, "attempt-2")

  let verified =
    run_shell(
      ".scherzo/workflows/scripts/scherzo-review verify-evidence --lane security-performance --draft "
      <> draft_path
      <> " --brief "
      <> prepare_dir
      <> "/review-brief.v1.json --diff-file "
      <> prepare_dir
      <> "/diff.patch --changed-files "
      <> prepare_dir
      <> "/changed-files.v1.json --validation-status "
      <> prepare_dir
      <> "/validation-status.v1.json --context-manifest "
      <> prepare_dir
      <> "/context-manifest.v1.json --output-dir "
      <> lane_dir,
    )
  assert verified.status == step_artifact.StepSucceeded
  assert verified.exit_code == Some(0)

  let normalized =
    run_shell(
      ".scherzo/workflows/scripts/scherzo-review normalize-lane-result --lane security-performance --draft "
      <> draft_path
      <> " --evidence-ledger "
      <> lane_dir
      <> "/evidence-ledger.v1.json --agent-artifact-dir "
      <> artifact_dir
      <> " --brief "
      <> prepare_dir
      <> "/review-brief.v1.json --output-dir "
      <> lane_dir,
    )
  assert normalized.status == step_artifact.StepSucceeded
  assert normalized.exit_code == Some(0)
  assert string.contains(normalized.stdout, "REVIEW_LANE_STATE=succeeded")

  let lane_result_path = lane_dir <> "/review-lane-security-performance.v1.json"
  let assert Ok(lane_result) = simplifile.read(lane_result_path)
  assert string.contains(lane_result, "\"state\": \"succeeded\"")
  assert !string.contains(lane_result, "review_infrastructure_failure")

  let lane_validation =
    run_shell(
      ".scherzo/workflows/scripts/scherzo-review validate --artifact "
      <> lane_result_path,
    )
  assert lane_validation.status == step_artifact.StepSucceeded
  assert lane_validation.exit_code == Some(0)
}

pub fn review_lane_contract_falls_back_to_latest_retained_submission_without_metadata_test() {
  let dir = "test/tmp/review-lane-contract-retry-fallback"
  test_helpers.reset_dir(dir)
  let artifact_dir = dir <> "/artifact-runs/run-1"
  let attempt_1_structured_dir =
    artifact_dir <> "/lane_security_performance/attempt-1/structured"
  let attempt_2_structured_dir =
    artifact_dir <> "/lane_security_performance/attempt-2/structured"
  let lane_dir = dir <> "/lane"
  let prepare_dir = "test/fixtures/review-lane-contract/prepared-review"
  let attempt_1_submission_path =
    attempt_1_structured_dir <> "/security_performance_submission.json"
  let attempt_2_submission_path =
    attempt_2_structured_dir <> "/security_performance_submission.json"
  let draft_path = lane_dir <> "/review-lane-draft.v1.json"
  let assert Ok(Nil) = simplifile.create_directory_all(attempt_1_structured_dir)
  let assert Ok(Nil) = simplifile.create_directory_all(attempt_2_structured_dir)
  let assert Ok(Nil) = simplifile.create_directory_all(lane_dir)
  let assert Ok(Nil) =
    simplifile.write(
      attempt_1_submission_path,
      review_lane_structured_output_artifact(
        "lane_security_performance",
        "security_performance_submission",
        1,
        valid_payload_with_summary("attempt one summary"),
      ),
    )
  let assert Ok(Nil) =
    simplifile.write(
      attempt_2_submission_path,
      review_lane_structured_output_artifact(
        "lane_security_performance",
        "security_performance_submission",
        2,
        valid_payload_with_summary("attempt two summary"),
      ),
    )

  let materialized =
    run_contract(
      "materialize --lane security-performance --artifact-dir "
      <> artifact_dir
      <> " --prepare-dir "
      <> prepare_dir
      <> " --output "
      <> draft_path,
    )
  assert materialized.status == step_artifact.StepSucceeded
  assert materialized.exit_code == Some(0)
  assert string.contains(materialized.stdout, "REVIEW_LANE_MATERIALIZE=ok")
  assert string.contains(materialized.stdout, "attempt-2")

  let assert Ok(draft) = simplifile.read(draft_path)
  assert string.contains(draft, "attempt two summary")
  assert !string.contains(draft, "attempt one summary")
}

pub fn review_lane_contract_rejects_metadata_path_outside_artifact_dir_test() {
  let dir = "test/tmp/review-lane-contract-metadata-path-outside"
  test_helpers.reset_dir(dir)
  let artifact_dir = dir <> "/artifact-runs/run-1"
  let metadata_dir = artifact_dir <> "/lane_security_performance-abc123def456"
  let outside_dir = dir <> "/outside"
  let lane_dir = dir <> "/lane"
  let prepare_dir = "test/fixtures/review-lane-contract/prepared-review"
  let outside_submission_path =
    outside_dir <> "/security_performance_submission.json"
  let draft_path = lane_dir <> "/review-lane-draft.v1.json"
  let assert Ok(Nil) = simplifile.create_directory_all(metadata_dir)
  let assert Ok(Nil) = simplifile.create_directory_all(outside_dir)
  let assert Ok(Nil) = simplifile.create_directory_all(lane_dir)
  let assert Ok(Nil) =
    simplifile.write(
      outside_submission_path,
      review_lane_structured_output_artifact(
        "lane_security_performance",
        "security_performance_submission",
        2,
        valid_payload_with_summary("outside artifact dir summary"),
      ),
    )
  let assert Ok(Nil) =
    simplifile.write(
      metadata_dir <> "/attempt-2.json",
      native_lane_step_metadata(
        "lane_security_performance",
        "security_performance_submission",
        2,
        "success",
        outside_submission_path,
      ),
    )

  let materialized =
    run_contract(
      "materialize --lane security-performance --artifact-dir "
      <> artifact_dir
      <> " --prepare-dir "
      <> prepare_dir
      <> " --output "
      <> draft_path,
    )
  assert materialized.status == step_artifact.StepFailed
  assert materialized.exit_code == Some(2)
  assert string.contains(
    materialized.stderr,
    "review_lane_submission_artifact_not_found",
  )
}

pub fn review_lane_contract_still_rejects_metadata_inside_payload_test() {
  let dir = "test/tmp/review-lane-contract-structured-output-invalid-payload"
  test_helpers.reset_dir(dir)
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
    assert string.contains(workflow, "finalize_lanes")
    assert string.contains(workflow, "finalize-lanes")
    assert string.contains(workflow, "--lane correctness")
    assert string.contains(workflow, "$SCHERZO_RUN_ROOT/artifacts/review")
  })
}

pub fn review_lane_contract_live_skips_without_credentials_test() {
  let dir = "test/tmp/review-lane-contract-live"
  test_helpers.reset_dir(dir)

  let artifact =
    run_shell(
      "env -u ANTHROPIC_API_KEY -u OPENAI_API_KEY -u GEMINI_API_KEY -u GOOGLE_API_KEY .scherzo/workflows/scripts/scherzo-review-lane-contract live --workflow .scherzo/workflows/implementation.yaml --output-dir "
      <> dir
      <> " --skip-if-missing-credentials",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(report) = simplifile.read(dir <> "/live-probe-report.v1.json")
  assert string.contains(report, "skipped_missing_credentials")
  assert string.contains(report, "\"remote_mutations\": \"none\"")
}
