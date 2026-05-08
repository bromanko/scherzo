import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/step_artifact
import simplifile

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 4000,
    template_field_max_chars: 4000,
    workflow_summary_max_chars: 4000,
  )
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

fn run_command(command: String) -> step_artifact.StepArtifact {
  command_step.run("review-artifacts", command, ".", 120_000, [], limits())
}

pub fn dry_run_writes_schema_valid_review_brief_and_lane_result_test() {
  let dir = "test/tmp/review-artifacts-dry-run"
  reset_dir(dir)
  let diff_path = dir <> "/change.diff"
  let output_dir = dir <> "/out"
  let assert Ok(Nil) =
    simplifile.write(
      diff_path,
      "diff --git a/scripts/example b/scripts/example\n"
        <> "index 1111111..2222222 100755\n"
        <> "--- a/scripts/example\n"
        <> "+++ b/scripts/example\n"
        <> "@@ -1,2 +1,3 @@\n"
        <> " #!/usr/bin/env sh\n"
        <> "+echo review\n"
        <> " echo done\n"
        <> "diff --git a/docs/schemas/example.json b/docs/schemas/example.json\n"
        <> "new file mode 100644\n"
        <> "index 0000000..3333333\n"
        <> "--- /dev/null\n"
        <> "+++ b/docs/schemas/example.json\n"
        <> "@@ -0,0 +1,3 @@\n"
        <> "+{\n"
        <> "+  \"schema_version\": 1\n"
        <> "+}\n",
    )

  let artifact =
    run_command(
      "scripts/scherzo-review dry-run --diff-file "
      <> diff_path
      <> " --output-dir "
      <> output_dir
      <> " --test-status unit=passed:ok",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "REVIEW_DRY_RUN=ok")
  assert string.contains(artifact.stdout, "REVIEW_SCHEMA_VERSION=1")
  assert string.contains(artifact.stdout, "REVIEW_REMOTE_MUTATIONS=none")
  assert string.contains(artifact.stdout, "REVIEW_CHANGED_FILES=2")

  let brief_path = output_dir <> "/review-brief.v1.json"
  let lane_result_path = output_dir <> "/review-lane-result.v1.json"
  let assert Ok(brief) = simplifile.read(brief_path)
  let assert Ok(lane_result) = simplifile.read(lane_result_path)
  let assert Ok(log) = simplifile.read(output_dir <> "/review-dry-run.log")
  let assert Ok(manifest) = simplifile.read(output_dir <> "/manifest.v1.json")

  assert string.contains(brief, "\"artifact_type\": \"review_brief\"")
  assert string.contains(brief, "\"schema_version\": 1")
  assert string.contains(brief, "\"implementation_summary\"")
  assert string.contains(brief, "\"workflow helper scripts\"")
  assert string.contains(brief, "\"artifact schema documentation\"")
  assert string.contains(brief, "\"artifact-contract-review\"")
  assert string.contains(brief, "\"status\": \"passed\"")
  assert string.contains(brief, "did not post PR comments")

  assert string.contains(
    lane_result,
    "\"artifact_type\": \"review_lane_result\"",
  )
  assert string.contains(lane_result, "\"id\": \"review_brief\"")
  assert string.contains(lane_result, "\"findings\": []")
  assert string.contains(log, "remote_mutations=none")
  assert string.contains(manifest, "review-brief.v1.json")

  let validation =
    run_command("scripts/scherzo-review validate --artifact " <> brief_path)
  assert validation.status == step_artifact.StepSucceeded
  assert validation.exit_code == Some(0)
  assert string.contains(validation.stdout, "REVIEW_ARTIFACT_VALID=ok")
  assert string.contains(validation.stdout, "REVIEW_ARTIFACT_TYPE=review_brief")

  let lane_validation =
    run_command(
      "scripts/scherzo-review validate --artifact " <> lane_result_path,
    )
  assert lane_validation.status == step_artifact.StepSucceeded
  assert lane_validation.exit_code == Some(0)
  assert string.contains(
    lane_validation.stdout,
    "REVIEW_ARTIFACT_TYPE=review_lane_result",
  )
}

pub fn specialist_review_lanes_emit_schema_valid_lane_results_test() {
  let dir = "test/tmp/review-artifacts-specialist-lanes"
  reset_dir(dir)
  let diff_path = dir <> "/change.diff"
  let brief_dir = dir <> "/brief"
  let assert Ok(Nil) =
    simplifile.write(
      diff_path,
      "diff --git a/src/scherzo/control/example.gleam b/src/scherzo/control/example.gleam\n"
        <> "index 1111111..2222222 100644\n"
        <> "--- a/src/scherzo/control/example.gleam\n"
        <> "+++ b/src/scherzo/control/example.gleam\n"
        <> "@@ -1,3 +1,5 @@\n"
        <> " pub fn changed(value) {\n"
        <> "+  let assert Ok(parsed) = parse(value)\n"
        <> "+  let token = \"supersecrettoken\"\n"
        <> "   value\n"
        <> " }\n",
    )

  let dry_run =
    run_command(
      "scripts/scherzo-review dry-run --diff-file "
      <> diff_path
      <> " --output-dir "
      <> brief_dir
      <> " --test-status unit=failed:failed",
    )
  assert dry_run.status == step_artifact.StepSucceeded
  assert dry_run.exit_code == Some(0)

  let brief_path = brief_dir <> "/review-brief.v1.json"

  let correctness_dir = dir <> "/correctness"
  let correctness =
    run_command(
      "scripts/scherzo-review run-lane --lane correctness --brief "
      <> brief_path
      <> " --diff-file "
      <> diff_path
      <> " --output-dir "
      <> correctness_dir,
    )
  assert correctness.status == step_artifact.StepSucceeded
  assert correctness.exit_code == Some(0)
  assert string.contains(correctness.stdout, "REVIEW_LANE_RUN=ok")
  assert string.contains(correctness.stdout, "REVIEW_LANE=correctness")
  let correctness_result_path =
    correctness_dir <> "/review-lane-correctness.v1.json"
  let assert Ok(correctness_result) = simplifile.read(correctness_result_path)
  assert string.contains(correctness_result, "\"id\": \"correctness\"")
  assert string.contains(correctness_result, "\"evidence_type\": \"test\"")
  assert string.contains(correctness_result, "\"executable_evidence\"")
  assert string.contains(correctness_result, "\"finding_type\": \"suspicion\"")
  let correctness_validation =
    run_command(
      "scripts/scherzo-review validate --artifact " <> correctness_result_path,
    )
  assert correctness_validation.status == step_artifact.StepSucceeded

  let test_quality_dir = dir <> "/test-quality"
  let test_quality =
    run_command(
      "scripts/scherzo-review run-lane --lane test-quality --brief "
      <> brief_path
      <> " --diff-file "
      <> diff_path
      <> " --output-dir "
      <> test_quality_dir,
    )
  assert test_quality.status == step_artifact.StepSucceeded
  assert test_quality.exit_code == Some(0)
  let test_quality_result_path =
    test_quality_dir <> "/review-lane-test-quality.v1.json"
  let assert Ok(test_quality_result) = simplifile.read(test_quality_result_path)
  assert string.contains(test_quality_result, "\"category\": \"testing\"")
  assert string.contains(test_quality_result, "\"proposed_tests\"")
  let test_quality_validation =
    run_command(
      "scripts/scherzo-review validate --artifact " <> test_quality_result_path,
    )
  assert test_quality_validation.status == step_artifact.StepSucceeded

  let idioms_dir = dir <> "/idioms"
  let idioms =
    run_command(
      "scripts/scherzo-review run-lane --lane idioms-maintainability --brief "
      <> brief_path
      <> " --diff-file "
      <> diff_path
      <> " --output-dir "
      <> idioms_dir,
    )
  assert idioms.status == step_artifact.StepSucceeded
  assert idioms.exit_code == Some(0)
  let idioms_result_path =
    idioms_dir <> "/review-lane-idioms-maintainability.v1.json"
  let assert Ok(idioms_result) = simplifile.read(idioms_result_path)
  assert string.contains(idioms_result, "\"review_priority\": \"must-fix\"")
  let idioms_validation =
    run_command(
      "scripts/scherzo-review validate --artifact " <> idioms_result_path,
    )
  assert idioms_validation.status == step_artifact.StepSucceeded

  let security_dir = dir <> "/security-performance"
  let security =
    run_command(
      "scripts/scherzo-review run-lane --lane security-performance --brief "
      <> brief_path
      <> " --diff-file "
      <> diff_path
      <> " --output-dir "
      <> security_dir,
    )
  assert security.status == step_artifact.StepSucceeded
  assert security.exit_code == Some(0)
  assert string.contains(security.stdout, "REVIEW_LANE_REVIEW_DEPTH=deep")
  let security_result_path =
    security_dir <> "/review-lane-security-performance.v1.json"
  let assert Ok(security_result) = simplifile.read(security_result_path)
  let assert Ok(security_log) =
    simplifile.read(security_dir <> "/review-lane-security-performance.log")
  let assert Ok(security_analysis) =
    simplifile.read(
      security_dir <> "/review-lane-security-performance-analysis.v1.json",
    )
  assert string.contains(security_result, "\"review_depth\": \"deep\"")
  assert string.contains(security_result, "Potential hard-coded secret")
  assert string.contains(security_log, "review_depth=deep")
  assert string.contains(
    security_analysis,
    "\"artifact_type\": \"review_lane_analysis\"",
  )
  let security_validation =
    run_command(
      "scripts/scherzo-review validate --artifact " <> security_result_path,
    )
  assert security_validation.status == step_artifact.StepSucceeded

  let synthesis_dir = dir <> "/synthesis"
  let synthesis =
    run_command(
      "scripts/scherzo-review synthesize --brief "
      <> brief_path
      <> " --lane-result "
      <> correctness_result_path
      <> " --lane-result "
      <> test_quality_result_path
      <> " --lane-result "
      <> idioms_result_path
      <> " --lane-result "
      <> security_result_path
      <> " --output-dir "
      <> synthesis_dir,
    )
  assert synthesis.status == step_artifact.StepSucceeded
  assert synthesis.exit_code == Some(0)
  assert string.contains(synthesis.stdout, "REVIEW_SYNTHESIS=ok")
  assert string.contains(synthesis.stdout, "REVIEW_FINAL_ARTIFACT_PATH=")
  assert string.contains(synthesis.stdout, "REVIEW_REMOTE_MUTATIONS=none")

  let synthesis_path = synthesis_dir <> "/review-synthesis.v1.json"
  let final_path = synthesis_dir <> "/final-review.v1.json"
  let assert Ok(synthesis_artifact) = simplifile.read(synthesis_path)
  let assert Ok(final_artifact) = simplifile.read(final_path)
  let assert Ok(synthesis_log) =
    simplifile.read(synthesis_dir <> "/review-synthesis.log")
  assert string.contains(
    synthesis_artifact,
    "\"artifact_type\": \"review_synthesis\"",
  )
  assert string.contains(synthesis_artifact, "\"grouped_findings\"")
  assert string.contains(synthesis_artifact, "\"lane_failed\": 0")
  assert string.contains(final_artifact, "\"artifact_type\": \"final_review\"")
  assert string.contains(final_artifact, "# Staged review summary")
  assert string.contains(final_artifact, "\"remote_mutations\": \"none\"")
  assert string.contains(synthesis_log, "remote_mutations=none")

  let synthesis_validation =
    run_command("scripts/scherzo-review validate --artifact " <> synthesis_path)
  assert synthesis_validation.status == step_artifact.StepSucceeded
  assert string.contains(
    synthesis_validation.stdout,
    "REVIEW_ARTIFACT_TYPE=review_synthesis",
  )

  let final_validation =
    run_command("scripts/scherzo-review validate --artifact " <> final_path)
  assert final_validation.status == step_artifact.StepSucceeded
  assert string.contains(
    final_validation.stdout,
    "REVIEW_ARTIFACT_TYPE=final_review",
  )
}

pub fn security_performance_lane_uses_low_risk_lightweight_depth_test() {
  let dir = "test/tmp/review-artifacts-security-lightweight"
  reset_dir(dir)
  let diff_path = dir <> "/docs.diff"
  let brief_dir = dir <> "/brief"
  let lane_dir = dir <> "/lane"
  let assert Ok(Nil) =
    simplifile.write(
      diff_path,
      "diff --git a/docs/example.md b/docs/example.md\n"
        <> "index 1111111..2222222 100644\n"
        <> "--- a/docs/example.md\n"
        <> "+++ b/docs/example.md\n"
        <> "@@ -1,2 +1,3 @@\n"
        <> " # Example\n"
        <> "+More documentation.\n"
        <> " Existing text.\n",
    )

  let dry_run =
    run_command(
      "scripts/scherzo-review dry-run --diff-file "
      <> diff_path
      <> " --output-dir "
      <> brief_dir,
    )
  assert dry_run.status == step_artifact.StepSucceeded

  let lane =
    run_command(
      "scripts/scherzo-review run-lane --lane security-performance --brief "
      <> brief_dir
      <> "/review-brief.v1.json --diff-file "
      <> diff_path
      <> " --output-dir "
      <> lane_dir,
    )
  assert lane.status == step_artifact.StepSucceeded
  assert lane.exit_code == Some(0)
  assert string.contains(lane.stdout, "REVIEW_LANE_REVIEW_DEPTH=lightweight")

  let lane_result_path = lane_dir <> "/review-lane-security-performance.v1.json"
  let assert Ok(lane_result) = simplifile.read(lane_result_path)
  let assert Ok(lane_log) =
    simplifile.read(lane_dir <> "/review-lane-security-performance.log")
  assert string.contains(lane_result, "\"findings\": []")
  assert string.contains(
    lane_result,
    "Lightweight security/performance review found no concrete heuristic findings.",
  )
  assert string.contains(lane_log, "review_depth=lightweight")
  assert string.contains(
    lane_log,
    "skipped deep security/performance heuristics",
  )

  let validation =
    run_command(
      "scripts/scherzo-review validate --artifact " <> lane_result_path,
    )
  assert validation.status == step_artifact.StepSucceeded
}

pub fn security_performance_lane_ignores_detector_token_literals_test() {
  let dir = "test/tmp/review-artifacts-security-token-literals"
  reset_dir(dir)
  let diff_path = dir <> "/tokens.diff"
  let brief_dir = dir <> "/brief"
  let lane_dir = dir <> "/lane"
  let assert Ok(Nil) =
    simplifile.write(
      diff_path,
      "diff --git a/scripts/scherzo-review b/scripts/scherzo-review\n"
        <> "index 1111111..2222222 100755\n"
        <> "--- a/scripts/scherzo-review\n"
        <> "+++ b/scripts/scherzo-review\n"
        <> "@@ -1,2 +1,8 @@\n"
        <> " def existing():\n"
        <> "+CONCRETE_PROCESS_TOKENS = [\n"
        <> "+    \"shell=True\",\n"
        <> "+    \"os.system(\",\n"
        <> "+    \":os.cmd\",\n"
        <> "+    \"system.cmd\",\n"
        <> "+]\n"
        <> "   return None\n",
    )

  let dry_run =
    run_command(
      "scripts/scherzo-review dry-run --diff-file "
      <> diff_path
      <> " --output-dir "
      <> brief_dir,
    )
  assert dry_run.status == step_artifact.StepSucceeded

  let lane =
    run_command(
      "scripts/scherzo-review run-lane --lane security-performance --brief "
      <> brief_dir
      <> "/review-brief.v1.json --diff-file "
      <> diff_path
      <> " --output-dir "
      <> lane_dir,
    )
  assert lane.status == step_artifact.StepSucceeded
  assert lane.exit_code == Some(0)
  assert string.contains(lane.stdout, "REVIEW_LANE_FINDINGS=0")
  assert string.contains(lane.stdout, "REVIEW_LANE_REVIEW_NOTES=1")

  let lane_result_path = lane_dir <> "/review-lane-security-performance.v1.json"
  let assert Ok(lane_result) = simplifile.read(lane_result_path)
  assert string.contains(lane_result, "\"findings\": []")
  assert string.contains(lane_result, "\"review_notes\"")
  assert !string.contains(
    lane_result,
    "Changed code adds a concrete shell execution hazard",
  )

  let validation =
    run_command(
      "scripts/scherzo-review validate --artifact " <> lane_result_path,
    )
  assert validation.status == step_artifact.StepSucceeded
}

pub fn review_lane_failure_writes_debug_artifacts_test() {
  let dir = "test/tmp/review-artifacts-lane-failure"
  reset_dir(dir)
  let brief_path = dir <> "/invalid-brief.json"
  let lane_dir = dir <> "/lane"
  let assert Ok(Nil) =
    simplifile.write(
      brief_path,
      "{\"schema_version\":1,\"artifact_type\":\"review_brief\"}\n",
    )

  let lane =
    run_command(
      "scripts/scherzo-review run-lane --lane correctness --brief "
      <> brief_path
      <> " --output-dir "
      <> lane_dir,
    )
  assert lane.status == step_artifact.StepFailed
  assert lane.exit_code == Some(1)
  assert string.contains(lane.stdout, "REVIEW_LANE_RUN=failed")
  assert string.contains(lane.stderr, "artifact field 'generated_at_utc'")

  let lane_result_path = lane_dir <> "/review-lane-correctness.v1.json"
  let assert Ok(lane_result) = simplifile.read(lane_result_path)
  let assert Ok(lane_log) =
    simplifile.read(lane_dir <> "/review-lane-correctness.log")
  assert string.contains(lane_result, "\"state\": \"failed\"")
  assert string.contains(lane_log, "state=failed")
  assert string.contains(lane_log, "artifact field 'generated_at_utc'")

  let validation =
    run_command(
      "scripts/scherzo-review validate --artifact " <> lane_result_path,
    )
  assert validation.status == step_artifact.StepSucceeded
}

pub fn review_preflight_runs_full_dry_run_suite_test() {
  let dir = "test/tmp/review-artifacts-preflight"
  reset_dir(dir)

  let preflight =
    run_command("scripts/scherzo-review preflight --output-dir " <> dir)

  assert preflight.status == step_artifact.StepSucceeded
  assert preflight.exit_code == Some(0)
  assert string.contains(preflight.stdout, "REVIEW_PREFLIGHT=ok")
  assert string.contains(preflight.stdout, "REVIEW_PREFLIGHT_SCENARIOS=12")
  assert string.contains(preflight.stdout, "REVIEW_REMOTE_MUTATIONS=none")

  let manifest_path = dir <> "/preflight-manifest.v1.json"
  let assert Ok(manifest) = simplifile.read(manifest_path)
  assert string.contains(manifest, "\"status\": \"passed\"")
  assert string.contains(manifest, "small/trivial PR")
  assert string.contains(manifest, "medium feature PR")
  assert string.contains(manifest, "test-heavy PR")
  assert string.contains(
    manifest,
    "PR #80-inspired staged review precision regression",
  )
  assert string.contains(manifest, "malformed lane output simulation")
  assert string.contains(manifest, "duplicate-conflicting-synthesis")
  assert string.contains(manifest, "\"remote_mutations\": \"none\"")
  assert string.contains(manifest, "\"failed_scenario_count\": 0")

  let assert Ok(empty_final) =
    simplifile.read(
      dir <> "/empty-findings-all-lanes/03-synthesis/final-review.v1.json",
    )
  assert string.contains(empty_final, "No findings from any lane")
  assert string.contains(empty_final, "\"artifact_type\": \"final_review\"")

  let assert Ok(pr80_final) =
    simplifile.read(
      dir <> "/pr80-staged-review-precision/03-synthesis/final-review.v1.json",
    )
  assert string.contains(pr80_final, "\"blocking\": 0")
  assert string.contains(pr80_final, "\"review_notes\"")
  assert string.contains(pr80_final, "scripts/scherzo-review")
  assert string.contains(pr80_final, "src/scherzo/control/review_lane.gleam")
  assert !string.contains(
    pr80_final,
    "New behavioral tests lack visible assertions",
  )

  let assert Ok(lane_failure_synthesis) =
    simplifile.read(
      dir
      <> "/lane-timeout-failure-simulation/03-synthesis/review-synthesis.v1.json",
    )
  assert string.contains(lane_failure_synthesis, "simulated timeout after 1ms")
  assert string.contains(lane_failure_synthesis, "\"kind\": \"lane_failure\"")

  let assert Ok(conflict_synthesis) =
    simplifile.read(
      dir
      <> "/duplicate-conflicting-synthesis/03-synthesis/review-synthesis.v1.json",
    )
  assert string.contains(conflict_synthesis, "deduplicated_finding")
  assert string.contains(
    conflict_synthesis,
    "resolved_conflicting_recommendation",
  )
  assert string.contains(
    conflict_synthesis,
    "downgraded_unproven_correctness_claim",
  )
}

pub fn review_artifact_validator_accepts_review_finding_test() {
  let dir = "test/tmp/review-artifacts-finding"
  reset_dir(dir)
  let artifact_path = dir <> "/finding.json"
  let assert Ok(Nil) =
    simplifile.write(
      artifact_path,
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"artifact_type\": \"review_finding\",\n"
        <> "  \"id\": \"artifact-contract-001\",\n"
        <> "  \"category\": \"artifact_contract\",\n"
        <> "  \"severity\": \"medium\",\n"
        <> "  \"evidence_type\": \"static\",\n"
        <> "  \"verified\": true,\n"
        <> "  \"blocking\": false,\n"
        <> "  \"locations\": [{ \"path\": \"docs/review-artifacts.md\" }],\n"
        <> "  \"summary\": \"Review finding summary\",\n"
        <> "  \"details\": \"Review finding details\",\n"
        <> "  \"suggested_fix\": \"Suggested review finding fix\"\n"
        <> "}\n",
    )

  let artifact =
    run_command("scripts/scherzo-review validate --artifact " <> artifact_path)

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "REVIEW_ARTIFACT_VALID=ok")
  assert string.contains(artifact.stdout, "REVIEW_ARTIFACT_TYPE=review_finding")
}

pub fn review_artifact_validator_rejects_blocking_correctness_without_executable_evidence_test() {
  let dir = "test/tmp/review-artifacts-invalid-correctness-blocker"
  reset_dir(dir)
  let artifact_path = dir <> "/lane-result.json"
  let assert Ok(Nil) =
    simplifile.write(
      artifact_path,
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"artifact_type\": \"review_lane_result\",\n"
        <> "  \"lane\": { \"id\": \"correctness\", \"name\": \"Correctness reviewer\", \"category\": \"correctness\", \"version\": \"1\" },\n"
        <> "  \"execution_status\": { \"state\": \"succeeded\", \"started_at_utc\": \"2026-05-08T00:00:00Z\", \"completed_at_utc\": \"2026-05-08T00:00:01Z\", \"summary\": \"done\" },\n"
        <> "  \"findings\": [{\n"
        <> "    \"id\": \"correctness-001\",\n"
        <> "    \"category\": \"correctness\",\n"
        <> "    \"severity\": \"high\",\n"
        <> "    \"evidence_type\": \"static\",\n"
        <> "    \"verified\": true,\n"
        <> "    \"blocking\": true,\n"
        <> "    \"locations\": [{ \"path\": \"src/example.gleam\" }],\n"
        <> "    \"summary\": \"Static bug claim\",\n"
        <> "    \"details\": \"No executable evidence was supplied.\",\n"
        <> "    \"suggested_fix\": \"Add a failing test or downgrade the finding.\"\n"
        <> "  }],\n"
        <> "  \"artifacts\": []\n"
        <> "}\n",
    )

  let artifact =
    run_command("scripts/scherzo-review validate --artifact " <> artifact_path)

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(
    artifact.stderr,
    "blocking correctness findings must be verified with executable evidence",
  )
}

pub fn review_artifact_validator_rejects_missing_required_brief_fields_test() {
  let dir = "test/tmp/review-artifacts-invalid"
  reset_dir(dir)
  let artifact_path = dir <> "/invalid.json"
  let assert Ok(Nil) =
    simplifile.write(
      artifact_path,
      "{\"schema_version\":1,\"artifact_type\":\"review_brief\"}\n",
    )

  let artifact =
    run_command("scripts/scherzo-review validate --artifact " <> artifact_path)

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(
    artifact.stderr,
    "artifact field 'generated_at_utc' must be a non-empty string",
  )
}
