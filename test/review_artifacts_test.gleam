import gleam/list
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
        <> "diff --git a/.scherzo/workflows/schemas/example.json b/.scherzo/workflows/schemas/example.json\n"
        <> "new file mode 100644\n"
        <> "index 0000000..3333333\n"
        <> "--- /dev/null\n"
        <> "+++ b/.scherzo/workflows/schemas/example.json\n"
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
  assert string.contains(brief, "\"artifact schema contract\"")
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

pub fn agent_fixture_lane_writes_bundle_artifacts_test() {
  let dir = "test/tmp/review-artifacts-agent-fixture-lane"
  reset_dir(dir)
  let diff_path = dir <> "/change.diff"
  let brief_dir = dir <> "/brief"
  let lane_dir = dir <> "/lane"
  let assert Ok(Nil) =
    simplifile.write(
      diff_path,
      "diff --git a/src/scherzo/agent_fixture_example.gleam b/src/scherzo/agent_fixture_example.gleam\n"
        <> "index 1111111..2222222 100644\n"
        <> "--- a/src/scherzo/agent_fixture_example.gleam\n"
        <> "+++ b/src/scherzo/agent_fixture_example.gleam\n"
        <> "@@ -1,3 +1,4 @@\n"
        <> " pub fn value() {\n"
        <> "+  2\n"
        <> "   1\n"
        <> " }\n",
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
      "scripts/scherzo-review run-lane --lane correctness --brief "
      <> brief_dir
      <> "/review-brief.v1.json --diff-file "
      <> diff_path
      <> " --output-dir "
      <> lane_dir
      <> " --source-label agent-fixture-test --agent-backend fixture",
    )
  assert lane.status == step_artifact.StepSucceeded
  assert lane.exit_code == Some(0)
  assert string.contains(lane.stdout, "REVIEW_AGENT_BACKEND=fixture")

  let lane_result_path = lane_dir <> "/review-lane-correctness.v1.json"
  let assert Ok(lane_result) = simplifile.read(lane_result_path)
  let assert Ok(changed_files) =
    simplifile.read(lane_dir <> "/input/changed-files.v1.json")
  let assert Ok(prompt) = simplifile.read(lane_dir <> "/prompt.md")
  let assert Ok(raw_output) =
    simplifile.read(lane_dir <> "/raw-agent-output.json")
  let assert Ok(log) =
    simplifile.read(lane_dir <> "/review-lane-correctness.log")

  assert string.contains(lane_result, "\"agent_backend\": \"fixture\"")
  assert string.contains(lane_result, "input/diff.patch")
  assert string.contains(lane_result, "raw-agent-output.json")
  assert string.contains(lane_result, "prompt.md")
  assert string.contains(
    changed_files,
    "src/scherzo/agent_fixture_example.gleam",
  )
  assert !string.contains(changed_files, "<absolute-local-path>")
  assert string.contains(prompt, "Inspect the actual unified diff")
  assert string.contains(raw_output, "analysis_summary")
  assert string.contains(log, "agent_backend=fixture")

  let validation =
    run_command(
      "scripts/scherzo-review validate --artifact " <> lane_result_path,
    )
  assert validation.status == step_artifact.StepSucceeded
}

pub fn preflight_fixture_backend_records_lane_backends_test() {
  let dir = "test/tmp/review-artifacts-preflight-fixture-backend"
  reset_dir(dir)

  let preflight =
    run_command(
      "scripts/scherzo-review preflight --agent-backend fixture --scenario no-meaningful-findings-pr --output-dir "
      <> dir,
    )

  assert preflight.status == step_artifact.StepSucceeded
  assert preflight.exit_code == Some(0)
  assert string.contains(preflight.stdout, "REVIEW_AGENT_BACKEND=fixture")

  let manifest_path = dir <> "/preflight-manifest.v1.json"
  let assert Ok(manifest) = simplifile.read(manifest_path)
  assert string.contains(manifest, "\"artifact_type\": \"preflight_manifest\"")
  assert string.contains(manifest, "\"agent_backend\": \"fixture\"")
  assert string.contains(manifest, "\"backend\": \"fixture\"")
  assert string.contains(manifest, "\"lane_id\": \"correctness\"")
  assert string.contains(manifest, "\"remote_mutations\": \"none\"")

  let validation =
    run_command("scripts/scherzo-review validate --artifact " <> manifest_path)
  assert validation.status == step_artifact.StepSucceeded
  assert string.contains(
    validation.stdout,
    "REVIEW_ARTIFACT_TYPE=preflight_manifest",
  )
}

pub fn external_agent_missing_command_writes_failed_lane_result_test() {
  let dir = "test/tmp/review-artifacts-external-missing-command"
  reset_dir(dir)
  let diff_path = dir <> "/change.diff"
  let brief_dir = dir <> "/brief"
  let lane_dir = dir <> "/lane"
  let assert Ok(Nil) =
    simplifile.write(
      diff_path,
      "diff --git a/src/scherzo/external_agent_example.gleam b/src/scherzo/external_agent_example.gleam\n"
        <> "index 1111111..2222222 100644\n"
        <> "--- a/src/scherzo/external_agent_example.gleam\n"
        <> "+++ b/src/scherzo/external_agent_example.gleam\n"
        <> "@@ -1,3 +1,4 @@\n"
        <> " pub fn value() {\n"
        <> "+  2\n"
        <> "   1\n"
        <> " }\n",
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
      "env -u SCHERZO_REVIEW_AGENT_COMMAND scripts/scherzo-review run-lane --lane correctness --brief "
      <> brief_dir
      <> "/review-brief.v1.json --diff-file "
      <> diff_path
      <> " --output-dir "
      <> lane_dir
      <> " --agent-backend external",
    )
  assert lane.status == step_artifact.StepFailed
  assert lane.exit_code == Some(1)
  assert string.contains(lane.stdout, "REVIEW_LANE_RUN=failed")
  assert string.contains(lane.stdout, "REVIEW_AGENT_BACKEND=external")
  assert string.contains(lane.stderr, "missing external backend configuration")

  let lane_result_path = lane_dir <> "/review-lane-correctness.v1.json"
  let assert Ok(lane_result) = simplifile.read(lane_result_path)
  assert string.contains(lane_result, "\"state\": \"failed\"")
  assert string.contains(lane_result, "\"agent_backend\": \"external\"")
  assert string.contains(lane_result, "missing external backend configuration")
  assert string.contains(lane_result, "input/diff.patch")

  let validation =
    run_command(
      "scripts/scherzo-review validate --artifact " <> lane_result_path,
    )
  assert validation.status == step_artifact.StepSucceeded
}

pub fn external_agent_command_writes_successful_lane_result_test() {
  let dir = "test/tmp/review-artifacts-external-success"
  reset_dir(dir)
  let diff_path = dir <> "/change.diff"
  let brief_dir = dir <> "/brief"
  let lane_dir = dir <> "/lane"
  let agent_path = dir <> "/external_agent.py"
  let assert Ok(Nil) =
    simplifile.write(
      diff_path,
      "diff --git a/src/scherzo/external_agent_success.gleam b/src/scherzo/external_agent_success.gleam\n"
        <> "index 1111111..2222222 100644\n"
        <> "--- a/src/scherzo/external_agent_success.gleam\n"
        <> "+++ b/src/scherzo/external_agent_success.gleam\n"
        <> "@@ -1,3 +1,4 @@\n"
        <> " pub fn value() {\n"
        <> "+  2\n"
        <> "   1\n"
        <> " }\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      agent_path,
      "import json\n"
        <> "import sys\n"
        <> "from pathlib import Path\n"
        <> "raw_output = Path(sys.argv[1])\n"
        <> "raw_output.write_text(json.dumps({\n"
        <> "  'lane_id': 'test-quality',\n"
        <> "  'analysis_summary': 'External agent completed successfully.',\n"
        <> "  'findings': [],\n"
        <> "  'review_notes': [{\n"
        <> "    'kind': 'coverage_note',\n"
        <> "    'category': 'testing',\n"
        <> "    'severity': 'info',\n"
        <> "    'locations': [{'path': 'src/scherzo/external_agent_success.gleam'}],\n"
        <> "    'summary': 'External agent produced a retained note.',\n"
        <> "    'details': 'The successful external backend path wrote JSON output.',\n"
        <> "    'suggested_action': 'Keep the external command contract documented.'\n"
        <> "  }],\n"
        <> "  'evidence_requests': []\n"
        <> "}, indent=2))\n"
        <> "print('EXTERNAL_AGENT_OK')\n",
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
      "SCHERZO_REVIEW_AGENT_COMMAND='python3 "
      <> agent_path
      <> " {raw_output_path}' scripts/scherzo-review run-lane --lane test-quality --brief "
      <> brief_dir
      <> "/review-brief.v1.json --diff-file "
      <> diff_path
      <> " --output-dir "
      <> lane_dir
      <> " --agent-backend external",
    )
  assert lane.status == step_artifact.StepSucceeded
  assert lane.exit_code == Some(0)
  assert string.contains(lane.stdout, "REVIEW_AGENT_BACKEND=external")
  assert string.contains(lane.stdout, "REVIEW_LANE_REVIEW_NOTES=1")

  let lane_result_path = lane_dir <> "/review-lane-test-quality.v1.json"
  let assert Ok(lane_result) = simplifile.read(lane_result_path)
  let assert Ok(transcript) =
    simplifile.read(lane_dir <> "/transcript.stdout.txt")
  assert string.contains(lane_result, "\"agent_backend\": \"external\"")
  assert string.contains(
    lane_result,
    "External agent produced a retained note.",
  )
  assert string.contains(lane_result, "transcript.stdout.txt")
  assert string.contains(transcript, "EXTERNAL_AGENT_OK")

  let validation =
    run_command(
      "scripts/scherzo-review validate --artifact " <> lane_result_path,
    )
  assert validation.status == step_artifact.StepSucceeded
}

pub fn agent_environment_sanitizer_strips_mutation_credentials_test() {
  let command =
    "PYTHONPATH=scripts python3 -c 'from scherzo_review.agent_lane_harness import sanitize_agent_environment; env={\"PATH\":\"/bin\",\"GITHUB_TOKEN\":\"gh\",\"GH_TOKEN\":\"gh\",\"LINEAR_API_KEY\":\"lin\",\"SCHERZO_AGENT_LINEAR_API_KEY\":\"lin\",\"SSH_AUTH_SOCK\":\"sock\",\"SCHERZO_REVIEW_AGENT_READONLY_FLAG\":\"1\"}; out=sanitize_agent_environment(env); assert out[\"PATH\"] == \"/bin\"; assert out[\"SCHERZO_REVIEW_AGENT_READONLY_FLAG\"] == \"1\"; assert \"GITHUB_TOKEN\" not in out; assert \"GH_TOKEN\" not in out; assert \"LINEAR_API_KEY\" not in out; assert \"SCHERZO_AGENT_LINEAR_API_KEY\" not in out; assert \"SSH_AUTH_SOCK\" not in out; print(\"SANITIZER_OK\")'"
  let result = run_command(command)
  assert result.status == step_artifact.StepSucceeded
  assert result.exit_code == Some(0)
  assert string.contains(result.stdout, "SANITIZER_OK")
}

pub fn correctness_fixture_evidence_gate_preflight_test() {
  let dir = "test/tmp/review-artifacts-correctness-fixture"
  reset_dir(dir)

  let preflight =
    run_command(
      "scripts/scherzo-review preflight --agent-backend fixture --scenario inverted-auth-control-condition --scenario auth-control-static-suspicion-without-repro --output-dir "
      <> dir,
    )
  assert preflight.status == step_artifact.StepSucceeded
  assert preflight.exit_code == Some(0)

  let inverted_lane =
    dir
    <> "/inverted-auth-control-condition/02-lane-correctness/review-lane-correctness.v1.json"
  let inverted_ledger =
    dir
    <> "/inverted-auth-control-condition/02-lane-correctness/evidence-ledger.v1.json"
  let inverted_stdout =
    dir
    <> "/inverted-auth-control-condition/02-lane-correctness/repro/inverted_auth_repro.stdout.txt"
  let assert Ok(inverted_result) = simplifile.read(inverted_lane)
  let assert Ok(ledger) = simplifile.read(inverted_ledger)
  let assert Ok(stdout) = simplifile.read(inverted_stdout)
  assert string.contains(inverted_result, "\"blocking\": true")
  assert string.contains(inverted_result, "\"verified\": true")
  assert string.contains(inverted_result, "\"evidence_type\": \"reproduction\"")
  assert string.contains(inverted_result, "\"evidence_id\"")
  assert string.contains(
    inverted_result,
    "src/liv_152_fixture/project_authorization.gleam",
  )
  assert string.contains(
    ledger,
    "python3 repro/inverted_auth_control_condition_repro.py",
  )
  assert string.contains(ledger, "\"exit_code\": 0")
  assert string.contains(
    stdout,
    "REPRODUCED: unauthorized User received Ok(\"deleted\")",
  )

  let static_lane =
    dir
    <> "/auth-control-static-suspicion-without-repro/02-lane-correctness/review-lane-correctness.v1.json"
  let assert Ok(static_result) = simplifile.read(static_lane)
  assert string.contains(static_result, "\"findings\": []")
  assert string.contains(static_result, "\"review_notes\"")
  assert string.contains(
    static_result,
    "downgraded_unverified_correctness_claim",
  )
  assert string.contains(static_result, "executable")

  let readiness =
    run_command(
      "scripts/scherzo-review validate --artifact "
      <> dir
      <> "/preflight-manifest.v1.json --require-cutover-ready",
    )
  assert readiness.status == step_artifact.StepSucceeded
  assert string.contains(readiness.stdout, "REVIEW_CUTOVER_READY=ok")
}

pub fn heuristic_preflight_is_not_cutover_ready_test() {
  let dir = "test/tmp/review-artifacts-heuristic-not-ready"
  reset_dir(dir)

  let preflight =
    run_command(
      "scripts/scherzo-review preflight --scenario no-meaningful-findings-pr --output-dir "
      <> dir,
    )
  assert preflight.status == step_artifact.StepSucceeded

  let readiness =
    run_command(
      "scripts/scherzo-review validate --artifact "
      <> dir
      <> "/preflight-manifest.v1.json --require-cutover-ready",
    )
  assert readiness.status == step_artifact.StepFailed
  assert readiness.exit_code == Some(1)
  assert string.contains(readiness.stdout, "REVIEW_CUTOVER_READY=failed")
  assert string.contains(
    readiness.stderr,
    "agent_backend must be fixture or external",
  )
}

pub fn implementation_workflows_native_cutover_removes_legacy_backend_default_test() {
  let workflow_paths = [
    ".scherzo/workflows/implementation.yaml",
    ".scherzo/workflows/execplan-implementation.yaml",
  ]

  list.each(workflow_paths, fn(path) {
    let assert Ok(workflow) = simplifile.read(path)
    assert_contains(workflow, "submit_structured_output")
    assert_contains(workflow, "prepare-native")
    assert_contains(workflow, "refuses fixture/scenario/heuristic")
    assert_not_contains(workflow, "run-lane --lane")
    assert_not_contains(workflow, "--agent-backend")
    assert_not_contains(
      workflow,
      "SCHERZO_STAGED_REVIEW_AGENT_BACKEND:-heuristic",
    )
  })
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

fn review_lane_draft_json(
  location_path: String,
  remote_mutations: String,
) -> String {
  "{\n"
  <> "  \"schema_version\": 1,\n"
  <> "  \"artifact_type\": \"review_lane_draft\",\n"
  <> "  \"generated_at_utc\": \"2026-05-09T00:00:00Z\",\n"
  <> "  \"producer\": { \"name\": \"test\", \"version\": \"1\", \"mode\": \"native\" },\n"
  <> "  \"lane\": { \"id\": \"correctness\", \"name\": \"Correctness reviewer\", \"category\": \"correctness\", \"version\": \"1\" },\n"
  <> "  \"input_refs\": [{ \"artifact_type\": \"review_brief\", \"path\": \"review-brief.v1.json\" }],\n"
  <> "  \"draft_findings\": [{\n"
  <> "    \"draft_finding_id\": \"F1\",\n"
  <> "    \"title\": \"Draft finding\",\n"
  <> "    \"claim\": \"A claim that needs deterministic evidence.\",\n"
  <> "    \"category\": \"correctness\",\n"
  <> "    \"severity\": \"high\",\n"
  <> "    \"proposed_blocking\": true,\n"
  <> "    \"locations\": [{ \"path\": \""
  <> location_path
  <> "\" }],\n"
  <> "    \"evidence_request_ids\": [\"E1\"],\n"
  <> "    \"suggested_fix\": \"Add a reproduction.\"\n"
  <> "  }],\n"
  <> "  \"review_notes\": [],\n"
  <> "  \"evidence_requests\": [{\n"
  <> "    \"request_id\": \"E1\",\n"
  <> "    \"draft_finding_id\": \"F1\",\n"
  <> "    \"evidence_key\": \"gleam_test\",\n"
  <> "    \"claim\": \"A claim that needs deterministic evidence.\",\n"
  <> "    \"expected_observation\": \"targeted test fails before the fix\",\n"
  <> "    \"target\": {}\n"
  <> "  }],\n"
  <> "  \"self_check\": { \"inspected_diff\": true, \"used_repository_relative_paths\": true },\n"
  <> "  \"remote_mutations\": \""
  <> remote_mutations
  <> "\"\n"
  <> "}\n"
}

fn write_native_brief(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"artifact_type\": \"review_brief\",\n"
        <> "  \"generated_at_utc\": \"2026-05-09T00:00:00Z\",\n"
        <> "  \"producer\": { \"name\": \"test\", \"version\": \"1\", \"mode\": \"test\" },\n"
        <> "  \"source\": { \"kind\": \"diff_file\", \"label\": \"test\", \"diff_sha256\": \"0000000000000000000000000000000000000000000000000000000000000000\", \"changed_file_count\": 1 },\n"
        <> "  \"implementation_summary\": \"Test summary\",\n"
        <> "  \"changed_areas\": [{ \"path\": \"src/example.gleam\", \"subsystem\": \"tests\", \"language\": \"gleam\", \"change_kind\": \"modified\", \"additions\": 1, \"deletions\": 0, \"hunks\": 1 }],\n"
        <> "  \"inferred_acceptance_criteria\": [\"works\"],\n"
        <> "  \"risk_profile\": { \"level\": \"medium\", \"rationale\": \"test\", \"risk_areas\": [\"tests\"] },\n"
        <> "  \"suggested_review_lanes\": [{ \"id\": \"correctness\", \"name\": \"Correctness reviewer\", \"reason\": \"test\" }],\n"
        <> "  \"test_build_status\": [],\n"
        <> "  \"notes\": []\n"
        <> "}\n",
    )
  Nil
}

fn write_native_support_files(dir: String) -> Nil {
  write_native_brief(dir <> "/review-brief.v1.json")
  let assert Ok(Nil) = simplifile.write(dir <> "/diff.patch", "")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/changed-files.v1.json",
      "{\"schema_version\":1,\"artifact_type\":\"changed_files\",\"files\":[]}",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/validation-status.v1.json",
      "{\"schema_version\":1,\"artifact_type\":\"validation_status\",\"test_build_status\":[]}",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/context-manifest.v1.json",
      "{\"schema_version\":1,\"artifact_type\":\"context_manifest\",\"files\":[]}",
    )
  Nil
}

fn write_metadata(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "{\"schema_version\":1,\"artifact_type\":\"agent_step_metadata\",\"state\":\"succeeded\",\"remote_mutations\":\"none\"}\n",
    )
  Nil
}

fn write_structured_output_error_metadata(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "{\n"
        <> "  \"step_id\": \"lane_correctness\",\n"
        <> "  \"status\": \"failure\",\n"
        <> "  \"failure_code\": \"structured_output_json_schema_rejected\",\n"
        <> "  \"structured_output\": {\n"
        <> "    \"status\": \"error\",\n"
        <> "    \"artifact_name\": \"correctness_draft\",\n"
        <> "    \"format\": \"json\",\n"
        <> "    \"error\": \"JSON Schema rejected payload at /input_refs/0/path: absolute paths are not allowed\",\n"
        <> "    \"failure\": {\n"
        <> "      \"code\": \"structured_output_json_schema_rejected\",\n"
        <> "      \"retryable\": true,\n"
        <> "      \"validator_name\": \"review_lane_draft_schema\",\n"
        <> "      \"validator_type\": \"json_schema\",\n"
        <> "      \"diagnostic_summary\": \"instance_path=/input_refs/0/path schema_path=not/anyOf\"\n"
        <> "    },\n"
        <> "    \"retry\": {\n"
        <> "      \"max_retries\": 1,\n"
        <> "      \"attempts\": 2,\n"
        <> "      \"outcome\": \"failed\",\n"
        <> "      \"diagnostics\": [{\n"
        <> "        \"attempt\": 1,\n"
        <> "        \"status\": \"validator_failure\",\n"
        <> "        \"failure_code\": \"structured_output_json_schema_rejected\",\n"
        <> "        \"message\": \"input_refs[0].path used /Users/example/run instead of artifacts/review/prepare_review/diff.patch\"\n"
        <> "      }]\n"
        <> "    }\n"
        <> "  }\n"
        <> "}\n",
    )
  Nil
}

fn write_wrong_finding_ledger(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"artifact_type\": \"review_evidence_ledger\",\n"
        <> "  \"generated_at_utc\": \"2026-05-09T00:00:00Z\",\n"
        <> "  \"lane_id\": \"correctness\",\n"
        <> "  \"draft_ref\": { \"artifact_type\": \"review_lane_draft\", \"path\": \"draft.v1.json\" },\n"
        <> "  \"checks\": [{ \"check_id\": \"C1\", \"request_id\": \"E1\", \"draft_finding_id\": \"F2\", \"evidence_key\": \"fixture_reproduction\", \"command\": \"fixed\", \"exit_status\": 0, \"output_excerpt\": \"verified\", \"remote_mutations\": \"none\" }],\n"
        <> "  \"verdicts\": [{ \"request_id\": \"E1\", \"draft_finding_id\": \"F2\", \"verdict\": \"verified\", \"evidence_type\": \"reproduction\", \"claim_supported\": \"other\", \"matched_expected_observation\": true, \"check_id\": \"C1\" }],\n"
        <> "  \"execution_status\": { \"state\": \"succeeded\", \"summary\": \"wrong finding\" },\n"
        <> "  \"remote_mutations\": \"none\"\n"
        <> "}\n",
    )
  Nil
}

pub fn review_lane_draft_path_safety_validation_test() {
  let dir = "test/tmp/review-lane-draft-path-safety"
  reset_dir(dir)
  let absolute_path = "/tmp/scherzo-review-absolute-path-fixture.gleam"
  let absolute_draft = dir <> "/absolute.json"
  let parent_draft = dir <> "/parent.json"
  let env_input_ref_draft = dir <> "/env-input-ref.json"
  let remote_draft = dir <> "/remote.json"
  let assert Ok(Nil) =
    simplifile.write(
      absolute_draft,
      review_lane_draft_json(absolute_path, "none"),
    )
  let assert Ok(Nil) =
    simplifile.write(
      parent_draft,
      review_lane_draft_json("../secret.txt", "none"),
    )
  let env_input_ref_contents =
    review_lane_draft_json("src/example.gleam", "none")
    |> string.replace(
      each: "\"path\": \"review-brief.v1.json\"",
      with: "\"path\": \"$SCHERZO_RUN_ROOT/artifacts/review/prepare_review/review-brief.v1.json\"",
    )
  let assert Ok(Nil) =
    simplifile.write(env_input_ref_draft, env_input_ref_contents)
  let assert Ok(Nil) =
    simplifile.write(
      remote_draft,
      review_lane_draft_json("src/example.gleam", "github"),
    )

  let absolute =
    run_command("scripts/scherzo-review validate --artifact " <> absolute_draft)
  assert absolute.status == step_artifact.StepFailed
  assert absolute.exit_code == Some(1)

  let parent =
    run_command("scripts/scherzo-review validate --artifact " <> parent_draft)
  assert parent.status == step_artifact.StepFailed
  assert parent.exit_code == Some(1)

  let env_input_ref =
    run_command(
      "scripts/scherzo-review validate --artifact " <> env_input_ref_draft,
    )
  assert env_input_ref.status == step_artifact.StepFailed
  assert env_input_ref.exit_code == Some(1)
  assert string.contains(env_input_ref.stderr, "input_refs[0].path")
  assert string.contains(env_input_ref.stderr, "environment-variable")

  let remote =
    run_command("scripts/scherzo-review validate --artifact " <> remote_draft)
  assert remote.status == step_artifact.StepFailed
  assert remote.exit_code == Some(1)
}

pub fn evidence_verdict_must_link_to_finding_test() {
  let dir = "test/tmp/native-evidence-linkage"
  reset_dir(dir)
  write_native_support_files(dir)
  let draft_path = dir <> "/draft.v1.json"
  let ledger_path = dir <> "/evidence-ledger.v1.json"
  let metadata_path = dir <> "/agent-step-metadata.v1.json"
  let lane_dir = dir <> "/lane"
  let assert Ok(Nil) =
    simplifile.write(
      draft_path,
      review_lane_draft_json("src/example.gleam", "none"),
    )
  write_wrong_finding_ledger(ledger_path)
  write_metadata(metadata_path)

  let normalized =
    run_command(
      "scripts/scherzo-review normalize-lane-result --lane correctness --draft "
      <> draft_path
      <> " --evidence-ledger "
      <> ledger_path
      <> " --agent-step-metadata "
      <> metadata_path
      <> " --brief "
      <> dir
      <> "/review-brief.v1.json --output-dir "
      <> lane_dir,
    )
  assert normalized.status == step_artifact.StepSucceeded
  let assert Ok(result) =
    simplifile.read(lane_dir <> "/review-lane-correctness.v1.json")
  assert string.contains(result, "\"verified\": false")
  assert string.contains(result, "\"blocking\": false")
  assert string.contains(result, "downgraded_unproven_correctness_claim")
}

pub fn generic_gleam_test_does_not_verify_arbitrary_correctness_claim_test() {
  let dir = "test/tmp/native-generic-gleam-test"
  reset_dir(dir)
  write_native_support_files(dir)
  let draft_path = dir <> "/draft.v1.json"
  let metadata_path = dir <> "/agent-step-metadata.v1.json"
  let lane_dir = dir <> "/lane"
  let assert Ok(Nil) =
    simplifile.write(
      draft_path,
      review_lane_draft_json("src/example.gleam", "none"),
    )
  write_metadata(metadata_path)

  let verify =
    run_command(
      "scripts/scherzo-review verify-evidence --lane correctness --draft "
      <> draft_path
      <> " --brief "
      <> dir
      <> "/review-brief.v1.json --diff-file "
      <> dir
      <> "/diff.patch --changed-files "
      <> dir
      <> "/changed-files.v1.json --validation-status "
      <> dir
      <> "/validation-status.v1.json --context-manifest "
      <> dir
      <> "/context-manifest.v1.json --output-dir "
      <> lane_dir,
    )
  assert verify.status == step_artifact.StepSucceeded
  let assert Ok(ledger) =
    simplifile.read(lane_dir <> "/evidence-ledger.v1.json")
  assert string.contains(ledger, "\"verdict\": \"context_only\"")

  let normalized =
    run_command(
      "scripts/scherzo-review normalize-lane-result --lane correctness --draft "
      <> draft_path
      <> " --evidence-ledger "
      <> lane_dir
      <> "/evidence-ledger.v1.json --agent-step-metadata "
      <> metadata_path
      <> " --brief "
      <> dir
      <> "/review-brief.v1.json --output-dir "
      <> lane_dir,
    )
  assert normalized.status == step_artifact.StepSucceeded
  let assert Ok(result) =
    simplifile.read(lane_dir <> "/review-lane-correctness.v1.json")
  assert string.contains(result, "\"verified\": false")
  assert string.contains(result, "\"blocking\": false")
}

pub fn verify_evidence_relativizes_absolute_draft_path_test() {
  let dir = "test/tmp/native-absolute-draft-path"
  reset_dir(dir)
  write_native_support_files(dir)
  let draft_path = dir <> "/draft.v1.json"
  let lane_dir = dir <> "/lane"
  let assert Ok(Nil) =
    simplifile.write(
      draft_path,
      review_lane_draft_json("src/example.gleam", "none"),
    )
  let assert Ok(cwd) = simplifile.current_directory()
  let absolute_draft_path = cwd <> "/" <> draft_path

  let verify =
    run_command(
      "scripts/scherzo-review verify-evidence --lane correctness --draft "
      <> absolute_draft_path
      <> " --brief "
      <> dir
      <> "/review-brief.v1.json --diff-file "
      <> dir
      <> "/diff.patch --changed-files "
      <> dir
      <> "/changed-files.v1.json --validation-status "
      <> dir
      <> "/validation-status.v1.json --context-manifest "
      <> dir
      <> "/context-manifest.v1.json --output-dir "
      <> lane_dir,
    )
  assert verify.status == step_artifact.StepSucceeded
  assert verify.exit_code == Some(0)
  let assert Ok(ledger) =
    simplifile.read(lane_dir <> "/evidence-ledger.v1.json")
  assert_not_contains(ledger, cwd)
  assert_contains(ledger, "\"path\": \"" <> draft_path <> "\"")
}

pub fn correctness_blocker_downgraded_without_verified_reproduction_test() {
  let dir = "test/tmp/native-correctness-downgrade"
  reset_dir(dir)
  write_native_support_files(dir)
  let draft_path = dir <> "/draft.v1.json"
  let ledger_path = dir <> "/evidence-ledger.v1.json"
  let metadata_path = dir <> "/agent-step-metadata.v1.json"
  let lane_dir = dir <> "/lane"
  let assert Ok(Nil) =
    simplifile.write(
      draft_path,
      review_lane_draft_json("src/example.gleam", "none"),
    )
  let assert Ok(Nil) =
    simplifile.write(
      ledger_path,
      "{\"schema_version\":1,\"artifact_type\":\"review_evidence_ledger\",\"generated_at_utc\":\"2026-05-09T00:00:00Z\",\"lane_id\":\"correctness\",\"draft_ref\":{\"artifact_type\":\"review_lane_draft\",\"path\":\"draft.v1.json\"},\"checks\":[],\"verdicts\":[],\"execution_status\":{\"state\":\"succeeded\",\"summary\":\"none\"},\"remote_mutations\":\"none\"}\n",
    )
  write_metadata(metadata_path)

  let normalized =
    run_command(
      "scripts/scherzo-review normalize-lane-result --lane correctness --draft "
      <> draft_path
      <> " --evidence-ledger "
      <> ledger_path
      <> " --agent-step-metadata "
      <> metadata_path
      <> " --brief "
      <> dir
      <> "/review-brief.v1.json --output-dir "
      <> lane_dir,
    )
  assert normalized.status == step_artifact.StepSucceeded
  let assert Ok(result) =
    simplifile.read(lane_dir <> "/review-lane-correctness.v1.json")
  assert string.contains(result, "\"blocking\": false")
  assert string.contains(result, "\"verified\": false")
  assert string.contains(result, "downgraded_unproven_correctness_claim")
}

pub fn missing_evidence_ledger_produces_failed_lane_result_test() {
  let dir = "test/tmp/native-missing-evidence-ledger"
  reset_dir(dir)
  write_native_support_files(dir)
  let draft_path = dir <> "/draft.v1.json"
  let metadata_path = dir <> "/agent-step-metadata.v1.json"
  let lane_dir = dir <> "/lane"
  let assert Ok(Nil) =
    simplifile.write(
      draft_path,
      review_lane_draft_json("src/example.gleam", "none"),
    )
  write_metadata(metadata_path)

  let normalized =
    run_command(
      "scripts/scherzo-review normalize-lane-result --lane correctness --draft "
      <> draft_path
      <> " --evidence-ledger "
      <> dir
      <> "/missing-ledger.json --agent-step-metadata "
      <> metadata_path
      <> " --brief "
      <> dir
      <> "/review-brief.v1.json --output-dir "
      <> lane_dir,
    )
  assert normalized.status == step_artifact.StepSucceeded
  assert string.contains(normalized.stdout, "REVIEW_LANE_STATE=failed")
  let assert Ok(result) =
    simplifile.read(lane_dir <> "/review-lane-correctness.v1.json")
  assert string.contains(result, "\"state\": \"failed\"")
  assert string.contains(result, "evidence verification failed")
  assert string.contains(result, "missing-ledger.json")
}

pub fn missing_draft_verify_evidence_reports_structured_output_root_cause_test() {
  let dir = "test/tmp/native-missing-draft-root-cause-verify"
  reset_dir(dir)
  write_native_support_files(dir)
  let artifact_dir = dir <> "/artifact-runs/run-1"
  let draft_path =
    artifact_dir
    <> "/lane_correctness/attempt-1/structured/correctness_draft.json"
  let metadata_dir = artifact_dir <> "/lane_correctness-abc123def456"
  let metadata_path = metadata_dir <> "/attempt-1.json"
  let lane_dir = dir <> "/lane"
  let assert Ok(Nil) = simplifile.create_directory_all(metadata_dir)
  write_structured_output_error_metadata(metadata_path)

  let verify =
    run_command(
      "scripts/scherzo-review verify-evidence --lane correctness --draft "
      <> draft_path
      <> " --brief "
      <> dir
      <> "/review-brief.v1.json --diff-file "
      <> dir
      <> "/diff.patch --changed-files "
      <> dir
      <> "/changed-files.v1.json --validation-status "
      <> dir
      <> "/validation-status.v1.json --context-manifest "
      <> dir
      <> "/context-manifest.v1.json --output-dir "
      <> lane_dir,
    )

  assert verify.status == step_artifact.StepFailed
  assert verify.exit_code == Some(1)
  assert_contains(verify.stderr, "structured_output_json_schema_rejected")
  assert_contains(verify.stderr, "review_lane_draft_schema")
  assert_contains(verify.stderr, "input_refs[0].path")
  assert_contains(verify.stderr, "draft_artifact_error")
}

pub fn missing_draft_normalize_preserves_structured_output_root_cause_test() {
  let dir = "test/tmp/native-missing-draft-root-cause-normalize"
  reset_dir(dir)
  write_native_support_files(dir)
  let artifact_dir = dir <> "/artifact-runs/run-1"
  let draft_path =
    artifact_dir
    <> "/lane_correctness/attempt-1/structured/correctness_draft.json"
  let metadata_dir = artifact_dir <> "/lane_correctness-abc123def456"
  let metadata_path = metadata_dir <> "/attempt-1.json"
  let lane_dir = dir <> "/lane"
  let assert Ok(Nil) = simplifile.create_directory_all(metadata_dir)
  write_structured_output_error_metadata(metadata_path)

  let normalized =
    run_command(
      "scripts/scherzo-review normalize-lane-result --lane correctness --draft "
      <> draft_path
      <> " --evidence-ledger "
      <> dir
      <> "/missing-ledger.json --agent-step-metadata "
      <> artifact_dir
      <> "/lane_correctness/attempt-1.json --brief "
      <> dir
      <> "/review-brief.v1.json --output-dir "
      <> lane_dir,
    )
  assert normalized.status == step_artifact.StepSucceeded
  assert string.contains(normalized.stdout, "REVIEW_LANE_STATE=failed")
  let assert Ok(result) =
    simplifile.read(lane_dir <> "/review-lane-correctness.v1.json")
  assert_contains(result, "structured_output_json_schema_rejected")
  assert_contains(result, "review_lane_draft_schema")
  assert_contains(result, "input_refs[0].path")
  assert_contains(result, "draft_artifact_error")
}

pub fn missing_or_malformed_draft_produces_failed_lane_result_test() {
  let dir = "test/tmp/native-malformed-draft"
  reset_dir(dir)
  write_native_support_files(dir)
  let draft_path = dir <> "/draft.v1.json"
  let metadata_path = dir <> "/agent-step-metadata.v1.json"
  let lane_dir = dir <> "/lane"
  let assert Ok(Nil) = simplifile.write(draft_path, "{ this is not json\n")
  write_metadata(metadata_path)

  let normalized =
    run_command(
      "scripts/scherzo-review normalize-lane-result --lane correctness --draft "
      <> draft_path
      <> " --evidence-ledger "
      <> dir
      <> "/missing-ledger.json --agent-step-metadata "
      <> metadata_path
      <> " --brief "
      <> dir
      <> "/review-brief.v1.json --output-dir "
      <> lane_dir,
    )
  assert normalized.status == step_artifact.StepSucceeded
  let assert Ok(result) =
    simplifile.read(lane_dir <> "/review-lane-correctness.v1.json")
  assert string.contains(result, "\"state\": \"failed\"")
  assert string.contains(result, "\"findings\": []")
}

fn final_review_json() -> String {
  "{\n"
  <> "  \"schema_version\": 1,\n"
  <> "  \"artifact_type\": \"final_review\",\n"
  <> "  \"generated_at_utc\": \"2026-05-09T00:00:00Z\",\n"
  <> "  \"producer\": { \"name\": \"test\", \"version\": \"1\", \"mode\": \"test\" },\n"
  <> "  \"brief_ref\": { \"artifact_type\": \"review_brief\", \"path\": \"review-brief.v1.json\" },\n"
  <> "  \"synthesis_ref\": { \"artifact_type\": \"review_synthesis\", \"path\": \"review-synthesis.v1.json\" },\n"
  <> "  \"title\": \"Final review\",\n"
  <> "  \"summary\": \"No findings.\",\n"
  <> "  \"finding_counts\": { \"total\": 0, \"blocking\": 0, \"by_severity\": {}, \"by_category\": {} },\n"
  <> "  \"grouped_findings\": {},\n"
  <> "  \"blockers\": [],\n"
  <> "  \"lane_statuses\": [],\n"
  <> "  \"execution_issues\": [],\n"
  <> "  \"markdown\": \"# Final review\\nNo findings.\",\n"
  <> "  \"remote_mutations\": \"none\"\n"
  <> "}\n"
}

pub fn publish_and_feedback_manifests_are_schema_valid_and_local_only_test() {
  let dir = "test/tmp/native-publish-feedback"
  reset_dir(dir)
  let final_path = dir <> "/final-review.v1.json"
  let feedback_dir = dir <> "/feedback"
  let publish_dir = dir <> "/publish"
  let assert Ok(Nil) = simplifile.write(final_path, final_review_json())

  let feedback =
    run_command(
      "scripts/scherzo-review apply-feedback --final-review "
      <> final_path
      <> " --output-dir "
      <> feedback_dir,
    )
  assert feedback.status == step_artifact.StepSucceeded
  let feedback_manifest = feedback_dir <> "/feedback-manifest.v1.json"
  let feedback_validation =
    run_command(
      "scripts/scherzo-review validate --artifact " <> feedback_manifest,
    )
  assert feedback_validation.status == step_artifact.StepSucceeded
  let assert Ok(feedback_json) = simplifile.read(feedback_manifest)
  assert string.contains(feedback_json, "\"remote_mutations\": \"none\"")
  assert string.contains(feedback_json, "\"actions\": []")

  let publish =
    run_command(
      "scripts/scherzo-review publish --final-review "
      <> final_path
      <> " --mode dry-run --output-dir "
      <> publish_dir,
    )
  assert publish.status == step_artifact.StepSucceeded
  let publish_manifest = publish_dir <> "/publish-manifest.v1.json"
  let publish_validation =
    run_command(
      "scripts/scherzo-review validate --artifact " <> publish_manifest,
    )
  assert publish_validation.status == step_artifact.StepSucceeded
  let assert Ok(publish_json) = simplifile.read(publish_manifest)
  assert string.contains(publish_json, "\"mode\": \"dry-run\"")
  assert string.contains(publish_json, "\"remote_mutations\": \"none\"")

  let invalid_publish =
    run_command(
      "scripts/scherzo-review publish --final-review "
      <> final_path
      <> " --mode live --output-dir "
      <> dir
      <> "/invalid-publish",
    )
  assert invalid_publish.status == step_artifact.StepFailed
}

pub fn legacy_pr_smoke_lists_curated_prs_with_rationale_test() {
  let artifact =
    run_command("scripts/scherzo-review legacy-pr-smoke --list-curated")

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "LEGACY_PR_SMOKE_CURATED_SET=")
  assert string.contains(artifact.stdout, "\"pr\": 116")
  assert string.contains(artifact.stdout, "small_clean")
  assert string.contains(
    artifact.stdout,
    "workflow_control_plane_orchestration",
  )
  assert string.contains(artifact.stdout, "test_heavy_artifact_schema")
  assert string.contains(
    artifact.stdout,
    "security_performance_correctness_sensitive",
  )
  assert string.contains(artifact.stdout, "rationale")
}

pub fn legacy_pr_smoke_rejects_scenario_environment_test() {
  let dir = "test/tmp/legacy-pr-smoke-rejects-env"
  reset_dir(dir)
  let artifact =
    run_command(
      "SCHERZO_NATIVE_REVIEW_SCENARIO=fixture scripts/scherzo-review legacy-pr-smoke --pr 116 --output-dir "
      <> dir,
    )

  assert artifact.status == step_artifact.StepFailed
  assert string.contains(
    artifact.stderr,
    "legacy-pr-smoke refuses fixture/scenario/heuristic override environment variables",
  )
  assert string.contains(artifact.stderr, "SCHERZO_NATIVE_REVIEW_SCENARIO")
}

pub fn native_preflight_requires_runner_provenance_test() {
  let dir = "test/tmp/native-preflight-provenance"
  reset_dir(dir)
  let manifest_path = dir <> "/preflight-manifest.v1.json"
  let assert Ok(Nil) =
    simplifile.write(
      manifest_path,
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"artifact_type\": \"preflight_manifest\",\n"
        <> "  \"generated_at_utc\": \"2026-05-09T00:00:00Z\",\n"
        <> "  \"started_at_utc\": \"2026-05-09T00:00:00Z\",\n"
        <> "  \"status\": \"passed\",\n"
        <> "  \"agent_backend\": \"native\",\n"
        <> "  \"execution_mode\": \"native\",\n"
        <> "  \"remote_mutations\": \"none\",\n"
        <> "  \"coverage\": [\"native\"],\n"
        <> "  \"scenario_count\": 1,\n"
        <> "  \"passed_scenario_count\": 1,\n"
        <> "  \"failed_scenario_count\": 0,\n"
        <> "  \"lane_runs\": [],\n"
        <> "  \"scenarios\": [],\n"
        <> "  \"cutover_readiness\": { \"ready\": true }\n"
        <> "}\n",
    )

  let validation =
    run_command("scripts/scherzo-review validate --artifact " <> manifest_path)
  assert validation.status == step_artifact.StepFailed
  assert string.contains(
    validation.stderr,
    "native preflight manifest must record native lane step provenance",
  )
}
