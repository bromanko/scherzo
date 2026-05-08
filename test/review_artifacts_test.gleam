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
  command_step.run("review-artifacts", command, ".", 10_000, [], limits())
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
