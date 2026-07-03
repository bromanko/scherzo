import gleam/list
import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/step_artifact
import simplifile
import support/test_helpers

fn run_command(command: String) -> step_artifact.StepArtifact {
  command_step.run(
    "review-artifacts",
    command,
    ".",
    120_000,
    [],
    test_helpers.default_artifact_limits(),
  )
}

fn run_command_with_env(
  command: String,
  env: List(#(String, String)),
) -> step_artifact.StepArtifact {
  command_step.run_with_env(
    "review-artifacts",
    command,
    ".",
    120_000,
    env,
    [],
    test_helpers.default_artifact_limits(),
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

fn native_prepare_diff() -> String {
  "diff --git a/src/example.gleam b/src/example.gleam\n"
  <> "index 1111111..2222222 100644\n"
  <> "--- a/src/example.gleam\n"
  <> "+++ b/src/example.gleam\n"
  <> "@@ -1,2 +1,3 @@\n"
  <> " pub fn value() {\n"
  <> "+  2\n"
  <> "   1\n"
  <> " }\n"
}

fn native_prepare_shared_inputs_diff(existing_path: String) -> String {
  "diff --git a/"
  <> existing_path
  <> " b/"
  <> existing_path
  <> "\n"
  <> "index 1111111..2222222 100644\n"
  <> "--- a/"
  <> existing_path
  <> "\n"
  <> "+++ b/"
  <> existing_path
  <> "\n"
  <> "@@ -1 +1,2 @@\n"
  <> " print('before')\n"
  <> "+print('after')\n"
  <> "diff --git a/src/scherzo/missing_module.gleam b/src/scherzo/missing_module.gleam\n"
  <> "new file mode 100644\n"
  <> "index 0000000..3333333\n"
  <> "--- /dev/null\n"
  <> "+++ b/src/scherzo/missing_module.gleam\n"
  <> "@@ -0,0 +1,2 @@\n"
  <> "+pub fn missing() {\n"
  <> "+  Nil\n"
  <> "+}\n"
}

pub fn prepare_native_serializes_passed_validation_evidence_test() {
  let dir = "test/tmp/review-artifacts-native-validation-passed"
  test_helpers.reset_dir(dir)
  let diff_path = dir <> "/change.diff"
  let output_dir = dir <> "/out"
  let validation_path = dir <> "/validation-passed.json"
  let assert Ok(Nil) = simplifile.write(diff_path, native_prepare_diff())
  let assert Ok(Nil) =
    simplifile.write(
      validation_path,
      "{\n"
        <> "  \"status\": \"passed\",\n"
        <> "  \"validator\": \"scherzo-ci\",\n"
        <> "  \"base_revision\": \"main@origin\",\n"
        <> "  \"commands\": [\"direnv exec . scripts/scherzo-ci\"],\n"
        <> "  \"setup_commands\": [\"direnv allow .\"]\n"
        <> "}\n",
    )

  let prepare =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review prepare-native --diff-file "
      <> diff_path
      <> " --output-dir "
      <> output_dir
      <> " --validation-result "
      <> validation_path
      <> " --validation-log-ref .scherzo/command-step-diagnostics/validate_before_native_review.txt",
    )
  assert prepare.status == step_artifact.StepSucceeded
  assert prepare.exit_code == Some(0)
  let assert Ok(validation_status) =
    simplifile.read(output_dir <> "/validation-status.v1.json")
  let assert Ok(brief) = simplifile.read(output_dir <> "/review-brief.v1.json")
  assert string.contains(
    validation_status,
    "\"artifact_type\": \"validation_status\"",
  )
  assert string.contains(validation_status, "\"overall_state\": \"passed\"")
  assert string.contains(
    validation_status,
    "\"command\": \"direnv exec . scripts/scherzo-ci\"",
  )
  assert string.contains(validation_status, "\"exit_status\": 0")
  assert string.contains(validation_status, validation_path)
  assert string.contains(
    validation_status,
    ".scherzo/command-step-diagnostics/validate_before_native_review.txt",
  )
  assert string.contains(brief, "\"status\": \"passed\"")
  assert string.contains(
    brief,
    "\"source\": \"structured_validation_artifact\"",
  )
}

pub fn prepare_native_writes_shared_input_artifacts_test() {
  let dir = "test/tmp/review-artifacts-native-shared-inputs"
  test_helpers.reset_dir(dir)
  let existing_path = dir <> "/context_fixture.py"
  let diff_path = dir <> "/change.diff"
  let output_dir = dir <> "/out"
  let assert Ok(Nil) =
    simplifile.write(
      existing_path,
      "print('before')\nprint('current checkout')\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      diff_path,
      native_prepare_shared_inputs_diff(existing_path),
    )

  let prepare =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review prepare-native --diff-file "
      <> diff_path
      <> " --output-dir "
      <> output_dir
      <> " --source-label shared-input-fixture",
    )
  assert prepare.status == step_artifact.StepSucceeded
  assert prepare.exit_code == Some(0)

  let assert Ok(source_metadata) =
    simplifile.read(output_dir <> "/source-metadata.v1.json")
  let assert Ok(changed_files) =
    simplifile.read(output_dir <> "/changed-files.v1.json")
  let assert Ok(context_manifest) =
    simplifile.read(output_dir <> "/context-manifest.v1.json")
  let assert Ok(context_snapshot) =
    simplifile.read(
      output_dir
      <> "/context/test__tmp__review-artifacts-native-shared-inputs__context_fixture.py.txt",
    )

  assert_contains(source_metadata, "\"kind\": \"diff_file\"")
  assert_contains(source_metadata, "\"label\": \"shared-input-fixture\"")
  assert_contains(source_metadata, "\"changed_file_count\": 2")
  assert_contains(source_metadata, "\"diff_sha256\":")
  assert_contains(changed_files, "\"artifact_type\": \"changed_files\"")
  assert_contains(changed_files, "\"path\": \"" <> existing_path <> "\"")
  assert_contains(changed_files, "\"language\": \"python\"")
  assert_contains(changed_files, "\"subsystem\": \"tests\"")
  assert_contains(
    changed_files,
    "\"path\": \"src/scherzo/missing_module.gleam\"",
  )
  assert_contains(changed_files, "\"language\": \"gleam\"")
  assert_contains(changed_files, "\"subsystem\": \"runtime:missing_module\"")
  assert_contains(context_manifest, "\"artifact_type\": \"context_manifest\"")
  assert_contains(context_manifest, "\"absolute_paths_serialized\": false")
  assert_contains(context_manifest, "\"available\": true")
  assert_contains(context_manifest, "\"truncated\": false")
  assert_contains(context_manifest, "\"available\": false")
  assert_contains(context_manifest, "file is absent in current checkout")
  assert_contains(context_snapshot, "print('current checkout')")
}

pub fn prepare_native_rejects_deprecated_review_backend_env_vars_test() {
  let dir = "test/tmp/review-artifacts-native-deprecated-backend-env"
  test_helpers.reset_dir(dir)
  let diff_path = dir <> "/change.diff"
  let output_dir = dir <> "/out"
  let assert Ok(Nil) = simplifile.write(diff_path, native_prepare_diff())

  let prepare =
    run_command_with_env(
      ".scherzo/workflows/scripts/scherzo-review prepare-native --diff-file "
        <> diff_path
        <> " --output-dir "
        <> output_dir,
      [#("SCHERZO_REVIEW_AGENT_BACKEND", "fixture")],
    )
  assert prepare.status == step_artifact.StepFailed
  assert prepare.exit_code == Some(1)
  assert_contains(
    prepare.stderr,
    "deprecated legacy review backend environment variables",
  )
  assert_contains(prepare.stderr, "SCHERZO_REVIEW_AGENT_BACKEND")
  assert_contains(prepare.stderr, "submit_review_lane_draft")
}

pub fn prepare_native_serializes_failed_validation_evidence_test() {
  let dir = "test/tmp/review-artifacts-native-validation-failed"
  test_helpers.reset_dir(dir)
  let diff_path = dir <> "/change.diff"
  let output_dir = dir <> "/out"
  let validation_path = dir <> "/validation-failed.json"
  let assert Ok(Nil) = simplifile.write(diff_path, native_prepare_diff())
  let assert Ok(Nil) =
    simplifile.write(
      validation_path,
      "{\n"
        <> "  \"status\": \"failed\",\n"
        <> "  \"validator\": \"scherzo-ci\",\n"
        <> "  \"exit_code\": 7,\n"
        <> "  \"commands\": [\"direnv exec . scripts/scherzo-ci\"],\n"
        <> "  \"failure_summary\": \"scherzo-ci failed on review fixture.\",\n"
        <> "  \"stdout_excerpt\": \"stdout line\",\n"
        <> "  \"stderr_excerpt\": \"stderr line\"\n"
        <> "}\n",
    )

  let prepare =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review prepare-native --diff-file "
      <> diff_path
      <> " --output-dir "
      <> output_dir
      <> " --validation-result "
      <> validation_path
      <> " --validation-log-ref .scherzo/command-step-diagnostics/validate_before_native_review.txt",
    )
  assert prepare.status == step_artifact.StepSucceeded
  let assert Ok(validation_status) =
    simplifile.read(output_dir <> "/validation-status.v1.json")
  let assert Ok(brief) = simplifile.read(output_dir <> "/review-brief.v1.json")
  assert string.contains(validation_status, "\"overall_state\": \"failed\"")
  assert string.contains(validation_status, "\"exit_status\": 7")
  assert string.contains(
    validation_status,
    "scherzo-ci failed on review fixture.",
  )
  assert string.contains(validation_status, "stdout line")
  assert string.contains(validation_status, "stderr line")
  assert string.contains(brief, "\"status\": \"failed\"")
}

pub fn prepare_native_records_not_run_validation_by_design_test() {
  let dir = "test/tmp/review-artifacts-native-validation-not-run"
  test_helpers.reset_dir(dir)
  let diff_path = dir <> "/change.diff"
  let output_dir = dir <> "/out"
  let assert Ok(Nil) = simplifile.write(diff_path, native_prepare_diff())

  let prepare =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review prepare-native --diff-file "
      <> diff_path
      <> " --output-dir "
      <> output_dir,
    )
  assert prepare.status == step_artifact.StepSucceeded
  let assert Ok(validation_status) =
    simplifile.read(output_dir <> "/validation-status.v1.json")
  let assert Ok(brief) = simplifile.read(output_dir <> "/review-brief.v1.json")
  assert string.contains(validation_status, "\"overall_state\": \"not_run\"")
  assert string.contains(
    validation_status,
    "\"source\": \"not_yet_run_by_design\"",
  )
  assert string.contains(
    validation_status,
    "Validation has not run before native review by workflow design.",
  )
  assert_not_contains(validation_status, "not_supplied")
  assert string.contains(brief, "\"status\": \"not_run\"")
}

pub fn prepare_native_records_missing_validation_artifact_test() {
  let dir = "test/tmp/review-artifacts-native-validation-missing"
  test_helpers.reset_dir(dir)
  let diff_path = dir <> "/change.diff"
  let output_dir = dir <> "/out"
  let missing_path = dir <> "/missing-validation.json"
  let assert Ok(Nil) = simplifile.write(diff_path, native_prepare_diff())

  let prepare =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review prepare-native --diff-file "
      <> diff_path
      <> " --output-dir "
      <> output_dir
      <> " --validation-result "
      <> missing_path,
    )
  assert prepare.status == step_artifact.StepSucceeded
  let assert Ok(validation_status) =
    simplifile.read(output_dir <> "/validation-status.v1.json")
  assert string.contains(validation_status, "\"overall_state\": \"missing\"")
  assert string.contains(
    validation_status,
    "\"source\": \"validation_artifact_missing\"",
  )
  assert string.contains(validation_status, missing_path)
  assert string.contains(
    validation_status,
    "expected validation evidence before native review, but the structured validation artifact was absent",
  )
  assert_not_contains(validation_status, "not_yet_run_by_design")
}

pub fn prepare_native_records_malformed_validation_artifact_test() {
  let dir = "test/tmp/review-artifacts-native-validation-malformed"
  test_helpers.reset_dir(dir)
  let diff_path = dir <> "/change.diff"
  let output_dir = dir <> "/out"
  let validation_path = dir <> "/validation-malformed.json"
  let assert Ok(Nil) = simplifile.write(diff_path, native_prepare_diff())
  let assert Ok(Nil) = simplifile.write(validation_path, "{not json}\n")

  let prepare =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review prepare-native --diff-file "
      <> diff_path
      <> " --output-dir "
      <> output_dir
      <> " --validation-result "
      <> validation_path,
    )
  assert prepare.status == step_artifact.StepSucceeded
  let assert Ok(validation_status) =
    simplifile.read(output_dir <> "/validation-status.v1.json")
  assert string.contains(validation_status, "\"overall_state\": \"malformed\"")
  assert string.contains(
    validation_status,
    "\"source\": \"malformed_validation_artifact\"",
  )
  assert string.contains(validation_status, validation_path)
  assert string.contains(validation_status, "invalid JSON in")
}

pub fn implementation_workflows_use_native_review_path_without_legacy_backends_test() {
  let workflow_paths = [
    ".scherzo/workflows/implementation.yaml",
    ".scherzo/workflows/execplan-implementation.yaml",
  ]

  list.each(workflow_paths, fn(path) {
    let assert Ok(workflow) = simplifile.read(path)
    assert_contains(workflow, "submit_review_lane_draft")
    assert_contains(workflow, "prepare-native")
    assert_contains(workflow, "validate_before_native_review")
    assert_contains(
      workflow,
      "--validation-result tmp/scherzo-implementation-validation.json",
    )
    assert_contains(
      workflow,
      ".scherzo/command-step-diagnostics/validate_before_native_review.txt",
    )
    assert_not_contains(workflow, "assert_native" <> "_review_cutover")
    assert_not_contains(workflow, "run-" <> "lane --lane")
    assert_not_contains(workflow, "--agent" <> "-backend")
    assert_not_contains(workflow, "SCHERZO_NATIVE" <> "_REVIEW" <> "_SCENARIO")
    assert_not_contains(
      workflow,
      "SCHERZO_STAGED" <> "_REVIEW" <> "_AGENT" <> "_BACKEND",
    )
    assert_not_contains(workflow, "SCHERZO_REVIEW" <> "_AGENT" <> "_BACKEND")
  })
}

pub fn review_artifact_validator_accepts_review_finding_test() {
  let dir = "test/tmp/review-artifacts-finding"
  test_helpers.reset_dir(dir)
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
    run_command(
      ".scherzo/workflows/scripts/scherzo-review validate --artifact "
      <> artifact_path,
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "REVIEW_ARTIFACT_VALID=ok")
  assert string.contains(artifact.stdout, "REVIEW_ARTIFACT_TYPE=review_finding")
}

pub fn review_artifact_validator_rejects_blocking_correctness_without_executable_evidence_test() {
  let dir = "test/tmp/review-artifacts-invalid-correctness-blocker"
  test_helpers.reset_dir(dir)
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
    run_command(
      ".scherzo/workflows/scripts/scherzo-review validate --artifact "
      <> artifact_path,
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(
    artifact.stderr,
    "blocking correctness findings must be verified with executable evidence",
  )
}

pub fn review_artifact_validator_rejects_missing_required_brief_fields_test() {
  let dir = "test/tmp/review-artifacts-invalid"
  test_helpers.reset_dir(dir)
  let artifact_path = dir <> "/invalid.json"
  let assert Ok(Nil) =
    simplifile.write(
      artifact_path,
      "{\"schema_version\":1,\"artifact_type\":\"review_brief\"}\n",
    )

  let artifact =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review validate --artifact "
      <> artifact_path,
    )

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
  test_helpers.reset_dir(dir)
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
    run_command(
      ".scherzo/workflows/scripts/scherzo-review validate --artifact "
      <> absolute_draft,
    )
  assert absolute.status == step_artifact.StepFailed
  assert absolute.exit_code == Some(1)

  let parent =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review validate --artifact "
      <> parent_draft,
    )
  assert parent.status == step_artifact.StepFailed
  assert parent.exit_code == Some(1)

  let env_input_ref =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review validate --artifact "
      <> env_input_ref_draft,
    )
  assert env_input_ref.status == step_artifact.StepFailed
  assert env_input_ref.exit_code == Some(1)
  assert string.contains(env_input_ref.stderr, "input_refs[0].path")
  assert string.contains(env_input_ref.stderr, "environment-variable")

  let remote =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review validate --artifact "
      <> remote_draft,
    )
  assert remote.status == step_artifact.StepFailed
  assert remote.exit_code == Some(1)
}

pub fn evidence_verdict_must_link_to_finding_test() {
  let dir = "test/tmp/native-evidence-linkage"
  test_helpers.reset_dir(dir)
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
      ".scherzo/workflows/scripts/scherzo-review normalize-lane-result --lane correctness --draft "
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
  test_helpers.reset_dir(dir)
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
      ".scherzo/workflows/scripts/scherzo-review verify-evidence --lane correctness --draft "
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
      ".scherzo/workflows/scripts/scherzo-review normalize-lane-result --lane correctness --draft "
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
  test_helpers.reset_dir(dir)
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
      ".scherzo/workflows/scripts/scherzo-review verify-evidence --lane correctness --draft "
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
  test_helpers.reset_dir(dir)
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
      ".scherzo/workflows/scripts/scherzo-review normalize-lane-result --lane correctness --draft "
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
  test_helpers.reset_dir(dir)
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
      ".scherzo/workflows/scripts/scherzo-review normalize-lane-result --lane correctness --draft "
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
  test_helpers.reset_dir(dir)
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
      ".scherzo/workflows/scripts/scherzo-review verify-evidence --lane correctness --draft "
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
  test_helpers.reset_dir(dir)
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
      ".scherzo/workflows/scripts/scherzo-review normalize-lane-result --lane correctness --draft "
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

pub fn all_lanes_review_infrastructure_failure_exits_42_test() {
  let dir = "test/tmp/native-all-lanes-infrastructure-failure"
  test_helpers.reset_dir(dir)
  write_native_support_files(dir)
  let lanes = [
    "correctness",
    "test-quality",
    "idioms-maintainability",
    "security-performance",
  ]

  list.each(lanes, fn(lane) {
    let lane_dir = dir <> "/" <> lane
    let metadata_path = lane_dir <> "/agent-step-metadata.v1.json"
    let assert Ok(Nil) = simplifile.create_directory_all(lane_dir)
    write_metadata(metadata_path)
    let normalized =
      run_command(
        ".scherzo/workflows/scripts/scherzo-review normalize-lane-result --lane "
        <> lane
        <> " --draft "
        <> lane_dir
        <> "/missing-draft.json --evidence-ledger "
        <> lane_dir
        <> "/missing-ledger.json --agent-step-metadata "
        <> metadata_path
        <> " --brief "
        <> dir
        <> "/review-brief.v1.json --output-dir "
        <> lane_dir,
      )
    assert normalized.status == step_artifact.StepSucceeded
  })

  let lane_args =
    lanes
    |> list.map(fn(lane) {
      " --lane-result "
      <> dir
      <> "/"
      <> lane
      <> "/review-lane-"
      <> lane
      <> ".v1.json"
    })
    |> string.join(with: "")
  let synth_dir = dir <> "/synthesis"
  let synthesized =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review synthesize --brief "
      <> dir
      <> "/review-brief.v1.json"
      <> lane_args
      <> " --output-dir "
      <> synth_dir,
    )

  assert synthesized.status == step_artifact.StepFailed
  assert synthesized.exit_code == Some(42)
  assert string.contains(
    synthesized.stdout,
    "REVIEW_SYNTHESIS=review_infrastructure_failure",
  )
  let assert Ok(diagnostic) =
    simplifile.read(synth_dir <> "/review-infrastructure-failure.v1.json")
  assert string.contains(diagnostic, "review_infrastructure_all_lanes_failed")
  assert string.contains(diagnostic, "review_infrastructure_failure")
  let assert Ok(False) =
    simplifile.is_file(synth_dir <> "/final-review.v1.json")
}

pub fn missing_or_malformed_draft_produces_failed_lane_result_test() {
  let dir = "test/tmp/native-malformed-draft"
  test_helpers.reset_dir(dir)
  write_native_support_files(dir)
  let draft_path = dir <> "/draft.v1.json"
  let metadata_path = dir <> "/agent-step-metadata.v1.json"
  let lane_dir = dir <> "/lane"
  let assert Ok(Nil) = simplifile.write(draft_path, "{ this is not json\n")
  write_metadata(metadata_path)

  let normalized =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review normalize-lane-result --lane correctness --draft "
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

fn final_review_with_findings_json() -> String {
  "{\n"
  <> "  \"schema_version\": 1,\n"
  <> "  \"artifact_type\": \"final_review\",\n"
  <> "  \"generated_at_utc\": \"2026-05-09T00:00:00Z\",\n"
  <> "  \"producer\": { \"name\": \"test\", \"version\": \"1\", \"mode\": \"test\" },\n"
  <> "  \"brief_ref\": { \"artifact_type\": \"review_brief\", \"path\": \"review-brief.v1.json\" },\n"
  <> "  \"synthesis_ref\": { \"artifact_type\": \"review_synthesis\", \"path\": \"review-synthesis.v1.json\" },\n"
  <> "  \"title\": \"Final review\",\n"
  <> "  \"summary\": \"Two findings.\",\n"
  <> "  \"finding_counts\": { \"total\": 2, \"blocking\": 1, \"non_blocking\": 1, \"by_severity\": {}, \"by_category\": {} },\n"
  <> "  \"grouped_findings\": {},\n"
  <> "  \"blockers\": [{\"id\":\"F-1\",\"category\":\"correctness\",\"severity\":\"high\",\"evidence_type\":\"test\",\"verified\":true,\"blocking\":true,\"locations\":[{\"path\":\"src/example.gleam\"}],\"summary\":\"Fix blocker\",\"details\":\"Broken behavior\",\"suggested_fix\":\"Add fix\"}],\n"
  <> "  \"non_blocking_findings\": [{\"id\":\"F-2\",\"category\":\"testing\",\"severity\":\"medium\",\"evidence_type\":\"static\",\"verified\":false,\"blocking\":false,\"locations\":[{\"path\":\"test/example_test.gleam\"}],\"summary\":\"Add test\",\"details\":\"Missing test\",\"suggested_fix\":\"Add coverage\"}],\n"
  <> "  \"lane_statuses\": [],\n"
  <> "  \"execution_issues\": [],\n"
  <> "  \"markdown\": \"# Final review\\nTwo findings.\",\n"
  <> "  \"remote_mutations\": \"none\"\n"
  <> "}\n"
}

fn final_review_with_all_dispositions_json() -> String {
  "{\n"
  <> "  \"schema_version\": 1,\n"
  <> "  \"artifact_type\": \"final_review\",\n"
  <> "  \"generated_at_utc\": \"2026-05-09T00:00:00Z\",\n"
  <> "  \"producer\": { \"name\": \"test\", \"version\": \"1\", \"mode\": \"test\" },\n"
  <> "  \"brief_ref\": { \"artifact_type\": \"review_brief\", \"path\": \"review-brief.v1.json\" },\n"
  <> "  \"synthesis_ref\": { \"artifact_type\": \"review_synthesis\", \"path\": \"review-synthesis.v1.json\" },\n"
  <> "  \"title\": \"Final review\",\n"
  <> "  \"summary\": \"Four findings.\",\n"
  <> "  \"finding_counts\": { \"total\": 4, \"blocking\": 1, \"non_blocking\": 3, \"by_severity\": {}, \"by_category\": {} },\n"
  <> "  \"grouped_findings\": {},\n"
  <> "  \"blockers\": [{\"id\":\"F-1\",\"category\":\"correctness\",\"severity\":\"high\",\"evidence_type\":\"test\",\"verified\":true,\"blocking\":true,\"locations\":[{\"path\":\"src/example.gleam\"}],\"summary\":\"Fix blocker\",\"details\":\"Broken behavior\",\"suggested_fix\":\"Add fix\"}],\n"
  <> "  \"non_blocking_findings\": [\n"
  <> "    {\"id\":\"F-2\",\"category\":\"testing\",\"severity\":\"medium\",\"evidence_type\":\"static\",\"verified\":false,\"blocking\":false,\"locations\":[{\"path\":\"test/example_test.gleam\"}],\"summary\":\"Add test\",\"details\":\"Missing test\",\"suggested_fix\":\"Add coverage\"},\n"
  <> "    {\"id\":\"F-3\",\"category\":\"maintainability\",\"severity\":\"low\",\"evidence_type\":\"static\",\"verified\":false,\"blocking\":false,\"locations\":[{\"path\":\"src/cleanup.gleam\"}],\"summary\":\"Follow-up cleanup\",\"details\":\"Can wait\",\"suggested_fix\":\"Track separately\"},\n"
  <> "    {\"id\":\"F-4\",\"category\":\"other\",\"severity\":\"info\",\"evidence_type\":\"static\",\"verified\":false,\"blocking\":false,\"locations\":[{\"path\":\"src/old_path.gleam\"}],\"summary\":\"Old path note\",\"details\":\"Later edits removed it\",\"suggested_fix\":\"None\"}\n"
  <> "  ],\n"
  <> "  \"lane_statuses\": [],\n"
  <> "  \"execution_issues\": [],\n"
  <> "  \"markdown\": \"# Final review\\nFour findings.\",\n"
  <> "  \"remote_mutations\": \"none\"\n"
  <> "}\n"
}

fn write_validation_artifact(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "{\"artifact_type\":\"implementation_validation\",\"status\":\"passed\"}\n",
    )
  Nil
}

pub fn disposition_input_structured_validator_rejects_string_schema_version_test() {
  let dir = "test/tmp/native-disposition-input-validator"
  test_helpers.reset_dir(dir)
  let payload_path = dir <> "/payload.json"
  let assert Ok(Nil) =
    simplifile.write(
      payload_path,
      "{\n"
        <> "  \"schema_version\": \"1\",\n"
        <> "  \"artifact_type\": \"review_finding_disposition_input\",\n"
        <> "  \"entries\": []\n"
        <> "}\n",
    )

  let validation =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review validate-structured-output --validator review_finding_disposition_input < "
      <> payload_path,
    )

  assert validation.status == step_artifact.StepFailed
  assert_contains(
    validation.stderr,
    "schema_version must be JSON number 1, got string \"1\"",
  )
}

pub fn materialize_disposition_input_uses_runner_validated_structured_output_test() {
  let dir = "test/tmp/native-disposition-input-materialize"
  test_helpers.reset_dir(dir)
  let artifact_dir = dir <> "/artifacts"
  let structured_dir = artifact_dir <> "/apply_feedback/attempt-2/structured"
  let assert Ok(Nil) = simplifile.create_directory_all(structured_dir)
  let structured_path =
    structured_dir <> "/review_finding_disposition_input.json"
  let assert Ok(Nil) =
    simplifile.write(
      artifact_dir <> "/apply_feedback/attempt-2.json",
      "{\n"
        <> "  \"artifact\": {\n"
        <> "    \"status\": \"success\",\n"
        <> "    \"structured_output\": {\n"
        <> "      \"status\": \"valid\",\n"
        <> "      \"artifact_name\": \"review_finding_disposition_input\",\n"
        <> "      \"path\": \"apply_feedback/attempt-2/structured/review_finding_disposition_input.json\",\n"
        <> "      \"source_type\": \"pi_tool_call\",\n"
        <> "      \"source_tool_name\": \"submit_review_finding_dispositions\",\n"
        <> "      \"source_parameters_schema_path\": \".scherzo/workflows/schemas/provider/review-finding-dispositions.v1.schema.json\"\n"
        <> "    }\n"
        <> "  }\n"
        <> "}\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      structured_path,
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"artifact_type\": \"structured_output\",\n"
        <> "  \"run_id\": \"run-1\",\n"
        <> "  \"workflow_id\": \"implementation\",\n"
        <> "  \"step_id\": \"apply_feedback\",\n"
        <> "  \"attempt_index\": 2,\n"
        <> "  \"artifact_name\": \"review_finding_disposition_input\",\n"
        <> "  \"format\": \"json\",\n"
        <> "  \"source_type\": \"pi_tool_call\",\n"
        <> "  \"source_tool_name\": \"submit_review_finding_dispositions\",\n"
        <> "  \"validation\": {\n"
        <> "    \"source_parameters_schema_path\": \".scherzo/workflows/schemas/provider/review-finding-dispositions.v1.schema.json\"\n"
        <> "  },\n"
        <> "  \"payload\": {\n"
        <> "    \"schema_version\": 1,\n"
        <> "    \"artifact_type\": \"review_finding_disposition_input\",\n"
        <> "    \"entries\": [\n"
        <> "      {\"finding_id\":\"F-1\",\"disposition\":\"resolved\",\"rationale\":\"Fixed and validated.\",\"evidence_refs\":[{\"type\":\"command\",\"description\":\"targeted tests\",\"command\":\"direnv exec . gleam test\"}]}\n"
        <> "    ]\n"
        <> "  }\n"
        <> "}\n",
    )
  let output_path = dir <> "/tmp/review-finding-dispositions.v1.json"

  let materialized =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review materialize-disposition-input --artifact-dir "
      <> artifact_dir
      <> " --submission-step apply_feedback --submission-artifact review_finding_disposition_input --output "
      <> output_path,
    )

  assert materialized.status == step_artifact.StepSucceeded
  assert_contains(materialized.stdout, "REVIEW_FINDING_DISPOSITION_INPUT=ok")
  let assert Ok(contents) = simplifile.read(output_path)
  assert_contains(
    contents,
    "\"artifact_type\": \"review_finding_disposition_input\"",
  )
  assert_not_contains(contents, "\"artifact_type\": \"structured_output\"")
  assert_contains(contents, "\"schema_version\": 1")
  assert_contains(contents, "\"finding_id\": \"F-1\"")
}

pub fn materialize_disposition_input_finds_hash_suffixed_step_metadata_test() {
  let dir = "test/tmp/native-disposition-input-hashed-metadata"
  test_helpers.reset_dir(dir)
  let artifact_dir = dir <> "/artifacts"
  let structured_dir = artifact_dir <> "/apply_feedback/attempt-2/structured"
  let metadata_dir = artifact_dir <> "/apply_feedback-8eeea1750f4d"
  let assert Ok(Nil) = simplifile.create_directory_all(structured_dir)
  let assert Ok(Nil) = simplifile.create_directory_all(metadata_dir)
  let structured_path =
    structured_dir <> "/review_finding_disposition_input.json"
  let assert Ok(Nil) =
    simplifile.write(
      metadata_dir <> "/attempt-2.json",
      "{\n"
        <> "  \"artifact\": {\n"
        <> "    \"status\": \"success\",\n"
        <> "    \"structured_output\": {\n"
        <> "      \"status\": \"valid\",\n"
        <> "      \"artifact_name\": \"review_finding_disposition_input\",\n"
        <> "      \"path\": \"apply_feedback/attempt-2/structured/review_finding_disposition_input.json\",\n"
        <> "      \"source_type\": \"pi_tool_call\",\n"
        <> "      \"source_tool_name\": \"submit_review_finding_dispositions\",\n"
        <> "      \"source_parameters_schema_path\": \".scherzo/workflows/schemas/provider/review-finding-dispositions.v1.schema.json\"\n"
        <> "    }\n"
        <> "  }\n"
        <> "}\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      structured_path,
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"artifact_type\": \"structured_output\",\n"
        <> "  \"run_id\": \"run-1\",\n"
        <> "  \"workflow_id\": \"implementation\",\n"
        <> "  \"step_id\": \"apply_feedback\",\n"
        <> "  \"attempt_index\": 2,\n"
        <> "  \"artifact_name\": \"review_finding_disposition_input\",\n"
        <> "  \"format\": \"json\",\n"
        <> "  \"source_type\": \"pi_tool_call\",\n"
        <> "  \"source_tool_name\": \"submit_review_finding_dispositions\",\n"
        <> "  \"validation\": {\n"
        <> "    \"source_parameters_schema_path\": \".scherzo/workflows/schemas/provider/review-finding-dispositions.v1.schema.json\"\n"
        <> "  },\n"
        <> "  \"payload\": {\n"
        <> "    \"schema_version\": 1,\n"
        <> "    \"artifact_type\": \"review_finding_disposition_input\",\n"
        <> "    \"entries\": [\n"
        <> "      {\"finding_id\":\"F-2\",\"disposition\":\"resolved\",\"rationale\":\"Fixed and validated from hash-suffixed metadata.\",\"evidence_refs\":[{\"type\":\"command\",\"description\":\"targeted tests\",\"command\":\"direnv exec . gleam test\"}]}\n"
        <> "    ]\n"
        <> "  }\n"
        <> "}\n",
    )
  let output_path = dir <> "/tmp/review-finding-dispositions.v1.json"

  let materialized =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review materialize-disposition-input --artifact-dir "
      <> artifact_dir
      <> " --submission-step apply_feedback --submission-artifact review_finding_disposition_input --output "
      <> output_path,
    )

  assert materialized.status == step_artifact.StepSucceeded
  assert_contains(materialized.stdout, "REVIEW_FINDING_DISPOSITION_INPUT=ok")
  let assert Ok(contents) = simplifile.read(output_path)
  assert_contains(contents, "\"finding_id\": \"F-2\"")
  assert_not_contains(contents, "\"artifact_type\": \"structured_output\"")
}

pub fn finalize_dispositions_writes_schema_valid_artifacts_test() {
  let dir = "test/tmp/native-finalize-dispositions"
  test_helpers.reset_dir(dir)
  let final_path = dir <> "/final-review.v1.json"
  let input_path = dir <> "/disposition-input.v1.json"
  let validation_path = dir <> "/validation.json"
  let output_dir = dir <> "/out"
  let assert Ok(Nil) =
    simplifile.write(final_path, final_review_with_all_dispositions_json())
  let assert Ok(Nil) =
    simplifile.write(
      input_path,
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"artifact_type\": \"review_finding_disposition_input\",\n"
        <> "  \"entries\": [\n"
        <> "    {\"finding_id\":\"F-1\",\"disposition\":\"resolved\",\"rationale\":\"Validation passed after the fix.\",\"evidence_refs\":[{\"type\":\"path\",\"description\":\"validation artifact\",\"path\":\"tmp/scherzo-implementation-validation.json\"}]},\n"
        <> "    {\"finding_id\":\"F-2\",\"disposition\":\"rejected\",\"rationale\":\"Existing tests already cover the change.\",\"evidence_refs\":[{\"type\":\"command\",\"description\":\"targeted test\",\"command\":\"direnv exec . gleam test\"}]},\n"
        <> "    {\"finding_id\":\"F-3\",\"disposition\":\"deferred\",\"rationale\":\"Documented as non-blocking follow-up.\",\"evidence_refs\":[{\"type\":\"path\",\"description\":\"follow-up note\",\"path\":\"tmp/follow-up.md\"}]},\n"
        <> "    {\"finding_id\":\"F-4\",\"disposition\":\"obsolete\",\"rationale\":\"Later edits removed the old path.\",\"evidence_refs\":[{\"type\":\"command\",\"description\":\"diff check\",\"command\":\"jj diff --stat\"}]}\n"
        <> "  ]\n"
        <> "}\n",
    )
  write_validation_artifact(validation_path)

  let finalized =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review finalize-dispositions --final-review "
      <> final_path
      <> " --disposition-input "
      <> input_path
      <> " --validation-artifact "
      <> validation_path
      <> " --output-dir "
      <> output_dir
      <> " --require-publishable",
    )
  assert finalized.status == step_artifact.StepSucceeded
  assert string.contains(finalized.stdout, "REVIEW_FINDING_DISPOSITIONS=ok")
  assert string.contains(finalized.stdout, "REVIEW_PUBLISH_READY=true")

  let artifact_path = output_dir <> "/review-finding-dispositions.v1.json"
  let final_output_path = output_dir <> "/final-review.v1.json"
  let assert Ok(dispositions) = simplifile.read(artifact_path)
  let assert Ok(finalized_review) = simplifile.read(final_output_path)
  let assert Ok(markdown) = simplifile.read(output_dir <> "/final-review.md")
  assert string.contains(
    dispositions,
    "\"artifact_type\": \"review_finding_dispositions\"",
  )
  assert string.contains(dispositions, "\"disposition\": \"resolved\"")
  assert string.contains(dispositions, "\"disposition\": \"rejected\"")
  assert string.contains(dispositions, "\"disposition\": \"deferred\"")
  assert string.contains(dispositions, "\"disposition\": \"obsolete\"")
  assert string.contains(finalized_review, "\"finding_dispositions\"")
  assert string.contains(finalized_review, "\"disposition_summary\"")
  assert string.contains(finalized_review, "\"publish_ready\": true")
  assert string.contains(markdown, "## Finding dispositions")
  assert string.contains(
    markdown,
    "| F-1 | high/correctness | yes | resolved |",
  )
  assert string.contains(
    markdown,
    "| F-3 | low/maintainability | no | deferred |",
  )
  assert string.contains(markdown, "| F-4 | info/other | no | obsolete |")

  let validation =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review validate --artifact "
      <> artifact_path,
    )
  assert validation.status == step_artifact.StepSucceeded
  assert string.contains(
    validation.stdout,
    "REVIEW_ARTIFACT_TYPE=review_finding_dispositions",
  )
}

pub fn finalize_dispositions_normalizes_string_evidence_refs_test() {
  let dir = "test/tmp/native-finalize-dispositions-string-evidence"
  test_helpers.reset_dir(dir)
  let final_path = dir <> "/final-review.v1.json"
  let input_path = dir <> "/disposition-input.v1.json"
  let validation_path = dir <> "/validation.json"
  let output_dir = dir <> "/out"
  let assert Ok(Nil) =
    simplifile.write(final_path, final_review_with_findings_json())
  let assert Ok(Nil) =
    simplifile.write(
      input_path,
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"artifact_type\": \"review_finding_disposition_input\",\n"
        <> "  \"entries\": [\n"
        <> "    {\"finding_id\":\"F-1\",\"disposition\":\"resolved\",\"rationale\":\"Fixed and validated.\",\"evidence_refs\":[\"src/example.gleam:Fix blocker\",\"direnv exec . gleam test\"]},\n"
        <> "    {\"finding_id\":\"F-2\",\"disposition\":\"deferred\",\"rationale\":\"Tracked as non-blocking follow-up.\",\"evidence_refs\":[\"../../artifacts/review/synthesize_review/final-review.v1.json:F-2\"]}\n"
        <> "  ]\n"
        <> "}\n",
    )
  write_validation_artifact(validation_path)

  let finalized =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review finalize-dispositions --final-review "
      <> final_path
      <> " --disposition-input "
      <> input_path
      <> " --validation-artifact "
      <> validation_path
      <> " --output-dir "
      <> output_dir,
    )
  assert finalized.status == step_artifact.StepSucceeded
  let artifact_path = output_dir <> "/review-finding-dispositions.v1.json"
  let assert Ok(dispositions) = simplifile.read(artifact_path)
  assert_contains(dispositions, "\"type\": \"reference\"")
  assert_contains(
    dispositions,
    "\"description\": \"src/example.gleam:Fix blocker\"",
  )
  assert_contains(dispositions, "\"description\": \"direnv exec . gleam test\"")
  let validation =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review validate --artifact "
      <> artifact_path,
    )
  assert validation.status == step_artifact.StepSucceeded
}

pub fn finalize_dispositions_rejects_deferred_blocking_findings_test() {
  let dir = "test/tmp/native-finalize-dispositions-blocked"
  test_helpers.reset_dir(dir)
  let final_path = dir <> "/final-review.v1.json"
  let input_path = dir <> "/disposition-input.v1.json"
  let validation_path = dir <> "/validation.json"
  let assert Ok(Nil) =
    simplifile.write(final_path, final_review_with_findings_json())
  let assert Ok(Nil) =
    simplifile.write(
      input_path,
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"artifact_type\": \"review_finding_disposition_input\",\n"
        <> "  \"entries\": [\n"
        <> "    {\"finding_id\":\"F-1\",\"disposition\":\"deferred\",\"rationale\":\"Too risky today.\",\"evidence_refs\":[{\"type\":\"path\",\"description\":\"note\",\"path\":\"tmp/defer.md\"}]},\n"
        <> "    {\"finding_id\":\"F-2\",\"disposition\":\"obsolete\",\"rationale\":\"Later edits removed the old path.\",\"evidence_refs\":[{\"type\":\"command\",\"description\":\"diff check\",\"command\":\"jj diff --stat\"}]}\n"
        <> "  ]\n"
        <> "}\n",
    )
  write_validation_artifact(validation_path)

  let finalized =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review finalize-dispositions --final-review "
      <> final_path
      <> " --disposition-input "
      <> input_path
      <> " --validation-artifact "
      <> validation_path
      <> " --output-dir "
      <> dir
      <> "/out --require-publishable",
    )
  assert finalized.status == step_artifact.StepFailed
  assert string.contains(
    finalized.stderr,
    "blocking finding disposition state is not publishable",
  )
}

pub fn finalize_dispositions_rejects_missing_finding_ids_test() {
  let dir = "test/tmp/native-finalize-dispositions-missing"
  test_helpers.reset_dir(dir)
  let final_path = dir <> "/final-review.v1.json"
  let input_path = dir <> "/disposition-input.v1.json"
  let validation_path = dir <> "/validation.json"
  let assert Ok(Nil) =
    simplifile.write(final_path, final_review_with_findings_json())
  let assert Ok(Nil) =
    simplifile.write(
      input_path,
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"artifact_type\": \"review_finding_disposition_input\",\n"
        <> "  \"entries\": [\n"
        <> "    {\"finding_id\":\"F-1\",\"disposition\":\"resolved\",\"rationale\":\"Fixed.\",\"evidence_refs\":[{\"type\":\"command\",\"description\":\"tests\",\"command\":\"direnv exec . gleam test\"}]}\n"
        <> "  ]\n"
        <> "}\n",
    )
  write_validation_artifact(validation_path)

  let finalized =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review finalize-dispositions --final-review "
      <> final_path
      <> " --disposition-input "
      <> input_path
      <> " --validation-artifact "
      <> validation_path
      <> " --output-dir "
      <> dir
      <> "/out",
    )
  assert finalized.status == step_artifact.StepFailed
  assert string.contains(
    finalized.stderr,
    "disposition input missing finding ids: F-2",
  )
}

pub fn finalize_dispositions_rejects_duplicate_finding_ids_test() {
  let dir = "test/tmp/native-finalize-dispositions-duplicate"
  test_helpers.reset_dir(dir)
  let final_path = dir <> "/final-review.v1.json"
  let input_path = dir <> "/disposition-input.v1.json"
  let validation_path = dir <> "/validation.json"
  let assert Ok(Nil) =
    simplifile.write(final_path, final_review_with_findings_json())
  let assert Ok(Nil) =
    simplifile.write(
      input_path,
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"artifact_type\": \"review_finding_disposition_input\",\n"
        <> "  \"entries\": [\n"
        <> "    {\"finding_id\":\"F-1\",\"disposition\":\"resolved\",\"rationale\":\"Fixed.\",\"evidence_refs\":[{\"type\":\"command\",\"description\":\"tests\",\"command\":\"direnv exec . gleam test\"}]},\n"
        <> "    {\"finding_id\":\"F-1\",\"disposition\":\"rejected\",\"rationale\":\"Duplicate entry.\",\"evidence_refs\":[{\"type\":\"path\",\"description\":\"note\",\"path\":\"tmp/duplicate.md\"}]}\n"
        <> "  ]\n"
        <> "}\n",
    )
  write_validation_artifact(validation_path)

  let finalized =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review finalize-dispositions --final-review "
      <> final_path
      <> " --disposition-input "
      <> input_path
      <> " --validation-artifact "
      <> validation_path
      <> " --output-dir "
      <> dir
      <> "/out",
    )
  assert finalized.status == step_artifact.StepFailed
  assert string.contains(
    finalized.stderr,
    "review finding disposition input finding_id values must be unique",
  )
}

pub fn finalize_dispositions_rejects_unknown_finding_ids_test() {
  let dir = "test/tmp/native-finalize-dispositions-unknown"
  test_helpers.reset_dir(dir)
  let final_path = dir <> "/final-review.v1.json"
  let input_path = dir <> "/disposition-input.v1.json"
  let validation_path = dir <> "/validation.json"
  let assert Ok(Nil) =
    simplifile.write(final_path, final_review_with_findings_json())
  let assert Ok(Nil) =
    simplifile.write(
      input_path,
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"artifact_type\": \"review_finding_disposition_input\",\n"
        <> "  \"entries\": [\n"
        <> "    {\"finding_id\":\"F-1\",\"disposition\":\"resolved\",\"rationale\":\"Fixed.\",\"evidence_refs\":[{\"type\":\"command\",\"description\":\"tests\",\"command\":\"direnv exec . gleam test\"}]},\n"
        <> "    {\"finding_id\":\"F-2\",\"disposition\":\"rejected\",\"rationale\":\"Not applicable.\",\"evidence_refs\":[{\"type\":\"path\",\"description\":\"note\",\"path\":\"tmp/note.md\"}]},\n"
        <> "    {\"finding_id\":\"F-99\",\"disposition\":\"obsolete\",\"rationale\":\"Unknown extra entry.\",\"evidence_refs\":[{\"type\":\"command\",\"description\":\"diff\",\"command\":\"jj diff --stat\"}]}\n"
        <> "  ]\n"
        <> "}\n",
    )
  write_validation_artifact(validation_path)

  let finalized =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review finalize-dispositions --final-review "
      <> final_path
      <> " --disposition-input "
      <> input_path
      <> " --validation-artifact "
      <> validation_path
      <> " --output-dir "
      <> dir
      <> "/out",
    )
  assert finalized.status == step_artifact.StepFailed
  assert string.contains(
    finalized.stderr,
    "disposition input contains unknown finding ids: F-99",
  )
}

pub fn finalize_dispositions_handles_no_findings_test() {
  let dir = "test/tmp/native-finalize-dispositions-empty"
  test_helpers.reset_dir(dir)
  let final_path = dir <> "/final-review.v1.json"
  let input_path = dir <> "/disposition-input.v1.json"
  let validation_path = dir <> "/validation.json"
  let output_dir = dir <> "/out"
  let assert Ok(Nil) = simplifile.write(final_path, final_review_json())
  let assert Ok(Nil) =
    simplifile.write(
      input_path,
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"artifact_type\": \"review_finding_disposition_input\",\n"
        <> "  \"entries\": []\n"
        <> "}\n",
    )
  write_validation_artifact(validation_path)

  let finalized =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review finalize-dispositions --final-review "
      <> final_path
      <> " --disposition-input "
      <> input_path
      <> " --validation-artifact "
      <> validation_path
      <> " --output-dir "
      <> output_dir
      <> " --require-publishable",
    )
  assert finalized.status == step_artifact.StepSucceeded
  let assert Ok(dispositions) =
    simplifile.read(output_dir <> "/review-finding-dispositions.v1.json")
  let assert Ok(markdown) = simplifile.read(output_dir <> "/final-review.md")
  assert_contains(dispositions, "\"total\": 0")
  assert_contains(dispositions, "\"publish_ready\": true")
  assert_contains(
    markdown,
    "| None | - | - | - | No synthesized findings. | - |",
  )
}

pub fn finalize_dispositions_is_idempotent_over_same_output_dir_test() {
  let dir = "test/tmp/native-finalize-dispositions-idempotent"
  test_helpers.reset_dir(dir)
  let final_path = dir <> "/final-review.v1.json"
  let input_path = dir <> "/disposition-input.v1.json"
  let validation_path = dir <> "/validation.json"
  let output_dir = dir <> "/out"
  let assert Ok(Nil) =
    simplifile.write(final_path, final_review_with_findings_json())
  let assert Ok(Nil) =
    simplifile.write(
      input_path,
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"artifact_type\": \"review_finding_disposition_input\",\n"
        <> "  \"entries\": [\n"
        <> "    {\"finding_id\":\"F-1\",\"disposition\":\"resolved\",\"rationale\":\"Fixed.\",\"evidence_refs\":[{\"type\":\"command\",\"description\":\"tests\",\"command\":\"direnv exec . gleam test\"}]},\n"
        <> "    {\"finding_id\":\"F-2\",\"disposition\":\"obsolete\",\"rationale\":\"Later edits removed the concern.\",\"evidence_refs\":[{\"type\":\"command\",\"description\":\"diff\",\"command\":\"jj diff --stat\"}]}\n"
        <> "  ]\n"
        <> "}\n",
    )
  write_validation_artifact(validation_path)

  let first =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review finalize-dispositions --final-review "
      <> final_path
      <> " --disposition-input "
      <> input_path
      <> " --validation-artifact "
      <> validation_path
      <> " --output-dir "
      <> output_dir
      <> " --require-publishable",
    )
  let second =
    run_command(
      ".scherzo/workflows/scripts/scherzo-review finalize-dispositions --final-review "
      <> final_path
      <> " --disposition-input "
      <> input_path
      <> " --validation-artifact "
      <> validation_path
      <> " --output-dir "
      <> output_dir
      <> " --require-publishable",
    )

  assert first.status == step_artifact.StepSucceeded
  assert second.status == step_artifact.StepSucceeded
  let assert Ok(dispositions) =
    simplifile.read(output_dir <> "/review-finding-dispositions.v1.json")
  let assert Ok(markdown) = simplifile.read(output_dir <> "/final-review.md")
  assert_contains(dispositions, "\"total\": 2")
  assert_contains(dispositions, "\"publish_ready\": true")
  assert_contains(markdown, "## Finding dispositions")
  assert_not_contains(
    markdown,
    "## Finding dispositions\n\n## Finding dispositions",
  )
}
