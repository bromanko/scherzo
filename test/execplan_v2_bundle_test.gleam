import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/step_artifact
import simplifile
import workflow_context_test_support

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 12_000,
    template_field_max_chars: 12_000,
    workflow_summary_max_chars: 12_000,
  )
}

fn run_helper(command: String) -> step_artifact.StepArtifact {
  command_step.run(
    "execplan_v2_helper",
    workflow_context_test_support.without_workflow_context(
      "scripts/scherzo-execplan " <> command,
    ),
    ".",
    30_000,
    [],
    limits(),
  )
}

fn run_shell(command: String) -> step_artifact.StepArtifact {
  command_step.run(
    "execplan_v2_helper",
    workflow_context_test_support.without_workflow_context(command),
    ".",
    30_000,
    [],
    limits(),
  )
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

fn shell_quote(value: String) -> String {
  "'" <> string.replace(value, each: "'", with: "'\\''") <> "'"
}

fn mutated_bundle(dir: String, each old: String, with new: String) -> String {
  reset_dir(dir)
  let assert Ok(source) =
    simplifile.read("test/fixtures/execplan_v2/exec-plan-bundle.valid.json")
  let path = dir <> "/bundle.json"
  let assert Ok(Nil) =
    simplifile.write(path, string.replace(source, each: old, with: new))
  path
}

fn pack_submission(title: String) -> String {
  "{\n"
  <> "  \"artifact_name\": \"implementation_pack_submission\",\n"
  <> "  \"payload\": {\n"
  <> "    \"schema_version\": 2,\n"
  <> "    \"artifact_type\": \"implementation_pack_submission\",\n"
  <> "    \"source_issue\": {\n"
  <> "      \"identifier\": \"LIV-314\",\n"
  <> "      \"title\": \""
  <> title
  <> "\",\n"
  <> "      \"url\": \"https://linear.app/living-systems/issue/LIV-314/fixture-v2-execplan-bundle\"\n"
  <> "    },\n"
  <> "    \"sections\": {\n"
  <> "      \"repo_context\": \"Fixture repo context.\",\n"
  <> "      \"verified_facts\": [{\"fact\": \"Fact\", \"evidence\": \"Evidence\"}],\n"
  <> "      \"concrete_steps\": [{\"title\": \"Step\", \"instructions\": \"Do it.\", \"files\": [\"docs/plans/example.md\"], \"commands\": [\"gleam test\"], \"expected_result\": \"Passes.\"}],\n"
  <> "      \"testing_and_falsifiability\": \"Run the helper tests.\",\n"
  <> "      \"interfaces_and_dependencies\": \"Use scripts/scherzo-execplan.\",\n"
  <> "      \"artifacts_and_notes\": \"No extra artifacts.\"\n"
  <> "    },\n"
  <> "    \"conflict_policy\": \"Stop on review doc and pack conflicts.\"\n"
  <> "  }\n"
  <> "}\n"
}

pub fn validate_bundle_accepts_valid_fixture_test() {
  let artifact =
    run_helper(
      "validate-bundle --bundle test/fixtures/execplan_v2/exec-plan-bundle.valid.json --repo-root .",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "BUNDLE_VALID=ok")
}

pub fn validate_bundle_rejects_stale_pack_fixture_test() {
  let artifact =
    run_helper(
      "validate-bundle --bundle test/fixtures/execplan_v2/exec-plan-bundle.stale-pack.json --repo-root .",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_v2_stale_implementation_pack",
  )
}

pub fn validate_bundle_rejects_missing_review_doc_test() {
  let path =
    mutated_bundle(
      "test/tmp/execplan-missing-review-doc",
      each: "test/fixtures/execplan_v2/review-doc.valid.md",
      with: "test/tmp/execplan-missing-review-doc/missing.md",
    )

  let artifact =
    run_helper("validate-bundle --bundle " <> path <> " --repo-root .")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_v2_review_doc_missing",
  )
}

pub fn validate_bundle_rejects_review_doc_hash_mismatch_test() {
  let path =
    mutated_bundle(
      "test/tmp/execplan-review-hash-mismatch",
      each: "64288f367d31d10a48decbb7f5b19ec4975e1a3a2991be2a4bc1007d8a61dcf4",
      with: "0000000000000000000000000000000000000000000000000000000000000000",
    )

  let artifact =
    run_helper("validate-bundle --bundle " <> path <> " --repo-root .")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_v2_review_doc_hash_mismatch",
  )
}

pub fn validate_bundle_rejects_missing_implementation_pack_test() {
  let path =
    mutated_bundle(
      "test/tmp/execplan-missing-pack",
      each: "runs/run-1/outputs/implementation_pack.json",
      with: "runs/run-missing/outputs/implementation_pack.json",
    )

  let artifact =
    run_helper("validate-bundle --bundle " <> path <> " --repo-root .")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_v2_implementation_pack_missing",
  )
}

pub fn validate_bundle_rejects_implementation_pack_hash_mismatch_test() {
  let path =
    mutated_bundle(
      "test/tmp/execplan-pack-hash-mismatch",
      each: "dbcb84d078e47839e8da760b1208e6b5606bce45ab3228a24a92e0b5afd21545",
      with: "0000000000000000000000000000000000000000000000000000000000000000",
    )

  let artifact =
    run_helper("validate-bundle --bundle " <> path <> " --repo-root .")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_v2_implementation_pack_hash_mismatch",
  )
}

pub fn validate_bundle_rejects_absolute_review_doc_path_test() {
  let artifact =
    run_helper(
      "validate-bundle --bundle test/fixtures/execplan_v2/exec-plan-bundle.absolute-path.json --repo-root .",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert string.contains(artifact.stderr, "schema validation failed")
}

pub fn validate_bundle_rejects_bundle_self_hash_test() {
  let dir = "test/tmp/execplan-self-hash"
  reset_dir(dir)
  let assert Ok(source) =
    simplifile.read("test/fixtures/execplan_v2/exec-plan-bundle.valid.json")
  let mutated =
    string.replace(
      source,
      each: "  \"artifact_type\": \"exec_plan_bundle\",\n",
      with: "  \"artifact_type\": \"exec_plan_bundle\",\n  \"sha256\": \"0000000000000000000000000000000000000000000000000000000000000000\",\n",
    )
  let path = dir <> "/bundle.json"
  let assert Ok(Nil) = simplifile.write(path, mutated)

  let artifact =
    run_helper("validate-bundle --bundle " <> path <> " --repo-root .")

  assert artifact.status == step_artifact.StepFailed
  assert string.contains(artifact.stderr, "must not store its own SHA-256")
}

pub fn validate_review_doc_rejects_mechanical_sections_test() {
  let dir = "test/tmp/execplan-review-doc"
  reset_dir(dir)
  let path = dir <> "/review.md"
  let assert Ok(valid) =
    simplifile.read("test/fixtures/execplan_v2/review-doc.valid.md")
  let assert Ok(Nil) =
    simplifile.write(
      path,
      valid <> "\n## Concrete Steps\n\nThese belong in the pack.\n",
    )

  let artifact = run_helper("validate-review-doc --path " <> path)

  assert artifact.status == step_artifact.StepFailed
  assert string.contains(artifact.stderr, "mechanical implementation sections")
}

pub fn discover_changed_review_doc_rejects_zero_candidates_test() {
  let dir = "test/tmp/execplan-discovery-zero"
  reset_dir(dir)

  let artifact =
    run_shell(
      "env SCHERZO_WORKSPACE_DRIVER=scripts/scherzo-workspace-noop SCHERZO_WORKSPACE_PATH="
      <> shell_quote(dir)
      <> " scripts/scherzo-execplan validate-review-doc --discover-changed-review-doc --write-path "
      <> shell_quote(dir <> "/review.path"),
    )

  assert artifact.status == step_artifact.StepFailed
  assert string.contains(artifact.stderr, "expected exactly one")
}

pub fn discover_changed_review_doc_rejects_multiple_candidates_test() {
  let dir = "test/tmp/execplan-discovery-multiple"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) = simplifile.write(dir <> "/docs/plans/a.md", "# A\n")
  let assert Ok(Nil) = simplifile.write(dir <> "/docs/plans/b.md", "# B\n")

  let artifact =
    run_shell(
      "env SCHERZO_WORKSPACE_DRIVER=scripts/scherzo-workspace-noop SCHERZO_WORKSPACE_PATH="
      <> shell_quote(dir)
      <> " scripts/scherzo-execplan validate-review-doc --discover-changed-review-doc --write-path "
      <> shell_quote(dir <> "/review.path"),
    )

  assert artifact.status == step_artifact.StepFailed
  assert string.contains(artifact.stderr, "expected exactly one")
  assert string.contains(artifact.stderr, "found 2")
}

pub fn materialize_pack_discovers_latest_structured_submission_test() {
  let dir = "test/tmp/execplan-structured-latest"
  reset_dir(dir)
  let run_dir = dir <> "/artifacts/runs/run-structured"
  let attempt_1 =
    run_dir
    <> "/incorporate_review/attempt-1/structured/implementation_pack_submission.json"
  let attempt_2 =
    run_dir
    <> "/incorporate_review/attempt-2/structured/implementation_pack_submission.json"
  let assert Ok(Nil) =
    simplifile.create_directory_all(
      run_dir <> "/incorporate_review/attempt-1/structured",
    )
  let assert Ok(Nil) =
    simplifile.create_directory_all(
      run_dir <> "/incorporate_review/attempt-2/structured",
    )
  let assert Ok(Nil) = simplifile.write(attempt_1, pack_submission("Old Pack"))
  let assert Ok(Nil) =
    simplifile.write(attempt_2, pack_submission("Latest Pack"))
  let output = dir <> "/implementation-pack.json"

  let artifact =
    run_shell(
      "env SCHERZO_RUN_ID=run-structured SCHERZO_RUN_ARTIFACT_DIR="
      <> shell_quote(run_dir)
      <> " scripts/scherzo-execplan materialize-pack --review-doc test/fixtures/execplan_v2/review-doc.valid.md --submission-step incorporate_review --submission-artifact implementation_pack_submission --output "
      <> shell_quote(output),
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(pack) = simplifile.read(output)
  assert string.contains(pack, "Latest Pack")
  assert !string.contains(pack, "Old Pack")
}

pub fn publish_review_doc_writes_offline_context_test() {
  let dir = "test/tmp/execplan-publish-context"
  reset_dir(dir)
  let path_file = dir <> "/review.path"
  let context_path = dir <> "/publish-context.json"
  let assert Ok(Nil) =
    simplifile.write(
      path_file,
      "test/fixtures/execplan_v2/review-doc.valid.md\n",
    )

  let artifact =
    run_shell(
      "env SCHERZO_WORKSPACE_DRIVER= SCHERZO_EXECPLAN_OFFLINE_PUBLISH=1 SCHERZO_EXECPLAN_FIXED_TIME=2026-05-15T00:00:00Z SCHERZO_ISSUE_IDENTIFIER=LIV-900 SCHERZO_ISSUE_TITLE="
      <> shell_quote("Offline publish fixture")
      <> " SCHERZO_ISSUE_URL=https://linear.app/living-systems/issue/LIV-900/offline-publish-fixture scripts/scherzo-execplan publish-review-doc --review-doc-path-file "
      <> shell_quote(path_file)
      <> " --publish-context "
      <> shell_quote(context_path),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert string.contains(artifact.stdout, "PUBLISH_REVIEW_DOC_STATUS=published")
  let assert Ok(context) = simplifile.read(context_path)
  assert string.contains(
    context,
    "\"artifact_type\": \"execplan_v2_publish_context\"",
  )
  assert string.contains(context, "\"identifier\": \"LIV-900\"")
  assert string.contains(
    context,
    "\"path\": \"test/fixtures/execplan_v2/review-doc.valid.md\"",
  )
  assert string.contains(context, "\"status\": \"published\"")
}

pub fn materialize_bundle_creates_handoff_with_non_json_linear_create_test() {
  let dir = "test/tmp/execplan-online-linear-create"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let log = dir <> "/linear.log"
  let update_desc = dir <> "/updated-description.md"
  let linear = dir <> "/bin/linear"
  let assert Ok(Nil) =
    simplifile.write(
      linear,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> "
        <> shell_quote(log)
        <> "\n"
        <> "if [ \"$1 $2\" = 'issue query' ]; then printf '%s\\n' '{\"nodes\":[]}'; exit 0; fi\n"
        <> "if [ \"$1 $2 $3\" = 'issue view LIV-314' ]; then printf '%s\\n' '{\"identifier\":\"LIV-314\",\"url\":\"https://linear.app/living-systems/issue/LIV-314/fixture-v2-execplan-bundle\",\"project\":{\"name\":\"Scherzo Core\"}}'; exit 0; fi\n"
        <> "if [ \"$1 $2\" = 'issue create' ]; then\n"
        <> "  for arg in \"$@\"; do if [ \"$arg\" = --json ]; then echo 'unexpected --json' >&2; exit 2; fi; done\n"
        <> "  printf '%s\\n' 'Creating issue in LIV' '' 'https://linear.app/living-systems/issue/LIV-315/implement-fixture-v2-execplan-bundle'\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1 $2 $3\" = 'issue view LIV-315' ]; then printf '%s\\n' '{\"identifier\":\"LIV-315\",\"url\":\"https://linear.app/living-systems/issue/LIV-315/implement-fixture-v2-execplan-bundle\"}'; exit 0; fi\n"
        <> "if [ \"$1 $2 $3\" = 'issue update LIV-315' ]; then\n"
        <> "  desc=''\n"
        <> "  prev=''\n"
        <> "  for arg in \"$@\"; do if [ \"$prev\" = --description-file ]; then desc=$arg; fi; prev=$arg; done\n"
        <> "  if [ -z \"$desc\" ]; then echo 'missing description file' >&2; exit 2; fi\n"
        <> "  cp \"$desc\" "
        <> shell_quote(update_desc)
        <> "\n"
        <> "  grep -Eq '^Bundle sha256: [a-f0-9]{64}$' \"$desc\" || { echo 'missing final bundle sha' >&2; exit 3; }\n"
        <> "  grep -q '^Bundle sha256: pending$' \"$desc\" && { echo 'pending bundle sha' >&2; exit 4; }\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1 $2 $3\" = 'issue comment add' ]; then exit 0; fi\n"
        <> "exit 1\n",
    )
  let chmod = run_shell("chmod +x " <> shell_quote(linear))
  assert chmod.status == step_artifact.StepSucceeded

  let path_file = dir <> "/review.path"
  let context_path = dir <> "/publish-context.json"
  let output = dir <> "/bundle.json"
  let assert Ok(Nil) =
    simplifile.write(
      path_file,
      "test/fixtures/execplan_v2/review-doc.valid.md\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      context_path,
      "{\n"
        <> "  \"artifact_type\": \"execplan_v2_publish_context\",\n"
        <> "  \"source_issue\": {\"identifier\": \"LIV-314\", \"title\": \"Fixture v2 ExecPlan bundle\", \"url\": \"https://linear.app/living-systems/issue/LIV-314/fixture-v2-execplan-bundle\"},\n"
        <> "  \"pr\": {\"url\": \"https://github.com/living-systems/scherzo/pull/314\", \"branch\": \"execplan/liv-314\"},\n"
        <> "  \"review_surface\": {\"status\": \"published\", \"source_bundle_ref\": null}\n"
        <> "}\n",
    )

  let artifact =
    run_shell(
      "env PATH="
      <> shell_quote(dir <> "/bin")
      <> ":$PATH SCHERZO_RUN_ID=run-online scripts/scherzo-execplan materialize-bundle --review-doc-path-file "
      <> shell_quote(path_file)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json --publish-context "
      <> shell_quote(context_path)
      <> " --output "
      <> shell_quote(output),
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(bundle) = simplifile.read(output)
  assert string.contains(bundle, "\"issue_identifier\": \"LIV-315\"")
  assert string.contains(
    bundle,
    "\"issue_url\": \"https://linear.app/living-systems/issue/LIV-315/implement-fixture-v2-execplan-bundle\"",
  )
  let assert Ok(linear_log) = simplifile.read(log)
  assert string.contains(linear_log, "issue create")
  assert string.contains(linear_log, "--project Scherzo Core")
  assert string.contains(linear_log, "issue update LIV-315")
  let assert Ok(updated_description) = simplifile.read(update_desc)
  assert string.contains(
    updated_description,
    "Bundle ref: runs/run-online/outputs/exec_plan_bundle.json",
  )
  assert !string.contains(updated_description, "Bundle sha256: pending")
}

pub fn materialize_revision_reuses_unchanged_review_surface_test() {
  let dir = "test/tmp/execplan-unchanged-revision"
  reset_dir(dir)
  let path_file = dir <> "/review.path"
  let context_path = dir <> "/publish-context.json"
  let output = dir <> "/bundle.json"
  let assert Ok(Nil) =
    simplifile.write(
      path_file,
      "test/fixtures/execplan_v2/review-doc.valid.md\n",
    )

  let publish =
    run_shell(
      "env SCHERZO_WORKSPACE_DRIVER= SCHERZO_EXECPLAN_OFFLINE_PUBLISH=1 SCHERZO_EXECPLAN_FIXED_TIME=2026-05-15T00:00:00Z scripts/scherzo-execplan publish-review-doc --review-doc-path-file "
      <> shell_quote(path_file)
      <> " --publish-context "
      <> shell_quote(context_path)
      <> " --previous-bundle test/fixtures/execplan_v2/exec-plan-bundle.valid.json --skip-if-unchanged",
    )
  assert publish.status == step_artifact.StepSucceeded
  assert string.contains(publish.stdout, "PUBLISH_REVIEW_DOC_STATUS=reused")

  let revision =
    run_shell(
      "env SCHERZO_RUN_ID=run-revision scripts/scherzo-execplan materialize-revision --previous-bundle test/fixtures/execplan_v2/exec-plan-bundle.valid.json --review-doc-path-file "
      <> shell_quote(path_file)
      <> " --pack test/fixtures/execplan_v2/implementation-pack.valid.json --publish-context "
      <> shell_quote(context_path)
      <> " --status auto --output "
      <> shell_quote(output),
    )

  assert revision.status == step_artifact.StepSucceeded
  let assert Ok(bundle) = simplifile.read(output)
  assert string.contains(bundle, "\"status\": \"unchanged\"")
  assert string.contains(
    bundle,
    "\"source_bundle_ref\": \"runs/run-1/outputs/exec_plan_bundle.json\"",
  )
  assert string.contains(
    bundle,
    "\"ref\": \"runs/run-1/outputs/exec_plan_bundle.json\"",
  )
  assert string.contains(
    bundle,
    "\"sha256\": \"e4117164704e943de716797a83f98cd4927833dc0f3b4a179c78c657b25334ec\"",
  )
}

pub fn materialize_code_change_bundle_emits_retained_refs_test() {
  let dir = "test/tmp/execplan-code-change"
  reset_dir(dir)
  let artifact_root = dir <> "/artifacts/runs/run-2"
  let assert Ok(Nil) = simplifile.create_directory_all(artifact_root)
  let assert Ok(Nil) = simplifile.create_directory_all("tmp")
  let assert Ok(Nil) =
    simplifile.write(
      "tmp/scherzo-implementation-publish.json",
      "{\n"
        <> "  \"branch\": \"impl/liv-315\",\n"
        <> "  \"changed_files\": [\"src/example.gleam\"],\n"
        <> "  \"driver_head_revision\": \"head-rev\",\n"
        <> "  \"pr_url\": \"https://github.com/living-systems/scherzo/pull/315\",\n"
        <> "  \"publish_base_revision\": \"base-rev\"\n"
        <> "}\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      "tmp/scherzo-implementation-validation.json",
      "{\"status\":\"passed\"}\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      "tmp/scherzo-plan-completion-verdict.json",
      "{\"verdict\":\"pass\"}\n",
    )
  let output = dir <> "/code-change-bundle.json"

  let artifact =
    run_shell(
      "env SCHERZO_RUN_ID=run-2 SCHERZO_RUN_ARTIFACT_DIR="
      <> shell_quote(artifact_root)
      <> " SCHERZO_EXECPLAN_DIFF_PATH=test/fixtures/execplan_v2/artifacts/runs/run-2/execplan/code-change/diff.patch scripts/scherzo-execplan materialize-code-change-bundle --bundle test/fixtures/execplan_v2/exec-plan-bundle.valid.json --output "
      <> shell_quote(output),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(bundle) = simplifile.read(output)
  assert string.contains(bundle, "\"artifact_type\": \"code_change_bundle\"")
  assert string.contains(bundle, "runs/run-2/execplan/code-change/diff.patch")
  assert string.contains(bundle, "\"verdict\": \"complete\"")
}
