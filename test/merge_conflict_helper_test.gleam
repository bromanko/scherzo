import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/step_artifact
import simplifile
import workflow_context_test_support

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

fn run_helper(command: String) -> step_artifact.StepArtifact {
  command_step.run(
    "helper",
    workflow_context_test_support.without_workflow_context(
      ".scherzo/workflows/scripts/scherzo-merge-conflict " <> command,
    ),
    ".",
    5000,
    [],
    limits(),
  )
}

fn run_helper_in(cwd: String, command: String) -> step_artifact.StepArtifact {
  command_step.run(
    "helper",
    workflow_context_test_support.without_workflow_context(command),
    cwd,
    10_000,
    [],
    limits(),
  )
}

fn chmod_executable(path: String) -> Nil {
  let artifact =
    command_step.run("chmod", "chmod +x " <> path, ".", 5000, [], limits())
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
}

pub fn extract_target_accepts_local_pr_reference_test() {
  let dir = "test/tmp/merge-conflict-extract-pr"
  reset_dir(dir)
  let text_path = dir <> "/issue.txt"
  let assert Ok(Nil) =
    simplifile.write(text_path, "Please resolve conflicts for PR #51.\n")

  let artifact =
    run_helper("extract-target " <> text_path <> " scherzo-systems/scherzo")

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "TARGET_KIND=pr")
  assert string.contains(artifact.stdout, "REPO=scherzo-systems/scherzo")
  assert string.contains(artifact.stdout, "PR_NUMBER=51")
}

pub fn extract_target_prefers_issue_fields_over_diagnostic_comments_test() {
  let dir = "test/tmp/merge-conflict-extract-pr-with-diagnostic-comment"
  reset_dir(dir)
  let text_path = dir <> "/issue.json"
  let assert Ok(Nil) =
    simplifile.write(
      text_path,
      "{\n"
        <> "  \"title\": \"Resolve conflicts for PR #31\",\n"
        <> "  \"description\": \"https://github.com/scherzo-systems/scherzo/pull/31\\n\",\n"
        <> "  \"comments\": {\"nodes\": [\n"
        <> "    {\"createdAt\": \"2026-05-05T01:31:05.807Z\", \"body\": \"Parent commit: Merge pull request #29 from scherzo-systems/scherzo/old\", \"user\": {\"name\": \"Bromanko Agent\"}}\n"
        <> "  ]}\n"
        <> "}\n",
    )

  let artifact =
    run_helper(
      "extract-target-issue " <> text_path <> " scherzo-systems/scherzo",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "TARGET_KIND=pr")
  assert string.contains(artifact.stdout, "REPO=scherzo-systems/scherzo")
  assert string.contains(artifact.stdout, "PR_NUMBER=31")
}

pub fn extract_target_accepts_explicit_branch_line_test() {
  let dir = "test/tmp/merge-conflict-extract-branch"
  reset_dir(dir)
  let text_path = dir <> "/issue.txt"
  let assert Ok(Nil) =
    simplifile.write(text_path, "Branch: feature/conflicted-branch\n")

  let artifact =
    run_helper("extract-target " <> text_path <> " scherzo-systems/scherzo")

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "TARGET_KIND=branch")
  assert string.contains(artifact.stdout, "REPO=scherzo-systems/scherzo")
  assert string.contains(artifact.stdout, "BRANCH=feature/conflicted-branch")
}

pub fn extract_target_rejects_ambiguous_pr_and_branch_test() {
  let dir = "test/tmp/merge-conflict-extract-ambiguous"
  reset_dir(dir)
  let text_path = dir <> "/issue.txt"
  let assert Ok(Nil) =
    simplifile.write(
      text_path,
      "Resolve PR #51.\n\nBranch: feature/conflicted-branch\n",
    )

  let artifact =
    run_helper("extract-target " <> text_path <> " scherzo-systems/scherzo")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stderr, "multiple PR/branch targets found")
}

pub fn validate_rejects_non_conflicted_file_changes_test() {
  let dir = "test/tmp/merge-conflict-validate-drift"
  write_validation_fixture(dir, "changed\n")

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-merge-conflict validate --skip-project-validation",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(
    artifact.stderr,
    "non-conflicted tracked files changed",
  )
  assert string.contains(artifact.stderr, "modified: safe.txt")
}

pub fn validate_accepts_manifested_mechanical_non_conflicted_file_change_test() {
  let dir = "test/tmp/merge-conflict-validate-mechanical-fallout"
  write_validation_fixture(dir, "changed\n")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-merge-conflict-mechanical-edits.json",
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"non_conflicted_edits\": [\n"
        <> "    {\"path\": \"safe.txt\", \"reason\": \"Mechanical callback arity update after resolved source API changed; behavior unchanged.\"}\n"
        <> "  ]\n"
        <> "}\n",
    )

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-merge-conflict validate --skip-project-validation",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "VALIDATION=ok")
  assert string.contains(artifact.stdout, "MECHANICAL_NON_CONFLICTED_EDITS=1")
  let assert Ok(validation) =
    simplifile.read(dir <> "/tmp/scherzo-merge-conflict-validation.json")
  assert string.contains(validation, "\"path\": \"safe.txt\"")
}

pub fn validate_accepts_resolved_conflicts_when_only_conflicted_files_changed_test() {
  let dir = "test/tmp/merge-conflict-validate-ok"
  write_validation_fixture(dir, "safe\n")

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-merge-conflict validate --skip-project-validation",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "VALIDATION=ok")
  assert string.contains(artifact.stdout, "RESOLUTION_STATUS=resolved")
  assert string.contains(
    artifact.stdout,
    "PROJECT_VALIDATION=external_required",
  )
}

pub fn validate_does_not_run_repo_specific_project_validation_test() {
  let dir = "test/tmp/merge-conflict-validate-no-project-validation"
  write_validation_fixture(dir, "safe\n")
  write_failing_direnv(dir <> "/bin/direnv")
  chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-merge-conflict validate",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "PROJECT_VALIDATION=external_required",
  )
  let assert Ok(False) = simplifile.is_file(dir <> "/direnv.log")
}

pub fn record_project_validation_marks_external_validation_passed_test() {
  let dir = "test/tmp/merge-conflict-record-project-validation"
  write_validation_fixture(dir, "safe\n")

  let validate =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-merge-conflict validate",
    )
  assert validate.status == step_artifact.StepSucceeded

  let record =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-merge-conflict record-project-validation",
    )

  assert record.status == step_artifact.StepSucceeded
  assert record.exit_code == Some(0)
  assert string.contains(record.stdout, "PROJECT_VALIDATION=passed")
  let assert Ok(validation) =
    simplifile.read(dir <> "/tmp/scherzo-merge-conflict-validation.json")
  assert string.contains(validation, "\"project_validation\": \"passed\"")
}

pub fn run_project_validation_scrubs_outer_workflow_context_test() {
  let dir = "test/tmp/merge-conflict-run-project-validation-env-clean"
  write_validation_fixture(dir, "safe\n")
  write_fake_project_validation_with_leak_guard(
    dir <> "/bin/project-validation",
  )
  chmod_executable(dir <> "/bin/project-validation")

  let validate =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-merge-conflict validate",
    )
  assert validate.status == step_artifact.StepSucceeded

  let artifact =
    run_helper_in(
      dir,
      outer_workflow_context_env()
        <> "PATH=\"$PWD/bin:$PATH\" "
        <> "../../../.scherzo/workflows/scripts/scherzo-merge-conflict run-project-validation -- ./bin/project-validation",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "PROJECT_VALIDATION=passed")
  let assert Ok(validation_log) =
    simplifile.read(dir <> "/project-validation.log")
  assert string.contains(validation_log, "project validation ran")
  let assert Ok(validation) =
    simplifile.read(dir <> "/tmp/scherzo-merge-conflict-validation.json")
  assert string.contains(validation, "\"project_validation\": \"passed\"")
}

pub fn validation_status_reports_validated_resolution_state_test() {
  let dir = "test/tmp/merge-conflict-validation-status"
  write_validation_fixture(dir, "safe\n")

  let validate =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-merge-conflict validate",
    )
  assert validate.status == step_artifact.StepSucceeded

  let status =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-merge-conflict validation-status",
    )

  assert status.status == step_artifact.StepSucceeded
  assert status.exit_code == Some(0)
  assert status.stdout == "resolved\n"
}

pub fn project_validation_wrapper_skips_command_when_no_conflicts_test() {
  let dir = "test/tmp/merge-conflict-project-validation-no-conflicts"
  write_no_conflicts_validation_fixture(dir)

  let validate =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-merge-conflict validate",
    )
  assert validate.status == step_artifact.StepSucceeded
  assert string.contains(validate.stdout, "RESOLUTION_STATUS=no_conflicts")

  let status =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-merge-conflict validation-status",
    )
  assert status.status == step_artifact.StepSucceeded
  assert status.stdout == "no_conflicts\n"

  let wrapper =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-merge-conflict run-project-validation -- ./bin/should-not-run",
    )

  assert wrapper.status == step_artifact.StepSucceeded
  assert wrapper.exit_code == Some(0)
  assert string.contains(wrapper.stdout, "PROJECT_VALIDATION=not_needed")
}

pub fn publish_requires_recorded_project_validation_test() {
  let dir = "test/tmp/merge-conflict-publish-project-validation-gate"
  write_validation_fixture(dir, "safe\n")
  write_fake_workspace_driver(dir <> "/bin/workspace-driver")
  chmod_executable(dir <> "/bin/workspace-driver")

  let validate =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-merge-conflict validate",
    )
  assert validate.status == step_artifact.StepSucceeded

  let blocked =
    run_helper_in(
      dir,
      "SCHERZO_WORKSPACE_DRIVER=./bin/workspace-driver "
        <> "PATH=\"$PWD/bin:$PATH\" "
        <> "../../../.scherzo/workflows/scripts/scherzo-merge-conflict publish",
    )

  assert blocked.status == step_artifact.StepFailed
  assert blocked.exit_code == Some(1)
  assert string.contains(
    blocked.stderr,
    "repo-local project validation has not been recorded",
  )
  let assert Ok(False) = simplifile.is_file(dir <> "/workspace-driver.log")

  let record =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-merge-conflict record-project-validation",
    )
  assert record.status == step_artifact.StepSucceeded

  let published =
    run_helper_in(
      dir,
      "SCHERZO_WORKSPACE_DRIVER=./bin/workspace-driver "
        <> "PATH=\"$PWD/bin:$PATH\" "
        <> "../../../.scherzo/workflows/scripts/scherzo-merge-conflict publish",
    )

  assert published.status == step_artifact.StepSucceeded
  assert published.exit_code == Some(0)
  assert string.contains(published.stdout, "PUSHED=true")
  assert string.contains(published.stdout, "PROJECT_VALIDATION=passed")
  let assert Ok(driver_log) = simplifile.read(dir <> "/workspace-driver.log")
  assert string.contains(driver_log, "publish-change")
}

pub fn validate_accepts_prepare_metadata_with_jj_conflict_status_suffix_test() {
  let dir = "test/tmp/merge-conflict-validate-status-suffix"
  write_validation_fixture(dir, "safe\n")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-merge-conflict.json",
      "{\n"
        <> "  \"linear_issue_identifier\": \"LIV-123\",\n"
        <> "  \"repo\": \"scherzo-systems/scherzo\",\n"
        <> "  \"remote\": \"origin\",\n"
        <> "  \"target_kind\": \"branch\",\n"
        <> "  \"head_branch\": \"feature/conflicted-branch\",\n"
        <> "  \"base_branch\": \"main\",\n"
        <> "  \"conflicted_files\": [\"conflicted.txt      2-sided conflict including an executable\"],\n"
        <> "  \"non_conflict_fingerprints\": {\n"
        <> "    \"conflicted.txt\": {\"type\": \"file\", \"sha256\": \"stale\", \"size\": 0},\n"
        <> "    \"safe.txt\": {\"type\": \"file\", \"sha256\": \"93d868f3b59590f611d7646894ce8def1cea5ad63a9af0d9ccc56e9bc6968c11\", \"size\": 5}\n"
        <> "  }\n"
        <> "}\n",
    )

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-merge-conflict validate --skip-project-validation",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "VALIDATION=ok")
}

pub fn validate_reports_unresolved_conflict_path_without_jj_status_suffix_test() {
  let dir = "test/tmp/merge-conflict-validate-unresolved"
  write_validation_fixture(dir, "safe\n")
  write_fake_unresolved_conflict_jj(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/jj")

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-merge-conflict validate --skip-project-validation",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(
    artifact.stderr,
    "unresolved conflicts remain: conflicted.txt",
  )
  assert !string.contains(artifact.stderr, "2-sided conflict")
}

pub fn checked_in_merge_conflict_workflow_is_routed_and_guarded_test() {
  let assert Ok(config) = simplifile.read(".scherzo/scherzo.yaml")
  let assert Ok(workflow) =
    simplifile.read(".scherzo/workflows/merge-conflict-resolution.yaml")
  let assert Ok(prompt) =
    simplifile.read(".scherzo/workflows/prompts/resolve-merge-conflicts.md")

  assert string.contains(
    config,
    "merge-conflict-resolution: workflows/merge-conflict-resolution.yaml",
  )
  assert string.contains(config, "merge-conflict-resolution]")
  assert string.contains(workflow, "id: merge-conflict-resolution")
  assert string.contains(workflow, "scripts/scherzo-merge-conflict\" prepare")
  assert string.contains(workflow, "scripts/scherzo-merge-conflict\" validate")
  assert string.contains(workflow, "scripts/scherzo-merge-conflict\" publish")
  assert string.contains(workflow, "from: main")
  assert string.contains(prompt, "tmp/scherzo-merge-conflict-failure.md")
  assert string.contains(
    prompt,
    "Edit only files listed under `CONFLICTED_FILES`",
  )
  assert string.contains(prompt, "scherzo-merge-conflict-mechanical-edits.json")
}

fn write_validation_fixture(dir: String, safe_contents: String) -> Nil {
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.write(dir <> "/conflicted.txt", "resolved\n")
  let assert Ok(Nil) = simplifile.write(dir <> "/safe.txt", safe_contents)
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-merge-conflict-resolution.md",
      "# Merge conflict resolution summary\n\nResolved.\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-merge-conflict.json",
      "{\n"
        <> "  \"linear_issue_identifier\": \"LIV-123\",\n"
        <> "  \"repo\": \"scherzo-systems/scherzo\",\n"
        <> "  \"remote\": \"origin\",\n"
        <> "  \"target_kind\": \"branch\",\n"
        <> "  \"head_branch\": \"feature/conflicted-branch\",\n"
        <> "  \"base_branch\": \"main\",\n"
        <> "  \"conflicted_files\": [\"conflicted.txt\"],\n"
        <> "  \"non_conflict_fingerprints\": {\n"
        <> "    \"safe.txt\": {\"type\": \"file\", \"sha256\": \"93d868f3b59590f611d7646894ce8def1cea5ad63a9af0d9ccc56e9bc6968c11\", \"size\": 5}\n"
        <> "  }\n"
        <> "}\n",
    )
  write_fake_validation_jj(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/jj")
}

fn write_no_conflicts_validation_fixture(dir: String) -> Nil {
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.write(dir <> "/conflicted.txt", "resolved\n")
  let assert Ok(Nil) = simplifile.write(dir <> "/safe.txt", "safe\n")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-merge-conflict.json",
      "{\n"
        <> "  \"linear_issue_identifier\": \"LIV-123\",\n"
        <> "  \"repo\": \"scherzo-systems/scherzo\",\n"
        <> "  \"remote\": \"origin\",\n"
        <> "  \"target_kind\": \"branch\",\n"
        <> "  \"head_branch\": \"feature/conflicted-branch\",\n"
        <> "  \"base_branch\": \"main\",\n"
        <> "  \"conflicted_files\": [],\n"
        <> "  \"non_conflict_fingerprints\": {\n"
        <> "    \"conflicted.txt\": {\"type\": \"file\", \"sha256\": \"3a6b975479a644e01da8a06ae3df67f52785abb2c35bf359efdfe40adea1da8c\", \"size\": 9},\n"
        <> "    \"safe.txt\": {\"type\": \"file\", \"sha256\": \"93d868f3b59590f611d7646894ce8def1cea5ad63a9af0d9ccc56e9bc6968c11\", \"size\": 5}\n"
        <> "  }\n"
        <> "}\n",
    )
  write_fake_validation_jj(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/jj")
}

fn outer_workflow_context_env() -> String {
  "SCHERZO_CONFIG_DIR=/outer/config "
  <> "SCHERZO_ISSUE_ID=issue-id "
  <> "SCHERZO_ISSUE_IDENTIFIER=LIV-123 "
  <> "SCHERZO_RUN_ID=run-id "
  <> "SCHERZO_RUN_ROOT= "
  <> "SCHERZO_SOURCE_WORKSPACE_PATH=/outer/source "
  <> "SCHERZO_WORKFLOW_ID=merge-conflict-resolution "
  <> "SCHERZO_WORKSPACE_CAPABILITIES=status,diff "
  <> "SCHERZO_WORKSPACE_DRIVER=/outer/driver "
  <> "SCHERZO_WORKSPACE_NAME=main "
  <> "SCHERZO_WORKSPACE_PATH=/outer/workspace "
  <> "SCHERZO_WORKSPACE_PROFILE=dogfood-jj "
  <> "SCHERZO_WORKSPACE_ROOT=/outer/workspaces "
  <> "SCHERZO_REPO_ROOT=/outer/repo "
  <> "SCHERZO_JJ_WORKSPACE_REMOTE=scherzo-agent "
  <> "SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE=scherzo-agent "
  <> "SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main "
  <> "SCHERZO_JJ_WORKSPACE_FETCH_BASE=true "
  <> "SCHERZO_PR_REMOTE=origin "
  <> "SCHERZO_PR_BASE=main "
  <> "SCHERZO_PR_REPO=example/repo "
}

fn write_failing_direnv(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> direnv.log\n"
        <> "echo 'direnv should not be called by generic merge-conflict validate' >&2\n"
        <> "exit 2\n",
    )
  Nil
}

fn write_fake_project_validation_with_leak_guard(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "set -eu\n"
        <> "echo 'project validation ran' >> project-validation.log\n"
        <> "if env | grep -E '^(SCHERZO_CONFIG_DIR|SCHERZO_ISSUE_ID|SCHERZO_ISSUE_IDENTIFIER|SCHERZO_JJ_WORKSPACE_BASE|SCHERZO_JJ_WORKSPACE_BASE_BRANCH|SCHERZO_JJ_WORKSPACE_FETCH_BASE|SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE|SCHERZO_JJ_WORKSPACE_REMOTE|SCHERZO_PR_BASE|SCHERZO_PR_REMOTE|SCHERZO_PR_REPO|SCHERZO_REPO_ROOT|SCHERZO_RUN_ID|SCHERZO_RUN_ROOT|SCHERZO_SOURCE_WORKSPACE_PATH|SCHERZO_WORKFLOW_ID|SCHERZO_WORKSPACE_CAPABILITIES|SCHERZO_WORKSPACE_DRIVER|SCHERZO_WORKSPACE_NAME|SCHERZO_WORKSPACE_PATH|SCHERZO_WORKSPACE_PROFILE|SCHERZO_WORKSPACE_ROOT)=' >/dev/null; then\n"
        <> "  echo 'SCHERZO workflow context leaked into validation' >&2\n"
        <> "  env | grep -E '^(SCHERZO_CONFIG_DIR|SCHERZO_ISSUE_ID|SCHERZO_ISSUE_IDENTIFIER|SCHERZO_JJ_WORKSPACE_BASE|SCHERZO_JJ_WORKSPACE_BASE_BRANCH|SCHERZO_JJ_WORKSPACE_FETCH_BASE|SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE|SCHERZO_JJ_WORKSPACE_REMOTE|SCHERZO_PR_BASE|SCHERZO_PR_REMOTE|SCHERZO_PR_REPO|SCHERZO_REPO_ROOT|SCHERZO_RUN_ID|SCHERZO_RUN_ROOT|SCHERZO_SOURCE_WORKSPACE_PATH|SCHERZO_WORKFLOW_ID|SCHERZO_WORKSPACE_CAPABILITIES|SCHERZO_WORKSPACE_DRIVER|SCHERZO_WORKSPACE_NAME|SCHERZO_WORKSPACE_PATH|SCHERZO_WORKSPACE_PROFILE|SCHERZO_WORKSPACE_ROOT)=' >&2\n"
        <> "  exit 1\n"
        <> "fi\n",
    )
  Nil
}

fn write_fake_workspace_driver(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> workspace-driver.log\n"
        <> "if [ \"$1\" = publish-change ]; then\n"
        <> "  printf '%s\\n' '{\"version\":1,\"head_revision\":\"abc123\"}'\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "echo \"unexpected workspace driver command: $*\" >&2\n"
        <> "exit 2\n",
    )
  Nil
}

fn write_fake_unresolved_conflict_jj(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> jj.log\n"
        <> "if [ \"$1\" = resolve ]; then echo 'conflicted.txt      2-sided conflict including an executable'; exit 0; fi\n"
        <> "if [ \"$1 $2\" = 'file list' ]; then printf '%s\\n' conflicted.txt safe.txt; exit 0; fi\n"
        <> "exit 1\n",
    )
  Nil
}

fn write_fake_validation_jj(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> jj.log\n"
        <> "if [ \"$1\" = resolve ]; then echo 'Error: No conflicts found at this revision' >&2; exit 2; fi\n"
        <> "if [ \"$1 $2\" = 'file list' ]; then printf '%s\\n' conflicted.txt safe.txt; exit 0; fi\n"
        <> "exit 1\n",
    )
  Nil
}
