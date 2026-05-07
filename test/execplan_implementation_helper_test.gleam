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

fn run_helper(command: String) -> step_artifact.StepArtifact {
  command_step.run(
    "helper",
    "scripts/scherzo-implementation " <> command,
    ".",
    5000,
    [],
    limits(),
  )
}

fn run_helper_in(cwd: String, command: String) -> step_artifact.StepArtifact {
  command_step.run("helper", command, cwd, 10_000, [], limits())
}

fn chmod_executable(path: String) -> Nil {
  let artifact =
    command_step.run("chmod", "chmod +x " <> path, ".", 5000, [], limits())
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
}

fn read_or_empty(path: String) -> String {
  case simplifile.read(path) {
    Ok(contents) -> contents
    Error(_) -> ""
  }
}

pub fn extract_plan_requires_exactly_one_existing_plan_path_test() {
  let dir = "test/tmp/execplan-helper-extract"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/docs/plans/example-plan.md", "# Example\n")
  let text_path = dir <> "/issue.txt"
  let assert Ok(Nil) =
    simplifile.write(
      text_path,
      "Please implement `docs/plans/example-plan.md`.\n",
    )

  let artifact = run_helper("extract-plan " <> text_path <> " " <> dir)

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "PLAN_PATH=docs/plans/example-plan.md",
  )
}

pub fn extract_plan_prefers_explicit_plan_field_over_liv59_context_references_test() {
  let dir = "test/tmp/execplan-helper-explicit-plan"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/docs/plans/LIV-59-implementation.md",
      "# Implementation\n",
    )
  let text_path = dir <> "/issue.txt"
  let assert Ok(Nil) =
    simplifile.write(
      text_path,
      "Umbrella: `docs/plans/LIV-59-umbrella.md`\n"
        <> "Plan path: `docs/plans/LIV-59-implementation.md`\n"
        <> "Supersedes: `docs/plans/LIV-59-old.md`\n",
    )

  let artifact = run_helper("extract-plan " <> text_path <> " " <> dir)

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "PLAN_PATH=docs/plans/LIV-59-implementation.md",
  )
}

pub fn extract_plan_fallback_ignores_contextual_plan_references_test() {
  let dir = "test/tmp/execplan-helper-contextual-fallback"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/docs/plans/implementation.md", "# Impl\n")
  let text_path = dir <> "/issue.txt"
  let assert Ok(Nil) =
    simplifile.write(
      text_path,
      "Please implement docs/plans/implementation.md.\n"
        <> "Umbrella: `docs/plans/umbrella.md`\n"
        <> "Supersedes: `docs/plans/old.md`\n",
    )

  let artifact = run_helper("extract-plan " <> text_path <> " " <> dir)

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "PLAN_PATH=docs/plans/implementation.md",
  )
}

pub fn extract_plan_rejects_multiple_explicit_plan_fields_test() {
  let dir = "test/tmp/execplan-helper-multiple-explicit"
  reset_dir(dir)
  let text_path = dir <> "/issue.txt"
  let assert Ok(Nil) =
    simplifile.write(
      text_path,
      "Plan: `docs/plans/one.md`\n" <> "Plan path: `docs/plans/two.md`\n",
    )

  let artifact = run_helper("extract-plan " <> text_path <> " " <> dir)

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(
    artifact.stderr,
    "found multiple explicit ExecPlan fields",
  )
  assert string.contains(artifact.stderr, "- `docs/plans/one.md`")
  assert string.contains(artifact.stderr, "- `docs/plans/two.md`")
  assert string.contains(artifact.stderr, "Suggested fix")
}

pub fn extract_plan_rejects_ambiguous_plan_paths_test() {
  let dir = "test/tmp/execplan-helper-ambiguous"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) = simplifile.write(dir <> "/docs/plans/one.md", "# One\n")
  let assert Ok(Nil) = simplifile.write(dir <> "/docs/plans/two.md", "# Two\n")
  let text_path = dir <> "/issue.txt"
  let assert Ok(Nil) =
    simplifile.write(
      text_path,
      "Compare docs/plans/one.md and docs/plans/two.md.\n",
    )

  let artifact = run_helper("extract-plan " <> text_path <> " " <> dir)

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("prepare_plan_ambiguous")
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=prepare_plan_ambiguous",
  )
  assert string.contains(
    artifact.stderr,
    "found multiple ExecPlan path candidates",
  )
  assert string.contains(artifact.stderr, "Suggested fix")
}

pub fn prepare_execplan_failure_writes_retention_marker_before_fetch_test() {
  let dir = "test/tmp/implementation-helper-prepare-retention"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/main")
  write_fake_prepare_jj(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/jj")

  let artifact =
    run_helper_in(
      dir <> "/main",
      "SCHERZO_ISSUE_IDENTIFIER=LIV-71 LINEAR_API_KEY= PATH=\"$PWD/../bin:$PATH\" ../../../../scripts/scherzo-implementation prepare --source execplan",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stderr, "LINEAR_API_KEY is required")
  let assert Ok(marker) = simplifile.read(dir <> "/.scherzo-keep-workspace")
  assert string.contains(marker, "Source kind: execplan")
  assert string.contains(marker, "Source: LIV-71")
}

pub fn prepare_command_failure_reports_bounded_diagnostic_excerpt_test() {
  let dir = "test/tmp/implementation-helper-bounded-diagnostics"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/main")
  write_noisy_failing_prepare_jj(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/jj")

  let artifact =
    run_helper_in(
      dir <> "/main",
      "SCHERZO_ISSUE_IDENTIFIER=LIV-71 LINEAR_API_KEY= PATH=\"$PWD/../bin:$PATH\" ../../../../scripts/scherzo-implementation prepare --source execplan",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stderr, "exit_code: 2")
  assert string.contains(artifact.stderr, "truncated")
  assert string.length(artifact.stderr) < 4000
  let assert Ok(marker) = simplifile.read(dir <> "/.scherzo-keep-workspace")
  assert string.contains(marker, "Source: LIV-71")
}

pub fn languages_detects_gleam_and_reports_unsupported_files_test() {
  let dir = "test/tmp/execplan-helper-languages"
  reset_dir(dir)
  let file_list = dir <> "/files.txt"
  let assert Ok(Nil) =
    simplifile.write(
      file_list,
      "src/scherzo/config.gleam\nsrc/scherzo_config_ffi.erl\ndocs/notes.md\n",
    )

  let artifact = run_helper("languages " <> file_list)

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "LANGUAGES=gleam")
  assert string.contains(artifact.stdout, "- src/scherzo/config.gleam")
  assert string.contains(artifact.stdout, "- src/scherzo_config_ffi.erl")
  assert string.contains(artifact.stdout, "/review gleam --fix medium")
}

pub fn ticket_brief_renders_linear_context_test() {
  let dir = "test/tmp/implementation-helper-ticket-brief"
  reset_dir(dir)
  let issue_json = dir <> "/issue.json"
  let assert Ok(Nil) =
    simplifile.write(
      issue_json,
      "{\n"
        <> "  \"identifier\": \"SCH-123\",\n"
        <> "  \"title\": \"Implement generic workflow\",\n"
        <> "  \"url\": \"https://linear.app/example/issue/SCH-123\",\n"
        <> "  \"priority\": 2,\n"
        <> "  \"state\": {\"name\": \"Todo\"},\n"
        <> "  \"description\": \"Implement from ticket context.\",\n"
        <> "  \"labels\": {\"nodes\": [{\"name\": \"workflow:implementation\"}], \"pageInfo\": {\"hasNextPage\": false}},\n"
        <> "  \"comments\": {\"nodes\": [\n"
        <> "    {\"createdAt\": \"2026-05-02T12:00:00Z\", \"body\": \"Second comment\", \"user\": {\"name\": \"Bob\"}},\n"
        <> "    {\"createdAt\": \"2026-05-02T11:00:00Z\", \"body\": \"First comment\", \"user\": {\"name\": \"Ada\"}}\n"
        <> "  ], \"pageInfo\": {\"hasNextPage\": false}}\n"
        <> "}\n",
    )

  let artifact = run_helper("ticket-brief " <> issue_json)

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "# Ticket context for SCH-123: Implement generic workflow",
  )
  assert string.contains(artifact.stdout, "- Labels: workflow:implementation")
  assert string.contains(artifact.stdout, "Implement from ticket context.")
  assert string.contains(
    artifact.stdout,
    "### Comment 1 — 2026-05-02T11:00:00Z — Ada",
  )
  assert string.contains(artifact.stdout, "First comment")
  assert string.contains(
    artifact.stdout,
    "### Comment 2 — 2026-05-02T12:00:00Z — Bob",
  )
}

pub fn jj_workspace_hook_prefers_configured_remote_base_for_new_root_workspaces_test() {
  let assert Ok(script) = simplifile.read("scripts/scherzo-jj-workspace")
  assert string.contains(script, "SCHERZO_JJ_WORKSPACE_BASE")
  assert string.contains(script, "default_base=${SCHERZO_PR_BASE:-main}")
  assert string.contains(script, "default_remote=${SCHERZO_PR_REMOTE:-origin}")
  assert string.contains(
    script,
    "remote_base=\"${default_base}@${default_remote}\"",
  )
  assert string.contains(script, "elif revision_exists \"$remote_base\"")
  assert string.contains(script, "--revision \"$base_revision\"")
}

pub fn validate_unsets_scherzo_run_root_for_nested_helper_tests_test() {
  let dir = "test/tmp/implementation-helper-validate-env"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_fake_direnv(dir <> "/bin/direnv")
  chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=/outer/run/root SCHERZO_FAIL_IF_RUN_ROOT_LEAKS=1 SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation validate",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "FINAL_VALIDATION=passed")
  let assert Ok(direnv_log) = simplifile.read(dir <> "/direnv.log")
  assert string.contains(direnv_log, "allow .")
  assert string.contains(
    direnv_log,
    "exec . selfci check --base main@origin --candidate @ --print-output",
  )
  assert !string.contains(direnv_log, "exec . gleam format --check src test")
  assert !string.contains(direnv_log, "exec . gleam test")
  let assert Ok(validation_json) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-validation.json")
  assert string.contains(validation_json, "\"status\": \"passed\"")
  assert string.contains(validation_json, "\"validator\": \"selfci\"")
  assert string.contains(validation_json, "\"base_revision\": \"main@origin\"")
  assert string.contains(
    validation_json,
    "direnv exec . selfci check --base main@origin --candidate @ --print-output",
  )
}

pub fn validate_uses_latest_refresh_base_revision_for_selfci_test() {
  let dir = "test/tmp/implementation-helper-validate-refresh-base"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation-refresh-base-latest.json",
      "{\"base_revision\":\"feature-base@origin\"}\n",
    )
  write_fake_direnv(dir <> "/bin/direnv")
  chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation validate",
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(direnv_log) = simplifile.read(dir <> "/direnv.log")
  assert string.contains(
    direnv_log,
    "exec . selfci check --base feature-base@origin --candidate @ --print-output",
  )
  let assert Ok(validation_json) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-validation.json")
  assert string.contains(
    validation_json,
    "\"base_revision\": \"feature-base@origin\"",
  )
}

pub fn publish_rebases_to_remote_base_and_revalidates_test() {
  let dir = "test/tmp/implementation-helper-publish-normalize"
  reset_dir(dir)
  write_publish_fixture_metadata(dir)
  write_fake_jj(dir <> "/bin/jj")
  write_fake_gh(dir <> "/bin/gh")
  write_fake_direnv(dir <> "/bin/direnv")
  chmod_executable(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/gh")
  chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=\"$PWD\" SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation publish",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "Publish base normalization")
  assert string.contains(
    artifact.stdout,
    "Revalidation after publish-base normalization",
  )
  assert string.contains(
    artifact.stdout,
    "PR_URL=https://github.com/example/repo/pull/123",
  )
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert string.contains(jj_log, "git fetch --remote origin --branch main")
  assert string.contains(jj_log, "rebase -r @ -d main@origin --color=never")
  assert string.contains(jj_log, "diff --from main@origin --to @ --name-only")
  let assert Ok(direnv_log) = simplifile.read(dir <> "/direnv.log")
  assert string.contains(
    direnv_log,
    "exec . selfci check --base main@origin --candidate @ --print-output",
  )
  assert !string.contains(direnv_log, "exec . gleam format --check src test")
  assert !string.contains(direnv_log, "exec . gleam test")
  assert string.contains(
    artifact.stdout,
    "`direnv exec . selfci check --base main@origin --candidate @ --print-output`: passed",
  )
  let assert Ok(body) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-pr-body.md")
  assert string.contains(body, "SelfCI validation completed before publication")
  assert string.contains(
    body,
    "`direnv exec . selfci check --base main@origin --candidate @ --print-output`",
  )
  let assert Ok(publish_json) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-publish.json")
  assert string.contains(
    publish_json,
    "\"publish_base_revision\": \"main@origin\"",
  )
}

pub fn execplan_implementation_publish_mentions_linear_issue_in_pr_metadata_test() {
  let dir = "test/tmp/execplan-implementation-publish-linking"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/docs/plans/LIV-58-workflow-recovery-operator-ux-retention.md",
      "# Make workflow recovery visible and safe for operators\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation.json",
      "{\n"
        <> "  \"source_kind\": \"execplan\",\n"
        <> "  \"issue_identifier\": \"LIV-65\",\n"
        <> "  \"issue_title\": \"Implement plan: workflow recovery operator UX and retention\",\n"
        <> "  \"issue_url\": \"https://linear.example/LIV-65\",\n"
        <> "  \"plan_path\": \"docs/plans/LIV-58-workflow-recovery-operator-ux-retention.md\",\n"
        <> "  \"base_change_id\": \"local-start\"\n"
        <> "}\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation-validation.json",
      "{\"status\": \"passed\", \"commands\": []}\n",
    )
  write_fake_jj(dir <> "/bin/jj")
  write_fake_gh(dir <> "/bin/gh")
  write_fake_direnv(dir <> "/bin/direnv")
  chmod_executable(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/gh")
  chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=\"$PWD\" SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation publish",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert string.contains(jj_log, "describe -m feat: implement liv-65")
  assert string.contains(
    jj_log,
    "bookmark set scherzo/execplan-implementation/liv-65-implement-plan",
  )
  assert !string.contains(jj_log, "scherzo/execplan-implementation/liv-58")
  let assert Ok(gh_log) = simplifile.read(dir <> "/gh.log")
  assert string.contains(
    gh_log,
    "--head scherzo/execplan-implementation/liv-65-implement-plan",
  )
  assert string.contains(
    gh_log,
    "--title Implement LIV-65: workflow recovery operator UX and retention",
  )
  assert !string.contains(gh_log, "liv-58-workflow-recovery")
  let assert Ok(body) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-pr-body.md")
  assert string.contains(
    body,
    "Issue: [LIV-65: Implement plan: workflow recovery operator UX and retention](https://linear.example/LIV-65)",
  )
  assert string.contains(
    body,
    "Source ExecPlan: `docs/plans/LIV-58-workflow-recovery-operator-ux-retention.md`",
  )
  assert string.contains(body, "SelfCI validation completed before publication")
  assert string.contains(
    body,
    "`direnv exec . selfci check --base main@origin --candidate @ --print-output`",
  )
  let assert Ok(publish_json) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-publish.json")
  assert string.contains(publish_json, "\"issue_identifier\": \"LIV-65\"")
}

pub fn publish_rebase_conflict_emits_stable_failure_code_test() {
  let dir = "test/tmp/implementation-helper-publish-rebase-conflict"
  reset_dir(dir)
  write_publish_fixture_metadata(dir)
  write_fake_jj(dir <> "/bin/jj")
  write_fake_gh(dir <> "/bin/gh")
  chmod_executable(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/gh")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_FAKE_JJ_REBASE_FAIL=1 SCHERZO_RUN_ROOT=\"$PWD\" SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation publish",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.failure_code == Some("publish_rebase_conflict")
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=publish_rebase_conflict",
  )
  assert string.contains(artifact.stderr, "could not rebase")
}

pub fn publish_revalidation_failure_emits_stable_failure_code_test() {
  let dir = "test/tmp/implementation-helper-publish-revalidation-failed"
  reset_dir(dir)
  write_publish_fixture_metadata(dir)
  write_fake_jj(dir <> "/bin/jj")
  write_fake_gh(dir <> "/bin/gh")
  write_fake_direnv(dir <> "/bin/direnv")
  chmod_executable(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/gh")
  chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_FAKE_DIRENV_SELFCI_FAIL=1 SCHERZO_RUN_ROOT=\"$PWD\" SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation publish",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.failure_code == Some("publish_revalidation_failed")
  assert string.contains(
    artifact.stdout,
    "Revalidation after publish-base normalization",
  )
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=publish_revalidation_failed",
  )
  assert string.contains(artifact.stderr, "command failed with exit code 1")
}

pub fn execplan_publish_fetches_rebases_and_reports_publish_base_test() {
  let dir = "test/tmp/execplan-publish-normalize"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/docs/plans/example.md",
      "# Example ExecPlan\n\n"
        <> "## Progress\n\n"
        <> "- [x] Drafted.\n\n"
        <> "## Open Questions and Clarifications Needed\n\n"
        <> "None.\n",
    )
  write_fake_execplan_jj(dir <> "/bin/jj")
  write_fake_gh(dir <> "/bin/gh")
  chmod_executable(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/gh")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-execplan create-pr",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "Publish base normalization")
  assert string.contains(artifact.stdout, "PUBLISH_BASE=main@origin")
  assert string.contains(
    artifact.stdout,
    "PR_URL=https://github.com/example/repo/pull/123",
  )
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert string.contains(jj_log, "git fetch --remote origin --branch main")
  assert string.contains(jj_log, "rebase -r @ -d main@origin --color=never")
  assert string.contains(
    jj_log,
    "describe -m docs(plan): add execplan for example",
  )
  assert string.contains(
    jj_log,
    "git push --remote origin --bookmark scherzo/execplan/example-execchange",
  )
}

pub fn execplan_workflow_creates_followup_issue_after_pr_test() {
  let assert Ok(workflow) = simplifile.read(".scherzo/workflows/execplan.yaml")

  assert string.contains(workflow, "- id: create_implementation_issue")
  assert string.contains(workflow, "depends_on: [create_pr]")
  assert string.contains(
    workflow,
    "scripts/scherzo-execplan create-implementation-issue",
  )
}

pub fn create_implementation_issue_creates_backlog_linear_ticket_test() {
  let dir = "test/tmp/execplan-create-implementation-issue"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  write_followup_plan(dir)
  write_source_issue(dir)
  write_created_issue(dir)
  write_fake_execplan_handoff_jj(dir <> "/bin/jj")
  write_fake_execplan_handoff_gh(dir <> "/bin/gh")
  write_fake_execplan_handoff_lc(
    dir <> "/bin/lc",
    "{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}",
  )
  chmod_executable(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/gh")
  chmod_executable(dir <> "/bin/lc")

  let artifact =
    run_helper_in(
      dir,
      "env -u SCHERZO_ISSUE_IDENTIFIER PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-execplan create-implementation-issue",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "IMPLEMENTATION_ISSUE_STATUS=created")
  assert string.contains(artifact.stdout, "IMPLEMENTATION_ISSUE=LIV-124")
  assert string.contains(artifact.stdout, "IMPLEMENTATION_ISSUE_STATE=Backlog")
  assert string.contains(
    artifact.stdout,
    "PLAN_PATH=docs/plans/LIV-123-example.md",
  )
  assert string.contains(
    artifact.stdout,
    "PR_URL=https://github.com/example/repo/pull/123",
  )

  let assert Ok(lc_log) = simplifile.read(dir <> "/lc.log")
  assert string.contains(lc_log, "ARG=issue\nARG=query")
  assert string.contains(lc_log, "ARG=issue\nARG=create")
  assert string.contains(lc_log, "ARG=Backlog")
  assert string.contains(lc_log, "ARG=--label\nARG=Improvement")
  assert string.contains(
    lc_log,
    "ARG=--label\nARG=workflow:execplan-implementation",
  )
  assert string.contains(lc_log, "ARG=--parent\nARG=LIV-123")
  assert string.contains(lc_log, "docs/plans/LIV-123-example.md")
  assert string.contains(lc_log, "ARG=issue\nARG=link")
  assert string.contains(lc_log, "ARG=ExecPlan PR")
}

pub fn create_implementation_issue_reuses_existing_ticket_test() {
  let dir = "test/tmp/execplan-create-implementation-issue-existing"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  write_followup_plan(dir)
  write_source_issue(dir)
  write_created_issue(dir)
  write_fake_execplan_handoff_jj(dir <> "/bin/jj")
  write_fake_execplan_handoff_gh(dir <> "/bin/gh")
  write_fake_execplan_handoff_lc(
    dir <> "/bin/lc",
    "{\"nodes\":[{\"identifier\":\"LIV-200\",\"url\":\"https://linear.example/LIV-200\",\"title\":\"Implement: Add queued plan\",\"description\":\"Plan path: `docs/plans/LIV-123-example.md`\",\"labels\":{\"nodes\":[{\"name\":\"workflow:execplan-implementation\"}]}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}",
  )
  chmod_executable(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/gh")
  chmod_executable(dir <> "/bin/lc")

  let artifact =
    run_helper_in(
      dir,
      "env -u SCHERZO_ISSUE_IDENTIFIER PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-execplan create-implementation-issue",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "IMPLEMENTATION_ISSUE_STATUS=existing",
  )
  assert string.contains(artifact.stdout, "IMPLEMENTATION_ISSUE=LIV-200")
  let assert Ok(lc_log) = simplifile.read(dir <> "/lc.log")
  assert !string.contains(lc_log, "ARG=create")
}

pub fn refresh_base_reports_fresh_base_test() {
  let dir = "test/tmp/implementation-helper-refresh-fresh"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_fake_refresh_jj(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/jj")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_FAKE_REFRESH_PARENT_MATCH=1 SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation refresh-base --stage before-validation",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "REFRESH_BASE_STATUS=fresh")
  assert string.contains(artifact.stdout, "REFRESH_BASE_REPAIRABLE=false")
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert !string.contains(jj_log, "rebase -r @ -d main@origin --color=never")
  let assert Ok(json) =
    simplifile.read(
      dir <> "/tmp/scherzo-implementation-refresh-base-before-validation.json",
    )
  assert string.contains(json, "\"status\": \"fresh\"")
  assert string.contains(json, "\"rebased\": false")
}

pub fn refresh_base_rebases_stale_base_and_updates_start_metadata_test() {
  let dir = "test/tmp/implementation-helper-refresh-rebase-start"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation.json",
      "{\"source_kind\":\"ticket\",\"base_change_id\":\"old-base\"}\n",
    )
  write_fake_refresh_jj(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/jj")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation refresh-base --stage before-implementation",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "REFRESH_BASE_STATUS=rebased_clean")
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert string.contains(jj_log, "git fetch --remote origin --branch main")
  assert string.contains(jj_log, "rebase -r @ -d main@origin --color=never")
  let assert Ok(metadata) =
    simplifile.read(dir <> "/tmp/scherzo-implementation.json")
  assert string.contains(
    metadata,
    "\"base_change_id\": \"refreshed-base-change\"",
  )
  assert string.contains(metadata, "\"initial_base_change_id\": \"old-base\"")
  let assert Ok(json) =
    simplifile.read(
      dir
      <> "/tmp/scherzo-implementation-refresh-base-before-implementation.json",
    )
  assert string.contains(json, "\"metadata_base_change_id_updated\": true")
}

pub fn refresh_base_reports_repairable_conflicts_test() {
  let dir = "test/tmp/implementation-helper-refresh-conflicts"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_fake_refresh_jj(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/jj")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_FAKE_REFRESH_CONFLICT_AFTER_REBASE=1 SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation refresh-base --stage before-validation",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(20)
  assert string.contains(artifact.stdout, "REFRESH_BASE_STATUS=conflicts")
  assert string.contains(artifact.stdout, "REFRESH_BASE_REPAIRABLE=true")
  assert string.contains(artifact.stdout, "- src/conflicted.gleam")
  let assert Ok(json) =
    simplifile.read(
      dir <> "/tmp/scherzo-implementation-refresh-base-before-validation.json",
    )
  assert string.contains(json, "\"status\": \"conflicts\"")
  assert string.contains(json, "\"has_unresolved_conflicts\": true")
}

pub fn refresh_base_fetch_failure_is_nonrepairable_test() {
  let dir = "test/tmp/implementation-helper-refresh-fetch-failure"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_fake_refresh_jj(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/jj")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_FAKE_REFRESH_FETCH_FAIL=1 SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation refresh-base --stage before-validation",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stdout, "REFRESH_BASE_STATUS=fetch_failed")
  let assert Ok(json) =
    simplifile.read(
      dir <> "/tmp/scherzo-implementation-refresh-base-before-validation.json",
    )
  assert string.contains(json, "\"status\": \"fetch_failed\"")
  assert string.contains(json, "\"repairable\": false")
}

pub fn refresh_base_base_not_found_is_nonrepairable_test() {
  let dir = "test/tmp/implementation-helper-refresh-base-not-found"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_fake_refresh_jj(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/jj")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_FAKE_REFRESH_BASE_MISSING=1 SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation refresh-base --stage before-validation",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  let assert Ok(json) =
    simplifile.read(
      dir <> "/tmp/scherzo-implementation-refresh-base-before-validation.json",
    )
  assert string.contains(json, "\"status\": \"base_not_found\"")
  assert string.contains(json, "\"repairable\": false")
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert !string.contains(jj_log, "rebase -r @")
}

pub fn refresh_base_rebase_failed_without_conflicts_is_nonrepairable_test() {
  let dir = "test/tmp/implementation-helper-refresh-rebase-failed"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_fake_refresh_jj(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/jj")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_FAKE_REFRESH_REBASE_FAIL=1 SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation refresh-base --stage before-validation",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  let assert Ok(json) =
    simplifile.read(
      dir <> "/tmp/scherzo-implementation-refresh-base-before-validation.json",
    )
  assert string.contains(json, "\"status\": \"rebase_failed\"")
  assert string.contains(json, "\"repairable\": false")
}

pub fn refresh_base_rejects_unsafe_stage_and_writes_latest_json_test() {
  let dir = "test/tmp/implementation-helper-refresh-safe-stage"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_fake_refresh_jj(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/jj")

  let bad =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation refresh-base --stage ../bad",
    )
  assert bad.status == step_artifact.StepFailed
  assert bad.exit_code == Some(1)
  assert string.contains(bad.stderr, "invalid refresh-base stage")
  let assert Error(_) = simplifile.read("test/tmp/bad")

  let good =
    run_helper_in(
      dir,
      "SCHERZO_FAKE_REFRESH_PARENT_MATCH=1 SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation refresh-base --stage before-validation",
    )
  assert good.status == step_artifact.StepSucceeded
  let assert Ok(stage_json) =
    simplifile.read(
      dir <> "/tmp/scherzo-implementation-refresh-base-before-validation.json",
    )
  let assert Ok(latest_json) =
    simplifile.read(
      dir <> "/tmp/scherzo-implementation-refresh-base-latest.json",
    )
  assert string.contains(stage_json, "\"stage\": \"before-validation\"")
  assert string.contains(latest_json, "\"stage\": \"before-validation\"")
  assert string.contains(latest_json, "\"status\": \"fresh\"")
}

pub fn validate_fails_on_base_drift_failure_marker_test() {
  let dir = "test/tmp/implementation-helper-validate-marker"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation-base-drift-failure.md",
      "# Base drift repair failure\n",
    )
  write_fake_direnv(dir <> "/bin/direnv")
  chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation validate",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(
    artifact.stderr,
    "base drift repair requested workflow failure",
  )
  assert read_or_empty(dir <> "/direnv.log") == ""
}

pub fn validate_fails_on_unresolved_jj_conflicts_test() {
  let dir = "test/tmp/implementation-helper-validate-conflicts"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_fake_refresh_jj(dir <> "/bin/jj")
  write_fake_direnv(dir <> "/bin/direnv")
  chmod_executable(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_FAKE_REFRESH_CONFLICT=1 PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation validate",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stderr, "src/conflicted.gleam")
  assert read_or_empty(dir <> "/direnv.log") == ""
}

pub fn publish_time_conflicts_do_not_publish_test() {
  let dir = "test/tmp/implementation-helper-publish-conflicts-blocked"
  reset_dir(dir)
  write_publish_fixture_metadata(dir)
  let assert Ok(Nil) =
    simplifile.write(dir <> "/.scherzo-keep-workspace", "keep\n")
  write_fake_refresh_jj(dir <> "/bin/jj")
  write_fake_gh(dir <> "/bin/gh")
  chmod_executable(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/gh")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_FAKE_REFRESH_CONFLICT_AFTER_REBASE=1 SCHERZO_RUN_ROOT=\"$PWD\" SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation publish",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stdout, "REFRESH_BASE_STATUS=conflicts")
  assert string.contains(artifact.stdout, "PUBLISH_BLOCKED=true")
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert !string.contains(jj_log, "describe -m")
  assert !string.contains(jj_log, "bookmark set")
  assert !string.contains(jj_log, "git push")
  assert read_or_empty(dir <> "/gh.log") == ""
  let assert Ok(_) = simplifile.read(dir <> "/.scherzo-keep-workspace")
  let assert Ok(json) =
    simplifile.read(
      dir <> "/tmp/scherzo-implementation-refresh-base-publish.json",
    )
  assert string.contains(json, "\"status\": \"conflicts\"")
}

pub fn publish_time_revalidation_failure_does_not_publish_test() {
  let dir = "test/tmp/implementation-helper-publish-revalidation-blocked"
  reset_dir(dir)
  write_publish_fixture_metadata(dir)
  let assert Ok(Nil) =
    simplifile.write(dir <> "/.scherzo-keep-workspace", "keep\n")
  write_fake_refresh_jj(dir <> "/bin/jj")
  write_fake_gh(dir <> "/bin/gh")
  write_fake_direnv(dir <> "/bin/direnv")
  chmod_executable(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/gh")
  chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_FAKE_DIRENV_SELFCI_FAIL=1 SCHERZO_RUN_ROOT=\"$PWD\" SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation publish",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.failure_code == Some("publish_revalidation_failed")
  assert string.contains(artifact.stdout, "REFRESH_BASE_STATUS=rebased_clean")
  assert string.contains(artifact.stdout, "PUBLISH_BLOCKED=true")
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert !string.contains(jj_log, "bookmark set")
  assert !string.contains(jj_log, "git push")
  assert read_or_empty(dir <> "/gh.log") == ""
  let assert Ok(_) = simplifile.read(dir <> "/.scherzo-keep-workspace")
  let assert Ok(json) =
    simplifile.read(
      dir <> "/tmp/scherzo-implementation-refresh-base-publish.json",
    )
  assert string.contains(json, "\"status\": \"rebased_clean\"")
}

pub fn publish_time_revalidation_success_may_publish_test() {
  let dir = "test/tmp/implementation-helper-publish-revalidation-success"
  reset_dir(dir)
  write_publish_fixture_metadata(dir)
  let assert Ok(Nil) =
    simplifile.write(dir <> "/.scherzo-keep-workspace", "keep\n")
  write_fake_refresh_jj(dir <> "/bin/jj")
  write_fake_gh(dir <> "/bin/gh")
  write_fake_direnv(dir <> "/bin/direnv")
  chmod_executable(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/gh")
  chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=\"$PWD\" SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation publish",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert string.contains(jj_log, "bookmark set")
  assert string.contains(jj_log, "git push --remote origin")
  let assert Ok(gh_log) = simplifile.read(dir <> "/gh.log")
  assert string.contains(gh_log, "pr create")
  let assert Error(_) = simplifile.read(dir <> "/.scherzo-keep-workspace")
}

pub fn publish_includes_base_drift_repair_summary_test() {
  let dir = "test/tmp/implementation-helper-publish-repair-summary"
  reset_dir(dir)
  write_publish_fixture_metadata(dir)
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation-base-drift-repair.md",
      "# Base drift repair summary\n\nNo-op summary.\n",
    )
  write_fake_refresh_jj(dir <> "/bin/jj")
  write_fake_gh(dir <> "/bin/gh")
  write_fake_direnv(dir <> "/bin/direnv")
  chmod_executable(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/gh")
  chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=\"$PWD\" SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation publish",
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(body) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-pr-body.md")
  assert string.contains(body, "## Base drift repair")
  let assert Ok(publish_json) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-publish.json")
  assert string.contains(
    publish_json,
    "\"base_drift_repair_summary_included\": true",
  )
  assert string.contains(
    publish_json,
    "\"base_drift_repair_summary_path\": \"tmp/scherzo-implementation-base-drift-repair.md\"",
  )

  let dir_without = "test/tmp/implementation-helper-publish-no-repair-summary"
  reset_dir(dir_without)
  write_publish_fixture_metadata(dir_without)
  write_fake_refresh_jj(dir_without <> "/bin/jj")
  write_fake_gh(dir_without <> "/bin/gh")
  write_fake_direnv(dir_without <> "/bin/direnv")
  chmod_executable(dir_without <> "/bin/jj")
  chmod_executable(dir_without <> "/bin/gh")
  chmod_executable(dir_without <> "/bin/direnv")

  let artifact_without =
    run_helper_in(
      dir_without,
      "SCHERZO_RUN_ROOT=\"$PWD\" SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation publish",
    )

  assert artifact_without.status == step_artifact.StepSucceeded
  let assert Ok(publish_json_without) =
    simplifile.read(dir_without <> "/tmp/scherzo-implementation-publish.json")
  assert string.contains(
    publish_json_without,
    "\"base_drift_repair_summary_included\": false",
  )
}

pub fn repair_base_drift_prompt_contains_state_table_test() {
  let assert Ok(prompt) =
    simplifile.read(".scherzo/workflows/prompts/repair-base-drift.md")
  assert string.contains(prompt, "tmp/scherzo-implementation-refresh-base")
  assert string.contains(prompt, "rebased_clean")
  assert string.contains(prompt, "conflicts")
  assert string.contains(prompt, "rebased_clean` and validation succeeded")
  assert string.contains(
    prompt,
    "tmp/scherzo-implementation-base-drift-repair.md",
  )
  assert string.contains(
    prompt,
    "tmp/scherzo-implementation-base-drift-failure.md",
  )
  assert string.contains(
    prompt,
    "Do not create, forget, finish, switch, push, bookmark",
  )
  assert string.contains(prompt, "pull requests")
}

pub fn implementation_workflows_refresh_and_repair_before_publish_test() {
  let assert Ok(implementation) =
    simplifile.read(".scherzo/workflows/implementation.yaml")
  let assert Ok(execplan) =
    simplifile.read(".scherzo/workflows/execplan-implementation.yaml")

  assert_workflow_refresh_ordering(
    implementation,
    "prepare_context",
    "implement",
    "apply_feedback",
  )
  assert_workflow_refresh_ordering(
    execplan,
    "prepare_plan",
    "implement_plan",
    "apply_review_feedback",
  )
}

fn assert_workflow_refresh_ordering(
  workflow: String,
  prepare_step: String,
  implement_step: String,
  feedback_step: String,
) -> Nil {
  assert string.contains(workflow, "- id: refresh_base_before_implementation")
  assert string.contains(workflow, "depends_on: [" <> prepare_step <> "]")
  assert string.contains(workflow, "- id: " <> implement_step)
  assert string.contains(
    workflow,
    "depends_on: [refresh_base_before_implementation]",
  )
  assert string.contains(workflow, "- id: refresh_base_before_validation")
  assert string.contains(workflow, "depends_on: [" <> feedback_step <> "]")
  assert string.contains(workflow, "refresh-base --stage before-validation")
  assert string.contains(workflow, "- id: validate_after_refresh")
  assert string.contains(
    workflow,
    "depends_on: [refresh_base_before_validation]",
  )
  assert string.contains(workflow, "on_failure: continue")
  assert string.contains(workflow, "- id: repair_base_drift")
  assert string.contains(workflow, "depends_on: [validate_after_refresh]")
  assert string.contains(workflow, "prompts/repair-base-drift.md")
  assert string.contains(workflow, "- id: final_validate")
  assert string.contains(workflow, "depends_on: [repair_base_drift]")
  assert string.contains(workflow, "- id: publish_pr")
  assert string.contains(workflow, "depends_on: [final_validate]")
}

fn write_publish_fixture_metadata(dir: String) -> Nil {
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation.json",
      "{\n"
        <> "  \"source_kind\": \"ticket\",\n"
        <> "  \"issue_identifier\": \"SCH-123\",\n"
        <> "  \"issue_title\": \"Fix publish\",\n"
        <> "  \"issue_url\": \"https://linear.example/SCH-123\",\n"
        <> "  \"base_change_id\": \"local-start\"\n"
        <> "}\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation-validation.json",
      "{\"status\": \"passed\", \"commands\": []}\n",
    )
  Nil
}

fn write_followup_plan(dir: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/docs/plans/LIV-123-example.md",
      "# Add queued plan\n\n"
        <> "## Progress\n\n"
        <> "- [x] Drafted.\n\n"
        <> "## Open Questions and Clarifications Needed\n\n"
        <> "None.\n",
    )
  Nil
}

fn write_source_issue(dir: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/source-issue.json",
      "{\n"
        <> "  \"id\": \"source-uuid\",\n"
        <> "  \"identifier\": \"LIV-123\",\n"
        <> "  \"title\": \"Write ExecPlan for queued plan\",\n"
        <> "  \"url\": \"https://linear.example/LIV-123\",\n"
        <> "  \"priority\": 3,\n"
        <> "  \"team\": {\"id\": \"team-uuid\", \"key\": \"LIV\", \"name\": \"Living systems\"},\n"
        <> "  \"project\": {\"id\": \"project-uuid\", \"name\": \"Scherzo\"},\n"
        <> "  \"labels\": [\n"
        <> "    {\"name\": \"Improvement\"},\n"
        <> "    {\"name\": \"workflow:execplan\"}\n"
        <> "  ]\n"
        <> "}\n",
    )
  Nil
}

fn write_created_issue(dir: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/created-issue.json",
      "{\"id\":\"created-uuid\",\"identifier\":\"LIV-124\",\"url\":\"https://linear.example/LIV-124\"}\n",
    )
  Nil
}

fn write_fake_execplan_handoff_jj(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> jj.log\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = remote ]; then echo 'origin https://github.com/example/repo.git'; exit 0; fi\n"
        <> "if [ \"$1\" = diff ]; then\n"
        <> "  case \" $* \" in *\" --summary \"*) echo 'A docs/plans/LIV-123-example.md';; *) echo 'docs/plans/LIV-123-example.md';; esac\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = log ]; then\n"
        <> "  rev=\n"
        <> "  template=\n"
        <> "  prev=\n"
        <> "  for arg in \"$@\"; do\n"
        <> "    if [ \"$prev\" = -r ]; then rev=$arg; fi\n"
        <> "    if [ \"$prev\" = -T ]; then template=$arg; fi\n"
        <> "    prev=$arg\n"
        <> "  done\n"
        <> "  case \"$rev\" in\n"
        <> "    @) case \"$template\" in *change_id.short*) echo execchange;; *) echo currentcommit;; esac; exit 0;;\n"
        <> "    @-) echo localparentcommit; exit 0;;\n"
        <> "    *) exit 0;;\n"
        <> "  esac\n"
        <> "fi\n"
        <> "exit 1\n",
    )
  Nil
}

fn write_fake_execplan_handoff_gh(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> gh.log\n"
        <> "if [ \"$1 $2\" = 'pr view' ]; then echo 'https://github.com/example/repo/pull/123'; exit 0; fi\n"
        <> "exit 1\n",
    )
  Nil
}

fn write_fake_execplan_handoff_lc(path: String, existing_json: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "for arg in \"$@\"; do printf 'ARG=%s\\n' \"$arg\"; done >> lc.log\n"
        <> "printf '%s\\n' '---' >> lc.log\n"
        <> "if [ \"$1 $2\" = 'issue view' ]; then cat source-issue.json; exit 0; fi\n"
        <> "if [ \"$1 $2\" = 'issue query' ]; then printf '%s\\n' '"
        <> existing_json
        <> "'; exit 0; fi\n"
        <> "if [ \"$1 $2\" = 'issue create' ]; then printf '%s\\n' 'Creating issue in LIV' '' 'https://linear.app/living-systems/issue/LIV-124/add-queued-plan'; exit 0; fi\n"
        <> "if [ \"$1 $2\" = 'issue link' ]; then echo '✓ Linked to LIV-124: ExecPlan PR'; exit 0; fi\n"
        <> "exit 1\n",
    )
  Nil
}

fn write_fake_prepare_jj(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> ../jj.log\n"
        <> "if [ \"$1\" = log ]; then echo basechange; exit 0; fi\n"
        <> "exit 1\n",
    )
  Nil
}

fn write_noisy_failing_prepare_jj(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> ../jj.log\n"
        <> "if [ \"$1\" = log ]; then\n"
        <> "  i=0\n"
        <> "  while [ $i -lt 9000 ]; do printf x >&2; i=$((i + 1)); done\n"
        <> "  printf '\\n' >&2\n"
        <> "  exit 2\n"
        <> "fi\n"
        <> "exit 1\n",
    )
  Nil
}

fn write_fake_execplan_jj(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> jj.log\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = remote ]; then echo 'origin https://github.com/example/repo.git'; exit 0; fi\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = fetch ]; then exit 0; fi\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = push ]; then exit 0; fi\n"
        <> "if [ \"$1\" = diff ]; then\n"
        <> "  case \" $* \" in *\" --summary \"*) echo 'A docs/plans/example.md';; *) echo 'docs/plans/example.md';; esac\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = rebase ]; then exit 0; fi\n"
        <> "if [ \"$1\" = describe ]; then exit 0; fi\n"
        <> "if [ \"$1\" = bookmark ]; then exit 0; fi\n"
        <> "if [ \"$1\" = status ]; then exit 0; fi\n"
        <> "if [ \"$1\" = log ]; then\n"
        <> "  rev=\n"
        <> "  template=\n"
        <> "  prev=\n"
        <> "  for arg in \"$@\"; do\n"
        <> "    if [ \"$prev\" = -r ]; then rev=$arg; fi\n"
        <> "    if [ \"$prev\" = -T ]; then template=$arg; fi\n"
        <> "    prev=$arg\n"
        <> "  done\n"
        <> "  case \"$rev\" in\n"
        <> "    main@origin) echo remotecommit; exit 0;;\n"
        <> "    @-) echo localparentcommit; exit 0;;\n"
        <> "    @) case \"$template\" in *change_id.short*) echo execchange;; *) echo currentcommit;; esac; exit 0;;\n"
        <> "    conflicts*) exit 0;;\n"
        <> "    remote_bookmarks*) exit 0;;\n"
        <> "    *) exit 1;;\n"
        <> "  esac\n"
        <> "fi\n"
        <> "exit 1\n",
    )
  Nil
}

fn write_fake_jj(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> jj.log\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = remote ]; then echo 'origin https://github.com/example/repo.git'; exit 0; fi\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = fetch ]; then exit 0; fi\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = push ]; then exit 0; fi\n"
        <> "if [ \"$1\" = diff ]; then echo 'scripts/scherzo-implementation'; exit 0; fi\n"
        <> "if [ \"$1\" = rebase ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_JJ_REBASE_FAIL:-}\" = 1 ]; then echo 'simulated rebase conflict' >&2; exit 1; fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = describe ]; then exit 0; fi\n"
        <> "if [ \"$1\" = bookmark ]; then exit 0; fi\n"
        <> "if [ \"$1\" = status ]; then exit 0; fi\n"
        <> "if [ \"$1\" = log ]; then\n"
        <> "  rev=\n"
        <> "  template=\n"
        <> "  prev=\n"
        <> "  for arg in \"$@\"; do\n"
        <> "    if [ \"$prev\" = -r ]; then rev=$arg; fi\n"
        <> "    if [ \"$prev\" = -T ]; then template=$arg; fi\n"
        <> "    prev=$arg\n"
        <> "  done\n"
        <> "  case \"$rev\" in\n"
        <> "    main@origin) echo remotecommit; exit 0;;\n"
        <> "    @-) echo localparentcommit; exit 0;;\n"
        <> "    @) case \"$template\" in *change_id.short*) echo publishchange;; *) echo currentcommit;; esac; exit 0;;\n"
        <> "    conflicts*) exit 0;;\n"
        <> "    remote_bookmarks*) exit 0;;\n"
        <> "    *) exit 1;;\n"
        <> "  esac\n"
        <> "fi\n"
        <> "exit 1\n",
    )
  Nil
}

fn write_fake_refresh_jj(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> jj.log\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = remote ]; then echo 'origin https://github.com/example/repo.git'; exit 0; fi\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = fetch ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_REFRESH_FETCH_FAIL:-}\" = 1 ]; then echo 'fetch failed' >&2; exit 3; fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = push ]; then exit 0; fi\n"
        <> "if [ \"$1\" = diff ]; then echo 'scripts/scherzo-implementation'; exit 0; fi\n"
        <> "if [ \"$1\" = rebase ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_REFRESH_CONFLICT_AFTER_REBASE:-}\" = 1 ]; then touch .fake-conflict; echo 'simulated conflict' >&2; exit 1; fi\n"
        <> "  if [ \"${SCHERZO_FAKE_REFRESH_REBASE_FAIL:-}\" = 1 ]; then echo 'simulated rebase infrastructure failure' >&2; exit 1; fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = resolve ] && [ \"$2\" = --list ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_REFRESH_CONFLICT:-}\" = 1 ] || [ -f .fake-conflict ]; then echo 'src/conflicted.gleam    2-sided conflict'; exit 0; fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = describe ]; then exit 0; fi\n"
        <> "if [ \"$1\" = bookmark ]; then exit 0; fi\n"
        <> "if [ \"$1\" = status ]; then exit 0; fi\n"
        <> "if [ \"$1\" = log ]; then\n"
        <> "  rev=\n"
        <> "  template=\n"
        <> "  prev=\n"
        <> "  for arg in \"$@\"; do\n"
        <> "    if [ \"$prev\" = -r ]; then rev=$arg; fi\n"
        <> "    if [ \"$prev\" = -T ]; then template=$arg; fi\n"
        <> "    prev=$arg\n"
        <> "  done\n"
        <> "  case \"$rev\" in\n"
        <> "    main@origin) if [ \"${SCHERZO_FAKE_REFRESH_BASE_MISSING:-}\" = 1 ]; then exit 1; fi; echo remotecommit; exit 0;;\n"
        <> "    main) if [ \"${SCHERZO_FAKE_REFRESH_BASE_MISSING:-}\" = 1 ]; then exit 1; fi; echo localfallbackcommit; exit 0;;\n"
        <> "    @-) case \"$template\" in *change_id*) echo refreshed-base-change;; *) if [ \"${SCHERZO_FAKE_REFRESH_PARENT_MATCH:-}\" = 1 ]; then echo remotecommit; else echo localparentcommit; fi;; esac; exit 0;;\n"
        <> "    @) case \"$template\" in *change_id.short*) echo refreshchange;; *) echo currentcommit;; esac; exit 0;;\n"
        <> "    conflicts*) if [ \"${SCHERZO_FAKE_REFRESH_CONFLICT:-}\" = 1 ] || [ -f .fake-conflict ]; then echo conflictchange; fi; exit 0;;\n"
        <> "    remote_bookmarks*) exit 0;;\n"
        <> "    *) exit 1;;\n"
        <> "  esac\n"
        <> "fi\n"
        <> "exit 1\n",
    )
  Nil
}

fn write_fake_gh(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> gh.log\n"
        <> "if [ \"$1 $2\" = 'pr view' ]; then exit 1; fi\n"
        <> "if [ \"$1 $2\" = 'pr create' ]; then echo 'https://github.com/example/repo/pull/123'; exit 0; fi\n"
        <> "exit 1\n",
    )
  Nil
}

fn write_fake_direnv(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> direnv.log\n"
        <> "if [ \"${SCHERZO_FAIL_IF_RUN_ROOT_LEAKS:-}\" = 1 ] && [ -n \"${SCHERZO_RUN_ROOT:-}\" ]; then echo 'SCHERZO_RUN_ROOT leaked into validation' >&2; exit 1; fi\n"
        <> "case \"$*\" in\n"
        <> "  'exec . selfci check '*) if [ \"${SCHERZO_FAKE_DIRENV_SELFCI_FAIL:-}\" = 1 ] || [ \"${SCHERZO_FAKE_DIRENV_TEST_FAIL:-}\" = 1 ]; then echo 'simulated SelfCI validation failure' >&2; exit 1; fi;;\n"
        <> "  'exec . gleam test') if [ \"${SCHERZO_FAKE_DIRENV_TEST_FAIL:-}\" = 1 ]; then echo 'simulated validation failure' >&2; exit 1; fi;;\n"
        <> "esac\n"
        <> "exit 0\n",
    )
  Nil
}
