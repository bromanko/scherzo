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
  assert string.contains(artifact.stderr, "expected exactly one ExecPlan path")
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
      "SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation publish",
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
  assert string.contains(direnv_log, "exec . gleam test")
  let assert Ok(publish_json) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-publish.json")
  assert string.contains(
    publish_json,
    "\"publish_base_revision\": \"main@origin\"",
  )
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
      "SCHERZO_FAKE_JJ_REBASE_FAIL=1 SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation publish",
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
      "SCHERZO_FAKE_DIRENV_TEST_FAIL=1 SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation publish",
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
  write_fake_execplan_handoff_lc(dir <> "/bin/lc", "[]")
  chmod_executable(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/gh")
  chmod_executable(dir <> "/bin/lc")

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-execplan create-implementation-issue",
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
  assert string.contains(lc_log, "ARG=issue\nARG=create")
  assert string.contains(lc_log, "ARG=Backlog")
  assert string.contains(lc_log, "ARG=Improvement,workflow-label-uuid")
  assert string.contains(lc_log, "ARG=source-uuid")
  assert string.contains(lc_log, "docs/plans/LIV-123-example.md")
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
    "[{\"identifier\":\"LIV-200\",\"url\":\"https://linear.example/LIV-200\",\"title\":\"Implement: Add queued plan\",\"description\":\"Plan path: `docs/plans/LIV-123-example.md`\",\"labels\":{\"nodes\":[{\"name\":\"workflow:execplan-implementation\"}]}}]",
  )
  chmod_executable(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/gh")
  chmod_executable(dir <> "/bin/lc")

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-execplan create-implementation-issue",
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
        <> "if [ \"$1 $2\" = 'issue get' ]; then cat source-issue.json; exit 0; fi\n"
        <> "if [ \"$1 $2\" = 'issue list' ]; then printf '%s\\n' '"
        <> existing_json
        <> "'; exit 0; fi\n"
        <> "if [ \"$1 $2\" = 'label list' ]; then echo '[{\"id\":\"workflow-label-uuid\",\"name\":\"workflow:execplan-implementation\"}]'; exit 0; fi\n"
        <> "if [ \"$1 $2\" = 'issue create' ]; then cat created-issue.json; exit 0; fi\n"
        <> "if [ \"$1 $2\" = 'attachment add' ]; then echo '{\"id\":\"attachment-uuid\"}'; exit 0; fi\n"
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
        <> "if [ \"${SCHERZO_FAKE_DIRENV_TEST_FAIL:-}\" = 1 ] && [ \"$*\" = 'exec . gleam test' ]; then echo 'simulated validation failure' >&2; exit 1; fi\n"
        <> "exit 0\n",
    )
  Nil
}
