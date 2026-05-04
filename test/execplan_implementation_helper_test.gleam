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
      "#!/bin/sh\n" <> "printf '%s\\n' \"$*\" >> direnv.log\n" <> "exit 0\n",
    )
  Nil
}
