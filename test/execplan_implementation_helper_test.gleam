import gleam/list
import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/path as scherzo_path
import scherzo/step_artifact
import simplifile
import support/test_helpers
import workflow_context_test_support

fn run_helper(command: String) -> step_artifact.StepArtifact {
  command_step.run(
    "helper",
    workflow_context_test_support.without_workflow_context(
      ".scherzo/workflows/scripts/scherzo-implementation " <> command,
    ),
    ".",
    5000,
    [],
    test_helpers.default_artifact_limits(),
  )
}

fn run_helper_in(cwd: String, command: String) -> step_artifact.StepArtifact {
  command_step.run(
    "helper",
    workflow_context_test_support.without_workflow_context(command),
    cwd,
    10_000,
    [],
    test_helpers.default_artifact_limits(),
  )
}

fn read_or_empty(path: String) -> String {
  case simplifile.read(path) {
    Ok(contents) -> contents
    Error(_) -> ""
  }
}

fn metadata_cache_path(dir: String) -> String {
  dir <> "/tmp/scherzo-implementation.json"
}

fn metadata_canonical_path(dir: String) -> String {
  dir <> "/run-root/state/implementation/metadata.json"
}

fn run_root_env() -> String {
  "SCHERZO_RUN_ROOT=\"$PWD/run-root\""
}

fn clean_workflow_env() -> String {
  "env -u SCHERZO_WORKSPACE_DRIVER"
  <> " -u SCHERZO_WORKSPACE_PROFILE"
  <> " -u SCHERZO_WORKSPACE_CAPABILITIES"
  <> " -u SCHERZO_WORKSPACE_ROOT"
  <> " -u SCHERZO_WORKSPACE_PATH"
  <> " -u SCHERZO_RUN_ROOT"
}

fn execplan_metadata(plan_path: String, base_change_id: String) -> String {
  "{\n"
  <> "  \"source_kind\": \"execplan\",\n"
  <> "  \"plan_path\": \""
  <> plan_path
  <> "\",\n"
  <> "  \"execplan_v2_bundle_path\": \"tmp/execplan-bundle.json\",\n"
  <> "  \"base_change_id\": \""
  <> base_change_id
  <> "\"\n"
  <> "}\n"
}

fn write_linear_graphql_fixture(path: String, issue_json: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(path, "{\"data\":{\"issue\":" <> issue_json <> "}}\n")
  Nil
}

pub fn plan_brief_command_generates_checks_and_refreshes_execplan_brief_test() {
  let dir = "test/tmp/implementation-helper-plan-brief"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/docs/plans/example.md", execplan_markdown())
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation.json",
      "{\n"
        <> "  \"source_kind\": \"execplan\",\n"
        <> "  \"plan_path\": \"docs/plans/example.md\",\n"
        <> "  \"execplan_v2_bundle_path\": \"tmp/execplan-bundle.json\",\n"
        <> "  \"base_change_id\": \"local-start\"\n"
        <> "}\n",
    )

  let generated =
    run_helper_in(
      dir,
      "../../../.scherzo/workflows/scripts/scherzo-implementation plan-brief",
    )

  assert generated.status == step_artifact.StepSucceeded
  assert generated.exit_code == Some(0)
  assert string.contains(generated.stdout, "PLAN_BRIEF_STATUS=ok")
  assert string.contains(
    generated.stdout,
    "PLAN_BRIEF_PATH=tmp/scherzo-execplan-brief.md",
  )
  assert string.contains(
    generated.stdout,
    "PLAN_INDEX_PATH=tmp/scherzo-execplan-index.json",
  )
  assert string.contains(generated.stdout, "PLAN_SOURCE_SHA256=")
  let assert Ok(metadata) =
    simplifile.read(dir <> "/tmp/scherzo-implementation.json")
  assert string.contains(metadata, "\"plan_brief_status\": \"ok\"")
  assert string.contains(
    metadata,
    "\"plan_brief_path\": \"tmp/scherzo-execplan-brief.md\"",
  )
  let assert Ok(brief) =
    simplifile.read(dir <> "/tmp/scherzo-execplan-brief.md")
  assert string.contains(brief, "# ExecPlanBrief for Example ExecPlan")
  assert string.contains(brief, "## Validation and Acceptance")

  let fresh =
    run_helper_in(
      dir,
      "../../../.scherzo/workflows/scripts/scherzo-implementation plan-brief --check",
    )
  assert fresh.status == step_artifact.StepSucceeded
  assert string.contains(fresh.stdout, "PLAN_BRIEF_STATUS=fresh")

  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/docs/plans/example.md",
      execplan_markdown() <> "\n## Extra\n\nMutated.\n",
    )
  let stale =
    run_helper_in(
      dir,
      "../../../.scherzo/workflows/scripts/scherzo-implementation plan-brief --check",
    )
  assert stale.status == step_artifact.StepFailed
  assert stale.exit_code == Some(2)
  assert string.contains(stale.stdout, "PLAN_BRIEF_STATUS=stale")

  let refreshed =
    run_helper_in(
      dir,
      "../../../.scherzo/workflows/scripts/scherzo-implementation plan-brief --refresh-if-stale",
    )
  assert refreshed.status == step_artifact.StepSucceeded
  assert string.contains(refreshed.stdout, "PLAN_BRIEF_STATUS=ok")
  let fresh_again =
    run_helper_in(
      dir,
      "../../../.scherzo/workflows/scripts/scherzo-implementation plan-brief --check",
    )
  assert fresh_again.status == step_artifact.StepSucceeded
  assert string.contains(fresh_again.stdout, "PLAN_BRIEF_STATUS=fresh")
}

pub fn plan_brief_command_resolves_legacy_workspace_prefixed_state_path_test() {
  let dir = "test/tmp/implementation-helper-legacy-workspace-plan-path"
  test_helpers.reset_dir(dir)
  let run_root = dir <> "/run-root"
  let workspace = run_root <> "/workspaces/main"
  let state_dir = run_root <> "/state/implementation"
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  let assert Ok(Nil) = simplifile.create_directory_all(state_dir)
  let assert Ok(Nil) =
    simplifile.write(
      state_dir <> "/execplan-review-doc.md",
      execplan_markdown(),
    )
  let assert Ok(Nil) =
    simplifile.write(
      state_dir <> "/metadata.json",
      "{\n"
        <> "  \"source_kind\": \"execplan\",\n"
        <> "  \"plan_path\": \"workspaces/main/state/implementation/execplan-review-doc.md\",\n"
        <> "  \"execplan_v2_bundle_path\": \"state/implementation/execplan-bundle.json\",\n"
        <> "  \"base_change_id\": \"local-start\"\n"
        <> "}\n",
    )
  let assert Ok(helper) =
    scherzo_path.absolute(".scherzo/workflows/scripts/scherzo-implementation")

  let artifact =
    run_helper_in(
      workspace,
      "SCHERZO_RUN_ROOT=\"$PWD/../..\" "
        <> test_helpers.shell_quote(helper)
        <> " plan-brief",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "PLAN_BRIEF_STATUS=ok")
  let assert Ok(brief) =
    simplifile.read(workspace <> "/tmp/scherzo-execplan-brief.md")
  assert string.contains(brief, "# ExecPlanBrief for Example ExecPlan")
}

pub fn plan_brief_command_reports_unavailable_and_removes_partial_files_test() {
  let dir = "test/tmp/implementation-helper-plan-brief-failure"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/docs/plans/example.md", execplan_markdown())
  let assert Ok(Nil) =
    simplifile.write(dir <> "/tmp/scherzo-execplan-brief.md", "stale\n")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/tmp/scherzo-execplan-index.json", "{}\n")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation.json",
      "{\n"
        <> "  \"source_kind\": \"execplan\",\n"
        <> "  \"plan_path\": \"docs/plans/example.md\",\n"
        <> "  \"execplan_v2_bundle_path\": \"tmp/execplan-bundle.json\",\n"
        <> "  \"base_change_id\": \"local-start\"\n"
        <> "}\n",
    )
  write_failing_brief_helper(dir <> "/bin/failing-brief-helper")
  test_helpers.chmod_executable(dir <> "/bin/failing-brief-helper")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_EXECPLAN_BRIEF_HELPER=bin/failing-brief-helper ../../../.scherzo/workflows/scripts/scherzo-implementation plan-brief",
    )

  assert artifact.status == step_artifact.StepFailed
  assert string.contains(artifact.stdout, "PLAN_BRIEF_STATUS=unavailable")
  assert string.contains(
    artifact.stdout,
    "PLAN_BRIEF_WARNING=brief generation failed",
  )
  let assert Ok(metadata) =
    simplifile.read(dir <> "/tmp/scherzo-implementation.json")
  assert string.contains(metadata, "\"plan_brief_status\": \"unavailable\"")
  assert string.contains(metadata, "\"plan_brief_warning\":")
  assert !string.contains(metadata, "plan_source_sha256")
  let assert Error(_) = simplifile.read(dir <> "/tmp/scherzo-execplan-brief.md")
  let assert Error(_) =
    simplifile.read(dir <> "/tmp/scherzo-execplan-index.json")
}

pub fn plan_brief_command_rejects_non_markdown_prepared_execplan_metadata_test() {
  let dir = "test/tmp/implementation-helper-plan-brief-html-path"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/docs/plans/example.html", "<h1>Legacy</h1>\n")
  let assert Ok(Nil) =
    simplifile.write(
      metadata_cache_path(dir),
      execplan_metadata("docs/plans/example.html", "local-start"),
    )

  let artifact =
    run_helper_in(
      dir,
      "../../../.scherzo/workflows/scripts/scherzo-implementation plan-brief",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("execplan_metadata_invalid")
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_metadata_invalid",
  )
  assert string.contains(
    artifact.stderr,
    "invalid prepared ExecPlan path: docs/plans/example.html; expected a Markdown path",
  )
}

pub fn plan_brief_command_requires_execplan_bundle_metadata_test() {
  let dir = "test/tmp/implementation-helper-plan-brief-missing-bundle"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/docs/plans/example.md", execplan_markdown())
  let assert Ok(Nil) =
    simplifile.write(
      metadata_cache_path(dir),
      "{\n"
        <> "  \"source_kind\": \"execplan\",\n"
        <> "  \"plan_path\": \"docs/plans/example.md\",\n"
        <> "  \"base_change_id\": \"local-start\"\n"
        <> "}\n",
    )

  let artifact =
    run_helper_in(
      dir,
      "../../../.scherzo/workflows/scripts/scherzo-implementation plan-brief",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("execplan_metadata_invalid")
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=execplan_metadata_invalid",
  )
  assert string.contains(
    artifact.stderr,
    "plan-brief requires ExecPlan bundle metadata produced by scherzo-execplan implementation-prepare",
  )
}

pub fn metadata_load_restores_tmp_cache_from_run_root_test() {
  let dir = "test/tmp/implementation-helper-metadata-canonical-load"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/run-root/state/implementation")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/docs/plans/example.md", execplan_markdown())
  let assert Ok(Nil) =
    simplifile.write(
      metadata_canonical_path(dir),
      execplan_metadata("docs/plans/example.md", "canonical-start"),
    )

  let artifact =
    run_helper_in(
      dir,
      clean_workflow_env()
        <> " "
        <> run_root_env()
        <> " ../../../.scherzo/workflows/scripts/scherzo-implementation plan-brief",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "PLAN_BRIEF_STATUS=ok")
  let assert Ok(cache) = simplifile.read(metadata_cache_path(dir))
  let assert Ok(canonical) = simplifile.read(metadata_canonical_path(dir))
  assert cache == canonical
  assert string.contains(cache, "\"base_change_id\": \"canonical-start\"")
  assert string.contains(cache, "\"plan_brief_status\": \"ok\"")
}

pub fn metadata_backfills_run_root_from_tmp_cache_with_diagnostic_test() {
  let dir = "test/tmp/implementation-helper-metadata-backfill"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/docs/plans/example.md", execplan_markdown())
  let assert Ok(Nil) =
    simplifile.write(
      metadata_cache_path(dir),
      execplan_metadata("docs/plans/example.md", "tmp-start"),
    )

  let artifact =
    run_helper_in(
      dir,
      clean_workflow_env()
        <> " "
        <> run_root_env()
        <> " ../../../.scherzo/workflows/scripts/scherzo-implementation plan-brief",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stderr,
    "backfilled canonical implementation metadata",
  )
  assert string.contains(artifact.stderr, "state/implementation/metadata.json")
  assert string.contains(artifact.stderr, "tmp/scherzo-implementation.json")
  let assert Ok(cache) = simplifile.read(metadata_cache_path(dir))
  let assert Ok(canonical) = simplifile.read(metadata_canonical_path(dir))
  assert cache == canonical
  assert string.contains(canonical, "\"base_change_id\": \"tmp-start\"")
}

pub fn analyze_uses_canonical_metadata_when_tmp_cache_is_deleted_test() {
  let dir = "test/tmp/implementation-helper-analyze-canonical"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/run-root/state/implementation")
  let assert Ok(Nil) =
    simplifile.write(
      metadata_canonical_path(dir),
      "{\n"
        <> "  \"source_kind\": \"ticket\",\n"
        <> "  \"base_change_id\": \"canonical-base\"\n"
        <> "}\n",
    )
  write_fake_analyze_jj(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/jj")

  let artifact =
    run_helper_in(
      dir,
      clean_workflow_env()
        <> " "
        <> run_root_env()
        <> " PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation analyze",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "CHANGED_FILES:")
  assert string.contains(artifact.stdout, "- src/example.gleam")
  assert string.contains(artifact.stdout, "LANGUAGES=gleam")
  let assert Ok(cache) = simplifile.read(metadata_cache_path(dir))
  assert string.contains(cache, "\"base_change_id\": \"canonical-base\"")
}

pub fn refresh_base_updates_canonical_metadata_and_cache_test() {
  let dir = "test/tmp/implementation-helper-refresh-canonical-start"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/run-root/state/implementation")
  let metadata =
    "{\"source_kind\":\"ticket\",\"base_change_id\":\"old-base\"}\n"
  let assert Ok(Nil) = simplifile.write(metadata_cache_path(dir), metadata)
  let assert Ok(Nil) = simplifile.write(metadata_canonical_path(dir), metadata)
  write_fake_refresh_jj(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/jj")

  let artifact =
    run_helper_in(
      dir,
      clean_workflow_env()
        <> " "
        <> run_root_env()
        <> " SCHERZO_JJ_WORKSPACE_REMOTE=origin SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation refresh-base --stage before-implementation",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "REFRESH_BASE_STATUS=rebased_clean")
  let assert Ok(cache) = simplifile.read(metadata_cache_path(dir))
  let assert Ok(canonical) = simplifile.read(metadata_canonical_path(dir))
  assert cache == canonical
  assert string.contains(cache, "\"base_change_id\": \"refreshed-base-change\"")
  assert string.contains(cache, "\"initial_base_change_id\": \"old-base\"")
  let assert Ok(result_json) =
    simplifile.read(
      dir
      <> "/tmp/scherzo-implementation-refresh-base-before-implementation.json",
    )
  assert string.contains(
    result_json,
    "\"metadata_base_change_id_updated\": true",
  )
}

pub fn metadata_missing_from_run_root_and_tmp_is_unrecoverable_test() {
  let dir = "test/tmp/implementation-helper-metadata-missing"
  test_helpers.reset_dir(dir)

  let artifact =
    run_helper_in(
      dir,
      clean_workflow_env()
        <> " "
        <> run_root_env()
        <> " ../../../.scherzo/workflows/scripts/scherzo-implementation analyze",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stderr, "unrecoverable workflow-state loss")
  assert string.contains(artifact.stderr, "state/implementation/metadata.json")
  assert string.contains(artifact.stderr, "tmp/scherzo-implementation.json")
  assert string.contains(
    artifact.stderr,
    "do not rerun prepare after implementation",
  )
}

pub fn prepare_ticket_writes_canonical_metadata_and_cache_test() {
  let dir = "test/tmp/implementation-helper-prepare-ticket-canonical"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/run-root")
  write_fake_jj(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  write_linear_graphql_fixture(
    dir <> "/linear-ticket.json",
    "{"
      <> "\"identifier\":\"LIV-254\","
      <> "\"title\":\"Implement from ticket\","
      <> "\"description\":\"Use the ticket body as implementation context.\","
      <> "\"url\":\"https://linear.example/LIV-254\","
      <> "\"priority\":2,"
      <> "\"state\":{\"name\":\"Todo\"},"
      <> "\"labels\":{\"nodes\":[{\"name\":\"workflow:implementation\"}],\"pageInfo\":{\"hasNextPage\":false}},"
      <> "\"comments\":{\"nodes\":[{\"createdAt\":\"2026-05-12T00:00:00Z\",\"body\":\"Ready.\",\"user\":{\"name\":\"Ada\"}}],\"pageInfo\":{\"hasNextPage\":false}}}",
  )

  let artifact =
    run_helper_in(
      dir,
      clean_workflow_env()
        <> " "
        <> run_root_env()
        <> " SCHERZO_ISSUE_IDENTIFIER=LIV-254 SCHERZO_TEST_LINEAR_GRAPHQL_JSON=linear-ticket.json PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation prepare --source ticket",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "SOURCE_KIND=ticket")
  assert string.contains(
    artifact.stdout,
    "BRIEF_PATH=tmp/scherzo-implementation-brief.md",
  )
  let assert Ok(cache) = simplifile.read(metadata_cache_path(dir))
  let assert Ok(canonical) = simplifile.read(metadata_canonical_path(dir))
  assert cache == canonical
  assert string.contains(cache, "\"source_kind\": \"ticket\"")
  assert string.contains(cache, "\"issue_identifier\": \"LIV-254\"")
  assert string.contains(cache, "\"issue_title\": \"Implement from ticket\"")
  assert string.contains(
    cache,
    "\"issue_url\": \"https://linear.example/LIV-254\"",
  )
  assert string.contains(
    cache,
    "\"brief_path\": \"tmp/scherzo-implementation-brief.md\"",
  )
  assert string.contains(cache, "\"base_change_id\": \"localparentcommit\"")
  let assert Ok(brief) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-brief.md")
  assert string.contains(
    brief,
    "# Task context for LIV-254: Implement from ticket",
  )
}

pub fn prepare_ticket_failure_writes_retention_marker_before_fetch_test() {
  let dir = "test/tmp/implementation-helper-prepare-retention"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/main")
  write_fake_prepare_jj(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/jj")

  let artifact =
    run_helper_in(
      dir <> "/main",
      "SCHERZO_ISSUE_IDENTIFIER=LIV-71 LINEAR_API_KEY= PATH=\"$PWD/../bin:$PATH\" ../../../../.scherzo/workflows/scripts/scherzo-implementation prepare --source ticket",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stderr, "LINEAR_API_KEY is required")
  let assert Ok(marker) = simplifile.read(dir <> "/.scherzo-keep-workspace")
  assert string.contains(marker, "Source kind: ticket")
  assert string.contains(marker, "Source: LIV-71")
}

pub fn prepare_command_failure_reports_bounded_diagnostic_excerpt_test() {
  let dir = "test/tmp/implementation-helper-bounded-diagnostics"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/main")
  write_noisy_failing_prepare_jj(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/jj")

  let artifact =
    run_helper_in(
      dir <> "/main",
      "SCHERZO_ISSUE_IDENTIFIER=LIV-71 LINEAR_API_KEY= PATH=\"$PWD/../bin:$PATH\" ../../../../.scherzo/workflows/scripts/scherzo-implementation prepare --source ticket",
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
  test_helpers.reset_dir(dir)
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
  assert string.contains(artifact.stdout, "REVIEW_COMMANDS:\n- None")
}

pub fn ticket_brief_renders_linear_context_test() {
  let dir = "test/tmp/implementation-helper-ticket-brief"
  test_helpers.reset_dir(dir)
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
    "# Task context for SCH-123: Implement generic workflow",
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

pub fn plan_completion_gate_passes_fresh_pass_verdict_test() {
  let dir = "test/tmp/plan-completion-gate-pass"
  let fingerprint = setup_plan_completion_gate_fixture(dir)
  write_plan_completion_verdict(dir, "pass", fingerprint, "[]")

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation gate-plan-completion --final",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_GATE_MODE=final")
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_VERDICT=pass")
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_GATE=passed")
}

pub fn plan_completion_gate_allows_deferred_manual_verification_test() {
  let dir = "test/tmp/plan-completion-gate-deferred-manual"
  let fingerprint = setup_plan_completion_gate_fixture(dir)
  write_plan_completion_verdict_with_deferred_manual_verification(
    dir,
    fingerprint,
  )

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation gate-plan-completion --final",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_VERDICT=pass")
  assert string.contains(
    artifact.stdout,
    "PLAN_COMPLETION_DEFERRED_MANUAL_VERIFICATION:",
  )
  assert string.contains(artifact.stdout, "Dogfood the browser flow")
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_GATE=passed")
}

pub fn plan_completion_gate_from_submission_stamps_current_workspace_test() {
  let dir = "test/tmp/plan-completion-gate-from-submission"
  let fingerprint = setup_plan_completion_gate_fixture(dir)
  write_plan_completion_submission(dir, "pass", "[]")
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/run-root/state/implementation")

  let artifact =
    run_helper_in(
      dir,
      "cat plan-completion-submission.json | SCHERZO_RUN_ROOT=\"$PWD/run-root\" SCHERZO_WORKSPACE_PATH=\"$PWD\" SCHERZO_REPO_ROOT=\"$PWD\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation gate-plan-completion --from-submission",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "PLAN_COMPLETION_GATE_MODE=from_submission",
  )
  assert string.contains(
    artifact.stdout,
    "PLAN_COMPLETION_VERDICT_STAMP=written",
  )
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_GATE=passed")
  let assert Ok(canonical_verdict) =
    simplifile.read(
      dir
      <> "/run-root/state/implementation/scherzo-plan-completion-verdict.json",
    )
  assert string.contains(canonical_verdict, "\"verdict\": \"pass\"")
  assert string.contains(
    canonical_verdict,
    "\"verified_diff_fingerprint\": \"" <> fingerprint <> "\"",
  )
  assert string.contains(
    canonical_verdict,
    "\"changed_files\": [\n    \".scherzo/workflows/scripts/scherzo-implementation\"\n  ]",
  )
  assert string.contains(
    canonical_verdict,
    "\"verified_change_id\": \"publishchange\"",
  )
  let assert Ok(False) =
    simplifile.is_file(dir <> "/tmp/scherzo-plan-completion-verdict.json")
}

pub fn plan_completion_gate_from_submission_blocks_fail_after_stamping_test() {
  let dir = "test/tmp/plan-completion-gate-from-submission-fail"
  let fingerprint = setup_plan_completion_gate_fixture(dir)
  write_plan_completion_submission(
    dir,
    "fail",
    "[\"Acceptance criterion remains unchecked.\"]",
  )
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/run-root/state/implementation")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=\"$PWD/run-root\" SCHERZO_WORKSPACE_PATH=\"$PWD\" SCHERZO_REPO_ROOT=\"$PWD\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation gate-plan-completion --from-submission --submission plan-completion-submission.json",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("plan_completion_failed")
  assert string.contains(
    artifact.stdout,
    "PLAN_COMPLETION_VERDICT_STAMP=written",
  )
  assert string.contains(
    artifact.stdout,
    "Acceptance criterion remains unchecked.",
  )
  let assert Ok(canonical_verdict) =
    simplifile.read(
      dir
      <> "/run-root/state/implementation/scherzo-plan-completion-verdict.json",
    )
  assert string.contains(canonical_verdict, "\"verdict\": \"fail\"")
  assert string.contains(
    canonical_verdict,
    "\"verified_diff_fingerprint\": \"" <> fingerprint <> "\"",
  )
  let assert Ok(False) =
    simplifile.is_file(dir <> "/tmp/scherzo-plan-completion-verdict.json")
}

pub fn plan_completion_gate_from_submission_requires_run_root_test() {
  let dir = "test/tmp/plan-completion-gate-from-submission-no-run-root"
  let _fingerprint = setup_plan_completion_gate_fixture(dir)
  write_plan_completion_submission(dir, "pass", "[]")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_WORKSPACE_PATH=\"$PWD\" SCHERZO_REPO_ROOT=\"$PWD\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation gate-plan-completion --from-submission --submission plan-completion-submission.json",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(2)
  assert artifact.failure_code == Some("plan_completion_verdict_write_failed")
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_GATE=failed")
  assert string.contains(artifact.stderr, "SCHERZO_RUN_ROOT is required")
  let assert Ok(False) =
    simplifile.is_file(dir <> "/tmp/scherzo-plan-completion-verdict.json")
}

pub fn plan_completion_gate_from_submission_rejects_malformed_submission_test() {
  let dir = "test/tmp/plan-completion-gate-from-submission-malformed"
  let _fingerprint = setup_plan_completion_gate_fixture(dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/run-root/state/implementation")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/plan-completion-submission.json", "{not json}\n")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=\"$PWD/run-root\" SCHERZO_WORKSPACE_PATH=\"$PWD\" SCHERZO_REPO_ROOT=\"$PWD\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation gate-plan-completion --from-submission --submission plan-completion-submission.json",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("plan_completion_submission_malformed")
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_GATE=failed")
  assert string.contains(artifact.stderr, "must be valid JSON")
  let assert Ok(False) =
    simplifile.is_file(
      dir
      <> "/run-root/state/implementation/scherzo-plan-completion-verdict.json",
    )
  let assert Ok(False) =
    simplifile.is_file(dir <> "/tmp/scherzo-plan-completion-verdict.json")
}

pub fn plan_completion_gate_from_submission_rejects_machine_fields_test() {
  let dir = "test/tmp/plan-completion-gate-from-submission-machine-field"
  let _fingerprint = setup_plan_completion_gate_fixture(dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/run-root/state/implementation")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/plan-completion-submission.json",
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"verdict\": \"pass\",\n"
        <> "  \"blocking_findings\": [],\n"
        <> "  \"evidence\": [\"Required behavior is present.\"],\n"
        <> "  \"checked_acceptance_criteria\": [\"Required work.\"],\n"
        <> "  \"verified_change_id\": \"agent-copied-value\"\n"
        <> "}\n",
    )

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=\"$PWD/run-root\" SCHERZO_WORKSPACE_PATH=\"$PWD\" SCHERZO_REPO_ROOT=\"$PWD\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation gate-plan-completion --from-submission --submission plan-completion-submission.json",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("plan_completion_submission_malformed")
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_GATE=failed")
  assert string.contains(
    artifact.stderr,
    "must not include workspace-stamped fields: verified_change_id",
  )
  let assert Ok(False) =
    simplifile.is_file(
      dir
      <> "/run-root/state/implementation/scherzo-plan-completion-verdict.json",
    )
  let assert Ok(False) =
    simplifile.is_file(dir <> "/tmp/scherzo-plan-completion-verdict.json")
}

pub fn plan_completion_gate_from_submission_rejects_pass_with_blockers_test() {
  let dir = "test/tmp/plan-completion-gate-from-submission-pass-blockers"
  let _fingerprint = setup_plan_completion_gate_fixture(dir)
  write_plan_completion_submission(
    dir,
    "pass",
    "[\"Acceptance criterion remains unchecked.\"]",
  )

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=\"$PWD/run-root\" SCHERZO_WORKSPACE_PATH=\"$PWD\" SCHERZO_REPO_ROOT=\"$PWD\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation gate-plan-completion --from-submission --submission plan-completion-submission.json",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("plan_completion_submission_malformed")
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_GATE=failed")
  assert string.contains(
    artifact.stderr,
    "pass verdict must not include blocking_findings",
  )
  let assert Ok(False) =
    simplifile.is_file(
      dir
      <> "/run-root/state/implementation/scherzo-plan-completion-verdict.json",
    )
}

pub fn checkpoint_plan_completion_verdict_stamps_retained_structured_submission_test() {
  let dir = "test/tmp/plan-completion-checkpoint-structured-submission"
  let fingerprint = setup_plan_completion_gate_fixture(dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/run-root/state/implementation")
  let artifact_dir = dir <> "/artifacts"
  let structured_dir =
    artifact_dir <> "/verify_plan_completion/attempt-0/structured"
  let assert Ok(Nil) = simplifile.create_directory_all(structured_dir)
  let structured_path =
    structured_dir <> "/plan_completion_verdict_submission.json"
  let assert Ok(Nil) =
    simplifile.write(
      structured_path,
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"artifact_type\": \"structured_output\",\n"
        <> "  \"run_id\": \"run-1\",\n"
        <> "  \"workflow_id\": \"execplan-implementation\",\n"
        <> "  \"step_id\": \"verify_plan_completion\",\n"
        <> "  \"attempt_index\": 0,\n"
        <> "  \"artifact_name\": \"plan_completion_verdict_submission\",\n"
        <> "  \"format\": \"json\",\n"
        <> "  \"source_type\": \"pi_tool_call\",\n"
        <> "  \"source_tool_name\": \"submit_plan_completion_verdict\",\n"
        <> "  \"payload\": {\n"
        <> "    \"verdict\": \"pass\",\n"
        <> "    \"blocking_findings\": [],\n"
        <> "    \"evidence\": [\"Required behavior is present.\"],\n"
        <> "    \"checked_acceptance_criteria\": [\"Required work.\"],\n"
        <> "    \"deferred_manual_verification\": []\n"
        <> "  }\n"
        <> "}\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      artifact_dir <> "/verify_plan_completion/attempt-0.json",
      "{\n"
        <> "  \"artifact\": {\n"
        <> "    \"status\": \"success\",\n"
        <> "    \"structured_output\": {\n"
        <> "      \"status\": \"valid\",\n"
        <> "      \"artifact_name\": \"plan_completion_verdict_submission\",\n"
        <> "      \"path\": \"verify_plan_completion/attempt-0/structured/plan_completion_verdict_submission.json\"\n"
        <> "    }\n"
        <> "  }\n"
        <> "}\n",
    )

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=\"$PWD/run-root\" SCHERZO_RUN_ID=run-1 SCHERZO_WORKSPACE_PATH=\"$PWD\" SCHERZO_REPO_ROOT=\"$PWD\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation checkpoint-plan-completion-verdict --submission-step verify_plan_completion --artifact-dir artifacts",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "PLAN_COMPLETION_VERDICT_SOURCE=structured_output",
  )
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_VERDICT=pass")
  let assert Ok(canonical_verdict) =
    simplifile.read(
      dir
      <> "/run-root/state/implementation/scherzo-plan-completion-verdict.json",
    )
  assert string.contains(
    canonical_verdict,
    "\"verified_diff_fingerprint\": \"" <> fingerprint <> "\"",
  )
  assert string.contains(
    canonical_verdict,
    "\"verified_change_id\": \"publishchange\"",
  )
  let assert Ok(False) =
    simplifile.is_file(dir <> "/tmp/scherzo-plan-completion-verdict.json")
}

pub fn checkpoint_plan_completion_verdict_uses_default_run_artifact_dir_test() {
  let dir = "test/tmp/plan-completion-checkpoint-default-artifact-dir"
  let fingerprint = setup_plan_completion_gate_fixture(dir)
  let artifact_dir = dir <> "/run-root/.scherzo-state/artifacts/runs/run-1"
  write_retained_plan_completion_submission(
    artifact_dir,
    "verify_plan_completion",
    structured_plan_completion_submission_json(
      "verify_plan_completion",
      "pass",
      "[]",
    ),
  )

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=\"$PWD/run-root\" SCHERZO_RUN_ID=run-1 SCHERZO_WORKSPACE_PATH=\"$PWD\" SCHERZO_REPO_ROOT=\"$PWD\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation checkpoint-plan-completion-verdict --submission-step verify_plan_completion",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "PLAN_COMPLETION_VERDICT_SOURCE=structured_output",
  )
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_VERDICT=pass")
  assert string.contains(
    artifact.stdout,
    ".scherzo-state/artifacts/runs/run-1/verify_plan_completion/attempt-0/structured/plan_completion_verdict_submission.json",
  )
  let assert Ok(canonical_verdict) =
    simplifile.read(
      dir
      <> "/run-root/state/implementation/scherzo-plan-completion-verdict.json",
    )
  assert string.contains(
    canonical_verdict,
    "\"verified_diff_fingerprint\": \"" <> fingerprint <> "\"",
  )
}

pub fn checkpoint_plan_completion_verdict_rejects_raw_retained_submission_test() {
  let dir = "test/tmp/plan-completion-checkpoint-raw-retained"
  let _fingerprint = setup_plan_completion_gate_fixture(dir)
  let artifact_dir = dir <> "/artifacts"
  write_retained_plan_completion_submission(
    artifact_dir,
    "verify_plan_completion",
    plan_completion_submission_json("pass", "[]"),
  )

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=\"$PWD/run-root\" SCHERZO_RUN_ID=run-1 SCHERZO_WORKSPACE_PATH=\"$PWD\" SCHERZO_REPO_ROOT=\"$PWD\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation checkpoint-plan-completion-verdict --submission-step verify_plan_completion --artifact-dir artifacts",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("plan_completion_submission_malformed")
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_GATE=failed")
  assert string.contains(
    artifact.stderr,
    "retained plan-completion submission must be a structured_output artifact",
  )
  let assert Ok(False) =
    simplifile.is_file(
      dir
      <> "/run-root/state/implementation/scherzo-plan-completion-verdict.json",
    )
}

pub fn plan_completion_gate_blocks_fail_verdict_test() {
  let dir = "test/tmp/plan-completion-gate-fail"
  let fingerprint = setup_plan_completion_gate_fixture(dir)
  write_plan_completion_verdict(
    dir,
    "fail",
    fingerprint,
    "[\"Acceptance criterion remains unchecked.\"]",
  )

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation gate-plan-completion",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("plan_completion_failed")
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_VERDICT=fail")
  assert string.contains(
    artifact.stdout,
    "Acceptance criterion remains unchecked.",
  )
  assert string.contains(
    artifact.stderr,
    "SCHERZO_FAILURE_CODE=plan_completion_failed",
  )
}

pub fn plan_completion_gate_rejects_pass_verdict_with_blocking_findings_test() {
  let dir = "test/tmp/plan-completion-gate-pass-blockers"
  let fingerprint = setup_plan_completion_gate_fixture(dir)
  write_plan_completion_verdict(
    dir,
    "pass",
    fingerprint,
    "[\"Acceptance criterion remains unchecked.\"]",
  )

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation gate-plan-completion",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("plan_completion_verdict_malformed")
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_GATE=failed")
  assert string.contains(
    artifact.stderr,
    "pass verdict must not include blocking_findings",
  )
}

pub fn plan_completion_gate_blocks_missing_semantic_evidence_verdict_test() {
  let dir = "test/tmp/plan-completion-gate-semantic-evidence-fail"
  let fingerprint = setup_plan_completion_gate_fixture(dir)
  write_plan_completion_verdict(
    dir,
    "fail",
    fingerprint,
    "[\"Missing negative/error-path, idempotency, lint, and provider-live/cache evidence required by the canonical plan or implementation pack.\"]",
  )

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation gate-plan-completion",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("plan_completion_failed")
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_VERDICT=fail")
  assert string.contains(artifact.stdout, "Missing negative/error-path")
  assert string.contains(artifact.stdout, "provider-live/cache evidence")
}

pub fn plan_completion_gate_blocks_malformed_verdict_test() {
  let dir = "test/tmp/plan-completion-gate-malformed"
  let _fingerprint = setup_plan_completion_gate_fixture(dir)
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-plan-completion-verdict.json",
      "{not json}\n",
    )

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation gate-plan-completion --final",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("plan_completion_verdict_malformed")
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_GATE=failed")
  assert string.contains(artifact.stderr, "invalid JSON")
}

pub fn plan_completion_gate_blocks_missing_verdict_test() {
  let dir = "test/tmp/plan-completion-gate-missing"
  let _fingerprint = setup_plan_completion_gate_fixture(dir)

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation gate-plan-completion --final",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("plan_completion_verdict_missing")
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_GATE=failed")
  assert string.contains(
    artifact.stderr,
    "missing tmp/scherzo-plan-completion-verdict.json",
  )
}

pub fn plan_completion_gate_blocks_stale_verdict_test() {
  let dir = "test/tmp/plan-completion-gate-stale"
  let _fingerprint = setup_plan_completion_gate_fixture(dir)
  write_plan_completion_verdict(
    dir,
    "pass",
    "0000000000000000000000000000000000000000000000000000000000000000",
    "[]",
  )

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation gate-plan-completion --final",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("plan_completion_verdict_stale")
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_GATE=failed")
  assert string.contains(artifact.stderr, "stale verdict fingerprint")
}

pub fn jj_workspace_driver_prefers_configured_remote_base_for_new_root_workspaces_test() {
  let assert Ok(script) = simplifile.read("scripts/scherzo-workspace-jj")
  assert string.contains(script, "SCHERZO_JJ_WORKSPACE_BASE")
  assert string.contains(script, "selected_workspace_base_branch")
  assert string.contains(script, "selected_workspace_base_remote")
  assert string.contains(script, "SCHERZO_JJ_WORKSPACE_BASE_BRANCH")
  assert string.contains(script, "SCHERZO_JJ_WORKSPACE_REMOTE")
  assert !string.contains(script, "SCHERZO_PR_BASE")
  assert !string.contains(script, "env_value(\"SCHERZO_PR_REMOTE\") or")
  assert string.contains(script, "legacy_publication_remote_unsupported")
  assert string.contains(script, "configured_base_candidates(branch, remote)")
  assert string.contains(
    script,
    "base_revision = workspace_base_revision(repo_root)",
  )
}

pub fn validate_unsets_scherzo_run_root_for_nested_helper_tests_test() {
  let dir = "test/tmp/implementation-helper-validate-env"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_fake_direnv(dir <> "/bin/direnv")
  test_helpers.chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=/outer/run/root SCHERZO_WORKFLOW_BUNDLE_DIR=/outer/bundle SCHERZO_WORKSPACE_DRIVER=/outer/driver SCHERZO_WORKSPACE_PROFILE=dogfood-jj SCHERZO_WORKSPACE_CAPABILITIES=status,diff SCHERZO_WORKSPACE_ROOT=/outer/workspaces SCHERZO_REPO_ROOT=/outer/repo GITHUB_REPOSITORY=outer/repo SCHERZO_GITHUB_REPO=example/repo SCHERZO_JJ_WORKSPACE_REMOTE=scherzo-agent SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE=scherzo-agent SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main SCHERZO_JJ_WORKSPACE_FETCH_BASE=true SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main SCHERZO_PR_REPO=example/repo SCHERZO_PR_DRAFT=true SCHERZO_FAIL_IF_RUN_ROOT_LEAKS=1 SCHERZO_FAIL_IF_WORKFLOW_BUNDLE_DIR_LEAKS=1 SCHERZO_FAIL_IF_WORKSPACE_DRIVER_LEAKS=1 SCHERZO_FAIL_IF_PUBLICATION_ENV_LEAKS=1 PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation validate",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "FINAL_VALIDATION=passed")
  let assert Ok(direnv_log) = simplifile.read(dir <> "/direnv.log")
  assert string.contains(direnv_log, "allow .")
  assert string.contains(direnv_log, "exec . scripts/scherzo-ci")
  assert !string.contains(direnv_log, "exec . gleam format --check src test")
  assert !string.contains(direnv_log, "exec . gleam test")
  let assert Ok(validation_json) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-validation.json")
  assert string.contains(validation_json, "\"status\": \"passed\"")
  assert string.contains(validation_json, "\"validator\": \"scherzo-ci\"")
  assert string.contains(
    validation_json,
    "\"base_revision\": \"main@scherzo-agent\"",
  )
  assert string.contains(validation_json, "direnv exec . scripts/scherzo-ci")
}

pub fn validate_records_latest_refresh_base_revision_test() {
  let dir = "test/tmp/implementation-helper-validate-refresh-base"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation-refresh-base-latest.json",
      "{\"base_revision\":\"feature-base@origin\"}\n",
    )
  write_fake_direnv(dir <> "/bin/direnv")
  test_helpers.chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation validate",
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(direnv_log) = simplifile.read(dir <> "/direnv.log")
  assert string.contains(direnv_log, "exec . scripts/scherzo-ci")
  let assert Ok(validation_json) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-validation.json")
  assert string.contains(
    validation_json,
    "\"base_revision\": \"feature-base@origin\"",
  )
}

pub fn validate_failure_writes_structured_failure_artifact_test() {
  let dir = "test/tmp/implementation-helper-validate-failure-artifact"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_fake_direnv(dir <> "/bin/direnv")
  test_helpers.chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_FAKE_DIRENV_CI_FAIL=1 SCHERZO_JJ_WORKSPACE_REMOTE=origin SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation validate",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stdout, "FINAL_VALIDATION=failed")
  assert string.contains(
    artifact.stdout,
    "VALIDATION_RESULT_PATH=tmp/scherzo-implementation-validation.json",
  )
  assert string.contains(artifact.stderr, "Structured validation artifact")
  assert string.contains(
    artifact.stderr,
    "simulated scherzo-ci validation failure",
  )
  let assert Ok(validation_json) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-validation.json")
  assert string.contains(validation_json, "\"status\": \"failed\"")
  assert string.contains(validation_json, "\"exit_code\": 1")
  assert string.contains(
    validation_json,
    "simulated scherzo-ci validation failure",
  )
}

pub fn validate_skips_when_fingerprint_matches_prior_pass_test() {
  let dir = "test/tmp/implementation-helper-validate-cache-hit"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  write_fake_direnv(dir <> "/bin/direnv")
  test_helpers.chmod_executable(dir <> "/bin/direnv")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation-validation.json",
      "{\n"
        <> "  \"status\": \"passed\",\n"
        <> "  \"validator\": \"scherzo-ci\",\n"
        <> "  \"base_revision\": \"main@origin\",\n"
        <> "  \"commands\": [\"direnv exec . scripts/scherzo-ci\"],\n"
        <> "  \"fingerprint\": \"cafef00d\"\n"
        <> "}\n",
    )

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_VALIDATE_FINGERPRINT=cafef00d SCHERZO_JJ_WORKSPACE_REMOTE=origin SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation validate",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "FINAL_VALIDATION=passed")
  assert string.contains(artifact.stdout, "VALIDATION_CACHED=1")
  // The gate was reused, so the scherzo-ci command was never executed.
  assert !string.contains(artifact.stdout, "$ direnv exec . scripts/scherzo-ci")
  let assert Ok(validation_json) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-validation.json")
  assert string.contains(validation_json, "\"cached\": true")
  assert string.contains(validation_json, "\"status\": \"passed\"")
}

pub fn validate_runs_when_prior_pass_fingerprint_differs_test() {
  let dir = "test/tmp/implementation-helper-validate-cache-miss"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  write_fake_direnv(dir <> "/bin/direnv")
  test_helpers.chmod_executable(dir <> "/bin/direnv")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation-validation.json",
      "{\n"
        <> "  \"status\": \"passed\",\n"
        <> "  \"validator\": \"scherzo-ci\",\n"
        <> "  \"base_revision\": \"main@origin\",\n"
        <> "  \"commands\": [\"direnv exec . scripts/scherzo-ci\"],\n"
        <> "  \"fingerprint\": \"stalefingerprint\"\n"
        <> "}\n",
    )

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_VALIDATE_FINGERPRINT=freshfingerprint SCHERZO_JJ_WORKSPACE_REMOTE=origin SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation validate",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert string.contains(artifact.stdout, "FINAL_VALIDATION=passed")
  assert !string.contains(artifact.stdout, "VALIDATION_CACHED=1")
  let assert Ok(direnv_log) = simplifile.read(dir <> "/direnv.log")
  assert string.contains(direnv_log, "exec . scripts/scherzo-ci")
  let assert Ok(validation_json) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-validation.json")
  // Re-ran, so the artifact carries the freshly computed fingerprint, not the stale one.
  assert string.contains(
    validation_json,
    "\"fingerprint\": \"freshfingerprint\"",
  )
  assert !string.contains(validation_json, "\"cached\": true")
}

pub fn validate_ignores_base_drift_failure_marker_test() {
  let dir = "test/tmp/implementation-helper-validate-ignores-base-drift-marker"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  write_fake_direnv(dir <> "/bin/direnv")
  test_helpers.chmod_executable(dir <> "/bin/direnv")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation-base-drift-failure.md",
      "# Base drift repair failure\n\n## Reason\nStale marker from an earlier validation failure.\n",
    )

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_JJ_WORKSPACE_REMOTE=origin SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation validate",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "FINAL_VALIDATION=passed")
  let assert Ok(direnv_log) = simplifile.read(dir <> "/direnv.log")
  assert string.contains(direnv_log, "exec . scripts/scherzo-ci")
}

pub fn refresh_base_and_validate_runs_validation_after_fresh_refresh_test() {
  let dir = "test/tmp/implementation-helper-refresh-and-validate-fresh"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_fake_refresh_jj(dir <> "/bin/jj")
  write_fake_direnv(dir <> "/bin/direnv")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_FAKE_REFRESH_PARENT_MATCH=1 SCHERZO_JJ_WORKSPACE_REMOTE=origin SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation refresh-base-and-validate --stage before-validation",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "REFRESH_BASE_STATUS=fresh")
  assert string.contains(artifact.stdout, "REFRESH_AND_VALIDATE_STATUS=fresh")
  assert string.contains(artifact.stdout, "VALIDATION_STATUS=passed")
  let assert Ok(direnv_log) = simplifile.read(dir <> "/direnv.log")
  assert string.contains(direnv_log, "exec . scripts/scherzo-ci")
  let assert Ok(refresh_json) =
    simplifile.read(
      dir <> "/tmp/scherzo-implementation-refresh-base-before-validation.json",
    )
  assert string.contains(refresh_json, "\"status\": \"fresh\"")
  let assert Ok(validation_json) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-validation.json")
  assert string.contains(validation_json, "\"status\": \"passed\"")
}

pub fn refresh_base_and_validate_starts_recovery_for_conflicts_test() {
  let dir = "test/tmp/implementation-helper-refresh-and-validate-conflicts"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_fake_refresh_jj(dir <> "/bin/jj")
  write_fake_direnv(dir <> "/bin/direnv")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_FAKE_REFRESH_CONFLICT=1 SCHERZO_JJ_WORKSPACE_REMOTE=origin SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation refresh-base-and-validate --stage before-validation",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(20)
  assert string.contains(artifact.stdout, "REFRESH_BASE_STATUS=conflicts")
  assert string.contains(
    artifact.stdout,
    "REFRESH_AND_VALIDATE_STATUS=conflicts",
  )
  assert string.contains(artifact.stdout, "VALIDATION_STATUS=not_run")
  let assert Ok(refresh_json) =
    simplifile.read(
      dir <> "/tmp/scherzo-implementation-refresh-base-before-validation.json",
    )
  assert string.contains(refresh_json, "\"status\": \"conflicts\"")
  assert string.contains(refresh_json, "\"repairable\": true")
  let assert Error(_) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-validation.json")
  assert read_or_empty(dir <> "/direnv.log") == ""
}

pub fn refresh_base_and_validate_reports_validation_failure_after_clean_rebase_test() {
  let dir = "test/tmp/implementation-helper-refresh-and-validate-rebased-fail"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_fake_refresh_jj(dir <> "/bin/jj")
  write_fake_direnv(dir <> "/bin/direnv")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_FAKE_DIRENV_CI_FAIL=1 SCHERZO_JJ_WORKSPACE_REMOTE=origin SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation refresh-base-and-validate --stage before-validation",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("base_refresh_validation_failed")
  assert string.contains(artifact.stdout, "REFRESH_BASE_STATUS=rebased_clean")
  assert string.contains(
    artifact.stdout,
    "REFRESH_AND_VALIDATE_STATUS=rebased_clean",
  )
  assert string.contains(artifact.stdout, "FINAL_VALIDATION=failed")
  let assert Ok(direnv_log) = simplifile.read(dir <> "/direnv.log")
  assert string.contains(direnv_log, "exec . scripts/scherzo-ci")
  let assert Ok(refresh_json) =
    simplifile.read(
      dir <> "/tmp/scherzo-implementation-refresh-base-before-validation.json",
    )
  assert string.contains(refresh_json, "\"status\": \"rebased_clean\"")
  let assert Ok(validation_json) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-validation.json")
  assert string.contains(validation_json, "\"status\": \"failed\"")
}

pub fn refresh_base_and_validate_reports_nonrepairable_refresh_test() {
  let dir = "test/tmp/implementation-helper-refresh-and-validate-fetch-fail"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_fake_refresh_jj(dir <> "/bin/jj")
  write_fake_direnv(dir <> "/bin/direnv")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_FAKE_REFRESH_FETCH_FAIL=1 SCHERZO_JJ_WORKSPACE_REMOTE=origin SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation refresh-base-and-validate --stage before-validation",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stdout, "REFRESH_BASE_STATUS=fetch_failed")
  assert string.contains(
    artifact.stdout,
    "REFRESH_AND_VALIDATE_STATUS=fetch_failed",
  )
  assert string.contains(artifact.stdout, "VALIDATION_STATUS=not_run")
  let assert Ok(refresh_json) =
    simplifile.read(
      dir <> "/tmp/scherzo-implementation-refresh-base-before-validation.json",
    )
  assert string.contains(refresh_json, "\"status\": \"fetch_failed\"")
  assert string.contains(refresh_json, "\"repairable\": false")
  let assert Error(_) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-validation.json")
  assert read_or_empty(dir <> "/direnv.log") == ""
}

pub fn publish_requires_workspace_driver_and_does_not_legacy_push_test() {
  let dir = "test/tmp/implementation-helper-publish-no-driver"
  test_helpers.reset_dir(dir)
  write_publish_fixture_metadata(dir)
  write_fake_jj(dir <> "/bin/jj")
  write_fake_gh(dir <> "/bin/gh")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/gh")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=\"$PWD\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation publish",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("workspace_driver_unavailable")
  assert string.contains(
    artifact.stderr,
    "SCHERZO_WORKSPACE_DRIVER is required",
  )
  assert read_or_empty(dir <> "/jj.log") == ""
  assert read_or_empty(dir <> "/gh.log") == ""
  let assert Error(_) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-commit-stack.json")
}

fn pr_draft_env_prefix(value: String) -> String {
  case value {
    "" -> ""
    draft -> "SCHERZO_PR_DRAFT=" <> draft <> " "
  }
}

fn run_driver_backed_publish_with_env(
  dir: String,
  env_prefix: String,
) -> step_artifact.StepArtifact {
  test_helpers.reset_dir(dir)
  write_publish_fixture_metadata(dir)
  write_fake_refresh_jj(dir <> "/bin/jj")
  write_fake_git(dir <> "/bin/git")
  write_fake_gh(dir <> "/bin/gh")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/git")
  test_helpers.chmod_executable(dir <> "/bin/gh")

  run_helper_in(
    dir,
    env_prefix
      <> "SCHERZO_FAKE_REFRESH_PARENT_MATCH=1 SCHERZO_RUN_ROOT=\"$PWD\" SCHERZO_WORKSPACE_DRIVER=../../../scripts/scherzo-workspace-jj SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE=origin SCHERZO_JJ_WORKSPACE_REMOTE=origin SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main SCHERZO_GITHUB_REPO=example/repo PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation publish",
  )
}

fn run_driver_backed_publish_with_pr_draft(
  dir: String,
  draft: String,
) -> step_artifact.StepArtifact {
  run_driver_backed_publish_with_env(dir, pr_draft_env_prefix(draft))
}

pub fn driver_backed_publish_pr_draft_true_prepares_core_publication_test() {
  let dir = "test/tmp/implementation-helper-driver-publish-draft-true"
  let artifact = run_driver_backed_publish_with_pr_draft(dir, "true")

  assert artifact.status == step_artifact.StepSucceeded
  assert string.contains(artifact.stdout, "PUBLICATION_MODE=commit_stack")
  assert string.contains(
    artifact.stdout,
    "PUBLICATION_ORCHESTRATOR=scherzo_core",
  )
  assert string.contains(
    artifact.stdout,
    "PR_URL will be reported by Scherzo core",
  )
  let assert Ok(commit_stack) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-commit-stack.json")
  assert string.contains(commit_stack, "scherzo.git_commit_stack.v1")
  assert string.contains(commit_stack, "\"ref\": \"main\"")
  assert string.contains(
    commit_stack,
    "\"sha\": \"1111111111111111111111111111111111111111\"",
  )
  assert string.contains(
    commit_stack,
    "\"sha\": \"3333333333333333333333333333333333333333\"",
  )
  assert string.contains(
    commit_stack,
    "\"tree\": \"4444444444444444444444444444444444444444\"",
  )
  assert string.contains(
    commit_stack,
    "\"ref\": \"runs/local-run/outputs/commit_stack.bundle\"",
  )
  assert string.contains(
    commit_stack,
    "\"media_type\": \"application/vnd.git.bundle\"",
  )
  let assert Ok(git_log) = simplifile.read(dir <> "/git.log")
  assert string.contains(git_log, "bundle create")
  assert string.contains(git_log, "bundle verify")
  let assert Ok(publish_json) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-publish.json")
  assert string.contains(publish_json, "ready_for_core_publication")
  assert string.contains(publish_json, "scherzo_core")
  assert read_or_empty(dir <> "/gh.log") == ""
}

pub fn driver_backed_publish_describes_empty_head_before_commit_stack_artifact_test() {
  let dir = "test/tmp/implementation-helper-driver-publish-empty-description"
  let artifact =
    run_driver_backed_publish_with_env(dir, "SCHERZO_FAKE_EMPTY_DESCRIPTION=1 ")

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(jj_log) = simplifile.read(dir <> "/jj.log")
  assert string.contains(jj_log, "log -r @ --no-graph -T description")
  assert string.contains(jj_log, "describe -m Implement SCH-123: Fix publish")
  let assert Ok(commit_stack) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-commit-stack.json")
  assert string.contains(
    commit_stack,
    "\"sha\": \"5555555555555555555555555555555555555555\"",
  )
  assert !string.contains(
    commit_stack,
    "\"sha\": \"3333333333333333333333333333333333333333\"",
  )
  let assert Ok(publish_json) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-publish.json")
  assert string.contains(
    publish_json,
    "\"commit_stack_head_revision\": \"5555555555555555555555555555555555555555\"",
  )
}

pub fn driver_backed_publish_pr_draft_false_prepares_core_publication_test() {
  let dir = "test/tmp/implementation-helper-driver-publish-draft-false"
  let artifact = run_driver_backed_publish_with_pr_draft(dir, "false")

  assert artifact.status == step_artifact.StepSucceeded
  assert string.contains(
    artifact.stdout,
    "COMMIT_STACK_PATH=state/implementation/scherzo-implementation-commit-stack.json",
  )
  assert read_or_empty(dir <> "/gh.log") == ""
}

pub fn driver_backed_publish_pr_draft_unset_prepares_core_publication_test() {
  let dir = "test/tmp/implementation-helper-driver-publish-draft-unset"
  let artifact = run_driver_backed_publish_with_pr_draft(dir, "")

  assert artifact.status == step_artifact.StepSucceeded
  assert string.contains(
    artifact.stdout,
    "PUBLICATION_ORCHESTRATOR=scherzo_core",
  )
  assert read_or_empty(dir <> "/gh.log") == ""
}

pub fn driver_backed_publish_invalid_pr_draft_fails_before_publication_test() {
  let dir = "test/tmp/implementation-helper-driver-publish-draft-invalid"
  let artifact = run_driver_backed_publish_with_pr_draft(dir, "maybe")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("invalid_configuration")
  assert string.contains(
    artifact.stderr,
    "SCHERZO_PR_DRAFT must be true or false",
  )
  assert read_or_empty(dir <> "/jj.log") == ""
  assert read_or_empty(dir <> "/gh.log") == ""
}

pub fn driver_backed_publish_rejects_unsafe_run_id_for_commit_stack_carrier_test() {
  let dir = "test/tmp/implementation-helper-driver-publish-unsafe-run-id"
  let artifact =
    run_driver_backed_publish_with_env(dir, "SCHERZO_RUN_ID=../bad ")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("invalid_configuration")
  assert string.contains(artifact.stderr, "SCHERZO_RUN_ID must be a safe")
  let assert Error(_) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-commit-stack.json")
}

pub fn driver_backed_publish_rejects_invalid_commit_stack_git_objects_test() {
  let dir = "test/tmp/implementation-helper-driver-publish-invalid-git-oid"
  let artifact =
    run_driver_backed_publish_with_env(dir, "SCHERZO_FAKE_INVALID_COMMIT_ID=1 ")

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("invalid_commit_stack_artifact")
  assert string.contains(
    artifact.stderr,
    "did not resolve to a 40-character Git object ID",
  )
  let assert Error(_) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-commit-stack.json")
}

pub fn driver_backed_publish_unknown_refresh_status_fails_closed_test() {
  let dir = "test/tmp/implementation-helper-driver-publish-refresh-failure"
  test_helpers.reset_dir(dir)
  write_publish_fixture_metadata(dir)
  write_fake_jj(dir <> "/bin/jj")
  write_fake_git(dir <> "/bin/git")
  write_unknown_refresh_status_driver(dir <> "/bin/fake-driver")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/git")
  test_helpers.chmod_executable(dir <> "/bin/fake-driver")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=\"$PWD\" SCHERZO_WORKSPACE_DRIVER=./bin/fake-driver PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation publish",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("boom")
  assert string.contains(artifact.stderr, "invalid_request")
  assert string.contains(artifact.stderr, "refresh failed")
  let assert Ok(driver_log) = simplifile.read(dir <> "/driver.log")
  assert string.contains(driver_log, "refresh-base --stage publish --json")
  assert read_or_empty(dir <> "/git.log") == ""
  let assert Error(_) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-commit-stack.json")
  let assert Error(_) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-publish.json")
}

pub fn driver_backed_publish_rejects_oversize_commit_stack_carrier_test() {
  let dir = "test/tmp/implementation-helper-driver-publish-oversize-carrier"
  let artifact =
    run_driver_backed_publish_with_env(
      dir,
      "SCHERZO_COMMIT_STACK_MAX_CARRIER_BYTES=8 ",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("commit_stack_carrier_too_large")
  assert string.contains(artifact.stderr, "commit_stack carrier bundle exceeds")
  let assert Ok(git_log) = simplifile.read(dir <> "/git.log")
  assert string.contains(git_log, "bundle create")
  assert string.contains(git_log, "bundle verify")
  let assert Error(_) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-commit-stack.json")
  let assert Error(_) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-publish.json")
}

pub fn publish_includes_base_drift_repair_summary_test() {
  let dir = "test/tmp/implementation-helper-publish-repair-summary"
  test_helpers.reset_dir(dir)
  write_publish_fixture_metadata(dir)
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation-base-drift-repair.md",
      "# Base drift repair summary\n\nNo-op summary.\n",
    )
  write_fake_refresh_jj(dir <> "/bin/jj")
  write_fake_git(dir <> "/bin/git")
  write_fake_gh(dir <> "/bin/gh")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/git")
  test_helpers.chmod_executable(dir <> "/bin/gh")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_FAKE_REFRESH_PARENT_MATCH=1 SCHERZO_RUN_ROOT=\"$PWD\" SCHERZO_WORKSPACE_DRIVER=../../../scripts/scherzo-workspace-jj SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE=origin SCHERZO_JJ_WORKSPACE_REMOTE=origin SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main SCHERZO_GITHUB_REPO=example/repo PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation publish",
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
  test_helpers.reset_dir(dir_without)
  write_publish_fixture_metadata(dir_without)
  write_fake_refresh_jj(dir_without <> "/bin/jj")
  write_fake_git(dir_without <> "/bin/git")
  write_fake_gh(dir_without <> "/bin/gh")
  test_helpers.chmod_executable(dir_without <> "/bin/jj")
  test_helpers.chmod_executable(dir_without <> "/bin/git")
  test_helpers.chmod_executable(dir_without <> "/bin/gh")

  let artifact_without =
    run_helper_in(
      dir_without,
      "SCHERZO_FAKE_REFRESH_PARENT_MATCH=1 SCHERZO_RUN_ROOT=\"$PWD\" SCHERZO_WORKSPACE_DRIVER=../../../scripts/scherzo-workspace-jj SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE=origin SCHERZO_JJ_WORKSPACE_REMOTE=origin SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main SCHERZO_GITHUB_REPO=example/repo PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation publish",
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
  assert string.contains(prompt, "workflow_step_recovery_input")
  assert string.contains(prompt, "tmp/scherzo-implementation-refresh-base")
  assert string.contains(prompt, "rebased_clean")
  assert string.contains(prompt, "conflicts")
  assert string.contains(prompt, "validation succeeded")
  assert string.contains(
    prompt,
    "tmp/scherzo-implementation-base-drift-repair.md",
  )
  assert string.contains(
    prompt,
    "tmp/scherzo-implementation-base-drift-failure.md",
  )
  assert string.contains(prompt, "failure_summary")
  assert string.contains(prompt, "stdout_excerpt")
  assert string.contains(prompt, "stderr_excerpt")
  assert string.contains(prompt, "submit_workflow_step_recovery_result")
  assert string.contains(
    prompt,
    "Do not create, forget, finish, switch, push, bookmark",
  )
  assert string.contains(prompt, "pull requests")
}

pub fn execplan_implementation_prompts_trim_validation_payloads_test() {
  let execplan_prompt_paths = [
    ".scherzo/workflows/prompts/execplan-implementation-implement.md",
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion.md",
    ".scherzo/workflows/prompts/execplan-implementation-apply-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-repair-base-drift.md",
    ".scherzo/workflows/prompts/execplan-implementation-recover-plan-completion.md",
  ]

  let assert Ok(identity_fragment) =
    simplifile.read(
      ".scherzo/workflows/prompts/fragments/execplan-identity-model.md",
    )

  list.each(execplan_prompt_paths, fn(path) {
    let assert Ok(prompt) = simplifile.read(path)
    assert !string.contains(prompt, "{{ issue.description }}")
    assert string.contains(prompt, "{{ issue.url }}")
    case
      path
      == ".scherzo/workflows/prompts/execplan-implementation-recover-plan-completion.md"
    {
      True -> {
        assert string.contains(
          prompt,
          "implementation_handoff.issue_identifier` may differ from `source_issue.identifier",
        )
      }
      False -> {
        assert string.contains(
          prompt,
          "{% include \"fragments/execplan-identity-model.md\" %}",
        )
      }
    }
  })
  assert string.contains(
    identity_fragment,
    "implementation_handoff.issue_identifier` may differ from `source_issue.identifier",
  )
  assert string.contains(identity_fragment, "expected")

  list.each(
    [
      ".scherzo/workflows/prompts/execplan-implementation-verify-completion.md",
      ".scherzo/workflows/prompts/execplan-implementation-repair-base-drift.md",
      ".scherzo/workflows/prompts/execplan-implementation-recover-plan-completion.md",
    ],
    fn(path) {
      let assert Ok(prompt) = simplifile.read(path)
      assert string.contains(
        prompt,
        "$SCHERZO_RUN_ROOT/state/implementation/execplan-review-doc.md",
      )
      assert string.contains(
        prompt,
        "$SCHERZO_RUN_ROOT/state/implementation/execplan-implementation-pack.json",
      )
      assert string.contains(
        prompt,
        "$SCHERZO_RUN_ROOT/state/implementation/execplan-bundle.json",
      )
    },
  )

  let assert Ok(verify_prompt) =
    simplifile.read(
      ".scherzo/workflows/prompts/execplan-implementation-verify-completion.md",
    )
  let deleted_restore_subcommand = "restore-" <> "execplan-artifacts"
  let deleted_restore_instruction = "Run the " <> "restore" <> " command"
  assert !string.contains(verify_prompt, deleted_restore_subcommand)
  assert !string.contains(verify_prompt, deleted_restore_instruction)
  assert string.contains(
    verify_prompt,
    "Treat unchecked Progress checklist items",
  )
  assert string.contains(verify_prompt, "deferred_manual_verification")
  assert string.contains(
    verify_prompt,
    "post-implementation manual/browser/dogfood",
  )
  assert string.contains(verify_prompt, "submit_plan_completion_verdict")
  assert string.contains(verify_prompt, "Submit only semantic verdict fields")
  assert string.contains(verify_prompt, "validation_retries")
  assert string.contains(
    verify_prompt,
    "gate-plan-completion --from-submission",
  )
  assert !string.contains(verify_prompt, "plan-completion-context")

  let assert Ok(recovery_prompt) =
    simplifile.read(
      ".scherzo/workflows/prompts/execplan-implementation-recover-plan-completion.md",
    )
  assert string.contains(recovery_prompt, "workflow_step_recovery_input")
  assert string.contains(
    recovery_prompt,
    "submit_workflow_step_recovery_result",
  )
  assert string.contains(recovery_prompt, "gave_up")
  assert string.contains(recovery_prompt, "recheck")

  let assert Ok(repair_prompt) =
    simplifile.read(".scherzo/workflows/prompts/repair-base-drift.md")
  assert !string.contains(repair_prompt, "{{ issue.description }}")
  assert string.contains(
    repair_prompt,
    "$SCHERZO_RUN_ROOT/state/implementation/scherzo-implementation-validation.json",
  )
  assert string.contains(repair_prompt, "failure_summary")
  assert string.contains(repair_prompt, "stdout_excerpt")
  assert string.contains(repair_prompt, "stderr_excerpt")
  assert string.contains(repair_prompt, "workflow_step_recovery_input")
  assert string.contains(repair_prompt, "submit_workflow_step_recovery_result")
}

pub fn implementation_workflows_refresh_and_repair_before_publish_test() {
  let assert Ok(implementation) =
    simplifile.read(".scherzo/workflows/implementation.yaml")
  let assert Ok(execplan) =
    simplifile.read(".scherzo/workflows/execplan-implementation.yaml")
  let assert Ok(merge_conflict) =
    simplifile.read(".scherzo/workflows/merge-conflict-resolution.yaml")

  list.each([implementation, execplan, merge_conflict], fn(workflow) {
    assert string.contains(workflow, "mode: commit_stack")
    assert string.contains(workflow, "publish-commit-stack")
    assert !string.contains(workflow, "publish-change")
  })

  assert_workflow_refresh_ordering(
    implementation,
    "prepare_context",
    "implement",
    "apply_feedback",
    "prompts/repair-base-drift.md",
    "refresh_and_validate_after_review",
    "finalize_review_dispositions",
  )
  assert_workflow_refresh_ordering(
    execplan,
    "prepare_bundle",
    "implement_plan",
    "apply_review_feedback",
    "prompts/execplan-implementation-repair-base-drift.md",
    "refresh_and_validate_after_review",
    "verify_plan_completion_before_final_validation",
  )
  assert string.contains(
    execplan,
    "- id: verify_plan_completion_before_final_validation",
  )
  assert string.contains(
    execplan,
    "depends_on: [verify_plan_completion_before_final_validation]",
  )
}

pub fn execplan_implementation_workflow_has_plan_completion_gates_test() {
  let assert Ok(workflow) =
    simplifile.read(".scherzo/workflows/execplan-implementation.yaml")
  let deleted_restore_command = "restore-" <> "execplan-artifacts"
  let deleted_restore_step_prefix = "restore_" <> "execplan_artifacts"

  assert !string.contains(workflow, deleted_restore_command)
  assert !string.contains(workflow, deleted_restore_step_prefix)
  assert string.contains(workflow, "- id: verify_plan_completion")
  assert string.contains(workflow, "depends_on: [analyze_changes]")
  assert string.contains(
    workflow,
    "prompts/execplan-implementation-verify-completion.md",
  )
  assert string.contains(workflow, "validation_retries: 0")
  assert string.contains(workflow, "gate-plan-completion")
  assert string.contains(workflow, "--from-submission")
  assert string.contains(
    workflow,
    "prompts/execplan-implementation-recover-plan-completion.md",
  )
  assert string.contains(workflow, "attempts: 2")
  assert string.contains(workflow, "- id: validate_before_native_review")
  assert string.contains(workflow, "depends_on: [verify_plan_completion]")
  assert string.contains(workflow, "- id: refresh_and_validate_after_review")
  assert string.contains(
    workflow,
    "refresh-base-and-validate --stage before-validation",
  )
  assert string.contains(
    workflow,
    "prompts/execplan-implementation-repair-base-drift.md",
  )
  assert string.contains(
    workflow,
    "- id: verify_plan_completion_before_final_validation",
  )
  assert string.contains(
    workflow,
    "depends_on: [refresh_and_validate_after_review]",
  )
  assert string.contains(workflow, "- id: final_validate")
  assert string.contains(
    workflow,
    "depends_on: [verify_plan_completion_before_final_validation]",
  )
  assert string.contains(workflow, "- id: materialize_commit_stack")
  assert string.contains(workflow, "depends_on: [finalize_review_dispositions]")

  assert !string.contains(workflow, "apply_plan_completion_feedback")
  assert !string.contains(workflow, "verify_plan_completion_after_feedback")
  assert !string.contains(workflow, "verify_plan_completion_after_late_repair")
  assert !string.contains(workflow, "verify_plan_completion_after_final_repair")
  assert !string.contains(workflow, "checkpoint_final_plan_completion_verdict")
  assert !string.contains(workflow, "final_plan_completion_gate")
  assert !string.contains(workflow, "finalize_final_plan_completion_gate")
  assert !string.contains(workflow, "repair_base_drift")
  assert !string.contains(workflow, "assert_base_drift_repair")
}

fn assert_workflow_refresh_ordering(
  workflow: String,
  prepare_step: String,
  implement_step: String,
  feedback_step: String,
  repair_prompt: String,
  refresh_step: String,
  next_step: String,
) -> Nil {
  assert string.contains(workflow, "- id: refresh_base_before_implementation")
  assert string.contains(workflow, "depends_on: [" <> prepare_step <> "]")
  assert string.contains(workflow, "- id: " <> implement_step)
  assert string.contains(
    workflow,
    "depends_on: [refresh_base_before_implementation]",
  )
  assert string.contains(workflow, "- id: " <> refresh_step)
  assert string.contains(workflow, "depends_on: [" <> feedback_step <> "]")
  assert string.contains(
    workflow,
    "refresh-base-and-validate --stage before-validation",
  )
  assert string.contains(workflow, repair_prompt)
  assert string.contains(workflow, "attempts: 1")
  assert string.contains(workflow, "- id: " <> next_step)
}

fn setup_plan_completion_gate_fixture(dir: String) -> String {
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/docs/plans/example.md",
      "# Example ExecPlan\n\n## Progress\n\n- [x] Required work.\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation.json",
      "{\n"
        <> "  \"source_kind\": \"execplan\",\n"
        <> "  \"issue_identifier\": \"LIV-128\",\n"
        <> "  \"issue_title\": \"Implement example plan\",\n"
        <> "  \"issue_url\": \"https://linear.example/LIV-128\",\n"
        <> "  \"implementation_issue_identifier\": \"LIV-128\",\n"
        <> "  \"implementation_issue_title\": \"Implement example plan\",\n"
        <> "  \"implementation_issue_url\": \"https://linear.example/LIV-128\",\n"
        <> "  \"source_issue_identifier\": \"LIV-127\",\n"
        <> "  \"source_issue_title\": \"Example ExecPlan source\",\n"
        <> "  \"source_issue_url\": \"https://linear.example/LIV-127\",\n"
        <> "  \"plan_path\": \"docs/plans/example.md\",\n"
        <> "  \"base_change_id\": \"local-start\"\n"
        <> "}\n",
    )
  write_fake_jj(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/jj")

  let context =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation plan-completion-context",
    )
  assert context.status == step_artifact.StepSucceeded
  assert context.exit_code == Some(0)
  output_value(context.stdout, "PLAN_COMPLETION_DIFF_FINGERPRINT=")
}

fn output_value(stdout: String, prefix: String) -> String {
  let assert Ok(line) =
    string.split(stdout, on: "\n")
    |> list.find(fn(line) { string.starts_with(line, prefix) })
  string.drop_start(line, string.length(prefix))
}

fn plan_completion_submission_json(
  verdict: String,
  blocking_findings_json: String,
) -> String {
  "{\n"
  <> "  \"schema_version\": 1,\n"
  <> "  \"verdict\": \""
  <> verdict
  <> "\",\n"
  <> "  \"blocking_findings\": "
  <> blocking_findings_json
  <> ",\n"
  <> "  \"evidence\": [\"Required behavior is present.\"],\n"
  <> "  \"checked_acceptance_criteria\": [\"Required work.\"],\n"
  <> "  \"deferred_manual_verification\": []\n"
  <> "}\n"
}

fn structured_plan_completion_submission_json(
  step_id: String,
  verdict: String,
  blocking_findings_json: String,
) -> String {
  "{\n"
  <> "  \"schema_version\": 1,\n"
  <> "  \"artifact_type\": \"structured_output\",\n"
  <> "  \"run_id\": \"run-1\",\n"
  <> "  \"workflow_id\": \"execplan-implementation\",\n"
  <> "  \"step_id\": \""
  <> step_id
  <> "\",\n"
  <> "  \"attempt_index\": 0,\n"
  <> "  \"artifact_name\": \"plan_completion_verdict_submission\",\n"
  <> "  \"format\": \"json\",\n"
  <> "  \"source_type\": \"pi_tool_call\",\n"
  <> "  \"source_tool_name\": \"submit_plan_completion_verdict\",\n"
  <> "  \"payload\": {\n"
  <> "    \"verdict\": \""
  <> verdict
  <> "\",\n"
  <> "    \"blocking_findings\": "
  <> blocking_findings_json
  <> ",\n"
  <> "    \"evidence\": [\"Required behavior is present.\"],\n"
  <> "    \"checked_acceptance_criteria\": [\"Required work.\"],\n"
  <> "    \"deferred_manual_verification\": []\n"
  <> "  }\n"
  <> "}\n"
}

fn write_retained_plan_completion_submission(
  artifact_dir: String,
  step_id: String,
  contents: String,
) -> Nil {
  let structured_dir = artifact_dir <> "/" <> step_id <> "/attempt-0/structured"
  let structured_path =
    structured_dir <> "/plan_completion_verdict_submission.json"
  let assert Ok(Nil) = simplifile.create_directory_all(structured_dir)
  let assert Ok(Nil) = simplifile.write(structured_path, contents)
  let assert Ok(Nil) =
    simplifile.write(
      artifact_dir <> "/" <> step_id <> "/attempt-0.json",
      "{\n"
        <> "  \"artifact\": {\n"
        <> "    \"status\": \"success\",\n"
        <> "    \"structured_output\": {\n"
        <> "      \"status\": \"valid\",\n"
        <> "      \"artifact_name\": \"plan_completion_verdict_submission\",\n"
        <> "      \"path\": \""
        <> step_id
        <> "/attempt-0/structured/plan_completion_verdict_submission.json\"\n"
        <> "    }\n"
        <> "  }\n"
        <> "}\n",
    )
  Nil
}

fn write_plan_completion_submission(
  dir: String,
  verdict: String,
  blocking_findings_json: String,
) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/plan-completion-submission.json",
      plan_completion_submission_json(verdict, blocking_findings_json),
    )
  Nil
}

fn write_plan_completion_verdict(
  dir: String,
  verdict: String,
  fingerprint: String,
  blocking_findings_json: String,
) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-plan-completion-verdict.json",
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"verdict\": \""
        <> verdict
        <> "\",\n"
        <> "  \"blocking_findings\": "
        <> blocking_findings_json
        <> ",\n"
        <> "  \"evidence\": [\"Required behavior is present.\"],\n"
        <> "  \"checked_acceptance_criteria\": [\"Required work.\"],\n"
        <> "  \"plan_path\": \"docs/plans/example.md\",\n"
        <> "  \"verified_base_change_id\": \"local-start\",\n"
        <> "  \"verified_change_id\": \"publishchange\",\n"
        <> "  \"verified_diff_fingerprint\": \""
        <> fingerprint
        <> "\",\n"
        <> "  \"changed_files\": [\"scripts/scherzo-implementation\"]\n"
        <> "}\n",
    )
  Nil
}

fn write_plan_completion_verdict_with_deferred_manual_verification(
  dir: String,
  fingerprint: String,
) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-plan-completion-verdict.json",
      "{\n"
        <> "  \"schema_version\": 1,\n"
        <> "  \"verdict\": \"pass\",\n"
        <> "  \"blocking_findings\": [],\n"
        <> "  \"evidence\": [\"Implementation prerequisites for manual check are present.\"],\n"
        <> "  \"checked_acceptance_criteria\": [\"Required work.\"],\n"
        <> "  \"plan_path\": \"docs/plans/example.md\",\n"
        <> "  \"verified_base_change_id\": \"local-start\",\n"
        <> "  \"verified_change_id\": \"publishchange\",\n"
        <> "  \"verified_diff_fingerprint\": \""
        <> fingerprint
        <> "\",\n"
        <> "  \"changed_files\": [\"scripts/scherzo-implementation\"],\n"
        <> "  \"deferred_manual_verification\": [\n"
        <> "    {\n"
        <> "      \"check\": \"Dogfood the browser flow\",\n"
        <> "      \"reason\": \"Requires a human browser session after implementation\",\n"
        <> "      \"owner\": \"operator\",\n"
        <> "      \"when\": \"after implementation workflow\"\n"
        <> "    }\n"
        <> "  ]\n"
        <> "}\n",
    )
  Nil
}

pub fn checkpointed_plan_completion_verdict_ignores_tmp_execplan_bundle_clobber_test() {
  let dir = "test/tmp/implementation-helper-verdict-checkpoint"
  let fingerprint = setup_plan_completion_gate_fixture(dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/run-root/state/implementation")
  write_plan_completion_verdict(dir, "pass", fingerprint, "[]")

  let checkpoint =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=\"$PWD/run-root\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation checkpoint-plan-completion-verdict",
    )

  assert checkpoint.status == step_artifact.StepSucceeded
  assert checkpoint.exit_code == Some(0)
  assert string.contains(
    checkpoint.stdout,
    "PLAN_COMPLETION_VERDICT_CHECKPOINT=written",
  )
  let assert Ok(canonical_verdict) =
    simplifile.read(
      dir
      <> "/run-root/state/implementation/scherzo-plan-completion-verdict.json",
    )
  assert string.contains(canonical_verdict, "\"verdict\": \"pass\"")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-plan-completion-verdict.json",
      "{\"verdict\":\"pass\"}\n",
    )
  let assert Ok(Nil) =
    simplifile.write(dir <> "/tmp/execplan-bundle.json", "not-json\n")

  let gate =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=\"$PWD/run-root\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation gate-plan-completion --final",
    )

  assert gate.status == step_artifact.StepSucceeded
  assert gate.exit_code == Some(0)
  assert !string.contains(
    gate.stdout,
    "PLAN_COMPLETION_VERDICT_" <> "RESTORE_STATUS",
  )
  assert string.contains(gate.stdout, "PLAN_COMPLETION_GATE=passed")
  let assert Ok(clobbered_bundle) =
    simplifile.read(dir <> "/tmp/execplan-bundle.json")
  assert clobbered_bundle == "not-json\n"
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

fn execplan_markdown() -> String {
  execplan_markdown_with_title("Example ExecPlan")
}

fn execplan_markdown_with_title(title: String) -> String {
  "# "
  <> title
  <> "\n\n"
  <> "## Progress\n\n- [x] Drafted.\n\n"
  <> "## Surprises & Discoveries\n\nNone yet.\n\n"
  <> "## Decision Log\n\n"
  <> "- Decision: Keep the fixture small.\n"
  <> "  Rationale: The tests only need a structurally valid plan.\n"
  <> "  Date: 2026-05-13\n\n"
  <> "## Outcomes & Retrospective\n\nPending completion.\n\n"
  <> "## Scope Boundaries\n\nIn scope: Brief generation.\n\n"
  <> "## Milestones\n\nGenerate a brief.\n\n"
  <> "## Concrete Steps\n\n1. Run the helper.\n\n"
  <> "## Testing and Falsifiability\n\nThe stale check must fail after mutation.\n\n"
  <> "## Validation and Acceptance\n\nThe generated brief names this section.\n\n"
  <> "## Open Questions and Clarifications Needed\n\nNone.\n"
}

fn write_failing_brief_helper(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "echo failing brief helper >&2\n"
        <> "touch tmp/scherzo-execplan-brief.md tmp/scherzo-execplan-index.json\n"
        <> "exit 9\n",
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

fn write_fake_analyze_jj(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> jj.log\n"
        <> "if [ \"$1\" = diff ]; then echo 'src/example.gleam'; exit 0; fi\n"
        <> "if [ \"$1\" = log ]; then echo localchange; exit 0; fi\n"
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
        <> "if [ \"$1\" = git ] && [ \"$2\" = remote ]; then if [ -n \"${SCHERZO_FAKE_JJ_REMOTES+x}\" ]; then printf '%s\\n' \"$SCHERZO_FAKE_JJ_REMOTES\"; else printf '%s\\n' 'origin https://github.com/example/repo.git' 'fork git@github-scherzo-agent:example/repo.git'; fi; exit 0; fi\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = fetch ]; then exit 0; fi\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = push ]; then exit 0; fi\n"
        <> "if [ \"$1\" = diff ]; then\n"
        <> "  summary=0\n"
        <> "  for arg in \"$@\"; do if [ \"$arg\" = --summary ]; then summary=1; fi; done\n"
        <> "  if [ \"$summary\" = 1 ]; then echo 'M .scherzo/workflows/scripts/scherzo-implementation'; else echo '.scherzo/workflows/scripts/scherzo-implementation'; fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = rebase ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_JJ_REBASE_FAIL:-}\" = 1 ]; then echo 'simulated rebase conflict' >&2; exit 1; fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = describe ]; then touch .fake-described; exit 0; fi\n"
        <> "if [ \"$1\" = bookmark ]; then exit 0; fi\n"
        <> "if [ \"$1\" = status ]; then exit 0; fi\n"
        <> "if [ \"$1\" = debug ] && [ \"$2\" = object ] && [ \"$3\" = commit ]; then printf '%s\n' 'Commit {' '  root_tree: Resolved(' '    TreeId(' '      \"4444444444444444444444444444444444444444\",' '    ),' '  ),' '}'; exit 0; fi\n"
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
        <> "    main@origin) case \"$template\" in *commit_id*) echo 1111111111111111111111111111111111111111;; *) echo remotecommit;; esac; exit 0;;\n"
        <> "    @-) case \"$template\" in *commit_id*) echo 2222222222222222222222222222222222222222;; *) echo localparentcommit;; esac; exit 0;;\n"
        <> "    @) case \"$template\" in *change_id.short*) echo publishchange;; *description*) if [ \"${SCHERZO_FAKE_EMPTY_DESCRIPTION:-}\" = 1 ] && [ ! -f .fake-described ]; then printf '\\n'; else echo currentdescription; fi;; *commit_id*) if [ \"${SCHERZO_FAKE_INVALID_COMMIT_ID:-}\" = 1 ]; then echo not-a-git-oid; elif [ -f .fake-described ]; then echo 5555555555555555555555555555555555555555; else echo 3333333333333333333333333333333333333333; fi;; *) echo currentcommit;; esac; exit 0;;\n"
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
        <> "if [ \"$1\" = git ] && [ \"$2\" = remote ]; then printf '%s\\n' 'origin https://github.com/example/repo.git' 'fork git@github-scherzo-agent:example/repo.git'; exit 0; fi\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = fetch ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_REFRESH_FETCH_FAIL:-}\" = 1 ]; then echo 'fetch failed' >&2; exit 3; fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = push ]; then exit 0; fi\n"
        <> "if [ \"$1\" = diff ]; then\n"
        <> "  summary=0\n"
        <> "  for arg in \"$@\"; do if [ \"$arg\" = --summary ]; then summary=1; fi; done\n"
        <> "  if [ \"$summary\" = 1 ]; then echo 'M .scherzo/workflows/scripts/scherzo-implementation'; else echo '.scherzo/workflows/scripts/scherzo-implementation'; fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = rebase ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_REFRESH_CONFLICT_AFTER_REBASE:-}\" = 1 ]; then touch .fake-conflict; echo 'simulated conflict' >&2; exit 1; fi\n"
        <> "  if [ \"${SCHERZO_FAKE_REFRESH_REBASE_FAIL:-}\" = 1 ]; then echo 'simulated rebase infrastructure failure' >&2; exit 1; fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = resolve ] && [ \"$2\" = --list ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_REFRESH_CONFLICT:-}\" = 1 ] || [ -f .fake-conflict ]; then echo 'src/conflicted.gleam    2-sided conflict'; exit 0; fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = describe ]; then touch .fake-described; exit 0; fi\n"
        <> "if [ \"$1\" = bookmark ]; then exit 0; fi\n"
        <> "if [ \"$1\" = status ]; then exit 0; fi\n"
        <> "if [ \"$1\" = debug ] && [ \"$2\" = object ] && [ \"$3\" = commit ]; then printf '%s\n' 'Commit {' '  root_tree: Resolved(' '    TreeId(' '      \"4444444444444444444444444444444444444444\",' '    ),' '  ),' '}'; exit 0; fi\n"
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
        <> "    main@origin) if [ \"${SCHERZO_FAKE_REFRESH_BASE_MISSING:-}\" = 1 ]; then exit 1; fi; case \"$template\" in *commit_id*) echo 1111111111111111111111111111111111111111;; *) echo remotecommit;; esac; exit 0;;\n"
        <> "    main) if [ \"${SCHERZO_FAKE_REFRESH_BASE_MISSING:-}\" = 1 ]; then exit 1; fi; case \"$template\" in *commit_id*) echo 1111111111111111111111111111111111111111;; *) echo localfallbackcommit;; esac; exit 0;;\n"
        <> "    @-) case \"$template\" in *change_id*) echo refreshed-base-change;; *commit_id*) if [ \"${SCHERZO_FAKE_REFRESH_PARENT_MATCH:-}\" = 1 ]; then echo 1111111111111111111111111111111111111111; else echo 2222222222222222222222222222222222222222; fi;; *) if [ \"${SCHERZO_FAKE_REFRESH_PARENT_MATCH:-}\" = 1 ]; then echo remotecommit; else echo localparentcommit; fi;; esac; exit 0;;\n"
        <> "    @) case \"$template\" in *change_id.short*) echo refreshchange;; *description*) if [ \"${SCHERZO_FAKE_EMPTY_DESCRIPTION:-}\" = 1 ] && [ ! -f .fake-described ]; then printf '\\n'; else echo currentdescription; fi;; *commit_id*) if [ \"${SCHERZO_FAKE_INVALID_COMMIT_ID:-}\" = 1 ]; then echo not-a-git-oid; elif [ -f .fake-described ]; then echo 5555555555555555555555555555555555555555; else echo 3333333333333333333333333333333333333333; fi;; *) echo currentcommit;; esac; exit 0;;\n"
        <> "    conflicts*) if [ \"${SCHERZO_FAKE_REFRESH_CONFLICT:-}\" = 1 ] || [ -f .fake-conflict ]; then echo conflictchange; fi; exit 0;;\n"
        <> "    remote_bookmarks*) exit 0;;\n"
        <> "    *) exit 1;;\n"
        <> "  esac\n"
        <> "fi\n"
        <> "exit 1\n",
    )
  Nil
}

fn write_fake_git(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> git.log\n"
        <> "if [ \"$1\" = update-ref ]; then exit 0; fi\n"
        <> "if [ \"$1 $2\" = 'bundle create' ]; then mkdir -p \"$(dirname \"$3\")\"; printf 'fake bundle for %s\\n' \"$*\" > \"$3\"; exit 0; fi\n"
        <> "if [ \"$1 $2\" = 'bundle verify' ]; then test -s \"$3\"; exit $?; fi\n"
        <> "exit 1\n",
    )
  Nil
}

fn write_unknown_refresh_status_driver(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> driver.log\n"
        <> "if [ \"$1\" = refresh-base ]; then\n"
        <> "  printf '%s\\n' '{\"version\":1,\"status\":\"invalid_request\",\"failure_code\":\"boom\",\"message\":\"refresh failed\"}'\n"
        <> "  exit 2\n"
        <> "fi\n"
        <> "printf 'unexpected driver command: %s\\n' \"$*\" >&2\n"
        <> "exit 2\n",
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
        <> "if [ \"${SCHERZO_FAIL_IF_WORKFLOW_BUNDLE_DIR_LEAKS:-}\" = 1 ] && [ -n \"${SCHERZO_WORKFLOW_BUNDLE_DIR:-}\" ]; then echo 'SCHERZO_WORKFLOW_BUNDLE_DIR leaked into validation' >&2; exit 1; fi\n"
        <> "if [ \"${SCHERZO_FAIL_IF_WORKSPACE_DRIVER_LEAKS:-}\" = 1 ] && { [ -n \"${SCHERZO_WORKSPACE_DRIVER:-}\" ] || [ -n \"${SCHERZO_WORKSPACE_PROFILE:-}\" ] || [ -n \"${SCHERZO_WORKSPACE_CAPABILITIES:-}\" ] || [ -n \"${SCHERZO_WORKSPACE_ROOT:-}\" ]; }; then echo 'SCHERZO_WORKSPACE driver context leaked into validation' >&2; exit 1; fi\n"
        <> "if [ \"${SCHERZO_FAIL_IF_PUBLICATION_ENV_LEAKS:-}\" = 1 ] && env | grep -E '^(GITHUB_REPOSITORY|SCHERZO_GITHUB_REPO|SCHERZO_JJ_WORKSPACE_BASE|SCHERZO_JJ_WORKSPACE_BASE_BRANCH|SCHERZO_JJ_WORKSPACE_FETCH_BASE|SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE|SCHERZO_JJ_WORKSPACE_REMOTE|SCHERZO_PR_BASE|SCHERZO_PR_DRAFT|SCHERZO_PR_REMOTE|SCHERZO_PR_REPO|SCHERZO_REPO_ROOT)=' >/dev/null; then echo 'SCHERZO publication environment leaked into validation' >&2; exit 1; fi\n"
        <> "case \"$*\" in\n"
        <> "  'exec . scripts/scherzo-ci'*) if [ \"${SCHERZO_FAKE_DIRENV_CI_FAIL:-}\" = 1 ] || [ \"${SCHERZO_FAKE_DIRENV_TEST_FAIL:-}\" = 1 ]; then echo 'simulated scherzo-ci validation failure' >&2; exit 1; fi;;\n"
        <> "esac\n"
        <> "exit 0\n",
    )
  Nil
}
