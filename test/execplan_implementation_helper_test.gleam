import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/hash
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

fn canonical_execplan_plan_path(dir: String) -> String {
  dir <> "/run-root/state/implementation/execplan-review-doc.md"
}

fn canonical_execplan_pack_path(dir: String) -> String {
  dir <> "/run-root/state/implementation/execplan-implementation-pack.json"
}

fn canonical_execplan_bundle_path(dir: String) -> String {
  dir <> "/run-root/state/implementation/execplan-bundle.json"
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

pub fn extract_plan_requires_exactly_one_existing_plan_path_test() {
  let dir = "test/tmp/execplan-helper-extract"
  test_helpers.reset_dir(dir)
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

pub fn extract_plan_accepts_html_plan_paths_test() {
  let dir = "test/tmp/execplan-helper-extract-html"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/docs/plans/example-plan.html",
      html_execplan("Example"),
    )
  let text_path = dir <> "/issue.txt"
  let assert Ok(Nil) =
    simplifile.write(text_path, "Plan path: `docs/plans/example-plan.html`\n")

  let artifact = run_helper("extract-plan " <> text_path <> " " <> dir)

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "PLAN_PATH=docs/plans/example-plan.html",
  )
}

pub fn extract_plan_error_messages_prefer_markdown_and_allow_legacy_html_test() {
  let dir = "test/tmp/execplan-helper-extract-error-copy"
  test_helpers.reset_dir(dir)
  let text_path = dir <> "/issue.txt"
  let assert Ok(Nil) =
    simplifile.write(text_path, "Plan path: `not-a-plan.txt`\n")

  let artifact = run_helper("extract-plan " <> text_path <> " " <> dir)

  assert artifact.status == step_artifact.StepFailed
  assert string.contains(artifact.stderr, "docs/plans/example.md")
  assert string.contains(artifact.stderr, "legacy `docs/plans/example.html`")
  assert !string.contains(
    artifact.stderr,
    "Plan path: `docs/plans/example.html`.",
  )
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
        <> "  \"base_change_id\": \"local-start\"\n"
        <> "}\n",
    )
  write_failing_brief_helper(dir <> "/bin/failing-brief-helper")
  test_helpers.chmod_executable(dir <> "/bin/failing-brief-helper")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_EXECPLAN_HTML_HELPER=bin/failing-brief-helper ../../../.scherzo/workflows/scripts/scherzo-implementation plan-brief",
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

pub fn restore_execplan_artifacts_restores_tmp_cache_from_run_root_test() {
  let dir = "test/tmp/implementation-helper-restore-execplan-artifacts"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/run-root/state/implementation")
  let plan =
    "# Canonical Plan\n\n## Progress\n\n- [ ] Implementation pending.\n"
  let pack = "{\"pack\":\"canonical\"}\n"
  let bundle = "{\"bundle\":\"canonical\"}\n"
  let assert Ok(Nil) = simplifile.write(canonical_execplan_plan_path(dir), plan)
  let assert Ok(Nil) = simplifile.write(canonical_execplan_pack_path(dir), pack)
  let assert Ok(Nil) =
    simplifile.write(canonical_execplan_bundle_path(dir), bundle)
  let metadata =
    "{\n"
    <> "  \"source_kind\": \"execplan\",\n"
    <> "  \"plan_path\": \"tmp/execplan-review-doc.md\",\n"
    <> "  \"plan_sha256\": \""
    <> hash.sha256_hex(plan)
    <> "\",\n"
    <> "  \"plan_bytes\": "
    <> int.to_string(string.length(plan))
    <> ",\n"
    <> "  \"base_change_id\": \"canonical-start\",\n"
    <> "  \"canonical_plan_path\": \"run-root/state/implementation/execplan-review-doc.md\",\n"
    <> "  \"execplan_v2_implementation_pack_path\": \"tmp/execplan-implementation-pack.json\",\n"
    <> "  \"execplan_v2_bundle_path\": \"tmp/execplan-bundle.json\",\n"
    <> "  \"canonical_execplan_v2_implementation_pack_path\": \"run-root/state/implementation/execplan-implementation-pack.json\",\n"
    <> "  \"canonical_execplan_v2_bundle_path\": \"run-root/state/implementation/execplan-bundle.json\"\n"
    <> "}\n"
  let assert Ok(Nil) = simplifile.write(metadata_canonical_path(dir), metadata)
  let assert Ok(Nil) =
    simplifile.write(dir <> "/tmp/execplan-review-doc.md", "# Fixture\n")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/execplan-implementation-pack.json",
      "fixture\n",
    )
  let assert Ok(Nil) =
    simplifile.write(dir <> "/tmp/execplan-bundle.json", "fixture\n")

  let artifact =
    run_helper_in(
      dir,
      clean_workflow_env()
        <> " "
        <> run_root_env()
        <> " ../../../.scherzo/workflows/scripts/scherzo-implementation restore-execplan-artifacts",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "EXECPLAN_ARTIFACT_RESTORE_STATUS=restored",
  )
  let assert Ok(restored_plan) =
    simplifile.read(dir <> "/tmp/execplan-review-doc.md")
  let assert Ok(restored_pack) =
    simplifile.read(dir <> "/tmp/execplan-implementation-pack.json")
  let assert Ok(restored_bundle) =
    simplifile.read(dir <> "/tmp/execplan-bundle.json")
  assert restored_plan == plan
  assert restored_pack == pack
  assert restored_bundle == bundle
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

pub fn prepare_execplan_writes_canonical_metadata_and_cache_test() {
  let dir = "test/tmp/implementation-helper-prepare-execplan-canonical"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/run-root")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/docs/plans/example.md", execplan_markdown())
  write_fake_jj(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/jj")
  write_linear_graphql_fixture(
    dir <> "/linear-execplan.json",
    "{"
      <> "\"identifier\":\"LIV-230\","
      <> "\"title\":\"Implement durable metadata\","
      <> "\"description\":\"Plan path: `docs/plans/example.md`\","
      <> "\"url\":\"https://linear.example/LIV-230\","
      <> "\"comments\":{\"nodes\":[]}}",
  )

  let artifact =
    run_helper_in(
      dir,
      clean_workflow_env()
        <> " "
        <> run_root_env()
        <> " SCHERZO_ISSUE_IDENTIFIER=LIV-230 SCHERZO_TEST_LINEAR_GRAPHQL_JSON=linear-execplan.json PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation prepare --source execplan",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "SOURCE_KIND=execplan")
  assert string.contains(artifact.stdout, "PLAN_PATH=docs/plans/example.md")
  assert string.contains(artifact.stdout, "PLAN_BRIEF_STATUS=ok")
  let assert Ok(cache) = simplifile.read(metadata_cache_path(dir))
  let assert Ok(canonical) = simplifile.read(metadata_canonical_path(dir))
  assert cache == canonical
  assert string.contains(cache, "\"source_kind\": \"execplan\"")
  assert string.contains(cache, "\"issue_identifier\": \"LIV-230\"")
  assert string.contains(
    cache,
    "\"issue_title\": \"Implement durable metadata\"",
  )
  assert string.contains(
    cache,
    "\"issue_url\": \"https://linear.example/LIV-230\"",
  )
  assert string.contains(cache, "\"plan_path\": \"docs/plans/example.md\"")
  assert string.contains(cache, "\"base_change_id\": \"localparentcommit\"")
  assert string.contains(cache, "\"plan_brief_status\": \"ok\"")
  assert string.contains(
    cache,
    "\"plan_brief_path\": \"tmp/scherzo-execplan-brief.md\"",
  )
  assert string.contains(
    cache,
    "\"plan_index_path\": \"tmp/scherzo-execplan-index.json\"",
  )
  assert string.contains(cache, "\"plan_source_sha256\":")
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

pub fn extract_plan_prefers_explicit_plan_field_over_liv59_context_references_test() {
  let dir = "test/tmp/execplan-helper-explicit-plan"
  test_helpers.reset_dir(dir)
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
  test_helpers.reset_dir(dir)
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
  test_helpers.reset_dir(dir)
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
  test_helpers.reset_dir(dir)
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
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/main")
  write_fake_prepare_jj(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/jj")

  let artifact =
    run_helper_in(
      dir <> "/main",
      "SCHERZO_ISSUE_IDENTIFIER=LIV-71 LINEAR_API_KEY= PATH=\"$PWD/../bin:$PATH\" ../../../../.scherzo/workflows/scripts/scherzo-implementation prepare --source execplan",
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
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/main")
  write_noisy_failing_prepare_jj(dir <> "/bin/jj")
  test_helpers.chmod_executable(dir <> "/bin/jj")

  let artifact =
    run_helper_in(
      dir <> "/main",
      "SCHERZO_ISSUE_IDENTIFIER=LIV-71 LINEAR_API_KEY= PATH=\"$PWD/../bin:$PATH\" ../../../../.scherzo/workflows/scripts/scherzo-implementation prepare --source execplan",
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

pub fn plan_completion_recovery_reports_repair_needed_for_fresh_fail_test() {
  let dir = "test/tmp/plan-completion-recovery-repair-needed"
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
      "SCHERZO_RUN_ROOT=\"$PWD\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation plan-completion-recovery --phase before-late-repair --attempt 2 --max-attempts 3",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "PLAN_COMPLETION_RECOVERY_STATUS=repair_needed",
  )
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_RECOVERY_ATTEMPT=2")
  assert string.contains(
    artifact.stdout,
    "PLAN_COMPLETION_RECOVERY_MAX_ATTEMPTS=3",
  )
  assert string.contains(
    artifact.stdout,
    "Acceptance criterion remains unchecked.",
  )
  let assert Ok(recovery_json) =
    simplifile.read(dir <> "/tmp/scherzo-plan-completion-recovery.json")
  assert string.contains(recovery_json, "\"status\": \"repair_needed\"")
  assert string.contains(recovery_json, "\"attempt\": 2")
  assert string.contains(recovery_json, "\"max_attempts\": 3")
  let assert Ok(recovery_md) =
    simplifile.read(dir <> "/tmp/scherzo-plan-completion-recovery.md")
  assert string.contains(recovery_md, "Acceptance criterion remains unchecked.")
}

pub fn plan_completion_recovery_reports_repair_needed_before_final_repair_test() {
  let dir = "test/tmp/plan-completion-recovery-final-repair-needed"
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
      "SCHERZO_RUN_ROOT=\"$PWD\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation plan-completion-recovery --phase before-final-repair --attempt 3 --max-attempts 3",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "PLAN_COMPLETION_RECOVERY_STATUS=repair_needed",
  )
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_RECOVERY_ATTEMPT=3")
  let assert Ok(recovery_json) =
    simplifile.read(dir <> "/tmp/scherzo-plan-completion-recovery.json")
  assert string.contains(recovery_json, "\"phase\": \"before-final-repair\"")
  assert string.contains(recovery_json, "\"status\": \"repair_needed\"")
  assert string.contains(recovery_json, "\"attempt\": 3")
  assert string.contains(recovery_json, "\"max_attempts\": 3")
}

pub fn plan_completion_recovery_reports_not_needed_for_fresh_pass_test() {
  let dir = "test/tmp/plan-completion-recovery-not-needed"
  let fingerprint = setup_plan_completion_gate_fixture(dir)
  write_plan_completion_verdict(dir, "pass", fingerprint, "[]")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=\"$PWD\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation plan-completion-recovery --phase before-late-repair --attempt 2 --max-attempts 3",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "PLAN_COMPLETION_RECOVERY_STATUS=not_needed",
  )
  let assert Ok(recovery_json) =
    simplifile.read(dir <> "/tmp/scherzo-plan-completion-recovery.json")
  assert string.contains(recovery_json, "\"status\": \"not_needed\"")
  let assert Error(_) = simplifile.read(dir <> "/.scherzo-keep-workspace")
}

pub fn plan_completion_recovery_preserves_terminal_verdict_failures_test() {
  let dir = "test/tmp/plan-completion-recovery-malformed"
  let _fingerprint = setup_plan_completion_gate_fixture(dir)
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-plan-completion-verdict.json",
      "{not json}\n",
    )

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=\"$PWD\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation plan-completion-recovery --phase before-late-repair --attempt 2 --max-attempts 3",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.failure_code == Some("plan_completion_verdict_malformed")
}

pub fn plan_completion_recovery_exhausts_after_final_repair_test() {
  let dir = "test/tmp/plan-completion-recovery-exhausted"
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
      "SCHERZO_RUN_ROOT=\"$PWD\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation plan-completion-recovery --phase after-final-repair --attempt 3 --max-attempts 3",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert artifact.failure_code == Some("plan_completion_recovery_exhausted")
  assert string.contains(
    artifact.stdout,
    "PLAN_COMPLETION_RETENTION_MARKER=.scherzo-keep-workspace",
  )
  let assert Ok(marker) = simplifile.read(dir <> "/.scherzo-keep-workspace")
  assert string.contains(marker, "Source kind: execplan")
  assert string.contains(marker, "Source: LIV-128")
  assert !string.contains(marker, "LIV-127")
  let assert Ok(recovery_json) =
    simplifile.read(dir <> "/tmp/scherzo-plan-completion-recovery.json")
  assert string.contains(recovery_json, "\"status\": \"exhausted\"")
  assert string.contains(
    recovery_json,
    "\"failure_code\": \"plan_completion_recovery_exhausted\"",
  )
  assert string.contains(
    recovery_json,
    "\"retention_marker\": \".scherzo-keep-workspace\"",
  )
  assert !string.contains(recovery_json, "/.scherzo-keep-workspace")
  let assert Ok(recovery_md) =
    simplifile.read(dir <> "/tmp/scherzo-plan-completion-recovery.md")
  assert string.contains(recovery_md, "Acceptance criterion remains unchecked.")
  assert string.contains(
    recovery_md,
    "Retention marker: `.scherzo-keep-workspace`",
  )
  assert string.contains(
    recovery_json,
    "\"retry_command\": \"scherzoctl retry LIV-128\"",
  )
  assert !string.contains(recovery_json, "LIV-127")
  assert string.contains(recovery_md, "scherzoctl retry LIV-128")
  assert !string.contains(recovery_md, "scherzoctl retry LIV-127")
}

pub fn plan_completion_recovery_exhausts_at_final_gate_test() {
  let dir = "test/tmp/plan-completion-recovery-final"
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
      "SCHERZO_RUN_ROOT=\"$PWD\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation plan-completion-recovery --phase final --attempt 3 --max-attempts 3",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.failure_code == Some("plan_completion_recovery_exhausted")
  assert !string.contains(
    artifact.stdout,
    "PLAN_COMPLETION_RECOVERY_STATUS=repair_needed",
  )
  let assert Ok(recovery_json) =
    simplifile.read(dir <> "/tmp/scherzo-plan-completion-recovery.json")
  assert string.contains(recovery_json, "\"phase\": \"final\"")
}

pub fn plan_completion_recovery_rejects_over_budget_before_final_repair_test() {
  let dir = "test/tmp/plan-completion-recovery-over-budget"
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
      "SCHERZO_RUN_ROOT=\"$PWD\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation plan-completion-recovery --phase before-final-repair --attempt 4 --max-attempts 3",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.failure_code == Some("plan_completion_recovery_exhausted")
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
  assert string.contains(
    direnv_log,
    "exec . selfci check --base main@scherzo-agent --candidate @ --print-output",
  )
  assert !string.contains(direnv_log, "exec . gleam format --check src test")
  assert !string.contains(direnv_log, "exec . gleam test")
  let assert Ok(validation_json) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-validation.json")
  assert string.contains(validation_json, "\"status\": \"passed\"")
  assert string.contains(validation_json, "\"validator\": \"selfci\"")
  assert string.contains(
    validation_json,
    "\"base_revision\": \"main@scherzo-agent\"",
  )
  assert string.contains(
    validation_json,
    "direnv exec . selfci check --base main@scherzo-agent --candidate @ --print-output",
  )
}

pub fn validate_uses_latest_refresh_base_revision_for_selfci_test() {
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

pub fn validate_failure_writes_structured_failure_artifact_test() {
  let dir = "test/tmp/implementation-helper-validate-failure-artifact"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_fake_direnv(dir <> "/bin/direnv")
  test_helpers.chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_FAKE_DIRENV_SELFCI_FAIL=1 SCHERZO_JJ_WORKSPACE_REMOTE=origin SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation validate",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stdout, "FINAL_VALIDATION=failed")
  assert string.contains(
    artifact.stdout,
    "VALIDATION_RESULT_PATH=tmp/scherzo-implementation-validation.json",
  )
  assert string.contains(artifact.stderr, "Structured validation artifact")
  assert string.contains(artifact.stderr, "simulated SelfCI validation failure")
  let assert Ok(validation_json) =
    simplifile.read(dir <> "/tmp/scherzo-implementation-validation.json")
  assert string.contains(validation_json, "\"status\": \"failed\"")
  assert string.contains(validation_json, "\"exit_code\": 1")
  assert string.contains(validation_json, "simulated SelfCI validation failure")
}

pub fn validate_base_drift_marker_reports_previous_validation_summary_test() {
  let dir = "test/tmp/implementation-helper-base-drift-validation-summary"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation-base-drift-failure.md",
      "# Base drift repair failure\n\n## Reason\nValidation failed without base drift.\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/tmp/scherzo-implementation-validation.json",
      "{\n"
        <> "  \"status\": \"failed\",\n"
        <> "  \"exit_code\": 1,\n"
        <> "  \"base_revision\": \"main@origin\",\n"
        <> "  \"commands\": [\"direnv exec . selfci check --base main@origin --candidate @ --print-output\"],\n"
        <> "  \"failure_summary\": \"error: hash mismatch in fixed-output derivation\\n         specified: sha256-old\\n            got:    sha256-new\"\n"
        <> "}\n",
    )

  let artifact =
    run_helper_in(
      dir,
      "../../../.scherzo/workflows/scripts/scherzo-implementation validate",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(
    artifact.stderr,
    "base drift repair requested workflow failure",
  )
  assert string.contains(artifact.stderr, "Previous validation result:")
  assert string.contains(artifact.stderr, "hash mismatch")
  assert string.contains(
    artifact.stderr,
    "structured_validation_artifact: tmp/scherzo-implementation-validation.json",
  )
  assert string.contains(
    artifact.stderr,
    ".scherzo/command-step-diagnostics/validate_after_refresh.txt",
  )
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
    "COMMIT_STACK_PATH=tmp/scherzo-implementation-commit-stack.json",
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

pub fn execplan_implementation_prompts_trim_validation_payloads_test() {
  let execplan_prompt_paths = [
    ".scherzo/workflows/prompts/execplan-implementation-implement.md",
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion.md",
    ".scherzo/workflows/prompts/execplan-implementation-apply-plan-completion-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion-after-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-review.md",
    ".scherzo/workflows/prompts/execplan-implementation-apply-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-repair-base-drift.md",
    ".scherzo/workflows/prompts/execplan-implementation-apply-late-plan-completion-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion-after-late-repair.md",
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion-before-final-validation.md",
  ]

  list.each(execplan_prompt_paths, fn(path) {
    let assert Ok(prompt) = simplifile.read(path)
    assert !string.contains(prompt, "{{ issue.description }}")
    assert string.contains(prompt, "{{ issue.url }}")
    assert string.contains(
      prompt,
      "implementation_handoff.issue_identifier` may differ from `source_issue.identifier",
    )
    assert string.contains(prompt, "expected for handoff tasks")
  })

  list.each(
    [
      ".scherzo/workflows/prompts/execplan-implementation-implement.md",
      ".scherzo/workflows/prompts/execplan-implementation-verify-completion.md",
      ".scherzo/workflows/prompts/execplan-implementation-apply-plan-completion-feedback.md",
      ".scherzo/workflows/prompts/execplan-implementation-verify-completion-after-feedback.md",
      ".scherzo/workflows/prompts/execplan-implementation-review.md",
      ".scherzo/workflows/prompts/execplan-implementation-apply-feedback.md",
      ".scherzo/workflows/prompts/execplan-implementation-apply-late-plan-completion-feedback.md",
      ".scherzo/workflows/prompts/execplan-implementation-verify-completion-after-late-repair.md",
      ".scherzo/workflows/prompts/execplan-implementation-verify-completion-before-final-validation.md",
    ],
    fn(path) {
      let assert Ok(prompt) = simplifile.read(path)
      assert string.contains(prompt, "tmp/execplan-review-doc.md")
      assert string.contains(prompt, "tmp/execplan-implementation-pack.json")
      assert string.contains(prompt, "tmp/execplan-bundle.json")
    },
  )

  list.each(
    [
      ".scherzo/workflows/prompts/execplan-implementation-verify-completion.md",
      ".scherzo/workflows/prompts/execplan-implementation-verify-completion-after-feedback.md",
      ".scherzo/workflows/prompts/execplan-implementation-verify-completion-after-late-repair.md",
      ".scherzo/workflows/prompts/execplan-implementation-verify-completion-before-final-validation.md",
    ],
    fn(path) {
      let assert Ok(prompt) = simplifile.read(path)
      assert string.contains(prompt, "restore-execplan-artifacts")
      assert string.contains(prompt, "Treat unchecked Progress checklist items")
      assert string.contains(prompt, "deferred_manual_verification")
      assert string.contains(
        prompt,
        "post-implementation manual/browser/dogfood",
      )
      assert !string.contains(
        prompt,
        "Explicitly return `fail` when required Progress checklist items are still unchecked",
      )
    },
  )

  let assert Ok(final_prompt) =
    simplifile.read(
      ".scherzo/workflows/prompts/execplan-implementation-verify-completion-before-final-validation.md",
    )
  assert !string.contains(
    final_prompt,
    "{{ steps.validate_after_refresh.stdout }}",
  )
  assert !string.contains(
    final_prompt,
    "{{ steps.validate_after_refresh.stderr }}",
  )
  assert string.contains(
    final_prompt,
    "{{ steps.repair_base_drift.final_response }}",
  )
  assert string.contains(
    final_prompt,
    "{{ steps.apply_review_feedback.final_response }}",
  )
  assert string.contains(final_prompt, "Targeted review remediation response:")
  assert string.contains(final_prompt, "targeted review remediation")
  assert !string.contains(final_prompt, "review_changes")
  assert string.contains(final_prompt, "plan-completion-context")
  assert string.contains(
    final_prompt,
    "tmp/scherzo-plan-completion-verdict.json",
  )

  let assert Ok(repair_prompt) =
    simplifile.read(".scherzo/workflows/prompts/repair-base-drift.md")
  assert !string.contains(repair_prompt, "{{ issue.description }}")
  assert !string.contains(
    repair_prompt,
    "{{ steps.validate_after_refresh.stdout }}",
  )
  assert !string.contains(
    repair_prompt,
    "{{ steps.validate_after_refresh.stderr }}",
  )
  assert string.contains(
    repair_prompt,
    "{{ steps.validate_after_refresh.exit_code }}",
  )
  assert string.contains(
    repair_prompt,
    "tmp/scherzo-implementation-validation.json",
  )
  assert string.contains(repair_prompt, "failure_summary")
  assert string.contains(repair_prompt, "stdout_excerpt")
  assert string.contains(repair_prompt, "stderr_excerpt")
  assert string.contains(
    repair_prompt,
    ".scherzo/command-step-diagnostics/validate_after_refresh.txt",
  )
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
    "repair_base_drift",
    "final_validate",
  )
  assert_workflow_refresh_ordering(
    execplan,
    "prepare_bundle",
    "implement_plan",
    "apply_review_feedback",
    "prompts/execplan-implementation-repair-base-drift.md",
    "checkpoint_final_plan_completion_verdict",
    "finalize_final_plan_completion_gate, finalize_review_dispositions",
  )
  assert string.contains(execplan, "- id: final_plan_completion_gate")
  assert string.contains(execplan, "on_failure: continue")
  assert string.contains(execplan, "- id: finalize_final_plan_completion_gate")
  assert string.contains(
    execplan,
    "plan-completion-recovery --phase final --attempt 3 --max-attempts 3",
  )
}

pub fn execplan_implementation_workflow_has_plan_completion_gates_test() {
  let assert Ok(workflow) =
    simplifile.read(".scherzo/workflows/execplan-implementation.yaml")

  assert string.contains(
    workflow,
    "- id: restore_execplan_artifacts_before_plan_completion",
  )
  assert string.contains(workflow, "depends_on: [analyze_changes]")
  assert string.contains(workflow, "restore-execplan-artifacts")
  assert string.contains(workflow, "- id: verify_plan_completion")
  assert string.contains(
    workflow,
    "depends_on: [restore_execplan_artifacts_before_plan_completion]",
  )
  assert string.contains(
    workflow,
    "prompts/execplan-implementation-verify-completion.md",
  )
  assert string.contains(workflow, "- id: apply_plan_completion_feedback")
  assert string.contains(workflow, "depends_on: [verify_plan_completion]")
  assert string.contains(workflow, "- id: analyze_changes_after_plan_feedback")
  assert string.contains(
    workflow,
    "depends_on: [apply_plan_completion_feedback]",
  )
  assert string.contains(
    workflow,
    "- id: restore_execplan_artifacts_after_plan_feedback",
  )
  assert string.contains(
    workflow,
    "depends_on: [analyze_changes_after_plan_feedback]",
  )
  assert string.contains(
    workflow,
    "- id: verify_plan_completion_after_feedback",
  )
  assert string.contains(
    workflow,
    "depends_on: [restore_execplan_artifacts_after_plan_feedback]",
  )
  assert string.contains(workflow, "- id: gate_plan_completion")
  assert string.contains(
    workflow,
    "depends_on: [verify_plan_completion_after_feedback]",
  )
  assert string.contains(workflow, "gate-plan-completion")
  assert string.contains(workflow, "- id: classify_plan_completion_gate")
  assert string.contains(workflow, "depends_on: [gate_plan_completion]")
  assert string.contains(
    workflow,
    "plan-completion-recovery --phase before-late-repair --attempt 2 --max-attempts 3",
  )
  assert string.contains(workflow, "- id: apply_late_plan_completion_feedback")
  assert string.contains(
    workflow,
    "depends_on: [classify_plan_completion_gate]",
  )
  assert string.contains(
    workflow,
    "prompts/execplan-implementation-apply-late-plan-completion-feedback.md",
  )
  assert string.contains(
    workflow,
    "- id: analyze_changes_after_late_plan_feedback",
  )
  assert string.contains(
    workflow,
    "depends_on: [apply_late_plan_completion_feedback]",
  )
  assert string.contains(
    workflow,
    "- id: restore_execplan_artifacts_after_late_plan_feedback",
  )
  assert string.contains(
    workflow,
    "depends_on: [analyze_changes_after_late_plan_feedback]",
  )
  assert string.contains(
    workflow,
    "- id: verify_plan_completion_after_late_repair",
  )
  assert string.contains(
    workflow,
    "depends_on: [restore_execplan_artifacts_after_late_plan_feedback]",
  )
  assert string.contains(
    workflow,
    "prompts/execplan-implementation-verify-completion-after-late-repair.md",
  )
  assert string.contains(
    workflow,
    "- id: gate_plan_completion_after_late_repair",
  )
  assert string.contains(
    workflow,
    "depends_on: [verify_plan_completion_after_late_repair]",
  )
  assert string.contains(
    workflow,
    "- id: classify_plan_completion_gate_after_late_repair",
  )
  assert string.contains(
    workflow,
    "depends_on: [gate_plan_completion_after_late_repair]",
  )
  assert string.contains(
    workflow,
    "plan-completion-recovery --phase before-final-repair --attempt 3 --max-attempts 3",
  )
  assert string.contains(workflow, "- id: apply_final_plan_completion_feedback")
  assert string.contains(
    workflow,
    "depends_on: [classify_plan_completion_gate_after_late_repair]",
  )
  assert string.contains(
    workflow,
    "prompts/execplan-implementation-apply-final-plan-completion-feedback.md",
  )
  assert string.contains(
    workflow,
    "- id: analyze_changes_after_final_plan_feedback",
  )
  assert string.contains(
    workflow,
    "depends_on: [apply_final_plan_completion_feedback]",
  )
  assert string.contains(
    workflow,
    "- id: restore_execplan_artifacts_after_final_plan_feedback",
  )
  assert string.contains(
    workflow,
    "depends_on: [analyze_changes_after_final_plan_feedback]",
  )
  assert string.contains(
    workflow,
    "- id: verify_plan_completion_after_final_repair",
  )
  assert string.contains(
    workflow,
    "depends_on: [restore_execplan_artifacts_after_final_plan_feedback]",
  )
  assert string.contains(
    workflow,
    "prompts/execplan-implementation-verify-completion-after-final-repair.md",
  )
  assert string.contains(
    workflow,
    "- id: gate_plan_completion_after_final_repair",
  )
  assert string.contains(
    workflow,
    "depends_on: [verify_plan_completion_after_final_repair]",
  )
  assert string.contains(
    workflow,
    "- id: finalize_plan_completion_gate_recovery",
  )
  assert string.contains(
    workflow,
    "depends_on: [gate_plan_completion_after_final_repair]",
  )
  assert string.contains(
    workflow,
    "plan-completion-recovery --phase after-final-repair --attempt 3 --max-attempts 3",
  )
  assert !string.contains(workflow, "- id: review_changes")
  assert string.contains(workflow, "- id: apply_review_feedback")
  assert string.contains(
    workflow,
    "depends_on: [validate_native_review_artifacts]",
  )
  assert string.contains(
    workflow,
    "depends_on: [finalize_plan_completion_gate_recovery]",
  )
  assert string.contains(
    workflow,
    "- id: restore_execplan_artifacts_before_final_verification",
  )
  assert string.contains(workflow, "depends_on: [repair_base_drift]")
  assert string.contains(
    workflow,
    "- id: verify_plan_completion_before_final_validation",
  )
  assert string.contains(
    workflow,
    "depends_on: [restore_execplan_artifacts_before_final_verification]",
  )
  assert string.contains(
    workflow,
    "prompts/execplan-implementation-verify-completion-before-final-validation.md",
  )
  assert string.contains(
    workflow,
    "- id: checkpoint_final_plan_completion_verdict",
  )
  assert string.contains(
    workflow,
    "depends_on: [verify_plan_completion_before_final_validation]",
  )
  assert string.contains(workflow, "checkpoint-plan-completion-verdict")
  assert string.contains(workflow, "- id: final_validate")
  assert string.contains(
    workflow,
    "depends_on: [checkpoint_final_plan_completion_verdict]",
  )
  assert string.contains(workflow, "- id: final_plan_completion_gate")
  assert string.contains(workflow, "depends_on: [final_validate]")
  assert string.contains(workflow, "gate-plan-completion --final")
  assert string.contains(workflow, "- id: finalize_final_plan_completion_gate")
  assert string.contains(
    workflow,
    "plan-completion-recovery --phase final --attempt 3 --max-attempts 3",
  )
  assert string.contains(workflow, "- id: materialize_commit_stack")
  assert string.contains(
    workflow,
    "depends_on: [finalize_final_plan_completion_gate, finalize_review_dispositions]",
  )
}

fn assert_workflow_refresh_ordering(
  workflow: String,
  prepare_step: String,
  implement_step: String,
  feedback_step: String,
  repair_prompt: String,
  final_validate_dependency: String,
  publish_dependency: String,
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
  assert string.contains(workflow, repair_prompt)
  assert string.contains(workflow, "- id: final_validate")
  assert string.contains(
    workflow,
    "depends_on: [" <> final_validate_dependency <> "]",
  )
  assert string.contains(workflow, "- id: materialize_commit_stack")
  assert string.contains(workflow, "depends_on: [" <> publish_dependency <> "]")
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

pub fn checkpointed_plan_completion_verdict_survives_tmp_clobber_test() {
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

  let gate =
    run_helper_in(
      dir,
      "SCHERZO_RUN_ROOT=\"$PWD/run-root\" PATH=\"$PWD/bin:$PATH\" ../../../.scherzo/workflows/scripts/scherzo-implementation gate-plan-completion --final",
    )

  assert gate.status == step_artifact.StepSucceeded
  assert gate.exit_code == Some(0)
  assert string.contains(
    gate.stdout,
    "PLAN_COMPLETION_VERDICT_RESTORE_STATUS=restored",
  )
  assert string.contains(gate.stdout, "PLAN_COMPLETION_GATE=passed")
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

fn html_execplan(title: String) -> String {
  "<!doctype html>\n"
  <> "<html lang=\"en\"><head><meta charset=\"utf-8\"><title>"
  <> title
  <> " — Scherzo ExecPlan</title></head>\n"
  <> "<body><div class=\"carbon-shell\"><nav class=\"toc-panel\">Plan contents</nav>\n"
  <> "<main><article><h1 class=\"commentable plan-heading\" data-comment-id=\"title\">"
  <> title
  <> "</h1>\n"
  <> "<section class=\"commentable plan-section\" data-comment-id=\"sec-progress\"><h2>Progress</h2><ul><li class=\"checklist-item\"><input type=\"checkbox\" checked disabled>Drafted.</li></ul></section>\n"
  <> "<section class=\"commentable plan-section\" data-comment-id=\"sec-open\"><h2>Open Questions and Clarifications Needed</h2><p>None.</p></section>\n"
  <> "</article></main></div></body></html>\n"
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
        <> "if [ \"$1\" = diff ]; then echo '.scherzo/workflows/scripts/scherzo-implementation'; exit 0; fi\n"
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
        <> "if [ \"$1\" = diff ]; then echo '.scherzo/workflows/scripts/scherzo-implementation'; exit 0; fi\n"
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
        <> "  'exec . selfci check '*) if [ \"${SCHERZO_FAKE_DIRENV_SELFCI_FAIL:-}\" = 1 ] || [ \"${SCHERZO_FAKE_DIRENV_TEST_FAIL:-}\" = 1 ]; then echo 'simulated SelfCI validation failure' >&2; exit 1; fi;;\n"
        <> "  'exec . gleam test') if [ \"${SCHERZO_FAKE_DIRENV_TEST_FAIL:-}\" = 1 ]; then echo 'simulated validation failure' >&2; exit 1; fi;;\n"
        <> "esac\n"
        <> "exit 0\n",
    )
  Nil
}
