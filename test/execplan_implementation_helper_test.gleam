import gleam/list
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
      "scripts/scherzo-implementation " <> command,
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

pub fn extract_plan_accepts_html_plan_paths_test() {
  let dir = "test/tmp/execplan-helper-extract-html"
  reset_dir(dir)
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
  reset_dir(dir)
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
  reset_dir(dir)
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
    run_helper_in(dir, "../../../scripts/scherzo-implementation plan-brief")

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
      "../../../scripts/scherzo-implementation plan-brief --check",
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
      "../../../scripts/scherzo-implementation plan-brief --check",
    )
  assert stale.status == step_artifact.StepFailed
  assert stale.exit_code == Some(2)
  assert string.contains(stale.stdout, "PLAN_BRIEF_STATUS=stale")

  let refreshed =
    run_helper_in(
      dir,
      "../../../scripts/scherzo-implementation plan-brief --refresh-if-stale",
    )
  assert refreshed.status == step_artifact.StepSucceeded
  assert string.contains(refreshed.stdout, "PLAN_BRIEF_STATUS=ok")
  let fresh_again =
    run_helper_in(
      dir,
      "../../../scripts/scherzo-implementation plan-brief --check",
    )
  assert fresh_again.status == step_artifact.StepSucceeded
  assert string.contains(fresh_again.stdout, "PLAN_BRIEF_STATUS=fresh")
}

pub fn plan_brief_command_reports_unavailable_and_removes_partial_files_test() {
  let dir = "test/tmp/implementation-helper-plan-brief-failure"
  reset_dir(dir)
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
  chmod_executable(dir <> "/bin/failing-brief-helper")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_EXECPLAN_HTML_HELPER=bin/failing-brief-helper ../../../scripts/scherzo-implementation plan-brief",
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
  reset_dir(dir)
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
        <> " ../../../scripts/scherzo-implementation plan-brief",
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
  reset_dir(dir)
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
        <> " ../../../scripts/scherzo-implementation plan-brief",
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
  reset_dir(dir)
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
  chmod_executable(dir <> "/bin/jj")

  let artifact =
    run_helper_in(
      dir,
      clean_workflow_env()
        <> " "
        <> run_root_env()
        <> " PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation analyze",
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
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/tmp")
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/run-root/state/implementation")
  let metadata =
    "{\"source_kind\":\"ticket\",\"base_change_id\":\"old-base\"}\n"
  let assert Ok(Nil) = simplifile.write(metadata_cache_path(dir), metadata)
  let assert Ok(Nil) = simplifile.write(metadata_canonical_path(dir), metadata)
  write_fake_refresh_jj(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/jj")

  let artifact =
    run_helper_in(
      dir,
      clean_workflow_env()
        <> " "
        <> run_root_env()
        <> " SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation refresh-base --stage before-implementation",
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
  reset_dir(dir)

  let artifact =
    run_helper_in(
      dir,
      clean_workflow_env()
        <> " "
        <> run_root_env()
        <> " ../../../scripts/scherzo-implementation analyze",
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
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/run-root")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/docs/plans/example.md", execplan_markdown())
  write_fake_jj(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/jj")
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
        <> " SCHERZO_ISSUE_IDENTIFIER=LIV-230 SCHERZO_TEST_LINEAR_GRAPHQL_JSON=linear-execplan.json PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation prepare --source execplan",
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
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/run-root")
  write_fake_jj(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/jj")
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
        <> " SCHERZO_ISSUE_IDENTIFIER=LIV-254 SCHERZO_TEST_LINEAR_GRAPHQL_JSON=linear-ticket.json PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation prepare --source ticket",
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
    "# Ticket context for LIV-254: Implement from ticket",
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
  assert string.contains(artifact.stdout, "REVIEW_COMMANDS:\n- None")
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

pub fn plan_completion_gate_passes_fresh_pass_verdict_test() {
  let dir = "test/tmp/plan-completion-gate-pass"
  let fingerprint = setup_plan_completion_gate_fixture(dir)
  write_plan_completion_verdict(dir, "pass", fingerprint, "[]")

  let artifact =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation gate-plan-completion --final",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_GATE_MODE=final")
  assert string.contains(artifact.stdout, "PLAN_COMPLETION_VERDICT=pass")
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
      "PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation gate-plan-completion",
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
      "PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation gate-plan-completion --final",
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
      "PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation gate-plan-completion --final",
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
      "PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation gate-plan-completion --final",
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
  assert string.contains(script, "SCHERZO_PR_BASE")
  assert string.contains(script, "SCHERZO_PR_REMOTE")
  assert string.contains(script, "configured_base_candidates(branch, remote)")
  assert string.contains(
    script,
    "base_revision = workspace_base_revision(repo_root)",
  )
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
      "SCHERZO_RUN_ROOT=/outer/run/root SCHERZO_WORKSPACE_DRIVER=/outer/driver SCHERZO_WORKSPACE_PROFILE=dogfood-jj SCHERZO_WORKSPACE_CAPABILITIES=status,diff SCHERZO_WORKSPACE_ROOT=/outer/workspaces SCHERZO_REPO_ROOT=/outer/repo SCHERZO_JJ_WORKSPACE_REMOTE=scherzo-agent SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE=scherzo-agent SCHERZO_JJ_WORKSPACE_BASE_BRANCH=main SCHERZO_JJ_WORKSPACE_FETCH_BASE=true SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main SCHERZO_PR_REPO=example/repo SCHERZO_FAIL_IF_RUN_ROOT_LEAKS=1 SCHERZO_FAIL_IF_WORKSPACE_DRIVER_LEAKS=1 SCHERZO_FAIL_IF_PUBLICATION_ENV_LEAKS=1 PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation validate",
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

pub fn validate_failure_writes_structured_failure_artifact_test() {
  let dir = "test/tmp/implementation-helper-validate-failure-artifact"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  write_fake_direnv(dir <> "/bin/direnv")
  chmod_executable(dir <> "/bin/direnv")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_FAKE_DIRENV_SELFCI_FAIL=1 SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation validate",
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
  reset_dir(dir)
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
    run_helper_in(dir, "../../../scripts/scherzo-implementation validate")

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
      execplan_markdown_with_title("Example ExecPlan"),
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
  assert string.contains(
    artifact.stdout,
    "PLAN_MARKDOWN_PATH=docs/plans/example.md",
  )
  assert !string.contains(artifact.stdout, "PLAN_HTML_PATH")
  assert string.contains(artifact.stdout, "PUBLISH_BASE=main@origin")
  assert string.contains(
    artifact.stdout,
    "PR_URL=https://github.com/example/repo/pull/123",
  )
  assert string.contains(
    artifact.stdout,
    "PUBLISH_CONTEXT=tmp/scherzo-execplan-publish-context.json",
  )
  let assert Ok(context) =
    simplifile.read(dir <> "/tmp/scherzo-execplan-publish-context.json")
  assert string.contains(context, "\"plan_path\": \"docs/plans/example.md\"")
  assert string.contains(
    context,
    "\"branch\": \"scherzo/execplan/example-execchange\"",
  )
  assert string.contains(
    context,
    "\"pr_url\": \"https://github.com/example/repo/pull/123\"",
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

pub fn create_implementation_issue_uses_publish_context_after_empty_diff_test() {
  let dir = "test/tmp/execplan-publish-context-empty-diff"
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/docs/plans")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/docs/plans/example.md",
      execplan_markdown_with_title("Example ExecPlan"),
    )
  write_source_issue(dir)
  write_created_issue(dir)
  write_fake_execplan_jj(dir <> "/bin/jj")
  write_fake_gh(dir <> "/bin/gh")
  write_fake_execplan_handoff_linear(
    dir <> "/bin/linear",
    "{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}",
  )
  chmod_executable(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/gh")
  chmod_executable(dir <> "/bin/linear")

  let publish =
    run_helper_in(
      dir,
      "SCHERZO_ISSUE_IDENTIFIER=LIV-123 SCHERZO_PR_REMOTE=origin SCHERZO_PR_BASE=main PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-execplan create-pr",
    )

  assert publish.status == step_artifact.StepSucceeded
  assert publish.exit_code == Some(0)
  let assert Ok(context) =
    simplifile.read(dir <> "/tmp/scherzo-execplan-publish-context.json")
  assert string.contains(context, "\"plan_path\": \"docs/plans/example.md\"")
  assert string.contains(
    context,
    "\"pr_url\": \"https://github.com/example/repo/pull/123\"",
  )
  assert string.contains(context, "\"source_issue\": \"LIV-123\"")
  let assert Ok(Nil) = simplifile.write(dir <> "/jj.log", "")
  let assert Ok(Nil) = simplifile.write(dir <> "/gh.log", "")

  let followup =
    run_helper_in(
      dir,
      "SCHERZO_ISSUE_IDENTIFIER= SCHERZO_FAKE_EXECPLAN_EMPTY_DIFF=1 PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-execplan create-implementation-issue",
    )

  assert followup.status == step_artifact.StepSucceeded
  assert followup.exit_code == Some(0)
  assert string.contains(followup.stdout, "IMPLEMENTATION_ISSUE_STATUS=created")
  assert string.contains(followup.stdout, "PLAN_PATH=docs/plans/example.md")
  assert string.contains(
    followup.stdout,
    "PR_URL=https://github.com/example/repo/pull/123",
  )
  let assert Ok(jj_log_after_followup) = simplifile.read(dir <> "/jj.log")
  assert !string.contains(jj_log_after_followup, "diff")
}

pub fn execplan_workflow_creates_followup_issue_after_pr_test() {
  let assert Ok(workflow) = simplifile.read(".scherzo/workflows/execplan.yaml")

  assert string.contains(workflow, "- id: create_implementation_issue")
  assert string.contains(workflow, "depends_on: [create_pr]")
  assert string.contains(
    workflow,
    "scripts/scherzo-execplan create-pr --publish-context tmp/scherzo-execplan-publish-context.json",
  )
  assert string.contains(
    workflow,
    "scripts/scherzo-execplan create-implementation-issue --publish-context tmp/scherzo-execplan-publish-context.json",
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
  write_fake_execplan_handoff_linear(
    dir <> "/bin/linear",
    "{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}",
  )
  chmod_executable(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/gh")
  chmod_executable(dir <> "/bin/linear")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_ISSUE_IDENTIFIER=LIV-123 PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-execplan create-implementation-issue --plan docs/plans/LIV-123-example.md --pr-url https://github.com/example/repo/pull/123 --branch scherzo/execplan/liv-123-example-execchange",
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

  let assert Ok(linear_log) = simplifile.read(dir <> "/linear.log")
  assert string.contains(linear_log, "ARG=issue\nARG=query")
  assert string.contains(linear_log, "ARG=issue\nARG=create")
  assert string.contains(linear_log, "ARG=Backlog")
  assert string.contains(linear_log, "ARG=--label\nARG=Improvement")
  assert string.contains(
    linear_log,
    "ARG=--label\nARG=workflow:execplan-implementation",
  )
  assert string.contains(linear_log, "ARG=--parent\nARG=LIV-123")
  assert string.contains(linear_log, "docs/plans/LIV-123-example.md")
  assert string.contains(linear_log, "ARG=issue\nARG=link")
  assert string.contains(linear_log, "ARG=ExecPlan PR")
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
  write_fake_execplan_handoff_linear(
    dir <> "/bin/linear",
    "{\"nodes\":[{\"identifier\":\"LIV-200\",\"url\":\"https://linear.example/LIV-200\",\"title\":\"Implement: Add queued plan\",\"description\":\"Plan path: `docs/plans/LIV-123-example.md`\",\"labels\":{\"nodes\":[{\"name\":\"workflow:execplan-implementation\"}]}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}",
  )
  chmod_executable(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/gh")
  chmod_executable(dir <> "/bin/linear")

  let artifact =
    run_helper_in(
      dir,
      "SCHERZO_ISSUE_IDENTIFIER=LIV-123 PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-execplan create-implementation-issue --plan docs/plans/LIV-123-example.md --pr-url https://github.com/example/repo/pull/123 --branch scherzo/execplan/liv-123-example-execchange",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "IMPLEMENTATION_ISSUE_STATUS=existing",
  )
  assert string.contains(artifact.stdout, "IMPLEMENTATION_ISSUE=LIV-200")
  let assert Ok(linear_log) = simplifile.read(dir <> "/linear.log")
  assert !string.contains(linear_log, "ARG=create")
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

pub fn execplan_implementation_prompts_trim_validation_payloads_test() {
  let execplan_prompt_paths = [
    ".scherzo/workflows/prompts/execplan-implementation-implement.md",
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion.md",
    ".scherzo/workflows/prompts/execplan-implementation-apply-plan-completion-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion-after-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-review.md",
    ".scherzo/workflows/prompts/execplan-implementation-apply-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion-before-final-validation.md",
  ]

  list.each(execplan_prompt_paths, fn(path) {
    let assert Ok(prompt) = simplifile.read(path)
    assert !string.contains(prompt, "{{ issue.description }}")
    assert string.contains(prompt, "{{ issue.url }}")
    assert string.contains(prompt, "PLAN_PATH")
  })

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
    "{{ steps.validate_after_refresh.exit_code }}",
  )
  assert string.contains(
    final_prompt,
    "tmp/scherzo-implementation-validation.json",
  )
  assert string.contains(final_prompt, "failure_summary")
  assert string.contains(
    final_prompt,
    ".scherzo/command-step-diagnostics/validate_after_refresh.txt",
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

  assert_workflow_refresh_ordering(
    implementation,
    "prepare_context",
    "implement",
    "apply_feedback",
    "final_validate",
  )
  assert_workflow_refresh_ordering(
    execplan,
    "prepare_plan",
    "implement_plan",
    "apply_review_feedback",
    "final_plan_completion_gate",
  )
}

pub fn execplan_implementation_workflow_has_plan_completion_gates_test() {
  let assert Ok(workflow) =
    simplifile.read(".scherzo/workflows/execplan-implementation.yaml")

  assert string.contains(workflow, "- id: verify_plan_completion")
  assert string.contains(workflow, "depends_on: [analyze_changes]")
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
    "- id: verify_plan_completion_after_feedback",
  )
  assert string.contains(
    workflow,
    "depends_on: [analyze_changes_after_plan_feedback]",
  )
  assert string.contains(workflow, "- id: gate_plan_completion")
  assert string.contains(
    workflow,
    "depends_on: [verify_plan_completion_after_feedback]",
  )
  assert string.contains(workflow, "gate-plan-completion")
  assert string.contains(workflow, "- id: review_changes")
  assert string.contains(workflow, "depends_on: [gate_plan_completion]")
  assert string.contains(
    workflow,
    "- id: verify_plan_completion_before_final_validation",
  )
  assert string.contains(workflow, "depends_on: [repair_base_drift]")
  assert string.contains(
    workflow,
    "prompts/execplan-implementation-verify-completion-before-final-validation.md",
  )
  assert string.contains(workflow, "- id: final_validate")
  assert string.contains(
    workflow,
    "depends_on: [verify_plan_completion_before_final_validation]",
  )
  assert string.contains(workflow, "- id: final_plan_completion_gate")
  assert string.contains(workflow, "depends_on: [final_validate]")
  assert string.contains(workflow, "gate-plan-completion --final")
  assert string.contains(workflow, "- id: publish_pr")
  assert string.contains(workflow, "depends_on: [final_plan_completion_gate]")
}

fn assert_workflow_refresh_ordering(
  workflow: String,
  prepare_step: String,
  implement_step: String,
  feedback_step: String,
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
  assert string.contains(workflow, "prompts/repair-base-drift.md")
  assert string.contains(workflow, "- id: final_validate")
  assert string.contains(workflow, "depends_on: [repair_base_drift]")
  assert string.contains(workflow, "- id: publish_pr")
  assert string.contains(workflow, "depends_on: [" <> publish_dependency <> "]")
}

fn setup_plan_completion_gate_fixture(dir: String) -> String {
  reset_dir(dir)
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
        <> "  \"plan_path\": \"docs/plans/example.md\",\n"
        <> "  \"base_change_id\": \"local-start\"\n"
        <> "}\n",
    )
  write_fake_jj(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/jj")

  let context =
    run_helper_in(
      dir,
      "PATH=\"$PWD/bin:$PATH\" ../../../scripts/scherzo-implementation plan-completion-context",
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

fn write_followup_plan(dir: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/docs/plans/LIV-123-example.md",
      execplan_markdown_with_title("Add queued plan"),
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

fn write_fake_execplan_handoff_linear(
  path: String,
  existing_json: String,
) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "for arg in \"$@\"; do printf 'ARG=%s\\n' \"$arg\"; done >> linear.log\n"
        <> "printf '%s\\n' '---' >> linear.log\n"
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
        <> "  if [ \"${SCHERZO_FAKE_EXECPLAN_EMPTY_DIFF:-}\" = 1 ]; then exit 0; fi\n"
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
        <> "if [ \"${SCHERZO_FAIL_IF_WORKSPACE_DRIVER_LEAKS:-}\" = 1 ] && { [ -n \"${SCHERZO_WORKSPACE_DRIVER:-}\" ] || [ -n \"${SCHERZO_WORKSPACE_PROFILE:-}\" ] || [ -n \"${SCHERZO_WORKSPACE_CAPABILITIES:-}\" ] || [ -n \"${SCHERZO_WORKSPACE_ROOT:-}\" ]; }; then echo 'SCHERZO_WORKSPACE driver context leaked into validation' >&2; exit 1; fi\n"
        <> "if [ \"${SCHERZO_FAIL_IF_PUBLICATION_ENV_LEAKS:-}\" = 1 ] && env | grep -E '^(SCHERZO_JJ_WORKSPACE_BASE|SCHERZO_JJ_WORKSPACE_BASE_BRANCH|SCHERZO_JJ_WORKSPACE_FETCH_BASE|SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE|SCHERZO_JJ_WORKSPACE_REMOTE|SCHERZO_PR_BASE|SCHERZO_PR_REMOTE|SCHERZO_PR_REPO|SCHERZO_REPO_ROOT)=' >/dev/null; then echo 'SCHERZO publication environment leaked into validation' >&2; exit 1; fi\n"
        <> "case \"$*\" in\n"
        <> "  'exec . selfci check '*) if [ \"${SCHERZO_FAKE_DIRENV_SELFCI_FAIL:-}\" = 1 ] || [ \"${SCHERZO_FAKE_DIRENV_TEST_FAIL:-}\" = 1 ]; then echo 'simulated SelfCI validation failure' >&2; exit 1; fi;;\n"
        <> "  'exec . gleam test') if [ \"${SCHERZO_FAKE_DIRENV_TEST_FAIL:-}\" = 1 ]; then echo 'simulated validation failure' >&2; exit 1; fi;;\n"
        <> "esac\n"
        <> "exit 0\n",
    )
  Nil
}
