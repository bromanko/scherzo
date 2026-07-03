import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/path as scherzo_path
import scherzo/step_artifact
import simplifile
import support/test_helpers
import workflow_context_test_support

fn run_in(cwd: String, command: String) -> step_artifact.StepArtifact {
  command_step.run(
    "execplan_brief",
    workflow_context_test_support.without_workflow_context(command),
    cwd,
    10_000,
    [],
    test_helpers.default_artifact_limits(),
  )
}

fn write_plan(path: String, markdown: String) -> Nil {
  let assert Ok(parent) = scherzo_path.dirname(path)
  let assert Ok(Nil) = simplifile.create_directory_all(parent)
  let assert Ok(Nil) = simplifile.write(path, markdown)
  Nil
}

fn representative_plan() -> String {
  "# Sample ExecPlan\n\n"
  <> "## Progress\n\n- [x] Drafted the brief helper.\n\n"
  <> "## Scope Boundaries\n\nIn scope: Markdown-only plan reads.\n\n"
  <> "## Testing and Falsifiability\n\nTests fail if the helper accepts HTML plans.\n\n"
  <> "## Validation and Acceptance\n\nRun the helper tests and expect success.\n\n"
  <> "## Rollout, Recovery, and Idempotence\n\nRegenerate tmp brief files to recover.\n\n"
  <> "## Open Questions and Clarifications Needed\n\nNone.\n\n"
  <> "## Outcomes & Retrospective\n\nPending completion.\n\n"
  <> "## Milestones\n\nMilestone one keeps descriptor plans readable.\n\n"
  <> "## Concrete Steps\n\n1. Read the Markdown plan.\n2. Keep the canonical artifact authoritative.\n\n"
  <> "## Plan of Work\n\nKeep implementation focused.\n"
}

pub fn brief_writes_markdown_and_json_with_source_hash_and_sections_test() {
  let dir = "test/tmp/execplan-brief-helper"
  test_helpers.reset_dir(dir)
  write_plan(dir <> "/docs/plans/sample.md", representative_plan())

  let artifact =
    run_in(
      dir,
      "python3 ../../../.scherzo/workflows/scripts/scherzo-execplan-brief brief docs/plans/sample.md tmp/brief.md tmp/index.json",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "PLAN_BRIEF_PATH=tmp/brief.md")
  assert string.contains(artifact.stdout, "PLAN_INDEX_PATH=tmp/index.json")
  assert string.contains(artifact.stdout, "PLAN_SOURCE_SHA256=")
  let assert Ok(brief) = simplifile.read(dir <> "/tmp/brief.md")
  let assert Ok(index) = simplifile.read(dir <> "/tmp/index.json")
  assert string.contains(brief, "# ExecPlanBrief for Sample ExecPlan")
  assert string.contains(brief, "PLAN_PATH=docs/plans/sample.md")
  assert string.contains(brief, "## Validation and Acceptance")
  assert string.contains(
    brief,
    ".scherzo/workflows/scripts/scherzo-execplan-brief section docs/plans/sample.md \"Validation and Acceptance\"",
  )
  assert string.contains(index, "\"source_kind\": \"markdown\"")
  assert string.contains(index, "\"missing_sections\": []")
}

pub fn section_extracts_named_section_without_neighbor_sections_test() {
  let dir = "test/tmp/execplan-brief-section"
  test_helpers.reset_dir(dir)
  write_plan(dir <> "/docs/plans/sample.md", representative_plan())

  let artifact =
    run_in(
      dir,
      "python3 ../../../.scherzo/workflows/scripts/scherzo-execplan-brief section docs/plans/sample.md \"Concrete Steps\"",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "## Concrete Steps")
  assert string.contains(artifact.stdout, "Read the Markdown plan")
  assert !string.contains(artifact.stdout, "## Plan of Work")
}

pub fn helper_rejects_html_plan_paths_test() {
  let dir = "test/tmp/execplan-brief-reject-html"
  test_helpers.reset_dir(dir)
  write_plan(dir <> "/docs/plans/sample.html", "<h1>Legacy HTML</h1>\n")

  let artifact =
    run_in(
      dir,
      "python3 ../../../.scherzo/workflows/scripts/scherzo-execplan-brief brief docs/plans/sample.html tmp/brief.md tmp/index.json",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stderr, "only accepts Markdown plan paths")
}

pub fn brief_bounds_large_section_index_test() {
  let dir = "test/tmp/execplan-brief-large-index"
  test_helpers.reset_dir(dir)
  let long_title =
    "## Extra Section "
    <> string.repeat("very-long-heading-", times: 20)
    <> "\n\nBody.\n\n"
  write_plan(
    dir <> "/docs/plans/sample.md",
    representative_plan() <> string.repeat(long_title, times: 300),
  )

  let artifact =
    run_in(
      dir,
      "python3 ../../../.scherzo/workflows/scripts/scherzo-execplan-brief brief docs/plans/sample.md tmp/brief.md tmp/index.json",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(brief) = simplifile.read(dir <> "/tmp/brief.md")
  let assert Ok(index) = simplifile.read(dir <> "/tmp/index.json")
  assert string.length(brief) <= 30_000
  assert string.contains(brief, "SECTION INDEX TRUNCATED")
  assert string.contains(index, "\"section_index_truncated\": true")
  assert string.contains(index, "\"max_section_index_entries\": 80")
}
