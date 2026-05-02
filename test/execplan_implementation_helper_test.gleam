import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/domain
import scherzo/step_artifact
import simplifile

fn limits() -> domain.ArtifactLimits {
  domain.ArtifactLimits(
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
