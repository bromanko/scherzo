import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/step_artifact
import simplifile

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 8000,
    template_field_max_chars: 8000,
    workflow_summary_max_chars: 8000,
  )
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

fn run_in(cwd: String, command: String) -> step_artifact.StepArtifact {
  command_step.run("execplan_html", command, cwd, 10_000, [], limits())
}

fn write_plan(path: String, body: String) -> Nil {
  let assert Ok(Nil) = simplifile.create_directory_all(path_dir(path))
  let assert Ok(Nil) = simplifile.write(path, body)
  Nil
}

fn path_dir(path: String) -> String {
  case string.split(path, "/") {
    [] -> "."
    [_] -> "."
    parts -> parts |> list_drop_last |> string.join("/")
  }
}

fn list_drop_last(parts: List(String)) -> List(String) {
  case parts {
    [] -> []
    [_] -> []
    [first, ..rest] -> [first, ..list_drop_last(rest)]
  }
}

fn render_plan(dir: String, markdown: String) -> String {
  reset_dir(dir)
  write_plan(dir <> "/docs/plans/sample.md", markdown)
  let artifact =
    run_in(
      dir,
      "python3 ../../../scripts/scherzo-execplan-html render docs/plans/sample.md html/sample.html",
    )
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(html) = simplifile.read(dir <> "/html/sample.html")
  html
}

fn representative_plan(open_questions: String) -> String {
  "# Sample ExecPlan\n\n"
  <> "Intro paragraph for review.\n\n"
  <> "## Purpose / Big Picture\n\n"
  <> "Render a plan with `inline code`.\n\n"
  <> "## Decision Log\n\n"
  <> "- Decision: Use HTML artifacts.\n"
  <> "  Rationale: They are easier to review from retained workflow output.\n"
  <> "  Date: 2026-05-09\n\n"
  <> "## Scope Boundaries\n\n"
  <> "In scope: Generate a single HTML file.\n\n"
  <> "Out of scope: Build the inline commenting UI.\n\n"
  <> "## Concrete Steps\n\n"
  <> "1. Run the focused renderer command:\n\n"
  <> "   ```sh\n"
  <> "   gleam test\n"
  <> "   ```\n\n"
  <> "   Confirm that the nested code block remains readable.\n"
  <> "2. Open the generated artifact from disk.\n\n"
  <> "## Progress\n\n"
  <> "- [x] Drafted the renderer.\n"
  <> "- [ ] Review the artifact.\n\n"
  <> "## Open Questions and Clarifications Needed\n\n"
  <> open_questions
}

pub fn renderer_emits_carbon_shell_commentable_metadata_and_open_question_badge_test() {
  let html =
    render_plan(
      "test/tmp/execplan-html-renderer-shell",
      representative_plan("- Should reviewers prefer HTML over Markdown?\n"),
    )

  assert string.contains(html, "Scherzo ExecPlan artifact / Carbon structured")
  assert string.contains(html, "class=\"toc-panel\"")
  assert string.contains(html, "id=\"sec-purpose-big-picture\"")
  assert string.contains(html, "class=\"commentable plan-section\"")
  assert string.contains(html, "data-comment-id=\"sec-purpose-big-picture\"")
  assert string.contains(
    html,
    "href=\"#sec-open-questions-and-clarifications-needed\"",
  )
  assert string.contains(html, "toc-badge toc-badge-warning\">Open")
  assert !string.contains(html, "Stable DOM targets")
  assert !string.contains(html, "comment-hint")
}

pub fn renderer_keeps_open_question_badge_off_when_section_is_none_test() {
  let html =
    render_plan(
      "test/tmp/execplan-html-renderer-no-open-questions",
      representative_plan("None.\n"),
    )

  assert string.contains(
    html,
    "href=\"#sec-open-questions-and-clarifications-needed\"",
  )
  assert !string.contains(html, "toc-badge toc-badge-warning\">Open")
}

pub fn renderer_keeps_open_question_badge_off_for_no_open_questions_text_test() {
  let html =
    render_plan(
      "test/tmp/execplan-html-renderer-no-open-questions-text",
      representative_plan("No open questions remain for this plan.\n"),
    )

  assert string.contains(
    html,
    "href=\"#sec-open-questions-and-clarifications-needed\"",
  )
  assert !string.contains(html, "toc-badge toc-badge-warning\">Open")
}

pub fn renderer_formats_label_body_rows_with_consistent_columns_test() {
  let html =
    render_plan(
      "test/tmp/execplan-html-renderer-labels",
      representative_plan("None.\n"),
    )

  assert string.contains(html, "grid-template-columns: 8.75rem minmax(0, 1fr)")
  assert string.contains(html, "labeled-statement labeled-statement-decision")
  assert string.contains(
    html,
    "<span class=\"statement-label\">Decision</span>",
  )
  assert string.contains(html, "labeled-statement labeled-statement-rationale")
  assert string.contains(
    html,
    "<span class=\"statement-label\">Rationale</span>",
  )
  assert string.contains(html, "labeled-statement labeled-statement-date")
  assert string.contains(html, "<span class=\"statement-label\">Date</span>")
  assert string.contains(html, "labeled-statement labeled-statement-in-scope")
  assert string.contains(
    html,
    "labeled-statement labeled-statement-out-of-scope",
  )
}

pub fn renderer_preserves_nested_code_blocks_and_neutral_ordered_markers_test() {
  let html =
    render_plan(
      "test/tmp/execplan-html-renderer-code",
      representative_plan("None.\n"),
    )

  assert string.contains(html, "<ol class=\"plan-list ordered\">")
  assert string.contains(html, "class=\"commentable code-block language-sh\"")
  assert string.contains(html, "gleam test")
  assert string.contains(
    html,
    ".ordered > .plan-list-item::marker { color: var(--gray-70); font-weight: 400; }",
  )
  assert string.contains(
    html,
    ".unordered > .plan-list-item::marker { color: var(--blue); font-weight: 600; }",
  )
}

pub fn renderer_uses_deterministic_unique_heading_ids_test() {
  let markdown =
    "# Duplicate Heading Plan\n\n"
    <> "## Repeat\n\n"
    <> "First.\n\n"
    <> "## Repeat\n\n"
    <> "Second.\n\n"
    <> "## Progress\n\n"
    <> "- [x] Done.\n\n"
    <> "## Open Questions and Clarifications Needed\n\n"
    <> "None.\n"
  reset_dir("test/tmp/execplan-html-renderer-deterministic")
  write_plan(
    "test/tmp/execplan-html-renderer-deterministic/docs/plans/sample.md",
    markdown,
  )

  let first =
    run_in(
      "test/tmp/execplan-html-renderer-deterministic",
      "python3 ../../../scripts/scherzo-execplan-html render docs/plans/sample.md html/first.html",
    )
  let second =
    run_in(
      "test/tmp/execplan-html-renderer-deterministic",
      "python3 ../../../scripts/scherzo-execplan-html render docs/plans/sample.md html/second.html",
    )
  assert first.status == step_artifact.StepSucceeded
  assert second.status == step_artifact.StepSucceeded
  let assert Ok(first_html) =
    simplifile.read(
      "test/tmp/execplan-html-renderer-deterministic/html/first.html",
    )
  let assert Ok(second_html) =
    simplifile.read(
      "test/tmp/execplan-html-renderer-deterministic/html/second.html",
    )

  assert first_html == second_html
  assert string.contains(first_html, "id=\"sec-repeat\"")
  assert string.contains(first_html, "id=\"sec-repeat-2\"")
}

pub fn execplan_validate_writes_html_as_primary_plan_artifact_test() {
  let dir = "test/tmp/execplan-html-validate-primary"
  reset_dir(dir)
  write_plan(dir <> "/docs/plans/sample.md", representative_plan("None.\n"))

  let artifact =
    run_in(
      dir,
      "SCHERZO_EXECPLAN_HTML_DIR= SCHERZO_RUN_ROOT= ../../../scripts/scherzo-execplan validate docs/plans/sample.md",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(
    artifact.stdout,
    "PRIMARY_PLAN_ARTIFACT=tmp/execplan-artifacts/sample.html",
  )
  assert string.contains(
    artifact.stdout,
    "PLAN_HTML_PATH=tmp/execplan-artifacts/sample.html",
  )
  assert string.contains(
    artifact.stdout,
    "PLAN_MARKDOWN_PATH=docs/plans/sample.md",
  )
  let assert Ok(html) =
    simplifile.read(dir <> "/tmp/execplan-artifacts/sample.html")
  assert string.contains(html, "Scherzo ExecPlan HTML artifact")
}
