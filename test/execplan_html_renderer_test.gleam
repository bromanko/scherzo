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
  write_plan(dir <> "/tmp/execplan-source.md", markdown)
  let artifact =
    run_in(
      dir,
      "python3 ../../../.scherzo/workflows/scripts/scherzo-execplan-html render tmp/execplan-source.md docs/plans/sample.html docs/plans/sample.html",
    )
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(html) = simplifile.read(dir <> "/docs/plans/sample.html")
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
  <> "## Surprises & Discoveries\n\n"
  <> "- Observation: No surprises yet.\n"
  <> "  Evidence: This is a draft plan fixture.\n\n"
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
  <> "## Outcomes & Retrospective\n\n"
  <> "Pending completion.\n\n"
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

pub fn renderer_emits_active_toc_scroll_spy_css_and_script_hooks_test() {
  let html =
    render_plan(
      "test/tmp/execplan-html-renderer-scroll-spy",
      "# Scroll Spy Plan\n\n"
        <> "Intro.\n\n"
        <> "## Section\n\n"
        <> "Body.\n\n"
        <> "### Nested Detail\n\n"
        <> "More.\n\n"
        <> "#### Deep Detail\n\n"
        <> "More.\n\n"
        <> "## Open Questions and Clarifications Needed\n\n"
        <> "None.\n",
    )

  assert string.contains(html, ".toc-entry.is-current > a")
  assert string.contains(html, ".toc-entry a[aria-current=\"location\"]")
  assert string.contains(
    html,
    "tocPanel.querySelectorAll('.toc-entry a[href^=\"#\"]')",
  )
  assert string.contains(html, "setAttribute('aria-current', 'location')")
  assert string.contains(html, "removeAttribute('aria-current')")
  assert string.contains(html, "classList.add('is-current')")
  assert string.contains(html, "function keepActiveVisible")
  assert string.contains(html, "IntersectionObserver")
  assert string.contains(html, "hashchange")
  assert string.contains(html, "href=\"#sec-section\"")
  assert string.contains(html, "href=\"#h3-nested-detail\"")
  assert string.contains(html, "href=\"#h4-deep-detail\"")
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
    "test/tmp/execplan-html-renderer-deterministic/tmp/execplan-source.md",
    markdown,
  )

  let first =
    run_in(
      "test/tmp/execplan-html-renderer-deterministic",
      "python3 ../../../.scherzo/workflows/scripts/scherzo-execplan-html render tmp/execplan-source.md docs/plans/first.html docs/plans/sample.html",
    )
  let second =
    run_in(
      "test/tmp/execplan-html-renderer-deterministic",
      "python3 ../../../.scherzo/workflows/scripts/scherzo-execplan-html render tmp/execplan-source.md docs/plans/second.html docs/plans/sample.html",
    )
  assert first.status == step_artifact.StepSucceeded
  assert second.status == step_artifact.StepSucceeded
  let assert Ok(first_html) =
    simplifile.read(
      "test/tmp/execplan-html-renderer-deterministic/docs/plans/first.html",
    )
  let assert Ok(second_html) =
    simplifile.read(
      "test/tmp/execplan-html-renderer-deterministic/docs/plans/second.html",
    )

  assert first_html == second_html
  assert string.contains(first_html, "id=\"sec-repeat\"")
  assert string.contains(first_html, "id=\"sec-repeat-2\"")
}

pub fn extract_md_recovers_readable_markdown_from_html_test() {
  let dir = "test/tmp/execplan-html-extract-md"
  let html = render_plan(dir, representative_plan("None.\n"))
  assert string.contains(html, "carbon-shell")

  let artifact =
    run_in(
      dir,
      "python3 ../../../.scherzo/workflows/scripts/scherzo-execplan-html extract-md docs/plans/sample.html",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "# Sample ExecPlan")
  assert string.contains(artifact.stdout, "## Purpose / Big Picture")
  assert string.contains(artifact.stdout, "- [x] Drafted the renderer.")
  assert string.contains(artifact.stdout, "gleam test")
  assert string.contains(
    artifact.stdout,
    "## Open Questions and Clarifications Needed",
  )
  assert !string.contains(artifact.stdout, "carbon-shell")
  assert !string.contains(artifact.stdout, "<style>")
  assert !string.contains(artifact.stdout, "data-comment-id")
  assert !string.contains(artifact.stdout, "toc-panel")
}

pub fn section_extracts_named_section_without_neighbor_sections_test() {
  let dir = "test/tmp/execplan-html-section"
  render_plan(dir, representative_plan("None.\n"))

  let artifact =
    run_in(
      dir,
      "python3 ../../../.scherzo/workflows/scripts/scherzo-execplan-html section docs/plans/sample.html \"Concrete Steps\"",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "## Concrete Steps")
  assert string.contains(artifact.stdout, "gleam test")
  assert !string.contains(artifact.stdout, "## Purpose / Big Picture")
  assert !string.contains(artifact.stdout, "## Progress")
}

pub fn section_reports_available_headings_for_missing_or_ambiguous_match_test() {
  let dir = "test/tmp/execplan-html-section-diagnostics"
  render_plan(
    dir,
    "# Diagnostic Plan\n\n"
      <> "## Retry Alpha\n\nAlpha.\n\n"
      <> "## Retry Beta\n\nBeta.\n\n"
      <> "## Progress\n\n- [x] Done.\n\n"
      <> "## Open Questions and Clarifications Needed\n\nNone.\n",
  )

  let missing =
    run_in(
      dir,
      "python3 ../../../.scherzo/workflows/scripts/scherzo-execplan-html section docs/plans/sample.html Missing",
    )
  assert missing.status == step_artifact.StepFailed
  assert string.contains(missing.stderr, "section not found")
  assert string.contains(missing.stderr, "Available headings")
  assert string.contains(missing.stderr, "Retry Alpha")

  let ambiguous =
    run_in(
      dir,
      "python3 ../../../.scherzo/workflows/scripts/scherzo-execplan-html section docs/plans/sample.html Retry",
    )
  assert ambiguous.status == step_artifact.StepFailed
  assert string.contains(ambiguous.stderr, "ambiguous section")
  assert string.contains(ambiguous.stderr, "Retry Alpha")
  assert string.contains(ambiguous.stderr, "Retry Beta")
}

pub fn extract_text_omits_html_shell_but_keeps_plan_words_test() {
  let dir = "test/tmp/execplan-html-extract-text"
  render_plan(dir, representative_plan("None.\n"))

  let artifact =
    run_in(
      dir,
      "python3 ../../../.scherzo/workflows/scripts/scherzo-execplan-html extract-text docs/plans/sample.html",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "Sample ExecPlan")
  assert string.contains(artifact.stdout, "Render a plan with inline code.")
  assert !string.contains(artifact.stdout, "data-source-line")
  assert !string.contains(artifact.stdout, "tocPanel")
  assert !string.contains(artifact.stdout, "carbon-shell")
  assert !string.contains(
    artifact.stdout,
    "Scherzo ExecPlan artifact / Carbon structured",
  )
}

pub fn brief_writes_markdown_and_json_with_source_hash_and_critical_sections_test() {
  let dir = "test/tmp/execplan-html-brief"
  render_plan(dir, representative_plan("None.\n") <> brief_extra_sections())

  let artifact =
    run_in(
      dir,
      "python3 ../../../.scherzo/workflows/scripts/scherzo-execplan-html brief docs/plans/sample.html tmp/brief.md tmp/index.json",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "PLAN_BRIEF_PATH=tmp/brief.md")
  assert string.contains(artifact.stdout, "PLAN_INDEX_PATH=tmp/index.json")
  assert string.contains(artifact.stdout, "PLAN_SOURCE_SHA256=")
  let assert Ok(brief) = simplifile.read(dir <> "/tmp/brief.md")
  let assert Ok(index) = simplifile.read(dir <> "/tmp/index.json")
  assert string.contains(brief, "# ExecPlanBrief for Sample ExecPlan")
  assert string.contains(brief, "PLAN_PATH=docs/plans/sample.html")
  assert string.contains(brief, "MAX_BRIEF_CHARS=30000")
  assert string.contains(brief, "MAX_BRIEF_SECTION_CHARS=6000")
  assert string.contains(brief, "## Validation and Acceptance")
  assert string.contains(brief, "- [x] Drafted the renderer.")
  assert string.contains(
    brief,
    ".scherzo/workflows/scripts/scherzo-execplan-html section docs/plans/sample.html \"Validation and Acceptance\"",
  )
  assert string.contains(index, "\"schema_version\": 1")
  assert string.contains(index, "\"source_sha256\":")
  assert string.contains(index, "\"sections\":")
  assert string.contains(index, "\"missing_sections\": []")
}

pub fn brief_truncates_oversized_sections_with_visible_fallback_test() {
  let dir = "test/tmp/execplan-html-brief-truncated"
  render_plan(
    dir,
    "# Sample ExecPlan\n\n"
      <> "## Progress\n\n- [x] Drafted the renderer.\n\n"
      <> "## Scope Boundaries\n\nIn scope: Exercise truncation.\n\n"
      <> brief_extra_sections()
      <> "\n## Outcomes & Retrospective\n\nPending.\n\n"
      <> "## Concrete Steps\n\n"
      <> repeated_line(220)
      <> "\n## Plan of Work\n\nKeep this short.\n\n"
      <> "## Open Questions and Clarifications Needed\n\nNone.\n",
  )

  let artifact =
    run_in(
      dir,
      "python3 ../../../.scherzo/workflows/scripts/scherzo-execplan-html brief docs/plans/sample.html tmp/brief.md tmp/index.json",
    )

  assert artifact.status == step_artifact.StepSucceeded
  let assert Ok(brief) = simplifile.read(dir <> "/tmp/brief.md")
  let assert Ok(index) = simplifile.read(dir <> "/tmp/index.json")
  assert string.contains(brief, "TRUNCATED SECTION: Concrete Steps")
  assert string.contains(
    brief,
    ".scherzo/workflows/scripts/scherzo-execplan-html section docs/plans/sample.html \"Concrete Steps\"",
  )
  assert string.contains(brief, "Validation command should pass.")
  assert string.contains(index, "\"truncated_sections\": [")
  assert string.contains(index, "\"title\": \"Concrete Steps\"")
  assert string.contains(index, "\"original_char_count\":")
  assert string.contains(index, "\"included_char_count\":")
}

pub fn render_includes_short_non_visible_extraction_hint_comment_test() {
  let html =
    render_plan(
      "test/tmp/execplan-html-renderer-hint-comment",
      representative_plan("None.\n"),
    )

  assert string.contains(html, "<!-- Scherzo ExecPlan HTML artifact:")
  assert string.contains(
    html,
    ".scherzo/workflows/scripts/scherzo-execplan-html extract-md",
  )
  assert string.contains(html, "section for token-efficient agent reads")
  assert !string.contains(html, "comment-hint")
  assert !string.contains(html, "Stable DOM targets")
}

pub fn legacy_markdown_and_old_html_plan_helpers_remain_compatible_test() {
  let dir = "test/tmp/execplan-html-legacy-compat"
  reset_dir(dir)
  write_plan(dir <> "/docs/plans/legacy.md", representative_plan("None.\n"))
  write_plan(dir <> "/docs/plans/old.html", old_html_plan())

  let legacy_extract =
    run_in(
      dir,
      "python3 ../../../.scherzo/workflows/scripts/scherzo-execplan-html extract-md docs/plans/legacy.md",
    )
  assert legacy_extract.status == step_artifact.StepSucceeded
  assert string.contains(legacy_extract.stdout, "# Sample ExecPlan")

  let legacy_section =
    run_in(
      dir,
      "python3 ../../../.scherzo/workflows/scripts/scherzo-execplan-html section docs/plans/legacy.md Progress",
    )
  assert legacy_section.status == step_artifact.StepSucceeded
  assert string.contains(legacy_section.stdout, "## Progress")

  let old_extract =
    run_in(
      dir,
      "python3 ../../../.scherzo/workflows/scripts/scherzo-execplan-html extract-md docs/plans/old.html",
    )
  assert old_extract.status == step_artifact.StepSucceeded
  assert string.contains(old_extract.stdout, "# Old HTML Plan")
  assert !string.contains(old_extract.stdout, "extract-md or section")

  let old_brief =
    run_in(
      dir,
      "python3 ../../../.scherzo/workflows/scripts/scherzo-execplan-html brief docs/plans/old.html tmp/old-brief.md tmp/old-index.json",
    )
  assert old_brief.status == step_artifact.StepSucceeded
  let assert Ok(old_index) = simplifile.read(dir <> "/tmp/old-index.json")
  assert string.contains(old_index, "\"source_kind\": \"html\"")
}

fn brief_extra_sections() -> String {
  "\n## Milestones\n\nMilestone one proves extraction.\n\n"
  <> "## Testing and Falsifiability\n\nTests fail if extraction keeps shell markup.\n\n"
  <> "## Validation and Acceptance\n\nValidation command should pass.\n\n"
  <> "## Rollout, Recovery, and Idempotence\n\nDelete tmp brief files to recover.\n\n"
  <> "## Outcomes & Retrospective\n\nPending completion.\n\n"
  <> "## Plan of Work\n\nKeep implementation focused.\n"
}

fn repeated_line(count: Int) -> String {
  case count {
    0 -> ""
    _ ->
      "This concrete step line is intentionally long enough to force deterministic truncation in the brief output.\n"
      <> repeated_line(count - 1)
  }
}

fn old_html_plan() -> String {
  "<!doctype html>\n"
  <> "<html><head><title>Old HTML Plan — Scherzo ExecPlan</title></head>\n"
  <> "<body><div class=\"carbon-shell\"><nav class=\"toc-panel\">Plan contents</nav>"
  <> "<main><article><h1>Old HTML Plan</h1>"
  <> "<section><h2>Progress</h2><ul><li><input type=\"checkbox\" checked disabled>Drafted.</li></ul></section>"
  <> "<section><h2>Validation and Acceptance</h2><p>Run the helper.</p></section>"
  <> "<section><h2>Open Questions and Clarifications Needed</h2><p>None.</p></section>"
  <> "</article></main></div></body></html>\n"
}
