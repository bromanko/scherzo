import gleam/list
import gleam/option.{Some}
import gleam/string
import scherzo/linear_comment_format as format
import scherzo/session/tokens as session_tokens

pub fn title_section_bullets_and_tokens_are_deterministic_test() {
  assert format.title("✅", "Scherzo completed the run")
    == "✅ Scherzo completed the run"
  assert format.section("Summary", "All done") == "## Summary\nAll done"
  assert format.bullet_section("Allowed labels", ["`workflow:bugfix`"])
    == "## Allowed labels\n- `workflow:bugfix`"
  assert format.token_usage_table(session_tokens.TokenTotals(
      input: 1,
      output: 2,
      cache_read: 3,
      cache_write: 4,
      total: 10,
    ))
    == "| Kind | Tokens |\n| --- | ---: |\n| Input | 1 |\n| Output | 2 |\n| Cache read | 3 |\n| Cache write | 4 |\n| Total | 10 |"
}

pub fn summary_table_and_optional_rows_escape_values_test() {
  let table =
    format.summary_table([
      format.SummaryRow("Issue", format.table_code("ABC-1", "unknown")),
      format.SummaryRow("Run", format.table_code("run-1", "unknown")),
    ])
  assert table
    == "| Field | Value |\n| --- | --- |\n| Issue | `ABC-1` |\n| Run | `run-1` |"

  let table =
    format.summary_table(
      [format.SummaryRow("Value", format.table_code("a|b", "unknown"))]
      |> list_append(format.optional_row("Optional", Some("two\nlines"))),
    )
  assert string.contains(table, "| Value | `a\\|b` |")
  assert string.contains(table, "| Optional | `two lines` |")
}

pub fn adversarial_markdown_inputs_stay_inside_cells_and_blocks_test() {
  let table =
    format.summary_table([
      format.SummaryRow("Backtick", format.table_code("a`b", "unknown")),
      format.SummaryRow("Empty", format.table_text("", "fallback")),
      format.SummaryRow(
        "Control",
        format.table_text("bad" <> "\u{0}" <> "x", "fallback"),
      ),
    ])

  assert string.contains(table, "| Backtick | `` a`b `` |")
  assert string.contains(table, "| Empty | fallback |")
  assert string.contains(table, "bad␀x")
  assert !string.contains(table, "\u{0}")

  let block = format.indented_block("line one\n## not a section")
  assert block == "    line one\n    ## not a section"
}

pub fn final_body_redacts_and_sanitizes_test() {
  let body =
    format.finalize_body(
      "test_comment",
      "hello secret" <> "\u{1b}" <> "[31m\n",
      ["secret"],
    )
  assert body == "hello [REDACTED]␛[31m"

  let table =
    format.summary_table([
      format.SummaryRow("Secret", format.table_text("secret|key", "fallback")),
    ])
  let table = format.finalize_body("test_comment", table, ["secret|key"])
  assert string.contains(table, "[REDACTED]")
  assert !string.contains(table, "secret")
}

fn list_append(left: List(a), right: List(a)) -> List(a) {
  list.append(left, right)
}
