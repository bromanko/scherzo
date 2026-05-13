import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/log
import scherzo/session/tokens as session_tokens
import scherzo/terminal/sanitize as terminal_sanitize

pub type SummaryRow {
  SummaryRow(label: String, value: String)
}

pub fn title(icon: String, text: String) -> String {
  let icon = safe_inline(icon, "")
  let text = safe_inline(text, "Scherzo update")
  case icon == "" {
    True -> text
    False -> icon <> " " <> text
  }
}

pub fn safe_inline(value: String, fallback: String) -> String {
  let value = normalize_inline(value)
  case value == "" {
    True -> normalize_inline(fallback) |> non_empty_fallback
    False -> value
  }
}

pub fn block_text(value: String, fallback: String) -> String {
  let value = normalize_block(value)
  case value == "" {
    True -> normalize_block(fallback) |> non_empty_fallback
    False -> value
  }
}

pub fn table_text(value: String, fallback: String) -> String {
  value
  |> safe_inline(fallback)
  |> escape_unescaped_pipes
}

pub fn table_code(value: String, fallback: String) -> String {
  value
  |> table_text(fallback)
  |> code_span_value
}

pub fn code_span(value: String, fallback: String) -> String {
  value
  |> safe_inline(fallback)
  |> code_span_value
}

pub fn summary_table(rows: List(SummaryRow)) -> String {
  ["| Field | Value |", "| --- | --- |", ..summary_rows(rows)]
  |> string.join(with: "\n")
}

pub fn section(title_text: String, body_text: String) -> String {
  let title_text = safe_inline(title_text, "Details")
  let body_text = block_text(body_text, "_not provided_")
  "## " <> title_text <> "\n" <> body_text
}

pub fn bullet_section(title_text: String, bullets: List(String)) -> String {
  let body = case bullets {
    [] -> "_None._"
    _ ->
      bullets
      |> list.map(fn(bullet) { "- " <> safe_inline(bullet, "_not provided_") })
      |> string.join(with: "\n")
  }
  section(title_text, body)
}

pub fn token_usage_table(tokens: session_tokens.TokenTotals) -> String {
  [
    "| Kind | Tokens |",
    "| --- | ---: |",
    "| Input | " <> int.to_string(tokens.input) <> " |",
    "| Output | " <> int.to_string(tokens.output) <> " |",
    "| Cache read | " <> int.to_string(tokens.cache_read) <> " |",
    "| Cache write | " <> int.to_string(tokens.cache_write) <> " |",
    "| Total | " <> int.to_string(tokens.total) <> " |",
  ]
  |> string.join(with: "\n")
}

pub fn indented_block(text: String) -> String {
  let lines = terminal_sanitize.block_lines(text)
  let lines = case lines {
    [] -> ["_No detail provided._"]
    _ -> lines
  }
  let body = lines |> string.join(with: "\n") |> string.trim
  let body = case body == "" {
    True -> "_No detail provided._"
    False -> body
  }
  body
  |> string.split(on: "\n")
  |> list.map(fn(line) { "    " <> line })
  |> string.join(with: "\n")
}

pub fn optional_row(label: String, value: Option(String)) -> List(SummaryRow) {
  case value {
    None -> []
    Some(value) -> [SummaryRow(label, table_code(value, "unknown"))]
  }
}

pub fn finalize_body(
  context: String,
  body: String,
  secrets: List(String),
) -> String {
  let body =
    body
    |> terminal_sanitize.block_lines
    |> string.join(with: "\n")
  log.redact(context, body, expanded_secrets(secrets)) |> string.trim
}

fn expanded_secrets(secrets: List(String)) -> List(String) {
  secrets
  |> list.fold([], fn(acc, secret) {
    [
      secret,
      escape_unescaped_pipes(secret),
      normalize_inline(secret),
      escape_unescaped_pipes(normalize_inline(secret)),
      ..acc
    ]
  })
}

fn summary_rows(rows: List(SummaryRow)) -> List(String) {
  rows
  |> list.map(fn(row) {
    let SummaryRow(label, value) = row
    "| "
    <> table_text(label, "Field")
    <> " | "
    <> sanitize_rendered_table_cell(value, "_not provided_")
    <> " |"
  })
}

fn sanitize_rendered_table_cell(value: String, fallback: String) -> String {
  value
  |> safe_inline(fallback)
  |> escape_unescaped_pipes
}

fn normalize_inline(value: String) -> String {
  value
  |> string.replace(each: "\t", with: " ")
  |> terminal_sanitize.block_lines
  |> string.join(with: " ")
  |> string.trim
}

fn normalize_block(value: String) -> String {
  value
  |> terminal_sanitize.block_lines
  |> string.join(with: "\n")
  |> string.trim
}

fn non_empty_fallback(value: String) -> String {
  case value == "" {
    True -> "_not provided_"
    False -> value
  }
}

fn code_span_value(value: String) -> String {
  let max_run = longest_backtick_run(value)
  case max_run == 0 {
    True -> "`" <> value <> "`"
    False -> {
      let delimiter = repeat_backtick(max_run + 1)
      delimiter <> " " <> value <> " " <> delimiter
    }
  }
}

fn longest_backtick_run(value: String) -> Int {
  value
  |> string.to_graphemes
  |> longest_backtick_run_loop(0, 0)
}

fn longest_backtick_run_loop(
  graphemes: List(String),
  current: Int,
  best: Int,
) -> Int {
  case graphemes {
    [] -> max_int(current, best)
    ["`", ..rest] -> longest_backtick_run_loop(rest, current + 1, best)
    [_, ..rest] -> longest_backtick_run_loop(rest, 0, max_int(current, best))
  }
}

fn max_int(a: Int, b: Int) -> Int {
  case a > b {
    True -> a
    False -> b
  }
}

fn repeat_backtick(count: Int) -> String {
  case count <= 0 {
    True -> ""
    False -> "`" <> repeat_backtick(count - 1)
  }
}

fn escape_unescaped_pipes(value: String) -> String {
  value
  |> string.to_graphemes
  |> escape_pipe_loop(False, [])
  |> string.join(with: "")
}

fn escape_pipe_loop(
  graphemes: List(String),
  previous_was_backslash: Bool,
  acc: List(String),
) -> List(String) {
  case graphemes {
    [] -> list.reverse(acc)
    ["|", ..rest] ->
      case previous_was_backslash {
        True -> escape_pipe_loop(rest, False, ["|", ..acc])
        False -> escape_pipe_loop(rest, False, ["\\|", ..acc])
      }
    ["\\", ..rest] -> escape_pipe_loop(rest, True, ["\\", ..acc])
    [ch, ..rest] -> escape_pipe_loop(rest, False, [ch, ..acc])
  }
}
