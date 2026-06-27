import gleam/int
import gleam/list
import gleam/order
import gleam/result
import gleam/string
import simplifile

pub const no_process_sleep_rule = "scherzo_no_process_sleep_in_tests"

pub const no_tiny_query_timeout_rule = "scherzo_no_tiny_query_timeout"

pub const no_raw_negative_receive_rule = "scherzo_no_raw_negative_receive"

pub const no_manual_drain_receive_rule = "scherzo_no_manual_drain_receive"

pub const no_global_env_mutation_rule = "scherzo_no_global_env_mutation_in_tests"

pub const split_large_table_tests_rule = "scherzo_split_large_table_tests"

const tiny_query_timeout_threshold_ms = 1000

const max_table_cases_per_test = 30

pub type Finding {
  Finding(rule: String, path: String, line: Int, message: String)
}

pub type GuardError {
  FileError(context: String, path: String, message: String)
  BaselineExceeded(findings: List(Finding))
}

type ScanState {
  ScanState(
    line_number: Int,
    current_function: String,
    previous_line: String,
    table: CaseTable,
    receive_case: ReceiveCase,
    findings: List(Finding),
  )
}

type CaseTable {
  NoTable
  InTable(start_line: Int, function_name: String, depth: Int, cases: Int)
}

type ReceiveCase {
  NoReceiveCase
  InReceiveCase(
    start_line: Int,
    remaining_lines: Int,
    saw_error_nil: Bool,
    saw_ok_panic: Bool,
  )
}

pub fn run() -> Result(Nil, GuardError) {
  use files <- result.try(test_files())
  let findings = analyze_files(files)

  case findings {
    [] -> Ok(Nil)
    _ -> Error(BaselineExceeded(sort_findings(findings)))
  }
}

pub fn error_message(error: GuardError) -> String {
  case error {
    FileError(context, path, message) ->
      "failed to " <> context <> " `" <> path <> "`: " <> message
    BaselineExceeded(findings) -> baseline_exceeded_message(findings)
  }
}

pub fn findings_for_source(
  path path: String,
  source source: String,
) -> List(Finding) {
  scan_source(path, source)
}

fn test_files() -> Result(List(String), GuardError) {
  simplifile.get_files("test")
  |> result.map_error(fn(error) {
    FileError("scan", "test", simplifile.describe_error(error))
  })
  |> result.map(fn(files) {
    files
    |> list.filter(keeping: fn(path) {
      { string.ends_with(path, ".gleam") || string.ends_with(path, ".erl") }
      && path != "test/test_async.gleam"
      && !string.starts_with(path, "test/scherzo_lint/")
    })
    |> list.sort(by: string.compare)
  })
}

fn analyze_files(files: List(String)) -> List(Finding) {
  files
  |> list.flat_map(fn(path) {
    case simplifile.read(path) {
      Ok(source) -> scan_source(path, source)
      Error(_) -> []
    }
  })
  |> sort_findings
}

fn scan_source(path: String, source: String) -> List(Finding) {
  let state =
    source
    |> string.split(on: "\n")
    |> scan_lines(
      path,
      ScanState(
        line_number: 1,
        current_function: "",
        previous_line: "",
        table: NoTable,
        receive_case: NoReceiveCase,
        findings: [],
      ),
    )

  finish_open_table(path, state)
  |> list.append(state.findings)
  |> sort_findings
}

fn scan_lines(
  lines: List(String),
  path: String,
  state: ScanState,
) -> ScanState {
  case lines {
    [] -> state
    [line, ..rest] -> {
      let function_name = current_function_name(line, state.current_function)
      let state = ScanState(..state, current_function: function_name)
      let state = scan_line_findings(path, line, state)
      let state = scan_receive_case(path, line, state)
      let state = scan_table(path, line, state)
      scan_lines(
        rest,
        path,
        ScanState(
          ..state,
          line_number: state.line_number + 1,
          previous_line: line,
        ),
      )
    }
  }
}

fn scan_line_findings(
  path: String,
  line: String,
  state: ScanState,
) -> ScanState {
  let findings = state.findings
  let findings = case should_report_process_sleep(line, state) {
    True -> [
      Finding(
        no_process_sleep_rule,
        path,
        state.line_number,
        "process.sleep in tests is non-deterministic; use test_async barriers, subjects, or an injected fake timer/clock",
      ),
      ..findings
    ]
    False -> findings
  }
  let findings = case
    tiny_query_timeout_ms(line),
    tiny_timeout_allowed(state.current_function),
    suppressed(line, state.previous_line, no_tiny_query_timeout_rule)
  {
    Ok(value), False, False -> [
      Finding(
        no_tiny_query_timeout_rule,
        path,
        state.line_number,
        "query_timeout_ms literal "
          <> int.to_string(value)
          <> "ms is too small for a non-timeout test; use a generous default and override only in timeout/heartbeat/stale/shutdown tests",
      ),
      ..findings
    ]
    _, _, _ -> findings
  }
  let findings = case should_report_raw_negative_receive(line, state) {
    True -> [
      Finding(
        no_raw_negative_receive_rule,
        path,
        state.line_number,
        "raw negative process.receive assertion in tests is timing-sensitive; use test_async.assert_no_extra_message(_within) after an explicit synchronization point",
      ),
      ..findings
    ]
    False -> findings
  }
  let findings = case should_report_manual_drain_receive(line, state) {
    True -> [
      Finding(
        no_manual_drain_receive_rule,
        path,
        state.line_number,
        "hand-rolled drain loop with process.receive is timing-sensitive; use test_async.drain_subject instead",
      ),
      ..findings
    ]
    False -> findings
  }
  let findings = case should_report_global_env_mutation(line, state) {
    True -> [
      Finding(
        no_global_env_mutation_rule,
        path,
        state.line_number,
        "test mutates the global BEAM process environment; inject an env reader or pass a child-process env instead",
      ),
      ..findings
    ]
    False -> findings
  }

  ScanState(..state, findings: findings)
}

fn should_report_process_sleep(line: String, state: ScanState) -> Bool {
  string.contains(line, "process.sleep(")
  && !sleep_helper_function(state.current_function)
  && !suppressed(line, state.previous_line, no_process_sleep_rule)
}

fn should_report_raw_negative_receive(line: String, state: ScanState) -> Bool {
  string.contains(line, "process.receive(")
  && string.contains(line, "within:")
  && {
    string.contains(line, "== Error(")
    || string.contains(line, "= process.receive(")
    && string.contains(line, "assert Error(")
  }
  && !suppressed(line, state.previous_line, no_raw_negative_receive_rule)
}

fn should_report_manual_drain_receive(line: String, state: ScanState) -> Bool {
  string.starts_with(state.current_function, "drain_")
  && string.contains(line, "process.receive(")
  && string.contains(line, "within:")
  && !suppressed(line, state.previous_line, no_manual_drain_receive_rule)
}

fn should_report_global_env_mutation(line: String, state: ScanState) -> Bool {
  global_env_mutation_line(line)
  && !suppressed(line, state.previous_line, no_global_env_mutation_rule)
}

fn global_env_mutation_line(line: String) -> Bool {
  string.contains(line, ".set_env(")
  || string.contains(line, ".unset_env(")
  || string.contains(line, "set_env(")
  || string.contains(line, "unset_env(")
  || string.contains(line, "setenv(")
  || string.contains(line, "unsetenv(")
  || string.contains(line, "os:putenv(")
  || string.contains(line, "os:unsetenv(")
}

fn sleep_helper_function(function_name: String) -> Bool {
  string.starts_with(function_name, "wait_")
  || string.starts_with(function_name, "retry_")
  || string.starts_with(function_name, "read_pid_file")
  || string.starts_with(function_name, "prompt_until_")
  || string.starts_with(function_name, "poll_")
}

fn tiny_query_timeout_ms(line: String) -> Result(Int, Nil) {
  case literal_int_after(line, "query_timeout_ms:") {
    Ok(value) if value > 0 && value < tiny_query_timeout_threshold_ms ->
      Ok(value)
    _ -> Error(Nil)
  }
}

fn tiny_timeout_allowed(function_name: String) -> Bool {
  let name = string.lowercase(function_name)
  string.contains(name, "timeout")
  || string.contains(name, "times_out")
  || string.contains(name, "heartbeat")
  || string.contains(name, "stale")
  || string.contains(name, "shutdown")
  || string.contains(name, "overload")
  || string.contains(name, "blocked")
}

fn scan_receive_case(
  path: String,
  line: String,
  state: ScanState,
) -> ScanState {
  let state = advance_receive_case(path, line, state)

  case should_track_receive_case(line, state) {
    True ->
      ScanState(
        ..state,
        receive_case: InReceiveCase(state.line_number, 8, False, False),
      )
    False -> state
  }
}

fn should_track_receive_case(line: String, state: ScanState) -> Bool {
  string.contains(line, "case process.receive(")
  && string.contains(line, "within:")
  && !suppressed(line, state.previous_line, no_raw_negative_receive_rule)
}

fn advance_receive_case(
  path: String,
  line: String,
  state: ScanState,
) -> ScanState {
  case state.receive_case {
    NoReceiveCase -> state
    InReceiveCase(start_line, remaining_lines, saw_error_nil, saw_ok_panic) -> {
      let saw_error_nil =
        saw_error_nil
        || string.contains(line, "Error(")
        && string.contains(line, "-> Nil")
      let saw_ok_panic =
        saw_ok_panic
        || string.contains(line, "Ok(")
        && string.contains(line, "-> panic")
      let remaining_lines = remaining_lines - 1
      let matched = saw_error_nil && saw_ok_panic
      let findings = case matched {
        True -> [
          Finding(
            no_raw_negative_receive_rule,
            path,
            start_line,
            "raw negative process.receive case in tests is timing-sensitive; use test_async.assert_no_extra_message(_within) after an explicit synchronization point",
          ),
          ..state.findings
        ]
        False -> state.findings
      }
      let receive_case = case matched || remaining_lines <= 0 {
        True -> NoReceiveCase
        False ->
          InReceiveCase(
            start_line,
            remaining_lines,
            saw_error_nil,
            saw_ok_panic,
          )
      }
      ScanState(..state, receive_case: receive_case, findings: findings)
    }
  }
}

fn scan_table(path: String, line: String, state: ScanState) -> ScanState {
  case state.table {
    NoTable -> {
      case
        string.contains(line, "let cases = [")
        && !suppressed(line, state.previous_line, split_large_table_tests_rule)
      {
        False -> state
        True -> {
          let depth = bracket_delta(line)
          let cases = count_occurrences(line, "#(")
          let table =
            InTable(state.line_number, state.current_function, depth, cases)
          close_table_if_done(path, ScanState(..state, table: table))
        }
      }
    }
    InTable(start_line, function_name, depth, cases) -> {
      let depth = depth + bracket_delta(line)
      let cases = cases + count_occurrences(line, "#(")
      let table = InTable(start_line, function_name, depth, cases)
      close_table_if_done(path, ScanState(..state, table: table))
    }
  }
}

fn close_table_if_done(path: String, state: ScanState) -> ScanState {
  case state.table {
    InTable(start_line, function_name, depth, cases) if depth <= 0 -> {
      let findings = case should_report_large_table(function_name, cases) {
        True -> [
          Finding(
            split_large_table_tests_rule,
            path,
            start_line,
            "single `let cases = [...]` table has "
              <> int.to_string(cases)
              <> " tuple cases; split large table-driven tests so one EUnit test cannot monopolize the per-test timeout",
          ),
          ..state.findings
        ]
        False -> state.findings
      }
      ScanState(..state, table: NoTable, findings: findings)
    }
    _ -> state
  }
}

fn finish_open_table(path: String, state: ScanState) -> List(Finding) {
  case state.table {
    InTable(start_line, function_name, _, cases) ->
      case should_report_large_table(function_name, cases) {
        True -> [
          Finding(
            split_large_table_tests_rule,
            path,
            start_line,
            "single `let cases = [...]` table has "
              <> int.to_string(cases)
              <> " tuple cases; split large table-driven tests so one EUnit test cannot monopolize the per-test timeout",
          ),
        ]
        False -> []
      }
    NoTable -> []
  }
}

fn should_report_large_table(function_name: String, cases: Int) -> Bool {
  string.ends_with(function_name, "_test") && cases > max_table_cases_per_test
}

fn current_function_name(line: String, current: String) -> String {
  let trimmed = string.trim(line)
  case string.starts_with(trimmed, "pub fn ") {
    True -> function_name_after_prefix(trimmed, 7, current)
    False ->
      case string.starts_with(trimmed, "fn ") {
        True -> function_name_after_prefix(trimmed, 3, current)
        False -> current
      }
  }
}

fn function_name_after_prefix(
  line: String,
  prefix_length: Int,
  fallback: String,
) -> String {
  line
  |> string.drop_start(prefix_length)
  |> string.split(on: "(")
  |> list.first
  |> result.unwrap(fallback)
  |> string.trim
}

fn literal_int_after(line: String, label: String) -> Result(Int, Nil) {
  case string.split(line, on: label) {
    [_, value, ..] ->
      value
      |> string.trim_start
      |> digit_prefix("")
      |> parse_int_literal
    _ -> Error(Nil)
  }
}

fn digit_prefix(value: String, acc: String) -> String {
  case string.to_graphemes(value) {
    [] -> acc
    [first, ..rest] ->
      case is_int_literal_grapheme(first) {
        True -> digit_prefix(string.join(rest, ""), acc <> first)
        False -> acc
      }
  }
}

fn is_int_literal_grapheme(value: String) -> Bool {
  value == "_"
  || list.contains(["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"], value)
}

fn parse_int_literal(value: String) -> Result(Int, Nil) {
  case value == "" {
    True -> Error(Nil)
    False ->
      value
      |> string.replace(each: "_", with: "")
      |> int.parse
      |> result.map_error(fn(_) { Nil })
  }
}

fn bracket_delta(line: String) -> Int {
  count_occurrences(line, "[") - count_occurrences(line, "]")
}

fn count_occurrences(value: String, token: String) -> Int {
  case token == "" {
    True -> 0
    False -> list.length(string.split(value, on: token)) - 1
  }
}

fn suppressed(line: String, previous_line: String, rule: String) -> Bool {
  line_suppresses(line, rule) || line_suppresses(previous_line, rule)
}

fn line_suppresses(line: String, rule: String) -> Bool {
  string.contains(line, "nolint:") && string.contains(line, rule)
}

pub fn sort_findings(findings: List(Finding)) -> List(Finding) {
  findings
  |> list.sort(by: fn(left, right) {
    compare_strings(finding_sort_key(left), finding_sort_key(right))
  })
}

fn finding_sort_key(finding: Finding) -> String {
  finding.path <> ":" <> int.to_string(finding.line) <> ":" <> finding.rule
}

fn compare_strings(left: String, right: String) {
  case string.compare(left, right) {
    order.Lt -> order.Lt
    order.Eq -> order.Eq
    order.Gt -> order.Gt
  }
}

fn baseline_exceeded_message(findings: List(Finding)) -> String {
  "scherzo_lint_test_determinism_guard failed: async tests must avoid known flaky timing patterns.\n\n"
  <> "Rules:\n"
  <> "- `"
  <> no_process_sleep_rule
  <> "`: no ad-hoc `process.sleep` in tests outside bounded wait/poll helpers; prefer test_async barriers or fake timers.\n"
  <> "- `"
  <> no_tiny_query_timeout_rule
  <> "`: no tiny `query_timeout_ms` literals in non-timeout tests.\n"
  <> "- `"
  <> no_raw_negative_receive_rule
  <> "`: no raw negative `process.receive` assertions/cases; use test_async no-extra-message helpers.\n"
  <> "- `"
  <> no_manual_drain_receive_rule
  <> "`: no hand-rolled drain loops around `process.receive`; use `test_async.drain_subject`.\n"
  <> "- `"
  <> no_global_env_mutation_rule
  <> "`: no global test environment mutation via `set_env`/`unset_env`, `setenv`/`unsetenv`, or Erlang `os:putenv`/`os:unsetenv`; inject env readers or pass child-process env instead.\n"
  <> "- `"
  <> split_large_table_tests_rule
  <> "`: split oversized table-driven EUnit tests.\n\n"
  <> "Findings:\n"
  <> render_findings(findings)
  <> "\nUse `// nolint: <rule> -- reason` only for a narrow, deterministic exception."
}

fn render_findings(findings: List(Finding)) -> String {
  findings
  |> list.map(fn(finding) {
    "- `"
    <> finding.path
    <> ":"
    <> int.to_string(finding.line)
    <> "` "
    <> finding.rule
    <> ": "
    <> finding.message
  })
  |> string.join("\n")
}
