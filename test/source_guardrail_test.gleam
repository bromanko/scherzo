import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/order.{type Order, Eq}
import gleam/string
import simplifile

const source_root = "src"

const line_threshold = 1000

const internal_import_threshold = 20

const report_limit = 10

type SourceStats {
  SourceStats(path: String, lines: Int, internal_imports: Int)
}

type SourceLimit {
  SourceLimit(path: String, max_lines: Int, max_internal_imports: Int)
}

pub fn source_guardrail_matches_checked_in_baseline_test() {
  let stats = scan_sources()
  let failures = guardrail_failures(stats)

  case failures {
    [] -> Nil
    _ -> io.println_error(failure_report(stats, failures))
  }

  assert failures == []
}

pub fn source_guardrail_flags_new_files_over_default_thresholds_test() {
  let failures =
    guardrail_failures_for(
      [SourceStats("src/scherzo/new_large_module.gleam", 1001, 21)],
      [],
    )

  assert list.any(failures, fn(failure) {
    string.contains(failure, "exceeds the new-module line threshold")
  })
  assert list.any(failures, fn(failure) {
    string.contains(failure, "exceeds the new-module internal-import threshold")
  })
}

pub fn source_guardrail_allows_files_at_default_thresholds_test() {
  let failures =
    guardrail_failures_for(
      [SourceStats("src/scherzo/threshold_sized_module.gleam", 1000, 20)],
      [],
    )

  assert failures == []
}

pub fn source_guardrail_flags_baselined_regressions_test() {
  let failures =
    guardrail_failures_for(
      [SourceStats("src/scherzo/large_existing.gleam", 1011, 4)],
      [SourceLimit("src/scherzo/large_existing.gleam", 1010, 3)],
    )

  assert list.any(failures, fn(failure) {
    string.contains(failure, "grew beyond its line baseline")
  })
  assert list.any(failures, fn(failure) {
    string.contains(failure, "grew beyond its internal-import baseline")
  })
}

pub fn source_guardrail_flags_baseline_entries_for_missing_files_test() {
  let failures =
    guardrail_failures_for([], [
      SourceLimit("src/scherzo/deleted_large_module.gleam", 1001, 0),
    ])

  assert failures
    == [
      "baseline entry references a missing source file: src/scherzo/deleted_large_module.gleam",
    ]
}

fn guardrail_failures(stats: List(SourceStats)) -> List(String) {
  guardrail_failures_for(stats, oversized_source_baseline())
}

fn guardrail_failures_for(
  stats: List(SourceStats),
  baseline: List(SourceLimit),
) -> List(String) {
  let baseline_by_path =
    baseline
    |> list.map(fn(limit) { #(limit.path, limit) })
    |> dict.from_list

  let metric_failures =
    stats
    |> list.fold([], fn(failures, source_stats) {
      list.append(file_failures(source_stats, baseline_by_path), failures)
    })
    |> list.reverse

  let stale_baseline_failures =
    baseline
    |> list.fold([], fn(failures, limit) {
      case
        list.any(stats, fn(source_stats) { source_stats.path == limit.path })
      {
        True -> failures
        False -> [
          "baseline entry references a missing source file: " <> limit.path,
          ..failures
        ]
      }
    })
    |> list.reverse

  list.append(metric_failures, stale_baseline_failures)
}

fn file_failures(
  stats: SourceStats,
  baseline_by_path: dict.Dict(String, SourceLimit),
) -> List(String) {
  case dict.get(baseline_by_path, stats.path) {
    Ok(limit) -> baselined_file_failures(stats, limit)
    Error(_) -> new_file_failures(stats)
  }
}

fn baselined_file_failures(
  stats: SourceStats,
  limit: SourceLimit,
) -> List(String) {
  let line_failures = case stats.lines > limit.max_lines {
    True -> [
      stats.path
      <> " grew beyond its line baseline: "
      <> int.to_string(stats.lines)
      <> " > "
      <> int.to_string(limit.max_lines),
    ]
    False -> []
  }

  let import_failures = case
    stats.internal_imports > limit.max_internal_imports
  {
    True -> [
      stats.path
      <> " grew beyond its internal-import baseline: "
      <> int.to_string(stats.internal_imports)
      <> " > "
      <> int.to_string(limit.max_internal_imports),
    ]
    False -> []
  }

  list.append(line_failures, import_failures)
}

fn new_file_failures(stats: SourceStats) -> List(String) {
  let line_failures = case stats.lines > line_threshold {
    True -> [
      stats.path
      <> " exceeds the new-module line threshold: "
      <> int.to_string(stats.lines)
      <> " > "
      <> int.to_string(line_threshold),
    ]
    False -> []
  }

  let import_failures = case
    stats.internal_imports > internal_import_threshold
  {
    True -> [
      stats.path
      <> " exceeds the new-module internal-import threshold: "
      <> int.to_string(stats.internal_imports)
      <> " > "
      <> int.to_string(internal_import_threshold),
    ]
    False -> []
  }

  list.append(line_failures, import_failures)
}

fn scan_sources() -> List(SourceStats) {
  source_files(source_root)
  |> list.sort(by: string.compare)
  |> list.map(scan_source_file)
}

fn source_files(root: String) -> List(String) {
  let assert Ok(entries) = simplifile.read_directory(root)

  entries
  |> list.sort(by: string.compare)
  |> list.fold([], fn(paths, entry) {
    let full_path = root <> "/" <> entry
    let assert Ok(is_directory) = simplifile.is_directory(full_path)

    case is_directory {
      True -> list.append(source_files(full_path), paths)
      False ->
        case is_source_path(full_path) {
          True -> [full_path, ..paths]
          False -> paths
        }
    }
  })
}

fn is_source_path(path: String) -> Bool {
  string.ends_with(path, ".gleam") || string.ends_with(path, ".erl")
}

fn scan_source_file(path: String) -> SourceStats {
  let assert Ok(contents) = simplifile.read(path)

  SourceStats(
    path: path,
    lines: line_count(contents),
    internal_imports: internal_import_count(contents),
  )
}

fn line_count(contents: String) -> Int {
  case contents == "" {
    True -> 0
    False -> {
      let split_line_count = contents |> string.split(on: "\n") |> list.length
      case string.ends_with(contents, "\n") {
        True -> split_line_count - 1
        False -> split_line_count
      }
    }
  }
}

fn internal_import_count(contents: String) -> Int {
  contents
  |> string.split(on: "\n")
  |> list.filter(fn(line) {
    line
    |> string.trim
    |> string.starts_with("import scherzo")
  })
  |> list.length
}

fn failure_report(stats: List(SourceStats), failures: List(String)) -> String {
  [
    "Source module guardrail failed.",
    "",
    "Oversized source modules increase agent context pressure. Before growing a large module, start with docs/ARCHITECTURE.md for subsystem boundaries and docs/runbooks/source-guardrail.md for the baseline policy.",
    "",
    "New source modules must stay at or below "
      <> int.to_string(line_threshold)
      <> " lines and "
      <> int.to_string(internal_import_threshold)
      <> " internal imports unless intentionally baselined.",
    "",
    "Violations:",
  ]
  |> list.append(list.map(failures, fn(failure) { "- " <> failure }))
  |> list.append(["", "Largest source modules by line count:"])
  |> list.append(top_by_lines(stats))
  |> list.append(["", "Largest source modules by internal imports:"])
  |> list.append(top_by_imports(stats))
  |> string.join(with: "\n")
}

fn top_by_lines(stats: List(SourceStats)) -> List(String) {
  stats
  |> list.sort(by: compare_lines_desc)
  |> list.take(report_limit)
  |> list.map(format_stats_row)
}

fn top_by_imports(stats: List(SourceStats)) -> List(String) {
  stats
  |> list.sort(by: compare_internal_imports_desc)
  |> list.take(report_limit)
  |> list.map(format_stats_row)
}

fn compare_lines_desc(a: SourceStats, b: SourceStats) -> Order {
  case int.compare(b.lines, a.lines) {
    Eq -> string.compare(a.path, b.path)
    order -> order
  }
}

fn compare_internal_imports_desc(a: SourceStats, b: SourceStats) -> Order {
  case int.compare(b.internal_imports, a.internal_imports) {
    Eq -> compare_lines_desc(a, b)
    order -> order
  }
}

fn format_stats_row(stats: SourceStats) -> String {
  "- "
  <> stats.path
  <> ": "
  <> int.to_string(stats.lines)
  <> " lines, "
  <> int.to_string(stats.internal_imports)
  <> " internal imports"
}

fn oversized_source_baseline() -> List(SourceLimit) {
  [
    SourceLimit("src/scherzo/agent/run_attempt.gleam", 2177, 28),
    SourceLimit("src/scherzo/agent/turn_loop.gleam", 1003, 18),
    SourceLimit("src/scherzo/artifact_publication_config.gleam", 1125, 2),
    SourceLimit("src/scherzo/artifact_publication_planner.gleam", 1011, 9),
    SourceLimit("src/scherzo/artifact_repository/github.gleam", 1028, 11),
    SourceLimit("src/scherzo/config.gleam", 2596, 9),
    SourceLimit("src/scherzo/control/protocol.gleam", 1730, 9),
    SourceLimit("src/scherzo/ctl.gleam", 3236, 22),
    SourceLimit("src/scherzo/ctl/artifact_publication.gleam", 1103, 14),
    SourceLimit("src/scherzo/linear.gleam", 1496, 7),
    SourceLimit("src/scherzo/local_workflow_run.gleam", 939, 21),
    SourceLimit("src/scherzo/orchestrator/core.gleam", 1330, 9),
    SourceLimit("src/scherzo/orchestrator/daemon.gleam", 6427, 69),
    SourceLimit("src/scherzo/state/ledger.gleam", 1008, 3),
    SourceLimit("src/scherzo/orchestrator/service.gleam", 2118, 25),
    SourceLimit("src/scherzo/orchestrator/transition.gleam", 3159, 23),
    SourceLimit("src/scherzo/pi/protocol.gleam", 1067, 3),
    SourceLimit("src/scherzo/state/artifact_store.gleam", 1134, 7),
    SourceLimit("src/scherzo/state/local_artifacts.gleam", 1232, 8),
    SourceLimit("src/scherzo/state/projection.gleam", 6091, 12),
    SourceLimit("src/scherzo/state/record.gleam", 4073, 10),
    SourceLimit("src/scherzo/state/recovery.gleam", 3031, 16),
    SourceLimit("src/scherzo/step_artifact.gleam", 1563, 9),
    SourceLimit("src/scherzo/terminal/render.gleam", 1090, 6),
    SourceLimit("src/scherzo/tracker/conformance/json.gleam", 1168, 5),
    SourceLimit("src/scherzo/workstream/artifacts.gleam", 1049, 5),
    SourceLimit("src/scherzo/workstream/projection.gleam", 1038, 6),
    SourceLimit("src/scherzo/workstream/start.gleam", 1076, 18),
    SourceLimit("src/scherzo/workflow_recovery_planner.gleam", 1194, 4),
    SourceLimit("src/scherzo/workflow_repair.gleam", 1855, 8),
    SourceLimit("src/scherzo/workflow_checkpoint.gleam", 1276, 15),
    SourceLimit("src/scherzo/workflow_contract.gleam", 1318, 1),
    SourceLimit("src/scherzo/workflow_dag.gleam", 1703, 8),
    SourceLimit("src/scherzo/workflow_run.gleam", 5660, 38),
    SourceLimit("src/scherzo/workspace_run.gleam", 1301, 12),
    SourceLimit("src/scherzo_port_ffi.erl", 1181, 0),
  ]
}
