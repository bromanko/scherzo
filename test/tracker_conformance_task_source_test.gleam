import gleam/string
import scherzo/tracker/conformance/runner
import scherzo/tracker/conformance/types
import simplifile

pub fn task_source_runner_passes_fake_driver_and_writes_report_test() {
  let report_path = "test/tmp/tracker-conformance/task-source-pass.report.json"
  reset_report_dir()

  let assert Ok(types.RunResult(
    report: run_report,
    summary: summary,
    exit_code: exit_code,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/task-source-pass.manifest.json",
      report_path,
    )
  let assert Ok(report_body) = simplifile.read(report_path)
  let types.Report(
    passed: passed,
    failed: failed,
    setup_failed: setup_failed,
    probe_failed: probe_failed,
    cleanup_failed: cleanup_failed,
    case_results: case_results,
    ..,
  ) = run_report

  assert exit_code == 0
  assert passed == 5
  assert failed == 0
  assert setup_failed == 0
  assert probe_failed == 0
  assert cleanup_failed == 0
  assert string_contains(summary, "adapter=test-memory")
  assert string_contains(summary, "profile=task_source")
  assert string_contains(summary, "total_cases=5")
  assert !string_contains(report_body, "SECRET_TOKEN")
  assert string_contains(report_body, "[REDACTED]")
  assert case_ids(case_results)
    == [
      "task_source.fetch.backend_kind",
      "task_source.refresh.stable_identity",
      "task_source.refresh.wrong_backend_ref",
      "task_source.lookup.empty_operator_ref",
      "task_source.lookup.known_operator_ref",
    ]
}

pub fn task_source_runner_reports_unstable_identity_and_wrong_backend_failures_test() {
  reset_report_dir()

  let assert Ok(types.RunResult(
    report: unstable_report,
    exit_code: unstable_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/task-source-unstable-identity.manifest.json",
      "test/tmp/tracker-conformance/task-source-unstable-identity.report.json",
    )
  let assert Ok(types.RunResult(
    report: wrong_backend_report,
    exit_code: wrong_backend_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/task-source-wrong-backend-ref.manifest.json",
      "test/tmp/tracker-conformance/task-source-wrong-backend-ref.report.json",
    )

  assert unstable_exit_code == 1
  assert wrong_backend_exit_code == 1
  assert has_failed_case(unstable_report, "task_source.refresh.stable_identity")
  assert has_failed_case(
    wrong_backend_report,
    "task_source.refresh.wrong_backend_ref",
  )
}

pub fn task_source_runner_reports_fetch_and_lookup_failures_test() {
  reset_report_dir()

  let assert Ok(types.RunResult(
    report: fetch_report,
    exit_code: fetch_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/task-source-fetch-wrong-backend.manifest.json",
      "test/tmp/tracker-conformance/task-source-fetch-wrong-backend.report.json",
    )
  let assert Ok(types.RunResult(
    report: lookup_empty_report,
    exit_code: lookup_empty_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/task-source-lookup-empty-matches.manifest.json",
      "test/tmp/tracker-conformance/task-source-lookup-empty-matches.report.json",
    )
  let assert Ok(types.RunResult(
    report: lookup_known_report,
    exit_code: lookup_known_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/task-source-lookup-known-wrong.manifest.json",
      "test/tmp/tracker-conformance/task-source-lookup-known-wrong.report.json",
    )

  assert fetch_exit_code == 1
  assert lookup_empty_exit_code == 1
  assert lookup_known_exit_code == 1
  assert has_failed_case(fetch_report, "task_source.fetch.backend_kind")
  assert has_failed_case(
    lookup_empty_report,
    "task_source.lookup.empty_operator_ref",
  )
  assert has_failed_case(
    lookup_known_report,
    "task_source.lookup.known_operator_ref",
  )
}

fn reset_report_dir() -> Nil {
  let _ = simplifile.delete("test/tmp/tracker-conformance")
  let assert Ok(Nil) =
    simplifile.create_directory_all("test/tmp/tracker-conformance")
  Nil
}

fn case_ids(case_results: List(types.CaseResult)) -> List(String) {
  case case_results {
    [] -> []
    [types.CaseResult(id: id, ..), ..rest] -> [id, ..case_ids(rest)]
  }
}

fn has_failed_case(report_value: types.Report, id target: String) -> Bool {
  let types.Report(case_results: case_results, ..) = report_value
  has_failed_case_in_list(case_results, target)
}

fn has_failed_case_in_list(
  case_results: List(types.CaseResult),
  target: String,
) -> Bool {
  case case_results {
    [] -> False
    [types.CaseResult(id: id, status: status, ..), ..rest] ->
      case id == target && status == types.FailedStatus {
        True -> True
        False -> has_failed_case_in_list(rest, target)
      }
  }
}

fn string_contains(text: String, fragment: String) -> Bool {
  string.contains(text, fragment)
}
