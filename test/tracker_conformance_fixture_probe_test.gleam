import gleam/string
import scherzo/tracker/conformance/runner
import scherzo/tracker/conformance/types
import simplifile

pub fn runner_classifies_setup_and_probe_failures_separately_test() {
  reset_report_dir()

  let assert Ok(types.RunResult(
    report: setup_report,
    exit_code: setup_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/setup-fails.manifest.json",
      "test/tmp/tracker-conformance/setup-fails.report.json",
    )
  let assert Ok(types.RunResult(
    report: probe_report,
    exit_code: probe_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/probe-fails.manifest.json",
      "test/tmp/tracker-conformance/probe-fails.report.json",
    )
  let assert Ok(types.RunResult(
    report: cleanup_report,
    exit_code: cleanup_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/cleanup-fails.manifest.json",
      "test/tmp/tracker-conformance/cleanup-fails.report.json",
    )
  let assert Ok(cleanup_report_body) =
    simplifile.read("test/tmp/tracker-conformance/cleanup-fails.report.json")

  let types.Report(
    passed: setup_passed,
    failed: setup_failed_cases,
    setup_failed: setup_failed,
    case_results: setup_case_results,
    ..,
  ) = setup_report
  let types.Report(
    passed: probe_passed,
    failed: probe_failed_cases,
    probe_failed: probe_failed,
    ..,
  ) = probe_report
  let types.Report(
    passed: cleanup_passed,
    failed: cleanup_failed_cases,
    cleanup_failed: cleanup_failed,
    ..,
  ) = cleanup_report

  assert setup_exit_code == 1
  assert setup_passed == 0
  assert setup_failed_cases == 0
  assert setup_failed == 1
  assert setup_case_results == []

  assert probe_exit_code == 1
  assert probe_passed == 5
  assert probe_failed_cases == 0
  assert probe_failed == 1

  assert cleanup_exit_code == 1
  assert cleanup_passed == 5
  assert cleanup_failed_cases == 0
  assert cleanup_failed == 1
  assert !string.contains(cleanup_report_body, "SECRET_TOKEN")
  assert string.contains(cleanup_report_body, "[REDACTED]")
}

fn reset_report_dir() -> Nil {
  let _ = simplifile.delete("test/tmp/tracker-conformance")
  let assert Ok(Nil) =
    simplifile.create_directory_all("test/tmp/tracker-conformance")
  Nil
}
