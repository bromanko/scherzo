import gleam/string
import scherzo/tracker/conformance/runner
import scherzo/tracker/conformance/types
import simplifile

pub fn handoff_pack_pass_scenario_covers_generic_legacy_retry_and_support_failures_test() {
  reset_report_dir()

  let types.RunResult(report: pass_report, exit_code: pass_exit_code, ..) =
    run_handoff_manifest(
      "test/fixtures/tracker_conformance/handoff-pass.manifest.json",
      "test/tmp/tracker-conformance/handoff-pass.report.json",
    )
  let types.Report(
    passed: pass_passed,
    failed: pass_failed,
    probe_failed: pass_probe_failed,
    cleanup_failed: pass_cleanup_failed,
    case_results: pass_cases,
    ..,
  ) = pass_report

  assert pass_exit_code == 0
  assert pass_passed == 21
  assert pass_failed == 0
  assert pass_probe_failed == 0
  assert pass_cleanup_failed == 0
  assert has_passed_case(pass_cases, "handoff.report.retry.claim")
  assert has_passed_case(pass_cases, "handoff.report.retry.success")
  assert has_passed_case(pass_cases, "handoff.report.retry.failure")
  assert has_passed_case(pass_cases, "handoff.report.retry.park")
  assert has_passed_case(pass_cases, "handoff.report.retry.legacy_claim")
  assert has_passed_case(pass_cases, "handoff.report.retry.legacy_success")
  assert has_passed_case(pass_cases, "handoff.report.retry.legacy_failure")
  assert has_passed_case(pass_cases, "handoff.report.retry.legacy_park")
}

pub fn handoff_pack_duplicate_retry_scenario_remains_successful_test() {
  reset_report_dir()

  let types.RunResult(
    report: duplicate_report,
    exit_code: duplicate_exit_code,
    ..,
  ) =
    run_handoff_manifest(
      "test/fixtures/tracker_conformance/handoff-retry-duplicate-visible.manifest.json",
      "test/tmp/tracker-conformance/handoff-duplicate.report.json",
    )
  let types.Report(
    passed: duplicate_passed,
    failed: duplicate_failed,
    probe_failed: duplicate_probe_failed,
    ..,
  ) = duplicate_report

  assert duplicate_exit_code == 0
  assert duplicate_passed == 21
  assert duplicate_failed == 0
  assert duplicate_probe_failed == 0
}

pub fn handoff_pack_defective_scenario_reports_failure_test() {
  reset_report_dir()

  let types.RunResult(
    report: defective_report,
    exit_code: defective_exit_code,
    ..,
  ) =
    run_handoff_manifest(
      "test/fixtures/tracker_conformance/handoff-defective.manifest.json",
      "test/tmp/tracker-conformance/handoff-defective.report.json",
    )
  let types.Report(
    passed: defective_passed,
    failed: defective_failed,
    case_results: defective_cases,
    ..,
  ) = defective_report

  assert defective_exit_code == 1
  assert defective_passed == 20
  assert defective_failed == 1
  assert has_failed_case(defective_cases, "handoff.report.failure")
}

pub fn handoff_pack_probe_failure_scenario_reports_probe_failure_test() {
  reset_report_dir()

  let types.RunResult(report: probe_report, exit_code: probe_exit_code, ..) =
    run_handoff_manifest(
      "test/fixtures/tracker_conformance/handoff-probe-fails.manifest.json",
      "test/tmp/tracker-conformance/handoff-probe.report.json",
    )
  let types.Report(
    passed: probe_passed,
    failed: probe_failed_cases,
    probe_failed: probe_failed,
    ..,
  ) = probe_report

  assert probe_exit_code == 1
  assert probe_passed == 21
  assert probe_failed_cases == 0
  assert probe_failed == 1
}

pub fn handoff_pack_cleanup_failure_scenario_reports_cleanup_failure_test() {
  reset_report_dir()

  let types.RunResult(report: cleanup_report, exit_code: cleanup_exit_code, ..) =
    run_handoff_manifest(
      "test/fixtures/tracker_conformance/handoff-cleanup-fails.manifest.json",
      "test/tmp/tracker-conformance/handoff-cleanup.report.json",
    )
  let assert Ok(cleanup_body) =
    simplifile.read("test/tmp/tracker-conformance/handoff-cleanup.report.json")
  let types.Report(
    passed: cleanup_passed,
    failed: cleanup_failed_cases,
    cleanup_failed: cleanup_failed,
    ..,
  ) = cleanup_report

  assert cleanup_exit_code == 1
  assert cleanup_passed == 21
  assert cleanup_failed_cases == 0
  assert cleanup_failed == 1
  assert !string.contains(cleanup_body, "SECRET_TOKEN")
}

fn run_handoff_manifest(
  manifest_path: String,
  report_path: String,
) -> types.RunResult {
  let assert Ok(result) = runner.run_manifest_path(manifest_path, report_path)
  result
}

fn reset_report_dir() -> Nil {
  let _ = simplifile.delete("test/tmp/tracker-conformance")
  let assert Ok(Nil) =
    simplifile.create_directory_all("test/tmp/tracker-conformance")
  Nil
}

fn has_failed_case(
  case_results: List(types.CaseResult),
  target: String,
) -> Bool {
  case case_results {
    [] -> False
    [types.CaseResult(id: id, status: status, ..), ..rest] ->
      id == target
      && status == types.FailedStatus
      || has_failed_case(rest, target)
  }
}

fn has_passed_case(
  case_results: List(types.CaseResult),
  target: String,
) -> Bool {
  case case_results {
    [] -> False
    [types.CaseResult(id: id, status: status, ..), ..rest] ->
      id == target
      && status == types.PassedStatus
      || has_passed_case(rest, target)
  }
}
