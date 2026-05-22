import gleam/string
import scherzo/tracker/conformance/runner
import scherzo/tracker/conformance/types
import simplifile

pub fn comments_pack_passes_and_classifies_receipt_probe_cleanup_and_unsupported_failures_test() {
  reset_report_dir()

  let assert Ok(types.RunResult(
    report: pass_report,
    exit_code: pass_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/comments-pass.manifest.json",
      "test/tmp/tracker-conformance/comments-pass.report.json",
    )
  let assert Ok(types.RunResult(
    report: rerun_report,
    exit_code: rerun_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/comments-pass.manifest.json",
      "test/tmp/tracker-conformance/comments-pass-rerun.report.json",
    )
  let assert Ok(types.RunResult(
    report: defective_report,
    exit_code: defective_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/comments-defective-wrong-receipt.manifest.json",
      "test/tmp/tracker-conformance/comments-defective-wrong-receipt.report.json",
    )
  let assert Ok(types.RunResult(
    report: duplicate_report,
    exit_code: duplicate_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/comments-duplicate-update.manifest.json",
      "test/tmp/tracker-conformance/comments-duplicate-update.report.json",
    )
  let assert Ok(types.RunResult(
    report: unsupported_report,
    exit_code: unsupported_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/comments-unsupported-update-claimed.manifest.json",
      "test/tmp/tracker-conformance/comments-unsupported-update-claimed.report.json",
    )
  let assert Ok(types.RunResult(
    report: probe_report,
    exit_code: probe_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/comments-probe-fails.manifest.json",
      "test/tmp/tracker-conformance/comments-probe-fails.report.json",
    )
  let assert Ok(types.RunResult(
    report: cleanup_report,
    exit_code: cleanup_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/comments-cleanup-fails.manifest.json",
      "test/tmp/tracker-conformance/comments-cleanup-fails.report.json",
    )
  let assert Ok(cleanup_body) =
    simplifile.read(
      "test/tmp/tracker-conformance/comments-cleanup-fails.report.json",
    )

  let types.Report(
    passed: pass_passed,
    failed: pass_failed,
    probe_failed: pass_probe_failed,
    cleanup_failed: pass_cleanup_failed,
    case_results: pass_case_results,
    ..,
  ) = pass_report
  let types.Report(
    passed: rerun_passed,
    failed: rerun_failed,
    probe_failed: rerun_probe_failed,
    cleanup_failed: rerun_cleanup_failed,
    ..,
  ) = rerun_report
  let types.Report(
    passed: defective_passed,
    failed: defective_failed,
    cleanup_failed: defective_cleanup_failed,
    case_results: defective_case_results,
    ..,
  ) = defective_report
  let types.Report(
    passed: duplicate_passed,
    failed: duplicate_failed,
    probe_failed: duplicate_probe_failed,
    ..,
  ) = duplicate_report
  let types.Report(
    passed: unsupported_passed,
    failed: unsupported_failed,
    case_results: unsupported_case_results,
    ..,
  ) = unsupported_report
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

  assert pass_exit_code == 0
  assert pass_passed == 9
  assert pass_failed == 0
  assert pass_probe_failed == 0
  assert pass_cleanup_failed == 0
  assert has_passed_case(
    pass_case_results,
    "comments.post_or_update.update_missing_no_fallback",
  )

  assert rerun_exit_code == 0
  assert rerun_passed == 9
  assert rerun_failed == 0
  assert rerun_probe_failed == 0
  assert rerun_cleanup_failed == 0

  assert defective_exit_code == 1
  assert defective_passed == 7
  assert defective_failed == 2
  assert defective_cleanup_failed == 0
  assert has_failed_case(
    defective_case_results,
    "comments.post_or_update.create_only",
  )
  assert has_failed_case(
    defective_case_results,
    "comments.post_or_update.update_existing",
  )

  assert duplicate_exit_code == 1
  assert duplicate_passed == 9
  assert duplicate_failed == 0
  assert duplicate_probe_failed == 1

  assert unsupported_exit_code == 1
  assert unsupported_passed == 8
  assert unsupported_failed == 1
  assert has_failed_case(
    unsupported_case_results,
    "comments.post_or_update.update_existing",
  )

  assert probe_exit_code == 1
  assert probe_passed == 9
  assert probe_failed_cases == 0
  assert probe_failed == 1

  assert cleanup_exit_code == 1
  assert cleanup_passed == 9
  assert cleanup_failed_cases == 0
  assert cleanup_failed == 1
  assert !string_contains(cleanup_body, "SECRET_TOKEN")
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

fn string_contains(text: String, fragment: String) -> Bool {
  string.contains(text, fragment)
}
