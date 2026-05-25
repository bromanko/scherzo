import gleam/string
import scherzo/tracker/conformance/runner
import scherzo/tracker/conformance/types
import simplifile

pub fn scheduled_failures_pack_covers_create_retry_recovery_and_support_failures_test() {
  reset_report_dir()

  let assert Ok(types.RunResult(
    report: pass_report,
    exit_code: pass_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/scheduled-failures-pass.manifest.json",
      "test/tmp/tracker-conformance/scheduled-failures-pass.report.json",
    )
  let assert Ok(types.RunResult(
    report: dynamic_report,
    exit_code: dynamic_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/scheduled-failures-dynamic-id.manifest.json",
      "test/tmp/tracker-conformance/scheduled-failures-dynamic-id.report.json",
    )
  let assert Ok(types.RunResult(
    report: duplicate_report,
    exit_code: duplicate_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/scheduled-failures-duplicate-defective.manifest.json",
      "test/tmp/tracker-conformance/scheduled-failures-duplicate-defective.report.json",
    )
  let assert Ok(types.RunResult(
    report: wrong_receipt_report,
    exit_code: wrong_receipt_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/scheduled-failures-wrong-receipt.manifest.json",
      "test/tmp/tracker-conformance/scheduled-failures-wrong-receipt.report.json",
    )
  let assert Ok(types.RunResult(
    report: no_visible_report,
    exit_code: no_visible_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/scheduled-failures-no-visible-task.manifest.json",
      "test/tmp/tracker-conformance/scheduled-failures-no-visible-task.report.json",
    )
  let assert Ok(types.RunResult(
    report: created_flag_report,
    exit_code: created_flag_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/scheduled-failures-created-flag-defective.manifest.json",
      "test/tmp/tracker-conformance/scheduled-failures-created-flag-defective.report.json",
    )
  let assert Ok(types.RunResult(
    report: metadata_loss_report,
    exit_code: metadata_loss_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/scheduled-failures-metadata-loss.manifest.json",
      "test/tmp/tracker-conformance/scheduled-failures-metadata-loss.report.json",
    )
  let assert Ok(types.RunResult(
    report: probe_failure_report,
    exit_code: probe_failure_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/scheduled-failures-probe-fails.manifest.json",
      "test/tmp/tracker-conformance/scheduled-failures-probe-fails.report.json",
    )
  let assert Ok(types.RunResult(
    report: cleanup_failure_report,
    exit_code: cleanup_failure_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/scheduled-failures-cleanup-fails.manifest.json",
      "test/tmp/tracker-conformance/scheduled-failures-cleanup-fails.report.json",
    )

  let assert Ok(pass_body) =
    simplifile.read(
      "test/tmp/tracker-conformance/scheduled-failures-pass.report.json",
    )
  let assert Ok(dynamic_body) =
    simplifile.read(
      "test/tmp/tracker-conformance/scheduled-failures-dynamic-id.report.json",
    )
  let assert Ok(duplicate_body) =
    simplifile.read(
      "test/tmp/tracker-conformance/scheduled-failures-duplicate-defective.report.json",
    )
  let assert Ok(cleanup_body) =
    simplifile.read(
      "test/tmp/tracker-conformance/scheduled-failures-cleanup-fails.report.json",
    )
  let assert Ok(probe_body) =
    simplifile.read(
      "test/tmp/tracker-conformance/scheduled-failures-probe-fails.report.json",
    )
  let assert Ok(expected_pass_body) =
    simplifile.read(
      "test/fixtures/tracker_conformance/report-scheduled-failures-pass.expected.json",
    )
  let assert Ok(expected_defective_body) =
    simplifile.read(
      "test/fixtures/tracker_conformance/report-scheduled-failures-defective.expected.json",
    )

  let types.Report(
    passed: pass_passed,
    failed: pass_failed,
    probe_failed: pass_probe_failed,
    cleanup_failed: pass_cleanup_failed,
    case_results: pass_cases,
    ..,
  ) = pass_report
  let types.Report(
    passed: dynamic_passed,
    failed: dynamic_failed,
    probe_failed: dynamic_probe_failed,
    cleanup_failed: dynamic_cleanup_failed,
    ..,
  ) = dynamic_report
  let types.Report(
    passed: duplicate_passed,
    failed: duplicate_failed,
    probe_failed: duplicate_probe_failed,
    ..,
  ) = duplicate_report
  let types.Report(
    passed: wrong_receipt_passed,
    failed: wrong_receipt_failed,
    case_results: wrong_receipt_cases,
    ..,
  ) = wrong_receipt_report
  let types.Report(
    passed: no_visible_passed,
    failed: no_visible_failed,
    probe_failed: no_visible_probe_failed,
    ..,
  ) = no_visible_report
  let types.Report(
    passed: created_flag_passed,
    failed: created_flag_failed,
    case_results: created_flag_cases,
    ..,
  ) = created_flag_report
  let types.Report(
    passed: metadata_loss_passed,
    failed: metadata_loss_failed,
    probe_failed: metadata_loss_probe_failed,
    ..,
  ) = metadata_loss_report
  let types.Report(
    passed: probe_failure_passed,
    failed: probe_failure_failed,
    probe_failed: probe_failure_probe_failed,
    ..,
  ) = probe_failure_report
  let types.Report(
    passed: cleanup_failure_passed,
    failed: cleanup_failure_failed,
    cleanup_failed: cleanup_failure_cleanup_failed,
    ..,
  ) = cleanup_failure_report

  assert pass_exit_code == 0
  assert pass_passed == 8
  assert pass_failed == 0
  assert pass_probe_failed == 0
  assert pass_cleanup_failed == 0
  assert has_passed_case(pass_cases, "scheduled_failures.publish.create")
  assert has_passed_case(
    pass_cases,
    "scheduled_failures.publish.remembered_retry",
  )
  assert has_passed_case(
    pass_cases,
    "scheduled_failures.publish.dedupe_recovery",
  )
  assert pass_body == expected_pass_body

  assert dynamic_exit_code == 0
  assert dynamic_passed == 8
  assert dynamic_failed == 0
  assert dynamic_probe_failed == 0
  assert dynamic_cleanup_failed == 0
  assert string.contains(dynamic_body, "scheduled-failure-task-generated")
  assert string.contains(
    dynamic_body,
    "\\\"previous_task_remote_id\\\":\\\"scheduled-failure-task-generated\\\"",
  )

  assert duplicate_exit_code == 1
  assert duplicate_passed == 8
  assert duplicate_failed == 0
  assert duplicate_probe_failed == 1
  assert duplicate_body == expected_defective_body

  assert wrong_receipt_exit_code == 1
  assert wrong_receipt_passed == 7
  assert wrong_receipt_failed == 1
  assert has_failed_case(
    wrong_receipt_cases,
    "scheduled_failures.publish.remembered_retry",
  )

  assert no_visible_exit_code == 1
  assert no_visible_passed == 8
  assert no_visible_failed == 0
  assert no_visible_probe_failed == 1

  assert created_flag_exit_code == 1
  assert created_flag_passed == 6
  assert created_flag_failed == 2
  assert has_failed_case(
    created_flag_cases,
    "scheduled_failures.publish.create",
  )
  assert has_failed_case(
    created_flag_cases,
    "scheduled_failures.publish.dedupe_recovery",
  )

  assert metadata_loss_exit_code == 1
  assert metadata_loss_passed == 8
  assert metadata_loss_failed == 0
  assert metadata_loss_probe_failed == 1

  assert probe_failure_exit_code == 1
  assert probe_failure_passed == 8
  assert probe_failure_failed == 0
  assert probe_failure_probe_failed == 1

  assert cleanup_failure_exit_code == 1
  assert cleanup_failure_passed == 8
  assert cleanup_failure_failed == 0
  assert cleanup_failure_cleanup_failed == 1

  assert !string.contains(pass_body, "SECRET_TOKEN")
  assert !string.contains(dynamic_body, "SECRET_TOKEN")
  assert !string.contains(duplicate_body, "SECRET_TOKEN")
  assert !string.contains(cleanup_body, "SECRET_TOKEN")
  assert !string.contains(probe_body, "SECRET_TOKEN")
  assert string.contains(cleanup_body, "scheduled-failure-task-1")
  assert string.contains(cleanup_body, "\"cleanup_status\":\"cleanup_failed\"")
  assert string.contains(cleanup_body, "\"probe_status\":\"passed\"")
  assert string.contains(probe_body, "\"cleanup_status\":\"passed\"")
  assert string.contains(probe_body, "\"probe_status\":\"probe_failed\"")
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
