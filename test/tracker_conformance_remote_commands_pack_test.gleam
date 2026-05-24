import gleam/string
import scherzo/tracker/conformance/runner
import scherzo/tracker/conformance/types
import simplifile

pub fn remote_commands_pack_covers_fetch_ack_retry_and_support_failures_test() {
  reset_report_dir()

  let assert Ok(types.RunResult(
    report: pass_report,
    exit_code: pass_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/remote-commands-pass.manifest.json",
      "test/tmp/tracker-conformance/remote-commands-pass.report.json",
    )
  let assert Ok(types.RunResult(
    report: combined_report,
    exit_code: combined_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/remote-commands-with-state-transitions.manifest.json",
      "test/tmp/tracker-conformance/remote-commands-with-state-transitions.report.json",
    )
  let assert Ok(types.RunResult(
    report: duplicate_report,
    exit_code: duplicate_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/remote-commands-ack-retry-duplicate-visible.manifest.json",
      "test/tmp/tracker-conformance/remote-commands-duplicate.report.json",
    )
  let assert Ok(types.RunResult(
    report: ack_defective_report,
    exit_code: ack_defective_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/remote-commands-ack-defective.manifest.json",
      "test/tmp/tracker-conformance/remote-commands-ack-defective.report.json",
    )
  let assert Ok(types.RunResult(
    report: defective_report,
    exit_code: defective_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/remote-commands-fetch-defective.manifest.json",
      "test/tmp/tracker-conformance/remote-commands-defective.report.json",
    )
  let assert Ok(types.RunResult(
    report: oversized_report,
    exit_code: oversized_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/remote-commands-oversized-body.manifest.json",
      "test/tmp/tracker-conformance/remote-commands-oversized.report.json",
    )
  let assert Ok(types.RunResult(
    report: probe_report,
    exit_code: probe_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/remote-commands-probe-fails.manifest.json",
      "test/tmp/tracker-conformance/remote-commands-probe.report.json",
    )
  let assert Ok(types.RunResult(
    report: cleanup_report,
    exit_code: cleanup_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/remote-commands-cleanup-fails.manifest.json",
      "test/tmp/tracker-conformance/remote-commands-cleanup.report.json",
    )
  let assert Ok(cleanup_body) =
    simplifile.read(
      "test/tmp/tracker-conformance/remote-commands-cleanup.report.json",
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
    passed: combined_passed,
    failed: combined_failed,
    case_results: combined_cases,
    ..,
  ) = combined_report
  let types.Report(
    passed: duplicate_passed,
    failed: duplicate_failed,
    probe_failed: duplicate_probe_failed,
    ..,
  ) = duplicate_report
  let types.Report(
    passed: ack_defective_passed,
    failed: ack_defective_failed,
    case_results: ack_defective_cases,
    ..,
  ) = ack_defective_report
  let types.Report(
    passed: defective_passed,
    failed: defective_failed,
    case_results: defective_cases,
    ..,
  ) = defective_report
  let types.Report(
    passed: oversized_passed,
    failed: oversized_failed,
    case_results: oversized_cases,
    ..,
  ) = oversized_report
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
  assert pass_passed == 11
  assert pass_failed == 0
  assert pass_probe_failed == 0
  assert pass_cleanup_failed == 0
  assert has_passed_case(
    pass_cases,
    "remote_commands.post_ack.same_event_retry",
  )

  assert combined_exit_code == 0
  assert combined_passed == 15
  assert combined_failed == 0
  assert has_passed_case(
    combined_cases,
    "remote_commands.fetch.normalized_events",
  )
  assert has_passed_case(
    combined_cases,
    "state_transitions.transition.target_id_precedence",
  )

  assert duplicate_exit_code == 0
  assert duplicate_passed == 11
  assert duplicate_failed == 0
  assert duplicate_probe_failed == 0

  assert ack_defective_exit_code == 1
  assert ack_defective_passed == 10
  assert ack_defective_failed == 1
  assert has_failed_case(
    ack_defective_cases,
    "remote_commands.post_ack.same_event_retry",
  )

  assert defective_exit_code == 1
  assert defective_passed == 9
  assert defective_failed == 2
  assert has_failed_case(
    defective_cases,
    "remote_commands.fetch.normalized_events",
  )
  assert has_failed_case(
    defective_cases,
    "remote_commands.post_ack.failure_visibility",
  )

  assert oversized_exit_code == 1
  assert oversized_passed == 10
  assert oversized_failed == 1
  assert has_failed_case(
    oversized_cases,
    "remote_commands.fetch.normalized_events",
  )

  assert probe_exit_code == 1
  assert probe_passed == 11
  assert probe_failed_cases == 0
  assert probe_failed == 1

  assert cleanup_exit_code == 1
  assert cleanup_passed == 11
  assert cleanup_failed_cases == 0
  assert cleanup_failed == 1
  assert !string.contains(cleanup_body, "SECRET_TOKEN")
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
