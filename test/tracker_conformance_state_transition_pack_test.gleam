import scherzo/tracker/conformance/runner
import scherzo/tracker/conformance/types
import simplifile

pub fn state_transition_pack_passes_and_classifies_reason_blank_target_and_cleanup_failures_test() {
  reset_report_dir()

  let assert Ok(types.RunResult(
    report: pass_report,
    exit_code: pass_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/state-transition-pass.manifest.json",
      "test/tmp/tracker-conformance/state-transition-pass.report.json",
    )
  let assert Ok(types.RunResult(
    report: rerun_report,
    exit_code: rerun_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/state-transition-pass.manifest.json",
      "test/tmp/tracker-conformance/state-transition-pass-rerun.report.json",
    )
  let assert Ok(types.RunResult(
    report: no_reason_report,
    exit_code: no_reason_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/state-transition-no-reason-capability.manifest.json",
      "test/tmp/tracker-conformance/state-transition-no-reason-capability.report.json",
    )
  let assert Ok(types.RunResult(
    report: reason_report,
    exit_code: reason_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/state-transition-reason-missing.manifest.json",
      "test/tmp/tracker-conformance/state-transition-reason-missing.report.json",
    )
  let assert Ok(types.RunResult(
    report: cleanup_report,
    exit_code: cleanup_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/state-transition-cleanup-fails.manifest.json",
      "test/tmp/tracker-conformance/state-transition-cleanup-fails.report.json",
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
    passed: no_reason_passed,
    failed: no_reason_failed,
    case_results: no_reason_case_results,
    ..,
  ) = no_reason_report
  let types.Report(
    passed: reason_passed,
    failed: reason_failed,
    cleanup_failed: reason_cleanup_failed,
    case_results: reason_case_results,
    ..,
  ) = reason_report
  let types.Report(
    passed: cleanup_passed,
    failed: cleanup_failed_cases,
    cleanup_failed: cleanup_failed,
    ..,
  ) = cleanup_report

  assert pass_exit_code == 0
  assert pass_passed == 10
  assert pass_failed == 0
  assert pass_probe_failed == 0
  assert pass_cleanup_failed == 0
  assert has_passed_case(
    pass_case_results,
    "state_transitions.transition.blank_target",
  )
  assert has_passed_case(
    pass_case_results,
    "state_transitions.transition.reason_propagation",
  )

  assert rerun_exit_code == 0
  assert rerun_passed == 10
  assert rerun_failed == 0
  assert rerun_probe_failed == 0
  assert rerun_cleanup_failed == 0

  assert no_reason_exit_code == 0
  assert no_reason_passed == 9
  assert no_reason_failed == 0
  assert !has_case(
    no_reason_case_results,
    "state_transitions.transition.reason_propagation",
  )

  assert reason_exit_code == 1
  assert reason_passed == 9
  assert reason_failed == 1
  assert reason_cleanup_failed == 0
  assert has_failed_case(
    reason_case_results,
    "state_transitions.transition.reason_propagation",
  )

  assert cleanup_exit_code == 1
  assert cleanup_passed == 10
  assert cleanup_failed_cases == 0
  assert cleanup_failed == 1
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

fn has_case(case_results: List(types.CaseResult), target: String) -> Bool {
  case case_results {
    [] -> False
    [types.CaseResult(id: id, ..), ..rest] ->
      id == target || has_case(rest, target)
  }
}
