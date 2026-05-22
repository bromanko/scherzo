import gleam/string
import scherzo/tracker/conformance/runner
import scherzo/tracker/conformance/types
import simplifile

pub fn routing_metadata_pack_passes_and_classifies_label_and_blocker_failures_test() {
  reset_report_dir()

  let assert Ok(types.RunResult(
    report: pass_report,
    exit_code: pass_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/routing-metadata-pass.manifest.json",
      "test/tmp/tracker-conformance/routing-metadata-pass.report.json",
    )
  let assert Ok(types.RunResult(
    report: label_report,
    exit_code: label_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/routing-metadata-missing-label.manifest.json",
      "test/tmp/tracker-conformance/routing-metadata-missing-label.report.json",
    )
  let assert Ok(types.RunResult(
    report: nonmatching_label_report,
    exit_code: nonmatching_label_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/routing-metadata-nonmatching-label.manifest.json",
      "test/tmp/tracker-conformance/routing-metadata-nonmatching-label.report.json",
    )
  let assert Ok(types.RunResult(
    report: blocker_report,
    exit_code: blocker_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/routing-metadata-wrong-blocker-ref.manifest.json",
      "test/tmp/tracker-conformance/routing-metadata-wrong-blocker-ref.report.json",
    )
  let assert Ok(types.RunResult(
    report: duplicate_blocker_report,
    exit_code: duplicate_blocker_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/routing-metadata-duplicate-blocker-ref.manifest.json",
      "test/tmp/tracker-conformance/routing-metadata-duplicate-blocker-ref.report.json",
    )
  let assert Ok(pass_body) =
    simplifile.read(
      "test/tmp/tracker-conformance/routing-metadata-pass.report.json",
    )

  let types.Report(
    passed: pass_passed,
    failed: pass_failed,
    probe_failed: pass_probe_failed,
    case_results: pass_case_results,
    ..,
  ) = pass_report
  let types.Report(
    passed: label_passed,
    failed: label_failed,
    case_results: label_case_results,
    ..,
  ) = label_report
  let types.Report(
    passed: nonmatching_label_passed,
    failed: nonmatching_label_failed,
    case_results: nonmatching_label_case_results,
    ..,
  ) = nonmatching_label_report
  let types.Report(
    passed: blocker_passed,
    failed: blocker_failed,
    case_results: blocker_case_results,
    ..,
  ) = blocker_report
  let types.Report(
    passed: duplicate_blocker_passed,
    failed: duplicate_blocker_failed,
    case_results: duplicate_blocker_case_results,
    ..,
  ) = duplicate_blocker_report

  assert pass_exit_code == 0
  assert pass_passed == 7
  assert pass_failed == 0
  assert pass_probe_failed == 0
  assert has_passed_case(
    pass_case_results,
    "routing_metadata.fetch.workflow_labels",
  )
  assert has_passed_case(
    pass_case_results,
    "routing_metadata.refresh.blocker_refs",
  )
  assert !string_contains(pass_body, "SECRET_TOKEN")

  assert label_exit_code == 1
  assert label_passed == 6
  assert label_failed == 1
  assert has_failed_case(
    label_case_results,
    "routing_metadata.fetch.workflow_labels",
  )

  assert nonmatching_label_exit_code == 1
  assert nonmatching_label_passed == 6
  assert nonmatching_label_failed == 1
  assert has_failed_case(
    nonmatching_label_case_results,
    "routing_metadata.fetch.workflow_labels",
  )

  assert blocker_exit_code == 1
  assert blocker_passed == 6
  assert blocker_failed == 1
  assert has_failed_case(
    blocker_case_results,
    "routing_metadata.refresh.blocker_refs",
  )

  assert duplicate_blocker_exit_code == 1
  assert duplicate_blocker_passed == 6
  assert duplicate_blocker_failed == 1
  assert has_failed_case(
    duplicate_blocker_case_results,
    "routing_metadata.refresh.blocker_refs",
  )
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
