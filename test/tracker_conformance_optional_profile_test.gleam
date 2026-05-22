import gleam/string
import scherzo/tracker/conformance/runner
import scherzo/tracker/conformance/types
import simplifile

pub fn claimed_comments_not_requested_runs_only_task_source_cases_test() {
  let report_path =
    "test/tmp/tracker-conformance/claimed-comments-not-requested.report.json"
  reset_report_dir()

  let assert Ok(types.RunResult(
    report: run_report,
    summary: summary,
    exit_code: exit_code,
  )) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/claimed-comments-not-requested.manifest.json",
      report_path,
    )

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
  assert string.contains(summary, "profile=task_source")
  assert case_ids(case_results)
    == [
      "task_source.fetch.backend_kind",
      "task_source.refresh.stable_identity",
      "task_source.refresh.wrong_backend_ref",
      "task_source.lookup.empty_operator_ref",
      "task_source.lookup.known_operator_ref",
    ]
}

pub fn requested_comments_without_capability_fails_manifest_validation_test() {
  reset_report_dir()

  let assert Error(runner.RunError(code: code, message: message)) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/invalid-requested-comments-without-capability.manifest.json",
      "test/tmp/tracker-conformance/invalid-requested-comments-without-capability.report.json",
    )

  assert code == "missing_requested_pack_capability"
  assert message
    == "profile.requested_packs includes comments but profile.capabilities is missing comments.create"
  let assert Error(_) =
    simplifile.read(
      "test/tmp/tracker-conformance/invalid-requested-comments-without-capability.report.json",
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
