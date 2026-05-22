import gleam/option.{type Option, None, Some}
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
  assert string_contains(report_body, "\"counts\":")
  assert case_ids(case_results)
    == [
      "task_source.fetch.backend_kind",
      "task_source.refresh.stable_identity",
      "task_source.refresh.wrong_backend_ref",
      "task_source.lookup.empty_operator_ref",
      "task_source.lookup.known_operator_ref",
    ]
  assert list_all(case_results, fn(case_result) {
    let types.CaseResult(
      expected_summary: expected_summary,
      actual_summary: actual_summary,
      request_transcript: request_transcript,
      response_transcript: response_transcript,
      ..,
    ) = case_result
    let types.TranscriptEvidence(body: request_body, ..) = request_transcript
    expected_summary != ""
    && actual_summary != ""
    && string.contains(request_body, "\"request_id\":")
    && has_response_transcript(response_transcript)
  })
}

pub fn task_source_runner_uses_explicit_fixture_declarations_test() {
  let report_path =
    "test/tmp/tracker-conformance/task-source-explicit-fixtures.report.json"
  reset_report_dir()

  let assert Ok(types.RunResult(report: run_report, exit_code: exit_code, ..)) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/task-source-explicit-fixtures.manifest.json",
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
  assert string_contains(report_body, "fixture declaration secondary-card")
  assert case_ids(case_results)
    == [
      "task_source.fetch.backend_kind",
      "task_source.refresh.stable_identity",
      "task_source.refresh.wrong_backend_ref",
      "task_source.lookup.empty_operator_ref",
      "task_source.lookup.known_operator_ref",
    ]
  let assert Some(wrong_backend_case) =
    case_result_by_id(case_results, "task_source.refresh.wrong_backend_ref")
  let assert Some(known_lookup_case) =
    case_result_by_id(case_results, "task_source.lookup.known_operator_ref")
  assert request_transcript_contains(
    wrong_backend_case,
    "\"remote_id\":\"card-2\"",
  )
  assert !request_transcript_contains(
    wrong_backend_case,
    "\"remote_id\":\"card-1\"",
  )
  assert request_transcript_contains(
    known_lookup_case,
    "\"operator_ref\":\"CARD-2\"",
  )
}

pub fn task_source_runner_rejects_empty_fixture_inventory_test() {
  reset_report_dir()

  let assert Error(runner.RunError(code: code, message: message)) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/task-source-empty-fixtures.manifest.json",
      "test/tmp/tracker-conformance/task-source-empty-fixtures.report.json",
    )

  assert code == "fixture_task_file_empty"
  assert message
    == "fixtures.task_file must contain at least one task for task_source conformance"
}

pub fn task_source_runner_redacts_secret_transcripts_test() {
  let report_path =
    "test/tmp/tracker-conformance/task-source-secret-transcripts.report.json"
  reset_report_dir()

  let assert Ok(types.RunResult(summary: summary, exit_code: exit_code, ..)) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/task-source-secret-transcripts.manifest.json",
      report_path,
    )
  let assert Ok(report_body) = simplifile.read(report_path)

  assert exit_code == 0
  assert !string_contains(report_body, "SECRET_TOKEN")
  assert !string_contains(summary, "SECRET_TOKEN")
  assert string_contains(report_body, "[REDACTED]")
  assert string_contains(report_body, "\"request_transcript\":")
  assert string_contains(report_body, "\"response_transcript\":")
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

fn case_result_by_id(
  case_results: List(types.CaseResult),
  target: String,
) -> Option(types.CaseResult) {
  case case_results {
    [] -> None
    [case_result, ..rest] -> {
      let types.CaseResult(id: id, ..) = case_result
      case id == target {
        True -> Some(case_result)
        False -> case_result_by_id(rest, target)
      }
    }
  }
}

fn request_transcript_contains(
  case_result: types.CaseResult,
  fragment: String,
) -> Bool {
  let types.CaseResult(request_transcript: request_transcript, ..) = case_result
  let types.TranscriptEvidence(body: body, ..) = request_transcript
  string.contains(body, fragment)
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

fn has_response_transcript(value: Option(types.TranscriptEvidence)) -> Bool {
  case value {
    Some(types.TranscriptEvidence(body: body, ..)) -> body != ""
    None -> False
  }
}

fn list_all(values: List(a), predicate: fn(a) -> Bool) -> Bool {
  case values {
    [] -> True
    [value, ..rest] -> predicate(value) && list_all(rest, predicate)
  }
}

fn string_contains(text: String, fragment: String) -> Bool {
  string.contains(text, fragment)
}
