import scherzo/tracker/conformance/report
import scherzo/tracker/conformance/types
import simplifile

pub fn report_to_string_redacts_and_matches_expected_fixtures_test() {
  let assert Ok(expected_pass) =
    simplifile.read(
      "test/fixtures/tracker_conformance/report-pass.expected.json",
    )
  let assert Ok(expected_failure) =
    simplifile.read(
      "test/fixtures/tracker_conformance/report-failure.expected.json",
    )

  assert report.to_string(pass_report(), redact: ["SECRET_TOKEN"])
    == expected_pass
  assert report.to_string(failure_report(), redact: ["SECRET_TOKEN"])
    == expected_failure
  assert report.exit_code(pass_report()) == 0
  assert report.exit_code(failure_report()) == 1
}

fn pass_report() -> types.Report {
  types.Report(
    schema_version: 1,
    adapter_kind: "test-memory",
    profile: "task_source",
    passed: 1,
    failed: 0,
    skipped: 0,
    setup_failed: 0,
    probe_failed: 0,
    cleanup_failed: 0,
    case_results: [
      types.CaseResult(
        id: "case-1",
        operation: "task_source.fetch_candidates",
        status: types.PassedStatus,
        request_id: "req-1",
        message: "ok SECRET_TOKEN",
        diagnostics: "diag SECRET_TOKEN",
      ),
    ],
    hook_results: [
      types.HookResult(
        phase: "setup",
        status: types.PassedStatus,
        message: "setup ok SECRET_TOKEN",
        diagnostics: "setup diag SECRET_TOKEN",
      ),
    ],
    probe_results: [
      types.ProbeResult(
        name: "probe-1",
        status: types.PassedStatus,
        message: "probe ok SECRET_TOKEN",
        diagnostics: "probe diag SECRET_TOKEN",
      ),
    ],
  )
}

fn failure_report() -> types.Report {
  types.Report(
    schema_version: 1,
    adapter_kind: "test-memory",
    profile: "task_source",
    passed: 0,
    failed: 1,
    skipped: 0,
    setup_failed: 1,
    probe_failed: 1,
    cleanup_failed: 1,
    case_results: [
      types.CaseResult(
        id: "case-2",
        operation: "task_source.refresh_by_refs",
        status: types.FailedStatus,
        request_id: "req-2",
        message: "failed SECRET_TOKEN",
        diagnostics: "case diag SECRET_TOKEN",
      ),
    ],
    hook_results: [
      types.HookResult(
        phase: "setup",
        status: types.SetupFailedStatus,
        message: "setup failed SECRET_TOKEN",
        diagnostics: "setup diag SECRET_TOKEN",
      ),
      types.HookResult(
        phase: "cleanup",
        status: types.CleanupFailedStatus,
        message: "cleanup failed SECRET_TOKEN",
        diagnostics: "cleanup diag SECRET_TOKEN",
      ),
    ],
    probe_results: [
      types.ProbeResult(
        name: "probe-2",
        status: types.ProbeFailedStatus,
        message: "probe failed SECRET_TOKEN",
        diagnostics: "probe diag SECRET_TOKEN",
      ),
    ],
  )
}
