import gleam/string
import scherzo/tracker/conformance
import scherzo/tracker/conformance/profile
import scherzo/tracker/conformance/runner
import scherzo/tracker/conformance/types
import simplifile

const guide_path = "docs/runbooks/tracker-adapter-author-guide.md"

const example_dir = "examples/tracker-conformance/adapter-author"

pub fn adapter_author_guide_and_snippets_stay_in_sync_test() {
  let assert Ok(guide) = simplifile.read(guide_path)
  let assert Ok(request_body) =
    simplifile.read(example_dir <> "/request.fetch_candidates.json")
  let assert Ok(success_response_body) =
    simplifile.read(example_dir <> "/response.fetch_candidates.success.json")
  let assert Ok(stale_response_body) =
    simplifile.read(example_dir <> "/response.fetch_candidates.stale.json")
  let assert Ok(request) = conformance.decode_request(request_body)
  let assert Ok(success_response) =
    conformance.decode_response(success_response_body)
  let assert Ok(stale_response) =
    conformance.decode_response(stale_response_body)

  let documented_artifacts = [
    "driver.sh",
    "manifest.pass.json",
    "manifest.invalid-shape.json",
    "manifest.missing-capability.json",
    "manifest.namespace-misuse.json",
    "manifest.malformed-response.json",
    "manifest.stale-response.json",
    "manifest.redaction.json",
    "request.fetch_candidates.json",
    "response.fetch_candidates.success.json",
    "response.fetch_candidates.stale.json",
  ]
  assert list_all(documented_artifacts, fn(artifact) {
    string.contains(guide, artifact)
  })

  let types.DriverRequest(request_id: request_id, operation: operation, ..) =
    request
  assert request_id == "req-fetch-1"
  assert operation == profile.TaskSourceFetchCandidates

  let assert types.DriverResponseSuccess(
    request_id: success_request_id,
    result: types.TaskListResult(tasks: tasks),
    ..,
  ) = success_response
  let assert types.DriverResponseSuccess(request_id: stale_request_id, ..) =
    stale_response
  let assert [task, ..] = tasks

  assert success_request_id == "req-fetch-1"
  assert stale_request_id == "stale-request"
  assert task.title == "Fake card"
}

pub fn adapter_author_example_manifests_validate_expected_paths_test() {
  let manifest_paths = [
    example_dir <> "/manifest.pass.json",
    example_dir <> "/manifest.invalid-shape.json",
    example_dir <> "/manifest.missing-capability.json",
    example_dir <> "/manifest.namespace-misuse.json",
    example_dir <> "/manifest.malformed-response.json",
    example_dir <> "/manifest.stale-response.json",
    example_dir <> "/manifest.redaction.json",
  ]

  assert list_all(manifest_paths, manifest_uses_repo_fixture)

  let assert Ok(invalid_shape_body) =
    simplifile.read(example_dir <> "/manifest.invalid-shape.json")
  let assert Error(types.ManifestError(code: invalid_shape_code, ..)) =
    conformance.decode_manifest(invalid_shape_body)

  assert invalid_shape_code == "invalid_manifest_json"
}

pub fn adapter_author_examples_produce_documented_results_test() {
  reset_report_dir()

  let pass_report_path =
    "test/tmp/tracker-conformance/adapter-author-pass.report.json"
  let malformed_report_path =
    "test/tmp/tracker-conformance/adapter-author-malformed.report.json"
  let stale_report_path =
    "test/tmp/tracker-conformance/adapter-author-stale.report.json"
  let redaction_report_path =
    "test/tmp/tracker-conformance/adapter-author-redaction.report.json"

  let assert Ok(types.RunResult(
    report: pass_report,
    summary: pass_summary,
    exit_code: pass_exit_code,
  )) =
    runner.run_manifest_path(
      example_dir <> "/manifest.pass.json",
      pass_report_path,
    )
  let assert Ok(types.RunResult(
    report: malformed_report,
    exit_code: malformed_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      example_dir <> "/manifest.malformed-response.json",
      malformed_report_path,
    )
  let assert Ok(types.RunResult(
    report: stale_report,
    exit_code: stale_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      example_dir <> "/manifest.stale-response.json",
      stale_report_path,
    )
  let assert Ok(types.RunResult(
    summary: redaction_summary,
    exit_code: redaction_exit_code,
    ..,
  )) =
    runner.run_manifest_path(
      example_dir <> "/manifest.redaction.json",
      redaction_report_path,
    )
  let assert Ok(pass_report_body) = simplifile.read(pass_report_path)
  let assert Ok(malformed_report_body) = simplifile.read(malformed_report_path)
  let assert Ok(stale_report_body) = simplifile.read(stale_report_path)
  let assert Ok(redaction_report_body) = simplifile.read(redaction_report_path)

  let types.Report(
    failed: pass_failed,
    setup_failed: pass_setup_failed,
    probe_failed: pass_probe_failed,
    cleanup_failed: pass_cleanup_failed,
    ..,
  ) = pass_report
  let types.Report(failed: malformed_failed, ..) = malformed_report
  let types.Report(failed: stale_failed, ..) = stale_report

  assert pass_exit_code == 0
  assert pass_failed == 0
  assert pass_setup_failed == 0
  assert pass_probe_failed == 0
  assert pass_cleanup_failed == 0
  assert string.contains(pass_summary, "tracker-conformance")
  assert !string.contains(pass_report_body, "SECRET_TOKEN")

  assert malformed_exit_code == 1
  assert malformed_failed > 0
  assert string.contains(
    malformed_report_body,
    "driver stdout was not valid conformance JSON",
  )

  assert stale_exit_code == 1
  assert stale_failed > 0
  assert string.contains(
    stale_report_body,
    "driver response envelope did not match request schema_version or request_id",
  )

  assert redaction_exit_code == 0
  assert !string.contains(redaction_summary, "SECRET_TOKEN")
  assert !string.contains(redaction_report_body, "SECRET_TOKEN")
  assert string.contains(redaction_report_body, "[REDACTED]")
}

pub fn adapter_author_invalid_examples_fail_with_documented_manifest_errors_test() {
  reset_report_dir()

  let assert Error(runner.RunError(
    code: missing_capability_code,
    message: missing_capability_message,
  )) =
    runner.run_manifest_path(
      example_dir <> "/manifest.missing-capability.json",
      "test/tmp/tracker-conformance/adapter-author-missing-capability.report.json",
    )
  let assert Error(runner.RunError(
    code: namespace_code,
    message: namespace_message,
  )) =
    runner.run_manifest_path(
      example_dir <> "/manifest.namespace-misuse.json",
      "test/tmp/tracker-conformance/adapter-author-namespace.report.json",
    )

  assert missing_capability_code == "missing_requested_pack_capability"
  assert missing_capability_message
    == "profile.requested_packs includes comments but profile.capabilities is missing comments.create"
  assert namespace_code == "fixture_operation_disallowed"
  assert namespace_message
    == "profile.adapter_operations must not include fixture/probe/hook operations: fixture.setup"
}

fn manifest_uses_repo_fixture(path: String) -> Bool {
  let assert Ok(contents) = simplifile.read(path)

  string.contains(
    contents,
    "\"task_file\": \"test/fixtures/tracker_conformance/",
  )
}

fn list_all(values: List(a), predicate: fn(a) -> Bool) -> Bool {
  case values {
    [] -> True
    [value, ..rest] -> predicate(value) && list_all(rest, predicate)
  }
}

fn reset_report_dir() -> Nil {
  let _ = simplifile.delete("test/tmp/tracker-conformance")
  let assert Ok(Nil) =
    simplifile.create_directory_all("test/tmp/tracker-conformance")
  Nil
}
