import gleam/string
import scherzo/tracker/conformance/runner
import simplifile

pub fn linear_offline_manifest_writes_redacted_report_test() {
  let report_path =
    "test/tmp/tracker-conformance/linear-task-source-offline.report.json"
  let assert Ok(result) =
    runner.run_manifest_path(
      "test/fixtures/tracker_conformance/linear-task-source-offline.manifest.json",
      report_path,
    )
  let assert Ok(report) = simplifile.read(report_path)

  assert string.contains(result.summary, "tracker-conformance adapter=linear")
  assert string.contains(result.summary, "failed=0")
  assert string.contains(result.summary, "probe_failed=0")
  assert string.contains(result.summary, "cleanup_failed=0")
  assert !string.contains(result.summary, "fake-linear-token")
  assert !string.contains(result.summary, "linear-fixture-secret")
  assert !string.contains(report, "fake-linear-token")
  assert !string.contains(report, "linear-fixture-secret")
  assert string.contains(report, "[REDACTED]")
}

pub fn conformance_runner_keeps_linear_adapter_imports_outside_the_driver_test() {
  let assert Ok(runner_source) =
    simplifile.read("src/scherzo/tracker/conformance/runner.gleam")
  let assert Ok(task_source_pack) =
    simplifile.read("src/scherzo/tracker/conformance/task_source_pack.gleam")
  let assert Ok(comments_pack) =
    simplifile.read("src/scherzo/tracker/conformance/comments_pack.gleam")
  let assert Ok(driver_source) =
    simplifile.read("src/scherzo_linear_conformance_live_driver.gleam")

  assert !string.contains(runner_source, "linear_adapter")
  assert !string.contains(task_source_pack, "linear_adapter")
  assert !string.contains(comments_pack, "linear_adapter")
  assert string.contains(driver_source, "linear_adapter")
}
