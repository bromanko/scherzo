import gleam/string
import scherzo/tracker/conformance/cli
import scherzo/tracker/conformance/types
import simplifile

pub fn tracker_conformance_cli_parses_args_runs_and_reports_usage_errors_test() {
  reset_report_dir()

  assert cli.parse_args([
      "test/fixtures/tracker_conformance/task-source-pass.manifest.json",
      "--report",
      "test/tmp/tracker-conformance/cli.report.json",
    ])
    == Ok(cli.Run(
      manifest_path: "test/fixtures/tracker_conformance/task-source-pass.manifest.json",
      report_path: "test/tmp/tracker-conformance/cli.report.json",
    ))
  assert cli.parse_args(["manifest.json"]) == Error(cli.UsageError)
  assert cli.run(["missing.json", "--report", "out.json"])
    == Error(cli.RunError(
      code: "manifest_read_failed",
      message: "could not read manifest: missing.json",
    ))

  let assert Ok(types.RunResult(summary: summary, exit_code: exit_code, ..)) =
    cli.run([
      "test/fixtures/tracker_conformance/task-source-pass.manifest.json",
      "--report",
      "test/tmp/tracker-conformance/cli.report.json",
    ])
  let assert Ok(report_body) =
    simplifile.read("test/tmp/tracker-conformance/cli.report.json")

  assert exit_code == 0
  assert string.contains(summary, "tracker-conformance")
  assert string.contains(report_body, "task_source.fetch.backend_kind")
}

fn reset_report_dir() -> Nil {
  let _ = simplifile.delete("test/tmp/tracker-conformance")
  let assert Ok(Nil) =
    simplifile.create_directory_all("test/tmp/tracker-conformance")
  Nil
}
