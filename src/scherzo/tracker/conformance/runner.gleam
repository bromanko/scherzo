import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/task
import scherzo/tracker/conformance
import scherzo/tracker/conformance/fixtures
import scherzo/tracker/conformance/probes
import scherzo/tracker/conformance/profile
import scherzo/tracker/conformance/report
import scherzo/tracker/conformance/task_source_pack
import scherzo/tracker/conformance/types
import simplifile

pub type RunError {
  RunError(code: String, message: String)
}

pub fn run_manifest_path(
  manifest_path: String,
  report_path: String,
) -> Result(types.RunResult, RunError) {
  use manifest <- result.try(load_manifest(manifest_path))
  run_manifest(manifest, report_path)
}

pub fn run_manifest(
  manifest: types.Manifest,
  report_path: String,
) -> Result(types.RunResult, RunError) {
  use fixture_tasks <- result.try(load_fixture_tasks(manifest))
  let setup_result = fixtures.run_setup(manifest)
  let case_results = case setup_result {
    Some(types.HookResult(status: types.SetupFailedStatus, ..)) -> []
    _ -> run_profile(manifest, fixture_tasks)
  }
  let probe_results = probes.run(manifest)
  let cleanup_result = fixtures.run_cleanup(manifest)
  let hook_results = collect_hook_results(setup_result, cleanup_result)
  let report_value =
    report.build(manifest, case_results, hook_results, probe_results)
  let types.Manifest(report: report_config, ..) = manifest
  let types.ReportConfig(redact: redact) = report_config
  let summary = report.summary(report_value, redact: redact)

  use Nil <- result.try(write_report(report_path, report_value, redact))

  Ok(types.RunResult(
    report: report_value,
    summary: summary,
    exit_code: report.exit_code(report_value),
  ))
}

fn load_manifest(manifest_path: String) -> Result(types.Manifest, RunError) {
  use contents <- result.try(case simplifile.read(manifest_path) {
    Ok(contents) -> Ok(contents)
    Error(_) ->
      Error(RunError(
        code: "manifest_read_failed",
        message: "could not read manifest: " <> manifest_path,
      ))
  })
  case conformance.decode_manifest(contents) {
    Ok(manifest) -> Ok(manifest)
    Error(types.ManifestError(code: code, message: message)) ->
      Error(RunError(code: code, message: message))
  }
}

fn load_fixture_tasks(
  manifest: types.Manifest,
) -> Result(List(task.Task), RunError) {
  case fixtures.load_tasks(manifest) {
    Ok(tasks) -> Ok(tasks)
    Error(fixtures.FixtureError(code: code, message: message)) ->
      Error(RunError(code: code, message: message))
  }
}

fn run_profile(
  manifest: types.Manifest,
  fixture_tasks: List(task.Task),
) -> List(types.CaseResult) {
  let types.Manifest(profile: manifest_profile, ..) = manifest
  let types.ProfileConfig(name: name, ..) = manifest_profile
  case name {
    profile.TaskSourceProfile -> task_source_pack.run(manifest, fixture_tasks)
  }
}

fn collect_hook_results(
  setup_result: Option(types.HookResult),
  cleanup_result: Option(types.HookResult),
) -> List(types.HookResult) {
  case setup_result, cleanup_result {
    None, None -> []
    Some(setup), None -> [setup]
    None, Some(cleanup) -> [cleanup]
    Some(setup), Some(cleanup) -> [setup, cleanup]
  }
}

fn write_report(
  report_path: String,
  report_value: types.Report,
  redact: List(String),
) -> Result(Nil, RunError) {
  use Nil <- result.try(case fixtures.ensure_report_directory(report_path) {
    Ok(Nil) -> Ok(Nil)
    Error(fixtures.FixtureError(code: code, message: message)) ->
      Error(RunError(code: code, message: message))
  })
  case
    simplifile.write(
      report_path,
      report.to_string(report_value, redact: redact),
    )
  {
    Ok(Nil) -> Ok(Nil)
    Error(_) ->
      Error(RunError(
        code: "report_write_failed",
        message: "could not write report: " <> report_path,
      ))
  }
}
