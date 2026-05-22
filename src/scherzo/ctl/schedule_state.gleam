import gleam/option.{type Option, None, Some}
import scherzo/runtime_bundle
import scherzo/schedule_doctor
import scherzo/state/ledger
import scherzo/state/projection

pub fn load_projection(
  root: String,
  fail: fn(String, String) -> error,
) -> Result(projection.Projection, error) {
  case ledger.path_for_workspace_root(root) {
    Error(_) ->
      Error(fail("ledger_path_failed", "could not resolve ledger path"))
    Ok(ledger_path) ->
      case ledger.load_projection(ledger_path) {
        Ok(projected) -> Ok(projected)
        Error(_) ->
          Error(fail("ledger_load_failed", "could not load local ledger"))
      }
  }
}

pub fn config_diagnostics(
  config_path: Option(String),
  job_id: String,
) -> List(schedule_doctor.Diagnostic) {
  case config_path {
    None -> [
      schedule_doctor.Diagnostic(
        name: "config_load",
        severity: schedule_doctor.Fail,
        code: "schedule_config_missing",
        message: "could not find scherzo.yaml for schedule doctor; run from the config directory or pass --root pointing at a directory that contains scherzo.yaml",
        fields: [#("job_id", job_id)],
      ),
    ]
    Some(path) ->
      case runtime_bundle.load(Some(path)) {
        Error(runtime_bundle.BundleError(code, message)) -> [
          schedule_doctor.Diagnostic(
            name: "config_load",
            severity: schedule_doctor.Fail,
            code: code,
            message: message,
            fields: [#("job_id", job_id), #("config_path", path)],
          ),
        ]
        Ok(bundle) -> {
          let schedule_doctor.Report(_, diagnostics) =
            schedule_doctor.inspect_bundle(bundle, Some(job_id))
          [
            schedule_doctor.Diagnostic(
              name: "config_load",
              severity: schedule_doctor.Pass,
              code: "ok",
              message: "scherzo.yaml and routed workflow DAGs loaded successfully",
              fields: [#("config_path", path), #("job_id", job_id)],
            ),
            ..diagnostics
          ]
        }
      }
  }
}
