import gleam/int
import scherzo/port
import scherzo/tracker/conformance/process_capture
import scherzo/tracker/conformance/types

const probe_recovery_guidance = "Fix backend visibility or readiness checks before treating the adapter result as trustworthy."

pub fn run(manifest: types.Manifest) -> List(types.ProbeResult) {
  let types.Manifest(probes: probes, ..) = manifest
  run_probes(probes)
}

fn run_probes(probes: List(types.ProbeConfig)) -> List(types.ProbeResult) {
  case probes {
    [] -> []
    [probe, ..rest] -> [run_probe(probe), ..run_probes(rest)]
  }
}

fn run_probe(probe: types.ProbeConfig) -> types.ProbeResult {
  let types.ProbeConfig(name: name, command: command) = probe
  let types.HookCommand(executable: executable, args: args, cwd: cwd) = command
  case port.start_argv(executable, args, cwd, []) {
    Error(error) ->
      types.ProbeResult(
        name: name,
        status: types.ProbeFailedStatus,
        message: "probe spawn failed: " <> port.port_error_to_string(error),
        diagnostics: "",
        recovery_guidance: probe_recovery_guidance,
      )
    Ok(process) ->
      case port.await_exit(process, 1000) {
        Ok(0) ->
          types.ProbeResult(
            name: name,
            status: types.PassedStatus,
            message: "probe passed",
            diagnostics: process_capture.truncate_diagnostics(
              diagnostics_or_empty(process),
            ),
            recovery_guidance: probe_recovery_guidance,
          )
        Ok(status) ->
          types.ProbeResult(
            name: name,
            status: types.ProbeFailedStatus,
            message: "probe exited with status " <> int.to_string(status),
            diagnostics: process_capture.truncate_diagnostics(
              diagnostics_or_empty(process),
            ),
            recovery_guidance: probe_recovery_guidance,
          )
        Error(error) -> {
          let diagnostics =
            diagnostics_or_empty(process) <> terminate_note(process)
          types.ProbeResult(
            name: name,
            status: types.ProbeFailedStatus,
            message: "probe failed: " <> port.port_error_to_string(error),
            diagnostics: process_capture.truncate_diagnostics(diagnostics),
            recovery_guidance: probe_recovery_guidance,
          )
        }
      }
  }
}

fn diagnostics_or_empty(process: port.Process) -> String {
  case port.read_diagnostics(process) {
    Ok(diagnostics) -> diagnostics
    Error(error) ->
      "diagnostics_unavailable: " <> port.port_error_to_string(error)
  }
}

fn terminate_note(process: port.Process) -> String {
  case port.terminate(process) {
    Ok(Nil) -> ""
    Error(error) -> " terminate_failed: " <> port.port_error_to_string(error)
  }
}
