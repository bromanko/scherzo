import gleam/string
import scherzo/domain
import scherzo/port
import scherzo/step_artifact

pub fn run(
  step_id: String,
  command: String,
  workspace_path: String,
  timeout_ms: Int,
  secrets: List(String),
  limits: domain.ArtifactLimits,
) -> step_artifact.StepArtifact {
  case port.start(command, workspace_path) {
    Error(err) ->
      step_artifact.from_command_result(
        step_id,
        127,
        "",
        port_error_to_string(err),
        False,
        secrets,
        limits,
      )
    Ok(process) ->
      read_loop(step_id, process, timeout_ms, secrets, limits, "", False)
  }
}

fn read_loop(
  step_id: String,
  process: port.Process,
  timeout_ms: Int,
  secrets: List(String),
  limits: domain.ArtifactLimits,
  stdout: String,
  stdout_truncated: Bool,
) -> step_artifact.StepArtifact {
  case port.read_stdout_line(process, timeout_ms) {
    Ok(line) -> {
      let #(stdout, stdout_truncated) =
        append_capped(
          stdout,
          stdout_truncated,
          line <> "\n",
          limits.command_stream_max_chars,
        )
      read_loop(
        step_id,
        process,
        timeout_ms,
        secrets,
        limits,
        stdout,
        stdout_truncated,
      )
    }
    Error(port.ProcessExited(status)) -> {
      let stderr = port.read_diagnostics(process) |> result_unwrap("")
      step_artifact.from_command_result_with_truncation(
        step_id,
        status,
        stdout,
        stderr,
        False,
        secrets,
        limits,
        stdout_truncated,
        False,
      )
    }
    Error(port.PortClosed) -> {
      let stderr = port.read_diagnostics(process) |> result_unwrap("")
      step_artifact.from_command_result_with_truncation(
        step_id,
        1,
        stdout,
        stderr,
        False,
        secrets,
        limits,
        stdout_truncated,
        False,
      )
    }
    Error(port.ReadTimeout) -> {
      let _ = port.terminate(process)
      let stderr = port.read_diagnostics(process) |> result_unwrap("")
      step_artifact.from_command_result_with_truncation(
        step_id,
        124,
        stdout,
        stderr,
        True,
        secrets,
        limits,
        stdout_truncated,
        False,
      )
    }
    Error(err) -> {
      let stderr = port.read_diagnostics(process) |> result_unwrap("")
      step_artifact.from_command_result_with_truncation(
        step_id,
        1,
        stdout,
        stderr <> port_error_to_string(err),
        False,
        secrets,
        limits,
        stdout_truncated,
        False,
      )
    }
  }
}

fn append_capped(
  current: String,
  already_truncated: Bool,
  chunk: String,
  max_chars: Int,
) -> #(String, Bool) {
  case already_truncated {
    True -> #(current, True)
    False -> {
      let remaining = max_chars - string.length(current)
      case remaining <= 0 {
        True -> #(current, True)
        False -> {
          case string.length(chunk) > remaining {
            True -> #(current <> string.slice(chunk, 0, remaining), True)
            False -> #(current <> chunk, False)
          }
        }
      }
    }
  }
}

fn port_error_to_string(err: port.PortError) -> String {
  case err {
    port.StartFailed(message) -> message
    port.SendFailed(message) -> message
    port.ReadTimeout -> "read timeout"
    port.LineTooLong -> "line too long"
    port.ProcessExited(status) -> "process exited " <> int_to_string(status)
    port.PortClosed -> "port closed"
    port.DiagnosticsFailed(message) -> message
    port.TerminateFailed(message) -> message
    port.AwaitTimeout -> "await timeout"
    port.AwaitFailed(message) -> message
  }
}

fn result_unwrap(result: Result(a, b), default: a) -> a {
  case result {
    Ok(value) -> value
    Error(_) -> default
  }
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
