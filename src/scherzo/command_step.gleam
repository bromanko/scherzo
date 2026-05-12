import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/log
import scherzo/path
import scherzo/port
import scherzo/step_artifact
import simplifile

type DiagnosticsCapture {
  DiagnosticsCapture(stdout_path: String, artifact_path: String)
}

pub fn run(
  step_id: String,
  command: String,
  workspace_path: String,
  timeout_ms: Int,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
) -> step_artifact.StepArtifact {
  run_with_env(
    step_id,
    command,
    workspace_path,
    timeout_ms,
    [],
    secrets,
    limits,
  )
}

pub fn run_with_env(
  step_id: String,
  command: String,
  workspace_path: String,
  timeout_ms: Int,
  env: List(#(String, String)),
  secrets: List(String),
  limits: config_types.ArtifactLimits,
) -> step_artifact.StepArtifact {
  let started_ms = monotonic_ms()
  let diagnostics = prepare_diagnostics(workspace_path, step_id)
  let command_for_child = command_with_shell_path_override(command, env)
  let command_to_run = case diagnostics {
    Some(DiagnosticsCapture(stdout_path: stdout_path, ..)) ->
      command_with_stdout_capture(command_for_child, stdout_path)
    None -> command_for_child
  }
  case port.start_with_env(command_to_run, workspace_path, env) {
    Error(err) -> {
      let stderr = port_error_to_string(err)
      finish_command(
        step_id,
        command,
        started_ms,
        127,
        "",
        stderr,
        False,
        secrets,
        limits,
        False,
        False,
        diagnostics,
      )
    }
    Ok(process) ->
      read_loop(
        step_id,
        command,
        process,
        timeout_ms,
        started_ms,
        secrets,
        limits,
        "",
        False,
        diagnostics,
      )
  }
}

fn read_loop(
  step_id: String,
  command: String,
  process: port.Process,
  timeout_ms: Int,
  started_ms: Int,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
  stdout: String,
  stdout_truncated: Bool,
  diagnostics: Option(DiagnosticsCapture),
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
        command,
        process,
        timeout_ms,
        started_ms,
        secrets,
        limits,
        stdout,
        stdout_truncated,
        diagnostics,
      )
    }
    Error(port.ProcessExited(status)) -> {
      let stderr = read_diagnostics_or_error(process)
      let _cleanup_result = port.terminate(process)
      finish_command(
        step_id,
        command,
        started_ms,
        status,
        stdout,
        stderr,
        False,
        secrets,
        limits,
        stdout_truncated,
        False,
        diagnostics,
      )
    }
    Error(port.Closed) -> {
      let stderr = read_diagnostics_or_error(process)
      let _cleanup_result = port.terminate(process)
      finish_command(
        step_id,
        command,
        started_ms,
        1,
        stdout,
        stderr,
        False,
        secrets,
        limits,
        stdout_truncated,
        False,
        diagnostics,
      )
    }
    Error(port.ReadTimeout) -> {
      let _ = port.terminate(process)
      let stderr = read_diagnostics_or_error(process)
      finish_command(
        step_id,
        command,
        started_ms,
        124,
        stdout,
        stderr,
        True,
        secrets,
        limits,
        stdout_truncated,
        False,
        diagnostics,
      )
    }
    Error(err) -> {
      let stderr = read_diagnostics_or_error(process)
      let _cleanup_result = port.terminate(process)
      finish_command(
        step_id,
        command,
        started_ms,
        1,
        stdout,
        stderr <> port_error_to_string(err),
        False,
        secrets,
        limits,
        stdout_truncated,
        False,
        diagnostics,
      )
    }
  }
}

fn read_diagnostics_or_error(process: port.Process) -> String {
  case port.read_diagnostics(process) {
    Ok(stderr) -> stderr
    Error(err) ->
      "[scherzo could not read command diagnostics: "
      <> port_error_to_string(err)
      <> "]\n"
  }
}

fn prepare_diagnostics(
  workspace_path: String,
  step_id: String,
) -> Option(DiagnosticsCapture) {
  let relative_dir =
    path.join(path.join(workspace_path, ".scherzo"), "command-step-diagnostics")
  let dir = path.absolute(relative_dir) |> result.unwrap(relative_dir)
  case simplifile.create_directory_all(dir) {
    Error(_) -> None
    Ok(Nil) -> {
      let stdout_path = path.join(dir, step_id <> ".stdout.raw")
      let artifact_path = path.join(dir, step_id <> ".txt")
      case simplifile.write(stdout_path, "") {
        Ok(Nil) -> Some(DiagnosticsCapture(stdout_path, artifact_path))
        Error(_) -> None
      }
    }
  }
}

// The port shell uses a login Bash, which may rewrite PATH after the
// process environment is applied. Re-export only PATH inside the user script so
// profile-local PATH remains a literal command-step override without copying
// every env value into the shell argv.
fn command_with_shell_path_override(
  command: String,
  env: List(#(String, String)),
) -> String {
  case env_value(env, "PATH") {
    Some(path) -> "export PATH=" <> shell_quote(path) <> "\n" <> command
    None -> command
  }
}

fn env_value(env: List(#(String, String)), key: String) -> Option(String) {
  case env {
    [] -> None
    [#(current, value), ..rest] ->
      case current == key {
        True -> Some(value)
        False -> env_value(rest, key)
      }
  }
}

fn command_with_stdout_capture(command: String, stdout_path: String) -> String {
  "(\n"
  <> command
  <> "\n) > >(command -p tee "
  <> shell_quote(stdout_path)
  <> ")"
}

fn finish_command(
  step_id: String,
  command: String,
  started_ms: Int,
  exit_code: Int,
  stdout: String,
  stderr: String,
  timed_out: Bool,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
  stdout_truncated: Bool,
  stderr_truncated: Bool,
  diagnostics: Option(DiagnosticsCapture),
) -> step_artifact.StepArtifact {
  let duration_ms = max_int(0, monotonic_ms() - started_ms)
  let full_stdout = captured_stdout(diagnostics, stdout)
  let stdout_truncated =
    stream_will_be_truncated(full_stdout, stdout_truncated, secrets, limits)
  let stderr_truncated =
    stream_will_be_truncated(stderr, stderr_truncated, secrets, limits)
  let diagnostic_path = case
    step_artifact.status_from_exit(exit_code, timed_out: timed_out)
  {
    step_artifact.StepSucceeded -> {
      cleanup_stdout_capture(diagnostics)
      None
    }
    step_artifact.StepFailed ->
      write_diagnostic_artifact(
        diagnostics,
        step_id,
        command,
        exit_code,
        duration_ms,
        timed_out,
        full_stdout,
        stderr,
        stdout_truncated,
        stderr_truncated,
        secrets,
      )
  }
  step_artifact.from_command_result_with_metadata(
    step_id,
    Some(command),
    exit_code,
    Some(duration_ms),
    diagnostic_path,
    full_stdout,
    stderr,
    timed_out,
    secrets,
    limits,
    stdout_truncated,
    stderr_truncated,
  )
}

fn stream_will_be_truncated(
  value: String,
  already_truncated: Bool,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
) -> Bool {
  already_truncated
  || string.length(log.redact("command_step_artifact", value, secrets))
  > limits.command_stream_max_chars
}

fn captured_stdout(
  diagnostics: Option(DiagnosticsCapture),
  fallback: String,
) -> String {
  case diagnostics {
    None -> fallback
    Some(DiagnosticsCapture(stdout_path: stdout_path, ..)) ->
      simplifile.read(stdout_path) |> result.unwrap(fallback)
  }
}

fn cleanup_stdout_capture(diagnostics: Option(DiagnosticsCapture)) -> Nil {
  case diagnostics {
    None -> Nil
    Some(DiagnosticsCapture(stdout_path: stdout_path, ..)) -> {
      let _ = simplifile.delete(stdout_path)
      Nil
    }
  }
}

fn write_diagnostic_artifact(
  diagnostics: Option(DiagnosticsCapture),
  step_id: String,
  command: String,
  exit_code: Int,
  duration_ms: Int,
  timed_out: Bool,
  stdout: String,
  stderr: String,
  stdout_truncated: Bool,
  stderr_truncated: Bool,
  secrets: List(String),
) -> Option(String) {
  case diagnostics {
    None -> None
    Some(DiagnosticsCapture(stdout_path: stdout_path, artifact_path: path)) -> {
      let body =
        diagnostic_body(
          step_id,
          command,
          exit_code,
          duration_ms,
          timed_out,
          stdout,
          stderr,
          stdout_truncated,
          stderr_truncated,
          secrets,
        )
      let result = simplifile.write(path, body)
      let _ = simplifile.delete(stdout_path)
      case result {
        Ok(Nil) -> Some(path)
        Error(_) -> None
      }
    }
  }
}

fn diagnostic_body(
  step_id: String,
  command: String,
  exit_code: Int,
  duration_ms: Int,
  timed_out: Bool,
  stdout: String,
  stderr: String,
  stdout_truncated: Bool,
  stderr_truncated: Bool,
  secrets: List(String),
) -> String {
  let failure_code = case
    step_artifact.failure_code_from_streams(stdout, stderr)
  {
    Some(code) -> "\nfailure_code: " <> code
    None -> ""
  }
  "Scherzo command-step diagnostics\n"
  <> "step_id: "
  <> step_id
  <> failure_code
  <> "\ncommand: "
  <> log.redact("command_step_artifact", command, secrets)
  <> "\nexit_code: "
  <> int.to_string(exit_code)
  <> "\nduration_ms: "
  <> int.to_string(duration_ms)
  <> "\ntimed_out: "
  <> bool_to_string(timed_out)
  <> "\nstdout_truncated_in_report: "
  <> bool_to_string(stdout_truncated)
  <> "\nstderr_truncated_in_report: "
  <> bool_to_string(stderr_truncated)
  <> "\n\nstdout:\n"
  <> log.redact("command_step_artifact", stdout, secrets)
  <> "\n\nstderr:\n"
  <> log.redact("command_step_artifact", stderr, secrets)
}

fn shell_quote(value: String) -> String {
  "'" <> string.replace(value, each: "'", with: "'\\''") <> "'"
}

fn bool_to_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn max_int(left: Int, right: Int) -> Int {
  case left > right {
    True -> left
    False -> right
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
  port.port_error_to_string(err)
}

@external(erlang, "scherzo_time_ffi", "monotonic_ms")
fn monotonic_ms() -> Int
