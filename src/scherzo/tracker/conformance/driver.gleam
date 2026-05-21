import gleam/int
import gleam/option.{type Option, None, Some}
import scherzo/port
import scherzo/tracker/conformance
import scherzo/tracker/conformance/process_capture
import scherzo/tracker/conformance/types

pub type DriverFailureKind {
  SpawnFailed
  TimeoutFailed
  MissingStdoutFailed
  ExitStatusFailed
  MalformedResponseFailed
}

pub type DriverFailure {
  DriverFailure(
    kind: DriverFailureKind,
    message: String,
    diagnostics: String,
    stdout: Option(String),
    exit_status: Option(Int),
  )
}

pub type DriverInvocation {
  DriverInvocation(
    response: types.DriverResponse,
    stdout: String,
    diagnostics: String,
  )
}

pub fn invoke(
  manifest: types.Manifest,
  request: types.DriverRequest,
) -> Result(DriverInvocation, DriverFailure) {
  let types.Manifest(driver: driver, ..) = manifest
  case driver {
    types.CliDriverConfig(command: command, timeout_ms: timeout_ms) -> {
      let types.DriverCommand(
        executable: executable,
        args: args,
        cwd: cwd,
        env: env,
      ) = command
      let stdin = conformance.request_to_string(request) <> "\n"

      case
        port.start_argv_with_input(executable, args, cwd, env_pairs(env), stdin)
      {
        Error(error) ->
          Error(DriverFailure(
            kind: SpawnFailed,
            message: "driver spawn failed: " <> port.port_error_to_string(error),
            diagnostics: "",
            stdout: None,
            exit_status: None,
          ))
        Ok(process) -> read_driver_stdout(process, timeout_ms, request)
      }
    }
    types.HttpDriverConfig(..) ->
      Error(DriverFailure(
        kind: SpawnFailed,
        message: "http driver transport is not implemented yet",
        diagnostics: "",
        stdout: None,
        exit_status: None,
      ))
  }
}

fn read_driver_stdout(
  process: port.Process,
  timeout_ms: Int,
  request: types.DriverRequest,
) -> Result(DriverInvocation, DriverFailure) {
  case port.read_stdout_line(process, timeout_ms) {
    Ok(stdout) -> finish_invocation(process, timeout_ms, request, stdout)
    Error(port.ReadTimeout) -> {
      let diagnostics = diagnostics_or_empty(process) <> terminate_note(process)
      Error(DriverFailure(
        kind: TimeoutFailed,
        message: "driver timed out waiting for stdout",
        diagnostics: process_capture.truncate_diagnostics(diagnostics),
        stdout: None,
        exit_status: None,
      ))
    }
    Error(port.ProcessExited(status)) -> classify_early_exit(process, status)
    Error(error) -> {
      let diagnostics = diagnostics_or_empty(process) <> terminate_note(process)
      Error(DriverFailure(
        kind: MissingStdoutFailed,
        message: "driver did not produce a response line: "
          <> port.port_error_to_string(error),
        diagnostics: process_capture.truncate_diagnostics(diagnostics),
        stdout: None,
        exit_status: None,
      ))
    }
  }
}

fn finish_invocation(
  process: port.Process,
  timeout_ms: Int,
  request: types.DriverRequest,
  stdout: String,
) -> Result(DriverInvocation, DriverFailure) {
  let diagnostics = diagnostics_or_empty(process)
  case conformance.decode_response(stdout) {
    Error(_) -> {
      let diagnostics = diagnostics <> terminate_note(process)
      Error(DriverFailure(
        kind: MalformedResponseFailed,
        message: "driver stdout was not valid conformance JSON",
        diagnostics: process_capture.truncate_diagnostics(diagnostics),
        stdout: Some(stdout),
        exit_status: None,
      ))
    }
    Ok(response) ->
      case port.await_exit(process, timeout_ms) {
        Ok(0) -> accept_response(request, response, stdout, diagnostics)
        Ok(status) ->
          Error(DriverFailure(
            kind: ExitStatusFailed,
            message: "driver exited with status " <> int.to_string(status),
            diagnostics: process_capture.truncate_diagnostics(diagnostics),
            stdout: Some(stdout),
            exit_status: Some(status),
          ))
        Error(port.ReadTimeout) -> {
          Error(DriverFailure(
            kind: TimeoutFailed,
            message: "driver timed out waiting for exit",
            diagnostics: process_capture.truncate_diagnostics(
              diagnostics <> terminate_note(process),
            ),
            stdout: Some(stdout),
            exit_status: None,
          ))
        }
        Error(error) -> {
          Error(DriverFailure(
            kind: ExitStatusFailed,
            message: "driver exit wait failed: "
              <> port.port_error_to_string(error),
            diagnostics: process_capture.truncate_diagnostics(
              diagnostics <> terminate_note(process),
            ),
            stdout: Some(stdout),
            exit_status: None,
          ))
        }
      }
  }
}

fn accept_response(
  request: types.DriverRequest,
  response: types.DriverResponse,
  stdout: String,
  diagnostics: String,
) -> Result(DriverInvocation, DriverFailure) {
  case response_matches_request(response, request) {
    True ->
      Ok(DriverInvocation(
        response: response,
        stdout: stdout,
        diagnostics: process_capture.truncate_diagnostics(diagnostics),
      ))
    False ->
      Error(DriverFailure(
        kind: MalformedResponseFailed,
        message: "driver response envelope did not match request schema_version or request_id",
        diagnostics: process_capture.truncate_diagnostics(diagnostics),
        stdout: Some(stdout),
        exit_status: None,
      ))
  }
}

fn response_matches_request(
  response: types.DriverResponse,
  request: types.DriverRequest,
) -> Bool {
  let types.DriverRequest(request_id: request_id, ..) = request
  response_schema_version(response) == types.schema_version
  && response_request_id(response) == request_id
}

fn response_schema_version(response: types.DriverResponse) -> Int {
  case response {
    types.DriverResponseSuccess(schema_version: schema_version, ..) ->
      schema_version
    types.DriverResponseError(schema_version: schema_version, ..) ->
      schema_version
  }
}

fn response_request_id(response: types.DriverResponse) -> String {
  case response {
    types.DriverResponseSuccess(request_id: request_id, ..) -> request_id
    types.DriverResponseError(request_id: request_id, ..) -> request_id
  }
}

fn classify_early_exit(
  process: port.Process,
  status: Int,
) -> Result(DriverInvocation, DriverFailure) {
  let diagnostics = diagnostics_or_empty(process)
  let kind = case status == 0 {
    True -> MissingStdoutFailed
    False -> ExitStatusFailed
  }
  let message = case status == 0 {
    True -> "driver exited before producing a response line"
    False -> "driver exited with status " <> int.to_string(status)
  }
  Error(DriverFailure(
    kind: kind,
    message: message,
    diagnostics: process_capture.truncate_diagnostics(diagnostics),
    stdout: None,
    exit_status: Some(status),
  ))
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

fn env_pairs(env: List(types.EnvVar)) -> List(#(String, String)) {
  case env {
    [] -> []
    [types.EnvVar(name: name, value: value), ..rest] -> [
      #(name, value),
      ..env_pairs(rest)
    ]
  }
}
