import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/string
import scherzo/log
import scherzo/path
import scherzo/port
import scherzo/step_artifact
import scherzo/structured_output_validator
import scherzo/workflow_dag

const stream_limit = 8192

const summary_limit = 1000

pub fn run_command_validator(
  declaration: workflow_dag.StructuredOutputValidator,
  payload_json: String,
  context: structured_output_validator.ValidatorContext,
  secrets: List(String),
) -> Result(
  structured_output_validator.ValidatorPass,
  structured_output_validator.ValidatorFailure,
) {
  case declaration {
    workflow_dag.CommandValidator(
      argv: argv,
      timeout_ms: timeout_ms,
      working_directory: working_directory,
      env: declared_env,
      ..,
    ) -> {
      use Nil <- result.try(validate_declared_env(
        context,
        declared_env,
        secrets,
      ))
      use executable_and_args <- result.try(resolve_executable_and_args(
        context,
        argv,
        secrets,
      ))
      let #(executable, args) = executable_and_args
      let cwd = working_directory_path(context, working_directory)
      let env = validator_env(context, declared_env)
      run_process(
        context,
        executable,
        args,
        cwd,
        env,
        payload_json <> "\n",
        timeout_ms,
        secrets,
      )
    }
    workflow_dag.JsonSchemaValidator(..) ->
      Error(failure(
        context,
        "structured_output_command_config_error",
        "internal error: JSON Schema validator passed to command runner",
        False,
        "",
        False,
        False,
        secrets,
      ))
  }
}

fn validate_declared_env(
  context: structured_output_validator.ValidatorContext,
  env: List(#(String, String)),
  secrets: List(String),
) -> Result(Nil, structured_output_validator.ValidatorFailure) {
  case first_invalid_env_key(env) {
    None -> Ok(Nil)
    Some(key) ->
      Error(failure(
        context,
        "structured_output_command_config_error",
        "invalid command validator env key: " <> key,
        False,
        "",
        False,
        False,
        secrets,
      ))
  }
}

fn first_invalid_env_key(env: List(#(String, String))) -> Option(String) {
  case env {
    [] -> None
    [#(key, _), ..rest] ->
      case valid_env_key(key) && !reserved_env_key(key) {
        True -> first_invalid_env_key(rest)
        False -> Some(key)
      }
  }
}

fn resolve_executable_and_args(
  context: structured_output_validator.ValidatorContext,
  argv: List(String),
  secrets: List(String),
) -> Result(
  #(String, List(String)),
  structured_output_validator.ValidatorFailure,
) {
  case argv {
    [] ->
      Error(failure(
        context,
        "structured_output_command_config_error",
        "command validator argv must not be empty",
        False,
        "",
        False,
        False,
        secrets,
      ))
    [executable, ..args] -> {
      let trimmed = string.trim(executable)
      case trimmed == "" {
        True ->
          Error(failure(
            context,
            "structured_output_command_config_error",
            "command validator executable must not be empty",
            False,
            "",
            False,
            False,
            secrets,
          ))
        False ->
          case string.contains(trimmed, "/") {
            False -> Ok(#(trimmed, args))
            True ->
              case valid_repository_relative_path(trimmed) {
                False ->
                  Error(failure(
                    context,
                    "structured_output_command_config_error",
                    "command validator executable path must be repository-relative and confined to the repository: "
                      <> trimmed,
                    False,
                    "",
                    False,
                    False,
                    secrets,
                  ))
                True -> Ok(#(path.join(context.repository_root, trimmed), args))
              }
          }
      }
    }
  }
}

fn working_directory_path(
  context: structured_output_validator.ValidatorContext,
  working_directory: workflow_dag.ValidatorWorkingDirectory,
) -> String {
  case working_directory {
    workflow_dag.ValidatorInWorkspace -> context.workspace_path
    workflow_dag.ValidatorInRepository -> context.repository_root
    workflow_dag.ValidatorInRunRoot -> context.run_root
  }
}

fn validator_env(
  context: structured_output_validator.ValidatorContext,
  declared_env: List(#(String, String)),
) -> List(#(String, String)) {
  list.append(
    list.append(allowlisted_parent_env(), declared_env),
    generated_env(context),
  )
}

fn allowlisted_parent_env() -> List(#(String, String)) {
  allowlisted_parent_env_loop(["PATH", "LANG", "LC_ALL", "TMPDIR"], [])
}

fn allowlisted_parent_env_loop(
  keys: List(String),
  acc: List(#(String, String)),
) -> List(#(String, String)) {
  case keys {
    [] -> list.reverse(acc)
    [key, ..rest] ->
      case path.env(key) {
        Some(value) -> allowlisted_parent_env_loop(rest, [#(key, value), ..acc])
        None -> allowlisted_parent_env_loop(rest, acc)
      }
  }
}

fn generated_env(
  context: structured_output_validator.ValidatorContext,
) -> List(#(String, String)) {
  [
    #("SCHERZO_CONFIG_DIR", context.config_dir),
    #("SCHERZO_REPO_ROOT", context.repository_root),
    #("SCHERZO_RUN_ROOT", context.run_root),
    #("SCHERZO_WORKFLOW_BUNDLE_DIR", context.workflow_bundle_dir),
    #("SCHERZO_WORKFLOW_ID", context.workflow_id),
    #("SCHERZO_RUN_ID", context.run_id),
    #("SCHERZO_STEP_ID", context.step_id),
    #("SCHERZO_ATTEMPT_INDEX", int.to_string(context.attempt_index)),
    #("SCHERZO_WORKSPACE_PATH", context.workspace_path),
    #("SCHERZO_STRUCTURED_OUTPUT_ARTIFACT_NAME", context.artifact_name),
    #("SCHERZO_STRUCTURED_OUTPUT_FORMAT", context.format),
    #("SCHERZO_STRUCTURED_OUTPUT_SOURCE_TYPE", context.source_type),
    #(
      "SCHERZO_STRUCTURED_OUTPUT_SOURCE_TOOL_NAME",
      structured_output_validator.source_tool_name_text(context),
    ),
    #("SCHERZO_VALIDATOR_NAME", context.validator_name),
    #("SCHERZO_VALIDATOR_TYPE", "command"),
    #("SCHERZO_VALIDATOR_INDEX", int.to_string(context.validator_index)),
  ]
}

fn run_process(
  context: structured_output_validator.ValidatorContext,
  executable: String,
  args: List(String),
  cwd: String,
  env: List(#(String, String)),
  stdin: String,
  timeout_ms: Int,
  secrets: List(String),
) -> Result(
  structured_output_validator.ValidatorPass,
  structured_output_validator.ValidatorFailure,
) {
  case port.start_argv_with_input(executable, args, cwd, env, stdin) {
    Error(error) ->
      Error(failure(
        context,
        "structured_output_command_config_error",
        "could not start command validator: "
          <> port.port_error_to_string(error),
        False,
        "",
        False,
        False,
        secrets,
      ))
    Ok(process) ->
      read_stdout_until_exit(process, context, timeout_ms, "", False, secrets)
  }
}

fn read_stdout_until_exit(
  process: port.Process,
  context: structured_output_validator.ValidatorContext,
  timeout_ms: Int,
  stdout: String,
  stdout_truncated: Bool,
  secrets: List(String),
) -> Result(
  structured_output_validator.ValidatorPass,
  structured_output_validator.ValidatorFailure,
) {
  case port.read_stdout_line(process, timeout_ms) {
    Ok(line) -> {
      let #(stdout, stdout_truncated) =
        append_capped(stdout, stdout_truncated, line <> "\n", stream_limit)
      read_stdout_until_exit(
        process,
        context,
        timeout_ms,
        stdout,
        stdout_truncated,
        secrets,
      )
    }
    Error(port.ProcessExited(status)) -> {
      let stderr = read_diagnostics(process)
      finish_process_result(
        status,
        stdout,
        stdout_truncated,
        stderr,
        False,
        context,
        secrets,
      )
    }
    Error(port.ReadTimeout) -> {
      let stderr = read_diagnostics(process)
      let _terminate_result = port.terminate(process)
      finish_timeout(stdout, stdout_truncated, stderr, context, secrets)
    }
    Error(error) -> {
      let stderr = read_diagnostics(process)
      let _terminate_result = port.terminate(process)
      Error(failure(
        context,
        "structured_output_command_config_error",
        "command validator process failed: " <> port.port_error_to_string(error),
        False,
        diagnostic_summary(stdout, stderr.value, secrets),
        stdout_truncated,
        stderr.truncated,
        secrets,
      ))
    }
  }
}

type CapturedStream {
  CapturedStream(value: String, truncated: Bool)
}

fn read_diagnostics(process: port.Process) -> CapturedStream {
  let raw = case port.read_diagnostics(process) {
    Ok(value) -> value
    Error(error) -> port.port_error_to_string(error)
  }
  let #(value, truncated) = append_capped("", False, raw, stream_limit)
  CapturedStream(value: value, truncated: truncated)
}

fn finish_process_result(
  status: Int,
  stdout: String,
  stdout_truncated: Bool,
  stderr: CapturedStream,
  timed_out: Bool,
  context: structured_output_validator.ValidatorContext,
  secrets: List(String),
) -> Result(
  structured_output_validator.ValidatorPass,
  structured_output_validator.ValidatorFailure,
) {
  case status, timed_out {
    0, False -> Ok(structured_output_validator.ValidatorPass)
    1, False -> {
      let failure_code = case
        step_artifact.failure_code_from_streams(stdout, stderr.value)
      {
        Some(code) -> code
        None -> "structured_output_command_rejected"
      }
      Error(failure(
        context,
        failure_code,
        "command validator rejected payload",
        command_rejection_retryable(failure_code),
        diagnostic_summary(stdout, stderr.value, secrets),
        stdout_truncated,
        stderr.truncated,
        secrets,
      ))
    }
    2, False ->
      Error(failure(
        context,
        "structured_output_command_config_error",
        "command validator reported configuration or internal error",
        False,
        diagnostic_summary(stdout, stderr.value, secrets),
        stdout_truncated,
        stderr.truncated,
        secrets,
      ))
    _, False ->
      Error(failure(
        context,
        "structured_output_command_config_error",
        "command validator exited " <> int.to_string(status),
        False,
        diagnostic_summary(stdout, stderr.value, secrets),
        stdout_truncated,
        stderr.truncated,
        secrets,
      ))
    _, True ->
      finish_timeout(stdout, stdout_truncated, stderr, context, secrets)
  }
}

fn finish_timeout(
  stdout: String,
  stdout_truncated: Bool,
  stderr: CapturedStream,
  context: structured_output_validator.ValidatorContext,
  secrets: List(String),
) -> Result(a, structured_output_validator.ValidatorFailure) {
  Error(failure(
    context,
    "structured_output_command_timeout",
    "command validator timed out",
    False,
    diagnostic_summary(stdout, stderr.value, secrets),
    stdout_truncated,
    stderr.truncated,
    secrets,
  ))
}

fn command_rejection_retryable(failure_code: String) -> Bool {
  case failure_code {
    "implementation_incomplete_noop" -> False
    _ -> True
  }
}

fn diagnostic_summary(
  stdout: String,
  stderr: String,
  secrets: List(String),
) -> String {
  let stdout = string.trim(stdout)
  let stderr = string.trim(stderr)
  let combined = case stdout, stderr {
    "", "" -> ""
    _, "" -> "stdout: " <> stdout
    "", _ -> "stderr: " <> stderr
    _, _ -> "stdout: " <> stdout <> "\nstderr: " <> stderr
  }
  combined
  |> redact(secrets)
  |> log.truncate(summary_limit)
}

fn failure(
  context: structured_output_validator.ValidatorContext,
  code: String,
  message: String,
  retryable: Bool,
  diagnostic_summary: String,
  stdout_truncated: Bool,
  stderr_truncated: Bool,
  secrets: List(String),
) -> structured_output_validator.ValidatorFailure {
  structured_output_validator.ValidatorFailure(
    validator_name: context.validator_name,
    validator_type: "command",
    code: code,
    message: redact(message, secrets),
    retryable: retryable,
    diagnostic_summary: diagnostic_summary,
    stdout_truncated: stdout_truncated,
    stderr_truncated: stderr_truncated,
  )
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

fn redact(value: String, secrets: List(String)) -> String {
  log.redact("structured_output_command_validator", value, secrets)
}

fn valid_env_key(value: String) -> Bool {
  case string.to_graphemes(value) {
    [] -> False
    [first, ..rest] ->
      is_env_first_char(first) && list.all(rest, is_env_rest_char)
  }
}

fn reserved_env_key(value: String) -> Bool {
  value == "PATH"
  || value == "HOME"
  || value == "PWD"
  || string.starts_with(value, "SCHERZO_")
}

fn valid_repository_relative_path(value: String) -> Bool {
  value != "" && !string.starts_with(value, "/") && !has_parent_segment(value)
}

fn has_parent_segment(value: String) -> Bool {
  value == ".."
  || string.starts_with(value, "../")
  || string.ends_with(value, "/..")
  || string.contains(value, "/../")
}

fn is_env_first_char(ch: String) -> Bool {
  is_lower(ch) || is_upper(ch) || ch == "_"
}

fn is_env_rest_char(ch: String) -> Bool {
  is_env_first_char(ch) || is_digit(ch)
}

fn is_lower(ch: String) -> Bool {
  string.compare(ch, "a") != order.Lt && string.compare(ch, "z") != order.Gt
}

fn is_upper(ch: String) -> Bool {
  string.compare(ch, "A") != order.Lt && string.compare(ch, "Z") != order.Gt
}

fn is_digit(ch: String) -> Bool {
  string.compare(ch, "0") != order.Lt && string.compare(ch, "9") != order.Gt
}
