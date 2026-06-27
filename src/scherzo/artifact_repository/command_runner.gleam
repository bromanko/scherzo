import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/path
import scherzo/port

pub type CommandSpec {
  CommandSpec(
    executable: String,
    args: List(String),
    cwd: String,
    env: List(#(String, String)),
    stdin: Option(String),
    timeout_ms: Option(Int),
  )
}

pub type CommandOutput {
  CommandOutput(exit_code: Int, stdout: String, diagnostics: String)
}

pub type CommandError {
  CommandError(message: String)
}

pub type Runner {
  Runner(run: fn(CommandSpec) -> Result(CommandOutput, CommandError))
}

pub fn production() -> Runner {
  production_with_env(path.env)
}

pub fn production_with_env(env: fn(String) -> Option(String)) -> Runner {
  Runner(run: fn(spec) { run_command(spec, env) })
}

pub fn command_error(message: String) -> CommandError {
  CommandError(message)
}

pub fn error_message(error: CommandError) -> String {
  let CommandError(message: message) = error
  message
}

fn run_command(
  spec: CommandSpec,
  env_reader: fn(String) -> Option(String),
) -> Result(CommandOutput, CommandError) {
  let CommandSpec(
    executable: executable,
    args: args,
    cwd: cwd,
    env: env,
    stdin: stdin,
    timeout_ms: timeout_ms,
  ) = spec
  let process_result = case stdin {
    Some(input) -> port.start_argv_with_input(executable, args, cwd, env, input)
    None -> port.start_argv(executable, args, cwd, env)
  }
  use process <- result.try(
    process_result
    |> result.map_error(fn(error) {
      CommandError("spawn_failed:" <> port.port_error_to_string(error))
    }),
  )
  case timeout_ms {
    Some(timeout_ms) -> run_command_with_timeout(process, timeout_ms)
    None -> run_command_with_idle_stdout(process, env_reader)
  }
}

fn run_command_with_timeout(
  process: port.Process,
  timeout_ms: Int,
) -> Result(CommandOutput, CommandError) {
  let timeout_ms = positive_timeout_ms(timeout_ms)
  case port.await_exit_with_stdout(process, timeout_ms) {
    Ok(#(status, stdout)) -> {
      let diagnostics = port.read_diagnostics(process) |> result.unwrap("")
      Ok(CommandOutput(
        exit_code: status,
        stdout: string.trim(stdout),
        diagnostics: diagnostics,
      ))
    }
    Error(port.ReadTimeout) -> {
      let terminate_suffix = terminate_process(process)
      let diagnostics = port.read_diagnostics(process) |> result.unwrap("")
      Error(CommandError(
        "timed_out after "
        <> int.to_string(timeout_ms)
        <> "ms"
        <> diagnostics_suffix(diagnostics)
        <> terminate_suffix,
      ))
    }
    Error(error) ->
      Error(CommandError(
        "await_exit_failed:" <> port.port_error_to_string(error),
      ))
  }
}

fn run_command_with_idle_stdout(
  process: port.Process,
  env_reader: fn(String) -> Option(String),
) -> Result(CommandOutput, CommandError) {
  let stdout = read_stdout(process, [], stdout_idle_timeouts(env_reader))
  case port.await_exit_with_stdout(process, 10_000) {
    Ok(#(status, late_stdout)) -> {
      let stdout = append_stdout(stdout, string.trim(late_stdout))
      let diagnostics = port.read_diagnostics(process) |> result.unwrap("")
      Ok(CommandOutput(
        exit_code: status,
        stdout: stdout,
        diagnostics: diagnostics,
      ))
    }
    Error(error) ->
      Error(CommandError(
        "await_exit_failed:" <> port.port_error_to_string(error),
      ))
  }
}

fn positive_timeout_ms(timeout_ms: Int) -> Int {
  case timeout_ms < 1 {
    True -> 1
    False -> timeout_ms
  }
}

fn diagnostics_suffix(diagnostics: String) -> String {
  case diagnostics == "" {
    True -> ""
    False -> ":" <> diagnostics
  }
}

fn terminate_process(process: port.Process) -> String {
  case port.terminate(process) {
    Ok(Nil) -> ""
    Error(error) -> "; terminate_failed:" <> port.port_error_to_string(error)
  }
}

fn stdout_idle_timeouts(env_reader: fn(String) -> Option(String)) -> Int {
  case env_reader("SCHERZO_COMMAND_RUNNER_STDOUT_IDLE_TIMEOUTS") {
    Some(value) ->
      case int.parse(value) {
        Ok(parsed) if parsed >= 0 -> parsed
        _ -> 200
      }
    None -> 200
  }
}

fn read_stdout(
  process: port.Process,
  acc: List(String),
  remaining_timeouts: Int,
) -> String {
  case port.read_stdout_line(process, 50) {
    Ok(line) -> read_stdout(process, [line, ..acc], 200)
    Error(port.ProcessExited(_)) | Error(port.Closed) -> joined_stdout(acc)
    Error(port.ReadTimeout) ->
      case remaining_timeouts <= 0 {
        True -> joined_stdout(acc)
        False -> read_stdout(process, acc, remaining_timeouts - 1)
      }
    Error(_) -> joined_stdout(acc)
  }
}

fn joined_stdout(acc: List(String)) -> String {
  list.reverse(acc) |> string.join(with: "\n") |> string.trim
}

fn append_stdout(before: String, after: String) -> String {
  case before, after {
    "", _ -> after
    _, "" -> before
    _, _ -> before <> "\n" <> after
  }
}

pub fn sh(executable: String, args: List(String), cwd: String) -> CommandSpec {
  CommandSpec(
    executable: executable,
    args: args,
    cwd: cwd,
    env: [],
    stdin: None,
    timeout_ms: None,
  )
}

pub fn with_input(spec: CommandSpec, stdin: String) -> CommandSpec {
  CommandSpec(..spec, stdin: Some(stdin))
}

pub fn with_env(
  spec: CommandSpec,
  env: List(#(String, String)),
) -> CommandSpec {
  let CommandSpec(env: existing, ..) = spec
  CommandSpec(..spec, env: list.append(env, existing))
}

pub fn with_timeout_ms(spec: CommandSpec, timeout_ms: Int) -> CommandSpec {
  CommandSpec(..spec, timeout_ms: Some(timeout_ms))
}

pub fn describe(spec: CommandSpec) -> String {
  let CommandSpec(
    executable: executable,
    args: args,
    cwd: cwd,
    stdin: stdin,
    ..,
  ) = spec
  executable
  <> " "
  <> string.join(args, with: " ")
  <> " (cwd="
  <> cwd
  <> ")"
  <> case stdin {
    Some(_) -> " [stdin]"
    None -> ""
  }
}

pub fn failed(output: CommandOutput) -> Bool {
  output.exit_code != 0
}

pub fn summarize(output: CommandOutput) -> String {
  "exit="
  <> int.to_string(output.exit_code)
  <> case output.stdout == "" {
    True -> ""
    False -> " stdout=" <> output.stdout
  }
  <> case output.diagnostics == "" {
    True -> ""
    False -> " diagnostics=" <> output.diagnostics
  }
}
