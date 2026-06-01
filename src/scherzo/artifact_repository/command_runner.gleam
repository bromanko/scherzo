import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/port

pub type CommandSpec {
  CommandSpec(
    executable: String,
    args: List(String),
    cwd: String,
    env: List(#(String, String)),
    stdin: Option(String),
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
  Runner(run: run_command)
}

pub fn command_error(message: String) -> CommandError {
  CommandError(message)
}

pub fn error_message(error: CommandError) -> String {
  let CommandError(message: message) = error
  message
}

fn run_command(spec: CommandSpec) -> Result(CommandOutput, CommandError) {
  let CommandSpec(executable, args, cwd, env, stdin) = spec
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
  let stdout = read_stdout(process, [])
  let diagnostics = port.read_diagnostics(process) |> result.unwrap("")
  case port.await_exit(process, 10_000) {
    Ok(status) ->
      Ok(CommandOutput(
        exit_code: status,
        stdout: stdout,
        diagnostics: diagnostics,
      ))
    Error(error) ->
      Error(CommandError(
        "await_exit_failed:" <> port.port_error_to_string(error),
      ))
  }
}

fn read_stdout(process: port.Process, acc: List(String)) -> String {
  case port.read_stdout_line(process, 50) {
    Ok(line) -> read_stdout(process, [line, ..acc])
    Error(port.ProcessExited(_)) | Error(port.Closed) ->
      list.reverse(acc) |> string.join(with: "\n") |> string.trim
    Error(port.ReadTimeout) ->
      list.reverse(acc) |> string.join(with: "\n") |> string.trim
    Error(_) -> list.reverse(acc) |> string.join(with: "\n") |> string.trim
  }
}

pub fn sh(executable: String, args: List(String), cwd: String) -> CommandSpec {
  CommandSpec(
    executable: executable,
    args: args,
    cwd: cwd,
    env: [],
    stdin: None,
  )
}

pub fn with_input(spec: CommandSpec, stdin: String) -> CommandSpec {
  CommandSpec(..spec, stdin: Some(stdin))
}

pub fn describe(spec: CommandSpec) -> String {
  let CommandSpec(executable, args, cwd, _, stdin) = spec
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
