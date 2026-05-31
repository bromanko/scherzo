//// Small Erlang port wrapper used by hooks, command steps, version discovery,
//// and pi RPC.
////
//// The wrapper keeps child stderr out of the stdout JSONL stream by redirecting
//// stderr to private per-process diagnostics storage. `read_diagnostics` reads
//// the current file before cleanup and cached diagnostics after `await_exit` or
//// `terminate` removes the private temp directory. Launchers preserve the
//// caller's PATH for children unless the caller supplies an explicit PATH, and
//// shell commands re-export that PATH after Bash startup files run so
//// Nix/devenv tools remain available even when a login shell rewrites PATH.

import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/path

pub const max_stdout_line_length = 10_000_000

pub type Process

pub type PortError {
  CwdNotDirectory
  InvalidCommand(reason: String)
  InvalidExecutable(reason: String)
  InvalidArgument(reason: String)
  InvalidEnvironment(reason: String)
  SpawnFailed(reason: String)
  SendFailed(reason: String)
  ReadTimeout
  LineTooLong(max_bytes: Int)
  ProcessExited(status: Int)
  Closed
  DiagnosticsFailed(reason: String)
  CleanupFailed(reason: String)
  UnexpectedFfiFailure(function: String, detail: String)
}

pub fn start(command: String, cwd: String) -> Result(Process, PortError) {
  start_with_env(command, cwd, [])
}

pub fn start_with_env(
  command: String,
  cwd: String,
  env: List(#(String, String)),
) -> Result(Process, PortError) {
  let env = env_with_current_path(env)
  let command = case string.trim(command) == "" {
    True -> command
    False -> shell_command_with_path_export(command, env)
  }
  ffi_start_with_env(command, cwd, env)
  |> result.map_error(fn(error) { raw_error("start_with_env", error) })
}

pub fn start_argv(
  executable: String,
  args: List(String),
  cwd: String,
  env: List(#(String, String)),
) -> Result(Process, PortError) {
  let env = env_with_current_path(env)
  ffi_start_argv(executable, args, cwd, env)
  |> result.map_error(fn(error) { raw_error("start_argv", error) })
}

pub fn start_argv_with_input(
  executable: String,
  args: List(String),
  cwd: String,
  env: List(#(String, String)),
  stdin: String,
) -> Result(Process, PortError) {
  let env = env_with_current_path(env)
  ffi_start_argv_with_input(executable, args, cwd, env, stdin)
  |> result.map_error(fn(error) { raw_error("start_argv_with_input", error) })
}

pub fn send_line(process: Process, line: String) -> Result(Nil, PortError) {
  ffi_send_line(process, line)
  |> result.map_error(fn(error) { raw_error("send_line", error) })
}

pub fn read_stdout_line(
  process: Process,
  timeout_ms: Int,
) -> Result(String, PortError) {
  ffi_read_stdout_line(process, timeout_ms)
  |> result.map_error(fn(error) { raw_error("read_stdout_line", error) })
}

pub fn read_diagnostics(process: Process) -> Result(String, PortError) {
  ffi_read_diagnostics(process)
  |> result.map_error(fn(error) { raw_error("read_diagnostics", error) })
}

pub fn terminate(process: Process) -> Result(Nil, PortError) {
  ffi_terminate(process)
  |> result.map_error(fn(error) { raw_error("terminate", error) })
}

pub fn await_exit(process: Process, timeout_ms: Int) -> Result(Int, PortError) {
  ffi_await_exit(process, timeout_ms)
  |> result.map_error(fn(error) { raw_error("await_exit", error) })
}

pub fn port_error_to_string(error: PortError) -> String {
  case error {
    CwdNotDirectory -> "working directory is not a directory"
    InvalidCommand(reason) -> "invalid command: " <> reason
    InvalidExecutable(reason) -> "invalid executable: " <> reason
    InvalidArgument(reason) -> "invalid argument: " <> reason
    InvalidEnvironment(reason) -> "invalid environment: " <> reason
    SpawnFailed(reason) -> "spawn failed: " <> reason
    SendFailed(reason) -> "send failed: " <> reason
    ReadTimeout -> "read timeout"
    LineTooLong(max_bytes) ->
      "line too long (max " <> int.to_string(max_bytes) <> " bytes)"
    ProcessExited(status) -> "process exited " <> int.to_string(status)
    Closed -> "port closed"
    DiagnosticsFailed(reason) -> "diagnostics failed: " <> reason
    CleanupFailed(reason) -> "cleanup failed: " <> reason
    UnexpectedFfiFailure(function, detail) ->
      function <> " failed unexpectedly: " <> detail
  }
}

pub fn temp_dir_for_test(process: Process) -> Result(String, PortError) {
  ffi_temp_dir_for_test(process)
  |> result.map_error(fn(error) { raw_error("temp_dir_for_test", error) })
}

fn raw_error(function: String, error: String) -> PortError {
  let #(tag, detail) = split_tag(error)
  case tag {
    "cwd_not_directory" -> CwdNotDirectory
    "invalid_command" -> InvalidCommand(detail)
    "invalid_executable" -> InvalidExecutable(detail)
    "invalid_arg" -> InvalidArgument(detail)
    "invalid_env" -> InvalidEnvironment(detail)
    "spawn_failed" -> SpawnFailed(detail)
    "send_failed" -> SendFailed(detail)
    "timeout" -> ReadTimeout
    "line_too_long" ->
      LineTooLong(parse_int_or_default(detail, max_stdout_line_length))
    "exit_status" -> ProcessExited(parse_int_or_default(detail, 0))
    "closed" -> Closed
    "diagnostics_failed" -> DiagnosticsFailed(detail)
    "cleanup_failed" -> CleanupFailed(detail)
    "unexpected_ffi_failure" -> UnexpectedFfiFailure(function, detail)
    _ -> UnexpectedFfiFailure(function, error)
  }
}

fn split_tag(error: String) -> #(String, String) {
  case string.split_once(error, on: ":") {
    Ok(#(tag, detail)) -> #(tag, detail)
    Error(Nil) -> #(error, "")
  }
}

fn parse_int_or_default(value: String, default: Int) -> Int {
  case int.parse(value) {
    Ok(i) -> i
    Error(Nil) -> default
  }
}

fn env_with_current_path(
  env: List(#(String, String)),
) -> List(#(String, String)) {
  case env_value(env, "PATH") {
    Some(_) -> env
    None ->
      case path.env("PATH") {
        Some(value) -> [#("PATH", value), ..env]
        None -> env
      }
  }
}

fn shell_command_with_path_export(
  command: String,
  env: List(#(String, String)),
) -> String {
  case env_value(env, "PATH") {
    Some(value) -> "export PATH=" <> shell_quote(value) <> "\n" <> command
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

fn shell_quote(value: String) -> String {
  "'" <> string.replace(value, each: "'", with: "'\\''") <> "'"
}

@external(erlang, "scherzo_port_ffi", "start_with_env")
fn ffi_start_with_env(
  command: String,
  cwd: String,
  env: List(#(String, String)),
) -> Result(Process, String)

@external(erlang, "scherzo_port_ffi", "start_argv")
fn ffi_start_argv(
  executable: String,
  args: List(String),
  cwd: String,
  env: List(#(String, String)),
) -> Result(Process, String)

@external(erlang, "scherzo_port_ffi", "start_argv_with_input")
fn ffi_start_argv_with_input(
  executable: String,
  args: List(String),
  cwd: String,
  env: List(#(String, String)),
  stdin: String,
) -> Result(Process, String)

@external(erlang, "scherzo_port_ffi", "send_line")
fn ffi_send_line(process: Process, line: String) -> Result(Nil, String)

@external(erlang, "scherzo_port_ffi", "read_stdout_line")
fn ffi_read_stdout_line(
  process: Process,
  timeout_ms: Int,
) -> Result(String, String)

@external(erlang, "scherzo_port_ffi", "read_diagnostics")
fn ffi_read_diagnostics(process: Process) -> Result(String, String)

@external(erlang, "scherzo_port_ffi", "terminate")
fn ffi_terminate(process: Process) -> Result(Nil, String)

@external(erlang, "scherzo_port_ffi", "await_exit")
fn ffi_await_exit(process: Process, timeout_ms: Int) -> Result(Int, String)

@external(erlang, "scherzo_port_ffi", "temp_dir_for_test")
fn ffi_temp_dir_for_test(process: Process) -> Result(String, String)
