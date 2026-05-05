//// Small Erlang port wrapper used by hooks and pi RPC.
////
//// The wrapper keeps child stderr out of the stdout JSONL stream by redirecting
//// stderr to a temporary diagnostics file. `read_diagnostics` reads the current
//// contents of that file; diagnostics are therefore available after the child
//// writes them and are most reliable after exit.

import gleam/int
import gleam/result
import gleam/string

pub const max_stdout_line_length = 10_000_000

pub type Process

pub type PortError {
  StartFailed(String)
  SendFailed(String)
  ReadTimeout
  LineTooLong
  ProcessExited(Int)
  PortClosed
  DiagnosticsFailed(String)
  TerminateFailed(String)
  AwaitTimeout
  AwaitFailed(String)
}

pub fn start(command: String, cwd: String) -> Result(Process, PortError) {
  ffi_start(command, cwd)
  |> result.map_error(StartFailed)
}

pub fn start_with_env(
  command: String,
  cwd: String,
  env: List(#(String, String)),
) -> Result(Process, PortError) {
  ffi_start_with_env(command, cwd, env)
  |> result.map_error(StartFailed)
}

pub fn start_argv(
  executable: String,
  args: List(String),
  cwd: String,
  env: List(#(String, String)),
) -> Result(Process, PortError) {
  ffi_start_argv(executable, args, cwd, env)
  |> result.map_error(StartFailed)
}

pub fn send_line(process: Process, line: String) -> Result(Nil, PortError) {
  ffi_send_line(process, line)
  |> result.map_error(fn(error) { SendFailed(error) })
}

pub fn read_stdout_line(
  process: Process,
  timeout_ms: Int,
) -> Result(String, PortError) {
  case ffi_read_stdout_line(process, timeout_ms) {
    Ok(line) -> Ok(line)
    Error(error) -> Error(read_error(error))
  }
}

pub fn read_diagnostics(process: Process) -> Result(String, PortError) {
  ffi_read_diagnostics(process)
  |> result.map_error(fn(error) { DiagnosticsFailed(error) })
}

pub fn terminate(process: Process) -> Result(Nil, PortError) {
  ffi_terminate(process)
  |> result.map_error(fn(error) { TerminateFailed(error) })
}

pub fn await_exit(process: Process, timeout_ms: Int) -> Result(Int, PortError) {
  case ffi_await_exit(process, timeout_ms) {
    Ok(status) -> Ok(status)
    Error(error) -> Error(await_error(error))
  }
}

fn read_error(error: String) -> PortError {
  case error {
    "timeout" -> ReadTimeout
    "line_too_long" -> LineTooLong
    "closed" -> PortClosed
    _ ->
      case string.starts_with(error, "exit_status:") {
        True -> {
          let status =
            error
            |> string.drop_start(string.length("exit_status:"))
            |> parse_int_or_zero
          ProcessExited(status)
        }
        False -> AwaitFailed(error)
      }
  }
}

fn await_error(error: String) -> PortError {
  case error {
    "timeout" -> AwaitTimeout
    _ -> AwaitFailed(error)
  }
}

fn parse_int_or_zero(value: String) -> Int {
  case int.parse(value) {
    Ok(i) -> i
    Error(_) -> 0
  }
}

@external(erlang, "scherzo_port_ffi", "start")
fn ffi_start(command: String, cwd: String) -> Result(Process, String)

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
