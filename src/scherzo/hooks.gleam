import gleam/result
import gleam/string
import scherzo/error
import scherzo/log
import scherzo/port

pub type HookMode {
  Fatal
  BestEffort
}

pub fn run_hook(
  name: String,
  script: String,
  cwd: String,
  timeout_ms: Int,
) -> Result(Nil, error.HookError) {
  run_hook_with_env(name, script, cwd, timeout_ms, [])
}

pub fn run_hook_with_env(
  name: String,
  script: String,
  cwd: String,
  timeout_ms: Int,
  env: List(#(String, String)),
) -> Result(Nil, error.HookError) {
  case string.trim(script) {
    "" -> Ok(Nil)
    script -> {
      case port.start_with_env(script, cwd, env) {
        Error(err) -> Error(error.HookIo(port_error_to_string(err)))
        Ok(process) -> wait_for_hook(name, process, timeout_ms)
      }
    }
  }
}

pub fn run_best_effort(
  name: String,
  script: String,
  cwd: String,
  timeout_ms: Int,
) -> String {
  run_best_effort_with_env(name, script, cwd, timeout_ms, [])
}

pub fn run_best_effort_with_env(
  name: String,
  script: String,
  cwd: String,
  timeout_ms: Int,
  env: List(#(String, String)),
) -> String {
  case run_hook_with_env(name, script, cwd, timeout_ms, env) {
    Ok(Nil) -> log.info("hook_succeeded", [#("hook", name), #("cwd", cwd)])
    Error(err) ->
      log.warn("hook_failed", [
        #("hook", name),
        #("cwd", cwd),
        #("error", hook_error_to_string(err)),
      ])
  }
}

fn wait_for_hook(
  name: String,
  process: port.Process,
  timeout_ms: Int,
) -> Result(Nil, error.HookError) {
  case port.await_exit(process, timeout_ms) {
    Ok(0) -> Ok(Nil)
    Ok(status) -> {
      let diagnostics =
        port.read_diagnostics(process)
        |> result.unwrap("")
        |> log.truncate(4000)
      Error(error.HookFailed(name, status, diagnostics))
    }
    Error(port.AwaitTimeout) -> {
      let _ = port.terminate(process)
      Error(error.HookTimedOut(name))
    }
    Error(err) -> Error(error.HookIo(port_error_to_string(err)))
  }
}

fn hook_error_to_string(err: error.HookError) -> String {
  case err {
    error.HookFailed(name, status, diagnostics) ->
      name <> " exited " <> int_to_string(status) <> ": " <> diagnostics
    error.HookTimedOut(name) -> name <> " timed out"
    error.HookIo(message) -> message
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

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
