import gleam/string
import scherzo/error
import scherzo/log
import scherzo/port

pub type HookMode {
  Fatal
  BestEffort
}

pub type BestEffortHookOutcome {
  BestEffortHookSucceeded(diagnostic: String)
  BestEffortHookFailed(diagnostic: String)
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
        Ok(process) -> wait_for_hook(name, process, timeout_ms, [])
      }
    }
  }
}

pub fn run_argv_with_env(
  name: String,
  executable: String,
  args: List(String),
  cwd: String,
  timeout_ms: Int,
  env: List(#(String, String)),
) -> Result(Nil, error.HookError) {
  run_argv_with_env_redacting(name, executable, args, cwd, timeout_ms, env, [])
}

pub fn run_argv_with_env_redacting(
  name: String,
  executable: String,
  args: List(String),
  cwd: String,
  timeout_ms: Int,
  env: List(#(String, String)),
  secrets: List(String),
) -> Result(Nil, error.HookError) {
  case port.start_argv(executable, args, cwd, env) {
    Error(err) ->
      Error(
        error.HookIo(log.redact(
          "hook_error",
          port_error_to_string(err),
          secrets,
        )),
      )
    Ok(process) -> wait_for_hook(name, process, timeout_ms, secrets)
  }
}

pub fn run_best_effort(
  name: String,
  script: String,
  cwd: String,
  timeout_ms: Int,
) -> String {
  run_best_effort_outcome(name, script, cwd, timeout_ms)
  |> best_effort_hook_diagnostic
}

pub fn run_best_effort_outcome(
  name: String,
  script: String,
  cwd: String,
  timeout_ms: Int,
) -> BestEffortHookOutcome {
  run_best_effort_with_env_outcome(name, script, cwd, timeout_ms, [])
}

pub fn run_best_effort_with_env(
  name: String,
  script: String,
  cwd: String,
  timeout_ms: Int,
  env: List(#(String, String)),
) -> String {
  run_best_effort_with_env_outcome(name, script, cwd, timeout_ms, env)
  |> best_effort_hook_diagnostic
}

pub fn run_best_effort_with_env_outcome(
  name: String,
  script: String,
  cwd: String,
  timeout_ms: Int,
  env: List(#(String, String)),
) -> BestEffortHookOutcome {
  case run_hook_with_env(name, script, cwd, timeout_ms, env) {
    Ok(Nil) ->
      BestEffortHookSucceeded(
        log.info("hook_succeeded", [
          #("hook", name),
          #("cwd", cwd),
        ]),
      )
    Error(err) ->
      BestEffortHookFailed(
        log.warn("hook_failed", [
          #("hook", name),
          #("cwd", cwd),
          #("error", hook_error_to_string(err)),
        ]),
      )
  }
}

pub fn run_best_effort_argv_with_env(
  name: String,
  executable: String,
  args: List(String),
  cwd: String,
  timeout_ms: Int,
  env: List(#(String, String)),
) -> String {
  run_best_effort_argv_with_env_redacting(
    name,
    executable,
    args,
    cwd,
    timeout_ms,
    env,
    [],
  )
}

pub fn run_best_effort_argv_with_env_redacting(
  name: String,
  executable: String,
  args: List(String),
  cwd: String,
  timeout_ms: Int,
  env: List(#(String, String)),
  secrets: List(String),
) -> String {
  case
    run_argv_with_env_redacting(
      name,
      executable,
      args,
      cwd,
      timeout_ms,
      env,
      secrets,
    )
  {
    Ok(Nil) ->
      BestEffortHookSucceeded(
        log.info("hook_succeeded", [
          #("hook", name),
          #("cwd", cwd),
        ]),
      )
    Error(err) ->
      BestEffortHookFailed(
        log.warn("hook_failed", [
          #("hook", name),
          #("cwd", cwd),
          #("error", hook_error_to_string(err)),
        ]),
      )
  }
  |> best_effort_hook_diagnostic
}

fn best_effort_hook_diagnostic(outcome: BestEffortHookOutcome) -> String {
  case outcome {
    BestEffortHookSucceeded(diagnostic) | BestEffortHookFailed(diagnostic) ->
      diagnostic
  }
}

fn wait_for_hook(
  name: String,
  process: port.Process,
  timeout_ms: Int,
  secrets: List(String),
) -> Result(Nil, error.HookError) {
  case port.await_exit(process, timeout_ms) {
    Ok(0) -> Ok(Nil)
    Ok(status) -> {
      let diagnostics = read_diagnostics_or_error(process)
      let diagnostics = log.redact("hook_diagnostics", diagnostics, secrets)
      let diagnostics = log.truncate(diagnostics, 4000)
      Error(error.HookFailed(name, status, diagnostics))
    }
    Error(port.ReadTimeout) -> {
      let _ = port.terminate(process)
      Error(error.HookTimedOut(name))
    }
    Error(err) -> Error(error.HookIo(port_error_to_string(err)))
  }
}

fn read_diagnostics_or_error(process: port.Process) -> String {
  case port.read_diagnostics(process) {
    Ok(diagnostics) -> diagnostics
    Error(err) ->
      "could not read hook diagnostics: " <> port_error_to_string(err)
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
  port.port_error_to_string(err)
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
