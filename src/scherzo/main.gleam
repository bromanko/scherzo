import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/ctl
import scherzo/log
import scherzo/orchestrator/service

pub type RunMode {
  Daemon
  Once
  LinearSmoke
  PiProbe
}

pub type CliResult {
  Run(RunMode, Option(String))
  Control(List(String))
  Help
}

pub type CliError {
  UsageError
}

pub fn parse_args(args: List(String)) -> Result(CliResult, CliError) {
  case args {
    [] -> Ok(Run(Daemon, None))
    ["--help"] | ["-h"] -> Ok(Help)
    ["ctl", ..rest] -> Ok(Control(rest))
    ["--once"] -> Ok(Run(Once, None))
    ["--once", path] -> Ok(Run(Once, Some(path)))
    ["--linear-smoke"] -> Ok(Run(LinearSmoke, None))
    ["--linear-smoke", path] -> Ok(Run(LinearSmoke, Some(path)))
    ["--pi-probe"] -> Ok(Run(PiProbe, None))
    ["--pi-probe", path] -> Ok(Run(PiProbe, Some(path)))
    [path] ->
      case string.starts_with(path, "-") {
        True -> Error(UsageError)
        False -> Ok(Run(Daemon, Some(path)))
      }
    _ -> Error(UsageError)
  }
}

pub fn usage() -> String {
  "Usage: gleam run -- [mode] [path-to-WORKFLOW.md]\n       gleam run -- ctl <command> [options]\n\nScherzo polls Linear and runs pi agents in per-issue workspaces. With no mode, Scherzo runs daemon mode and keeps polling until the VM process is terminated.\n\nModes:\n  --once           Run one deterministic poll/dispatch tick, then exit.\n  --linear-smoke   Perform a bounded read-only Linear API check; no hooks, workspace, or pi prompt.\n  --pi-probe       Prepare a scratch workspace and launch pi RPC without sending a prompt.\n  ctl              Inspect a running daemon through the local read-only control API.\n  --help, -h       Show this help.\n\nControl commands:\n  ctl ping\n  ctl ps [--json]\n  ctl session <session-id> [--json]\n  ctl events <session-id> [--json]\n  ctl attach --raw <session-id>\n  ctl ... --control-file <path>\n\nRequired runtime inputs: LINEAR_API_KEY, a Linear project slug, pi --mode rpc, and either REPO_URL for the example hooks.after_create clone or an explicit workspace verification hook such as hooks.before_run: test -d .git.\n\nSet agent.max_concurrent_agents: 0 to pause new dispatch while reconciliation remains active. Run only one Scherzo instance per Linear project and canonical workspace root until durable claiming is implemented. Daemon mode handles SIGTERM gracefully by running daemon.shutdown, removing the control file, and releasing the local instance lock before exit. Ctrl-C/SIGINT may still terminate abruptly in this runtime phase, and kill -9 or VM crashes may leave a stale instance lock that must be removed manually after verifying no Scherzo process is active."
}

pub fn main() -> Nil {
  case parse_args(args()) {
    Ok(Help) -> io.println(usage())
    Ok(Control(args)) ->
      case ctl.main(args) {
        Ok(Nil) -> Nil
        Error(err) -> {
          io.println_error(
            log.error("control_failed", [
              #("code", ctl.error_code(err)),
              #("message", ctl.error_message(err)),
            ]),
          )
          case err {
            ctl.UsageError(_) -> halt(2)
            _ -> halt(1)
          }
        }
      }
    Ok(Run(mode, path)) ->
      case start_mode(mode, path) {
        Ok(Nil) -> Nil
        Error(err) -> {
          io.println_error(
            log.error("startup_failed", [
              #("code", err.code),
              #("message", err.message),
            ]),
          )
          halt(1)
        }
      }
    Error(UsageError) -> {
      io.println_error(usage())
      halt(2)
    }
  }
}

fn start_mode(
  mode: RunMode,
  path: Option(String),
) -> Result(Nil, service.StartupError) {
  case mode {
    Daemon -> service.start_daemon(path)
    Once -> service.start_once(path)
    LinearSmoke -> service.start_linear_smoke(path)
    PiProbe -> service.start_pi_probe(path)
  }
}

@external(erlang, "scherzo_main_ffi", "args")
fn args() -> List(String)

@external(erlang, "scherzo_main_ffi", "halt")
fn halt(code: Int) -> Nil
