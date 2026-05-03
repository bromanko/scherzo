import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/ctl
import scherzo/doctor
import scherzo/log
import scherzo/orchestrator/service

pub type RunMode {
  Daemon
  Once
  LinearSmoke
  LinearContractCheck
  PiProbe
}

pub type CliResult {
  Run(RunMode, Option(String))
  LinearAttachCommentFile(String, String, Option(String))
  Control(List(String))
  Doctor(doctor.Options)
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
    ["doctor", ..rest] ->
      parse_doctor_args(rest, doctor.Options(None, [], False, doctor.Human))
    ["--once"] -> Ok(Run(Once, None))
    ["--once", path] -> Ok(Run(Once, Some(path)))
    ["--linear-smoke"] -> Ok(Run(LinearSmoke, None))
    ["--linear-smoke", path] -> Ok(Run(LinearSmoke, Some(path)))
    ["--linear-contract-check"] -> Ok(Run(LinearContractCheck, None))
    ["--linear-contract-check", path] ->
      Ok(Run(LinearContractCheck, Some(path)))
    ["--pi-probe"] -> Ok(Run(PiProbe, None))
    ["--pi-probe", path] -> Ok(Run(PiProbe, Some(path)))
    ["--linear-attach-comment-file", comment_id, file_path] ->
      Ok(LinearAttachCommentFile(comment_id, file_path, None))
    ["--linear-attach-comment-file", comment_id, file_path, path] ->
      Ok(LinearAttachCommentFile(comment_id, file_path, Some(path)))
    [path] ->
      case string.starts_with(path, "-") {
        True -> Error(UsageError)
        False -> Ok(Run(Daemon, Some(path)))
      }
    _ -> Error(UsageError)
  }
}

fn parse_doctor_args(
  args: List(String),
  options: doctor.Options,
) -> Result(CliResult, CliError) {
  case args {
    [] -> Ok(Doctor(options))
    ["--list-checks", ..rest] ->
      parse_doctor_args(rest, doctor.Options(..options, list_checks: True))
    ["--logfmt", ..rest] ->
      parse_doctor_args(rest, doctor.Options(..options, output: doctor.Logfmt))
    ["--check"] -> Error(UsageError)
    ["--check", name, ..rest] ->
      parse_doctor_args(
        rest,
        doctor.Options(..options, checks: list.append(options.checks, [name])),
      )
    [arg, ..rest] ->
      case string.starts_with(arg, "-") {
        True -> Error(UsageError)
        False ->
          case options.path {
            Some(_) -> Error(UsageError)
            None ->
              parse_doctor_args(
                rest,
                doctor.Options(..options, path: Some(arg)),
              )
          }
      }
  }
}

pub fn usage() -> String {
  "Usage: gleam run -- [mode] [path-to-scherzo.yaml]\n       gleam run -- --linear-attach-comment-file <comment-id> <file.md> [path-to-scherzo.yaml]\n       gleam run -- doctor [options] [path-to-scherzo.yaml]\n       gleam run -- ctl <command> [options]\n\nScherzo polls Linear and runs pi agents in per-issue workspaces. With no mode, Scherzo runs daemon mode and keeps polling until the VM process is terminated.\n\nModes:\n  doctor                  Run readiness checks in stable order; default checks are workflow-config, linear-contract, linear-smoke, instance-lock, workspace-hooks, pi-probe.\n  doctor --check <name>   Run one named readiness check; repeat --check for a subset.\n  doctor --list-checks    Print available doctor check names and exit without loading config.\n  doctor --logfmt         Emit machine-readable logfmt doctor_check_* events instead of human-readable output.\n  --once                  Run one deterministic poll/dispatch tick, then exit.\n  --linear-smoke          Perform a bounded read-only Linear API check; no hooks, workspace, or pi prompt.\n  --linear-contract-check Compare workflow state/label policy to the Linear project board; read-only.\n  --linear-attach-comment-file <comment-id> <file.md> [path-to-scherzo.yaml]\n                          Upload a local Markdown file to Linear and attach it to an existing comment; mutates Linear.\n  --pi-probe              Prepare a scratch workspace and launch pi RPC without sending a prompt.\n  ctl                     Inspect a running daemon through the local read-only control API.\n  --help, -h              Show this help.\n\nControl commands:\n  ctl ping\n  ctl ps [--json]\n  ctl session <session-id> [--json]\n  ctl events <session-id> [--json]\n  ctl attach --raw <session-id>\n  ctl ... --control-file <path>\n\nRequired runtime inputs: LINEAR_API_KEY, a Linear project slug, pi --mode rpc, a YAML orchestrator config such as .scherzo/scherzo.yaml, YAML workflow DAG files, and workspace.hooks that create or verify each step workspace.\n\nSet agent.max_concurrent_agents: 0 to pause new dispatch while reconciliation remains active. Run only one Scherzo instance per Linear project and canonical workspace root until durable claiming is implemented. Daemon mode handles SIGTERM gracefully by running daemon.shutdown, removing the control file, and releasing the local instance lock before exit. The repository scherzo-start helper translates Ctrl-C/SIGINT into SIGTERM for this path; direct gleam run Ctrl-C may still terminate abruptly, and kill -9 or VM crashes may leave a stale instance lock that must be removed manually after verifying no Scherzo process is active."
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
    Ok(Doctor(options)) ->
      case service.start_doctor(options) {
        Ok(Nil) -> Nil
        Error(err) -> {
          case options.output {
            doctor.Human ->
              case err.code == "doctor_failed" {
                True -> Nil
                False -> io.println_error("Error: " <> err.message)
              }
            doctor.Logfmt ->
              io.println_error(
                log.error("startup_failed", [
                  #("code", err.code),
                  #("message", err.message),
                ]),
              )
          }
          halt(1)
        }
      }
    Ok(LinearAttachCommentFile(comment_id, file_path, path)) ->
      case
        service.start_linear_attach_comment_file(path, comment_id, file_path)
      {
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
    Ok(Run(mode, path)) ->
      case start_mode(mode, path) {
        Ok(Nil) -> finish_successful_run(mode)
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
    LinearContractCheck -> service.start_linear_contract_check(path)
    PiProbe -> service.start_pi_probe(path)
  }
}

fn finish_successful_run(mode: RunMode) -> Nil {
  case mode {
    // Daemon mode only returns after graceful shutdown has completed. Halt the
    // VM explicitly so long-lived Erlang runtime support processes, such as HTTP
    // client or SSL supervisors started during polling, cannot keep the
    // scherzo-start process-group wrapper alive after daemon_shutdown_complete.
    Daemon -> halt(0)
    _ -> Nil
  }
}

@external(erlang, "scherzo_main_ffi", "args")
fn args() -> List(String)

@external(erlang, "scherzo_main_ffi", "halt")
fn halt(code: Int) -> Nil
