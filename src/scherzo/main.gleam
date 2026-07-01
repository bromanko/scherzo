import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/code_snapshot
import scherzo/connect
import scherzo/ctl
import scherzo/doctor
import scherzo/json_schema_self_check
import scherzo/local_workflow_run
import scherzo/log
import scherzo/orchestrator/service
import scherzo/path as scherzo_path
import scherzo/tracker/conformance/cli as tracker_conformance_cli
import scherzo/version

pub type RunMode {
  Daemon
  Once
}

pub type CliResult {
  Run(RunMode, service.DaemonStartOptions)
  WorkflowRun(local_workflow_run.Options)
  JsonSchemaSelfCheck(String, String, String)
  TrackerConformance(List(String))
  Control(List(String))
  Offline(List(String))
  Connect(List(String))
  Doctor(doctor.Options)
  Version
  Help
}

pub type CliError {
  UsageError
}

pub type LauncherRoute {
  LauncherDaemon
  LauncherDirect
}

const launcher_route_only_env = "SCHERZO_LAUNCHER_ROUTE_ONLY"

pub fn parse_args(args: List(String)) -> Result(CliResult, CliError) {
  case args {
    ["--help"] | ["-h"] -> Ok(Help)
    ["--version"] -> Ok(Version)
    ["ctl", ..rest] -> reject_managed_launch_flags(rest, Control(rest))
    ["cleanup", ..rest] ->
      reject_managed_launch_flags(rest, Offline(["cleanup", ..rest]))
    ["schedules", ..rest] ->
      reject_managed_launch_flags(rest, Offline(["schedules", ..rest]))
    ["artifact", ..rest] ->
      reject_managed_launch_flags(rest, Offline(["artifact", ..rest]))
    ["workstream", ..rest] ->
      reject_managed_launch_flags(rest, Offline(["workstream", ..rest]))
    ["state", ..rest] ->
      reject_managed_launch_flags(rest, Offline(["state", ..rest]))
    ["connect", ..rest] -> reject_managed_launch_flags(rest, Connect(rest))
    ["__tracker-conformance-run", ..rest] -> Ok(TrackerConformance(rest))
    ["__json-schema-self-check", repository_root, schema_path, payload_path] ->
      Ok(JsonSchemaSelfCheck(repository_root, schema_path, payload_path))
    ["workflow", "run", workflow_path, ..rest] ->
      parse_workflow_run_args(
        rest,
        local_workflow_run.Options(
          workflow_path: workflow_path,
          run_root: "tmp/scherzo-workflow-run",
          run_id: "local-workflow-run",
          native_review_scenario: None,
        ),
      )
    ["doctor", ..rest] ->
      parse_doctor_args(rest, doctor.Options(None, [], False, doctor.Human))
    _ -> parse_run_args(args)
  }
}

fn parse_workflow_run_args(
  args: List(String),
  options: local_workflow_run.Options,
) -> Result(CliResult, CliError) {
  case args {
    [] -> Ok(WorkflowRun(options))
    ["--run-root", run_root, ..rest] ->
      parse_workflow_run_args(
        rest,
        local_workflow_run.Options(..options, run_root: run_root),
      )
    ["--run-id", run_id, ..rest] ->
      parse_workflow_run_args(
        rest,
        local_workflow_run.Options(..options, run_id: run_id),
      )
    ["--native-review-scenario", scenario, ..rest] ->
      parse_workflow_run_args(
        rest,
        local_workflow_run.Options(
          ..options,
          native_review_scenario: Some(scenario),
        ),
      )
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

type RunArgState {
  RunArgState(
    mode: RunMode,
    workflow_path: Option(String),
    grant_file: Option(String),
    status_file: Option(String),
  )
}

fn parse_run_args(args: List(String)) -> Result(CliResult, CliError) {
  parse_run_args_loop(args, RunArgState(Daemon, None, None, None))
}

fn parse_run_args_loop(
  args: List(String),
  state: RunArgState,
) -> Result(CliResult, CliError) {
  case args {
    [] -> finalize_run_args(state)
    ["--once", ..rest] ->
      case
        state.mode == Once
        || state.workflow_path != None
        || has_managed_launch_flags(state)
      {
        True -> Error(UsageError)
        False -> parse_run_args_loop(rest, RunArgState(Once, None, None, None))
      }
    ["--managed-launch-grant-file", path, ..rest] ->
      case state.grant_file {
        Some(_) -> Error(UsageError)
        None ->
          parse_run_args_loop(
            rest,
            RunArgState(
              state.mode,
              state.workflow_path,
              Some(path),
              state.status_file,
            ),
          )
      }
    ["--managed-launch-status-file", path, ..rest] ->
      case state.status_file {
        Some(_) -> Error(UsageError)
        None ->
          parse_run_args_loop(
            rest,
            RunArgState(
              state.mode,
              state.workflow_path,
              state.grant_file,
              Some(path),
            ),
          )
      }
    [arg, ..rest] ->
      case string.starts_with(arg, "-") {
        True -> Error(UsageError)
        False ->
          case state.workflow_path {
            Some(_) -> Error(UsageError)
            None ->
              parse_run_args_loop(
                rest,
                RunArgState(
                  state.mode,
                  Some(arg),
                  state.grant_file,
                  state.status_file,
                ),
              )
          }
      }
  }
}

fn finalize_run_args(state: RunArgState) -> Result(CliResult, CliError) {
  case state.mode, state.grant_file, state.status_file {
    Once, Some(_), _ | Once, _, Some(_) -> Error(UsageError)
    _, Some(grant_file), Some(status_file) ->
      Ok(Run(
        state.mode,
        service.DaemonStartOptions(
          workflow_path: state.workflow_path,
          managed_launch: Some(service.ManagedLaunchFiles(
            grant_file,
            status_file,
          )),
        ),
      ))
    _, Some(_), None | _, None, Some(_) -> Error(UsageError)
    _, None, None ->
      Ok(Run(
        state.mode,
        service.DaemonStartOptions(
          workflow_path: state.workflow_path,
          managed_launch: None,
        ),
      ))
  }
}

fn has_managed_launch_flags(state: RunArgState) -> Bool {
  state.grant_file != None || state.status_file != None
}

fn reject_managed_launch_flags(
  args: List(String),
  result: CliResult,
) -> Result(CliResult, CliError) {
  case contains_managed_launch_flags(args) {
    True -> Error(UsageError)
    False -> Ok(result)
  }
}

fn contains_managed_launch_flags(args: List(String)) -> Bool {
  list.any(args, fn(arg) {
    arg == "--managed-launch-grant-file"
    || arg == "--managed-launch-status-file"
  })
}

pub fn usage() -> String {
  "Usage: scherzo [mode] [path-to-scherzo.yaml]\n       scherzo --managed-launch-grant-file <grant.json> --managed-launch-status-file <status.json> [path-to-scherzo.yaml]\n       scherzo --version\n       scherzo doctor [options] [path-to-scherzo.yaml]\n       scherzo workflow run <workflow.yml> [--run-root <dir>] [--run-id <id>] [--native-review-scenario <id>]\n       scherzo ctl <command> [options]\n       scherzo cleanup [options]\n       scherzo schedules <subcommand> [options]\n       scherzo artifact publication <subcommand> [options]\n       scherzo workstream <subcommand> [options]\n       scherzo state <subcommand> [options]\n       scherzo connect --pairing-token <pair_...> --server-url <url> [options]\n\nScherzo polls a tracker and runs pi agents in per-task workspaces. With no mode, Scherzo runs daemon mode and keeps polling until the VM process is terminated.\n\nModes:\n  doctor                  Run readiness checks in stable order; default checks are workflow-config, tracker-contract, tracker-smoke, instance-lock, workspace-hooks, pi-probe.\n  doctor --check <name>   Run one named readiness check; repeat --check for a subset.\n  doctor --list-checks    Print available doctor check names and exit without loading config.\n  doctor --logfmt         Emit machine-readable logfmt doctor_check_* events instead of human-readable output.\n  workflow run            Run one workflow DAG file locally through Scherzo's workflow runner; by default agent steps use real pi-backed Scherzo agents. Native-review scenarios requested with --native-review-scenario use fixture responses for preflight only.\n  --once                  Run one deterministic poll/dispatch tick, then exit.\n  ctl                     Inspect or control a running daemon through the local control API.\n  cleanup                 Inspect or apply local owned-workspace cleanup directly from disk-backed state.\n  schedules               Inspect local scheduled-job status, history, logs, and doctor output.\n  artifact publication    Inspect or recover retained artifact publication state from disk.\n  workstream              Inspect or operate on retained workstream state.\n  state                   Inspect or repair retained local ledger state.\n  connect                 Exchange a pairing token for a durable daemon credential.\n  --version               Print source/build identity for logs and bug reports.\n  --help, -h              Show this help.\n\nDaemon control examples:\n  ctl ping\n  ctl ps [--json]\n  ctl session <session-id> [--json]\n  ctl events <session-id> [--json]\n  ctl attach --raw <session-id>\n  ctl run-schedule <job> --now\n  ctl ... --control-file <path>\n\nOffline command examples:\n  cleanup --root <workspace-root> [--json] [--dry-run|--yes]\n  schedules status [job] --root <workspace-root>\n  artifact publication list --run <run-id> --root <workspace-root>\n  workstream list [task]\n  state status --root <workspace-root>\n\nWhen using scripts/scherzoctl, relative --control-file, SCHERZO_CONTROL_FILE, and --root paths are resolved from the caller working directory before the wrapper enters the Scherzo source checkout. JSON ctl responses include non-secret target context (control file path and daemon workspace root).\n\nRequired runtime inputs: LINEAR_API_KEY, a tracker project slug such as tracker.linear.project, agents.runtime.type: pi, a YAML orchestrator config such as .scherzo/scherzo.yaml, YAML workflow DAG files, and workspace profiles with drivers that can prepare each step workspace.\n\nSet agents.concurrency: 0 to pause new dispatch while reconciliation remains active. Run only one Scherzo instance per tracker project and canonical workspace root until durable claiming is implemented. Daemon mode handles SIGTERM gracefully by running daemon.shutdown, removing the control file, and releasing the local instance lock before exit. The packaged scherzo launcher translates daemon-mode Ctrl-C/SIGINT into SIGTERM for this path. Direct gleam run Ctrl-C may still terminate abruptly because Scherzo's current Erlang signal FFI installs only the SIGTERM handler; kill -9 or VM crashes may leave a stale instance lock that must be removed manually after verifying no Scherzo process is active."
}

pub fn usage_error_hint(args: List(String)) -> Option(String) {
  case args {
    ["--tracker-smoke"] | ["--tracker-smoke", _] ->
      Some("--tracker-smoke was retired; use doctor --check tracker-smoke.")
    ["--tracker-contract-check"] | ["--tracker-contract-check", _] ->
      Some(
        "--tracker-contract-check was retired; use doctor --check tracker-contract.",
      )
    ["tracker-conformance", "run", ..] ->
      Some(
        "tracker-conformance run was retired; use scripts/scherzo-linear-conformance run for Linear dogfood or repo-maintainer contract tests for generic fixtures.",
      )
    ["--pi-probe"] | ["--pi-probe", _] ->
      Some("--pi-probe was retired; use doctor --check pi-probe.")
    ["--linear-attach-comment-file", ..] ->
      Some(
        "--linear-attach-comment-file was retired with no direct CLI replacement; "
        <> "configure task_updates.result.on_success: attachment for result uploads.",
      )
    _ -> None
  }
}

pub fn launcher_route(args: List(String)) -> LauncherRoute {
  case parse_args(args) {
    Ok(Run(Daemon, _)) -> LauncherDaemon
    _ -> LauncherDirect
  }
}

fn launcher_route_label(route: LauncherRoute) -> String {
  case route {
    LauncherDaemon -> "daemon"
    LauncherDirect -> "direct"
  }
}

fn launcher_route_only_requested() -> Bool {
  case scherzo_path.env(launcher_route_only_env) {
    Some("1") -> True
    _ -> False
  }
}

pub fn main() -> Nil {
  let arguments = args()
  case launcher_route_only_requested() {
    True -> {
      arguments
      |> launcher_route
      |> launcher_route_label
      |> io.println
      halt(0)
    }
    False -> run_from_args(arguments)
  }
}

fn run_from_args(arguments: List(String)) -> Nil {
  case parse_args(arguments) {
    Ok(Help) -> io.println(usage())
    Ok(Version) -> io.println(version.string())
    Ok(TrackerConformance(args)) -> {
      let result = tracker_conformance_cli.run(args)
      tracker_conformance_cli.print_summary_or_error(result)
      halt(tracker_conformance_cli.exit_code(result))
    }
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
    Ok(Offline(args)) ->
      case ctl.offline_main(args) {
        Ok(Nil) -> Nil
        Error(err) -> {
          io.println_error(
            log.error("offline_command_failed", [
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
    Ok(Connect(["--help"])) | Ok(Connect(["-h"])) -> io.println(connect.usage())
    Ok(Connect(args)) ->
      case connect.main(args) {
        Ok(Nil) -> Nil
        Error(err) -> {
          io.println_error(
            log.error("connect_failed", [
              #("code", connect.error_code(err)),
              #("message", connect.error_message(err)),
            ]),
          )
          case err {
            connect.UsageError(_) -> halt(2)
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
    Ok(WorkflowRun(options)) ->
      case local_workflow_run.run(options) {
        Ok(Nil) -> Nil
        Error(err) -> {
          io.println_error(
            log.error("workflow_run_failed", [
              #("code", err.code),
              #("message", err.message),
            ]),
          )
          halt(1)
        }
      }
    Ok(JsonSchemaSelfCheck(repository_root, schema_path, payload_path)) ->
      case
        json_schema_self_check.run(repository_root, schema_path, payload_path)
      {
        Ok(Nil) -> io.println("json_schema_self_check=ok")
        Error(json_schema_self_check.SelfCheckError(code, message)) -> {
          io.println_error(
            log.error("json_schema_self_check_failed", [
              #("code", code),
              #("message", message),
            ]),
          )
          halt(1)
        }
      }
    Ok(Run(mode, start_options)) ->
      case start_mode(mode, start_options) {
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
      case usage_error_hint(arguments) {
        Some(message) -> io.println_error("Error: " <> message)
        None -> Nil
      }
      io.println_error(usage())
      halt(2)
    }
  }
}

fn start_mode(
  mode: RunMode,
  start_options: service.DaemonStartOptions,
) -> Result(Nil, service.StartupError) {
  case mode {
    Daemon -> start_daemon_with_code_snapshot(start_options)
    Once -> service.start_once(start_options.workflow_path)
  }
}

fn start_daemon_with_code_snapshot(
  start_options: service.DaemonStartOptions,
) -> Result(Nil, service.StartupError) {
  case code_snapshot.ensure_scherzo_modules_loaded() {
    Ok(_) -> service.start_daemon(start_options)
    Error(error) ->
      Error(service.StartupError(
        "code_snapshot_failed",
        code_snapshot.describe_error(error),
      ))
  }
}

fn finish_successful_run(mode: RunMode) -> Nil {
  case mode {
    // Daemon mode only returns after graceful shutdown has completed. Halt the
    // VM explicitly so long-lived Erlang runtime support processes, such as HTTP
    // client or SSL supervisors started during polling, cannot keep the
    // foreground process-group wrapper alive after daemon_shutdown_complete.
    Daemon -> halt(0)
    _ -> Nil
  }
}

@external(erlang, "scherzo_main_ffi", "args")
fn args() -> List(String)

@external(erlang, "scherzo_main_ffi", "halt")
fn halt(code: Int) -> Nil
