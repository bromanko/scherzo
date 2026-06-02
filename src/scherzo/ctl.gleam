import gleam/dynamic
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/control/client
import scherzo/control/command as control_command
import scherzo/control/file
import scherzo/control/protocol
import scherzo/control/query/types as query_types
import scherzo/ctl/artifact_publication as ctl_artifact_publication
import scherzo/ctl/schedule_state
import scherzo/ctl/task_output
import scherzo/ctl/workflow_recovery_history
import scherzo/ctl/workstream as ctl_workstream
import scherzo/path
import scherzo/schedule_doctor
import scherzo/session/event
import scherzo/session/reason as session_reason
import scherzo/state/ledger
import scherzo/state/local_artifacts
import scherzo/state/projection
import scherzo/state/record
import scherzo/task
import scherzo/terminal/render
import scherzo/terminal/style
import scherzo/turn_telemetry
import scherzo/workflow_repair
import simplifile

pub type OutputMode {
  Pretty
  Raw
  Json
}

pub type FollowMode {
  Follow
  NoFollow
}

pub type Command {
  Help
  Ping(control_file: Option(String), json: Bool)
  Ps(control_file: Option(String), json: Bool)
  Query(
    control_file: Option(String),
    json: Bool,
    query: query_types.QueryRequest,
  )
  TaskList(
    control_file: Option(String),
    json: Bool,
    states: List(task.TaskStateCategory),
    limit: Int,
    cursor: Option(String),
  )
  TaskShow(
    control_file: Option(String),
    json: Bool,
    ref: query_types.TaskQueryRef,
  )
  Session(control_file: Option(String), json: Bool, session_id: String)
  Events(
    control_file: Option(String),
    mode: OutputMode,
    color: style.ColorMode,
    since_cursor: Int,
    verbose: Bool,
    session_id: String,
  )
  Attach(
    control_file: Option(String),
    mode: OutputMode,
    color: style.ColorMode,
    follow: FollowMode,
    since_cursor: Int,
    verbose: Bool,
    session_id: String,
  )
  Operator(
    control_file: Option(String),
    json: Bool,
    command: control_command.OperatorCommand,
  )
  Cleanup(
    control_file: Option(String),
    root: Option(String),
    json: Bool,
    dry_run: Bool,
    yes: Bool,
  )
  SchedulesStatus(
    control_file: Option(String),
    root: Option(String),
    json: Bool,
    job_id: Option(String),
  )
  SchedulesHistory(
    control_file: Option(String),
    root: Option(String),
    json: Bool,
    job_id: String,
  )
  SchedulesLogs(
    control_file: Option(String),
    root: Option(String),
    json: Bool,
    color: style.ColorMode,
    verbose: Bool,
    job_id: String,
  )
  SchedulesDoctor(
    control_file: Option(String),
    root: Option(String),
    json: Bool,
    job_id: String,
  )
  Workstream(ctl_workstream.Command)
  ArtifactPublicationList(
    control_file: Option(String),
    root: Option(String),
    json: Bool,
    run_id: String,
  )
  ArtifactPublicationShow(
    control_file: Option(String),
    root: Option(String),
    json: Bool,
    run_id: String,
    publication_id: String,
  )
  ArtifactPublicationRetry(
    control_file: Option(String),
    root: Option(String),
    json: Bool,
    run_id: String,
    publication_id: String,
  )
  StateStatus(root: String, json: Bool)
  StateArchiveOld(root: String, json: Bool, yes: Bool)
  StateDiscardOld(root: String, json: Bool, yes: Bool)
  StateReinitialize(root: String, json: Bool, yes: Bool)
  StateRepairRunProvenance(
    root: String,
    json: Bool,
    run_id: String,
    dry_run: Bool,
    yes: Bool,
  )
}

pub type Error {
  UsageError(message: String)
  Failed(code: String, message: String)
}

pub type ControlClient {
  ControlClient(
    list_sessions: fn(file.ControlFile) ->
      Result(event.SessionList, client.ControlError),
    get_session: fn(file.ControlFile, String) ->
      Result(Option(event.SessionSummary), client.ControlError),
    get_events: fn(file.ControlFile, String, Int, Int) ->
      Result(event.EventPage, client.ControlError),
    stream_events: fn(
      file.ControlFile,
      String,
      Int,
      fn(event.SessionEvent) -> client.StreamAction,
    ) -> Result(Nil, client.ControlError),
    query: fn(file.ControlFile, query_types.QueryRequest) ->
      Result(query_types.QueryResponse, client.ControlError),
    apply_command: fn(file.ControlFile, control_command.OperatorCommand) ->
      Result(control_command.CommandResult, client.ControlError),
    raw_request: fn(file.ControlFile, protocol.Request) ->
      Result(String, client.ControlError),
  )
}

pub type Output {
  Output(line: fn(String) -> Nil, inline: fn(String) -> Nil)
}

pub type Replay {
  Replay(events: List(event.SessionEvent), last_cursor: Int, truncated: Bool)
}

type ScheduleDoctorReport {
  ScheduleDoctorReport(
    job_id: String,
    config_path: Option(String),
    diagnostics: List(schedule_doctor.Diagnostic),
  )
}

type StateRunProvenanceRepairResult {
  StateRunProvenanceRepairResult(
    status: String,
    run_id: String,
    repair_status: String,
    repair_mode: String,
    source_evidence: List(String),
    reason: Option(String),
    message: Option(String),
  )
}

type Flags {
  Flags(
    control_file: Option(String),
    json: Bool,
    raw: Bool,
    pretty: Bool,
    yes: Bool,
    dry_run: Bool,
    root: Option(String),
    reason: Option(String),
    step: Option(String),
    cancel: Bool,
    value: Option(String),
    no_follow: Bool,
    since_cursor: Int,
    color: style.ColorMode,
    verbose: Bool,
    now: Bool,
    last: Bool,
    run_id: Option(String),
    publication_id: Option(String),
    state_filters: List(task.TaskStateCategory),
    limit: Option(Int),
    cursor: Option(String),
    positional: List(String),
  )
}

pub fn main(args: List(String)) -> Result(Nil, Error) {
  case parse(args) {
    Error(err) -> Error(err)
    Ok(Help) -> {
      io.println(usage())
      Ok(Nil)
    }
    Ok(command) -> run(command)
  }
}

pub fn parse(args: List(String)) -> Result(Command, Error) {
  case args {
    [] | ["--help"] | ["-h"] -> Ok(Help)
    [name, ..rest] ->
      case parse_flags(rest, default_flags()) {
        Error(err) -> Error(err)
        Ok(flags) -> command_from(name, flags)
      }
  }
}

fn default_flags() -> Flags {
  Flags(
    control_file: None,
    json: False,
    raw: False,
    pretty: False,
    yes: False,
    dry_run: False,
    root: None,
    reason: None,
    step: None,
    cancel: False,
    value: None,
    no_follow: False,
    since_cursor: 0,
    color: style.ColorAuto,
    verbose: False,
    now: False,
    last: False,
    run_id: None,
    publication_id: None,
    state_filters: [],
    limit: None,
    cursor: None,
    positional: [],
  )
}

pub fn usage() -> String {
  "Usage: scherzo ctl <command> [options]\n       scherzoctl <command> [options]\n\nLocal Scherzo daemon inspection and operator controls. Commands:\n  ping                         Check that the daemon control API is reachable.\n  ps                           List sessions (LAST EVENT is daemon-relative age; long session names are shortened).\n  query status                 Run the additive read-query status/introspection surface.\n  task list                    List tracker tasks through the daemon query surface.\n  task show <task|id:<id>>     Show one tracker task through the daemon query surface.\n  session <session-ref>        Show one session summary.\n  events <session-ref>         Replay recent compact event lines.\n  events --pretty <session-ref>\n                               Replay retained events with human-readable rendering.\n  events --pretty --verbose <session-ref>\n                               Include pi cycle and raw diagnostic lines in pretty replay.\n  attach <session-ref>         Replay retained events and follow with human-readable rendering.\n  attach --verbose <session-ref>\n                               Include pi cycle and raw diagnostic lines in pretty attach.\n  attach --raw <session-ref>   Replay and follow compact event lines.\n  attach --json <session-ref>  Replay and follow JSON stream event envelopes.\n  attach --raw --json <session-ref>\n                               Legacy alias for attach --json.\n  pause                        Pause new dispatch.\n  resume                       Resume new dispatch.\n  reload                       Reload the workflow now.\n  retry <task>                 Retry a task now.\n  retry-step <target> [--step <step-id>]\n                               Retry a failed or interrupted workflow step without redispatching the whole task.\n  recovery cleanup-orphan-steps run:<run-id> [--dry-run|--yes]\n                               Dry run orphaned YAML child-step cleanup by default; use --yes to mutate.\n  park <task> --reason <text> --yes\n                               Park a task until explicitly unparked.\n  unpark <task>                Unpark a task.\n  abort <session-ref> --yes    Abort a running session.\n  stop-after-turn <session-ref> --yes\n                               Stop after the current turn.\n  prompt <session-ref> <text>  Queue an operator prompt for a session.\n  ui respond <session-ref> <request-id> (--cancel | --value <text>)\n                               Respond to an operator-managed UI request.\n  cleanup                     Dry-run local retention cleanup.\n  cleanup --yes               Apply eligible local cleanup after safety checks.\n  schedules status [job]      Inspect local scheduled job status/history summary.\n  schedules history <job>     Inspect local scheduled job history summary.\n  schedules logs <job> --last Replay the latest retained scheduled session logs.\n  schedules doctor <job>      Show local scheduled job diagnostics.\n  schedules run <job> --now   Start a scheduled job immediately.\n  workstream list [task]      List local workstreams, optionally for a Linear/task ref.\n  workstream show <ref>       Inspect one workstream id or Linear/task ref.\n  workstream start-from-handoff <workflow> <action> <ref> <sha256> [decision-id...]\n                               Create an input bundle and queue a phase from a retained handoff.\n  workstream start-from-input-bundle <workflow> <action> <ref> <sha256> [decision-id...]\n                               Queue a phase from an already retained workstream input bundle.\n  workstream decision <kind> <workstream-id> <action-id> <gate-id> <actor> <rationale> <name>:<ref>:<sha256>...\n                               Record approve/request-changes/reject/deviate gate decisions for exact snapshots.\n  artifact publication list --run <run-id> [--root <workspace-root>]\n                               Inspect the latest local publication status for one workflow run.\n  artifact publication show --run <run-id> --publication <publication-id> [--root <workspace-root>]\n                               Inspect the full local publication attempt history for one publication.\n  artifact publication retry --run <run-id> --publication <publication-id> [--root <workspace-root>]\n                               Replay the retained publication manifest without reading a step workspace.\n  state status --root <workspace-root>\n                               Inspect offline local state schema.\n  state archive-old --root <workspace-root> --yes\n                               Archive unsupported old local ledger state.\n  state discard-old --root <workspace-root> --yes\n                               Irreversibly discard unsupported old local ledger state.\n  state reinitialize --root <workspace-root> --yes\n                               Create an empty current ledger layout.\n  state repair-run-provenance run:<run-id> --root <workspace-root> --dry-run|--yes\n                               Inspect or append an auditable workflow provenance repair.\n\nOptions:\n  --control-file <path>        Use an explicit control.json path; relative paths resolve from the caller working directory.\n  --root <workspace-root>      Workspace root for cleanup or offline state commands; relative paths resolve from the caller working directory.\n  --raw                        Compact line output for attach/events.\n  --pretty                     Human-readable output for attach/events.\n  --json                       Protocol JSON for non-streaming commands, including target context; attach prints one JSON stream object per event.\n  --color=auto|always|never    Color policy for pretty output.\n  --no-follow                  For attach, replay retained events without following live events.\n  --since-cursor <n>           Replay events after cursor n.\n  --verbose                    Include pi lifecycle and raw diagnostics in pretty attach/events output.\n  --now                        Required for schedules run <job> --now.\n  --last                       Required for schedules logs <job> --last.\n  --run <run-id>               Workflow run id for artifact publication inspection.\n  --publication <publication>  Publication id for artifact publication show.\n  --state <state>              Filter task list by canonical state; may be repeated.\n  --limit <n>                  Maximum task list items (daemon clamps to 100).\n  --cursor <cursor>            Opaque cursor returned by task list.\n  --yes                        Confirm destructive commands.\n  --dry-run                    Force read-only cleanup inventory.\n  --reason <text>              Reason for parking a task.\n  --step <step-id>             Select a failed or interrupted workflow step for retry-step.\n  --cancel                     Cancel a UI request response.\n  --value <text>               Value for a UI request response.\n  --help, -h                   Show this help."
}

fn parse_flags(args: List(String), flags: Flags) -> Result(Flags, Error) {
  case args {
    [] ->
      Ok(
        Flags(
          ..flags,
          positional: list.reverse(flags.positional),
          state_filters: list.reverse(flags.state_filters),
        ),
      )
    ["--control-file", path, ..rest] ->
      parse_flags(rest, Flags(..flags, control_file: Some(path)))
    ["--control-file"] -> Error(UsageError("--control-file requires a path"))
    ["--root", root, ..rest] ->
      parse_flags(rest, Flags(..flags, root: Some(root)))
    ["--root"] -> Error(UsageError("--root requires a workspace root"))
    ["--json", ..rest] -> parse_flags(rest, Flags(..flags, json: True))
    ["--dry-run", ..rest] -> parse_flags(rest, Flags(..flags, dry_run: True))
    ["--raw", ..rest] -> parse_flags(rest, Flags(..flags, raw: True))
    ["--pretty", ..rest] -> parse_flags(rest, Flags(..flags, pretty: True))
    ["--verbose", ..rest] -> parse_flags(rest, Flags(..flags, verbose: True))
    ["--now", ..rest] -> parse_flags(rest, Flags(..flags, now: True))
    ["--last", ..rest] -> parse_flags(rest, Flags(..flags, last: True))
    ["--run", run_id, ..rest] ->
      parse_flags(rest, Flags(..flags, run_id: Some(run_id)))
    ["--run"] -> Error(UsageError("--run requires a run id"))
    ["--publication", publication_id, ..rest] ->
      parse_flags(rest, Flags(..flags, publication_id: Some(publication_id)))
    ["--publication"] ->
      Error(UsageError("--publication requires a publication id"))
    ["--state", state, ..rest] ->
      case task.state_category_from_string(state) {
        Ok(category) ->
          parse_flags(
            rest,
            Flags(..flags, state_filters: [category, ..flags.state_filters]),
          )
        Error(_) ->
          Error(UsageError(
            "--state must be backlog, ready, active, done, canceled, duplicate, or unknown",
          ))
      }
    ["--state"] -> Error(UsageError("--state requires a task state"))
    ["--limit", value, ..rest] ->
      case parse_task_limit(value) {
        Ok(limit) -> parse_flags(rest, Flags(..flags, limit: Some(limit)))
        Error(err) -> Error(err)
      }
    ["--limit"] -> Error(UsageError("--limit requires a positive integer"))
    ["--cursor", cursor, ..rest] ->
      case string.trim(cursor) {
        "" -> Error(UsageError("--cursor must not be empty"))
        _ -> parse_flags(rest, Flags(..flags, cursor: Some(cursor)))
      }
    ["--cursor"] -> Error(UsageError("--cursor requires a cursor"))
    ["--yes", ..rest] -> parse_flags(rest, Flags(..flags, yes: True))
    ["--no-follow", ..rest] ->
      parse_flags(rest, Flags(..flags, no_follow: True))
    ["--since-cursor", value, ..rest] ->
      case parse_cursor(value) {
        Ok(cursor) -> parse_flags(rest, Flags(..flags, since_cursor: cursor))
        Error(err) -> Error(err)
      }
    ["--since-cursor"] ->
      Error(UsageError("--since-cursor requires a non-negative integer"))
    ["--color", value, ..rest] ->
      case style.parse_color_mode(value) {
        Ok(mode) -> parse_flags(rest, Flags(..flags, color: mode))
        Error(_) -> Error(UsageError("--color must be auto, always, or never"))
      }
    ["--color"] -> Error(UsageError("--color requires auto, always, or never"))
    ["--reason", reason, ..rest] ->
      parse_flags(rest, Flags(..flags, reason: Some(reason)))
    ["--reason"] -> Error(UsageError("--reason requires text"))
    ["--step", step, ..rest] ->
      parse_flags(rest, Flags(..flags, step: Some(step)))
    ["--step"] -> Error(UsageError("--step requires a step id"))
    ["--cancel", ..rest] -> parse_flags(rest, Flags(..flags, cancel: True))
    ["--value", value, ..rest] ->
      parse_flags(rest, Flags(..flags, value: Some(value)))
    ["--value"] -> Error(UsageError("--value requires text"))
    ["--help", ..] | ["-h", ..] -> Ok(Flags(..flags, positional: ["--help"]))
    [arg, ..rest] ->
      case string.starts_with(arg, "--color=") {
        True -> {
          let value = string.drop_start(arg, 8)
          case style.parse_color_mode(value) {
            Ok(mode) -> parse_flags(rest, Flags(..flags, color: mode))
            Error(_) ->
              Error(UsageError("--color must be auto, always, or never"))
          }
        }
        False ->
          case string.starts_with(arg, "--") {
            True -> Error(UsageError("unknown option: " <> arg))
            False ->
              parse_flags(
                rest,
                Flags(..flags, positional: [arg, ..flags.positional]),
              )
          }
      }
  }
}

fn parse_cursor(value: String) -> Result(Int, Error) {
  case int.parse(value) {
    Ok(cursor) ->
      case cursor < 0 {
        True ->
          Error(UsageError("--since-cursor requires a non-negative integer"))
        False -> Ok(cursor)
      }
    Error(_) ->
      Error(UsageError("--since-cursor requires a non-negative integer"))
  }
}

fn parse_task_limit(value: String) -> Result(Int, Error) {
  case int.parse(value) {
    Ok(limit) if limit > 0 -> Ok(limit)
    _ -> Error(UsageError("--limit requires a positive integer"))
  }
}

fn option_with_default(value: Option(a), default: a) -> a {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn task_query_ref(value: String) -> Result(query_types.TaskQueryRef, Error) {
  let value = string.trim(value)
  case value == "" {
    True -> Error(UsageError("task show requires a non-empty task reference"))
    False ->
      case string.starts_with(value, "id:") {
        True -> {
          let id = string.drop_start(value, 3) |> string.trim
          case id == "" {
            True -> Error(UsageError("task show id must include a remote id"))
            False -> Ok(query_types.TaskRemoteId(provider: None, id: id))
          }
        }
        False -> Ok(query_types.TaskDisplayId(value))
      }
  }
}

fn command_from(name: String, flags: Flags) -> Result(Command, Error) {
  case name, flags.positional {
    "--help", _ | "-h", _ -> Ok(Help)
    _, ["--help"] -> Ok(Help)
    "ping", [] -> Ok(Ping(flags.control_file, flags.json))
    "ps", [] -> Ok(Ps(flags.control_file, flags.json))
    "query", ["status"] ->
      Ok(Query(flags.control_file, flags.json, query_types.Status))
    "task", ["list"] ->
      Ok(TaskList(
        flags.control_file,
        flags.json,
        flags.state_filters,
        option_with_default(flags.limit, 50),
        flags.cursor,
      ))
    "task", ["show", ref] -> {
      use ref <- try_ctl(task_query_ref(ref))
      Ok(TaskShow(flags.control_file, flags.json, ref))
    }
    "task", _ ->
      Error(UsageError(
        "task usage: task list [--state <state>] [--limit <n>] [--cursor <cursor>] | task show <task-or-id:<remote-id>>",
      ))
    "session", [session_id] ->
      Ok(Session(flags.control_file, flags.json, session_id))
    "events", [session_id] -> {
      use mode <- try_ctl(events_mode(flags))
      Ok(Events(
        flags.control_file,
        mode,
        events_color(mode, flags.color),
        flags.since_cursor,
        flags.verbose,
        session_id,
      ))
    }
    "attach", [session_id] -> {
      use mode <- try_ctl(attach_mode(flags))
      Ok(Attach(
        flags.control_file,
        mode,
        attach_color(mode, flags.color),
        case flags.no_follow {
          True -> NoFollow
          False -> Follow
        },
        flags.since_cursor,
        flags.verbose,
        session_id,
      ))
    }
    "attach", _ ->
      Error(UsageError(
        "attach usage: attach [--raw|--json|--pretty] <session-ref>",
      ))
    "pause", [] -> Ok(operator(flags, control_command.PauseDispatch))
    "resume", [] -> Ok(operator(flags, control_command.ResumeDispatch))
    "reload", [] -> Ok(operator(flags, control_command.ReloadWorkflow))
    "retry", [issue] ->
      Ok(operator(flags, control_command.RetryIssue(issue_ref(issue))))
    "retry-step", [target] ->
      Ok(operator(
        flags,
        control_command.RetryWorkflowStep(
          retry_workflow_step_target(target),
          flags.step,
        ),
      ))
    "recovery", ["cleanup-orphan-steps", target] ->
      case recovery_cleanup_run_id(target), flags.yes, flags.dry_run {
        Error(message), _, _ -> Error(UsageError(message))
        Ok(_), True, True ->
          Error(UsageError(
            "recovery cleanup-orphan-steps --yes cannot be combined with --dry-run",
          ))
        Ok(run_id), yes, _ ->
          Ok(operator(flags, control_command.CleanupOrphanSteps(run_id, !yes)))
      }
    "park", [issue] ->
      case flags.reason, flags.yes {
        Some(reason), True ->
          Ok(operator(
            flags,
            control_command.ParkIssue(issue_ref(issue), reason),
          ))
        None, _ -> Error(UsageError("park requires --reason <text>"))
        Some(_), False -> Error(UsageError("park requires --yes"))
      }
    "unpark", [issue] ->
      Ok(operator(flags, control_command.UnparkIssue(issue_ref(issue))))
    "abort", [session_id] ->
      case flags.yes {
        True -> Ok(operator(flags, control_command.AbortSession(session_id)))
        False -> Error(UsageError("abort requires --yes"))
      }
    "stop-after-turn", [session_id] ->
      case flags.yes {
        True ->
          Ok(operator(flags, control_command.StopAfterCurrentTurn(session_id)))
        False -> Error(UsageError("stop-after-turn requires --yes"))
      }
    "prompt", [session_id, message] ->
      Ok(operator(flags, control_command.PromptSession(session_id, message)))
    "ui", ["respond", session_id, request_id] ->
      case flags.cancel, flags.value {
        True, None ->
          Ok(operator(
            flags,
            control_command.RespondUi(
              session_id,
              request_id,
              control_command.UiCancel,
            ),
          ))
        False, Some(value) ->
          Ok(operator(
            flags,
            control_command.RespondUi(
              session_id,
              request_id,
              control_command.UiValue(value),
            ),
          ))
        True, Some(_) ->
          Error(UsageError(
            "ui respond requires exactly one of --cancel or --value",
          ))
        False, None ->
          Error(UsageError("ui respond requires --cancel or --value <text>"))
      }
    "cleanup", [] ->
      case flags.yes, flags.dry_run {
        True, True ->
          Error(UsageError("cleanup --yes cannot be combined with --dry-run"))
        True, False ->
          Ok(Cleanup(flags.control_file, flags.root, flags.json, False, True))
        False, _ ->
          Ok(Cleanup(flags.control_file, flags.root, flags.json, True, False))
      }
    "schedules", ["status"] ->
      Ok(SchedulesStatus(flags.control_file, flags.root, flags.json, None))
    "schedules", ["status", job_id] ->
      Ok(SchedulesStatus(
        flags.control_file,
        flags.root,
        flags.json,
        Some(job_id),
      ))
    "schedules", ["history", job_id] ->
      Ok(SchedulesHistory(flags.control_file, flags.root, flags.json, job_id))
    "schedules", ["logs", job_id] ->
      case flags.last {
        True ->
          Ok(SchedulesLogs(
            flags.control_file,
            flags.root,
            flags.json,
            flags.color,
            flags.verbose,
            job_id,
          ))
        False -> Error(UsageError("schedules logs requires --last"))
      }
    "schedules", ["doctor", job_id] ->
      Ok(SchedulesDoctor(flags.control_file, flags.root, flags.json, job_id))
    "schedules", ["run", job_id] ->
      case flags.now {
        True -> Ok(operator(flags, control_command.RunScheduleNow(job_id)))
        False -> Error(UsageError("schedules run requires --now"))
      }
    "schedules", _ ->
      Error(UsageError(
        "schedules usage: schedules status [job] | history <job> | logs <job> --last | doctor <job> | run <job> --now",
      ))
    "workstream", args ->
      case
        ctl_workstream.parse(args, flags.control_file, flags.root, flags.json)
      {
        Ok(command) -> Ok(Workstream(command))
        Error(message) -> Error(UsageError(message))
      }
    "artifact", ["publication", "list"] -> {
      use run_id <- try_ctl(required_run_id(flags))
      Ok(ArtifactPublicationList(
        flags.control_file,
        flags.root,
        flags.json,
        run_id,
      ))
    }
    "artifact", ["publication", "show"] -> {
      use run_id <- try_ctl(required_run_id(flags))
      use publication_id <- try_ctl(required_publication_id(flags))
      Ok(ArtifactPublicationShow(
        flags.control_file,
        flags.root,
        flags.json,
        run_id,
        publication_id,
      ))
    }
    "artifact", ["publication", "retry"] -> {
      use run_id <- try_ctl(required_run_id(flags))
      use publication_id <- try_ctl(required_publication_id(flags))
      Ok(ArtifactPublicationRetry(
        flags.control_file,
        flags.root,
        flags.json,
        run_id,
        publication_id,
      ))
    }
    "artifact", _ ->
      Error(UsageError(
        "artifact usage: artifact publication list --run <run-id> | artifact publication show --run <run-id> --publication <publication-id> | artifact publication retry --run <run-id> --publication <publication-id>",
      ))
    "state", ["status"] -> {
      use root <- try_ctl(required_root(flags))
      Ok(StateStatus(root, flags.json))
    }
    "state", ["archive-old"] -> {
      use root <- try_ctl(required_root(flags))
      Ok(StateArchiveOld(root, flags.json, flags.yes))
    }
    "state", ["discard-old"] -> {
      use root <- try_ctl(required_root(flags))
      Ok(StateDiscardOld(root, flags.json, flags.yes))
    }
    "state", ["reinitialize"] -> {
      use root <- try_ctl(required_root(flags))
      Ok(StateReinitialize(root, flags.json, flags.yes))
    }
    "state", ["repair-run-provenance", target] -> {
      use root <- try_ctl(required_root(flags))
      use run_id <- try_ctl(repair_run_provenance_target(target))
      case flags.yes, flags.dry_run {
        True, True ->
          Error(UsageError(
            "state repair-run-provenance requires exactly one of --dry-run or --yes",
          ))
        False, False ->
          Error(UsageError(
            "state repair-run-provenance requires --dry-run or --yes",
          ))
        _, _ ->
          Ok(StateRepairRunProvenance(
            root,
            flags.json,
            run_id,
            flags.dry_run,
            flags.yes,
          ))
      }
    }
    "state", _ ->
      Error(UsageError(
        "state usage: state status|archive-old|discard-old|reinitialize|repair-run-provenance --root <workspace-root>",
      ))
    _, _ -> Error(UsageError("unknown or invalid ctl command: " <> name))
  }
}

fn recovery_cleanup_run_id(target: String) -> Result(String, String) {
  case string.starts_with(target, "run:") {
    True ->
      case string.drop_start(target, 4) |> string.trim {
        "" -> Error("recovery cleanup-orphan-steps requires run:<run-id>")
        run_id -> Ok(run_id)
      }
    False -> Error("recovery cleanup-orphan-steps requires run:<run-id>")
  }
}

fn required_root(flags: Flags) -> Result(String, Error) {
  case flags.root {
    Some(root) -> Ok(root)
    None -> Error(UsageError("state commands require --root <workspace-root>"))
  }
}

fn required_run_id(flags: Flags) -> Result(String, Error) {
  case flags.run_id {
    Some(run_id) -> Ok(run_id)
    None ->
      Error(UsageError("artifact publication commands require --run <run-id>"))
  }
}

fn required_publication_id(flags: Flags) -> Result(String, Error) {
  case flags.publication_id {
    Some(publication_id) -> Ok(publication_id)
    None ->
      Error(UsageError(
        "artifact publication show requires --publication <publication-id>",
      ))
  }
}

fn attach_mode(flags: Flags) -> Result(OutputMode, Error) {
  case flags.pretty, flags.raw, flags.json {
    True, True, _ | True, _, True ->
      Error(UsageError("choose only one of --pretty, --raw, or --json"))
    _, True, True -> Ok(Json)
    True, False, False -> Ok(Pretty)
    False, True, False -> Ok(Raw)
    False, False, True -> Ok(Json)
    False, False, False -> Ok(Pretty)
  }
}

fn events_mode(flags: Flags) -> Result(OutputMode, Error) {
  case flags.pretty, flags.raw, flags.json {
    True, True, _ | True, _, True | False, True, True ->
      Error(UsageError("choose only one of --pretty, --raw, or --json"))
    True, False, False -> Ok(Pretty)
    False, _, True -> Ok(Json)
    False, _, False -> Ok(Raw)
  }
}

fn attach_color(mode: OutputMode, color: style.ColorMode) -> style.ColorMode {
  case mode {
    Pretty -> color
    Raw | Json -> style.ColorNever
  }
}

fn events_color(mode: OutputMode, color: style.ColorMode) -> style.ColorMode {
  case mode {
    Pretty -> color
    Raw | Json -> style.ColorNever
  }
}

fn pretty_options(
  color: style.ColorMode,
  verbose: Bool,
) -> render.RenderOptions {
  case verbose {
    True -> render.verbose_options(color)
    False -> render.default_options(color)
  }
}

fn operator(flags: Flags, command: control_command.OperatorCommand) -> Command {
  Operator(flags.control_file, flags.json, command)
}

fn issue_ref(value: String) -> control_command.IssueRef {
  case string.starts_with(value, "id:") {
    True -> control_command.IssueId(string.drop_start(value, 3))
    False -> control_command.IssueIdentifier(value)
  }
}

fn retry_workflow_step_target(
  value: String,
) -> control_command.RetryWorkflowStepTarget {
  case string.starts_with(value, "id:") {
    True ->
      control_command.RetryWorkflowStepIssueRef(
        control_command.IssueId(string.drop_start(value, 3)),
      )
    False ->
      case string.starts_with(value, "run:") {
        True ->
          control_command.RetryWorkflowStepRunId(string.drop_start(value, 4))
        False -> control_command.RetryWorkflowStepAutoTarget(value)
      }
  }
}

fn repair_run_provenance_target(value: String) -> Result(String, Error) {
  case string.starts_with(value, "run:") {
    True -> {
      let run_id = string.drop_start(value, 4) |> string.trim
      case run_id == "" {
        True -> Error(UsageError("repair-run-provenance requires run:<run-id>"))
        False -> Ok(run_id)
      }
    }
    False -> Error(UsageError("repair-run-provenance requires run:<run-id>"))
  }
}

fn run(command: Command) -> Result(Nil, Error) {
  run_with_deps(command, real_control_client(), real_output())
}

pub fn run_with_deps(
  command: Command,
  deps: ControlClient,
  output: Output,
) -> Result(Nil, Error) {
  case command {
    Help -> {
      output.line(usage())
      Ok(Nil)
    }
    Ping(control_path, json) -> {
      use target <- try_ctl(load_control_target(control_path))
      let control_file = target.control_file
      case json {
        True -> print_raw_request(target, protocol.Ping("1", ""), deps, output)
        False ->
          case client.ping(control_file) {
            Ok(Nil) -> {
              output.line("ok")
              Ok(Nil)
            }
            Error(err) -> Error(client_error(err))
          }
      }
    }
    Ps(control_path, json) -> {
      use target <- try_ctl(load_control_target(control_path))
      let control_file = target.control_file
      case json {
        True ->
          print_raw_request(
            target,
            protocol.ListSessions("1", ""),
            deps,
            output,
          )
        False ->
          case deps.list_sessions(control_file) {
            Ok(snapshot) -> {
              print_sessions_table(snapshot.sessions, snapshot.now_ms, output)
              Ok(Nil)
            }
            Error(err) -> Error(client_error(err))
          }
      }
    }
    Query(control_path, json, query) -> {
      use target <- try_ctl(load_control_target(control_path))
      let control_file = target.control_file
      case json {
        True ->
          print_raw_request(
            target,
            protocol.query_request("1", "", query),
            deps,
            output,
          )
        False ->
          case deps.query(control_file, query) {
            Ok(query_types.StatusResponse(status)) -> {
              print_query_status(status, output)
              Ok(Nil)
            }
            Ok(_) ->
              Error(Failed(
                "unexpected_query_response",
                "unexpected query response",
              ))
            Error(err) -> Error(client_error(err))
          }
      }
    }
    TaskList(control_path, json, states, limit, cursor) -> {
      use target <- try_ctl(load_control_target(control_path))
      let query =
        query_types.TaskList(query_types.TaskListQuery(
          states: states,
          limit: limit,
          cursor: cursor,
        ))
      case deps.query(target.control_file, query) {
        Ok(query_types.TaskListResponse(tasks)) -> {
          case json {
            True -> output.line(task_output.list_json(tasks))
            False -> task_output.print_list(tasks, output.line)
          }
          Ok(Nil)
        }
        Ok(_) ->
          Error(Failed("unexpected_query_response", "unexpected query response"))
        Error(err) -> Error(client_error(err))
      }
    }
    TaskShow(control_path, json, ref) -> {
      use target <- try_ctl(load_control_target(control_path))
      let query = query_types.TaskShow(query_types.TaskShowQuery(ref: ref))
      case deps.query(target.control_file, query) {
        Ok(query_types.TaskShowResponse(task_detail)) -> {
          case json {
            True -> output.line(task_output.detail_json(task_detail))
            False -> task_output.print_detail(task_detail, output.line)
          }
          Ok(Nil)
        }
        Ok(_) ->
          Error(Failed("unexpected_query_response", "unexpected query response"))
        Error(err) -> Error(client_error(err))
      }
    }
    Session(control_path, json, session_ref) -> {
      use target <- try_ctl(load_control_target(control_path))
      let control_file = target.control_file
      use session_id <- try_ctl(resolve_session_ref(
        control_file,
        deps,
        session_ref,
      ))
      case json {
        True ->
          print_raw_request(
            target,
            protocol.GetSession("1", "", session_id),
            deps,
            output,
          )
        False ->
          case deps.get_session(control_file, session_id) {
            Ok(Some(summary)) -> {
              print_session(summary, output)
              print_workflow_recovery_history(
                control_file.workspace_root,
                summary,
                output,
              )
              Ok(Nil)
            }
            Ok(None) -> Error(Failed("missing_session", "session not found"))
            Error(err) -> Error(client_error(err))
          }
      }
    }
    Events(control_path, mode, color, since_cursor, verbose, session_id) -> {
      use target <- try_ctl(load_control_target(control_path))
      run_events(
        target,
        deps,
        output,
        mode,
        color,
        since_cursor,
        verbose,
        session_id,
      )
    }
    Attach(control_path, mode, color, follow, since_cursor, verbose, session_id) -> {
      use target <- try_ctl(load_control_target(control_path))
      run_attach(
        target.control_file,
        deps,
        output,
        mode,
        color,
        follow,
        since_cursor,
        verbose,
        session_id,
      )
    }
    Operator(control_path, json, operator_command) -> {
      use target <- try_ctl(load_control_target(control_path))
      let control_file = target.control_file
      use resolved_command <- try_ctl(resolve_operator_command(
        control_file,
        deps,
        operator_command,
      ))
      case json {
        True ->
          print_raw_request(
            target,
            protocol.command_request("1", "", resolved_command),
            deps,
            output,
          )
        False ->
          case deps.apply_command(control_file, resolved_command) {
            Ok(result) -> {
              print_command_result(result, output)
              Ok(Nil)
            }
            Error(err) -> Error(client_error(err))
          }
      }
    }
    Cleanup(control_path, root, json, dry_run, yes) ->
      run_cleanup(control_path, root, json, dry_run, yes, output)
    SchedulesStatus(control_path, root, json, job_id) ->
      run_schedules_status(control_path, root, json, job_id, output)
    SchedulesHistory(control_path, root, json, job_id) ->
      run_schedules_history(control_path, root, json, job_id, output)
    SchedulesLogs(control_path, root, json, color, verbose, job_id) ->
      run_schedules_logs(
        control_path,
        root,
        json,
        color,
        verbose,
        deps,
        job_id,
        output,
      )
    SchedulesDoctor(control_path, root, json, job_id) ->
      run_schedules_doctor(control_path, root, json, job_id, output)
    Workstream(command) ->
      case ctl_workstream.run(command, output.line, output.inline) {
        Ok(Nil) -> Ok(Nil)
        Error(#(code, message)) -> Error(Failed(code, message))
      }
    ArtifactPublicationList(control_path, root, json, run_id) ->
      run_artifact_publication_list(control_path, root, json, run_id, output)
    ArtifactPublicationShow(control_path, root, json, run_id, publication_id) ->
      run_artifact_publication_show(
        control_path,
        root,
        json,
        run_id,
        publication_id,
        output,
      )
    ArtifactPublicationRetry(control_path, root, json, run_id, publication_id) ->
      run_artifact_publication_retry(
        control_path,
        root,
        json,
        run_id,
        publication_id,
        output,
      )
    StateStatus(root, json) ->
      run_state_status(resolve_path_option(root), json, output)
    StateArchiveOld(root, json, yes) ->
      run_state_archive_old(resolve_path_option(root), json, yes, output)
    StateDiscardOld(root, json, yes) ->
      run_state_discard_old(resolve_path_option(root), json, yes, output)
    StateReinitialize(root, json, yes) ->
      run_state_reinitialize(resolve_path_option(root), json, yes, output)
    StateRepairRunProvenance(root, json, run_id, dry_run, yes) ->
      run_state_repair_run_provenance(
        resolve_path_option(root),
        json,
        run_id,
        dry_run,
        yes,
        output,
      )
  }
}

fn run_cleanup(
  control_path: Option(String),
  explicit_root: Option(String),
  json_output: Bool,
  dry_run: Bool,
  yes: Bool,
  output: Output,
) -> Result(Nil, Error) {
  use workspace_root <- try_ctl(cleanup_workspace_root(
    control_path,
    explicit_root,
  ))
  let now_ms = local_artifacts.now_ms()
  let result = case dry_run || !yes {
    True -> local_artifacts.inventory(workspace_root, now_ms, True)
    False -> local_artifacts.apply_cleanup(workspace_root, now_ms)
  }
  case json_output {
    True ->
      output.line(
        result |> local_artifacts.cleanup_result_to_json |> json.to_string,
      )
    False -> print_cleanup_result(result, output)
  }
  Ok(Nil)
}

fn cleanup_workspace_root(
  control_path: Option(String),
  explicit_root: Option(String),
) -> Result(String, Error) {
  case explicit_root {
    Some(root) -> Ok(resolve_path_option(root))
    None -> {
      use control_file <- try_ctl(load_control_file(control_path))
      Ok(control_file.workspace_root)
    }
  }
}

fn print_cleanup_result(
  result: local_artifacts.CleanupResult,
  output: Output,
) -> Nil {
  output.line(local_artifacts.cleanup_summary(result))
  output.line("transcript_root_status: " <> result.transcript_root_status)
  output.line("roots:")
  list.each(result.roots, fn(root) { output.line("  " <> root) })
  print_decision_group("would_delete", result.would_delete, output)
  print_decision_group("deleted", result.deleted, output)
  print_decision_group("retained", result.retained, output)
  case result.warnings {
    [] -> Nil
    _ -> {
      output.line("warnings:")
      list.each(result.warnings, fn(warning) { output.line("  " <> warning) })
    }
  }
}

fn print_decision_group(
  name: String,
  decisions: List(local_artifacts.LocalArtifactDecision),
  output: Output,
) -> Nil {
  output.line(name <> ":")
  case decisions {
    [] -> output.line("  -")
    _ ->
      list.each(decisions, fn(decision) {
        output.line(
          "  "
          <> decision.id
          <> " "
          <> event.cleanup_phase_to_string(decision.cleanup_phase)
          <> " "
          <> decision.display_path,
        )
        output.line("    reason: " <> decision.reason)
      })
  }
}

fn run_schedules_status(
  control_path: Option(String),
  explicit_root: Option(String),
  json_output: Bool,
  job_id: Option(String),
  output: Output,
) -> Result(Nil, Error) {
  use root <- try_ctl(schedule_workspace_root(control_path, explicit_root))
  use projected <- try_ctl(schedule_state.load_projection(root, Failed))
  let statuses = case job_id {
    None -> projection.scheduled_statuses(projected)
    Some(id) ->
      case projection.scheduled_status_for(projected, id) {
        Ok(status) -> [status]
        Error(Nil) -> []
      }
  }
  case json_output {
    True ->
      output.line(
        json.object([
          #("schedules", json.array(statuses, of: scheduled_status_to_json)),
        ])
        |> json.to_string,
      )
    False -> print_scheduled_statuses(statuses, output)
  }
  Ok(Nil)
}

fn run_schedules_history(
  control_path: Option(String),
  explicit_root: Option(String),
  json_output: Bool,
  job_id: String,
  output: Output,
) -> Result(Nil, Error) {
  use root <- try_ctl(schedule_workspace_root(control_path, explicit_root))
  use projected <- try_ctl(schedule_state.load_projection(root, Failed))
  let status = projection.scheduled_status_for(projected, job_id)
  case status {
    Error(_) -> Error(Failed("schedule_not_found", "scheduled job not found"))
    Ok(status) -> {
      case json_output {
        True -> output.line(scheduled_status_to_json(status) |> json.to_string)
        False -> print_scheduled_history(status, output)
      }
      Ok(Nil)
    }
  }
}

fn run_schedules_logs(
  control_path: Option(String),
  explicit_root: Option(String),
  json_output: Bool,
  color: style.ColorMode,
  verbose: Bool,
  deps: ControlClient,
  job_id: String,
  output: Output,
) -> Result(Nil, Error) {
  use root <- try_ctl(schedule_workspace_root(control_path, explicit_root))
  use projected <- try_ctl(schedule_state.load_projection(root, Failed))
  use status <- try_ctl(schedule_status_or_error(projected, job_id))
  use run <- try_ctl(current_scheduled_run_or_error(status))
  case json_output {
    True -> {
      output.line(scheduled_log_lookup_to_json(status, run) |> json.to_string)
      Ok(Nil)
    }
    False ->
      case run.session_id {
        Some(session_id) ->
          case load_control_target(control_path) {
            Ok(target) ->
              run_events(
                target,
                deps,
                output,
                Pretty,
                color,
                0,
                verbose,
                session_id,
              )
            Error(err) -> {
              output.line("control_error: " <> error_message(err))
              print_scheduled_transcript_expired(status, run, output)
              Ok(Nil)
            }
          }
        None -> {
          print_scheduled_transcript_expired(status, run, output)
          Ok(Nil)
        }
      }
  }
}

fn run_schedules_doctor(
  control_path: Option(String),
  explicit_root: Option(String),
  json_output: Bool,
  job_id: String,
  output: Output,
) -> Result(Nil, Error) {
  let report = build_schedule_doctor_report(control_path, explicit_root, job_id)
  case json_output {
    True ->
      output.line(schedule_doctor_report_to_json(report) |> json.to_string)
    False -> print_schedule_doctor_report(report, output)
  }
  Ok(Nil)
}

fn build_schedule_doctor_report(
  control_path: Option(String),
  explicit_root: Option(String),
  job_id: String,
) -> ScheduleDoctorReport {
  let config_path = schedule_config_path(explicit_root)
  let config_diagnostics =
    schedule_state.config_diagnostics(config_path, job_id)
  let projection_diagnostics =
    schedule_projection_diagnostics(control_path, explicit_root, job_id)
  ScheduleDoctorReport(
    job_id: job_id,
    config_path: config_path,
    diagnostics: list.append(config_diagnostics, projection_diagnostics),
  )
}

fn schedule_projection_diagnostics(
  control_path: Option(String),
  explicit_root: Option(String),
  job_id: String,
) -> List(schedule_doctor.Diagnostic) {
  case schedule_workspace_root(control_path, explicit_root) {
    Error(err) -> [workspace_root_unavailable_diagnostic(job_id, err)]
    Ok(root) ->
      case schedule_state.load_projection(root, Failed) {
        Error(err) -> [projection_load_failed_diagnostic(root, job_id, err)]
        Ok(projected) ->
          case projection.scheduled_status_for(projected, job_id) {
            Error(Nil) -> [
              schedule_doctor.Diagnostic(
                name: "local_projection",
                severity: schedule_doctor.Pass,
                code: "ok",
                message: "local ledger projection is readable; no scheduled runs are recorded for this job yet",
                fields: [#("job_id", job_id), #("workspace_root", root)],
              ),
            ]
            Ok(status) -> scheduled_projection_status_diagnostics(root, status)
          }
      }
  }
}

fn workspace_root_unavailable_diagnostic(
  job_id: String,
  err: Error,
) -> schedule_doctor.Diagnostic {
  schedule_doctor.Diagnostic(
    name: "local_projection",
    severity: schedule_doctor.Skip,
    code: error_code(err),
    message: "local schedule history was not inspected: " <> error_message(err),
    fields: [#("job_id", job_id)],
  )
}

fn projection_load_failed_diagnostic(
  root: String,
  job_id: String,
  err: Error,
) -> schedule_doctor.Diagnostic {
  let #(code, message) = case err {
    Failed(code, message) -> #(code, message)
    UsageError(message) -> #("usage_error", message)
  }
  schedule_doctor.Diagnostic(
    name: "local_projection",
    severity: schedule_doctor.Warn,
    code: code,
    message: message,
    fields: [#("job_id", job_id), #("workspace_root", root)],
  )
}

fn scheduled_projection_status_diagnostics(
  root: String,
  status: projection.ScheduledJobStatus,
) -> List(schedule_doctor.Diagnostic) {
  let base = [
    schedule_doctor.Diagnostic(
      name: "local_projection",
      severity: schedule_doctor.Pass,
      code: "ok",
      message: "local ledger projection is readable",
      fields: [
        #("job_id", status.job_id),
        #("workspace_root", root),
        #("state", scheduled_state_to_string(status.state)),
      ],
    ),
  ]
  let base = case status.current_run {
    Some(run) -> [
      schedule_doctor.Diagnostic(
        name: "latest_run",
        severity: schedule_doctor.Pass,
        code: "ok",
        message: "latest scheduled run is visible in local history",
        fields: [
          #("job_id", status.job_id),
          #("run_id", run.run_id),
          #("run_status", run.status),
          #("session_id", optional_string(run.session_id)),
          #("run_root", optional_string(run.run_root)),
        ],
      ),
      ..base
    ]
    None -> [
      schedule_doctor.Diagnostic(
        name: "latest_run",
        severity: schedule_doctor.Pass,
        code: "ok",
        message: "no scheduled runs are recorded for this job yet",
        fields: [#("job_id", status.job_id)],
      ),
      ..base
    ]
  }
  let base = case status.failure_issue_id {
    Some(issue_id) -> [
      schedule_doctor.Diagnostic(
        name: "failure_issue",
        severity: schedule_doctor.Pass,
        code: "ok",
        message: "latest scheduled failure report remembered a failure task in Linear",
        fields: [#("job_id", status.job_id), #("linear_issue_id", issue_id)],
      ),
      ..base
    ]
    None -> base
  }
  let base = case status.report_retry {
    Some(retry) -> [
      schedule_doctor.Diagnostic(
        name: "failure_report_retry",
        severity: schedule_doctor.Warn,
        code: retry.error_code,
        message: "failure report retry is pending and will retry without rerunning the workflow",
        fields: [
          #("job_id", status.job_id),
          #("run_id", retry.run_id),
          #("next_retry_at_ms", int.to_string(retry.next_retry_at_ms)),
          #("generation", int.to_string(retry.generation)),
        ],
      ),
      ..base
    ]
    None -> base
  }
  list.reverse(base)
}

fn schedule_config_path(explicit_root: Option(String)) -> Option(String) {
  schedule_config_candidates(explicit_root)
  |> first_existing_file
}

fn schedule_config_candidates(explicit_root: Option(String)) -> List(String) {
  let caller_config = resolve_path_option("scherzo.yaml")
  case explicit_root {
    None -> [caller_config]
    Some(root) -> {
      let root = resolve_path_option(root)
      list.append(
        [path.join(root, "scherzo.yaml")],
        list.append(parent_config_candidates(root), [caller_config]),
      )
    }
  }
}

fn parent_config_candidates(root: String) -> List(String) {
  case path.dirname(root) {
    Ok(parent) -> [path.join(parent, "scherzo.yaml")]
    Error(Nil) -> []
  }
}

fn first_existing_file(paths: List(String)) -> Option(String) {
  case paths {
    [] -> None
    [candidate, ..rest] ->
      case is_file(candidate) {
        True -> Some(candidate)
        False -> first_existing_file(rest)
      }
  }
}

fn is_file(candidate: String) -> Bool {
  case simplifile.is_file(candidate) {
    Ok(True) -> True
    _ -> False
  }
}

fn schedule_status_or_error(
  projected: projection.Projection,
  job_id: String,
) -> Result(projection.ScheduledJobStatus, Error) {
  case projection.scheduled_status_for(projected, job_id) {
    Ok(status) -> Ok(status)
    Error(_) -> Error(Failed("schedule_not_found", "scheduled job not found"))
  }
}

fn current_scheduled_run_or_error(
  status: projection.ScheduledJobStatus,
) -> Result(projection.ScheduledRunSummary, Error) {
  case status.current_run {
    Some(run) -> Ok(run)
    None -> Error(Failed("schedule_no_runs", "scheduled job has no runs"))
  }
}

fn scheduled_log_lookup_to_json(
  status: projection.ScheduledJobStatus,
  run: projection.ScheduledRunSummary,
) -> json.Json {
  json.object([
    #("job_id", json.string(status.job_id)),
    #("run_id", json.string(run.run_id)),
    #("session_id", optional_string_json(run.session_id)),
    #("run_root", optional_string_json(run.run_root)),
    #("status", json.string(run.status)),
  ])
}

fn print_scheduled_transcript_expired(
  status: projection.ScheduledJobStatus,
  run: projection.ScheduledRunSummary,
  output: Output,
) -> Nil {
  output.line("job: " <> status.job_id)
  output.line("run_id: " <> run.run_id)
  output.line("session_id: " <> optional_string(run.session_id))
  output.line("run_root: " <> optional_string(run.run_root))
  output.line(
    "logs: latest scheduled session transcript is not available from the local event hub",
  )
}

fn schedule_doctor_report_to_json(report: ScheduleDoctorReport) -> json.Json {
  json.object([
    #("job_id", json.string(report.job_id)),
    #("config_path", optional_string_json(report.config_path)),
    #(
      "status",
      json.string(
        schedule_doctor.severity_to_string(schedule_doctor.most_severe(
          report.diagnostics,
        )),
      ),
    ),
    #(
      "checks",
      json.array(report.diagnostics, of: schedule_doctor_diagnostic_to_json),
    ),
  ])
}

fn schedule_doctor_diagnostic_to_json(
  diagnostic: schedule_doctor.Diagnostic,
) -> json.Json {
  json.object([
    #("name", json.string(diagnostic.name)),
    #(
      "status",
      json.string(schedule_doctor.severity_to_string(diagnostic.severity)),
    ),
    #("code", json.string(diagnostic.code)),
    #("message", json.string(diagnostic.message)),
    #(
      "fields",
      json.object(
        list.map(diagnostic.fields, fn(field) {
          let #(key, value) = field
          #(key, json.string(value))
        }),
      ),
    ),
  ])
}

fn print_schedule_doctor_report(
  report: ScheduleDoctorReport,
  output: Output,
) -> Nil {
  output.line("schedule doctor: " <> report.job_id)
  output.line("config: " <> optional_string(report.config_path))
  output.line(
    "status: "
    <> schedule_doctor.severity_to_string(schedule_doctor.most_severe(
      report.diagnostics,
    )),
  )
  list.each(report.diagnostics, fn(diagnostic) {
    output.line(
      "- "
      <> schedule_doctor_marker(diagnostic.severity)
      <> " "
      <> diagnostic.name
      <> ": "
      <> diagnostic.message
      <> " ("
      <> diagnostic.code
      <> ")",
    )
    case diagnostic.fields {
      [] -> Nil
      _ ->
        output.line(
          "  fields: "
          <> string.join(
            list.map(diagnostic.fields, fn(field) {
              let #(key, value) = field
              key <> "=" <> value
            }),
            with: " ",
          ),
        )
    }
  })
}

fn schedule_doctor_marker(severity: schedule_doctor.Severity) -> String {
  case severity {
    schedule_doctor.Pass -> "PASS"
    schedule_doctor.Warn -> "WARN"
    schedule_doctor.Fail -> "FAIL"
    schedule_doctor.Skip -> "SKIP"
  }
}

fn schedule_workspace_root(
  control_path: Option(String),
  explicit_root: Option(String),
) -> Result(String, Error) {
  case explicit_root {
    Some(root) -> Ok(resolve_path_option(root))
    None -> {
      use control_file <- try_ctl(load_control_file(control_path))
      Ok(control_file.workspace_root)
    }
  }
}

fn artifact_workspace_root(
  control_path: Option(String),
  explicit_root: Option(String),
) -> Result(String, Error) {
  schedule_workspace_root(control_path, explicit_root)
}

fn run_artifact_publication_list(
  control_path: Option(String),
  explicit_root: Option(String),
  json_output: Bool,
  run_id: String,
  output: Output,
) -> Result(Nil, Error) {
  use root <- try_ctl(artifact_workspace_root(control_path, explicit_root))
  ctl_artifact_publication.list(root, json_output, run_id, output.line)
  |> result.map_error(fn(error) {
    let #(code, message) = error
    Failed(code, message)
  })
}

fn run_artifact_publication_show(
  control_path: Option(String),
  explicit_root: Option(String),
  json_output: Bool,
  run_id: String,
  publication_id: String,
  output: Output,
) -> Result(Nil, Error) {
  use root <- try_ctl(artifact_workspace_root(control_path, explicit_root))
  ctl_artifact_publication.show(
    root,
    json_output,
    run_id,
    publication_id,
    output.line,
  )
  |> result.map_error(fn(error) {
    let #(code, message) = error
    Failed(code, message)
  })
}

fn run_artifact_publication_retry(
  control_path: Option(String),
  explicit_root: Option(String),
  json_output: Bool,
  run_id: String,
  publication_id: String,
  output: Output,
) -> Result(Nil, Error) {
  use root <- try_ctl(artifact_workspace_root(control_path, explicit_root))
  ctl_artifact_publication.retry(
    root,
    json_output,
    run_id,
    publication_id,
    output.line,
  )
  |> result.map_error(fn(error) {
    let #(code, message) = error
    Failed(code, message)
  })
}

fn bool_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn print_scheduled_statuses(
  statuses: List(projection.ScheduledJobStatus),
  output: Output,
) -> Nil {
  case statuses {
    [] -> output.line("No scheduled job history found.")
    _ -> {
      output.line(
        "JOB  WORKFLOW  STATUS  LAST SUCCESS  LAST FAILURE  SKIPPED  RECENT RUNS",
      )
      list.each(statuses, fn(status) {
        output.line(
          status.job_id
          <> "  "
          <> status.workflow_id
          <> "  "
          <> scheduled_state_to_string(status.state)
          <> "  "
          <> optional_ms(status.last_success_at_ms)
          <> "  "
          <> optional_ms(status.last_failure_at_ms)
          <> "  "
          <> int.to_string(scheduled_skipped_total(status))
          <> "  "
          <> string.join(status.recent_run_ids, with: ","),
        )
      })
    }
  }
}

fn print_scheduled_history(
  status: projection.ScheduledJobStatus,
  output: Output,
) -> Nil {
  output.line("job: " <> status.job_id)
  output.line("workflow: " <> status.workflow_id)
  output.line("status: " <> scheduled_state_to_string(status.state))
  output.line("last_due_at: " <> optional_ms(status.last_due_at_ms))
  output.line("last_success_at: " <> optional_ms(status.last_success_at_ms))
  output.line(
    "last_success_run_id: " <> optional_string(status.last_success_run_id),
  )
  output.line("last_failure_at: " <> optional_ms(status.last_failure_at_ms))
  output.line(
    "last_failure_run_id: " <> optional_string(status.last_failure_run_id),
  )
  output.line(
    "last_failure_reason: " <> optional_string(status.last_failure_reason),
  )
  output.line(
    "skipped_overlap_count: " <> int.to_string(status.skipped_overlap_count),
  )
  output.line(
    "skipped_catch_up_count: " <> int.to_string(status.skipped_catch_up_count),
  )
  output.line(
    "skipped_paused_count: " <> int.to_string(status.skipped_paused_count),
  )
  output.line(
    "skipped_capacity_count: " <> int.to_string(status.skipped_capacity_count),
  )
  output.line("failure_issue_id: " <> optional_string(status.failure_issue_id))
  output.line(
    "failure_dedupe_key: " <> optional_string(status.failure_dedupe_key),
  )
  case status.report_retry {
    None -> output.line("report_retry: -")
    Some(report_retry) -> {
      output.line("report_retry: " <> report_retry.run_id)
      output.line("report_retry_error: " <> report_retry.error_code)
      output.line(
        "report_retry_next_retry_at_ms: "
        <> int.to_string(report_retry.next_retry_at_ms),
      )
    }
  }
  output.line(
    "recent_run_ids: " <> string.join(status.recent_run_ids, with: ","),
  )
  case status.current_run {
    None -> output.line("current_run: -")
    Some(run) -> {
      output.line("current_run: " <> run.run_id)
      output.line("current_run_status: " <> run.status)
      output.line("current_run_trigger: " <> run.trigger)
      output.line("current_run_due_at: " <> int.to_string(run.due_at_ms))
      output.line("current_run_attempt: " <> int.to_string(run.attempt))
      output.line("current_run_reason: " <> optional_string(run.reason))
      output.line("current_run_session_id: " <> optional_string(run.session_id))
      output.line("current_run_root: " <> optional_string(run.run_root))
    }
  }
}

fn scheduled_status_to_json(
  status: projection.ScheduledJobStatus,
) -> json.Json {
  json.object([
    #("job_id", json.string(status.job_id)),
    #("workflow_id", json.string(status.workflow_id)),
    #("state", json.string(scheduled_state_to_string(status.state))),
    #("current_run", scheduled_run_to_json(status.current_run)),
    #("last_due_at_ms", optional_int_json(status.last_due_at_ms)),
    #("last_success_at_ms", optional_int_json(status.last_success_at_ms)),
    #("last_success_run_id", optional_string_json(status.last_success_run_id)),
    #("last_failure_at_ms", optional_int_json(status.last_failure_at_ms)),
    #("last_failure_run_id", optional_string_json(status.last_failure_run_id)),
    #("last_failure_reason", optional_string_json(status.last_failure_reason)),
    #("retry_count", json.int(status.retry_count)),
    #("skipped_overlap_count", json.int(status.skipped_overlap_count)),
    #("skipped_catch_up_count", json.int(status.skipped_catch_up_count)),
    #("skipped_paused_count", json.int(status.skipped_paused_count)),
    #("skipped_capacity_count", json.int(status.skipped_capacity_count)),
    #("failure_issue_id", optional_string_json(status.failure_issue_id)),
    #("failure_dedupe_key", optional_string_json(status.failure_dedupe_key)),
    #("report_retry", scheduled_report_retry_to_json(status.report_retry)),
    #("recent_run_ids", json.array(status.recent_run_ids, of: json.string)),
  ])
}

fn scheduled_report_retry_to_json(
  retry: Option(projection.ScheduledReportRetry),
) -> json.Json {
  case retry {
    None -> json.null()
    Some(retry) ->
      json.object([
        #("run_id", json.string(retry.run_id)),
        #("attempt", json.int(retry.attempt)),
        #("dedupe_key", json.string(retry.dedupe_key)),
        #("error_code", json.string(retry.error_code)),
        #("error_message", json.string(retry.error_message)),
        #("next_retry_at_ms", json.int(retry.next_retry_at_ms)),
        #("generation", json.int(retry.generation)),
      ])
  }
}

fn scheduled_run_to_json(
  run: Option(projection.ScheduledRunSummary),
) -> json.Json {
  case run {
    None -> json.null()
    Some(run) ->
      json.object([
        #("run_id", json.string(run.run_id)),
        #("due_at_ms", json.int(run.due_at_ms)),
        #("trigger", json.string(run.trigger)),
        #("attempt", json.int(run.attempt)),
        #("status", json.string(run.status)),
        #("reason", optional_string_json(run.reason)),
        #("session_id", optional_string_json(run.session_id)),
        #("run_root", optional_string_json(run.run_root)),
      ])
  }
}

fn scheduled_skipped_total(status: projection.ScheduledJobStatus) -> Int {
  status.skipped_overlap_count
  + status.skipped_catch_up_count
  + status.skipped_paused_count
  + status.skipped_capacity_count
}

fn scheduled_state_to_string(state: projection.ScheduledRunState) -> String {
  case state {
    projection.ScheduledIdle -> "idle"
    projection.ScheduledDuePending -> "due_pending"
    projection.ScheduledPaused -> "paused"
    projection.ScheduledWaitingForGlobalSlot -> "waiting_for_global_slot"
    projection.ScheduledActive -> "active"
    projection.ScheduledRetryWaiting -> "retry_waiting"
    projection.ScheduledReportRetryWaiting -> "report_retry_waiting"
    projection.ScheduledTerminalSuccess -> "terminal_success"
    projection.ScheduledTerminalFailure -> "terminal_failure"
  }
}

fn optional_ms(value: Option(Int)) -> String {
  case value {
    Some(ms) -> int.to_string(ms)
    None -> "-"
  }
}

fn optional_string(value: Option(String)) -> String {
  case value {
    Some(value) -> value
    None -> "-"
  }
}

fn optional_int_json(value: Option(Int)) -> json.Json {
  case value {
    Some(value) -> json.int(value)
    None -> json.null()
  }
}

fn optional_string_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn run_state_status(
  root: String,
  json_output: Bool,
  output: Output,
) -> Result(Nil, Error) {
  let status = local_artifacts.inspect_state(root)
  case json_output {
    True ->
      output.line(
        status |> local_artifacts.state_status_to_json |> json.to_string,
      )
    False -> print_state_status(status, output)
  }
  Ok(Nil)
}

fn run_state_archive_old(
  root: String,
  json_output: Bool,
  yes: Bool,
  output: Output,
) -> Result(Nil, Error) {
  let result =
    local_artifacts.archive_old_state(root, yes, local_artifacts.now_ms())
  print_state_mutation(result, json_output, output)
  Ok(Nil)
}

fn run_state_discard_old(
  root: String,
  json_output: Bool,
  yes: Bool,
  output: Output,
) -> Result(Nil, Error) {
  let result =
    local_artifacts.discard_old_state(root, yes, local_artifacts.now_ms())
  print_state_mutation(result, json_output, output)
  Ok(Nil)
}

fn run_state_reinitialize(
  root: String,
  json_output: Bool,
  yes: Bool,
  output: Output,
) -> Result(Nil, Error) {
  let result = local_artifacts.reinitialize_state(root, yes: yes)
  print_state_mutation(result, json_output, output)
  Ok(Nil)
}

fn run_state_repair_run_provenance(
  root: String,
  json_output: Bool,
  run_id: String,
  dry_run: Bool,
  yes: Bool,
  output: Output,
) -> Result(Nil, Error) {
  let result = state_repair_run_provenance(root, run_id, dry_run, yes)
  print_state_repair_run_provenance(result, json_output, output)
  Ok(Nil)
}

fn state_repair_run_provenance(
  root: String,
  run_id: String,
  dry_run: Bool,
  yes: Bool,
) -> StateRunProvenanceRepairResult {
  case ledger.path_for_workspace_root(root) {
    Error(error) ->
      rejected_state_repair_result(
        run_id,
        "ledger_path_failed",
        ledger_error_message(error),
      )
    Ok(ledger_path) ->
      case ledger.read_records(ledger_path) {
        Error(error) ->
          rejected_state_repair_result(
            run_id,
            "ledger_read_failed",
            ledger_error_message(error),
          )
        Ok(read) -> {
          let projection_state = projection.fold(read.records)
          case
            workflow_repair.inspect_run_provenance_repair(
              projection_state,
              run_id,
              workflow_repair.state_repair_explicit_mode,
            )
          {
            Error(error) ->
              rejected_state_repair_result(
                run_id,
                workflow_repair.describe_error(error),
                repair_error_message_text(error),
              )
            Ok(workflow_repair.RunProvenanceRepairAlreadyPresent(..)) ->
              StateRunProvenanceRepairResult(
                status: "already_repaired",
                run_id: run_id,
                repair_status: "already_repaired",
                repair_mode: workflow_repair.state_repair_explicit_mode,
                source_evidence: [],
                reason: None,
                message: Some("workflow run provenance is already present"),
              )
            Ok(workflow_repair.RunProvenanceRepairRequired(plan)) ->
              case
                workflow_repair.validate_run_root_for_repair(
                  run_id,
                  plan.run_root,
                  root,
                )
              {
                Error(error) ->
                  rejected_state_repair_result(
                    run_id,
                    workflow_repair.describe_error(error),
                    repair_error_message_text(error),
                  )
                Ok(Nil) ->
                  case dry_run, yes {
                    True, _ ->
                      StateRunProvenanceRepairResult(
                        status: "dry_run",
                        run_id: run_id,
                        repair_status: "would_repair",
                        repair_mode: plan.repair_mode,
                        source_evidence: plan.source_evidence,
                        reason: None,
                        message: Some("workflow run provenance can be repaired"),
                      )
                    _, True ->
                      append_state_repair_run_provenance(ledger_path, plan)
                    _, _ ->
                      rejected_state_repair_result(
                        run_id,
                        "confirmation_required",
                        "pass --dry-run to inspect or --yes to repair",
                      )
                  }
              }
          }
        }
      }
  }
}

fn append_state_repair_run_provenance(
  ledger_path: ledger.LedgerPath,
  plan: workflow_repair.RunProvenanceRepairPlan,
) -> StateRunProvenanceRepairResult {
  let ledger_record =
    record.with_id(
      "workflow-run-provenance-repaired-" <> plan.run_id,
      local_artifacts.now_ms(),
      plan.record_body,
    )
  case ledger.append_idempotent(ledger_path, ledger_record, True) {
    Ok(ledger.Appended) ->
      StateRunProvenanceRepairResult(
        status: "repaired",
        run_id: plan.run_id,
        repair_status: "repaired",
        repair_mode: plan.repair_mode,
        source_evidence: plan.source_evidence,
        reason: None,
        message: Some("workflow run provenance repaired"),
      )
    Ok(ledger.AlreadyRecorded(_)) ->
      StateRunProvenanceRepairResult(
        status: "already_repaired",
        run_id: plan.run_id,
        repair_status: "already_repaired",
        repair_mode: plan.repair_mode,
        source_evidence: plan.source_evidence,
        reason: None,
        message: Some("workflow run provenance repair was already recorded"),
      )
    Error(error) ->
      rejected_state_repair_result(
        plan.run_id,
        "ledger_append_failed",
        append_idempotent_error_message(error),
      )
  }
}

fn repair_error_message_text(error: workflow_repair.RepairError) -> String {
  case workflow_repair.error_message(error) {
    Some(message) -> message
    None -> workflow_repair.describe_error(error)
  }
}

fn rejected_state_repair_result(
  run_id: String,
  reason: String,
  message: String,
) -> StateRunProvenanceRepairResult {
  StateRunProvenanceRepairResult(
    status: "rejected",
    run_id: run_id,
    repair_status: "rejected",
    repair_mode: workflow_repair.state_repair_explicit_mode,
    source_evidence: [],
    reason: Some(reason),
    message: Some(message),
  )
}

fn print_state_repair_run_provenance(
  result: StateRunProvenanceRepairResult,
  json_output: Bool,
  output: Output,
) -> Nil {
  case json_output {
    True ->
      output.line(
        result
        |> state_repair_run_provenance_to_json
        |> json.to_string,
      )
    False -> {
      output.line("state repair-run-provenance " <> result.status)
      output.line("run_id: " <> result.run_id)
      output.line("repair_status: " <> result.repair_status)
      case result.reason {
        Some(reason) -> output.line("reason: " <> reason)
        None -> Nil
      }
      case result.message {
        Some(message) -> output.line("message: " <> message)
        None -> Nil
      }
      case result.source_evidence {
        [] -> Nil
        _ -> {
          output.line("source_evidence:")
          list.each(result.source_evidence, fn(evidence) {
            output.line("  " <> evidence)
          })
        }
      }
    }
  }
}

fn state_repair_run_provenance_to_json(
  result: StateRunProvenanceRepairResult,
) -> json.Json {
  [
    #("command", json.string("state repair-run-provenance")),
    #("status", json.string(result.status)),
    #("run_id", json.string(result.run_id)),
    #("repair_status", json.string(result.repair_status)),
    #("repair_mode", json.string(result.repair_mode)),
    #("source_evidence", json.array(result.source_evidence, of: json.string)),
    #("reason", optional_string_json(result.reason)),
    #("message", optional_string_json(result.message)),
  ]
  |> json.object
}

fn ledger_error_message(error: ledger.LedgerError) -> String {
  ledger.ledger_error_to_string(error)
}

fn append_idempotent_error_message(
  error: ledger.AppendIdempotentError,
) -> String {
  case error {
    ledger.AppendLedgerError(error) -> ledger_error_message(error)
    ledger.RecordIdConflict(record_id) ->
      "ledger record id conflict: " <> record_id
  }
}

fn print_state_status(
  status: local_artifacts.StateStatusResult,
  output: Output,
) -> Nil {
  output.line("state: " <> state_status_name(status.status))
  output.line("message: " <> status.message)
  output.line("workspace_root: " <> status.workspace_root)
  output.line("ledger_dir: " <> status.ledger_dir)
  case status.warnings {
    [] -> Nil
    _ -> {
      output.line("warnings:")
      list.each(status.warnings, fn(warning) { output.line("  " <> warning) })
    }
  }
  case status.status {
    local_artifacts.StateUnsupported(_, _) -> {
      output.line("recovery: old_state_reset_required")
      output.line("safe actions: archive-old, discard-old, reinitialize")
    }
    _ -> output.line("recovery: -")
  }
}

fn state_status_name(status: local_artifacts.StateStatus) -> String {
  case status {
    local_artifacts.StateCurrent -> "current"
    local_artifacts.StateUnsupported(_, _) -> "unsupported"
    local_artifacts.StateCorrupt(_) -> "corrupt"
    local_artifacts.StateMissing -> "missing"
    local_artifacts.StateArchived -> "archived"
  }
}

fn print_state_mutation(
  result: local_artifacts.StateMutationResult,
  json_output: Bool,
  output: Output,
) -> Nil {
  case json_output {
    True ->
      output.line(
        result |> local_artifacts.state_mutation_to_json |> json.to_string,
      )
    False -> {
      output.line(result.action <> " " <> result.status)
      output.line("message: " <> result.message)
      case result.archive_path {
        Some(path) -> output.line("archive_path: " <> path)
        None -> Nil
      }
    }
  }
}

fn run_events(
  target: client.ControlTarget,
  deps: ControlClient,
  output: Output,
  mode: OutputMode,
  color: style.ColorMode,
  since_cursor: Int,
  verbose: Bool,
  session_ref: String,
) -> Result(Nil, Error) {
  let control_file = target.control_file
  use session_id <- try_ctl(resolve_session_ref(control_file, deps, session_ref))
  case mode {
    Json ->
      print_raw_request(
        target,
        protocol.GetEvents("1", "", session_id, since_cursor, 200),
        deps,
        output,
      )
    Raw ->
      case deps.get_events(control_file, session_id, since_cursor, 200) {
        Ok(page) -> {
          list.each(page.events, fn(stored_event) {
            output.line(client.compact_event_line(stored_event))
          })
          Ok(Nil)
        }
        Error(err) -> Error(client_error(err))
      }
    Pretty -> {
      use summary <- try_ctl(require_session(control_file, deps, session_id))
      use replay <- try_ctl(
        fetch_replay_pages(deps, control_file, session_id, since_cursor, 200)
        |> map_client_error,
      )
      let options = pretty_options(color, verbose)
      print_chunks(output, render.render_header(summary, options))
      case replay.truncated {
        True -> print_chunks(output, render.render_truncation_warning(options))
        False -> Nil
      }
      let #(_, chunks) =
        render.render_events(
          render.initial_state(since_cursor),
          replay.events,
          options,
        )
      print_chunks(output, chunks)
      Ok(Nil)
    }
  }
}

fn run_attach(
  control_file: file.ControlFile,
  deps: ControlClient,
  output: Output,
  mode: OutputMode,
  color: style.ColorMode,
  follow: FollowMode,
  since_cursor: Int,
  verbose: Bool,
  session_ref: String,
) -> Result(Nil, Error) {
  use session_id <- try_ctl(resolve_session_ref(control_file, deps, session_ref))
  use summary <- try_ctl(require_session(control_file, deps, session_id))
  use replay <- try_ctl(
    fetch_replay_pages(deps, control_file, session_id, since_cursor, 200)
    |> map_client_error,
  )
  case mode {
    Pretty -> {
      let options = pretty_options(color, verbose)
      print_chunks(output, render.render_header(summary, options))
      case replay.truncated {
        True -> print_chunks(output, render.render_truncation_warning(options))
        False -> Nil
      }
      let #(state, chunks) =
        render.render_events(
          render.initial_state(since_cursor),
          replay.events,
          options,
        )
      print_chunks(output, chunks)
      case follow {
        NoFollow -> Ok(Nil)
        Follow -> {
          let state_key = render_state_key(session_id)
          put_render_state(state_key, state)
          deps.stream_events(
            control_file,
            session_id,
            state.last_cursor,
            fn(stored_event) {
              let current_state = get_render_state(state_key)
              let #(next_state, chunks) =
                render.render_event(current_state, stored_event, options)
              print_chunks(output, chunks)
              put_render_state(state_key, next_state)
              client.Continue
            },
          )
          |> map_client_error
        }
      }
    }
    Raw -> {
      print_replay_raw(output, replay.events)
      follow_raw_or_json(
        control_file,
        deps,
        output,
        follow,
        session_id,
        replay.last_cursor,
        Raw,
      )
    }
    Json -> {
      print_replay_json(output, replay.events)
      follow_raw_or_json(
        control_file,
        deps,
        output,
        follow,
        session_id,
        replay.last_cursor,
        Json,
      )
    }
  }
}

fn follow_raw_or_json(
  control_file: file.ControlFile,
  deps: ControlClient,
  output: Output,
  follow: FollowMode,
  session_id: String,
  last_replay_cursor: Int,
  mode: OutputMode,
) -> Result(Nil, Error) {
  case follow {
    NoFollow -> Ok(Nil)
    Follow -> {
      let state_key = cursor_state_key(session_id, mode)
      put_cursor_state(state_key, last_replay_cursor)
      deps.stream_events(
        control_file,
        session_id,
        last_replay_cursor,
        fn(stored_event) {
          let last_printed_cursor = get_cursor_state(state_key)
          case stored_event.cursor <= last_printed_cursor {
            True -> Nil
            False -> {
              print_raw_or_json_event(output, stored_event, mode)
              put_cursor_state(state_key, stored_event.cursor)
              Nil
            }
          }
          client.Continue
        },
      )
      |> map_client_error
    }
  }
}

fn print_replay_raw(output: Output, events: List(event.SessionEvent)) -> Nil {
  list.each(events, fn(stored_event) {
    output.line(client.compact_event_line(stored_event))
  })
}

fn print_replay_json(output: Output, events: List(event.SessionEvent)) -> Nil {
  list.each(events, fn(stored_event) {
    output.line(protocol.stream_event_to_string("1", stored_event))
  })
}

fn print_raw_or_json_event(
  output: Output,
  stored_event: event.SessionEvent,
  mode: OutputMode,
) -> Nil {
  case mode {
    Json -> output.line(protocol.stream_event_to_string("1", stored_event))
    Raw | Pretty -> output.line(client.compact_event_line(stored_event))
  }
}

fn resolve_session_ref(
  control_file: file.ControlFile,
  deps: ControlClient,
  session_ref: String,
) -> Result(String, Error) {
  case deps.list_sessions(control_file) {
    Error(err) -> Error(client_error(err))
    Ok(snapshot) ->
      case exact_session_match(snapshot.sessions, session_ref) {
        Some(session_id) -> Ok(session_id)
        None -> resolve_display_name_match(snapshot.sessions, session_ref)
      }
  }
}

fn exact_session_match(
  sessions: List(event.SessionSummary),
  session_ref: String,
) -> Option(String) {
  case
    list.filter(sessions, fn(summary) { summary.session_id == session_ref })
  {
    [summary, ..] -> Some(summary.session_id)
    [] -> None
  }
}

fn resolve_display_name_match(
  sessions: List(event.SessionSummary),
  session_ref: String,
) -> Result(String, Error) {
  case
    list.filter(sessions, fn(summary) { summary.display_name == session_ref })
  {
    [] -> Ok(session_ref)
    [summary] -> Ok(summary.session_id)
    [_, ..] -> Error(ambiguous_session_ref(session_ref))
  }
}

fn ambiguous_session_ref(session_ref: String) -> Error {
  Failed(
    "ambiguous_session_ref",
    "session display name \""
      <> session_ref
      <> "\" is ambiguous; use the canonical session_id from scherzoctl ps --json or scherzoctl session <session-id>",
  )
}

fn resolve_operator_command(
  control_file: file.ControlFile,
  deps: ControlClient,
  operator_command: control_command.OperatorCommand,
) -> Result(control_command.OperatorCommand, Error) {
  case operator_command {
    control_command.AbortSession(session_ref) -> {
      use session_id <- try_ctl(resolve_session_ref(
        control_file,
        deps,
        session_ref,
      ))
      Ok(control_command.AbortSession(session_id))
    }
    control_command.StopAfterCurrentTurn(session_ref) -> {
      use session_id <- try_ctl(resolve_session_ref(
        control_file,
        deps,
        session_ref,
      ))
      Ok(control_command.StopAfterCurrentTurn(session_id))
    }
    control_command.PromptSession(session_ref, message) -> {
      use session_id <- try_ctl(resolve_session_ref(
        control_file,
        deps,
        session_ref,
      ))
      Ok(control_command.PromptSession(session_id, message))
    }
    control_command.RespondUi(session_ref, request_id, response) -> {
      use session_id <- try_ctl(resolve_session_ref(
        control_file,
        deps,
        session_ref,
      ))
      Ok(control_command.RespondUi(session_id, request_id, response))
    }
    _ -> Ok(operator_command)
  }
}

fn require_session(
  control_file: file.ControlFile,
  deps: ControlClient,
  session_id: String,
) -> Result(event.SessionSummary, Error) {
  case deps.get_session(control_file, session_id) {
    Ok(Some(summary)) -> Ok(summary)
    Ok(None) -> Error(Failed("missing_session", "session not found"))
    Error(err) -> Error(client_error(err))
  }
}

pub fn fetch_replay_pages(
  deps: ControlClient,
  control_file: file.ControlFile,
  session_id: String,
  since_cursor: Int,
  page_size: Int,
) -> Result(Replay, client.ControlError) {
  fetch_replay_pages_loop(
    deps,
    control_file,
    session_id,
    since_cursor,
    page_size,
    [],
    False,
  )
}

fn fetch_replay_pages_loop(
  deps: ControlClient,
  control_file: file.ControlFile,
  session_id: String,
  cursor: Int,
  page_size: Int,
  acc: List(event.SessionEvent),
  truncated: Bool,
) -> Result(Replay, client.ControlError) {
  use page <- try_client(deps.get_events(
    control_file,
    session_id,
    cursor,
    page_size,
  ))
  let events = list.append(acc, page.events)
  let truncated = truncated || page.truncated
  let count = list.length(page.events)
  case count == 0 || count < page_size || page.next_cursor <= cursor {
    True ->
      Ok(Replay(
        events: events,
        last_cursor: page.next_cursor,
        truncated: truncated,
      ))
    False ->
      fetch_replay_pages_loop(
        deps,
        control_file,
        session_id,
        page.next_cursor,
        page_size,
        events,
        truncated,
      )
  }
}

fn print_chunks(output: Output, chunks: List(render.RenderChunk)) -> Nil {
  list.each(chunks, fn(chunk) {
    case chunk {
      render.Line(text) -> output.line(text)
      render.Inline(text) -> output.inline(text)
    }
  })
}

fn print_raw_request(
  target: client.ControlTarget,
  request: protocol.Request,
  deps: ControlClient,
  output: Output,
) -> Result(Nil, Error) {
  case deps.raw_request(target.control_file, request) {
    Ok(line) -> {
      output.line(client.target_response_line(line, target))
      Ok(Nil)
    }
    Error(err) -> Error(client_error(err))
  }
}

const ps_session_width = 20

const ps_issue_width = 6

const ps_turn_width = 14

const ps_status_width = 11

const ps_recovery_width = 8

fn print_sessions_table(
  sessions: List(event.SessionSummary),
  now_ms: Int,
  output: Output,
) -> Nil {
  output.line(ps_table_row(
    "SESSION",
    "ISSUE",
    "TURN",
    "STATUS",
    "RECOVERY",
    "LAST EVENT",
  ))
  list.each(sessions, fn(summary) {
    output.line(ps_table_row(
      ellipsize_middle(summary.display_name, ps_session_width),
      ellipsize_middle(summary.issue_identifier, ps_issue_width),
      ellipsize_middle(turn_summary_text(summary), ps_turn_width),
      ps_status_to_string(summary.status),
      ps_recovery_to_string(summary.recovery),
      format_last_event_age(now_ms, summary.last_event_at_ms),
    ))
  })
}

fn turn_summary_text(summary: event.SessionSummary) -> String {
  let base = "turn " <> int.to_string(summary.current_turn)
  let with_status = case summary.current_turn_status {
    Some(status) -> base <> " " <> turn_telemetry.status_to_string(status)
    None -> base
  }
  let with_duration = case summary.last_turn_duration_ms {
    Some(duration) -> with_status <> " " <> format_duration(duration)
    None -> with_status
  }
  case summary.last_turn_token_delta.total > 0 {
    True ->
      with_duration
      <> " +"
      <> int.to_string(summary.last_turn_token_delta.total)
      <> " tok"
    False -> with_duration
  }
}

fn format_duration(duration_ms: Int) -> String {
  case duration_ms < 1000 {
    True -> int.to_string(duration_ms) <> "ms"
    False -> {
      let tenths = duration_ms / 100
      let whole = tenths / 10
      let decimal = tenths - whole * 10
      int.to_string(whole) <> "." <> int.to_string(decimal) <> "s"
    }
  }
}

fn print_optional_int(
  label: String,
  value: Option(Int),
  output: Output,
) -> Nil {
  case value {
    Some(value) -> output.line(label <> ": " <> int.to_string(value))
    None -> Nil
  }
}

fn print_token_delta(summary: event.SessionSummary, output: Output) -> Nil {
  case summary.last_turn_token_delta.total > 0 {
    True ->
      output.line(
        "last_turn_token_delta: "
        <> int.to_string(summary.last_turn_token_delta.total),
      )
    False -> Nil
  }
}

fn print_optional_reason(
  reason: Option(turn_telemetry.TurnReason),
  output: Output,
) -> Nil {
  case reason {
    Some(reason) ->
      output.line(
        "last_turn_reason: " <> turn_telemetry.reason_to_string(reason),
      )
    None -> Nil
  }
}

fn ps_status_to_string(status: event.SessionStatus) -> String {
  case status {
    event.Exited(reason) -> ps_exit_reason_to_string(reason)
    _ -> event.status_to_string(status)
  }
}

fn ps_recovery_to_string(recovery: Option(event.RecoveryInfo)) -> String {
  case recovery {
    Some(recovery) -> event.recovery_status_to_string(recovery.status)
    None -> "-"
  }
}

fn ps_exit_reason_to_string(reason: session_reason.WorkerExitReason) -> String {
  case reason {
    session_reason.Normal -> "success"
    session_reason.Failed -> "failed"
    session_reason.WorkerDown -> "worker_down"
    session_reason.OperatorAbort -> "operator_abort"
    session_reason.OperatorStopAfterCurrentTurn -> "op_stop_after"
    session_reason.Stopped -> "stopped"
  }
}

fn ps_table_row(
  session_name: String,
  issue: String,
  turn: String,
  status: String,
  recovery: String,
  last_event: String,
) -> String {
  pad_right(session_name, ps_session_width)
  <> "  "
  <> pad_right(issue, ps_issue_width)
  <> "  "
  <> pad_left(turn, ps_turn_width)
  <> "  "
  <> pad_right(status, ps_status_width)
  <> "  "
  <> pad_right(recovery, ps_recovery_width)
  <> "  "
  <> last_event
}

fn format_last_event_age(now_ms: Int, event_ms: Int) -> String {
  let age_ms = case now_ms - event_ms < 0 {
    True -> 0
    False -> now_ms - event_ms
  }
  let seconds = age_ms / 1000
  case seconds < 60 {
    True -> int.to_string(seconds) <> "s ago"
    False -> {
      let minutes = seconds / 60
      case minutes < 60 {
        True -> int.to_string(minutes) <> "m ago"
        False -> {
          let hours = minutes / 60
          case hours < 24 {
            True -> int.to_string(hours) <> "h ago"
            False -> int.to_string(hours / 24) <> "d ago"
          }
        }
      }
    }
  }
}

fn ellipsize_middle(value: String, max_width: Int) -> String {
  case string.length(value) <= max_width {
    True -> value
    False ->
      case max_width <= 0 {
        True -> ""
        False ->
          case max_width == 1 {
            True -> "…"
            False -> {
              let available_width = max_width - 1
              let prefix_width = available_width / 2
              let suffix_width = available_width - prefix_width
              string.slice(value, 0, prefix_width)
              <> "…"
              <> string.slice(
                value,
                string.length(value) - suffix_width,
                suffix_width,
              )
            }
          }
      }
  }
}

fn pad_right(value: String, width: Int) -> String {
  let padding = width - string.length(value)
  case padding > 0 {
    True -> value <> string.repeat(" ", times: padding)
    False -> value
  }
}

fn pad_left(value: String, width: Int) -> String {
  let padding = width - string.length(value)
  case padding > 0 {
    True -> string.repeat(" ", times: padding) <> value
    False -> value
  }
}

fn print_session(summary: event.SessionSummary, output: Output) -> Nil {
  output.line("display_name: " <> summary.display_name)
  output.line("session_id: " <> summary.session_id)
  output.line(
    "issue: " <> summary.issue_identifier <> " " <> summary.issue_title,
  )
  output.line("status: " <> event.status_to_string(summary.status))
  output.line("turn: " <> turn_summary_text(summary))
  print_optional_int(
    "turn_started_at_ms",
    summary.current_turn_started_at_ms,
    output,
  )
  print_optional_int(
    "last_turn_finished_at_ms",
    summary.last_turn_finished_at_ms,
    output,
  )
  print_optional_int(
    "last_turn_duration_ms",
    summary.last_turn_duration_ms,
    output,
  )
  print_token_delta(summary, output)
  print_optional_reason(summary.last_turn_reason, output)
  output.line("workspace: " <> summary.workspace_path)
  output.line("last_event_at_ms: " <> int.to_string(summary.last_event_at_ms))
  print_recovery_section(summary.recovery, output)
}

fn print_workflow_recovery_history(
  workspace_root: String,
  summary: event.SessionSummary,
  output: Output,
) -> Nil {
  case workflow_recovery_history.load(workspace_root, summary) {
    Ok(history) ->
      workflow_recovery_history.render(history)
      |> list.each(output.line)
    Error(error) ->
      output.line(
        "workflow_step_recovery_history: unavailable ("
        <> workflow_recovery_history.describe_load_error(error)
        <> ")",
      )
  }
}

fn print_recovery_section(
  recovery: Option(event.RecoveryInfo),
  output: Output,
) -> Nil {
  case recovery {
    None -> output.line("recovery: -")
    Some(recovery) -> {
      output.line("recovery:")
      output.line(
        "  status: " <> event.recovery_status_to_string(recovery.status),
      )
      output.line("  source: " <> recovery.source)
      case recovery.message {
        Some(message) -> output.line("  reason: " <> message)
        None -> Nil
      }
      let actions =
        recovery.safe_actions
        |> list.map(event.recovery_action_to_string)
        |> string.join(with: ", ")
      output.line("  safe_actions: " <> actions)
      print_optional(
        "  current_pi_session_id",
        recovery.current_pi_session_id,
        output,
      )
      print_optional("  workflow_run_id", recovery.workflow_run_id, output)
      print_optional("  workflow_step_id", recovery.workflow_step_id, output)
      print_optional(
        "  previous_pi_session_id",
        recovery.previous_pi_session_id,
        output,
      )
      print_optional("  park_reason", recovery.park_reason, output)
      print_optional(
        "  park_release_policy",
        recovery.park_release_policy,
        output,
      )
      print_optional_int("  parked_at_ms", recovery.parked_at_ms, output)
      print_optional("  drift_kind", recovery.drift_kind, output)
      print_optional_int(
        "  retention_until_ms",
        recovery.retention_until_ms,
        output,
      )
      print_optional_int(
        "  cleanup_eligible_at_ms",
        recovery.cleanup_eligible_at_ms,
        output,
      )
      case recovery.cleanup_phase {
        Some(phase) ->
          output.line(
            "  cleanup_phase: " <> event.cleanup_phase_to_string(phase),
          )
        None -> Nil
      }
    }
  }
}

fn print_optional(label: String, value: Option(String), output: Output) -> Nil {
  case value {
    Some(value) -> output.line(label <> ": " <> value)
    None -> Nil
  }
}

fn print_query_status(status: query_types.StatusDto, output: Output) -> Nil {
  output.line("daemon_id: " <> status.daemon_id)
  output.line("boot_id: " <> status.boot_id)
  output.line("dispatch_paused: " <> bool_string(status.dispatch_paused))
  output.line("ui_server_enabled: " <> bool_string(status.ui_server_enabled))
  output.line(
    "supported_queries: " <> string.join(status.supported_queries, with: ", "),
  )
}

fn print_command_result(
  result: control_command.CommandResult,
  output: Output,
) -> Nil {
  let target = case result.target {
    Some(target) -> " target=" <> target
    None -> ""
  }
  let reason = case control_command.status_reason(result.status) {
    Some(reason) -> " reason=" <> reason
    None -> ""
  }
  let message = case result.message {
    Some(message) -> " " <> message
    None -> ""
  }
  output.line(
    result.command
    <> " "
    <> control_command.status_to_string(result.status)
    <> target
    <> reason
    <> message,
  )
}

fn render_state_key(session_id: String) -> String {
  "scherzoctl-render-state:" <> session_id
}

fn cursor_state_key(session_id: String, mode: OutputMode) -> String {
  "scherzoctl-cursor-state:" <> output_mode_name(mode) <> ":" <> session_id
}

fn output_mode_name(mode: OutputMode) -> String {
  case mode {
    Pretty -> "pretty"
    Raw -> "raw"
    Json -> "json"
  }
}

@external(erlang, "erlang", "put")
fn put_render_state(key: String, state: render.RenderState) -> dynamic.Dynamic

@external(erlang, "erlang", "get")
fn get_render_state(key: String) -> render.RenderState

@external(erlang, "erlang", "put")
fn put_cursor_state(key: String, cursor: Int) -> dynamic.Dynamic

@external(erlang, "erlang", "get")
fn get_cursor_state(key: String) -> Int

fn real_control_client() -> ControlClient {
  ControlClient(
    list_sessions: client.list_sessions_snapshot,
    get_session: client.get_session,
    get_events: client.get_events,
    stream_events: client.stream_events,
    query: client.query,
    apply_command: client.apply_command,
    raw_request: client.raw_request,
  )
}

fn real_output() -> Output {
  Output(line: io.println, inline: io.print)
}

fn resolve_path_option(value: String) -> String {
  file.resolve_cli_path(value, file.get_env)
}

fn load_control_target(
  explicit_path: Option(String),
) -> Result(client.ControlTarget, Error) {
  client.discover_target(explicit_path, file.get_env) |> map_file_error
}

fn load_control_file(
  explicit_path: Option(String),
) -> Result(file.ControlFile, Error) {
  use target <- try_ctl(load_control_target(explicit_path))
  Ok(target.control_file)
}

fn map_file_error(
  result: Result(a, file.ControlFileError),
) -> Result(a, Error) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) -> Error(file_error(err))
  }
}

fn map_client_error(
  result: Result(a, client.ControlError),
) -> Result(a, Error) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) -> Error(client_error(err))
  }
}

fn client_error(error: client.ControlError) -> Error {
  Failed(client.error_code(error), client.error_message(error))
}

fn file_error(error: file.ControlFileError) -> Error {
  case error {
    file.ControlFileNotFound(path) ->
      Failed("control_file_not_found", "control file not found: " <> path)
    file.ControlFileReadFailed(_, message) ->
      Failed("control_file_read_failed", message)
    file.ControlFileWriteFailed(_, message) ->
      Failed("control_file_write_failed", message)
    file.ControlFileInvalid(_, message) ->
      Failed("control_file_invalid", message)
    file.ControlFilePermissionFailed(_, message) ->
      Failed("control_file_permission_failed", message)
    file.TokenGenerationFailed(message) ->
      Failed("token_generation_failed", message)
  }
}

pub fn error_code(error: Error) -> String {
  case error {
    UsageError(_) -> "usage_error"
    Failed(code, _) -> code
  }
}

pub fn error_message(error: Error) -> String {
  case error {
    UsageError(message) -> message
    Failed(_, message) -> message
  }
}

fn try_ctl(
  result: Result(a, Error),
  next: fn(a) -> Result(b, Error),
) -> Result(b, Error) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}

fn try_client(
  result: Result(a, client.ControlError),
  next: fn(a) -> Result(b, client.ControlError),
) -> Result(b, client.ControlError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}
