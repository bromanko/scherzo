import gleam/dynamic
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/cleanup
import scherzo/control/client
import scherzo/control/command as control_command
import scherzo/control/file
import scherzo/control/protocol
import scherzo/control/query/types as query_types
import scherzo/ctl/artifact_publication as ctl_artifact_publication
import scherzo/ctl/parser
import scherzo/ctl/renderers as ctl_renderers
import scherzo/ctl/schedules as ctl_schedules
import scherzo/ctl/state_handlers as ctl_state_handlers
import scherzo/ctl/task_output
import scherzo/ctl/usage as ctl_usage
import scherzo/ctl/workstream as ctl_workstream
import scherzo/session/event
import scherzo/state/local_artifacts
import scherzo/terminal/render
import scherzo/terminal/style

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
    states: List(task_output.StateCategory),
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
    publication_id: Option(String),
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
      case parser.parse_flags(rest, parser.default_flags()) {
        Error(error) -> Error(UsageError(parser.error_message(error)))
        Ok(flags) -> command_from(name, flags)
      }
  }
}

pub fn usage() -> String {
  ctl_usage.text()
}

fn option_with_default(value: Option(a), default: a) -> a {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn command_from(name: String, flags: parser.Flags) -> Result(Command, Error) {
  case name, flags.positional {
    "--help", _ | "-h", _ -> Ok(Help)
    _, ["--help"] -> Ok(Help)
    "ping", [] -> Ok(Ping(flags.control_file, flags.json))
    "ps", [] -> Ok(Ps(flags.control_file, flags.json))
    "query", ["status"] ->
      Ok(Query(flags.control_file, flags.json, query_types.Status))
    "query", ["metrics"] ->
      Ok(Query(flags.control_file, flags.json, query_types.Metrics))
    "task", ["list"] ->
      Ok(TaskList(
        flags.control_file,
        flags.json,
        flags.state_filters,
        option_with_default(flags.limit, 50),
        flags.cursor,
      ))
    "task", ["show", ref] -> {
      use ref <- try_ctl(
        parser.task_query_ref(ref)
        |> result.map_error(fn(error) {
          UsageError(parser.error_message(error))
        }),
      )
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
      Ok(ArtifactPublicationRetry(
        flags.control_file,
        flags.root,
        flags.json,
        run_id,
        flags.publication_id,
      ))
    }
    "artifact", _ ->
      Error(UsageError(
        "artifact usage: artifact publication list --run <run-id> | artifact publication show --run <run-id> --publication <publication-id> | artifact publication retry --run <run-id> [--publication <publication-id>]",
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

fn required_root(flags: parser.Flags) -> Result(String, Error) {
  case flags.root {
    Some(root) -> Ok(root)
    None -> Error(UsageError("state commands require --root <workspace-root>"))
  }
}

fn required_run_id(flags: parser.Flags) -> Result(String, Error) {
  case flags.run_id {
    Some(run_id) -> Ok(run_id)
    None ->
      Error(UsageError("artifact publication commands require --run <run-id>"))
  }
}

fn required_publication_id(flags: parser.Flags) -> Result(String, Error) {
  case flags.publication_id {
    Some(publication_id) -> Ok(publication_id)
    None ->
      Error(UsageError(
        "artifact publication show requires --publication <publication-id>",
      ))
  }
}

fn attach_mode(flags: parser.Flags) -> Result(OutputMode, Error) {
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

fn events_mode(flags: parser.Flags) -> Result(OutputMode, Error) {
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

fn operator(
  flags: parser.Flags,
  command: control_command.OperatorCommand,
) -> Command {
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
              ctl_renderers.print_sessions_table(
                snapshot.sessions,
                snapshot.now_ms,
                line: output.line,
              )
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
              ctl_renderers.print_query_status(status, line: output.line)
              Ok(Nil)
            }
            Ok(query_types.MetricsResponse(metrics)) -> {
              ctl_renderers.print_query_metrics(metrics, line: output.line)
              Ok(Nil)
            }
            Ok(query_types.TaskListResponse(tasks)) -> {
              task_output.print_list(tasks, output.line)
              Ok(Nil)
            }
            Ok(query_types.TaskShowResponse(task_detail)) -> {
              task_output.print_detail(task_detail, output.line)
              Ok(Nil)
            }
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
              ctl_renderers.print_session(summary, line: output.line)
              ctl_renderers.print_workflow_recovery_history(
                control_file.workspace_root,
                summary,
                line: output.line,
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
              ctl_renderers.print_command_result(result, line: output.line)
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
        deps,
        run_id,
        publication_id,
        output,
      )
    StateStatus(root, json) ->
      ctl_state_handlers.run_status(
        resolve_path_option(root),
        json_output: json,
        line: output.line,
      )
      |> result.map_error(pair_error_to_failed)
    StateArchiveOld(root, json, yes) ->
      ctl_state_handlers.run_archive_old(
        resolve_path_option(root),
        json_output: json,
        yes: yes,
        line: output.line,
      )
      |> result.map_error(pair_error_to_failed)
    StateDiscardOld(root, json, yes) ->
      ctl_state_handlers.run_discard_old(
        resolve_path_option(root),
        json_output: json,
        yes: yes,
        line: output.line,
      )
      |> result.map_error(pair_error_to_failed)
    StateReinitialize(root, json, yes) ->
      ctl_state_handlers.run_reinitialize(
        resolve_path_option(root),
        json_output: json,
        yes: yes,
        line: output.line,
      )
      |> result.map_error(pair_error_to_failed)
    StateRepairRunProvenance(root, json, run_id, dry_run, yes) ->
      ctl_state_handlers.run_repair_run_provenance(
        resolve_path_option(root),
        run_id,
        json_output: json,
        dry_run: dry_run,
        yes: yes,
        line: output.line,
      )
      |> result.map_error(pair_error_to_failed)
  }
}

fn pair_error_to_failed(error: #(String, String)) -> Error {
  let #(code, message) = error
  Failed(code, message)
}

fn error_to_pair(error: Error) -> #(String, String) {
  #(error_code(error), error_message(error))
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
    True -> cleanup.inventory(workspace_root, now_ms)
    False -> cleanup.apply(workspace_root, now_ms)
  }
  case json_output {
    True ->
      output.line(result |> cleanup.cleanup_report_to_json |> json.to_string)
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

fn print_cleanup_result(result: cleanup.CleanupReport, output: Output) -> Nil {
  output.line(cleanup.cleanup_summary(result))
  list.each(result.providers, fn(provider) {
    output.line("provider: " <> provider.provider_id)
    output.line("  available: " <> bool_to_text(provider.available))
    output.line("  transcript_root_status: " <> provider.transcript_root_status)
    output.line("  roots:")
    case provider.roots {
      [] -> output.line("    -")
      roots -> list.each(roots, fn(root) { output.line("    " <> root) })
    }
    print_cleanup_items(provider.items, output)
    case provider.warnings {
      [] -> Nil
      warnings -> {
        output.line("  warnings:")
        list.each(warnings, fn(warning) { output.line("    " <> warning) })
      }
    }
  })
  case result.warnings {
    [] -> Nil
    warnings -> {
      output.line("warnings:")
      list.each(warnings, fn(warning) { output.line("  " <> warning) })
    }
  }
}

fn print_cleanup_items(
  items: List(cleanup.CleanupItemReport),
  output: Output,
) -> Nil {
  output.line("  items:")
  case items {
    [] -> output.line("    -")
    _ ->
      list.each(items, fn(item) {
        output.line(
          "    "
          <> item.status
          <> " "
          <> item.item_id
          <> " "
          <> item.display_path,
        )
        output.line("      intended_action: " <> item.intended_action)
        output.line("      reason: " <> item.reason)
      })
  }
}

fn bool_to_text(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
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
  ctl_schedules.run_status(
    root,
    job_id,
    json_output: json_output,
    line: output.line,
  )
  |> result.map_error(pair_error_to_failed)
}

fn run_schedules_history(
  control_path: Option(String),
  explicit_root: Option(String),
  json_output: Bool,
  job_id: String,
  output: Output,
) -> Result(Nil, Error) {
  use root <- try_ctl(schedule_workspace_root(control_path, explicit_root))
  ctl_schedules.run_history(
    root,
    job_id,
    json_output: json_output,
    line: output.line,
  )
  |> result.map_error(pair_error_to_failed)
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
  use projected <- try_ctl(
    ctl_schedules.load_projection(root)
    |> result.map_error(pair_error_to_failed),
  )
  use status <- try_ctl(
    ctl_schedules.status_or_error(projected, job_id)
    |> result.map_error(pair_error_to_failed),
  )
  use run <- try_ctl(
    ctl_schedules.current_run_or_error(status)
    |> result.map_error(pair_error_to_failed),
  )
  case json_output {
    True -> {
      output.line(ctl_schedules.log_lookup_json(status, run))
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
              ctl_schedules.print_transcript_expired(
                status,
                run,
                line: output.line,
              )
              Ok(Nil)
            }
          }
        None -> {
          ctl_schedules.print_transcript_expired(status, run, line: output.line)
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
  ctl_schedules.run_doctor(
    schedule_workspace_root(control_path, explicit_root)
      |> result.map_error(error_to_pair),
    explicit_root,
    job_id,
    json_output: json_output,
    line: output.line,
  )
  |> result.map_error(pair_error_to_failed)
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
  deps: ControlClient,
  run_id: String,
  publication_id: Option(String),
  output: Output,
) -> Result(Nil, Error) {
  case explicit_root {
    None -> {
      use target <- try_ctl(load_control_target(control_path))
      let operator_command =
        control_command.RetryArtifactPublication(run_id, publication_id)
      case json_output {
        True ->
          print_raw_request(
            target,
            protocol.command_request("1", "", operator_command),
            deps,
            output,
          )
        False ->
          case deps.apply_command(target.control_file, operator_command) {
            Ok(result) -> {
              ctl_renderers.print_command_result(result, line: output.line)
              Ok(Nil)
            }
            Error(err) -> Error(client_error(err))
          }
      }
    }
    Some(_) -> {
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
