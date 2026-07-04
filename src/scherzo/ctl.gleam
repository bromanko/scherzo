import gleam/dynamic
import gleam/erlang/process
import gleam/int
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
import scherzo/control/query/codec as query_codec
import scherzo/control/query/types as query_types
import scherzo/control/timeout_policy
import scherzo/ctl/artifact_publication as ctl_artifact_publication
import scherzo/ctl/artifact_publication_abandon as ctl_artifact_publication_abandon
import scherzo/ctl/artifact_publication_retry as ctl_artifact_publication_retry
import scherzo/ctl/command_registry
import scherzo/ctl/command_spec
import scherzo/ctl/parser
import scherzo/ctl/renderers as ctl_renderers
import scherzo/ctl/schedules as ctl_schedules
import scherzo/ctl/state_handlers as ctl_state_handlers
import scherzo/ctl/task_output
import scherzo/ctl/timeout_settings
import scherzo/ctl/workstream as ctl_workstream
import scherzo/instance_lock
import scherzo/session/event
import scherzo/state/local_artifacts
import scherzo/terminal/render
import scherzo/terminal/style
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
    states: List(task_output.StateCategory),
    limit: Int,
    cursor: Option(String),
  )
  TaskShow(
    control_file: Option(String),
    json: Bool,
    ref: query_types.TaskQueryRef,
  )
  Outbox(
    control_file: Option(String),
    json: Bool,
    outbox_id: Option(String),
    statuses: List(query_types.OutboxRecordStatus),
    kinds: List(String),
    limit: Int,
    cursor: Option(String),
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
  TaskRetryStartFresh(
    control_file: Option(String),
    json: Bool,
    issue_ref: control_command.IssueRef,
    reason: String,
  )
  RunFinalize(
    control_file: Option(String),
    json: Bool,
    run_id: String,
    reason: String,
    dry_run: Bool,
  )
  Cleanup(
    control_file: Option(String),
    root: Option(String),
    json: Bool,
    dry_run: Bool,
    yes: Bool,
    limit: Option(Int),
    cursor: Option(String),
    max_runtime_ms: Option(Int),
    provider_selection: cleanup.CleanupProviderSelection,
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
  ArtifactPublicationAbandon(
    control_file: Option(String),
    root: Option(String),
    json: Bool,
    run_id: String,
    publication_id: String,
    reason: String,
  )
  StateStatus(root: String, json: Bool)
  StateArchiveOld(root: String, json: Bool, yes: Bool)
  StateDiscardOld(root: String, json: Bool, yes: Bool)
  StateReinitialize(root: String, json: Bool, yes: Bool)
  StateCompact(root: String, json: Bool, dry_run: Bool, yes: Bool)
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
  run_control_args_with_deps_and_env(
    args,
    real_control_client(),
    real_output(),
    io.println_error,
    file.get_env,
  )
}

pub fn offline_main(args: List(String)) -> Result(Nil, Error) {
  run_offline_args_with_deps_and_env(
    args,
    real_control_client(),
    real_output(),
    io.println_error,
    file.get_env,
  )
}

pub fn run_control_args_with_deps_and_env(
  args: List(String),
  deps: ControlClient,
  output: Output,
  error_line: fn(String) -> Nil,
  env: fn(String) -> Option(String),
) -> Result(Nil, Error) {
  use command <- try_ctl(parse(args))
  use settings <- try_ctl(
    timeout_settings.resolve(args, env)
    |> result.map_error(fn(error) {
      case error {
        timeout_settings.InvalidDuration(message) -> UsageError(message)
      }
    }),
  )
  case command_registry.deprecated_alias_hint(args) {
    Some(message) -> error_line(message)
    None -> Nil
  }
  with_timeout_settings(settings, fn() {
    run_with_deps_and_env_internal(command, deps, output, env)
  })
}

pub fn run_offline_args_with_deps_and_env(
  args: List(String),
  deps: ControlClient,
  output: Output,
  error_line: fn(String) -> Nil,
  env: fn(String) -> Option(String),
) -> Result(Nil, Error) {
  let _ = error_line
  case parse_offline(args) {
    Error(error) -> Error(error)
    Ok(Help) -> {
      output.line(offline_usage())
      Ok(Nil)
    }
    Ok(command) -> run_with_deps_and_env(command, deps, output, env)
  }
}

pub fn parse(args: List(String)) -> Result(Command, Error) {
  parse_command(args, command_registry.parse_control, True)
}

pub fn parse_offline(args: List(String)) -> Result(Command, Error) {
  parse_command(args, command_registry.parse_offline, False)
}

pub fn usage() -> String {
  let lines =
    [
      "Usage: scherzo ctl <command> [options]",
      "       scherzoctl <command> [options]",
      "",
      "Local Scherzo daemon inspection and operator controls. Commands:",
    ]
    |> list.append(command_registry.control_usage_lines())
    |> list.append(["", "Options:"])
    |> list.append(command_registry.control_option_usage_lines())

  string.join(lines, with: "\n")
}

pub fn offline_usage() -> String {
  let lines =
    [
      "Usage: scherzo <offline-command> [options]",
      "",
      "Local retained-state commands. Commands:",
    ]
    |> list.append(command_registry.offline_usage_lines())
    |> list.append(["", "Options:"])
    |> list.append(command_registry.offline_option_usage_lines())

  string.join(lines, with: "\n")
}

fn parse_command(
  args: List(String),
  parse_with_registry: fn(List(String)) ->
    Result(
      command_spec.ParseOutcome(command_registry.HandlerKey),
      command_spec.ParseError,
    ),
  control_context: Bool,
) -> Result(Command, Error) {
  case args {
    [] -> Ok(Help)
    _ ->
      case parse_with_registry(args) {
        Ok(command_spec.HelpRequested) -> Ok(Help)
        Ok(command_spec.Parsed(parsed)) -> build_command(parsed)
        Error(error) ->
          Error(UsageError(parse_error_message(error, control_context)))
      }
  }
}

fn parse_error_message(
  error: command_spec.ParseError,
  control_context: Bool,
) -> String {
  let message = command_spec.error_message(error)
  case control_context, message {
    False, "unknown or invalid ctl command: " <> command_name ->
      "unknown or invalid command: " <> command_name
    False, "unknown or invalid ctl command" -> "unknown or invalid command"
    _, _ -> message
  }
}

fn build_command(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(Command, Error) {
  case parsed.handler {
    command_registry.PingKey ->
      Ok(Ping(control_file_option(parsed), json_output(parsed)))
    command_registry.PsKey ->
      Ok(Ps(control_file_option(parsed), json_output(parsed)))
    command_registry.QueryStatusKey ->
      Ok(Query(
        control_file_option(parsed),
        json_output(parsed),
        query_types.Status,
      ))
    command_registry.QueryMetricsKey ->
      Ok(Query(
        control_file_option(parsed),
        json_output(parsed),
        query_types.Metrics,
      ))
    command_registry.QueryOperationStatusKey ->
      Ok(Query(
        control_file_option(parsed),
        json_output(parsed),
        query_types.OperationStatus(
          query_types.OperationStatusQuery(operation_id: first_positional(
            parsed,
            parsed.usage,
          )),
        ),
      ))
    command_registry.TaskListKey ->
      Ok(TaskList(
        control_file_option(parsed),
        json_output(parsed),
        task_states(parsed),
        int_option_with_default(parsed, "--limit", 50),
        command_spec.option_value(parsed, "--cursor"),
      ))
    command_registry.TaskShowKey -> {
      use ref <- try_ctl(
        parser.task_query_ref(first_positional(parsed, parsed.usage))
        |> result.map_error(fn(error) {
          UsageError(parser.error_message(error))
        }),
      )
      Ok(TaskShow(control_file_option(parsed), json_output(parsed), ref))
    }
    command_registry.OutboxKey -> build_outbox_command(parsed)
    command_registry.SessionKey ->
      Ok(Session(
        control_file_option(parsed),
        json_output(parsed),
        first_positional(parsed, parsed.usage),
      ))
    command_registry.EventsKey -> {
      use mode <- try_ctl(parsed_events_mode(parsed))
      Ok(Events(
        control_file_option(parsed),
        mode,
        events_color(mode, parsed_color(parsed)),
        int_option_with_default(parsed, "--since-cursor", 0),
        command_spec.has_flag(parsed, "--verbose"),
        first_positional(parsed, parsed.usage),
      ))
    }
    command_registry.AttachKey -> {
      use mode <- try_ctl(parsed_attach_mode(parsed))
      Ok(Attach(
        control_file_option(parsed),
        mode,
        attach_color(mode, parsed_color(parsed)),
        case command_spec.has_flag(parsed, "--no-follow") {
          True -> NoFollow
          False -> Follow
        },
        int_option_with_default(parsed, "--since-cursor", 0),
        command_spec.has_flag(parsed, "--verbose"),
        first_positional(parsed, parsed.usage),
      ))
    }
    command_registry.PauseKey ->
      Ok(operator_command(parsed, control_command.PauseDispatch))
    command_registry.ResumeKey ->
      Ok(operator_command(parsed, control_command.ResumeDispatch))
    command_registry.ReloadKey ->
      Ok(operator_command(parsed, control_command.ReloadWorkflow))
    command_registry.RetryKey -> build_retry_command(parsed)
    command_registry.RetryStepKey -> build_retry_step_command(parsed)
    command_registry.RecollectOutputsKey ->
      build_recollect_outputs_command(parsed)
    command_registry.RunFinalizeKey -> build_run_finalize_command(parsed)
    command_registry.RecoveryCleanupOrphanStepsKey ->
      build_recovery_cleanup_command(parsed)
    command_registry.ParkKey -> build_park_command(parsed)
    command_registry.UnparkKey ->
      Ok(operator_command(
        parsed,
        control_command.UnparkIssue(
          issue_ref(first_positional(parsed, parsed.usage)),
        ),
      ))
    command_registry.AbortKey -> build_abort_command(parsed)
    command_registry.StopAfterTurnKey -> build_stop_after_turn_command(parsed)
    command_registry.PromptKey ->
      Ok(operator_command(
        parsed,
        control_command.PromptSession(
          first_positional(parsed, parsed.usage),
          second_positional(parsed, parsed.usage),
        ),
      ))
    command_registry.UiRespondKey -> build_ui_respond_command(parsed)
    command_registry.CleanupKey -> build_cleanup_command(parsed)
    command_registry.SchedulesStatusKey ->
      Ok(SchedulesStatus(
        control_file_option(parsed),
        root_option_value(parsed),
        json_output(parsed),
        optional_first_positional(parsed),
      ))
    command_registry.SchedulesHistoryKey ->
      Ok(SchedulesHistory(
        control_file_option(parsed),
        root_option_value(parsed),
        json_output(parsed),
        first_positional(parsed, parsed.usage),
      ))
    command_registry.SchedulesLogsKey -> build_schedules_logs_command(parsed)
    command_registry.SchedulesDoctorKey ->
      Ok(SchedulesDoctor(
        control_file_option(parsed),
        root_option_value(parsed),
        json_output(parsed),
        first_positional(parsed, parsed.usage),
      ))
    command_registry.SchedulesRunKey -> build_schedules_run_command(parsed)
    command_registry.SchedulesReenableKey ->
      Ok(operator_command(
        parsed,
        control_command.ReenableSchedule(first_positional(parsed, parsed.usage)),
      ))
    command_registry.WorkstreamKey ->
      case
        ctl_workstream.parse(
          parsed.positionals,
          control_file_option(parsed),
          root_option_value(parsed),
          json_output(parsed),
        )
      {
        Ok(command) -> Ok(Workstream(command))
        Error(message) -> Error(UsageError(message))
      }
    command_registry.ArtifactPublicationListKey -> {
      use run_id <- try_ctl(required_run_id_from_parsed(parsed))
      Ok(ArtifactPublicationList(
        control_file_option(parsed),
        root_option_value(parsed),
        json_output(parsed),
        run_id,
      ))
    }
    command_registry.ArtifactPublicationShowKey -> {
      use run_id <- try_ctl(required_run_id_from_parsed(parsed))
      use publication_id <- try_ctl(required_publication_id_from_parsed(parsed))
      Ok(ArtifactPublicationShow(
        control_file_option(parsed),
        root_option_value(parsed),
        json_output(parsed),
        run_id,
        publication_id,
      ))
    }
    command_registry.PublicationRetryKey ->
      Ok(operator_command(
        parsed,
        control_command.RetryArtifactPublication(
          first_positional(parsed, parsed.usage),
          command_spec.option_value(parsed, "--publication"),
        ),
      ))
    command_registry.ArtifactPublicationRetryKey -> {
      use run_id <- try_ctl(required_run_id_from_parsed(parsed))
      Ok(ArtifactPublicationRetry(
        control_file_option(parsed),
        root_option_value(parsed),
        json_output(parsed),
        run_id,
        command_spec.option_value(parsed, "--publication"),
      ))
    }
    command_registry.ArtifactPublicationAbandonKey ->
      build_artifact_publication_abandon_command(parsed)
    command_registry.StateStatusKey -> {
      use root <- try_ctl(required_root_from_parsed(parsed))
      Ok(StateStatus(root, json_output(parsed)))
    }
    command_registry.StateArchiveOldKey -> {
      use root <- try_ctl(required_root_from_parsed(parsed))
      Ok(StateArchiveOld(
        root,
        json_output(parsed),
        command_spec.has_flag(parsed, "--yes"),
      ))
    }
    command_registry.StateDiscardOldKey -> {
      use root <- try_ctl(required_root_from_parsed(parsed))
      Ok(StateDiscardOld(
        root,
        json_output(parsed),
        command_spec.has_flag(parsed, "--yes"),
      ))
    }
    command_registry.StateReinitializeKey -> {
      use root <- try_ctl(required_root_from_parsed(parsed))
      Ok(StateReinitialize(
        root,
        json_output(parsed),
        command_spec.has_flag(parsed, "--yes"),
      ))
    }
    command_registry.StateCompactKey -> build_state_compact_command(parsed)
    command_registry.StateRepairRunProvenanceKey ->
      build_state_repair_run_provenance_command(parsed)
  }
}

fn control_file_option(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Option(String) {
  command_spec.option_value(parsed, "--control-file")
}

fn root_option_value(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Option(String) {
  command_spec.option_value(parsed, "--root")
}

fn json_output(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Bool {
  command_spec.has_flag(parsed, "--json")
}

fn parsed_color(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> style.ColorMode {
  case command_spec.option_value(parsed, "--color") {
    Some(value) ->
      case style.parse_color_mode(value) {
        Ok(mode) -> mode
        Error(_) -> style.ColorAuto
      }
    None -> style.ColorAuto
  }
}

fn int_option_with_default(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
  name: String,
  default: Int,
) -> Int {
  case command_spec.option_value(parsed, name) {
    Some(value) ->
      case int.parse(value) {
        Ok(parsed_value) -> parsed_value
        Error(_) -> default
      }
    None -> default
  }
}

fn task_states(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> List(task_output.StateCategory) {
  task_states_loop(command_spec.option_values(parsed, "--state"), [])
}

fn task_states_loop(
  values: List(String),
  acc: List(task_output.StateCategory),
) -> List(task_output.StateCategory) {
  case values {
    [] -> list.reverse(acc)
    [value, ..rest] ->
      case task_output.state_category_from_string(value) {
        Ok(state) -> task_states_loop(rest, [state, ..acc])
        Error(_) -> task_states_loop(rest, acc)
      }
  }
}

fn build_outbox_command(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(Command, Error) {
  let outbox_id = optional_first_positional(parsed)
  case outbox_id, outbox_show_disallowed_option(parsed) {
    Some(_), Some(option) ->
      Error(UsageError("unsupported option for outbox <outbox-id>: " <> option))
    _, _ ->
      Ok(Outbox(
        control_file_option(parsed),
        json_output(parsed),
        outbox_id,
        outbox_statuses(parsed),
        command_spec.option_values(parsed, "--kind"),
        int_option_with_default(parsed, "--limit", 50),
        command_spec.option_value(parsed, "--cursor"),
      ))
  }
}

fn outbox_show_disallowed_option(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Option(String) {
  case command_spec.option_values(parsed, "--status") {
    [_, ..] -> Some("--status")
    [] ->
      case command_spec.option_values(parsed, "--kind") {
        [_, ..] -> Some("--kind")
        [] ->
          case command_spec.option_value(parsed, "--limit") {
            Some(_) -> Some("--limit")
            None ->
              case command_spec.option_value(parsed, "--cursor") {
                Some(_) -> Some("--cursor")
                None -> None
              }
          }
      }
  }
}

fn outbox_statuses(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> List(query_types.OutboxRecordStatus) {
  outbox_statuses_loop(command_spec.option_values(parsed, "--status"), [])
}

fn outbox_statuses_loop(
  values: List(String),
  acc: List(query_types.OutboxRecordStatus),
) -> List(query_types.OutboxRecordStatus) {
  case values {
    [] -> list.reverse(acc)
    [value, ..rest] ->
      case task_output.outbox_status_from_string(value) {
        Ok(status) -> outbox_statuses_loop(rest, [status, ..acc])
        Error(_) -> outbox_statuses_loop(rest, acc)
      }
  }
}

fn first_positional(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
  usage: String,
) -> String {
  case parsed.positionals {
    [value, ..] -> value
    [] -> usage
  }
}

fn second_positional(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
  usage: String,
) -> String {
  case parsed.positionals {
    [_, value, ..] -> value
    _ -> usage
  }
}

fn optional_first_positional(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Option(String) {
  case parsed.positionals {
    [value, ..] -> Some(value)
    [] -> None
  }
}

fn parsed_attach_mode(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(OutputMode, Error) {
  let pretty = command_spec.has_flag(parsed, "--pretty")
  let raw = command_spec.has_flag(parsed, "--raw")
  let json = command_spec.has_flag(parsed, "--json")
  case pretty, raw, json {
    True, True, _ | True, _, True | False, True, True ->
      Error(UsageError("choose only one of --pretty, --raw, or --json"))
    True, False, False -> Ok(Pretty)
    False, True, False -> Ok(Raw)
    False, False, True -> Ok(Json)
    False, False, False -> Ok(Pretty)
  }
}

fn parsed_events_mode(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(OutputMode, Error) {
  let pretty = command_spec.has_flag(parsed, "--pretty")
  let raw = command_spec.has_flag(parsed, "--raw")
  let json = command_spec.has_flag(parsed, "--json")
  case pretty, raw, json {
    True, True, _ | True, _, True | False, True, True ->
      Error(UsageError("choose only one of --pretty, --raw, or --json"))
    True, False, False -> Ok(Pretty)
    False, _, True -> Ok(Json)
    False, _, False -> Ok(Raw)
  }
}

fn operator_command(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
  command: control_command.OperatorCommand,
) -> Command {
  Operator(control_file_option(parsed), json_output(parsed), command)
}

fn build_retry_command(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(Command, Error) {
  let issue = issue_ref(first_positional(parsed, parsed.usage))
  case
    command_spec.has_flag(parsed, "--start-fresh"),
    command_spec.option_value(parsed, "--reason")
  {
    True, Some(reason) ->
      Ok(TaskRetryStartFresh(
        control_file_option(parsed),
        json_output(parsed),
        issue,
        reason,
      ))
    True, None ->
      Error(UsageError("task retry --start-fresh requires --reason <text>"))
    False, Some(_) ->
      Error(UsageError("task retry --reason <text> requires --start-fresh"))
    False, None ->
      Ok(operator_command(parsed, control_command.RetryIssue(issue)))
  }
}

fn build_retry_step_command(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(Command, Error) {
  case parsed.path {
    ["run", "retry-step"] ->
      case command_spec.option_value(parsed, "--step") {
        Some(step_id) ->
          Ok(operator_command(
            parsed,
            control_command.RetryWorkflowStep(
              control_command.RetryWorkflowStepRunId(first_positional(
                parsed,
                parsed.usage,
              )),
              Some(step_id),
            ),
          ))
        None -> Error(UsageError("run retry-step requires --step <step-id>"))
      }
    _ ->
      Ok(operator_command(
        parsed,
        control_command.RetryWorkflowStep(
          retry_workflow_step_target(first_positional(parsed, parsed.usage)),
          command_spec.option_value(parsed, "--step"),
        ),
      ))
  }
}

fn build_recollect_outputs_command(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(Command, Error) {
  case parsed.path {
    ["run", "recollect-outputs"] ->
      Ok(operator_command(
        parsed,
        control_command.RecollectWorkflowOutputs(first_positional(
          parsed,
          parsed.usage,
        )),
      ))
    _ -> {
      use run_id <- try_ctl(
        recollect_outputs_run_id(first_positional(parsed, parsed.usage)),
      )
      Ok(operator_command(
        parsed,
        control_command.RecollectWorkflowOutputs(run_id),
      ))
    }
  }
}

fn build_run_finalize_command(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(Command, Error) {
  let dry_run = command_spec.has_flag(parsed, "--dry-run")
  let yes = command_spec.has_flag(parsed, "--yes")
  case dry_run, yes {
    True, True ->
      Error(UsageError(
        "run finalize requires exactly one of --dry-run or --yes",
      ))
    False, False ->
      Error(UsageError("run finalize requires --dry-run or --yes"))
    _, _ ->
      case
        command_spec.has_flag(parsed, "--validate"),
        command_spec.option_value(parsed, "--outputs"),
        command_spec.has_flag(parsed, "--publish"),
        command_spec.has_flag(parsed, "--update-tracker"),
        command_spec.option_value(parsed, "--reason")
      {
        False, _, _, _, _ ->
          Error(UsageError("run finalize requires --validate"))
        _, None, _, _, _ ->
          Error(UsageError("run finalize requires --outputs auto"))
        _, _, False, _, _ ->
          Error(UsageError("run finalize requires --publish"))
        _, _, _, False, _ ->
          Error(UsageError("run finalize requires --update-tracker"))
        _, _, _, _, None ->
          Error(UsageError("run finalize requires --reason <text>"))
        True, Some(_), True, True, Some(reason) ->
          Ok(RunFinalize(
            control_file_option(parsed),
            json_output(parsed),
            first_positional(parsed, parsed.usage),
            reason,
            dry_run,
          ))
      }
  }
}

fn build_recovery_cleanup_command(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(Command, Error) {
  case
    recovery_cleanup_run_id(first_positional(parsed, parsed.usage)),
    command_spec.has_flag(parsed, "--yes"),
    command_spec.has_flag(parsed, "--dry-run")
  {
    Error(message), _, _ -> Error(UsageError(message))
    Ok(_), True, True ->
      Error(UsageError(
        "recovery cleanup-orphan-steps --yes cannot be combined with --dry-run",
      ))
    Ok(run_id), yes, _ ->
      Ok(operator_command(
        parsed,
        control_command.CleanupOrphanSteps(run_id, !yes),
      ))
  }
}

fn build_park_command(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(Command, Error) {
  case
    command_spec.option_value(parsed, "--reason"),
    command_spec.has_flag(parsed, "--yes")
  {
    Some(reason), True ->
      Ok(operator_command(
        parsed,
        control_command.ParkIssue(
          issue_ref(first_positional(parsed, parsed.usage)),
          reason,
        ),
      ))
    None, _ -> Error(UsageError("park requires --reason <text>"))
    Some(_), False -> Error(UsageError("park requires --yes"))
  }
}

fn build_abort_command(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(Command, Error) {
  case command_spec.has_flag(parsed, "--yes") {
    True ->
      Ok(operator_command(
        parsed,
        control_command.AbortSession(first_positional(parsed, parsed.usage)),
      ))
    False -> Error(UsageError("abort requires --yes"))
  }
}

fn build_stop_after_turn_command(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(Command, Error) {
  case command_spec.has_flag(parsed, "--yes") {
    True ->
      Ok(operator_command(
        parsed,
        control_command.StopAfterCurrentTurn(first_positional(
          parsed,
          parsed.usage,
        )),
      ))
    False -> Error(UsageError("stop-after-turn requires --yes"))
  }
}

fn build_ui_respond_command(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(Command, Error) {
  let session_id = first_positional(parsed, parsed.usage)
  let request_id = second_positional(parsed, parsed.usage)
  case
    command_spec.has_flag(parsed, "--cancel"),
    command_spec.option_value(parsed, "--value")
  {
    True, None ->
      Ok(operator_command(
        parsed,
        control_command.RespondUi(
          session_id,
          request_id,
          control_command.UiCancel,
        ),
      ))
    False, Some(value) ->
      Ok(operator_command(
        parsed,
        control_command.RespondUi(
          session_id,
          request_id,
          control_command.UiValue(value),
        ),
      ))
    True, Some(_) ->
      Error(UsageError("ui respond requires exactly one of --cancel or --value"))
    False, None ->
      Error(UsageError("ui respond requires --cancel or --value <text>"))
  }
}

fn build_cleanup_command(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(Command, Error) {
  use provider_selection <- try_ctl(parsed_cleanup_provider(parsed))
  let limit =
    option_int_value(parsed, "--limit", "--limit requires a positive integer")
  let max_runtime_ms =
    option_int_value(
      parsed,
      "--max-runtime-ms",
      "--max-runtime-ms requires a positive integer",
    )
  let cursor =
    nonempty_option_string_value(
      parsed,
      "--cursor",
      "--cursor requires a non-empty value",
    )
  case limit, max_runtime_ms, cursor {
    Error(err), _, _ | _, Error(err), _ | _, _, Error(err) ->
      Error(UsageError(err))
    Ok(limit), Ok(max_runtime_ms), Ok(cursor) ->
      case
        command_spec.has_flag(parsed, "--yes"),
        command_spec.has_flag(parsed, "--dry-run")
      {
        True, True ->
          Error(UsageError("cleanup --yes cannot be combined with --dry-run"))
        True, False ->
          Ok(Cleanup(
            control_file_option(parsed),
            root_option_value(parsed),
            json_output(parsed),
            False,
            True,
            limit,
            cursor,
            max_runtime_ms,
            provider_selection,
          ))
        False, _ ->
          Ok(Cleanup(
            control_file_option(parsed),
            root_option_value(parsed),
            json_output(parsed),
            True,
            False,
            limit,
            cursor,
            max_runtime_ms,
            provider_selection,
          ))
      }
  }
}

fn parsed_cleanup_provider(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(cleanup.CleanupProviderSelection, Error) {
  case command_spec.option_value(parsed, "--provider") {
    Some(provider) ->
      cleanup.parse_provider_selection(provider)
      |> result.map_error(fn(error) {
        UsageError(cleanup.provider_selection_error_message(error))
      })
    None -> Ok(cleanup.AllProviders)
  }
}

fn build_schedules_logs_command(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(Command, Error) {
  case command_spec.has_flag(parsed, "--last") {
    True ->
      Ok(SchedulesLogs(
        control_file_option(parsed),
        root_option_value(parsed),
        json_output(parsed),
        parsed_color(parsed),
        command_spec.has_flag(parsed, "--verbose"),
        first_positional(parsed, parsed.usage),
      ))
    False -> Error(UsageError("schedules logs requires --last"))
  }
}

fn build_schedules_run_command(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(Command, Error) {
  case command_spec.has_flag(parsed, "--now") {
    True ->
      Ok(operator_command(
        parsed,
        control_command.RunScheduleNow(first_positional(parsed, parsed.usage)),
      ))
    False -> Error(UsageError("schedules run requires --now"))
  }
}

fn required_root_from_parsed(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(String, Error) {
  case root_option_value(parsed) {
    Some(root) -> Ok(root)
    None -> Error(UsageError("state commands require --root <workspace-root>"))
  }
}

fn required_run_id_from_parsed(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(String, Error) {
  case command_spec.option_value(parsed, "--run") {
    Some(run_id) -> Ok(run_id)
    None ->
      Error(UsageError("artifact publication commands require --run <run-id>"))
  }
}

fn required_publication_id_from_parsed(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(String, Error) {
  case command_spec.option_value(parsed, "--publication") {
    Some(publication_id) -> Ok(publication_id)
    None ->
      Error(UsageError(
        "artifact publication show requires --publication <publication-id>",
      ))
  }
}

fn build_artifact_publication_abandon_command(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(Command, Error) {
  case
    command_spec.option_value(parsed, "--reason"),
    command_spec.has_flag(parsed, "--yes")
  {
    Some(reason), True ->
      case
        required_run_id_from_parsed(parsed),
        command_spec.option_value(parsed, "--publication")
      {
        Ok(run_id), Some(publication_id) ->
          Ok(ArtifactPublicationAbandon(
            control_file_option(parsed),
            root_option_value(parsed),
            json_output(parsed),
            run_id,
            publication_id,
            reason,
          ))
        Error(error), _ -> Error(error)
        _, None ->
          Error(UsageError(
            "artifact publication abandon requires --publication <publication-id>",
          ))
      }
    None, _ ->
      Error(UsageError("artifact publication abandon requires --reason <text>"))
    Some(_), False ->
      Error(UsageError("artifact publication abandon requires --yes"))
  }
}

fn build_state_compact_command(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(Command, Error) {
  case
    command_spec.has_flag(parsed, "--yes"),
    command_spec.has_flag(parsed, "--dry-run")
  {
    True, True ->
      Error(UsageError(
        "state compact requires exactly one of --dry-run or --yes",
      ))
    False, False ->
      Error(UsageError("state compact requires --dry-run or --yes"))
    _, _ ->
      case required_root_from_parsed(parsed) {
        Ok(root) ->
          Ok(StateCompact(
            root,
            json_output(parsed),
            command_spec.has_flag(parsed, "--dry-run"),
            command_spec.has_flag(parsed, "--yes"),
          ))
        Error(error) -> Error(error)
      }
  }
}

fn build_state_repair_run_provenance_command(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
) -> Result(Command, Error) {
  let run_target = first_positional(parsed, parsed.usage)
  use run_id <- try_ctl(repair_run_provenance_target(run_target))
  case
    command_spec.has_flag(parsed, "--yes"),
    command_spec.has_flag(parsed, "--dry-run")
  {
    True, True ->
      Error(UsageError(
        "state repair-run-provenance requires exactly one of --dry-run or --yes",
      ))
    False, False ->
      Error(UsageError(
        "state repair-run-provenance requires --dry-run or --yes",
      ))
    _, _ ->
      case required_root_from_parsed(parsed) {
        Ok(root) ->
          Ok(StateRepairRunProvenance(
            root,
            json_output(parsed),
            run_id,
            command_spec.has_flag(parsed, "--dry-run"),
            command_spec.has_flag(parsed, "--yes"),
          ))
        Error(error) -> Error(error)
      }
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

fn recollect_outputs_run_id(target: String) -> Result(String, Error) {
  case string.starts_with(target, "run:") {
    True -> {
      let run_id = string.drop_start(target, 4) |> string.trim
      case run_id == "" {
        True -> Error(UsageError("recollect-outputs requires run:<run-id>"))
        False -> Ok(run_id)
      }
    }
    False -> Error(UsageError("recollect-outputs requires run:<run-id>"))
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

pub fn run_with_deps(
  command: Command,
  deps: ControlClient,
  output: Output,
) -> Result(Nil, Error) {
  run_with_deps_and_env(command, deps, output, file.get_env)
}

pub fn run_with_deps_and_env(
  command: Command,
  deps: ControlClient,
  output: Output,
  env: fn(String) -> Option(String),
) -> Result(Nil, Error) {
  let settings = case timeout_settings.resolve([], env) {
    Ok(settings) -> settings
    Error(_) -> timeout_settings.default_settings()
  }
  with_timeout_settings(settings, fn() {
    run_with_deps_and_env_internal(command, deps, output, env)
  })
}

fn run_with_deps_and_env_internal(
  command: Command,
  deps: ControlClient,
  output: Output,
  env: fn(String) -> Option(String),
) -> Result(Nil, Error) {
  case command {
    Help -> {
      output.line(usage())
      Ok(Nil)
    }
    Ping(control_path, json) -> {
      case json {
        True ->
          run_json_request(
            control_path,
            env,
            deps,
            output,
            protocol.Ping("1", ""),
          )
        False -> {
          use target <- try_ctl(load_control_target(control_path, env))
          let control_file = target.control_file
          case client.ping(control_file) {
            Ok(Nil) -> {
              output.line("ok")
              Ok(Nil)
            }
            Error(err) -> Error(client_error(err))
          }
        }
      }
    }
    Ps(control_path, json) -> {
      case json {
        True ->
          run_json_request(
            control_path,
            env,
            deps,
            output,
            protocol.ListSessions("1", ""),
          )
        False -> {
          use target <- try_ctl(load_control_target(control_path, env))
          let control_file = target.control_file
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
    }
    Query(control_path, json, query) -> {
      case json {
        True ->
          run_json_request(
            control_path,
            env,
            deps,
            output,
            protocol.query_request("1", "", query),
          )
        False -> {
          use target <- try_ctl(load_control_target(control_path, env))
          let control_file = target.control_file
          case deps.query(control_file, query) {
            Ok(query_types.StatusResponse(status)) -> {
              ctl_renderers.print_query_status(status, line: output.line)
              Ok(Nil)
            }
            Ok(query_types.MetricsResponse(metrics)) -> {
              ctl_renderers.print_query_metrics(metrics, line: output.line)
              Ok(Nil)
            }
            Ok(query_types.OperationStatusResponse(operation)) -> {
              case timeout_settings.current_wait() {
                True ->
                  wait_for_operation_status(
                    control_file,
                    deps,
                    operation,
                    output.line,
                  )
                False -> {
                  ctl_renderers.print_operation_status(
                    operation,
                    line: output.line,
                  )
                  Ok(Nil)
                }
              }
            }
            Ok(query_types.TaskListResponse(tasks)) -> {
              task_output.print_list(tasks, output.line)
              Ok(Nil)
            }
            Ok(query_types.TaskShowResponse(task_detail)) -> {
              task_output.print_detail(task_detail, output.line)
              Ok(Nil)
            }
            Ok(query_types.WorkItemListResponse(_))
            | Ok(query_types.WorkItemShowResponse(_))
            | Ok(query_types.WorkflowListResponse(_))
            | Ok(query_types.WorkflowDetailResponse(_)) ->
              Error(Failed(
                "unsupported_query_response",
                "query output is not available yet",
              ))
            Ok(query_types.OutboxListResponse(outbox)) -> {
              task_output.print_outbox_list(outbox, output.line)
              Ok(Nil)
            }
            Ok(query_types.OutboxShowResponse(outbox_record)) -> {
              task_output.print_outbox_record(outbox_record, output.line)
              Ok(Nil)
            }
            Error(err) -> Error(client_error(err))
          }
        }
      }
    }
    TaskList(control_path, json, states, limit, cursor) -> {
      use target <- try_ctl(load_control_target(control_path, env))
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
      use target <- try_ctl(load_control_target(control_path, env))
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
    Outbox(control_path, json, outbox_id, statuses, kinds, limit, cursor) -> {
      use target <- try_ctl(load_control_target(control_path, env))
      let query = case outbox_id {
        Some(outbox_id) ->
          query_types.OutboxShow(query_types.OutboxShowQuery(
            outbox_id: outbox_id,
          ))
        None ->
          query_types.OutboxList(query_types.OutboxListQuery(
            statuses: statuses,
            kinds: kinds,
            limit: limit,
            cursor: cursor,
          ))
      }
      case deps.query(target.control_file, query) {
        Ok(query_types.OutboxListResponse(outbox)) -> {
          case json {
            True -> output.line(task_output.outbox_list_json(outbox))
            False -> task_output.print_outbox_list(outbox, output.line)
          }
          Ok(Nil)
        }
        Ok(query_types.OutboxShowResponse(outbox_record)) -> {
          case json {
            True -> output.line(task_output.outbox_record_json(outbox_record))
            False -> task_output.print_outbox_record(outbox_record, output.line)
          }
          Ok(Nil)
        }
        Ok(_) ->
          Error(Failed("unexpected_query_response", "unexpected query response"))
        Error(err) -> Error(client_error(err))
      }
    }
    Session(control_path, json, session_ref) -> {
      use target <- try_ctl(load_control_target(control_path, env))
      let control_file = target.control_file
      use session_id <- try_ctl(resolve_session_ref(
        control_file,
        deps,
        session_ref,
      ))
      case json {
        True ->
          run_json_request(
            control_path,
            env,
            deps,
            output,
            protocol.GetSession("1", "", session_id),
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
      use target <- try_ctl(load_control_target(control_path, env))
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
      use target <- try_ctl(load_control_target(control_path, env))
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
      use target <- try_ctl(load_control_target(control_path, env))
      let control_file = target.control_file
      use resolved_command <- try_ctl(resolve_operator_command(
        control_file,
        deps,
        operator_command,
      ))
      case json {
        True ->
          run_json_request(
            control_path,
            env,
            deps,
            output,
            protocol.command_request("1", "", resolved_command),
          )
        False ->
          case deps.apply_command(control_file, resolved_command) {
            Ok(result) ->
              handle_command_result_wait(
                control_file,
                deps,
                result,
                output.line,
              )
            Error(err) -> Error(client_error(err))
          }
      }
    }
    TaskRetryStartFresh(control_path, json, issue_ref, reason) -> {
      use target <- try_ctl(load_control_target(control_path, env))
      let operator_command =
        control_command.RetryIssueStartFresh(issue_ref, reason)
      case json {
        True ->
          run_json_request(
            control_path,
            env,
            deps,
            output,
            protocol.command_request("1", "", operator_command),
          )
        False ->
          case deps.apply_command(target.control_file, operator_command) {
            Ok(result) ->
              handle_command_result_wait(
                target.control_file,
                deps,
                result,
                output.line,
              )
            Error(err) -> Error(client_error(err))
          }
      }
    }
    RunFinalize(control_path, json, run_id, reason, dry_run) -> {
      use target <- try_ctl(load_control_target(control_path, env))
      let operator_command =
        control_command.RunFinalize(
          run_id: run_id,
          validate: True,
          outputs: control_command.RunFinalizeOutputsAuto,
          publish: True,
          update_tracker: True,
          dry_run: dry_run,
          reason: reason,
        )
      case json {
        True ->
          run_json_request(
            control_path,
            env,
            deps,
            output,
            protocol.command_request("1", "", operator_command),
          )
        False ->
          case deps.apply_command(target.control_file, operator_command) {
            Ok(result) ->
              handle_command_result_wait(
                target.control_file,
                deps,
                result,
                output.line,
              )
            Error(err) -> Error(client_error(err))
          }
      }
    }
    Cleanup(
      control_path,
      root,
      json,
      dry_run,
      yes,
      limit,
      cursor,
      max_runtime_ms,
      provider_selection,
    ) ->
      run_cleanup(
        control_path,
        root,
        json,
        dry_run,
        yes,
        limit,
        cursor,
        max_runtime_ms,
        provider_selection,
        output,
        env,
      )
    SchedulesStatus(control_path, root, json, job_id) ->
      run_schedules_status(control_path, root, json, job_id, output, env)
    SchedulesHistory(control_path, root, json, job_id) ->
      run_schedules_history(control_path, root, json, job_id, output, env)
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
        env,
      )
    SchedulesDoctor(control_path, root, json, job_id) ->
      run_schedules_doctor(control_path, root, json, job_id, output, env)
    Workstream(command) ->
      case ctl_workstream.run(command, output.line, output.inline) {
        Ok(Nil) -> Ok(Nil)
        Error(#(code, message)) -> Error(Failed(code, message))
      }
    ArtifactPublicationList(control_path, root, json, run_id) ->
      run_artifact_publication_list(
        control_path,
        root,
        json,
        run_id,
        output,
        env,
      )
    ArtifactPublicationShow(control_path, root, json, run_id, publication_id) ->
      run_artifact_publication_show(
        control_path,
        root,
        json,
        run_id,
        publication_id,
        output,
        env,
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
        env,
      )
    ArtifactPublicationAbandon(
      control_path,
      root,
      json,
      run_id,
      publication_id,
      reason,
    ) ->
      run_artifact_publication_abandon(
        control_path,
        root,
        json,
        run_id,
        publication_id,
        reason,
        output,
        env,
      )
    StateStatus(root, json) ->
      ctl_state_handlers.run_status(
        resolve_path_option(root, env),
        json_output: json,
        line: output.line,
      )
      |> result.map_error(pair_error_to_failed)
    StateArchiveOld(root, json, yes) ->
      ctl_state_handlers.run_archive_old(
        resolve_path_option(root, env),
        json_output: json,
        yes: yes,
        line: output.line,
      )
      |> result.map_error(pair_error_to_failed)
    StateDiscardOld(root, json, yes) ->
      ctl_state_handlers.run_discard_old(
        resolve_path_option(root, env),
        json_output: json,
        yes: yes,
        line: output.line,
      )
      |> result.map_error(pair_error_to_failed)
    StateReinitialize(root, json, yes) ->
      ctl_state_handlers.run_reinitialize(
        resolve_path_option(root, env),
        json_output: json,
        yes: yes,
        line: output.line,
      )
      |> result.map_error(pair_error_to_failed)
    StateCompact(root, json, dry_run, yes) ->
      ctl_state_handlers.run_compact(
        resolve_path_option(root, env),
        json_output: json,
        dry_run: dry_run,
        yes: yes,
        line: output.line,
      )
      |> result.map_error(pair_error_to_failed)
    StateRepairRunProvenance(root, json, run_id, dry_run, yes) ->
      ctl_state_handlers.run_repair_run_provenance(
        resolve_path_option(root, env),
        run_id,
        json_output: json,
        dry_run: dry_run,
        yes: yes,
        line: output.line,
      )
      |> result.map_error(pair_error_to_failed)
  }
}

fn with_timeout_settings(
  settings: timeout_settings.Settings,
  run: fn() -> Result(Nil, Error),
) -> Result(Nil, Error) {
  timeout_settings.put_current(settings)
  let result = run()
  timeout_settings.clear_current()
  result
}

fn handle_command_result_wait(
  control_file: file.ControlFile,
  deps: ControlClient,
  result: control_command.CommandResult,
  line: fn(String) -> Nil,
) -> Result(Nil, Error) {
  ctl_renderers.print_command_result(result, line: line)
  case timeout_settings.current_wait(), result.operation_id {
    True, Some(operation_id) ->
      wait_for_operation_id(control_file, deps, operation_id, line)
    _, _ -> Ok(Nil)
  }
}

fn run_json_request(
  control_path: Option(String),
  env: fn(String) -> Option(String),
  deps: ControlClient,
  output: Output,
  request: protocol.Request,
) -> Result(Nil, Error) {
  case client.discover_target(control_path, env) {
    Ok(target) -> print_raw_request(target, request, deps, output)
    Error(err) -> {
      output.line(json_error_response(
        None,
        file_error(err),
        Some(file_discovery_timeout_error(err, request_cli_name(request))),
      ))
      Ok(Nil)
    }
  }
}

fn wait_for_operation_status(
  control_file: file.ControlFile,
  deps: ControlClient,
  operation: query_types.OperationStatusDto,
  line: fn(String) -> Nil,
) -> Result(Nil, Error) {
  ctl_renderers.print_operation_status(operation, line: line)
  case operation.status {
    "completed" | "failed" | "rejected" | "not_found" | "not_allowed" -> Ok(Nil)
    _ -> wait_for_operation_id(control_file, deps, operation.operation_id, line)
  }
}

fn wait_for_operation_id(
  control_file: file.ControlFile,
  deps: ControlClient,
  operation_id: String,
  line: fn(String) -> Nil,
) -> Result(Nil, Error) {
  wait_for_operation_id_loop(
    control_file,
    deps,
    operation_id,
    timeout_settings.current_wait_timeout_ms(),
    line,
  )
}

fn wait_for_operation_id_loop(
  control_file: file.ControlFile,
  deps: ControlClient,
  operation_id: String,
  remaining_ms: Int,
  line: fn(String) -> Nil,
) -> Result(Nil, Error) {
  case
    deps.query(
      control_file,
      query_types.OperationStatus(query_types.OperationStatusQuery(
        operation_id: operation_id,
      )),
    )
  {
    Ok(query_types.OperationStatusResponse(operation)) ->
      case operation.status {
        "completed" | "failed" | "rejected" | "not_found" | "not_allowed" -> {
          ctl_renderers.print_operation_status(operation, line: line)
          Ok(Nil)
        }
        _ ->
          case remaining_ms <= 0 {
            True -> {
              let timeout_error =
                timeout_policy.TimeoutError(
                  phase: timeout_policy.OperationWait,
                  timeout_ms: timeout_settings.current_wait_timeout_ms(),
                  accepted: timeout_policy.AcceptedTrue,
                  retryable: True,
                  message: "Timed out waiting for the accepted operation to finish.",
                  suggested_next_command: Some(operation_status_wait_command(
                    operation_id,
                  )),
                )
              timeout_policy.error_lines(timeout_error)
              |> list.each(line)
              Ok(Nil)
            }
            False -> {
              process.sleep(100)
              wait_for_operation_id_loop(
                control_file,
                deps,
                operation_id,
                remaining_ms - 100,
                line,
              )
            }
          }
      }
    Ok(_) ->
      Error(Failed("unexpected_query_response", "unexpected query response"))
    Error(error) -> Error(client_error(error))
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
  limit: Option(Int),
  cursor: Option(String),
  max_runtime_ms: Option(Int),
  provider_selection: cleanup.CleanupProviderSelection,
  output: Output,
  env: fn(String) -> Option(String),
) -> Result(Nil, Error) {
  use workspace_root <- try_ctl(cleanup_workspace_root(
    control_path,
    explicit_root,
    env,
  ))
  let now_ms = local_artifacts.now_ms()
  let mode = case dry_run || !yes {
    True -> cleanup.DryRun
    False -> cleanup.Apply
  }
  case
    cleanup.run_request_for(
      cleanup.CleanupRequest(
        mode,
        workspace_root,
        now_ms,
        limit,
        cursor,
        max_runtime_ms,
      ),
      provider_selection,
    )
    |> result.map_error(fn(err) {
      let cleanup.CleanupError(code, message) = err
      Failed("cleanup_" <> code, message)
    })
  {
    Ok(result) -> {
      case json_output {
        True ->
          output.line(
            result |> cleanup.cleanup_report_to_json |> json.to_string,
          )
        False -> print_cleanup_result(result, output)
      }
      Ok(Nil)
    }
    Error(err) -> Error(err)
  }
}

fn cleanup_workspace_root(
  control_path: Option(String),
  explicit_root: Option(String),
  env: fn(String) -> Option(String),
) -> Result(String, Error) {
  case explicit_root {
    Some(root) -> Ok(resolve_path_option(root, env))
    None -> {
      use control_file <- try_ctl(load_control_file(control_path, env))
      Ok(control_file.workspace_root)
    }
  }
}

fn print_cleanup_result(result: cleanup.CleanupReport, output: Output) -> Nil {
  output.line(cleanup.cleanup_summary(result))
  list.each(result.providers, fn(provider) {
    output.line("provider: " <> provider.provider_id)
    output.line("  available: " <> bool_to_text(provider.available))
    output.line("  elapsed_ms: " <> int.to_string(provider.elapsed_ms))
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
  case result.limit, result.cursor, result.max_runtime_ms {
    None, None, None -> Nil
    _, _, _ -> {
      output.line("page:")
      output.line("  truncated: " <> bool_to_text(result.truncated))
      output.line(
        "  next_cursor: " <> option_string_or_null(result.next_cursor),
      )
      output.line("  cursor: " <> option_string_or_null(result.cursor))
      output.line("  limit: " <> option_int_or_null(result.limit))
      output.line(
        "  max_runtime_ms: " <> option_int_or_null(result.max_runtime_ms),
      )
      output.line("  scanned: " <> option_int_or_null(result.scanned))
      output.line("  applied: " <> option_int_or_null(result.applied))
      output.line(
        "  truncated_reason: " <> option_string_or_null(result.truncated_reason),
      )
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

fn option_int_value(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
  name: String,
  error_message: String,
) -> Result(Option(Int), String) {
  case command_spec.option_value(parsed, name) {
    Some(value) ->
      case int.parse(value) {
        Ok(parsed_value) if parsed_value > 0 -> Ok(Some(parsed_value))
        Ok(_) | Error(_) -> Error(error_message)
      }
    None -> Ok(None)
  }
}

fn nonempty_option_string_value(
  parsed: command_spec.ParsedCommand(command_registry.HandlerKey),
  name: String,
  error_message: String,
) -> Result(Option(String), String) {
  case command_spec.option_value(parsed, name) {
    Some(value) ->
      case string.trim(value) {
        "" -> Error(error_message)
        trimmed -> Ok(Some(trimmed))
      }
    None -> Ok(None)
  }
}

fn option_string_or_null(value: Option(String)) -> String {
  case value {
    Some(value) -> value
    None -> "null"
  }
}

fn option_int_or_null(value: Option(Int)) -> String {
  case value {
    Some(value) -> int.to_string(value)
    None -> "null"
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
  env: fn(String) -> Option(String),
) -> Result(Nil, Error) {
  use root <- try_ctl(schedule_workspace_root(control_path, explicit_root, env))
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
  env: fn(String) -> Option(String),
) -> Result(Nil, Error) {
  use root <- try_ctl(schedule_workspace_root(control_path, explicit_root, env))
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
  env: fn(String) -> Option(String),
) -> Result(Nil, Error) {
  use root <- try_ctl(schedule_workspace_root(control_path, explicit_root, env))
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
          case load_control_target(control_path, env) {
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
  env: fn(String) -> Option(String),
) -> Result(Nil, Error) {
  ctl_schedules.run_doctor_with_env(
    schedule_workspace_root(control_path, explicit_root, env)
      |> result.map_error(error_to_pair),
    explicit_root,
    job_id,
    json_output: json_output,
    line: output.line,
    env: env,
  )
  |> result.map_error(pair_error_to_failed)
}

fn schedule_workspace_root(
  control_path: Option(String),
  explicit_root: Option(String),
  env: fn(String) -> Option(String),
) -> Result(String, Error) {
  case explicit_root {
    Some(root) -> Ok(resolve_path_option(root, env))
    None -> {
      use control_file <- try_ctl(load_control_file(control_path, env))
      Ok(control_file.workspace_root)
    }
  }
}

fn artifact_workspace_root(
  control_path: Option(String),
  explicit_root: Option(String),
  env: fn(String) -> Option(String),
) -> Result(String, Error) {
  schedule_workspace_root(control_path, explicit_root, env)
}

fn run_artifact_publication_list(
  control_path: Option(String),
  explicit_root: Option(String),
  json_output: Bool,
  run_id: String,
  output: Output,
  env: fn(String) -> Option(String),
) -> Result(Nil, Error) {
  use root <- try_ctl(artifact_workspace_root(control_path, explicit_root, env))
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
  env: fn(String) -> Option(String),
) -> Result(Nil, Error) {
  use root <- try_ctl(artifact_workspace_root(control_path, explicit_root, env))
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

fn run_artifact_publication_abandon(
  control_path: Option(String),
  explicit_root: Option(String),
  json_output: Bool,
  run_id: String,
  publication_id: String,
  reason: String,
  output: Output,
  env: fn(String) -> Option(String),
) -> Result(Nil, Error) {
  use root <- try_ctl(artifact_workspace_root(control_path, explicit_root, env))
  ctl_artifact_publication_abandon.abandon(
    root,
    json_output,
    run_id,
    publication_id,
    reason,
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
  env: fn(String) -> Option(String),
) -> Result(Nil, Error) {
  let _ = deps
  case explicit_root {
    None ->
      Error(UsageError(
        "artifact publication retry requires --root <workspace-root>",
      ))
    Some(_) -> {
      use root <- try_ctl(artifact_workspace_root(
        control_path,
        explicit_root,
        env,
      ))
      use _ <- try_ctl(validate_artifact_retry_root(root))
      use lock <- try_ctl(
        instance_lock.acquire(root)
        |> result.map_error(fn(error) {
          Failed("instance_lock_failed", instance_lock.error_message(error))
        }),
      )
      let result =
        ctl_artifact_publication_retry.retry(
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
      instance_lock.release(lock)
      result
    }
  }
}

fn validate_artifact_retry_root(root: String) -> Result(Nil, Error) {
  let has_state = simplifile.is_directory(root <> "/.scherzo-state") == Ok(True)
  let has_config =
    simplifile.is_file(root <> "/scherzo.yaml") == Ok(True)
    || simplifile.is_file(root <> "/../scherzo.yaml") == Ok(True)
  case has_state || has_config {
    True -> Ok(Nil)
    False ->
      Error(Failed(
        "publication_retry_root_invalid",
        "artifact publication retry requires --root pointing at an existing Scherzo workspace root",
      ))
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
    Ok(line) ->
      print_wait_aware_json_response(target, request, line, deps, output)
    Error(err) -> {
      output.line(json_error_response(
        Some(target),
        client_error(err),
        client.timeout_error_for_request(err, request),
      ))
      Ok(Nil)
    }
  }
}

fn print_wait_aware_json_response(
  target: client.ControlTarget,
  request: protocol.Request,
  line: String,
  deps: ControlClient,
  output: Output,
) -> Result(Nil, Error) {
  case timeout_settings.current_wait() {
    False -> {
      output.line(client.target_response_line(line, target))
      Ok(Nil)
    }
    True ->
      case waitable_operation_id_from_response(request, line) {
        Some(operation_id) ->
          print_wait_operation_json(target, deps, operation_id, output)
        None -> {
          output.line(client.target_response_line(line, target))
          Ok(Nil)
        }
      }
  }
}

fn waitable_operation_id_from_response(
  request: protocol.Request,
  line: String,
) -> Option(String) {
  case protocol.request_operator_command(request) {
    Some(_) ->
      case protocol.decode_command_result_response(line) {
        Ok(result) -> result.operation_id
        Error(_) -> None
      }
    None ->
      case request {
        protocol.Query(_, _, query_types.OperationStatus(_)) ->
          case protocol.decode_response(line) {
            Ok(response) ->
              case response.data {
                Some(data) ->
                  case query_codec.decode_response(json.to_string(data)) {
                    Ok(query_types.OperationStatusResponse(operation)) ->
                      case operation.status {
                        "completed"
                        | "failed"
                        | "rejected"
                        | "not_found"
                        | "not_allowed" -> None
                        _ -> Some(operation.operation_id)
                      }
                    _ -> None
                  }
                None -> None
              }
            Error(_) -> None
          }
        _ -> None
      }
  }
}

fn print_wait_operation_json(
  target: client.ControlTarget,
  deps: ControlClient,
  operation_id: String,
  output: Output,
) -> Result(Nil, Error) {
  wait_for_operation_id_json_loop(
    target,
    deps,
    operation_id,
    timeout_settings.current_wait_timeout_ms(),
    None,
    output,
  )
}

fn wait_for_operation_id_json_loop(
  target: client.ControlTarget,
  deps: ControlClient,
  operation_id: String,
  remaining_ms: Int,
  last_seen: Option(query_types.OperationStatusDto),
  output: Output,
) -> Result(Nil, Error) {
  case
    deps.query(
      target.control_file,
      query_types.OperationStatus(query_types.OperationStatusQuery(
        operation_id: operation_id,
      )),
    )
  {
    Ok(query_types.OperationStatusResponse(operation)) ->
      case operation.status {
        "completed" | "failed" | "rejected" | "not_found" | "not_allowed" -> {
          output.line(json_success_response(
            target,
            protocol.query_data(
              Ok(query_types.OperationStatusResponse(operation)),
            ),
          ))
          Ok(Nil)
        }
        _ ->
          case remaining_ms <= 0 {
            True -> {
              output.line(json_success_response(
                target,
                operation_wait_data(operation),
              ))
              Ok(Nil)
            }
            False -> {
              process.sleep(100)
              wait_for_operation_id_json_loop(
                target,
                deps,
                operation_id,
                remaining_ms - 100,
                Some(operation),
                output,
              )
            }
          }
      }
    Ok(_) ->
      Error(Failed("unexpected_query_response", "unexpected query response"))
    Error(error) ->
      case last_seen {
        Some(operation) if remaining_ms <= 0 -> {
          output.line(json_success_response(
            target,
            operation_wait_data(operation),
          ))
          Ok(Nil)
        }
        _ -> Error(client_error(error))
      }
  }
}

fn operation_wait_data(operation: query_types.OperationStatusDto) -> json.Json {
  let base = [
    #("operation_id", json.string(operation.operation_id)),
    #("status", json.string(operation.status)),
    #("accepted", json.bool(True)),
    #(
      "wait",
      json.object([
        #("timed_out", json.bool(True)),
        #("phase", json.string("operation_wait")),
        #("timeout_ms", json.int(timeout_settings.current_wait_timeout_ms())),
      ]),
    ),
    #(
      "suggested_next_command",
      json.string(operation_status_wait_command(operation.operation_id)),
    ),
  ]
  let with_message = case operation.message {
    Some(message) -> [#("message", json.string(message)), ..base]
    None -> base
  }
  with_message |> list.reverse |> json.object
}

fn operation_status_wait_command(operation_id: String) -> String {
  "scripts/scherzoctl query operation-status "
  <> operation_id
  <> " --json --wait --timeout "
  <> int.to_string(timeout_settings.current_wait_timeout_ms())
  <> "ms"
}

fn request_cli_name(request: protocol.Request) -> String {
  case request {
    protocol.Ping(_, _) -> "ping"
    protocol.ListSessions(_, _) -> "ps"
    protocol.GetSession(_, _, session_id) -> "session " <> session_id
    protocol.GetEvents(_, _, session_id, _, _) -> "events " <> session_id
    protocol.Query(_, _, query_types.Status) -> "query status"
    protocol.Query(_, _, query_types.Metrics) -> "query metrics"
    protocol.Query(_, _, query_types.OperationStatus(query)) ->
      "query operation-status " <> query.operation_id
    protocol.Query(_, _, _) -> "query status"
    _ ->
      case protocol.request_operator_command(request) {
        Some(operator_command) -> operator_command_cli_name(operator_command)
        None -> "ping"
      }
  }
}

fn operator_command_cli_name(
  operator_command: control_command.OperatorCommand,
) -> String {
  let name =
    string.replace(control_command.command_name(operator_command), "_", "-")
  case control_command.command_target(operator_command) {
    Some(target) -> name <> " " <> target
    None -> name
  }
}

fn json_success_response(
  target: client.ControlTarget,
  data: json.Json,
) -> String {
  json.object([
    #("version", json.int(protocol.version)),
    #("id", json.string("1")),
    #("ok", json.bool(True)),
    #("target", client.target_to_json(target)),
    #("data", data),
  ])
  |> json.to_string
}

fn json_error_response(
  target: Option(client.ControlTarget),
  error: Error,
  timeout: Option(timeout_policy.TimeoutError),
) -> String {
  let error_json = case timeout {
    Some(timeout_error) -> timeout_policy.error_json(timeout_error)
    None ->
      json.object([
        #("code", json.string(error_code(error))),
        #("message", json.string(error_message(error))),
      ])
  }
  let target_entries = case target {
    Some(value) -> [#("target", client.target_to_json(value))]
    None -> []
  }
  json.object(
    [
      #("version", json.int(protocol.version)),
      #("id", json.string("1")),
      #("ok", json.bool(False)),
      #("error", error_json),
      ..target_entries
    ]
    |> list.reverse,
  )
  |> json.to_string
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

fn resolve_path_option(
  value: String,
  env: fn(String) -> Option(String),
) -> String {
  file.resolve_cli_path(value, env)
}

fn load_control_target(
  explicit_path: Option(String),
  env: fn(String) -> Option(String),
) -> Result(client.ControlTarget, Error) {
  client.discover_target(explicit_path, env) |> map_file_error
}

fn load_control_file(
  explicit_path: Option(String),
  env: fn(String) -> Option(String),
) -> Result(file.ControlFile, Error) {
  use target <- try_ctl(load_control_target(explicit_path, env))
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
  case client.timeout_error(error, "ping") {
    Some(timeout_error) ->
      Failed(
        "timeout",
        string.join(timeout_policy.error_lines(timeout_error), with: "\n"),
      )
    None -> Failed(client.error_code(error), client.error_message(error))
  }
}

fn file_discovery_timeout_error(
  _error: file.ControlFileError,
  command: String,
) -> timeout_policy.TimeoutError {
  timeout_policy.TimeoutError(
    phase: timeout_policy.ControlFileDiscovery,
    timeout_ms: timeout_settings.current_timeout_ms(),
    accepted: timeout_policy.AcceptedFalse,
    retryable: True,
    message: "Control file could not be found, read, or validated before contacting the daemon.",
    suggested_next_command: Some(
      "scripts/scherzoctl " <> command <> " --json --timeout 10s",
    ),
  )
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
