import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/control/client
import scherzo/control/command as control_command
import scherzo/control/file
import scherzo/control/protocol
import scherzo/session/event

pub type Command {
  Help
  Ping(control_file: Option(String), json: Bool)
  Ps(control_file: Option(String), json: Bool)
  Session(control_file: Option(String), json: Bool, session_id: String)
  Events(control_file: Option(String), json: Bool, session_id: String)
  AttachRaw(control_file: Option(String), json: Bool, session_id: String)
  Operator(
    control_file: Option(String),
    json: Bool,
    command: control_command.OperatorCommand,
  )
}

pub type Error {
  UsageError(message: String)
  Failed(code: String, message: String)
}

type Flags {
  Flags(
    control_file: Option(String),
    json: Bool,
    raw: Bool,
    pretty: Bool,
    yes: Bool,
    reason: Option(String),
    cancel: Bool,
    value: Option(String),
    no_follow: Bool,
    since_cursor: Int,
    color: style.ColorMode,
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

pub fn usage() -> String {
  "Usage: gleam run -- ctl <command> [options]\n\nLocal Scherzo daemon inspection and operator controls. Commands:\n  ping                         Check that the daemon control API is reachable.\n  ps                           List sessions.\n  session <session-id>         Show one session summary.\n  events <session-id>          Replay recent compact event lines.\n  events --pretty <session-id> Replay retained events with human-readable rendering.\n  attach <session-id>          Replay retained events and follow with human-readable rendering.\n  attach --raw <session-id>    Replay and follow compact event lines.\n  attach --json <session-id>   Replay and follow JSON stream event envelopes.\n  attach --raw --json <session-id>\n                               Legacy alias for attach --json.\n  pause                        Pause new dispatch.\n  resume                       Resume new dispatch.\n  reload                       Reload the workflow now.\n  retry <issue>                Retry an issue now.\n  park <issue> --reason <text> --yes\n                               Park an issue until explicitly unparked.\n  unpark <issue>               Unpark an issue.\n  abort <session-id> --yes     Abort a running session.\n  stop-after-turn <session-id> --yes\n                               Stop after the current turn.\n  prompt <session-id> <text>   Queue an operator prompt for a session.\n  ui respond <session-id> <request-id> (--cancel | --value <text>)\n                               Respond to an operator-managed UI request.\n\nOptions:\n  --control-file <path>        Use an explicit control.json path.\n  --raw                        Compact line output for attach/events.\n  --pretty                     Human-readable output for attach/events.\n  --json                       Protocol JSON for non-streaming commands; attach prints one JSON stream object per event.\n  --color=auto|always|never    Color policy for pretty output.\n  --no-follow                  For attach, replay retained events without following live events.\n  --since-cursor <n>           Replay events after cursor n.\n  --yes                        Confirm destructive commands.\n  --reason <text>              Reason for park.\n  --cancel                     Cancel a UI request response.\n  --value <text>               Value for a UI request response.\n  --help, -h                   Show this help."
}

fn parse_flags(args: List(String), flags: Flags) -> Result(Flags, Error) {
  case args {
    [] -> Ok(Flags(..flags, positional: list.reverse(flags.positional)))
    ["--control-file", path, ..rest] ->
      parse_flags(rest, Flags(..flags, control_file: Some(path)))
    ["--control-file"] -> Error(UsageError("--control-file requires a path"))
    ["--json", ..rest] -> parse_flags(rest, Flags(..flags, json: True))
    ["--raw", ..rest] -> parse_flags(rest, Flags(..flags, raw: True))
    ["--pretty", ..rest] -> parse_flags(rest, Flags(..flags, pretty: True))
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
    ["--cancel", ..rest] -> parse_flags(rest, Flags(..flags, cancel: True))
    ["--value", value, ..rest] ->
      parse_flags(rest, Flags(..flags, value: Some(value)))
    ["--value"] -> Error(UsageError("--value requires text"))
    ["--help", ..] | ["-h", ..] -> Ok(Flags(..flags, positional: ["--help"]))
    [arg, ..rest] ->
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

fn command_from(name: String, flags: Flags) -> Result(Command, Error) {
  case name, flags.positional {
    "--help", _ | "-h", _ -> Ok(Help)
    _, ["--help"] -> Ok(Help)
    "ping", [] -> Ok(Ping(flags.control_file, flags.json))
    "ps", [] -> Ok(Ps(flags.control_file, flags.json))
    "session", [session_id] ->
      Ok(Session(flags.control_file, flags.json, session_id))
    "events", [session_id] ->
      Ok(Events(flags.control_file, flags.json, session_id))
    "attach", [session_id] ->
      case flags.raw {
        True -> Ok(AttachRaw(flags.control_file, flags.json, session_id))
        False -> Error(UsageError("attach requires --raw in this phase"))
      }
    "attach", _ -> Error(UsageError("attach usage: attach --raw <session-id>"))
    "pause", [] -> Ok(operator(flags, control_command.PauseDispatch))
    "resume", [] -> Ok(operator(flags, control_command.ResumeDispatch))
    "reload", [] -> Ok(operator(flags, control_command.ReloadWorkflow))
    "retry", [issue] ->
      Ok(operator(flags, control_command.RetryIssue(issue_ref(issue))))
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
    _, _ -> Error(UsageError("unknown or invalid ctl command: " <> name))
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

fn operator(flags: Flags, command: control_command.OperatorCommand) -> Command {
  Operator(flags.control_file, flags.json, command)
}

fn issue_ref(value: String) -> control_command.IssueRef {
  case string.starts_with(value, "id:") {
    True -> control_command.IssueId(string.drop_start(value, 3))
    False -> control_command.IssueIdentifier(value)
  }
}

fn run(command: Command) -> Result(Nil, Error) {
  case command {
    Help -> {
      io.println(usage())
      Ok(Nil)
    }
    Ping(control_path, json) -> {
      use control_file <- try_ctl(load_control_file(control_path))
      case json {
        True -> print_raw(control_file, protocol.Ping("1", ""))
        False ->
          case client.ping(control_file) {
            Ok(Nil) -> {
              io.println("ok")
              Ok(Nil)
            }
            Error(err) -> Error(client_error(err))
          }
      }
    }
    Ps(control_path, json) -> {
      use control_file <- try_ctl(load_control_file(control_path))
      case json {
        True -> print_raw(control_file, protocol.ListSessions("1", ""))
        False ->
          case client.list_sessions(control_file) {
            Ok(sessions) -> {
              print_sessions_table(sessions)
              Ok(Nil)
            }
            Error(err) -> Error(client_error(err))
          }
      }
    }
    Session(control_path, json, session_id) -> {
      use control_file <- try_ctl(load_control_file(control_path))
      case json {
        True ->
          print_raw(control_file, protocol.GetSession("1", "", session_id))
        False ->
          case client.get_session(control_file, session_id) {
            Ok(Some(summary)) -> {
              print_session(summary)
              Ok(Nil)
            }
            Ok(None) -> Error(Failed("missing_session", "session not found"))
            Error(err) -> Error(client_error(err))
          }
      }
    }
    Events(control_path, json, session_id) -> {
      use control_file <- try_ctl(load_control_file(control_path))
      case json {
        True ->
          print_raw(
            control_file,
            protocol.GetEvents("1", "", session_id, 0, 200),
          )
        False ->
          case client.get_events(control_file, session_id, 0, 200) {
            Ok(page) -> {
              list.each(page.events, fn(stored_event) {
                io.println(client.compact_event_line(stored_event))
              })
              Ok(Nil)
            }
            Error(err) -> Error(client_error(err))
          }
      }
    }
    AttachRaw(control_path, json, session_id) -> {
      use control_file <- try_ctl(load_control_file(control_path))
      client.stream_events(control_file, session_id, 0, fn(stored_event) {
        case json {
          True -> io.println(protocol.stream_event_to_string("1", stored_event))
          False -> io.println(client.compact_event_line(stored_event))
        }
        client.Continue
      })
      |> map_client_error
    }
    Operator(control_path, json, operator_command) -> {
      use control_file <- try_ctl(load_control_file(control_path))
      case json {
        True ->
          print_raw_request(
            control_file,
            protocol.command_request("1", "", operator_command),
            deps,
            output,
          )
        False ->
          case client.apply_command(control_file, operator_command) {
            Ok(result) -> {
              print_command_result(result, output)
              Ok(Nil)
            }
            Error(err) -> Error(client_error(err))
          }
      }
    }
  }
}

fn print_raw(
  control_file: file.ControlFile,
  request: protocol.Request,
) -> Result(Nil, Error) {
  case client.raw_request(control_file, request) {
    Ok(line) -> {
      io.println(line)
      Ok(Nil)
    }
    Error(err) -> Error(client_error(err))
  }
}

fn print_sessions_table(sessions: List(event.SessionSummary)) -> Nil {
  io.println("SESSION\tISSUE\tSTATUS\tTURN\tLAST_EVENT")
  list.each(sessions, fn(summary) {
    io.println(
      summary.session_id
      <> "\t"
      <> summary.issue_identifier
      <> "\t"
      <> event.status_to_string(summary.status)
      <> "\t"
      <> int.to_string(summary.current_turn)
      <> "\t"
      <> int.to_string(summary.last_event_at_ms),
    )
  })
}

fn print_session(summary: event.SessionSummary) -> Nil {
  io.println("session_id: " <> summary.session_id)
  io.println(
    "issue: " <> summary.issue_identifier <> " " <> summary.issue_title,
  )
  io.println("status: " <> event.status_to_string(summary.status))
  io.println("turn: " <> int.to_string(summary.current_turn))
  io.println("workspace: " <> summary.workspace_path)
  io.println("last_event_at_ms: " <> int.to_string(summary.last_event_at_ms))
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
    get_session: client.get_session,
    get_events: client.get_events,
    stream_events: client.stream_events,
    raw_request: client.raw_request,
  )
}

fn real_output() -> Output {
  Output(line: io.println, inline: io.print)
}

fn load_control_file(
  explicit_path: Option(String),
) -> Result(file.ControlFile, Error) {
  file.discover(explicit_path, file.get_env) |> map_file_error
}

fn map_file_error(result: Result(a, file.ControlFileError)) -> Result(a, Error) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) -> Error(file_error(err))
  }
}

fn map_client_error(result: Result(a, client.ControlError)) -> Result(a, Error) {
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
