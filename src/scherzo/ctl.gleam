import gleam/dynamic
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
import scherzo/session/reason as session_reason
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
    verbose: Bool,
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
    reason: None,
    cancel: False,
    value: None,
    no_follow: False,
    since_cursor: 0,
    color: style.ColorAuto,
    verbose: False,
    positional: [],
  )
}

pub fn usage() -> String {
  "Usage: gleam run -- ctl <command> [options]\n\nLocal Scherzo daemon inspection and operator controls. Commands:\n  ping                         Check that the daemon control API is reachable.\n  ps                           List sessions (LAST EVENT is daemon-relative age; long session names are shortened).\n  session <session-ref>        Show one session summary.\n  events <session-ref>         Replay recent compact event lines.\n  events --pretty <session-ref>\n                               Replay retained events with human-readable rendering.\n  events --pretty --verbose <session-ref>\n                               Include pi cycle and raw diagnostic lines in pretty replay.\n  attach <session-ref>         Replay retained events and follow with human-readable rendering.\n  attach --verbose <session-ref>\n                               Include pi cycle and raw diagnostic lines in pretty attach.\n  attach --raw <session-ref>   Replay and follow compact event lines.\n  attach --json <session-ref>  Replay and follow JSON stream event envelopes.\n  attach --raw --json <session-ref>\n                               Legacy alias for attach --json.\n  pause                        Pause new dispatch.\n  resume                       Resume new dispatch.\n  reload                       Reload the workflow now.\n  retry <issue>                Retry an issue now.\n  park <issue> --reason <text> --yes\n                               Park an issue until explicitly unparked.\n  unpark <issue>               Unpark an issue.\n  abort <session-ref> --yes    Abort a running session.\n  stop-after-turn <session-ref> --yes\n                               Stop after the current turn.\n  prompt <session-ref> <text>  Queue an operator prompt for a session.\n  ui respond <session-ref> <request-id> (--cancel | --value <text>)\n                               Respond to an operator-managed UI request.\n\nOptions:\n  --control-file <path>        Use an explicit control.json path.\n  --raw                        Compact line output for attach/events.\n  --pretty                     Human-readable output for attach/events.\n  --json                       Protocol JSON for non-streaming commands; attach prints one JSON stream object per event.\n  --color=auto|always|never    Color policy for pretty output.\n  --no-follow                  For attach, replay retained events without following live events.\n  --since-cursor <n>           Replay events after cursor n.\n  --verbose                    Include pi lifecycle and raw diagnostics in pretty attach/events output.\n  --yes                        Confirm destructive commands.\n  --reason <text>              Reason for park.\n  --cancel                     Cancel a UI request response.\n  --value <text>               Value for a UI request response.\n  --help, -h                   Show this help."
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
    ["--verbose", ..rest] -> parse_flags(rest, Flags(..flags, verbose: True))
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

fn command_from(name: String, flags: Flags) -> Result(Command, Error) {
  case name, flags.positional {
    "--help", _ | "-h", _ -> Ok(Help)
    _, ["--help"] -> Ok(Help)
    "ping", [] -> Ok(Ping(flags.control_file, flags.json))
    "ps", [] -> Ok(Ps(flags.control_file, flags.json))
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
      use control_file <- try_ctl(load_control_file(control_path))
      case json {
        True ->
          print_raw_request(control_file, protocol.Ping("1", ""), deps, output)
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
      use control_file <- try_ctl(load_control_file(control_path))
      case json {
        True ->
          print_raw_request(
            control_file,
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
    Session(control_path, json, session_ref) -> {
      use control_file <- try_ctl(load_control_file(control_path))
      use session_id <- try_ctl(resolve_session_ref(
        control_file,
        deps,
        session_ref,
      ))
      case json {
        True ->
          print_raw_request(
            control_file,
            protocol.GetSession("1", "", session_id),
            deps,
            output,
          )
        False ->
          case deps.get_session(control_file, session_id) {
            Ok(Some(summary)) -> {
              print_session(summary, output)
              Ok(Nil)
            }
            Ok(None) -> Error(Failed("missing_session", "session not found"))
            Error(err) -> Error(client_error(err))
          }
      }
    }
    Events(control_path, mode, color, since_cursor, verbose, session_id) -> {
      use control_file <- try_ctl(load_control_file(control_path))
      run_events(
        control_file,
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
      use control_file <- try_ctl(load_control_file(control_path))
      run_attach(
        control_file,
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
      use control_file <- try_ctl(load_control_file(control_path))
      use resolved_command <- try_ctl(resolve_operator_command(
        control_file,
        deps,
        operator_command,
      ))
      case json {
        True ->
          print_raw_request(
            control_file,
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
  }
}

fn run_events(
  control_file: file.ControlFile,
  deps: ControlClient,
  output: Output,
  mode: OutputMode,
  color: style.ColorMode,
  since_cursor: Int,
  verbose: Bool,
  session_ref: String,
) -> Result(Nil, Error) {
  use session_id <- try_ctl(resolve_session_ref(control_file, deps, session_ref))
  case mode {
    Json ->
      print_raw_request(
        control_file,
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
  control_file: file.ControlFile,
  request: protocol.Request,
  deps: ControlClient,
  output: Output,
) -> Result(Nil, Error) {
  case deps.raw_request(control_file, request) {
    Ok(line) -> {
      output.line(line)
      Ok(Nil)
    }
    Error(err) -> Error(client_error(err))
  }
}

const ps_session_width = 34

const ps_issue_width = 10

const ps_turn_width = 4

const ps_status_width = 14

fn print_sessions_table(
  sessions: List(event.SessionSummary),
  now_ms: Int,
  output: Output,
) -> Nil {
  output.line(ps_table_row("SESSION", "ISSUE", "TURN", "STATUS", "LAST EVENT"))
  list.each(sessions, fn(summary) {
    output.line(ps_table_row(
      ellipsize_middle(summary.display_name, ps_session_width),
      ellipsize_middle(summary.issue_identifier, ps_issue_width),
      int.to_string(summary.current_turn),
      ps_status_to_string(summary.status),
      format_last_event_age(now_ms, summary.last_event_at_ms),
    ))
  })
}

fn ps_status_to_string(status: event.SessionStatus) -> String {
  case status {
    event.Exited(reason) -> ps_exit_reason_to_string(reason)
    _ -> event.status_to_string(status)
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
  output.line("turn: " <> int.to_string(summary.current_turn))
  output.line("workspace: " <> summary.workspace_path)
  output.line("last_event_at_ms: " <> int.to_string(summary.last_event_at_ms))
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
    apply_command: client.apply_command,
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
