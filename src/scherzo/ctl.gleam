import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/control/client
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
      case parse_flags(rest, Flags(None, False, False, [])) {
        Error(err) -> Error(err)
        Ok(flags) -> command_from(name, flags)
      }
  }
}

pub fn usage() -> String {
  "Usage: gleam run -- ctl <command> [options]\n\nRead-only local Scherzo daemon inspection. Commands:\n  ping                         Check that the daemon control API is reachable.\n  ps                           List sessions.\n  session <session-id>         Show one session summary.\n  events <session-id>          Replay recent events for a session.\n  attach --raw <session-id>    Replay and follow raw event lines.\n\nOptions:\n  --control-file <path>        Use an explicit control.json path.\n  --json                       Print protocol JSON for non-streaming commands; attach prints one JSON stream object per event.\n  --help, -h                   Show this help."
}

fn parse_flags(args: List(String), flags: Flags) -> Result(Flags, Error) {
  case args {
    [] -> Ok(Flags(..flags, positional: list.reverse(flags.positional)))
    ["--control-file", path, ..rest] ->
      parse_flags(rest, Flags(..flags, control_file: Some(path)))
    ["--control-file"] -> Error(UsageError("--control-file requires a path"))
    ["--json", ..rest] -> parse_flags(rest, Flags(..flags, json: True))
    ["--raw", ..rest] -> parse_flags(rest, Flags(..flags, raw: True))
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
    _, _ -> Error(UsageError("unknown or invalid ctl command: " <> name))
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
