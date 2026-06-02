import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/control/query/types as query_types
import scherzo/task
import scherzo/terminal/style

pub type ParseError {
  ParseError(message: String)
}

pub type Flags {
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

pub fn default_flags() -> Flags {
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

pub fn parse_flags(
  args: List(String),
  flags: Flags,
) -> Result(Flags, ParseError) {
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
    ["--control-file"] -> Error(parse_error("--control-file requires a path"))
    ["--root", root, ..rest] ->
      parse_flags(rest, Flags(..flags, root: Some(root)))
    ["--root"] -> Error(parse_error("--root requires a workspace root"))
    ["--json", ..rest] -> parse_flags(rest, Flags(..flags, json: True))
    ["--dry-run", ..rest] -> parse_flags(rest, Flags(..flags, dry_run: True))
    ["--raw", ..rest] -> parse_flags(rest, Flags(..flags, raw: True))
    ["--pretty", ..rest] -> parse_flags(rest, Flags(..flags, pretty: True))
    ["--verbose", ..rest] -> parse_flags(rest, Flags(..flags, verbose: True))
    ["--now", ..rest] -> parse_flags(rest, Flags(..flags, now: True))
    ["--last", ..rest] -> parse_flags(rest, Flags(..flags, last: True))
    ["--run", run_id, ..rest] ->
      parse_flags(rest, Flags(..flags, run_id: Some(run_id)))
    ["--run"] -> Error(parse_error("--run requires a run id"))
    ["--publication", publication_id, ..rest] ->
      parse_flags(rest, Flags(..flags, publication_id: Some(publication_id)))
    ["--publication"] ->
      Error(parse_error("--publication requires a publication id"))
    ["--state", state, ..rest] ->
      case task.state_category_from_string(state) {
        Ok(category) ->
          parse_flags(
            rest,
            Flags(..flags, state_filters: [category, ..flags.state_filters]),
          )
        Error(_) ->
          Error(parse_error(
            "--state must be backlog, ready, active, done, canceled, duplicate, or unknown",
          ))
      }
    ["--state"] -> Error(parse_error("--state requires a task state"))
    ["--limit", value, ..rest] ->
      case parse_task_limit(value) {
        Ok(limit) -> parse_flags(rest, Flags(..flags, limit: Some(limit)))
        Error(message) -> Error(message)
      }
    ["--limit"] -> Error(parse_error("--limit requires a positive integer"))
    ["--cursor", cursor, ..rest] ->
      case string.trim(cursor) {
        "" -> Error(parse_error("--cursor must not be empty"))
        _ -> parse_flags(rest, Flags(..flags, cursor: Some(cursor)))
      }
    ["--cursor"] -> Error(parse_error("--cursor requires a cursor"))
    ["--yes", ..rest] -> parse_flags(rest, Flags(..flags, yes: True))
    ["--no-follow", ..rest] ->
      parse_flags(rest, Flags(..flags, no_follow: True))
    ["--since-cursor", value, ..rest] ->
      case parse_cursor(value) {
        Ok(cursor) -> parse_flags(rest, Flags(..flags, since_cursor: cursor))
        Error(message) -> Error(message)
      }
    ["--since-cursor"] ->
      Error(parse_error("--since-cursor requires a non-negative integer"))
    ["--color", value, ..rest] ->
      case style.parse_color_mode(value) {
        Ok(mode) -> parse_flags(rest, Flags(..flags, color: mode))
        Error(_) -> Error(parse_error("--color must be auto, always, or never"))
      }
    ["--color"] -> Error(parse_error("--color requires auto, always, or never"))
    ["--reason", reason, ..rest] ->
      parse_flags(rest, Flags(..flags, reason: Some(reason)))
    ["--reason"] -> Error(parse_error("--reason requires text"))
    ["--step", step, ..rest] ->
      parse_flags(rest, Flags(..flags, step: Some(step)))
    ["--step"] -> Error(parse_error("--step requires a step id"))
    ["--cancel", ..rest] -> parse_flags(rest, Flags(..flags, cancel: True))
    ["--value", value, ..rest] ->
      parse_flags(rest, Flags(..flags, value: Some(value)))
    ["--value"] -> Error(parse_error("--value requires text"))
    ["--help", ..] | ["-h", ..] -> Ok(Flags(..flags, positional: ["--help"]))
    [arg, ..rest] ->
      case string.starts_with(arg, "--color=") {
        True -> {
          let value = string.drop_start(arg, 8)
          case style.parse_color_mode(value) {
            Ok(mode) -> parse_flags(rest, Flags(..flags, color: mode))
            Error(_) ->
              Error(parse_error("--color must be auto, always, or never"))
          }
        }
        False ->
          case string.starts_with(arg, "--") {
            True -> Error(parse_error("unknown option: " <> arg))
            False ->
              parse_flags(
                rest,
                Flags(..flags, positional: [arg, ..flags.positional]),
              )
          }
      }
  }
}

pub fn task_query_ref(
  value: String,
) -> Result(query_types.TaskQueryRef, ParseError) {
  let value = string.trim(value)
  case value == "" {
    True -> Error(parse_error("task show requires a non-empty task reference"))
    False ->
      case string.starts_with(value, "id:") {
        True -> {
          let id = string.drop_start(value, 3) |> string.trim
          case id == "" {
            True -> Error(parse_error("task show id must include a remote id"))
            False -> Ok(query_types.TaskRemoteId(provider: None, id: id))
          }
        }
        False -> Ok(query_types.TaskDisplayId(value))
      }
  }
}

pub fn error_message(error: ParseError) -> String {
  let ParseError(message) = error
  message
}

fn parse_cursor(value: String) -> Result(Int, ParseError) {
  case int.parse(value) {
    Ok(cursor) ->
      case cursor < 0 {
        True ->
          Error(parse_error("--since-cursor requires a non-negative integer"))
        False -> Ok(cursor)
      }
    Error(_) ->
      Error(parse_error("--since-cursor requires a non-negative integer"))
  }
}

fn parse_task_limit(value: String) -> Result(Int, ParseError) {
  case int.parse(value) {
    Ok(limit) if limit > 0 -> Ok(limit)
    _ -> Error(parse_error("--limit requires a positive integer"))
  }
}

fn parse_error(message: String) -> ParseError {
  ParseError(message)
}
