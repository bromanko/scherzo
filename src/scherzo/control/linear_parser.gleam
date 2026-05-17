import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/control/command

pub type ParseError {
  UnknownCommand(String)
  MissingArgument(String)
  InvalidArgument(String)
  MultipleCommands
  NoCurrentSession(String)
}

pub type ParsedLinearCommand {
  ParsedLinearCommand(
    source_issue_id: String,
    source_comment_id: String,
    command: command.OperatorCommand,
    excerpt: String,
  )
}

pub fn parse_comment(
  prefix: String,
  source_issue_id: String,
  source_issue_identifier: String,
  current_session_id: Option(String),
  comment_id: String,
  body: String,
) -> Result(Option(ParsedLinearCommand), ParseError) {
  let _ = source_issue_identifier
  let command_lines = command_lines(body, string.trim(prefix))
  case command_lines {
    [] -> Ok(None)
    [line] -> {
      use parsed <- result.try(parse_line(
        string.trim(prefix),
        source_issue_id,
        current_session_id,
        comment_id,
        line,
      ))
      Ok(Some(parsed))
    }
    [_, ..] -> Error(MultipleCommands)
  }
}

pub fn contains_command_line(prefix: String, body: String) -> Bool {
  case command_lines(body, string.trim(prefix)) {
    [] -> False
    [_, ..] -> True
  }
}

fn command_lines(body: String, prefix: String) -> List(String) {
  body
  |> string.split("\n")
  |> command_lines_loop(prefix, False, [])
}

fn command_lines_loop(
  lines: List(String),
  prefix: String,
  in_fence: Bool,
  acc: List(String),
) -> List(String) {
  case lines {
    [] -> list.reverse(acc)
    [line, ..rest] -> {
      let trimmed = string.trim(line)
      case string.starts_with(trimmed, "```") {
        True -> command_lines_loop(rest, prefix, !in_fence, acc)
        False ->
          case !in_fence && has_prefix_boundary(trimmed, prefix) {
            True -> command_lines_loop(rest, prefix, in_fence, [trimmed, ..acc])
            False -> command_lines_loop(rest, prefix, in_fence, acc)
          }
      }
    }
  }
}

fn has_prefix_boundary(line: String, prefix: String) -> Bool {
  case prefix == "" || !string.starts_with(line, prefix) {
    True -> False
    False -> {
      let prefix_len = string.length(prefix)
      case string.length(line) == prefix_len {
        True -> True
        False -> is_space(string.slice(line, prefix_len, 1))
      }
    }
  }
}

fn parse_line(
  prefix: String,
  source_issue_id: String,
  current_session_id: Option(String),
  comment_id: String,
  line: String,
) -> Result(ParsedLinearCommand, ParseError) {
  let body = string.drop_start(line, string.length(prefix)) |> string.trim
  let #(name, rest) = split_first_token(body)
  case name {
    "" -> Error(MissingArgument("command"))
    "retry" ->
      no_args(rest, name, fn() {
        parsed(
          source_issue_id,
          comment_id,
          command.RetryIssue(command.IssueId(source_issue_id)),
          "",
        )
      })
    "retry-step" -> parse_retry_step(source_issue_id, comment_id, rest)
    "park" -> parse_park(source_issue_id, comment_id, rest)
    "unpark" ->
      no_args(rest, name, fn() {
        parsed(
          source_issue_id,
          comment_id,
          command.UnparkIssue(command.IssueId(source_issue_id)),
          "",
        )
      })
    "abort" ->
      no_args(rest, name, fn() {
        use session_id <- result.try(require_session(current_session_id, name))
        parsed(
          source_issue_id,
          comment_id,
          command.AbortSession(session_id),
          "",
        )
      })
    "stop-after-turn" ->
      no_args(rest, name, fn() {
        use session_id <- result.try(require_session(current_session_id, name))
        parsed(
          source_issue_id,
          comment_id,
          command.StopAfterCurrentTurn(session_id),
          "",
        )
      })
    "prompt" ->
      parse_prompt(source_issue_id, comment_id, current_session_id, rest)
    "ui" -> parse_ui(source_issue_id, comment_id, current_session_id, rest)
    other -> Error(UnknownCommand(other))
  }
}

fn parse_retry_step(
  source_issue_id: String,
  comment_id: String,
  rest: String,
) -> Result(ParsedLinearCommand, ParseError) {
  let #(flag, value) = split_first_token(rest)
  case flag {
    "" ->
      parsed(
        source_issue_id,
        comment_id,
        command.RetryWorkflowStep(
          command.RetryWorkflowStepIssueRef(command.IssueId(source_issue_id)),
          None,
        ),
        "",
      )
    "--step" -> {
      let step_id = string.trim(value)
      case step_id == "" {
        True -> Error(MissingArgument("step"))
        False ->
          parsed(
            source_issue_id,
            comment_id,
            command.RetryWorkflowStep(
              command.RetryWorkflowStepIssueRef(command.IssueId(source_issue_id)),
              Some(step_id),
            ),
            step_id,
          )
      }
    }
    other -> Error(InvalidArgument(other))
  }
}

fn parse_park(
  source_issue_id: String,
  comment_id: String,
  rest: String,
) -> Result(ParsedLinearCommand, ParseError) {
  let #(flag, reason) = split_first_token(rest)
  case flag {
    "--reason" -> {
      let reason = string.trim(reason)
      case reason == "" {
        True -> Error(MissingArgument("reason"))
        False ->
          parsed(
            source_issue_id,
            comment_id,
            command.ParkIssue(command.IssueId(source_issue_id), reason),
            reason,
          )
      }
    }
    "" -> Error(MissingArgument("--reason"))
    other -> Error(InvalidArgument(other))
  }
}

fn parse_prompt(
  source_issue_id: String,
  comment_id: String,
  current_session_id: Option(String),
  rest: String,
) -> Result(ParsedLinearCommand, ParseError) {
  use session_id <- result.try(require_session(current_session_id, "prompt"))
  let message = string.trim(rest)
  case message == "" {
    True -> Error(MissingArgument("prompt"))
    False ->
      parsed(
        source_issue_id,
        comment_id,
        command.PromptSession(session_id, message),
        message,
      )
  }
}

fn parse_ui(
  source_issue_id: String,
  comment_id: String,
  current_session_id: Option(String),
  rest: String,
) -> Result(ParsedLinearCommand, ParseError) {
  use session_id <- result.try(require_session(current_session_id, "ui respond"))
  let #(subcommand, rest) = split_first_token(rest)
  case subcommand {
    "respond" -> parse_ui_respond(source_issue_id, comment_id, session_id, rest)
    "" -> Error(MissingArgument("ui command"))
    other -> Error(UnknownCommand("ui " <> other))
  }
}

fn parse_ui_respond(
  source_issue_id: String,
  comment_id: String,
  session_id: String,
  rest: String,
) -> Result(ParsedLinearCommand, ParseError) {
  let #(request_id, rest) = split_first_token(rest)
  let #(flag, value) = split_first_token(rest)
  case request_id, flag {
    "", _ -> Error(MissingArgument("request_id"))
    _, "--cancel" ->
      case string.trim(value) == "" {
        True ->
          parsed(
            source_issue_id,
            comment_id,
            command.RespondUi(session_id, request_id, command.UiCancel),
            "",
          )
        False -> Error(InvalidArgument(value))
      }
    _, "--value" -> {
      let value = string.trim(value)
      case value == "" {
        True -> Error(MissingArgument("value"))
        False ->
          parsed(
            source_issue_id,
            comment_id,
            command.RespondUi(session_id, request_id, command.UiValue(value)),
            value,
          )
      }
    }
    _, "" -> Error(MissingArgument("--cancel or --value"))
    _, other -> Error(InvalidArgument(other))
  }
}

fn no_args(
  rest: String,
  name: String,
  build: fn() -> Result(ParsedLinearCommand, ParseError),
) -> Result(ParsedLinearCommand, ParseError) {
  case string.trim(rest) == "" {
    True -> build()
    False -> Error(InvalidArgument(name <> " " <> rest))
  }
}

fn parsed(
  source_issue_id: String,
  comment_id: String,
  operator_command: command.OperatorCommand,
  excerpt: String,
) -> Result(ParsedLinearCommand, ParseError) {
  Ok(ParsedLinearCommand(
    source_issue_id: source_issue_id,
    source_comment_id: comment_id,
    command: operator_command,
    excerpt: excerpt,
  ))
}

fn require_session(
  current_session_id: Option(String),
  command_name: String,
) -> Result(String, ParseError) {
  case current_session_id {
    Some(session_id) -> Ok(session_id)
    None -> Error(NoCurrentSession(command_name))
  }
}

fn split_first_token(value: String) -> #(String, String) {
  let value = string.trim(value)
  split_first_token_loop(value, 0)
}

fn split_first_token_loop(value: String, index: Int) -> #(String, String) {
  case index >= string.length(value) {
    True -> #(value, "")
    False ->
      case is_space(string.slice(value, index, 1)) {
        True -> #(
          string.slice(value, 0, index),
          string.slice(value, index, string.length(value) - index)
            |> string.trim,
        )
        False -> split_first_token_loop(value, index + 1)
      }
  }
}

fn is_space(value: String) -> Bool {
  value == " " || value == "\t"
}
