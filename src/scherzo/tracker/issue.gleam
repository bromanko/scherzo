import birl.{type Time}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/tracker/state as issue_state

pub type BlockerRef {
  BlockerRef(
    id: Option(String),
    identifier: Option(String),
    state: Option(issue_state.IssueState),
  )
}

pub type Issue {
  Issue(
    id: String,
    identifier: String,
    title: String,
    description: Option(String),
    priority: Option(Int),
    state: issue_state.IssueState,
    branch_name: Option(String),
    url: Option(String),
    labels: List(String),
    blocked_by: List(BlockerRef),
    blocked_by_complete: Bool,
    created_at: Option(Time),
    updated_at: Option(Time),
  )
}

pub fn content_fingerprint(issue: Issue) -> String {
  [
    encode_string(issue.id),
    encode_string(issue.identifier),
    encode_string(issue.title),
    encode_optional_string(issue.description),
    encode_optional_int(issue.priority),
    encode_optional_string(issue.branch_name),
    encode_string(bool_to_string(issue.blocked_by_complete)),
    blocker_fingerprint(issue.blocked_by),
  ]
  |> string.join(with: "|")
}

pub fn fingerprint_equivalent(recorded: String, current: String) -> Bool {
  canonical_content_fingerprint(recorded)
  == canonical_content_fingerprint(current)
}

pub fn fingerprint_matches(recorded: String, issue: Issue) -> Bool {
  fingerprint_equivalent(recorded, content_fingerprint(issue))
}

fn canonical_content_fingerprint(fingerprint: String) -> String {
  case legacy_stateful_content_fingerprint(fingerprint) {
    Ok(content_fingerprint) -> content_fingerprint
    Error(Nil) -> fingerprint
  }
}

fn legacy_stateful_content_fingerprint(
  fingerprint: String,
) -> Result(String, Nil) {
  use #(issue_id, rest) <- result.try(take_delimited_string(fingerprint))
  use #(identifier, rest) <- result.try(take_delimited_string(rest))
  use #(title, rest) <- result.try(take_delimited_string(rest))
  use #(description, rest) <- result.try(take_delimited_optional(rest))
  use #(priority, rest) <- result.try(take_delimited_optional(rest))
  use #(_, rest) <- result.try(take_delimited_string(rest))
  use #(branch_name, rest) <- result.try(take_delimited_optional(rest))
  use #(blocked_by_complete, blockers) <- result.try(take_delimited_string(rest))
  [
    issue_id,
    identifier,
    title,
    description,
    priority,
    branch_name,
    blocked_by_complete,
    blockers,
  ]
  |> string.join(with: "|")
  |> Ok
}

fn take_delimited_string(input: String) -> Result(#(String, String), Nil) {
  take_delimited(input, parse_string_segment)
}

fn take_delimited_optional(input: String) -> Result(#(String, String), Nil) {
  take_delimited(input, parse_optional_encoded_segment)
}

fn take_delimited(
  input: String,
  parser: fn(String) -> Result(#(String, String), Nil),
) -> Result(#(String, String), Nil) {
  use #(segment, rest) <- result.try(parser(input))
  use rest <- result.try(take_separator(rest))
  Ok(#(segment, rest))
}

fn take_separator(input: String) -> Result(String, Nil) {
  case string.starts_with(input, "|") {
    True -> Ok(string.drop_start(input, 1))
    False -> Error(Nil)
  }
}

fn parse_string_segment(input: String) -> Result(#(String, String), Nil) {
  use #(length_text, after_colon) <- result.try(
    string.split_once(input, on: ":")
    |> result.replace_error(Nil),
  )
  use length <- result.try(int.parse(length_text) |> result.replace_error(Nil))
  case length < 0 || string.length(after_colon) < length {
    True -> Error(Nil)
    False -> {
      let value = string.slice(after_colon, 0, length)
      let rest = string.drop_start(after_colon, length)
      Ok(#(length_text <> ":" <> value, rest))
    }
  }
}

fn parse_optional_encoded_segment(
  input: String,
) -> Result(#(String, String), Nil) {
  case string.starts_with(input, "none") {
    True -> Ok(#("none", string.drop_start(input, 4)))
    False -> {
      case string.starts_with(input, "some:") {
        True -> {
          use #(encoded, rest) <- result.try(
            parse_string_segment(string.drop_start(input, 5)),
          )
          Ok(#("some:" <> encoded, rest))
        }
        False -> Error(Nil)
      }
    }
  }
}

fn encode_string(value: String) -> String {
  int.to_string(string.length(value)) <> ":" <> value
}

fn bool_to_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn encode_optional_string(value: Option(String)) -> String {
  case value {
    None -> "none"
    Some(value) -> "some:" <> encode_string(value)
  }
}

fn encode_optional_issue_state(
  value: Option(issue_state.IssueState),
) -> String {
  case value {
    None -> "none"
    Some(value) -> "some:" <> encode_string(issue_state.to_string(value))
  }
}

fn encode_optional_int(value: Option(Int)) -> String {
  case value {
    None -> "none"
    Some(value) -> "some:" <> encode_string(int.to_string(value))
  }
}

fn blocker_fingerprint(blockers: List(BlockerRef)) -> String {
  blockers
  |> list.map(fn(blocker) {
    [
      encode_optional_string(blocker.id),
      encode_optional_string(blocker.identifier),
      encode_optional_issue_state(blocker.state),
    ]
    |> string.join(with: ",")
  })
  |> list.sort(by: string.compare)
  |> string.join(with: ";")
}
