import gleam/json
import gleam/option.{type Option, None, Some}

pub const context_name = "commands"

pub fn linear_seen_entries(
  comment_id: String,
  issue_id: String,
  author_id: String,
  command_name: String,
  excerpt: String,
) -> List(#(String, json.Json)) {
  [
    #("comment_id", json.string(comment_id)),
    #("issue_id", json.string(issue_id)),
    #("author_id", json.string(author_id)),
    #("command_name", json.string(command_name)),
    #("excerpt", json.string(excerpt)),
  ]
}

pub fn linear_started_entries(
  comment_id: String,
  issue_id: String,
  command_name: String,
) -> List(#(String, json.Json)) {
  [
    #("comment_id", json.string(comment_id)),
    #("issue_id", json.string(issue_id)),
    #("command_name", json.string(command_name)),
  ]
}

pub fn linear_completed_entries(
  comment_id: String,
  issue_id: String,
  status: String,
  message_excerpt: String,
) -> List(#(String, json.Json)) {
  [
    #("comment_id", json.string(comment_id)),
    #("issue_id", json.string(issue_id)),
    #("status", json.string(status)),
    #("message_excerpt", json.string(message_excerpt)),
  ]
}

pub fn linear_acked_entries(
  comment_id: String,
  issue_id: String,
) -> List(#(String, json.Json)) {
  [
    #("comment_id", json.string(comment_id)),
    #("issue_id", json.string(issue_id)),
  ]
}

pub fn remote_seen_entries(
  backend_kind: String,
  event_id: String,
  task_remote_id: String,
  task_key: Option(String),
  author_id: String,
  command_name: String,
  excerpt: String,
) -> List(#(String, json.Json)) {
  [
    #("backend_kind", json.string(backend_kind)),
    #("event_id", json.string(event_id)),
    #("task_remote_id", json.string(task_remote_id)),
    #("task_key", option_string(task_key)),
    #("author_id", json.string(author_id)),
    #("command_name", json.string(command_name)),
    #("excerpt", json.string(excerpt)),
  ]
}

pub fn remote_started_entries(
  backend_kind: String,
  event_id: String,
  task_remote_id: String,
  command_name: String,
) -> List(#(String, json.Json)) {
  [
    #("backend_kind", json.string(backend_kind)),
    #("event_id", json.string(event_id)),
    #("task_remote_id", json.string(task_remote_id)),
    #("command_name", json.string(command_name)),
  ]
}

pub fn remote_completed_entries(
  backend_kind: String,
  event_id: String,
  task_remote_id: String,
  status: String,
  message_excerpt: String,
) -> List(#(String, json.Json)) {
  [
    #("backend_kind", json.string(backend_kind)),
    #("event_id", json.string(event_id)),
    #("task_remote_id", json.string(task_remote_id)),
    #("status", json.string(status)),
    #("message_excerpt", json.string(message_excerpt)),
  ]
}

pub fn remote_acked_entries(
  backend_kind: String,
  event_id: String,
  task_remote_id: String,
) -> List(#(String, json.Json)) {
  [
    #("backend_kind", json.string(backend_kind)),
    #("event_id", json.string(event_id)),
    #("task_remote_id", json.string(task_remote_id)),
  ]
}

fn option_string(value: Option(String)) -> json.Json {
  case value {
    Some(inner) -> json.string(inner)
    None -> json.null()
  }
}
