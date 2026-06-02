import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None, Some}

pub const context_name = "commands"

pub fn seen_status_entry_to_json(
  comment_id: String,
  issue_id: String,
  author_id: String,
  command_name: String,
  excerpt: String,
  seen_at_ms: Int,
) -> json.Json {
  json.object([
    #("comment_id", json.string(comment_id)),
    #("status", json.string("seen")),
    #("issue_id", json.string(issue_id)),
    #("author_id", json.string(author_id)),
    #("command_name", json.string(command_name)),
    #("excerpt", json.string(excerpt)),
    #("seen_at_ms", json.int(seen_at_ms)),
  ])
}

pub fn started_status_entry_to_json(
  comment_id: String,
  issue_id: String,
  command_name: String,
  started_at_ms: Int,
) -> json.Json {
  json.object([
    #("comment_id", json.string(comment_id)),
    #("status", json.string("started")),
    #("issue_id", json.string(issue_id)),
    #("command_name", json.string(command_name)),
    #("started_at_ms", json.int(started_at_ms)),
  ])
}

pub fn completed_status_entry_to_json(
  comment_id: String,
  issue_id: String,
  result_status: String,
  message_excerpt: String,
  completed_at_ms: Int,
) -> json.Json {
  json.object([
    #("comment_id", json.string(comment_id)),
    #("status", json.string("completed")),
    #("issue_id", json.string(issue_id)),
    #("result_status", json.string(result_status)),
    #("message_excerpt", json.string(message_excerpt)),
    #("completed_at_ms", json.int(completed_at_ms)),
  ])
}

pub fn acked_status_entry_to_json(
  comment_id: String,
  issue_id: String,
  acked_at_ms: Int,
) -> json.Json {
  json.object([
    #("comment_id", json.string(comment_id)),
    #("status", json.string("acked")),
    #("issue_id", json.string(issue_id)),
    #("acked_at_ms", json.int(acked_at_ms)),
  ])
}

pub fn unseen_receipt_entry_to_json(comment_id: String) -> json.Json {
  json.object([
    #("comment_id", json.string(comment_id)),
    #("status", json.string("unseen")),
  ])
}

pub fn seen_receipt_entry_to_json(
  comment_id: String,
  issue_id: String,
  author_id: String,
  command_name: String,
  excerpt: String,
  seen_at_ms: Int,
) -> json.Json {
  json.object([
    #("comment_id", json.string(comment_id)),
    #("status", json.string("seen")),
    #("issue_id", json.string(issue_id)),
    #("author_id", json.string(author_id)),
    #("command_name", json.string(command_name)),
    #("excerpt", json.string(excerpt)),
    #("seen_at_ms", json.int(seen_at_ms)),
  ])
}

pub fn started_receipt_entry_to_json(
  comment_id: String,
  issue_id: String,
  author_id: String,
  command_name: String,
  excerpt: String,
  seen_at_ms: Int,
  started_at_ms: Int,
) -> json.Json {
  json.object([
    #("comment_id", json.string(comment_id)),
    #("status", json.string("started")),
    #("issue_id", json.string(issue_id)),
    #("author_id", json.string(author_id)),
    #("command_name", json.string(command_name)),
    #("excerpt", json.string(excerpt)),
    #("seen_at_ms", json.int(seen_at_ms)),
    #("started_at_ms", json.int(started_at_ms)),
  ])
}

pub fn completed_receipt_entry_to_json(
  comment_id: String,
  issue_id: String,
  author_id: String,
  command_name: String,
  excerpt: String,
  result_status: String,
  message_excerpt: String,
  seen_at_ms: Int,
  started_at_ms: Int,
  completed_at_ms: Int,
  acked_at_ms: Option(Int),
) -> json.Json {
  json.object([
    #("comment_id", json.string(comment_id)),
    #("status", json.string("completed")),
    #("issue_id", json.string(issue_id)),
    #("author_id", json.string(author_id)),
    #("command_name", json.string(command_name)),
    #("excerpt", json.string(excerpt)),
    #("result_status", json.string(result_status)),
    #("message_excerpt", json.string(message_excerpt)),
    #("seen_at_ms", json.int(seen_at_ms)),
    #("started_at_ms", json.int(started_at_ms)),
    #("completed_at_ms", json.int(completed_at_ms)),
    #("acked_at_ms", option_int_to_json(acked_at_ms)),
  ])
}

pub fn acked_receipt_entry_to_json(
  comment_id: String,
  issue_id: String,
  acked_at_ms: Int,
) -> json.Json {
  json.object([
    #("comment_id", json.string(comment_id)),
    #("status", json.string("acked")),
    #("issue_id", json.string(issue_id)),
    #("acked_at_ms", json.int(acked_at_ms)),
  ])
}

pub fn status_snapshot_decoder(
  seen: fn(String, String, String, String, Int) -> status_value,
  started: fn(String, String, Int) -> status_value,
  completed: fn(String, String, String, Int) -> status_value,
  acked: fn(String, Int) -> status_value,
  fallback: status_value,
) -> decode.Decoder(#(String, status_value)) {
  use comment_id <- decode.field("comment_id", decode.string)
  use status <- decode.field("status", decode.string)
  case status {
    "seen" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use author_id <- decode.field("author_id", decode.string)
      use command_name <- decode.field("command_name", decode.string)
      use excerpt <- decode.field("excerpt", decode.string)
      use seen_at_ms <- decode.field("seen_at_ms", decode.int)
      decode.success(#(
        comment_id,
        seen(issue_id, author_id, command_name, excerpt, seen_at_ms),
      ))
    }
    "started" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use command_name <- decode.field("command_name", decode.string)
      use started_at_ms <- decode.field("started_at_ms", decode.int)
      decode.success(#(
        comment_id,
        started(issue_id, command_name, started_at_ms),
      ))
    }
    "completed" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use result_status <- decode.field("result_status", decode.string)
      use message_excerpt <- decode.field("message_excerpt", decode.string)
      use completed_at_ms <- decode.field("completed_at_ms", decode.int)
      decode.success(#(
        comment_id,
        completed(issue_id, result_status, message_excerpt, completed_at_ms),
      ))
    }
    "acked" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use acked_at_ms <- decode.field("acked_at_ms", decode.int)
      decode.success(#(comment_id, acked(issue_id, acked_at_ms)))
    }
    _ -> decode.failure(#("", fallback), expected: "CommandSnapshot")
  }
}

pub fn receipt_snapshot_decoder(
  unseen: receipt_value,
  seen: fn(String, String, String, String, Int) -> receipt_value,
  started: fn(String, String, String, String, Int, Int) -> receipt_value,
  completed: fn(
    String,
    String,
    String,
    String,
    String,
    String,
    Int,
    Int,
    Int,
    Option(Int),
  ) -> receipt_value,
  acked: fn(String, Int) -> receipt_value,
  fallback: receipt_value,
) -> decode.Decoder(#(String, receipt_value)) {
  use comment_id <- decode.field("comment_id", decode.string)
  use status <- decode.field("status", decode.string)
  case status {
    "unseen" -> decode.success(#(comment_id, unseen))
    "seen" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use author_id <- decode.field("author_id", decode.string)
      use command_name <- decode.field("command_name", decode.string)
      use excerpt <- decode.field("excerpt", decode.string)
      use seen_at_ms <- decode.field("seen_at_ms", decode.int)
      decode.success(#(
        comment_id,
        seen(issue_id, author_id, command_name, excerpt, seen_at_ms),
      ))
    }
    "started" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use author_id <- decode.field("author_id", decode.string)
      use command_name <- decode.field("command_name", decode.string)
      use excerpt <- decode.field("excerpt", decode.string)
      use seen_at_ms <- decode.field("seen_at_ms", decode.int)
      use started_at_ms <- decode.field("started_at_ms", decode.int)
      decode.success(#(
        comment_id,
        started(
          issue_id,
          author_id,
          command_name,
          excerpt,
          seen_at_ms,
          started_at_ms,
        ),
      ))
    }
    "completed" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use author_id <- decode.field("author_id", decode.string)
      use command_name <- decode.field("command_name", decode.string)
      use excerpt <- decode.field("excerpt", decode.string)
      use result_status <- decode.field("result_status", decode.string)
      use message_excerpt <- decode.field("message_excerpt", decode.string)
      use seen_at_ms <- decode.field("seen_at_ms", decode.int)
      use started_at_ms <- decode.field("started_at_ms", decode.int)
      use completed_at_ms <- decode.field("completed_at_ms", decode.int)
      use acked_at_ms <- decode.optional_field(
        "acked_at_ms",
        None,
        decode.optional(decode.int),
      )
      decode.success(#(
        comment_id,
        completed(
          issue_id,
          author_id,
          command_name,
          excerpt,
          result_status,
          message_excerpt,
          seen_at_ms,
          started_at_ms,
          completed_at_ms,
          acked_at_ms,
        ),
      ))
    }
    "acked" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use acked_at_ms <- decode.field("acked_at_ms", decode.int)
      decode.success(#(comment_id, acked(issue_id, acked_at_ms)))
    }
    _ -> decode.failure(#("", fallback), expected: "CommandReceiptSnapshot")
  }
}

pub fn insert_status(
  statuses: Dict(String, status),
  id: String,
  status: status,
) -> Dict(String, status) {
  dict.insert(statuses, id, status)
}

pub fn command_receipt(
  receipts: Dict(String, receipt),
  comment_id: String,
  unseen: receipt,
) -> receipt {
  case dict.get(receipts, comment_id) {
    Ok(receipt) -> receipt
    Error(Nil) -> unseen
  }
}

fn option_int_to_json(value: Option(Int)) -> json.Json {
  case value {
    Some(value) -> json.int(value)
    None -> json.null()
  }
}
