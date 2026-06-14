import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/handoff_format
import scherzo/state/record
import scherzo/task
import scherzo/tracker/idempotency

pub const claim_kind = "claim"

pub const release_claim_kind = "release_claim"

pub const explicit_unpark_only = "explicit_unpark_only"

const reason_prefix = "abandoned_claim:"

pub fn reason_text(abandonment_reason: String) -> String {
  reason_prefix <> abandonment_reason
}

pub fn is_reason_text(reason_text: String) -> Bool {
  string.starts_with(reason_text, reason_prefix)
}

pub fn source_run_id(run_id: String) -> Option(String) {
  case string.trim(run_id) == "" {
    True -> None
    False -> Some(run_id)
  }
}

pub fn release_source(run_id: Option(String), reason: String) -> String {
  case run_id {
    Some(value) -> value
    None -> fallback_source(reason)
  }
}

pub fn claim_key(
  backend_kind: String,
  task_remote_id: String,
  run_id: String,
) -> String {
  claim_key_prefix(backend_kind, task_remote_id) <> run_id
}

pub fn release_key(
  backend_kind: String,
  task_remote_id: String,
  run_or_reason: String,
) -> String {
  "release_claim:"
  <> backend_kind
  <> ":"
  <> task_remote_id
  <> ":"
  <> run_or_reason
}

pub fn task_identifier(task_ref: task.TaskRef) -> String {
  case task_ref.key {
    Some(value) -> value
    None -> task_ref.remote_id
  }
}

pub fn task_identifier_fields(task_ref: record.TaskRefFields) -> String {
  case task_ref.task_key {
    Some(value) -> value
    None -> task_ref.task_remote_id
  }
}

pub fn claim_source(
  task_ref: record.TaskRefFields,
  claim_outbox_id: String,
  fallback_reason: String,
) -> #(String, Option(String)) {
  let prefix =
    claim_key_prefix(task_ref.task_backend_kind, task_ref.task_remote_id)
  case string.starts_with(claim_outbox_id, prefix) {
    True -> {
      let run_id = string.drop_start(claim_outbox_id, string.length(prefix))
      case source_run_id(run_id) {
        Some(run_id) -> #(run_id, Some(run_id))
        None -> #(fallback_source(fallback_reason), None)
      }
    }
    False -> #(fallback_source(fallback_reason), None)
  }
}

pub fn release_comment_body(
  issue_identifier: String,
  reason_text: String,
  source_run_id: Option(String),
  release_id: String,
  secrets: List(String),
) -> String {
  handoff_format.park_comment(
    issue_identifier,
    reason_text,
    Some(explicit_unpark_only),
    source_run_id,
    secrets,
  )
  |> idempotency.append_marker(release_id)
}

fn claim_key_prefix(backend_kind: String, task_remote_id: String) -> String {
  claim_kind <> ":" <> backend_kind <> ":" <> task_remote_id <> ":"
}

fn fallback_source(reason: String) -> String {
  reason |> string.replace(":", "_")
}
