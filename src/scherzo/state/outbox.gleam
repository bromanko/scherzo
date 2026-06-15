import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/claim_abandonment
import scherzo/log

pub const max_payload_chars = 500

pub fn bounded_payload_json(
  outbox_kind: String,
  body: String,
  secrets: List(String),
) -> String {
  json.object([
    #("type", json.string(outbox_kind)),
    #("body", json.string(safe_body(body, secrets))),
  ])
  |> json.to_string
}

pub fn linear_comment_payload(body: String, secrets: List(String)) -> String {
  bounded_payload_json("linear_comment", body, secrets)
}

pub fn linear_command_ack_payload(
  source_comment_id: String,
  body: String,
  secrets: List(String),
) -> String {
  json.object([
    #("type", json.string("linear_command_ack")),
    #("source_comment_id", json.string(source_comment_id)),
    #("body", json.string(safe_body(body, secrets))),
  ])
  |> json.to_string
}

pub const scheduled_failure_publication_kind = "scheduled_failure_publication"

pub fn remote_command_ack_payload(
  backend_kind: String,
  event_id: String,
  task_remote_id: String,
  body: String,
  secrets: List(String),
) -> String {
  json.object([
    #("type", json.string("remote_command_ack")),
    #("task_backend_kind", json.string(backend_kind)),
    #("event_id", json.string(event_id)),
    #("task_remote_id", json.string(task_remote_id)),
    #("body", json.string(safe_body(body, secrets))),
  ])
  |> json.to_string
}

pub type ScheduledFailurePayload {
  ScheduledFailurePayload(
    kind: String,
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    attempt: Int,
    max_attempts: Int,
    reason: String,
    run_root: Option(String),
    session_id: Option(String),
    dedupe_key: String,
    title: String,
    body: String,
    labels: List(String),
    target_state_name: Option(String),
    previous_task_remote_id: Option(String),
    report_attempt_index: Int,
  )
}

pub fn scheduled_failure_payload(
  payload: ScheduledFailurePayload,
  secrets: List(String),
) -> String {
  let entries = [
    #("type", json.string(payload.kind)),
    #("job_id", json.string(payload.job_id)),
    #("workflow_id", json.string(payload.workflow_id)),
    #("due_at_ms", json.int(payload.due_at_ms)),
    #("run_id", json.string(payload.run_id)),
    #("attempt", json.int(payload.attempt)),
    #("max_attempts", json.int(payload.max_attempts)),
    #("reason", json.string(safe_body(payload.reason, secrets))),
    #("dedupe_key", json.string(payload.dedupe_key)),
    #("title", json.string(safe_body(payload.title, secrets))),
    #("body", json.string(safe_body(payload.body, secrets))),
    #("labels", json.array(payload.labels, of: json.string)),
    #("report_attempt_index", json.int(payload.report_attempt_index)),
  ]
  let entries = append_optional_string(entries, "run_root", payload.run_root)
  let entries =
    append_optional_string(entries, "session_id", payload.session_id)
  let entries =
    append_optional_string(
      entries,
      "target_state_name",
      payload.target_state_name,
    )
  let entries =
    append_optional_string(
      entries,
      "previous_task_remote_id",
      payload.previous_task_remote_id,
    )
  json.object(entries) |> json.to_string
}

pub fn tracker_update_payload(
  kind: String,
  marker: String,
  body: String,
  target_state_id: Option(String),
  target_state_name: Option(String),
  secrets: List(String),
) -> String {
  let entries = [
    #("type", json.string(kind)),
    #("marker", json.string(marker)),
    #("body", json.string(safe_body(body, secrets))),
  ]
  let entries =
    append_optional_string(entries, "target_state_id", target_state_id)
  let entries =
    append_optional_string(entries, "target_state_name", target_state_name)
  json.object(entries) |> json.to_string
}

pub fn safe_body(body: String, secrets: List(String)) -> String {
  log.redact("outbox_payload_body", body, secrets)
  |> log.truncate(max_payload_chars)
}

pub type Payload {
  Payload(
    kind: String,
    body: String,
    source_comment_id: Option(String),
    backend_kind: Option(String),
    event_id: Option(String),
    task_remote_id: Option(String),
  )
}

pub type TrackerUpdatePayload {
  TrackerUpdatePayload(
    kind: String,
    marker: String,
    body: String,
    target_state_id: Option(String),
    target_state_name: Option(String),
  )
}

pub type ReplayError {
  OutboxPayloadMissing
  InvalidOutboxPayload
  UnsupportedOutboxPayloadKind(String)
  UnsupportedOutboxKind(String)
}

pub fn decode_payload(payload_json: String) -> Result(Payload, ReplayError) {
  case json.parse(payload_json, payload_decoder()) {
    Ok(payload) -> Ok(payload)
    Error(_) -> Error(InvalidOutboxPayload)
  }
}

pub fn decode_tracker_update_payload(
  payload_json: String,
) -> Result(TrackerUpdatePayload, ReplayError) {
  case json.parse(payload_json, tracker_update_payload_decoder()) {
    Ok(payload) -> Ok(payload)
    Error(_) -> Error(InvalidOutboxPayload)
  }
}

pub fn decode_scheduled_failure_payload(
  payload_json: String,
) -> Result(ScheduledFailurePayload, ReplayError) {
  case json.parse(payload_json, scheduled_failure_payload_decoder()) {
    Ok(payload) -> Ok(payload)
    Error(_) -> Error(InvalidOutboxPayload)
  }
}

pub fn recovery_replay_error(
  outbox_kind: String,
  payload_kind: String,
) -> Result(Nil, ReplayError) {
  case outbox_kind == payload_kind && replayable_kind(outbox_kind) {
    True -> Ok(Nil)
    False -> Error(UnsupportedOutboxKind(outbox_kind))
  }
}

pub fn retry_due_on_recovery(
  outbox_kind: String,
  next_attempt_at_ms: Int,
  now_ms: Int,
) -> Bool {
  outbox_kind != scheduled_failure_publication_kind
  || next_attempt_at_ms <= now_ms
}

fn replayable_kind(kind: String) -> Bool {
  case kind {
    "linear_comment"
    | "linear_command_ack"
    | "remote_command_ack"
    | "report_success"
    | "report_failure"
    | "park"
    | "invalid_workflow" -> True
    kind if kind == scheduled_failure_publication_kind -> True
    _ ->
      kind == claim_abandonment.claim_kind
      || kind == claim_abandonment.release_claim_kind
  }
}

pub fn replay_error_code(error: ReplayError) -> String {
  case error {
    OutboxPayloadMissing -> "outbox_payload_missing"
    InvalidOutboxPayload -> "invalid_outbox_payload"
    UnsupportedOutboxPayloadKind(kind) ->
      "unsupported_outbox_payload_kind:" <> kind
    UnsupportedOutboxKind(kind) -> "unsupported_outbox_kind:" <> kind
  }
}

pub fn describe_replay_error(error: ReplayError) -> String {
  case error {
    OutboxPayloadMissing -> "outbox payload missing"
    InvalidOutboxPayload -> "invalid outbox payload JSON"
    UnsupportedOutboxPayloadKind(kind) ->
      "unsupported outbox payload kind: " <> kind
    UnsupportedOutboxKind(kind) -> "unsupported outbox kind: " <> kind
  }
}

fn append_optional_string(
  entries: List(#(String, json.Json)),
  key: String,
  value: Option(String),
) -> List(#(String, json.Json)) {
  case value {
    Some(value) -> list.append(entries, [#(key, json.string(value))])
    None -> entries
  }
}

fn first_some(value: Option(a), fallback: Option(a)) -> Option(a) {
  case value {
    Some(_) -> value
    None -> fallback
  }
}

fn payload_decoder() -> decode.Decoder(Payload) {
  use kind <- decode.field("type", decode.string)
  use body <- decode.field("body", decode.string)
  use source_comment_id <- decode.optional_field(
    "source_comment_id",
    None,
    decode.optional(decode.string),
  )
  use legacy_backend_kind <- decode.optional_field(
    "backend_kind",
    None,
    decode.optional(decode.string),
  )
  use task_backend_kind <- decode.optional_field(
    "task_backend_kind",
    None,
    decode.optional(decode.string),
  )
  let backend_kind = first_some(task_backend_kind, legacy_backend_kind)
  use event_id <- decode.optional_field(
    "event_id",
    None,
    decode.optional(decode.string),
  )
  use task_remote_id <- decode.optional_field(
    "task_remote_id",
    None,
    decode.optional(decode.string),
  )
  case kind {
    "remote_command_ack" ->
      case backend_kind, event_id, task_remote_id {
        Some(_), Some(_), Some(_) ->
          decode.success(Payload(
            kind,
            body,
            source_comment_id,
            backend_kind,
            event_id,
            task_remote_id,
          ))
        _, _, _ ->
          decode.failure(
            Payload(kind, body, source_comment_id, None, None, None),
            expected: "remote_command_ack payload fields",
          )
      }
    _ ->
      decode.success(Payload(
        kind,
        body,
        source_comment_id,
        backend_kind,
        event_id,
        task_remote_id,
      ))
  }
}

fn tracker_update_payload_decoder() -> decode.Decoder(TrackerUpdatePayload) {
  use kind <- decode.field("type", decode.string)
  use marker <- decode.field("marker", decode.string)
  use body <- decode.field("body", decode.string)
  use target_state_id <- decode.optional_field(
    "target_state_id",
    None,
    decode.optional(decode.string),
  )
  use target_state_name <- decode.optional_field(
    "target_state_name",
    None,
    decode.optional(decode.string),
  )
  decode.success(TrackerUpdatePayload(
    kind,
    marker,
    body,
    target_state_id,
    target_state_name,
  ))
}

fn scheduled_failure_payload_decoder() -> decode.Decoder(
  ScheduledFailurePayload,
) {
  use kind <- decode.field("type", decode.string)
  use job_id <- decode.field("job_id", decode.string)
  use workflow_id <- decode.field("workflow_id", decode.string)
  use due_at_ms <- decode.field("due_at_ms", decode.int)
  use run_id <- decode.field("run_id", decode.string)
  use attempt <- decode.field("attempt", decode.int)
  use max_attempts <- decode.field("max_attempts", decode.int)
  use reason <- decode.field("reason", decode.string)
  use run_root <- decode.optional_field(
    "run_root",
    None,
    decode.optional(decode.string),
  )
  use session_id <- decode.optional_field(
    "session_id",
    None,
    decode.optional(decode.string),
  )
  use dedupe_key <- decode.field("dedupe_key", decode.string)
  use title <- decode.field("title", decode.string)
  use body <- decode.field("body", decode.string)
  use labels <- decode.field("labels", decode.list(of: decode.string))
  use target_state_name <- decode.optional_field(
    "target_state_name",
    None,
    decode.optional(decode.string),
  )
  use previous_task_remote_id <- decode.optional_field(
    "previous_task_remote_id",
    None,
    decode.optional(decode.string),
  )
  use report_attempt_index <- decode.field("report_attempt_index", decode.int)
  decode.success(ScheduledFailurePayload(
    kind,
    job_id,
    workflow_id,
    due_at_ms,
    run_id,
    attempt,
    max_attempts,
    reason,
    run_root,
    session_id,
    dedupe_key,
    title,
    body,
    labels,
    target_state_name,
    previous_task_remote_id,
    report_attempt_index,
  ))
}
