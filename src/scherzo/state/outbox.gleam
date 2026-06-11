import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
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

pub fn recovery_replay_error(
  outbox_kind: String,
  payload_kind: String,
) -> Result(Nil, ReplayError) {
  case outbox_kind, payload_kind {
    "linear_comment", "linear_comment"
    | "linear_command_ack", "linear_command_ack"
    | "remote_command_ack", "remote_command_ack"
    | "claim", "claim"
    | "report_success", "report_success"
    | "report_failure", "report_failure"
    | "park", "park"
    | "invalid_workflow", "invalid_workflow"
    -> Ok(Nil)
    _, _ -> Error(UnsupportedOutboxKind(outbox_kind))
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
