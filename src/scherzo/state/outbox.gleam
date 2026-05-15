import gleam/dynamic/decode
import gleam/json
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
    #("backend_kind", json.string(backend_kind)),
    #("event_id", json.string(event_id)),
    #("task_remote_id", json.string(task_remote_id)),
    #("body", json.string(safe_body(body, secrets))),
  ])
  |> json.to_string
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

pub fn recovery_replay_error(
  outbox_kind: String,
  payload_kind: String,
) -> Result(Nil, ReplayError) {
  case outbox_kind, payload_kind {
    "linear_command_ack", "linear_command_ack" -> Ok(Nil)
    "remote_command_ack", "remote_command_ack" -> Ok(Nil)
    "linear_command_ack", other -> Error(UnsupportedOutboxPayloadKind(other))
    "remote_command_ack", other -> Error(UnsupportedOutboxPayloadKind(other))
    other, _ -> Error(UnsupportedOutboxKind(other))
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

fn payload_decoder() -> decode.Decoder(Payload) {
  use kind <- decode.field("type", decode.string)
  use body <- decode.field("body", decode.string)
  use source_comment_id <- decode.optional_field(
    "source_comment_id",
    None,
    decode.optional(decode.string),
  )
  use backend_kind <- decode.optional_field(
    "backend_kind",
    None,
    decode.optional(decode.string),
  )
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
