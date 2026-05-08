import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None}
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

pub fn safe_body(body: String, secrets: List(String)) -> String {
  log.redact("outbox_payload_body", body, secrets)
  |> log.truncate(max_payload_chars)
}

pub type Payload {
  Payload(kind: String, body: String, source_comment_id: Option(String))
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
    "linear_command_ack", other -> Error(UnsupportedOutboxPayloadKind(other))
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
  decode.success(Payload(kind, body, source_comment_id))
}
