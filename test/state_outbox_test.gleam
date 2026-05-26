import gleam/option.{None, Some}
import scherzo/state/outbox

pub fn decode_payload_returns_typed_invalid_error_test() {
  let assert Error(outbox.InvalidOutboxPayload) =
    outbox.decode_payload("not-json")

  assert outbox.replay_error_code(outbox.InvalidOutboxPayload)
    == "invalid_outbox_payload"
  assert outbox.describe_replay_error(outbox.InvalidOutboxPayload)
    == "invalid outbox payload JSON"
}

pub fn linear_command_ack_payload_decodes_and_replays_test() {
  let assert Ok(outbox.Payload(
    kind: "linear_command_ack",
    body: "ack",
    source_comment_id: Some("comment-1"),
    backend_kind: None,
    event_id: None,
    task_remote_id: None,
  )) =
    outbox.decode_payload(
      "{\"type\":\"linear_command_ack\",\"source_comment_id\":\"comment-1\",\"body\":\"ack\"}",
    )

  let assert Ok(Nil) =
    outbox.recovery_replay_error("linear_command_ack", "linear_command_ack")
}

pub fn remote_command_ack_payload_decodes_and_replays_test() {
  let assert Ok(outbox.Payload(
    kind: "remote_command_ack",
    body: "ack",
    source_comment_id: None,
    backend_kind: Some("linear"),
    event_id: Some("comment-1"),
    task_remote_id: Some("issue-1"),
  )) =
    outbox.decode_payload(
      outbox.remote_command_ack_payload(
        "linear",
        "comment-1",
        "issue-1",
        "ack",
        [],
      ),
    )

  let assert Ok(Nil) =
    outbox.recovery_replay_error("remote_command_ack", "remote_command_ack")
}

pub fn legacy_remote_command_ack_payload_backend_kind_decodes_test() {
  let assert Ok(outbox.Payload(
    kind: "remote_command_ack",
    body: "ack",
    source_comment_id: None,
    backend_kind: Some("linear"),
    event_id: Some("comment-1"),
    task_remote_id: Some("issue-1"),
  )) =
    outbox.decode_payload(
      "{\"type\":\"remote_command_ack\",\"backend_kind\":\"linear\",\"event_id\":\"comment-1\",\"task_remote_id\":\"issue-1\",\"body\":\"ack\"}",
    )
}

pub fn replay_kind_mismatch_returns_typed_error_with_stable_code_test() {
  let assert Error(outbox.UnsupportedOutboxKind("linear_comment")) =
    outbox.recovery_replay_error("linear_comment", "linear_comment")

  let unsupported_kind = outbox.UnsupportedOutboxKind("linear_comment")
  assert outbox.replay_error_code(unsupported_kind)
    == "unsupported_outbox_kind:linear_comment"
  assert outbox.describe_replay_error(unsupported_kind)
    == "unsupported outbox kind: linear_comment"

  let assert Error(outbox.UnsupportedOutboxPayloadKind("linear_comment")) =
    outbox.recovery_replay_error("linear_command_ack", "linear_comment")

  let unsupported_payload_kind =
    outbox.UnsupportedOutboxPayloadKind("linear_comment")
  assert outbox.replay_error_code(unsupported_payload_kind)
    == "unsupported_outbox_payload_kind:linear_comment"
  assert outbox.describe_replay_error(unsupported_payload_kind)
    == "unsupported outbox payload kind: linear_comment"
}
