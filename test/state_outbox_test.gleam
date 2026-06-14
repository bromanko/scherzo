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

pub fn linear_command_ack_payload_decodes_and_is_replayed_test() {
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

  assert outbox.recovery_replay_error(
      "linear_command_ack",
      "linear_command_ack",
    )
    == Ok(Nil)
}

pub fn remote_command_ack_payload_decodes_and_is_replayed_test() {
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

  assert outbox.recovery_replay_error(
      "remote_command_ack",
      "remote_command_ack",
    )
    == Ok(Nil)
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

pub fn release_claim_payload_is_replayable_test() {
  assert outbox.recovery_replay_error("release_claim", "release_claim")
    == Ok(Nil)
}

pub fn scheduled_failure_payload_decodes_and_is_replayable_test() {
  let payload_json =
    outbox.scheduled_failure_payload(
      outbox.ScheduledFailurePayload(
        kind: outbox.scheduled_failure_publication_kind,
        job_id: "scheduled-job",
        workflow_id: "implementation",
        due_at_ms: 1000,
        run_id: "run-1",
        attempt: 2,
        max_attempts: 3,
        reason: "boom secret",
        run_root: Some("/tmp/run"),
        session_id: Some("session-1"),
        dedupe_key: "scheduled-job:scheduled-job",
        title: "Scheduled failure",
        body: "boom secret",
        labels: ["job:scheduled-job"],
        target_state_name: Some("Triage"),
        previous_task_remote_id: Some("lin-1"),
        report_attempt_index: 2,
      ),
      ["secret"],
    )

  let assert Ok(outbox.ScheduledFailurePayload(
    kind: kind,
    job_id: "scheduled-job",
    run_id: "run-1",
    reason: "boom [REDACTED]",
    previous_task_remote_id: Some("lin-1"),
    report_attempt_index: 2,
    ..,
  )) = outbox.decode_scheduled_failure_payload(payload_json)
  assert kind == outbox.scheduled_failure_publication_kind
  assert outbox.recovery_replay_error(kind, kind) == Ok(Nil)
}

pub fn replay_kind_mismatch_returns_typed_error_with_stable_code_test() {
  assert outbox.recovery_replay_error("linear_comment", "linear_comment")
    == Ok(Nil)

  let unsupported_kind = outbox.UnsupportedOutboxKind("other_kind")
  assert outbox.replay_error_code(unsupported_kind)
    == "unsupported_outbox_kind:other_kind"
  assert outbox.describe_replay_error(unsupported_kind)
    == "unsupported outbox kind: other_kind"
}
