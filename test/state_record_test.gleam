import gleam/option.{Some}
import gleam/string
import scherzo/state/record

pub fn encodes_and_decodes_run_records_test() {
  let started =
    record.with_id(
      "run-started-1",
      1000,
      record.RunStarted(
        run_id: "run-1",
        issue_id: "issue-1",
        issue_identifier: "SCH-1",
        workspace_path: ".scherzo/workspaces/SCH-1",
      ),
    )
  let finished =
    record.with_id(
      "run-finished-1",
      2000,
      record.RunFinished(
        run_id: "run-1",
        issue_id: "issue-1",
        classification: "success",
        token_total: 42,
        turns: 3,
      ),
    )
  let interrupted =
    record.with_id(
      "run-interrupted-1",
      3000,
      record.RunInterrupted(
        run_id: "run-2",
        issue_id: "issue-2",
        reason: "daemon_exit",
      ),
    )

  assert_roundtrip(started)
  assert_roundtrip(finished)
  assert_roundtrip(interrupted)
  assert string.contains(record.to_string(started), "\"kind\":\"run_started\"")
}

pub fn encodes_and_decodes_retry_and_park_records_test() {
  assert_roundtrip(record.with_id(
    "retry-scheduled-1",
    4000,
    record.RetryScheduled(
      issue_id: "issue-1",
      issue_identifier: "SCH-1",
      delay_ms: 10_000,
      generation: 2,
      reason: "backoff",
    ),
  ))
  assert_roundtrip(record.with_id(
    "retry-cancelled-1",
    4500,
    record.RetryCancelled(
      issue_id: "issue-1",
      generation: 2,
      reason: "manual_retry",
    ),
  ))
  assert_roundtrip(record.with_id(
    "issue-parked-1",
    5000,
    record.IssueParked(
      issue_id: "issue-2",
      issue_identifier: "SCH-2",
      reason: "blocked",
      observed_updated_at_ms: 4999,
    ),
  ))
  assert_roundtrip(record.with_id(
    "issue-unparked-1",
    6000,
    record.IssueUnparked(
      issue_id: "issue-2",
      issue_identifier: "SCH-2",
      reason: "operator",
    ),
  ))
}

pub fn encodes_and_decodes_recovery_records_test() {
  assert_roundtrip(record.with_id(
    "counter-1",
    9000,
    record.IssueCounterUpdated(
      issue_id: "issue-1",
      issue_identifier: "SCH-1",
      failure_attempts: 2,
      worker_sessions: 1,
      observed_updated_at_ms: 8999,
      source_run_id: Some("run-1"),
    ),
  ))
  assert_roundtrip(record.with_id(
    "workspace-1",
    9100,
    record.KnownWorkspace(
      issue_id: "issue-1",
      issue_identifier: "SCH-1",
      workspace_path: ".scherzo/workspaces/SCH-1",
    ),
  ))
  assert_roundtrip(record.with_id(
    "park-v2-1",
    9200,
    record.IssueParkedV2(
      issue_id: "issue-2",
      issue_identifier: "SCH-2",
      reason: "max_retry_attempts",
      release_policy: "auto_unpark_on_issue_change",
      issue_fingerprint: "fingerprint",
      observed_updated_at_ms: 9199,
    ),
  ))
  assert_roundtrip(record.with_id(
    "outbox-v2-1",
    9300,
    record.OutboxPendingV2(
      outbox_id: "outbox-1",
      issue_id: "issue-1",
      outbox_kind: "linear_comment",
      dedupe_key: "run-1:success",
      payload_json: "{\"body\":\"ok\"}",
    ),
  ))
}

pub fn outbox_pending_v2_payload_is_redacted_and_bounded_test() {
  let long = string.repeat("x", times: record.max_excerpt_chars + 20)
  let unsafe =
    record.with_id(
      "outbox-secret",
      9400,
      record.OutboxPendingV2(
        outbox_id: "outbox-secret",
        issue_id: "issue-1",
        outbox_kind: "linear_comment",
        dedupe_key: "run-1:secret",
        payload_json: "{\"body\":\"secret-value " <> long <> "\"}",
      ),
    )
  let encoded =
    unsafe
    |> record.redact_excerpts(["secret-value"])
    |> record.to_string

  assert !string.contains(encoded, "secret-value")
  assert string.contains(encoded, "[REDACTED]")
  assert string.length(encoded) < record.max_excerpt_chars + 260
}

pub fn retry_scheduled_requires_delay_ms_test() {
  let missing_delay_line =
    "{\"schema_version\":1,\"record_id\":\"retry-missing-delay\",\"at_ms\":4000,\"kind\":\"retry_scheduled\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"SCH-1\",\"generation\":2,\"reason\":\"backoff\"}"

  let assert Error(_) = record.decode_string(missing_delay_line)
}

pub fn encodes_and_decodes_linear_command_records_test() {
  assert_roundtrip(record.with_id(
    "command-seen-1",
    7000,
    record.LinearCommandSeen(
      comment_id: "comment-1",
      issue_id: "issue-1",
      author_id: "user-1",
      command_name: "retry",
      excerpt: "/scherzo retry",
    ),
  ))
  assert_roundtrip(record.with_id(
    "command-started-1",
    7100,
    record.LinearCommandStarted(
      comment_id: "comment-1",
      issue_id: "issue-1",
      command_name: "retry",
    ),
  ))
  assert_roundtrip(record.with_id(
    "command-completed-1",
    7200,
    record.LinearCommandCompleted(
      comment_id: "comment-1",
      issue_id: "issue-1",
      status: "accepted",
      message_excerpt: "queued retry",
    ),
  ))
  assert_roundtrip(record.with_id(
    "command-acked-1",
    7300,
    record.LinearCommandAcked(comment_id: "comment-1", issue_id: "issue-1"),
  ))
  assert_roundtrip(record.with_id(
    "outbox-pending-1",
    8000,
    record.OutboxPending(
      outbox_id: "outbox-1",
      issue_id: "issue-1",
      outbox_kind: "linear_comment",
      dedupe_key: "comment-1:ack",
    ),
  ))
  assert_roundtrip(record.with_id(
    "outbox-completed-1",
    8100,
    record.OutboxCompleted(
      outbox_id: "outbox-1",
      issue_id: "issue-1",
      outbox_kind: "linear_comment",
    ),
  ))
  assert_roundtrip(record.with_id(
    "outbox-failed-1",
    8200,
    record.OutboxFailed(
      outbox_id: "outbox-2",
      issue_id: "issue-2",
      outbox_kind: "linear_comment",
      error_code: "http_500",
    ),
  ))
}

pub fn unsupported_schema_version_is_rejected_test() {
  let line =
    "{\"schema_version\":2,\"record_id\":\"future\",\"at_ms\":1,\"kind\":\"run_started\",\"run_id\":\"run-1\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"SCH-1\",\"workspace_path\":\"work\"}"
  let assert Error(record.UnsupportedVersion(2)) = record.decode_string(line)
}

pub fn redacts_record_excerpts_test() {
  let unsafe =
    record.with_id(
      "command-seen-secret",
      9000,
      record.LinearCommandSeen(
        comment_id: "comment-secret",
        issue_id: "issue-1",
        author_id: "user-1",
        command_name: "prompt",
        excerpt: "please use secret-value in the next step",
      ),
    )
  let encoded =
    unsafe
    |> record.redact_excerpts(["secret-value"])
    |> record.to_string

  assert !string.contains(encoded, "secret-value")
  assert string.contains(encoded, "[REDACTED]")
}

pub fn malformed_json_is_rejected_test() {
  let assert Error(record.MalformedJson(_)) = record.decode_string("{")
}

pub fn unknown_record_kind_is_rejected_test() {
  let line =
    "{\"schema_version\":1,\"record_id\":\"unknown-1\",\"at_ms\":1,\"kind\":\"unknown\"}"
  let assert Error(record.UnknownKind("unknown")) = record.decode_string(line)
}

pub fn missing_required_body_field_is_rejected_test() {
  let line =
    "{\"schema_version\":1,\"record_id\":\"run-started-missing\",\"at_ms\":1,\"kind\":\"run_started\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"SCH-1\",\"workspace_path\":\"work\"}"
  let assert Error(record.InvalidRecord("missing run_id")) =
    record.decode_string(line)
}

pub fn invalid_top_level_record_shape_is_rejected_test() {
  let missing_record_id =
    "{\"schema_version\":1,\"at_ms\":1,\"kind\":\"run_started\",\"run_id\":\"run-1\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"SCH-1\",\"workspace_path\":\"work\"}"
  let assert Error(record.InvalidRecord("invalid ledger record shape")) =
    record.decode_string(missing_record_id)

  let wrong_at_ms_type =
    "{\"schema_version\":1,\"record_id\":\"bad-at\",\"at_ms\":\"soon\",\"kind\":\"run_started\",\"run_id\":\"run-1\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"SCH-1\",\"workspace_path\":\"work\"}"
  let assert Error(record.InvalidRecord("invalid ledger record shape")) =
    record.decode_string(wrong_at_ms_type)
}

fn assert_roundtrip(ledger_record: record.LedgerRecord) -> Nil {
  let assert Ok(decoded) = record.decode_string(record.to_string(ledger_record))
  assert decoded == ledger_record
}
