import gleam/dict
import gleam/option.{None, Some}
import scherzo/state/projection
import scherzo/state/record

pub fn folding_records_produces_expected_projection_test() {
  let folded = projection.fold(sample_records())

  let assert Ok(projection.RunFinished(
    issue_id: "issue-1",
    classification: "success",
    token_total: 100,
    turns: 4,
    finished_at_ms: 2000,
  )) = dict.get(folded.runs, "run-1")

  let assert Ok(projection.RunInterrupted(
    issue_id: "issue-2",
    reason: "daemon_exit",
    interrupted_at_ms: 2500,
  )) = dict.get(folded.runs, "run-2")

  let assert Ok(projection.RetryScheduled(
    issue_identifier: "SCH-1",
    delay_ms: 10_000,
    generation: 3,
    reason: "backoff",
    scheduled_at_ms: 3000,
  )) = dict.get(folded.retries, "issue-1")

  let assert Ok(projection.ParkedIssue(
    issue_identifier: "SCH-2",
    reason: "blocked",
    observed_updated_at_ms: 3900,
    parked_at_ms: 4000,
    release_policy: "explicit_unpark_only",
    issue_fingerprint: "",
  )) = dict.get(folded.parked_issues, "issue-2")
  let assert Error(_) = dict.get(folded.parked_issues, "issue-3")

  let assert Ok(projection.CommandCompleted(
    issue_id: "issue-1",
    status: "accepted",
    message_excerpt: "retry queued",
    completed_at_ms: 5000,
  )) = dict.get(folded.commands, "comment-1")

  let assert Ok(projection.CommandAcked(issue_id: "issue-1", acked_at_ms: 5500)) =
    dict.get(folded.commands, "comment-2")

  let assert Ok(projection.OutboxFailed(
    issue_id: "issue-2",
    outbox_kind: "linear_comment",
    error_code: "http_500",
    failed_at_ms: 6000,
  )) = dict.get(folded.outbox, "outbox-1")
}

pub fn projection_exposes_recovery_facts_test() {
  let records = [
    record.with_id(
      "known-1",
      1000,
      record.KnownWorkspace(
        issue_id: "issue-1",
        issue_identifier: "SCH-1",
        workspace_path: ".scherzo/workspaces/SCH-1",
      ),
    ),
    record.with_id(
      "run-started-1",
      1100,
      record.RunStarted(
        run_id: "run-1",
        issue_id: "issue-1",
        issue_identifier: "SCH-1",
        workspace_path: ".scherzo/workspaces/SCH-1",
      ),
    ),
    record.with_id(
      "run-finished-1",
      1200,
      record.RunFinished(
        run_id: "run-1",
        issue_id: "issue-1",
        classification: "success",
        token_total: 1,
        turns: 1,
      ),
    ),
    record.with_id(
      "counter-1",
      1300,
      record.IssueCounterUpdated(
        issue_id: "issue-1",
        issue_identifier: "SCH-1",
        failure_attempts: 2,
        worker_sessions: 3,
        observed_updated_at_ms: 1299,
        source_run_id: Some("run-1"),
      ),
    ),
    record.with_id(
      "park-v2-1",
      1400,
      record.IssueParkedV2(
        issue_id: "issue-2",
        issue_identifier: "SCH-2",
        reason: "max_retry_attempts",
        release_policy: "auto_unpark_on_issue_change",
        issue_fingerprint: "fingerprint",
        observed_updated_at_ms: 1399,
      ),
    ),
    record.with_id(
      "retry-1",
      1500,
      record.RetryScheduled(
        issue_id: "issue-1",
        issue_identifier: "SCH-1",
        delay_ms: 5000,
        generation: 4,
        reason: "failure",
      ),
    ),
    record.with_id(
      "outbox-v2-1",
      1600,
      record.OutboxPendingV2(
        outbox_id: "outbox-1",
        issue_id: "issue-1",
        outbox_kind: "linear_comment",
        dedupe_key: "run-1:success",
        payload_json: "{\"body\":\"ok\"}",
      ),
    ),
  ]
  let folded = projection.fold(records)

  let assert Ok(".scherzo/workspaces/SCH-1") =
    projection.known_workspace_for_issue(folded, "issue-1")
  assert projection.latest_counter(folded, "issue-1").failure_attempts == 2
  assert projection.latest_counter(folded, "issue-1").worker_sessions == 3
  assert projection.counter_has_source_run(folded, "issue-1", "run-1")

  let assert Ok(projection.ParkedIssue(
    issue_identifier: "SCH-2",
    reason: "max_retry_attempts",
    observed_updated_at_ms: 1399,
    parked_at_ms: 1400,
    release_policy: "auto_unpark_on_issue_change",
    issue_fingerprint: "fingerprint",
  )) = dict.get(folded.parked_issues, "issue-2")
  let assert Ok(retry_status) = dict.get(folded.retries, "issue-1")
  let assert Ok(6500) = projection.retry_due_at_ms(retry_status)
  let assert Ok([
    projection.OutboxReplay(
      outbox_id: "outbox-1",
      issue_id: "issue-1",
      outbox_kind: "linear_comment",
      dedupe_key: "run-1:success",
      payload_json: "{\"body\":\"ok\"}",
    ),
  ]) = projection.pending_outbox_replays(folded)
}

pub fn payload_less_pending_outbox_is_skipped_test() {
  let folded =
    projection.fold([
      record.with_id(
        "outbox-old",
        1000,
        record.OutboxPending(
          outbox_id: "outbox-old",
          issue_id: "issue-1",
          outbox_kind: "linear_comment",
          dedupe_key: "old",
        ),
      ),
    ])

  let assert Ok([]) = projection.pending_outbox_replays(folded)
}

pub fn pending_outbox_replays_are_chronological_test() {
  let folded =
    projection.fold([
      record.with_id(
        "outbox-a",
        2000,
        record.OutboxPendingV2(
          outbox_id: "outbox-a",
          issue_id: "issue-a",
          outbox_kind: "linear_command_ack",
          dedupe_key: "ack:a",
          payload_json: "{\"type\":\"linear_command_ack\",\"body\":\"a\"}",
        ),
      ),
      record.with_id(
        "outbox-b",
        1000,
        record.OutboxPendingV2(
          outbox_id: "outbox-b",
          issue_id: "issue-b",
          outbox_kind: "linear_command_ack",
          dedupe_key: "ack:b",
          payload_json: "{\"type\":\"linear_command_ack\",\"body\":\"b\"}",
        ),
      ),
    ])

  let assert Ok([
    projection.OutboxReplay(outbox_id: "outbox-b", ..),
    projection.OutboxReplay(outbox_id: "outbox-a", ..),
  ]) = projection.pending_outbox_replays(folded)
}

pub fn projection_snapshot_roundtrips_test() {
  let folded = projection.fold(sample_records())
  let assert Ok(decoded) =
    projection.decode_string(projection.to_string(folded))
  assert decoded == folded
}

pub fn projection_snapshot_requires_retry_delay_ms_test() {
  let snapshot =
    snapshot_json(
      runs: "[]",
      retries: "[{\"issue_id\":\"issue-missing-delay\",\"status\":\"scheduled\",\"issue_identifier\":\"SCH-1\",\"generation\":4,\"reason\":\"backoff\",\"scheduled_at_ms\":3000}]",
      parked_issues: "[]",
      commands: "[]",
      outbox: "[]",
    )

  let assert Error(_) = projection.decode_string(snapshot)
}

pub fn retry_status_transitions_replace_previous_status_test() {
  let scheduled =
    record.with_id(
      "retry-replace-1",
      100,
      record.RetryScheduled(
        issue_id: "issue-retry",
        issue_identifier: "SCH-10",
        delay_ms: 1000,
        generation: 7,
        reason: "backoff",
      ),
    )
  let cancelled =
    record.with_id(
      "retry-replace-2",
      200,
      record.RetryCancelled(
        issue_id: "issue-retry",
        generation: 7,
        reason: "operator",
      ),
    )

  let after_scheduled = projection.apply(projection.new(), scheduled)
  let assert Ok(projection.RetryScheduled(
    issue_identifier: "SCH-10",
    delay_ms: 1000,
    generation: 7,
    reason: "backoff",
    scheduled_at_ms: 100,
  )) = dict.get(after_scheduled.retries, "issue-retry")

  let after_cancelled = projection.apply(after_scheduled, cancelled)
  let assert Ok(projection.RetryCancelled(
    generation: 7,
    reason: "operator",
    cancelled_at_ms: 200,
  )) = dict.get(after_cancelled.retries, "issue-retry")
}

pub fn run_status_transitions_replace_previous_status_test() {
  let started =
    record.with_id(
      "run-replace-1",
      100,
      record.RunStarted(
        run_id: "run-replace",
        issue_id: "issue-run",
        issue_identifier: "SCH-11",
        workspace_path: ".scherzo/workspaces/SCH-11",
      ),
    )
  let finished =
    record.with_id(
      "run-replace-2",
      200,
      record.RunFinished(
        run_id: "run-replace",
        issue_id: "issue-run",
        classification: "success",
        token_total: 10,
        turns: 2,
      ),
    )
  let interrupted =
    record.with_id(
      "run-replace-3",
      300,
      record.RunInterrupted(
        run_id: "run-replace",
        issue_id: "issue-run",
        reason: "operator",
      ),
    )

  let after_started = projection.apply(projection.new(), started)
  let assert Ok(projection.RunRunning(
    issue_id: "issue-run",
    issue_identifier: "SCH-11",
    workspace_path: ".scherzo/workspaces/SCH-11",
    started_at_ms: 100,
  )) = dict.get(after_started.runs, "run-replace")

  let after_finished = projection.apply(after_started, finished)
  let assert Ok(projection.RunFinished(
    issue_id: "issue-run",
    classification: "success",
    token_total: 10,
    turns: 2,
    finished_at_ms: 200,
  )) = dict.get(after_finished.runs, "run-replace")

  let after_interrupted = projection.apply(after_finished, interrupted)
  let assert Ok(projection.RunInterrupted(
    issue_id: "issue-run",
    reason: "operator",
    interrupted_at_ms: 300,
  )) = dict.get(after_interrupted.runs, "run-replace")
}

pub fn linear_command_status_transitions_replace_previous_status_test() {
  let seen =
    record.with_id(
      "command-replace-1",
      100,
      record.LinearCommandSeen(
        comment_id: "comment-replace",
        issue_id: "issue-command",
        author_id: "user-1",
        command_name: "retry",
        excerpt: "/scherzo retry",
      ),
    )
  let started =
    record.with_id(
      "command-replace-2",
      200,
      record.LinearCommandStarted(
        comment_id: "comment-replace",
        issue_id: "issue-command",
        command_name: "retry",
      ),
    )
  let completed =
    record.with_id(
      "command-replace-3",
      300,
      record.LinearCommandCompleted(
        comment_id: "comment-replace",
        issue_id: "issue-command",
        status: "accepted",
        message_excerpt: "retry queued",
      ),
    )
  let acked =
    record.with_id(
      "command-replace-4",
      400,
      record.LinearCommandAcked(
        comment_id: "comment-replace",
        issue_id: "issue-command",
      ),
    )

  let after_seen = projection.apply(projection.new(), seen)
  let assert Ok(projection.CommandSeen(
    issue_id: "issue-command",
    author_id: "user-1",
    command_name: "retry",
    excerpt: "/scherzo retry",
    seen_at_ms: 100,
  )) = dict.get(after_seen.commands, "comment-replace")

  let after_started = projection.apply(after_seen, started)
  let assert Ok(projection.CommandStarted(
    issue_id: "issue-command",
    command_name: "retry",
    started_at_ms: 200,
  )) = dict.get(after_started.commands, "comment-replace")

  let after_completed = projection.apply(after_started, completed)
  let assert Ok(projection.CommandCompleted(
    issue_id: "issue-command",
    status: "accepted",
    message_excerpt: "retry queued",
    completed_at_ms: 300,
  )) = dict.get(after_completed.commands, "comment-replace")

  let after_acked = projection.apply(after_completed, acked)
  let assert Ok(projection.CommandAcked(
    issue_id: "issue-command",
    acked_at_ms: 400,
  )) = dict.get(after_acked.commands, "comment-replace")
}

pub fn linear_command_receipt_projection_tracks_lifecycle_test() {
  let records = [
    record.with_id(
      "receipt-1",
      100,
      record.LinearCommandSeen(
        comment_id: "comment-receipt",
        issue_id: "issue-command",
        author_id: "user-1",
        command_name: "park",
        excerpt: "hold",
      ),
    ),
    record.with_id(
      "receipt-2",
      200,
      record.LinearCommandStarted(
        comment_id: "comment-receipt",
        issue_id: "issue-command",
        command_name: "park",
      ),
    ),
    record.with_id(
      "receipt-3",
      300,
      record.LinearCommandCompleted(
        comment_id: "comment-receipt",
        issue_id: "issue-command",
        status: "applied",
        message_excerpt: "issue parked",
      ),
    ),
  ]
  let folded = projection.fold(records)

  let assert projection.CommandReceiptCompleted(
    issue_id: "issue-command",
    author_id: "user-1",
    command_name: "park",
    excerpt: "hold",
    result_status: "applied",
    message_excerpt: "issue parked",
    seen_at_ms: 100,
    started_at_ms: 200,
    completed_at_ms: 300,
    acked_at_ms: None,
  ) = projection.command_receipt(folded, "comment-receipt")

  let acked =
    projection.apply(
      folded,
      record.with_id(
        "receipt-4",
        400,
        record.LinearCommandAcked(
          comment_id: "comment-receipt",
          issue_id: "issue-command",
        ),
      ),
    )
  let assert projection.CommandReceiptCompleted(
    issue_id: "issue-command",
    author_id: "user-1",
    command_name: "park",
    excerpt: "hold",
    result_status: "applied",
    message_excerpt: "issue parked",
    seen_at_ms: 100,
    started_at_ms: 200,
    completed_at_ms: 300,
    acked_at_ms: Some(400),
  ) = projection.command_receipt(acked, "comment-receipt")
}

pub fn command_receipt_projection_does_not_reopen_completed_or_acked_receipts_test() {
  let folded =
    projection.fold([
      record.with_id(
        "receipt-monotonic-1",
        100,
        record.LinearCommandSeen(
          comment_id: "comment-monotonic",
          issue_id: "issue-command",
          author_id: "user-1",
          command_name: "park",
          excerpt: "hold",
        ),
      ),
      record.with_id(
        "receipt-monotonic-2",
        200,
        record.LinearCommandStarted(
          comment_id: "comment-monotonic",
          issue_id: "issue-command",
          command_name: "park",
        ),
      ),
      record.with_id(
        "receipt-monotonic-3",
        300,
        record.LinearCommandCompleted(
          comment_id: "comment-monotonic",
          issue_id: "issue-command",
          status: "applied",
          message_excerpt: "issue parked",
        ),
      ),
      record.with_id(
        "receipt-monotonic-4",
        400,
        record.LinearCommandAcked(
          comment_id: "comment-monotonic",
          issue_id: "issue-command",
        ),
      ),
      record.with_id(
        "receipt-monotonic-5",
        500,
        record.LinearCommandSeen(
          comment_id: "comment-monotonic",
          issue_id: "issue-command",
          author_id: "user-1",
          command_name: "park",
          excerpt: "duplicate",
        ),
      ),
      record.with_id(
        "receipt-monotonic-6",
        600,
        record.LinearCommandStarted(
          comment_id: "comment-monotonic",
          issue_id: "issue-command",
          command_name: "park",
        ),
      ),
    ])

  let assert projection.CommandReceiptCompleted(
    issue_id: "issue-command",
    author_id: "user-1",
    command_name: "park",
    excerpt: "hold",
    result_status: "applied",
    message_excerpt: "issue parked",
    seen_at_ms: 100,
    started_at_ms: 200,
    completed_at_ms: 300,
    acked_at_ms: Some(400),
  ) = projection.command_receipt(folded, "comment-monotonic")
}

pub fn started_without_completed_receipt_survives_projection_test() {
  let folded =
    projection.fold([
      record.with_id(
        "receipt-started-1",
        100,
        record.LinearCommandSeen(
          comment_id: "comment-started",
          issue_id: "issue-command",
          author_id: "user-1",
          command_name: "retry",
          excerpt: "",
        ),
      ),
      record.with_id(
        "receipt-started-2",
        200,
        record.LinearCommandStarted(
          comment_id: "comment-started",
          issue_id: "issue-command",
          command_name: "retry",
        ),
      ),
    ])

  let assert projection.CommandReceiptStarted(
    issue_id: "issue-command",
    author_id: "user-1",
    command_name: "retry",
    excerpt: "",
    seen_at_ms: 100,
    started_at_ms: 200,
  ) = projection.command_receipt(folded, "comment-started")
  let assert projection.CommandReceiptUnseen =
    projection.command_receipt(folded, "missing-comment")
}

pub fn outbox_status_transitions_replace_previous_status_test() {
  let pending =
    record.with_id(
      "outbox-replace-1",
      100,
      record.OutboxPending(
        outbox_id: "outbox-replace",
        issue_id: "issue-outbox",
        outbox_kind: "linear_comment",
        dedupe_key: "comment:ack",
      ),
    )
  let completed =
    record.with_id(
      "outbox-replace-2",
      200,
      record.OutboxCompleted(
        outbox_id: "outbox-replace",
        issue_id: "issue-outbox",
        outbox_kind: "linear_comment",
      ),
    )
  let failed =
    record.with_id(
      "outbox-replace-3",
      300,
      record.OutboxFailed(
        outbox_id: "outbox-replace",
        issue_id: "issue-outbox",
        outbox_kind: "linear_comment",
        error_code: "http_500",
      ),
    )

  let after_pending = projection.apply(projection.new(), pending)
  let assert Ok(projection.OutboxPending(
    issue_id: "issue-outbox",
    outbox_kind: "linear_comment",
    dedupe_key: "comment:ack",
    pending_at_ms: 100,
  )) = dict.get(after_pending.outbox, "outbox-replace")

  let after_completed = projection.apply(after_pending, completed)
  let assert Ok(projection.OutboxCompleted(
    issue_id: "issue-outbox",
    outbox_kind: "linear_comment",
    completed_at_ms: 200,
  )) = dict.get(after_completed.outbox, "outbox-replace")

  let after_failed = projection.apply(after_completed, failed)
  let assert Ok(projection.OutboxFailed(
    issue_id: "issue-outbox",
    outbox_kind: "linear_comment",
    error_code: "http_500",
    failed_at_ms: 300,
  )) = dict.get(after_failed.outbox, "outbox-replace")
}

pub fn projection_snapshot_decoder_rejects_invalid_snapshots_test() {
  assert_malformed_projection_snapshot("{")
  assert_malformed_projection_snapshot(
    "{\"schema_version\":2,\"kind\":\"not_projection\",\"runs\":[],\"retries\":[],\"parked_issues\":[],\"commands\":[],\"outbox\":[]}",
  )
  assert_malformed_projection_snapshot(
    "{\"schema_version\":3,\"kind\":\"projection_snapshot\",\"runs\":[],\"retries\":[],\"parked_issues\":[],\"commands\":[],\"outbox\":[]}",
  )
  assert_malformed_projection_snapshot(snapshot_json(
    runs: "[{\"run_id\":\"run-1\",\"status\":\"paused\"}]",
    retries: "[]",
    parked_issues: "[]",
    commands: "[]",
    outbox: "[]",
  ))
  assert_malformed_projection_snapshot(snapshot_json(
    runs: "[]",
    retries: "[{\"issue_id\":\"issue-1\",\"status\":\"snoozed\"}]",
    parked_issues: "[]",
    commands: "[]",
    outbox: "[]",
  ))
  assert_malformed_projection_snapshot(snapshot_json(
    runs: "[]",
    retries: "[]",
    parked_issues: "[]",
    commands: "[{\"comment_id\":\"comment-1\",\"status\":\"queued\"}]",
    outbox: "[]",
  ))
  assert_malformed_projection_snapshot(snapshot_json(
    runs: "[]",
    retries: "[]",
    parked_issues: "[]",
    commands: "[]",
    outbox: "[{\"outbox_id\":\"outbox-1\",\"status\":\"sent\"}]",
  ))
  assert_malformed_projection_snapshot(snapshot_json(
    runs: "[{\"run_id\":\"run-1\",\"status\":\"running\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"SCH-1\",\"started_at_ms\":1}]",
    retries: "[]",
    parked_issues: "[]",
    commands: "[]",
    outbox: "[]",
  ))
}

fn assert_malformed_projection_snapshot(contents: String) -> Nil {
  let assert Error(_) = projection.decode_string(contents)
  Nil
}

fn snapshot_json(
  runs runs: String,
  retries retries: String,
  parked_issues parked_issues: String,
  commands commands: String,
  outbox outbox: String,
) -> String {
  "{\"schema_version\":2,\"kind\":\"projection_snapshot\",\"runs\":"
  <> runs
  <> ",\"retries\":"
  <> retries
  <> ",\"parked_issues\":"
  <> parked_issues
  <> ",\"commands\":"
  <> commands
  <> ",\"outbox\":"
  <> outbox
  <> "}"
}

fn sample_records() -> List(record.LedgerRecord) {
  [
    record.with_id(
      "r1",
      1000,
      record.RunStarted(
        run_id: "run-1",
        issue_id: "issue-1",
        issue_identifier: "SCH-1",
        workspace_path: ".scherzo/workspaces/SCH-1",
      ),
    ),
    record.with_id(
      "r2",
      2000,
      record.RunFinished(
        run_id: "run-1",
        issue_id: "issue-1",
        classification: "success",
        token_total: 100,
        turns: 4,
      ),
    ),
    record.with_id(
      "r3",
      2100,
      record.RunStarted(
        run_id: "run-2",
        issue_id: "issue-2",
        issue_identifier: "SCH-2",
        workspace_path: ".scherzo/workspaces/SCH-2",
      ),
    ),
    record.with_id(
      "r4",
      2500,
      record.RunInterrupted(
        run_id: "run-2",
        issue_id: "issue-2",
        reason: "daemon_exit",
      ),
    ),
    record.with_id(
      "r5",
      3000,
      record.RetryScheduled(
        issue_id: "issue-1",
        issue_identifier: "SCH-1",
        delay_ms: 10_000,
        generation: 3,
        reason: "backoff",
      ),
    ),
    record.with_id(
      "r6",
      4000,
      record.IssueParked(
        issue_id: "issue-2",
        issue_identifier: "SCH-2",
        reason: "blocked",
        observed_updated_at_ms: 3900,
      ),
    ),
    record.with_id(
      "r7",
      4100,
      record.IssueParked(
        issue_id: "issue-3",
        issue_identifier: "SCH-3",
        reason: "blocked",
        observed_updated_at_ms: 4000,
      ),
    ),
    record.with_id(
      "r8",
      4200,
      record.IssueUnparked(
        issue_id: "issue-3",
        issue_identifier: "SCH-3",
        reason: "operator",
      ),
    ),
    record.with_id(
      "r9",
      5000,
      record.LinearCommandCompleted(
        comment_id: "comment-1",
        issue_id: "issue-1",
        status: "accepted",
        message_excerpt: "retry queued",
      ),
    ),
    record.with_id(
      "r10",
      5500,
      record.LinearCommandAcked(comment_id: "comment-2", issue_id: "issue-1"),
    ),
    record.with_id(
      "r11",
      5900,
      record.OutboxPending(
        outbox_id: "outbox-1",
        issue_id: "issue-2",
        outbox_kind: "linear_comment",
        dedupe_key: "comment-1:ack",
      ),
    ),
    record.with_id(
      "r12",
      6000,
      record.OutboxFailed(
        outbox_id: "outbox-1",
        issue_id: "issue-2",
        outbox_kind: "linear_comment",
        error_code: "http_500",
      ),
    ),
  ]
}
