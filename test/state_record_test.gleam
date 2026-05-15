import gleam/option.{Some}
import gleam/string
import legacy_ledger_fixtures
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

pub fn scheduled_failure_report_errors_are_redacted_test() {
  let unsafe =
    record.with_id(
      "scheduled-report-secret",
      9500,
      record.ScheduledFailureReportFailed(
        job_id: "repair",
        workflow_id: "repair",
        due_at_ms: 900_000,
        run_id: "schedule-repair-20260505T120000Z",
        attempt: 1,
        dedupe_key: "scheduled-job:repair",
        error_code: "linear_api_request",
        error_message: "request failed with secret-value",
        next_retry_at_ms: 20_000,
        generation: 1,
      ),
    )
  let encoded =
    unsafe
    |> record.redact_excerpts(["secret-value"])
    |> record.to_string

  assert !string.contains(encoded, "secret-value")
  assert string.contains(encoded, "[REDACTED]")
}

pub fn retry_scheduled_requires_delay_ms_test() {
  let missing_delay_line =
    "{\"schema_version\":2,\"record_id\":\"retry-missing-delay\",\"at_ms\":4000,\"kind\":\"retry_scheduled\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"SCH-1\",\"generation\":2,\"reason\":\"backoff\"}"

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

pub fn decodes_new_task_workflow_records_without_schema_bump_test() {
  assert record.schema_version == 2

  let started_line =
    legacy_ledger_fixtures.workflow_run_started_with_task_v2("new-1", 2)
  let finished_line =
    legacy_ledger_fixtures.workflow_run_finished_with_task_v2("new-2", 4)

  let assert Ok(started) = record.decode_string(started_line)
  let assert Ok(finished) = record.decode_string(finished_line)

  assert started.body
    == record.WorkflowRunStartedWithTask(
      run_id: "run-1",
      workflow_id: "execplan",
      workflow_fingerprint: "wf-new",
      issue_id: "issue-1",
      issue_identifier: "LIV-266",
      task_ref: record.TaskRefFields(
        task_backend_kind: "linear",
        task_remote_id: "issue-1",
        task_key: Some("LIV-266"),
        task_url: Some("https://linear.app/living-systems/issue/LIV-266"),
      ),
      issue_fingerprint: "fp-new",
      observed_updated_at_ms: 20,
      run_root: "test/tmp/run-root",
    )
  assert finished.body
    == record.WorkflowRunFinishedWithTask(
      run_id: "run-1",
      workflow_id: "execplan",
      issue_id: "issue-1",
      task_ref: record.TaskRefFields(
        task_backend_kind: "linear",
        task_remote_id: "issue-1",
        task_key: Some("LIV-266"),
        task_url: Some("https://linear.app/living-systems/issue/LIV-266"),
      ),
      outcome: "success",
      token_total: 10,
      turns: 2,
    )
  assert record.to_string(started) == started_line
  assert record.to_string(finished) == finished_line
}

pub fn encodes_and_decodes_remote_command_records_test() {
  let seen =
    decode_record(legacy_ledger_fixtures.remote_command_seen_v2("cmd-new-1", 9))
  let started =
    decode_record(legacy_ledger_fixtures.remote_command_started_v2(
      "cmd-new-2",
      10,
    ))
  let completed =
    decode_record(legacy_ledger_fixtures.remote_command_completed_v2(
      "cmd-new-3",
      11,
      "ok",
      "Retry queued",
    ))
  let acked =
    decode_record(legacy_ledger_fixtures.remote_command_acked_v2(
      "cmd-new-4",
      12,
    ))

  assert seen.body
    == record.RemoteCommandSeen(
      backend_kind: "linear",
      event_id: "comment-1",
      task_remote_id: "issue-1",
      task_key: Some("LIV-266"),
      author_id: "user-1",
      command_name: "retry",
      excerpt: "/scherzo retry",
    )
  assert started.body
    == record.RemoteCommandStarted(
      backend_kind: "linear",
      event_id: "comment-1",
      task_remote_id: "issue-1",
      command_name: "retry",
    )
  assert completed.body
    == record.RemoteCommandCompleted(
      backend_kind: "linear",
      event_id: "comment-1",
      task_remote_id: "issue-1",
      status: "ok",
      message_excerpt: "Retry queued",
    )
  assert acked.body
    == record.RemoteCommandAcked(
      backend_kind: "linear",
      event_id: "comment-1",
      task_remote_id: "issue-1",
    )
}

pub fn encodes_and_decodes_scheduled_records_test() {
  assert_roundtrip(record.with_id(
    "scheduled-due-1",
    10_000,
    record.ScheduledJobDue(
      job_id: "repair",
      workflow_id: "repair",
      due_at_ms: 900_000,
      run_id: "schedule-repair-20260505T120000Z",
      trigger: "automatic",
    ),
  ))
  assert_roundtrip(record.with_id(
    "scheduled-skipped-1",
    10_001,
    record.ScheduledJobSkipped(
      job_id: "repair",
      workflow_id: "repair",
      due_at_ms: 1_800_000,
      run_id: "schedule-repair-20260505T121500Z",
      reason: "overlap_running",
      skipped_count: 2,
    ),
  ))
  assert_roundtrip(record.with_id(
    "scheduled-pending-1",
    10_002,
    record.ScheduledRunPending(
      job_id: "repair",
      workflow_id: "repair",
      due_at_ms: 900_000,
      run_id: "schedule-repair-20260505T120000Z",
      trigger: "manual",
      requested_at_ms: 10_002,
    ),
  ))
  assert_roundtrip(record.with_id(
    "scheduled-blocked-1",
    10_003,
    record.ScheduledRunPendingBlocked(
      job_id: "repair",
      workflow_id: "repair",
      due_at_ms: 900_000,
      run_id: "schedule-repair-20260505T120000Z",
      reason: "waiting_for_global_slot",
      observed_at_ms: 10_003,
    ),
  ))
  assert_roundtrip(record.with_id(
    "scheduled-cancelled-1",
    10_004,
    record.ScheduledRunPendingCancelled(
      job_id: "repair",
      workflow_id: "repair",
      due_at_ms: 900_000,
      run_id: "schedule-repair-20260505T120000Z",
      reason: "job_disabled",
      cancelled_at_ms: 10_004,
    ),
  ))
  assert_roundtrip(record.with_id(
    "scheduled-started-1",
    10_005,
    record.ScheduledRunStarted(
      job_id: "repair",
      workflow_id: "repair",
      due_at_ms: 900_000,
      started_at_ms: 10_005,
      run_id: "schedule-repair-20260505T120000Z",
      attempt: 1,
      session_id: "session-1",
      run_root: "workspaces/repair/scheduled/repair/run",
    ),
  ))
  assert_roundtrip(record.with_id(
    "scheduled-succeeded-1",
    10_006,
    record.ScheduledRunSucceeded(
      job_id: "repair",
      workflow_id: "repair",
      due_at_ms: 900_000,
      run_id: "schedule-repair-20260505T120000Z",
      attempt: 1,
      finished_at_ms: 10_006,
      token_total: 42,
      turns: 3,
    ),
  ))
  assert_roundtrip(record.with_id(
    "scheduled-failed-1",
    10_007,
    record.ScheduledRunFailed(
      job_id: "repair",
      workflow_id: "repair",
      due_at_ms: 900_000,
      run_id: "schedule-repair-20260505T120000Z",
      attempt: 1,
      finished_at_ms: 10_007,
      reason: "workflow_step_failed",
      retry_exhausted: False,
      run_root: Some("workspaces/repair/scheduled/repair/run"),
    ),
  ))
  assert_roundtrip(record.with_id(
    "scheduled-retry-1",
    10_008,
    record.ScheduledRunRetryScheduled(
      job_id: "repair",
      workflow_id: "repair",
      due_at_ms: 900_000,
      run_id: "schedule-repair-20260505T120000Z",
      next_attempt: 2,
      delay_ms: 10_000,
      generation: 1,
      reason: "workflow_step_failed",
    ),
  ))
  assert_roundtrip(record.with_id(
    "scheduled-retry-cancelled-1",
    10_009,
    record.ScheduledRunRetryCancelled(
      job_id: "repair",
      run_id: "schedule-repair-20260505T120000Z",
      generation: 1,
      reason: "superseded",
    ),
  ))
  assert_roundtrip(record.with_id(
    "scheduled-reported-1",
    10_010,
    record.ScheduledFailureReported(
      job_id: "repair",
      workflow_id: "repair",
      due_at_ms: 900_000,
      run_id: "schedule-repair-20260505T120000Z",
      attempt: 2,
      dedupe_key: "scheduled-job:repair",
      linear_issue_id: "issue-linear",
      action: "created",
    ),
  ))
  assert_roundtrip(record.with_id(
    "scheduled-report-failed-1",
    10_011,
    record.ScheduledFailureReportFailed(
      job_id: "repair",
      workflow_id: "repair",
      due_at_ms: 900_000,
      run_id: "schedule-repair-20260505T120000Z",
      attempt: 2,
      dedupe_key: "scheduled-job:repair",
      error_code: "linear_api_request",
      error_message: "network",
      next_retry_at_ms: 20_000,
      generation: 1,
    ),
  ))
}

pub fn unsupported_schema_version_is_rejected_test() {
  let line =
    "{\"schema_version\":3,\"record_id\":\"future\",\"at_ms\":1,\"kind\":\"run_started\",\"run_id\":\"run-1\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"SCH-1\",\"workspace_path\":\"work\"}"
  let assert Error(record.UnsupportedVersion(3)) = record.decode_string(line)
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
    "{\"schema_version\":2,\"record_id\":\"unknown-1\",\"at_ms\":1,\"kind\":\"unknown\"}"
  let assert Error(record.UnknownKind("unknown")) = record.decode_string(line)
}

pub fn missing_required_body_field_is_rejected_test() {
  let line =
    "{\"schema_version\":2,\"record_id\":\"run-started-missing\",\"at_ms\":1,\"kind\":\"run_started\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"SCH-1\",\"workspace_path\":\"work\"}"
  let assert Error(record.InvalidRecord("missing run_id")) =
    record.decode_string(line)
}

pub fn invalid_top_level_record_shape_is_rejected_test() {
  let missing_record_id =
    "{\"schema_version\":2,\"at_ms\":1,\"kind\":\"run_started\",\"run_id\":\"run-1\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"SCH-1\",\"workspace_path\":\"work\"}"
  let assert Error(record.InvalidRecord("invalid ledger record shape")) =
    record.decode_string(missing_record_id)

  let wrong_at_ms_type =
    "{\"schema_version\":2,\"record_id\":\"bad-at\",\"at_ms\":\"soon\",\"kind\":\"run_started\",\"run_id\":\"run-1\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"SCH-1\",\"workspace_path\":\"work\"}"
  let assert Error(record.InvalidRecord("invalid ledger record shape")) =
    record.decode_string(wrong_at_ms_type)
}

fn decode_record(line: String) -> record.LedgerRecord {
  let assert Ok(decoded) = record.decode_string(line)
  decoded
}

fn assert_roundtrip(ledger_record: record.LedgerRecord) -> Nil {
  let assert Ok(decoded) = record.decode_string(record.to_string(ledger_record))
  assert decoded == ledger_record
}
