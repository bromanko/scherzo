import gleam/json
import gleam/option.{None, Some}
import gleam/string
import legacy_ledger_fixtures
import scherzo/state/record
import scherzo/state/record/commands as record_commands
import scherzo/state/record/issue_recovery as record_issue_recovery
import scherzo/state/record/legacy_runs as record_legacy_runs
import scherzo/state/record/outbox as record_outbox
import scherzo/state/record/steps as record_steps
import scherzo/state/record/workflow_runs as record_workflow_runs
import scherzo/state/record/workstreams as record_workstreams

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

pub fn encodes_and_decodes_control_operation_records_test() {
  assert_roundtrip(record.with_id(
    "control-operation-queued-1",
    9400,
    record.ControlOperationQueued(
      operation_id: "op-1",
      operation_kind: "retry_step",
      command_name: "retry_step",
      target: "run:run-1",
      run_id: Some("run-1"),
      issue_id: Some("issue-1"),
      issue_identifier: Some("LIV-1"),
      requested_step_id: Some("apply_feedback"),
      publication_id: None,
    ),
  ))
  assert_roundtrip(record.with_id(
    "control-operation-started-1",
    9401,
    record.ControlOperationStarted(operation_id: "op-1"),
  ))
  assert_roundtrip(record.with_id(
    "control-operation-completed-1",
    9402,
    record.ControlOperationCompleted(
      operation_id: "op-1",
      message: Some("retry-step completed"),
    ),
  ))
  assert_roundtrip(record.with_id(
    "control-operation-failed-1",
    9403,
    record.ControlOperationFailed(
      operation_id: "op-2",
      reason: "artifact_recovery_failed",
      message: Some("retry-step failed"),
    ),
  ))
}

pub fn legacy_issue_id_outbox_records_decode_test() {
  let pending =
    decode_record(
      "{\"schema_version\":2,\"record_id\":\"legacy-outbox-pending\",\"at_ms\":9300,\"kind\":\"outbox_pending_v2\",\"outbox_id\":\"outbox-1\",\"issue_id\":\"issue-1\",\"outbox_kind\":\"linear_comment\",\"dedupe_key\":\"run-1:success\",\"payload_json\":\"{\\\"body\\\":\\\"ok\\\"}\"}",
    )
  let completed =
    decode_record(
      "{\"schema_version\":2,\"record_id\":\"legacy-outbox-completed\",\"at_ms\":9301,\"kind\":\"outbox_completed\",\"outbox_id\":\"outbox-1\",\"issue_id\":\"issue-1\",\"outbox_kind\":\"linear_comment\"}",
    )
  let failed =
    decode_record(
      "{\"schema_version\":2,\"record_id\":\"legacy-outbox-failed\",\"at_ms\":9302,\"kind\":\"outbox_failed\",\"outbox_id\":\"outbox-2\",\"issue_id\":\"issue-2\",\"outbox_kind\":\"linear_comment\",\"error_code\":\"http_500\"}",
    )

  assert pending.body
    == record.OutboxPendingV2(
      outbox_id: "outbox-1",
      issue_id: "issue-1",
      outbox_kind: "linear_comment",
      dedupe_key: "run-1:success",
      payload_json: "{\"body\":\"ok\"}",
    )
  assert completed.body
    == record.OutboxCompleted(
      outbox_id: "outbox-1",
      issue_id: "issue-1",
      outbox_kind: "linear_comment",
    )
  assert failed.body
    == record.OutboxFailed(
      outbox_id: "outbox-2",
      issue_id: "issue-2",
      outbox_kind: "linear_comment",
      error_code: "http_500",
    )
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

pub fn publication_attempt_records_roundtrip_and_redact_errors_test() {
  let planned =
    record.with_id(
      "publication-planned",
      9600,
      record.PublicationAttemptRecorded(
        run_id: "run-1",
        workflow_id: "execplan",
        publication_id: "review_doc",
        series_id: "task-1:execplan:review_doc",
        attempt_id: "version-1",
        status: "planned",
        required: True,
        retryable: False,
        retry_execution_available: False,
        version_id: Some("version-1"),
        manifest_ref: Some("runs/run-1/publications/review_doc/version-1.json"),
        manifest_sha256: Some("manifest-sha"),
        manifest_bytes: Some(42),
        error_code: None,
        error_message: None,
      ),
    )
  let failed =
    record.with_id(
      "publication-failed",
      9601,
      record.PublicationAttemptRecorded(
        run_id: "run-1",
        workflow_id: "execplan",
        publication_id: "review_doc",
        series_id: "task-1:execplan:review_doc",
        attempt_id: "failed-hash",
        status: "failed",
        required: False,
        retryable: True,
        retry_execution_available: False,
        version_id: None,
        manifest_ref: Some(
          "runs/run-1/publications/review_doc/failed-hash.json",
        ),
        manifest_sha256: Some("manifest-sha-2"),
        manifest_bytes: Some(64),
        error_code: Some("unknown_output"),
        error_message: Some("secret-value missing output"),
      ),
    )

  assert_roundtrip(planned)
  assert_roundtrip(failed)
  let redacted =
    failed
    |> record.redact_excerpts(["secret-value"])
    |> record.to_string
  assert !string.contains(redacted, "secret-value")
  assert string.contains(redacted, "[REDACTED]")
}

pub fn retry_scheduled_requires_delay_ms_test() {
  let missing_delay_line =
    "{\"schema_version\":2,\"record_id\":\"retry-missing-delay\",\"at_ms\":4000,\"kind\":\"retry_scheduled\",\"issue_id\":\"issue-1\",\"issue_identifier\":\"SCH-1\",\"generation\":2,\"reason\":\"backoff\"}"

  let assert Error(_) = record.decode_string(missing_delay_line)
}

pub fn scheduled_failure_report_failed_requires_next_retry_at_ms_test() {
  let missing_next_retry_line =
    "{\"schema_version\":2,\"record_id\":\"scheduled-report-missing-retry\",\"at_ms\":4001,\"kind\":\"scheduled_failure_report_failed\",\"job_id\":\"repair\",\"workflow_id\":\"repair\",\"due_at_ms\":900000,\"run_id\":\"schedule-repair-20260505T120000Z\",\"attempt\":2,\"dedupe_key\":\"scheduled-job:repair\",\"error_code\":\"linear_api_request\",\"error_message\":\"network\",\"generation\":1}"

  let assert Error(_) = record.decode_string(missing_next_retry_line)
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

pub fn encodes_and_decodes_workflow_run_provenance_repaired_test() {
  let repair =
    record.with_id(
      "provenance-repair-1",
      11_000,
      record.WorkflowRunProvenanceRepaired(
        run_id: "run-1",
        workflow_id: "implementation",
        workflow_fingerprint: "wf-1",
        issue_id: "issue-1",
        issue_identifier: "LIV-695",
        task_ref: record.linear_task_ref_fields(
          "issue-1",
          Some("LIV-695"),
          None,
        ),
        issue_fingerprint: "issue-fp-1",
        observed_updated_at_ms: 10_999,
        run_root: "test/tmp/run-root",
        repair_mode: "state_repair_explicit",
        source_evidence: [
          "workflow_run_interrupted:run-1",
          "workflow_run_inputs_recorded:run-1",
        ],
      ),
    )

  assert_roundtrip(repair)
  assert string.contains(
    record.to_string(repair),
    "workflow_run_provenance_repaired",
  )
  assert string.contains(record.to_string(repair), "source_evidence")
}

pub fn encodes_and_decodes_workflow_contract_manifest_records_test() {
  let inputs =
    record.with_id(
      "workflow-inputs-1",
      12_000,
      record.WorkflowRunInputsRecorded(
        run_id: "run-1",
        workflow_id: "research",
        workflow_fingerprint: "fp",
        artifact_ref: "runs/run-1/inputs.v1.json",
        artifact_sha256: "abc",
        artifact_bytes: 123,
      ),
    )
  let outputs =
    record.with_id(
      "workflow-outputs-1",
      12_100,
      record.WorkflowRunOutputsRecorded(
        run_id: "run-1",
        workflow_id: "research",
        workflow_fingerprint: "fp",
        artifact_ref: "runs/run-1/outputs.v1.json",
        artifact_sha256: "def",
        artifact_bytes: 456,
      ),
    )
  assert_roundtrip(inputs)
  assert_roundtrip(outputs)
  assert string.contains(
    record.to_string(inputs),
    "workflow_run_inputs_recorded",
  )
  assert string.contains(record.to_string(outputs), "artifact_bytes")
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

pub fn bounded_context_record_helpers_cover_remaining_slices_test() {
  assert record_legacy_runs.run_started_entries(
      "run-1",
      "issue-1",
      "SCH-1",
      "workspace",
    )
    == [
      #("run_id", json.string("run-1")),
      #("issue_id", json.string("issue-1")),
      #("issue_identifier", json.string("SCH-1")),
      #("workspace_path", json.string("workspace")),
    ]
  assert record_issue_recovery.retry_scheduled_entries(
      "issue-1",
      "SCH-1",
      1000,
      2,
      "backoff",
    )
    == [
      #("issue_id", json.string("issue-1")),
      #("issue_identifier", json.string("SCH-1")),
      #("delay_ms", json.int(1000)),
      #("generation", json.int(2)),
      #("reason", json.string("backoff")),
    ]
  assert record_commands.linear_seen_entries(
      "comment-1",
      "issue-1",
      "user-1",
      "retry",
      "/scherzo retry",
    )
    == [
      #("comment_id", json.string("comment-1")),
      #("issue_id", json.string("issue-1")),
      #("author_id", json.string("user-1")),
      #("command_name", json.string("retry")),
      #("excerpt", json.string("/scherzo retry")),
    ]
  assert record_outbox.pending_v2_entries(
      "outbox-1",
      "issue-1",
      "linear_comment",
      "dedupe",
      "{\"body\":\"ok\"}",
    )
    == [
      #("outbox_id", json.string("outbox-1")),
      #("issue_id", json.string("issue-1")),
      #("outbox_kind", json.string("linear_comment")),
      #("dedupe_key", json.string("dedupe")),
      #("payload_json", json.string("{\"body\":\"ok\"}")),
    ]
  assert record_workflow_runs.contract_record_entries(
      "run-1",
      "workflow",
      "wf-1",
      "artifact.json",
      "sha",
      12,
    )
    == [
      #("run_id", json.string("run-1")),
      #("workflow_id", json.string("workflow")),
      #("workflow_fingerprint", json.string("wf-1")),
      #("artifact_ref", json.string("artifact.json")),
      #("artifact_sha256", json.string("sha")),
      #("artifact_bytes", json.int(12)),
    ]
  assert record_steps.prepared_entries(
      "run-1",
      "workflow",
      "step-1",
      1,
      "ws",
      "/tmp/ws",
      "/tmp/run",
      Some("src"),
      None,
    )
    == [
      #("run_id", json.string("run-1")),
      #("workflow_id", json.string("workflow")),
      #("step_id", json.string("step-1")),
      #("attempt_index", json.int(1)),
      #("workspace_name", json.string("ws")),
      #("workspace_path", json.string("/tmp/ws")),
      #("run_root", json.string("/tmp/run")),
      #("source_workspace_name", json.string("src")),
      #("source_workspace_path", json.null()),
    ]
  assert record_workstreams.assigned_entries(
      "ws-1",
      "assign-1",
      "workflow",
      Some("playbook"),
      "handoff",
      "idem-1",
    )
    == [
      #("workstream_id", json.string("ws-1")),
      #("assignment_id", json.string("assign-1")),
      #("workflow_id", json.string("workflow")),
      #("playbook_id", json.string("playbook")),
      #("reason", json.string("handoff")),
      #("idempotency_key", json.string("idem-1")),
    ]
}

fn assert_roundtrip(ledger_record: record.LedgerRecord) -> Nil {
  let assert Ok(decoded) = record.decode_string(record.to_string(ledger_record))
  assert decoded == ledger_record
}
