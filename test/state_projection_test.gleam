import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/state/projection
import scherzo/state/projection/commands as projection_commands
import scherzo/state/projection/issue_recovery as projection_issue_recovery
import scherzo/state/projection/legacy_runs as projection_legacy_runs
import scherzo/state/projection/outbox as projection_outbox
import scherzo/state/projection/steps as projection_steps
import scherzo/state/projection/workflow_runs as projection_workflow_runs
import scherzo/state/projection/workstreams as projection_workstreams
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
      task_ref: replay_task_ref,
      outbox_kind: "linear_comment",
      dedupe_key: "run-1:success",
      payload_json: "{\"body\":\"ok\"}",
    ),
  ]) = projection.pending_outbox_replays(folded)
  assert replay_task_ref == record.linear_task_ref_fields("issue-1", None, None)
}

pub fn projection_records_workflow_contract_manifest_refs_test() {
  let folded =
    projection.fold([
      record.with_id(
        "inputs",
        2000,
        record.WorkflowRunInputsRecorded(
          run_id: "run-1",
          workflow_id: "implementation",
          workflow_fingerprint: "wf-1",
          artifact_ref: "runs/run-1/inputs.v1.json",
          artifact_sha256: "sha-inputs",
          artifact_bytes: 12,
        ),
      ),
      record.with_id(
        "outputs",
        3000,
        record.WorkflowRunOutputsRecorded(
          run_id: "run-1",
          workflow_id: "implementation",
          workflow_fingerprint: "wf-1",
          artifact_ref: "runs/run-1/outputs.v1.json",
          artifact_sha256: "sha-outputs",
          artifact_bytes: 34,
        ),
      ),
    ])

  let assert Some(input_ref) =
    projection.workflow_input_manifest(folded, "run-1")
  assert input_ref.artifact_ref == "runs/run-1/inputs.v1.json"
  assert input_ref.artifact_sha256 == "sha-inputs"
  assert input_ref.artifact_bytes == 12
  let assert Some(output_ref) =
    projection.workflow_output_manifest(folded, "run-1")
  assert output_ref.artifact_ref == "runs/run-1/outputs.v1.json"
  assert output_ref.recorded_at_ms == 3000
}

pub fn legacy_projection_snapshot_without_workstreams_decodes_test() {
  let legacy_snapshot =
    "{\"schema_version\":2,\"kind\":\"projection_snapshot\",\"runs\":[],\"retries\":[],\"parked_issues\":[],\"commands\":[],\"outbox\":[]}"

  let assert Ok(decoded) = projection.decode_string(legacy_snapshot)
  assert decoded.workstreams == dict.new()
  assert decoded.step_recoveries == dict.new()
  assert decoded.scheduled_jobs == dict.new()
  assert decoded.publication_attempts == dict.new()
}

pub fn projection_tracks_publication_attempt_history_and_latest_status_test() {
  let folded =
    projection.fold([
      record.with_id(
        "workflow-started",
        1000,
        record.WorkflowRunStarted(
          run_id: "run-1",
          workflow_id: "execplan",
          workflow_fingerprint: "wf-1",
          issue_id: "issue-1",
          issue_identifier: "LIV-739",
          issue_fingerprint: "issue-fingerprint",
          observed_updated_at_ms: 999,
          run_root: "root/run-1",
        ),
      ),
      record.with_id(
        "publication-planned",
        1010,
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
          manifest_ref: Some(
            "runs/run-1/publications/review_doc/version-1.json",
          ),
          manifest_sha256: Some("sha-1"),
          manifest_bytes: Some(10),
          error_code: None,
          error_message: None,
        ),
      ),
      record.with_id(
        "publication-failed",
        1020,
        record.PublicationAttemptRecorded(
          run_id: "run-1",
          workflow_id: "execplan",
          publication_id: "review_doc",
          series_id: "task-1:execplan:review_doc",
          attempt_id: "failed-1",
          status: "failed",
          required: True,
          retryable: True,
          retry_execution_available: False,
          version_id: None,
          manifest_ref: Some("runs/run-1/publications/review_doc/failed-1.json"),
          manifest_sha256: Some("sha-2"),
          manifest_bytes: Some(11),
          error_code: Some("unknown_output"),
          error_message: Some("missing"),
        ),
      ),
    ])

  let attempts =
    projection.publication_attempts_for_run(folded, "run-1", "review_doc")
  assert list.length(attempts) == 2
  let assert Ok(latest) =
    projection.latest_publication_for_run(folded, "run-1", "review_doc")
  assert latest.status == "failed"
  let assert Ok(series_latest) =
    projection.latest_publication_for_series(
      folded,
      "task-1:execplan:review_doc",
    )
  assert series_latest.attempt_id == "failed-1"
}

pub fn projection_records_step_recoveries_test() {
  let folded =
    projection.fold([
      record.with_id(
        "recovery-started",
        1000,
        record.WorkflowStepRecoveryStarted(
          run_id: "run-1",
          workflow_id: "implementation",
          step_id: "implement",
          failed_attempt_index: 1,
          recovery_attempt_number: 1,
          recovery_session_id: "recover-1",
          model: Some("gpt-5"),
          prompt_ref: ".scherzo/workflows/prompts/recover_failed_step.md",
        ),
      ),
      record.with_id(
        "recovery-finished",
        1010,
        record.WorkflowStepRecoveryFinished(
          run_id: "run-1",
          workflow_id: "implementation",
          step_id: "implement",
          failed_attempt_index: 1,
          recovery_attempt_number: 1,
          recovery_session_id: "recover-1",
          result: "retry_requested",
          summary: "Fixed tests",
          reason: "Ready for retry",
          retry_attempt_index: Some(2),
        ),
      ),
    ])

  let key = projection.step_recovery_key("run-1", "implement", 1, 1)
  let assert Ok(projection.StepRecoveryFinishedStatus(
    result: "retry_requested",
    retry_attempt_index: Some(2),
    finished_at_ms: 1010,
    ..,
  )) = dict.get(folded.step_recoveries, key)
}

pub fn projection_snapshot_round_trips_started_recovery_without_finish_test() {
  let folded =
    projection.fold([
      record.with_id(
        "recovery-started",
        1000,
        record.WorkflowStepRecoveryStarted(
          run_id: "run-1",
          workflow_id: "implementation",
          step_id: "implement",
          failed_attempt_index: 1,
          recovery_attempt_number: 1,
          recovery_session_id: "recover-1",
          model: Some("gpt-5"),
          prompt_ref: ".scherzo/workflows/prompts/recover_failed_step.md",
        ),
      ),
    ])

  let key = projection.step_recovery_key("run-1", "implement", 1, 1)
  let assert Ok(projection.StepRecoveryStartedStatus(
    recovery_session_id: "recover-1",
    started_at_ms: 1000,
    ..,
  )) = dict.get(folded.step_recoveries, key)

  let assert Ok(decoded) =
    projection.decode_string(projection.to_string(folded))
  let assert Ok(projection.StepRecoveryStartedStatus(
    recovery_session_id: "recover-1",
    started_at_ms: 1000,
    ..,
  )) = dict.get(decoded.step_recoveries, key)
}

pub fn workflow_run_provenance_survives_interrupted_and_snapshot_round_trip_test() {
  let folded =
    projection.fold([
      record.with_id(
        "run-started",
        1000,
        record.WorkflowRunStartedWithTask(
          run_id: "run-1",
          workflow_id: "implementation",
          workflow_fingerprint: "wf-1",
          issue_id: "issue-1",
          issue_identifier: "LIV-1",
          task_ref: record.linear_task_ref_fields(
            "issue-1",
            Some("LIV-1"),
            None,
          ),
          issue_fingerprint: "issue-fp-1",
          observed_updated_at_ms: 900,
          run_root: "test/tmp/projection/run-1",
        ),
      ),
      record.with_id(
        "run-interrupted",
        1010,
        record.WorkflowRunInterrupted(
          run_id: "run-1",
          workflow_id: "implementation",
          issue_id: "issue-1",
          reason: "daemon_shutdown",
        ),
      ),
    ])

  let assert Ok(provenance) =
    projection.workflow_run_provenance(folded, "run-1")
  assert provenance.workflow_id == "implementation"
  assert provenance.workflow_fingerprint == "wf-1"
  assert provenance.issue_identifier == "LIV-1"
  assert provenance.issue_fingerprint == "issue-fp-1"
  assert provenance.observed_updated_at_ms == 900
  assert provenance.run_root == "test/tmp/projection/run-1"
  assert provenance.task_ref.task_remote_id == "issue-1"
  assert provenance.task_ref.task_key == Some("LIV-1")

  let assert Ok(decoded) =
    projection.decode_string(projection.to_string(folded))
  let assert Ok(decoded_provenance) =
    projection.workflow_run_provenance(decoded, "run-1")
  assert decoded_provenance == provenance
}

pub fn workflow_run_provenance_repair_does_not_change_terminal_status_test() {
  let folded =
    projection.fold([
      record.with_id(
        "run-interrupted",
        1010,
        record.WorkflowRunInterrupted(
          run_id: "run-1",
          workflow_id: "implementation",
          issue_id: "issue-1",
          reason: "daemon_shutdown",
        ),
      ),
      record.with_id(
        "run-provenance-repaired",
        1020,
        record.WorkflowRunProvenanceRepaired(
          run_id: "run-1",
          workflow_id: "implementation",
          workflow_fingerprint: "wf-1",
          issue_id: "issue-1",
          issue_identifier: "LIV-1",
          task_ref: record.linear_task_ref_fields(
            "issue-1",
            Some("LIV-1"),
            None,
          ),
          issue_fingerprint: "issue-fp-1",
          observed_updated_at_ms: 900,
          run_root: "test/tmp/projection/run-1",
          repair_mode: "state_repair_explicit",
          source_evidence: ["workflow_run_interrupted:run-1"],
        ),
      ),
    ])

  let assert Ok(projection.WorkflowRunInterrupted(
    reason: "daemon_shutdown",
    run_root: "",
    ..,
  )) = dict.get(folded.workflow_runs, "run-1")
  let assert Ok(provenance) =
    projection.workflow_run_provenance(folded, "run-1")
  assert provenance.run_root == "test/tmp/projection/run-1"
  assert provenance.task_ref.task_key == Some("LIV-1")
}

pub fn projection_snapshot_with_partial_workstream_task_ref_fails_test() {
  let malformed_snapshot =
    "{\"schema_version\":2,\"kind\":\"projection_snapshot\",\"runs\":[],\"retries\":[],\"parked_issues\":[],\"commands\":[],\"outbox\":[],\"workstreams\":[{\"workstream_id\":\"linear:LIV-393\",\"task_backend_kind\":\"linear\"}]}"

  let assert Error(_) = projection.decode_string(malformed_snapshot)
}

pub fn projection_snapshot_with_partial_outbox_task_ref_fails_test() {
  let malformed_snapshot =
    "{\"schema_version\":2,\"kind\":\"projection_snapshot\",\"runs\":[],\"retries\":[],\"parked_issues\":[],\"commands\":[],\"outbox\":[{\"outbox_id\":\"outbox-1\",\"status\":\"pending_v2\",\"issue_id\":\"legacy-issue\",\"task_backend_kind\":\"github\",\"outbox_kind\":\"remote_command_ack\",\"dedupe_key\":\"dedupe-1\",\"payload_json\":\"{}\",\"pending_at_ms\":1000}]}"

  let assert Error(_) = projection.decode_string(malformed_snapshot)
}

pub fn legacy_issue_id_outbox_snapshot_decodes_test() {
  let legacy_snapshot =
    "{\"schema_version\":2,\"kind\":\"projection_snapshot\",\"runs\":[],\"retries\":[],\"parked_issues\":[],\"commands\":[],\"outbox\":[{\"outbox_id\":\"outbox-1\",\"status\":\"pending_v2\",\"issue_id\":\"issue-1\",\"outbox_kind\":\"linear_comment\",\"dedupe_key\":\"run-1\",\"payload_json\":\"{\\\"body\\\":\\\"ok\\\"}\",\"pending_at_ms\":1000},{\"outbox_id\":\"outbox-2\",\"status\":\"completed\",\"issue_id\":\"issue-1\",\"outbox_kind\":\"linear_comment\",\"completed_at_ms\":1001},{\"outbox_id\":\"outbox-3\",\"status\":\"failed\",\"issue_id\":\"issue-2\",\"outbox_kind\":\"linear_comment\",\"error_code\":\"http_500\",\"failed_at_ms\":1002}]}"

  let assert Ok(decoded) = projection.decode_string(legacy_snapshot)
  let assert Ok(projection.OutboxPendingV2(issue_id: "issue-1", ..)) =
    dict.get(decoded.outbox, "outbox-1")
  let assert Ok(projection.OutboxCompleted(issue_id: "issue-1", ..)) =
    dict.get(decoded.outbox, "outbox-2")
  let assert Ok(projection.OutboxFailed(issue_id: "issue-2", ..)) =
    dict.get(decoded.outbox, "outbox-3")
}

pub fn task_ref_outbox_projection_survives_snapshot_migration_test() {
  let task_ref =
    record.TaskRefFields(
      task_backend_kind: "github",
      task_remote_id: "octo/repo#42",
      task_key: Some("GH-42"),
      task_url: Some("https://github.example/octo/repo/issues/42"),
    )
  let folded =
    projection.fold([
      record.with_id(
        "outbox-task",
        1700,
        record.OutboxPendingV2WithTask(
          outbox_id: "outbox-task",
          task_ref: task_ref,
          outbox_kind: "remote_command_ack",
          dedupe_key: "remote_command_ack:github:event-42",
          payload_json: outbox_payload("github", "event-42", "octo/repo#42"),
        ),
      ),
    ])

  let assert Ok(projection.OutboxPendingV2WithTask(
    task_ref: stored_task_ref,
    ..,
  )) = dict.get(folded.outbox, "outbox-task")
  assert stored_task_ref == task_ref
  let assert Ok([projection.OutboxReplay(task_ref: replay_task_ref, ..)]) =
    projection.pending_outbox_replays(folded)
  assert replay_task_ref == task_ref

  let assert Ok(decoded) =
    projection.decode_string(projection.to_string(folded))
  let assert Ok(projection.OutboxPendingV2WithTask(
    task_ref: decoded_task_ref,
    ..,
  )) = dict.get(decoded.outbox, "outbox-task")
  assert decoded_task_ref == task_ref
}

pub fn known_task_refs_includes_remote_command_task_identity_test() {
  let folded =
    projection.fold([
      record.with_id(
        "remote-seen",
        1800,
        record.RemoteCommandSeen(
          backend_kind: "github",
          event_id: "event-42",
          task_remote_id: "octo/repo#42",
          task_key: Some("GH-42"),
          author_id: "user-1",
          command_name: "retry",
          excerpt: "/scherzo retry",
        ),
      ),
    ])

  assert has_task_ref(
    projection.known_task_refs(folded),
    "github",
    "octo/repo#42",
  )
}

pub fn known_issue_ids_omits_blank_issue_ids_test() {
  let folded =
    projection.fold([
      record.with_id(
        "scheduled-workflow-finished",
        1000,
        record.WorkflowRunFinished(
          run_id: "schedule-repair-20260505T120000Z",
          workflow_id: "repair",
          issue_id: "",
          outcome: "completed",
          token_total: 0,
          turns: 0,
        ),
      ),
      record.with_id(
        "retry-real",
        1001,
        record.RetryScheduled(
          issue_id: "issue-1",
          issue_identifier: "SCH-1",
          delay_ms: 5000,
          generation: 1,
          reason: "failure",
        ),
      ),
    ])

  let ids = projection.known_issue_ids(folded)
  assert list.contains(ids, "") == False
  assert list.contains(ids, "issue-1")
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

pub fn scheduled_records_fold_into_status_test() {
  let run_id = "schedule-repair-20260505T120000Z"
  let folded =
    projection.fold([
      record.with_id(
        "scheduled-due",
        1000,
        record.ScheduledJobDue(
          job_id: "repair",
          workflow_id: "repair",
          due_at_ms: 900_000,
          run_id: run_id,
          trigger: "automatic",
        ),
      ),
      record.with_id(
        "scheduled-pending",
        1001,
        record.ScheduledRunPending(
          job_id: "repair",
          workflow_id: "repair",
          due_at_ms: 900_000,
          run_id: run_id,
          trigger: "automatic",
          requested_at_ms: 1001,
        ),
      ),
      record.with_id(
        "scheduled-blocked",
        1002,
        record.ScheduledRunPendingBlocked(
          job_id: "repair",
          workflow_id: "repair",
          due_at_ms: 900_000,
          run_id: run_id,
          reason: "waiting_for_global_slot",
          observed_at_ms: 1002,
        ),
      ),
      record.with_id(
        "scheduled-skip",
        1003,
        record.ScheduledJobSkipped(
          job_id: "repair",
          workflow_id: "repair",
          due_at_ms: 1_800_000,
          run_id: "schedule-repair-20260505T121500Z",
          reason: "overlap_running",
          skipped_count: 2,
        ),
      ),
      record.with_id(
        "scheduled-started",
        1004,
        record.ScheduledRunStarted(
          job_id: "repair",
          workflow_id: "repair",
          due_at_ms: 900_000,
          started_at_ms: 1004,
          run_id: run_id,
          attempt: 1,
          session_id: "scheduled-session",
          run_root: "workspaces/repair/scheduled/repair/" <> run_id,
        ),
      ),
      record.with_id(
        "scheduled-failed",
        1005,
        record.ScheduledRunFailed(
          job_id: "repair",
          workflow_id: "repair",
          due_at_ms: 900_000,
          run_id: run_id,
          attempt: 1,
          finished_at_ms: 1005,
          reason: "workflow_step_failed",
          retry_exhausted: False,
          run_root: Some("workspaces/repair/scheduled/repair/" <> run_id),
        ),
      ),
      record.with_id(
        "scheduled-retry",
        1006,
        record.ScheduledRunRetryScheduled(
          job_id: "repair",
          workflow_id: "repair",
          due_at_ms: 900_000,
          run_id: run_id,
          next_attempt: 2,
          delay_ms: 10_000,
          generation: 1,
          reason: "workflow_step_failed",
        ),
      ),
      record.with_id(
        "scheduled-report-failed",
        1007,
        record.ScheduledFailureReportFailed(
          job_id: "repair",
          workflow_id: "repair",
          due_at_ms: 900_000,
          run_id: run_id,
          attempt: 2,
          dedupe_key: "scheduled-job:repair",
          error_code: "linear_api_request",
          error_message: "network",
          next_retry_at_ms: 20_000,
          generation: 1,
        ),
      ),
      record.with_id(
        "scheduled-reported",
        1008,
        record.ScheduledFailureReported(
          job_id: "repair",
          workflow_id: "repair",
          due_at_ms: 900_000,
          run_id: run_id,
          attempt: 2,
          dedupe_key: "scheduled-job:repair",
          linear_issue_id: "linear-issue",
          action: "created",
        ),
      ),
    ])

  let assert Ok(status) = projection.scheduled_status_for(folded, "repair")
  assert status.state == projection.ScheduledTerminalFailure
  assert status.last_due_at_ms == Some(1_800_000)
  assert status.last_failure_at_ms == Some(1005)
  assert status.last_failure_reason == Some("workflow_step_failed")
  assert status.retry_count == 1
  assert status.skipped_overlap_count == 2
  assert status.failure_issue_id == Some("linear-issue")
  assert status.failure_dedupe_key == Some("scheduled-job:repair")
  assert status.report_retry == None
  assert status.recent_run_ids == ["schedule-repair-20260505T121500Z", run_id]
  let assert Ok(decoded) =
    projection.decode_string(projection.to_string(folded))
  assert decoded == folded
}

pub fn scheduled_due_preserves_existing_history_test() {
  let first_run = "schedule-repair-20260505T120000Z"
  let second_run = "schedule-repair-20260505T121500Z"
  let folded =
    projection.fold([
      record.with_id(
        "scheduled-due-1",
        1000,
        record.ScheduledJobDue(
          job_id: "repair",
          workflow_id: "repair",
          due_at_ms: 900_000,
          run_id: first_run,
          trigger: "automatic",
        ),
      ),
      record.with_id(
        "scheduled-started-1",
        1001,
        record.ScheduledRunStarted(
          job_id: "repair",
          workflow_id: "repair",
          due_at_ms: 900_000,
          started_at_ms: 1001,
          run_id: first_run,
          attempt: 1,
          session_id: "session-1",
          run_root: "workspaces/repair/scheduled/repair/" <> first_run,
        ),
      ),
      record.with_id(
        "scheduled-succeeded-1",
        1002,
        record.ScheduledRunSucceeded(
          job_id: "repair",
          workflow_id: "repair",
          due_at_ms: 900_000,
          run_id: first_run,
          attempt: 1,
          finished_at_ms: 1002,
          token_total: 42,
          turns: 3,
        ),
      ),
      record.with_id(
        "scheduled-due-2",
        1003,
        record.ScheduledJobDue(
          job_id: "repair",
          workflow_id: "repair",
          due_at_ms: 1_800_000,
          run_id: second_run,
          trigger: "automatic",
        ),
      ),
    ])

  let assert Ok(status) = projection.scheduled_status_for(folded, "repair")
  assert status.state == projection.ScheduledDuePending
  assert status.last_success_at_ms == Some(1002)
  assert status.last_success_run_id == Some(first_run)
  assert status.last_due_at_ms == Some(1_800_000)
  assert status.recent_run_ids == [second_run, first_run]
}

pub fn scheduled_recent_run_ids_are_capped_test() {
  let folded = projection.fold(scheduled_due_records(30, []))

  let assert Ok(status) = projection.scheduled_status_for(folded, "repair")
  assert list.length(status.recent_run_ids) == 25
  let assert Ok("run-30") = list.first(status.recent_run_ids)
  let assert Ok("run-6") = list.last(status.recent_run_ids)
  assert !list.contains(status.recent_run_ids, "run-5")
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

pub fn remote_command_records_project_to_command_receipts_test() {
  let folded =
    projection.fold([
      record.with_id(
        "remote-1",
        100,
        record.RemoteCommandSeen(
          backend_kind: "linear",
          event_id: "comment-remote",
          task_remote_id: "issue-command",
          task_key: Some("LIV-266"),
          author_id: "user-1",
          command_name: "retry",
          excerpt: "/scherzo retry",
        ),
      ),
      record.with_id(
        "remote-2",
        200,
        record.RemoteCommandStarted(
          backend_kind: "linear",
          event_id: "comment-remote",
          task_remote_id: "issue-command",
          command_name: "retry",
        ),
      ),
      record.with_id(
        "remote-3",
        300,
        record.RemoteCommandCompleted(
          backend_kind: "linear",
          event_id: "comment-remote",
          task_remote_id: "issue-command",
          status: "applied",
          message_excerpt: "retry queued",
        ),
      ),
      record.with_id(
        "remote-4",
        400,
        record.RemoteCommandAcked(
          backend_kind: "linear",
          event_id: "comment-remote",
          task_remote_id: "issue-command",
        ),
      ),
    ])

  let assert projection.CommandReceiptCompleted(
    issue_id: "issue-command",
    author_id: "user-1",
    command_name: "retry",
    excerpt: "/scherzo retry",
    result_status: "applied",
    message_excerpt: "retry queued",
    seen_at_ms: 100,
    started_at_ms: 200,
    completed_at_ms: 300,
    acked_at_ms: Some(400),
  ) = projection.command_receipt(folded, "comment-remote")
}

pub fn mixed_linear_and_remote_command_records_do_not_duplicate_receipts_test() {
  let folded =
    projection.fold([
      record.with_id(
        "linear-1",
        100,
        record.LinearCommandSeen(
          comment_id: "comment-1",
          issue_id: "issue-1",
          author_id: "user-1",
          command_name: "retry",
          excerpt: "/scherzo retry",
        ),
      ),
      record.with_id(
        "linear-2",
        200,
        record.LinearCommandStarted(
          comment_id: "comment-1",
          issue_id: "issue-1",
          command_name: "retry",
        ),
      ),
      record.with_id(
        "linear-3",
        300,
        record.LinearCommandCompleted(
          comment_id: "comment-1",
          issue_id: "issue-1",
          status: "ok",
          message_excerpt: "Retry queued",
        ),
      ),
      record.with_id(
        "remote-4",
        400,
        record.RemoteCommandAcked(
          backend_kind: "linear",
          event_id: "comment-1",
          task_remote_id: "issue-1",
        ),
      ),
    ])

  assert list.length(dict.keys(folded.command_receipts)) == 1
  let assert projection.CommandReceiptCompleted(
    issue_id: "issue-1",
    author_id: "user-1",
    command_name: "retry",
    excerpt: "/scherzo retry",
    result_status: "ok",
    message_excerpt: "Retry queued",
    seen_at_ms: 100,
    started_at_ms: 200,
    completed_at_ms: 300,
    acked_at_ms: Some(400),
  ) = projection.command_receipt(folded, "comment-1")
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
  assert_unsupported_projection_snapshot(
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
  assert_projection_snapshot_error(contents, "malformed projection snapshot:")
}

fn assert_unsupported_projection_snapshot(contents: String) -> Nil {
  assert_projection_snapshot_error(contents, "unsupported schema version ")
}

fn assert_projection_snapshot_error(
  contents: String,
  expected_prefix: String,
) -> Nil {
  let assert Error(error) = projection.decode_string(contents)
  let message = projection.describe_decode_error(error)
  assert string.starts_with(message, expected_prefix)
  Nil
}

fn outbox_payload(
  backend_kind: String,
  event_id: String,
  task_remote_id: String,
) -> String {
  "{\"type\":\"remote_command_ack\",\"task_backend_kind\":\""
  <> backend_kind
  <> "\",\"event_id\":\""
  <> event_id
  <> "\",\"task_remote_id\":\""
  <> task_remote_id
  <> "\",\"body\":\"ack\"}"
}

fn has_task_ref(
  refs: List(record.TaskRefFields),
  backend_kind: String,
  task_remote_id: String,
) -> Bool {
  list.any(refs, fn(ref) {
    ref.task_backend_kind == backend_kind
    && ref.task_remote_id == task_remote_id
  })
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

fn scheduled_due_records(
  next_index: Int,
  records: List(record.LedgerRecord),
) -> List(record.LedgerRecord) {
  case next_index {
    0 -> records
    index ->
      scheduled_due_records(index - 1, [
        record.with_id(
          "scheduled-due-" <> int.to_string(index),
          index,
          record.ScheduledJobDue(
            job_id: "repair",
            workflow_id: "repair",
            due_at_ms: index * 1000,
            run_id: "run-" <> int.to_string(index),
            trigger: "automatic",
          ),
        ),
        ..records
      ])
  }
}

pub fn bounded_context_projection_helpers_cover_remaining_slices_test() {
  let runs = projection_legacy_runs.started(dict.new(), "run-1", "running")
  let assert ["running"] = dict.values(runs)
  assert projection_steps.attempt_key("run-1", "step-1", 2)
    == "run-1\u{001f}step-1\u{001f}2"
  assert projection_steps.recovery_key("run-1", "step-1", 2, 1)
    == "run-1\u{001f}step-1\u{001f}2\u{001f}1"
  assert projection_steps.session_fact_values(
      "workflow",
      "ws",
      "/tmp/ws",
      "workflow",
      "ws",
      "/tmp/ws",
      "session-1",
      "session.json",
      0,
    )
    == #(Some("session-1"), Some("session.json"), 1)
  assert projection_issue_recovery.retry_due_at_ms(#(1000, 2000), fn(value) {
      Ok(value)
    })
    == Ok(3000)
  assert projection_workflow_runs.has_run(
    dict.from_list([#("run-1", "active")]),
    "run-1",
  )
  assert projection_commands.command_receipt(dict.new(), "comment-1", "unseen")
    == "unseen"
  assert projection_outbox.pending_replays(
      dict.from_list([#("outbox-1", "pending")]),
      fn(entry) {
        let #(outbox_id, _) = entry
        Ok(outbox_id)
      },
    )
    == Ok(["outbox-1"])
  assert projection_workstreams.update_status(
      dict.new(),
      "workstream-1",
      fn(id) { id },
      fn(_) { "updated" },
    )
    == dict.from_list([#("workstream-1", "updated")])
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
