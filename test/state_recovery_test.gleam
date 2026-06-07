import birl
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import legacy_ledger_fixtures
import scherzo/config/types as config_types
import scherzo/orchestrator/core
import scherzo/runtime/state as orchestrator_state
import scherzo/session/event as session_event
import scherzo/session/recovery as session_recovery
import scherzo/state/outbox
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import scherzo/workflow_completion_policy
import scherzo/workflow_outcome

pub fn current_projection_sources_emit_only_backed_recovery_metadata_test() {
  let projection =
    projection.fold([
      record.with_id(
        "run-started",
        1000,
        record.RunStarted(
          run_id: "run-1",
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          workspace_path: ".scherzo/workspaces/ABC-1",
        ),
      ),
      record.with_id(
        "run-interrupted",
        2000,
        record.RunInterrupted(
          run_id: "run-2",
          issue_id: "issue-2",
          reason: "daemon_restart",
        ),
      ),
      record.with_id(
        "park",
        3000,
        record.IssueParkedV2(
          issue_id: "issue-3",
          issue_identifier: "ABC-3",
          reason: "operator hold",
          release_policy: "explicit_unpark_only",
          issue_fingerprint: "fingerprint",
          observed_updated_at_ms: 2900,
        ),
      ),
      record.with_id(
        "issue-content-drift-park",
        3100,
        record.IssueParkedV2(
          issue_id: "issue-5",
          issue_identifier: "ABC-5",
          reason: "issue_content_drift:issue_fingerprint_changed",
          release_policy: "explicit_unpark_only",
          issue_fingerprint: "fingerprint",
          observed_updated_at_ms: 3000,
        ),
      ),
      record.with_id(
        "workflow-definition-drift-park",
        3200,
        record.IssueParkedV2(
          issue_id: "issue-6",
          issue_identifier: "ABC-6",
          reason: "workflow_definition_drift:workflow_fingerprint_changed",
          release_policy: "explicit_unpark_only",
          issue_fingerprint: "fingerprint",
          observed_updated_at_ms: 3100,
        ),
      ),
      record.with_id(
        "issue-state-drift-park",
        3300,
        record.IssueParkedV2(
          issue_id: "issue-7",
          issue_identifier: "ABC-7",
          reason: "issue_state_drift:terminal_state",
          release_policy: "explicit_unpark_only",
          issue_fingerprint: "fingerprint",
          observed_updated_at_ms: 3200,
        ),
      ),
      record.with_id(
        "run-finished",
        4000,
        record.RunFinished(
          run_id: "run-4",
          issue_id: "issue-4",
          classification: "success",
          token_total: 10,
          turns: 1,
        ),
      ),
    ])

  let assert Ok(running) = dict.get(projection.runs, "run-1")
  let assert Some(running_info) =
    session_recovery.interrupted_run("run-1", running, None)
  assert running_info.status == session_event.Interrupted
  assert running_info.workflow_run_id == Some("run-1")
  assert running_info.workflow_step_id == None
  assert running_info.previous_pi_session_id == None
  assert running_info.source == "projection.run_running"

  let assert Ok(interrupted) = dict.get(projection.runs, "run-2")
  let assert Some(interrupted_info) =
    session_recovery.interrupted_run("run-2", interrupted, Some("pi-current"))
  assert interrupted_info.status == session_event.Interrupted
  assert interrupted_info.current_pi_session_id == Some("pi-current")
  assert interrupted_info.previous_pi_session_id == None

  let assert Ok(parked) = dict.get(projection.parked_issues, "issue-3")
  let parked_info = session_recovery.parked_issue(parked)
  assert parked_info.status == session_event.Parked
  assert parked_info.park_reason == Some("operator hold")
  assert parked_info.park_release_policy == Some("explicit_unpark_only")
  assert parked_info.drift_kind == None

  let assert Ok(issue_drift) = dict.get(projection.parked_issues, "issue-5")
  let issue_drift_info = session_recovery.parked_issue(issue_drift)
  assert issue_drift_info.drift_kind == Some("issue_content")

  let assert Ok(workflow_drift) = dict.get(projection.parked_issues, "issue-6")
  let workflow_drift_info = session_recovery.parked_issue(workflow_drift)
  assert workflow_drift_info.drift_kind == Some("workflow_definition")

  let assert Ok(state_drift) = dict.get(projection.parked_issues, "issue-7")
  let state_drift_info = session_recovery.parked_issue(state_drift)
  assert state_drift_info.drift_kind == Some("issue_state")

  let assert Ok(finished) = dict.get(projection.runs, "run-4")
  assert session_recovery.interrupted_run("run-4", finished, None) == None
}

pub fn unfinished_run_becomes_interrupted_retry_test() {
  let projection =
    projection.fold([
      record.with_id(
        "run-started",
        1000,
        record.RunStarted(
          run_id: "run-1",
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          workspace_path: ".scherzo/workspaces/ABC-1",
        ),
      ),
    ])
  let refreshed = issue("issue-1", "ABC-1", "Todo")
  let assert Ok(plan) = recovery.plan(projection, config(), [refreshed], 7000)

  assert plan.runtime.issue_counters
    |> dict.get(orchestrator_state.linear_issue_id_identity("issue-1"))
    |> unwrap_counter
    |> fn(counter) { counter.failure_attempts }
    == 1
  assert has_record_kind(plan.records_to_append, "run_interrupted")
  assert has_record_kind(plan.records_to_append, "issue_counter_updated")
  assert has_record_kind(plan.records_to_append, "retry_scheduled")
  let assert [
    recovery.RecoveredRetry(
      issue_id: "issue-1",
      issue_identifier: "ABC-1",
      delay_ms: 10_000,
      generation: 1,
      reason: "failure",
    ),
  ] = plan.retry_timers
}

pub fn interrupted_run_recovery_is_idempotent_test() {
  let projection =
    projection.fold([
      record.with_id(
        "run-started",
        1000,
        record.RunStarted(
          run_id: "run-1",
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          workspace_path: ".scherzo/workspaces/ABC-1",
        ),
      ),
      record.with_id(
        "run-interrupted",
        2000,
        record.RunInterrupted(
          run_id: "run-1",
          issue_id: "issue-1",
          reason: "daemon_restart",
        ),
      ),
      record.with_id(
        "counter",
        2100,
        record.IssueCounterUpdated(
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          failure_attempts: 1,
          worker_sessions: 0,
          observed_updated_at_ms: 2000,
          source_run_id: Some("run-1"),
        ),
      ),
    ])
  let refreshed = issue("issue-1", "ABC-1", "Todo")

  let assert Ok(first) = recovery.plan(projection, config(), [refreshed], 7000)
  let assert Ok(second) = recovery.plan(projection, config(), [refreshed], 8000)

  assert counter_failure_attempts(first.runtime, "issue-1") == 1
  assert counter_failure_attempts(second.runtime, "issue-1") == 1
}

pub fn workflow_candidates_preserve_started_recovery_without_finish_test() {
  let projection =
    projection.fold([
      record.with_id(
        "workflow-run-started",
        1000,
        record.WorkflowRunStarted(
          run_id: "run-1",
          workflow_id: "implementation",
          workflow_fingerprint: "wf-1",
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          issue_fingerprint: "issue-fp-1",
          observed_updated_at_ms: 900,
          run_root: "test/tmp/state-recovery/run-1",
        ),
      ),
      record.with_id(
        "recovery-started",
        1010,
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

  let assert [candidate] = recovery.workflow_candidates(projection)
  assert candidate.run_id == "run-1"
  assert candidate.recovery_evidence == workflow_outcome.StepRecoveryRan
}

pub fn unfinished_run_terminal_issue_cleans_known_workspace_test() {
  let projection =
    projection.fold([
      record.with_id(
        "known",
        900,
        record.KnownWorkspace(
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          workspace_path: ".scherzo/workspaces/ABC-1",
        ),
      ),
      record.with_id(
        "run-started",
        1000,
        record.RunStarted(
          run_id: "run-1",
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          workspace_path: ".scherzo/workspaces/ABC-1",
        ),
      ),
    ])
  let refreshed = issue("issue-1", "ABC-1", "Done")

  let assert Ok(plan) = recovery.plan(projection, config(), [refreshed], 7000)

  let assert [
    recovery.CleanupRequest(
      issue_id: "issue-1",
      issue_identifier: "ABC-1",
      workspace_path: ".scherzo/workspaces/ABC-1",
    ),
  ] = plan.cleanup_workspaces
  assert !has_retry(plan.runtime, "issue-1")
}

pub fn parked_issue_survives_restart_test() {
  let projection =
    projection.fold([
      record.with_id(
        "park",
        1000,
        record.IssueParkedV2(
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          reason: "operator",
          release_policy: "explicit_unpark_only",
          issue_fingerprint: "old",
          observed_updated_at_ms: 1000,
        ),
      ),
    ])
  let refreshed =
    tracker_issue.Issue(..issue("issue-1", "ABC-1", "Todo"), title: "changed")

  let assert Ok(plan) = recovery.plan(projection, config(), [refreshed], 7000)

  assert dict.has_key(
    plan.runtime.parked,
    orchestrator_state.linear_issue_id_identity("issue-1"),
  )
}

pub fn auto_parked_issue_with_same_fingerprint_survives_restart_test() {
  let refreshed = issue("issue-1", "ABC-1", "Todo")
  let projection =
    projection.fold([
      record.with_id(
        "park",
        1000,
        record.IssueParkedV2(
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          reason: "max_retry_attempts",
          release_policy: "auto_unpark_on_issue_change",
          issue_fingerprint: core.issue_fingerprint(refreshed),
          observed_updated_at_ms: 1000,
        ),
      ),
    ])

  let assert Ok(plan) = recovery.plan(projection, config(), [refreshed], 7000)

  assert dict.has_key(
    plan.runtime.parked,
    orchestrator_state.linear_issue_id_identity("issue-1"),
  )
}

pub fn auto_parked_legacy_stateful_fingerprint_survives_state_change_test() {
  let refreshed = issue("issue-1", "ABC-1", "In Progress")
  let projection =
    projection.fold([
      record.with_id(
        "park",
        1000,
        record.IssueParkedV2(
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          reason: "max_retry_attempts",
          release_policy: "auto_unpark_on_issue_change",
          issue_fingerprint: "7:issue-1|5:ABC-1|11:Title ABC-1|none|none|4:Todo|none|4:true|",
          observed_updated_at_ms: 1000,
        ),
      ),
    ])

  let assert Ok(plan) = recovery.plan(projection, config(), [refreshed], 7000)

  assert dict.has_key(
    plan.runtime.parked,
    orchestrator_state.linear_issue_id_identity("issue-1"),
  )
  assert plan.records_to_append == []
}

pub fn auto_parked_issue_with_new_fingerprint_unparks_test() {
  let original = issue("issue-1", "ABC-1", "Todo")
  let refreshed = tracker_issue.Issue(..original, title: "changed")
  let projection =
    projection.fold([
      record.with_id(
        "counter",
        900,
        record.IssueCounterUpdated(
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          failure_attempts: 2,
          worker_sessions: 0,
          observed_updated_at_ms: 900,
          source_run_id: None,
        ),
      ),
      record.with_id(
        "retry",
        950,
        record.RetryScheduled(
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          delay_ms: 1000,
          generation: 1,
          reason: "failure",
        ),
      ),
      record.with_id(
        "park",
        1000,
        record.IssueParkedV2(
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          reason: "max_retry_attempts",
          release_policy: "auto_unpark_on_issue_change",
          issue_fingerprint: core.issue_fingerprint(original),
          observed_updated_at_ms: 1000,
        ),
      ),
    ])

  let assert Ok(plan) = recovery.plan(projection, config(), [refreshed], 7000)

  assert !dict.has_key(
    plan.runtime.parked,
    orchestrator_state.linear_issue_id_identity("issue-1"),
  )
  assert !dict.has_key(
    plan.runtime.retry_attempts,
    orchestrator_state.linear_issue_id_identity("issue-1"),
  )
  assert !dict.has_key(
    plan.runtime.issue_counters,
    orchestrator_state.linear_issue_id_identity("issue-1"),
  )
}

pub fn overdue_retry_is_scheduled_immediately_test() {
  let projection =
    projection.fold([
      record.with_id(
        "retry",
        1000,
        record.RetryScheduled(
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          delay_ms: 5000,
          generation: 2,
          reason: "failure",
        ),
      ),
    ])
  let refreshed = issue("issue-1", "ABC-1", "Todo")

  let assert Ok(plan) = recovery.plan(projection, config(), [refreshed], 7000)

  let assert [recovery.RecoveredRetry(delay_ms: 0, generation: 2, ..)] =
    plan.retry_timers
  let assert Ok(retry) =
    dict.get(
      plan.runtime.retry_attempts,
      orchestrator_state.linear_issue_id_identity("issue-1"),
    )
  assert retry.delay_ms == 0
}

pub fn future_retry_keeps_remaining_delay_test() {
  let projection =
    projection.fold([
      record.with_id(
        "retry",
        1000,
        record.RetryScheduled(
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          delay_ms: 5000,
          generation: 2,
          reason: "failure",
        ),
      ),
    ])
  let refreshed = issue("issue-1", "ABC-1", "Todo")

  let assert Ok(plan) = recovery.plan(projection, config(), [refreshed], 3000)

  let assert [recovery.RecoveredRetry(delay_ms: 3000, generation: 2, ..)] =
    plan.retry_timers
  let assert Ok(retry) =
    dict.get(
      plan.runtime.retry_attempts,
      orchestrator_state.linear_issue_id_identity("issue-1"),
    )
  assert retry.delay_ms == 3000
}

pub fn mixed_workflow_task_ref_history_restores_non_linear_retry_test() {
  let non_linear_ref =
    record.TaskRefFields(
      task_backend_kind: "fake",
      task_remote_id: "issue-1",
      task_key: Some("ABC-1"),
      task_url: Some("https://example.test/cards/issue-1"),
    )
  let projection =
    projection.fold([
      record.with_id(
        "run-started",
        900,
        record.WorkflowRunStartedWithTask(
          run_id: "run-1",
          workflow_id: "implementation",
          workflow_fingerprint: "workflow-fingerprint",
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          task_ref: non_linear_ref,
          issue_fingerprint: "issue-fingerprint",
          observed_updated_at_ms: 800,
          run_root: "test/tmp/state-recovery/run-1",
        ),
      ),
      record.with_id(
        "run-finished-linear-fallback",
        950,
        record.WorkflowRunFinished(
          run_id: "run-1",
          workflow_id: "implementation",
          issue_id: "issue-1",
          outcome: "failure",
          token_total: 0,
          turns: 1,
        ),
      ),
      record.with_id(
        "retry",
        1000,
        record.RetryScheduled(
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          delay_ms: 5000,
          generation: 2,
          reason: "failure",
        ),
      ),
    ])
  let assert Ok(preserved_ref) =
    projection.workflow_task_ref(projection, "run-1")
  assert preserved_ref == non_linear_ref
  let refreshed = issue("issue-1", "ABC-1", "Todo")

  let assert Ok(plan) = recovery.plan(projection, config(), [refreshed], 7000)

  let fake_identity =
    orchestrator_state.issue_id_identity_for_backend("issue-1", "fake")
  let linear_identity = orchestrator_state.linear_issue_id_identity("issue-1")
  let assert Ok(retry) = dict.get(plan.runtime.retry_attempts, fake_identity)
  assert retry.task_ref.backend_kind == "fake"
  assert retry.task_ref.remote_id == "issue-1"
  assert retry.task_ref.key == Some("ABC-1")
  assert !dict.has_key(plan.runtime.retry_attempts, linear_identity)
  assert dict.has_key(plan.runtime.claimed, fake_identity)
  assert !dict.has_key(plan.runtime.claimed, linear_identity)
}

pub fn retry_for_missing_issue_is_cancelled_during_recovery_test() {
  let projection =
    projection.fold([
      record.with_id(
        "retry",
        1000,
        record.RetryScheduled(
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          delay_ms: 5000,
          generation: 2,
          reason: "failure",
        ),
      ),
    ])

  let assert Ok(plan) = recovery.plan(projection, config(), [], 7000)

  assert plan.retry_timers == []
  assert !dict.has_key(
    plan.runtime.retry_attempts,
    orchestrator_state.linear_issue_id_identity("issue-1"),
  )
  assert !dict.has_key(
    plan.runtime.claimed,
    orchestrator_state.linear_issue_id_identity("issue-1"),
  )
  assert plan.warnings == []
  assert has_retry_cancelled(
    plan.records_to_append,
    "issue-1",
    2,
    "recovery_missing_issue",
  )
}

pub fn retry_for_configured_failure_state_is_restored_during_recovery_test() {
  let projection =
    projection.fold([
      record.with_id(
        "retry",
        1000,
        record.RetryScheduled(
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          delay_ms: 5000,
          generation: 2,
          reason: "failure",
        ),
      ),
    ])
  let refreshed = issue("issue-1", "ABC-1", "Triage")

  let assert Ok(plan) =
    recovery.plan(
      projection,
      config_with_failure_state("Triage"),
      [refreshed],
      7000,
    )

  let assert [recovery.RecoveredRetry(delay_ms: 0, generation: 2, ..)] =
    plan.retry_timers
  assert dict.has_key(
    plan.runtime.retry_attempts,
    orchestrator_state.linear_issue_id_identity("issue-1"),
  )
  assert dict.has_key(
    plan.runtime.claimed,
    orchestrator_state.linear_issue_id_identity("issue-1"),
  )
  assert plan.warnings == []
  assert !has_retry_cancelled(
    plan.records_to_append,
    "issue-1",
    2,
    "recovery_non_retryable_state:Triage",
  )
}

pub fn retry_for_non_retryable_issue_is_cancelled_during_recovery_test() {
  let projection =
    projection.fold([
      record.with_id(
        "retry",
        1000,
        record.RetryScheduled(
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          delay_ms: 5000,
          generation: 2,
          reason: "failure",
        ),
      ),
    ])
  let refreshed = issue("issue-1", "ABC-1", "Backlog")

  let assert Ok(plan) =
    recovery.plan(
      projection,
      config_with_failure_state("Triage"),
      [refreshed],
      7000,
    )

  assert plan.retry_timers == []
  assert !dict.has_key(
    plan.runtime.retry_attempts,
    orchestrator_state.linear_issue_id_identity("issue-1"),
  )
  assert !dict.has_key(
    plan.runtime.claimed,
    orchestrator_state.linear_issue_id_identity("issue-1"),
  )
  assert plan.warnings == []
  assert has_retry_cancelled(
    plan.records_to_append,
    "issue-1",
    2,
    "recovery_non_retryable_state:Backlog",
  )
}

pub fn retry_for_terminal_issue_is_cancelled_during_recovery_test() {
  let projection =
    projection.fold([
      record.with_id(
        "retry",
        1000,
        record.RetryScheduled(
          issue_id: "issue-1",
          issue_identifier: "ABC-1",
          delay_ms: 5000,
          generation: 2,
          reason: "failure",
        ),
      ),
    ])
  let refreshed = issue("issue-1", "ABC-1", "Done")

  let assert Ok(plan) = recovery.plan(projection, config(), [refreshed], 7000)

  assert plan.retry_timers == []
  assert !dict.has_key(
    plan.runtime.retry_attempts,
    orchestrator_state.linear_issue_id_identity("issue-1"),
  )
  assert !dict.has_key(
    plan.runtime.claimed,
    orchestrator_state.linear_issue_id_identity("issue-1"),
  )
  assert plan.warnings == []
  assert has_retry_cancelled(
    plan.records_to_append,
    "issue-1",
    2,
    "recovery_terminal_issue",
  )
}

pub fn payload_less_pending_outbox_is_marked_failed_test() {
  let projection =
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

  let assert Ok(plan) = recovery.plan(projection, config(), [], 7000)

  assert plan.outbox_to_replay == []
  assert has_outbox_failed(
    plan.records_to_append,
    "outbox-old",
    "outbox_payload_missing",
  )
  assert list.contains(
    plan.warnings,
    "outbox_replay_failed:outbox-old:outbox_payload_missing",
  )
}

pub fn unsupported_pending_outbox_payload_is_marked_failed_test() {
  let projection =
    projection.fold([
      record.with_id(
        "outbox-comment",
        1000,
        record.OutboxPendingV2(
          outbox_id: "outbox-comment",
          issue_id: "issue-1",
          outbox_kind: "linear_comment",
          dedupe_key: "comment",
          payload_json: "{\"type\":\"linear_comment\",\"body\":\"hello\"}",
        ),
      ),
    ])

  let assert Ok(plan) = recovery.plan(projection, config(), [], 7000)

  assert plan.outbox_to_replay == []
  assert has_outbox_failed(
    plan.records_to_append,
    "outbox-comment",
    "unsupported_outbox_kind:linear_comment",
  )
}

pub fn invalid_pending_outbox_payload_is_marked_failed_test() {
  let projection =
    projection.fold([
      record.with_id(
        "outbox-invalid",
        1000,
        record.OutboxPendingV2(
          outbox_id: "outbox-invalid",
          issue_id: "issue-1",
          outbox_kind: "linear_command_ack",
          dedupe_key: "ack",
          payload_json: "not-json",
        ),
      ),
    ])

  let assert Ok(plan) = recovery.plan(projection, config(), [], 7000)

  assert plan.outbox_to_replay == []
  assert has_outbox_failed(
    plan.records_to_append,
    "outbox-invalid",
    "invalid_outbox_payload",
  )
}

pub fn linear_command_ack_outbox_is_marked_failed_test() {
  let projection =
    projection.fold([
      record.with_id(
        "outbox-ack",
        1000,
        record.OutboxPendingV2(
          outbox_id: "outbox-ack",
          issue_id: "issue-1",
          outbox_kind: "linear_command_ack",
          dedupe_key: "ack",
          payload_json: "{\"type\":\"linear_command_ack\",\"body\":\"ack\"}",
        ),
      ),
    ])

  let assert Ok(plan) = recovery.plan(projection, config(), [], 7000)

  assert plan.outbox_to_replay == []
  assert has_outbox_failed(
    plan.records_to_append,
    "outbox-ack",
    "unsupported_outbox_kind:linear_command_ack",
  )
}

pub fn remote_command_ack_outbox_is_marked_failed_test() {
  let payload =
    outbox.remote_command_ack_payload(
      "linear",
      "comment-1",
      "issue-1",
      "ack",
      [],
    )
  let projection =
    projection.fold([
      record.with_id(
        "outbox-remote-ack",
        1000,
        record.OutboxPendingV2(
          outbox_id: "comment-1",
          issue_id: "issue-1",
          outbox_kind: "remote_command_ack",
          dedupe_key: "remote_command_ack:comment-1",
          payload_json: payload,
        ),
      ),
    ])

  let assert Ok(plan) = recovery.plan(projection, config(), [], 7000)

  assert plan.outbox_to_replay == []
  assert has_outbox_failed(
    plan.records_to_append,
    "comment-1",
    "unsupported_outbox_kind:remote_command_ack",
  )
}

pub fn remote_command_ack_failure_dedupes_by_task_ref_and_event_test() {
  let acked_task_ref = task_ref("github", "octo/repo#1")
  let other_task_ref = task_ref("github", "octo/repo#2")
  let projection =
    projection.fold([
      record.with_id(
        "remote-acked",
        1000,
        record.RemoteCommandAcked(
          backend_kind: "github",
          event_id: "event-1",
          task_remote_id: "octo/repo#1",
        ),
      ),
      record.with_id(
        "outbox-acked",
        1001,
        record.OutboxPendingV2WithTask(
          outbox_id: "event-1-acked",
          task_ref: acked_task_ref,
          outbox_kind: "remote_command_ack",
          dedupe_key: "remote_command_ack:github:octo/repo#1:event-1",
          payload_json: remote_ack_payload("github", "event-1", "octo/repo#1"),
        ),
      ),
      record.with_id(
        "outbox-other",
        1002,
        record.OutboxPendingV2WithTask(
          outbox_id: "event-1-other",
          task_ref: other_task_ref,
          outbox_kind: "remote_command_ack",
          dedupe_key: "remote_command_ack:github:octo/repo#2:event-1",
          payload_json: remote_ack_payload("github", "event-1", "octo/repo#2"),
        ),
      ),
    ])

  let assert Ok(plan) = recovery.plan(projection, config(), [], 7000)

  assert plan.outbox_to_replay == []
  let assert [
    record.LedgerRecord(
      body: record.OutboxFailedWithTask(
        outbox_id: "event-1-other",
        task_ref: failed_task_ref,
        error_code: "unsupported_outbox_kind:remote_command_ack",
        ..,
      ),
      ..,
    ),
  ] = plan.records_to_append
  assert failed_task_ref == other_task_ref
}

pub fn failed_task_ref_outbox_recovery_records_task_ref_test() {
  let task_ref = task_ref("github", "octo/repo#42")
  let projection =
    projection.fold([
      record.with_id(
        "outbox-invalid-task",
        1000,
        record.OutboxPendingV2WithTask(
          outbox_id: "outbox-invalid-task",
          task_ref: task_ref,
          outbox_kind: "remote_command_ack",
          dedupe_key: "remote_command_ack:github:event-42",
          payload_json: "not-json",
        ),
      ),
    ])

  let assert Ok(plan) = recovery.plan(projection, config(), [], 7000)

  let assert [
    record.LedgerRecord(
      body: record.OutboxFailedWithTask(
        outbox_id: "outbox-invalid-task",
        task_ref: failed_task_ref,
        error_code: "invalid_outbox_payload",
        ..,
      ),
      ..,
    ),
  ] = plan.records_to_append
  assert failed_task_ref == task_ref
}

pub fn acked_remote_command_suppresses_legacy_linear_ack_outbox_replay_test() {
  let projection =
    projection.fold([
      record.with_id(
        "outbox-ack",
        1000,
        record.OutboxPendingV2(
          outbox_id: "comment-1",
          issue_id: "issue-1",
          outbox_kind: "linear_command_ack",
          dedupe_key: "linear_command_ack:comment-1",
          payload_json: "{\"type\":\"linear_command_ack\",\"source_comment_id\":\"comment-1\",\"body\":\"ack\"}",
        ),
      ),
      record.with_id(
        "remote-acked",
        1001,
        record.RemoteCommandAcked(
          backend_kind: "linear",
          event_id: "comment-1",
          task_remote_id: "issue-1",
        ),
      ),
    ])

  let assert Ok(plan) = recovery.plan(projection, config(), [], 7000)

  assert plan.outbox_to_replay == []
  assert plan.records_to_append == []
}

pub fn mixed_issue_and_task_workflow_records_recover_one_task_ref_test() {
  let old_started =
    decode_ledger_record(legacy_ledger_fixtures.workflow_run_started_v2(
      "old-1",
      1,
    ))
  let new_finished =
    decode_ledger_record(
      legacy_ledger_fixtures.workflow_run_finished_with_task_v2("new-2", 4),
    )
  let folded = projection.fold([old_started, new_finished])

  let assert Ok(task_ref) = projection.workflow_task_ref(folded, "run-1")
  assert task_ref
    == record.TaskRefFields(
      task_backend_kind: "linear",
      task_remote_id: "issue-1",
      task_key: Some("LIV-266"),
      task_url: Some("https://linear.app/living-systems/issue/LIV-266"),
    )
}

pub fn old_workflow_and_linear_command_ledger_records_remain_recoverable_test() {
  let workflow_started =
    decode_ledger_record(legacy_ledger_fixtures.workflow_run_started_v2(
      "old-1",
      1,
    ))
  let workflow_finished =
    decode_ledger_record(legacy_ledger_fixtures.workflow_run_finished_v2(
      "old-2",
      3,
    ))
  let step_session_recorded =
    decode_ledger_record(
      legacy_ledger_fixtures.step_attempt_pi_session_recorded_v2("old-step", 4),
    )
  let command_seen =
    decode_ledger_record(legacy_ledger_fixtures.linear_command_seen_v2(
      "cmd-old-1",
      5,
    ))
  let command_started =
    decode_ledger_record(legacy_ledger_fixtures.linear_command_started_v2(
      "cmd-old-2",
      6,
    ))
  let command_completed =
    decode_ledger_record(legacy_ledger_fixtures.linear_command_completed_v2(
      "cmd-old-3",
      7,
      "ok",
      "Retry queued",
    ))
  let command_acked =
    decode_ledger_record(legacy_ledger_fixtures.linear_command_acked_v2(
      "cmd-old-4",
      8,
    ))

  assert workflow_started.body
    == record.WorkflowRunStarted(
      run_id: "run-1",
      workflow_id: "execplan",
      workflow_fingerprint: "wf-old",
      issue_id: "issue-1",
      issue_identifier: "LIV-266",
      issue_fingerprint: "fp-old",
      observed_updated_at_ms: 10,
      run_root: "test/tmp/run-root",
    )
  assert workflow_finished.body
    == record.WorkflowRunFinished(
      run_id: "run-1",
      workflow_id: "execplan",
      issue_id: "issue-1",
      outcome: "success",
      token_total: 10,
      turns: 2,
    )
  assert step_session_recorded.body
    == record.StepAttemptPiSessionRecorded(
      run_id: "run-1",
      issue_id: "issue-1",
      issue_identifier: "LIV-266",
      workflow_id: "execplan",
      workflow_fingerprint: "wf-old",
      step_id: "step-1",
      workspace_name: "main",
      attempt_index: 1,
      workspace_path: "test/tmp/run-root/workspaces/main",
      session_id: "pi-session-1",
      session_file: "state/sessions/run-1/step-1.json",
    )
  assert command_seen.body
    == record.LinearCommandSeen(
      comment_id: "comment-1",
      issue_id: "issue-1",
      author_id: "user-1",
      command_name: "retry",
      excerpt: "/scherzo retry",
    )
  assert command_started.body
    == record.LinearCommandStarted(
      comment_id: "comment-1",
      issue_id: "issue-1",
      command_name: "retry",
    )
  assert command_completed.body
    == record.LinearCommandCompleted(
      comment_id: "comment-1",
      issue_id: "issue-1",
      status: "ok",
      message_excerpt: "Retry queued",
    )
  assert command_acked.body
    == record.LinearCommandAcked(comment_id: "comment-1", issue_id: "issue-1")

  let folded =
    projection.fold([
      workflow_started,
      workflow_finished,
      step_session_recorded,
      command_seen,
      command_started,
      command_completed,
      command_acked,
    ])
  let assert Ok(workflow_status) = dict.get(folded.workflow_runs, "run-1")
  assert workflow_status
    == projection.WorkflowRunFinished(
      workflow_id: "execplan",
      issue_id: "issue-1",
      outcome: "success",
      token_total: 10,
      turns: 2,
      finished_at_ms: 3,
      run_root: "test/tmp/run-root",
    )
  let assert Ok(command_receipt) =
    dict.get(folded.command_receipts, "comment-1")
  assert command_receipt
    == projection.CommandReceiptCompleted(
      issue_id: "issue-1",
      author_id: "user-1",
      command_name: "retry",
      excerpt: "/scherzo retry",
      result_status: "ok",
      message_excerpt: "Retry queued",
      seen_at_ms: 5,
      started_at_ms: 6,
      completed_at_ms: 7,
      acked_at_ms: Some(8),
    )
}

fn decode_ledger_record(line: String) -> record.LedgerRecord {
  let assert Ok(decoded) = record.decode_string(line)
  decoded
}

fn task_ref(
  backend_kind: String,
  task_remote_id: String,
) -> record.TaskRefFields {
  record.TaskRefFields(
    task_backend_kind: backend_kind,
    task_remote_id: task_remote_id,
    task_key: None,
    task_url: None,
  )
}

fn remote_ack_payload(
  backend_kind: String,
  event_id: String,
  task_remote_id: String,
) -> String {
  outbox.remote_command_ack_payload(
    backend_kind,
    event_id,
    task_remote_id,
    "ack",
    [],
  )
}

fn config() -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    tracker: config_types.TrackerConfig(
      kind: tracker_kind.LinearTracker,
      endpoint: "endpoint",
      api_key: Some("key"),
      project_slug: Some("PROJ"),
      active_states: issue_state.list_from_strings(["Todo", "In Progress"]),
      dispatch_states: issue_state.list_from_strings(["Todo"]),
      terminal_states: issue_state.list_from_strings(["Done", "Closed"]),
    ),
    polling: config_types.PollingConfig(interval_ms: 30_000),
    workspace: config_types.WorkspaceConfig(root: "test/tmp/workspaces"),
    hooks: config_types.HooksConfig(
      after_create: Some("true"),
      before_run: None,
      after_run: None,
      before_remove: None,
      timeout_ms: 1000,
    ),
    agent: config_types.AgentConfig(
      max_concurrent_agents: 2,
      max_turns: 20,
      max_retry_backoff_ms: 40_000,
      max_retry_attempts: 3,
      max_sessions_per_issue: 2,
      context_recovery_max_attempts: 1,
      context_recovery_prompt_char_limit: 40_000,
      max_concurrent_agents_by_state: dict.new(),
    ),
    pi: config_types.PiConfig(
      command: "fake",
      turn_timeout_ms: 1000,
      read_timeout_ms: 1000,
      stall_timeout_ms: 1000,
      auto_retry: True,
      ui_request_policy: config_types.Cancel,
      ui_request_timeout_ms: 300_000,
      compatibility_probe: True,
      rate_limit_payload: None,
      argv_command: None,
      session_persistence: config_types.PiSessionPersistenceConfig(
        enabled: False,
        recovery_prompt: "",
      ),
    ),
    handoff: config_types.HandoffConfig(
      enabled: False,
      comment_on_claim: False,
      comment_on_success: False,
      comment_on_failure: False,
      comment_on_park: False,
      claim_state_id: None,
      success_state_id: None,
      failure_state_id: None,
      include_result_on_success: False,
      attach_result_on_success: False,
      attachment_fallback_to_markdown_link: True,
      result_max_chars: 8000,
      completion_states: None,
    ),
    linear_contract: config_types.LinearContractConfig(
      enabled: False,
      workflow_label_prefix: "workflow:",
      workflow_labels: [],
      support_labels: [],
      required_states: dict.new(),
      handoff_state_bindings: dict.new(),
      enforce_issue_workflow_labels: False,
      invalid_workflow_state_id: None,
      invalid_workflow_state_target: None,
      comment_on_invalid_workflow: False,
    ),
    linear_commands: config_types.LinearCommandConfig(
      enabled: False,
      prefix: "/scherzo",
      authorized_user_ids: [],
      poll_limit_per_issue: 25,
      max_comments_per_tick: 50,
      acknowledge_success: True,
      acknowledge_rejection: True,
    ),
    ui_server: config_types.UiServerConfig(
      enabled: False,
      endpoint: None,
      credential_ref: None,
      daemon_label: None,
      command_bridge_enabled: False,
      heartbeat_interval_ms: 5000,
      state_interval_ms: 5000,
      retry_initial_ms: 500,
      retry_max_ms: 30_000,
    ),
  )
}

fn config_with_failure_state(
  state_name: String,
) -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    ..config(),
    handoff: config_types.HandoffConfig(
      ..config().handoff,
      completion_states: Some(workflow_completion_policy.CompletionStatePolicy(
        default_completion_state: workflow_completion_policy.StateByName(
          "In Review",
        ),
        no_review_completion_state: Some(workflow_completion_policy.StateByName(
          "Done",
        )),
        failure_state: workflow_completion_policy.StateByName(state_name),
        partial_success_state: workflow_completion_policy.StateByName(
          state_name,
        ),
        cancellation_state: None,
        workflows: dict.new(),
      )),
    ),
  )
}

fn issue(id: String, identifier: String, state: String) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: id,
    identifier: identifier,
    title: "Title " <> identifier,
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked(state),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

fn has_record_kind(records: List(record.LedgerRecord), kind: String) -> Bool {
  list.any(records, fn(ledger_record) {
    record.kind(ledger_record.body) == kind
  })
}

fn unwrap_counter(
  result: Result(orchestrator_state.IssueCounter, a),
) -> orchestrator_state.IssueCounter {
  let assert Ok(counter) = result
  counter
}

fn counter_failure_attempts(
  runtime: orchestrator_state.RuntimeState,
  issue_id: String,
) -> Int {
  runtime.issue_counters
  |> dict.get(orchestrator_state.linear_issue_id_identity(issue_id))
  |> unwrap_counter
  |> fn(counter) { counter.failure_attempts }
}

fn has_retry(
  runtime: orchestrator_state.RuntimeState,
  issue_id: String,
) -> Bool {
  dict.has_key(
    runtime.retry_attempts,
    orchestrator_state.linear_issue_id_identity(issue_id),
  )
}

fn has_outbox_failed(
  records: List(record.LedgerRecord),
  outbox_id: String,
  error_code: String,
) -> Bool {
  list.any(records, fn(ledger_record) {
    case ledger_record.body {
      record.OutboxFailed(
        outbox_id: failed_outbox_id,
        error_code: failed_error_code,
        ..,
      ) -> failed_outbox_id == outbox_id && failed_error_code == error_code
      record.OutboxFailedWithTask(
        outbox_id: failed_outbox_id,
        error_code: failed_error_code,
        ..,
      ) -> failed_outbox_id == outbox_id && failed_error_code == error_code
      _ -> False
    }
  })
}

fn has_retry_cancelled(
  records: List(record.LedgerRecord),
  issue_id: String,
  generation: Int,
  reason: String,
) -> Bool {
  list.any(records, fn(ledger_record) {
    case ledger_record.body {
      record.RetryCancelled(
        issue_id: cancelled_issue_id,
        generation: cancelled_generation,
        reason: cancelled_reason,
      ) ->
        cancelled_issue_id == issue_id
        && cancelled_generation == generation
        && cancelled_reason == reason
      _ -> False
    }
  })
}
