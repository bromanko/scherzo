import birl
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import scherzo/domain
import scherzo/orchestrator/core
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state

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
    |> dict.get("issue-1")
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
    domain.Issue(..issue("issue-1", "ABC-1", "Todo"), title: "changed")

  let assert Ok(plan) = recovery.plan(projection, config(), [refreshed], 7000)

  assert dict.has_key(plan.runtime.parked, "issue-1")
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

  assert dict.has_key(plan.runtime.parked, "issue-1")
}

pub fn auto_parked_issue_with_new_fingerprint_unparks_test() {
  let original = issue("issue-1", "ABC-1", "Todo")
  let refreshed = domain.Issue(..original, title: "changed")
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

  assert !dict.has_key(plan.runtime.parked, "issue-1")
  assert !dict.has_key(plan.runtime.retry_attempts, "issue-1")
  assert !dict.has_key(plan.runtime.issue_counters, "issue-1")
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
  let assert Ok(retry) = dict.get(plan.runtime.retry_attempts, "issue-1")
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
  let assert Ok(retry) = dict.get(plan.runtime.retry_attempts, "issue-1")
  assert retry.delay_ms == 3000
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

pub fn linear_command_ack_outbox_is_replayed_test() {
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

  let assert [
    recovery.OutboxReplay(
      outbox_id: "outbox-ack",
      issue_id: "issue-1",
      outbox_kind: "linear_command_ack",
      dedupe_key: "ack",
      payload_json: "{\"type\":\"linear_command_ack\",\"body\":\"ack\"}",
    ),
  ] = plan.outbox_to_replay
  assert plan.records_to_append == []
}

fn config() -> domain.EffectiveConfig {
  domain.EffectiveConfig(
    tracker: domain.TrackerConfig(
      kind: tracker_kind.LinearTracker,
      endpoint: "endpoint",
      api_key: Some("key"),
      project_slug: Some("PROJ"),
      active_states: issue_state.list_from_strings(["Todo", "In Progress"]),
      terminal_states: issue_state.list_from_strings(["Done", "Closed"]),
    ),
    polling: domain.PollingConfig(interval_ms: 30_000),
    workspace: domain.WorkspaceConfig(root: "test/tmp/workspaces"),
    hooks: domain.HooksConfig(
      after_create: Some("true"),
      before_run: None,
      after_run: None,
      before_remove: None,
      timeout_ms: 1000,
    ),
    agent: domain.AgentConfig(
      max_concurrent_agents: 2,
      max_turns: 20,
      max_retry_backoff_ms: 40_000,
      max_retry_attempts: 3,
      max_sessions_per_issue: 2,
      max_concurrent_agents_by_state: dict.new(),
    ),
    pi: domain.PiConfig(
      command: "fake",
      turn_timeout_ms: 1000,
      read_timeout_ms: 1000,
      stall_timeout_ms: 1000,
      auto_retry: True,
      ui_request_policy: domain.Cancel,
      ui_request_timeout_ms: 300_000,
      compatibility_probe: True,
      rate_limit_payload: None,
    ),
    handoff: domain.HandoffConfig(
      enabled: False,
      comment_on_claim: False,
      comment_on_success: False,
      comment_on_failure: False,
      claim_state_id: None,
      success_state_id: None,
      failure_state_id: None,
      include_result_on_success: False,
      attach_result_on_success: False,
      attachment_fallback_to_markdown_link: True,
      result_max_chars: 8000,
    ),
    linear_contract: domain.LinearContractConfig(
      enabled: False,
      workflow_label_prefix: "workflow:",
      workflow_labels: [],
      support_labels: [],
      required_states: dict.new(),
      handoff_state_bindings: dict.new(),
      enforce_issue_workflow_labels: False,
      invalid_workflow_state_id: None,
      comment_on_invalid_workflow: False,
    ),
    linear_commands: domain.LinearCommandConfig(
      enabled: False,
      prefix: "/scherzo",
      authorized_user_ids: [],
      poll_limit_per_issue: 25,
      max_comments_per_tick: 50,
      acknowledge_success: True,
      acknowledge_rejection: True,
    ),
  )
}

fn issue(id: String, identifier: String, state: String) -> domain.Issue {
  domain.Issue(
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
  result: Result(domain.IssueCounter, a),
) -> domain.IssueCounter {
  let assert Ok(counter) = result
  counter
}

fn counter_failure_attempts(
  runtime: domain.RuntimeState,
  issue_id: String,
) -> Int {
  runtime.issue_counters
  |> dict.get(issue_id)
  |> unwrap_counter
  |> fn(counter) { counter.failure_attempts }
}

fn has_retry(runtime: domain.RuntimeState, issue_id: String) -> Bool {
  dict.has_key(runtime.retry_attempts, issue_id)
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
      _ -> False
    }
  })
}
