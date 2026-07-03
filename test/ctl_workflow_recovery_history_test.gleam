import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/ctl/workflow_recovery_history
import scherzo/session/event
import scherzo/session/tokens as session_tokens
import scherzo/state/artifact_store
import scherzo/state/projection
import scherzo/state/record
import scherzo/workflow_outcome

pub fn original_step_session_renders_recheck_history_test() {
  let transcript = render(sample_success_records(), "session-1")

  assert string.contains(transcript, "workflow_step_recovery_history:")
  assert string.contains(
    transcript,
    "failed_attempt_artifact_ref: runs/run-1/implement",
  )
  assert string.contains(transcript, "/attempt-1.json")
  assert string.contains(
    transcript,
    "recovery_result_artifact_ref: "
      <> artifact_store.recovery_artifact_ref(
      "run-1",
      "implement",
      1,
      1,
      "workflow_step_recovery_result",
    ),
  )
  assert string.contains(transcript, "decision: recheck")
  assert string.contains(transcript, "recheck_attempt_index: 2")
  assert string.contains(
    transcript,
    "recheck_attempt_artifact_ref: runs/run-1/implement",
  )
  assert string.contains(transcript, "/attempt-2.json")
  assert string.contains(transcript, "recheck_result: succeeded")
  assert string.contains(
    transcript,
    "final_workflow_outcome: succeeded_after_recovery",
  )
  assert string.contains(transcript, "summary: Fixed tests ⏎ with details")
  assert string.contains(transcript, "reason: ")
  assert string.contains(transcript, "protected_checkpoint_restored")
  assert string.contains(transcript, "…")
}

pub fn continuation_session_resolves_via_recheck_attempt_index_test() {
  let transcript = render(sample_success_records(), "continue-2")

  assert string.contains(transcript, "decision: recheck")
  assert string.contains(transcript, "recheck_attempt_index: 2")
  assert string.contains(transcript, "recheck_result: succeeded")
}

pub fn original_step_session_renders_multi_attempt_recovery_sequence_test() {
  let rendered =
    render_lines(sample_multi_attempt_success_records(), "session-1")

  assert rendered == expected_multi_attempt_success_lines()
}

pub fn continuation_session_renders_multi_attempt_recovery_sequence_test() {
  let rendered =
    render_lines(sample_multi_attempt_success_records(), "continue-3")

  assert rendered == expected_multi_attempt_success_lines()
}

pub fn control_characters_in_summary_and_reason_are_escaped_test() {
  let transcript = render(sample_control_character_records(), "session-1")

  assert string.contains(transcript, "summary: Fixed␛[31m ⏎ next␍tail")
  assert string.contains(transcript, "reason: osc ␛]0;bad␇ c1 \\u{9B}31m")
  assert !string.contains(transcript, "\u{1b}")
  assert !string.contains(transcript, "\u{7}")
  assert !string.contains(transcript, "\u{9b}")
  assert !string.contains(transcript, "\r")
}

pub fn recovery_session_renders_multiple_duplicate_and_incomplete_entries_test() {
  let records = sample_multiple_recovery_records()
  let history = history(records, "recover-2")
  let first_render =
    workflow_recovery_history.render(history) |> string.join(with: "\n")
  let second_render =
    workflow_recovery_history.render(history) |> string.join(with: "\n")

  assert first_render == second_render
  assert count_occurrences(first_render, "recovery_session_id: recover-2") == 2
  assert string.contains(first_render, "status: incomplete")
  assert string.contains(
    first_render,
    "failed_attempt_artifact_ref: runs/run-1/implement",
  )
  assert string.contains(first_render, "/attempt-1.json")
  assert !string.contains(first_render, "recovery_result_artifact_ref:")
}

pub fn gave_up_history_renders_failed_after_recovery_outcome_test() {
  let transcript = render(sample_gave_up_records(), "session-gave-up")

  assert string.contains(
    transcript,
    "failed_attempt_artifact_ref: runs/run-gave-up/implement",
  )
  assert string.contains(transcript, "/attempt-1.json")
  assert string.contains(
    transcript,
    "recovery_result_artifact_ref: "
      <> artifact_store.recovery_artifact_ref(
      "run-gave-up",
      "implement",
      1,
      1,
      "workflow_step_recovery_result",
    ),
  )
  assert string.contains(transcript, "decision: gave_up")
  assert string.contains(
    transcript,
    "final_workflow_outcome: failed_after_recovery",
  )
  assert !string.contains(transcript, "recheck_attempt_index:")
  assert !string.contains(transcript, "recheck_attempt_artifact_ref:")
  assert !string.contains(transcript, "recheck_result:")
}

pub fn failed_recovery_history_omits_result_and_recheck_artifact_refs_test() {
  let transcript =
    render(sample_artifact_write_failed_records(), "session-artifact-failed")

  assert string.contains(
    transcript,
    "failed_attempt_artifact_ref: "
      <> artifact_store.artifact_ref("run-artifact-failed", "implement", 1),
  )
  assert string.contains(transcript, "status: finished")
  assert string.contains(transcript, "decision: artifact_write_failed")
  assert !string.contains(transcript, "recovery_result_artifact_ref:")
  assert !string.contains(transcript, "recheck_attempt_index:")
  assert !string.contains(transcript, "recheck_attempt_artifact_ref:")
}

pub fn unrelated_session_renders_empty_history_marker_test() {
  let rendered =
    history(sample_success_records(), "session-unrelated")
    |> workflow_recovery_history.render

  assert rendered == ["workflow_step_recovery_history: -"]
}

fn render(records: List(record.LedgerRecord), session_id: String) -> String {
  render_lines(records, session_id)
  |> string.join(with: "\n")
}

fn render_lines(
  records: List(record.LedgerRecord),
  session_id: String,
) -> List(String) {
  history(records, session_id)
  |> workflow_recovery_history.render
}

fn history(
  records: List(record.LedgerRecord),
  session_id: String,
) -> workflow_recovery_history.History {
  workflow_recovery_history.from_replay(
    session_summary(session_id),
    records,
    projection.fold(records),
  )
}

fn session_summary(session_id: String) -> event.SessionSummary {
  event.SessionSummary(
    session_id: session_id,
    display_name: session_id,
    issue_id: "issue-1",
    issue_identifier: "LIV-490",
    issue_title: "Operator history",
    workspace_path: "test/tmp/recovery-history",
    pi_session_id: None,
    status: event.Running,
    recovery: None,
    current_turn: 1,
    current_turn_status: None,
    current_turn_started_at_ms: None,
    last_turn_finished_at_ms: None,
    last_turn_duration_ms: None,
    last_turn_token_delta: session_tokens.zero_token_totals(),
    last_turn_reason: None,
    started_at_ms: 1000,
    last_event_at_ms: 2000,
    token_totals: session_tokens.zero_token_totals(),
  )
}

fn expected_multi_attempt_success_lines() -> List(String) {
  [
    "workflow_step_recovery_history:",
    "  - run_id: run-multi",
    "    workflow_id: implementation",
    "    step_id: verify",
    "    failed_attempt_index: 1",
    "    recovery_attempt_number: 1",
    "    recovery_session_id: recover-1",
    "    status: finished",
    "    failed_attempt_artifact_ref: "
      <> artifact_store.artifact_ref("run-multi", "verify", 1),
    "    recovery_result_artifact_ref: "
      <> artifact_store.recovery_artifact_ref(
      "run-multi",
      "verify",
      1,
      1,
      "workflow_step_recovery_result",
    ),
    "    decision: recheck",
    "    summary: Patched first failure",
    "    reason: Ready for first recheck",
    "    recheck_attempt_index: 2",
    "    recheck_attempt_artifact_ref: "
      <> artifact_store.artifact_ref("run-multi", "verify", 2),
    "    recheck_result: failed",
    "    final_workflow_outcome: succeeded_after_recovery",
    "  - run_id: run-multi",
    "    workflow_id: implementation",
    "    step_id: verify",
    "    failed_attempt_index: 2",
    "    recovery_attempt_number: 2",
    "    recovery_session_id: recover-2",
    "    status: finished",
    "    failed_attempt_artifact_ref: "
      <> artifact_store.artifact_ref("run-multi", "verify", 2),
    "    recovery_result_artifact_ref: "
      <> artifact_store.recovery_artifact_ref(
      "run-multi",
      "verify",
      2,
      2,
      "workflow_step_recovery_result",
    ),
    "    decision: recheck",
    "    summary: Patched remaining failure",
    "    reason: Ready for second recheck",
    "    recheck_attempt_index: 3",
    "    recheck_attempt_artifact_ref: "
      <> artifact_store.artifact_ref("run-multi", "verify", 3),
    "    recheck_result: succeeded",
    "    final_workflow_outcome: succeeded_after_recovery",
  ]
}

fn sample_success_records() -> List(record.LedgerRecord) {
  [
    record.with_id(
      "run-started",
      900,
      record.WorkflowRunStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        workflow_fingerprint: "wf-1",
        issue_id: "issue-1",
        issue_identifier: "LIV-490",
        issue_fingerprint: "issue-fp-1",
        observed_updated_at_ms: 800,
        run_root: "test/tmp/recovery-history",
      ),
    ),
    record.with_id(
      "attempt-1-started",
      1000,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "implement",
        attempt_index: 1,
        operator_session_id: "session-1",
        external_session_ref: None,
        continuation_capable: True,
      ),
    ),
    record.with_id(
      "recovery-1-started",
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
    record.with_id(
      "recovery-1-finished",
      1020,
      record.WorkflowStepRecoveryFinished(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "implement",
        failed_attempt_index: 1,
        recovery_attempt_number: 1,
        recovery_session_id: "recover-1",
        result: "recheck",
        summary: "Fixed tests\nwith details",
        reason: "recheck-ready; protected_checkpoint_restored kind=step_attempt_artifact ref=runs/run-1/implement/attempt-1.json expected_sha256=abc observed=mutated",
        retry_attempt_index: Some(2),
      ),
    ),
    record.with_id(
      "attempt-2-started",
      1030,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "implement",
        attempt_index: 2,
        operator_session_id: "session-2",
        external_session_ref: None,
        continuation_capable: True,
      ),
    ),
    record.with_id(
      "attempt-2-continuation",
      1031,
      record.StepAttemptContinuationStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "implement",
        attempt_index: 2,
        session_id: "continue-2",
      ),
    ),
    record.with_id(
      "attempt-2-finished",
      1040,
      record.StepAttemptFinished(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "implement",
        attempt_index: 2,
        outcome: "succeeded",
        artifact_ref: "runs/run-1/implement/attempt-2.json",
        artifact_sha256: "sha-2",
        workspace_name: "main",
        workspace_path: "test/tmp/recovery-history/main",
        token_total: 0,
        turns: 0,
      ),
    ),
    record.with_id(
      "run-finished",
      1050,
      record.WorkflowRunFinished(
        run_id: "run-1",
        workflow_id: "implementation",
        issue_id: "issue-1",
        outcome: workflow_outcome.succeeded_after_recovery,
        token_total: 0,
        turns: 0,
      ),
    ),
  ]
}

fn sample_multi_attempt_success_records() -> List(record.LedgerRecord) {
  [
    record.with_id(
      "run-multi-started",
      900,
      record.WorkflowRunStarted(
        run_id: "run-multi",
        workflow_id: "implementation",
        workflow_fingerprint: "wf-1",
        issue_id: "issue-1",
        issue_identifier: "LIV-1348",
        issue_fingerprint: "issue-fp-1",
        observed_updated_at_ms: 800,
        run_root: "test/tmp/recovery-history",
      ),
    ),
    record.with_id(
      "attempt-multi-1-started",
      1000,
      record.StepAttemptStarted(
        run_id: "run-multi",
        workflow_id: "implementation",
        step_id: "verify",
        attempt_index: 1,
        operator_session_id: "session-1",
        external_session_ref: None,
        continuation_capable: True,
      ),
    ),
    record.with_id(
      "recovery-multi-1-started",
      1010,
      record.WorkflowStepRecoveryStarted(
        run_id: "run-multi",
        workflow_id: "implementation",
        step_id: "verify",
        failed_attempt_index: 1,
        recovery_attempt_number: 1,
        recovery_session_id: "recover-1",
        model: Some("gpt-5"),
        prompt_ref: ".scherzo/workflows/prompts/recover_failed_step.md",
      ),
    ),
    record.with_id(
      "recovery-multi-1-finished",
      1020,
      record.WorkflowStepRecoveryFinished(
        run_id: "run-multi",
        workflow_id: "implementation",
        step_id: "verify",
        failed_attempt_index: 1,
        recovery_attempt_number: 1,
        recovery_session_id: "recover-1",
        result: "recheck",
        summary: "Patched first failure",
        reason: "Ready for first recheck",
        retry_attempt_index: Some(2),
      ),
    ),
    record.with_id(
      "attempt-multi-2-started",
      1030,
      record.StepAttemptStarted(
        run_id: "run-multi",
        workflow_id: "implementation",
        step_id: "verify",
        attempt_index: 2,
        operator_session_id: "session-2",
        external_session_ref: None,
        continuation_capable: True,
      ),
    ),
    record.with_id(
      "attempt-multi-2-finished",
      1040,
      record.StepAttemptFinished(
        run_id: "run-multi",
        workflow_id: "implementation",
        step_id: "verify",
        attempt_index: 2,
        outcome: "failed",
        artifact_ref: "runs/run-multi/verify/attempt-2.json",
        artifact_sha256: "sha-2",
        workspace_name: "main",
        workspace_path: "test/tmp/recovery-history/main",
        token_total: 0,
        turns: 0,
      ),
    ),
    record.with_id(
      "recovery-multi-2-started",
      1050,
      record.WorkflowStepRecoveryStarted(
        run_id: "run-multi",
        workflow_id: "implementation",
        step_id: "verify",
        failed_attempt_index: 2,
        recovery_attempt_number: 2,
        recovery_session_id: "recover-2",
        model: Some("gpt-5"),
        prompt_ref: ".scherzo/workflows/prompts/recover_failed_step.md",
      ),
    ),
    record.with_id(
      "recovery-multi-2-finished",
      1060,
      record.WorkflowStepRecoveryFinished(
        run_id: "run-multi",
        workflow_id: "implementation",
        step_id: "verify",
        failed_attempt_index: 2,
        recovery_attempt_number: 2,
        recovery_session_id: "recover-2",
        result: "recheck",
        summary: "Patched remaining failure",
        reason: "Ready for second recheck",
        retry_attempt_index: Some(3),
      ),
    ),
    record.with_id(
      "attempt-multi-3-started",
      1070,
      record.StepAttemptStarted(
        run_id: "run-multi",
        workflow_id: "implementation",
        step_id: "verify",
        attempt_index: 3,
        operator_session_id: "session-3",
        external_session_ref: None,
        continuation_capable: True,
      ),
    ),
    record.with_id(
      "attempt-multi-3-continuation",
      1071,
      record.StepAttemptContinuationStarted(
        run_id: "run-multi",
        workflow_id: "implementation",
        step_id: "verify",
        attempt_index: 3,
        session_id: "continue-3",
      ),
    ),
    record.with_id(
      "attempt-multi-3-finished",
      1080,
      record.StepAttemptFinished(
        run_id: "run-multi",
        workflow_id: "implementation",
        step_id: "verify",
        attempt_index: 3,
        outcome: "succeeded",
        artifact_ref: "runs/run-multi/verify/attempt-3.json",
        artifact_sha256: "sha-3",
        workspace_name: "main",
        workspace_path: "test/tmp/recovery-history/main",
        token_total: 0,
        turns: 0,
      ),
    ),
    record.with_id(
      "run-multi-finished",
      1090,
      record.WorkflowRunFinished(
        run_id: "run-multi",
        workflow_id: "implementation",
        issue_id: "issue-1",
        outcome: workflow_outcome.succeeded_after_recovery,
        token_total: 0,
        turns: 0,
      ),
    ),
  ]
}

fn sample_control_character_records() -> List(record.LedgerRecord) {
  [
    record.with_id(
      "attempt-control-started",
      1000,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "implement",
        attempt_index: 1,
        operator_session_id: "session-1",
        external_session_ref: None,
        continuation_capable: True,
      ),
    ),
    record.with_id(
      "recovery-control-started",
      1010,
      record.WorkflowStepRecoveryStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "implement",
        failed_attempt_index: 1,
        recovery_attempt_number: 1,
        recovery_session_id: "recover-1",
        model: None,
        prompt_ref: ".scherzo/workflows/prompts/recover_failed_step.md",
      ),
    ),
    record.with_id(
      "recovery-control-finished",
      1020,
      record.WorkflowStepRecoveryFinished(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "implement",
        failed_attempt_index: 1,
        recovery_attempt_number: 1,
        recovery_session_id: "recover-1",
        result: "recheck",
        summary: "Fixed\u{1b}[31m\nnext\rtail",
        reason: "osc \u{1b}]0;bad\u{7} c1 \u{9b}31m",
        retry_attempt_index: None,
      ),
    ),
  ]
}

fn sample_multiple_recovery_records() -> List(record.LedgerRecord) {
  list.append(sample_success_records(), [
    record.with_id(
      "recovery-2-started-a",
      1060,
      record.WorkflowStepRecoveryStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "implement",
        failed_attempt_index: 1,
        recovery_attempt_number: 2,
        recovery_session_id: "recover-2",
        model: None,
        prompt_ref: ".scherzo/workflows/prompts/recover_failed_step.md",
      ),
    ),
    record.with_id(
      "recovery-2-started-b",
      1061,
      record.WorkflowStepRecoveryStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "implement",
        failed_attempt_index: 1,
        recovery_attempt_number: 2,
        recovery_session_id: "recover-2",
        model: None,
        prompt_ref: ".scherzo/workflows/prompts/recover_failed_step.md",
      ),
    ),
  ])
}

fn sample_gave_up_records() -> List(record.LedgerRecord) {
  [
    record.with_id(
      "attempt-gave-up-started",
      1000,
      record.StepAttemptStarted(
        run_id: "run-gave-up",
        workflow_id: "implementation",
        step_id: "implement",
        attempt_index: 1,
        operator_session_id: "session-gave-up",
        external_session_ref: None,
        continuation_capable: False,
      ),
    ),
    record.with_id(
      "recovery-gave-up-started",
      1010,
      record.WorkflowStepRecoveryStarted(
        run_id: "run-gave-up",
        workflow_id: "implementation",
        step_id: "implement",
        failed_attempt_index: 1,
        recovery_attempt_number: 1,
        recovery_session_id: "recover-gave-up",
        model: None,
        prompt_ref: ".scherzo/workflows/prompts/recover_failed_step.md",
      ),
    ),
    record.with_id(
      "recovery-gave-up-finished",
      1020,
      record.WorkflowStepRecoveryFinished(
        run_id: "run-gave-up",
        workflow_id: "implementation",
        step_id: "implement",
        failed_attempt_index: 1,
        recovery_attempt_number: 1,
        recovery_session_id: "recover-gave-up",
        result: "gave_up",
        summary: "Could not repair",
        reason: "Still failing",
        retry_attempt_index: None,
      ),
    ),
    record.with_id(
      "run-gave-up-finished",
      1030,
      record.WorkflowRunFinished(
        run_id: "run-gave-up",
        workflow_id: "implementation",
        issue_id: "issue-1",
        outcome: workflow_outcome.failed_after_recovery,
        token_total: 0,
        turns: 0,
      ),
    ),
  ]
}

fn sample_artifact_write_failed_records() -> List(record.LedgerRecord) {
  [
    record.with_id(
      "attempt-artifact-failed-started",
      1000,
      record.StepAttemptStarted(
        run_id: "run-artifact-failed",
        workflow_id: "implementation",
        step_id: "implement",
        attempt_index: 1,
        operator_session_id: "session-artifact-failed",
        external_session_ref: None,
        continuation_capable: False,
      ),
    ),
    record.with_id(
      "recovery-artifact-failed-started",
      1010,
      record.WorkflowStepRecoveryStarted(
        run_id: "run-artifact-failed",
        workflow_id: "implementation",
        step_id: "implement",
        failed_attempt_index: 1,
        recovery_attempt_number: 1,
        recovery_session_id: "recover-artifact-failed",
        model: None,
        prompt_ref: ".scherzo/workflows/prompts/recover_failed_step.md",
      ),
    ),
    record.with_id(
      "recovery-artifact-failed-finished",
      1020,
      record.WorkflowStepRecoveryFinished(
        run_id: "run-artifact-failed",
        workflow_id: "implementation",
        step_id: "implement",
        failed_attempt_index: 1,
        recovery_attempt_number: 1,
        recovery_session_id: "recover-artifact-failed",
        result: "artifact_write_failed",
        summary: "Recovery artifact write failed",
        reason: "artifact_write_failed: immutable conflict",
        retry_attempt_index: None,
      ),
    ),
  ]
}

fn count_occurrences(haystack: String, needle: String) -> Int {
  let parts = haystack |> string.split(on: needle) |> list.length
  parts - 1
}
