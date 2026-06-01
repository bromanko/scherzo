import gleam/option.{None, Some}
import scherzo/state/ledger_batch
import scherzo/state/record
import scherzo/task

pub fn claim_started_batch_emits_expected_records_in_order_test() {
  let workflow_started =
    record.WorkflowRunStartedWithTask(
      "run-1",
      "default",
      "workflow-fingerprint",
      "issue-1",
      "ABC-1",
      record.linear_task_ref_fields("issue-1", Some("ABC-1"), None),
      "issue-fingerprint",
      123,
      "test/tmp/workspaces/ABC-1",
    )

  assert ledger_batch.to_bodies(ledger_batch.claim_started(
      workflow_started,
      "issue-1",
      "ABC-1",
      "test/tmp/workspaces/ABC-1",
      2,
      3,
      456,
    ))
    == [
      workflow_started,
      record.KnownWorkspace("issue-1", "ABC-1", "test/tmp/workspaces/ABC-1"),
      record.IssueCounterUpdated("issue-1", "ABC-1", 2, 3, 456, None),
    ]
}

pub fn worker_terminal_batches_do_not_duplicate_workflow_outcomes_test() {
  let task_ref = task.TaskRef("linear", "issue-1", Some("ABC-1"), None)
  let durable_task_ref =
    record.linear_task_ref_fields("issue-1", Some("ABC-1"), None)
  let counter =
    record.IssueCounterUpdated("issue-1", "ABC-1", 0, 1, 789, Some("run-1"))

  assert ledger_batch.to_bodies(ledger_batch.worker_succeeded(counter))
    == [counter]

  assert ledger_batch.to_bodies(ledger_batch.worker_failed(counter))
    == [counter]

  assert ledger_batch.to_bodies(ledger_batch.workflow_cancelled(
      #("run-3", "default", task_ref),
      9,
    ))
    == [
      record.WorkflowRunFinishedWithTask(
        "run-3",
        "default",
        "issue-1",
        durable_task_ref,
        "cancelled",
        9,
        0,
      ),
    ]
}

pub fn workflow_checkpoint_step_batches_preserve_record_shapes_test() {
  assert ledger_batch.to_bodies(ledger_batch.step_attempt_prepared(
      "run-1",
      "default",
      "step-1",
      1,
      "workspace-1",
      "test/tmp/workspaces/ABC-1",
      "test/tmp/workspaces",
      Some("source"),
      Some("test/tmp/source"),
    ))
    == [
      record.StepAttemptPrepared(
        "run-1",
        "default",
        "step-1",
        1,
        "workspace-1",
        "test/tmp/workspaces/ABC-1",
        "test/tmp/workspaces",
        Some("source"),
        Some("test/tmp/source"),
      ),
    ]

  assert ledger_batch.to_bodies(ledger_batch.step_attempt_started(
      "run-1",
      "default",
      "step-1",
      1,
      "session-1",
      Some("external"),
      True,
    ))
    == [
      record.StepAttemptStarted(
        "run-1",
        "default",
        "step-1",
        1,
        "session-1",
        Some("external"),
        True,
      ),
    ]

  assert ledger_batch.to_bodies(
      ledger_batch.step_attempt_pi_session_recorded_with_task(
        "run-1",
        "issue-1",
        "ABC-1",
        "default",
        "workflow-fingerprint",
        "step-1",
        "workspace-1",
        1,
        "test/tmp/workspaces/ABC-1",
        "session-1",
        "session.json",
      ),
    )
    == [
      record.StepAttemptPiSessionRecordedWithTask(
        "run-1",
        "issue-1",
        "ABC-1",
        record.linear_task_ref_fields("issue-1", Some("ABC-1"), None),
        "default",
        "workflow-fingerprint",
        "step-1",
        "workspace-1",
        1,
        "test/tmp/workspaces/ABC-1",
        "session-1",
        "session.json",
      ),
    ]

  assert ledger_batch.to_bodies(ledger_batch.workflow_step_recovery_started(
      "run-1",
      "default",
      "step-1",
      1,
      2,
      "session-1",
      Some("model"),
      "prompt-ref",
    ))
    == [
      record.WorkflowStepRecoveryStarted(
        "run-1",
        "default",
        "step-1",
        1,
        2,
        "session-1",
        Some("model"),
        "prompt-ref",
      ),
    ]

  assert ledger_batch.to_bodies(ledger_batch.workflow_step_recovery_finished(
      "run-1",
      "default",
      "step-1",
      1,
      2,
      "session-1",
      "recovered",
      "summary",
      "reason",
      Some(3),
    ))
    == [
      record.WorkflowStepRecoveryFinished(
        "run-1",
        "default",
        "step-1",
        1,
        2,
        "session-1",
        "recovered",
        "summary",
        "reason",
        Some(3),
      ),
    ]

  assert ledger_batch.to_bodies(ledger_batch.step_attempt_interrupted(
      "run-1",
      "default",
      "step-1",
      1,
      "daemon_shutdown",
    ))
    == [
      record.StepAttemptInterrupted(
        "run-1",
        "default",
        "step-1",
        1,
        "daemon_shutdown",
      ),
    ]
}
