import gleam/option.{type Option, None}
import scherzo/state/record

pub opaque type LedgerBatch {
  LedgerBatch(List(record.RecordBody))
}

pub fn to_bodies(batch: LedgerBatch) -> List(record.RecordBody) {
  let LedgerBatch(bodies) = batch
  bodies
}

pub fn empty() -> LedgerBatch {
  LedgerBatch([])
}

pub fn claim_started(
  workflow_started: record.RecordBody,
  issue_id: String,
  issue_identifier: String,
  workspace_path: String,
  run_id: String,
  failure_attempts: Int,
  worker_sessions: Int,
  observed_updated_at_ms: Int,
) -> LedgerBatch {
  LedgerBatch([
    workflow_started,
    record.KnownWorkspace(issue_id, issue_identifier, workspace_path),
    record.RunStarted(run_id, issue_id, issue_identifier, workspace_path),
    record.IssueCounterUpdated(
      issue_id,
      issue_identifier,
      failure_attempts,
      worker_sessions,
      observed_updated_at_ms,
      None,
    ),
  ])
}

pub fn retry_scheduled(
  issue_id: String,
  issue_identifier: String,
  delay_ms: Int,
  generation: Int,
  reason: String,
) -> LedgerBatch {
  LedgerBatch([
    record.RetryScheduled(
      issue_id,
      issue_identifier,
      delay_ms,
      generation,
      reason,
    ),
  ])
}

pub fn retry_cancelled(
  issue_id: String,
  generation: Int,
  reason: String,
) -> LedgerBatch {
  LedgerBatch([record.RetryCancelled(issue_id, generation, reason)])
}

pub fn worker_succeeded(
  run_id: String,
  issue_id: String,
  classification: String,
  token_total: Int,
  turns: Int,
  counter_record: record.RecordBody,
) -> LedgerBatch {
  LedgerBatch([
    record.RunFinished(run_id, issue_id, classification, token_total, turns),
    counter_record,
  ])
}

pub fn worker_failed(
  run_id: String,
  issue_id: String,
  token_total: Int,
  turns: Int,
  counter_record: record.RecordBody,
) -> LedgerBatch {
  LedgerBatch([
    record.RunFinished(run_id, issue_id, "failure", token_total, turns),
    counter_record,
  ])
}

pub fn workflow_cancelled(
  run_id: String,
  workflow_id: String,
  issue_id: String,
  token_total: Int,
) -> LedgerBatch {
  LedgerBatch([
    record.WorkflowRunFinished(
      run_id,
      workflow_id,
      issue_id,
      "cancelled",
      token_total,
      0,
    ),
  ])
}

pub fn operator_retry_counter_reset(
  issue_id: String,
  issue_identifier: String,
  observed_updated_at_ms: Int,
) -> LedgerBatch {
  LedgerBatch([
    record.IssueCounterUpdated(
      issue_id,
      issue_identifier,
      0,
      0,
      observed_updated_at_ms,
      None,
    ),
  ])
}

pub fn issue_parked(
  issue_id: String,
  issue_identifier: String,
  reason: String,
  release_policy: String,
  issue_fingerprint: String,
  observed_updated_at_ms: Int,
) -> LedgerBatch {
  LedgerBatch([
    record.IssueParkedV2(
      issue_id,
      issue_identifier,
      reason,
      release_policy,
      issue_fingerprint,
      observed_updated_at_ms,
    ),
  ])
}

pub fn issue_unparked(
  issue_id: String,
  issue_identifier: String,
  reason: String,
  observed_updated_at_ms: Int,
) -> LedgerBatch {
  LedgerBatch([
    record.IssueUnparked(issue_id, issue_identifier, reason),
    record.IssueCounterUpdated(
      issue_id,
      issue_identifier,
      0,
      0,
      observed_updated_at_ms,
      None,
    ),
  ])
}

pub fn step_attempt_prepared(
  run_id: String,
  workflow_id: String,
  step_id: String,
  attempt_index: Int,
  workspace_name: String,
  workspace_path: String,
  run_root: String,
  source_workspace_name: Option(String),
  source_workspace_path: Option(String),
) -> LedgerBatch {
  LedgerBatch([
    record.StepAttemptPrepared(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      workspace_name,
      workspace_path,
      run_root,
      source_workspace_name,
      source_workspace_path,
    ),
  ])
}

pub fn step_attempt_started(
  run_id: String,
  workflow_id: String,
  step_id: String,
  attempt_index: Int,
  operator_session_id: String,
  external_session_ref: Option(String),
  continuation_capable: Bool,
) -> LedgerBatch {
  LedgerBatch([
    record.StepAttemptStarted(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      operator_session_id,
      external_session_ref,
      continuation_capable,
    ),
  ])
}

pub fn step_attempt_continuation_started(
  run_id: String,
  workflow_id: String,
  step_id: String,
  attempt_index: Int,
  session_id: String,
) -> LedgerBatch {
  LedgerBatch([
    record.StepAttemptContinuationStarted(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      session_id,
    ),
  ])
}

pub fn step_attempt_pi_session_recorded_with_task(
  run_id: String,
  issue_id: String,
  issue_identifier: String,
  task_ref: record.TaskRefFields,
  workflow_id: String,
  workflow_fingerprint: String,
  step_id: String,
  workspace_name: String,
  attempt_index: Int,
  workspace_path: String,
  session_id: String,
  session_file: String,
) -> LedgerBatch {
  LedgerBatch([
    record.StepAttemptPiSessionRecordedWithTask(
      run_id,
      issue_id,
      issue_identifier,
      task_ref,
      workflow_id,
      workflow_fingerprint,
      step_id,
      workspace_name,
      attempt_index,
      workspace_path,
      session_id,
      session_file,
    ),
  ])
}

pub fn step_attempt_finished(
  run_id: String,
  workflow_id: String,
  step_id: String,
  attempt_index: Int,
  outcome: String,
  artifact_ref: String,
  artifact_sha256: String,
  workspace_name: String,
  workspace_path: String,
  token_total: Int,
  turns: Int,
) -> LedgerBatch {
  LedgerBatch([
    record.StepAttemptFinished(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      outcome,
      artifact_ref,
      artifact_sha256,
      workspace_name,
      workspace_path,
      token_total,
      turns,
    ),
  ])
}

pub fn workflow_step_recovery_started(
  run_id: String,
  workflow_id: String,
  step_id: String,
  failed_attempt_index: Int,
  recovery_attempt_number: Int,
  recovery_session_id: String,
  model: Option(String),
  prompt_ref: String,
) -> LedgerBatch {
  LedgerBatch([
    record.WorkflowStepRecoveryStarted(
      run_id,
      workflow_id,
      step_id,
      failed_attempt_index,
      recovery_attempt_number,
      recovery_session_id,
      model,
      prompt_ref,
    ),
  ])
}

pub fn workflow_step_recovery_finished(
  run_id: String,
  workflow_id: String,
  step_id: String,
  failed_attempt_index: Int,
  recovery_attempt_number: Int,
  recovery_session_id: String,
  result: String,
  summary: String,
  reason: String,
  retry_attempt_index: Option(Int),
) -> LedgerBatch {
  LedgerBatch([
    record.WorkflowStepRecoveryFinished(
      run_id,
      workflow_id,
      step_id,
      failed_attempt_index,
      recovery_attempt_number,
      recovery_session_id,
      result,
      summary,
      reason,
      retry_attempt_index,
    ),
  ])
}

pub fn step_attempt_interrupted(
  run_id: String,
  workflow_id: String,
  step_id: String,
  attempt_index: Int,
  reason: String,
) -> LedgerBatch {
  LedgerBatch([
    record.StepAttemptInterrupted(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      reason,
    ),
  ])
}
