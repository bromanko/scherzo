import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq}
import gleam/result
import gleam/string
import scherzo/orchestrator/state as orchestrator_state
import scherzo/state/record

const max_recent_scheduled_run_ids = 25

pub type Projection {
  Projection(
    runs: Dict(String, RunStatus),
    workflow_runs: Dict(String, WorkflowRunStatus),
    workflow_task_refs: Dict(String, record.TaskRefFields),
    workflow_input_manifests: Dict(String, WorkflowContractManifestRef),
    workflow_output_manifests: Dict(String, WorkflowContractManifestRef),
    workflow_repairs: Dict(String, WorkflowRepairStatus),
    step_attempts: Dict(String, StepAttemptStatus),
    retries: Dict(String, RetryStatus),
    parked_issues: Dict(String, ParkedIssue),
    commands: Dict(String, CommandStatus),
    command_receipts: Dict(String, CommandReceiptState),
    outbox: Dict(String, OutboxStatus),
    issue_counters: Dict(String, IssueCounterStatus),
    known_workspaces: Dict(String, KnownWorkspace),
    workstreams: Dict(String, WorkstreamStatus),
    scheduled_jobs: Dict(String, ScheduledJobStatus),
  )
}

pub type RunStatus {
  RunRunning(
    issue_id: String,
    issue_identifier: String,
    workspace_path: String,
    started_at_ms: Int,
  )
  RunFinished(
    issue_id: String,
    classification: String,
    token_total: Int,
    turns: Int,
    finished_at_ms: Int,
  )
  RunInterrupted(issue_id: String, reason: String, interrupted_at_ms: Int)
}

pub type WorkflowRunStatus {
  WorkflowRunActive(
    workflow_id: String,
    workflow_fingerprint: String,
    issue_id: String,
    issue_identifier: String,
    issue_fingerprint: String,
    observed_updated_at_ms: Int,
    run_root: String,
    started_at_ms: Int,
  )
  WorkflowRunFinished(
    workflow_id: String,
    issue_id: String,
    outcome: String,
    token_total: Int,
    turns: Int,
    finished_at_ms: Int,
    run_root: String,
  )
  WorkflowRunInterrupted(
    workflow_id: String,
    issue_id: String,
    reason: String,
    interrupted_at_ms: Int,
    run_root: String,
  )
  WorkflowRunSuperseded(
    workflow_id: String,
    issue_id: String,
    superseded_by_run_id: String,
    reason: String,
    superseded_at_ms: Int,
    run_root: String,
  )
}

pub type WorkflowContractManifestRef {
  WorkflowContractManifestRef(
    workflow_id: String,
    workflow_fingerprint: String,
    artifact_ref: String,
    artifact_sha256: String,
    artifact_bytes: Int,
    recorded_at_ms: Int,
  )
}

pub type WorkflowRepairStatus {
  WorkflowRepairStatus(
    workflow_id: String,
    issue_id: String,
    issue_identifier: String,
    requested_target: String,
    requested_step_id: Option(String),
    selected_step_id: String,
    failed_attempt_index: Int,
    next_attempt_index: Int,
    reason: String,
    requested_at_ms: Int,
    generation: Int,
  )
}

pub type StepAttemptStatus {
  StepAttemptPending(
    run_id: String,
    workflow_id: String,
    step_id: String,
    attempt_index: Int,
    workspace_name: String,
    workspace_path: String,
    run_root: String,
    source_workspace_name: Option(String),
    source_workspace_path: Option(String),
    prepared_at_ms: Int,
  )
  StepAttemptRunning(
    run_id: String,
    workflow_id: String,
    step_id: String,
    attempt_index: Int,
    workspace_name: String,
    workspace_path: String,
    run_root: String,
    source_workspace_name: Option(String),
    source_workspace_path: Option(String),
    operator_session_id: String,
    external_session_ref: Option(String),
    continuation_capable: Bool,
    pi_session_id: Option(String),
    pi_session_file: Option(String),
    pi_session_fact_count: Int,
    started_at_ms: Int,
  )
  StepAttemptFinishedStatus(
    run_id: String,
    workflow_id: String,
    step_id: String,
    attempt_index: Int,
    outcome: String,
    artifact_ref: String,
    artifact_sha256: String,
    workspace_name: String,
    workspace_path: String,
    run_root: String,
    source_workspace_name: Option(String),
    source_workspace_path: Option(String),
    token_total: Int,
    turns: Int,
    finished_at_ms: Int,
  )
  StepAttemptInterruptedStatus(
    run_id: String,
    workflow_id: String,
    step_id: String,
    attempt_index: Int,
    workspace_name: String,
    workspace_path: String,
    run_root: String,
    reason: String,
    continuation_capable: Bool,
    pi_session_id: Option(String),
    pi_session_file: Option(String),
    pi_session_fact_count: Int,
    interrupted_at_ms: Int,
  )
  StepAttemptSupersededStatus(
    run_id: String,
    workflow_id: String,
    step_id: String,
    attempt_index: Int,
    superseded_by_attempt_index: Int,
    reason: String,
    superseded_at_ms: Int,
  )
}

pub type CompletedWorkspace {
  CompletedWorkspace(
    workflow_id: String,
    run_id: String,
    run_root: String,
    workspace_name: String,
    path: String,
    source_workspace_name: Option(String),
    source_workspace_path: Option(String),
    attempt_index: Int,
  )
}

pub type RetryStatus {
  RetryScheduled(
    issue_identifier: String,
    delay_ms: Int,
    generation: Int,
    reason: String,
    scheduled_at_ms: Int,
  )
  RetryCancelled(generation: Int, reason: String, cancelled_at_ms: Int)
}

pub type ParkedIssue {
  ParkedIssue(
    issue_identifier: String,
    reason: String,
    observed_updated_at_ms: Int,
    parked_at_ms: Int,
    release_policy: String,
    issue_fingerprint: String,
  )
}

pub type IssueCounterStatus {
  IssueCounterStatus(
    issue_identifier: String,
    failure_attempts: Int,
    worker_sessions: Int,
    observed_updated_at_ms: Int,
    source_run_ids: List(String),
    updated_at_ms: Int,
  )
}

pub type KnownWorkspace {
  KnownWorkspace(
    issue_identifier: String,
    workspace_path: String,
    recorded_at_ms: Int,
  )
}

pub type CommandStatus {
  CommandSeen(
    issue_id: String,
    author_id: String,
    command_name: String,
    excerpt: String,
    seen_at_ms: Int,
  )
  CommandStarted(issue_id: String, command_name: String, started_at_ms: Int)
  CommandCompleted(
    issue_id: String,
    status: String,
    message_excerpt: String,
    completed_at_ms: Int,
  )
  CommandAcked(issue_id: String, acked_at_ms: Int)
}

pub type CommandReceiptState {
  CommandReceiptUnseen
  CommandReceiptSeen(
    issue_id: String,
    author_id: String,
    command_name: String,
    excerpt: String,
    seen_at_ms: Int,
  )
  CommandReceiptStarted(
    issue_id: String,
    author_id: String,
    command_name: String,
    excerpt: String,
    seen_at_ms: Int,
    started_at_ms: Int,
  )
  CommandReceiptCompleted(
    issue_id: String,
    author_id: String,
    command_name: String,
    excerpt: String,
    result_status: String,
    message_excerpt: String,
    seen_at_ms: Int,
    started_at_ms: Int,
    completed_at_ms: Int,
    acked_at_ms: Option(Int),
  )
  CommandReceiptAcked(issue_id: String, acked_at_ms: Int)
}

pub type OutboxStatus {
  OutboxPending(
    issue_id: String,
    outbox_kind: String,
    dedupe_key: String,
    pending_at_ms: Int,
  )
  OutboxPendingV2(
    issue_id: String,
    outbox_kind: String,
    dedupe_key: String,
    payload_json: String,
    pending_at_ms: Int,
  )
  OutboxCompleted(issue_id: String, outbox_kind: String, completed_at_ms: Int)
  OutboxFailed(
    issue_id: String,
    outbox_kind: String,
    error_code: String,
    failed_at_ms: Int,
  )
}

pub type ScheduledRunState {
  ScheduledIdle
  ScheduledDuePending
  ScheduledPaused
  ScheduledWaitingForGlobalSlot
  ScheduledActive
  ScheduledRetryWaiting
  ScheduledReportRetryWaiting
  ScheduledTerminalSuccess
  ScheduledTerminalFailure
}

pub type ScheduledRunSummary {
  ScheduledRunSummary(
    run_id: String,
    due_at_ms: Int,
    trigger: String,
    attempt: Int,
    status: String,
    reason: Option(String),
    session_id: Option(String),
    run_root: Option(String),
  )
}

pub type ScheduledReportRetry {
  ScheduledReportRetry(
    run_id: String,
    attempt: Int,
    dedupe_key: String,
    error_code: String,
    error_message: String,
    next_retry_at_ms: Int,
    generation: Int,
  )
}

pub type ScheduledJobStatus {
  ScheduledJobStatus(
    job_id: String,
    workflow_id: String,
    state: ScheduledRunState,
    current_run: Option(ScheduledRunSummary),
    last_due_at_ms: Option(Int),
    last_success_at_ms: Option(Int),
    last_success_run_id: Option(String),
    last_failure_at_ms: Option(Int),
    last_failure_run_id: Option(String),
    last_failure_reason: Option(String),
    retry_count: Int,
    skipped_overlap_count: Int,
    skipped_catch_up_count: Int,
    skipped_paused_count: Int,
    skipped_capacity_count: Int,
    failure_issue_id: Option(String),
    failure_dedupe_key: Option(String),
    report_retry: Option(ScheduledReportRetry),
    recent_run_ids: List(String),
  )
}

pub type WorkstreamStatus {
  WorkstreamStatus(
    workstream_id: String,
    task_ref: Option(record.TaskRefFields),
    created_at_ms: Option(Int),
    latest_assignment: Option(WorkstreamAssignment),
    artifacts: Dict(String, WorkstreamArtifactSnapshot),
    handoffs: Dict(String, WorkstreamHandoffSnapshot),
    queued_phase_runs: Dict(String, WorkstreamPhaseRun),
  )
}

pub type WorkstreamAssignment {
  WorkstreamAssignment(
    assignment_id: String,
    workflow_id: String,
    playbook_id: Option(String),
    reason: String,
    idempotency_key: String,
    assigned_at_ms: Int,
  )
}

pub type WorkstreamArtifactSnapshot {
  WorkstreamArtifactSnapshot(
    artifact_id: String,
    artifact_type: String,
    snapshot_ref: String,
    snapshot_sha256: String,
    snapshot_bytes: Int,
    original_path: String,
    contract_type: String,
    media_type: String,
    producer_workflow_id: String,
    producer_run_id: String,
    producer_step_id: String,
    idempotency_key: String,
    recorded_at_ms: Int,
  )
}

pub type WorkstreamHandoffSnapshot {
  WorkstreamHandoffSnapshot(
    handoff_id: String,
    handoff_ref: String,
    handoff_sha256: String,
    handoff_bytes: Int,
    source_workflow_id: String,
    source_run_id: String,
    idempotency_key: String,
    recorded_at_ms: Int,
  )
}

pub type WorkstreamPhaseRun {
  WorkstreamPhaseRun(
    phase_run_id: String,
    action_id: String,
    workflow_id: String,
    input_bundle_ref: String,
    input_bundle_sha256: String,
    input_bundle_bytes: Int,
    idempotency_key: String,
    queued_at_ms: Int,
  )
}

pub type OutboxReplay {
  OutboxReplay(
    outbox_id: String,
    issue_id: String,
    outbox_kind: String,
    dedupe_key: String,
    payload_json: String,
  )
}

pub type PendingOutboxError {
  OutboxPayloadMissing(outbox_id: String)
}

type RunSnapshot {
  RunSnapshot(run_id: String, status: RunStatus)
}

type WorkflowRunSnapshot {
  WorkflowRunSnapshot(run_id: String, status: WorkflowRunStatus)
}

type WorkflowTaskRefSnapshot {
  WorkflowTaskRefSnapshot(run_id: String, task_ref: record.TaskRefFields)
}

type StepAttemptSnapshot {
  StepAttemptSnapshot(key: String, status: StepAttemptStatus)
}

type RetrySnapshot {
  RetrySnapshot(issue_id: String, status: RetryStatus)
}

type ParkedSnapshot {
  ParkedSnapshot(issue_id: String, parked: ParkedIssue)
}

type CommandSnapshot {
  CommandSnapshot(comment_id: String, status: CommandStatus)
}

type CommandReceiptSnapshot {
  CommandReceiptSnapshot(comment_id: String, receipt: CommandReceiptState)
}

type OutboxSnapshot {
  OutboxSnapshot(outbox_id: String, status: OutboxStatus)
}

type IssueCounterSnapshot {
  IssueCounterSnapshot(issue_id: String, status: IssueCounterStatus)
}

type KnownWorkspaceSnapshot {
  KnownWorkspaceSnapshot(issue_id: String, workspace: KnownWorkspace)
}

type WorkflowContractManifestSnapshot {
  WorkflowContractManifestSnapshot(
    run_id: String,
    manifest: WorkflowContractManifestRef,
  )
}

type WorkflowRepairSnapshot {
  WorkflowRepairSnapshot(run_id: String, status: WorkflowRepairStatus)
}

type SnapshotFields {
  SnapshotFields(
    runs: List(RunSnapshot),
    workflow_runs: List(WorkflowRunSnapshot),
    workflow_task_refs: List(WorkflowTaskRefSnapshot),
    workflow_input_manifests: List(WorkflowContractManifestSnapshot),
    workflow_output_manifests: List(WorkflowContractManifestSnapshot),
    workflow_repairs: List(WorkflowRepairSnapshot),
    step_attempts: List(StepAttemptSnapshot),
    retries: List(RetrySnapshot),
    parked_issues: List(ParkedSnapshot),
    commands: List(CommandSnapshot),
    command_receipts: List(CommandReceiptSnapshot),
    outbox: List(OutboxSnapshot),
    issue_counters: List(IssueCounterSnapshot),
    known_workspaces: List(KnownWorkspaceSnapshot),
    workstreams: List(WorkstreamSnapshot),
    scheduled_jobs: List(ScheduledJobSnapshot),
  )
}

type WorkstreamSnapshot {
  WorkstreamSnapshot(workstream_id: String, status: WorkstreamStatus)
}

// Snapshot support for scheduled projection is intentionally lightweight in the
// first slice. Old snapshots omit this optional field and decode as an empty map.
type ScheduledJobSnapshot {
  ScheduledJobSnapshot(job_id: String, status: ScheduledJobStatus)
}

pub fn new() -> Projection {
  Projection(
    runs: dict.new(),
    workflow_runs: dict.new(),
    workflow_task_refs: dict.new(),
    workflow_input_manifests: dict.new(),
    workflow_output_manifests: dict.new(),
    workflow_repairs: dict.new(),
    step_attempts: dict.new(),
    retries: dict.new(),
    parked_issues: dict.new(),
    commands: dict.new(),
    command_receipts: dict.new(),
    outbox: dict.new(),
    issue_counters: dict.new(),
    known_workspaces: dict.new(),
    workstreams: dict.new(),
    scheduled_jobs: dict.new(),
  )
}

pub fn fold(records: List(record.LedgerRecord)) -> Projection {
  fold_from(new(), records)
}

pub fn fold_from(
  projection: Projection,
  records: List(record.LedgerRecord),
) -> Projection {
  list.fold(records, projection, fn(acc, ledger_record) {
    apply(acc, ledger_record)
  })
}

pub fn apply(
  projection: Projection,
  ledger_record: record.LedgerRecord,
) -> Projection {
  let at_ms = ledger_record.at_ms
  case ledger_record.body {
    record.RunStarted(run_id, issue_id, issue_identifier, workspace_path) ->
      Projection(
        ..projection,
        runs: dict.insert(
          projection.runs,
          run_id,
          RunRunning(issue_id, issue_identifier, workspace_path, at_ms),
        ),
      )
    record.RunFinished(run_id, issue_id, classification, token_total, turns) ->
      Projection(
        ..projection,
        runs: dict.insert(
          projection.runs,
          run_id,
          RunFinished(issue_id, classification, token_total, turns, at_ms),
        ),
      )
    record.RunInterrupted(run_id, issue_id, reason) ->
      Projection(
        ..projection,
        runs: dict.insert(
          projection.runs,
          run_id,
          RunInterrupted(issue_id, reason, at_ms),
        ),
      )
    record.WorkflowRunStarted(
      run_id,
      workflow_id,
      workflow_fingerprint,
      issue_id,
      issue_identifier,
      issue_fingerprint,
      observed_updated_at_ms,
      run_root,
    ) ->
      Projection(
        ..projection,
        workflow_runs: dict.insert(
          projection.workflow_runs,
          run_id,
          WorkflowRunActive(
            workflow_id,
            workflow_fingerprint,
            issue_id,
            issue_identifier,
            issue_fingerprint,
            observed_updated_at_ms,
            run_root,
            at_ms,
          ),
        ),
        workflow_task_refs: dict.insert(
          projection.workflow_task_refs,
          run_id,
          record.legacy_linear_task_ref_fields(issue_id, issue_identifier),
        ),
      )
    record.WorkflowRunStartedWithTask(
      run_id,
      workflow_id,
      workflow_fingerprint,
      issue_id,
      issue_identifier,
      task_ref,
      issue_fingerprint,
      observed_updated_at_ms,
      run_root,
    ) ->
      Projection(
        ..projection,
        workflow_runs: dict.insert(
          projection.workflow_runs,
          run_id,
          WorkflowRunActive(
            workflow_id,
            workflow_fingerprint,
            issue_id,
            issue_identifier,
            issue_fingerprint,
            observed_updated_at_ms,
            run_root,
            at_ms,
          ),
        ),
        workflow_task_refs: dict.insert(
          projection.workflow_task_refs,
          run_id,
          task_ref,
        ),
      )
    record.WorkflowRunFinished(
      run_id,
      workflow_id,
      issue_id,
      outcome,
      token_total,
      turns,
    ) -> {
      let run_root = workflow_run_root(projection, run_id)
      Projection(
        ..projection,
        workflow_runs: dict.insert(
          projection.workflow_runs,
          run_id,
          WorkflowRunFinished(
            workflow_id,
            issue_id,
            outcome,
            token_total,
            turns,
            at_ms,
            run_root,
          ),
        ),
        workflow_task_refs: preserve_or_insert_workflow_task_ref(
          projection.workflow_task_refs,
          run_id,
          record.linear_task_ref_fields(issue_id, None, None),
        ),
      )
    }
    record.WorkflowRunFinishedWithTask(
      run_id,
      workflow_id,
      issue_id,
      task_ref,
      outcome,
      token_total,
      turns,
    ) -> {
      let run_root = workflow_run_root(projection, run_id)
      Projection(
        ..projection,
        workflow_runs: dict.insert(
          projection.workflow_runs,
          run_id,
          WorkflowRunFinished(
            workflow_id,
            issue_id,
            outcome,
            token_total,
            turns,
            at_ms,
            run_root,
          ),
        ),
        workflow_task_refs: dict.insert(
          projection.workflow_task_refs,
          run_id,
          task_ref,
        ),
      )
    }
    record.WorkflowRunInputsRecorded(
      run_id,
      workflow_id,
      workflow_fingerprint,
      artifact_ref,
      artifact_sha256,
      artifact_bytes,
    ) ->
      Projection(
        ..projection,
        workflow_input_manifests: dict.insert(
          projection.workflow_input_manifests,
          run_id,
          WorkflowContractManifestRef(
            workflow_id,
            workflow_fingerprint,
            artifact_ref,
            artifact_sha256,
            artifact_bytes,
            at_ms,
          ),
        ),
      )
    record.WorkflowRunOutputsRecorded(
      run_id,
      workflow_id,
      workflow_fingerprint,
      artifact_ref,
      artifact_sha256,
      artifact_bytes,
    ) ->
      Projection(
        ..projection,
        workflow_output_manifests: dict.insert(
          projection.workflow_output_manifests,
          run_id,
          WorkflowContractManifestRef(
            workflow_id,
            workflow_fingerprint,
            artifact_ref,
            artifact_sha256,
            artifact_bytes,
            at_ms,
          ),
        ),
      )
    record.WorkflowRepairRequested(
      run_id,
      workflow_id,
      issue_id,
      issue_identifier,
      requested_target,
      requested_step_id,
      selected_step_id,
      failed_attempt_index,
      next_attempt_index,
      reason,
    ) ->
      Projection(
        ..projection,
        workflow_output_manifests: dict.delete(
          projection.workflow_output_manifests,
          run_id,
        ),
        workflow_repairs: dict.insert(
          projection.workflow_repairs,
          run_id,
          WorkflowRepairStatus(
            workflow_id: workflow_id,
            issue_id: issue_id,
            issue_identifier: issue_identifier,
            requested_target: requested_target,
            requested_step_id: requested_step_id,
            selected_step_id: selected_step_id,
            failed_attempt_index: failed_attempt_index,
            next_attempt_index: next_attempt_index,
            reason: reason,
            requested_at_ms: at_ms,
            generation: case latest_workflow_repair(projection, run_id) {
              Some(previous) -> previous.generation + 1
              None -> 1
            },
          ),
        ),
      )
    record.WorkflowRunInterrupted(run_id, workflow_id, issue_id, reason) -> {
      let run_root = workflow_run_root(projection, run_id)
      Projection(
        ..projection,
        workflow_runs: dict.insert(
          projection.workflow_runs,
          run_id,
          WorkflowRunInterrupted(workflow_id, issue_id, reason, at_ms, run_root),
        ),
      )
    }
    record.WorkflowRunSuperseded(
      run_id,
      workflow_id,
      issue_id,
      superseded_by_run_id,
      reason,
    ) -> {
      let run_root = workflow_run_root(projection, run_id)
      Projection(
        ..projection,
        workflow_runs: dict.insert(
          projection.workflow_runs,
          run_id,
          WorkflowRunSuperseded(
            workflow_id,
            issue_id,
            superseded_by_run_id,
            reason,
            at_ms,
            run_root,
          ),
        ),
      )
    }
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
    ) ->
      Projection(
        ..projection,
        step_attempts: dict.insert(
          projection.step_attempts,
          step_attempt_key(run_id, step_id, attempt_index),
          StepAttemptPending(
            run_id,
            workflow_id,
            step_id,
            attempt_index,
            workspace_name,
            workspace_path,
            run_root,
            source_workspace_name,
            source_workspace_path,
            at_ms,
          ),
        ),
      )
    record.StepAttemptStarted(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      operator_session_id,
      external_session_ref,
      continuation_capable,
    ) -> {
      let key = step_attempt_key(run_id, step_id, attempt_index)
      let status = case dict.get(projection.step_attempts, key) {
        Ok(StepAttemptPending(
          _,
          _,
          _,
          _,
          workspace_name,
          workspace_path,
          run_root,
          source_workspace_name,
          source_workspace_path,
          _,
        )) ->
          StepAttemptRunning(
            run_id,
            workflow_id,
            step_id,
            attempt_index,
            workspace_name,
            workspace_path,
            run_root,
            source_workspace_name,
            source_workspace_path,
            operator_session_id,
            external_session_ref,
            continuation_capable,
            None,
            None,
            0,
            at_ms,
          )
        _ ->
          StepAttemptRunning(
            run_id,
            workflow_id,
            step_id,
            attempt_index,
            "",
            "",
            "",
            None,
            None,
            operator_session_id,
            external_session_ref,
            continuation_capable,
            None,
            None,
            0,
            at_ms,
          )
      }
      Projection(
        ..projection,
        step_attempts: dict.insert(projection.step_attempts, key, status),
      )
    }
    record.StepAttemptContinuationStarted(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      session_id,
    ) -> {
      let key = step_attempt_key(run_id, step_id, attempt_index)
      let status = case dict.get(projection.step_attempts, key) {
        Ok(StepAttemptPending(
          _,
          _,
          _,
          _,
          workspace_name,
          workspace_path,
          run_root,
          source_workspace_name,
          source_workspace_path,
          _,
        )) ->
          StepAttemptRunning(
            run_id,
            workflow_id,
            step_id,
            attempt_index,
            workspace_name,
            workspace_path,
            run_root,
            source_workspace_name,
            source_workspace_path,
            "",
            Some(session_id),
            True,
            Some(session_id),
            None,
            1,
            at_ms,
          )
        Ok(StepAttemptInterruptedStatus(
          workspace_name: workspace_name,
          workspace_path: workspace_path,
          run_root: run_root,
          pi_session_file: pi_session_file,
          pi_session_fact_count: pi_session_fact_count,
          ..,
        )) ->
          StepAttemptRunning(
            run_id,
            workflow_id,
            step_id,
            attempt_index,
            workspace_name,
            workspace_path,
            run_root,
            None,
            None,
            "",
            Some(session_id),
            True,
            Some(session_id),
            pi_session_file,
            pi_session_fact_count,
            at_ms,
          )
        _ ->
          StepAttemptRunning(
            run_id,
            workflow_id,
            step_id,
            attempt_index,
            "",
            "",
            "",
            None,
            None,
            "",
            Some(session_id),
            True,
            Some(session_id),
            None,
            1,
            at_ms,
          )
      }
      Projection(
        ..projection,
        step_attempts: dict.insert(projection.step_attempts, key, status),
      )
    }
    record.StepAttemptPiSessionRecorded(
      run_id,
      issue_id,
      issue_identifier,
      workflow_id,
      _,
      step_id,
      workspace_name,
      attempt_index,
      workspace_path,
      session_id,
      session_file,
    ) ->
      apply_step_attempt_pi_session_recorded(
        projection,
        at_ms,
        run_id,
        workflow_id,
        step_id,
        workspace_name,
        attempt_index,
        workspace_path,
        session_id,
        session_file,
        record.legacy_linear_task_ref_fields(issue_id, issue_identifier),
      )
    record.StepAttemptPiSessionRecordedWithTask(
      run_id,
      _,
      _,
      task_ref,
      workflow_id,
      _,
      step_id,
      workspace_name,
      attempt_index,
      workspace_path,
      session_id,
      session_file,
    ) ->
      apply_step_attempt_pi_session_recorded(
        projection,
        at_ms,
        run_id,
        workflow_id,
        step_id,
        workspace_name,
        attempt_index,
        workspace_path,
        session_id,
        session_file,
        task_ref,
      )
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
    ) -> {
      let key = step_attempt_key(run_id, step_id, attempt_index)
      let #(run_root, source_workspace_name, source_workspace_path) =
        finished_workspace_metadata(projection.step_attempts, key)
      Projection(
        ..projection,
        step_attempts: dict.insert(
          projection.step_attempts,
          key,
          StepAttemptFinishedStatus(
            run_id,
            workflow_id,
            step_id,
            attempt_index,
            outcome,
            artifact_ref,
            artifact_sha256,
            workspace_name,
            workspace_path,
            run_root,
            source_workspace_name,
            source_workspace_path,
            token_total,
            turns,
            at_ms,
          ),
        ),
      )
    }
    record.StepAttemptInterrupted(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      reason,
    ) -> {
      let key = step_attempt_key(run_id, step_id, attempt_index)
      let status = case dict.get(projection.step_attempts, key) {
        Ok(StepAttemptRunning(
          workspace_name: workspace_name,
          workspace_path: workspace_path,
          run_root: run_root,
          continuation_capable: continuation_capable,
          pi_session_id: pi_session_id,
          pi_session_file: pi_session_file,
          pi_session_fact_count: pi_session_fact_count,
          ..,
        )) ->
          StepAttemptInterruptedStatus(
            run_id,
            workflow_id,
            step_id,
            attempt_index,
            workspace_name,
            workspace_path,
            run_root,
            reason,
            continuation_capable,
            pi_session_id,
            pi_session_file,
            pi_session_fact_count,
            at_ms,
          )
        Ok(StepAttemptPending(
          workspace_name: workspace_name,
          workspace_path: workspace_path,
          run_root: run_root,
          ..,
        )) ->
          StepAttemptInterruptedStatus(
            run_id,
            workflow_id,
            step_id,
            attempt_index,
            workspace_name,
            workspace_path,
            run_root,
            reason,
            False,
            None,
            None,
            0,
            at_ms,
          )
        _ ->
          StepAttemptInterruptedStatus(
            run_id,
            workflow_id,
            step_id,
            attempt_index,
            "",
            "",
            "",
            reason,
            False,
            None,
            None,
            0,
            at_ms,
          )
      }
      Projection(
        ..projection,
        step_attempts: dict.insert(projection.step_attempts, key, status),
      )
    }
    record.StepAttemptSuperseded(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      superseded_by_attempt_index,
      reason,
    ) ->
      Projection(
        ..projection,
        step_attempts: dict.insert(
          projection.step_attempts,
          step_attempt_key(run_id, step_id, attempt_index),
          StepAttemptSupersededStatus(
            run_id,
            workflow_id,
            step_id,
            attempt_index,
            superseded_by_attempt_index,
            reason,
            at_ms,
          ),
        ),
      )
    record.RetryScheduled(
      issue_id,
      issue_identifier,
      delay_ms,
      generation,
      reason,
    ) ->
      Projection(
        ..projection,
        retries: dict.insert(
          projection.retries,
          issue_id,
          RetryScheduled(issue_identifier, delay_ms, generation, reason, at_ms),
        ),
      )
    record.RetryCancelled(issue_id, generation, reason) ->
      Projection(
        ..projection,
        retries: dict.insert(
          projection.retries,
          issue_id,
          RetryCancelled(generation, reason, at_ms),
        ),
      )
    record.IssueCounterUpdated(
      issue_id,
      issue_identifier,
      failure_attempts,
      worker_sessions,
      observed_updated_at_ms,
      source_run_id,
    ) -> {
      let source_run_ids = case dict.get(projection.issue_counters, issue_id) {
        Ok(existing) -> existing.source_run_ids
        Error(_) -> []
      }
      let source_run_ids = case source_run_id {
        Some(run_id) -> insert_unique_string(source_run_ids, run_id)
        None -> source_run_ids
      }
      Projection(
        ..projection,
        issue_counters: dict.insert(
          projection.issue_counters,
          issue_id,
          IssueCounterStatus(
            issue_identifier,
            failure_attempts,
            worker_sessions,
            observed_updated_at_ms,
            source_run_ids,
            at_ms,
          ),
        ),
      )
    }
    record.KnownWorkspace(issue_id, issue_identifier, workspace_path) ->
      Projection(
        ..projection,
        known_workspaces: dict.insert(
          projection.known_workspaces,
          issue_id,
          KnownWorkspace(issue_identifier, workspace_path, at_ms),
        ),
      )
    record.IssueParked(
      issue_id,
      issue_identifier,
      reason,
      observed_updated_at_ms,
    ) ->
      Projection(
        ..projection,
        parked_issues: dict.insert(
          projection.parked_issues,
          issue_id,
          ParkedIssue(
            issue_identifier,
            reason,
            observed_updated_at_ms,
            at_ms,
            "explicit_unpark_only",
            "",
          ),
        ),
      )
    record.IssueParkedV2(
      issue_id,
      issue_identifier,
      reason,
      release_policy,
      issue_fingerprint,
      observed_updated_at_ms,
    ) ->
      Projection(
        ..projection,
        parked_issues: dict.insert(
          projection.parked_issues,
          issue_id,
          ParkedIssue(
            issue_identifier,
            reason,
            observed_updated_at_ms,
            at_ms,
            release_policy,
            issue_fingerprint,
          ),
        ),
      )
    record.IssueUnparked(issue_id, _, _) ->
      Projection(
        ..projection,
        parked_issues: dict.delete(projection.parked_issues, issue_id),
      )
    record.LinearCommandSeen(
      comment_id,
      issue_id,
      author_id,
      command_name,
      excerpt,
    ) -> {
      let receipt =
        seen_receipt(
          projection.command_receipts,
          comment_id,
          issue_id,
          author_id,
          command_name,
          excerpt,
          at_ms,
        )
      Projection(
        ..projection,
        commands: dict.insert(
          projection.commands,
          comment_id,
          CommandSeen(issue_id, author_id, command_name, excerpt, at_ms),
        ),
        command_receipts: dict.insert(
          projection.command_receipts,
          comment_id,
          receipt,
        ),
      )
    }
    record.LinearCommandStarted(comment_id, issue_id, command_name) -> {
      let receipt =
        started_receipt(
          projection.command_receipts,
          comment_id,
          issue_id,
          command_name,
          at_ms,
        )
      Projection(
        ..projection,
        commands: dict.insert(
          projection.commands,
          comment_id,
          CommandStarted(issue_id, command_name, at_ms),
        ),
        command_receipts: dict.insert(
          projection.command_receipts,
          comment_id,
          receipt,
        ),
      )
    }
    record.LinearCommandCompleted(comment_id, issue_id, status, message_excerpt) -> {
      let receipt =
        completed_receipt(
          projection.command_receipts,
          comment_id,
          issue_id,
          status,
          message_excerpt,
          at_ms,
        )
      Projection(
        ..projection,
        commands: dict.insert(
          projection.commands,
          comment_id,
          CommandCompleted(issue_id, status, message_excerpt, at_ms),
        ),
        command_receipts: dict.insert(
          projection.command_receipts,
          comment_id,
          receipt,
        ),
      )
    }
    record.LinearCommandAcked(comment_id, issue_id) -> {
      let receipt =
        acked_receipt(projection.command_receipts, comment_id, issue_id, at_ms)
      Projection(
        ..projection,
        commands: dict.insert(
          projection.commands,
          comment_id,
          CommandAcked(issue_id, at_ms),
        ),
        command_receipts: dict.insert(
          projection.command_receipts,
          comment_id,
          receipt,
        ),
      )
    }
    record.RemoteCommandSeen(
      _,
      event_id,
      task_remote_id,
      _,
      author_id,
      command_name,
      excerpt,
    ) -> {
      let receipt =
        seen_receipt(
          projection.command_receipts,
          event_id,
          task_remote_id,
          author_id,
          command_name,
          excerpt,
          at_ms,
        )
      Projection(
        ..projection,
        commands: dict.insert(
          projection.commands,
          event_id,
          CommandSeen(task_remote_id, author_id, command_name, excerpt, at_ms),
        ),
        command_receipts: dict.insert(
          projection.command_receipts,
          event_id,
          receipt,
        ),
      )
    }
    record.RemoteCommandStarted(_, event_id, task_remote_id, command_name) -> {
      let receipt =
        started_receipt(
          projection.command_receipts,
          event_id,
          task_remote_id,
          command_name,
          at_ms,
        )
      Projection(
        ..projection,
        commands: dict.insert(
          projection.commands,
          event_id,
          CommandStarted(task_remote_id, command_name, at_ms),
        ),
        command_receipts: dict.insert(
          projection.command_receipts,
          event_id,
          receipt,
        ),
      )
    }
    record.RemoteCommandCompleted(
      _,
      event_id,
      task_remote_id,
      status,
      message_excerpt,
    ) -> {
      let receipt =
        completed_receipt(
          projection.command_receipts,
          event_id,
          task_remote_id,
          status,
          message_excerpt,
          at_ms,
        )
      Projection(
        ..projection,
        commands: dict.insert(
          projection.commands,
          event_id,
          CommandCompleted(task_remote_id, status, message_excerpt, at_ms),
        ),
        command_receipts: dict.insert(
          projection.command_receipts,
          event_id,
          receipt,
        ),
      )
    }
    record.RemoteCommandAcked(_, event_id, task_remote_id) -> {
      let receipt =
        acked_receipt(
          projection.command_receipts,
          event_id,
          task_remote_id,
          at_ms,
        )
      Projection(
        ..projection,
        commands: dict.insert(
          projection.commands,
          event_id,
          CommandAcked(task_remote_id, at_ms),
        ),
        command_receipts: dict.insert(
          projection.command_receipts,
          event_id,
          receipt,
        ),
      )
    }
    record.ScheduledJobDue(job_id, workflow_id, due_at_ms, run_id, trigger) ->
      update_scheduled_job(
        projection,
        scheduled_due_status(
          scheduled_status(projection, job_id, workflow_id),
          due_at_ms,
          run_id,
          trigger,
        ),
      )
    record.ScheduledJobSkipped(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      reason,
      skipped_count,
    ) ->
      update_scheduled_job(
        projection,
        scheduled_skipped_status(
          scheduled_status(projection, job_id, workflow_id),
          due_at_ms,
          run_id,
          reason,
          skipped_count,
        ),
      )
    record.ScheduledRunPending(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      trigger,
      _,
    ) ->
      update_scheduled_job(
        projection,
        scheduled_pending_status(
          scheduled_status(projection, job_id, workflow_id),
          due_at_ms,
          run_id,
          trigger,
        ),
      )
    record.ScheduledRunPendingBlocked(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      reason,
      _,
    ) ->
      update_scheduled_job(
        projection,
        scheduled_blocked_status(
          scheduled_status(projection, job_id, workflow_id),
          due_at_ms,
          run_id,
          reason,
        ),
      )
    record.ScheduledRunPendingCancelled(
      job_id,
      workflow_id,
      _,
      run_id,
      reason,
      _,
    ) ->
      update_scheduled_job(
        projection,
        scheduled_cancelled_status(
          scheduled_status(projection, job_id, workflow_id),
          run_id,
          reason,
        ),
      )
    record.ScheduledRunStarted(
      job_id,
      workflow_id,
      due_at_ms,
      _,
      run_id,
      attempt,
      session_id,
      run_root,
    ) ->
      update_scheduled_job(
        projection,
        scheduled_started_status(
          scheduled_status(projection, job_id, workflow_id),
          due_at_ms,
          run_id,
          attempt,
          session_id,
          run_root,
        ),
      )
    record.ScheduledRunSucceeded(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      attempt,
      finished_at_ms,
      _,
      _,
    ) ->
      update_scheduled_job(
        projection,
        scheduled_succeeded_status(
          scheduled_status(projection, job_id, workflow_id),
          due_at_ms,
          run_id,
          attempt,
          finished_at_ms,
        ),
      )
    record.ScheduledRunFailed(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      attempt,
      finished_at_ms,
      reason,
      retry_exhausted,
      run_root,
    ) ->
      update_scheduled_job(
        projection,
        scheduled_failed_status(
          scheduled_status(projection, job_id, workflow_id),
          due_at_ms,
          run_id,
          attempt,
          finished_at_ms,
          reason,
          retry_exhausted,
          run_root,
        ),
      )
    record.ScheduledRunRetryScheduled(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      next_attempt,
      _,
      _,
      reason,
    ) ->
      update_scheduled_job(
        projection,
        scheduled_retry_status(
          scheduled_status(projection, job_id, workflow_id),
          due_at_ms,
          run_id,
          next_attempt,
          reason,
        ),
      )
    record.ScheduledRunRetryCancelled(job_id, run_id, _, reason) ->
      update_scheduled_job(
        projection,
        scheduled_cancelled_status(
          scheduled_status(projection, job_id, ""),
          run_id,
          reason,
        ),
      )
    record.ScheduledFailureReported(
      job_id,
      workflow_id,
      _,
      _,
      _,
      dedupe_key,
      linear_issue_id,
      _,
    ) ->
      update_scheduled_job(
        projection,
        scheduled_reported_status(
          scheduled_status(projection, job_id, workflow_id),
          dedupe_key,
          linear_issue_id,
        ),
      )
    record.ScheduledFailureReportFailed(
      job_id,
      workflow_id,
      _,
      run_id,
      attempt,
      dedupe_key,
      error_code,
      error_message,
      next_retry_at_ms,
      generation,
    ) ->
      update_scheduled_job(
        projection,
        scheduled_report_failed_status(
          scheduled_status(projection, job_id, workflow_id),
          run_id,
          attempt,
          dedupe_key,
          error_code,
          error_message,
          next_retry_at_ms,
          generation,
        ),
      )
    record.OutboxPending(outbox_id, issue_id, outbox_kind, dedupe_key) ->
      Projection(
        ..projection,
        outbox: dict.insert(
          projection.outbox,
          outbox_id,
          OutboxPending(issue_id, outbox_kind, dedupe_key, at_ms),
        ),
      )
    record.OutboxPendingV2(
      outbox_id,
      issue_id,
      outbox_kind,
      dedupe_key,
      payload_json,
    ) ->
      Projection(
        ..projection,
        outbox: dict.insert(
          projection.outbox,
          outbox_id,
          OutboxPendingV2(
            issue_id,
            outbox_kind,
            dedupe_key,
            payload_json,
            at_ms,
          ),
        ),
      )
    record.OutboxCompleted(outbox_id, issue_id, outbox_kind) ->
      Projection(
        ..projection,
        outbox: dict.insert(
          projection.outbox,
          outbox_id,
          OutboxCompleted(issue_id, outbox_kind, at_ms),
        ),
      )
    record.OutboxFailed(outbox_id, issue_id, outbox_kind, error_code) ->
      Projection(
        ..projection,
        outbox: dict.insert(
          projection.outbox,
          outbox_id,
          OutboxFailed(issue_id, outbox_kind, error_code, at_ms),
        ),
      )
    record.WorkstreamCreated(workstream_id, task_ref, _) ->
      update_workstream(projection, workstream_id, fn(status) {
        WorkstreamStatus(
          ..status,
          task_ref: Some(task_ref),
          created_at_ms: first_some_int(status.created_at_ms, at_ms),
        )
      })
    record.WorkstreamAssigned(
      workstream_id,
      assignment_id,
      workflow_id,
      playbook_id,
      reason,
      idempotency_key,
    ) ->
      update_workstream(projection, workstream_id, fn(status) {
        WorkstreamStatus(
          ..status,
          latest_assignment: Some(WorkstreamAssignment(
            assignment_id: assignment_id,
            workflow_id: workflow_id,
            playbook_id: playbook_id,
            reason: reason,
            idempotency_key: idempotency_key,
            assigned_at_ms: at_ms,
          )),
        )
      })
    record.WorkstreamArtifactRecorded(
      workstream_id,
      artifact_id,
      artifact_type,
      snapshot_ref,
      snapshot_sha256,
      snapshot_bytes,
      original_path,
      contract_type,
      media_type,
      producer_workflow_id,
      producer_run_id,
      producer_step_id,
      idempotency_key,
    ) ->
      update_workstream(projection, workstream_id, fn(status) {
        WorkstreamStatus(
          ..status,
          artifacts: dict.insert(
            status.artifacts,
            snapshot_ref,
            WorkstreamArtifactSnapshot(
              artifact_id: artifact_id,
              artifact_type: artifact_type,
              snapshot_ref: snapshot_ref,
              snapshot_sha256: snapshot_sha256,
              snapshot_bytes: snapshot_bytes,
              original_path: original_path,
              contract_type: contract_type,
              media_type: media_type,
              producer_workflow_id: producer_workflow_id,
              producer_run_id: producer_run_id,
              producer_step_id: producer_step_id,
              idempotency_key: idempotency_key,
              recorded_at_ms: at_ms,
            ),
          ),
        )
      })
    record.WorkstreamHandoffRecorded(
      workstream_id,
      handoff_id,
      handoff_ref,
      handoff_sha256,
      handoff_bytes,
      source_workflow_id,
      source_run_id,
      idempotency_key,
    ) ->
      update_workstream(projection, workstream_id, fn(status) {
        WorkstreamStatus(
          ..status,
          handoffs: dict.insert(
            status.handoffs,
            handoff_ref,
            WorkstreamHandoffSnapshot(
              handoff_id: handoff_id,
              handoff_ref: handoff_ref,
              handoff_sha256: handoff_sha256,
              handoff_bytes: handoff_bytes,
              source_workflow_id: source_workflow_id,
              source_run_id: source_run_id,
              idempotency_key: idempotency_key,
              recorded_at_ms: at_ms,
            ),
          ),
        )
      })
    record.WorkstreamPhaseRunQueued(
      workstream_id,
      phase_run_id,
      action_id,
      workflow_id,
      input_bundle_ref,
      input_bundle_sha256,
      input_bundle_bytes,
      idempotency_key,
    ) ->
      update_workstream(projection, workstream_id, fn(status) {
        WorkstreamStatus(
          ..status,
          queued_phase_runs: dict.insert(
            status.queued_phase_runs,
            phase_run_id,
            WorkstreamPhaseRun(
              phase_run_id: phase_run_id,
              action_id: action_id,
              workflow_id: workflow_id,
              input_bundle_ref: input_bundle_ref,
              input_bundle_sha256: input_bundle_sha256,
              input_bundle_bytes: input_bundle_bytes,
              idempotency_key: idempotency_key,
              queued_at_ms: at_ms,
            ),
          ),
        )
      })
  }
}

fn update_workstream(
  projection: Projection,
  workstream_id: String,
  update: fn(WorkstreamStatus) -> WorkstreamStatus,
) -> Projection {
  let current = case dict.get(projection.workstreams, workstream_id) {
    Ok(status) -> status
    Error(_) -> empty_workstream_status(workstream_id)
  }
  Projection(
    ..projection,
    workstreams: dict.insert(
      projection.workstreams,
      workstream_id,
      update(current),
    ),
  )
}

fn empty_workstream_status(workstream_id: String) -> WorkstreamStatus {
  WorkstreamStatus(
    workstream_id: workstream_id,
    task_ref: None,
    created_at_ms: None,
    latest_assignment: None,
    artifacts: dict.new(),
    handoffs: dict.new(),
    queued_phase_runs: dict.new(),
  )
}

fn first_some_int(existing: Option(Int), fallback: Int) -> Option(Int) {
  case existing {
    Some(value) -> Some(value)
    None -> Some(fallback)
  }
}

pub fn scheduled_status_for(
  projection: Projection,
  job_id: String,
) -> Result(ScheduledJobStatus, Nil) {
  dict.get(projection.scheduled_jobs, job_id)
}

pub fn scheduled_statuses(projection: Projection) -> List(ScheduledJobStatus) {
  dict.values(projection.scheduled_jobs)
}

fn apply_step_attempt_pi_session_recorded(
  projection: Projection,
  at_ms: Int,
  run_id: String,
  workflow_id: String,
  step_id: String,
  workspace_name: String,
  attempt_index: Int,
  workspace_path: String,
  session_id: String,
  session_file: String,
  task_ref: record.TaskRefFields,
) -> Projection {
  let key = step_attempt_key(run_id, step_id, attempt_index)
  let status = case dict.get(projection.step_attempts, key) {
    Ok(StepAttemptRunning(
      workflow_id: status_workflow_id,
      workspace_name: status_workspace_name,
      workspace_path: status_workspace_path,
      run_root: run_root,
      source_workspace_name: source_workspace_name,
      source_workspace_path: source_workspace_path,
      operator_session_id: operator_session_id,
      external_session_ref: external_session_ref,
      continuation_capable: continuation_capable,
      pi_session_fact_count: count,
      started_at_ms: started_at_ms,
      ..,
    )) -> {
      let #(pi_session_id, pi_session_file, fact_count) =
        session_fact_values(
          status_workflow_id,
          status_workspace_name,
          status_workspace_path,
          workflow_id,
          workspace_name,
          workspace_path,
          session_id,
          session_file,
          count,
        )
      StepAttemptRunning(
        run_id,
        status_workflow_id,
        step_id,
        attempt_index,
        status_workspace_name,
        status_workspace_path,
        run_root,
        source_workspace_name,
        source_workspace_path,
        operator_session_id,
        external_session_ref,
        continuation_capable,
        pi_session_id,
        pi_session_file,
        fact_count,
        started_at_ms,
      )
    }
    Ok(StepAttemptInterruptedStatus(
      workflow_id: status_workflow_id,
      workspace_name: status_workspace_name,
      workspace_path: status_workspace_path,
      run_root: run_root,
      reason: reason,
      continuation_capable: continuation_capable,
      pi_session_fact_count: count,
      interrupted_at_ms: interrupted_at_ms,
      ..,
    )) -> {
      let #(pi_session_id, pi_session_file, fact_count) =
        session_fact_values(
          status_workflow_id,
          status_workspace_name,
          status_workspace_path,
          workflow_id,
          workspace_name,
          workspace_path,
          session_id,
          session_file,
          count,
        )
      StepAttemptInterruptedStatus(
        run_id,
        status_workflow_id,
        step_id,
        attempt_index,
        status_workspace_name,
        status_workspace_path,
        run_root,
        reason,
        continuation_capable,
        pi_session_id,
        pi_session_file,
        fact_count,
        interrupted_at_ms,
      )
    }
    _ ->
      StepAttemptRunning(
        run_id,
        workflow_id,
        step_id,
        attempt_index,
        workspace_name,
        workspace_path,
        "",
        None,
        None,
        "",
        None,
        True,
        Some(session_id),
        Some(session_file),
        1,
        at_ms,
      )
  }
  Projection(
    ..projection,
    step_attempts: dict.insert(projection.step_attempts, key, status),
    workflow_task_refs: preserve_or_insert_workflow_task_ref(
      projection.workflow_task_refs,
      run_id,
      task_ref,
    ),
  )
}

fn update_scheduled_job(
  projection: Projection,
  status: ScheduledJobStatus,
) -> Projection {
  Projection(
    ..projection,
    scheduled_jobs: dict.insert(
      projection.scheduled_jobs,
      status.job_id,
      status,
    ),
  )
}

fn scheduled_status(
  projection: Projection,
  job_id: String,
  workflow_id: String,
) -> ScheduledJobStatus {
  case dict.get(projection.scheduled_jobs, job_id) {
    Ok(status) ->
      case status.workflow_id == "" && workflow_id != "" {
        True -> ScheduledJobStatus(..status, workflow_id: workflow_id)
        False -> status
      }
    Error(_) -> empty_scheduled_status(job_id, workflow_id)
  }
}

fn empty_scheduled_status(
  job_id: String,
  workflow_id: String,
) -> ScheduledJobStatus {
  ScheduledJobStatus(
    job_id: job_id,
    workflow_id: workflow_id,
    state: ScheduledIdle,
    current_run: None,
    last_due_at_ms: None,
    last_success_at_ms: None,
    last_success_run_id: None,
    last_failure_at_ms: None,
    last_failure_run_id: None,
    last_failure_reason: None,
    retry_count: 0,
    skipped_overlap_count: 0,
    skipped_catch_up_count: 0,
    skipped_paused_count: 0,
    skipped_capacity_count: 0,
    failure_issue_id: None,
    failure_dedupe_key: None,
    report_retry: None,
    recent_run_ids: [],
  )
}

fn scheduled_due_status(
  status: ScheduledJobStatus,
  due_at_ms: Int,
  run_id: String,
  trigger: String,
) -> ScheduledJobStatus {
  ScheduledJobStatus(
    ..status,
    state: ScheduledDuePending,
    current_run: Some(ScheduledRunSummary(
      run_id: run_id,
      due_at_ms: due_at_ms,
      trigger: trigger,
      attempt: 0,
      status: "due",
      reason: None,
      session_id: None,
      run_root: None,
    )),
    last_due_at_ms: Some(due_at_ms),
    recent_run_ids: insert_recent_run(status.recent_run_ids, run_id),
  )
}

fn scheduled_pending_status(
  status: ScheduledJobStatus,
  due_at_ms: Int,
  run_id: String,
  trigger: String,
) -> ScheduledJobStatus {
  ScheduledJobStatus(
    ..status,
    state: ScheduledDuePending,
    current_run: Some(ScheduledRunSummary(
      run_id: run_id,
      due_at_ms: due_at_ms,
      trigger: trigger,
      attempt: 0,
      status: "pending",
      reason: None,
      session_id: None,
      run_root: None,
    )),
    recent_run_ids: insert_recent_run(status.recent_run_ids, run_id),
  )
}

fn scheduled_blocked_status(
  status: ScheduledJobStatus,
  due_at_ms: Int,
  run_id: String,
  reason: String,
) -> ScheduledJobStatus {
  let state = case reason {
    "paused" -> ScheduledPaused
    "waiting_for_global_slot" -> ScheduledWaitingForGlobalSlot
    _ -> ScheduledDuePending
  }
  ScheduledJobStatus(
    ..status,
    state: state,
    current_run: Some(ScheduledRunSummary(
      run_id: run_id,
      due_at_ms: due_at_ms,
      trigger: current_trigger(status),
      attempt: current_attempt(status),
      status: "blocked",
      reason: Some(reason),
      session_id: None,
      run_root: current_run_root(status),
    )),
  )
}

fn scheduled_started_status(
  status: ScheduledJobStatus,
  due_at_ms: Int,
  run_id: String,
  attempt: Int,
  session_id: String,
  run_root: String,
) -> ScheduledJobStatus {
  ScheduledJobStatus(
    ..status,
    state: ScheduledActive,
    current_run: Some(ScheduledRunSummary(
      run_id: run_id,
      due_at_ms: due_at_ms,
      trigger: current_trigger(status),
      attempt: attempt,
      status: "active",
      reason: None,
      session_id: Some(session_id),
      run_root: Some(run_root),
    )),
    recent_run_ids: insert_recent_run(status.recent_run_ids, run_id),
  )
}

fn scheduled_succeeded_status(
  status: ScheduledJobStatus,
  due_at_ms: Int,
  run_id: String,
  attempt: Int,
  finished_at_ms: Int,
) -> ScheduledJobStatus {
  ScheduledJobStatus(
    ..status,
    state: ScheduledTerminalSuccess,
    current_run: Some(ScheduledRunSummary(
      run_id: run_id,
      due_at_ms: due_at_ms,
      trigger: current_trigger(status),
      attempt: attempt,
      status: "succeeded",
      reason: None,
      session_id: current_session_id(status),
      run_root: current_run_root(status),
    )),
    last_success_at_ms: Some(finished_at_ms),
    last_success_run_id: Some(run_id),
    report_retry: None,
    recent_run_ids: insert_recent_run(status.recent_run_ids, run_id),
  )
}

fn scheduled_failed_status(
  status: ScheduledJobStatus,
  due_at_ms: Int,
  run_id: String,
  attempt: Int,
  finished_at_ms: Int,
  reason: String,
  retry_exhausted: Bool,
  run_root: Option(String),
) -> ScheduledJobStatus {
  let state = case retry_exhausted {
    True -> ScheduledTerminalFailure
    False -> ScheduledRetryWaiting
  }
  ScheduledJobStatus(
    ..status,
    state: state,
    current_run: Some(ScheduledRunSummary(
      run_id: run_id,
      due_at_ms: due_at_ms,
      trigger: current_trigger(status),
      attempt: attempt,
      status: "failed",
      reason: Some(reason),
      session_id: current_session_id(status),
      run_root: option.or(run_root, current_run_root(status)),
    )),
    last_failure_at_ms: Some(finished_at_ms),
    last_failure_run_id: Some(run_id),
    last_failure_reason: Some(reason),
    retry_count: attempt,
    recent_run_ids: insert_recent_run(status.recent_run_ids, run_id),
  )
}

fn scheduled_retry_status(
  status: ScheduledJobStatus,
  due_at_ms: Int,
  run_id: String,
  next_attempt: Int,
  reason: String,
) -> ScheduledJobStatus {
  ScheduledJobStatus(
    ..status,
    state: ScheduledRetryWaiting,
    current_run: Some(ScheduledRunSummary(
      run_id: run_id,
      due_at_ms: due_at_ms,
      trigger: current_trigger(status),
      attempt: next_attempt,
      status: "retry_waiting",
      reason: Some(reason),
      session_id: current_session_id(status),
      run_root: current_run_root(status),
    )),
    retry_count: next_attempt - 1,
  )
}

fn scheduled_cancelled_status(
  status: ScheduledJobStatus,
  run_id: String,
  reason: String,
) -> ScheduledJobStatus {
  ScheduledJobStatus(
    ..status,
    state: ScheduledIdle,
    current_run: Some(ScheduledRunSummary(
      run_id: run_id,
      due_at_ms: current_due_at(status),
      trigger: current_trigger(status),
      attempt: current_attempt(status),
      status: "cancelled",
      reason: Some(reason),
      session_id: current_session_id(status),
      run_root: current_run_root(status),
    )),
  )
}

fn scheduled_reported_status(
  status: ScheduledJobStatus,
  dedupe_key: String,
  linear_issue_id: String,
) -> ScheduledJobStatus {
  ScheduledJobStatus(
    ..status,
    state: ScheduledTerminalFailure,
    failure_issue_id: Some(linear_issue_id),
    failure_dedupe_key: Some(dedupe_key),
    report_retry: None,
  )
}

fn scheduled_report_failed_status(
  status: ScheduledJobStatus,
  run_id: String,
  attempt: Int,
  dedupe_key: String,
  error_code: String,
  error_message: String,
  next_retry_at_ms: Int,
  generation: Int,
) -> ScheduledJobStatus {
  ScheduledJobStatus(
    ..status,
    state: ScheduledReportRetryWaiting,
    failure_dedupe_key: Some(dedupe_key),
    report_retry: Some(ScheduledReportRetry(
      run_id: run_id,
      attempt: attempt,
      dedupe_key: dedupe_key,
      error_code: error_code,
      error_message: error_message,
      next_retry_at_ms: next_retry_at_ms,
      generation: generation,
    )),
  )
}

fn scheduled_skipped_status(
  status: ScheduledJobStatus,
  due_at_ms: Int,
  run_id: String,
  reason: String,
  skipped_count: Int,
) -> ScheduledJobStatus {
  let #(overlap, catch_up, paused, capacity) = case reason {
    "overlap_running" -> #(skipped_count, 0, 0, 0)
    "catch_up_disabled" -> #(0, skipped_count, 0, 0)
    "schedule_paused" -> #(0, 0, skipped_count, 0)
    "waiting_for_global_slot" -> #(0, 0, 0, skipped_count)
    _ -> #(0, 0, 0, 0)
  }
  ScheduledJobStatus(
    ..status,
    last_due_at_ms: Some(due_at_ms),
    skipped_overlap_count: status.skipped_overlap_count + overlap,
    skipped_catch_up_count: status.skipped_catch_up_count + catch_up,
    skipped_paused_count: status.skipped_paused_count + paused,
    skipped_capacity_count: status.skipped_capacity_count + capacity,
    recent_run_ids: insert_recent_run(status.recent_run_ids, run_id),
  )
}

fn current_trigger(status: ScheduledJobStatus) -> String {
  case status.current_run {
    Some(run) -> run.trigger
    None -> "automatic"
  }
}

fn current_due_at(status: ScheduledJobStatus) -> Int {
  case status.current_run {
    Some(run) -> run.due_at_ms
    None -> 0
  }
}

fn current_attempt(status: ScheduledJobStatus) -> Int {
  case status.current_run {
    Some(run) -> run.attempt
    None -> 0
  }
}

fn current_session_id(status: ScheduledJobStatus) -> Option(String) {
  case status.current_run {
    Some(run) -> run.session_id
    None -> None
  }
}

fn current_run_root(status: ScheduledJobStatus) -> Option(String) {
  case status.current_run {
    Some(run) -> run.run_root
    None -> None
  }
}

fn insert_recent_run(ids: List(String), run_id: String) -> List(String) {
  case list.contains(ids, run_id) {
    True -> trim_recent_runs(ids)
    False -> trim_recent_runs([run_id, ..ids])
  }
}

fn trim_recent_runs(ids: List(String)) -> List(String) {
  list.take(ids, max_recent_scheduled_run_ids)
}

fn seen_receipt(
  receipts: Dict(String, CommandReceiptState),
  comment_id: String,
  issue_id: String,
  author_id: String,
  command_name: String,
  excerpt: String,
  seen_at_ms: Int,
) -> CommandReceiptState {
  case dict.get(receipts, comment_id) {
    Ok(CommandReceiptUnseen) | Error(_) ->
      CommandReceiptSeen(issue_id, author_id, command_name, excerpt, seen_at_ms)
    Ok(receipt) -> receipt
  }
}

fn started_receipt(
  receipts: Dict(String, CommandReceiptState),
  comment_id: String,
  issue_id: String,
  command_name: String,
  started_at_ms: Int,
) -> CommandReceiptState {
  case dict.get(receipts, comment_id) {
    Ok(receipt) ->
      case receipt {
        CommandReceiptSeen(_, author_id, _, excerpt, seen_at_ms) ->
          CommandReceiptStarted(
            issue_id,
            author_id,
            command_name,
            excerpt,
            seen_at_ms,
            started_at_ms,
          )
        CommandReceiptStarted(_, author_id, _, excerpt, seen_at_ms, _) ->
          CommandReceiptStarted(
            issue_id,
            author_id,
            command_name,
            excerpt,
            seen_at_ms,
            started_at_ms,
          )
        CommandReceiptCompleted(..) | CommandReceiptAcked(..) -> receipt
        CommandReceiptUnseen ->
          CommandReceiptStarted(
            issue_id,
            "",
            command_name,
            "",
            0,
            started_at_ms,
          )
      }
    Error(_) ->
      CommandReceiptStarted(issue_id, "", command_name, "", 0, started_at_ms)
  }
}

fn completed_receipt(
  receipts: Dict(String, CommandReceiptState),
  comment_id: String,
  issue_id: String,
  result_status: String,
  message_excerpt: String,
  completed_at_ms: Int,
) -> CommandReceiptState {
  case dict.get(receipts, comment_id) {
    Ok(CommandReceiptStarted(
      _,
      author_id,
      command_name,
      excerpt,
      seen_at_ms,
      started_at_ms,
    )) ->
      CommandReceiptCompleted(
        issue_id,
        author_id,
        command_name,
        excerpt,
        result_status,
        message_excerpt,
        seen_at_ms,
        started_at_ms,
        completed_at_ms,
        None,
      )
    Ok(CommandReceiptSeen(_, author_id, command_name, excerpt, seen_at_ms)) ->
      CommandReceiptCompleted(
        issue_id,
        author_id,
        command_name,
        excerpt,
        result_status,
        message_excerpt,
        seen_at_ms,
        0,
        completed_at_ms,
        None,
      )
    Ok(CommandReceiptCompleted(
      _,
      author_id,
      command_name,
      excerpt,
      _,
      _,
      seen_at_ms,
      started_at_ms,
      _,
      acked_at_ms,
    )) ->
      CommandReceiptCompleted(
        issue_id,
        author_id,
        command_name,
        excerpt,
        result_status,
        message_excerpt,
        seen_at_ms,
        started_at_ms,
        completed_at_ms,
        acked_at_ms,
      )
    Ok(CommandReceiptAcked(_, acked_at_ms)) ->
      CommandReceiptCompleted(
        issue_id,
        "",
        "unknown",
        "",
        result_status,
        message_excerpt,
        0,
        0,
        completed_at_ms,
        Some(acked_at_ms),
      )
    _ ->
      CommandReceiptCompleted(
        issue_id,
        "",
        "unknown",
        "",
        result_status,
        message_excerpt,
        0,
        0,
        completed_at_ms,
        None,
      )
  }
}

fn acked_receipt(
  receipts: Dict(String, CommandReceiptState),
  comment_id: String,
  issue_id: String,
  acked_at_ms: Int,
) -> CommandReceiptState {
  case dict.get(receipts, comment_id) {
    Ok(CommandReceiptCompleted(
      _,
      author_id,
      command_name,
      excerpt,
      result_status,
      message_excerpt,
      seen_at_ms,
      started_at_ms,
      completed_at_ms,
      _,
    )) ->
      CommandReceiptCompleted(
        issue_id,
        author_id,
        command_name,
        excerpt,
        result_status,
        message_excerpt,
        seen_at_ms,
        started_at_ms,
        completed_at_ms,
        Some(acked_at_ms),
      )
    _ -> CommandReceiptAcked(issue_id, acked_at_ms)
  }
}

pub fn step_attempt_key(
  run_id: String,
  step_id: String,
  attempt_index: Int,
) -> String {
  run_id <> "\u{001f}" <> step_id <> "\u{001f}" <> int.to_string(attempt_index)
}

fn session_fact_values(
  status_workflow_id: String,
  status_workspace_name: String,
  status_workspace_path: String,
  fact_workflow_id: String,
  fact_workspace_name: String,
  fact_workspace_path: String,
  session_id: String,
  session_file: String,
  current_count: Int,
) -> #(Option(String), Option(String), Int) {
  let fact_count = current_count + 1
  case
    status_workflow_id == fact_workflow_id
    && status_workspace_name == fact_workspace_name
    && status_workspace_path == fact_workspace_path
  {
    True -> #(Some(session_id), Some(session_file), fact_count)
    False -> #(None, None, fact_count)
  }
}

pub fn next_attempt_index(
  projection: Projection,
  run_id: String,
  step_id: String,
) -> Int {
  projection.step_attempts
  |> dict.values
  |> list.fold(0, fn(max_index, status) {
    case attempt_identity(status) {
      #(status_run_id, status_step_id, attempt_index) ->
        case
          status_run_id == run_id
          && status_step_id == step_id
          && attempt_index > max_index
        {
          True -> attempt_index
          False -> max_index
        }
    }
  })
  |> add_one
}

pub fn dependency_satisfying_attempt(status: StepAttemptStatus) -> Bool {
  case status {
    StepAttemptFinishedStatus(outcome: outcome, ..) ->
      outcome == "completed" || outcome == "failed_continued"
    _ -> False
  }
}

fn finished_workspace_metadata(
  attempts: Dict(String, StepAttemptStatus),
  key: String,
) -> #(String, Option(String), Option(String)) {
  case dict.get(attempts, key) {
    Ok(StepAttemptPending(
      run_root: run_root,
      source_workspace_name: source_workspace_name,
      source_workspace_path: source_workspace_path,
      ..,
    )) -> #(run_root, source_workspace_name, source_workspace_path)
    Ok(StepAttemptRunning(
      run_root: run_root,
      source_workspace_name: source_workspace_name,
      source_workspace_path: source_workspace_path,
      ..,
    )) -> #(run_root, source_workspace_name, source_workspace_path)
    Ok(StepAttemptFinishedStatus(
      run_root: run_root,
      source_workspace_name: source_workspace_name,
      source_workspace_path: source_workspace_path,
      ..,
    )) -> #(run_root, source_workspace_name, source_workspace_path)
    _ -> #("", None, None)
  }
}

pub fn latest_completed_workspace(
  projection: Projection,
  run_id: String,
  workspace_name: String,
) -> Result(CompletedWorkspace, Nil) {
  projection.step_attempts
  |> dict.values
  |> list.filter(fn(status) {
    case status {
      StepAttemptFinishedStatus(
        run_id: status_run_id,
        workspace_name: status_workspace_name,
        outcome: outcome,
        ..,
      ) ->
        status_run_id == run_id
        && status_workspace_name == workspace_name
        && { outcome == "completed" || outcome == "failed_continued" }
      _ -> False
    }
  })
  |> latest_finished_workspace(None)
}

pub fn active_workflow_runs(
  projection: Projection,
) -> List(#(String, WorkflowRunStatus)) {
  projection.workflow_runs
  |> dict.to_list
  |> list.filter(fn(entry) {
    let #(_, status) = entry
    case status {
      WorkflowRunActive(..) -> True
      _ -> False
    }
  })
}

pub fn has_workflow_run(projection: Projection, run_id: String) -> Bool {
  dict.has_key(projection.workflow_runs, run_id)
}

pub fn workflow_input_manifest(
  projection: Projection,
  run_id: String,
) -> Option(WorkflowContractManifestRef) {
  dict.get(projection.workflow_input_manifests, run_id) |> option.from_result
}

pub fn workflow_output_manifest(
  projection: Projection,
  run_id: String,
) -> Option(WorkflowContractManifestRef) {
  dict.get(projection.workflow_output_manifests, run_id) |> option.from_result
}

pub fn latest_workflow_repair(
  projection: Projection,
  run_id: String,
) -> Option(WorkflowRepairStatus) {
  dict.get(projection.workflow_repairs, run_id) |> option.from_result
}

fn latest_finished_workspace(
  statuses: List(StepAttemptStatus),
  best: Option(StepAttemptStatus),
) -> Result(CompletedWorkspace, Nil) {
  case statuses {
    [] ->
      case best {
        Some(status) -> completed_workspace_from_status(status)
        None -> Error(Nil)
      }
    [status, ..rest] -> {
      let best = case best {
        None -> Some(status)
        Some(existing) ->
          case attempt_index_of(status) > attempt_index_of(existing) {
            True -> Some(status)
            False -> best
          }
      }
      latest_finished_workspace(rest, best)
    }
  }
}

fn completed_workspace_from_status(
  status: StepAttemptStatus,
) -> Result(CompletedWorkspace, Nil) {
  case status {
    StepAttemptFinishedStatus(
      run_id,
      workflow_id,
      _,
      attempt_index,
      _,
      _,
      _,
      workspace_name,
      workspace_path,
      run_root,
      source_workspace_name,
      source_workspace_path,
      _,
      _,
      _,
    ) ->
      Ok(CompletedWorkspace(
        workflow_id: workflow_id,
        run_id: run_id,
        run_root: run_root,
        workspace_name: workspace_name,
        path: workspace_path,
        source_workspace_name: source_workspace_name,
        source_workspace_path: source_workspace_path,
        attempt_index: attempt_index,
      ))
    _ -> Error(Nil)
  }
}

fn attempt_identity(status: StepAttemptStatus) -> #(String, String, Int) {
  case status {
    StepAttemptPending(run_id, _, step_id, attempt_index, _, _, _, _, _, _) -> #(
      run_id,
      step_id,
      attempt_index,
    )
    StepAttemptRunning(
      run_id: run_id,
      step_id: step_id,
      attempt_index: attempt_index,
      ..,
    ) -> #(run_id, step_id, attempt_index)
    StepAttemptFinishedStatus(
      run_id: run_id,
      step_id: step_id,
      attempt_index: attempt_index,
      ..,
    ) -> #(run_id, step_id, attempt_index)
    StepAttemptInterruptedStatus(
      run_id: run_id,
      step_id: step_id,
      attempt_index: attempt_index,
      ..,
    ) -> #(run_id, step_id, attempt_index)
    StepAttemptSupersededStatus(run_id, _, step_id, attempt_index, _, _, _) -> #(
      run_id,
      step_id,
      attempt_index,
    )
  }
}

fn attempt_index_of(status: StepAttemptStatus) -> Int {
  let #(_, _, attempt_index) = attempt_identity(status)
  attempt_index
}

fn add_one(value: Int) -> Int {
  value + 1
}

fn preserve_or_insert_workflow_task_ref(
  refs: Dict(String, record.TaskRefFields),
  run_id: String,
  fallback: record.TaskRefFields,
) -> Dict(String, record.TaskRefFields) {
  case dict.has_key(refs, run_id) {
    True -> refs
    False -> dict.insert(refs, run_id, fallback)
  }
}

fn workflow_run_root(projection: Projection, run_id: String) -> String {
  case dict.get(projection.workflow_runs, run_id) {
    Ok(WorkflowRunActive(run_root: run_root, ..)) -> run_root
    Ok(WorkflowRunFinished(run_root: run_root, ..)) -> run_root
    Ok(WorkflowRunInterrupted(run_root: run_root, ..)) -> run_root
    Ok(WorkflowRunSuperseded(run_root: run_root, ..)) -> run_root
    Error(_) -> ""
  }
}

pub fn known_issue_ids(projection: Projection) -> List(String) {
  []
  |> append_unique_strings(run_issue_ids(projection.runs))
  |> append_unique_strings(workflow_run_issue_ids(projection.workflow_runs))
  |> append_unique_strings(workflow_task_ref_issue_ids(
    projection.workflow_task_refs,
  ))
  |> append_unique_strings(dict.keys(projection.retries))
  |> append_unique_strings(dict.keys(projection.parked_issues))
  |> append_unique_strings(command_issue_ids(projection.commands))
  |> append_unique_strings(outbox_issue_ids(projection.outbox))
  |> append_unique_strings(dict.keys(projection.issue_counters))
  |> append_unique_strings(dict.keys(projection.known_workspaces))
}

pub fn known_workspace_for_issue(
  projection: Projection,
  issue_id: String,
) -> Result(String, Nil) {
  case dict.get(projection.known_workspaces, issue_id) {
    Ok(workspace) -> Ok(workspace.workspace_path)
    Error(_) -> Error(Nil)
  }
}

pub fn latest_counter(
  projection: Projection,
  issue_id: String,
) -> orchestrator_state.IssueCounter {
  case dict.get(projection.issue_counters, issue_id) {
    Ok(counter) ->
      orchestrator_state.IssueCounter(
        counter.failure_attempts,
        counter.worker_sessions,
      )
    Error(_) -> orchestrator_state.new_issue_counter()
  }
}

pub fn counter_has_source_run(
  projection: Projection,
  issue_id: String,
  run_id: String,
) -> Bool {
  case dict.get(projection.issue_counters, issue_id) {
    Ok(counter) -> list.contains(counter.source_run_ids, run_id)
    Error(_) -> False
  }
}

pub fn workflow_task_ref(
  projection: Projection,
  run_id: String,
) -> Result(record.TaskRefFields, Nil) {
  dict.get(projection.workflow_task_refs, run_id)
}

pub fn command_receipt(
  projection: Projection,
  comment_id: String,
) -> CommandReceiptState {
  dict.get(projection.command_receipts, comment_id)
  |> result.unwrap(CommandReceiptUnseen)
}

pub fn retry_due_at_ms(status: RetryStatus) -> Result(Int, Nil) {
  case status {
    RetryScheduled(_, delay_ms, _, _, scheduled_at_ms) ->
      Ok(scheduled_at_ms + delay_ms)
    RetryCancelled(_, _, _) -> Error(Nil)
  }
}

pub fn pending_outbox_replays(
  projection: Projection,
) -> Result(List(OutboxReplay), PendingOutboxError) {
  let entries =
    projection.outbox
    |> dict.to_list
    |> list.sort(by: compare_outbox_entries_by_time)
  pending_outbox_replays_loop(entries, [])
}

pub fn to_json(projection: Projection) -> json.Json {
  json.object([
    #("schema_version", json.int(record.schema_version)),
    #("kind", json.string("projection_snapshot")),
    #("runs", json.array(dict.to_list(projection.runs), of: run_entry_to_json)),
    #(
      "workflow_runs",
      json.array(
        dict.to_list(projection.workflow_runs),
        of: workflow_run_entry_to_json,
      ),
    ),
    #(
      "workflow_task_refs",
      json.array(
        dict.to_list(projection.workflow_task_refs),
        of: workflow_task_ref_entry_to_json,
      ),
    ),
    #(
      "workflow_input_manifests",
      json.array(
        dict.to_list(projection.workflow_input_manifests),
        of: workflow_contract_manifest_entry_to_json,
      ),
    ),
    #(
      "workflow_output_manifests",
      json.array(
        dict.to_list(projection.workflow_output_manifests),
        of: workflow_contract_manifest_entry_to_json,
      ),
    ),
    #(
      "workflow_repairs",
      json.array(
        dict.to_list(projection.workflow_repairs),
        of: workflow_repair_entry_to_json,
      ),
    ),
    #(
      "step_attempts",
      json.array(
        dict.to_list(projection.step_attempts),
        of: step_attempt_entry_to_json,
      ),
    ),
    #(
      "retries",
      json.array(dict.to_list(projection.retries), of: retry_entry_to_json),
    ),
    #(
      "parked_issues",
      json.array(
        dict.to_list(projection.parked_issues),
        of: parked_entry_to_json,
      ),
    ),
    #(
      "commands",
      json.array(dict.to_list(projection.commands), of: command_entry_to_json),
    ),
    #(
      "command_receipts",
      json.array(
        dict.to_list(projection.command_receipts),
        of: command_receipt_entry_to_json,
      ),
    ),
    #(
      "outbox",
      json.array(dict.to_list(projection.outbox), of: outbox_entry_to_json),
    ),
    #(
      "issue_counters",
      json.array(
        dict.to_list(projection.issue_counters),
        of: issue_counter_entry_to_json,
      ),
    ),
    #(
      "known_workspaces",
      json.array(
        dict.to_list(projection.known_workspaces),
        of: known_workspace_entry_to_json,
      ),
    ),
    #(
      "workstreams",
      json.array(
        dict.to_list(projection.workstreams),
        of: workstream_entry_to_json,
      ),
    ),
    #(
      "scheduled_jobs",
      json.array(
        dict.to_list(projection.scheduled_jobs),
        of: scheduled_job_entry_to_json,
      ),
    ),
  ])
}

pub fn to_string(projection: Projection) -> String {
  projection |> to_json |> json.to_string
}

pub fn decode_string(contents: String) -> Result(Projection, String) {
  case json.parse(contents, snapshot_header_decoder()) {
    Ok(#(version, _)) if version != record.schema_version ->
      Error("unsupported schema version " <> int.to_string(version))
    _ -> decode_current_snapshot(contents)
  }
}

fn decode_current_snapshot(contents: String) -> Result(Projection, String) {
  case json.parse(contents, snapshot_decoder()) {
    Ok(fields) ->
      Ok(Projection(
        runs: fields.runs
          |> list.map(fn(entry) {
            let RunSnapshot(run_id, status) = entry
            #(run_id, status)
          })
          |> dict.from_list,
        workflow_runs: fields.workflow_runs
          |> list.map(fn(entry) {
            let WorkflowRunSnapshot(run_id, status) = entry
            #(run_id, status)
          })
          |> dict.from_list,
        workflow_task_refs: fields.workflow_task_refs
          |> list.map(fn(entry) {
            let WorkflowTaskRefSnapshot(run_id, task_ref) = entry
            #(run_id, task_ref)
          })
          |> dict.from_list,
        workflow_input_manifests: fields.workflow_input_manifests
          |> list.map(fn(entry) {
            let WorkflowContractManifestSnapshot(run_id, manifest) = entry
            #(run_id, manifest)
          })
          |> dict.from_list,
        workflow_output_manifests: fields.workflow_output_manifests
          |> list.map(fn(entry) {
            let WorkflowContractManifestSnapshot(run_id, manifest) = entry
            #(run_id, manifest)
          })
          |> dict.from_list,
        workflow_repairs: fields.workflow_repairs
          |> list.map(fn(entry) {
            let WorkflowRepairSnapshot(run_id, repair) = entry
            #(run_id, repair)
          })
          |> dict.from_list,
        step_attempts: fields.step_attempts
          |> list.map(fn(entry) {
            let StepAttemptSnapshot(key, status) = entry
            #(key, status)
          })
          |> dict.from_list,
        retries: fields.retries
          |> list.map(fn(entry) {
            let RetrySnapshot(issue_id, status) = entry
            #(issue_id, status)
          })
          |> dict.from_list,
        parked_issues: fields.parked_issues
          |> list.map(fn(entry) {
            let ParkedSnapshot(issue_id, parked) = entry
            #(issue_id, parked)
          })
          |> dict.from_list,
        commands: fields.commands
          |> list.map(fn(entry) {
            let CommandSnapshot(comment_id, status) = entry
            #(comment_id, status)
          })
          |> dict.from_list,
        command_receipts: fields.command_receipts
          |> list.map(fn(entry) {
            let CommandReceiptSnapshot(comment_id, receipt) = entry
            #(comment_id, receipt)
          })
          |> dict.from_list,
        outbox: fields.outbox
          |> list.map(fn(entry) {
            let OutboxSnapshot(outbox_id, status) = entry
            #(outbox_id, status)
          })
          |> dict.from_list,
        issue_counters: fields.issue_counters
          |> list.map(fn(entry) {
            let IssueCounterSnapshot(issue_id, status) = entry
            #(issue_id, status)
          })
          |> dict.from_list,
        known_workspaces: fields.known_workspaces
          |> list.map(fn(entry) {
            let KnownWorkspaceSnapshot(issue_id, workspace) = entry
            #(issue_id, workspace)
          })
          |> dict.from_list,
        workstreams: fields.workstreams
          |> list.map(fn(entry) {
            let WorkstreamSnapshot(workstream_id, status) = entry
            #(workstream_id, status)
          })
          |> dict.from_list,
        scheduled_jobs: fields.scheduled_jobs
          |> list.map(fn(entry) {
            let ScheduledJobSnapshot(job_id, status) = entry
            #(job_id, status)
          })
          |> dict.from_list,
      ))
    Error(_) -> Error("malformed projection snapshot")
  }
}

fn snapshot_header_decoder() -> decode.Decoder(#(Int, String)) {
  use schema_version <- decode.field("schema_version", decode.int)
  use kind <- decode.field("kind", decode.string)
  decode.success(#(schema_version, kind))
}

fn run_entry_to_json(entry: #(String, RunStatus)) -> json.Json {
  let #(run_id, status) = entry
  case status {
    RunRunning(issue_id, issue_identifier, workspace_path, started_at_ms) ->
      json.object([
        #("run_id", json.string(run_id)),
        #("status", json.string("running")),
        #("issue_id", json.string(issue_id)),
        #("issue_identifier", json.string(issue_identifier)),
        #("workspace_path", json.string(workspace_path)),
        #("started_at_ms", json.int(started_at_ms)),
      ])
    RunFinished(issue_id, classification, token_total, turns, finished_at_ms) ->
      json.object([
        #("run_id", json.string(run_id)),
        #("status", json.string("finished")),
        #("issue_id", json.string(issue_id)),
        #("classification", json.string(classification)),
        #("token_total", json.int(token_total)),
        #("turns", json.int(turns)),
        #("finished_at_ms", json.int(finished_at_ms)),
      ])
    RunInterrupted(issue_id, reason, interrupted_at_ms) ->
      json.object([
        #("run_id", json.string(run_id)),
        #("status", json.string("interrupted")),
        #("issue_id", json.string(issue_id)),
        #("reason", json.string(reason)),
        #("interrupted_at_ms", json.int(interrupted_at_ms)),
      ])
  }
}

fn workflow_run_entry_to_json(
  entry: #(String, WorkflowRunStatus),
) -> json.Json {
  let #(run_id, status) = entry
  case status {
    WorkflowRunActive(
      workflow_id,
      workflow_fingerprint,
      issue_id,
      issue_identifier,
      issue_fingerprint,
      observed_updated_at_ms,
      run_root,
      started_at_ms,
    ) ->
      json.object([
        #("run_id", json.string(run_id)),
        #("status", json.string("active")),
        #("workflow_id", json.string(workflow_id)),
        #("workflow_fingerprint", json.string(workflow_fingerprint)),
        #("issue_id", json.string(issue_id)),
        #("issue_identifier", json.string(issue_identifier)),
        #("issue_fingerprint", json.string(issue_fingerprint)),
        #("observed_updated_at_ms", json.int(observed_updated_at_ms)),
        #("run_root", json.string(run_root)),
        #("started_at_ms", json.int(started_at_ms)),
      ])
    WorkflowRunFinished(
      workflow_id,
      issue_id,
      outcome,
      token_total,
      turns,
      finished_at_ms,
      run_root,
    ) ->
      json.object([
        #("run_id", json.string(run_id)),
        #("status", json.string("finished")),
        #("workflow_id", json.string(workflow_id)),
        #("issue_id", json.string(issue_id)),
        #("outcome", json.string(outcome)),
        #("token_total", json.int(token_total)),
        #("turns", json.int(turns)),
        #("finished_at_ms", json.int(finished_at_ms)),
        #("run_root", json.string(run_root)),
      ])
    WorkflowRunInterrupted(
      workflow_id,
      issue_id,
      reason,
      interrupted_at_ms,
      run_root,
    ) ->
      json.object([
        #("run_id", json.string(run_id)),
        #("status", json.string("interrupted")),
        #("workflow_id", json.string(workflow_id)),
        #("issue_id", json.string(issue_id)),
        #("reason", json.string(reason)),
        #("interrupted_at_ms", json.int(interrupted_at_ms)),
        #("run_root", json.string(run_root)),
      ])
    WorkflowRunSuperseded(
      workflow_id,
      issue_id,
      superseded_by_run_id,
      reason,
      superseded_at_ms,
      run_root,
    ) ->
      json.object([
        #("run_id", json.string(run_id)),
        #("status", json.string("superseded")),
        #("workflow_id", json.string(workflow_id)),
        #("issue_id", json.string(issue_id)),
        #("superseded_by_run_id", json.string(superseded_by_run_id)),
        #("reason", json.string(reason)),
        #("superseded_at_ms", json.int(superseded_at_ms)),
        #("run_root", json.string(run_root)),
      ])
  }
}

fn workflow_task_ref_entry_to_json(
  entry: #(String, record.TaskRefFields),
) -> json.Json {
  let #(run_id, task_ref) = entry
  let record.TaskRefFields(
    task_backend_kind,
    task_remote_id,
    task_key,
    task_url,
  ) = task_ref
  json.object([
    #("run_id", json.string(run_id)),
    #("task_backend_kind", json.string(task_backend_kind)),
    #("task_remote_id", json.string(task_remote_id)),
    #("task_key", option_string_to_json(task_key)),
    #("task_url", option_string_to_json(task_url)),
  ])
}

fn workflow_repair_entry_to_json(
  entry: #(String, WorkflowRepairStatus),
) -> json.Json {
  let #(run_id, repair) = entry
  json.object([
    #("run_id", json.string(run_id)),
    #("workflow_id", json.string(repair.workflow_id)),
    #("issue_id", json.string(repair.issue_id)),
    #("issue_identifier", json.string(repair.issue_identifier)),
    #("requested_target", json.string(repair.requested_target)),
    #("requested_step_id", option_string_to_json(repair.requested_step_id)),
    #("selected_step_id", json.string(repair.selected_step_id)),
    #("failed_attempt_index", json.int(repair.failed_attempt_index)),
    #("next_attempt_index", json.int(repair.next_attempt_index)),
    #("reason", json.string(repair.reason)),
    #("requested_at_ms", json.int(repair.requested_at_ms)),
    #("generation", json.int(repair.generation)),
  ])
}

fn workflow_contract_manifest_entry_to_json(
  entry: #(String, WorkflowContractManifestRef),
) -> json.Json {
  let #(run_id, manifest) = entry
  json.object([
    #("run_id", json.string(run_id)),
    #("workflow_id", json.string(manifest.workflow_id)),
    #("workflow_fingerprint", json.string(manifest.workflow_fingerprint)),
    #("artifact_ref", json.string(manifest.artifact_ref)),
    #("artifact_sha256", json.string(manifest.artifact_sha256)),
    #("artifact_bytes", json.int(manifest.artifact_bytes)),
    #("recorded_at_ms", json.int(manifest.recorded_at_ms)),
  ])
}

fn step_attempt_entry_to_json(
  entry: #(String, StepAttemptStatus),
) -> json.Json {
  let #(key, status) = entry
  case status {
    StepAttemptPending(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      workspace_name,
      workspace_path,
      run_root,
      source_workspace_name,
      source_workspace_path,
      prepared_at_ms,
    ) ->
      json.object([
        #("key", json.string(key)),
        #("status", json.string("pending")),
        #("run_id", json.string(run_id)),
        #("workflow_id", json.string(workflow_id)),
        #("step_id", json.string(step_id)),
        #("attempt_index", json.int(attempt_index)),
        #("workspace_name", json.string(workspace_name)),
        #("workspace_path", json.string(workspace_path)),
        #("run_root", json.string(run_root)),
        #("source_workspace_name", option_string_to_json(source_workspace_name)),
        #("source_workspace_path", option_string_to_json(source_workspace_path)),
        #("prepared_at_ms", json.int(prepared_at_ms)),
      ])
    StepAttemptRunning(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      workspace_name,
      workspace_path,
      run_root,
      source_workspace_name,
      source_workspace_path,
      operator_session_id,
      external_session_ref,
      continuation_capable,
      pi_session_id,
      pi_session_file,
      pi_session_fact_count,
      started_at_ms,
    ) ->
      json.object([
        #("key", json.string(key)),
        #("status", json.string("running")),
        #("run_id", json.string(run_id)),
        #("workflow_id", json.string(workflow_id)),
        #("step_id", json.string(step_id)),
        #("attempt_index", json.int(attempt_index)),
        #("workspace_name", json.string(workspace_name)),
        #("workspace_path", json.string(workspace_path)),
        #("run_root", json.string(run_root)),
        #("source_workspace_name", option_string_to_json(source_workspace_name)),
        #("source_workspace_path", option_string_to_json(source_workspace_path)),
        #("operator_session_id", json.string(operator_session_id)),
        #("external_session_ref", option_string_to_json(external_session_ref)),
        #("continuation_capable", json.bool(continuation_capable)),
        #("pi_session_id", option_string_to_json(pi_session_id)),
        #("pi_session_file", option_string_to_json(pi_session_file)),
        #("pi_session_fact_count", json.int(pi_session_fact_count)),
        #("started_at_ms", json.int(started_at_ms)),
      ])
    StepAttemptFinishedStatus(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      outcome,
      artifact_ref,
      artifact_sha256,
      workspace_name,
      workspace_path,
      run_root,
      source_workspace_name,
      source_workspace_path,
      token_total,
      turns,
      finished_at_ms,
    ) ->
      json.object([
        #("key", json.string(key)),
        #("status", json.string("finished")),
        #("run_id", json.string(run_id)),
        #("workflow_id", json.string(workflow_id)),
        #("step_id", json.string(step_id)),
        #("attempt_index", json.int(attempt_index)),
        #("outcome", json.string(outcome)),
        #("artifact_ref", json.string(artifact_ref)),
        #("artifact_sha256", json.string(artifact_sha256)),
        #("workspace_name", json.string(workspace_name)),
        #("workspace_path", json.string(workspace_path)),
        #("run_root", json.string(run_root)),
        #("source_workspace_name", option_string_to_json(source_workspace_name)),
        #("source_workspace_path", option_string_to_json(source_workspace_path)),
        #("token_total", json.int(token_total)),
        #("turns", json.int(turns)),
        #("finished_at_ms", json.int(finished_at_ms)),
      ])
    StepAttemptInterruptedStatus(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      workspace_name,
      workspace_path,
      run_root,
      reason,
      continuation_capable,
      pi_session_id,
      pi_session_file,
      pi_session_fact_count,
      interrupted_at_ms,
    ) ->
      json.object([
        #("key", json.string(key)),
        #("status", json.string("interrupted")),
        #("run_id", json.string(run_id)),
        #("workflow_id", json.string(workflow_id)),
        #("step_id", json.string(step_id)),
        #("attempt_index", json.int(attempt_index)),
        #("workspace_name", json.string(workspace_name)),
        #("workspace_path", json.string(workspace_path)),
        #("run_root", json.string(run_root)),
        #("reason", json.string(reason)),
        #("continuation_capable", json.bool(continuation_capable)),
        #("pi_session_id", option_string_to_json(pi_session_id)),
        #("pi_session_file", option_string_to_json(pi_session_file)),
        #("pi_session_fact_count", json.int(pi_session_fact_count)),
        #("interrupted_at_ms", json.int(interrupted_at_ms)),
      ])
    StepAttemptSupersededStatus(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      superseded_by_attempt_index,
      reason,
      superseded_at_ms,
    ) ->
      json.object([
        #("key", json.string(key)),
        #("status", json.string("superseded")),
        #("run_id", json.string(run_id)),
        #("workflow_id", json.string(workflow_id)),
        #("step_id", json.string(step_id)),
        #("attempt_index", json.int(attempt_index)),
        #("superseded_by_attempt_index", json.int(superseded_by_attempt_index)),
        #("reason", json.string(reason)),
        #("superseded_at_ms", json.int(superseded_at_ms)),
      ])
  }
}

fn option_string_to_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn retry_entry_to_json(entry: #(String, RetryStatus)) -> json.Json {
  let #(issue_id, status) = entry
  case status {
    RetryScheduled(
      issue_identifier,
      delay_ms,
      generation,
      reason,
      scheduled_at_ms,
    ) ->
      json.object([
        #("issue_id", json.string(issue_id)),
        #("status", json.string("scheduled")),
        #("issue_identifier", json.string(issue_identifier)),
        #("delay_ms", json.int(delay_ms)),
        #("generation", json.int(generation)),
        #("reason", json.string(reason)),
        #("scheduled_at_ms", json.int(scheduled_at_ms)),
      ])
    RetryCancelled(generation, reason, cancelled_at_ms) ->
      json.object([
        #("issue_id", json.string(issue_id)),
        #("status", json.string("cancelled")),
        #("generation", json.int(generation)),
        #("reason", json.string(reason)),
        #("cancelled_at_ms", json.int(cancelled_at_ms)),
      ])
  }
}

fn parked_entry_to_json(entry: #(String, ParkedIssue)) -> json.Json {
  let #(issue_id, parked) = entry
  let ParkedIssue(
    issue_identifier,
    reason,
    observed_updated_at_ms,
    parked_at_ms,
    release_policy,
    issue_fingerprint,
  ) = parked
  json.object([
    #("issue_id", json.string(issue_id)),
    #("issue_identifier", json.string(issue_identifier)),
    #("reason", json.string(reason)),
    #("observed_updated_at_ms", json.int(observed_updated_at_ms)),
    #("parked_at_ms", json.int(parked_at_ms)),
    #("release_policy", json.string(release_policy)),
    #("issue_fingerprint", json.string(issue_fingerprint)),
  ])
}

fn command_entry_to_json(entry: #(String, CommandStatus)) -> json.Json {
  let #(comment_id, status) = entry
  case status {
    CommandSeen(issue_id, author_id, command_name, excerpt, seen_at_ms) ->
      json.object([
        #("comment_id", json.string(comment_id)),
        #("status", json.string("seen")),
        #("issue_id", json.string(issue_id)),
        #("author_id", json.string(author_id)),
        #("command_name", json.string(command_name)),
        #("excerpt", json.string(excerpt)),
        #("seen_at_ms", json.int(seen_at_ms)),
      ])
    CommandStarted(issue_id, command_name, started_at_ms) ->
      json.object([
        #("comment_id", json.string(comment_id)),
        #("status", json.string("started")),
        #("issue_id", json.string(issue_id)),
        #("command_name", json.string(command_name)),
        #("started_at_ms", json.int(started_at_ms)),
      ])
    CommandCompleted(issue_id, result_status, message_excerpt, completed_at_ms) ->
      json.object([
        #("comment_id", json.string(comment_id)),
        #("status", json.string("completed")),
        #("issue_id", json.string(issue_id)),
        #("result_status", json.string(result_status)),
        #("message_excerpt", json.string(message_excerpt)),
        #("completed_at_ms", json.int(completed_at_ms)),
      ])
    CommandAcked(issue_id, acked_at_ms) ->
      json.object([
        #("comment_id", json.string(comment_id)),
        #("status", json.string("acked")),
        #("issue_id", json.string(issue_id)),
        #("acked_at_ms", json.int(acked_at_ms)),
      ])
  }
}

fn option_int_to_json(value: Option(Int)) -> json.Json {
  case value {
    Some(value) -> json.int(value)
    None -> json.null()
  }
}

fn command_receipt_entry_to_json(
  entry: #(String, CommandReceiptState),
) -> json.Json {
  let #(comment_id, receipt) = entry
  case receipt {
    CommandReceiptUnseen ->
      json.object([
        #("comment_id", json.string(comment_id)),
        #("status", json.string("unseen")),
      ])
    CommandReceiptSeen(issue_id, author_id, command_name, excerpt, seen_at_ms) ->
      json.object([
        #("comment_id", json.string(comment_id)),
        #("status", json.string("seen")),
        #("issue_id", json.string(issue_id)),
        #("author_id", json.string(author_id)),
        #("command_name", json.string(command_name)),
        #("excerpt", json.string(excerpt)),
        #("seen_at_ms", json.int(seen_at_ms)),
      ])
    CommandReceiptStarted(
      issue_id,
      author_id,
      command_name,
      excerpt,
      seen_at_ms,
      started_at_ms,
    ) ->
      json.object([
        #("comment_id", json.string(comment_id)),
        #("status", json.string("started")),
        #("issue_id", json.string(issue_id)),
        #("author_id", json.string(author_id)),
        #("command_name", json.string(command_name)),
        #("excerpt", json.string(excerpt)),
        #("seen_at_ms", json.int(seen_at_ms)),
        #("started_at_ms", json.int(started_at_ms)),
      ])
    CommandReceiptCompleted(
      issue_id,
      author_id,
      command_name,
      excerpt,
      result_status,
      message_excerpt,
      seen_at_ms,
      started_at_ms,
      completed_at_ms,
      acked_at_ms,
    ) ->
      json.object([
        #("comment_id", json.string(comment_id)),
        #("status", json.string("completed")),
        #("issue_id", json.string(issue_id)),
        #("author_id", json.string(author_id)),
        #("command_name", json.string(command_name)),
        #("excerpt", json.string(excerpt)),
        #("result_status", json.string(result_status)),
        #("message_excerpt", json.string(message_excerpt)),
        #("seen_at_ms", json.int(seen_at_ms)),
        #("started_at_ms", json.int(started_at_ms)),
        #("completed_at_ms", json.int(completed_at_ms)),
        #("acked_at_ms", option_int_to_json(acked_at_ms)),
      ])
    CommandReceiptAcked(issue_id, acked_at_ms) ->
      json.object([
        #("comment_id", json.string(comment_id)),
        #("status", json.string("acked")),
        #("issue_id", json.string(issue_id)),
        #("acked_at_ms", json.int(acked_at_ms)),
      ])
  }
}

fn outbox_entry_to_json(entry: #(String, OutboxStatus)) -> json.Json {
  let #(outbox_id, status) = entry
  case status {
    OutboxPending(issue_id, outbox_kind, dedupe_key, pending_at_ms) ->
      json.object([
        #("outbox_id", json.string(outbox_id)),
        #("status", json.string("pending")),
        #("issue_id", json.string(issue_id)),
        #("outbox_kind", json.string(outbox_kind)),
        #("dedupe_key", json.string(dedupe_key)),
        #("pending_at_ms", json.int(pending_at_ms)),
      ])
    OutboxPendingV2(
      issue_id,
      outbox_kind,
      dedupe_key,
      payload_json,
      pending_at_ms,
    ) ->
      json.object([
        #("outbox_id", json.string(outbox_id)),
        #("status", json.string("pending_v2")),
        #("issue_id", json.string(issue_id)),
        #("outbox_kind", json.string(outbox_kind)),
        #("dedupe_key", json.string(dedupe_key)),
        #("payload_json", json.string(payload_json)),
        #("pending_at_ms", json.int(pending_at_ms)),
      ])
    OutboxCompleted(issue_id, outbox_kind, completed_at_ms) ->
      json.object([
        #("outbox_id", json.string(outbox_id)),
        #("status", json.string("completed")),
        #("issue_id", json.string(issue_id)),
        #("outbox_kind", json.string(outbox_kind)),
        #("completed_at_ms", json.int(completed_at_ms)),
      ])
    OutboxFailed(issue_id, outbox_kind, error_code, failed_at_ms) ->
      json.object([
        #("outbox_id", json.string(outbox_id)),
        #("status", json.string("failed")),
        #("issue_id", json.string(issue_id)),
        #("outbox_kind", json.string(outbox_kind)),
        #("error_code", json.string(error_code)),
        #("failed_at_ms", json.int(failed_at_ms)),
      ])
  }
}

fn issue_counter_entry_to_json(
  entry: #(String, IssueCounterStatus),
) -> json.Json {
  let #(issue_id, status) = entry
  json.object([
    #("issue_id", json.string(issue_id)),
    #("issue_identifier", json.string(status.issue_identifier)),
    #("failure_attempts", json.int(status.failure_attempts)),
    #("worker_sessions", json.int(status.worker_sessions)),
    #("observed_updated_at_ms", json.int(status.observed_updated_at_ms)),
    #("source_run_ids", json.array(status.source_run_ids, of: json.string)),
    #("updated_at_ms", json.int(status.updated_at_ms)),
  ])
}

fn known_workspace_entry_to_json(
  entry: #(String, KnownWorkspace),
) -> json.Json {
  let #(issue_id, workspace) = entry
  json.object([
    #("issue_id", json.string(issue_id)),
    #("issue_identifier", json.string(workspace.issue_identifier)),
    #("workspace_path", json.string(workspace.workspace_path)),
    #("recorded_at_ms", json.int(workspace.recorded_at_ms)),
  ])
}

fn workstream_entry_to_json(entry: #(String, WorkstreamStatus)) -> json.Json {
  let #(workstream_id, status) = entry
  json.object([
    #("workstream_id", json.string(workstream_id)),
    #(
      "task_backend_kind",
      option_task_ref_string(status.task_ref, fn(task_ref) {
        task_ref.task_backend_kind
      }),
    ),
    #(
      "task_remote_id",
      option_task_ref_string(status.task_ref, fn(task_ref) {
        task_ref.task_remote_id
      }),
    ),
    #(
      "task_key",
      option_task_ref_option_string(status.task_ref, fn(task_ref) {
        task_ref.task_key
      }),
    ),
    #(
      "task_url",
      option_task_ref_option_string(status.task_ref, fn(task_ref) {
        task_ref.task_url
      }),
    ),
    #("created_at_ms", option_int_to_json(status.created_at_ms)),
    #(
      "latest_assignment",
      option_workstream_assignment_to_json(status.latest_assignment),
    ),
    #(
      "artifacts",
      json.array(
        dict.to_list(status.artifacts),
        of: workstream_artifact_entry_to_json,
      ),
    ),
    #(
      "handoffs",
      json.array(
        dict.to_list(status.handoffs),
        of: workstream_handoff_entry_to_json,
      ),
    ),
    #(
      "queued_phase_runs",
      json.array(
        dict.to_list(status.queued_phase_runs),
        of: workstream_phase_run_entry_to_json,
      ),
    ),
  ])
}

fn option_task_ref_string(
  task_ref: Option(record.TaskRefFields),
  select: fn(record.TaskRefFields) -> String,
) -> json.Json {
  case task_ref {
    Some(task_ref) -> json.string(select(task_ref))
    None -> json.null()
  }
}

fn option_task_ref_option_string(
  task_ref: Option(record.TaskRefFields),
  select: fn(record.TaskRefFields) -> Option(String),
) -> json.Json {
  case task_ref {
    Some(task_ref) -> option_string_to_json(select(task_ref))
    None -> json.null()
  }
}

fn option_workstream_assignment_to_json(
  value: Option(WorkstreamAssignment),
) -> json.Json {
  case value {
    None -> json.null()
    Some(assignment) ->
      json.object([
        #("assignment_id", json.string(assignment.assignment_id)),
        #("workflow_id", json.string(assignment.workflow_id)),
        #("playbook_id", option_string_to_json(assignment.playbook_id)),
        #("reason", json.string(assignment.reason)),
        #("idempotency_key", json.string(assignment.idempotency_key)),
        #("assigned_at_ms", json.int(assignment.assigned_at_ms)),
      ])
  }
}

fn workstream_artifact_entry_to_json(
  entry: #(String, WorkstreamArtifactSnapshot),
) -> json.Json {
  let #(_, artifact) = entry
  json.object([
    #("snapshot_ref", json.string(artifact.snapshot_ref)),
    #("artifact_id", json.string(artifact.artifact_id)),
    #("artifact_type", json.string(artifact.artifact_type)),
    #("snapshot_sha256", json.string(artifact.snapshot_sha256)),
    #("snapshot_bytes", json.int(artifact.snapshot_bytes)),
    #("original_path", json.string(artifact.original_path)),
    #("contract_type", json.string(artifact.contract_type)),
    #("media_type", json.string(artifact.media_type)),
    #("producer_workflow_id", json.string(artifact.producer_workflow_id)),
    #("producer_run_id", json.string(artifact.producer_run_id)),
    #("producer_step_id", json.string(artifact.producer_step_id)),
    #("idempotency_key", json.string(artifact.idempotency_key)),
    #("recorded_at_ms", json.int(artifact.recorded_at_ms)),
  ])
}

fn workstream_handoff_entry_to_json(
  entry: #(String, WorkstreamHandoffSnapshot),
) -> json.Json {
  let #(_, handoff) = entry
  json.object([
    #("handoff_ref", json.string(handoff.handoff_ref)),
    #("handoff_id", json.string(handoff.handoff_id)),
    #("handoff_sha256", json.string(handoff.handoff_sha256)),
    #("handoff_bytes", json.int(handoff.handoff_bytes)),
    #("source_workflow_id", json.string(handoff.source_workflow_id)),
    #("source_run_id", json.string(handoff.source_run_id)),
    #("idempotency_key", json.string(handoff.idempotency_key)),
    #("recorded_at_ms", json.int(handoff.recorded_at_ms)),
  ])
}

fn workstream_phase_run_entry_to_json(
  entry: #(String, WorkstreamPhaseRun),
) -> json.Json {
  let #(_, phase_run) = entry
  json.object([
    #("phase_run_id", json.string(phase_run.phase_run_id)),
    #("action_id", json.string(phase_run.action_id)),
    #("workflow_id", json.string(phase_run.workflow_id)),
    #("input_bundle_ref", json.string(phase_run.input_bundle_ref)),
    #("input_bundle_sha256", json.string(phase_run.input_bundle_sha256)),
    #("input_bundle_bytes", json.int(phase_run.input_bundle_bytes)),
    #("idempotency_key", json.string(phase_run.idempotency_key)),
    #("queued_at_ms", json.int(phase_run.queued_at_ms)),
  ])
}

fn scheduled_job_entry_to_json(
  entry: #(String, ScheduledJobStatus),
) -> json.Json {
  let #(job_id, status) = entry
  json.object([
    #("job_id", json.string(job_id)),
    #("workflow_id", json.string(status.workflow_id)),
    #("state", json.string(scheduled_state_to_string(status.state))),
    #("current_run", option_scheduled_run_to_json(status.current_run)),
    #("last_due_at_ms", option_int_to_json(status.last_due_at_ms)),
    #("last_success_at_ms", option_int_to_json(status.last_success_at_ms)),
    #("last_success_run_id", option_string_to_json(status.last_success_run_id)),
    #("last_failure_at_ms", option_int_to_json(status.last_failure_at_ms)),
    #("last_failure_run_id", option_string_to_json(status.last_failure_run_id)),
    #("last_failure_reason", option_string_to_json(status.last_failure_reason)),
    #("retry_count", json.int(status.retry_count)),
    #("skipped_overlap_count", json.int(status.skipped_overlap_count)),
    #("skipped_catch_up_count", json.int(status.skipped_catch_up_count)),
    #("skipped_paused_count", json.int(status.skipped_paused_count)),
    #("skipped_capacity_count", json.int(status.skipped_capacity_count)),
    #("failure_issue_id", option_string_to_json(status.failure_issue_id)),
    #("failure_dedupe_key", option_string_to_json(status.failure_dedupe_key)),
    #("report_retry", option_report_retry_to_json(status.report_retry)),
    #("recent_run_ids", json.array(status.recent_run_ids, of: json.string)),
  ])
}

fn option_scheduled_run_to_json(
  value: Option(ScheduledRunSummary),
) -> json.Json {
  case value {
    None -> json.null()
    Some(run) ->
      json.object([
        #("run_id", json.string(run.run_id)),
        #("due_at_ms", json.int(run.due_at_ms)),
        #("trigger", json.string(run.trigger)),
        #("attempt", json.int(run.attempt)),
        #("status", json.string(run.status)),
        #("reason", option_string_to_json(run.reason)),
        #("session_id", option_string_to_json(run.session_id)),
        #("run_root", option_string_to_json(run.run_root)),
      ])
  }
}

fn option_report_retry_to_json(
  value: Option(ScheduledReportRetry),
) -> json.Json {
  case value {
    None -> json.null()
    Some(retry) ->
      json.object([
        #("run_id", json.string(retry.run_id)),
        #("attempt", json.int(retry.attempt)),
        #("dedupe_key", json.string(retry.dedupe_key)),
        #("error_code", json.string(retry.error_code)),
        #("error_message", json.string(retry.error_message)),
        #("next_retry_at_ms", json.int(retry.next_retry_at_ms)),
        #("generation", json.int(retry.generation)),
      ])
  }
}

fn scheduled_state_to_string(state: ScheduledRunState) -> String {
  case state {
    ScheduledIdle -> "idle"
    ScheduledDuePending -> "due_pending"
    ScheduledPaused -> "paused"
    ScheduledWaitingForGlobalSlot -> "waiting_for_global_slot"
    ScheduledActive -> "active"
    ScheduledRetryWaiting -> "retry_waiting"
    ScheduledReportRetryWaiting -> "report_retry_waiting"
    ScheduledTerminalSuccess -> "terminal_success"
    ScheduledTerminalFailure -> "terminal_failure"
  }
}

fn scheduled_state_from_string(value: String) -> ScheduledRunState {
  case value {
    "due_pending" -> ScheduledDuePending
    "paused" -> ScheduledPaused
    "waiting_for_global_slot" -> ScheduledWaitingForGlobalSlot
    "active" -> ScheduledActive
    "retry_waiting" -> ScheduledRetryWaiting
    "report_retry_waiting" -> ScheduledReportRetryWaiting
    "terminal_success" -> ScheduledTerminalSuccess
    "terminal_failure" -> ScheduledTerminalFailure
    _ -> ScheduledIdle
  }
}

fn snapshot_decoder() -> decode.Decoder(SnapshotFields) {
  use schema_version <- decode.field("schema_version", decode.int)
  use kind <- decode.field("kind", decode.string)
  use runs <- decode.field("runs", decode.list(of: run_snapshot_decoder()))
  use workflow_runs <- decode.optional_field(
    "workflow_runs",
    [],
    decode.list(of: workflow_run_snapshot_decoder()),
  )
  use workflow_task_refs <- decode.optional_field(
    "workflow_task_refs",
    [],
    decode.list(of: workflow_task_ref_snapshot_decoder()),
  )
  use workflow_input_manifests <- decode.optional_field(
    "workflow_input_manifests",
    [],
    decode.list(of: workflow_contract_manifest_snapshot_decoder()),
  )
  use workflow_output_manifests <- decode.optional_field(
    "workflow_output_manifests",
    [],
    decode.list(of: workflow_contract_manifest_snapshot_decoder()),
  )
  use workflow_repairs <- decode.optional_field(
    "workflow_repairs",
    [],
    decode.list(of: workflow_repair_snapshot_decoder()),
  )
  use step_attempts <- decode.optional_field(
    "step_attempts",
    [],
    decode.list(of: step_attempt_snapshot_decoder()),
  )
  use retries <- decode.field(
    "retries",
    decode.list(of: retry_snapshot_decoder()),
  )
  use parked_issues <- decode.field(
    "parked_issues",
    decode.list(of: parked_snapshot_decoder()),
  )
  use commands <- decode.field(
    "commands",
    decode.list(of: command_snapshot_decoder()),
  )
  use command_receipts <- decode.optional_field(
    "command_receipts",
    [],
    decode.list(of: command_receipt_snapshot_decoder()),
  )
  use outbox <- decode.field(
    "outbox",
    decode.list(of: outbox_snapshot_decoder()),
  )
  use issue_counters <- decode.optional_field(
    "issue_counters",
    [],
    decode.list(of: issue_counter_snapshot_decoder()),
  )
  use known_workspaces <- decode.optional_field(
    "known_workspaces",
    [],
    decode.list(of: known_workspace_snapshot_decoder()),
  )
  use workstreams <- decode.optional_field(
    "workstreams",
    [],
    decode.list(of: workstream_snapshot_decoder()),
  )
  use scheduled_jobs <- decode.optional_field(
    "scheduled_jobs",
    [],
    decode.list(of: scheduled_job_snapshot_decoder()),
  )
  case
    schema_version == record.schema_version && kind == "projection_snapshot"
  {
    True ->
      decode.success(SnapshotFields(
        runs,
        workflow_runs,
        workflow_task_refs,
        workflow_input_manifests,
        workflow_output_manifests,
        workflow_repairs,
        step_attempts,
        retries,
        parked_issues,
        commands,
        command_receipts,
        outbox,
        issue_counters,
        known_workspaces,
        workstreams,
        scheduled_jobs,
      ))
    False ->
      decode.failure(
        SnapshotFields(
          [],
          [],
          [],
          [],
          [],
          [],
          [],
          [],
          [],
          [],
          [],
          [],
          [],
          [],
          [],
          [],
        ),
        expected: "SnapshotFields",
      )
  }
}

fn run_snapshot_decoder() -> decode.Decoder(RunSnapshot) {
  use run_id <- decode.field("run_id", decode.string)
  use status <- decode.field("status", decode.string)
  case status {
    "running" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use issue_identifier <- decode.field("issue_identifier", decode.string)
      use workspace_path <- decode.field("workspace_path", decode.string)
      use started_at_ms <- decode.field("started_at_ms", decode.int)
      decode.success(RunSnapshot(
        run_id,
        RunRunning(issue_id, issue_identifier, workspace_path, started_at_ms),
      ))
    }
    "finished" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use classification <- decode.field("classification", decode.string)
      use token_total <- decode.field("token_total", decode.int)
      use turns <- decode.field("turns", decode.int)
      use finished_at_ms <- decode.field("finished_at_ms", decode.int)
      decode.success(RunSnapshot(
        run_id,
        RunFinished(
          issue_id,
          classification,
          token_total,
          turns,
          finished_at_ms,
        ),
      ))
    }
    "interrupted" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use reason <- decode.field("reason", decode.string)
      use interrupted_at_ms <- decode.field("interrupted_at_ms", decode.int)
      decode.success(RunSnapshot(
        run_id,
        RunInterrupted(issue_id, reason, interrupted_at_ms),
      ))
    }
    _ ->
      decode.failure(
        RunSnapshot("", RunInterrupted("", "", 0)),
        expected: "RunSnapshot",
      )
  }
}

fn workflow_run_snapshot_decoder() -> decode.Decoder(WorkflowRunSnapshot) {
  use run_id <- decode.field("run_id", decode.string)
  use status <- decode.field("status", decode.string)
  use workflow_id <- decode.field("workflow_id", decode.string)
  use issue_id <- decode.field("issue_id", decode.string)
  use run_root <- decode.optional_field("run_root", "", decode.string)
  case status {
    "active" -> {
      use workflow_fingerprint <- decode.field(
        "workflow_fingerprint",
        decode.string,
      )
      use issue_identifier <- decode.field("issue_identifier", decode.string)
      use issue_fingerprint <- decode.field("issue_fingerprint", decode.string)
      use observed_updated_at_ms <- decode.field(
        "observed_updated_at_ms",
        decode.int,
      )
      use started_at_ms <- decode.field("started_at_ms", decode.int)
      decode.success(WorkflowRunSnapshot(
        run_id,
        WorkflowRunActive(
          workflow_id,
          workflow_fingerprint,
          issue_id,
          issue_identifier,
          issue_fingerprint,
          observed_updated_at_ms,
          run_root,
          started_at_ms,
        ),
      ))
    }
    "finished" -> {
      use outcome <- decode.field("outcome", decode.string)
      use token_total <- decode.field("token_total", decode.int)
      use turns <- decode.field("turns", decode.int)
      use finished_at_ms <- decode.field("finished_at_ms", decode.int)
      decode.success(WorkflowRunSnapshot(
        run_id,
        WorkflowRunFinished(
          workflow_id,
          issue_id,
          outcome,
          token_total,
          turns,
          finished_at_ms,
          run_root,
        ),
      ))
    }
    "interrupted" -> {
      use reason <- decode.field("reason", decode.string)
      use interrupted_at_ms <- decode.field("interrupted_at_ms", decode.int)
      decode.success(WorkflowRunSnapshot(
        run_id,
        WorkflowRunInterrupted(
          workflow_id,
          issue_id,
          reason,
          interrupted_at_ms,
          run_root,
        ),
      ))
    }
    "superseded" -> {
      use superseded_by_run_id <- decode.field(
        "superseded_by_run_id",
        decode.string,
      )
      use reason <- decode.field("reason", decode.string)
      use superseded_at_ms <- decode.field("superseded_at_ms", decode.int)
      decode.success(WorkflowRunSnapshot(
        run_id,
        WorkflowRunSuperseded(
          workflow_id,
          issue_id,
          superseded_by_run_id,
          reason,
          superseded_at_ms,
          run_root,
        ),
      ))
    }
    _ ->
      decode.failure(
        WorkflowRunSnapshot("", WorkflowRunInterrupted("", "", "", 0, "")),
        expected: "WorkflowRunSnapshot",
      )
  }
}

fn workflow_task_ref_snapshot_decoder() -> decode.Decoder(
  WorkflowTaskRefSnapshot,
) {
  use run_id <- decode.field("run_id", decode.string)
  use task_backend_kind <- decode.field("task_backend_kind", decode.string)
  use task_remote_id <- decode.field("task_remote_id", decode.string)
  use task_key <- decode.optional_field(
    "task_key",
    None,
    decode.optional(decode.string),
  )
  use task_url <- decode.optional_field(
    "task_url",
    None,
    decode.optional(decode.string),
  )
  decode.success(WorkflowTaskRefSnapshot(
    run_id,
    record.TaskRefFields(
      task_backend_kind: task_backend_kind,
      task_remote_id: task_remote_id,
      task_key: task_key,
      task_url: task_url,
    ),
  ))
}

fn workflow_contract_manifest_snapshot_decoder() -> decode.Decoder(
  WorkflowContractManifestSnapshot,
) {
  use run_id <- decode.field("run_id", decode.string)
  use workflow_id <- decode.field("workflow_id", decode.string)
  use workflow_fingerprint <- decode.field(
    "workflow_fingerprint",
    decode.string,
  )
  use artifact_ref <- decode.field("artifact_ref", decode.string)
  use artifact_sha256 <- decode.field("artifact_sha256", decode.string)
  use artifact_bytes <- decode.field("artifact_bytes", decode.int)
  use recorded_at_ms <- decode.field("recorded_at_ms", decode.int)
  decode.success(WorkflowContractManifestSnapshot(
    run_id,
    WorkflowContractManifestRef(
      workflow_id,
      workflow_fingerprint,
      artifact_ref,
      artifact_sha256,
      artifact_bytes,
      recorded_at_ms,
    ),
  ))
}

fn workflow_repair_snapshot_decoder() -> decode.Decoder(WorkflowRepairSnapshot) {
  use run_id <- decode.field("run_id", decode.string)
  use workflow_id <- decode.field("workflow_id", decode.string)
  use issue_id <- decode.field("issue_id", decode.string)
  use issue_identifier <- decode.field("issue_identifier", decode.string)
  use requested_target <- decode.field("requested_target", decode.string)
  use requested_step_id <- decode.optional_field(
    "requested_step_id",
    None,
    decode.optional(decode.string),
  )
  use selected_step_id <- decode.field("selected_step_id", decode.string)
  use failed_attempt_index <- decode.field("failed_attempt_index", decode.int)
  use next_attempt_index <- decode.field("next_attempt_index", decode.int)
  use reason <- decode.field("reason", decode.string)
  use requested_at_ms <- decode.field("requested_at_ms", decode.int)
  use generation <- decode.optional_field("generation", 1, decode.int)
  decode.success(WorkflowRepairSnapshot(
    run_id,
    WorkflowRepairStatus(
      workflow_id: workflow_id,
      issue_id: issue_id,
      issue_identifier: issue_identifier,
      requested_target: requested_target,
      requested_step_id: requested_step_id,
      selected_step_id: selected_step_id,
      failed_attempt_index: failed_attempt_index,
      next_attempt_index: next_attempt_index,
      reason: reason,
      requested_at_ms: requested_at_ms,
      generation: generation,
    ),
  ))
}

fn step_attempt_snapshot_decoder() -> decode.Decoder(StepAttemptSnapshot) {
  use key <- decode.field("key", decode.string)
  use status <- decode.field("status", decode.string)
  use run_id <- decode.field("run_id", decode.string)
  use workflow_id <- decode.field("workflow_id", decode.string)
  use step_id <- decode.field("step_id", decode.string)
  use attempt_index <- decode.field("attempt_index", decode.int)
  case status {
    "pending" -> {
      use workspace_name <- decode.field("workspace_name", decode.string)
      use workspace_path <- decode.field("workspace_path", decode.string)
      use run_root <- decode.field("run_root", decode.string)
      use source_workspace_name <- decode.optional_field(
        "source_workspace_name",
        None,
        decode.optional(decode.string),
      )
      use source_workspace_path <- decode.optional_field(
        "source_workspace_path",
        None,
        decode.optional(decode.string),
      )
      use prepared_at_ms <- decode.field("prepared_at_ms", decode.int)
      decode.success(StepAttemptSnapshot(
        key,
        StepAttemptPending(
          run_id,
          workflow_id,
          step_id,
          attempt_index,
          workspace_name,
          workspace_path,
          run_root,
          source_workspace_name,
          source_workspace_path,
          prepared_at_ms,
        ),
      ))
    }
    "running" -> {
      use workspace_name <- decode.field("workspace_name", decode.string)
      use workspace_path <- decode.field("workspace_path", decode.string)
      use run_root <- decode.field("run_root", decode.string)
      use source_workspace_name <- decode.optional_field(
        "source_workspace_name",
        None,
        decode.optional(decode.string),
      )
      use source_workspace_path <- decode.optional_field(
        "source_workspace_path",
        None,
        decode.optional(decode.string),
      )
      use operator_session_id <- decode.field(
        "operator_session_id",
        decode.string,
      )
      use external_session_ref <- decode.optional_field(
        "external_session_ref",
        None,
        decode.optional(decode.string),
      )
      use continuation_capable <- decode.optional_field(
        "continuation_capable",
        False,
        decode.bool,
      )
      use pi_session_id <- decode.optional_field(
        "pi_session_id",
        None,
        decode.optional(decode.string),
      )
      use pi_session_file <- decode.optional_field(
        "pi_session_file",
        None,
        decode.optional(decode.string),
      )
      use pi_session_fact_count <- decode.optional_field(
        "pi_session_fact_count",
        0,
        decode.int,
      )
      use started_at_ms <- decode.field("started_at_ms", decode.int)
      decode.success(StepAttemptSnapshot(
        key,
        StepAttemptRunning(
          run_id,
          workflow_id,
          step_id,
          attempt_index,
          workspace_name,
          workspace_path,
          run_root,
          source_workspace_name,
          source_workspace_path,
          operator_session_id,
          external_session_ref,
          continuation_capable,
          pi_session_id,
          pi_session_file,
          pi_session_fact_count,
          started_at_ms,
        ),
      ))
    }
    "finished" -> {
      use outcome <- decode.field("outcome", decode.string)
      use artifact_ref <- decode.field("artifact_ref", decode.string)
      use artifact_sha256 <- decode.field("artifact_sha256", decode.string)
      use workspace_name <- decode.field("workspace_name", decode.string)
      use workspace_path <- decode.field("workspace_path", decode.string)
      use run_root <- decode.optional_field("run_root", "", decode.string)
      use source_workspace_name <- decode.optional_field(
        "source_workspace_name",
        None,
        decode.optional(decode.string),
      )
      use source_workspace_path <- decode.optional_field(
        "source_workspace_path",
        None,
        decode.optional(decode.string),
      )
      use token_total <- decode.field("token_total", decode.int)
      use turns <- decode.field("turns", decode.int)
      use finished_at_ms <- decode.field("finished_at_ms", decode.int)
      decode.success(StepAttemptSnapshot(
        key,
        StepAttemptFinishedStatus(
          run_id,
          workflow_id,
          step_id,
          attempt_index,
          outcome,
          artifact_ref,
          artifact_sha256,
          workspace_name,
          workspace_path,
          run_root,
          source_workspace_name,
          source_workspace_path,
          token_total,
          turns,
          finished_at_ms,
        ),
      ))
    }
    "interrupted" -> {
      use workspace_name <- decode.optional_field(
        "workspace_name",
        "",
        decode.string,
      )
      use workspace_path <- decode.optional_field(
        "workspace_path",
        "",
        decode.string,
      )
      use run_root <- decode.optional_field("run_root", "", decode.string)
      use reason <- decode.field("reason", decode.string)
      use continuation_capable <- decode.optional_field(
        "continuation_capable",
        False,
        decode.bool,
      )
      use pi_session_id <- decode.optional_field(
        "pi_session_id",
        None,
        decode.optional(decode.string),
      )
      use pi_session_file <- decode.optional_field(
        "pi_session_file",
        None,
        decode.optional(decode.string),
      )
      use pi_session_fact_count <- decode.optional_field(
        "pi_session_fact_count",
        0,
        decode.int,
      )
      use interrupted_at_ms <- decode.field("interrupted_at_ms", decode.int)
      decode.success(StepAttemptSnapshot(
        key,
        StepAttemptInterruptedStatus(
          run_id,
          workflow_id,
          step_id,
          attempt_index,
          workspace_name,
          workspace_path,
          run_root,
          reason,
          continuation_capable,
          pi_session_id,
          pi_session_file,
          pi_session_fact_count,
          interrupted_at_ms,
        ),
      ))
    }
    "superseded" -> {
      use superseded_by_attempt_index <- decode.field(
        "superseded_by_attempt_index",
        decode.int,
      )
      use reason <- decode.field("reason", decode.string)
      use superseded_at_ms <- decode.field("superseded_at_ms", decode.int)
      decode.success(StepAttemptSnapshot(
        key,
        StepAttemptSupersededStatus(
          run_id,
          workflow_id,
          step_id,
          attempt_index,
          superseded_by_attempt_index,
          reason,
          superseded_at_ms,
        ),
      ))
    }
    _ ->
      decode.failure(
        StepAttemptSnapshot(
          "",
          StepAttemptInterruptedStatus(
            "",
            "",
            "",
            0,
            "",
            "",
            "",
            "",
            False,
            None,
            None,
            0,
            0,
          ),
        ),
        expected: "StepAttemptSnapshot",
      )
  }
}

fn retry_snapshot_decoder() -> decode.Decoder(RetrySnapshot) {
  use issue_id <- decode.field("issue_id", decode.string)
  use status <- decode.field("status", decode.string)
  case status {
    "scheduled" -> {
      use issue_identifier <- decode.field("issue_identifier", decode.string)
      use delay_ms <- decode.field("delay_ms", decode.int)
      use generation <- decode.field("generation", decode.int)
      use reason <- decode.field("reason", decode.string)
      use scheduled_at_ms <- decode.field("scheduled_at_ms", decode.int)
      decode.success(RetrySnapshot(
        issue_id,
        RetryScheduled(
          issue_identifier,
          delay_ms,
          generation,
          reason,
          scheduled_at_ms,
        ),
      ))
    }
    "cancelled" -> {
      use generation <- decode.field("generation", decode.int)
      use reason <- decode.field("reason", decode.string)
      use cancelled_at_ms <- decode.field("cancelled_at_ms", decode.int)
      decode.success(RetrySnapshot(
        issue_id,
        RetryCancelled(generation, reason, cancelled_at_ms),
      ))
    }
    _ ->
      decode.failure(
        RetrySnapshot("", RetryCancelled(0, "", 0)),
        expected: "RetrySnapshot",
      )
  }
}

fn parked_snapshot_decoder() -> decode.Decoder(ParkedSnapshot) {
  use issue_id <- decode.field("issue_id", decode.string)
  use issue_identifier <- decode.field("issue_identifier", decode.string)
  use reason <- decode.field("reason", decode.string)
  use observed_updated_at_ms <- decode.field(
    "observed_updated_at_ms",
    decode.int,
  )
  use parked_at_ms <- decode.field("parked_at_ms", decode.int)
  use release_policy <- decode.optional_field(
    "release_policy",
    "explicit_unpark_only",
    decode.string,
  )
  use issue_fingerprint <- decode.optional_field(
    "issue_fingerprint",
    "",
    decode.string,
  )
  decode.success(ParkedSnapshot(
    issue_id,
    ParkedIssue(
      issue_identifier,
      reason,
      observed_updated_at_ms,
      parked_at_ms,
      release_policy,
      issue_fingerprint,
    ),
  ))
}

fn command_snapshot_decoder() -> decode.Decoder(CommandSnapshot) {
  use comment_id <- decode.field("comment_id", decode.string)
  use status <- decode.field("status", decode.string)
  case status {
    "seen" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use author_id <- decode.field("author_id", decode.string)
      use command_name <- decode.field("command_name", decode.string)
      use excerpt <- decode.field("excerpt", decode.string)
      use seen_at_ms <- decode.field("seen_at_ms", decode.int)
      decode.success(CommandSnapshot(
        comment_id,
        CommandSeen(issue_id, author_id, command_name, excerpt, seen_at_ms),
      ))
    }
    "started" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use command_name <- decode.field("command_name", decode.string)
      use started_at_ms <- decode.field("started_at_ms", decode.int)
      decode.success(CommandSnapshot(
        comment_id,
        CommandStarted(issue_id, command_name, started_at_ms),
      ))
    }
    "completed" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use result_status <- decode.field("result_status", decode.string)
      use message_excerpt <- decode.field("message_excerpt", decode.string)
      use completed_at_ms <- decode.field("completed_at_ms", decode.int)
      decode.success(CommandSnapshot(
        comment_id,
        CommandCompleted(
          issue_id,
          result_status,
          message_excerpt,
          completed_at_ms,
        ),
      ))
    }
    "acked" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use acked_at_ms <- decode.field("acked_at_ms", decode.int)
      decode.success(CommandSnapshot(
        comment_id,
        CommandAcked(issue_id, acked_at_ms),
      ))
    }
    _ ->
      decode.failure(
        CommandSnapshot("", CommandAcked("", 0)),
        expected: "CommandSnapshot",
      )
  }
}

fn command_receipt_snapshot_decoder() -> decode.Decoder(CommandReceiptSnapshot) {
  use comment_id <- decode.field("comment_id", decode.string)
  use status <- decode.field("status", decode.string)
  case status {
    "unseen" ->
      decode.success(CommandReceiptSnapshot(comment_id, CommandReceiptUnseen))
    "seen" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use author_id <- decode.field("author_id", decode.string)
      use command_name <- decode.field("command_name", decode.string)
      use excerpt <- decode.field("excerpt", decode.string)
      use seen_at_ms <- decode.field("seen_at_ms", decode.int)
      decode.success(CommandReceiptSnapshot(
        comment_id,
        CommandReceiptSeen(
          issue_id,
          author_id,
          command_name,
          excerpt,
          seen_at_ms,
        ),
      ))
    }
    "started" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use author_id <- decode.field("author_id", decode.string)
      use command_name <- decode.field("command_name", decode.string)
      use excerpt <- decode.field("excerpt", decode.string)
      use seen_at_ms <- decode.field("seen_at_ms", decode.int)
      use started_at_ms <- decode.field("started_at_ms", decode.int)
      decode.success(CommandReceiptSnapshot(
        comment_id,
        CommandReceiptStarted(
          issue_id,
          author_id,
          command_name,
          excerpt,
          seen_at_ms,
          started_at_ms,
        ),
      ))
    }
    "completed" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use author_id <- decode.field("author_id", decode.string)
      use command_name <- decode.field("command_name", decode.string)
      use excerpt <- decode.field("excerpt", decode.string)
      use result_status <- decode.field("result_status", decode.string)
      use message_excerpt <- decode.field("message_excerpt", decode.string)
      use seen_at_ms <- decode.field("seen_at_ms", decode.int)
      use started_at_ms <- decode.field("started_at_ms", decode.int)
      use completed_at_ms <- decode.field("completed_at_ms", decode.int)
      use acked_at_ms <- decode.optional_field(
        "acked_at_ms",
        None,
        decode.optional(decode.int),
      )
      decode.success(CommandReceiptSnapshot(
        comment_id,
        CommandReceiptCompleted(
          issue_id,
          author_id,
          command_name,
          excerpt,
          result_status,
          message_excerpt,
          seen_at_ms,
          started_at_ms,
          completed_at_ms,
          acked_at_ms,
        ),
      ))
    }
    "acked" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use acked_at_ms <- decode.field("acked_at_ms", decode.int)
      decode.success(CommandReceiptSnapshot(
        comment_id,
        CommandReceiptAcked(issue_id, acked_at_ms),
      ))
    }
    _ ->
      decode.failure(
        CommandReceiptSnapshot("", CommandReceiptUnseen),
        expected: "CommandReceiptSnapshot",
      )
  }
}

fn outbox_snapshot_decoder() -> decode.Decoder(OutboxSnapshot) {
  use outbox_id <- decode.field("outbox_id", decode.string)
  use status <- decode.field("status", decode.string)
  case status {
    "pending" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use outbox_kind <- decode.field("outbox_kind", decode.string)
      use dedupe_key <- decode.field("dedupe_key", decode.string)
      use pending_at_ms <- decode.field("pending_at_ms", decode.int)
      decode.success(OutboxSnapshot(
        outbox_id,
        OutboxPending(issue_id, outbox_kind, dedupe_key, pending_at_ms),
      ))
    }
    "pending_v2" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use outbox_kind <- decode.field("outbox_kind", decode.string)
      use dedupe_key <- decode.field("dedupe_key", decode.string)
      use payload_json <- decode.field("payload_json", decode.string)
      use pending_at_ms <- decode.field("pending_at_ms", decode.int)
      decode.success(OutboxSnapshot(
        outbox_id,
        OutboxPendingV2(
          issue_id,
          outbox_kind,
          dedupe_key,
          payload_json,
          pending_at_ms,
        ),
      ))
    }
    "completed" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use outbox_kind <- decode.field("outbox_kind", decode.string)
      use completed_at_ms <- decode.field("completed_at_ms", decode.int)
      decode.success(OutboxSnapshot(
        outbox_id,
        OutboxCompleted(issue_id, outbox_kind, completed_at_ms),
      ))
    }
    "failed" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use outbox_kind <- decode.field("outbox_kind", decode.string)
      use error_code <- decode.field("error_code", decode.string)
      use failed_at_ms <- decode.field("failed_at_ms", decode.int)
      decode.success(OutboxSnapshot(
        outbox_id,
        OutboxFailed(issue_id, outbox_kind, error_code, failed_at_ms),
      ))
    }
    _ ->
      decode.failure(
        OutboxSnapshot("", OutboxFailed("", "", "", 0)),
        expected: "OutboxSnapshot",
      )
  }
}

fn issue_counter_snapshot_decoder() -> decode.Decoder(IssueCounterSnapshot) {
  use issue_id <- decode.field("issue_id", decode.string)
  use issue_identifier <- decode.field("issue_identifier", decode.string)
  use failure_attempts <- decode.field("failure_attempts", decode.int)
  use worker_sessions <- decode.field("worker_sessions", decode.int)
  use observed_updated_at_ms <- decode.field(
    "observed_updated_at_ms",
    decode.int,
  )
  use source_run_ids <- decode.optional_field(
    "source_run_ids",
    [],
    decode.list(of: decode.string),
  )
  use updated_at_ms <- decode.field("updated_at_ms", decode.int)
  decode.success(IssueCounterSnapshot(
    issue_id,
    IssueCounterStatus(
      issue_identifier,
      failure_attempts,
      worker_sessions,
      observed_updated_at_ms,
      source_run_ids,
      updated_at_ms,
    ),
  ))
}

fn known_workspace_snapshot_decoder() -> decode.Decoder(KnownWorkspaceSnapshot) {
  use issue_id <- decode.field("issue_id", decode.string)
  use issue_identifier <- decode.field("issue_identifier", decode.string)
  use workspace_path <- decode.field("workspace_path", decode.string)
  use recorded_at_ms <- decode.field("recorded_at_ms", decode.int)
  decode.success(KnownWorkspaceSnapshot(
    issue_id,
    KnownWorkspace(issue_identifier, workspace_path, recorded_at_ms),
  ))
}

fn scheduled_run_summary_decoder() -> decode.Decoder(ScheduledRunSummary) {
  use run_id <- decode.field("run_id", decode.string)
  use due_at_ms <- decode.field("due_at_ms", decode.int)
  use trigger <- decode.field("trigger", decode.string)
  use attempt <- decode.field("attempt", decode.int)
  use status <- decode.field("status", decode.string)
  use reason <- decode.optional_field(
    "reason",
    None,
    decode.optional(decode.string),
  )
  use session_id <- decode.optional_field(
    "session_id",
    None,
    decode.optional(decode.string),
  )
  use run_root <- decode.optional_field(
    "run_root",
    None,
    decode.optional(decode.string),
  )
  decode.success(ScheduledRunSummary(
    run_id: run_id,
    due_at_ms: due_at_ms,
    trigger: trigger,
    attempt: attempt,
    status: status,
    reason: reason,
    session_id: session_id,
    run_root: run_root,
  ))
}

fn scheduled_report_retry_decoder() -> decode.Decoder(ScheduledReportRetry) {
  use run_id <- decode.field("run_id", decode.string)
  use attempt <- decode.field("attempt", decode.int)
  use dedupe_key <- decode.field("dedupe_key", decode.string)
  use error_code <- decode.field("error_code", decode.string)
  use error_message <- decode.field("error_message", decode.string)
  use next_retry_at_ms <- decode.field("next_retry_at_ms", decode.int)
  use generation <- decode.field("generation", decode.int)
  decode.success(ScheduledReportRetry(
    run_id: run_id,
    attempt: attempt,
    dedupe_key: dedupe_key,
    error_code: error_code,
    error_message: error_message,
    next_retry_at_ms: next_retry_at_ms,
    generation: generation,
  ))
}

fn workstream_snapshot_decoder() -> decode.Decoder(WorkstreamSnapshot) {
  use workstream_id <- decode.field("workstream_id", decode.string)
  use task_backend_kind <- decode.optional_field(
    "task_backend_kind",
    None,
    decode.optional(decode.string),
  )
  use task_remote_id <- decode.optional_field(
    "task_remote_id",
    None,
    decode.optional(decode.string),
  )
  use task_key <- decode.optional_field(
    "task_key",
    None,
    decode.optional(decode.string),
  )
  use task_url <- decode.optional_field(
    "task_url",
    None,
    decode.optional(decode.string),
  )
  use created_at_ms <- decode.optional_field(
    "created_at_ms",
    None,
    decode.optional(decode.int),
  )
  use latest_assignment <- decode.optional_field(
    "latest_assignment",
    None,
    decode.optional(workstream_assignment_decoder()),
  )
  use artifacts <- decode.optional_field(
    "artifacts",
    [],
    decode.list(of: workstream_artifact_snapshot_decoder()),
  )
  use handoffs <- decode.optional_field(
    "handoffs",
    [],
    decode.list(of: workstream_handoff_snapshot_decoder()),
  )
  use queued_phase_runs <- decode.optional_field(
    "queued_phase_runs",
    [],
    decode.list(of: workstream_phase_run_snapshot_decoder()),
  )
  case
    task_ref_from_snapshot(
      task_backend_kind,
      task_remote_id,
      task_key,
      task_url,
    )
  {
    Ok(task_ref) ->
      decode.success(WorkstreamSnapshot(
        workstream_id: workstream_id,
        status: WorkstreamStatus(
          workstream_id: workstream_id,
          task_ref: task_ref,
          created_at_ms: created_at_ms,
          latest_assignment: latest_assignment,
          artifacts: artifacts
            |> list.map(fn(entry) { #(entry.snapshot_ref, entry) })
            |> dict.from_list,
          handoffs: handoffs
            |> list.map(fn(entry) { #(entry.handoff_ref, entry) })
            |> dict.from_list,
          queued_phase_runs: queued_phase_runs
            |> list.map(fn(entry) { #(entry.phase_run_id, entry) })
            |> dict.from_list,
        ),
      ))
    Error(Nil) ->
      decode.failure(
        empty_workstream_snapshot(),
        expected: "complete workstream task ref fields",
      )
  }
}

fn task_ref_from_snapshot(
  backend_kind: Option(String),
  remote_id: Option(String),
  task_key: Option(String),
  task_url: Option(String),
) -> Result(Option(record.TaskRefFields), Nil) {
  case backend_kind, remote_id {
    None, None -> Ok(None)
    Some(kind), Some(id) ->
      Ok(Some(record.TaskRefFields(kind, id, task_key, task_url)))
    _, _ -> Error(Nil)
  }
}

fn empty_workstream_snapshot() -> WorkstreamSnapshot {
  WorkstreamSnapshot(
    "",
    WorkstreamStatus(
      workstream_id: "",
      task_ref: None,
      created_at_ms: None,
      latest_assignment: None,
      artifacts: dict.new(),
      handoffs: dict.new(),
      queued_phase_runs: dict.new(),
    ),
  )
}

fn workstream_assignment_decoder() -> decode.Decoder(WorkstreamAssignment) {
  use assignment_id <- decode.field("assignment_id", decode.string)
  use workflow_id <- decode.field("workflow_id", decode.string)
  use playbook_id <- decode.optional_field(
    "playbook_id",
    None,
    decode.optional(decode.string),
  )
  use reason <- decode.field("reason", decode.string)
  use idempotency_key <- decode.field("idempotency_key", decode.string)
  use assigned_at_ms <- decode.field("assigned_at_ms", decode.int)
  decode.success(WorkstreamAssignment(
    assignment_id: assignment_id,
    workflow_id: workflow_id,
    playbook_id: playbook_id,
    reason: reason,
    idempotency_key: idempotency_key,
    assigned_at_ms: assigned_at_ms,
  ))
}

fn workstream_artifact_snapshot_decoder() -> decode.Decoder(
  WorkstreamArtifactSnapshot,
) {
  use artifact_id <- decode.field("artifact_id", decode.string)
  use artifact_type <- decode.field("artifact_type", decode.string)
  use snapshot_ref <- decode.field("snapshot_ref", decode.string)
  use snapshot_sha256 <- decode.field("snapshot_sha256", decode.string)
  use snapshot_bytes <- decode.field("snapshot_bytes", decode.int)
  use original_path <- decode.field("original_path", decode.string)
  use contract_type <- decode.field("contract_type", decode.string)
  use media_type <- decode.field("media_type", decode.string)
  use producer_workflow_id <- decode.field(
    "producer_workflow_id",
    decode.string,
  )
  use producer_run_id <- decode.field("producer_run_id", decode.string)
  use producer_step_id <- decode.field("producer_step_id", decode.string)
  use idempotency_key <- decode.field("idempotency_key", decode.string)
  use recorded_at_ms <- decode.field("recorded_at_ms", decode.int)
  decode.success(WorkstreamArtifactSnapshot(
    artifact_id: artifact_id,
    artifact_type: artifact_type,
    snapshot_ref: snapshot_ref,
    snapshot_sha256: snapshot_sha256,
    snapshot_bytes: snapshot_bytes,
    original_path: original_path,
    contract_type: contract_type,
    media_type: media_type,
    producer_workflow_id: producer_workflow_id,
    producer_run_id: producer_run_id,
    producer_step_id: producer_step_id,
    idempotency_key: idempotency_key,
    recorded_at_ms: recorded_at_ms,
  ))
}

fn workstream_handoff_snapshot_decoder() -> decode.Decoder(
  WorkstreamHandoffSnapshot,
) {
  use handoff_id <- decode.field("handoff_id", decode.string)
  use handoff_ref <- decode.field("handoff_ref", decode.string)
  use handoff_sha256 <- decode.field("handoff_sha256", decode.string)
  use handoff_bytes <- decode.field("handoff_bytes", decode.int)
  use source_workflow_id <- decode.field("source_workflow_id", decode.string)
  use source_run_id <- decode.field("source_run_id", decode.string)
  use idempotency_key <- decode.field("idempotency_key", decode.string)
  use recorded_at_ms <- decode.field("recorded_at_ms", decode.int)
  decode.success(WorkstreamHandoffSnapshot(
    handoff_id: handoff_id,
    handoff_ref: handoff_ref,
    handoff_sha256: handoff_sha256,
    handoff_bytes: handoff_bytes,
    source_workflow_id: source_workflow_id,
    source_run_id: source_run_id,
    idempotency_key: idempotency_key,
    recorded_at_ms: recorded_at_ms,
  ))
}

fn workstream_phase_run_snapshot_decoder() -> decode.Decoder(WorkstreamPhaseRun) {
  use phase_run_id <- decode.field("phase_run_id", decode.string)
  use action_id <- decode.field("action_id", decode.string)
  use workflow_id <- decode.field("workflow_id", decode.string)
  use input_bundle_ref <- decode.field("input_bundle_ref", decode.string)
  use input_bundle_sha256 <- decode.field("input_bundle_sha256", decode.string)
  use input_bundle_bytes <- decode.field("input_bundle_bytes", decode.int)
  use idempotency_key <- decode.field("idempotency_key", decode.string)
  use queued_at_ms <- decode.field("queued_at_ms", decode.int)
  decode.success(WorkstreamPhaseRun(
    phase_run_id: phase_run_id,
    action_id: action_id,
    workflow_id: workflow_id,
    input_bundle_ref: input_bundle_ref,
    input_bundle_sha256: input_bundle_sha256,
    input_bundle_bytes: input_bundle_bytes,
    idempotency_key: idempotency_key,
    queued_at_ms: queued_at_ms,
  ))
}

fn scheduled_job_snapshot_decoder() -> decode.Decoder(ScheduledJobSnapshot) {
  use job_id <- decode.field("job_id", decode.string)
  use workflow_id <- decode.field("workflow_id", decode.string)
  use state <- decode.optional_field("state", "idle", decode.string)
  use current_run <- decode.optional_field(
    "current_run",
    None,
    decode.optional(scheduled_run_summary_decoder()),
  )
  use last_due_at_ms <- decode.optional_field(
    "last_due_at_ms",
    None,
    decode.optional(decode.int),
  )
  use last_success_at_ms <- decode.optional_field(
    "last_success_at_ms",
    None,
    decode.optional(decode.int),
  )
  use last_success_run_id <- decode.optional_field(
    "last_success_run_id",
    None,
    decode.optional(decode.string),
  )
  use last_failure_at_ms <- decode.optional_field(
    "last_failure_at_ms",
    None,
    decode.optional(decode.int),
  )
  use last_failure_run_id <- decode.optional_field(
    "last_failure_run_id",
    None,
    decode.optional(decode.string),
  )
  use last_failure_reason <- decode.optional_field(
    "last_failure_reason",
    None,
    decode.optional(decode.string),
  )
  use retry_count <- decode.optional_field("retry_count", 0, decode.int)
  use skipped_overlap_count <- decode.optional_field(
    "skipped_overlap_count",
    0,
    decode.int,
  )
  use skipped_catch_up_count <- decode.optional_field(
    "skipped_catch_up_count",
    0,
    decode.int,
  )
  use skipped_paused_count <- decode.optional_field(
    "skipped_paused_count",
    0,
    decode.int,
  )
  use skipped_capacity_count <- decode.optional_field(
    "skipped_capacity_count",
    0,
    decode.int,
  )
  use failure_issue_id <- decode.optional_field(
    "failure_issue_id",
    None,
    decode.optional(decode.string),
  )
  use failure_dedupe_key <- decode.optional_field(
    "failure_dedupe_key",
    None,
    decode.optional(decode.string),
  )
  use report_retry <- decode.optional_field(
    "report_retry",
    None,
    decode.optional(scheduled_report_retry_decoder()),
  )
  use recent_run_ids <- decode.optional_field(
    "recent_run_ids",
    [],
    decode.list(of: decode.string),
  )
  decode.success(ScheduledJobSnapshot(
    job_id,
    ScheduledJobStatus(
      job_id: job_id,
      workflow_id: workflow_id,
      state: scheduled_state_from_string(state),
      current_run: current_run,
      last_due_at_ms: last_due_at_ms,
      last_success_at_ms: last_success_at_ms,
      last_success_run_id: last_success_run_id,
      last_failure_at_ms: last_failure_at_ms,
      last_failure_run_id: last_failure_run_id,
      last_failure_reason: last_failure_reason,
      retry_count: retry_count,
      skipped_overlap_count: skipped_overlap_count,
      skipped_catch_up_count: skipped_catch_up_count,
      skipped_paused_count: skipped_paused_count,
      skipped_capacity_count: skipped_capacity_count,
      failure_issue_id: failure_issue_id,
      failure_dedupe_key: failure_dedupe_key,
      report_retry: report_retry,
      recent_run_ids: trim_recent_runs(recent_run_ids),
    ),
  ))
}

fn run_issue_ids(runs: Dict(String, RunStatus)) -> List(String) {
  runs
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(_, status) = entry
    case status {
      RunRunning(issue_id, _, _, _) -> issue_id
      RunFinished(issue_id, _, _, _, _) -> issue_id
      RunInterrupted(issue_id, _, _) -> issue_id
    }
  })
}

fn workflow_run_issue_ids(
  runs: Dict(String, WorkflowRunStatus),
) -> List(String) {
  runs
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(_, status) = entry
    case status {
      WorkflowRunActive(issue_id: issue_id, ..) -> issue_id
      WorkflowRunFinished(issue_id: issue_id, ..) -> issue_id
      WorkflowRunInterrupted(issue_id: issue_id, ..) -> issue_id
      WorkflowRunSuperseded(issue_id: issue_id, ..) -> issue_id
    }
  })
}

fn workflow_task_ref_issue_ids(
  task_refs: Dict(String, record.TaskRefFields),
) -> List(String) {
  task_refs
  |> dict.values
  |> list.map(fn(task_ref) { task_ref.task_remote_id })
}

fn command_issue_ids(commands: Dict(String, CommandStatus)) -> List(String) {
  commands
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(_, status) = entry
    case status {
      CommandSeen(issue_id, _, _, _, _) -> issue_id
      CommandStarted(issue_id, _, _) -> issue_id
      CommandCompleted(issue_id, _, _, _) -> issue_id
      CommandAcked(issue_id, _) -> issue_id
    }
  })
}

fn outbox_issue_ids(outbox: Dict(String, OutboxStatus)) -> List(String) {
  outbox
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(_, status) = entry
    case status {
      OutboxPending(issue_id, _, _, _) -> issue_id
      OutboxPendingV2(issue_id, _, _, _, _) -> issue_id
      OutboxCompleted(issue_id, _, _) -> issue_id
      OutboxFailed(issue_id, _, _, _) -> issue_id
    }
  })
}

fn append_unique_strings(
  values: List(String),
  more: List(String),
) -> List(String) {
  list.fold(more, values, insert_unique_string)
}

fn insert_unique_string(values: List(String), value: String) -> List(String) {
  case string.trim(value) == "" || list.contains(values, value) {
    True -> values
    False -> [value, ..values]
  }
}

fn pending_outbox_replays_loop(
  entries: List(#(String, OutboxStatus)),
  acc: List(OutboxReplay),
) -> Result(List(OutboxReplay), PendingOutboxError) {
  case entries {
    [] -> Ok(list.reverse(acc))
    [entry, ..rest] -> {
      let #(outbox_id, status) = entry
      case status {
        OutboxPending(_, _, _, _) -> pending_outbox_replays_loop(rest, acc)
        OutboxPendingV2(issue_id, outbox_kind, dedupe_key, payload_json, _) ->
          pending_outbox_replays_loop(rest, [
            OutboxReplay(
              outbox_id,
              issue_id,
              outbox_kind,
              dedupe_key,
              payload_json,
            ),
            ..acc
          ])
        OutboxCompleted(_, _, _) | OutboxFailed(_, _, _, _) ->
          pending_outbox_replays_loop(rest, acc)
      }
    }
  }
}

fn compare_outbox_entries_by_time(
  a: #(String, OutboxStatus),
  b: #(String, OutboxStatus),
) -> Order {
  let #(a_id, a_status) = a
  let #(b_id, b_status) = b
  case int.compare(outbox_status_time(a_status), outbox_status_time(b_status)) {
    Eq -> string.compare(a_id, b_id)
    order -> order
  }
}

fn outbox_status_time(status: OutboxStatus) -> Int {
  case status {
    OutboxPending(_, _, _, pending_at_ms) -> pending_at_ms
    OutboxPendingV2(_, _, _, _, pending_at_ms) -> pending_at_ms
    OutboxCompleted(_, _, completed_at_ms) -> completed_at_ms
    OutboxFailed(_, _, _, failed_at_ms) -> failed_at_ms
  }
}

pub fn describe_pending_outbox_error(error: PendingOutboxError) -> String {
  case error {
    OutboxPayloadMissing(outbox_id) -> "outbox_payload_missing:" <> outbox_id
  }
}

pub fn retry_status_to_string(status: RetryStatus) -> String {
  case status {
    RetryScheduled(_, delay_ms, generation, reason, scheduled_at_ms) ->
      "scheduled delay_ms="
      <> int.to_string(delay_ms)
      <> " generation="
      <> int.to_string(generation)
      <> " reason="
      <> reason
      <> " scheduled_at_ms="
      <> int.to_string(scheduled_at_ms)
    RetryCancelled(generation, reason, cancelled_at_ms) ->
      "cancelled generation="
      <> int.to_string(generation)
      <> " reason="
      <> reason
      <> " cancelled_at_ms="
      <> int.to_string(cancelled_at_ms)
  }
}
