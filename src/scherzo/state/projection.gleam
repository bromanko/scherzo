import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq}
import gleam/result
import gleam/string
import scherzo/json_decode_error
import scherzo/runtime/state as orchestrator_state
import scherzo/state/projection/commands as commands_projection
import scherzo/state/projection/issue_recovery as issue_recovery_projection
import scherzo/state/projection/legacy_runs as legacy_runs_projection
import scherzo/state/projection/outbox as outbox_projection
import scherzo/state/projection/publications as publication_projection
import scherzo/state/projection/scheduled as scheduled_projection
import scherzo/state/projection/steps as steps_projection
import scherzo/state/projection/workflow_runs as workflow_runs_projection
import scherzo/state/projection/workstreams as workstreams_projection
import scherzo/state/record

pub type Projection {
  Projection(
    runs: Dict(String, RunStatus),
    dispatch_paused: Bool,
    workflow_runs: Dict(String, WorkflowRunStatus),
    workflow_run_provenances: Dict(String, WorkflowRunProvenance),
    workflow_task_refs: Dict(String, record.TaskRefFields),
    workflow_input_manifests: Dict(String, WorkflowContractManifestRef),
    workflow_output_manifests: Dict(String, WorkflowContractManifestRef),
    publication_attempts: Dict(String, List(PublicationAttempt)),
    publication_latest_by_series: Dict(String, PublicationAttempt),
    workflow_repairs: Dict(String, WorkflowRepairStatus),
    step_attempts: Dict(String, StepAttemptStatus),
    step_recoveries: Dict(String, StepRecoveryStatus),
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

pub type PublicationAttempt {
  PublicationAttempt(
    run_id: String,
    workflow_id: String,
    publication_id: String,
    series_id: String,
    attempt_id: String,
    status: String,
    required: Bool,
    retryable: Bool,
    retry_execution_available: Bool,
    version_id: Option(String),
    manifest_ref: Option(String),
    manifest_sha256: Option(String),
    manifest_bytes: Option(Int),
    error_code: Option(String),
    error_message: Option(String),
    recorded_at_ms: Int,
  )
}

pub type WorkflowRunProvenance {
  WorkflowRunProvenance(
    workflow_id: String,
    workflow_fingerprint: String,
    issue_id: String,
    issue_identifier: String,
    issue_fingerprint: String,
    observed_updated_at_ms: Int,
    run_root: String,
    task_ref: record.TaskRefFields,
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

pub type StepRecoveryStatus {
  StepRecoveryStartedStatus(
    run_id: String,
    workflow_id: String,
    step_id: String,
    failed_attempt_index: Int,
    recovery_attempt_number: Int,
    recovery_session_id: String,
    model: Option(String),
    prompt_ref: String,
    started_at_ms: Int,
  )
  StepRecoveryFinishedStatus(
    run_id: String,
    workflow_id: String,
    step_id: String,
    failed_attempt_index: Int,
    recovery_attempt_number: Int,
    recovery_session_id: String,
    model: Option(String),
    prompt_ref: String,
    result: String,
    summary: String,
    reason: String,
    retry_attempt_index: Option(Int),
    started_at_ms: Int,
    finished_at_ms: Int,
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
  OutboxPendingV2WithTask(
    task_ref: record.TaskRefFields,
    outbox_kind: String,
    dedupe_key: String,
    payload_json: String,
    pending_at_ms: Int,
  )
  OutboxAttempted(
    issue_id: String,
    outbox_kind: String,
    dedupe_key: String,
    payload_json: String,
    attempt_count: Int,
    attempted_at_ms: Int,
  )
  OutboxAttemptedWithTask(
    task_ref: record.TaskRefFields,
    outbox_kind: String,
    dedupe_key: String,
    payload_json: String,
    attempt_count: Int,
    attempted_at_ms: Int,
  )
  OutboxRetryScheduled(
    issue_id: String,
    outbox_kind: String,
    dedupe_key: String,
    payload_json: String,
    error_code: String,
    attempt_count: Int,
    next_attempt_at_ms: Int,
    failed_at_ms: Int,
  )
  OutboxRetryScheduledWithTask(
    task_ref: record.TaskRefFields,
    outbox_kind: String,
    dedupe_key: String,
    payload_json: String,
    error_code: String,
    attempt_count: Int,
    next_attempt_at_ms: Int,
    failed_at_ms: Int,
  )
  OutboxCompleted(issue_id: String, outbox_kind: String, completed_at_ms: Int)
  OutboxCompletedWithTask(
    task_ref: record.TaskRefFields,
    outbox_kind: String,
    completed_at_ms: Int,
  )
  OutboxFailed(
    issue_id: String,
    outbox_kind: String,
    error_code: String,
    failed_at_ms: Int,
  )
  OutboxFailedWithTask(
    task_ref: record.TaskRefFields,
    outbox_kind: String,
    error_code: String,
    failed_at_ms: Int,
  )
  OutboxPermanentlyFailed(
    issue_id: String,
    outbox_kind: String,
    error_code: String,
    attempt_count: Int,
    failed_at_ms: Int,
  )
  OutboxPermanentlyFailedWithTask(
    task_ref: record.TaskRefFields,
    outbox_kind: String,
    error_code: String,
    attempt_count: Int,
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
    task_ref: record.TaskRefFields,
    outbox_kind: String,
    dedupe_key: String,
    payload_json: String,
  )
}

pub type PendingOutboxError {
  OutboxPayloadMissing(outbox_id: String)
}

pub type DecodeError {
  UnsupportedSnapshotVersion(Int)
  MalformedProjectionSnapshot(
    header_error: Option(json.DecodeError),
    snapshot_error: json.DecodeError,
  )
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

type WorkflowRunProvenanceSnapshot {
  WorkflowRunProvenanceSnapshot(
    run_id: String,
    provenance: WorkflowRunProvenance,
  )
}

type StepAttemptSnapshot {
  StepAttemptSnapshot(key: String, status: StepAttemptStatus)
}

type StepRecoverySnapshot {
  StepRecoverySnapshot(key: String, status: StepRecoveryStatus)
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

type PublicationAttemptSnapshot {
  PublicationAttemptSnapshot(key: String, attempts: List(PublicationAttempt))
}

type WorkflowRepairSnapshot {
  WorkflowRepairSnapshot(run_id: String, status: WorkflowRepairStatus)
}

type SnapshotFields {
  SnapshotFields(
    runs: List(RunSnapshot),
    dispatch_paused: Bool,
    workflow_runs: List(WorkflowRunSnapshot),
    workflow_run_provenances: List(WorkflowRunProvenanceSnapshot),
    workflow_task_refs: List(WorkflowTaskRefSnapshot),
    workflow_input_manifests: List(WorkflowContractManifestSnapshot),
    workflow_output_manifests: List(WorkflowContractManifestSnapshot),
    publication_attempts: List(PublicationAttemptSnapshot),
    publication_latest_by_series: List(#(String, PublicationAttempt)),
    workflow_repairs: List(WorkflowRepairSnapshot),
    step_attempts: List(StepAttemptSnapshot),
    step_recoveries: List(StepRecoverySnapshot),
    retries: List(RetrySnapshot),
    parked_issues: List(ParkedSnapshot),
    commands: List(CommandSnapshot),
    command_receipts: List(CommandReceiptSnapshot),
    outbox: List(OutboxSnapshot),
    issue_counters: List(IssueCounterSnapshot),
    known_workspaces: List(KnownWorkspaceSnapshot),
    workstreams: List(WorkstreamSnapshot),
    scheduled_jobs: List(#(String, scheduled_projection.ScheduledJobStatus)),
  )
}

type WorkstreamSnapshot {
  WorkstreamSnapshot(workstream_id: String, status: WorkstreamStatus)
}

pub fn new() -> Projection {
  Projection(
    runs: dict.new(),
    dispatch_paused: False,
    workflow_runs: dict.new(),
    workflow_run_provenances: dict.new(),
    workflow_task_refs: dict.new(),
    workflow_input_manifests: dict.new(),
    workflow_output_manifests: dict.new(),
    publication_attempts: dict.new(),
    publication_latest_by_series: dict.new(),
    workflow_repairs: dict.new(),
    step_attempts: dict.new(),
    step_recoveries: dict.new(),
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
        runs: legacy_runs_projection.started(
          projection.runs,
          run_id,
          RunRunning(issue_id, issue_identifier, workspace_path, at_ms),
        ),
      )
    record.RunFinished(run_id, issue_id, classification, token_total, turns) ->
      Projection(
        ..projection,
        runs: legacy_runs_projection.finished(
          projection.runs,
          run_id,
          RunFinished(issue_id, classification, token_total, turns, at_ms),
        ),
      )
    record.RunInterrupted(run_id, issue_id, reason) ->
      Projection(
        ..projection,
        runs: legacy_runs_projection.interrupted(
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
    ) -> {
      let ref =
        record.linear_task_ref_fields(issue_id, Some(issue_identifier), None)
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
        workflow_run_provenances: dict.insert(
          projection.workflow_run_provenances,
          run_id,
          WorkflowRunProvenance(
            workflow_id: workflow_id,
            workflow_fingerprint: workflow_fingerprint,
            issue_id: issue_id,
            issue_identifier: issue_identifier,
            issue_fingerprint: issue_fingerprint,
            observed_updated_at_ms: observed_updated_at_ms,
            run_root: run_root,
            task_ref: ref,
          ),
        ),
        workflow_task_refs: dict.insert(
          projection.workflow_task_refs,
          run_id,
          ref,
        ),
      )
    }
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
        workflow_run_provenances: dict.insert(
          projection.workflow_run_provenances,
          run_id,
          WorkflowRunProvenance(
            workflow_id: workflow_id,
            workflow_fingerprint: workflow_fingerprint,
            issue_id: issue_id,
            issue_identifier: issue_identifier,
            issue_fingerprint: issue_fingerprint,
            observed_updated_at_ms: observed_updated_at_ms,
            run_root: run_root,
            task_ref: task_ref,
          ),
        ),
        workflow_task_refs: dict.insert(
          projection.workflow_task_refs,
          run_id,
          task_ref,
        ),
      )
    record.WorkflowRunProvenanceRepaired(
      run_id,
      workflow_id,
      workflow_fingerprint,
      issue_id,
      issue_identifier,
      task_ref,
      issue_fingerprint,
      observed_updated_at_ms,
      run_root,
      _,
      _,
    ) ->
      Projection(
        ..projection,
        workflow_run_provenances: dict.insert(
          projection.workflow_run_provenances,
          run_id,
          WorkflowRunProvenance(
            workflow_id: workflow_id,
            workflow_fingerprint: workflow_fingerprint,
            issue_id: issue_id,
            issue_identifier: issue_identifier,
            issue_fingerprint: issue_fingerprint,
            observed_updated_at_ms: observed_updated_at_ms,
            run_root: run_root,
            task_ref: task_ref,
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
        workflow_task_refs: preserve_or_insert_workflow_task_ref(
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
    record.PublicationAttemptRecorded(
      run_id,
      workflow_id,
      publication_id,
      series_id,
      attempt_id,
      status,
      required,
      retryable,
      retry_execution_available,
      version_id,
      manifest_ref,
      manifest_sha256,
      manifest_bytes,
      error_code,
      error_message,
    ) -> {
      let attempt =
        PublicationAttempt(
          run_id: run_id,
          workflow_id: workflow_id,
          publication_id: publication_id,
          series_id: series_id,
          attempt_id: attempt_id,
          status: status,
          required: required,
          retryable: retryable,
          retry_execution_available: retry_execution_available,
          version_id: version_id,
          manifest_ref: manifest_ref,
          manifest_sha256: manifest_sha256,
          manifest_bytes: manifest_bytes,
          error_code: error_code,
          error_message: error_message,
          recorded_at_ms: at_ms,
        )
      Projection(
        ..projection,
        publication_attempts: publication_projection.append_attempt(
          projection.publication_attempts,
          run_id,
          publication_id,
          attempt,
        ),
        publication_latest_by_series: dict.insert(
          projection.publication_latest_by_series,
          publication_projection.series_key(series_id),
          attempt,
        ),
      )
    }
    record.WorkflowRunDiagnostic(..) -> projection
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
        record.linear_task_ref_fields(issue_id, Some(issue_identifier), None),
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
    record.WorkflowStepRecoveryStarted(
      run_id,
      workflow_id,
      step_id,
      failed_attempt_index,
      recovery_attempt_number,
      recovery_session_id,
      model,
      prompt_ref,
    ) ->
      Projection(
        ..projection,
        step_recoveries: dict.insert(
          projection.step_recoveries,
          step_recovery_key(
            run_id,
            step_id,
            failed_attempt_index,
            recovery_attempt_number,
          ),
          StepRecoveryStartedStatus(
            run_id: run_id,
            workflow_id: workflow_id,
            step_id: step_id,
            failed_attempt_index: failed_attempt_index,
            recovery_attempt_number: recovery_attempt_number,
            recovery_session_id: recovery_session_id,
            model: model,
            prompt_ref: prompt_ref,
            started_at_ms: at_ms,
          ),
        ),
      )
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
    ) ->
      apply_step_recovery_finished(
        projection,
        at_ms,
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
      )
    record.StepAttemptInterrupted(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      reason,
    ) -> {
      let key = step_attempt_key(run_id, step_id, attempt_index)
      let status = case dict.get(projection.step_attempts, key) {
        Ok(existing) ->
          case existing {
            StepAttemptFinishedStatus(..)
            | StepAttemptInterruptedStatus(..)
            | StepAttemptSupersededStatus(..) -> existing
            StepAttemptRunning(
              workspace_name: workspace_name,
              workspace_path: workspace_path,
              run_root: run_root,
              continuation_capable: continuation_capable,
              pi_session_id: pi_session_id,
              pi_session_file: pi_session_file,
              pi_session_fact_count: pi_session_fact_count,
              ..,
            ) ->
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
            StepAttemptPending(
              workspace_name: workspace_name,
              workspace_path: workspace_path,
              run_root: run_root,
              ..,
            ) ->
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
          }
        Error(Nil) ->
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
    record.RetryCancelled(issue_id, generation, reason) -> {
      let next_status = case dict.get(projection.retries, issue_id) {
        Ok(RetryScheduled(
          issue_identifier,
          delay_ms,
          current_generation,
          current_reason,
          scheduled_at_ms,
        )) ->
          case current_generation == generation {
            True -> RetryCancelled(generation, reason, at_ms)
            False ->
              RetryScheduled(
                issue_identifier,
                delay_ms,
                current_generation,
                current_reason,
                scheduled_at_ms,
              )
          }
        Ok(RetryCancelled(_, _, _)) | Error(Nil) ->
          RetryCancelled(generation, reason, at_ms)
      }
      Projection(
        ..projection,
        retries: dict.insert(projection.retries, issue_id, next_status),
      )
    }
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
        Error(Nil) -> []
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
    record.DispatchPauseChanged(paused) ->
      Projection(..projection, dispatch_paused: paused)
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
        commands: commands_projection.insert_status(
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
        commands: commands_projection.insert_status(
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
        commands: commands_projection.insert_status(
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
        commands: commands_projection.insert_status(
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
      backend_kind,
      event_id,
      task_remote_id,
      _,
      author_id,
      command_name,
      excerpt,
    ) -> {
      let receipt_key =
        remote_command_receipt_key(backend_kind, task_remote_id, event_id)
      let receipt =
        seen_receipt(
          projection.command_receipts,
          receipt_key,
          task_remote_id,
          author_id,
          command_name,
          excerpt,
          at_ms,
        )
      Projection(
        ..projection,
        commands: commands_projection.insert_status(
          projection.commands,
          receipt_key,
          CommandSeen(task_remote_id, author_id, command_name, excerpt, at_ms),
        ),
        command_receipts: dict.insert(
          projection.command_receipts,
          receipt_key,
          receipt,
        ),
      )
    }
    record.RemoteCommandStarted(
      backend_kind,
      event_id,
      task_remote_id,
      command_name,
    ) -> {
      let receipt_key =
        remote_command_receipt_key(backend_kind, task_remote_id, event_id)
      let receipt =
        started_receipt(
          projection.command_receipts,
          receipt_key,
          task_remote_id,
          command_name,
          at_ms,
        )
      Projection(
        ..projection,
        commands: commands_projection.insert_status(
          projection.commands,
          receipt_key,
          CommandStarted(task_remote_id, command_name, at_ms),
        ),
        command_receipts: dict.insert(
          projection.command_receipts,
          receipt_key,
          receipt,
        ),
      )
    }
    record.RemoteCommandCompleted(
      backend_kind,
      event_id,
      task_remote_id,
      status,
      message_excerpt,
    ) -> {
      let receipt_key =
        remote_command_receipt_key(backend_kind, task_remote_id, event_id)
      let receipt =
        completed_receipt(
          projection.command_receipts,
          receipt_key,
          task_remote_id,
          status,
          message_excerpt,
          at_ms,
        )
      Projection(
        ..projection,
        commands: commands_projection.insert_status(
          projection.commands,
          receipt_key,
          CommandCompleted(task_remote_id, status, message_excerpt, at_ms),
        ),
        command_receipts: dict.insert(
          projection.command_receipts,
          receipt_key,
          receipt,
        ),
      )
    }
    record.RemoteCommandAcked(backend_kind, event_id, task_remote_id) -> {
      let receipt_key =
        remote_command_receipt_key(backend_kind, task_remote_id, event_id)
      let receipt =
        acked_receipt(
          projection.command_receipts,
          receipt_key,
          task_remote_id,
          at_ms,
        )
      Projection(
        ..projection,
        commands: commands_projection.insert_status(
          projection.commands,
          receipt_key,
          CommandAcked(task_remote_id, at_ms),
        ),
        command_receipts: dict.insert(
          projection.command_receipts,
          receipt_key,
          receipt,
        ),
      )
    }
    record.ScheduledJobDue(..) ->
      apply_scheduled_record(projection, ledger_record)
    record.ScheduledJobSkipped(..) ->
      apply_scheduled_record(projection, ledger_record)
    record.ScheduledRunPending(..) ->
      apply_scheduled_record(projection, ledger_record)
    record.ScheduledRunPendingBlocked(..) ->
      apply_scheduled_record(projection, ledger_record)
    record.ScheduledRunPendingCancelled(..) ->
      apply_scheduled_record(projection, ledger_record)
    record.ScheduledRunStarted(..) ->
      apply_scheduled_record(projection, ledger_record)
    record.ScheduledRunSucceeded(..) ->
      apply_scheduled_record(projection, ledger_record)
    record.ScheduledRunFailed(..) ->
      apply_scheduled_record(projection, ledger_record)
    record.ScheduledRunRetryScheduled(..) ->
      apply_scheduled_record(projection, ledger_record)
    record.ScheduledRunRetryCancelled(..) ->
      apply_scheduled_record(projection, ledger_record)
    record.ScheduledFailureReported(..) ->
      apply_scheduled_record(projection, ledger_record)
    record.ScheduledFailureReportFailed(..) ->
      apply_scheduled_record(projection, ledger_record)
    record.OutboxPending(outbox_id, issue_id, outbox_kind, dedupe_key) ->
      Projection(
        ..projection,
        outbox: outbox_projection.insert_status(
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
        outbox: outbox_projection.insert_status(
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
    record.OutboxPendingV2WithTask(
      outbox_id,
      task_ref,
      outbox_kind,
      dedupe_key,
      payload_json,
    ) ->
      Projection(
        ..projection,
        outbox: outbox_projection.insert_status(
          projection.outbox,
          outbox_id,
          OutboxPendingV2WithTask(
            task_ref,
            outbox_kind,
            dedupe_key,
            payload_json,
            at_ms,
          ),
        ),
      )
    record.OutboxAttempted(
      outbox_id,
      issue_id,
      outbox_kind,
      dedupe_key,
      payload_json,
      attempt_count,
    ) ->
      Projection(
        ..projection,
        outbox: outbox_projection.insert_status(
          projection.outbox,
          outbox_id,
          OutboxAttempted(
            issue_id,
            outbox_kind,
            dedupe_key,
            payload_json,
            attempt_count,
            at_ms,
          ),
        ),
      )
    record.OutboxAttemptedWithTask(
      outbox_id,
      task_ref,
      outbox_kind,
      dedupe_key,
      payload_json,
      attempt_count,
    ) ->
      Projection(
        ..projection,
        outbox: outbox_projection.insert_status(
          projection.outbox,
          outbox_id,
          OutboxAttemptedWithTask(
            task_ref,
            outbox_kind,
            dedupe_key,
            payload_json,
            attempt_count,
            at_ms,
          ),
        ),
      )
    record.OutboxRetryScheduled(
      outbox_id,
      issue_id,
      outbox_kind,
      dedupe_key,
      payload_json,
      error_code,
      attempt_count,
      next_attempt_at_ms,
    ) ->
      Projection(
        ..projection,
        outbox: outbox_projection.insert_status(
          projection.outbox,
          outbox_id,
          OutboxRetryScheduled(
            issue_id,
            outbox_kind,
            dedupe_key,
            payload_json,
            error_code,
            attempt_count,
            next_attempt_at_ms,
            at_ms,
          ),
        ),
      )
    record.OutboxRetryScheduledWithTask(
      outbox_id,
      task_ref,
      outbox_kind,
      dedupe_key,
      payload_json,
      error_code,
      attempt_count,
      next_attempt_at_ms,
    ) ->
      Projection(
        ..projection,
        outbox: outbox_projection.insert_status(
          projection.outbox,
          outbox_id,
          OutboxRetryScheduledWithTask(
            task_ref,
            outbox_kind,
            dedupe_key,
            payload_json,
            error_code,
            attempt_count,
            next_attempt_at_ms,
            at_ms,
          ),
        ),
      )
    record.OutboxCompleted(outbox_id, issue_id, outbox_kind) ->
      Projection(
        ..projection,
        outbox: outbox_projection.insert_status(
          projection.outbox,
          outbox_id,
          OutboxCompleted(issue_id, outbox_kind, at_ms),
        ),
      )
    record.OutboxCompletedWithTask(outbox_id, task_ref, outbox_kind) ->
      Projection(
        ..projection,
        outbox: outbox_projection.insert_status(
          projection.outbox,
          outbox_id,
          OutboxCompletedWithTask(task_ref, outbox_kind, at_ms),
        ),
      )
    record.OutboxFailed(outbox_id, issue_id, outbox_kind, error_code) ->
      Projection(
        ..projection,
        outbox: outbox_projection.insert_status(
          projection.outbox,
          outbox_id,
          OutboxFailed(issue_id, outbox_kind, error_code, at_ms),
        ),
      )
    record.OutboxFailedWithTask(outbox_id, task_ref, outbox_kind, error_code) ->
      Projection(
        ..projection,
        outbox: outbox_projection.insert_status(
          projection.outbox,
          outbox_id,
          OutboxFailedWithTask(task_ref, outbox_kind, error_code, at_ms),
        ),
      )
    record.OutboxPermanentlyFailed(
      outbox_id,
      issue_id,
      outbox_kind,
      error_code,
      attempt_count,
    ) ->
      Projection(
        ..projection,
        outbox: outbox_projection.insert_status(
          projection.outbox,
          outbox_id,
          OutboxPermanentlyFailed(
            issue_id,
            outbox_kind,
            error_code,
            attempt_count,
            at_ms,
          ),
        ),
      )
    record.OutboxPermanentlyFailedWithTask(
      outbox_id,
      task_ref,
      outbox_kind,
      error_code,
      attempt_count,
    ) ->
      Projection(
        ..projection,
        outbox: outbox_projection.insert_status(
          projection.outbox,
          outbox_id,
          OutboxPermanentlyFailedWithTask(
            task_ref,
            outbox_kind,
            error_code,
            attempt_count,
            at_ms,
          ),
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
  Projection(
    ..projection,
    workstreams: workstreams_projection.update_status(
      projection.workstreams,
      workstream_id,
      empty_workstream_status,
      update,
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
  projection.scheduled_jobs
  |> scheduled_jobs_to_local
  |> scheduled_projection.status_for(job_id)
  |> result.map(local_scheduled_status_to_parent)
}

pub fn scheduled_statuses(projection: Projection) -> List(ScheduledJobStatus) {
  projection.scheduled_jobs
  |> scheduled_jobs_to_local
  |> scheduled_projection.statuses
  |> list.map(local_scheduled_status_to_parent)
}

pub fn dispatch_paused(projection: Projection) -> Bool {
  projection.dispatch_paused
}

fn scheduled_jobs_to_local(
  statuses: Dict(String, ScheduledJobStatus),
) -> Dict(String, scheduled_projection.ScheduledJobStatus) {
  statuses
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(job_id, status) = entry
    #(job_id, parent_scheduled_status_to_local(status))
  })
  |> dict.from_list
}

fn local_scheduled_status_to_parent(
  status: scheduled_projection.ScheduledJobStatus,
) -> ScheduledJobStatus {
  ScheduledJobStatus(
    job_id: status.job_id,
    workflow_id: status.workflow_id,
    state: case status.state {
      scheduled_projection.ScheduledIdle -> ScheduledIdle
      scheduled_projection.ScheduledDuePending -> ScheduledDuePending
      scheduled_projection.ScheduledPaused -> ScheduledPaused
      scheduled_projection.ScheduledWaitingForGlobalSlot ->
        ScheduledWaitingForGlobalSlot
      scheduled_projection.ScheduledActive -> ScheduledActive
      scheduled_projection.ScheduledRetryWaiting -> ScheduledRetryWaiting
      scheduled_projection.ScheduledReportRetryWaiting ->
        ScheduledReportRetryWaiting
      scheduled_projection.ScheduledTerminalSuccess -> ScheduledTerminalSuccess
      scheduled_projection.ScheduledTerminalFailure -> ScheduledTerminalFailure
    },
    current_run: case status.current_run {
      Some(run) ->
        Some(ScheduledRunSummary(
          run_id: run.run_id,
          due_at_ms: run.due_at_ms,
          trigger: run.trigger,
          attempt: run.attempt,
          status: run.status,
          reason: run.reason,
          session_id: run.session_id,
          run_root: run.run_root,
        ))
      None -> None
    },
    last_due_at_ms: status.last_due_at_ms,
    last_success_at_ms: status.last_success_at_ms,
    last_success_run_id: status.last_success_run_id,
    last_failure_at_ms: status.last_failure_at_ms,
    last_failure_run_id: status.last_failure_run_id,
    last_failure_reason: status.last_failure_reason,
    retry_count: status.retry_count,
    skipped_overlap_count: status.skipped_overlap_count,
    skipped_catch_up_count: status.skipped_catch_up_count,
    skipped_paused_count: status.skipped_paused_count,
    skipped_capacity_count: status.skipped_capacity_count,
    failure_issue_id: status.failure_issue_id,
    failure_dedupe_key: status.failure_dedupe_key,
    report_retry: case status.report_retry {
      Some(retry) ->
        Some(ScheduledReportRetry(
          run_id: retry.run_id,
          attempt: retry.attempt,
          dedupe_key: retry.dedupe_key,
          error_code: retry.error_code,
          error_message: retry.error_message,
          next_retry_at_ms: retry.next_retry_at_ms,
          generation: retry.generation,
        ))
      None -> None
    },
    recent_run_ids: status.recent_run_ids,
  )
}

fn parent_scheduled_status_to_local(
  status: ScheduledJobStatus,
) -> scheduled_projection.ScheduledJobStatus {
  scheduled_projection.ScheduledJobStatus(
    job_id: status.job_id,
    workflow_id: status.workflow_id,
    state: case status.state {
      ScheduledIdle -> scheduled_projection.ScheduledIdle
      ScheduledDuePending -> scheduled_projection.ScheduledDuePending
      ScheduledPaused -> scheduled_projection.ScheduledPaused
      ScheduledWaitingForGlobalSlot ->
        scheduled_projection.ScheduledWaitingForGlobalSlot
      ScheduledActive -> scheduled_projection.ScheduledActive
      ScheduledRetryWaiting -> scheduled_projection.ScheduledRetryWaiting
      ScheduledReportRetryWaiting ->
        scheduled_projection.ScheduledReportRetryWaiting
      ScheduledTerminalSuccess -> scheduled_projection.ScheduledTerminalSuccess
      ScheduledTerminalFailure -> scheduled_projection.ScheduledTerminalFailure
    },
    current_run: case status.current_run {
      Some(run) ->
        Some(scheduled_projection.ScheduledRunSummary(
          run_id: run.run_id,
          due_at_ms: run.due_at_ms,
          trigger: run.trigger,
          attempt: run.attempt,
          status: run.status,
          reason: run.reason,
          session_id: run.session_id,
          run_root: run.run_root,
        ))
      None -> None
    },
    last_due_at_ms: status.last_due_at_ms,
    last_success_at_ms: status.last_success_at_ms,
    last_success_run_id: status.last_success_run_id,
    last_failure_at_ms: status.last_failure_at_ms,
    last_failure_run_id: status.last_failure_run_id,
    last_failure_reason: status.last_failure_reason,
    retry_count: status.retry_count,
    skipped_overlap_count: status.skipped_overlap_count,
    skipped_catch_up_count: status.skipped_catch_up_count,
    skipped_paused_count: status.skipped_paused_count,
    skipped_capacity_count: status.skipped_capacity_count,
    failure_issue_id: status.failure_issue_id,
    failure_dedupe_key: status.failure_dedupe_key,
    report_retry: case status.report_retry {
      Some(retry) ->
        Some(scheduled_projection.ScheduledReportRetry(
          run_id: retry.run_id,
          attempt: retry.attempt,
          dedupe_key: retry.dedupe_key,
          error_code: retry.error_code,
          error_message: retry.error_message,
          next_retry_at_ms: retry.next_retry_at_ms,
          generation: retry.generation,
        ))
      None -> None
    },
    recent_run_ids: status.recent_run_ids,
  )
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

fn apply_scheduled_record(
  projection: Projection,
  ledger_record: record.LedgerRecord,
) -> Projection {
  case
    scheduled_projection.apply_record(
      scheduled_jobs_to_local(projection.scheduled_jobs),
      ledger_record,
    )
  {
    Ok(status) ->
      update_scheduled_job(projection, local_scheduled_status_to_parent(status))
    Error(Nil) -> projection
  }
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

pub fn remote_command_receipt_key(
  backend_kind: String,
  task_remote_id: String,
  event_id: String,
) -> String {
  case backend_kind {
    "linear" -> event_id
    _ ->
      encode_identity_component(backend_kind)
      <> "|"
      <> encode_identity_component(task_remote_id)
      <> "\u{001f}"
      <> event_id
  }
}

fn encode_identity_component(value: String) -> String {
  int.to_string(string.length(value)) <> ":" <> value
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
    Ok(CommandReceiptUnseen) | Error(Nil) ->
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
    Error(Nil) ->
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
  steps_projection.attempt_key(run_id, step_id, attempt_index)
}

pub fn step_recovery_key(
  run_id: String,
  step_id: String,
  failed_attempt_index: Int,
  recovery_attempt_number: Int,
) -> String {
  steps_projection.recovery_key(
    run_id,
    step_id,
    failed_attempt_index,
    recovery_attempt_number,
  )
}

fn apply_step_recovery_finished(
  projection: Projection,
  at_ms: Int,
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
) -> Projection {
  let key =
    step_recovery_key(
      run_id,
      step_id,
      failed_attempt_index,
      recovery_attempt_number,
    )
  let #(model, prompt_ref, started_at_ms) = case
    dict.get(projection.step_recoveries, key)
  {
    Ok(StepRecoveryStartedStatus(
      model: model,
      prompt_ref: prompt_ref,
      started_at_ms: started_at_ms,
      ..,
    )) -> #(model, prompt_ref, started_at_ms)
    Ok(StepRecoveryFinishedStatus(
      model: model,
      prompt_ref: prompt_ref,
      started_at_ms: started_at_ms,
      ..,
    )) -> #(model, prompt_ref, started_at_ms)
    Error(Nil) -> #(None, "", 0)
  }
  Projection(
    ..projection,
    step_recoveries: dict.insert(
      projection.step_recoveries,
      key,
      StepRecoveryFinishedStatus(
        run_id: run_id,
        workflow_id: workflow_id,
        step_id: step_id,
        failed_attempt_index: failed_attempt_index,
        recovery_attempt_number: recovery_attempt_number,
        recovery_session_id: recovery_session_id,
        model: model,
        prompt_ref: prompt_ref,
        result: result,
        summary: summary,
        reason: reason,
        retry_attempt_index: retry_attempt_index,
        started_at_ms: started_at_ms,
        finished_at_ms: at_ms,
      ),
    ),
  )
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
  steps_projection.session_fact_values(
    status_workflow_id,
    status_workspace_name,
    status_workspace_path,
    fact_workflow_id,
    fact_workspace_name,
    fact_workspace_path,
    session_id,
    session_file,
    current_count,
  )
}

pub fn next_attempt_index(
  projection: Projection,
  run_id: String,
  step_id: String,
) -> Int {
  steps_projection.next_attempt_index(
    projection.step_attempts,
    attempt_identity,
    run_id,
    step_id,
  )
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
  workflow_runs_projection.active_entries(projection.workflow_runs, fn(status) {
    case status {
      WorkflowRunActive(..) -> True
      _ -> False
    }
  })
}

pub fn has_workflow_run(projection: Projection, run_id: String) -> Bool {
  workflow_runs_projection.has_run(projection.workflow_runs, run_id)
}

pub fn workflow_run(
  projection: Projection,
  run_id: String,
) -> Result(WorkflowRunStatus, Nil) {
  dict.get(projection.workflow_runs, run_id)
}

pub fn workflow_input_manifest(
  projection: Projection,
  run_id: String,
) -> Option(WorkflowContractManifestRef) {
  workflow_runs_projection.workflow_input_manifest(
    projection.workflow_input_manifests,
    run_id,
  )
}

pub fn workflow_output_manifest(
  projection: Projection,
  run_id: String,
) -> Option(WorkflowContractManifestRef) {
  workflow_runs_projection.workflow_output_manifest(
    projection.workflow_output_manifests,
    run_id,
  )
}

pub fn latest_workflow_repair(
  projection: Projection,
  run_id: String,
) -> Option(WorkflowRepairStatus) {
  workflow_runs_projection.latest_workflow_repair(
    projection.workflow_repairs,
    run_id,
  )
}

pub fn publication_attempts_for_run(
  projection: Projection,
  run_id: String,
  publication_id: String,
) -> List(PublicationAttempt) {
  publication_projection.attempts_for(
    projection.publication_attempts,
    run_id,
    publication_id,
  )
}

pub fn latest_publication_for_run(
  projection: Projection,
  run_id: String,
  publication_id: String,
) -> Result(PublicationAttempt, Nil) {
  publication_projection.latest_for(
    projection.publication_attempts,
    run_id,
    publication_id,
  )
}

pub fn latest_publication_for_series(
  projection: Projection,
  series_id: String,
) -> Result(PublicationAttempt, Nil) {
  publication_projection.latest_for_series(
    projection.publication_latest_by_series,
    series_id,
  )
}

pub fn publication_ids_for_run(
  projection: Projection,
  run_id: String,
) -> List(String) {
  publication_projection.publication_ids_for_run(
    projection.publication_attempts,
    run_id,
    fn(attempt) { attempt.publication_id },
  )
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

fn preserve_or_insert_workflow_task_ref(
  refs: Dict(String, record.TaskRefFields),
  run_id: String,
  fallback: record.TaskRefFields,
) -> Dict(String, record.TaskRefFields) {
  case dict.get(refs, run_id) {
    Ok(existing) ->
      case
        existing.task_backend_kind != "linear"
        && fallback.task_backend_kind == "linear"
      {
        True -> refs
        False -> dict.insert(refs, run_id, fallback)
      }
    Error(Nil) -> dict.insert(refs, run_id, fallback)
  }
}

fn workflow_run_root(projection: Projection, run_id: String) -> String {
  workflow_runs_projection.run_root(
    projection.workflow_runs,
    run_id,
    fn(status) {
      case status {
        WorkflowRunActive(run_root: run_root, ..) -> run_root
        WorkflowRunFinished(run_root: run_root, ..) -> run_root
        WorkflowRunInterrupted(run_root: run_root, ..) -> run_root
        WorkflowRunSuperseded(run_root: run_root, ..) -> run_root
      }
    },
  )
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

pub fn known_task_refs(projection: Projection) -> List(record.TaskRefFields) {
  []
  |> append_unique_task_refs(
    run_issue_ids(projection.runs) |> list.map(linear_task_ref_for_issue_id),
  )
  |> append_unique_task_refs(
    workflow_run_issue_ids(projection.workflow_runs)
    |> list.map(linear_task_ref_for_issue_id),
  )
  |> append_unique_task_refs(dict.values(projection.workflow_task_refs))
  |> append_unique_task_refs(
    dict.keys(projection.retries) |> list.map(linear_task_ref_for_issue_id),
  )
  |> append_unique_task_refs(
    dict.keys(projection.parked_issues)
    |> list.map(linear_task_ref_for_issue_id),
  )
  |> append_unique_task_refs(command_task_refs(projection.commands))
  |> append_unique_task_refs(outbox_task_refs(projection.outbox))
  |> append_unique_task_refs(
    dict.keys(projection.issue_counters)
    |> list.map(linear_task_ref_for_issue_id),
  )
  |> append_unique_task_refs(
    dict.keys(projection.known_workspaces)
    |> list.map(linear_task_ref_for_issue_id),
  )
}

pub fn recovery_task_refs(
  projection: Projection,
) -> List(record.TaskRefFields) {
  []
  |> append_unique_task_refs(recovery_run_task_refs(projection.runs))
  |> append_unique_task_refs(recovery_workflow_task_refs(projection))
  |> append_unique_task_refs(retry_task_refs(projection))
  |> append_unique_task_refs(parked_task_refs(projection.parked_issues))
  |> append_unique_task_refs(recovery_issue_counter_task_refs(projection))
}

pub fn known_workspace_for_issue(
  projection: Projection,
  issue_id: String,
) -> Result(String, Nil) {
  issue_recovery_projection.known_workspace_for_issue(
    projection.known_workspaces,
    issue_id,
    fn(workspace) { workspace.workspace_path },
  )
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
    Error(Nil) -> orchestrator_state.new_issue_counter()
  }
}

pub fn counter_has_source_run(
  projection: Projection,
  issue_id: String,
  run_id: String,
) -> Bool {
  issue_recovery_projection.counter_has_source_run(
    projection.issue_counters,
    issue_id,
    run_id,
    fn(counter) { counter.source_run_ids },
  )
}

pub fn workflow_task_ref(
  projection: Projection,
  run_id: String,
) -> Result(record.TaskRefFields, Nil) {
  dict.get(projection.workflow_task_refs, run_id)
}

pub fn workflow_run_provenance(
  projection: Projection,
  run_id: String,
) -> Result(WorkflowRunProvenance, Nil) {
  dict.get(projection.workflow_run_provenances, run_id)
}

pub fn command_receipt(
  projection: Projection,
  comment_id: String,
) -> CommandReceiptState {
  commands_projection.command_receipt(
    projection.command_receipts,
    comment_id,
    CommandReceiptUnseen,
  )
}

pub fn retry_due_at_ms(status: RetryStatus) -> Result(Int, Nil) {
  issue_recovery_projection.retry_due_at_ms(status, fn(status) {
    case status {
      RetryScheduled(_, delay_ms, _, _, scheduled_at_ms) ->
        Ok(#(delay_ms, scheduled_at_ms))
      RetryCancelled(_, _, _) -> Error(Nil)
    }
  })
}

pub fn pending_outbox_replays(
  projection: Projection,
) -> Result(List(OutboxReplay), PendingOutboxError) {
  pending_outbox_replays_at(projection, projection_latest_at_ms(projection))
}

pub fn pending_outbox_replays_at(
  projection: Projection,
  now_ms: Int,
) -> Result(List(OutboxReplay), PendingOutboxError) {
  let entries =
    projection.outbox
    |> dict.to_list
    |> list.sort(by: compare_outbox_entries_by_time)
  pending_outbox_replays_loop(entries, now_ms, [])
}

pub fn to_json(projection: Projection) -> json.Json {
  json.object([
    #("schema_version", json.int(record.schema_version)),
    #("kind", json.string("projection_snapshot")),
    #("dispatch_paused", json.bool(projection.dispatch_paused)),
    #("runs", json.array(dict.to_list(projection.runs), of: run_entry_to_json)),
    #(
      "workflow_runs",
      json.array(
        dict.to_list(projection.workflow_runs),
        of: workflow_run_entry_to_json,
      ),
    ),
    #(
      "workflow_run_provenances",
      json.array(
        dict.to_list(projection.workflow_run_provenances),
        of: workflow_run_provenance_entry_to_json,
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
      "publication_attempts",
      json.array(
        dict.to_list(projection.publication_attempts),
        of: publication_attempts_entry_to_json,
      ),
    ),
    #(
      "publication_latest_by_series",
      json.array(
        dict.to_list(projection.publication_latest_by_series),
        of: publication_latest_series_entry_to_json,
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
      "step_recoveries",
      json.array(
        dict.to_list(projection.step_recoveries),
        of: step_recovery_entry_to_json,
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
        projection.scheduled_jobs
          |> dict.to_list
          |> list.map(fn(entry) {
            let #(job_id, status) = entry
            #(job_id, parent_scheduled_status_to_local(status))
          }),
        of: scheduled_projection.entry_to_json,
      ),
    ),
  ])
}

pub fn to_string(projection: Projection) -> String {
  projection |> to_json |> json.to_string
}

pub fn decode_string(contents: String) -> Result(Projection, DecodeError) {
  case json.parse(contents, snapshot_header_decoder()) {
    Ok(#(version, _)) if version != record.schema_version ->
      Error(UnsupportedSnapshotVersion(version))
    Ok(_) -> decode_current_snapshot(contents, None)
    Error(header_error) -> decode_current_snapshot(contents, Some(header_error))
  }
}

pub fn describe_decode_error(error: DecodeError) -> String {
  case error {
    UnsupportedSnapshotVersion(version) ->
      "unsupported schema version " <> int.to_string(version)
    MalformedProjectionSnapshot(header_error, snapshot_error) ->
      "malformed projection snapshot:"
      <> json_decode_error.to_string(snapshot_error)
      <> optional_json_decode_error("header", header_error)
  }
}

fn decode_current_snapshot(
  contents: String,
  header_error: Option(json.DecodeError),
) -> Result(Projection, DecodeError) {
  case json.parse(contents, snapshot_decoder()) {
    Ok(fields) ->
      Ok(Projection(
        runs: fields.runs
          |> list.map(fn(entry) {
            let RunSnapshot(run_id, status) = entry
            #(run_id, status)
          })
          |> dict.from_list,
        dispatch_paused: fields.dispatch_paused,
        workflow_runs: fields.workflow_runs
          |> list.map(fn(entry) {
            let WorkflowRunSnapshot(run_id, status) = entry
            #(run_id, status)
          })
          |> dict.from_list,
        workflow_run_provenances: fields.workflow_run_provenances
          |> list.map(fn(entry) {
            let WorkflowRunProvenanceSnapshot(run_id, provenance) = entry
            #(run_id, provenance)
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
        publication_attempts: fields.publication_attempts
          |> list.map(fn(entry) {
            let PublicationAttemptSnapshot(key, attempts) = entry
            #(key, attempts)
          })
          |> dict.from_list,
        publication_latest_by_series: fields.publication_latest_by_series
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
        step_recoveries: fields.step_recoveries
          |> list.map(fn(entry) {
            let StepRecoverySnapshot(key, status) = entry
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
            let #(job_id, status) = entry
            #(job_id, local_scheduled_status_to_parent(status))
          })
          |> dict.from_list,
      ))
    Error(error) -> Error(MalformedProjectionSnapshot(header_error, error))
  }
}

fn optional_json_decode_error(
  label: String,
  error: Option(json.DecodeError),
) -> String {
  case error {
    Some(error) -> " " <> label <> "=" <> json_decode_error.to_string(error)
    None -> ""
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

fn workflow_run_provenance_entry_to_json(
  entry: #(String, WorkflowRunProvenance),
) -> json.Json {
  let #(run_id, provenance) = entry
  let record.TaskRefFields(
    task_backend_kind,
    task_remote_id,
    task_key,
    task_url,
  ) = provenance.task_ref
  json.object([
    #("run_id", json.string(run_id)),
    #("workflow_id", json.string(provenance.workflow_id)),
    #("workflow_fingerprint", json.string(provenance.workflow_fingerprint)),
    #("issue_id", json.string(provenance.issue_id)),
    #("issue_identifier", json.string(provenance.issue_identifier)),
    #("issue_fingerprint", json.string(provenance.issue_fingerprint)),
    #("observed_updated_at_ms", json.int(provenance.observed_updated_at_ms)),
    #("run_root", json.string(provenance.run_root)),
    #("task_backend_kind", json.string(task_backend_kind)),
    #("task_remote_id", json.string(task_remote_id)),
    #("task_key", option_string_to_json(task_key)),
    #("task_url", option_string_to_json(task_url)),
  ])
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

fn publication_attempt_to_json(attempt: PublicationAttempt) -> json.Json {
  json.object([
    #("run_id", json.string(attempt.run_id)),
    #("workflow_id", json.string(attempt.workflow_id)),
    #("publication_id", json.string(attempt.publication_id)),
    #("series_id", json.string(attempt.series_id)),
    #("attempt_id", json.string(attempt.attempt_id)),
    #("status", json.string(attempt.status)),
    #("required", json.bool(attempt.required)),
    #("retryable", json.bool(attempt.retryable)),
    #("retry_execution_available", json.bool(attempt.retry_execution_available)),
    #("version_id", option_string_to_json(attempt.version_id)),
    #("manifest_ref", option_string_to_json(attempt.manifest_ref)),
    #("manifest_sha256", option_string_to_json(attempt.manifest_sha256)),
    #("manifest_bytes", option_int_to_json(attempt.manifest_bytes)),
    #("error_code", option_string_to_json(attempt.error_code)),
    #("error_message", option_string_to_json(attempt.error_message)),
    #("recorded_at_ms", json.int(attempt.recorded_at_ms)),
  ])
}

fn publication_attempts_entry_to_json(
  entry: #(String, List(PublicationAttempt)),
) -> json.Json {
  let #(key, attempts) = entry
  json.object([
    #("key", json.string(key)),
    #("attempts", json.array(attempts, of: publication_attempt_to_json)),
  ])
}

fn publication_latest_series_entry_to_json(
  entry: #(String, PublicationAttempt),
) -> json.Json {
  let #(series_id, attempt) = entry
  json.object([
    #("series_id", json.string(series_id)),
    #("attempt", publication_attempt_to_json(attempt)),
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

fn task_ref_entries(
  task_ref: record.TaskRefFields,
) -> List(#(String, json.Json)) {
  [
    #("task_backend_kind", json.string(task_ref.task_backend_kind)),
    #("task_remote_id", json.string(task_ref.task_remote_id)),
    #("task_key", option_string_to_json(task_ref.task_key)),
    #("task_url", option_string_to_json(task_ref.task_url)),
  ]
}

fn step_recovery_entry_to_json(
  entry: #(String, StepRecoveryStatus),
) -> json.Json {
  let #(key, status) = entry
  case status {
    StepRecoveryStartedStatus(
      run_id,
      workflow_id,
      step_id,
      failed_attempt_index,
      recovery_attempt_number,
      recovery_session_id,
      model,
      prompt_ref,
      started_at_ms,
    ) ->
      json.object([
        #("key", json.string(key)),
        #("status", json.string("started")),
        #("run_id", json.string(run_id)),
        #("workflow_id", json.string(workflow_id)),
        #("step_id", json.string(step_id)),
        #("failed_attempt_index", json.int(failed_attempt_index)),
        #("recovery_attempt_number", json.int(recovery_attempt_number)),
        #("recovery_session_id", json.string(recovery_session_id)),
        #("model", option_string_to_json(model)),
        #("prompt_ref", json.string(prompt_ref)),
        #("started_at_ms", json.int(started_at_ms)),
      ])
    StepRecoveryFinishedStatus(
      run_id,
      workflow_id,
      step_id,
      failed_attempt_index,
      recovery_attempt_number,
      recovery_session_id,
      model,
      prompt_ref,
      result,
      summary,
      reason,
      retry_attempt_index,
      started_at_ms,
      finished_at_ms,
    ) ->
      json.object([
        #("key", json.string(key)),
        #("status", json.string("finished")),
        #("run_id", json.string(run_id)),
        #("workflow_id", json.string(workflow_id)),
        #("step_id", json.string(step_id)),
        #("failed_attempt_index", json.int(failed_attempt_index)),
        #("recovery_attempt_number", json.int(recovery_attempt_number)),
        #("recovery_session_id", json.string(recovery_session_id)),
        #("model", option_string_to_json(model)),
        #("prompt_ref", json.string(prompt_ref)),
        #("result", json.string(result)),
        #("summary", json.string(summary)),
        #("reason", json.string(reason)),
        #("retry_attempt_index", option_int_to_json(retry_attempt_index)),
        #("started_at_ms", json.int(started_at_ms)),
        #("finished_at_ms", json.int(finished_at_ms)),
      ])
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
      commands_projection.seen_status_entry_to_json(
        comment_id,
        issue_id,
        author_id,
        command_name,
        excerpt,
        seen_at_ms,
      )
    CommandStarted(issue_id, command_name, started_at_ms) ->
      commands_projection.started_status_entry_to_json(
        comment_id,
        issue_id,
        command_name,
        started_at_ms,
      )
    CommandCompleted(issue_id, status, message_excerpt, completed_at_ms) ->
      commands_projection.completed_status_entry_to_json(
        comment_id,
        issue_id,
        status,
        message_excerpt,
        completed_at_ms,
      )
    CommandAcked(issue_id, acked_at_ms) ->
      commands_projection.acked_status_entry_to_json(
        comment_id,
        issue_id,
        acked_at_ms,
      )
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
      commands_projection.unseen_receipt_entry_to_json(comment_id)
    CommandReceiptSeen(issue_id, author_id, command_name, excerpt, seen_at_ms) ->
      commands_projection.seen_receipt_entry_to_json(
        comment_id,
        issue_id,
        author_id,
        command_name,
        excerpt,
        seen_at_ms,
      )
    CommandReceiptStarted(
      issue_id,
      author_id,
      command_name,
      excerpt,
      seen_at_ms,
      started_at_ms,
    ) ->
      commands_projection.started_receipt_entry_to_json(
        comment_id,
        issue_id,
        author_id,
        command_name,
        excerpt,
        seen_at_ms,
        started_at_ms,
      )
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
      commands_projection.completed_receipt_entry_to_json(
        comment_id,
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
    CommandReceiptAcked(issue_id, acked_at_ms) ->
      commands_projection.acked_receipt_entry_to_json(
        comment_id,
        issue_id,
        acked_at_ms,
      )
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
    OutboxPendingV2WithTask(
      task_ref,
      outbox_kind,
      dedupe_key,
      payload_json,
      pending_at_ms,
    ) ->
      json.object(list.append(
        [
          #("outbox_id", json.string(outbox_id)),
          #("status", json.string("pending_v2")),
        ],
        list.append(task_ref_entries(task_ref), [
          #("outbox_kind", json.string(outbox_kind)),
          #("dedupe_key", json.string(dedupe_key)),
          #("payload_json", json.string(payload_json)),
          #("pending_at_ms", json.int(pending_at_ms)),
        ]),
      ))
    OutboxAttempted(
      issue_id,
      outbox_kind,
      dedupe_key,
      payload_json,
      attempt_count,
      attempted_at_ms,
    ) ->
      json.object([
        #("outbox_id", json.string(outbox_id)),
        #("status", json.string("attempted")),
        #("issue_id", json.string(issue_id)),
        #("outbox_kind", json.string(outbox_kind)),
        #("dedupe_key", json.string(dedupe_key)),
        #("payload_json", json.string(payload_json)),
        #("attempt_count", json.int(attempt_count)),
        #("attempted_at_ms", json.int(attempted_at_ms)),
      ])
    OutboxAttemptedWithTask(
      task_ref,
      outbox_kind,
      dedupe_key,
      payload_json,
      attempt_count,
      attempted_at_ms,
    ) ->
      json.object(list.append(
        [
          #("outbox_id", json.string(outbox_id)),
          #("status", json.string("attempted")),
        ],
        list.append(task_ref_entries(task_ref), [
          #("outbox_kind", json.string(outbox_kind)),
          #("dedupe_key", json.string(dedupe_key)),
          #("payload_json", json.string(payload_json)),
          #("attempt_count", json.int(attempt_count)),
          #("attempted_at_ms", json.int(attempted_at_ms)),
        ]),
      ))
    OutboxRetryScheduled(
      issue_id,
      outbox_kind,
      dedupe_key,
      payload_json,
      error_code,
      attempt_count,
      next_attempt_at_ms,
      failed_at_ms,
    ) ->
      json.object([
        #("outbox_id", json.string(outbox_id)),
        #("status", json.string("retry_scheduled")),
        #("issue_id", json.string(issue_id)),
        #("outbox_kind", json.string(outbox_kind)),
        #("dedupe_key", json.string(dedupe_key)),
        #("payload_json", json.string(payload_json)),
        #("error_code", json.string(error_code)),
        #("attempt_count", json.int(attempt_count)),
        #("next_attempt_at_ms", json.int(next_attempt_at_ms)),
        #("failed_at_ms", json.int(failed_at_ms)),
      ])
    OutboxRetryScheduledWithTask(
      task_ref,
      outbox_kind,
      dedupe_key,
      payload_json,
      error_code,
      attempt_count,
      next_attempt_at_ms,
      failed_at_ms,
    ) ->
      json.object(list.append(
        [
          #("outbox_id", json.string(outbox_id)),
          #("status", json.string("retry_scheduled")),
        ],
        list.append(task_ref_entries(task_ref), [
          #("outbox_kind", json.string(outbox_kind)),
          #("dedupe_key", json.string(dedupe_key)),
          #("payload_json", json.string(payload_json)),
          #("error_code", json.string(error_code)),
          #("attempt_count", json.int(attempt_count)),
          #("next_attempt_at_ms", json.int(next_attempt_at_ms)),
          #("failed_at_ms", json.int(failed_at_ms)),
        ]),
      ))
    OutboxCompleted(issue_id, outbox_kind, completed_at_ms) ->
      json.object([
        #("outbox_id", json.string(outbox_id)),
        #("status", json.string("completed")),
        #("issue_id", json.string(issue_id)),
        #("outbox_kind", json.string(outbox_kind)),
        #("completed_at_ms", json.int(completed_at_ms)),
      ])
    OutboxCompletedWithTask(task_ref, outbox_kind, completed_at_ms) ->
      json.object(list.append(
        [
          #("outbox_id", json.string(outbox_id)),
          #("status", json.string("completed")),
        ],
        list.append(task_ref_entries(task_ref), [
          #("outbox_kind", json.string(outbox_kind)),
          #("completed_at_ms", json.int(completed_at_ms)),
        ]),
      ))
    OutboxFailed(issue_id, outbox_kind, error_code, failed_at_ms) ->
      json.object([
        #("outbox_id", json.string(outbox_id)),
        #("status", json.string("failed")),
        #("issue_id", json.string(issue_id)),
        #("outbox_kind", json.string(outbox_kind)),
        #("error_code", json.string(error_code)),
        #("failed_at_ms", json.int(failed_at_ms)),
      ])
    OutboxFailedWithTask(task_ref, outbox_kind, error_code, failed_at_ms) ->
      json.object(list.append(
        [
          #("outbox_id", json.string(outbox_id)),
          #("status", json.string("failed")),
        ],
        list.append(task_ref_entries(task_ref), [
          #("outbox_kind", json.string(outbox_kind)),
          #("error_code", json.string(error_code)),
          #("failed_at_ms", json.int(failed_at_ms)),
        ]),
      ))
    OutboxPermanentlyFailed(
      issue_id,
      outbox_kind,
      error_code,
      attempt_count,
      failed_at_ms,
    ) ->
      json.object([
        #("outbox_id", json.string(outbox_id)),
        #("status", json.string("permanently_failed")),
        #("issue_id", json.string(issue_id)),
        #("outbox_kind", json.string(outbox_kind)),
        #("error_code", json.string(error_code)),
        #("attempt_count", json.int(attempt_count)),
        #("failed_at_ms", json.int(failed_at_ms)),
      ])
    OutboxPermanentlyFailedWithTask(
      task_ref,
      outbox_kind,
      error_code,
      attempt_count,
      failed_at_ms,
    ) ->
      json.object(list.append(
        [
          #("outbox_id", json.string(outbox_id)),
          #("status", json.string("permanently_failed")),
        ],
        list.append(task_ref_entries(task_ref), [
          #("outbox_kind", json.string(outbox_kind)),
          #("error_code", json.string(error_code)),
          #("attempt_count", json.int(attempt_count)),
          #("failed_at_ms", json.int(failed_at_ms)),
        ]),
      ))
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

fn snapshot_decoder() -> decode.Decoder(SnapshotFields) {
  use schema_version <- decode.field("schema_version", decode.int)
  use kind <- decode.field("kind", decode.string)
  use dispatch_paused <- decode.optional_field(
    "dispatch_paused",
    False,
    decode.bool,
  )
  use runs <- decode.field("runs", decode.list(of: run_snapshot_decoder()))
  use workflow_runs <- decode.optional_field(
    "workflow_runs",
    [],
    decode.list(of: workflow_run_snapshot_decoder()),
  )
  use workflow_run_provenances <- decode.optional_field(
    "workflow_run_provenances",
    [],
    decode.list(of: workflow_run_provenance_snapshot_decoder()),
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
  use publication_attempts <- decode.optional_field(
    "publication_attempts",
    [],
    decode.list(of: publication_attempt_snapshot_decoder()),
  )
  use publication_latest_by_series <- decode.optional_field(
    "publication_latest_by_series",
    [],
    decode.list(of: publication_latest_series_snapshot_decoder()),
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
  use step_recoveries <- decode.optional_field(
    "step_recoveries",
    [],
    decode.list(of: step_recovery_snapshot_decoder()),
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
    decode.list(of: scheduled_projection.snapshot_decoder()),
  )
  case
    schema_version == record.schema_version && kind == "projection_snapshot"
  {
    True ->
      decode.success(SnapshotFields(
        runs,
        dispatch_paused,
        workflow_runs,
        workflow_run_provenances,
        workflow_task_refs,
        workflow_input_manifests,
        workflow_output_manifests,
        publication_attempts,
        publication_latest_by_series,
        workflow_repairs,
        step_attempts,
        step_recoveries,
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
          False,
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

fn workflow_run_provenance_snapshot_decoder() -> decode.Decoder(
  WorkflowRunProvenanceSnapshot,
) {
  use run_id <- decode.field("run_id", decode.string)
  use workflow_id <- decode.field("workflow_id", decode.string)
  use workflow_fingerprint <- decode.field(
    "workflow_fingerprint",
    decode.string,
  )
  use issue_id <- decode.field("issue_id", decode.string)
  use issue_identifier <- decode.field("issue_identifier", decode.string)
  use issue_fingerprint <- decode.field("issue_fingerprint", decode.string)
  use observed_updated_at_ms <- decode.field(
    "observed_updated_at_ms",
    decode.int,
  )
  use run_root <- decode.optional_field("run_root", "", decode.string)
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
  decode.success(WorkflowRunProvenanceSnapshot(
    run_id,
    WorkflowRunProvenance(
      workflow_id: workflow_id,
      workflow_fingerprint: workflow_fingerprint,
      issue_id: issue_id,
      issue_identifier: issue_identifier,
      issue_fingerprint: issue_fingerprint,
      observed_updated_at_ms: observed_updated_at_ms,
      run_root: run_root,
      task_ref: record.TaskRefFields(
        task_backend_kind: task_backend_kind,
        task_remote_id: task_remote_id,
        task_key: task_key,
        task_url: task_url,
      ),
    ),
  ))
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

fn publication_attempt_decoder() -> decode.Decoder(PublicationAttempt) {
  use run_id <- decode.field("run_id", decode.string)
  use workflow_id <- decode.field("workflow_id", decode.string)
  use publication_id <- decode.field("publication_id", decode.string)
  use series_id <- decode.field("series_id", decode.string)
  use attempt_id <- decode.field("attempt_id", decode.string)
  use status <- decode.field("status", decode.string)
  use required <- decode.field("required", decode.bool)
  use retryable <- decode.field("retryable", decode.bool)
  use retry_execution_available <- decode.field(
    "retry_execution_available",
    decode.bool,
  )
  use version_id <- decode.optional_field(
    "version_id",
    None,
    decode.optional(decode.string),
  )
  use manifest_ref <- decode.optional_field(
    "manifest_ref",
    None,
    decode.optional(decode.string),
  )
  use manifest_sha256 <- decode.optional_field(
    "manifest_sha256",
    None,
    decode.optional(decode.string),
  )
  use manifest_bytes <- decode.optional_field(
    "manifest_bytes",
    None,
    decode.optional(decode.int),
  )
  use error_code <- decode.optional_field(
    "error_code",
    None,
    decode.optional(decode.string),
  )
  use error_message <- decode.optional_field(
    "error_message",
    None,
    decode.optional(decode.string),
  )
  use recorded_at_ms <- decode.field("recorded_at_ms", decode.int)
  decode.success(PublicationAttempt(
    run_id: run_id,
    workflow_id: workflow_id,
    publication_id: publication_id,
    series_id: series_id,
    attempt_id: attempt_id,
    status: status,
    required: required,
    retryable: retryable,
    retry_execution_available: retry_execution_available,
    version_id: version_id,
    manifest_ref: manifest_ref,
    manifest_sha256: manifest_sha256,
    manifest_bytes: manifest_bytes,
    error_code: error_code,
    error_message: error_message,
    recorded_at_ms: recorded_at_ms,
  ))
}

fn publication_attempt_snapshot_decoder() -> decode.Decoder(
  PublicationAttemptSnapshot,
) {
  use key <- decode.field("key", decode.string)
  use attempts <- decode.field(
    "attempts",
    decode.list(of: publication_attempt_decoder()),
  )
  decode.success(PublicationAttemptSnapshot(key, attempts))
}

fn publication_latest_series_snapshot_decoder() -> decode.Decoder(
  #(String, PublicationAttempt),
) {
  use series_id <- decode.field("series_id", decode.string)
  use attempt <- decode.field("attempt", publication_attempt_decoder())
  decode.success(#(series_id, attempt))
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

fn step_recovery_snapshot_decoder() -> decode.Decoder(StepRecoverySnapshot) {
  use key <- decode.field("key", decode.string)
  use status <- decode.field("status", decode.string)
  use run_id <- decode.field("run_id", decode.string)
  use workflow_id <- decode.field("workflow_id", decode.string)
  use step_id <- decode.field("step_id", decode.string)
  use failed_attempt_index <- decode.field("failed_attempt_index", decode.int)
  use recovery_attempt_number <- decode.field(
    "recovery_attempt_number",
    decode.int,
  )
  use recovery_session_id <- decode.field("recovery_session_id", decode.string)
  use model <- decode.optional_field(
    "model",
    None,
    decode.optional(decode.string),
  )
  use prompt_ref <- decode.optional_field("prompt_ref", "", decode.string)
  use started_at_ms <- decode.optional_field("started_at_ms", 0, decode.int)
  case status {
    "started" ->
      decode.success(StepRecoverySnapshot(
        key,
        StepRecoveryStartedStatus(
          run_id: run_id,
          workflow_id: workflow_id,
          step_id: step_id,
          failed_attempt_index: failed_attempt_index,
          recovery_attempt_number: recovery_attempt_number,
          recovery_session_id: recovery_session_id,
          model: model,
          prompt_ref: prompt_ref,
          started_at_ms: started_at_ms,
        ),
      ))
    "finished" -> {
      use result <- decode.field("result", decode.string)
      use summary <- decode.field("summary", decode.string)
      use reason <- decode.field("reason", decode.string)
      use retry_attempt_index <- decode.optional_field(
        "retry_attempt_index",
        None,
        decode.optional(decode.int),
      )
      use finished_at_ms <- decode.optional_field(
        "finished_at_ms",
        0,
        decode.int,
      )
      decode.success(StepRecoverySnapshot(
        key,
        StepRecoveryFinishedStatus(
          run_id: run_id,
          workflow_id: workflow_id,
          step_id: step_id,
          failed_attempt_index: failed_attempt_index,
          recovery_attempt_number: recovery_attempt_number,
          recovery_session_id: recovery_session_id,
          model: model,
          prompt_ref: prompt_ref,
          result: result,
          summary: summary,
          reason: reason,
          retry_attempt_index: retry_attempt_index,
          started_at_ms: started_at_ms,
          finished_at_ms: finished_at_ms,
        ),
      ))
    }
    _ ->
      decode.failure(
        StepRecoverySnapshot(
          "",
          StepRecoveryStartedStatus("", "", "", 0, 0, "", None, "", 0),
        ),
        expected: "StepRecoverySnapshot",
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
  commands_projection.status_snapshot_decoder(
    CommandSeen,
    CommandStarted,
    CommandCompleted,
    CommandAcked,
    CommandAcked("", 0),
  )
  |> decode.map(fn(entry) {
    let #(comment_id, status) = entry
    CommandSnapshot(comment_id, status)
  })
}

fn command_receipt_snapshot_decoder() -> decode.Decoder(CommandReceiptSnapshot) {
  commands_projection.receipt_snapshot_decoder(
    CommandReceiptUnseen,
    CommandReceiptSeen,
    CommandReceiptStarted,
    CommandReceiptCompleted,
    CommandReceiptAcked,
    CommandReceiptUnseen,
  )
  |> decode.map(fn(entry) {
    let #(comment_id, receipt) = entry
    CommandReceiptSnapshot(comment_id, receipt)
  })
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
      use issue_id <- decode.optional_field(
        "issue_id",
        None,
        decode.optional(decode.string),
      )
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
      use task_ref <- outbox_task_ref_from_snapshot_decoder(
        task_backend_kind,
        task_remote_id,
        task_key,
        task_url,
      )
      use outbox_kind <- decode.field("outbox_kind", decode.string)
      use dedupe_key <- decode.field("dedupe_key", decode.string)
      use payload_json <- decode.field("payload_json", decode.string)
      use pending_at_ms <- decode.field("pending_at_ms", decode.int)
      case task_ref, issue_id {
        Some(task_ref), _ ->
          decode.success(OutboxSnapshot(
            outbox_id,
            OutboxPendingV2WithTask(
              task_ref,
              outbox_kind,
              dedupe_key,
              payload_json,
              pending_at_ms,
            ),
          ))
        None, Some(issue_id) ->
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
        None, None -> outbox_snapshot_decode_failure()
      }
    }
    "attempted" -> {
      use issue_id <- decode.optional_field(
        "issue_id",
        None,
        decode.optional(decode.string),
      )
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
      use task_ref <- outbox_task_ref_from_snapshot_decoder(
        task_backend_kind,
        task_remote_id,
        task_key,
        task_url,
      )
      use outbox_kind <- decode.field("outbox_kind", decode.string)
      use dedupe_key <- decode.field("dedupe_key", decode.string)
      use payload_json <- decode.field("payload_json", decode.string)
      use attempt_count <- decode.field("attempt_count", decode.int)
      use attempted_at_ms <- decode.field("attempted_at_ms", decode.int)
      case task_ref, issue_id {
        Some(task_ref), _ ->
          decode.success(OutboxSnapshot(
            outbox_id,
            OutboxAttemptedWithTask(
              task_ref,
              outbox_kind,
              dedupe_key,
              payload_json,
              attempt_count,
              attempted_at_ms,
            ),
          ))
        None, Some(issue_id) ->
          decode.success(OutboxSnapshot(
            outbox_id,
            OutboxAttempted(
              issue_id,
              outbox_kind,
              dedupe_key,
              payload_json,
              attempt_count,
              attempted_at_ms,
            ),
          ))
        None, None -> outbox_snapshot_decode_failure()
      }
    }
    "retry_scheduled" -> {
      use issue_id <- decode.optional_field(
        "issue_id",
        None,
        decode.optional(decode.string),
      )
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
      use task_ref <- outbox_task_ref_from_snapshot_decoder(
        task_backend_kind,
        task_remote_id,
        task_key,
        task_url,
      )
      use outbox_kind <- decode.field("outbox_kind", decode.string)
      use dedupe_key <- decode.field("dedupe_key", decode.string)
      use payload_json <- decode.field("payload_json", decode.string)
      use error_code <- decode.field("error_code", decode.string)
      use attempt_count <- decode.field("attempt_count", decode.int)
      use next_attempt_at_ms <- decode.field("next_attempt_at_ms", decode.int)
      use failed_at_ms <- decode.field("failed_at_ms", decode.int)
      case task_ref, issue_id {
        Some(task_ref), _ ->
          decode.success(OutboxSnapshot(
            outbox_id,
            OutboxRetryScheduledWithTask(
              task_ref,
              outbox_kind,
              dedupe_key,
              payload_json,
              error_code,
              attempt_count,
              next_attempt_at_ms,
              failed_at_ms,
            ),
          ))
        None, Some(issue_id) ->
          decode.success(OutboxSnapshot(
            outbox_id,
            OutboxRetryScheduled(
              issue_id,
              outbox_kind,
              dedupe_key,
              payload_json,
              error_code,
              attempt_count,
              next_attempt_at_ms,
              failed_at_ms,
            ),
          ))
        None, None -> outbox_snapshot_decode_failure()
      }
    }
    "completed" -> {
      use issue_id <- decode.optional_field(
        "issue_id",
        None,
        decode.optional(decode.string),
      )
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
      use task_ref <- outbox_task_ref_from_snapshot_decoder(
        task_backend_kind,
        task_remote_id,
        task_key,
        task_url,
      )
      use outbox_kind <- decode.field("outbox_kind", decode.string)
      use completed_at_ms <- decode.field("completed_at_ms", decode.int)
      case task_ref, issue_id {
        Some(task_ref), _ ->
          decode.success(OutboxSnapshot(
            outbox_id,
            OutboxCompletedWithTask(task_ref, outbox_kind, completed_at_ms),
          ))
        None, Some(issue_id) ->
          decode.success(OutboxSnapshot(
            outbox_id,
            OutboxCompleted(issue_id, outbox_kind, completed_at_ms),
          ))
        None, None -> outbox_snapshot_decode_failure()
      }
    }
    "failed" -> {
      use issue_id <- decode.optional_field(
        "issue_id",
        None,
        decode.optional(decode.string),
      )
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
      use task_ref <- outbox_task_ref_from_snapshot_decoder(
        task_backend_kind,
        task_remote_id,
        task_key,
        task_url,
      )
      use outbox_kind <- decode.field("outbox_kind", decode.string)
      use error_code <- decode.field("error_code", decode.string)
      use failed_at_ms <- decode.field("failed_at_ms", decode.int)
      case task_ref, issue_id {
        Some(task_ref), _ ->
          decode.success(OutboxSnapshot(
            outbox_id,
            OutboxFailedWithTask(
              task_ref,
              outbox_kind,
              error_code,
              failed_at_ms,
            ),
          ))
        None, Some(issue_id) ->
          decode.success(OutboxSnapshot(
            outbox_id,
            OutboxFailed(issue_id, outbox_kind, error_code, failed_at_ms),
          ))
        None, None -> outbox_snapshot_decode_failure()
      }
    }
    "permanently_failed" -> {
      use issue_id <- decode.optional_field(
        "issue_id",
        None,
        decode.optional(decode.string),
      )
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
      use task_ref <- outbox_task_ref_from_snapshot_decoder(
        task_backend_kind,
        task_remote_id,
        task_key,
        task_url,
      )
      use outbox_kind <- decode.field("outbox_kind", decode.string)
      use error_code <- decode.field("error_code", decode.string)
      use attempt_count <- decode.field("attempt_count", decode.int)
      use failed_at_ms <- decode.field("failed_at_ms", decode.int)
      case task_ref, issue_id {
        Some(task_ref), _ ->
          decode.success(OutboxSnapshot(
            outbox_id,
            OutboxPermanentlyFailedWithTask(
              task_ref,
              outbox_kind,
              error_code,
              attempt_count,
              failed_at_ms,
            ),
          ))
        None, Some(issue_id) ->
          decode.success(OutboxSnapshot(
            outbox_id,
            OutboxPermanentlyFailed(
              issue_id,
              outbox_kind,
              error_code,
              attempt_count,
              failed_at_ms,
            ),
          ))
        None, None -> outbox_snapshot_decode_failure()
      }
    }
    _ -> outbox_snapshot_decode_failure()
  }
}

fn outbox_snapshot_decode_failure() -> decode.Decoder(OutboxSnapshot) {
  decode.failure(
    OutboxSnapshot("", OutboxPermanentlyFailed("", "", "", 0, 0)),
    expected: "OutboxSnapshot",
  )
}

fn outbox_task_ref_from_snapshot_decoder(
  backend_kind: Option(String),
  remote_id: Option(String),
  task_key: Option(String),
  task_url: Option(String),
  next: fn(Option(record.TaskRefFields)) -> decode.Decoder(OutboxSnapshot),
) -> decode.Decoder(OutboxSnapshot) {
  case task_ref_from_snapshot(backend_kind, remote_id, task_key, task_url) {
    Ok(task_ref) -> next(task_ref)
    Error(Nil) -> outbox_snapshot_decode_failure()
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
  case backend_kind, remote_id, task_key, task_url {
    None, None, None, None -> Ok(None)
    Some(kind), Some(id), _, _ ->
      Ok(Some(record.TaskRefFields(kind, id, task_key, task_url)))
    _, _, _, _ -> Error(Nil)
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
    command_status_issue_id(status)
  })
}

fn command_task_refs(
  commands: Dict(String, CommandStatus),
) -> List(record.TaskRefFields) {
  commands
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(receipt_key, status) = entry
    case task_ref_from_remote_command_receipt_key(receipt_key) {
      Some(task_ref) -> task_ref
      None -> linear_task_ref_for_issue_id(command_status_issue_id(status))
    }
  })
}

fn command_status_issue_id(status: CommandStatus) -> String {
  case status {
    CommandSeen(issue_id, _, _, _, _) -> issue_id
    CommandStarted(issue_id, _, _) -> issue_id
    CommandCompleted(issue_id, _, _, _) -> issue_id
    CommandAcked(issue_id, _) -> issue_id
  }
}

fn task_ref_from_remote_command_receipt_key(
  receipt_key: String,
) -> Option(record.TaskRefFields) {
  case string.split_once(receipt_key, on: "\u{001f}") {
    Ok(#(identity, _event_id)) ->
      case decode_remote_command_receipt_identity(identity) {
        Ok(#(backend_kind, task_remote_id)) ->
          Some(record.TaskRefFields(backend_kind, task_remote_id, None, None))
        Error(Nil) -> None
      }
    Error(Nil) -> None
  }
}

fn decode_remote_command_receipt_identity(
  identity: String,
) -> Result(#(String, String), Nil) {
  use #(backend_kind, rest) <- result.try(decode_identity_component(identity))
  case string.starts_with(rest, "|") {
    False -> Error(Nil)
    True -> {
      let rest = string.drop_start(rest, 1)
      use #(task_remote_id, rest) <- result.try(decode_identity_component(rest))
      case rest == "" {
        True -> Ok(#(backend_kind, task_remote_id))
        False -> Error(Nil)
      }
    }
  }
}

fn decode_identity_component(input: String) -> Result(#(String, String), Nil) {
  use #(length_text, after_colon) <- result.try(
    string.split_once(input, on: ":")
    |> result.replace_error(Nil),
  )
  use length <- result.try(int.parse(length_text) |> result.replace_error(Nil))
  case length < 0 || string.length(after_colon) < length {
    True -> Error(Nil)
    False -> {
      let value = string.slice(after_colon, 0, length)
      let rest = string.drop_start(after_colon, length)
      Ok(#(value, rest))
    }
  }
}

fn outbox_issue_ids(outbox: Dict(String, OutboxStatus)) -> List(String) {
  outbox
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(_, status) = entry
    outbox_status_task_ref(status).task_remote_id
  })
}

fn outbox_task_refs(
  outbox: Dict(String, OutboxStatus),
) -> List(record.TaskRefFields) {
  outbox
  |> dict.values
  |> list.map(outbox_status_task_ref)
}

fn outbox_status_task_ref(status: OutboxStatus) -> record.TaskRefFields {
  case status {
    OutboxPending(issue_id, _, _, _) -> linear_task_ref_for_issue_id(issue_id)
    OutboxPendingV2(issue_id, _, _, _, _) ->
      linear_task_ref_for_issue_id(issue_id)
    OutboxPendingV2WithTask(task_ref, _, _, _, _) -> task_ref
    OutboxAttempted(issue_id, _, _, _, _, _) ->
      linear_task_ref_for_issue_id(issue_id)
    OutboxAttemptedWithTask(task_ref, _, _, _, _, _) -> task_ref
    OutboxRetryScheduled(issue_id, _, _, _, _, _, _, _) ->
      linear_task_ref_for_issue_id(issue_id)
    OutboxRetryScheduledWithTask(task_ref, _, _, _, _, _, _, _) -> task_ref
    OutboxCompleted(issue_id, _, _) -> linear_task_ref_for_issue_id(issue_id)
    OutboxCompletedWithTask(task_ref, _, _) -> task_ref
    OutboxFailed(issue_id, _, _, _) -> linear_task_ref_for_issue_id(issue_id)
    OutboxFailedWithTask(task_ref, _, _, _) -> task_ref
    OutboxPermanentlyFailed(issue_id, _, _, _, _) ->
      linear_task_ref_for_issue_id(issue_id)
    OutboxPermanentlyFailedWithTask(task_ref, _, _, _, _) -> task_ref
  }
}

fn linear_task_ref_for_issue_id(issue_id: String) -> record.TaskRefFields {
  record.linear_task_ref_fields(issue_id, None, None)
}

fn linear_task_ref_for_issue(
  issue_id: String,
  issue_identifier: Option(String),
) -> record.TaskRefFields {
  record.linear_task_ref_fields(issue_id, issue_identifier, None)
}

fn recovery_run_task_refs(
  runs: Dict(String, RunStatus),
) -> List(record.TaskRefFields) {
  runs
  |> dict.values
  |> list.fold([], fn(refs, status) {
    case status {
      RunRunning(issue_id, issue_identifier, _, _) ->
        append_unique_task_refs(refs, [
          linear_task_ref_for_issue(issue_id, Some(issue_identifier)),
        ])
      RunInterrupted(issue_id, _, _) ->
        append_unique_task_refs(refs, [linear_task_ref_for_issue_id(issue_id)])
      RunFinished(..) -> refs
    }
  })
}

fn recovery_workflow_task_refs(
  projection: Projection,
) -> List(record.TaskRefFields) {
  active_workflow_runs(projection)
  |> list.fold([], fn(refs, entry) {
    let #(run_id, status) = entry
    case status {
      WorkflowRunActive(_, _, issue_id, issue_identifier, _, _, _, _) -> {
        let task_ref = case dict.get(projection.workflow_task_refs, run_id) {
          Ok(task_ref) -> task_ref
          Error(Nil) ->
            linear_task_ref_for_issue(issue_id, Some(issue_identifier))
        }
        append_unique_task_refs(refs, [task_ref])
      }
      _ -> refs
    }
  })
}

fn retry_task_refs(projection: Projection) -> List(record.TaskRefFields) {
  projection.retries
  |> dict.to_list
  |> list.fold([], fn(refs, entry) {
    let #(issue_id, status) = entry
    case status {
      RetryScheduled(issue_identifier, _, _, _, _) ->
        append_unique_task_refs(refs, [
          recovery_task_ref_for_issue(
            projection,
            issue_id,
            Some(issue_identifier),
          ),
        ])
      RetryCancelled(..) -> refs
    }
  })
}

fn recovery_task_ref_for_issue(
  projection: Projection,
  issue_id: String,
  issue_identifier: Option(String),
) -> record.TaskRefFields {
  let matching_refs =
    known_task_refs(projection)
    |> list.filter(fn(task_ref) { task_ref.task_remote_id == issue_id })

  case
    list.find(matching_refs, fn(task_ref) {
      task_ref.task_backend_kind != "linear"
    })
  {
    Ok(task_ref) -> task_ref
    Error(Nil) -> linear_task_ref_for_issue(issue_id, issue_identifier)
  }
}

fn parked_task_refs(
  parked_issues: Dict(String, ParkedIssue),
) -> List(record.TaskRefFields) {
  parked_issues
  |> dict.to_list
  |> list.fold([], fn(refs, entry) {
    let #(issue_id, parked) = entry
    append_unique_task_refs(refs, [
      linear_task_ref_for_issue(issue_id, Some(parked.issue_identifier)),
    ])
  })
}

fn recovery_issue_counter_task_refs(
  projection: Projection,
) -> List(record.TaskRefFields) {
  projection.issue_counters
  |> dict.to_list
  |> list.fold([], fn(refs, entry) {
    let #(issue_id, counter) = entry
    case counter.failure_attempts > 0 || counter.worker_sessions > 0 {
      True ->
        append_unique_task_refs(refs, [
          linear_task_ref_for_issue(issue_id, Some(counter.issue_identifier)),
        ])
      False -> refs
    }
  })
}

fn append_unique_task_refs(
  values: List(record.TaskRefFields),
  more: List(record.TaskRefFields),
) -> List(record.TaskRefFields) {
  list.fold(more, values, insert_unique_task_ref)
}

fn insert_unique_task_ref(
  values: List(record.TaskRefFields),
  value: record.TaskRefFields,
) -> List(record.TaskRefFields) {
  case
    string.trim(value.task_remote_id) == "" || task_ref_in_list(values, value)
  {
    True -> values
    False -> [value, ..values]
  }
}

fn task_ref_in_list(
  values: List(record.TaskRefFields),
  value: record.TaskRefFields,
) -> Bool {
  list.any(values, fn(existing) {
    existing.task_backend_kind == value.task_backend_kind
    && existing.task_remote_id == value.task_remote_id
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
  now_ms: Int,
  acc: List(OutboxReplay),
) -> Result(List(OutboxReplay), PendingOutboxError) {
  case entries {
    [] -> Ok(list.reverse(acc))
    [entry, ..rest] -> {
      let #(outbox_id, status) = entry
      case status {
        OutboxPending(_, _, _, _) ->
          pending_outbox_replays_loop(rest, now_ms, acc)
        OutboxPendingV2(issue_id, outbox_kind, dedupe_key, payload_json, _)
        | OutboxAttempted(issue_id, outbox_kind, dedupe_key, payload_json, _, _)
        | OutboxRetryScheduled(
            issue_id,
            outbox_kind,
            dedupe_key,
            payload_json,
            _,
            _,
            _,
            _,
          ) ->
          case outbox_status_is_ready(status, now_ms) {
            True ->
              pending_outbox_replays_loop(rest, now_ms, [
                OutboxReplay(
                  outbox_id,
                  linear_task_ref_for_issue_id(issue_id),
                  outbox_kind,
                  dedupe_key,
                  payload_json,
                ),
                ..acc
              ])
            False -> pending_outbox_replays_loop(rest, now_ms, acc)
          }
        OutboxPendingV2WithTask(
          task_ref,
          outbox_kind,
          dedupe_key,
          payload_json,
          _,
        )
        | OutboxAttemptedWithTask(
            task_ref,
            outbox_kind,
            dedupe_key,
            payload_json,
            _,
            _,
          )
        | OutboxRetryScheduledWithTask(
            task_ref,
            outbox_kind,
            dedupe_key,
            payload_json,
            _,
            _,
            _,
            _,
          ) ->
          case outbox_status_is_ready(status, now_ms) {
            True ->
              pending_outbox_replays_loop(rest, now_ms, [
                OutboxReplay(
                  outbox_id,
                  task_ref,
                  outbox_kind,
                  dedupe_key,
                  payload_json,
                ),
                ..acc
              ])
            False -> pending_outbox_replays_loop(rest, now_ms, acc)
          }
        OutboxCompleted(_, _, _)
        | OutboxCompletedWithTask(_, _, _)
        | OutboxFailed(_, _, _, _)
        | OutboxFailedWithTask(_, _, _, _)
        | OutboxPermanentlyFailed(_, _, _, _, _)
        | OutboxPermanentlyFailedWithTask(_, _, _, _, _) ->
          pending_outbox_replays_loop(rest, now_ms, acc)
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
    OutboxPendingV2WithTask(_, _, _, _, pending_at_ms) -> pending_at_ms
    OutboxAttempted(_, _, _, _, _, attempted_at_ms) -> attempted_at_ms
    OutboxAttemptedWithTask(_, _, _, _, _, attempted_at_ms) -> attempted_at_ms
    OutboxRetryScheduled(_, _, _, _, _, _, _, failed_at_ms) -> failed_at_ms
    OutboxRetryScheduledWithTask(_, _, _, _, _, _, _, failed_at_ms) ->
      failed_at_ms
    OutboxCompleted(_, _, completed_at_ms) -> completed_at_ms
    OutboxCompletedWithTask(_, _, completed_at_ms) -> completed_at_ms
    OutboxFailed(_, _, _, failed_at_ms) -> failed_at_ms
    OutboxFailedWithTask(_, _, _, failed_at_ms) -> failed_at_ms
    OutboxPermanentlyFailed(_, _, _, _, failed_at_ms) -> failed_at_ms
    OutboxPermanentlyFailedWithTask(_, _, _, _, failed_at_ms) -> failed_at_ms
  }
}

fn outbox_status_is_ready(status: OutboxStatus, now_ms: Int) -> Bool {
  case status {
    OutboxRetryScheduled(_, _, _, _, _, _, next_attempt_at_ms, _)
    | OutboxRetryScheduledWithTask(_, _, _, _, _, _, next_attempt_at_ms, _) ->
      next_attempt_at_ms <= now_ms
    OutboxPending(_, _, _, _)
    | OutboxPendingV2(_, _, _, _, _)
    | OutboxPendingV2WithTask(_, _, _, _, _)
    | OutboxAttempted(_, _, _, _, _, _)
    | OutboxAttemptedWithTask(_, _, _, _, _, _) -> True
    OutboxCompleted(_, _, _)
    | OutboxCompletedWithTask(_, _, _)
    | OutboxFailed(_, _, _, _)
    | OutboxFailedWithTask(_, _, _, _)
    | OutboxPermanentlyFailed(_, _, _, _, _)
    | OutboxPermanentlyFailedWithTask(_, _, _, _, _) -> False
  }
}

fn projection_latest_at_ms(projection: Projection) -> Int {
  projection.outbox
  |> dict.values
  |> list.map(outbox_status_time)
  |> list.fold(0, int.max)
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
