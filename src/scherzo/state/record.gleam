import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/log
import scherzo/state/record/commands as command_record
import scherzo/state/record/issue_recovery as issue_recovery_record
import scherzo/state/record/legacy_runs as legacy_run_record
import scherzo/state/record/outbox as outbox_record
import scherzo/state/record/publications as publication_record
import scherzo/state/record/scheduled as scheduled_record
import scherzo/state/record/steps as step_record
import scherzo/state/record/workflow_runs as workflow_run_record
import scherzo/state/record/workstreams as workstream_record

pub const schema_version = 2

pub const max_excerpt_chars = 500

pub type TaskRefFields {
  TaskRefFields(
    task_backend_kind: String,
    task_remote_id: String,
    task_key: Option(String),
    task_url: Option(String),
  )
}

pub fn linear_task_ref_fields(
  task_remote_id: String,
  task_key: Option(String),
  task_url: Option(String),
) -> TaskRefFields {
  TaskRefFields(
    task_backend_kind: "linear",
    task_remote_id: task_remote_id,
    task_key: task_key,
    task_url: task_url,
  )
}

pub fn legacy_linear_task_ref_fields(
  issue_id: String,
  issue_identifier: String,
) -> TaskRefFields {
  linear_task_ref_fields(issue_id, Some(issue_identifier), None)
}

pub type LedgerRecord {
  LedgerRecord(record_id: String, at_ms: Int, body: RecordBody)
}

pub type RecordBody {
  RunStarted(
    run_id: String,
    issue_id: String,
    issue_identifier: String,
    workspace_path: String,
  )
  RunFinished(
    run_id: String,
    issue_id: String,
    classification: String,
    token_total: Int,
    turns: Int,
  )
  RunInterrupted(run_id: String, issue_id: String, reason: String)
  WorkflowRunStarted(
    run_id: String,
    workflow_id: String,
    workflow_fingerprint: String,
    issue_id: String,
    issue_identifier: String,
    issue_fingerprint: String,
    observed_updated_at_ms: Int,
    run_root: String,
  )
  WorkflowRunStartedWithTask(
    run_id: String,
    workflow_id: String,
    workflow_fingerprint: String,
    issue_id: String,
    issue_identifier: String,
    task_ref: TaskRefFields,
    issue_fingerprint: String,
    observed_updated_at_ms: Int,
    run_root: String,
  )
  WorkflowRunProvenanceRepaired(
    run_id: String,
    workflow_id: String,
    workflow_fingerprint: String,
    issue_id: String,
    issue_identifier: String,
    task_ref: TaskRefFields,
    issue_fingerprint: String,
    observed_updated_at_ms: Int,
    run_root: String,
    repair_mode: String,
    source_evidence: List(String),
  )
  WorkflowRunFinished(
    run_id: String,
    workflow_id: String,
    issue_id: String,
    outcome: String,
    token_total: Int,
    turns: Int,
  )
  WorkflowRunFinishedWithTask(
    run_id: String,
    workflow_id: String,
    issue_id: String,
    task_ref: TaskRefFields,
    outcome: String,
    token_total: Int,
    turns: Int,
  )
  WorkflowRunInputsRecorded(
    run_id: String,
    workflow_id: String,
    workflow_fingerprint: String,
    artifact_ref: String,
    artifact_sha256: String,
    artifact_bytes: Int,
  )
  WorkflowRunOutputsRecorded(
    run_id: String,
    workflow_id: String,
    workflow_fingerprint: String,
    artifact_ref: String,
    artifact_sha256: String,
    artifact_bytes: Int,
  )
  PublicationAttemptRecorded(
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
  )
  WorkflowRunDiagnostic(
    run_id: String,
    workflow_id: String,
    issue_id: String,
    reason: String,
  )
  WorkflowRunInterrupted(
    run_id: String,
    workflow_id: String,
    issue_id: String,
    reason: String,
  )
  WorkflowRunSuperseded(
    run_id: String,
    workflow_id: String,
    issue_id: String,
    superseded_by_run_id: String,
    reason: String,
  )
  WorkflowRepairRequested(
    run_id: String,
    workflow_id: String,
    issue_id: String,
    issue_identifier: String,
    requested_target: String,
    requested_step_id: Option(String),
    selected_step_id: String,
    failed_attempt_index: Int,
    next_attempt_index: Int,
    reason: String,
  )
  StepAttemptPrepared(
    run_id: String,
    workflow_id: String,
    step_id: String,
    attempt_index: Int,
    workspace_name: String,
    workspace_path: String,
    run_root: String,
    source_workspace_name: Option(String),
    source_workspace_path: Option(String),
  )
  StepAttemptStarted(
    run_id: String,
    workflow_id: String,
    step_id: String,
    attempt_index: Int,
    operator_session_id: String,
    external_session_ref: Option(String),
    continuation_capable: Bool,
  )
  StepAttemptContinuationStarted(
    run_id: String,
    workflow_id: String,
    step_id: String,
    attempt_index: Int,
    session_id: String,
  )
  StepAttemptPiSessionRecorded(
    run_id: String,
    issue_id: String,
    issue_identifier: String,
    workflow_id: String,
    workflow_fingerprint: String,
    step_id: String,
    workspace_name: String,
    attempt_index: Int,
    workspace_path: String,
    session_id: String,
    session_file: String,
  )
  StepAttemptPiSessionRecordedWithTask(
    run_id: String,
    issue_id: String,
    issue_identifier: String,
    task_ref: TaskRefFields,
    workflow_id: String,
    workflow_fingerprint: String,
    step_id: String,
    workspace_name: String,
    attempt_index: Int,
    workspace_path: String,
    session_id: String,
    session_file: String,
  )
  StepAttemptFinished(
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
  )
  WorkflowStepRecoveryStarted(
    run_id: String,
    workflow_id: String,
    step_id: String,
    failed_attempt_index: Int,
    recovery_attempt_number: Int,
    recovery_session_id: String,
    model: Option(String),
    prompt_ref: String,
  )
  WorkflowStepRecoveryFinished(
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
  )
  StepAttemptInterrupted(
    run_id: String,
    workflow_id: String,
    step_id: String,
    attempt_index: Int,
    reason: String,
  )
  StepAttemptSuperseded(
    run_id: String,
    workflow_id: String,
    step_id: String,
    attempt_index: Int,
    superseded_by_attempt_index: Int,
    reason: String,
  )
  RetryScheduled(
    issue_id: String,
    issue_identifier: String,
    delay_ms: Int,
    generation: Int,
    reason: String,
  )
  RetryCancelled(issue_id: String, generation: Int, reason: String)
  IssueCounterUpdated(
    issue_id: String,
    issue_identifier: String,
    failure_attempts: Int,
    worker_sessions: Int,
    observed_updated_at_ms: Int,
    source_run_id: Option(String),
  )
  KnownWorkspace(
    issue_id: String,
    issue_identifier: String,
    workspace_path: String,
  )
  IssueParked(
    issue_id: String,
    issue_identifier: String,
    reason: String,
    observed_updated_at_ms: Int,
  )
  IssueParkedV2(
    issue_id: String,
    issue_identifier: String,
    reason: String,
    release_policy: String,
    issue_fingerprint: String,
    observed_updated_at_ms: Int,
  )
  IssueUnparked(issue_id: String, issue_identifier: String, reason: String)
  LinearCommandSeen(
    comment_id: String,
    issue_id: String,
    author_id: String,
    command_name: String,
    excerpt: String,
  )
  LinearCommandStarted(
    comment_id: String,
    issue_id: String,
    command_name: String,
  )
  LinearCommandCompleted(
    comment_id: String,
    issue_id: String,
    status: String,
    message_excerpt: String,
  )
  LinearCommandAcked(comment_id: String, issue_id: String)
  RemoteCommandSeen(
    backend_kind: String,
    event_id: String,
    task_remote_id: String,
    task_key: Option(String),
    author_id: String,
    command_name: String,
    excerpt: String,
  )
  RemoteCommandStarted(
    backend_kind: String,
    event_id: String,
    task_remote_id: String,
    command_name: String,
  )
  RemoteCommandCompleted(
    backend_kind: String,
    event_id: String,
    task_remote_id: String,
    status: String,
    message_excerpt: String,
  )
  RemoteCommandAcked(
    backend_kind: String,
    event_id: String,
    task_remote_id: String,
  )
  ScheduledJobDue(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    trigger: String,
  )
  ScheduledJobSkipped(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    reason: String,
    skipped_count: Int,
  )
  ScheduledRunPending(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    trigger: String,
    requested_at_ms: Int,
  )
  ScheduledRunPendingBlocked(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    reason: String,
    observed_at_ms: Int,
  )
  ScheduledRunPendingCancelled(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    reason: String,
    cancelled_at_ms: Int,
  )
  ScheduledRunStarted(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    started_at_ms: Int,
    run_id: String,
    attempt: Int,
    session_id: String,
    run_root: String,
  )
  ScheduledRunSucceeded(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    attempt: Int,
    finished_at_ms: Int,
    token_total: Int,
    turns: Int,
  )
  ScheduledRunFailed(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    attempt: Int,
    finished_at_ms: Int,
    reason: String,
    retry_exhausted: Bool,
    run_root: Option(String),
  )
  ScheduledRunRetryScheduled(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    next_attempt: Int,
    delay_ms: Int,
    generation: Int,
    reason: String,
  )
  ScheduledRunRetryCancelled(
    job_id: String,
    run_id: String,
    generation: Int,
    reason: String,
  )
  ScheduledFailureReported(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    attempt: Int,
    dedupe_key: String,
    linear_issue_id: String,
    action: String,
  )
  ScheduledFailureReportFailed(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    attempt: Int,
    dedupe_key: String,
    error_code: String,
    error_message: String,
    next_retry_at_ms: Int,
    generation: Int,
  )
  OutboxPending(
    outbox_id: String,
    issue_id: String,
    outbox_kind: String,
    dedupe_key: String,
  )
  OutboxPendingV2(
    outbox_id: String,
    issue_id: String,
    outbox_kind: String,
    dedupe_key: String,
    payload_json: String,
  )
  OutboxPendingV2WithTask(
    outbox_id: String,
    task_ref: TaskRefFields,
    outbox_kind: String,
    dedupe_key: String,
    payload_json: String,
  )
  OutboxCompleted(outbox_id: String, issue_id: String, outbox_kind: String)
  OutboxCompletedWithTask(
    outbox_id: String,
    task_ref: TaskRefFields,
    outbox_kind: String,
  )
  OutboxFailed(
    outbox_id: String,
    issue_id: String,
    outbox_kind: String,
    error_code: String,
  )
  OutboxFailedWithTask(
    outbox_id: String,
    task_ref: TaskRefFields,
    outbox_kind: String,
    error_code: String,
  )
  WorkstreamCreated(
    workstream_id: String,
    task_ref: TaskRefFields,
    idempotency_key: String,
  )
  WorkstreamAssigned(
    workstream_id: String,
    assignment_id: String,
    workflow_id: String,
    playbook_id: Option(String),
    reason: String,
    idempotency_key: String,
  )
  WorkstreamArtifactRecorded(
    workstream_id: String,
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
  )
  WorkstreamHandoffRecorded(
    workstream_id: String,
    handoff_id: String,
    handoff_ref: String,
    handoff_sha256: String,
    handoff_bytes: Int,
    source_workflow_id: String,
    source_run_id: String,
    idempotency_key: String,
  )
  WorkstreamPhaseRunQueued(
    workstream_id: String,
    phase_run_id: String,
    action_id: String,
    workflow_id: String,
    input_bundle_ref: String,
    input_bundle_sha256: String,
    input_bundle_bytes: Int,
    idempotency_key: String,
  )
}

pub type DecodeError {
  MalformedJson(String)
  UnsupportedVersion(Int)
  InvalidRecord(String)
  UnknownKind(String)
}

type RecordFields {
  RecordFields(
    schema_version: Int,
    record_id: String,
    at_ms: Int,
    kind: String,
    run_id: Option(String),
    workflow_id: Option(String),
    workflow_fingerprint: Option(String),
    issue_id: Option(String),
    issue_identifier: Option(String),
    task_backend_kind: Option(String),
    task_remote_id: Option(String),
    task_key: Option(String),
    task_url: Option(String),
    workspace_path: Option(String),
    workspace_name: Option(String),
    run_root: Option(String),
    source_workspace_name: Option(String),
    source_workspace_path: Option(String),
    step_id: Option(String),
    attempt_index: Option(Int),
    operator_session_id: Option(String),
    external_session_ref: Option(String),
    continuation_capable: Option(Bool),
    session_id: Option(String),
    session_file: Option(String),
    outcome: Option(String),
    artifact_ref: Option(String),
    artifact_sha256: Option(String),
    artifact_bytes: Option(Int),
    publication_id: Option(String),
    series_id: Option(String),
    attempt_id: Option(String),
    retryable: Option(Bool),
    retry_execution_available: Option(Bool),
    version_id: Option(String),
    manifest_ref: Option(String),
    manifest_sha256: Option(String),
    manifest_bytes: Option(Int),
    superseded_by_run_id: Option(String),
    superseded_by_attempt_index: Option(Int),
    classification: Option(String),
    token_total: Option(Int),
    turns: Option(Int),
    reason: Option(String),
    delay_ms: Option(Int),
    generation: Option(Int),
    failure_attempts: Option(Int),
    worker_sessions: Option(Int),
    observed_updated_at_ms: Option(Int),
    source_run_id: Option(String),
    release_policy: Option(String),
    issue_fingerprint: Option(String),
    repair_mode: Option(String),
    source_evidence: List(String),
    requested_target: Option(String),
    requested_step_id: Option(String),
    selected_step_id: Option(String),
    failed_attempt_index: Option(Int),
    next_attempt_index: Option(Int),
    backend_kind: Option(String),
    event_id: Option(String),
    comment_id: Option(String),
    author_id: Option(String),
    command_name: Option(String),
    excerpt: Option(String),
    status: Option(String),
    required: Option(Bool),
    message_excerpt: Option(String),
    outbox_id: Option(String),
    outbox_kind: Option(String),
    dedupe_key: Option(String),
    payload_json: Option(String),
    error_code: Option(String),
    job_id: Option(String),
    due_at_ms: Option(Int),
    trigger: Option(String),
    skipped_count: Option(Int),
    requested_at_ms: Option(Int),
    observed_at_ms: Option(Int),
    cancelled_at_ms: Option(Int),
    started_at_ms: Option(Int),
    finished_at_ms: Option(Int),
    attempt: Option(Int),
    retry_exhausted: Option(Bool),
    next_attempt: Option(Int),
    linear_issue_id: Option(String),
    action: Option(String),
    error_message: Option(String),
    next_retry_at_ms: Option(Int),
    workstream_id: Option(String),
    assignment_id: Option(String),
    playbook_id: Option(String),
    idempotency_key: Option(String),
    artifact_id: Option(String),
    artifact_type: Option(String),
    snapshot_ref: Option(String),
    snapshot_sha256: Option(String),
    snapshot_bytes: Option(Int),
    original_path: Option(String),
    contract_type: Option(String),
    media_type: Option(String),
    producer_workflow_id: Option(String),
    producer_run_id: Option(String),
    producer_step_id: Option(String),
    handoff_id: Option(String),
    handoff_ref: Option(String),
    handoff_sha256: Option(String),
    handoff_bytes: Option(Int),
    source_workflow_id: Option(String),
    phase_run_id: Option(String),
    action_id: Option(String),
    input_bundle_ref: Option(String),
    input_bundle_sha256: Option(String),
    input_bundle_bytes: Option(Int),
    recovery_attempt_number: Option(Int),
    recovery_session_id: Option(String),
    model: Option(String),
    prompt_ref: Option(String),
    result: Option(String),
    summary: Option(String),
    retry_attempt_index: Option(Int),
  )
}

pub fn new(at_ms: Int, sequence: Int, body: RecordBody) -> LedgerRecord {
  LedgerRecord(
    record_id: int.to_string(at_ms)
      <> "-"
      <> int.to_string(sequence)
      <> "-"
      <> kind(body),
    at_ms: at_ms,
    body: body,
  )
}

pub fn with_id(
  record_id: String,
  at_ms: Int,
  body: RecordBody,
) -> LedgerRecord {
  LedgerRecord(record_id: record_id, at_ms: at_ms, body: body)
}

pub fn kind(body: RecordBody) -> String {
  case body {
    RunStarted(..) -> "run_started"
    RunFinished(..) -> "run_finished"
    RunInterrupted(..) -> "run_interrupted"
    WorkflowRunStarted(..) -> "workflow_run_started"
    WorkflowRunStartedWithTask(..) -> "workflow_run_started"
    WorkflowRunProvenanceRepaired(..) -> "workflow_run_provenance_repaired"
    WorkflowRunFinished(..) -> "workflow_run_finished"
    WorkflowRunFinishedWithTask(..) -> "workflow_run_finished"
    WorkflowRunInputsRecorded(..) -> "workflow_run_inputs_recorded"
    WorkflowRunOutputsRecorded(..) -> "workflow_run_outputs_recorded"
    PublicationAttemptRecorded(..) -> "publication_attempt_recorded"
    WorkflowRunDiagnostic(..) -> "workflow_run_diagnostic"
    WorkflowRunInterrupted(..) -> "workflow_run_interrupted"
    WorkflowRunSuperseded(..) -> "workflow_run_superseded"
    WorkflowRepairRequested(..) -> "workflow_repair_requested"
    StepAttemptPrepared(..) -> "step_attempt_prepared"
    StepAttemptStarted(..) -> "step_attempt_started"
    StepAttemptContinuationStarted(..) -> "step_attempt_continuation_started"
    StepAttemptPiSessionRecorded(..) -> "step_attempt_pi_session_recorded"
    StepAttemptPiSessionRecordedWithTask(..) ->
      "step_attempt_pi_session_recorded"
    StepAttemptFinished(..) -> "step_attempt_finished"
    WorkflowStepRecoveryStarted(..) -> "workflow_step_recovery_started"
    WorkflowStepRecoveryFinished(..) -> "workflow_step_recovery_finished"
    StepAttemptInterrupted(..) -> "step_attempt_interrupted"
    StepAttemptSuperseded(..) -> "step_attempt_superseded"
    RetryScheduled(..) -> "retry_scheduled"
    RetryCancelled(..) -> "retry_cancelled"
    IssueCounterUpdated(..) -> "issue_counter_updated"
    KnownWorkspace(..) -> "known_workspace"
    IssueParked(..) -> "issue_parked"
    IssueParkedV2(..) -> "issue_parked_v2"
    IssueUnparked(..) -> "issue_unparked"
    LinearCommandSeen(..) -> "linear_command_seen"
    LinearCommandStarted(..) -> "linear_command_started"
    LinearCommandCompleted(..) -> "linear_command_completed"
    LinearCommandAcked(..) -> "linear_command_acked"
    RemoteCommandSeen(..) -> "remote_command_seen"
    RemoteCommandStarted(..) -> "remote_command_started"
    RemoteCommandCompleted(..) -> "remote_command_completed"
    RemoteCommandAcked(..) -> "remote_command_acked"
    ScheduledJobDue(..) -> "scheduled_job_due"
    ScheduledJobSkipped(..) -> "scheduled_job_skipped"
    ScheduledRunPending(..) -> "scheduled_run_pending"
    ScheduledRunPendingBlocked(..) -> "scheduled_run_pending_blocked"
    ScheduledRunPendingCancelled(..) -> "scheduled_run_pending_cancelled"
    ScheduledRunStarted(..) -> "scheduled_run_started"
    ScheduledRunSucceeded(..) -> "scheduled_run_succeeded"
    ScheduledRunFailed(..) -> "scheduled_run_failed"
    ScheduledRunRetryScheduled(..) -> "scheduled_run_retry_scheduled"
    ScheduledRunRetryCancelled(..) -> "scheduled_run_retry_cancelled"
    ScheduledFailureReported(..) -> "scheduled_failure_reported"
    ScheduledFailureReportFailed(..) -> "scheduled_failure_report_failed"
    OutboxPending(..) -> "outbox_pending"
    OutboxPendingV2(..) | OutboxPendingV2WithTask(..) -> "outbox_pending_v2"
    OutboxCompleted(..) | OutboxCompletedWithTask(..) -> "outbox_completed"
    OutboxFailed(..) | OutboxFailedWithTask(..) -> "outbox_failed"
    WorkstreamCreated(..) -> "workstream_created"
    WorkstreamAssigned(..) -> "workstream_assigned"
    WorkstreamArtifactRecorded(..) -> "workstream_artifact_recorded"
    WorkstreamHandoffRecorded(..) -> "workstream_handoff_recorded"
    WorkstreamPhaseRunQueued(..) -> "workstream_phase_run_queued"
  }
}

pub fn to_json(ledger_record: LedgerRecord) -> json.Json {
  [
    #("schema_version", json.int(schema_version)),
    #("record_id", json.string(ledger_record.record_id)),
    #("at_ms", json.int(ledger_record.at_ms)),
    #("kind", json.string(kind(ledger_record.body))),
    ..body_entries(ledger_record.body)
  ]
  |> json.object
}

pub fn to_string(ledger_record: LedgerRecord) -> String {
  ledger_record |> to_json |> json.to_string
}

pub fn decode_string(line: String) -> Result(LedgerRecord, DecodeError) {
  case json.parse(line, fields_decoder()) {
    Error(json.UnexpectedEndOfInput) -> Error(MalformedJson("malformed JSON"))
    Error(json.UnexpectedByte(_)) -> Error(MalformedJson("malformed JSON"))
    Error(json.UnexpectedSequence(_)) -> Error(MalformedJson("malformed JSON"))
    Error(json.UnableToDecode(_)) ->
      Error(InvalidRecord("invalid ledger record shape"))
    Ok(fields) -> fields_to_record(fields)
  }
}

pub fn redact_excerpts(
  ledger_record: LedgerRecord,
  secrets: List(String),
) -> LedgerRecord {
  LedgerRecord(..ledger_record, body: redact_body(ledger_record.body, secrets))
}

pub fn describe_error(error: DecodeError) -> String {
  case error {
    MalformedJson(reason) -> reason
    UnsupportedVersion(version) ->
      "unsupported schema version " <> int.to_string(version)
    InvalidRecord(reason) -> reason
    UnknownKind(kind) -> "unknown ledger record kind " <> kind
  }
}

fn body_entries(body: RecordBody) -> List(#(String, json.Json)) {
  case body {
    RunStarted(run_id, issue_id, issue_identifier, workspace_path) ->
      legacy_run_record.run_started_entries(
        run_id,
        issue_id,
        issue_identifier,
        workspace_path,
      )
    RunFinished(run_id, issue_id, classification, token_total, turns) ->
      legacy_run_record.run_finished_entries(
        run_id,
        issue_id,
        classification,
        token_total,
        turns,
      )
    RunInterrupted(run_id, issue_id, reason) ->
      legacy_run_record.run_interrupted_entries(run_id, issue_id, reason)
    WorkflowRunStarted(
      run_id,
      workflow_id,
      workflow_fingerprint,
      issue_id,
      issue_identifier,
      issue_fingerprint,
      observed_updated_at_ms,
      run_root,
    ) ->
      workflow_run_record.started_entries(
        run_id,
        workflow_id,
        workflow_fingerprint,
        issue_id,
        issue_identifier,
        issue_fingerprint,
        observed_updated_at_ms,
        run_root,
      )
    WorkflowRunStartedWithTask(
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
      workflow_run_record.started_with_task_entries(
        run_id,
        workflow_id,
        workflow_fingerprint,
        issue_id,
        issue_identifier,
        task_ref_entries(task_ref),
        issue_fingerprint,
        observed_updated_at_ms,
        run_root,
      )
    WorkflowRunProvenanceRepaired(
      run_id,
      workflow_id,
      workflow_fingerprint,
      issue_id,
      issue_identifier,
      task_ref,
      issue_fingerprint,
      observed_updated_at_ms,
      run_root,
      repair_mode,
      source_evidence,
    ) ->
      workflow_run_record.provenance_repaired_entries(
        run_id,
        workflow_id,
        workflow_fingerprint,
        issue_id,
        issue_identifier,
        task_ref_entries(task_ref),
        issue_fingerprint,
        observed_updated_at_ms,
        run_root,
        repair_mode,
        source_evidence,
      )
    WorkflowRunFinished(
      run_id,
      workflow_id,
      issue_id,
      outcome,
      token_total,
      turns,
    ) ->
      workflow_run_record.finished_entries(
        run_id,
        workflow_id,
        issue_id,
        outcome,
        token_total,
        turns,
      )
    WorkflowRunFinishedWithTask(
      run_id,
      workflow_id,
      issue_id,
      task_ref,
      outcome,
      token_total,
      turns,
    ) ->
      workflow_run_record.finished_with_task_entries(
        run_id,
        workflow_id,
        issue_id,
        task_ref_entries(task_ref),
        outcome,
        token_total,
        turns,
      )
    WorkflowRunInputsRecorded(
      run_id,
      workflow_id,
      workflow_fingerprint,
      artifact_ref,
      artifact_sha256,
      artifact_bytes,
    ) ->
      workflow_contract_record_entries(
        run_id,
        workflow_id,
        workflow_fingerprint,
        artifact_ref,
        artifact_sha256,
        artifact_bytes,
      )
    WorkflowRunOutputsRecorded(
      run_id,
      workflow_id,
      workflow_fingerprint,
      artifact_ref,
      artifact_sha256,
      artifact_bytes,
    ) ->
      workflow_contract_record_entries(
        run_id,
        workflow_id,
        workflow_fingerprint,
        artifact_ref,
        artifact_sha256,
        artifact_bytes,
      )
    PublicationAttemptRecorded(
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
    ) ->
      publication_record.attempt_recorded_entries(
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
      )
    WorkflowRunDiagnostic(run_id, workflow_id, issue_id, reason) ->
      workflow_run_record.diagnostic_entries(
        run_id,
        workflow_id,
        issue_id,
        reason,
      )
    WorkflowRunInterrupted(run_id, workflow_id, issue_id, reason) ->
      workflow_run_record.interrupted_entries(
        run_id,
        workflow_id,
        issue_id,
        reason,
      )
    WorkflowRunSuperseded(
      run_id,
      workflow_id,
      issue_id,
      superseded_by_run_id,
      reason,
    ) ->
      workflow_run_record.superseded_entries(
        run_id,
        workflow_id,
        issue_id,
        superseded_by_run_id,
        reason,
      )
    WorkflowRepairRequested(
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
      workflow_run_record.repair_requested_entries(
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
      )
    StepAttemptPrepared(
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
      step_record.prepared_entries(
        run_id,
        workflow_id,
        step_id,
        attempt_index,
        workspace_name,
        workspace_path,
        run_root,
        source_workspace_name,
        source_workspace_path,
      )
    StepAttemptStarted(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      operator_session_id,
      external_session_ref,
      continuation_capable,
    ) ->
      step_record.started_entries(
        run_id,
        workflow_id,
        step_id,
        attempt_index,
        operator_session_id,
        external_session_ref,
        continuation_capable,
      )
    StepAttemptContinuationStarted(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      session_id,
    ) ->
      step_record.continuation_started_entries(
        run_id,
        workflow_id,
        step_id,
        attempt_index,
        session_id,
      )
    StepAttemptPiSessionRecorded(
      run_id,
      issue_id,
      issue_identifier,
      workflow_id,
      workflow_fingerprint,
      step_id,
      workspace_name,
      attempt_index,
      workspace_path,
      session_id,
      session_file,
    ) ->
      step_record.pi_session_recorded_entries(
        run_id,
        issue_id,
        issue_identifier,
        workflow_id,
        workflow_fingerprint,
        step_id,
        workspace_name,
        attempt_index,
        workspace_path,
        session_id,
        session_file,
      )
    StepAttemptPiSessionRecordedWithTask(
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
    ) ->
      step_record.pi_session_recorded_with_task_entries(
        run_id,
        issue_id,
        issue_identifier,
        task_ref_entries(task_ref),
        workflow_id,
        workflow_fingerprint,
        step_id,
        workspace_name,
        attempt_index,
        workspace_path,
        session_id,
        session_file,
      )
    StepAttemptFinished(
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
    ) ->
      step_record.finished_entries(
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
      )
    WorkflowStepRecoveryStarted(
      run_id,
      workflow_id,
      step_id,
      failed_attempt_index,
      recovery_attempt_number,
      recovery_session_id,
      model,
      prompt_ref,
    ) ->
      step_record.recovery_started_entries(
        run_id,
        workflow_id,
        step_id,
        failed_attempt_index,
        recovery_attempt_number,
        recovery_session_id,
        model,
        prompt_ref,
      )
    WorkflowStepRecoveryFinished(
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
      step_record.recovery_finished_entries(
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
    StepAttemptInterrupted(run_id, workflow_id, step_id, attempt_index, reason) ->
      step_record.interrupted_entries(
        run_id,
        workflow_id,
        step_id,
        attempt_index,
        reason,
      )
    StepAttemptSuperseded(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      superseded_by_attempt_index,
      reason,
    ) ->
      step_record.superseded_entries(
        run_id,
        workflow_id,
        step_id,
        attempt_index,
        superseded_by_attempt_index,
        reason,
      )
    RetryScheduled(issue_id, issue_identifier, delay_ms, generation, reason) ->
      issue_recovery_record.retry_scheduled_entries(
        issue_id,
        issue_identifier,
        delay_ms,
        generation,
        reason,
      )
    RetryCancelled(issue_id, generation, reason) ->
      issue_recovery_record.retry_cancelled_entries(
        issue_id,
        generation,
        reason,
      )
    IssueCounterUpdated(
      issue_id,
      issue_identifier,
      failure_attempts,
      worker_sessions,
      observed_updated_at_ms,
      source_run_id,
    ) ->
      issue_recovery_record.issue_counter_entries(
        issue_id,
        issue_identifier,
        failure_attempts,
        worker_sessions,
        observed_updated_at_ms,
        source_run_id,
      )
    KnownWorkspace(issue_id, issue_identifier, workspace_path) ->
      issue_recovery_record.known_workspace_entries(
        issue_id,
        issue_identifier,
        workspace_path,
      )
    IssueParked(issue_id, issue_identifier, reason, observed_updated_at_ms) ->
      issue_recovery_record.issue_parked_entries(
        issue_id,
        issue_identifier,
        reason,
        observed_updated_at_ms,
      )
    IssueParkedV2(
      issue_id,
      issue_identifier,
      reason,
      release_policy,
      issue_fingerprint,
      observed_updated_at_ms,
    ) ->
      issue_recovery_record.issue_parked_v2_entries(
        issue_id,
        issue_identifier,
        reason,
        release_policy,
        issue_fingerprint,
        observed_updated_at_ms,
      )
    IssueUnparked(issue_id, issue_identifier, reason) ->
      issue_recovery_record.issue_unparked_entries(
        issue_id,
        issue_identifier,
        reason,
      )
    LinearCommandSeen(comment_id, issue_id, author_id, command_name, excerpt) ->
      command_record.linear_seen_entries(
        comment_id,
        issue_id,
        author_id,
        command_name,
        excerpt,
      )
    LinearCommandStarted(comment_id, issue_id, command_name) ->
      command_record.linear_started_entries(comment_id, issue_id, command_name)
    LinearCommandCompleted(comment_id, issue_id, status, message_excerpt) ->
      command_record.linear_completed_entries(
        comment_id,
        issue_id,
        status,
        message_excerpt,
      )
    LinearCommandAcked(comment_id, issue_id) ->
      command_record.linear_acked_entries(comment_id, issue_id)
    RemoteCommandSeen(
      backend_kind,
      event_id,
      task_remote_id,
      task_key,
      author_id,
      command_name,
      excerpt,
    ) ->
      command_record.remote_seen_entries(
        backend_kind,
        event_id,
        task_remote_id,
        task_key,
        author_id,
        command_name,
        excerpt,
      )
    RemoteCommandStarted(backend_kind, event_id, task_remote_id, command_name) ->
      command_record.remote_started_entries(
        backend_kind,
        event_id,
        task_remote_id,
        command_name,
      )
    RemoteCommandCompleted(
      backend_kind,
      event_id,
      task_remote_id,
      status,
      message_excerpt,
    ) ->
      command_record.remote_completed_entries(
        backend_kind,
        event_id,
        task_remote_id,
        status,
        message_excerpt,
      )
    RemoteCommandAcked(backend_kind, event_id, task_remote_id) ->
      command_record.remote_acked_entries(
        backend_kind,
        event_id,
        task_remote_id,
      )
    ScheduledJobDue(job_id, workflow_id, due_at_ms, run_id, trigger) ->
      scheduled_record.job_due_entries(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        trigger,
      )
    ScheduledJobSkipped(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      reason,
      skipped_count,
    ) ->
      scheduled_record.job_skipped_entries(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        reason,
        skipped_count,
      )
    ScheduledRunPending(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      trigger,
      requested_at_ms,
    ) ->
      scheduled_record.run_pending_entries(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        trigger,
        requested_at_ms,
      )
    ScheduledRunPendingBlocked(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      reason,
      observed_at_ms,
    ) ->
      scheduled_record.run_pending_blocked_entries(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        reason,
        observed_at_ms,
      )
    ScheduledRunPendingCancelled(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      reason,
      cancelled_at_ms,
    ) ->
      scheduled_record.run_pending_cancelled_entries(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        reason,
        cancelled_at_ms,
      )
    ScheduledRunStarted(
      job_id,
      workflow_id,
      due_at_ms,
      started_at_ms,
      run_id,
      attempt,
      session_id,
      run_root,
    ) ->
      scheduled_record.run_started_entries(
        job_id,
        workflow_id,
        due_at_ms,
        started_at_ms,
        run_id,
        attempt,
        session_id,
        run_root,
      )
    ScheduledRunSucceeded(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      attempt,
      finished_at_ms,
      token_total,
      turns,
    ) ->
      scheduled_record.run_succeeded_entries(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        attempt,
        finished_at_ms,
        token_total,
        turns,
      )
    ScheduledRunFailed(
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
      scheduled_record.run_failed_entries(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        attempt,
        finished_at_ms,
        reason,
        retry_exhausted,
        run_root,
      )
    ScheduledRunRetryScheduled(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      next_attempt,
      delay_ms,
      generation,
      reason,
    ) ->
      scheduled_record.run_retry_scheduled_entries(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        next_attempt,
        delay_ms,
        generation,
        reason,
      )
    ScheduledRunRetryCancelled(job_id, run_id, generation, reason) ->
      scheduled_record.run_retry_cancelled_entries(
        job_id,
        run_id,
        generation,
        reason,
      )
    ScheduledFailureReported(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      attempt,
      dedupe_key,
      linear_issue_id,
      action,
    ) ->
      scheduled_record.failure_reported_entries(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        attempt,
        dedupe_key,
        linear_issue_id,
        action,
      )
    ScheduledFailureReportFailed(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      attempt,
      dedupe_key,
      error_code,
      error_message,
      next_retry_at_ms,
      generation,
    ) ->
      scheduled_record.failure_report_failed_entries(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        attempt,
        dedupe_key,
        error_code,
        error_message,
        next_retry_at_ms,
        generation,
      )
    OutboxPending(outbox_id, issue_id, outbox_kind, dedupe_key) ->
      outbox_record.pending_entries(
        outbox_id,
        issue_id,
        outbox_kind,
        dedupe_key,
      )
    OutboxPendingV2(outbox_id, issue_id, outbox_kind, dedupe_key, payload_json) ->
      outbox_record.pending_v2_entries(
        outbox_id,
        issue_id,
        outbox_kind,
        dedupe_key,
        payload_json,
      )
    OutboxPendingV2WithTask(
      outbox_id,
      task_ref,
      outbox_kind,
      dedupe_key,
      payload_json,
    ) ->
      outbox_record.pending_v2_with_task_entries(
        outbox_id,
        task_ref_entries(task_ref),
        outbox_kind,
        dedupe_key,
        payload_json,
      )
    OutboxCompleted(outbox_id, issue_id, outbox_kind) ->
      outbox_record.completed_entries(outbox_id, issue_id, outbox_kind)
    OutboxCompletedWithTask(outbox_id, task_ref, outbox_kind) ->
      outbox_record.completed_with_task_entries(
        outbox_id,
        task_ref_entries(task_ref),
        outbox_kind,
      )
    OutboxFailed(outbox_id, issue_id, outbox_kind, error_code) ->
      outbox_record.failed_entries(outbox_id, issue_id, outbox_kind, error_code)
    OutboxFailedWithTask(outbox_id, task_ref, outbox_kind, error_code) ->
      outbox_record.failed_with_task_entries(
        outbox_id,
        task_ref_entries(task_ref),
        outbox_kind,
        error_code,
      )
    WorkstreamCreated(workstream_id, task_ref, idempotency_key) ->
      workstream_record.created_with_task_entries(
        workstream_id,
        task_ref_entries(task_ref),
        idempotency_key,
      )
    WorkstreamAssigned(
      workstream_id,
      assignment_id,
      workflow_id,
      playbook_id,
      reason,
      idempotency_key,
    ) ->
      workstream_record.assigned_entries(
        workstream_id,
        assignment_id,
        workflow_id,
        playbook_id,
        reason,
        idempotency_key,
      )
    WorkstreamArtifactRecorded(
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
      workstream_record.artifact_entries(
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
      )
    WorkstreamHandoffRecorded(
      workstream_id,
      handoff_id,
      handoff_ref,
      handoff_sha256,
      handoff_bytes,
      source_workflow_id,
      source_run_id,
      idempotency_key,
    ) ->
      workstream_record.handoff_entries(
        workstream_id,
        handoff_id,
        handoff_ref,
        handoff_sha256,
        handoff_bytes,
        source_workflow_id,
        source_run_id,
        idempotency_key,
      )
    WorkstreamPhaseRunQueued(
      workstream_id,
      phase_run_id,
      action_id,
      workflow_id,
      input_bundle_ref,
      input_bundle_sha256,
      input_bundle_bytes,
      idempotency_key,
    ) ->
      workstream_record.phase_run_entries(
        workstream_id,
        phase_run_id,
        action_id,
        workflow_id,
        input_bundle_ref,
        input_bundle_sha256,
        input_bundle_bytes,
        idempotency_key,
      )
  }
}

fn workflow_contract_record_entries(
  run_id: String,
  workflow_id: String,
  workflow_fingerprint: String,
  artifact_ref: String,
  artifact_sha256: String,
  artifact_bytes: Int,
) -> List(#(String, json.Json)) {
  workflow_run_record.contract_record_entries(
    run_id,
    workflow_id,
    workflow_fingerprint,
    artifact_ref,
    artifact_sha256,
    artifact_bytes,
  )
}

fn option_string_to_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn task_ref_entries(task_ref: TaskRefFields) -> List(#(String, json.Json)) {
  let TaskRefFields(task_backend_kind, task_remote_id, task_key, task_url) =
    task_ref
  [
    #("task_backend_kind", json.string(task_backend_kind)),
    #("task_remote_id", json.string(task_remote_id)),
    #("task_key", option_string_to_json(task_key)),
    #("task_url", option_string_to_json(task_url)),
  ]
}

fn fields_to_record(fields: RecordFields) -> Result(LedgerRecord, DecodeError) {
  case fields.schema_version != schema_version {
    True -> Error(UnsupportedVersion(fields.schema_version))
    False -> {
      use body <- result.try(body_from_fields(fields))
      Ok(LedgerRecord(
        record_id: fields.record_id,
        at_ms: fields.at_ms,
        body: body,
      ))
    }
  }
}

fn legacy_run_body_from_fields(
  fields: RecordFields,
) -> Result(RecordBody, DecodeError) {
  let legacy_fields =
    legacy_run_record.Fields(
      run_id: fields.run_id,
      issue_id: fields.issue_id,
      issue_identifier: fields.issue_identifier,
      workspace_path: fields.workspace_path,
      classification: fields.classification,
      token_total: fields.token_total,
      turns: fields.turns,
      reason: fields.reason,
    )
  case
    legacy_run_record.decode(
      fields.kind,
      legacy_fields,
      required_string,
      required_int,
      UnknownKind,
    )
  {
    Ok(legacy_run_record.RunStartedBody(
      run_id,
      issue_id,
      issue_identifier,
      workspace_path,
    )) -> Ok(RunStarted(run_id, issue_id, issue_identifier, workspace_path))
    Ok(legacy_run_record.RunFinishedBody(
      run_id,
      issue_id,
      classification,
      token_total,
      turns,
    )) -> Ok(RunFinished(run_id, issue_id, classification, token_total, turns))
    Ok(legacy_run_record.RunInterruptedBody(run_id, issue_id, reason)) ->
      Ok(RunInterrupted(run_id, issue_id, reason))
    Error(error) -> Error(error)
  }
}

fn workstream_body_from_fields(
  fields: RecordFields,
) -> Result(RecordBody, DecodeError) {
  let workstream_fields =
    workstream_record.Fields(
      workstream_id: fields.workstream_id,
      assignment_id: fields.assignment_id,
      workflow_id: fields.workflow_id,
      playbook_id: fields.playbook_id,
      reason: fields.reason,
      idempotency_key: fields.idempotency_key,
      artifact_id: fields.artifact_id,
      artifact_type: fields.artifact_type,
      snapshot_ref: fields.snapshot_ref,
      snapshot_sha256: fields.snapshot_sha256,
      snapshot_bytes: fields.snapshot_bytes,
      original_path: fields.original_path,
      contract_type: fields.contract_type,
      media_type: fields.media_type,
      producer_workflow_id: fields.producer_workflow_id,
      producer_run_id: fields.producer_run_id,
      producer_step_id: fields.producer_step_id,
      handoff_id: fields.handoff_id,
      handoff_ref: fields.handoff_ref,
      handoff_sha256: fields.handoff_sha256,
      handoff_bytes: fields.handoff_bytes,
      source_workflow_id: fields.source_workflow_id,
      source_run_id: fields.source_run_id,
      phase_run_id: fields.phase_run_id,
      action_id: fields.action_id,
      input_bundle_ref: fields.input_bundle_ref,
      input_bundle_sha256: fields.input_bundle_sha256,
      input_bundle_bytes: fields.input_bundle_bytes,
    )

  workstream_record.decode(
    fields.kind,
    workstream_fields,
    workstream_record.BodyConstructors(
      WorkstreamCreated,
      WorkstreamAssigned,
      WorkstreamArtifactRecorded,
      WorkstreamHandoffRecorded,
      WorkstreamPhaseRunQueued,
    ),
    fn() { required_task_ref_fields(fields) },
    required_string,
    required_int,
    UnknownKind,
  )
}

fn body_from_fields(fields: RecordFields) -> Result(RecordBody, DecodeError) {
  case fields.kind {
    "run_started" | "run_finished" | "run_interrupted" ->
      legacy_run_body_from_fields(fields)
    "workflow_run_started" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use workflow_id <- result.try(required_string(
        fields.workflow_id,
        "workflow_id",
      ))
      use workflow_fingerprint <- result.try(required_string(
        fields.workflow_fingerprint,
        "workflow_fingerprint",
      ))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use issue_identifier <- result.try(required_string(
        fields.issue_identifier,
        "issue_identifier",
      ))
      use issue_fingerprint <- result.try(required_string(
        fields.issue_fingerprint,
        "issue_fingerprint",
      ))
      use observed_updated_at_ms <- result.try(required_int(
        fields.observed_updated_at_ms,
        "observed_updated_at_ms",
      ))
      use run_root <- result.try(required_string(fields.run_root, "run_root"))
      use task_ref <- result.try(optional_task_ref_fields(fields))
      case task_ref {
        Some(task_ref) ->
          Ok(WorkflowRunStartedWithTask(
            run_id,
            workflow_id,
            workflow_fingerprint,
            issue_id,
            issue_identifier,
            task_ref,
            issue_fingerprint,
            observed_updated_at_ms,
            run_root,
          ))
        None ->
          Ok(WorkflowRunStarted(
            run_id,
            workflow_id,
            workflow_fingerprint,
            issue_id,
            issue_identifier,
            issue_fingerprint,
            observed_updated_at_ms,
            run_root,
          ))
      }
    }
    "workflow_run_provenance_repaired" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use workflow_id <- result.try(required_string(
        fields.workflow_id,
        "workflow_id",
      ))
      use workflow_fingerprint <- result.try(required_string(
        fields.workflow_fingerprint,
        "workflow_fingerprint",
      ))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use issue_identifier <- result.try(required_string(
        fields.issue_identifier,
        "issue_identifier",
      ))
      use task_ref <- result.try(required_task_ref_fields(fields))
      use issue_fingerprint <- result.try(required_string(
        fields.issue_fingerprint,
        "issue_fingerprint",
      ))
      use observed_updated_at_ms <- result.try(required_int(
        fields.observed_updated_at_ms,
        "observed_updated_at_ms",
      ))
      use run_root <- result.try(required_string(fields.run_root, "run_root"))
      use repair_mode <- result.try(required_string(
        fields.repair_mode,
        "repair_mode",
      ))
      Ok(WorkflowRunProvenanceRepaired(
        run_id,
        workflow_id,
        workflow_fingerprint,
        issue_id,
        issue_identifier,
        task_ref,
        issue_fingerprint,
        observed_updated_at_ms,
        run_root,
        repair_mode,
        fields.source_evidence,
      ))
    }
    "workflow_run_finished" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use workflow_id <- result.try(required_string(
        fields.workflow_id,
        "workflow_id",
      ))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use outcome <- result.try(required_string(fields.outcome, "outcome"))
      use token_total <- result.try(required_int(
        fields.token_total,
        "token_total",
      ))
      use turns <- result.try(required_int(fields.turns, "turns"))
      use task_ref <- result.try(optional_task_ref_fields(fields))
      case task_ref {
        Some(task_ref) ->
          Ok(WorkflowRunFinishedWithTask(
            run_id,
            workflow_id,
            issue_id,
            task_ref,
            outcome,
            token_total,
            turns,
          ))
        None ->
          Ok(WorkflowRunFinished(
            run_id,
            workflow_id,
            issue_id,
            outcome,
            token_total,
            turns,
          ))
      }
    }
    "workflow_run_inputs_recorded" ->
      decode_workflow_contract_record(fields, WorkflowRunInputsRecorded)
    "workflow_run_outputs_recorded" ->
      decode_workflow_contract_record(fields, WorkflowRunOutputsRecorded)
    "publication_attempt_recorded" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use workflow_id <- result.try(required_string(
        fields.workflow_id,
        "workflow_id",
      ))
      use publication_id <- result.try(required_string(
        fields.publication_id,
        "publication_id",
      ))
      use series_id <- result.try(required_string(fields.series_id, "series_id"))
      use attempt_id <- result.try(required_string(
        fields.attempt_id,
        "attempt_id",
      ))
      use status <- result.try(required_string(fields.status, "status"))
      use required <- result.try(required_bool(fields.required, "required"))
      use retryable <- result.try(required_bool(fields.retryable, "retryable"))
      use retry_execution_available <- result.try(required_bool(
        fields.retry_execution_available,
        "retry_execution_available",
      ))
      Ok(PublicationAttemptRecorded(
        run_id,
        workflow_id,
        publication_id,
        series_id,
        attempt_id,
        status,
        required,
        retryable,
        retry_execution_available,
        fields.version_id,
        fields.manifest_ref,
        fields.manifest_sha256,
        fields.manifest_bytes,
        fields.error_code,
        fields.error_message,
      ))
    }
    "workflow_run_diagnostic" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use workflow_id <- result.try(required_string(
        fields.workflow_id,
        "workflow_id",
      ))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use reason <- result.try(required_string(fields.reason, "reason"))
      Ok(WorkflowRunDiagnostic(run_id, workflow_id, issue_id, reason))
    }
    "workflow_run_interrupted" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use workflow_id <- result.try(required_string(
        fields.workflow_id,
        "workflow_id",
      ))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use reason <- result.try(required_string(fields.reason, "reason"))
      Ok(WorkflowRunInterrupted(run_id, workflow_id, issue_id, reason))
    }
    "workflow_run_superseded" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use workflow_id <- result.try(required_string(
        fields.workflow_id,
        "workflow_id",
      ))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use superseded_by_run_id <- result.try(required_string(
        fields.superseded_by_run_id,
        "superseded_by_run_id",
      ))
      use reason <- result.try(required_string(fields.reason, "reason"))
      Ok(WorkflowRunSuperseded(
        run_id,
        workflow_id,
        issue_id,
        superseded_by_run_id,
        reason,
      ))
    }
    "workflow_repair_requested" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use workflow_id <- result.try(required_string(
        fields.workflow_id,
        "workflow_id",
      ))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use issue_identifier <- result.try(required_string(
        fields.issue_identifier,
        "issue_identifier",
      ))
      use requested_target <- result.try(required_string(
        fields.requested_target,
        "requested_target",
      ))
      use selected_step_id <- result.try(required_string(
        fields.selected_step_id,
        "selected_step_id",
      ))
      use failed_attempt_index <- result.try(required_int(
        fields.failed_attempt_index,
        "failed_attempt_index",
      ))
      use next_attempt_index <- result.try(required_int(
        fields.next_attempt_index,
        "next_attempt_index",
      ))
      use reason <- result.try(required_string(fields.reason, "reason"))
      Ok(WorkflowRepairRequested(
        run_id,
        workflow_id,
        issue_id,
        issue_identifier,
        requested_target,
        fields.requested_step_id,
        selected_step_id,
        failed_attempt_index,
        next_attempt_index,
        reason,
      ))
    }
    "step_attempt_prepared" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use workflow_id <- result.try(required_string(
        fields.workflow_id,
        "workflow_id",
      ))
      use step_id <- result.try(required_string(fields.step_id, "step_id"))
      use attempt_index <- result.try(required_int(
        fields.attempt_index,
        "attempt_index",
      ))
      use workspace_name <- result.try(required_string(
        fields.workspace_name,
        "workspace_name",
      ))
      use workspace_path <- result.try(required_string(
        fields.workspace_path,
        "workspace_path",
      ))
      use run_root <- result.try(required_string(fields.run_root, "run_root"))
      Ok(StepAttemptPrepared(
        run_id,
        workflow_id,
        step_id,
        attempt_index,
        workspace_name,
        workspace_path,
        run_root,
        fields.source_workspace_name,
        fields.source_workspace_path,
      ))
    }
    "step_attempt_started" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use workflow_id <- result.try(required_string(
        fields.workflow_id,
        "workflow_id",
      ))
      use step_id <- result.try(required_string(fields.step_id, "step_id"))
      use attempt_index <- result.try(required_int(
        fields.attempt_index,
        "attempt_index",
      ))
      use operator_session_id <- result.try(required_string(
        fields.operator_session_id,
        "operator_session_id",
      ))
      Ok(StepAttemptStarted(
        run_id,
        workflow_id,
        step_id,
        attempt_index,
        operator_session_id,
        fields.external_session_ref,
        option.unwrap(fields.continuation_capable, False),
      ))
    }
    "step_attempt_continuation_started" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use workflow_id <- result.try(required_string(
        fields.workflow_id,
        "workflow_id",
      ))
      use step_id <- result.try(required_string(fields.step_id, "step_id"))
      use attempt_index <- result.try(required_int(
        fields.attempt_index,
        "attempt_index",
      ))
      use session_id <- result.try(required_string(
        fields.session_id,
        "session_id",
      ))
      Ok(StepAttemptContinuationStarted(
        run_id,
        workflow_id,
        step_id,
        attempt_index,
        session_id,
      ))
    }
    "step_attempt_pi_session_recorded" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use issue_identifier <- result.try(required_string(
        fields.issue_identifier,
        "issue_identifier",
      ))
      use workflow_id <- result.try(required_string(
        fields.workflow_id,
        "workflow_id",
      ))
      use workflow_fingerprint <- result.try(required_string(
        fields.workflow_fingerprint,
        "workflow_fingerprint",
      ))
      use step_id <- result.try(required_string(fields.step_id, "step_id"))
      use workspace_name <- result.try(required_string(
        fields.workspace_name,
        "workspace_name",
      ))
      use attempt_index <- result.try(required_int(
        fields.attempt_index,
        "attempt_index",
      ))
      use workspace_path <- result.try(required_string(
        fields.workspace_path,
        "workspace_path",
      ))
      use session_id <- result.try(required_string(
        fields.session_id,
        "session_id",
      ))
      use session_file <- result.try(required_string(
        fields.session_file,
        "session_file",
      ))
      use task_ref <- result.try(optional_task_ref_fields(fields))
      case task_ref {
        Some(task_ref) ->
          Ok(StepAttemptPiSessionRecordedWithTask(
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
          ))
        None ->
          Ok(StepAttemptPiSessionRecorded(
            run_id,
            issue_id,
            issue_identifier,
            workflow_id,
            workflow_fingerprint,
            step_id,
            workspace_name,
            attempt_index,
            workspace_path,
            session_id,
            session_file,
          ))
      }
    }
    "step_attempt_finished" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use workflow_id <- result.try(required_string(
        fields.workflow_id,
        "workflow_id",
      ))
      use step_id <- result.try(required_string(fields.step_id, "step_id"))
      use attempt_index <- result.try(required_int(
        fields.attempt_index,
        "attempt_index",
      ))
      use outcome <- result.try(required_string(fields.outcome, "outcome"))
      use artifact_ref <- result.try(required_string(
        fields.artifact_ref,
        "artifact_ref",
      ))
      use artifact_sha256 <- result.try(required_string(
        fields.artifact_sha256,
        "artifact_sha256",
      ))
      use workspace_name <- result.try(required_string(
        fields.workspace_name,
        "workspace_name",
      ))
      use workspace_path <- result.try(required_string(
        fields.workspace_path,
        "workspace_path",
      ))
      use token_total <- result.try(required_int(
        fields.token_total,
        "token_total",
      ))
      use turns <- result.try(required_int(fields.turns, "turns"))
      Ok(StepAttemptFinished(
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
      ))
    }
    "workflow_step_recovery_started" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use workflow_id <- result.try(required_string(
        fields.workflow_id,
        "workflow_id",
      ))
      use step_id <- result.try(required_string(fields.step_id, "step_id"))
      use failed_attempt_index <- result.try(required_int(
        fields.failed_attempt_index,
        "failed_attempt_index",
      ))
      use recovery_attempt_number <- result.try(required_int(
        fields.recovery_attempt_number,
        "recovery_attempt_number",
      ))
      use recovery_session_id <- result.try(required_string(
        fields.recovery_session_id,
        "recovery_session_id",
      ))
      use prompt_ref <- result.try(required_string(
        fields.prompt_ref,
        "prompt_ref",
      ))
      Ok(WorkflowStepRecoveryStarted(
        run_id,
        workflow_id,
        step_id,
        failed_attempt_index,
        recovery_attempt_number,
        recovery_session_id,
        fields.model,
        prompt_ref,
      ))
    }
    "workflow_step_recovery_finished" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use workflow_id <- result.try(required_string(
        fields.workflow_id,
        "workflow_id",
      ))
      use step_id <- result.try(required_string(fields.step_id, "step_id"))
      use failed_attempt_index <- result.try(required_int(
        fields.failed_attempt_index,
        "failed_attempt_index",
      ))
      use recovery_attempt_number <- result.try(required_int(
        fields.recovery_attempt_number,
        "recovery_attempt_number",
      ))
      use recovery_session_id <- result.try(required_string(
        fields.recovery_session_id,
        "recovery_session_id",
      ))
      use result_value <- result.try(required_string(fields.result, "result"))
      use summary <- result.try(required_string(fields.summary, "summary"))
      use reason <- result.try(required_string(fields.reason, "reason"))
      Ok(WorkflowStepRecoveryFinished(
        run_id,
        workflow_id,
        step_id,
        failed_attempt_index,
        recovery_attempt_number,
        recovery_session_id,
        result_value,
        summary,
        reason,
        fields.retry_attempt_index,
      ))
    }
    "step_attempt_interrupted" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use workflow_id <- result.try(required_string(
        fields.workflow_id,
        "workflow_id",
      ))
      use step_id <- result.try(required_string(fields.step_id, "step_id"))
      use attempt_index <- result.try(required_int(
        fields.attempt_index,
        "attempt_index",
      ))
      use reason <- result.try(required_string(fields.reason, "reason"))
      Ok(StepAttemptInterrupted(
        run_id,
        workflow_id,
        step_id,
        attempt_index,
        reason,
      ))
    }
    "step_attempt_superseded" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use workflow_id <- result.try(required_string(
        fields.workflow_id,
        "workflow_id",
      ))
      use step_id <- result.try(required_string(fields.step_id, "step_id"))
      use attempt_index <- result.try(required_int(
        fields.attempt_index,
        "attempt_index",
      ))
      use superseded_by_attempt_index <- result.try(required_int(
        fields.superseded_by_attempt_index,
        "superseded_by_attempt_index",
      ))
      use reason <- result.try(required_string(fields.reason, "reason"))
      Ok(StepAttemptSuperseded(
        run_id,
        workflow_id,
        step_id,
        attempt_index,
        superseded_by_attempt_index,
        reason,
      ))
    }
    "retry_scheduled" -> {
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use issue_identifier <- result.try(required_string(
        fields.issue_identifier,
        "issue_identifier",
      ))
      use delay_ms <- result.try(required_int(fields.delay_ms, "delay_ms"))
      use generation <- result.try(required_int(fields.generation, "generation"))
      use reason <- result.try(required_string(fields.reason, "reason"))
      Ok(RetryScheduled(
        issue_id,
        issue_identifier,
        delay_ms,
        generation,
        reason,
      ))
    }
    "retry_cancelled" -> {
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use generation <- result.try(required_int(fields.generation, "generation"))
      use reason <- result.try(required_string(fields.reason, "reason"))
      Ok(RetryCancelled(issue_id, generation, reason))
    }
    "issue_counter_updated" -> {
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use issue_identifier <- result.try(required_string(
        fields.issue_identifier,
        "issue_identifier",
      ))
      use failure_attempts <- result.try(required_int(
        fields.failure_attempts,
        "failure_attempts",
      ))
      use worker_sessions <- result.try(required_int(
        fields.worker_sessions,
        "worker_sessions",
      ))
      use observed_updated_at_ms <- result.try(required_int(
        fields.observed_updated_at_ms,
        "observed_updated_at_ms",
      ))
      Ok(IssueCounterUpdated(
        issue_id,
        issue_identifier,
        failure_attempts,
        worker_sessions,
        observed_updated_at_ms,
        fields.source_run_id,
      ))
    }
    "known_workspace" -> {
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use issue_identifier <- result.try(required_string(
        fields.issue_identifier,
        "issue_identifier",
      ))
      use workspace_path <- result.try(required_string(
        fields.workspace_path,
        "workspace_path",
      ))
      Ok(KnownWorkspace(issue_id, issue_identifier, workspace_path))
    }
    "issue_parked" -> {
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use issue_identifier <- result.try(required_string(
        fields.issue_identifier,
        "issue_identifier",
      ))
      use reason <- result.try(required_string(fields.reason, "reason"))
      use observed_updated_at_ms <- result.try(required_int(
        fields.observed_updated_at_ms,
        "observed_updated_at_ms",
      ))
      Ok(IssueParked(issue_id, issue_identifier, reason, observed_updated_at_ms))
    }
    "issue_parked_v2" -> {
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use issue_identifier <- result.try(required_string(
        fields.issue_identifier,
        "issue_identifier",
      ))
      use reason <- result.try(required_string(fields.reason, "reason"))
      use release_policy <- result.try(required_string(
        fields.release_policy,
        "release_policy",
      ))
      use issue_fingerprint <- result.try(required_string(
        fields.issue_fingerprint,
        "issue_fingerprint",
      ))
      use observed_updated_at_ms <- result.try(required_int(
        fields.observed_updated_at_ms,
        "observed_updated_at_ms",
      ))
      Ok(IssueParkedV2(
        issue_id,
        issue_identifier,
        reason,
        release_policy,
        issue_fingerprint,
        observed_updated_at_ms,
      ))
    }
    "issue_unparked" -> {
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use issue_identifier <- result.try(required_string(
        fields.issue_identifier,
        "issue_identifier",
      ))
      use reason <- result.try(required_string(fields.reason, "reason"))
      Ok(IssueUnparked(issue_id, issue_identifier, reason))
    }
    "linear_command_seen" -> {
      use comment_id <- result.try(required_string(
        fields.comment_id,
        "comment_id",
      ))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use author_id <- result.try(required_string(fields.author_id, "author_id"))
      use command_name <- result.try(required_string(
        fields.command_name,
        "command_name",
      ))
      use excerpt <- result.try(required_string(fields.excerpt, "excerpt"))
      Ok(LinearCommandSeen(
        comment_id,
        issue_id,
        author_id,
        command_name,
        excerpt,
      ))
    }
    "linear_command_started" -> {
      use comment_id <- result.try(required_string(
        fields.comment_id,
        "comment_id",
      ))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use command_name <- result.try(required_string(
        fields.command_name,
        "command_name",
      ))
      Ok(LinearCommandStarted(comment_id, issue_id, command_name))
    }
    "linear_command_completed" -> {
      use comment_id <- result.try(required_string(
        fields.comment_id,
        "comment_id",
      ))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use status <- result.try(required_string(fields.status, "status"))
      use message_excerpt <- result.try(required_string(
        fields.message_excerpt,
        "message_excerpt",
      ))
      Ok(LinearCommandCompleted(comment_id, issue_id, status, message_excerpt))
    }
    "linear_command_acked" -> {
      use comment_id <- result.try(required_string(
        fields.comment_id,
        "comment_id",
      ))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      Ok(LinearCommandAcked(comment_id, issue_id))
    }
    "remote_command_seen" -> {
      use backend_kind <- result.try(required_task_backend_kind(fields))
      use event_id <- result.try(required_string(fields.event_id, "event_id"))
      use task_remote_id <- result.try(required_string(
        fields.task_remote_id,
        "task_remote_id",
      ))
      use author_id <- result.try(required_string(fields.author_id, "author_id"))
      use command_name <- result.try(required_string(
        fields.command_name,
        "command_name",
      ))
      use excerpt <- result.try(required_string(fields.excerpt, "excerpt"))
      Ok(RemoteCommandSeen(
        backend_kind,
        event_id,
        task_remote_id,
        fields.task_key,
        author_id,
        command_name,
        excerpt,
      ))
    }
    "remote_command_started" -> {
      use backend_kind <- result.try(required_task_backend_kind(fields))
      use event_id <- result.try(required_string(fields.event_id, "event_id"))
      use task_remote_id <- result.try(required_string(
        fields.task_remote_id,
        "task_remote_id",
      ))
      use command_name <- result.try(required_string(
        fields.command_name,
        "command_name",
      ))
      Ok(RemoteCommandStarted(
        backend_kind,
        event_id,
        task_remote_id,
        command_name,
      ))
    }
    "remote_command_completed" -> {
      use backend_kind <- result.try(required_task_backend_kind(fields))
      use event_id <- result.try(required_string(fields.event_id, "event_id"))
      use task_remote_id <- result.try(required_string(
        fields.task_remote_id,
        "task_remote_id",
      ))
      use status <- result.try(required_string(fields.status, "status"))
      use message_excerpt <- result.try(required_string(
        fields.message_excerpt,
        "message_excerpt",
      ))
      Ok(RemoteCommandCompleted(
        backend_kind,
        event_id,
        task_remote_id,
        status,
        message_excerpt,
      ))
    }
    "remote_command_acked" -> {
      use backend_kind <- result.try(required_task_backend_kind(fields))
      use event_id <- result.try(required_string(fields.event_id, "event_id"))
      use task_remote_id <- result.try(required_string(
        fields.task_remote_id,
        "task_remote_id",
      ))
      Ok(RemoteCommandAcked(backend_kind, event_id, task_remote_id))
    }
    "scheduled_job_due"
    | "scheduled_job_skipped"
    | "scheduled_run_pending"
    | "scheduled_run_pending_blocked"
    | "scheduled_run_pending_cancelled"
    | "scheduled_run_started"
    | "scheduled_run_succeeded"
    | "scheduled_run_failed"
    | "scheduled_run_retry_scheduled"
    | "scheduled_run_retry_cancelled"
    | "scheduled_failure_reported"
    | "scheduled_failure_report_failed" -> {
      use decoded <- result.try(
        scheduled_record.decode(
          fields.kind,
          scheduled_record.ScheduledFields(
            job_id: fields.job_id,
            workflow_id: fields.workflow_id,
            due_at_ms: fields.due_at_ms,
            run_id: fields.run_id,
            trigger: fields.trigger,
            reason: fields.reason,
            skipped_count: fields.skipped_count,
            requested_at_ms: fields.requested_at_ms,
            observed_at_ms: fields.observed_at_ms,
            cancelled_at_ms: fields.cancelled_at_ms,
            started_at_ms: fields.started_at_ms,
            finished_at_ms: fields.finished_at_ms,
            attempt: fields.attempt,
            session_id: fields.session_id,
            retry_exhausted: fields.retry_exhausted,
            next_attempt: fields.next_attempt,
            generation: fields.generation,
            delay_ms: fields.delay_ms,
            dedupe_key: fields.dedupe_key,
            linear_issue_id: fields.linear_issue_id,
            action: fields.action,
            error_code: fields.error_code,
            error_message: fields.error_message,
            next_retry_at_ms: fields.next_retry_at_ms,
            run_root: fields.run_root,
            token_total: fields.token_total,
            turns: fields.turns,
          ),
          fn(value, field) {
            required_string(value, field) |> result.map_error(describe_error)
          },
          fn(value, field) {
            required_int(value, field) |> result.map_error(describe_error)
          },
          fn(value, field) {
            required_bool(value, field) |> result.map_error(describe_error)
          },
        )
        |> result.map_error(InvalidRecord),
      )
      case decoded {
        scheduled_record.ScheduledJobDueBody(
          job_id,
          workflow_id,
          due_at_ms,
          run_id,
          trigger,
        ) ->
          Ok(ScheduledJobDue(job_id, workflow_id, due_at_ms, run_id, trigger))
        scheduled_record.ScheduledJobSkippedBody(
          job_id,
          workflow_id,
          due_at_ms,
          run_id,
          reason,
          skipped_count,
        ) ->
          Ok(ScheduledJobSkipped(
            job_id,
            workflow_id,
            due_at_ms,
            run_id,
            reason,
            skipped_count,
          ))
        scheduled_record.ScheduledRunPendingBody(
          job_id,
          workflow_id,
          due_at_ms,
          run_id,
          trigger,
          requested_at_ms,
        ) ->
          Ok(ScheduledRunPending(
            job_id,
            workflow_id,
            due_at_ms,
            run_id,
            trigger,
            requested_at_ms,
          ))
        scheduled_record.ScheduledRunPendingBlockedBody(
          job_id,
          workflow_id,
          due_at_ms,
          run_id,
          reason,
          observed_at_ms,
        ) ->
          Ok(ScheduledRunPendingBlocked(
            job_id,
            workflow_id,
            due_at_ms,
            run_id,
            reason,
            observed_at_ms,
          ))
        scheduled_record.ScheduledRunPendingCancelledBody(
          job_id,
          workflow_id,
          due_at_ms,
          run_id,
          reason,
          cancelled_at_ms,
        ) ->
          Ok(ScheduledRunPendingCancelled(
            job_id,
            workflow_id,
            due_at_ms,
            run_id,
            reason,
            cancelled_at_ms,
          ))
        scheduled_record.ScheduledRunStartedBody(
          job_id,
          workflow_id,
          due_at_ms,
          started_at_ms,
          run_id,
          attempt,
          session_id,
          run_root,
        ) ->
          Ok(ScheduledRunStarted(
            job_id,
            workflow_id,
            due_at_ms,
            started_at_ms,
            run_id,
            attempt,
            session_id,
            run_root,
          ))
        scheduled_record.ScheduledRunSucceededBody(
          job_id,
          workflow_id,
          due_at_ms,
          run_id,
          attempt,
          finished_at_ms,
          token_total,
          turns,
        ) ->
          Ok(ScheduledRunSucceeded(
            job_id,
            workflow_id,
            due_at_ms,
            run_id,
            attempt,
            finished_at_ms,
            token_total,
            turns,
          ))
        scheduled_record.ScheduledRunFailedBody(
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
          Ok(ScheduledRunFailed(
            job_id,
            workflow_id,
            due_at_ms,
            run_id,
            attempt,
            finished_at_ms,
            reason,
            retry_exhausted,
            run_root,
          ))
        scheduled_record.ScheduledRunRetryScheduledBody(
          job_id,
          workflow_id,
          due_at_ms,
          run_id,
          next_attempt,
          delay_ms,
          generation,
          reason,
        ) ->
          Ok(ScheduledRunRetryScheduled(
            job_id,
            workflow_id,
            due_at_ms,
            run_id,
            next_attempt,
            delay_ms,
            generation,
            reason,
          ))
        scheduled_record.ScheduledRunRetryCancelledBody(
          job_id,
          run_id,
          generation,
          reason,
        ) -> Ok(ScheduledRunRetryCancelled(job_id, run_id, generation, reason))
        scheduled_record.ScheduledFailureReportedBody(
          job_id,
          workflow_id,
          due_at_ms,
          run_id,
          attempt,
          dedupe_key,
          linear_issue_id,
          action,
        ) ->
          Ok(ScheduledFailureReported(
            job_id,
            workflow_id,
            due_at_ms,
            run_id,
            attempt,
            dedupe_key,
            linear_issue_id,
            action,
          ))
        scheduled_record.ScheduledFailureReportFailedBody(
          job_id,
          workflow_id,
          due_at_ms,
          run_id,
          attempt,
          dedupe_key,
          error_code,
          error_message,
          next_retry_at_ms,
          generation,
        ) ->
          Ok(ScheduledFailureReportFailed(
            job_id,
            workflow_id,
            due_at_ms,
            run_id,
            attempt,
            dedupe_key,
            error_code,
            error_message,
            next_retry_at_ms,
            generation,
          ))
      }
    }
    "outbox_pending" -> {
      use outbox_id <- result.try(required_string(fields.outbox_id, "outbox_id"))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use outbox_kind <- result.try(required_string(
        fields.outbox_kind,
        "outbox_kind",
      ))
      use dedupe_key <- result.try(required_string(
        fields.dedupe_key,
        "dedupe_key",
      ))
      Ok(OutboxPending(outbox_id, issue_id, outbox_kind, dedupe_key))
    }
    "outbox_pending_v2" -> {
      use outbox_id <- result.try(required_string(fields.outbox_id, "outbox_id"))
      use outbox_kind <- result.try(required_string(
        fields.outbox_kind,
        "outbox_kind",
      ))
      use dedupe_key <- result.try(required_string(
        fields.dedupe_key,
        "dedupe_key",
      ))
      use payload_json <- result.try(required_string(
        fields.payload_json,
        "payload_json",
      ))
      use task_ref <- result.try(optional_task_ref_fields(fields))
      case task_ref {
        Some(task_ref) ->
          Ok(OutboxPendingV2WithTask(
            outbox_id,
            task_ref,
            outbox_kind,
            dedupe_key,
            payload_json,
          ))
        None -> {
          use issue_id <- result.try(required_string(
            fields.issue_id,
            "issue_id",
          ))
          Ok(OutboxPendingV2(
            outbox_id,
            issue_id,
            outbox_kind,
            dedupe_key,
            payload_json,
          ))
        }
      }
    }
    "outbox_completed" -> {
      use outbox_id <- result.try(required_string(fields.outbox_id, "outbox_id"))
      use outbox_kind <- result.try(required_string(
        fields.outbox_kind,
        "outbox_kind",
      ))
      use task_ref <- result.try(optional_task_ref_fields(fields))
      case task_ref {
        Some(task_ref) ->
          Ok(OutboxCompletedWithTask(outbox_id, task_ref, outbox_kind))
        None -> {
          use issue_id <- result.try(required_string(
            fields.issue_id,
            "issue_id",
          ))
          Ok(OutboxCompleted(outbox_id, issue_id, outbox_kind))
        }
      }
    }
    "outbox_failed" -> {
      use outbox_id <- result.try(required_string(fields.outbox_id, "outbox_id"))
      use outbox_kind <- result.try(required_string(
        fields.outbox_kind,
        "outbox_kind",
      ))
      use error_code <- result.try(required_string(
        fields.error_code,
        "error_code",
      ))
      use task_ref <- result.try(optional_task_ref_fields(fields))
      case task_ref {
        Some(task_ref) ->
          Ok(OutboxFailedWithTask(outbox_id, task_ref, outbox_kind, error_code))
        None -> {
          use issue_id <- result.try(required_string(
            fields.issue_id,
            "issue_id",
          ))
          Ok(OutboxFailed(outbox_id, issue_id, outbox_kind, error_code))
        }
      }
    }
    "workstream_created"
    | "workstream_assigned"
    | "workstream_artifact_recorded"
    | "workstream_handoff_recorded"
    | "workstream_phase_run_queued" -> workstream_body_from_fields(fields)
    other -> Error(UnknownKind(other))
  }
}

fn fields_decoder() -> decode.Decoder(RecordFields) {
  use schema_version <- decode.field("schema_version", decode.int)
  use record_id <- decode.field("record_id", decode.string)
  use at_ms <- decode.field("at_ms", decode.int)
  use kind <- decode.field("kind", decode.string)
  use run_id <- decode.optional_field(
    "run_id",
    None,
    decode.optional(decode.string),
  )
  use workflow_id <- decode.optional_field(
    "workflow_id",
    None,
    decode.optional(decode.string),
  )
  use workflow_fingerprint <- decode.optional_field(
    "workflow_fingerprint",
    None,
    decode.optional(decode.string),
  )
  use issue_id <- decode.optional_field(
    "issue_id",
    None,
    decode.optional(decode.string),
  )
  use issue_identifier <- decode.optional_field(
    "issue_identifier",
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
  use workspace_path <- decode.optional_field(
    "workspace_path",
    None,
    decode.optional(decode.string),
  )
  use workspace_name <- decode.optional_field(
    "workspace_name",
    None,
    decode.optional(decode.string),
  )
  use run_root <- decode.optional_field(
    "run_root",
    None,
    decode.optional(decode.string),
  )
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
  use step_id <- decode.optional_field(
    "step_id",
    None,
    decode.optional(decode.string),
  )
  use attempt_index <- decode.optional_field(
    "attempt_index",
    None,
    decode.optional(decode.int),
  )
  use operator_session_id <- decode.optional_field(
    "operator_session_id",
    None,
    decode.optional(decode.string),
  )
  use external_session_ref <- decode.optional_field(
    "external_session_ref",
    None,
    decode.optional(decode.string),
  )
  use continuation_capable <- decode.optional_field(
    "continuation_capable",
    None,
    decode.optional(decode.bool),
  )
  use session_id <- decode.optional_field(
    "session_id",
    None,
    decode.optional(decode.string),
  )
  use session_file <- decode.optional_field(
    "session_file",
    None,
    decode.optional(decode.string),
  )
  use outcome <- decode.optional_field(
    "outcome",
    None,
    decode.optional(decode.string),
  )
  use artifact_ref <- decode.optional_field(
    "artifact_ref",
    None,
    decode.optional(decode.string),
  )
  use artifact_sha256 <- decode.optional_field(
    "artifact_sha256",
    None,
    decode.optional(decode.string),
  )
  use artifact_bytes <- decode.optional_field(
    "artifact_bytes",
    None,
    decode.optional(decode.int),
  )
  use publication_id <- decode.optional_field(
    "publication_id",
    None,
    decode.optional(decode.string),
  )
  use series_id <- decode.optional_field(
    "series_id",
    None,
    decode.optional(decode.string),
  )
  use attempt_id <- decode.optional_field(
    "attempt_id",
    None,
    decode.optional(decode.string),
  )
  use retryable <- decode.optional_field(
    "retryable",
    None,
    decode.optional(decode.bool),
  )
  use retry_execution_available <- decode.optional_field(
    "retry_execution_available",
    None,
    decode.optional(decode.bool),
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
  use superseded_by_run_id <- decode.optional_field(
    "superseded_by_run_id",
    None,
    decode.optional(decode.string),
  )
  use superseded_by_attempt_index <- decode.optional_field(
    "superseded_by_attempt_index",
    None,
    decode.optional(decode.int),
  )
  use classification <- decode.optional_field(
    "classification",
    None,
    decode.optional(decode.string),
  )
  use token_total <- decode.optional_field(
    "token_total",
    None,
    decode.optional(decode.int),
  )
  use turns <- decode.optional_field("turns", None, decode.optional(decode.int))
  use reason <- decode.optional_field(
    "reason",
    None,
    decode.optional(decode.string),
  )
  use delay_ms <- decode.optional_field(
    "delay_ms",
    None,
    decode.optional(decode.int),
  )
  use generation <- decode.optional_field(
    "generation",
    None,
    decode.optional(decode.int),
  )
  use failure_attempts <- decode.optional_field(
    "failure_attempts",
    None,
    decode.optional(decode.int),
  )
  use worker_sessions <- decode.optional_field(
    "worker_sessions",
    None,
    decode.optional(decode.int),
  )
  use observed_updated_at_ms <- decode.optional_field(
    "observed_updated_at_ms",
    None,
    decode.optional(decode.int),
  )
  use source_run_id <- decode.optional_field(
    "source_run_id",
    None,
    decode.optional(decode.string),
  )
  use release_policy <- decode.optional_field(
    "release_policy",
    None,
    decode.optional(decode.string),
  )
  use issue_fingerprint <- decode.optional_field(
    "issue_fingerprint",
    None,
    decode.optional(decode.string),
  )
  use repair_mode <- decode.optional_field(
    "repair_mode",
    None,
    decode.optional(decode.string),
  )
  use source_evidence <- decode.optional_field(
    "source_evidence",
    [],
    decode.list(of: decode.string),
  )
  use requested_target <- decode.optional_field(
    "requested_target",
    None,
    decode.optional(decode.string),
  )
  use requested_step_id <- decode.optional_field(
    "requested_step_id",
    None,
    decode.optional(decode.string),
  )
  use selected_step_id <- decode.optional_field(
    "selected_step_id",
    None,
    decode.optional(decode.string),
  )
  use failed_attempt_index <- decode.optional_field(
    "failed_attempt_index",
    None,
    decode.optional(decode.int),
  )
  use next_attempt_index <- decode.optional_field(
    "next_attempt_index",
    None,
    decode.optional(decode.int),
  )
  use backend_kind <- decode.optional_field(
    "backend_kind",
    None,
    decode.optional(decode.string),
  )
  use event_id <- decode.optional_field(
    "event_id",
    None,
    decode.optional(decode.string),
  )
  use comment_id <- decode.optional_field(
    "comment_id",
    None,
    decode.optional(decode.string),
  )
  use author_id <- decode.optional_field(
    "author_id",
    None,
    decode.optional(decode.string),
  )
  use command_name <- decode.optional_field(
    "command_name",
    None,
    decode.optional(decode.string),
  )
  use excerpt <- decode.optional_field(
    "excerpt",
    None,
    decode.optional(decode.string),
  )
  use status <- decode.optional_field(
    "status",
    None,
    decode.optional(decode.string),
  )
  use required <- decode.optional_field(
    "required",
    None,
    decode.optional(decode.bool),
  )
  use message_excerpt <- decode.optional_field(
    "message_excerpt",
    None,
    decode.optional(decode.string),
  )
  use outbox_id <- decode.optional_field(
    "outbox_id",
    None,
    decode.optional(decode.string),
  )
  use outbox_kind <- decode.optional_field(
    "outbox_kind",
    None,
    decode.optional(decode.string),
  )
  use dedupe_key <- decode.optional_field(
    "dedupe_key",
    None,
    decode.optional(decode.string),
  )
  use payload_json <- decode.optional_field(
    "payload_json",
    None,
    decode.optional(decode.string),
  )
  use error_code <- decode.optional_field(
    "error_code",
    None,
    decode.optional(decode.string),
  )
  use job_id <- decode.optional_field(
    "job_id",
    None,
    decode.optional(decode.string),
  )
  use due_at_ms <- decode.optional_field(
    "due_at_ms",
    None,
    decode.optional(decode.int),
  )
  use trigger <- decode.optional_field(
    "trigger",
    None,
    decode.optional(decode.string),
  )
  use skipped_count <- decode.optional_field(
    "skipped_count",
    None,
    decode.optional(decode.int),
  )
  use requested_at_ms <- decode.optional_field(
    "requested_at_ms",
    None,
    decode.optional(decode.int),
  )
  use observed_at_ms <- decode.optional_field(
    "observed_at_ms",
    None,
    decode.optional(decode.int),
  )
  use cancelled_at_ms <- decode.optional_field(
    "cancelled_at_ms",
    None,
    decode.optional(decode.int),
  )
  use started_at_ms <- decode.optional_field(
    "started_at_ms",
    None,
    decode.optional(decode.int),
  )
  use finished_at_ms <- decode.optional_field(
    "finished_at_ms",
    None,
    decode.optional(decode.int),
  )
  use attempt <- decode.optional_field(
    "attempt",
    None,
    decode.optional(decode.int),
  )
  use retry_exhausted <- decode.optional_field(
    "retry_exhausted",
    None,
    decode.optional(decode.bool),
  )
  use next_attempt <- decode.optional_field(
    "next_attempt",
    None,
    decode.optional(decode.int),
  )
  use linear_issue_id <- decode.optional_field(
    "linear_issue_id",
    None,
    decode.optional(decode.string),
  )
  use action <- decode.optional_field(
    "action",
    None,
    decode.optional(decode.string),
  )
  use error_message <- decode.optional_field(
    "error_message",
    None,
    decode.optional(decode.string),
  )
  use next_retry_at_ms <- decode.optional_field(
    "next_retry_at_ms",
    None,
    decode.optional(decode.int),
  )
  use workstream_id <- decode.optional_field(
    "workstream_id",
    None,
    decode.optional(decode.string),
  )
  use assignment_id <- decode.optional_field(
    "assignment_id",
    None,
    decode.optional(decode.string),
  )
  use playbook_id <- decode.optional_field(
    "playbook_id",
    None,
    decode.optional(decode.string),
  )
  use idempotency_key <- decode.optional_field(
    "idempotency_key",
    None,
    decode.optional(decode.string),
  )
  use artifact_id <- decode.optional_field(
    "artifact_id",
    None,
    decode.optional(decode.string),
  )
  use artifact_type <- decode.optional_field(
    "artifact_type",
    None,
    decode.optional(decode.string),
  )
  use snapshot_ref <- decode.optional_field(
    "snapshot_ref",
    None,
    decode.optional(decode.string),
  )
  use snapshot_sha256 <- decode.optional_field(
    "snapshot_sha256",
    None,
    decode.optional(decode.string),
  )
  use snapshot_bytes <- decode.optional_field(
    "snapshot_bytes",
    None,
    decode.optional(decode.int),
  )
  use original_path <- decode.optional_field(
    "original_path",
    None,
    decode.optional(decode.string),
  )
  use contract_type <- decode.optional_field(
    "contract_type",
    None,
    decode.optional(decode.string),
  )
  use media_type <- decode.optional_field(
    "media_type",
    None,
    decode.optional(decode.string),
  )
  use producer_workflow_id <- decode.optional_field(
    "producer_workflow_id",
    None,
    decode.optional(decode.string),
  )
  use producer_run_id <- decode.optional_field(
    "producer_run_id",
    None,
    decode.optional(decode.string),
  )
  use producer_step_id <- decode.optional_field(
    "producer_step_id",
    None,
    decode.optional(decode.string),
  )
  use handoff_id <- decode.optional_field(
    "handoff_id",
    None,
    decode.optional(decode.string),
  )
  use handoff_ref <- decode.optional_field(
    "handoff_ref",
    None,
    decode.optional(decode.string),
  )
  use handoff_sha256 <- decode.optional_field(
    "handoff_sha256",
    None,
    decode.optional(decode.string),
  )
  use handoff_bytes <- decode.optional_field(
    "handoff_bytes",
    None,
    decode.optional(decode.int),
  )
  use source_workflow_id <- decode.optional_field(
    "source_workflow_id",
    None,
    decode.optional(decode.string),
  )
  use phase_run_id <- decode.optional_field(
    "phase_run_id",
    None,
    decode.optional(decode.string),
  )
  use action_id <- decode.optional_field(
    "action_id",
    None,
    decode.optional(decode.string),
  )
  use input_bundle_ref <- decode.optional_field(
    "input_bundle_ref",
    None,
    decode.optional(decode.string),
  )
  use input_bundle_sha256 <- decode.optional_field(
    "input_bundle_sha256",
    None,
    decode.optional(decode.string),
  )
  use input_bundle_bytes <- decode.optional_field(
    "input_bundle_bytes",
    None,
    decode.optional(decode.int),
  )
  use recovery_attempt_number <- decode.optional_field(
    "recovery_attempt_number",
    None,
    decode.optional(decode.int),
  )
  use recovery_session_id <- decode.optional_field(
    "recovery_session_id",
    None,
    decode.optional(decode.string),
  )
  use model <- decode.optional_field(
    "model",
    None,
    decode.optional(decode.string),
  )
  use prompt_ref <- decode.optional_field(
    "prompt_ref",
    None,
    decode.optional(decode.string),
  )
  use result <- decode.optional_field(
    "result",
    None,
    decode.optional(decode.string),
  )
  use summary <- decode.optional_field(
    "summary",
    None,
    decode.optional(decode.string),
  )
  use retry_attempt_index <- decode.optional_field(
    "retry_attempt_index",
    None,
    decode.optional(decode.int),
  )
  decode.success(RecordFields(
    schema_version: schema_version,
    record_id: record_id,
    at_ms: at_ms,
    kind: kind,
    run_id: run_id,
    workflow_id: workflow_id,
    workflow_fingerprint: workflow_fingerprint,
    issue_id: issue_id,
    issue_identifier: issue_identifier,
    task_backend_kind: task_backend_kind,
    task_remote_id: task_remote_id,
    task_key: task_key,
    task_url: task_url,
    workspace_path: workspace_path,
    workspace_name: workspace_name,
    run_root: run_root,
    source_workspace_name: source_workspace_name,
    source_workspace_path: source_workspace_path,
    step_id: step_id,
    attempt_index: attempt_index,
    operator_session_id: operator_session_id,
    external_session_ref: external_session_ref,
    continuation_capable: continuation_capable,
    session_id: session_id,
    session_file: session_file,
    outcome: outcome,
    artifact_ref: artifact_ref,
    artifact_sha256: artifact_sha256,
    artifact_bytes: artifact_bytes,
    publication_id: publication_id,
    series_id: series_id,
    attempt_id: attempt_id,
    retryable: retryable,
    retry_execution_available: retry_execution_available,
    version_id: version_id,
    manifest_ref: manifest_ref,
    manifest_sha256: manifest_sha256,
    manifest_bytes: manifest_bytes,
    superseded_by_run_id: superseded_by_run_id,
    superseded_by_attempt_index: superseded_by_attempt_index,
    classification: classification,
    token_total: token_total,
    turns: turns,
    reason: reason,
    delay_ms: delay_ms,
    generation: generation,
    failure_attempts: failure_attempts,
    worker_sessions: worker_sessions,
    observed_updated_at_ms: observed_updated_at_ms,
    source_run_id: source_run_id,
    release_policy: release_policy,
    issue_fingerprint: issue_fingerprint,
    repair_mode: repair_mode,
    source_evidence: source_evidence,
    requested_target: requested_target,
    requested_step_id: requested_step_id,
    selected_step_id: selected_step_id,
    failed_attempt_index: failed_attempt_index,
    next_attempt_index: next_attempt_index,
    backend_kind: backend_kind,
    event_id: event_id,
    comment_id: comment_id,
    author_id: author_id,
    command_name: command_name,
    excerpt: excerpt,
    status: status,
    required: required,
    message_excerpt: message_excerpt,
    outbox_id: outbox_id,
    outbox_kind: outbox_kind,
    dedupe_key: dedupe_key,
    payload_json: payload_json,
    error_code: error_code,
    job_id: job_id,
    due_at_ms: due_at_ms,
    trigger: trigger,
    skipped_count: skipped_count,
    requested_at_ms: requested_at_ms,
    observed_at_ms: observed_at_ms,
    cancelled_at_ms: cancelled_at_ms,
    started_at_ms: started_at_ms,
    finished_at_ms: finished_at_ms,
    attempt: attempt,
    retry_exhausted: retry_exhausted,
    next_attempt: next_attempt,
    linear_issue_id: linear_issue_id,
    action: action,
    error_message: error_message,
    next_retry_at_ms: next_retry_at_ms,
    workstream_id: workstream_id,
    assignment_id: assignment_id,
    playbook_id: playbook_id,
    idempotency_key: idempotency_key,
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
    handoff_id: handoff_id,
    handoff_ref: handoff_ref,
    handoff_sha256: handoff_sha256,
    handoff_bytes: handoff_bytes,
    source_workflow_id: source_workflow_id,
    phase_run_id: phase_run_id,
    action_id: action_id,
    input_bundle_ref: input_bundle_ref,
    input_bundle_sha256: input_bundle_sha256,
    input_bundle_bytes: input_bundle_bytes,
    recovery_attempt_number: recovery_attempt_number,
    recovery_session_id: recovery_session_id,
    model: model,
    prompt_ref: prompt_ref,
    result: result,
    summary: summary,
    retry_attempt_index: retry_attempt_index,
  ))
}

fn optional_task_ref_fields(
  fields: RecordFields,
) -> Result(Option(TaskRefFields), DecodeError) {
  case fields.task_backend_kind, fields.task_remote_id {
    Some(task_backend_kind), Some(task_remote_id) ->
      Ok(
        Some(TaskRefFields(
          task_backend_kind: task_backend_kind,
          task_remote_id: task_remote_id,
          task_key: fields.task_key,
          task_url: fields.task_url,
        )),
      )
    None, None ->
      case fields.task_key, fields.task_url {
        None, None -> Ok(None)
        _, _ -> Error(InvalidRecord("missing task_backend_kind"))
      }
    None, Some(_) -> Error(InvalidRecord("missing task_backend_kind"))
    Some(_), None -> Error(InvalidRecord("missing task_remote_id"))
  }
}

fn required_task_ref_fields(
  fields: RecordFields,
) -> Result(TaskRefFields, DecodeError) {
  use task_ref <- result.try(optional_task_ref_fields(fields))
  case task_ref {
    Some(task_ref) -> Ok(task_ref)
    None -> Error(InvalidRecord("missing task_backend_kind"))
  }
}

fn required_task_backend_kind(
  fields: RecordFields,
) -> Result(String, DecodeError) {
  case fields.task_backend_kind, fields.backend_kind {
    Some(kind), _ -> Ok(kind)
    None, Some(kind) -> Ok(kind)
    None, None -> Error(InvalidRecord("missing task_backend_kind"))
  }
}

fn required_string(
  value: Option(String),
  field: String,
) -> Result(String, DecodeError) {
  case value {
    Some(value) -> Ok(value)
    None -> Error(InvalidRecord("missing " <> field))
  }
}

fn required_int(value: Option(Int), field: String) -> Result(Int, DecodeError) {
  case value {
    Some(value) -> Ok(value)
    None -> Error(InvalidRecord("missing " <> field))
  }
}

fn required_bool(
  value: Option(Bool),
  field: String,
) -> Result(Bool, DecodeError) {
  case value {
    Some(value) -> Ok(value)
    None -> Error(InvalidRecord("missing " <> field))
  }
}

fn decode_workflow_contract_record(
  fields: RecordFields,
  make_record: fn(String, String, String, String, String, Int) -> RecordBody,
) -> Result(RecordBody, DecodeError) {
  use run_id <- result.try(required_string(fields.run_id, "run_id"))
  use workflow_id <- result.try(required_string(
    fields.workflow_id,
    "workflow_id",
  ))
  use workflow_fingerprint <- result.try(required_string(
    fields.workflow_fingerprint,
    "workflow_fingerprint",
  ))
  use artifact_ref <- result.try(required_string(
    fields.artifact_ref,
    "artifact_ref",
  ))
  use artifact_sha256 <- result.try(required_string(
    fields.artifact_sha256,
    "artifact_sha256",
  ))
  use artifact_bytes <- result.try(required_int(
    fields.artifact_bytes,
    "artifact_bytes",
  ))
  Ok(make_record(
    run_id,
    workflow_id,
    workflow_fingerprint,
    artifact_ref,
    artifact_sha256,
    artifact_bytes,
  ))
}

fn redact_body(body: RecordBody, secrets: List(String)) -> RecordBody {
  case body {
    LinearCommandSeen(comment_id, issue_id, author_id, command_name, excerpt) ->
      LinearCommandSeen(
        comment_id,
        issue_id,
        author_id,
        command_name,
        safe_excerpt(excerpt, secrets),
      )
    LinearCommandCompleted(comment_id, issue_id, status, message_excerpt) ->
      LinearCommandCompleted(
        comment_id,
        issue_id,
        status,
        safe_excerpt(message_excerpt, secrets),
      )
    RemoteCommandSeen(
      backend_kind,
      event_id,
      task_remote_id,
      task_key,
      author_id,
      command_name,
      excerpt,
    ) ->
      RemoteCommandSeen(
        backend_kind,
        event_id,
        task_remote_id,
        task_key,
        author_id,
        command_name,
        safe_excerpt(excerpt, secrets),
      )
    RemoteCommandCompleted(
      backend_kind,
      event_id,
      task_remote_id,
      status,
      message_excerpt,
    ) ->
      RemoteCommandCompleted(
        backend_kind,
        event_id,
        task_remote_id,
        status,
        safe_excerpt(message_excerpt, secrets),
      )
    OutboxPendingV2(outbox_id, issue_id, outbox_kind, dedupe_key, payload_json) ->
      OutboxPendingV2(
        outbox_id,
        issue_id,
        outbox_kind,
        dedupe_key,
        safe_payload(payload_json, secrets),
      )
    OutboxPendingV2WithTask(
      outbox_id,
      task_ref,
      outbox_kind,
      dedupe_key,
      payload_json,
    ) ->
      OutboxPendingV2WithTask(
        outbox_id,
        task_ref,
        outbox_kind,
        dedupe_key,
        safe_payload(payload_json, secrets),
      )
    ScheduledFailureReportFailed(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      attempt,
      dedupe_key,
      error_code,
      error_message,
      next_retry_at_ms,
      generation,
    ) ->
      ScheduledFailureReportFailed(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        attempt,
        dedupe_key,
        error_code,
        safe_excerpt(error_message, secrets),
        next_retry_at_ms,
        generation,
      )
    PublicationAttemptRecorded(
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
    ) ->
      PublicationAttemptRecorded(
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
        case error_message {
          Some(message) -> Some(safe_excerpt(message, secrets))
          None -> None
        },
      )
    StepAttemptPiSessionRecorded(
      run_id,
      issue_id,
      issue_identifier,
      workflow_id,
      workflow_fingerprint,
      step_id,
      workspace_name,
      attempt_index,
      _,
      session_id,
      _,
    ) ->
      StepAttemptPiSessionRecorded(
        run_id,
        issue_id,
        issue_identifier,
        workflow_id,
        workflow_fingerprint,
        step_id,
        workspace_name,
        attempt_index,
        "[redacted workspace path]",
        session_id,
        "[redacted pi session file]",
      )
    StepAttemptPiSessionRecordedWithTask(
      run_id,
      issue_id,
      issue_identifier,
      task_ref,
      workflow_id,
      workflow_fingerprint,
      step_id,
      workspace_name,
      attempt_index,
      _,
      session_id,
      _,
    ) ->
      StepAttemptPiSessionRecordedWithTask(
        run_id,
        issue_id,
        issue_identifier,
        task_ref,
        workflow_id,
        workflow_fingerprint,
        step_id,
        workspace_name,
        attempt_index,
        "[redacted workspace path]",
        session_id,
        "[redacted pi session file]",
      )
    other -> other
  }
}

fn safe_payload(value: String, secrets: List(String)) -> String {
  log.redact("outbox_payload", value, secrets)
  |> log.truncate(max_excerpt_chars)
}

fn safe_excerpt(value: String, secrets: List(String)) -> String {
  log.redact("ledger_excerpt", value, secrets)
  |> log.truncate(max_excerpt_chars)
}
