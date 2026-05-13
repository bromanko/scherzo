import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/log

pub const schema_version = 2

pub const max_excerpt_chars = 500

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
  WorkflowRunFinished(
    run_id: String,
    workflow_id: String,
    issue_id: String,
    outcome: String,
    token_total: Int,
    turns: Int,
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
  OutboxCompleted(outbox_id: String, issue_id: String, outbox_kind: String)
  OutboxFailed(
    outbox_id: String,
    issue_id: String,
    outbox_kind: String,
    error_code: String,
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
    comment_id: Option(String),
    author_id: Option(String),
    command_name: Option(String),
    excerpt: Option(String),
    status: Option(String),
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
    WorkflowRunFinished(..) -> "workflow_run_finished"
    WorkflowRunInterrupted(..) -> "workflow_run_interrupted"
    WorkflowRunSuperseded(..) -> "workflow_run_superseded"
    StepAttemptPrepared(..) -> "step_attempt_prepared"
    StepAttemptStarted(..) -> "step_attempt_started"
    StepAttemptContinuationStarted(..) -> "step_attempt_continuation_started"
    StepAttemptPiSessionRecorded(..) -> "step_attempt_pi_session_recorded"
    StepAttemptFinished(..) -> "step_attempt_finished"
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
    OutboxPendingV2(..) -> "outbox_pending_v2"
    OutboxCompleted(..) -> "outbox_completed"
    OutboxFailed(..) -> "outbox_failed"
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
    RunStarted(run_id, issue_id, issue_identifier, workspace_path) -> [
      #("run_id", json.string(run_id)),
      #("issue_id", json.string(issue_id)),
      #("issue_identifier", json.string(issue_identifier)),
      #("workspace_path", json.string(workspace_path)),
    ]
    RunFinished(run_id, issue_id, classification, token_total, turns) -> [
      #("run_id", json.string(run_id)),
      #("issue_id", json.string(issue_id)),
      #("classification", json.string(classification)),
      #("token_total", json.int(token_total)),
      #("turns", json.int(turns)),
    ]
    RunInterrupted(run_id, issue_id, reason) -> [
      #("run_id", json.string(run_id)),
      #("issue_id", json.string(issue_id)),
      #("reason", json.string(reason)),
    ]
    WorkflowRunStarted(
      run_id,
      workflow_id,
      workflow_fingerprint,
      issue_id,
      issue_identifier,
      issue_fingerprint,
      observed_updated_at_ms,
      run_root,
    ) -> [
      #("run_id", json.string(run_id)),
      #("workflow_id", json.string(workflow_id)),
      #("workflow_fingerprint", json.string(workflow_fingerprint)),
      #("issue_id", json.string(issue_id)),
      #("issue_identifier", json.string(issue_identifier)),
      #("issue_fingerprint", json.string(issue_fingerprint)),
      #("observed_updated_at_ms", json.int(observed_updated_at_ms)),
      #("run_root", json.string(run_root)),
    ]
    WorkflowRunFinished(
      run_id,
      workflow_id,
      issue_id,
      outcome,
      token_total,
      turns,
    ) -> [
      #("run_id", json.string(run_id)),
      #("workflow_id", json.string(workflow_id)),
      #("issue_id", json.string(issue_id)),
      #("outcome", json.string(outcome)),
      #("token_total", json.int(token_total)),
      #("turns", json.int(turns)),
    ]
    WorkflowRunInterrupted(run_id, workflow_id, issue_id, reason) -> [
      #("run_id", json.string(run_id)),
      #("workflow_id", json.string(workflow_id)),
      #("issue_id", json.string(issue_id)),
      #("reason", json.string(reason)),
    ]
    WorkflowRunSuperseded(
      run_id,
      workflow_id,
      issue_id,
      superseded_by_run_id,
      reason,
    ) -> [
      #("run_id", json.string(run_id)),
      #("workflow_id", json.string(workflow_id)),
      #("issue_id", json.string(issue_id)),
      #("superseded_by_run_id", json.string(superseded_by_run_id)),
      #("reason", json.string(reason)),
    ]
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
    ) -> [
      #("run_id", json.string(run_id)),
      #("workflow_id", json.string(workflow_id)),
      #("step_id", json.string(step_id)),
      #("attempt_index", json.int(attempt_index)),
      #("workspace_name", json.string(workspace_name)),
      #("workspace_path", json.string(workspace_path)),
      #("run_root", json.string(run_root)),
      #("source_workspace_name", option_string_to_json(source_workspace_name)),
      #("source_workspace_path", option_string_to_json(source_workspace_path)),
    ]
    StepAttemptStarted(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      operator_session_id,
      external_session_ref,
      continuation_capable,
    ) -> [
      #("run_id", json.string(run_id)),
      #("workflow_id", json.string(workflow_id)),
      #("step_id", json.string(step_id)),
      #("attempt_index", json.int(attempt_index)),
      #("operator_session_id", json.string(operator_session_id)),
      #("external_session_ref", option_string_to_json(external_session_ref)),
      #("continuation_capable", json.bool(continuation_capable)),
    ]
    StepAttemptContinuationStarted(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      session_id,
    ) -> [
      #("run_id", json.string(run_id)),
      #("workflow_id", json.string(workflow_id)),
      #("step_id", json.string(step_id)),
      #("attempt_index", json.int(attempt_index)),
      #("session_id", json.string(session_id)),
    ]
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
    ) -> [
      #("run_id", json.string(run_id)),
      #("issue_id", json.string(issue_id)),
      #("issue_identifier", json.string(issue_identifier)),
      #("workflow_id", json.string(workflow_id)),
      #("workflow_fingerprint", json.string(workflow_fingerprint)),
      #("step_id", json.string(step_id)),
      #("workspace_name", json.string(workspace_name)),
      #("attempt_index", json.int(attempt_index)),
      #("workspace_path", json.string(workspace_path)),
      #("session_id", json.string(session_id)),
      #("session_file", json.string(session_file)),
    ]
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
    ) -> [
      #("run_id", json.string(run_id)),
      #("workflow_id", json.string(workflow_id)),
      #("step_id", json.string(step_id)),
      #("attempt_index", json.int(attempt_index)),
      #("outcome", json.string(outcome)),
      #("artifact_ref", json.string(artifact_ref)),
      #("artifact_sha256", json.string(artifact_sha256)),
      #("workspace_name", json.string(workspace_name)),
      #("workspace_path", json.string(workspace_path)),
      #("token_total", json.int(token_total)),
      #("turns", json.int(turns)),
    ]
    StepAttemptInterrupted(run_id, workflow_id, step_id, attempt_index, reason) -> [
      #("run_id", json.string(run_id)),
      #("workflow_id", json.string(workflow_id)),
      #("step_id", json.string(step_id)),
      #("attempt_index", json.int(attempt_index)),
      #("reason", json.string(reason)),
    ]
    StepAttemptSuperseded(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      superseded_by_attempt_index,
      reason,
    ) -> [
      #("run_id", json.string(run_id)),
      #("workflow_id", json.string(workflow_id)),
      #("step_id", json.string(step_id)),
      #("attempt_index", json.int(attempt_index)),
      #("superseded_by_attempt_index", json.int(superseded_by_attempt_index)),
      #("reason", json.string(reason)),
    ]
    RetryScheduled(issue_id, issue_identifier, delay_ms, generation, reason) -> [
      #("issue_id", json.string(issue_id)),
      #("issue_identifier", json.string(issue_identifier)),
      #("delay_ms", json.int(delay_ms)),
      #("generation", json.int(generation)),
      #("reason", json.string(reason)),
    ]
    RetryCancelled(issue_id, generation, reason) -> [
      #("issue_id", json.string(issue_id)),
      #("generation", json.int(generation)),
      #("reason", json.string(reason)),
    ]
    IssueCounterUpdated(
      issue_id,
      issue_identifier,
      failure_attempts,
      worker_sessions,
      observed_updated_at_ms,
      source_run_id,
    ) -> [
      #("issue_id", json.string(issue_id)),
      #("issue_identifier", json.string(issue_identifier)),
      #("failure_attempts", json.int(failure_attempts)),
      #("worker_sessions", json.int(worker_sessions)),
      #("observed_updated_at_ms", json.int(observed_updated_at_ms)),
      #("source_run_id", option_string_to_json(source_run_id)),
    ]
    KnownWorkspace(issue_id, issue_identifier, workspace_path) -> [
      #("issue_id", json.string(issue_id)),
      #("issue_identifier", json.string(issue_identifier)),
      #("workspace_path", json.string(workspace_path)),
    ]
    IssueParked(issue_id, issue_identifier, reason, observed_updated_at_ms) -> [
      #("issue_id", json.string(issue_id)),
      #("issue_identifier", json.string(issue_identifier)),
      #("reason", json.string(reason)),
      #("observed_updated_at_ms", json.int(observed_updated_at_ms)),
    ]
    IssueParkedV2(
      issue_id,
      issue_identifier,
      reason,
      release_policy,
      issue_fingerprint,
      observed_updated_at_ms,
    ) -> [
      #("issue_id", json.string(issue_id)),
      #("issue_identifier", json.string(issue_identifier)),
      #("reason", json.string(reason)),
      #("release_policy", json.string(release_policy)),
      #("issue_fingerprint", json.string(issue_fingerprint)),
      #("observed_updated_at_ms", json.int(observed_updated_at_ms)),
    ]
    IssueUnparked(issue_id, issue_identifier, reason) -> [
      #("issue_id", json.string(issue_id)),
      #("issue_identifier", json.string(issue_identifier)),
      #("reason", json.string(reason)),
    ]
    LinearCommandSeen(comment_id, issue_id, author_id, command_name, excerpt) -> [
      #("comment_id", json.string(comment_id)),
      #("issue_id", json.string(issue_id)),
      #("author_id", json.string(author_id)),
      #("command_name", json.string(command_name)),
      #("excerpt", json.string(excerpt)),
    ]
    LinearCommandStarted(comment_id, issue_id, command_name) -> [
      #("comment_id", json.string(comment_id)),
      #("issue_id", json.string(issue_id)),
      #("command_name", json.string(command_name)),
    ]
    LinearCommandCompleted(comment_id, issue_id, status, message_excerpt) -> [
      #("comment_id", json.string(comment_id)),
      #("issue_id", json.string(issue_id)),
      #("status", json.string(status)),
      #("message_excerpt", json.string(message_excerpt)),
    ]
    LinearCommandAcked(comment_id, issue_id) -> [
      #("comment_id", json.string(comment_id)),
      #("issue_id", json.string(issue_id)),
    ]
    ScheduledJobDue(job_id, workflow_id, due_at_ms, run_id, trigger) ->
      scheduled_base_entries(job_id, workflow_id, due_at_ms, run_id)
      |> append_json_entries([#("trigger", json.string(trigger))])
    ScheduledJobSkipped(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      reason,
      skipped_count,
    ) ->
      scheduled_base_entries(job_id, workflow_id, due_at_ms, run_id)
      |> append_json_entries([
        #("reason", json.string(reason)),
        #("skipped_count", json.int(skipped_count)),
      ])
    ScheduledRunPending(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      trigger,
      requested_at_ms,
    ) ->
      scheduled_base_entries(job_id, workflow_id, due_at_ms, run_id)
      |> append_json_entries([
        #("trigger", json.string(trigger)),
        #("requested_at_ms", json.int(requested_at_ms)),
      ])
    ScheduledRunPendingBlocked(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      reason,
      observed_at_ms,
    ) ->
      scheduled_base_entries(job_id, workflow_id, due_at_ms, run_id)
      |> append_json_entries([
        #("reason", json.string(reason)),
        #("observed_at_ms", json.int(observed_at_ms)),
      ])
    ScheduledRunPendingCancelled(
      job_id,
      workflow_id,
      due_at_ms,
      run_id,
      reason,
      cancelled_at_ms,
    ) ->
      scheduled_base_entries(job_id, workflow_id, due_at_ms, run_id)
      |> append_json_entries([
        #("reason", json.string(reason)),
        #("cancelled_at_ms", json.int(cancelled_at_ms)),
      ])
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
      scheduled_base_entries(job_id, workflow_id, due_at_ms, run_id)
      |> append_json_entries([
        #("started_at_ms", json.int(started_at_ms)),
        #("attempt", json.int(attempt)),
        #("session_id", json.string(session_id)),
        #("run_root", json.string(run_root)),
      ])
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
      scheduled_base_entries(job_id, workflow_id, due_at_ms, run_id)
      |> append_json_entries([
        #("attempt", json.int(attempt)),
        #("finished_at_ms", json.int(finished_at_ms)),
        #("token_total", json.int(token_total)),
        #("turns", json.int(turns)),
      ])
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
      scheduled_base_entries(job_id, workflow_id, due_at_ms, run_id)
      |> append_json_entries([
        #("attempt", json.int(attempt)),
        #("finished_at_ms", json.int(finished_at_ms)),
        #("reason", json.string(reason)),
        #("retry_exhausted", json.bool(retry_exhausted)),
        #("run_root", option_string_to_json(run_root)),
      ])
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
      scheduled_base_entries(job_id, workflow_id, due_at_ms, run_id)
      |> append_json_entries([
        #("next_attempt", json.int(next_attempt)),
        #("delay_ms", json.int(delay_ms)),
        #("generation", json.int(generation)),
        #("reason", json.string(reason)),
      ])
    ScheduledRunRetryCancelled(job_id, run_id, generation, reason) -> [
      #("job_id", json.string(job_id)),
      #("run_id", json.string(run_id)),
      #("generation", json.int(generation)),
      #("reason", json.string(reason)),
    ]
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
      scheduled_base_entries(job_id, workflow_id, due_at_ms, run_id)
      |> append_json_entries([
        #("attempt", json.int(attempt)),
        #("dedupe_key", json.string(dedupe_key)),
        #("linear_issue_id", json.string(linear_issue_id)),
        #("action", json.string(action)),
      ])
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
      scheduled_base_entries(job_id, workflow_id, due_at_ms, run_id)
      |> append_json_entries([
        #("attempt", json.int(attempt)),
        #("dedupe_key", json.string(dedupe_key)),
        #("error_code", json.string(error_code)),
        #("error_message", json.string(error_message)),
        #("next_retry_at_ms", json.int(next_retry_at_ms)),
        #("generation", json.int(generation)),
      ])
    OutboxPending(outbox_id, issue_id, outbox_kind, dedupe_key) -> [
      #("outbox_id", json.string(outbox_id)),
      #("issue_id", json.string(issue_id)),
      #("outbox_kind", json.string(outbox_kind)),
      #("dedupe_key", json.string(dedupe_key)),
    ]
    OutboxPendingV2(outbox_id, issue_id, outbox_kind, dedupe_key, payload_json) -> [
      #("outbox_id", json.string(outbox_id)),
      #("issue_id", json.string(issue_id)),
      #("outbox_kind", json.string(outbox_kind)),
      #("dedupe_key", json.string(dedupe_key)),
      #("payload_json", json.string(payload_json)),
    ]
    OutboxCompleted(outbox_id, issue_id, outbox_kind) -> [
      #("outbox_id", json.string(outbox_id)),
      #("issue_id", json.string(issue_id)),
      #("outbox_kind", json.string(outbox_kind)),
    ]
    OutboxFailed(outbox_id, issue_id, outbox_kind, error_code) -> [
      #("outbox_id", json.string(outbox_id)),
      #("issue_id", json.string(issue_id)),
      #("outbox_kind", json.string(outbox_kind)),
      #("error_code", json.string(error_code)),
    ]
  }
}

fn scheduled_base_entries(
  job_id: String,
  workflow_id: String,
  due_at_ms: Int,
  run_id: String,
) -> List(#(String, json.Json)) {
  [
    #("job_id", json.string(job_id)),
    #("workflow_id", json.string(workflow_id)),
    #("due_at_ms", json.int(due_at_ms)),
    #("run_id", json.string(run_id)),
  ]
}

fn append_json_entries(
  base: List(#(String, json.Json)),
  extra: List(#(String, json.Json)),
) -> List(#(String, json.Json)) {
  list_append(base, extra)
}

fn list_append(left: List(a), right: List(a)) -> List(a) {
  case left {
    [] -> right
    [first, ..rest] -> [first, ..list_append(rest, right)]
  }
}

fn option_string_to_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
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

fn body_from_fields(fields: RecordFields) -> Result(RecordBody, DecodeError) {
  case fields.kind {
    "run_started" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use issue_identifier <- result.try(required_string(
        fields.issue_identifier,
        "issue_identifier",
      ))
      use workspace_path <- result.try(required_string(
        fields.workspace_path,
        "workspace_path",
      ))
      Ok(RunStarted(run_id, issue_id, issue_identifier, workspace_path))
    }
    "run_finished" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use classification <- result.try(required_string(
        fields.classification,
        "classification",
      ))
      use token_total <- result.try(required_int(
        fields.token_total,
        "token_total",
      ))
      use turns <- result.try(required_int(fields.turns, "turns"))
      Ok(RunFinished(run_id, issue_id, classification, token_total, turns))
    }
    "run_interrupted" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use reason <- result.try(required_string(fields.reason, "reason"))
      Ok(RunInterrupted(run_id, issue_id, reason))
    }
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
      Ok(WorkflowRunFinished(
        run_id,
        workflow_id,
        issue_id,
        outcome,
        token_total,
        turns,
      ))
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
    "scheduled_job_due" -> {
      use base <- result.try(required_scheduled_base(fields))
      let #(job_id, workflow_id, due_at_ms, run_id) = base
      use trigger <- result.try(required_string(fields.trigger, "trigger"))
      Ok(ScheduledJobDue(job_id, workflow_id, due_at_ms, run_id, trigger))
    }
    "scheduled_job_skipped" -> {
      use base <- result.try(required_scheduled_base(fields))
      let #(job_id, workflow_id, due_at_ms, run_id) = base
      use reason <- result.try(required_string(fields.reason, "reason"))
      use skipped_count <- result.try(required_int(
        fields.skipped_count,
        "skipped_count",
      ))
      Ok(ScheduledJobSkipped(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        reason,
        skipped_count,
      ))
    }
    "scheduled_run_pending" -> {
      use base <- result.try(required_scheduled_base(fields))
      let #(job_id, workflow_id, due_at_ms, run_id) = base
      use trigger <- result.try(required_string(fields.trigger, "trigger"))
      use requested_at_ms <- result.try(required_int(
        fields.requested_at_ms,
        "requested_at_ms",
      ))
      Ok(ScheduledRunPending(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        trigger,
        requested_at_ms,
      ))
    }
    "scheduled_run_pending_blocked" -> {
      use base <- result.try(required_scheduled_base(fields))
      let #(job_id, workflow_id, due_at_ms, run_id) = base
      use reason <- result.try(required_string(fields.reason, "reason"))
      use observed_at_ms <- result.try(required_int(
        fields.observed_at_ms,
        "observed_at_ms",
      ))
      Ok(ScheduledRunPendingBlocked(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        reason,
        observed_at_ms,
      ))
    }
    "scheduled_run_pending_cancelled" -> {
      use base <- result.try(required_scheduled_base(fields))
      let #(job_id, workflow_id, due_at_ms, run_id) = base
      use reason <- result.try(required_string(fields.reason, "reason"))
      use cancelled_at_ms <- result.try(required_int(
        fields.cancelled_at_ms,
        "cancelled_at_ms",
      ))
      Ok(ScheduledRunPendingCancelled(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        reason,
        cancelled_at_ms,
      ))
    }
    "scheduled_run_started" -> {
      use base <- result.try(required_scheduled_base(fields))
      let #(job_id, workflow_id, due_at_ms, run_id) = base
      use started_at_ms <- result.try(required_int(
        fields.started_at_ms,
        "started_at_ms",
      ))
      use attempt <- result.try(required_int(fields.attempt, "attempt"))
      use session_id <- result.try(required_string(
        fields.session_id,
        "session_id",
      ))
      use run_root <- result.try(required_string(fields.run_root, "run_root"))
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
    }
    "scheduled_run_succeeded" -> {
      use base <- result.try(required_scheduled_base(fields))
      let #(job_id, workflow_id, due_at_ms, run_id) = base
      use attempt <- result.try(required_int(fields.attempt, "attempt"))
      use finished_at_ms <- result.try(required_int(
        fields.finished_at_ms,
        "finished_at_ms",
      ))
      use token_total <- result.try(required_int(
        fields.token_total,
        "token_total",
      ))
      use turns <- result.try(required_int(fields.turns, "turns"))
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
    }
    "scheduled_run_failed" -> {
      use base <- result.try(required_scheduled_base(fields))
      let #(job_id, workflow_id, due_at_ms, run_id) = base
      use attempt <- result.try(required_int(fields.attempt, "attempt"))
      use finished_at_ms <- result.try(required_int(
        fields.finished_at_ms,
        "finished_at_ms",
      ))
      use reason <- result.try(required_string(fields.reason, "reason"))
      use retry_exhausted <- result.try(required_bool(
        fields.retry_exhausted,
        "retry_exhausted",
      ))
      Ok(ScheduledRunFailed(
        job_id,
        workflow_id,
        due_at_ms,
        run_id,
        attempt,
        finished_at_ms,
        reason,
        retry_exhausted,
        fields.run_root,
      ))
    }
    "scheduled_run_retry_scheduled" -> {
      use base <- result.try(required_scheduled_base(fields))
      let #(job_id, workflow_id, due_at_ms, run_id) = base
      use next_attempt <- result.try(required_int(
        fields.next_attempt,
        "next_attempt",
      ))
      use delay_ms <- result.try(required_int(fields.delay_ms, "delay_ms"))
      use generation <- result.try(required_int(fields.generation, "generation"))
      use reason <- result.try(required_string(fields.reason, "reason"))
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
    }
    "scheduled_run_retry_cancelled" -> {
      use job_id <- result.try(required_string(fields.job_id, "job_id"))
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use generation <- result.try(required_int(fields.generation, "generation"))
      use reason <- result.try(required_string(fields.reason, "reason"))
      Ok(ScheduledRunRetryCancelled(job_id, run_id, generation, reason))
    }
    "scheduled_failure_reported" -> {
      use base <- result.try(required_scheduled_base(fields))
      let #(job_id, workflow_id, due_at_ms, run_id) = base
      use attempt <- result.try(required_int(fields.attempt, "attempt"))
      use dedupe_key <- result.try(required_string(
        fields.dedupe_key,
        "dedupe_key",
      ))
      use linear_issue_id <- result.try(required_string(
        fields.linear_issue_id,
        "linear_issue_id",
      ))
      use action <- result.try(required_string(fields.action, "action"))
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
    }
    "scheduled_failure_report_failed" -> {
      use base <- result.try(required_scheduled_base(fields))
      let #(job_id, workflow_id, due_at_ms, run_id) = base
      use attempt <- result.try(required_int(fields.attempt, "attempt"))
      use dedupe_key <- result.try(required_string(
        fields.dedupe_key,
        "dedupe_key",
      ))
      use error_code <- result.try(required_string(
        fields.error_code,
        "error_code",
      ))
      use error_message <- result.try(required_string(
        fields.error_message,
        "error_message",
      ))
      use next_retry_at_ms <- result.try(required_int(
        fields.next_retry_at_ms,
        "next_retry_at_ms",
      ))
      use generation <- result.try(required_int(fields.generation, "generation"))
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
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
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
      Ok(OutboxPendingV2(
        outbox_id,
        issue_id,
        outbox_kind,
        dedupe_key,
        payload_json,
      ))
    }
    "outbox_completed" -> {
      use outbox_id <- result.try(required_string(fields.outbox_id, "outbox_id"))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use outbox_kind <- result.try(required_string(
        fields.outbox_kind,
        "outbox_kind",
      ))
      Ok(OutboxCompleted(outbox_id, issue_id, outbox_kind))
    }
    "outbox_failed" -> {
      use outbox_id <- result.try(required_string(fields.outbox_id, "outbox_id"))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use outbox_kind <- result.try(required_string(
        fields.outbox_kind,
        "outbox_kind",
      ))
      use error_code <- result.try(required_string(
        fields.error_code,
        "error_code",
      ))
      Ok(OutboxFailed(outbox_id, issue_id, outbox_kind, error_code))
    }
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
    comment_id: comment_id,
    author_id: author_id,
    command_name: command_name,
    excerpt: excerpt,
    status: status,
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
  ))
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

fn required_scheduled_base(
  fields: RecordFields,
) -> Result(#(String, String, Int, String), DecodeError) {
  use job_id <- result.try(required_string(fields.job_id, "job_id"))
  use workflow_id <- result.try(required_string(
    fields.workflow_id,
    "workflow_id",
  ))
  use due_at_ms <- result.try(required_int(fields.due_at_ms, "due_at_ms"))
  use run_id <- result.try(required_string(fields.run_id, "run_id"))
  Ok(#(job_id, workflow_id, due_at_ms, run_id))
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
    OutboxPendingV2(outbox_id, issue_id, outbox_kind, dedupe_key, payload_json) ->
      OutboxPendingV2(
        outbox_id,
        issue_id,
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
