import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/claim_abandonment
import scherzo/config/types as config_types
import scherzo/error
import scherzo/handoff_format
import scherzo/runtime/state as orchestrator_state
import scherzo/scheduled_failure_reporter
import scherzo/state/outbox
import scherzo/state/record
import scherzo/state/recovery
import scherzo/task
import scherzo/tracker/adapter
import scherzo/tracker/idempotency
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_completion_policy
import scherzo/workflow_policy

pub const report_success_kind = "report_success"

pub const report_failure_kind = "report_failure"

pub const park_kind = "park"

pub const invalid_workflow_kind = "invalid_workflow"

pub type Intent {
  Intent(
    outbox_id: String,
    task_ref: record.TaskRefFields,
    outbox_kind: String,
    dedupe_key: String,
    payload_json: String,
  )
}

pub fn task_ref_fields(ref: task.TaskRef) -> record.TaskRefFields {
  record.TaskRefFields(
    task_backend_kind: ref.backend_kind,
    task_remote_id: ref.remote_id,
    task_key: ref.key,
    task_url: ref.url,
  )
}

pub fn task_ref_from_fields(ref: record.TaskRefFields) -> task.TaskRef {
  task.TaskRef(
    backend_kind: ref.task_backend_kind,
    remote_id: ref.task_remote_id,
    key: ref.task_key,
    url: ref.task_url,
  )
}

pub fn claim_intent(
  task_ref: task.TaskRef,
  issue: tracker_issue.Issue,
  run_id: String,
  handoff: config_types.HandoffConfig,
  secrets: List(String),
) -> Intent {
  let key =
    claim_abandonment.claim_key(
      task_ref.backend_kind,
      task_ref.remote_id,
      run_id,
    )
  let #(target_state_id, target_state_name) =
    optional_state_ref(handoff.claim_state_id)
  let body =
    handoff_format.claim_comment(issue.identifier, run_id, secrets)
    |> idempotency.append_marker(key)
  intent(
    task_ref,
    claim_abandonment.claim_kind,
    key,
    body,
    target_state_id,
    target_state_name,
    secrets,
  )
}

pub fn success_intent(
  task_ref: task.TaskRef,
  issue: tracker_issue.Issue,
  success: agent_types.WorkerSuccess,
  run_id: String,
  workflow_id: String,
  handoff: config_types.HandoffConfig,
  secrets: List(String),
) -> Intent {
  let key =
    report_key(
      report_success_kind,
      task_ref.backend_kind,
      task_ref.remote_id,
      run_id,
      workflow_id,
    )
  let decision = success_completion_decision(handoff, workflow_id, success)
  let tracking_state =
    option_map(decision, workflow_completion_policy.decision_reason)
  let #(target_state_id, target_state_name) =
    completion_state_target(decision, handoff.success_state_id)
  let options =
    handoff_format.SuccessCommentOptions(
      include_result: handoff.include_result_on_success,
      attachment_filename: None,
    )
  let body =
    handoff_format.success_comment_with_tracking(
      issue,
      success,
      run_id,
      options,
      tracking_state,
      secrets,
    )
    |> idempotency.append_marker(key)
  intent(
    task_ref,
    report_success_kind,
    key,
    body,
    target_state_id,
    target_state_name,
    secrets,
  )
}

pub fn failure_intent(
  task_ref: task.TaskRef,
  issue: tracker_issue.Issue,
  failure: agent_types.WorkerFailure,
  run_id: String,
  workflow_id: String,
  handoff: config_types.HandoffConfig,
  secrets: List(String),
) -> Intent {
  let key =
    report_key(
      report_failure_kind,
      task_ref.backend_kind,
      task_ref.remote_id,
      run_id,
      workflow_id,
    )
  let decision = failure_completion_decision(handoff, workflow_id, failure)
  let tracking_state =
    option_map(decision, workflow_completion_policy.decision_reason)
  let #(target_state_id, target_state_name) =
    completion_state_target(decision, handoff.failure_state_id)
  let body =
    handoff_format.failure_comment_with_tracking(
      issue,
      failure,
      run_id,
      tracking_state,
      secrets,
    )
    |> idempotency.append_marker(key)
  intent(
    task_ref,
    report_failure_kind,
    key,
    body,
    target_state_id,
    target_state_name,
    secrets,
  )
}

pub fn park_report_intent(
  report: adapter.ParkReport,
  secrets: List(String),
) -> Intent {
  let key =
    park_key(
      report.task.backend_kind,
      report.task.remote_id,
      report.run_id,
      report.reason,
    )
  let body =
    handoff_format.park_comment(
      report.issue_identifier,
      report.reason,
      report.release_policy,
      report.run_id,
      secrets,
    )
    |> idempotency.append_marker(key)
  intent(report.task, park_kind, key, body, None, None, secrets)
}

pub fn parked_entry_intent(
  parked: orchestrator_state.ParkedEntry,
  reason_text: String,
  source_run_id: Option(String),
  secrets: List(String),
) -> Intent {
  park_report_intent(
    adapter.ParkReport(
      task: parked.task_ref,
      issue_identifier: parked.identifier,
      reason: reason_text,
      release_policy: Some(park_release_policy_to_string(parked.release_policy)),
      run_id: source_run_id,
    ),
    secrets,
  )
}

pub fn release_claim_intent(
  report: adapter.ParkReport,
  run_or_reason: String,
  secrets: List(String),
) -> Intent {
  let key =
    claim_abandonment.release_key(
      report.task.backend_kind,
      report.task.remote_id,
      run_or_reason,
    )
  let body =
    claim_abandonment.release_comment_body(
      report.issue_identifier,
      report.reason,
      report.run_id,
      key,
      secrets,
    )
  intent(
    report.task,
    claim_abandonment.release_claim_kind,
    key,
    body,
    None,
    None,
    secrets,
  )
}

pub fn invalid_workflow_intent(
  issue: tracker_issue.Issue,
  violation: workflow_policy.IssueWorkflowViolation,
  violation_fingerprint: String,
  reporting_policy_fingerprint: String,
  contract: config_types.LinearContractConfig,
  secrets: List(String),
) -> Intent {
  let task_ref = task.from_legacy_issue(issue).ref
  let key =
    invalid_workflow_key(
      task_ref.backend_kind,
      task_ref.remote_id,
      violation_fingerprint,
      reporting_policy_fingerprint,
    )
  let body =
    workflow_policy.violation_comment(issue.identifier, violation, contract)
    |> idempotency.append_marker(key)
  let #(target_state_id, target_state_name) = invalid_workflow_target(contract)
  intent(
    task_ref,
    invalid_workflow_kind,
    key,
    body,
    target_state_id,
    target_state_name,
    secrets,
  )
}

pub fn scheduled_failure_dedupe_key(job_id: String) -> String {
  scheduled_failure_reporter.dedupe_key(job_id)
}

pub fn scheduled_failure_intent(
  publication: adapter.ScheduledFailurePublication,
  report_attempt_index: Int,
  secrets: List(String),
) -> Intent {
  let key = scheduled_failure_dedupe_key(publication.job_id)
  Intent(
    outbox_id: key,
    task_ref: scheduled_failure_task_ref(publication.job_id),
    outbox_kind: outbox.scheduled_failure_publication_kind,
    dedupe_key: key,
    payload_json: outbox.scheduled_failure_payload(
      outbox.ScheduledFailurePayload(
        kind: outbox.scheduled_failure_publication_kind,
        job_id: publication.job_id,
        workflow_id: publication.workflow_id,
        due_at_ms: publication.due_at_ms,
        run_id: publication.run_id,
        attempt: publication.attempt,
        max_attempts: publication.max_attempts,
        reason: publication.reason,
        run_root: publication.run_root,
        session_id: publication.session_id,
        dedupe_key: key,
        title: publication.title,
        body: publication.body,
        labels: publication.labels,
        target_state_name: publication.target_state_name,
        previous_task_remote_id: publication.previous_task_remote_id,
        report_attempt_index: report_attempt_index,
      ),
      secrets,
    ),
  )
}

pub fn replay_attempt_count(intent: Intent) -> Result(Int, outbox.ReplayError) {
  case intent.outbox_kind == outbox.scheduled_failure_publication_kind {
    True ->
      scheduled_failure_publication_from_payload(intent.payload_json)
      |> result.map(fn(decoded) {
        let #(attempt_count, _) = decoded
        attempt_count
      })
    False -> Ok(1)
  }
}

pub fn replay_failed_body(
  intent: Intent,
  error: outbox.ReplayError,
) -> record.RecordBody {
  record.OutboxFailedWithTask(
    intent.outbox_id,
    intent.task_ref,
    intent.outbox_kind,
    outbox.replay_error_code(error),
  )
}

pub fn replay_error_code(error: outbox.ReplayError) -> String {
  outbox.replay_error_code(error)
}

pub fn scheduled_failure_publication_from_payload(
  payload_json: String,
) -> Result(#(Int, adapter.ScheduledFailurePublication), outbox.ReplayError) {
  use payload <- result.try(outbox.decode_scheduled_failure_payload(
    payload_json,
  ))
  use Nil <- result.try(outbox.recovery_replay_error(
    outbox.scheduled_failure_publication_kind,
    payload.kind,
  ))
  Ok(#(
    payload.report_attempt_index,
    adapter.ScheduledFailurePublication(
      job_id: payload.job_id,
      workflow_id: payload.workflow_id,
      due_at_ms: payload.due_at_ms,
      run_id: payload.run_id,
      attempt: payload.attempt,
      max_attempts: payload.max_attempts,
      reason: payload.reason,
      run_root: payload.run_root,
      session_id: payload.session_id,
      dedupe_key: payload.dedupe_key,
      title: payload.title,
      body: payload.body,
      labels: payload.labels,
      target_state_name: payload.target_state_name,
      previous_task_remote_id: payload.previous_task_remote_id,
    ),
  ))
}

pub fn pending_body(intent: Intent) -> record.RecordBody {
  record.OutboxPendingV2WithTask(
    intent.outbox_id,
    intent.task_ref,
    intent.outbox_kind,
    intent.dedupe_key,
    intent.payload_json,
  )
}

pub fn attempted_body(intent: Intent, attempt_count: Int) -> record.RecordBody {
  record.OutboxAttemptedWithTask(
    intent.outbox_id,
    intent.task_ref,
    intent.outbox_kind,
    intent.dedupe_key,
    intent.payload_json,
    attempt_count,
  )
}

pub fn completed_body(intent: Intent) -> record.RecordBody {
  record.OutboxCompletedWithTask(
    intent.outbox_id,
    intent.task_ref,
    intent.outbox_kind,
  )
}

pub fn retry_scheduled_body(
  intent: Intent,
  error_code: String,
  attempt_count: Int,
  next_attempt_at_ms: Int,
) -> record.RecordBody {
  record.OutboxRetryScheduledWithTask(
    intent.outbox_id,
    intent.task_ref,
    intent.outbox_kind,
    intent.dedupe_key,
    intent.payload_json,
    error_code,
    attempt_count,
    next_attempt_at_ms,
  )
}

pub fn permanently_failed_body(
  intent: Intent,
  error_code: String,
  attempt_count: Int,
) -> record.RecordBody {
  record.OutboxPermanentlyFailedWithTask(
    intent.outbox_id,
    intent.task_ref,
    intent.outbox_kind,
    error_code,
    attempt_count,
  )
}

pub fn recovered_intent(outbox_replay: recovery.OutboxReplay) -> Intent {
  let recovery.OutboxReplay(
    outbox_id,
    task_ref,
    outbox_kind,
    dedupe_key,
    payload_json,
  ) = outbox_replay
  Intent(outbox_id, task_ref, outbox_kind, dedupe_key, payload_json)
}

fn intent(
  task_ref: task.TaskRef,
  kind: String,
  key: String,
  body: String,
  target_state_id: Option(String),
  target_state_name: Option(String),
  secrets: List(String),
) -> Intent {
  Intent(
    outbox_id: key,
    task_ref: task_ref_fields(task_ref),
    outbox_kind: kind,
    dedupe_key: key,
    payload_json: outbox.tracker_update_payload(
      kind,
      key,
      body,
      target_state_id,
      target_state_name,
      secrets,
    ),
  )
}

fn report_key(
  kind: String,
  backend_kind: String,
  task_remote_id: String,
  run_id: String,
  workflow_id: String,
) -> String {
  kind
  <> ":"
  <> backend_kind
  <> ":"
  <> task_remote_id
  <> ":"
  <> run_id
  <> ":"
  <> workflow_id
}

fn park_key(
  backend_kind: String,
  task_remote_id: String,
  run_id: Option(String),
  reason: String,
) -> String {
  let source = case run_id {
    Some(value) -> value
    None -> reason |> string.replace(":", "_")
  }
  "park:" <> backend_kind <> ":" <> task_remote_id <> ":" <> source
}

fn scheduled_failure_task_ref(job_id: String) -> record.TaskRefFields {
  record.TaskRefFields(
    task_backend_kind: "scheduled_failure",
    task_remote_id: job_id,
    task_key: Some(job_id),
    task_url: None,
  )
}

fn invalid_workflow_key(
  backend_kind: String,
  task_remote_id: String,
  violation_fingerprint: String,
  reporting_policy_fingerprint: String,
) -> String {
  "invalid_workflow:"
  <> backend_kind
  <> ":"
  <> task_remote_id
  <> ":"
  <> violation_fingerprint
  <> ":"
  <> reporting_policy_fingerprint
}

fn success_completion_decision(
  handoff: config_types.HandoffConfig,
  workflow_id: String,
  success: agent_types.WorkerSuccess,
) -> Option(workflow_completion_policy.CompletionStateDecision) {
  case handoff.completion_states {
    None -> None
    Some(policy) ->
      Some(workflow_completion_policy.choose_linear_completion_state(
        policy,
        workflow_id,
        workflow_completion_policy.success_outcome(
          handoff.completion_states,
          workflow_id,
          success,
        ),
      ))
  }
}

fn failure_completion_decision(
  handoff: config_types.HandoffConfig,
  workflow_id: String,
  failure: agent_types.WorkerFailure,
) -> Option(workflow_completion_policy.CompletionStateDecision) {
  case handoff.completion_states {
    None -> None
    Some(policy) ->
      Some(workflow_completion_policy.choose_linear_completion_state(
        policy,
        workflow_id,
        failure_completion_outcome(failure),
      ))
  }
}

fn failure_completion_outcome(
  failure: agent_types.WorkerFailure,
) -> workflow_completion_policy.WorkflowCompletionOutcome {
  case failure.reason {
    error.OperatorAbort -> workflow_completion_policy.cancellation_outcome()
    error.OperatorStopAfterCurrentTurn ->
      workflow_completion_policy.cancellation_outcome()
    _ -> workflow_completion_policy.failure_outcome()
  }
}

fn completion_state_target(
  decision: Option(workflow_completion_policy.CompletionStateDecision),
  fallback: Option(workflow_completion_policy.LinearStateRef),
) -> #(Option(String), Option(String)) {
  case decision {
    Some(workflow_completion_policy.LeaveLinearState(_)) -> #(None, None)
    Some(workflow_completion_policy.MoveToState(state, _)) ->
      optional_state_ref(Some(state))
    None -> optional_state_ref(fallback)
  }
}

fn option_map(value: Option(a), mapper: fn(a) -> b) -> Option(b) {
  case value {
    Some(value) -> Some(mapper(value))
    None -> None
  }
}

fn optional_state_ref(
  value: Option(workflow_completion_policy.LinearStateRef),
) -> #(Option(String), Option(String)) {
  case value {
    Some(workflow_completion_policy.StateById(id)) -> #(Some(id), None)
    Some(workflow_completion_policy.StateByName(name)) -> #(None, Some(name))
    None -> #(None, None)
  }
}

fn invalid_workflow_target(
  contract: config_types.LinearContractConfig,
) -> #(Option(String), Option(String)) {
  case config_types.normalized_invalid_workflow_state_target(contract) {
    Some(config_types.InvalidWorkflowStateId(value)) -> #(Some(value), None)
    Some(config_types.InvalidWorkflowStateName(value)) -> #(None, Some(value))
    None -> #(None, None)
  }
}

fn park_release_policy_to_string(
  release_policy: orchestrator_state.ParkReleasePolicy,
) -> String {
  case release_policy {
    orchestrator_state.ExplicitUnparkOnly -> "explicit_unpark_only"
    orchestrator_state.AutoUnparkOnIssueChange(_) ->
      "auto_unpark_on_issue_change"
  }
}
