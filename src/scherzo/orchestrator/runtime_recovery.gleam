import gleam/list
import scherzo/runtime/reason
import scherzo/runtime/state as orchestrator_state
import scherzo/state/record

pub fn runtime_with_appended_park_records(
  runtime: orchestrator_state.RuntimeState,
  records: List(record.LedgerRecord),
) -> orchestrator_state.RuntimeState {
  list.fold(records, runtime, fn(runtime, ledger_record) {
    case ledger_record {
      record.LedgerRecord(
        at_ms: parked_at_ms,
        body: record.IssueParked(issue_id, issue_identifier, reason_text, _),
        ..,
      ) ->
        runtime_with_parked_issue(
          runtime,
          issue_id,
          issue_identifier,
          reason_text,
          "explicit_unpark_only",
          "",
          parked_at_ms,
        )
      record.LedgerRecord(
        at_ms: parked_at_ms,
        body: record.IssueParkedV2(
          issue_id,
          issue_identifier,
          reason_text,
          release_policy,
          issue_fingerprint,
          _,
        ),
        ..,
      ) ->
        runtime_with_parked_issue(
          runtime,
          issue_id,
          issue_identifier,
          reason_text,
          release_policy,
          issue_fingerprint,
          parked_at_ms,
        )
      _ -> runtime
    }
  })
}

fn runtime_with_parked_issue(
  runtime: orchestrator_state.RuntimeState,
  issue_id: String,
  issue_identifier: String,
  reason_text: String,
  release_policy: String,
  issue_fingerprint: String,
  parked_at_ms: Int,
) -> orchestrator_state.RuntimeState {
  case issue_id == "" {
    True -> runtime
    False ->
      runtime_with_non_empty_parked_issue(
        runtime,
        issue_id,
        issue_identifier,
        reason_text,
        release_policy,
        issue_fingerprint,
        parked_at_ms,
      )
  }
}

fn runtime_with_non_empty_parked_issue(
  runtime: orchestrator_state.RuntimeState,
  issue_id: String,
  issue_identifier: String,
  reason_text: String,
  release_policy: String,
  issue_fingerprint: String,
  parked_at_ms: Int,
) -> orchestrator_state.RuntimeState {
  let task_ref = orchestrator_state.linear_issue_id_ref(issue_id)
  let identity = orchestrator_state.task_ref_identity(task_ref)
  let parked_entry =
    orchestrator_state.ParkedEntry(
      task_ref: task_ref,
      issue_id: issue_id,
      identifier: issue_identifier,
      reason: park_reason_from_string(reason_text),
      release_policy: orchestrator_state.park_release_policy_from_string(
        release_policy,
        issue_fingerprint,
      ),
      parked_at_ms: parked_at_ms,
    )
  orchestrator_state.mark_task_parked(runtime, identity, parked_entry)
}

fn park_reason_from_string(text: String) -> reason.ParkReason {
  case text {
    "worker_failure" -> reason.ParkWorkerFailure
    "max_retry_attempts" -> reason.ParkMaxRetryAttempts
    "max_sessions_per_issue" -> reason.ParkMaxSessionsPerIssue
    other -> reason.ParkOperator(other)
  }
}
