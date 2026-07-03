import gleam/list
import gleam/option.{type Option, Some}
import scherzo/retry_step_validation
import scherzo/state/record
import scherzo/state/recovery

pub fn validation_rejection_message(
  failure: retry_step_validation.Failure,
  run_id: String,
  step_id: Option(String),
) -> String {
  retry_step_validation.validation_rejection_message(failure, run_id, step_id)
}

pub fn failure_message(
  reason: String,
  detail: Option(String),
  run_id: String,
  step_id: Option(String),
) -> String {
  retry_step_validation.operation_failure_message(
    reason,
    detail,
    run_id,
    step_id,
  )
}

pub fn rejection_message(
  finalization: recovery.WorkflowFinalization,
  run_id: String,
  step_id: Option(String),
) -> Option(String) {
  let detail = case finalization.diagnostics {
    [diagnostic, ..] ->
      Some(recovery.workflow_recovery_diagnostic_message(diagnostic))
    [] -> Some("recovery validation rejected the retry-step repair")
  }
  Some(failure_message(rejection_reason(finalization), detail, run_id, step_id))
}

pub fn dispatch_rejection_message(
  finalization: recovery.WorkflowFinalization,
) -> Option(String) {
  case finalization.diagnostics {
    [diagnostic, ..] ->
      Some(
        "dispatch recovery was rejected by recovery validation: "
        <> recovery.workflow_recovery_diagnostic_message(diagnostic),
      )
    [] -> Some("dispatch recovery was rejected by recovery validation")
  }
}

pub fn diagnostic_bodies(
  finalization: recovery.WorkflowFinalization,
) -> List(record.RecordBody) {
  finalization.diagnostics
  |> list.map(recovery.workflow_recovery_diagnostic_record_body)
}

pub fn rejection_reason(finalization: recovery.WorkflowFinalization) -> String {
  case finalization.diagnostics {
    [diagnostic, ..] -> recovery.workflow_recovery_diagnostic_reason(diagnostic)
    [] ->
      case finalization.records_to_append {
        [
          record.LedgerRecord(
            body: record.IssueParkedV2(reason: reason, ..),
            ..,
          ),
          ..
        ] -> retry_step_validation.stable_rejection_reason(reason)
        [
          record.LedgerRecord(
            body: record.WorkflowRunInterrupted(reason: reason, ..),
            ..,
          ),
          ..
        ] -> retry_step_validation.stable_rejection_reason(reason)
        _ -> "artifact_recovery_failed"
      }
  }
}
