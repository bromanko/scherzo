import gleam/dict
import gleam/list
import gleam/option.{type Option, Some}
import scherzo/control/command
import scherzo/retry_step_validation
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
import scherzo/state/projection
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

pub fn parked_issue(
  runtime: orchestrator_state.RuntimeState,
  projection_state: projection.Projection,
  operator_command: command.OperatorCommand,
  run_id: String,
  issue_id: String,
) -> Result(Nil, command.CommandResult) {
  case
    dict.get(
      runtime.parked,
      orchestrator_state.linear_issue_id_identity(issue_id),
    )
  {
    Error(Nil) -> Ok(Nil)
    Ok(parked) -> {
      let reason = orchestrator_reason.park_to_string(parked.reason)
      case
        retry_step_validation.parked_issue_can_retry_step(
          projection_state,
          run_id,
          issue_id,
        )
      {
        True -> Ok(Nil)
        False ->
          Error(command.rejected(
            operator_command,
            "issue_parked",
            Some(
              "issue is parked for " <> reason <> "; unpark before retry-step",
            ),
          ))
      }
    }
  }
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
