import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/orchestrator/core
import scherzo/retry_step_validation
import scherzo/runtime/identity
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

pub type IssuePreflight {
  IssuePreflight(
    issue: tracker_issue.Issue,
    released_park: Option(orchestrator_state.ParkedEntry),
  )
}

pub fn issue_preflight(
  runtime: orchestrator_state.RuntimeState,
  projection_state: projection.Projection,
  effective: config_types.EffectiveConfig,
  operator_command: command.OperatorCommand,
  target: command.RetryWorkflowStepTarget,
  run_id: String,
  issue_id: String,
  fetch_issue_by_id: fn(String) ->
    Result(tracker_issue.Issue, command.CommandStatus),
  issue_is_active_or_pending: fn(Option(orchestrator_state.ParkedEntry)) -> Bool,
) -> Result(IssuePreflight, command.CommandResult) {
  use released_park <- result.try(released_park_result(
    runtime,
    projection_state,
    operator_command,
    run_id,
    issue_id,
  ))
  case issue_is_active_or_pending(released_park) {
    True ->
      Error(command.rejected(
        operator_command,
        "issue_already_active",
        Some("issue already has an active or pending workflow"),
      ))
    False ->
      case fetch_issue_by_id(issue_id) {
        Error(status) ->
          Error(command.result_for(operator_command, status, None))
        Ok(issue) ->
          validate_issue_state(
            effective,
            operator_command,
            target,
            issue,
            released_park,
          )
      }
  }
}

fn released_park_result(
  runtime: orchestrator_state.RuntimeState,
  projection_state: projection.Projection,
  operator_command: command.OperatorCommand,
  run_id: String,
  issue_id: String,
) -> Result(Option(orchestrator_state.ParkedEntry), command.CommandResult) {
  case
    dict.get(
      runtime.parked,
      orchestrator_state.linear_issue_id_identity(issue_id),
    )
  {
    Ok(parked) ->
      case
        core.retry_intent_releases_park(parked)
        || retry_step_validation.parked_issue_can_retry_step(
          projection_state,
          run_id,
          issue_id,
        )
      {
        True -> Ok(Some(parked))
        False ->
          Error(command.rejected(
            operator_command,
            "issue_parked",
            Some(parked_rejection_message(parked)),
          ))
      }
    Error(Nil) -> Ok(None)
  }
}

fn validate_issue_state(
  effective: config_types.EffectiveConfig,
  operator_command: command.OperatorCommand,
  target: command.RetryWorkflowStepTarget,
  issue: tracker_issue.Issue,
  released_park: Option(orchestrator_state.ParkedEntry),
) -> Result(IssuePreflight, command.CommandResult) {
  case core.is_terminal(effective, issue.state) {
    True ->
      Error(command.rejected(
        operator_command,
        "issue_state_drift:terminal_state",
        Some(
          "run "
          <> command.retry_workflow_step_target_to_string(target)
          <> " for issue "
          <> issue.identifier
          <> " is currently in terminal state "
          <> issue_state.to_string(issue.state)
          <> "; no run, park, or tracker state was changed. Next safe command: scripts/scherzoctl task show "
          <> issue.identifier
          <> " --json",
        ),
      ))
    False -> Ok(IssuePreflight(issue, released_park))
  }
}

pub fn parked_rejection_message(
  parked: orchestrator_state.ParkedEntry,
) -> String {
  "issue is parked for "
  <> orchestrator_reason.park_to_string(parked.reason)
  <> "; no run, park, or tracker state was changed. Next safe command: "
  <> scripts_command(core.parked_unpark_command(parked))
  <> " --json"
}

pub fn clear_released_park(
  runtime: orchestrator_state.RuntimeState,
  released_park: Option(orchestrator_state.ParkedEntry),
) -> orchestrator_state.RuntimeState {
  case released_park {
    None -> runtime
    Some(parked) -> {
      let identity = orchestrator_state.task_ref_identity(parked.task_ref)
      orchestrator_state.RuntimeState(
        ..orchestrator_state.clear_task_lifecycle(runtime, identity),
        issue_counters: dict.delete(runtime.issue_counters, identity),
      )
    }
  }
}

pub fn unpark_bodies(
  released_park: Option(orchestrator_state.ParkedEntry),
  now_ms: Int,
) -> List(record.RecordBody) {
  case released_park {
    None -> []
    Some(parked) -> [
      record.IssueUnparked(parked.issue_id, parked.identifier, "retry_step"),
      record.IssueCounterUpdated(
        parked.issue_id,
        parked.identifier,
        0,
        0,
        now_ms,
        None,
      ),
    ]
  }
}

pub fn queue_released_park(
  released_park: Option(orchestrator_state.ParkedEntry),
) -> Option(orchestrator_state.ParkedEntry) {
  case released_park {
    Some(parked) ->
      case core.retry_intent_releases_park(parked) {
        True -> Some(parked)
        False -> None
      }
    None -> None
  }
}

pub fn issue_is_active_or_pending(
  runtime: orchestrator_state.RuntimeState,
  tracker_kind: String,
  issue_id: String,
  released_park: Option(orchestrator_state.ParkedEntry),
  has_active_run: Bool,
  pending_claims: dict.Dict(identity.TaskIdentity, a),
  pending_dispatch_validations: dict.Dict(identity.TaskIdentity, b),
  pending_review_lane_preflights: dict.Dict(identity.TaskIdentity, c),
) -> Bool {
  let identity =
    orchestrator_state.issue_id_identity_for_backend(issue_id, tracker_kind)
  let parked = case released_park {
    Some(_) -> False
    None -> dict.has_key(runtime.parked, identity)
  }
  has_active_run
  || dict.has_key(pending_claims, identity)
  || dict.has_key(pending_dispatch_validations, identity)
  || dict.has_key(pending_review_lane_preflights, identity)
  || dict.has_key(runtime.claimed, identity)
  || dict.has_key(runtime.retry_attempts, identity)
  || parked
}

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
              "issue is parked for "
              <> reason
              <> "; no run, park, or tracker state was changed. Next safe command: "
              <> scripts_command(core.parked_unpark_command(parked))
              <> " --json",
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

fn scripts_command(command_text: String) -> String {
  case string.starts_with(command_text, "scripts/") {
    True -> command_text
    False -> "scripts/" <> command_text
  }
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
