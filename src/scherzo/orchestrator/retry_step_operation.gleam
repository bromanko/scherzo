import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/orchestrator/core
import scherzo/orchestrator/retry_step_resumption
import scherzo/retry_step_validation
import scherzo/runtime/identity
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
import scherzo/runtime_bundle
import scherzo/state/artifact_store
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_repair

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

pub fn retained_recovery_unavailable_message(
  detail: String,
  run_id: String,
  fresh_issue_identifier: Option(String),
) -> String {
  retry_step_validation.retained_recovery_unavailable_message(
    detail,
    run_id,
    fresh_issue_identifier,
  )
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
      recovery.workflow_recovery_diagnostic_message(diagnostic)
    [] -> "recovery validation rejected the retry-step repair"
  }
  case rejection_reason(finalization) {
    "retained_recovery_unavailable" ->
      Some(retained_recovery_unavailable_message(detail, run_id, None))
    reason -> Some(failure_message(reason, Some(detail), run_id, step_id))
  }
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
  let reason = case finalization.diagnostics {
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
  case retry_step_validation.physical_recovery_failure(reason) {
    True -> "retained_recovery_unavailable"
    False -> reason
  }
}

pub fn operational_preflight(
  bundle: runtime_bundle.RuntimeBundle,
  effective: config_types.EffectiveConfig,
  projection_state: projection.Projection,
  operator_command: command.OperatorCommand,
  run_id: String,
  issue_id: String,
  issue_identifier: String,
  step_id: Option(String),
  observation: recovery.CurrentWorkflowObservation,
  now_ms: Int,
) -> Result(Nil, command.CommandResult) {
  let target = command.RetryWorkflowStepRunId(run_id)
  case
    repair_plan_for_command(
      operator_command,
      projection_state,
      target,
      step_id,
      observation,
    )
  {
    Error(error) -> {
      let reason = workflow_repair.describe_error(error)
      case retry_step_validation.physical_recovery_failure(reason) {
        True ->
          Error(retained_recovery_rejection(
            operator_command,
            option.unwrap(workflow_repair.error_message(error), reason),
            run_id,
            issue_id,
            issue_identifier,
          ))
        False ->
          Error(command.rejected(
            operator_command,
            reason,
            Some(failure_message(
              reason,
              workflow_repair.error_message(error),
              run_id,
              step_id,
            )),
          ))
      }
    }
    Ok(plan) ->
      candidate_preflight(
        bundle,
        effective,
        projection_state,
        operator_command,
        plan,
        observation,
        issue_id,
        issue_identifier,
        step_id,
        now_ms,
      )
  }
}

fn repair_plan_for_command(
  operator_command: command.OperatorCommand,
  projection_state: projection.Projection,
  target: command.RetryWorkflowStepTarget,
  step_id: Option(String),
  observation: recovery.CurrentWorkflowObservation,
) -> Result(workflow_repair.RepairPlan, workflow_repair.RepairError) {
  case operator_command {
    command.RetryWorkflowStepExact(_, _) ->
      workflow_repair.plan_exact(projection_state, target, step_id, observation)
    _ -> workflow_repair.plan(projection_state, target, step_id, observation)
  }
}

fn candidate_preflight(
  bundle: runtime_bundle.RuntimeBundle,
  effective: config_types.EffectiveConfig,
  projection_state: projection.Projection,
  operator_command: command.OperatorCommand,
  plan: workflow_repair.RepairPlan,
  observation: recovery.CurrentWorkflowObservation,
  issue_id: String,
  issue_identifier: String,
  step_id: Option(String),
  now_ms: Int,
) -> Result(Nil, command.CommandResult) {
  case
    recovery.finalize_retry_step_candidates_with_config(
      projection_state,
      [plan.candidate],
      dict.from_list([#(plan.run_id, observation)]),
      artifact_store.new(effective.workspace.root),
      now_ms,
      effective,
    )
  {
    Error(recovery_error) -> {
      let detail = recovery.describe_error(recovery_error)
      case retry_step_validation.physical_recovery_failure(detail) {
        True ->
          Error(retained_recovery_rejection(
            operator_command,
            detail,
            plan.run_id,
            issue_id,
            issue_identifier,
          ))
        False ->
          Error(command.rejected(
            operator_command,
            "workflow_recovery_failed",
            Some(failure_message(
              "workflow_recovery_failed",
              Some(detail),
              plan.run_id,
              step_id,
            )),
          ))
      }
    }
    Ok(finalization) ->
      finalization_preflight(
        bundle,
        operator_command,
        plan.run_id,
        issue_id,
        issue_identifier,
        step_id,
        finalization,
      )
  }
}

fn finalization_preflight(
  bundle: runtime_bundle.RuntimeBundle,
  operator_command: command.OperatorCommand,
  run_id: String,
  issue_id: String,
  issue_identifier: String,
  step_id: Option(String),
  finalization: recovery.WorkflowFinalization,
) -> Result(Nil, command.CommandResult) {
  case finalization.resumptions {
    [resumption] ->
      case
        retry_step_resumption.validate_operational_inputs(bundle, resumption)
      {
        Ok(_) -> Ok(Nil)
        Error(failure) ->
          case retry_step_validation.physical_recovery_failure(failure.reason) {
            True ->
              Error(retained_recovery_rejection(
                operator_command,
                failure.message,
                run_id,
                issue_id,
                issue_identifier,
              ))
            False ->
              Error(command.rejected(
                operator_command,
                failure.reason,
                Some(validation_rejection_message(failure, run_id, step_id)),
              ))
          }
      }
    _ -> {
      let reason = rejection_reason(finalization)
      let detail = case finalization.diagnostics {
        [diagnostic, ..] ->
          recovery.workflow_recovery_diagnostic_message(diagnostic)
        [] -> "recovery validation did not produce an executable resumption"
      }
      case retry_step_validation.physical_recovery_failure(reason) {
        True ->
          Error(retained_recovery_rejection(
            operator_command,
            detail,
            run_id,
            issue_id,
            issue_identifier,
          ))
        False ->
          Error(command.rejected(
            operator_command,
            reason,
            rejection_message(finalization, run_id, step_id),
          ))
      }
    }
  }
}

fn retained_recovery_rejection(
  operator_command: command.OperatorCommand,
  detail: String,
  run_id: String,
  issue_id: String,
  issue_identifier: String,
) -> command.CommandResult {
  let fresh_issue_identifier = case string.trim(issue_id) {
    "" -> None
    _ -> Some(issue_identifier)
  }
  command.rejected(
    operator_command,
    "retained_recovery_unavailable",
    Some(retained_recovery_unavailable_message(
      detail,
      run_id,
      fresh_issue_identifier,
    )),
  )
}

pub fn admission_error_message(
  error: workflow_repair.RepairError,
  target: command.RetryWorkflowStepTarget,
) -> Option(String) {
  let reason = workflow_repair.describe_error(error)
  let base = option.unwrap(workflow_repair.error_message(error), reason)
  Some(
    base
    <> "; no run, park, or tracker state was changed. Next safe command: "
    <> admission_next_command(reason, target),
  )
}

fn admission_next_command(
  reason: String,
  target: command.RetryWorkflowStepTarget,
) -> String {
  case reason {
    "ambiguous_failed_run" -> "scripts/scherzoctl ps --json"
    "no_failed_workflow_run" -> no_failed_run_next_command(target)
    _ -> "scripts/scherzoctl ps --json"
  }
}

fn no_failed_run_next_command(
  target: command.RetryWorkflowStepTarget,
) -> String {
  case target {
    command.RetryWorkflowStepRunId(run_id) ->
      "scripts/scherzoctl session " <> run_id <> " --json"
    command.RetryWorkflowStepIssueRef(issue_ref) ->
      "scripts/scherzoctl retry all "
      <> command.issue_ref_to_string(issue_ref)
      <> " --json"
    command.RetryWorkflowStepAutoTarget(target) ->
      "scripts/scherzoctl retry all " <> target <> " --json"
  }
}
