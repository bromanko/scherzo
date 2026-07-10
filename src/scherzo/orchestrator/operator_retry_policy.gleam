import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/orchestrator/core
import scherzo/orchestrator/retry_issue_reactivation
import scherzo/orchestrator/transition_types
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
import scherzo/session/event
import scherzo/session/recovery as session_recovery
import scherzo/state/ledger
import scherzo/state/projection
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_outcome

pub type StartFreshPlan {
  StartFreshDispatch
  StartFreshSupersession(run_id: String, workflow_id: String)
}

pub type StartFreshPlanError {
  StartFreshNotAllowed
  StartFreshAlreadySuperseded
  StartFreshProjectionUnavailable(ledger.LedgerError)
}

pub fn start_fresh_plan(
  runtime: orchestrator_state.RuntimeState,
  context: transition_types.DispatchContext,
  issue: tracker_issue.Issue,
  _reason: String,
) -> Result(StartFreshPlan, StartFreshPlanError) {
  case
    dict.get(
      runtime.parked,
      orchestrator_state.linear_issue_id_identity(issue.id),
    )
  {
    Ok(orchestrator_state.ParkedEntry(reason: parked_reason, ..)) ->
      case
        qualifying_start_fresh_reason(orchestrator_reason.park_to_string(
          parked_reason,
        ))
      {
        True -> plan_from_retained_run(context, issue)
        False -> Error(StartFreshNotAllowed)
      }
    Error(Nil) ->
      case parked_reason_from_projection(context.workspace_root, issue.id) {
        Some(parked_reason) ->
          case qualifying_start_fresh_reason(parked_reason) {
            True -> plan_from_retained_run(context, issue)
            False -> Error(StartFreshNotAllowed)
          }
        None ->
          case plan_from_retained_run(context, issue) {
            Ok(StartFreshSupersession(run_id, workflow_id)) ->
              Ok(StartFreshSupersession(run_id, workflow_id))
            Error(error) -> Error(error)
            Ok(StartFreshDispatch) ->
              case dict.get(context.recovery_by_issue, issue.id) {
                Ok(recovery) ->
                  case
                    recovery_allows_start_fresh(recovery)
                    || can_reactivate_non_active_issue(context, issue)
                  {
                    True -> Ok(StartFreshDispatch)
                    False -> Error(StartFreshNotAllowed)
                  }
                Error(Nil) -> Ok(StartFreshDispatch)
              }
          }
      }
  }
}

pub fn context_for_start_fresh(
  context: transition_types.DispatchContext,
  issue_id: String,
  plan: StartFreshPlan,
  reason: String,
) -> transition_types.DispatchContext {
  let superseded_run_id = case plan {
    StartFreshDispatch -> None
    StartFreshSupersession(run_id, _) -> Some(run_id)
  }
  let marker =
    event.RecoveryInfo(
      ..session_recovery.base_info(
        event.Recovered,
        "operator_start_fresh",
        Some(reason),
        [],
      ),
      workflow_run_id: superseded_run_id,
    )
  transition_types.DispatchContext(
    ..context,
    recovery_by_issue: dict.insert(context.recovery_by_issue, issue_id, marker),
  )
}

fn plan_from_retained_run(
  context: transition_types.DispatchContext,
  issue: tracker_issue.Issue,
) -> Result(StartFreshPlan, StartFreshPlanError) {
  case retained_projection(context.workspace_root) {
    Error(error) -> Error(StartFreshProjectionUnavailable(error))
    Ok(projected) ->
      case latest_unsuperseded_run_for_issue(projected, issue.id) {
        Some(#(
          run_id,
          projection.WorkflowRunFinished(
            workflow_id: workflow_id,
            outcome: outcome,
            ..,
          ),
        )) ->
          case workflow_outcome.is_terminal_failure(outcome) {
            True -> Ok(StartFreshSupersession(run_id, workflow_id))
            False -> already_superseded_or_dispatch(projected, issue.id, run_id)
          }
        Some(#(run_id, _)) ->
          already_superseded_or_dispatch(projected, issue.id, run_id)
        None ->
          case has_operator_supersession_for_issue(projected, issue.id) {
            True -> Error(StartFreshAlreadySuperseded)
            False -> Ok(StartFreshDispatch)
          }
      }
  }
}

fn retained_projection(
  workspace_root: String,
) -> Result(projection.Projection, ledger.LedgerError) {
  use ledger_path <- result.try(ledger.path_for_workspace_root(workspace_root))
  ledger.load_projection(ledger_path)
}

pub fn start_fresh_plan_error_reason(error: StartFreshPlanError) -> String {
  case error {
    StartFreshNotAllowed -> "start_fresh_not_allowed"
    StartFreshAlreadySuperseded -> "start_fresh_already_superseded"
    StartFreshProjectionUnavailable(_error) -> "ledger_read_failed"
  }
}

fn already_superseded_or_dispatch(
  projected: projection.Projection,
  issue_id: String,
  run_id: String,
) -> Result(StartFreshPlan, StartFreshPlanError) {
  case has_operator_supersession_target(projected, issue_id, run_id) {
    True -> Error(StartFreshAlreadySuperseded)
    False -> Ok(StartFreshDispatch)
  }
}

fn has_operator_supersession_target(
  projected: projection.Projection,
  issue_id: String,
  superseding_run_id: String,
) -> Bool {
  projected.workflow_runs
  |> dict.values
  |> list.any(fn(status) {
    case status {
      projection.WorkflowRunSuperseded(
        issue_id: candidate_issue_id,
        superseded_by_run_id: candidate_run_id,
        reason: candidate_reason,
        ..,
      ) ->
        candidate_issue_id == issue_id
        && candidate_run_id == superseding_run_id
        && string.starts_with(candidate_reason, "operator_start_fresh:")
      _ -> False
    }
  })
}

fn has_operator_supersession_for_issue(
  projected: projection.Projection,
  issue_id: String,
) -> Bool {
  projected.workflow_runs
  |> dict.values
  |> list.any(fn(status) {
    case status {
      projection.WorkflowRunSuperseded(
        issue_id: candidate_issue_id,
        reason: candidate_reason,
        ..,
      ) ->
        candidate_issue_id == issue_id
        && string.starts_with(candidate_reason, "operator_start_fresh:")
      _ -> False
    }
  })
}

fn latest_unsuperseded_run_for_issue(
  projected: projection.Projection,
  issue_id: String,
) -> Option(#(String, projection.WorkflowRunStatus)) {
  projected.workflow_runs
  |> dict.to_list
  |> list.filter(fn(entry) {
    let #(_, status) = entry
    workflow_run_issue_id(status) == issue_id && !is_superseded(status)
  })
  |> list.fold(
    None,
    fn(
      latest: Option(#(String, projection.WorkflowRunStatus)),
      candidate: #(String, projection.WorkflowRunStatus),
    ) {
      case latest {
        None -> Some(candidate)
        Some(current) ->
          case
            workflow_run_recorded_at(candidate.1)
            > workflow_run_recorded_at(current.1)
          {
            True -> Some(candidate)
            False -> Some(current)
          }
      }
    },
  )
}

fn is_superseded(status: projection.WorkflowRunStatus) -> Bool {
  case status {
    projection.WorkflowRunSuperseded(..) -> True
    _ -> False
  }
}

fn workflow_run_issue_id(status: projection.WorkflowRunStatus) -> String {
  case status {
    projection.WorkflowRunActive(issue_id: issue_id, ..)
    | projection.WorkflowRunFinished(issue_id: issue_id, ..)
    | projection.WorkflowRunInterrupted(issue_id: issue_id, ..)
    | projection.WorkflowRunSuperseded(issue_id: issue_id, ..) -> issue_id
  }
}

fn workflow_run_recorded_at(status: projection.WorkflowRunStatus) -> Int {
  case status {
    projection.WorkflowRunActive(started_at_ms: at_ms, ..)
    | projection.WorkflowRunFinished(finished_at_ms: at_ms, ..)
    | projection.WorkflowRunInterrupted(interrupted_at_ms: at_ms, ..)
    | projection.WorkflowRunSuperseded(superseded_at_ms: at_ms, ..) -> at_ms
  }
}

pub fn operator_supersession_reason(reason: String) -> String {
  "operator_start_fresh:" <> reason
}

pub fn start_fresh_applied_message(
  plan: StartFreshPlan,
  run_description: String,
  reason: String,
) -> String {
  case plan {
    StartFreshDispatch ->
      "retry accepted; starts a fresh run; "
      <> run_description
      <> "; reason: "
      <> reason
    StartFreshSupersession(superseded_run_id, _) ->
      "retry accepted; superseded run "
      <> superseded_run_id
      <> "; starts a fresh run; "
      <> run_description
      <> "; reason: "
      <> reason
  }
}

pub fn prepare_start_fresh_issue(
  context: transition_types.DispatchContext,
  issue: tracker_issue.Issue,
) -> Result(tracker_issue.Issue, #(String, String)) {
  case retry_issue_reactivation.for_fresh_claim(context.effective, issue) {
    Ok(issue) -> Ok(issue)
    Error(retry_issue_reactivation.ReactivationError(reason, message)) ->
      Error(#(reason, message))
  }
}

pub fn start_fresh_has_recovery_state(
  runtime: orchestrator_state.RuntimeState,
  context: transition_types.DispatchContext,
  issue: tracker_issue.Issue,
) -> Bool {
  case
    dict.get(
      runtime.parked,
      orchestrator_state.linear_issue_id_identity(issue.id),
    )
  {
    Ok(_) -> True
    Error(Nil) ->
      case parked_reason_from_projection(context.workspace_root, issue.id) {
        Some(_) -> True
        None -> dict.has_key(context.recovery_by_issue, issue.id)
      }
  }
}

fn parked_reason_from_projection(
  workspace_root: String,
  issue_id: String,
) -> Option(String) {
  case ledger.path_for_workspace_root(workspace_root) |> option.from_result {
    Some(ledger_path) ->
      case ledger.load_projection(ledger_path) |> option.from_result {
        Some(projected) ->
          case
            dict.get(projected.parked_issues, issue_id)
            |> option.from_result
          {
            Some(parked) -> Some(parked.reason)
            None -> None
          }
        None -> None
      }
    None -> None
  }
}

fn can_reactivate_non_active_issue(
  context: transition_types.DispatchContext,
  issue: tracker_issue.Issue,
) -> Bool {
  case
    !core.is_active(context.effective, issue.state)
    && !core.is_terminal(context.effective, issue.state)
  {
    False -> False
    True -> prepare_start_fresh_issue(context, issue) |> result.is_ok
  }
}

fn recovery_allows_start_fresh(recovery: event.RecoveryInfo) -> Bool {
  case recovery.park_reason {
    Some(reason) -> qualifying_start_fresh_reason(reason)
    None ->
      case recovery.drift_kind {
        Some(_) -> True
        None ->
          case recovery.status {
            event.Blocked
            | event.Parked
            | event.DriftDetected
            | event.OldStateResetRequired -> True
            _ -> False
          }
      }
  }
}

fn qualifying_start_fresh_reason(reason: String) -> Bool {
  string.starts_with(reason, "workflow_definition_drift")
  || string.starts_with(reason, "issue_content_drift")
  || string.starts_with(reason, "issue_state_drift")
  || string.starts_with(reason, "dispatch_recovery")
}
