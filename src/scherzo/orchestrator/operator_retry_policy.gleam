import gleam/dict
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
import scherzo/tracker/issue as tracker_issue

pub fn start_fresh_block_reason(
  runtime: orchestrator_state.RuntimeState,
  context: transition_types.DispatchContext,
  issue: tracker_issue.Issue,
) -> Result(Nil, String) {
  case
    dict.get(
      runtime.parked,
      orchestrator_state.linear_issue_id_identity(issue.id),
    )
  {
    Ok(orchestrator_state.ParkedEntry(reason: reason, ..)) ->
      case
        qualifying_start_fresh_reason(orchestrator_reason.park_to_string(reason))
      {
        True -> Ok(Nil)
        False -> Error("start_fresh_not_allowed")
      }
    Error(Nil) ->
      case parked_reason_from_projection(context.workspace_root, issue.id) {
        Some(reason) ->
          case qualifying_start_fresh_reason(reason) {
            True -> Ok(Nil)
            False -> Error("start_fresh_not_allowed")
          }
        None ->
          case dict.get(context.recovery_by_issue, issue.id) {
            Ok(recovery) ->
              case
                recovery_allows_start_fresh(recovery)
                || can_reactivate_non_active_issue(context, issue)
              {
                True -> Ok(Nil)
                False -> Error("start_fresh_not_allowed")
              }
            Error(Nil) -> Ok(Nil)
          }
      }
  }
}

pub fn context_for_start_fresh(
  context: transition_types.DispatchContext,
  issue_id: String,
) -> transition_types.DispatchContext {
  let marker =
    session_recovery.base_info(
      event.Recovered,
      "operator_start_fresh",
      None,
      [],
    )
  transition_types.DispatchContext(
    ..context,
    recovery_by_issue: dict.insert(context.recovery_by_issue, issue_id, marker),
  )
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
