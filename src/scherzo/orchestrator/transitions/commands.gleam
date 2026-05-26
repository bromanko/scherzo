import gleam/option.{type Option}
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/transition_types
import scherzo/orchestrator/transitions/operator
import scherzo/state/ledger
import scherzo/tracker/issue as tracker_issue

pub type OperatorCallbacks =
  operator.Callbacks

pub fn operator_callbacks(
  issue_is_running_claimed_or_pending: fn(
    transition_types.State,
    transition_types.DispatchContext,
    String,
  ) -> Bool,
  can_reserve_dispatch_slot: fn(
    transition_types.State,
    transition_types.DispatchContext,
    tracker_issue.Issue,
  ) -> Bool,
  dispatch_candidates: fn(
    List(tracker_issue.Issue),
    transition_types.State,
    transition_types.DispatchContext,
  ) -> transition_types.Outcome,
) -> operator.Callbacks {
  operator.Callbacks(
    issue_is_running_claimed_or_pending: issue_is_running_claimed_or_pending,
    can_reserve_dispatch_slot: can_reserve_dispatch_slot,
    dispatch_candidates: dispatch_candidates,
  )
}

pub fn handle_operator_submitted(
  state: transition_types.State,
  request: effects_types.OperatorCommandRequest,
  context: transition_types.DispatchContext,
  issue_resolution: transition_types.OperatorIssueResolution,
  parked_issue_resolution: transition_types.ParkedIssueResolution,
  callbacks: operator.Callbacks,
) -> transition_types.Outcome {
  operator.handle_submitted(
    state,
    request,
    context,
    issue_resolution,
    parked_issue_resolution,
    callbacks,
  )
}

pub fn handle_operator_report_park_continuation(
  state: transition_types.State,
  correlation_id: String,
  issue_id: String,
  issue_identifier: String,
  reason: String,
  release_policy: String,
  source_run_id: Option(String),
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  operator.handle_report_park_continuation(
    state,
    correlation_id,
    issue_id,
    issue_identifier,
    reason,
    release_policy,
    source_run_id,
    result,
  )
}
