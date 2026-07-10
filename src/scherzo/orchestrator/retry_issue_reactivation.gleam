import gleam/dict
import gleam/option.{None, Some}
import gleam/result
import scherzo/config/types as config_types
import scherzo/orchestrator/core
import scherzo/task
import scherzo/tracker/adapter
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_completion_policy

pub type ReactivationError {
  ReactivationError(reason: String, message: String)
}

type ReactivationTarget {
  ReactivationTarget(
    state_id: option.Option(String),
    state_name: String,
    validate_active: Bool,
  )
}

pub fn for_operator_retry(
  tracker_adapter: adapter.TrackerAdapter,
  effective: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
  reason: String,
) -> Result(tracker_issue.Issue, ReactivationError) {
  use _ <- result.try(retry_allowed(effective, issue))
  case core.is_active(effective, issue.state) {
    True -> Ok(issue)
    False -> reactivate(tracker_adapter, effective, issue, reason)
  }
}

pub fn for_fresh_claim(
  effective: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
) -> Result(tracker_issue.Issue, ReactivationError) {
  use _ <- result.try(retry_allowed(effective, issue))
  case core.is_active(effective, issue.state) {
    True -> Ok(issue)
    False -> {
      use target <- result.try(configured_claim_target(effective, issue))
      use _ <- result.try(validate_target(effective, issue, target))
      Ok(
        tracker_issue.Issue(
          ..issue,
          state: issue_state.from_string_unchecked(target.state_name),
        ),
      )
    }
  }
}

fn retry_allowed(
  effective: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
) -> Result(Nil, ReactivationError) {
  case core.is_terminal(effective, issue.state) {
    False -> Ok(Nil)
    True ->
      Error(ReactivationError(
        "retry_terminal_state:" <> issue_state.to_string(issue.state),
        "terminal issue "
          <> issue.identifier
          <> " cannot be reactivated for operator retry",
      ))
  }
}

fn reactivate(
  tracker_adapter: adapter.TrackerAdapter,
  effective: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
  reason: String,
) -> Result(tracker_issue.Issue, ReactivationError) {
  use target <- result.try(reactivation_target(effective, issue))
  use _ <- result.try(validate_target(effective, issue, target))
  case tracker_adapter.state_transitions {
    None ->
      Error(ReactivationError(
        "retry_issue_reactivation_unsupported",
        "issue "
          <> issue.identifier
          <> " is non-active in "
          <> issue_state.to_string(issue.state)
          <> ", but tracker "
          <> tracker_adapter.kind
          <> " does not support the state transition required for operator retry",
      ))
    Some(state_transitions) ->
      case
        state_transitions.transition(adapter.StateTransitionRequest(
          task: task.from_legacy_issue(issue).ref,
          target_state_id: target.state_id,
          target_state_name: target.state_name,
          reason: reason,
        ))
      {
        Error(error) ->
          Error(ReactivationError(
            "retry_issue_reactivation_failed",
            "failed to move issue "
              <> issue.identifier
              <> " from "
              <> issue_state.to_string(issue.state)
              <> " to active retry state "
              <> target.state_name
              <> ": "
              <> adapter_error_message(error),
          ))
        Ok(adapter.StateTransitionReceipt(state: transitioned_state, ..)) -> {
          let task.TaskState(name: state_name, ..) = transitioned_state
          Ok(
            tracker_issue.Issue(
              ..issue,
              state: issue_state.from_string_unchecked(state_name),
            ),
          )
        }
      }
  }
}

fn reactivation_target(
  effective: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
) -> Result(ReactivationTarget, ReactivationError) {
  case effective.handoff.claim_state_id {
    Some(_) -> configured_claim_target(effective, issue)
    None ->
      case effective.tracker.active_states {
        [state, ..] ->
          Ok(ReactivationTarget(None, issue_state.to_string(state), True))
        [] ->
          Error(ReactivationError(
            "retry_issue_reactivation_not_configured",
            "issue "
              <> issue.identifier
              <> " is non-active in "
              <> issue_state.to_string(issue.state)
              <> ", and no task_updates.states.claim or tracker.states.active target is configured for operator retry",
          ))
      }
  }
}

fn configured_claim_target(
  effective: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
) -> Result(ReactivationTarget, ReactivationError) {
  case effective.handoff.claim_state_id {
    Some(workflow_completion_policy.StateByName(state_name)) ->
      Ok(ReactivationTarget(None, state_name, True))
    Some(workflow_completion_policy.StateById(state_id)) -> {
      let #(state_name, validate_active) =
        configured_claim_state_name(effective, state_id)
      Ok(ReactivationTarget(Some(state_id), state_name, validate_active))
    }
    None ->
      Error(ReactivationError(
        "retry_issue_reactivation_not_configured",
        "issue "
          <> issue.identifier
          <> " is non-active in "
          <> issue_state.to_string(issue.state)
          <> ", and no task_updates.states.claim target is configured for a fresh operator retry",
      ))
  }
}

fn configured_claim_state_name(
  effective: config_types.EffectiveConfig,
  fallback: String,
) -> #(String, Bool) {
  case dict.get(effective.linear_contract.handoff_state_bindings, "claim") {
    Error(Nil) -> #(fallback, False)
    Ok(required_state_key) ->
      case
        dict.get(effective.linear_contract.required_states, required_state_key)
      {
        Error(Nil) -> #(fallback, False)
        Ok(state_name) -> #(state_name, True)
      }
  }
}

fn validate_target(
  effective: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
  target: ReactivationTarget,
) -> Result(Nil, ReactivationError) {
  case
    target.validate_active
    && !core.is_active(
      effective,
      issue_state.from_string_unchecked(target.state_name),
    )
  {
    False -> Ok(Nil)
    True ->
      Error(ReactivationError(
        "retry_issue_reactivation_invalid_target",
        "configured retry claim state "
          <> target.state_name
          <> " is not included in tracker.states.active; issue "
          <> issue.identifier
          <> " was not changed",
      ))
  }
}

fn adapter_error_message(error: adapter.TrackerError) -> String {
  case error {
    adapter.Unauthorized(message)
    | adapter.Transient(message)
    | adapter.Permanent(message)
    | adapter.DecodeFailed(message) -> message
    adapter.NotFound(ref) -> "task not found: " <> ref.remote_id
    adapter.UnsupportedCapability(capability) ->
      "unsupported capability: " <> capability
  }
}
