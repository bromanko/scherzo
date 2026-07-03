import gleam/dict
import gleam/option.{type Option, None, Some}
import scherzo/config/types as config_types
import scherzo/runtime/reason
import scherzo/runtime/state as orchestrator_state
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

pub fn unpark_if_issue_changed(
  state: orchestrator_state.RuntimeState,
  issue: tracker_issue.Issue,
) -> orchestrator_state.RuntimeState {
  unpark_if_issue_changed_with_retry_intent(state, issue, False)
}

pub fn is_parked_for_dispatch(
  state: orchestrator_state.RuntimeState,
  issue: tracker_issue.Issue,
) -> Bool {
  case dict.get(state.parked, orchestrator_state.issue_identity(issue)) {
    Ok(parked) -> park_blocks_dispatch(parked, issue)
    Error(Nil) -> False
  }
}

pub fn is_parked_for_retry(
  state: orchestrator_state.RuntimeState,
  issue: tracker_issue.Issue,
) -> Bool {
  case dict.get(state.parked, orchestrator_state.issue_identity(issue)) {
    Ok(parked) -> park_blocks_retry(parked, issue)
    Error(Nil) -> False
  }
}

pub fn retry_releasable_park_for_issue(
  state: orchestrator_state.RuntimeState,
  issue: tracker_issue.Issue,
) -> Option(orchestrator_state.ParkedEntry) {
  case dict.get(state.parked, orchestrator_state.issue_identity(issue)) {
    Ok(parked) ->
      case retry_intent_releases_park(parked) {
        True -> Some(parked)
        False -> None
      }
    Error(Nil) -> None
  }
}

pub fn unpark_if_issue_changed_or_retry_intent(
  state: orchestrator_state.RuntimeState,
  config: config_types.EffectiveConfig,
  issue: tracker_issue.Issue,
) -> orchestrator_state.RuntimeState {
  unpark_if_issue_changed_with_retry_intent(
    state,
    issue,
    issue_state.contains_normalized(config.tracker.dispatch_states, issue.state),
  )
}

fn unpark_if_issue_changed_with_retry_intent(
  state: orchestrator_state.RuntimeState,
  issue: tracker_issue.Issue,
  retry_intent: Bool,
) -> orchestrator_state.RuntimeState {
  let identity = orchestrator_state.issue_identity(issue)
  case dict.get(state.parked, identity) {
    Ok(parked) ->
      case parked.release_policy {
        orchestrator_state.ExplicitUnparkOnly -> state
        orchestrator_state.AutoUnparkOnIssueChange(stored) -> {
          let release_on_issue_change =
            !tracker_issue.fingerprint_matches(stored, issue)
          let release_on_retry_intent =
            retry_intent && retry_intent_releases_park(parked)
          case release_on_issue_change || release_on_retry_intent {
            False -> state
            True ->
              orchestrator_state.RuntimeState(
                ..orchestrator_state.clear_task_lifecycle(state, identity),
                issue_counters: dict.delete(state.issue_counters, identity),
              )
          }
        }
      }
    Error(Nil) -> state
  }
}

pub fn retry_intent_releases_park(
  parked: orchestrator_state.ParkedEntry,
) -> Bool {
  case parked.release_policy {
    orchestrator_state.AutoUnparkOnIssueChange(_) ->
      case parked.reason {
        reason.ParkWorkerFailure -> True
        _ -> False
      }
    orchestrator_state.ExplicitUnparkOnly -> False
  }
}

fn park_blocks_dispatch(
  parked: orchestrator_state.ParkedEntry,
  issue: tracker_issue.Issue,
) -> Bool {
  case parked.release_policy {
    orchestrator_state.ExplicitUnparkOnly -> True
    orchestrator_state.AutoUnparkOnIssueChange(stored) ->
      tracker_issue.fingerprint_matches(stored, issue)
  }
}

fn park_blocks_retry(
  parked: orchestrator_state.ParkedEntry,
  issue: tracker_issue.Issue,
) -> Bool {
  park_blocks_dispatch(parked, issue) && !retry_intent_releases_park(parked)
}
