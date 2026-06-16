import gleam/dict
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
                ..state,
                claimed: dict.delete(state.claimed, identity),
                parked: dict.delete(state.parked, identity),
                retry_attempts: dict.delete(state.retry_attempts, identity),
                issue_counters: dict.delete(state.issue_counters, identity),
              )
          }
        }
      }
    Error(Nil) -> state
  }
}

fn retry_intent_releases_park(parked: orchestrator_state.ParkedEntry) -> Bool {
  case parked.reason {
    reason.ParkOperator(_) -> False
    _ -> True
  }
}
