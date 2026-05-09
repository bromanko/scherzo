import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/orchestrator/core
import scherzo/orchestrator/transition_types
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

pub fn select_workflow_route(
  context: transition_types.DispatchContext,
  issue: tracker_issue.Issue,
) -> Result(String, #(String, String)) {
  let labels =
    workflow_labels(issue.labels, context.routing.workflow_label_prefix, [])
  case labels {
    [] ->
      case context.routing.require_exactly_one_workflow_label {
        True ->
          Error(#("missing_workflow_label", "issue has no workflow label"))
        False ->
          case context.routing.default_workflow {
            Some(id) -> lookup_workflow(context.available_workflow_ids, id)
            None ->
              Error(#("missing_workflow_label", "issue has no workflow label"))
          }
      }
    [id] -> lookup_workflow(context.available_workflow_ids, id)
    _ ->
      Error(#("multiple_workflow_labels", "issue has multiple workflow labels"))
  }
}

pub fn dispatch_validation_error_reason(
  err: transition_types.DispatchValidationError,
) -> String {
  case err {
    transition_types.DispatchValidationTrackerError(tracker_error) ->
      "tracker_error:" <> tracker_error
    transition_types.DispatchValidationMissingIssue -> "missing_issue"
    transition_types.DispatchValidationDuplicateIssue -> "duplicate_issue"
    transition_types.DispatchValidationIdMismatch(_, _) -> "id_mismatch"
  }
}

pub fn blocker_summary(
  issue: tracker_issue.Issue,
  decision: core.BlockerDecision,
) -> String {
  let blockers = case decision {
    core.BlockersSatisfied -> issue.blocked_by
    core.BlockedByDependency(open_blockers, _) ->
      case open_blockers {
        [] -> issue.blocked_by
        _ -> open_blockers
      }
  }
  blockers
  |> list.map(blocker_to_summary)
  |> string.join(with: ",")
}

pub fn bool_field(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

pub fn make_run_id(
  issue: tracker_issue.Issue,
  now_ms: Int,
  sequence: Int,
) -> String {
  issue.identifier
  <> "-"
  <> int.to_string(now_ms)
  <> "-"
  <> int.to_string(sequence)
}

pub fn make_session_id(
  _issue_identifier: String,
  run_id: String,
  _sequence: Int,
) -> String {
  run_id
}

pub fn claim_correlation_id(issue_id: String, run_id: String) -> String {
  "claim:" <> issue_id <> ":" <> run_id
}

fn lookup_workflow(
  available_workflow_ids: List(String),
  id: String,
) -> Result(String, #(String, String)) {
  case list.contains(available_workflow_ids, id) {
    True -> Ok(id)
    False ->
      Error(#("unknown_workflow_label", "unknown workflow label: " <> id))
  }
}

fn workflow_labels(
  labels: List(String),
  prefix: String,
  acc: List(String),
) -> List(String) {
  case labels {
    [] -> list.reverse(acc)
    [label, ..rest] -> {
      let label = label |> string.trim |> string.lowercase
      case prefix != "" && string.starts_with(label, prefix) {
        True ->
          workflow_labels(rest, prefix, [
            string.drop_start(label, string.length(prefix)),
            ..acc
          ])
        False -> workflow_labels(rest, prefix, acc)
      }
    }
  }
}

fn blocker_to_summary(blocker: tracker_issue.BlockerRef) -> String {
  let name = case blocker.identifier {
    Some(identifier) -> identifier
    None ->
      case blocker.id {
        Some(id) -> id
        None -> "unknown"
      }
  }
  let state = case blocker.state {
    Some(state) -> issue_state.to_string(state)
    None -> "unknown"
  }
  name <> ":" <> state
}
