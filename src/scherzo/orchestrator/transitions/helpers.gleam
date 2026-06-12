import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/orchestrator/core
import scherzo/orchestrator/transition_types
import scherzo/orchestrator/workflow_snapshot
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_policy

pub fn select_workflow_route(
  context: transition_types.DispatchContext,
  issue: tracker_issue.Issue,
) -> Result(String, #(String, String)) {
  case
    workflow_policy.classify_issue(context.effective.linear_contract, issue)
  {
    workflow_policy.WorkflowInvalid(violation) ->
      Error(workflow_violation_to_route_error(violation))
    workflow_policy.WorkflowSelected(id, _) ->
      lookup_workflow(context.available_workflow_ids, id)
    workflow_policy.WorkflowPolicyDisabled ->
      select_unenforced_workflow_route(context, issue)
  }
}

fn select_unenforced_workflow_route(
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

fn workflow_violation_to_route_error(
  violation: workflow_policy.IssueWorkflowViolation,
) -> #(String, String) {
  case violation {
    workflow_policy.MissingWorkflowLabel -> #(
      "missing_workflow_label",
      "issue has no workflow label",
    )
    workflow_policy.MultipleWorkflowLabels(_) -> #(
      "multiple_workflow_labels",
      "issue has multiple workflow labels",
    )
    workflow_policy.UnknownWorkflowLabel(label) -> #(
      "unknown_workflow_label",
      "unknown workflow label: " <> label,
    )
  }
}

pub fn workflow_snapshot_for_claim(
  context: transition_types.DispatchContext,
  issue: tracker_issue.Issue,
  workflow_id: String,
  run_id: String,
) -> Result(workflow_snapshot.Snapshot, #(String, String)) {
  case
    workflow_snapshot.for_workflow_id(
      context.review_lane_preflight.workflow_dags,
      context.orchestrator,
      issue,
      workflow_id,
      run_id,
    )
  {
    Ok(snapshot) -> Ok(snapshot)
    Error(error) -> Error(workflow_snapshot.error_fields(error))
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
