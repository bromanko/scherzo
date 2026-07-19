import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/cleanup/retention_marker
import scherzo/state/projection
import scherzo/workflow_outcome
import scherzo/workspace_run
import simplifile

pub fn required_run_ids(
  projected: projection.Projection,
) -> dict.Dict(String, Bool) {
  let boundary_run_ids = repair_boundary_run_ids(projected)
  projected.workflow_runs
  |> dict.to_list
  |> list.fold(dict.new(), fn(required, entry) {
    let #(run_id, status) = entry
    case status {
      projection.WorkflowRunFinished(outcome: outcome, ..) ->
        case
          workflow_outcome.is_terminal_failure(outcome)
          && dict.has_key(boundary_run_ids, run_id)
        {
          True -> dict.insert(required, run_id, True)
          False -> required
        }
      _ -> required
    }
  })
}

fn repair_boundary_run_ids(
  projected: projection.Projection,
) -> dict.Dict(String, Bool) {
  projected.step_attempts
  |> dict.values
  |> list.fold(dict.new(), fn(required, status) {
    case status {
      projection.StepAttemptPending(run_id: run_id, ..)
      | projection.StepAttemptRunning(run_id: run_id, ..)
      | projection.StepAttemptInterruptedStatus(run_id: run_id, ..) ->
        dict.insert(required, run_id, True)
      projection.StepAttemptFinishedStatus(run_id: run_id, outcome: outcome, ..) ->
        case workflow_outcome.is_terminal_failure(outcome) {
          True -> dict.insert(required, run_id, True)
          False -> required
        }
      projection.StepAttemptSupersededStatus(..) -> required
    }
  })
}

pub fn hard_hold_reason(
  run_statuses: dict.Dict(String, projection.WorkflowRunStatus),
  retry_required_run_ids: dict.Dict(String, Bool),
  parked_issue_ids: dict.Dict(String, Bool),
  run_root: String,
  run_id: String,
) -> Option(String) {
  case dict.get(run_statuses, run_root) {
    Ok(projection.WorkflowRunInterrupted(issue_id: issue_id, ..)) ->
      case dict.get(parked_issue_ids, issue_id) {
        Ok(True) ->
          Some(
            "issue for retained workspace run is parked and must be released before cleanup",
          )
        _ ->
          Some(
            "workspace run was interrupted and still requires operator review",
          )
      }
    Ok(projection.WorkflowRunFinished(issue_id: issue_id, ..)) ->
      case dict.get(parked_issue_ids, issue_id) {
        Ok(True) ->
          Some(
            "issue for retained workspace run is parked and must be released before cleanup",
          )
        _ ->
          case
            dict.has_key(retry_required_run_ids, run_id)
            && !retry_retention_released(run_root)
          {
            True ->
              Some(
                "terminal failed workflow has a logical repair boundary and requires its retained workspace for retry",
              )
            False -> None
          }
      }
    Ok(projection.WorkflowRunActive(issue_id: issue_id, ..))
    | Ok(projection.WorkflowRunSuperseded(issue_id: issue_id, ..)) ->
      case dict.get(parked_issue_ids, issue_id) {
        Ok(True) ->
          Some(
            "issue for retained workspace run is parked and must be released before cleanup",
          )
        _ -> None
      }
    Error(Nil) -> None
  }
}

fn retry_retention_released(run_root: String) -> Bool {
  case simplifile.read(workspace_run.cleanup_retention_marker(run_root)) {
    Ok(contents) ->
      case retention_marker.parse(contents) {
        retention_marker.SchemaMarker(retention_marker.SafeToDelete, _, _, _)
        | retention_marker.SchemaMarker(retention_marker.Abandoned, _, _, _) ->
          True
        _ -> False
      }
    Error(error) -> marker_unavailable(simplifile.describe_error(error))
  }
}

fn marker_unavailable(_reason: String) -> Bool {
  False
}
