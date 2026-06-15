import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/control/command
import scherzo/ctl/artifact_publication_retry
import scherzo/state/projection
import scherzo/state/recovery.{
  type CurrentWorkflowObservation, CurrentWorkflow, IssueUnavailable,
  TrackerRefreshUnavailable, WorkflowUnavailable,
}
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_repair

pub type Outcome {
  FreshDispatch
  StepRecovery(plan: workflow_repair.RepairPlan)
  PublicationRecovery(run_id: String, workflow_id: String)
  RejectRecovery(reason: String, message: String)
}

pub fn classify(
  projected: projection.Projection,
  issue: tracker_issue.Issue,
  observation: CurrentWorkflowObservation,
) -> Outcome {
  case is_auto_unpark_issue_change(projected, issue) {
    True -> FreshDispatch
    False ->
      case latest_workflow_run_for_issue(projected, issue.id) {
        Error(Nil) -> classify_by_step_plan(projected, issue, observation)
        Ok(run_id) ->
          case classify_by_step_plan(projected, issue, observation) {
            FreshDispatch ->
              classify_by_publication_retry(projected, run_id, observation)
            outcome -> outcome
          }
      }
  }
}

fn is_auto_unpark_issue_change(
  projected: projection.Projection,
  issue: tracker_issue.Issue,
) -> Bool {
  case dict.get(projected.parked_issues, issue.id) {
    Ok(parked) ->
      parked.release_policy == "auto_unpark_on_issue_change"
      && !tracker_issue.fingerprint_matches(parked.issue_fingerprint, issue)
    Error(Nil) -> False
  }
}

fn classify_by_step_plan(
  projected: projection.Projection,
  issue: tracker_issue.Issue,
  observation: CurrentWorkflowObservation,
) -> Outcome {
  let target = command.RetryWorkflowStepIssueRef(command.IssueId(issue.id))
  case workflow_repair.plan(projected, target, None, observation) {
    Ok(plan) -> StepRecovery(plan)
    Error(error) -> classify_after_step_plan_failure(error)
  }
}

fn classify_after_step_plan_failure(
  error: workflow_repair.RepairError,
) -> Outcome {
  let reason = workflow_repair.describe_error(error)
  let message = workflow_repair.error_message(error)
  case reason {
    "no_failed_workflow_run" -> FreshDispatch
    _ -> RejectRecovery(reason, option_string(message))
  }
}

fn classify_by_publication_retry(
  projected: projection.Projection,
  run_id: String,
  observation: CurrentWorkflowObservation,
) -> Outcome {
  case
    validate_publication_recovery_provenance(projected, run_id, observation)
  {
    Error(#(reason, message)) -> RejectRecovery(reason, message)
    Ok(workflow_id) ->
      case
        artifact_publication_retry.inspect_retryable_attempts(
          projected,
          run_id,
          None,
        )
      {
        Ok(_) -> PublicationRecovery(run_id, workflow_id)
        Error(#(publication_reason, publication_message)) ->
          RejectRecovery(publication_reason, publication_message)
      }
  }
}

fn validate_publication_recovery_provenance(
  projected: projection.Projection,
  run_id: String,
  observation: CurrentWorkflowObservation,
) -> Result(String, #(String, String)) {
  case observation {
    CurrentWorkflow(
      issue,
      workflow_id,
      workflow_fingerprint,
      issue_fingerprint,
      _,
      _,
    ) -> {
      use provenance <- result.try(
        projection.workflow_run_provenance(projected, run_id)
        |> result.map_error(fn(_) {
          #(
            "publication_recovery_provenance_missing",
            "workflow run is missing retained provenance for publication recovery: "
              <> run_id,
          )
        }),
      )
      case
        provenance.workflow_id != workflow_id
        || {
          provenance.workflow_fingerprint != ""
          && provenance.workflow_fingerprint != workflow_fingerprint
        }
      {
        True ->
          Error(#(
            "publication_recovery_workflow_drift",
            "retained publication workflow no longer matches current workflow: "
              <> run_id,
          ))
        False ->
          validate_publication_issue_provenance(
            provenance,
            issue,
            issue_fingerprint,
            run_id,
          )
          |> result.map(fn(_) { workflow_id })
      }
    }
    IssueUnavailable ->
      Error(#(
        "publication_recovery_issue_unavailable",
        "current issue could not be refreshed for publication recovery: "
          <> run_id,
      ))
    TrackerRefreshUnavailable ->
      Error(#(
        "publication_recovery_tracker_refresh_unavailable",
        "current issue refresh is unavailable for publication recovery: "
          <> run_id,
      ))
    WorkflowUnavailable(reason) ->
      Error(#(
        "publication_recovery_workflow_unavailable",
        "current workflow could not be loaded for publication recovery: "
          <> reason,
      ))
  }
}

fn validate_publication_issue_provenance(
  provenance: projection.WorkflowRunProvenance,
  issue: tracker_issue.Issue,
  issue_fingerprint: String,
  run_id: String,
) -> Result(Nil, #(String, String)) {
  let matches =
    provenance.issue_id == issue.id
    && provenance.issue_identifier == issue.identifier
    && tracker_issue.fingerprint_equivalent(
      provenance.issue_fingerprint,
      issue_fingerprint,
    )
  case matches {
    True -> Ok(Nil)
    False ->
      Error(#(
        "publication_recovery_issue_drift",
        "retained publication output no longer matches current issue: "
          <> run_id,
      ))
  }
}

fn latest_workflow_run_for_issue(
  projected: projection.Projection,
  issue_id: String,
) -> Result(String, Nil) {
  projected.workflow_runs
  |> dict.to_list
  |> list.filter_map(fn(entry) {
    let #(run_id, status) = entry
    case workflow_run_issue_id(status) == issue_id {
      True -> Ok(#(run_id, workflow_run_recorded_at_ms(status)))
      False -> Error(Nil)
    }
  })
  |> latest_run_id()
}

fn latest_run_id(runs: List(#(String, Int))) -> Result(String, Nil) {
  case runs {
    [] -> Error(Nil)
    [first, ..rest] -> {
      let #(run_id, _) =
        list.fold(rest, first, fn(best, candidate) {
          let #(_, best_at_ms) = best
          let #(_, candidate_at_ms) = candidate
          case candidate_at_ms > best_at_ms {
            True -> candidate
            False -> best
          }
        })
      Ok(run_id)
    }
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

fn workflow_run_recorded_at_ms(status: projection.WorkflowRunStatus) -> Int {
  case status {
    projection.WorkflowRunActive(started_at_ms: started_at_ms, ..) ->
      started_at_ms
    projection.WorkflowRunFinished(finished_at_ms: finished_at_ms, ..) ->
      finished_at_ms
    projection.WorkflowRunInterrupted(interrupted_at_ms: interrupted_at_ms, ..) ->
      interrupted_at_ms
    projection.WorkflowRunSuperseded(superseded_at_ms: superseded_at_ms, ..) ->
      superseded_at_ms
  }
}

fn option_string(value: Option(String)) -> String {
  case value {
    None -> ""
    Some(text) -> text
  }
}
