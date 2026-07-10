import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/ctl/artifact_publication_retry
import scherzo/orchestrator/operator_retry_policy
import scherzo/session/event
import scherzo/state/projection
import scherzo/state/recovery.{
  type CurrentWorkflowObservation, CurrentWorkflow, IssueUnavailable,
  TrackerRefreshUnavailable, WorkflowUnavailable,
}
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_completion_policy
import scherzo/workflow_outcome
import scherzo/workflow_repair

pub type Outcome {
  FreshDispatch
  FreshSupersedingDispatch(
    superseded_run_id: String,
    workflow_id: String,
    reason: String,
    message: String,
  )
  SupersedingRunAlreadyExists(
    superseded_run_id: String,
    superseded_by_run_id: String,
  )
  RequeueRecovery(reason: String, message: String)
  StepRecovery(plan: workflow_repair.RepairPlan)
  PublicationRecovery(run_id: String, workflow_id: String)
  PublicationAlreadyPublished(run_id: String, workflow_id: String)
  RejectRecovery(reason: String, message: String)
}

pub fn classify_for_claim(
  projected: projection.Projection,
  issue: tracker_issue.Issue,
  observation: CurrentWorkflowObservation,
  recovery: Option(event.RecoveryInfo),
) -> Outcome {
  case recovery {
    Some(event.RecoveryInfo(
      source: "operator_start_fresh",
      workflow_run_id: Some(superseded_run_id),
      message: operator_reason,
      ..,
    )) ->
      classify_operator_supersession(
        projected,
        issue,
        superseded_run_id,
        operator_reason,
      )
    Some(event.RecoveryInfo(source: "operator_start_fresh", ..)) ->
      FreshDispatch
    _ -> classify(projected, issue, observation)
  }
}

fn classify_operator_supersession(
  projected: projection.Projection,
  issue: tracker_issue.Issue,
  superseded_run_id: String,
  operator_reason: Option(String),
) -> Outcome {
  case dict.get(projected.workflow_runs, superseded_run_id) {
    Ok(projection.WorkflowRunFinished(
      workflow_id: workflow_id,
      issue_id: issue_id,
      outcome: outcome,
      ..,
    )) ->
      case
        issue_id == issue.id && workflow_outcome.is_terminal_failure(outcome)
      {
        True -> {
          let reason = operator_reason |> option.unwrap("")
          FreshSupersedingDispatch(
            superseded_run_id,
            workflow_id,
            operator_retry_policy.operator_supersession_reason(reason),
            "operator-authorized fresh retry: " <> reason,
          )
        }
        False ->
          RejectRecovery(
            "start_fresh_not_allowed",
            "operator fresh retry target is not an eligible terminal failed run: "
              <> superseded_run_id,
          )
      }
    Ok(projection.WorkflowRunSuperseded(
      workflow_id: workflow_id,
      superseded_by_run_id: superseded_by_run_id,
      reason: reason,
      ..,
    )) ->
      case superseding_run_is_durable(projected, superseded_by_run_id) {
        True ->
          SupersedingRunAlreadyExists(superseded_run_id, superseded_by_run_id)
        False ->
          FreshSupersedingDispatch(
            superseded_run_id,
            workflow_id,
            reason,
            "reconciling operator fresh retry whose replacement was not durably queued",
          )
      }
    _ ->
      RejectRecovery(
        "start_fresh_not_allowed",
        "operator fresh retry target is no longer an eligible terminal failed run: "
          <> superseded_run_id,
      )
  }
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
  case superseding_run_for_existing_run(projected, run_id) {
    Ok(superseded_by_run_id) ->
      SupersedingRunAlreadyExists(run_id, superseded_by_run_id)
    Error(Nil) ->
      case projection.publication_ids_for_run(projected, run_id) {
        [] -> FreshDispatch
        _ ->
          classify_publication_recovery_attempts(projected, run_id, observation)
      }
  }
}

fn classify_publication_recovery_attempts(
  projected: projection.Projection,
  run_id: String,
  observation: CurrentWorkflowObservation,
) -> Outcome {
  case
    artifact_publication_retry.inspect_publication_recovery(projected, run_id)
  {
    Ok(artifact_publication_retry.RetryablePublicationAttempts(_)) ->
      classify_validated_publication_recovery(
        projected,
        run_id,
        observation,
        retried: True,
      )
    Ok(artifact_publication_retry.RequiredPublicationsAlreadyPublished(_)) ->
      classify_validated_publication_recovery(
        projected,
        run_id,
        observation,
        retried: False,
      )
    Error(#(publication_reason, publication_message)) ->
      RejectRecovery(publication_reason, publication_message)
  }
}

fn classify_validated_publication_recovery(
  projected: projection.Projection,
  run_id: String,
  observation: CurrentWorkflowObservation,
  retried retried: Bool,
) -> Outcome {
  case
    validate_publication_recovery_provenance(projected, run_id, observation)
  {
    Ok(workflow_id) ->
      case retried {
        True -> PublicationRecovery(run_id, workflow_id)
        False -> PublicationAlreadyPublished(run_id, workflow_id)
      }
    Error(#(reason, message)) ->
      classify_publication_recovery_validation_failure(
        projected,
        run_id,
        reason,
        message,
      )
  }
}

fn classify_publication_recovery_validation_failure(
  projected: projection.Projection,
  run_id: String,
  reason: String,
  message: String,
) -> Outcome {
  case reason {
    "publication_recovery_workflow_drift"
    | "publication_recovery_issue_drift"
    | "publication_recovery_provenance_missing" ->
      case workflow_id_for_run(projected, run_id) {
        Ok(workflow_id) ->
          FreshSupersedingDispatch(run_id, workflow_id, reason, message)
        Error(Nil) -> FreshDispatch
      }
    "publication_recovery_issue_unavailable"
    | "publication_recovery_tracker_refresh_unavailable"
    | "publication_recovery_workflow_unavailable" ->
      RequeueRecovery(reason, message)
    _ -> RejectRecovery(reason, message)
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

fn superseding_run_for_existing_run(
  projected: projection.Projection,
  run_id: String,
) -> Result(String, Nil) {
  case dict.get(projected.workflow_runs, run_id) {
    Ok(projection.WorkflowRunSuperseded(
      superseded_by_run_id: superseded_by_run_id,
      ..,
    )) ->
      case superseding_run_is_durable(projected, superseded_by_run_id) {
        True -> Ok(superseded_by_run_id)
        False -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

fn superseding_run_is_durable(
  projected: projection.Projection,
  run_id: String,
) -> Bool {
  projection.has_workflow_run(projected, run_id)
  || {
    let claim_suffix = ":" <> run_id
    projected.outbox
    |> dict.keys
    |> list.any(fn(outbox_id) {
      string.starts_with(outbox_id, "claim:")
      && string.ends_with(outbox_id, claim_suffix)
    })
  }
}

fn workflow_id_for_run(
  projected: projection.Projection,
  run_id: String,
) -> Result(String, Nil) {
  dict.get(projected.workflow_runs, run_id)
  |> result.map(workflow_run_workflow_id)
}

fn workflow_run_workflow_id(status: projection.WorkflowRunStatus) -> String {
  case status {
    projection.WorkflowRunActive(workflow_id: workflow_id, ..)
    | projection.WorkflowRunFinished(workflow_id: workflow_id, ..)
    | projection.WorkflowRunInterrupted(workflow_id: workflow_id, ..)
    | projection.WorkflowRunSuperseded(workflow_id: workflow_id, ..) ->
      workflow_id
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

pub fn publication_recovery_completion_target(
  handoff: config_types.HandoffConfig,
  workflow_id: String,
) -> Result(#(Option(String), String), String) {
  let missing =
    "publication retry completed but no success or completion state is configured"
  case handoff.completion_states {
    Some(policy) ->
      policy
      |> workflow_completion_policy.choose_linear_completion_state(
        workflow_id,
        workflow_completion_policy.WorkflowCompletionOutcome(
          workflow_completion_policy.CompletionSucceeded,
          [],
          workflow_completion_policy.ReviewUnknown,
          None,
          False,
        ),
      )
      |> publication_recovery_decision_target
    None ->
      handoff.success_state_id
      |> option.to_result(missing)
      |> result.map(linear_state_target)
  }
}

fn publication_recovery_decision_target(
  decision: workflow_completion_policy.CompletionStateDecision,
) -> Result(#(Option(String), String), String) {
  case decision {
    workflow_completion_policy.MoveToState(state, _) ->
      Ok(linear_state_target(state))
    workflow_completion_policy.LeaveLinearState(reason) ->
      Error("publication retry completed but " <> reason)
  }
}

fn linear_state_target(
  state_ref: workflow_completion_policy.LinearStateRef,
) -> #(Option(String), String) {
  case state_ref {
    workflow_completion_policy.StateById(value) -> #(Some(value), value)
    workflow_completion_policy.StateByName(value) -> #(None, value)
  }
}
