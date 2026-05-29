import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/orchestrator/yaml_step_session
import scherzo/state/projection
import scherzo/state/record

pub type CleanupCandidate {
  CleanupCandidate(
    workflow_id: String,
    step_id: String,
    attempt_index: Int,
    session_id: Option(String),
  )
}

pub type CleanupPlan {
  CleanupPlan(
    run_id: String,
    parent_state: String,
    candidates: List(CleanupCandidate),
  )
}

pub type CleanupPlanError {
  UnknownRun
  ParentStillActive(parent_state: String)
}

pub fn plan_cleanup(
  projected: projection.Projection,
  run_id: String,
  active_session_ids: List(String),
  parent_active: Bool,
) -> Result(CleanupPlan, CleanupPlanError) {
  case workflow_parent_state(projected, run_id) {
    Error(Nil) -> Error(UnknownRun)
    Ok(parent_state) ->
      case parent_active {
        True -> Error(ParentStillActive(parent_state))
        False ->
          Ok(CleanupPlan(
            run_id: run_id,
            parent_state: parent_state,
            candidates: unfinished_candidates(
              projected,
              run_id,
              active_session_ids,
            ),
          ))
      }
  }
}

pub fn describe_cleanup(plan: CleanupPlan, dry_run dry_run: Bool) -> String {
  let candidates = case plan.candidates {
    [] -> "none"
    _ ->
      plan.candidates |> list.map(candidate_message) |> string.join(with: ", ")
  }
  let records = case plan.candidates {
    [] -> "none"
    _ ->
      plan.candidates
      |> list.map(record_message(plan.run_id, _))
      |> string.join(with: "; ")
  }
  let action = case dry_run {
    True -> "dry run for "
    False -> "cleaned orphaned YAML child steps for "
  }
  action
  <> plan.run_id
  <> " parent="
  <> plan.parent_state
  <> " candidates="
  <> candidates
  <> " records="
  <> records
}

pub fn interruption_records(
  run_id: String,
  candidates: List(CleanupCandidate),
  reason: String,
) -> List(record.RecordBody) {
  list.map(candidates, fn(candidate) {
    record.StepAttemptInterrupted(
      run_id: run_id,
      workflow_id: candidate.workflow_id,
      step_id: candidate.step_id,
      attempt_index: candidate.attempt_index,
      reason: reason,
    )
  })
}

pub fn unfinished_candidates(
  projected: projection.Projection,
  run_id: String,
  active_session_ids: List(String),
) -> List(CleanupCandidate) {
  projected.step_attempts
  |> dict.values
  |> list.fold([], fn(acc, status) {
    case status_to_candidate(status, run_id, active_session_ids) {
      Some(candidate) -> [candidate, ..acc]
      None -> acc
    }
  })
  |> list.reverse
}

pub fn workflow_parent_state(
  projected: projection.Projection,
  run_id: String,
) -> Result(String, Nil) {
  case dict.get(projected.workflow_runs, run_id) {
    Ok(status) -> Ok(workflow_run_state(status))
    Error(Nil) -> Error(Nil)
  }
}

fn workflow_run_state(status: projection.WorkflowRunStatus) -> String {
  case status {
    projection.WorkflowRunActive(..) -> "active"
    projection.WorkflowRunFinished(outcome: outcome, ..) ->
      "finished:" <> outcome
    projection.WorkflowRunInterrupted(reason: reason, ..) ->
      "interrupted:" <> reason
    projection.WorkflowRunSuperseded(reason: reason, ..) ->
      "superseded:" <> reason
  }
}

fn status_to_candidate(
  status: projection.StepAttemptStatus,
  run_id: String,
  active_session_ids: List(String),
) -> Option(CleanupCandidate) {
  case status {
    projection.StepAttemptPending(
      run_id: status_run_id,
      workflow_id: workflow_id,
      step_id: step_id,
      attempt_index: attempt_index,
      ..,
    )
    | projection.StepAttemptRunning(
        run_id: status_run_id,
        workflow_id: workflow_id,
        step_id: step_id,
        attempt_index: attempt_index,
        ..,
      ) ->
      case status_run_id == run_id {
        True ->
          Some(CleanupCandidate(
            workflow_id: workflow_id,
            step_id: step_id,
            attempt_index: attempt_index,
            session_id: candidate_session_id(
              run_id,
              step_id,
              attempt_index,
              active_session_ids,
            ),
          ))
        False -> None
      }
    projection.StepAttemptFinishedStatus(..)
    | projection.StepAttemptInterruptedStatus(..)
    | projection.StepAttemptSupersededStatus(..) -> None
  }
}

fn candidate_session_id(
  run_id: String,
  step_id: String,
  attempt_index: Int,
  active_session_ids: List(String),
) -> Option(String) {
  let session_id = yaml_step_session.id(run_id, step_id, attempt_index)
  case list.contains(active_session_ids, session_id) {
    True -> Some(session_id)
    False -> None
  }
}

fn candidate_message(candidate: CleanupCandidate) -> String {
  candidate.step_id
  <> "#"
  <> int.to_string(candidate.attempt_index)
  <> " workflow="
  <> candidate.workflow_id
  <> case candidate.session_id {
    Some(session_id) -> " session=" <> session_id
    None -> ""
  }
}

fn record_message(run_id: String, candidate: CleanupCandidate) -> String {
  "step_attempt_interrupted(run_id="
  <> run_id
  <> ", workflow_id="
  <> candidate.workflow_id
  <> ", step_id="
  <> candidate.step_id
  <> ", attempt_index="
  <> int.to_string(candidate.attempt_index)
  <> ", reason=orphaned_parent_stopped)"
}
