import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq, Gt, Lt}
import gleam/result
import gleam/string
import scherzo/control/command
import scherzo/path
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_dag
import scherzo/workflow_dag_compat
import scherzo/workflow_outcome

pub type RepairError {
  RepairError(reason: String, message: Option(String))
}

pub type RepairPlan {
  RepairPlan(
    run_id: String,
    issue_id: String,
    issue_identifier: String,
    selected_step_id: String,
    failed_attempt_index: Int,
    next_attempt_index: Int,
    records_to_append: List(record.RecordBody),
    candidate: recovery.WorkflowRecoveryCandidate,
  )
}

type SelectedRun {
  SelectedRun(
    run_id: String,
    workflow_id: String,
    workflow_fingerprint: String,
    issue_id: String,
    issue_identifier: String,
    issue_fingerprint: String,
    observed_updated_at_ms: Int,
    repairable_at_ms: Int,
    run_root: String,
    task_ref: record.TaskRefFields,
    recovery_evidence: workflow_outcome.RecoveryEvidence,
  )
}

type RepairBoundary {
  RepairBoundary(step_id: String, attempt_index: Int)
}

type IssueTarget {
  ByIssueId(String)
  ByIssueIdentifier(String)
}

pub fn resolve_target_run(
  projection_state: projection.Projection,
  target: command.RetryWorkflowStepTarget,
) -> Result(#(String, String, String), RepairError) {
  use run <- result.try(select_run(projection_state, target))
  Ok(#(run.run_id, run.issue_id, run.issue_identifier))
}

pub fn describe_error(error: RepairError) -> String {
  let RepairError(reason, _) = error
  reason
}

pub fn error_message(error: RepairError) -> Option(String) {
  let RepairError(_, message) = error
  message
}

pub fn normalize_observation(
  current: recovery.CurrentWorkflowObservation,
) -> recovery.CurrentWorkflowObservation {
  case current {
    recovery.CurrentWorkflow(
      issue,
      workflow_id,
      workflow_fingerprint,
      issue_fingerprint,
      dag,
      workspace_root,
    ) ->
      recovery.CurrentWorkflow(
        issue,
        workflow_id,
        workflow_fingerprint,
        issue_fingerprint,
        workflow_dag_compat.normalize(dag),
        workspace_root,
      )
    _ -> current
  }
}

pub fn plan(
  projection_state: projection.Projection,
  target: command.RetryWorkflowStepTarget,
  selected_step_id: Option(String),
  current: recovery.CurrentWorkflowObservation,
) -> Result(RepairPlan, RepairError) {
  use run <- result.try(select_run(projection_state, target))
  case require_current_workflow(current) {
    Error(error) -> Error(error)
    Ok(recovery.CurrentWorkflow(
      issue,
      current_workflow_id,
      current_workflow_fingerprint,
      current_issue_fingerprint,
      dag,
      workspace_root,
    )) -> {
      use _ <- result.try(validate_drift(
        run,
        issue,
        current_workflow_id,
        current_workflow_fingerprint,
        current_issue_fingerprint,
      ))
      use _ <- result.try(validate_run_root(
        run.run_id,
        run.run_root,
        workspace_root,
      ))
      let dag = workflow_dag_compat.normalize(dag)
      let attempts = attempts_for_run(projection_state, run.run_id)
      use failed_attempt <- result.try(select_repair_boundary(
        attempts,
        dag,
        selected_step_id,
      ))
      let excluded_steps =
        descendants_including_self(dag, failed_attempt.step_id)
      let next_attempt_index =
        projection.next_attempt_index(
          projection_state,
          run.run_id,
          failed_attempt.step_id,
        )
      let candidate_attempts =
        rewritten_attempts(
          attempts,
          excluded_steps,
          run.run_id,
          run.workflow_id,
          failed_attempt.step_id,
          failed_attempt.attempt_index,
          projection_state,
        )
      let records_to_append =
        repair_records(
          projection_state,
          target,
          selected_step_id,
          run,
          issue,
          current_issue_fingerprint,
          failed_attempt,
          next_attempt_index,
          excluded_steps,
          attempts,
        )
      Ok(RepairPlan(
        run_id: run.run_id,
        issue_id: run.issue_id,
        issue_identifier: run.issue_identifier,
        selected_step_id: failed_attempt.step_id,
        failed_attempt_index: failed_attempt.attempt_index,
        next_attempt_index: next_attempt_index,
        records_to_append: records_to_append,
        candidate: recovery.WorkflowRecoveryCandidate(
          run_id: run.run_id,
          workflow_id: run.workflow_id,
          workflow_fingerprint: workflow_fingerprint_for_candidate(
            run.workflow_fingerprint,
            current_workflow_fingerprint,
          ),
          issue_id: run.issue_id,
          issue_identifier: run.issue_identifier,
          task_ref: run.task_ref,
          issue_fingerprint: issue_fingerprint_for_candidate(
            run.issue_fingerprint,
            current_issue_fingerprint,
          ),
          observed_updated_at_ms: run.observed_updated_at_ms,
          run_root: run.run_root,
          recovery_evidence: run.recovery_evidence,
          attempts: candidate_attempts,
          contract_input_manifest: manifest_to_recovered(
            projection.workflow_input_manifest(projection_state, run.run_id),
          ),
          contract_output_manifest: None,
        ),
      ))
    }
    Ok(_) ->
      Error(RepairError("workflow_unavailable", Some("workflow is unavailable")))
  }
}

fn manifest_to_recovered(
  manifest: Option(projection.WorkflowContractManifestRef),
) -> Option(recovery.RecoveredContractManifest) {
  manifest
  |> option.map(fn(manifest) {
    recovery.RecoveredContractManifest(
      ref: manifest.artifact_ref,
      sha256: manifest.artifact_sha256,
      bytes: manifest.artifact_bytes,
    )
  })
}

fn select_run(
  projection_state: projection.Projection,
  target: command.RetryWorkflowStepTarget,
) -> Result(SelectedRun, RepairError) {
  case target {
    command.RetryWorkflowStepRunId(run_id) ->
      select_run_by_id(projection_state, run_id)
    command.RetryWorkflowStepIssueRef(issue_ref) ->
      select_latest_failed_run(projection_state, issue_target(issue_ref))
    command.RetryWorkflowStepAutoTarget(value) ->
      case projection.has_workflow_run(projection_state, value) {
        True -> select_run_by_id(projection_state, value)
        False ->
          select_latest_failed_run(projection_state, ByIssueIdentifier(value))
      }
  }
}

fn issue_target(issue_ref: command.IssueRef) -> IssueTarget {
  case issue_ref {
    command.IssueId(issue_id) -> ByIssueId(issue_id)
    command.IssueIdentifier(identifier) -> ByIssueIdentifier(identifier)
  }
}

fn select_run_by_id(
  projection_state: projection.Projection,
  run_id: String,
) -> Result(SelectedRun, RepairError) {
  case dict.get(projection_state.workflow_runs, run_id) {
    Ok(status) -> selected_run_from_status(projection_state, run_id, status)
    Error(Nil) ->
      Error(RepairError(
        "no_failed_workflow_run",
        Some("workflow run not found"),
      ))
  }
}

fn select_latest_failed_run(
  projection_state: projection.Projection,
  target: IssueTarget,
) -> Result(SelectedRun, RepairError) {
  let candidates =
    projection_state.workflow_runs
    |> dict.to_list
    |> list.fold([], fn(acc, entry) {
      let #(run_id, status) = entry
      case
        status_matches_issue_target(projection_state, run_id, status, target)
      {
        False -> acc
        True ->
          case selected_run_from_status(projection_state, run_id, status) {
            Ok(selected_run) -> [selected_run, ..acc]
            Error(_) -> acc
          }
      }
    })
    |> list.sort(by: compare_selected_runs_desc)

  case candidates {
    [] ->
      Error(RepairError(
        "no_failed_workflow_run",
        Some("no failed or interrupted workflow run found"),
      ))
    [candidate] -> Ok(candidate)
    [first, second, ..] ->
      case first.repairable_at_ms == second.repairable_at_ms {
        True ->
          Error(RepairError(
            "ambiguous_failed_run",
            Some(
              "multiple failed or interrupted workflow runs match; use a run id",
            ),
          ))
        False -> Ok(first)
      }
  }
}

fn status_matches_issue_target(
  projection_state: projection.Projection,
  run_id: String,
  status: projection.WorkflowRunStatus,
  target: IssueTarget,
) -> Bool {
  case selected_run_from_status(projection_state, run_id, status) {
    Error(_) -> False
    Ok(selected_run) ->
      case target {
        ByIssueId(target_issue_id) -> selected_run.issue_id == target_issue_id
        ByIssueIdentifier(identifier) ->
          selected_run.issue_identifier == identifier
      }
  }
}

fn selected_run_from_status(
  projection_state: projection.Projection,
  run_id: String,
  status: projection.WorkflowRunStatus,
) -> Result(SelectedRun, RepairError) {
  use provenance <- result.try(
    projection.workflow_run_provenance(projection_state, run_id)
    |> result.map_error(fn(_) {
      RepairError(
        "workspace_recovery_failed",
        Some("workflow run provenance is missing"),
      )
    }),
  )

  case status {
    projection.WorkflowRunFinished(
      outcome: outcome,
      finished_at_ms: finished_at_ms,
      ..,
    ) ->
      case workflow_outcome.is_terminal_failure(outcome) {
        False ->
          Error(RepairError(
            "no_failed_workflow_run",
            Some("workflow run is not repairable"),
          ))
        True ->
          Ok(
            selected_run_from_provenance(
              run_id,
              provenance,
              finished_at_ms,
              case outcome == workflow_outcome.failed_after_recovery {
                True -> workflow_outcome.StepRecoveryRan
                False -> workflow_outcome.NoStepRecovery
              },
            ),
          )
      }
    projection.WorkflowRunInterrupted(interrupted_at_ms: interrupted_at_ms, ..) ->
      Ok(selected_run_from_provenance(
        run_id,
        provenance,
        interrupted_at_ms,
        recovery.step_recovery_evidence_for_run(projection_state, run_id),
      ))
    _ ->
      Error(RepairError(
        "no_failed_workflow_run",
        Some("workflow run is not repairable"),
      ))
  }
}

fn selected_run_from_provenance(
  run_id: String,
  provenance: projection.WorkflowRunProvenance,
  repairable_at_ms: Int,
  recovery_evidence: workflow_outcome.RecoveryEvidence,
) -> SelectedRun {
  SelectedRun(
    run_id: run_id,
    workflow_id: provenance.workflow_id,
    workflow_fingerprint: provenance.workflow_fingerprint,
    issue_id: provenance.issue_id,
    issue_identifier: provenance.issue_identifier,
    issue_fingerprint: provenance.issue_fingerprint,
    observed_updated_at_ms: provenance.observed_updated_at_ms,
    repairable_at_ms: repairable_at_ms,
    run_root: provenance.run_root,
    task_ref: provenance.task_ref,
    recovery_evidence: recovery_evidence,
  )
}

fn compare_selected_runs_desc(a: SelectedRun, b: SelectedRun) -> Order {
  case int_compare_desc(a.repairable_at_ms, b.repairable_at_ms) {
    Eq -> string.compare(a.run_id, b.run_id)
    order -> order
  }
}

fn int_compare_desc(a: Int, b: Int) -> Order {
  case a < b {
    True -> Gt
    False ->
      case a > b {
        True -> Lt
        False -> Eq
      }
  }
}

fn require_current_workflow(
  current: recovery.CurrentWorkflowObservation,
) -> Result(recovery.CurrentWorkflowObservation, RepairError) {
  case current {
    recovery.CurrentWorkflow(..) -> Ok(current)
    recovery.IssueUnavailable ->
      Error(RepairError("issue_unavailable", Some("issue is unavailable")))
    recovery.WorkflowUnavailable(reason) ->
      Error(RepairError(
        "workflow_unavailable",
        Some("workflow is unavailable: " <> reason),
      ))
  }
}

fn validate_drift(
  run: SelectedRun,
  issue: tracker_issue.Issue,
  current_workflow_id: String,
  current_workflow_fingerprint: String,
  current_issue_fingerprint: String,
) -> Result(Nil, RepairError) {
  case run.workflow_id != current_workflow_id {
    True -> Error(RepairError("workflow_drift", Some("workflow id drifted")))
    False ->
      case
        run.workflow_fingerprint != ""
        && run.workflow_fingerprint != current_workflow_fingerprint
      {
        True ->
          Error(RepairError(
            "workflow_drift",
            Some("workflow fingerprint drifted"),
          ))
        False ->
          case
            run.issue_fingerprint != ""
            && !tracker_issue.fingerprint_equivalent(
              run.issue_fingerprint,
              current_issue_fingerprint,
            )
          {
            True ->
              Error(RepairError(
                "issue_drift",
                Some("issue fingerprint drifted"),
              ))
            False ->
              case task_ref_matches_issue(run.task_ref, issue) {
                True -> Ok(Nil)
                False ->
                  Error(RepairError(
                    "issue_drift",
                    Some("task identity drifted"),
                  ))
              }
          }
      }
  }
}

fn workflow_fingerprint_for_candidate(
  recorded: String,
  current: String,
) -> String {
  case string.trim(recorded) == "" {
    True -> current
    False -> recorded
  }
}

fn issue_fingerprint_for_candidate(
  recorded: String,
  current: String,
) -> String {
  case string.trim(recorded) == "" {
    True -> current
    False -> recorded
  }
}

fn validate_run_root(
  run_id: String,
  run_root: String,
  workspace_root: String,
) -> Result(Nil, RepairError) {
  let root_abs = path.absolute(workspace_root) |> result.unwrap(workspace_root)
  let run_root_abs = path.absolute(run_root) |> result.unwrap(run_root)
  case
    string.trim(run_root_abs) == ""
    || run_root_abs == root_abs
    || !path.contains(root_abs, run_root_abs)
  {
    True ->
      Error(RepairError(
        "workspace_recovery_failed",
        Some("invalid run root for " <> run_id),
      ))
    False -> Ok(Nil)
  }
}

fn task_ref_matches_issue(
  task_ref: record.TaskRefFields,
  issue: tracker_issue.Issue,
) -> Bool {
  task_ref.task_remote_id == issue.id
  && case task_ref.task_key {
    Some(task_key) -> task_key == issue.identifier
    None -> True
  }
}

fn attempts_for_run(
  projection_state: projection.Projection,
  run_id: String,
) -> List(projection.StepAttemptStatus) {
  projection_state.step_attempts
  |> dict.values
  |> list.fold([], fn(acc, status) {
    let #(status_run_id, _, _) = attempt_identity(status)
    case status_run_id == run_id {
      True -> [status, ..acc]
      False -> acc
    }
  })
}

fn select_repair_boundary(
  attempts: List(projection.StepAttemptStatus),
  _dag: workflow_dag.WorkflowDag,
  selected_step_id: Option(String),
) -> Result(RepairBoundary, RepairError) {
  let repairable =
    repair_boundaries(attempts)
    |> list.sort(by: compare_repair_boundaries_desc)
  case selected_step_id {
    Some(step_id) ->
      case find_repair_boundary(repairable, step_id) {
        Some(candidate) -> Ok(candidate)
        None ->
          Error(RepairError(
            "step_not_repairable",
            Some("selected step is not failed or interrupted"),
          ))
      }
    None ->
      case repairable {
        [] ->
          Error(RepairError(
            "no_failed_workflow_run",
            Some("workflow run has no failed or interrupted step"),
          ))
        [candidate] -> Ok(candidate)
        _ ->
          Error(RepairError(
            "ambiguous_repair_step",
            Some("multiple failed or interrupted steps match; use --step"),
          ))
      }
  }
}

fn repair_boundaries(
  attempts: List(projection.StepAttemptStatus),
) -> List(RepairBoundary) {
  attempts
  |> list.fold([], fn(acc, status) {
    case status {
      projection.StepAttemptFinishedStatus(
        step_id: step_id,
        attempt_index: attempt_index,
        outcome: outcome,
        ..,
      ) ->
        case workflow_outcome.is_terminal_failure(outcome) {
          True -> [RepairBoundary(step_id, attempt_index), ..acc]
          False -> acc
        }
      projection.StepAttemptInterruptedStatus(
        step_id: step_id,
        attempt_index: attempt_index,
        ..,
      ) -> [RepairBoundary(step_id, attempt_index), ..acc]
      _ -> acc
    }
  })
}

fn compare_repair_boundaries_desc(
  a: RepairBoundary,
  b: RepairBoundary,
) -> Order {
  case int_compare_desc(a.attempt_index, b.attempt_index) {
    Eq -> string.compare(a.step_id, b.step_id)
    order -> order
  }
}

fn find_repair_boundary(
  boundaries: List(RepairBoundary),
  step_id: String,
) -> Option(RepairBoundary) {
  case boundaries {
    [] -> None
    [candidate, ..rest] ->
      case candidate.step_id == step_id {
        True -> Some(candidate)
        False -> find_repair_boundary(rest, step_id)
      }
  }
}

fn descendants_including_self(
  dag: workflow_dag.WorkflowDag,
  step_id: String,
) -> List(String) {
  descendants_loop(dag.steps, [step_id], [step_id])
}

fn descendants_loop(
  steps: List(workflow_dag.WorkflowStep),
  frontier: List(String),
  seen: List(String),
) -> List(String) {
  case frontier {
    [] -> seen
    [current, ..rest] -> {
      let children =
        steps
        |> list.fold([], fn(acc, step) {
          case
            list.contains(step.depends_on, current)
            && !list.contains(seen, step.id)
          {
            True -> [step.id, ..acc]
            False -> acc
          }
        })
      descendants_loop(
        steps,
        list.append(children, rest),
        list.append(children, seen),
      )
    }
  }
}

fn rewritten_attempts(
  attempts: List(projection.StepAttemptStatus),
  excluded_steps: List(String),
  run_id: String,
  workflow_id: String,
  selected_step_id: String,
  selected_attempt_index: Int,
  projection_state: projection.Projection,
) -> List(projection.StepAttemptStatus) {
  attempts
  |> list.fold([], fn(acc, status) {
    let #(_status_run_id, status_step_id, attempt_index) =
      attempt_identity(status)
    let next = case list.contains(excluded_steps, status_step_id) {
      False -> status
      True ->
        case status {
          projection.StepAttemptSupersededStatus(..) -> status
          _ ->
            projection.StepAttemptSupersededStatus(
              run_id,
              workflow_id,
              status_step_id,
              attempt_index,
              superseded_by_attempt_index(
                status_step_id,
                attempt_index,
                selected_step_id,
                selected_attempt_index,
                projection_state,
                run_id,
              ),
              "retry_accepted",
              0,
            )
        }
    }
    [next, ..acc]
  })
  |> list.reverse
}

fn repair_records(
  projection_state: projection.Projection,
  target: command.RetryWorkflowStepTarget,
  requested_step_id: Option(String),
  run: SelectedRun,
  issue: tracker_issue.Issue,
  current_issue_fingerprint: String,
  failed_attempt: RepairBoundary,
  next_attempt_index: Int,
  excluded_steps: List(String),
  attempts: List(projection.StepAttemptStatus),
) -> List(record.RecordBody) {
  let workspace_path =
    projection.known_workspace_for_issue(projection_state, issue.id)
    |> result.unwrap(run.run_root)
  let prefix = [
    record.WorkflowRepairRequested(
      run.run_id,
      run.workflow_id,
      issue.id,
      issue.identifier,
      command.retry_workflow_step_target_to_string(target),
      requested_step_id,
      failed_attempt.step_id,
      failed_attempt.attempt_index,
      next_attempt_index,
      "retry-step",
    ),
  ]
  let middle =
    supersede_records(
      attempts,
      excluded_steps,
      run,
      failed_attempt,
      projection_state,
    )
  let suffix = [
    record.WorkflowRunStartedWithTask(
      run.run_id,
      run.workflow_id,
      run.workflow_fingerprint,
      issue.id,
      issue.identifier,
      run.task_ref,
      issue_fingerprint_for_candidate(
        run.issue_fingerprint,
        current_issue_fingerprint,
      ),
      run.observed_updated_at_ms,
      run.run_root,
    ),
    record.RunStarted(run.run_id, issue.id, issue.identifier, workspace_path),
    record.KnownWorkspace(issue.id, issue.identifier, workspace_path),
    record.IssueCounterUpdated(
      issue.id,
      issue.identifier,
      projection.latest_counter(projection_state, issue.id).failure_attempts,
      projection.latest_counter(projection_state, issue.id).worker_sessions,
      run.observed_updated_at_ms,
      Some(run.run_id),
    ),
  ]
  list.append(prefix, list.append(middle, suffix))
}

fn supersede_records(
  attempts: List(projection.StepAttemptStatus),
  excluded_steps: List(String),
  run: SelectedRun,
  failed_attempt: RepairBoundary,
  projection_state: projection.Projection,
) -> List(record.RecordBody) {
  attempts
  |> list.fold([], fn(acc, status) {
    let #(_status_run_id, step_id, attempt_index) = attempt_identity(status)
    case list.contains(excluded_steps, step_id) {
      False -> acc
      True ->
        case status {
          projection.StepAttemptSupersededStatus(..) -> acc
          _ -> [
            record.StepAttemptSuperseded(
              run.run_id,
              run.workflow_id,
              step_id,
              attempt_index,
              superseded_by_attempt_index(
                step_id,
                attempt_index,
                failed_attempt.step_id,
                failed_attempt.attempt_index,
                projection_state,
                run.run_id,
              ),
              "retry_accepted",
            ),
            ..acc
          ]
        }
    }
  })
  |> list.reverse
}

fn superseded_by_attempt_index(
  step_id: String,
  attempt_index: Int,
  selected_step_id: String,
  selected_attempt_index: Int,
  projection_state: projection.Projection,
  run_id: String,
) -> Int {
  case step_id == selected_step_id && attempt_index == selected_attempt_index {
    True -> projection.next_attempt_index(projection_state, run_id, step_id)
    False -> attempt_index + 1
  }
}

fn attempt_identity(
  status: projection.StepAttemptStatus,
) -> #(String, String, Int) {
  case status {
    projection.StepAttemptPending(
      run_id,
      _,
      step_id,
      attempt_index,
      _,
      _,
      _,
      _,
      _,
      _,
    ) -> #(run_id, step_id, attempt_index)
    projection.StepAttemptRunning(
      run_id: run_id,
      step_id: step_id,
      attempt_index: attempt_index,
      ..,
    ) -> #(run_id, step_id, attempt_index)
    projection.StepAttemptFinishedStatus(
      run_id: run_id,
      step_id: step_id,
      attempt_index: attempt_index,
      ..,
    ) -> #(run_id, step_id, attempt_index)
    projection.StepAttemptInterruptedStatus(
      run_id: run_id,
      step_id: step_id,
      attempt_index: attempt_index,
      ..,
    ) -> #(run_id, step_id, attempt_index)
    projection.StepAttemptSupersededStatus(
      run_id,
      _,
      step_id,
      attempt_index,
      _,
      _,
      _,
    ) -> #(run_id, step_id, attempt_index)
  }
}
