import birl
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq, Gt, Lt}
import gleam/result
import gleam/string
import scherzo/control/command
import scherzo/path
import scherzo/retry_step_validation
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_dag
import scherzo/workflow_outcome

pub const retry_step_auto_repair_mode = "retry_step_auto"

pub const state_repair_explicit_mode = "state_repair_explicit"

pub type RepairError {
  RepairError(reason: String, message: Option(String))
}

pub type RunProvenanceRepairPlan {
  RunProvenanceRepairPlan(
    run_id: String,
    workflow_id: String,
    workflow_fingerprint: String,
    issue_id: String,
    issue_identifier: String,
    issue_fingerprint: String,
    observed_updated_at_ms: Int,
    run_root: String,
    task_ref: record.TaskRefFields,
    repair_mode: String,
    source_evidence: List(String),
    record_body: record.RecordBody,
  )
}

pub type RunProvenanceRepairInspection {
  RunProvenanceRepairAlreadyPresent(
    run_id: String,
    issue_id: String,
    issue_identifier: String,
    run_root: String,
  )
  RunProvenanceRepairRequired(plan: RunProvenanceRepairPlan)
}

pub type RepairPlan {
  RepairPlan(
    run_id: String,
    issue_id: String,
    issue_identifier: String,
    selected_step_id: String,
    failed_attempt_index: Int,
    next_attempt_index: Int,
    provenance_repair: Option(RunProvenanceRepairPlan),
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
    terminal_failed: Bool,
    provenance_repair: Option(RunProvenanceRepairPlan),
  )
}

type RepairBoundary {
  RepairBoundary(
    step_id: String,
    attempt_index: Int,
    normalization_records: List(record.RecordBody),
  )
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

pub fn inspect_run_provenance_repair(
  projection_state: projection.Projection,
  run_id: String,
  repair_mode: String,
) -> Result(RunProvenanceRepairInspection, RepairError) {
  use status <- result.try(workflow_run_status_for_repair(
    projection_state,
    run_id,
  ))
  use _ <- result.try(repairable_run_status_info(
    projection_state,
    run_id,
    status,
  ))
  case projection.workflow_run_provenance(projection_state, run_id) {
    Ok(provenance) ->
      Ok(RunProvenanceRepairAlreadyPresent(
        run_id,
        provenance.issue_id,
        provenance.issue_identifier,
        provenance.run_root,
      ))
    Error(Nil) -> {
      use repair <- result.try(reconstruct_run_provenance(
        projection_state,
        run_id,
        status,
        repair_mode,
      ))
      Ok(RunProvenanceRepairRequired(repair))
    }
  }
}

pub fn validate_run_root_for_repair(
  run_id: String,
  run_root: String,
  workspace_root: String,
) -> Result(Nil, RepairError) {
  use _ <- result.try(validate_run_root(run_id, run_root, workspace_root))
  validate_existing_run_root(run_id, run_root, workspace_root)
}

fn workflow_run_status_for_repair(
  projection_state: projection.Projection,
  run_id: String,
) -> Result(projection.WorkflowRunStatus, RepairError) {
  case dict.get(projection_state.workflow_runs, run_id) {
    Ok(status) -> Ok(status)
    Error(Nil) ->
      Error(RepairError(
        "no_failed_workflow_run",
        Some("workflow run not found"),
      ))
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
      ))
      use _ <- result.try(validate_run_root(
        run.run_id,
        run.run_root,
        workspace_root,
      ))
      let attempts = attempts_for_run(projection_state, run.run_id)
      use failed_attempt <- result.try(select_repair_boundary(
        run,
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
      let task_ref = task_ref_for_current_issue(run.task_ref, issue)
      let observed_updated_at_ms =
        observed_updated_at_ms_for_candidate(run.observed_updated_at_ms, issue)
      let issue_fingerprint =
        current_or_recorded(current_issue_fingerprint, run.issue_fingerprint)
      let workflow_fingerprint =
        retry_step_validation.recorded_or_current(
          run.workflow_fingerprint,
          current_workflow_fingerprint,
        )
      let provenance_repair =
        run.provenance_repair
        |> option.map(fn(repair) {
          repair_plan_with_current_issue(
            repair,
            issue,
            task_ref,
            workflow_fingerprint,
            issue_fingerprint,
            observed_updated_at_ms,
            retry_step_auto_repair_mode,
          )
        })
      let records_to_append =
        repair_records(
          projection_state,
          target,
          selected_step_id,
          run,
          issue,
          workflow_fingerprint,
          task_ref,
          issue_fingerprint,
          observed_updated_at_ms,
          provenance_repair,
          failed_attempt,
          next_attempt_index,
          excluded_steps,
          attempts,
        )
      Ok(RepairPlan(
        run_id: run.run_id,
        issue_id: issue.id,
        issue_identifier: issue.identifier,
        selected_step_id: failed_attempt.step_id,
        failed_attempt_index: failed_attempt.attempt_index,
        next_attempt_index: next_attempt_index,
        provenance_repair: provenance_repair,
        records_to_append: records_to_append,
        candidate: recovery.WorkflowRecoveryCandidate(
          run_id: run.run_id,
          workflow_id: run.workflow_id,
          workflow_fingerprint: workflow_fingerprint,
          issue_id: issue.id,
          issue_identifier: issue.identifier,
          task_ref: task_ref,
          issue_fingerprint: issue_fingerprint,
          observed_updated_at_ms: observed_updated_at_ms,
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
      case selected_run_from_status(projection_state, run_id, status) {
        Ok(selected_run) ->
          case selected_run_matches_issue_target(selected_run, target) {
            True -> [selected_run, ..acc]
            False -> acc
          }
        Error(RepairError(_, _)) -> acc
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

fn selected_run_matches_issue_target(
  selected_run: SelectedRun,
  target: IssueTarget,
) -> Bool {
  case target {
    ByIssueId(target_issue_id) -> selected_run.issue_id == target_issue_id
    ByIssueIdentifier(identifier) -> selected_run.issue_identifier == identifier
  }
}

type RepairableRunStatusInfo {
  RepairableRunStatusInfo(
    repairable_at_ms: Int,
    recovery_evidence: workflow_outcome.RecoveryEvidence,
    terminal_failed: Bool,
  )
}

fn selected_run_from_status(
  projection_state: projection.Projection,
  run_id: String,
  status: projection.WorkflowRunStatus,
) -> Result(SelectedRun, RepairError) {
  use info <- result.try(repairable_run_status_info(
    projection_state,
    run_id,
    status,
  ))
  case projection.workflow_run_provenance(projection_state, run_id) {
    Ok(provenance) ->
      Ok(selected_run_from_provenance(
        run_id,
        provenance,
        info.repairable_at_ms,
        info.recovery_evidence,
        info.terminal_failed,
      ))
    Error(Nil) -> {
      use repair <- result.try(reconstruct_run_provenance(
        projection_state,
        run_id,
        status,
        retry_step_auto_repair_mode,
      ))
      Ok(selected_run_from_repair(
        repair,
        info.repairable_at_ms,
        info.recovery_evidence,
        info.terminal_failed,
      ))
    }
  }
}

fn repairable_run_status_info(
  projection_state: projection.Projection,
  run_id: String,
  status: projection.WorkflowRunStatus,
) -> Result(RepairableRunStatusInfo, RepairError) {
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
          Ok(RepairableRunStatusInfo(
            finished_at_ms,
            case outcome == workflow_outcome.failed_after_recovery {
              True -> workflow_outcome.StepRecoveryRan
              False -> workflow_outcome.NoStepRecovery
            },
            True,
          ))
      }
    projection.WorkflowRunInterrupted(interrupted_at_ms: interrupted_at_ms, ..) ->
      Ok(RepairableRunStatusInfo(
        interrupted_at_ms,
        recovery.step_recovery_evidence_for_run(projection_state, run_id),
        False,
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
  terminal_failed: Bool,
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
    terminal_failed: terminal_failed,
    provenance_repair: None,
  )
}

fn selected_run_from_repair(
  repair: RunProvenanceRepairPlan,
  repairable_at_ms: Int,
  recovery_evidence: workflow_outcome.RecoveryEvidence,
  terminal_failed: Bool,
) -> SelectedRun {
  SelectedRun(
    run_id: repair.run_id,
    workflow_id: repair.workflow_id,
    workflow_fingerprint: repair.workflow_fingerprint,
    issue_id: repair.issue_id,
    issue_identifier: repair.issue_identifier,
    issue_fingerprint: repair.issue_fingerprint,
    observed_updated_at_ms: repair.observed_updated_at_ms,
    repairable_at_ms: repairable_at_ms,
    run_root: repair.run_root,
    task_ref: repair.task_ref,
    recovery_evidence: recovery_evidence,
    terminal_failed: terminal_failed,
    provenance_repair: Some(repair),
  )
}

fn reconstruct_run_provenance(
  projection_state: projection.Projection,
  run_id: String,
  status: projection.WorkflowRunStatus,
  repair_mode: String,
) -> Result(RunProvenanceRepairPlan, RepairError) {
  let attempts = attempts_for_run(projection_state, run_id)
  use workflow_id <- result.try(single_evidence_value(
    "workflow_id",
    list.append(
      status_workflow_id_evidence(run_id, status),
      list.append(
        manifest_workflow_id_evidence(projection_state, run_id),
        attempt_workflow_id_evidence(attempts),
      ),
    ),
  ))
  use workflow_fingerprint <- result.try(optional_evidence_value(
    "workflow_fingerprint",
    manifest_workflow_fingerprint_evidence(projection_state, run_id),
  ))
  use issue_id <- result.try(single_evidence_value(
    "issue_id",
    list.append(
      status_issue_id_evidence(run_id, status),
      task_ref_issue_id_evidence(projection_state, run_id),
    ),
  ))
  use issue_identifier <- result.try(optional_evidence_value(
    "issue_identifier",
    issue_identifier_evidence(projection_state, run_id, issue_id),
  ))
  use run_root <- result.try(single_evidence_value(
    "run_root",
    list.append(
      status_run_root_evidence(run_id, status),
      attempt_run_root_evidence(attempts),
    ),
  ))
  let task_ref =
    reconstructed_task_ref(projection_state, run_id, issue_id, issue_identifier)
  let issue_fingerprint =
    reconstructed_issue_fingerprint(projection_state, issue_id)
  let observed_updated_at_ms =
    reconstructed_observed_updated_at_ms(projection_state, issue_id)
  let source_evidence =
    evidence_sources([
      status_workflow_id_evidence(run_id, status),
      manifest_workflow_id_evidence(projection_state, run_id),
      manifest_workflow_fingerprint_evidence(projection_state, run_id),
      status_issue_id_evidence(run_id, status),
      task_ref_issue_id_evidence(projection_state, run_id),
      issue_identifier_evidence(projection_state, run_id, issue_id),
      status_run_root_evidence(run_id, status),
      attempt_workflow_id_evidence(attempts),
      attempt_run_root_evidence(attempts),
    ])
  case
    explicit_repair_incomplete(
      repair_mode,
      workflow_fingerprint,
      issue_identifier,
    )
  {
    Some(field) ->
      Error(RepairError(
        "workflow_provenance_incomplete",
        Some(
          "workflow run provenance is missing and fallback evidence did not include "
          <> field,
        ),
      ))
    None ->
      Ok(make_run_provenance_repair_plan(
        run_id,
        workflow_id,
        workflow_fingerprint,
        issue_id,
        issue_identifier,
        issue_fingerprint,
        observed_updated_at_ms,
        run_root,
        task_ref,
        repair_mode,
        source_evidence,
      ))
  }
}

fn make_run_provenance_repair_plan(
  run_id: String,
  workflow_id: String,
  workflow_fingerprint: String,
  issue_id: String,
  issue_identifier: String,
  issue_fingerprint: String,
  observed_updated_at_ms: Int,
  run_root: String,
  task_ref: record.TaskRefFields,
  repair_mode: String,
  source_evidence: List(String),
) -> RunProvenanceRepairPlan {
  let body =
    record.WorkflowRunProvenanceRepaired(
      run_id,
      workflow_id,
      workflow_fingerprint,
      issue_id,
      issue_identifier,
      task_ref,
      issue_fingerprint,
      observed_updated_at_ms,
      run_root,
      repair_mode,
      source_evidence,
    )
  RunProvenanceRepairPlan(
    run_id: run_id,
    workflow_id: workflow_id,
    workflow_fingerprint: workflow_fingerprint,
    issue_id: issue_id,
    issue_identifier: issue_identifier,
    issue_fingerprint: issue_fingerprint,
    observed_updated_at_ms: observed_updated_at_ms,
    run_root: run_root,
    task_ref: task_ref,
    repair_mode: repair_mode,
    source_evidence: source_evidence,
    record_body: body,
  )
}

fn repair_plan_with_current_issue(
  repair: RunProvenanceRepairPlan,
  issue: tracker_issue.Issue,
  task_ref: record.TaskRefFields,
  workflow_fingerprint: String,
  issue_fingerprint: String,
  observed_updated_at_ms: Int,
  repair_mode: String,
) -> RunProvenanceRepairPlan {
  make_run_provenance_repair_plan(
    repair.run_id,
    repair.workflow_id,
    workflow_fingerprint,
    issue.id,
    issue.identifier,
    issue_fingerprint,
    observed_updated_at_ms,
    repair.run_root,
    task_ref,
    repair_mode,
    dedupe_strings([
      "current_issue:" <> issue.identifier,
      ..repair.source_evidence
    ]),
  )
}

fn explicit_repair_incomplete(
  repair_mode: String,
  workflow_fingerprint: String,
  issue_identifier: String,
) -> Option(String) {
  case repair_mode == state_repair_explicit_mode {
    False -> None
    True ->
      case string.trim(workflow_fingerprint) == "" {
        True -> Some("workflow_fingerprint")
        False ->
          case string.trim(issue_identifier) == "" {
            True -> Some("issue_identifier")
            False -> None
          }
      }
  }
}

fn single_evidence_value(
  field: String,
  candidates: List(#(String, String)),
) -> Result(String, RepairError) {
  let candidates = non_empty_evidence(candidates)
  case candidates {
    [] ->
      Error(RepairError(
        "workflow_provenance_incomplete",
        Some(
          "workflow run provenance is missing and fallback evidence did not include "
          <> field,
        ),
      ))
    [#(value, _), ..rest] ->
      case
        list.all(rest, fn(candidate) {
          let #(candidate_value, _) = candidate
          candidate_value == value
        })
      {
        True -> Ok(value)
        False ->
          Error(RepairError(
            "workflow_provenance_ambiguous",
            Some(
              "workflow run provenance is missing and fallback evidence conflicts for "
              <> field,
            ),
          ))
      }
  }
}

fn optional_evidence_value(
  field: String,
  candidates: List(#(String, String)),
) -> Result(String, RepairError) {
  case non_empty_evidence(candidates) {
    [] -> Ok("")
    non_empty -> single_evidence_value(field, non_empty)
  }
}

fn non_empty_evidence(
  candidates: List(#(String, String)),
) -> List(#(String, String)) {
  candidates
  |> list.filter(fn(candidate) {
    let #(value, _) = candidate
    string.trim(value) != ""
  })
}

fn status_workflow_id_evidence(
  run_id: String,
  status: projection.WorkflowRunStatus,
) -> List(#(String, String)) {
  case status {
    projection.WorkflowRunFinished(workflow_id: workflow_id, ..) -> [
      #(workflow_id, "workflow_run_finished:" <> run_id),
    ]
    projection.WorkflowRunInterrupted(workflow_id: workflow_id, ..) -> [
      #(workflow_id, "workflow_run_interrupted:" <> run_id),
    ]
    projection.WorkflowRunSuperseded(workflow_id: workflow_id, ..) -> [
      #(workflow_id, "workflow_run_superseded:" <> run_id),
    ]
    projection.WorkflowRunActive(workflow_id: workflow_id, ..) -> [
      #(workflow_id, "workflow_run_active:" <> run_id),
    ]
  }
}

fn status_issue_id_evidence(
  run_id: String,
  status: projection.WorkflowRunStatus,
) -> List(#(String, String)) {
  case status {
    projection.WorkflowRunFinished(issue_id: issue_id, ..) -> [
      #(issue_id, "workflow_run_finished:" <> run_id),
    ]
    projection.WorkflowRunInterrupted(issue_id: issue_id, ..) -> [
      #(issue_id, "workflow_run_interrupted:" <> run_id),
    ]
    projection.WorkflowRunSuperseded(issue_id: issue_id, ..) -> [
      #(issue_id, "workflow_run_superseded:" <> run_id),
    ]
    projection.WorkflowRunActive(issue_id: issue_id, ..) -> [
      #(issue_id, "workflow_run_active:" <> run_id),
    ]
  }
}

fn status_run_root_evidence(
  run_id: String,
  status: projection.WorkflowRunStatus,
) -> List(#(String, String)) {
  case status {
    projection.WorkflowRunFinished(run_root: run_root, ..) -> [
      #(run_root, "workflow_run_finished:" <> run_id),
    ]
    projection.WorkflowRunInterrupted(run_root: run_root, ..) -> [
      #(run_root, "workflow_run_interrupted:" <> run_id),
    ]
    projection.WorkflowRunSuperseded(run_root: run_root, ..) -> [
      #(run_root, "workflow_run_superseded:" <> run_id),
    ]
    projection.WorkflowRunActive(run_root: run_root, ..) -> [
      #(run_root, "workflow_run_active:" <> run_id),
    ]
  }
}

fn manifest_workflow_id_evidence(
  projection_state: projection.Projection,
  run_id: String,
) -> List(#(String, String)) {
  case projection.workflow_input_manifest(projection_state, run_id) {
    Some(manifest) -> [
      #(manifest.workflow_id, "workflow_run_inputs_recorded:" <> run_id),
    ]
    None -> []
  }
}

fn manifest_workflow_fingerprint_evidence(
  projection_state: projection.Projection,
  run_id: String,
) -> List(#(String, String)) {
  case projection.workflow_input_manifest(projection_state, run_id) {
    Some(manifest) -> [
      #(
        manifest.workflow_fingerprint,
        "workflow_run_inputs_recorded:" <> run_id,
      ),
    ]
    None -> []
  }
}

fn task_ref_issue_id_evidence(
  projection_state: projection.Projection,
  run_id: String,
) -> List(#(String, String)) {
  case projection.workflow_task_ref(projection_state, run_id) {
    Ok(task_ref) -> [#(task_ref.task_remote_id, "workflow_task_ref:" <> run_id)]
    Error(Nil) -> []
  }
}

fn issue_identifier_evidence(
  projection_state: projection.Projection,
  run_id: String,
  issue_id: String,
) -> List(#(String, String)) {
  let task_key = case projection.workflow_task_ref(projection_state, run_id) {
    Ok(task_ref) ->
      case task_ref.task_key {
        Some(key) -> [#(key, "workflow_task_ref:" <> run_id)]
        None -> []
      }
    Error(Nil) -> []
  }
  let workspace = case dict.get(projection_state.known_workspaces, issue_id) {
    Ok(workspace) -> [
      #(workspace.issue_identifier, "known_workspace:" <> issue_id),
    ]
    Error(Nil) -> []
  }
  let counter = case dict.get(projection_state.issue_counters, issue_id) {
    Ok(counter) -> [
      #(counter.issue_identifier, "issue_counter_updated:" <> issue_id),
    ]
    Error(Nil) -> []
  }
  let parked = case dict.get(projection_state.parked_issues, issue_id) {
    Ok(parked) -> [#(parked.issue_identifier, "issue_parked_v2:" <> issue_id)]
    Error(Nil) -> []
  }
  list.append(task_key, list.append(workspace, list.append(counter, parked)))
}

fn attempt_workflow_id_evidence(
  attempts: List(projection.StepAttemptStatus),
) -> List(#(String, String)) {
  attempts
  |> list.fold([], fn(acc, attempt) {
    case attempt_workflow_id(attempt) {
      #(workflow_id, evidence) -> [#(workflow_id, evidence), ..acc]
    }
  })
}

fn attempt_run_root_evidence(
  attempts: List(projection.StepAttemptStatus),
) -> List(#(String, String)) {
  attempts
  |> list.fold([], fn(acc, attempt) {
    case attempt_run_root(attempt) {
      #(run_root, evidence) -> [#(run_root, evidence), ..acc]
    }
  })
}

fn attempt_workflow_id(
  attempt: projection.StepAttemptStatus,
) -> #(String, String) {
  case attempt {
    projection.StepAttemptPending(
      run_id: run_id,
      workflow_id: workflow_id,
      step_id: step_id,
      attempt_index: attempt_index,
      ..,
    ) -> #(
      workflow_id,
      "step_attempt_prepared:"
        <> run_id
        <> ":"
        <> step_id
        <> ":"
        <> int.to_string(attempt_index),
    )
    projection.StepAttemptRunning(
      run_id: run_id,
      workflow_id: workflow_id,
      step_id: step_id,
      attempt_index: attempt_index,
      ..,
    ) -> #(
      workflow_id,
      "step_attempt_started:"
        <> run_id
        <> ":"
        <> step_id
        <> ":"
        <> int.to_string(attempt_index),
    )
    projection.StepAttemptFinishedStatus(
      run_id: run_id,
      workflow_id: workflow_id,
      step_id: step_id,
      attempt_index: attempt_index,
      ..,
    ) -> #(
      workflow_id,
      "step_attempt_finished:"
        <> run_id
        <> ":"
        <> step_id
        <> ":"
        <> int.to_string(attempt_index),
    )
    projection.StepAttemptInterruptedStatus(
      run_id: run_id,
      workflow_id: workflow_id,
      step_id: step_id,
      attempt_index: attempt_index,
      ..,
    ) -> #(
      workflow_id,
      "step_attempt_interrupted:"
        <> run_id
        <> ":"
        <> step_id
        <> ":"
        <> int.to_string(attempt_index),
    )
    projection.StepAttemptSupersededStatus(
      run_id: run_id,
      workflow_id: workflow_id,
      step_id: step_id,
      attempt_index: attempt_index,
      ..,
    ) -> #(
      workflow_id,
      "step_attempt_superseded:"
        <> run_id
        <> ":"
        <> step_id
        <> ":"
        <> int.to_string(attempt_index),
    )
  }
}

fn attempt_run_root(
  attempt: projection.StepAttemptStatus,
) -> #(String, String) {
  case attempt {
    projection.StepAttemptPending(
      run_id: run_id,
      step_id: step_id,
      attempt_index: attempt_index,
      run_root: run_root,
      ..,
    ) -> #(
      run_root,
      "step_attempt_prepared:"
        <> run_id
        <> ":"
        <> step_id
        <> ":"
        <> int.to_string(attempt_index),
    )
    projection.StepAttemptRunning(
      run_id: run_id,
      step_id: step_id,
      attempt_index: attempt_index,
      run_root: run_root,
      ..,
    ) -> #(
      run_root,
      "step_attempt_started:"
        <> run_id
        <> ":"
        <> step_id
        <> ":"
        <> int.to_string(attempt_index),
    )
    projection.StepAttemptFinishedStatus(
      run_id: run_id,
      step_id: step_id,
      attempt_index: attempt_index,
      run_root: run_root,
      ..,
    ) -> #(
      run_root,
      "step_attempt_finished:"
        <> run_id
        <> ":"
        <> step_id
        <> ":"
        <> int.to_string(attempt_index),
    )
    projection.StepAttemptInterruptedStatus(
      run_id: run_id,
      step_id: step_id,
      attempt_index: attempt_index,
      run_root: run_root,
      ..,
    ) -> #(
      run_root,
      "step_attempt_interrupted:"
        <> run_id
        <> ":"
        <> step_id
        <> ":"
        <> int.to_string(attempt_index),
    )
    projection.StepAttemptSupersededStatus(
      run_id: run_id,
      step_id: step_id,
      attempt_index: attempt_index,
      ..,
    ) -> #(
      "",
      "step_attempt_superseded:"
        <> run_id
        <> ":"
        <> step_id
        <> ":"
        <> int.to_string(attempt_index),
    )
  }
}

fn reconstructed_task_ref(
  projection_state: projection.Projection,
  run_id: String,
  issue_id: String,
  issue_identifier: String,
) -> record.TaskRefFields {
  projection.workflow_task_ref(projection_state, run_id)
  |> result.unwrap(record.linear_task_ref_fields(
    issue_id,
    optional_non_empty(issue_identifier),
    None,
  ))
}

fn optional_non_empty(value: String) -> Option(String) {
  case string.trim(value) == "" {
    True -> None
    False -> Some(value)
  }
}

fn reconstructed_issue_fingerprint(
  projection_state: projection.Projection,
  issue_id: String,
) -> String {
  case dict.get(projection_state.parked_issues, issue_id) {
    Ok(parked) -> parked.issue_fingerprint
    Error(Nil) -> ""
  }
}

fn reconstructed_observed_updated_at_ms(
  projection_state: projection.Projection,
  issue_id: String,
) -> Int {
  case dict.get(projection_state.issue_counters, issue_id) {
    Ok(counter) -> counter.observed_updated_at_ms
    Error(Nil) ->
      case dict.get(projection_state.parked_issues, issue_id) {
        Ok(parked) -> parked.observed_updated_at_ms
        Error(Nil) -> 0
      }
  }
}

fn evidence_sources(groups: List(List(#(String, String)))) -> List(String) {
  groups
  |> list.fold([], fn(acc, group) { list.append(group, acc) })
  |> non_empty_evidence
  |> list.map(fn(entry) {
    let #(_, evidence) = entry
    evidence
  })
  |> dedupe_strings
}

fn dedupe_strings(values: List(String)) -> List(String) {
  values
  |> list.fold([], fn(acc, value) {
    case list.contains(acc, value) {
      True -> acc
      False -> [value, ..acc]
    }
  })
  |> list.reverse
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
    recovery.TrackerRefreshUnavailable ->
      Error(RepairError(
        "tracker_refresh_unavailable",
        Some("tracker refresh is unavailable"),
      ))
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
) -> Result(Nil, RepairError) {
  case run.issue_id != issue.id {
    True -> Error(RepairError("issue_drift", Some("issue id drifted")))
    False ->
      case
        run.issue_identifier != "" && run.issue_identifier != issue.identifier
      {
        True ->
          Error(RepairError("issue_drift", Some("issue identifier drifted")))
        False ->
          validate_fingerprints_and_task(
            run,
            issue,
            current_workflow_id,
            current_workflow_fingerprint,
          )
      }
  }
}

fn validate_fingerprints_and_task(
  run: SelectedRun,
  issue: tracker_issue.Issue,
  current_workflow_id: String,
  current_workflow_fingerprint: String,
) -> Result(Nil, RepairError) {
  let workflow_fingerprint =
    retry_step_validation.recorded_or_current(
      run.workflow_fingerprint,
      current_workflow_fingerprint,
    )
  case
    retry_step_validation.validate_workflow_identity(
      run.workflow_id,
      current_workflow_id,
      workflow_fingerprint,
      current_workflow_fingerprint,
    )
  {
    Error(failure) -> Error(RepairError(failure.reason, Some(failure.message)))
    Ok(Nil) ->
      case task_ref_matches_issue(run.task_ref, issue) {
        True -> Ok(Nil)
        False ->
          Error(RepairError("issue_drift", Some("task identity drifted")))
      }
  }
}

fn current_or_recorded(current: String, recorded: String) -> String {
  case string.trim(current) == "" {
    True -> recorded
    False -> current
  }
}

fn observed_updated_at_ms_for_candidate(
  recorded: Int,
  issue: tracker_issue.Issue,
) -> Int {
  case recorded > 0 {
    True -> recorded
    False -> observed_updated_at_ms(issue)
  }
}

fn observed_updated_at_ms(issue: tracker_issue.Issue) -> Int {
  case issue.updated_at {
    Some(time) -> birl.to_unix_milli(time)
    None -> 0
  }
}

fn task_ref_for_current_issue(
  task_ref: record.TaskRefFields,
  issue: tracker_issue.Issue,
) -> record.TaskRefFields {
  case
    task_ref.task_backend_kind == "linear"
    && task_ref.task_remote_id == issue.id
  {
    True ->
      record.TaskRefFields(
        ..task_ref,
        task_key: Some(issue.identifier),
        task_url: first_some(task_ref.task_url, issue.url),
      )
    False -> task_ref
  }
}

fn first_some(
  value: Option(String),
  fallback: Option(String),
) -> Option(String) {
  case value {
    Some(_) -> value
    None -> fallback
  }
}

fn validate_run_root(
  run_id: String,
  run_root: String,
  workspace_root: String,
) -> Result(Nil, RepairError) {
  let root_abs = path.absolute(workspace_root) |> result.unwrap(workspace_root)
  let run_root_abs = path.absolute(run_root) |> result.unwrap(run_root)
  case invalid_run_root_syntax(run_root, run_root_abs) {
    True -> invalid_run_root_error(run_id)
    False ->
      case path.realpath(root_abs), path.realpath(run_root_abs) {
        Ok(root_real), Ok(run_root_real) ->
          validate_run_root_containment(run_id, root_real, run_root_real)
        _, _ -> validate_run_root_containment(run_id, root_abs, run_root_abs)
      }
  }
}

fn validate_existing_run_root(
  run_id: String,
  run_root: String,
  workspace_root: String,
) -> Result(Nil, RepairError) {
  let root_abs = path.absolute(workspace_root) |> result.unwrap(workspace_root)
  let run_root_abs = path.absolute(run_root) |> result.unwrap(run_root)
  case invalid_run_root_syntax(run_root, run_root_abs) {
    True -> invalid_run_root_error(run_id)
    False ->
      case path.realpath(root_abs), path.realpath(run_root_abs) {
        Ok(root_real), Ok(run_root_real) ->
          validate_run_root_containment(run_id, root_real, run_root_real)
        _, _ -> invalid_run_root_error(run_id)
      }
  }
}

fn invalid_run_root_syntax(run_root: String, run_root_abs: String) -> Bool {
  string.trim(run_root) == ""
  || string.trim(run_root_abs) == ""
  || path.has_parent_segment(run_root)
  || path.has_parent_segment(run_root_abs)
  || path.contains_control_character(run_root)
}

fn validate_run_root_containment(
  run_id: String,
  root: String,
  run_root: String,
) -> Result(Nil, RepairError) {
  case run_root == root || !path.contains(root, run_root) {
    True -> invalid_run_root_error(run_id)
    False -> Ok(Nil)
  }
}

fn invalid_run_root_error(run_id: String) -> Result(Nil, RepairError) {
  Error(RepairError(
    "workspace_recovery_failed",
    Some("invalid run root for " <> run_id),
  ))
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
  run: SelectedRun,
  attempts: List(projection.StepAttemptStatus),
  dag: workflow_dag.WorkflowDag,
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
          case run.terminal_failed {
            True -> select_stale_active_boundary(attempts, dag, step_id)
            False ->
              Error(RepairError(
                "step_not_repairable",
                Some("selected step is not failed or interrupted"),
              ))
          }
      }
    None ->
      case repairable {
        [] ->
          case run.terminal_failed {
            True ->
              case stale_active_repair_boundaries(attempts, dag) {
                [] ->
                  Error(RepairError(
                    "no_failed_workflow_run",
                    Some("workflow run has no failed or interrupted step"),
                  ))
                [candidate] -> Ok(candidate)
                _ ->
                  Error(RepairError(
                    "ambiguous_repair_step",
                    Some("multiple stale active steps match; use --step"),
                  ))
              }
            False ->
              Error(RepairError(
                "no_failed_workflow_run",
                Some("workflow run has no failed or interrupted step"),
              ))
          }
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
          True -> [RepairBoundary(step_id, attempt_index, []), ..acc]
          False -> acc
        }
      projection.StepAttemptInterruptedStatus(
        step_id: step_id,
        attempt_index: attempt_index,
        ..,
      ) -> [RepairBoundary(step_id, attempt_index, []), ..acc]
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

fn stale_active_repair_boundaries(
  attempts: List(projection.StepAttemptStatus),
  dag: workflow_dag.WorkflowDag,
) -> List(RepairBoundary) {
  attempts
  |> list.fold([], fn(acc, status) {
    case stale_active_repair_boundary(status, dag) {
      Some(boundary) -> [boundary, ..acc]
      None -> acc
    }
  })
  |> list.sort(by: compare_repair_boundaries_desc)
}

fn select_stale_active_boundary(
  attempts: List(projection.StepAttemptStatus),
  dag: workflow_dag.WorkflowDag,
  selected_step_id: String,
) -> Result(RepairBoundary, RepairError) {
  case
    find_repair_boundary(
      stale_active_repair_boundaries(attempts, dag),
      selected_step_id,
    )
  {
    Some(candidate) -> Ok(candidate)
    None ->
      Error(RepairError(
        "step_not_repairable",
        Some("selected step is not safely repairable"),
      ))
  }
}

fn stale_active_repair_boundary(
  status: projection.StepAttemptStatus,
  dag: workflow_dag.WorkflowDag,
) -> Option(RepairBoundary) {
  case status {
    projection.StepAttemptPending(
      run_id,
      workflow_id,
      step_id,
      attempt_index,
      ..,
    ) ->
      stale_active_boundary_for_step(
        run_id,
        workflow_id,
        step_id,
        attempt_index,
        dag,
      )
    projection.StepAttemptRunning(
      run_id: run_id,
      workflow_id: workflow_id,
      step_id: step_id,
      attempt_index: attempt_index,
      ..,
    ) ->
      stale_active_boundary_for_step(
        run_id,
        workflow_id,
        step_id,
        attempt_index,
        dag,
      )
    _ -> None
  }
}

fn stale_active_boundary_for_step(
  run_id: String,
  workflow_id: String,
  step_id: String,
  attempt_index: Int,
  dag: workflow_dag.WorkflowDag,
) -> Option(RepairBoundary) {
  case workflow_dag.step_by_id(dag, step_id) {
    Ok(workflow_dag.WorkflowStep(kind: workflow_dag.AgentStep(..), ..)) ->
      Some(
        RepairBoundary(
          step_id: step_id,
          attempt_index: attempt_index,
          normalization_records: [
            record.StepAttemptInterrupted(
              run_id,
              workflow_id,
              step_id,
              attempt_index,
              "terminal_failure_repair_normalized",
            ),
          ],
        ),
      )
    _ -> None
  }
}

fn descendants_including_self(
  dag: workflow_dag.WorkflowDag,
  step_id: String,
) -> List(String) {
  descendants_loop(workflow_dag.steps(dag), [step_id], [step_id])
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
  workflow_fingerprint: String,
  task_ref: record.TaskRefFields,
  issue_fingerprint: String,
  observed_updated_at_ms: Int,
  provenance_repair: Option(RunProvenanceRepairPlan),
  failed_attempt: RepairBoundary,
  next_attempt_index: Int,
  excluded_steps: List(String),
  attempts: List(projection.StepAttemptStatus),
) -> List(record.RecordBody) {
  let workspace_path =
    projection.known_workspace_for_issue(projection_state, issue.id)
    |> result.unwrap(run.run_root)
  let provenance_repair_records = case provenance_repair {
    Some(repair) -> [repair.record_body]
    None -> []
  }
  let unpark_records =
    retry_step_unpark_records(projection_state, run.run_id, issue)
  let prefix =
    list.append(
      unpark_records,
      list.append(
        provenance_repair_records,
        list.append(failed_attempt.normalization_records, [
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
        ]),
      ),
    )
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
      workflow_fingerprint,
      issue.id,
      issue.identifier,
      task_ref,
      issue_fingerprint,
      observed_updated_at_ms,
      run.run_root,
    ),
    record.KnownWorkspace(issue.id, issue.identifier, workspace_path),
    record.IssueCounterUpdated(
      issue.id,
      issue.identifier,
      projection.latest_counter(projection_state, issue.id).failure_attempts,
      projection.latest_counter(projection_state, issue.id).worker_sessions,
      observed_updated_at_ms,
      Some(run.run_id),
    ),
  ]
  list.append(prefix, list.append(middle, suffix))
}

fn retry_step_unpark_records(
  projection_state: projection.Projection,
  run_id: String,
  issue: tracker_issue.Issue,
) -> List(record.RecordBody) {
  case
    retry_step_validation.parked_issue_can_retry_step(
      projection_state,
      run_id,
      issue.id,
    )
  {
    True -> [record.IssueUnparked(issue.id, issue.identifier, "retry_step")]
    False -> []
  }
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
