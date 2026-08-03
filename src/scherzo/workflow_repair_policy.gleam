import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq, Gt, Lt}
import gleam/result
import gleam/string
import scherzo/path
import scherzo/state/projection
import scherzo/state/record
import scherzo/workflow_dag
import scherzo/workflow_outcome

pub type SelectionMode {
  TotalSelection
  ExactSelection
}

pub type RepairBoundary {
  RepairBoundary(
    step_id: String,
    attempt_index: Int,
    normalization_records: List(record.RecordBody),
  )
}

pub type SelectionError {
  SelectionError(reason: String, message: Option(String))
}

pub fn select(
  mode: SelectionMode,
  attempts: List(projection.StepAttemptStatus),
  dag: workflow_dag.WorkflowDag,
  selected_step_id: Option(String),
  terminal_failed terminal_failed: Bool,
) -> Result(RepairBoundary, SelectionError) {
  let repairable =
    repair_boundaries(attempts)
    |> list.sort(by: compare_repair_boundaries_desc)
  case selected_step_id {
    Some(step_id) ->
      case find_repair_boundary(repairable, step_id) {
        Some(candidate) -> Ok(candidate)
        None ->
          select_explicit_continued_or_stale_boundary(
            mode,
            terminal_failed,
            attempts,
            dag,
            step_id,
          )
      }
    None -> select_implicit_boundary(repairable, terminal_failed, attempts, dag)
  }
}

pub fn has_repairable_boundary(
  attempts: List(projection.StepAttemptStatus),
) -> Bool {
  case repair_boundaries(attempts) {
    [] -> False
    _ -> True
  }
}

pub fn repairable_step_ids(
  attempts: List(projection.StepAttemptStatus),
  dag: workflow_dag.WorkflowDag,
  terminal_failed terminal_failed: Bool,
) -> List(String) {
  let direct = repair_boundaries(attempts)
  let stale = case terminal_failed {
    True -> stale_active_repair_boundaries(attempts, dag)
    False -> []
  }
  list.append(direct, stale)
  |> list.map(fn(boundary) { boundary.step_id })
}

pub fn validate_run_root(
  run_id: String,
  run_root: String,
  workspace_root: String,
) -> Result(Nil, SelectionError) {
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

pub fn validate_existing_run_root(
  run_id: String,
  run_root: String,
  workspace_root: String,
) -> Result(Nil, SelectionError) {
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
) -> Result(Nil, SelectionError) {
  case run_root == root || !path.contains(root, run_root) {
    True -> invalid_run_root_error(run_id)
    False -> Ok(Nil)
  }
}

fn invalid_run_root_error(run_id: String) -> Result(Nil, SelectionError) {
  Error(SelectionError(
    "workspace_recovery_failed",
    Some("invalid run root for " <> run_id),
  ))
}

fn select_implicit_boundary(
  repairable: List(RepairBoundary),
  terminal_failed: Bool,
  attempts: List(projection.StepAttemptStatus),
  dag: workflow_dag.WorkflowDag,
) -> Result(RepairBoundary, SelectionError) {
  case repairable {
    [] -> select_implicit_stale_boundary(terminal_failed, attempts, dag)
    [candidate] -> Ok(candidate)
    _ ->
      Error(SelectionError(
        "ambiguous_repair_step",
        Some("multiple failed or interrupted steps match; use --step"),
      ))
  }
}

fn select_implicit_stale_boundary(
  terminal_failed: Bool,
  attempts: List(projection.StepAttemptStatus),
  dag: workflow_dag.WorkflowDag,
) -> Result(RepairBoundary, SelectionError) {
  case terminal_failed {
    True ->
      case stale_active_repair_boundaries(attempts, dag) {
        [] -> no_failed_boundary_error()
        [candidate] -> Ok(candidate)
        _ ->
          Error(SelectionError(
            "ambiguous_repair_step",
            Some("multiple stale active steps match; use --step"),
          ))
      }
    False -> no_failed_boundary_error()
  }
}

fn no_failed_boundary_error() -> Result(RepairBoundary, SelectionError) {
  Error(SelectionError(
    "no_failed_workflow_run",
    Some("workflow run has no failed or interrupted step"),
  ))
}

fn select_explicit_continued_or_stale_boundary(
  mode: SelectionMode,
  terminal_failed: Bool,
  attempts: List(projection.StepAttemptStatus),
  dag: workflow_dag.WorkflowDag,
  step_id: String,
) -> Result(RepairBoundary, SelectionError) {
  case mode, terminal_failed {
    ExactSelection, True ->
      case exact_continued_boundary(attempts, dag, step_id) {
        Some(candidate) -> Ok(candidate)
        None -> select_stale_active_boundary(attempts, dag, step_id)
      }
    _, True -> select_stale_active_boundary(attempts, dag, step_id)
    _, False ->
      Error(SelectionError(
        "step_not_repairable",
        Some("selected step is not failed or interrupted"),
      ))
  }
}

fn exact_continued_boundary(
  attempts: List(projection.StepAttemptStatus),
  dag: workflow_dag.WorkflowDag,
  step_id: String,
) -> Option(RepairBoundary) {
  case workflow_dag.step_by_id(dag, step_id) {
    Ok(workflow_dag.WorkflowStep(
      kind: workflow_dag.AgentStep(_, Some(output)),
      on_failure: workflow_dag.ContinueWorkflow,
      ..,
    ))
      if output.required
    ->
      case
        attempts
        |> repair_boundaries_for_continued_step(step_id)
        |> list.sort(by: compare_repair_boundaries_desc)
      {
        [candidate, ..] -> Some(candidate)
        [] -> None
      }
    _ -> None
  }
}

fn repair_boundaries_for_continued_step(
  attempts: List(projection.StepAttemptStatus),
  selected_step_id: String,
) -> List(RepairBoundary) {
  attempts
  |> list.filter_map(fn(status) {
    case status {
      projection.StepAttemptFinishedStatus(
        step_id: step_id,
        attempt_index: attempt_index,
        outcome: "failed_continued",
        ..,
      )
        if step_id == selected_step_id
      -> Ok(RepairBoundary(step_id, attempt_index, []))
      _ -> Error(Nil)
    }
  })
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
  case int.compare(a.attempt_index, b.attempt_index) {
    Eq -> string.compare(a.step_id, b.step_id)
    Lt -> Gt
    Gt -> Lt
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
) -> Result(RepairBoundary, SelectionError) {
  case
    find_repair_boundary(
      stale_active_repair_boundaries(attempts, dag),
      selected_step_id,
    )
  {
    Some(candidate) -> Ok(candidate)
    None ->
      Error(SelectionError(
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
