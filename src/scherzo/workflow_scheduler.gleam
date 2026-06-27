import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import scherzo/step_artifact
import scherzo/workflow_dag

pub type StepRuntime {
  Pending
  Running
  Succeeded
  FailedContinued
  FailedFatal
}

pub type SchedulerState {
  SchedulerState(
    statuses: Dict(String, StepRuntime),
    failure_policies: Dict(String, workflow_dag.FailurePolicy),
    cancelling: Bool,
  )
}

pub type WorkflowOutcome {
  WorkflowInProgress
  WorkflowSucceeded
  WorkflowFailed
}

pub fn init(dag: workflow_dag.WorkflowDag) -> SchedulerState {
  SchedulerState(
    statuses: fresh_statuses(dag),
    failure_policies: failure_policies(dag),
    cancelling: False,
  )
}

pub fn init_with_statuses(
  dag: workflow_dag.WorkflowDag,
  recovered: Dict(String, StepRuntime),
) -> Result(SchedulerState, String) {
  use _ <- result.try(validate_recovered_step_ids(dag, recovered))
  use _ <- result.try(validate_recovered_statuses(recovered))
  let statuses =
    recovered
    |> dict.to_list
    |> list.fold(fresh_statuses(dag), fn(statuses, entry) {
      let #(step_id, status) = entry
      dict.insert(statuses, step_id, status)
    })
  Ok(SchedulerState(
    statuses: statuses,
    failure_policies: failure_policies(dag),
    cancelling: False,
  ))
}

pub fn ready_steps(
  dag: workflow_dag.WorkflowDag,
  state: SchedulerState,
) -> List(workflow_dag.WorkflowStep) {
  case state.cancelling || has_fatal_failure(dict.values(state.statuses)) {
    True -> []
    False -> {
      let capacity =
        workflow_dag.max_parallel_steps(dag)
        - running_count(dict.values(state.statuses))
      case capacity <= 0 {
        True -> []
        False ->
          select_ready(
            workflow_dag.steps(dag),
            state,
            running_workspaces(workflow_dag.steps(dag), state, []),
            [],
            capacity,
            [],
          )
      }
    }
  }
}

pub fn mark_running(state: SchedulerState, step_id: String) -> SchedulerState {
  SchedulerState(
    ..state,
    statuses: dict.insert(state.statuses, step_id, Running),
  )
}

pub fn mark_pending(state: SchedulerState, step_id: String) -> SchedulerState {
  SchedulerState(
    ..state,
    statuses: dict.insert(state.statuses, step_id, Pending),
  )
}

pub fn mark_finished(
  state: SchedulerState,
  step_id: String,
  artifact: step_artifact.StepArtifact,
) -> SchedulerState {
  let status = case step_artifact.succeeded(artifact.status) {
    True -> Succeeded
    False ->
      case dict.get(state.failure_policies, step_id) {
        Ok(workflow_dag.ContinueWorkflow) -> FailedContinued
        _ -> FailedFatal
      }
  }
  SchedulerState(
    ..state,
    statuses: dict.insert(state.statuses, step_id, status),
  )
}

pub fn mark_cancelling(state: SchedulerState) -> SchedulerState {
  SchedulerState(..state, cancelling: True)
}

pub fn outcome(
  dag: workflow_dag.WorkflowDag,
  state: SchedulerState,
) -> WorkflowOutcome {
  case has_fatal_failure(dict.values(state.statuses)) {
    True -> WorkflowFailed
    False ->
      case all_steps_terminal(workflow_dag.steps(dag), state) {
        True -> WorkflowSucceeded
        False -> WorkflowInProgress
      }
  }
}

pub fn status_of(
  state: SchedulerState,
  step_id: String,
) -> Result(StepRuntime, Nil) {
  dict.get(state.statuses, step_id)
}

fn fresh_statuses(dag: workflow_dag.WorkflowDag) -> Dict(String, StepRuntime) {
  workflow_dag.steps(dag)
  |> list.map(fn(step) { #(step.id, Pending) })
  |> dict.from_list
}

fn failure_policies(
  dag: workflow_dag.WorkflowDag,
) -> Dict(String, workflow_dag.FailurePolicy) {
  workflow_dag.steps(dag)
  |> list.map(fn(step) { #(step.id, step.on_failure) })
  |> dict.from_list
}

fn validate_recovered_step_ids(
  dag: workflow_dag.WorkflowDag,
  recovered: Dict(String, StepRuntime),
) -> Result(Nil, String) {
  let dag_step_ids = list.map(workflow_dag.steps(dag), fn(step) { step.id })
  validate_recovered_step_ids_loop(dict.keys(recovered), dag_step_ids)
}

fn validate_recovered_step_ids_loop(
  recovered_step_ids: List(String),
  dag_step_ids: List(String),
) -> Result(Nil, String) {
  case recovered_step_ids {
    [] -> Ok(Nil)
    [step_id, ..rest] ->
      case list.contains(dag_step_ids, step_id) {
        True -> validate_recovered_step_ids_loop(rest, dag_step_ids)
        False -> Error("unknown_recovered_step:" <> step_id)
      }
  }
}

fn validate_recovered_statuses(
  recovered: Dict(String, StepRuntime),
) -> Result(Nil, String) {
  validate_recovered_statuses_loop(dict.to_list(recovered))
}

fn validate_recovered_statuses_loop(
  recovered: List(#(String, StepRuntime)),
) -> Result(Nil, String) {
  case recovered {
    [] -> Ok(Nil)
    [#(step_id, Running), ..] -> Error("running_recovered_step:" <> step_id)
    [_, ..rest] -> validate_recovered_statuses_loop(rest)
  }
}

fn select_ready(
  steps: List(workflow_dag.WorkflowStep),
  state: SchedulerState,
  blocked_workspaces: List(String),
  selected_workspaces: List(String),
  remaining_capacity: Int,
  acc: List(workflow_dag.WorkflowStep),
) -> List(workflow_dag.WorkflowStep) {
  case steps, remaining_capacity {
    [], _ -> list.reverse(acc)
    _, 0 -> list.reverse(acc)
    [step, ..rest], _ -> {
      let workspace_name = step.workspace.name
      case
        is_pending(state, step.id)
        && dependencies_complete(step.depends_on, state)
        && !list.contains(blocked_workspaces, workspace_name)
        && !list.contains(selected_workspaces, workspace_name)
      {
        True ->
          select_ready(
            rest,
            state,
            blocked_workspaces,
            [workspace_name, ..selected_workspaces],
            remaining_capacity - 1,
            [step, ..acc],
          )
        False ->
          select_ready(
            rest,
            state,
            blocked_workspaces,
            selected_workspaces,
            remaining_capacity,
            acc,
          )
      }
    }
  }
}

fn is_pending(state: SchedulerState, step_id: String) -> Bool {
  dict.get(state.statuses, step_id) == Ok(Pending)
}

fn dependencies_complete(deps: List(String), state: SchedulerState) -> Bool {
  case deps {
    [] -> True
    [dep, ..rest] ->
      is_complete_status(status_or_pending(state, dep))
      && dependencies_complete(rest, state)
  }
}

fn all_steps_terminal(
  steps: List(workflow_dag.WorkflowStep),
  state: SchedulerState,
) -> Bool {
  case steps {
    [] -> True
    [step, ..rest] ->
      is_complete_status(status_or_pending(state, step.id))
      && all_steps_terminal(rest, state)
  }
}

fn status_or_pending(state: SchedulerState, step_id: String) -> StepRuntime {
  case dict.get(state.statuses, step_id) {
    Ok(status) -> status
    Error(_) -> Pending
  }
}

fn is_complete_status(status: StepRuntime) -> Bool {
  case status {
    Succeeded | FailedContinued -> True
    _ -> False
  }
}

fn running_workspaces(
  steps: List(workflow_dag.WorkflowStep),
  state: SchedulerState,
  acc: List(String),
) -> List(String) {
  case steps {
    [] -> acc
    [step, ..rest] ->
      case status_or_pending(state, step.id) {
        Running -> running_workspaces(rest, state, [step.workspace.name, ..acc])
        _ -> running_workspaces(rest, state, acc)
      }
  }
}

fn running_count(statuses: List(StepRuntime)) -> Int {
  case statuses {
    [] -> 0
    [Running, ..rest] -> 1 + running_count(rest)
    [_, ..rest] -> running_count(rest)
  }
}

fn has_fatal_failure(statuses: List(StepRuntime)) -> Bool {
  case statuses {
    [] -> False
    [FailedFatal, ..] -> True
    [_, ..rest] -> has_fatal_failure(rest)
  }
}
