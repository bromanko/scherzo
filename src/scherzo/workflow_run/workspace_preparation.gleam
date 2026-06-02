import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/log
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_checkpoint
import scherzo/workflow_dag
import scherzo/workspace_run

pub type PreparedStart {
  PreparedStart(
    step: workflow_dag.WorkflowStep,
    workspace: workspace_run.PreparedStepWorkspace,
  )
}

pub type PreparedBatch {
  PreparedBatch(
    prepared_starts: List(PreparedStart),
    prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
    run_root: Option(String),
    attempt_indexes: Dict(String, Int),
  )
}

pub type PrepareReadyFailure {
  PrepareReadyFailure(
    reason: String,
    agent_reason: Option(error.AgentRunnerError),
    run_root: Option(String),
    prepared_starts: List(PreparedStart),
  )
}

pub type Dependencies {
  Dependencies(
    prepare_step: fn(
      tracker_issue.Issue,
      String,
      String,
      String,
      Int,
      workflow_dag.WorkspaceRef,
      config_types.OrchestratorConfig,
      config_types.WorkspaceHookProfile,
      Dict(String, workspace_run.PreparedStepWorkspace),
    ) -> Result(workspace_run.PreparedStepWorkspace, workspace_run.PrepareError),
    prepare_recovered_step: fn(
      tracker_issue.Issue,
      String,
      String,
      String,
      String,
      Int,
      workflow_dag.WorkspaceRef,
      config_types.OrchestratorConfig,
      config_types.WorkspaceHookProfile,
      Dict(String, workspace_run.PreparedStepWorkspace),
    ) -> Result(workspace_run.PreparedStepWorkspace, workspace_run.PrepareError),
    step_prepared: fn(
      String,
      String,
      String,
      workspace_run.PreparedStepWorkspace,
    ) -> Result(Nil, workflow_checkpoint.CheckpointError),
  )
}

pub type Context {
  Context(
    issue: tracker_issue.Issue,
    workflow_id: String,
    run_id: String,
    orchestrator: config_types.OrchestratorConfig,
    secrets: List(String),
    current_run_root: Option(String),
    recovered_execution: Bool,
    profile: config_types.WorkspaceHookProfile,
  )
}

pub fn prepare_ready_batch(
  ready_steps: List(workflow_dag.WorkflowStep),
  context: Context,
  dependencies: Dependencies,
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
  attempt_indexes: Dict(String, Int),
) -> Result(PreparedBatch, PrepareReadyFailure) {
  let steps =
    select_workspace_serial_batch(
      ready_steps,
      context.issue,
      context.workflow_id,
      context.run_id,
      context.orchestrator,
      attempt_indexes,
      dict.new(),
      [],
    )

  prepare_ready_steps(
    steps,
    context,
    dependencies,
    prepared_workspaces,
    attempt_indexes,
    [],
  )
}

// Workspace paths are shared per logical workspace for the whole workflow run.
// Keep each ready batch to one step per resolved workspace path so command
// execution and before_step hooks for mutable worktrees never overlap in the
// same directory, while still allowing different workspaces to run together.
fn select_workspace_serial_batch(
  steps: List(workflow_dag.WorkflowStep),
  issue: tracker_issue.Issue,
  workflow_id: String,
  run_id: String,
  orchestrator: config_types.OrchestratorConfig,
  attempt_indexes: Dict(String, Int),
  selected_locks: Dict(String, Nil),
  acc: List(workflow_dag.WorkflowStep),
) -> List(workflow_dag.WorkflowStep) {
  case steps {
    [] -> list.reverse(acc)
    [step, ..rest] -> {
      let lock =
        workspace_lock_for_step(
          step,
          issue,
          workflow_id,
          run_id,
          orchestrator,
          attempt_indexes,
        )
      case dict.get(selected_locks, lock) {
        Ok(_) ->
          select_workspace_serial_batch(
            rest,
            issue,
            workflow_id,
            run_id,
            orchestrator,
            attempt_indexes,
            selected_locks,
            acc,
          )
        Error(Nil) ->
          select_workspace_serial_batch(
            rest,
            issue,
            workflow_id,
            run_id,
            orchestrator,
            attempt_indexes,
            dict.insert(selected_locks, lock, Nil),
            [step, ..acc],
          )
      }
    }
  }
}

fn workspace_lock_for_step(
  step: workflow_dag.WorkflowStep,
  issue: tracker_issue.Issue,
  workflow_id: String,
  run_id: String,
  orchestrator: config_types.OrchestratorConfig,
  attempt_indexes: Dict(String, Int),
) -> String {
  let attempt_index = attempt_index_for_step(attempt_indexes, step.id)
  case
    workspace_run.workspace_path_for_attempt(
      issue,
      workflow_id,
      run_id,
      step.id,
      attempt_index,
      step.workspace.name,
      orchestrator,
    )
  {
    Ok(path) -> "path:" <> path
    // nolint: thrown_away_error -- path rendering failure still needs a stable serialization lock; workspace name is the safe fallback.
    Error(_) -> "name:" <> step.workspace.name
  }
}

fn prepare_ready_steps(
  steps: List(workflow_dag.WorkflowStep),
  context: Context,
  dependencies: Dependencies,
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
  attempt_indexes: Dict(String, Int),
  acc: List(PreparedStart),
) -> Result(PreparedBatch, PrepareReadyFailure) {
  case steps {
    [] -> {
      let run_root = option.or(prepared_run_root(acc), context.current_run_root)
      Ok(PreparedBatch(
        prepared_starts: list.reverse(acc),
        prepared_workspaces: prepared_workspaces,
        run_root: run_root,
        attempt_indexes: attempt_indexes,
      ))
    }
    [step, ..rest] -> {
      let attempt_index = attempt_index_for_step(attempt_indexes, step.id)
      let next_attempt_indexes =
        dict.insert(attempt_indexes, step.id, attempt_index + 1)
      case
        prepare_step_for_mode(
          dependencies,
          context,
          step.id,
          attempt_index,
          step.workspace,
          prepared_workspaces,
        )
      {
        Error(workspace_run.WorkspaceFailure(err)) ->
          Error(PrepareReadyFailure(
            "workspace_failed:" <> error.workspace_code(err),
            None,
            option.or(prepared_run_root(acc), context.current_run_root),
            list.reverse(acc),
          ))
        Error(workspace_run.HookFailure(err)) ->
          Error(PrepareReadyFailure(
            hook_failure_report(err, context.secrets),
            Some(error.WorkflowHookFailed(err)),
            option.or(prepared_run_root(acc), context.current_run_root),
            list.reverse(acc),
          ))
        Ok(prepared) -> {
          case
            dependencies.step_prepared(
              context.run_id,
              context.workflow_id,
              step.id,
              prepared,
            )
          {
            Error(checkpoint_error) -> {
              let prepared_start =
                PreparedStart(step: step, workspace: prepared)
              Error(PrepareReadyFailure(
                "checkpoint_failed:"
                  <> workflow_checkpoint.describe_error(checkpoint_error),
                None,
                prepared_run_root([prepared_start, ..acc]),
                list.reverse([prepared_start, ..acc]),
              ))
            }
            Ok(Nil) -> {
              let prepared_workspaces =
                dict.insert(prepared_workspaces, step.workspace.name, prepared)
              prepare_ready_steps(
                rest,
                context,
                dependencies,
                prepared_workspaces,
                next_attempt_indexes,
                [PreparedStart(step: step, workspace: prepared), ..acc],
              )
            }
          }
        }
      }
    }
  }
}

fn attempt_index_for_step(
  attempt_indexes: Dict(String, Int),
  step_id: String,
) -> Int {
  case dict.get(attempt_indexes, step_id) {
    Ok(attempt_index) -> attempt_index
    Error(Nil) -> 1
  }
}

fn prepare_step_for_mode(
  dependencies: Dependencies,
  context: Context,
  step_id: String,
  attempt_index: Int,
  workspace_ref: workflow_dag.WorkspaceRef,
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
) -> Result(workspace_run.PreparedStepWorkspace, workspace_run.PrepareError) {
  case context.recovered_execution, context.current_run_root {
    True, Some(expected_run_root) ->
      dependencies.prepare_recovered_step(
        context.issue,
        context.workflow_id,
        context.run_id,
        expected_run_root,
        step_id,
        attempt_index,
        workspace_ref,
        context.orchestrator,
        context.profile,
        prepared_workspaces,
      )
    _, _ ->
      dependencies.prepare_step(
        context.issue,
        context.workflow_id,
        context.run_id,
        step_id,
        attempt_index,
        workspace_ref,
        context.orchestrator,
        context.profile,
        prepared_workspaces,
      )
  }
}

fn hook_failure_report(err: error.HookError, secrets: List(String)) -> String {
  let code = "hook_failed:" <> error.hook_code(err)
  let detail = case err {
    error.HookFailed(name, status, diagnostics) -> {
      let diagnostics = string.trim(diagnostics)
      case diagnostics == "" {
        True -> code <> ":" <> name <> " exited " <> int.to_string(status)
        False ->
          code
          <> ":"
          <> name
          <> " exited "
          <> int.to_string(status)
          <> ": "
          <> diagnostics
      }
    }
    error.HookTimedOut(name) -> code <> ":" <> name <> " timed out"
    error.HookIo(message) -> code <> ":" <> message
  }
  log.redact("failure", detail, secrets)
  |> log.truncate(4000)
}

fn prepared_run_root(starts: List(PreparedStart)) -> Option(String) {
  case starts {
    [PreparedStart(workspace: workspace, ..), ..] -> Some(workspace.run_root)
    [] -> None
  }
}
