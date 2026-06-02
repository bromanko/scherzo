import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, Some}
import gleam/result
import scherzo/process_ext
import scherzo/session/tokens as session_tokens
import scherzo/step_artifact
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_attempt
import scherzo/workflow_dag
import scherzo/workspace_run

pub opaque type PreparedStart {
  PreparedStart(
    step: workflow_dag.WorkflowStep,
    workspace: workspace_run.PreparedStepWorkspace,
  )
}

pub opaque type StepExecutionResult {
  StepExecutionResult(
    step_id: String,
    artifact: step_artifact.StepArtifact,
    tokens: session_tokens.TokenTotals,
    final_issue: Option(tracker_issue.Issue),
    turns: Int,
  )
}

type SpawnedStepWorker {
  SpawnedStepWorker(step_id: String, pid: process.Pid, monitor: process.Monitor)
}

type StepBatchMessage {
  StepBatchResult(StepExecutionResult)
  StepBatchDown(process.Down)
  StepBatchLinkedExit
}

pub opaque type StepBatchOutcome {
  StepBatchCompleted(List(StepExecutionResult))
  StepBatchFatal(StepExecutionResult)
}

pub opaque type StepBatchError {
  StepBatchStartFailed(reason: String, cleanup_allowed: Bool)
  StepBatchWorkerFailed(reason: String)
}

type StepBatchFailure {
  StepBatchFailure(reason: String)
}

pub opaque type AfterStepError {
  AfterStepExitedWithoutResult(step_id: String)
  AfterStepKilled(step_id: String)
  AfterStepCrashed(step_id: String)
  AfterStepMonitorDown(step_id: String)
}

type AfterStepMessage {
  AfterStepCompleted
  AfterStepDown(process.Down)
  AfterStepLinkedExit
}

pub fn prepared_start(
  step: workflow_dag.WorkflowStep,
  workspace: workspace_run.PreparedStepWorkspace,
) -> PreparedStart {
  PreparedStart(step: step, workspace: workspace)
}

pub fn prepared_start_step(start: PreparedStart) -> workflow_dag.WorkflowStep {
  start.step
}

pub fn prepared_start_workspace(
  start: PreparedStart,
) -> workspace_run.PreparedStepWorkspace {
  start.workspace
}

pub fn prepared_start_step_id(start: PreparedStart) -> String {
  start.step.id
}

pub fn prepared_start_run_root(start: PreparedStart) -> String {
  start.workspace.run_root
}

pub fn step_result_step_id(result: StepExecutionResult) -> String {
  result.step_id
}

pub fn step_result_artifact(
  result: StepExecutionResult,
) -> step_artifact.StepArtifact {
  result.artifact
}

pub fn step_result_tokens(
  result: StepExecutionResult,
) -> session_tokens.TokenTotals {
  result.tokens
}

pub fn step_result_final_issue(
  result: StepExecutionResult,
) -> Option(tracker_issue.Issue) {
  result.final_issue
}

pub fn step_result_turns(result: StepExecutionResult) -> Int {
  result.turns
}

pub fn fold_step_batch_outcome(
  outcome: StepBatchOutcome,
  on_completed: fn(List(StepExecutionResult)) -> return_value,
  on_fatal: fn(StepExecutionResult) -> return_value,
) -> return_value {
  case outcome {
    StepBatchCompleted(results) -> on_completed(results)
    StepBatchFatal(fatal_result) -> on_fatal(fatal_result)
  }
}

pub fn run_prepared_batch(
  starts: List(PreparedStart),
  start_step: fn(workflow_dag.WorkflowStep, workspace_run.PreparedStepWorkspace) ->
    Result(Nil, String),
  run_step: fn(workflow_dag.WorkflowStep, workspace_run.PreparedStepWorkspace) ->
    #(
      step_artifact.StepArtifact,
      session_tokens.TokenTotals,
      Option(tracker_issue.Issue),
      Int,
    ),
) -> Result(StepBatchOutcome, StepBatchError) {
  let was_trapping_exits = process_ext.trap_exits(True)
  let subject = process.new_subject()
  let spawned = spawn_prepared_steps(starts, subject, start_step, run_step)
  case spawned {
    Error(error) -> {
      let _previous_trap_exits = process_ext.trap_exits(was_trapping_exits)
      Error(error)
    }
    Ok(workers) -> {
      let selector =
        process.new_selector()
        |> process.select_map(subject, StepBatchResult)
        |> process.select_monitors(StepBatchDown)
        |> process.select_trapped_exits(fn(_) { StepBatchLinkedExit })
      let result =
        collect_step_results(
          count_prepared(starts),
          selector,
          monitor_to_step(workers, dict.new()),
          step_to_monitor(workers, dict.new()),
          monitor_to_pid(workers, dict.new()),
          failure_policy_by_step(starts, dict.new()),
          [],
        )
        |> result.map_error(fn(failure) {
          StepBatchWorkerFailed(describe_step_batch_failure(failure))
        })
      let _previous_trap_exits = process_ext.trap_exits(was_trapping_exits)
      result
    }
  }
}

fn spawn_prepared_steps(
  starts: List(PreparedStart),
  subject: process.Subject(StepExecutionResult),
  start_step: fn(workflow_dag.WorkflowStep, workspace_run.PreparedStepWorkspace) ->
    Result(Nil, String),
  run_step: fn(workflow_dag.WorkflowStep, workspace_run.PreparedStepWorkspace) ->
    #(
      step_artifact.StepArtifact,
      session_tokens.TokenTotals,
      Option(tracker_issue.Issue),
      Int,
    ),
) -> Result(List(SpawnedStepWorker), StepBatchError) {
  spawn_prepared_steps_loop(starts, subject, start_step, run_step, [])
}

fn spawn_prepared_steps_loop(
  starts: List(PreparedStart),
  subject: process.Subject(StepExecutionResult),
  start_step: fn(workflow_dag.WorkflowStep, workspace_run.PreparedStepWorkspace) ->
    Result(Nil, String),
  run_step: fn(workflow_dag.WorkflowStep, workspace_run.PreparedStepWorkspace) ->
    #(
      step_artifact.StepArtifact,
      session_tokens.TokenTotals,
      Option(tracker_issue.Issue),
      Int,
    ),
  acc: List(SpawnedStepWorker),
) -> Result(List(SpawnedStepWorker), StepBatchError) {
  case starts {
    [] -> Ok(list.reverse(acc))
    [PreparedStart(step, workspace), ..rest] -> {
      case start_step(step, workspace) {
        Error(reason) -> {
          terminate_step_workers(monitor_to_pid(acc, dict.new()))
          Error(StepBatchStartFailed(reason: reason, cleanup_allowed: acc != []))
        }
        Ok(Nil) -> {
          let pid =
            process.spawn(fn() {
              let #(artifact, tokens, final_issue, turns) =
                run_step(step, workspace)
              process.send(
                subject,
                StepExecutionResult(
                  step_id: step.id,
                  artifact: artifact,
                  tokens: tokens,
                  final_issue: final_issue,
                  turns: turns,
                ),
              )
            })
          let monitor = process.monitor(pid)
          spawn_prepared_steps_loop(rest, subject, start_step, run_step, [
            SpawnedStepWorker(step_id: step.id, pid: pid, monitor: monitor),
            ..acc
          ])
        }
      }
    }
  }
}

fn count_prepared(starts: List(PreparedStart)) -> Int {
  case starts {
    [] -> 0
    [_, ..rest] -> 1 + count_prepared(rest)
  }
}

fn monitor_to_step(
  workers: List(SpawnedStepWorker),
  acc: Dict(process.Monitor, String),
) -> Dict(process.Monitor, String) {
  case workers {
    [] -> acc
    [SpawnedStepWorker(step_id: step_id, monitor: monitor, ..), ..rest] ->
      monitor_to_step(rest, dict.insert(acc, monitor, step_id))
  }
}

fn step_to_monitor(
  workers: List(SpawnedStepWorker),
  acc: Dict(String, process.Monitor),
) -> Dict(String, process.Monitor) {
  case workers {
    [] -> acc
    [SpawnedStepWorker(step_id: step_id, monitor: monitor, ..), ..rest] ->
      step_to_monitor(rest, dict.insert(acc, step_id, monitor))
  }
}

fn monitor_to_pid(
  workers: List(SpawnedStepWorker),
  acc: Dict(process.Monitor, process.Pid),
) -> Dict(process.Monitor, process.Pid) {
  case workers {
    [] -> acc
    [SpawnedStepWorker(pid: pid, monitor: monitor, ..), ..rest] ->
      monitor_to_pid(rest, dict.insert(acc, monitor, pid))
  }
}

fn failure_policy_by_step(
  starts: List(PreparedStart),
  acc: Dict(String, workflow_dag.FailurePolicy),
) -> Dict(String, workflow_dag.FailurePolicy) {
  case starts {
    [] -> acc
    [PreparedStart(step: step, ..), ..rest] ->
      failure_policy_by_step(rest, dict.insert(acc, step.id, step.on_failure))
  }
}

fn is_fatal_result(
  result: StepExecutionResult,
  failure_policies: Dict(String, workflow_dag.FailurePolicy),
) -> Bool {
  case is_recovery_resume_validation_artifact(result.artifact) {
    True -> True
    False ->
      case step_artifact.succeeded(result.artifact.status) {
        True -> False
        False ->
          case dict.get(failure_policies, result.step_id) {
            Ok(workflow_dag.ContinueWorkflow) -> False
            _ -> True
          }
      }
  }
}

fn is_recovery_resume_validation_artifact(
  artifact: step_artifact.StepArtifact,
) -> Bool {
  artifact.failure_code
  == Some(workflow_attempt.recovery_pi_resume_validation_failed)
}

fn collect_step_results(
  remaining: Int,
  selector: process.Selector(StepBatchMessage),
  monitor_to_step: Dict(process.Monitor, String),
  step_to_monitor: Dict(String, process.Monitor),
  monitor_to_pid: Dict(process.Monitor, process.Pid),
  failure_policies: Dict(String, workflow_dag.FailurePolicy),
  acc: List(StepExecutionResult),
) -> Result(StepBatchOutcome, StepBatchFailure) {
  case remaining <= 0 {
    True -> Ok(StepBatchCompleted(acc))
    False ->
      case process.selector_receive_forever(selector) {
        StepBatchResult(result) ->
          case dict.get(step_to_monitor, result.step_id) {
            Error(Nil) ->
              collect_step_results(
                remaining,
                selector,
                monitor_to_step,
                step_to_monitor,
                monitor_to_pid,
                failure_policies,
                acc,
              )
            Ok(monitor) -> {
              process.demonitor_process(monitor)
              let monitor_to_step = dict.delete(monitor_to_step, monitor)
              let step_to_monitor = dict.delete(step_to_monitor, result.step_id)
              let monitor_to_pid = dict.delete(monitor_to_pid, monitor)
              case is_fatal_result(result, failure_policies) {
                True -> {
                  terminate_step_workers(monitor_to_pid)
                  Ok(StepBatchFatal(result))
                }
                False ->
                  collect_step_results(
                    remaining - 1,
                    selector,
                    monitor_to_step,
                    step_to_monitor,
                    monitor_to_pid,
                    failure_policies,
                    [result, ..acc],
                  )
              }
            }
          }
        StepBatchDown(down) ->
          handle_step_worker_down(
            down,
            selector,
            remaining,
            monitor_to_step,
            step_to_monitor,
            monitor_to_pid,
            failure_policies,
            acc,
          )
        StepBatchLinkedExit ->
          collect_step_results(
            remaining,
            selector,
            monitor_to_step,
            step_to_monitor,
            monitor_to_pid,
            failure_policies,
            acc,
          )
      }
  }
}

fn handle_step_worker_down(
  down: process.Down,
  selector: process.Selector(StepBatchMessage),
  remaining: Int,
  monitor_to_step: Dict(process.Monitor, String),
  step_to_monitor: Dict(String, process.Monitor),
  monitor_to_pid: Dict(process.Monitor, process.Pid),
  failure_policies: Dict(String, workflow_dag.FailurePolicy),
  acc: List(StepExecutionResult),
) -> Result(StepBatchOutcome, StepBatchFailure) {
  case down {
    process.ProcessDown(monitor, _, reason) ->
      case dict.get(monitor_to_step, monitor) {
        Error(Nil) ->
          collect_step_results(
            remaining,
            selector,
            monitor_to_step,
            step_to_monitor,
            monitor_to_pid,
            failure_policies,
            acc,
          )
        Ok(step_id) -> {
          terminate_step_workers(monitor_to_pid)
          Error(step_worker_down_failure(step_id, reason))
        }
      }
    process.PortDown(_, _, _) ->
      collect_step_results(
        remaining,
        selector,
        monitor_to_step,
        step_to_monitor,
        monitor_to_pid,
        failure_policies,
        acc,
      )
  }
}

fn terminate_step_workers(
  monitor_to_pid: Dict(process.Monitor, process.Pid),
) -> Nil {
  kill_pids(dict.values(monitor_to_pid))
  demonitor_all(dict.keys(monitor_to_pid))
}

fn kill_pids(pids: List(process.Pid)) -> Nil {
  case pids {
    [] -> Nil
    [pid, ..rest] -> {
      process.unlink(pid)
      process.kill(pid)
      kill_pids(rest)
    }
  }
}

fn demonitor_all(monitors: List(process.Monitor)) -> Nil {
  case monitors {
    [] -> Nil
    [monitor, ..rest] -> {
      process.demonitor_process(monitor)
      demonitor_all(rest)
    }
  }
}

pub fn describe_step_batch_error(error: StepBatchError) -> String {
  case error {
    StepBatchStartFailed(reason: reason, ..) -> reason
    StepBatchWorkerFailed(reason: reason) -> reason
  }
}

pub fn step_batch_error_cleanup_allowed(error: StepBatchError) -> Bool {
  case error {
    StepBatchStartFailed(cleanup_allowed: cleanup_allowed, ..) ->
      cleanup_allowed
    StepBatchWorkerFailed(..) -> True
  }
}

fn describe_step_batch_failure(failure: StepBatchFailure) -> String {
  case failure {
    StepBatchFailure(reason: reason) -> reason
  }
}

fn step_worker_down_failure(
  step_id: String,
  reason: process.ExitReason,
) -> StepBatchFailure {
  case reason {
    process.Normal ->
      StepBatchFailure(reason: "step_worker_exited_without_result:" <> step_id)
    process.Killed -> StepBatchFailure(reason: "step_worker_killed:" <> step_id)
    process.Abnormal(_) ->
      StepBatchFailure(reason: "step_worker_crashed:" <> step_id)
  }
}

pub fn run_after_step(
  step_id: String,
  after_step: fn() -> Nil,
) -> Result(Nil, AfterStepError) {
  let was_trapping_exits = process_ext.trap_exits(True)
  let subject = process.new_subject()
  let pid =
    process.spawn(fn() {
      after_step()
      process.send(subject, Nil)
    })
  let monitor = process.monitor(pid)
  let selector =
    process.new_selector()
    |> process.select_map(subject, fn(_) { AfterStepCompleted })
    |> process.select_specific_monitor(monitor, AfterStepDown)
    |> process.select_trapped_exits(fn(_) { AfterStepLinkedExit })
  let result = receive_after_step_result(selector, monitor, step_id)
  let _previous_trap_exits = process_ext.trap_exits(was_trapping_exits)
  result
}

fn receive_after_step_result(
  selector: process.Selector(AfterStepMessage),
  monitor: process.Monitor,
  step_id: String,
) -> Result(Nil, AfterStepError) {
  case process.selector_receive_forever(selector) {
    AfterStepCompleted -> {
      process.demonitor_process(monitor)
      Ok(Nil)
    }
    AfterStepDown(down) -> after_step_down_result(step_id, down)
    AfterStepLinkedExit -> receive_after_step_result(selector, monitor, step_id)
  }
}

fn after_step_down_result(
  step_id: String,
  down: process.Down,
) -> Result(Nil, AfterStepError) {
  case down {
    process.ProcessDown(_, _, reason) ->
      Error(after_step_down_error(step_id, reason))
    process.PortDown(_, _, _) -> Error(AfterStepMonitorDown(step_id))
  }
}

fn after_step_down_error(
  step_id: String,
  reason: process.ExitReason,
) -> AfterStepError {
  case reason {
    process.Normal -> AfterStepExitedWithoutResult(step_id)
    process.Killed -> AfterStepKilled(step_id)
    process.Abnormal(_) -> AfterStepCrashed(step_id)
  }
}

pub fn describe_after_step_error(error: AfterStepError) -> String {
  case error {
    AfterStepExitedWithoutResult(step_id) ->
      "after_step_exited_without_result:" <> step_id
    AfterStepKilled(step_id) -> "after_step_killed:" <> step_id
    AfterStepCrashed(step_id) -> "after_step_crashed:" <> step_id
    AfterStepMonitorDown(step_id) -> "after_step_monitor_down:" <> step_id
  }
}
