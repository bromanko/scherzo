import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/agent/run_attempt
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/command_step
import scherzo/domain
import scherzo/error
import scherzo/model_config
import scherzo/process_ext
import scherzo/step_artifact
import scherzo/template
import scherzo/tracker
import scherzo/workflow_dag
import scherzo/workflow_scheduler
import scherzo/workspace_run

pub type WorkflowRunSuccess {
  WorkflowRunSuccess(
    worker_success: agent_types.WorkerSuccess,
    artifacts: Dict(String, step_artifact.StepArtifact),
    run_root: String,
  )
}

pub type WorkflowRunFailure {
  WorkflowRunFailure(
    reason: String,
    artifacts: Dict(String, step_artifact.StepArtifact),
    run_root: Option(String),
  )
}

pub type Dependencies {
  Dependencies(
    prepare_step: fn(
      domain.Issue,
      String,
      String,
      String,
      workflow_dag.WorkspaceRef,
      domain.OrchestratorConfig,
      Dict(String, workspace_run.PreparedStepWorkspace),
    ) -> Result(workspace_run.PreparedStepWorkspace, workspace_run.PrepareError),
    after_step: fn(
      domain.Issue,
      String,
      workspace_run.PreparedStepWorkspace,
      domain.OrchestratorConfig,
    ) -> Nil,
    cleanup_run: fn(String, domain.OrchestratorConfig) ->
      Result(Nil, error.WorkspaceError),
    command_step: fn(
      String,
      String,
      String,
      Int,
      List(String),
      domain.ArtifactLimits,
    ) -> step_artifact.StepArtifact,
    agent_step: fn(
      domain.Issue,
      String,
      String,
      domain.EffectiveConfig,
      tracker.Client,
      String,
      fn(agent_types.PiUpdate) -> Nil,
      fn(process.Subject(worker_command.Command)) -> Nil,
    ) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
  )
}

type PreparedStart {
  PreparedStart(
    step: workflow_dag.WorkflowStep,
    workspace: workspace_run.PreparedStepWorkspace,
  )
}

type StepExecutionResult {
  StepExecutionResult(
    step_id: String,
    artifact: step_artifact.StepArtifact,
    tokens: domain.TokenTotals,
    final_issue: Option(domain.Issue),
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

type StepBatchOutcome {
  StepBatchCompleted(List(StepExecutionResult))
  StepBatchFatal(StepExecutionResult)
}

type AfterStepMessage {
  AfterStepCompleted
  AfterStepDown(process.Down)
  AfterStepLinkedExit
}

type PrepareReadyFailure {
  PrepareReadyFailure(reason: String, run_root: Option(String))
}

pub fn default_dependencies() -> Dependencies {
  Dependencies(
    prepare_step: workspace_run.prepare_step,
    after_step: workspace_run.after_step,
    cleanup_run: workspace_run.cleanup_run,
    command_step: command_step.run,
    agent_step: fn(
      issue,
      _step_id,
      prompt,
      effective,
      tracker_client,
      workspace_path,
      emit_update,
      command_ready,
    ) {
      let command_subject = process.new_subject()
      run_attempt.run_prompt_in_workspace(
        issue,
        prompt,
        effective,
        tracker_client,
        fn(_, update) { emit_update(update) },
        command_subject,
        fn() { command_ready(command_subject) },
        workspace_path,
      )
    },
  )
}

pub fn execute(
  issue: domain.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: domain.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  run_id: String,
  dependencies: Dependencies,
) -> Result(WorkflowRunSuccess, WorkflowRunFailure) {
  loop(
    issue,
    dag,
    orchestrator,
    tracker_client,
    secrets,
    run_id,
    dependencies,
    workflow_scheduler.init(dag),
    dict.new(),
    dict.new(),
    None,
    domain.zero_token_totals(),
    None,
    0,
  )
}

fn loop(
  issue: domain.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: domain.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  run_id: String,
  dependencies: Dependencies,
  scheduler_state: workflow_scheduler.SchedulerState,
  artifacts: Dict(String, step_artifact.StepArtifact),
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
  run_root: Option(String),
  tokens: domain.TokenTotals,
  final_issue: Option(domain.Issue),
  turns: Int,
) -> Result(WorkflowRunSuccess, WorkflowRunFailure) {
  case workflow_scheduler.outcome(dag, scheduler_state) {
    workflow_scheduler.WorkflowSucceeded -> {
      let result =
        step_artifact.workflow_result_artifact(
          dag,
          artifacts,
          orchestrator.artifact_limits,
        )
      let final_issue = option_unwrap(final_issue, issue)
      let workspace_path = option_unwrap(run_root, "")
      let cleanup_result =
        cleanup_if_needed(run_root, orchestrator, dependencies)
      case cleanup_result {
        Ok(Nil) ->
          Ok(WorkflowRunSuccess(
            worker_success: agent_types.WorkerSuccess(
              final_issue: Some(final_issue),
              final_classification: agent_types.FinalTerminal,
              workspace_path: workspace_path,
              tokens: tokens,
              turns: turns,
              result: result,
            ),
            artifacts: artifacts,
            run_root: workspace_path,
          ))
        Error(err) ->
          Error(WorkflowRunFailure(
            reason: "cleanup_failed:" <> error.workspace_code(err),
            artifacts: artifacts,
            run_root: run_root,
          ))
      }
    }
    workflow_scheduler.WorkflowFailed -> {
      let _ = cleanup_if_needed(run_root, orchestrator, dependencies)
      Error(WorkflowRunFailure(
        reason: "workflow_step_failed",
        artifacts: artifacts,
        run_root: run_root,
      ))
    }
    workflow_scheduler.WorkflowInProgress -> {
      let ready = workflow_scheduler.ready_steps(dag, scheduler_state)
      case ready {
        [] -> {
          let _ = cleanup_if_needed(run_root, orchestrator, dependencies)
          Error(WorkflowRunFailure(
            reason: "workflow_deadlocked",
            artifacts: artifacts,
            run_root: run_root,
          ))
        }
        steps -> {
          case
            prepare_ready_steps(
              steps,
              issue,
              dag.id,
              run_id,
              orchestrator,
              dependencies,
              prepared_workspaces,
              [],
            )
          {
            Error(PrepareReadyFailure(reason, prepared_run_root)) -> {
              let failure_run_root = option_or(prepared_run_root, run_root)
              let _ =
                cleanup_if_needed(failure_run_root, orchestrator, dependencies)
              Error(WorkflowRunFailure(
                reason: reason,
                artifacts: artifacts,
                run_root: failure_run_root,
              ))
            }
            Ok(prepared) -> {
              let #(prepared_starts, prepared_workspaces, run_root) = prepared
              let scheduler_state =
                mark_all_running(scheduler_state, prepared_starts)
              execute_prepared_steps(
                prepared_starts,
                issue,
                dag,
                orchestrator,
                tracker_client,
                secrets,
                run_id,
                dependencies,
                scheduler_state,
                artifacts,
                prepared_workspaces,
                run_root,
                tokens,
                final_issue,
                turns,
              )
            }
          }
        }
      }
    }
  }
}

fn prepare_ready_steps(
  steps: List(workflow_dag.WorkflowStep),
  issue: domain.Issue,
  workflow_id: String,
  run_id: String,
  orchestrator: domain.OrchestratorConfig,
  dependencies: Dependencies,
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
  acc: List(PreparedStart),
) -> Result(
  #(
    List(PreparedStart),
    Dict(String, workspace_run.PreparedStepWorkspace),
    Option(String),
  ),
  PrepareReadyFailure,
) {
  case steps {
    [] -> {
      let run_root = case acc {
        [PreparedStart(workspace: workspace, ..), ..] ->
          Some(workspace.run_root)
        [] -> None
      }
      Ok(#(list.reverse(acc), prepared_workspaces, run_root))
    }
    [step, ..rest] -> {
      case
        dependencies.prepare_step(
          issue,
          workflow_id,
          run_id,
          step.id,
          step.workspace,
          orchestrator,
          prepared_workspaces,
        )
      {
        Error(workspace_run.WorkspaceFailure(err)) ->
          Error(PrepareReadyFailure(
            "workspace_failed:" <> error.workspace_code(err),
            prepared_run_root(acc),
          ))
        Error(workspace_run.HookFailure(err)) ->
          Error(PrepareReadyFailure(
            "hook_failed:" <> error.hook_code(err),
            prepared_run_root(acc),
          ))
        Ok(prepared) -> {
          let prepared_workspaces =
            dict.insert(prepared_workspaces, step.workspace.name, prepared)
          prepare_ready_steps(
            rest,
            issue,
            workflow_id,
            run_id,
            orchestrator,
            dependencies,
            prepared_workspaces,
            [PreparedStart(step: step, workspace: prepared), ..acc],
          )
        }
      }
    }
  }
}

// The scheduler can return a ready batch larger than one when independent steps
// have different logical workspaces. The runner prepares the whole selected
// batch before starting any of those steps, then executes the batch concurrently
// and applies results back in DAG order so downstream prompts see deterministic
// artifact state.
fn execute_prepared_steps(
  starts: List(PreparedStart),
  issue: domain.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: domain.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  run_id: String,
  dependencies: Dependencies,
  scheduler_state: workflow_scheduler.SchedulerState,
  artifacts: Dict(String, step_artifact.StepArtifact),
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
  run_root: Option(String),
  tokens: domain.TokenTotals,
  final_issue: Option(domain.Issue),
  turns: Int,
) -> Result(WorkflowRunSuccess, WorkflowRunFailure) {
  case starts {
    [] ->
      loop(
        issue,
        dag,
        orchestrator,
        tracker_client,
        secrets,
        run_id,
        dependencies,
        scheduler_state,
        artifacts,
        prepared_workspaces,
        run_root,
        tokens,
        final_issue,
        turns,
      )
    _ -> {
      case
        run_prepared_batch(
          starts,
          issue,
          orchestrator,
          tracker_client,
          secrets,
          dependencies,
          artifacts,
        )
      {
        Error(reason) -> {
          let _ = cleanup_if_needed(run_root, orchestrator, dependencies)
          Error(WorkflowRunFailure(
            reason: reason,
            artifacts: artifacts,
            run_root: run_root,
          ))
        }
        Ok(StepBatchCompleted(results)) -> {
          let result_by_step =
            results
            |> list.map(fn(result) { #(result.step_id, result) })
            |> dict.from_list
          apply_prepared_results(
            starts,
            result_by_step,
            issue,
            dag,
            orchestrator,
            tracker_client,
            secrets,
            run_id,
            dependencies,
            scheduler_state,
            artifacts,
            prepared_workspaces,
            run_root,
            tokens,
            final_issue,
            turns,
          )
        }
        Ok(StepBatchFatal(result)) ->
          finish_fatal_batch_result(
            starts,
            result,
            issue,
            orchestrator,
            dependencies,
            artifacts,
            run_root,
          )
      }
    }
  }
}

fn run_prepared_batch(
  starts: List(PreparedStart),
  issue: domain.Issue,
  orchestrator: domain.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  dependencies: Dependencies,
  artifacts: Dict(String, step_artifact.StepArtifact),
) -> Result(StepBatchOutcome, String) {
  let was_trapping_exits = process_ext.trap_exits(True)
  let subject = process.new_subject()
  let workers =
    spawn_prepared_steps(
      starts,
      subject,
      issue,
      orchestrator,
      tracker_client,
      secrets,
      dependencies,
      artifacts,
    )
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
  let _ = process_ext.trap_exits(was_trapping_exits)
  result
}

fn spawn_prepared_steps(
  starts: List(PreparedStart),
  subject: process.Subject(StepExecutionResult),
  issue: domain.Issue,
  orchestrator: domain.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  dependencies: Dependencies,
  artifacts: Dict(String, step_artifact.StepArtifact),
) -> List(SpawnedStepWorker) {
  spawn_prepared_steps_loop(
    starts,
    subject,
    issue,
    orchestrator,
    tracker_client,
    secrets,
    dependencies,
    artifacts,
    [],
  )
}

fn spawn_prepared_steps_loop(
  starts: List(PreparedStart),
  subject: process.Subject(StepExecutionResult),
  issue: domain.Issue,
  orchestrator: domain.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  dependencies: Dependencies,
  artifacts: Dict(String, step_artifact.StepArtifact),
  acc: List(SpawnedStepWorker),
) -> List(SpawnedStepWorker) {
  case starts {
    [] -> list.reverse(acc)
    [PreparedStart(step, workspace), ..rest] -> {
      let pid =
        process.spawn(fn() {
          let #(artifact, tokens, final_issue, turns) =
            run_step(
              step,
              workspace,
              issue,
              orchestrator,
              tracker_client,
              secrets,
              dependencies,
              artifacts,
            )
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
      spawn_prepared_steps_loop(
        rest,
        subject,
        issue,
        orchestrator,
        tracker_client,
        secrets,
        dependencies,
        artifacts,
        [SpawnedStepWorker(step_id: step.id, pid: pid, monitor: monitor), ..acc],
      )
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
  case step_artifact.succeeded(result.artifact.status) {
    True -> False
    False ->
      case dict.get(failure_policies, result.step_id) {
        Ok(workflow_dag.ContinueWorkflow) -> False
        _ -> True
      }
  }
}

fn collect_step_results(
  remaining: Int,
  selector: process.Selector(StepBatchMessage),
  monitor_to_step: Dict(process.Monitor, String),
  step_to_monitor: Dict(String, process.Monitor),
  monitor_to_pid: Dict(process.Monitor, process.Pid),
  failure_policies: Dict(String, workflow_dag.FailurePolicy),
  acc: List(StepExecutionResult),
) -> Result(StepBatchOutcome, String) {
  case remaining <= 0 {
    True -> Ok(StepBatchCompleted(acc))
    False ->
      case process.selector_receive_forever(selector) {
        StepBatchResult(result) ->
          case dict.get(step_to_monitor, result.step_id) {
            Error(_) ->
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
) -> Result(StepBatchOutcome, String) {
  case down {
    process.ProcessDown(monitor, _, reason) ->
      case dict.get(monitor_to_step, monitor) {
        Error(_) ->
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
          Error(step_worker_down_reason(step_id, reason))
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

fn step_worker_down_reason(
  step_id: String,
  reason: process.ExitReason,
) -> String {
  case reason {
    process.Normal -> "step_worker_exited_without_result:" <> step_id
    process.Killed -> "step_worker_killed:" <> step_id
    process.Abnormal(_) -> "step_worker_crashed:" <> step_id
  }
}

fn finish_fatal_batch_result(
  starts: List(PreparedStart),
  result: StepExecutionResult,
  issue: domain.Issue,
  orchestrator: domain.OrchestratorConfig,
  dependencies: Dependencies,
  artifacts: Dict(String, step_artifact.StepArtifact),
  run_root: Option(String),
) -> Result(WorkflowRunSuccess, WorkflowRunFailure) {
  let artifacts = dict.insert(artifacts, result.step_id, result.artifact)
  let after_step_result = case prepared_start_by_step(starts, result.step_id) {
    Error(_) -> Ok(Nil)
    Ok(PreparedStart(step, workspace)) ->
      run_after_step(dependencies, issue, step.id, workspace, orchestrator)
  }
  let reason = case after_step_result {
    Ok(Nil) -> "workflow_step_failed"
    Error(reason) -> reason
  }
  let _ = cleanup_if_needed(run_root, orchestrator, dependencies)
  Error(WorkflowRunFailure(
    reason: reason,
    artifacts: artifacts,
    run_root: run_root,
  ))
}

fn prepared_start_by_step(
  starts: List(PreparedStart),
  step_id: String,
) -> Result(PreparedStart, Nil) {
  case starts {
    [] -> Error(Nil)
    [start, ..rest] -> {
      let PreparedStart(step: step, ..) = start
      case step.id == step_id {
        True -> Ok(start)
        False -> prepared_start_by_step(rest, step_id)
      }
    }
  }
}

fn apply_prepared_results(
  starts: List(PreparedStart),
  result_by_step: Dict(String, StepExecutionResult),
  issue: domain.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: domain.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  run_id: String,
  dependencies: Dependencies,
  scheduler_state: workflow_scheduler.SchedulerState,
  artifacts: Dict(String, step_artifact.StepArtifact),
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
  run_root: Option(String),
  tokens: domain.TokenTotals,
  final_issue: Option(domain.Issue),
  turns: Int,
) -> Result(WorkflowRunSuccess, WorkflowRunFailure) {
  case starts {
    [] ->
      loop(
        issue,
        dag,
        orchestrator,
        tracker_client,
        secrets,
        run_id,
        dependencies,
        scheduler_state,
        artifacts,
        prepared_workspaces,
        run_root,
        tokens,
        final_issue,
        turns,
      )
    [PreparedStart(step: step, workspace: workspace), ..rest] -> {
      let assert Ok(result) = dict.get(result_by_step, step.id)
      let artifacts = dict.insert(artifacts, step.id, result.artifact)
      let scheduler_state =
        workflow_scheduler.mark_finished(
          scheduler_state,
          step.id,
          result.artifact,
        )
      let tokens = add_tokens(tokens, result.tokens)
      let final_issue = case result.final_issue {
        Some(_) -> result.final_issue
        None -> final_issue
      }
      case
        run_after_step(dependencies, issue, step.id, workspace, orchestrator)
      {
        Error(reason) -> {
          let _ = cleanup_if_needed(run_root, orchestrator, dependencies)
          Error(WorkflowRunFailure(
            reason: reason,
            artifacts: artifacts,
            run_root: run_root,
          ))
        }
        Ok(Nil) ->
          apply_prepared_results(
            rest,
            result_by_step,
            issue,
            dag,
            orchestrator,
            tracker_client,
            secrets,
            run_id,
            dependencies,
            scheduler_state,
            artifacts,
            prepared_workspaces,
            run_root,
            tokens,
            final_issue,
            turns + result.turns,
          )
      }
    }
  }
}

fn run_after_step(
  dependencies: Dependencies,
  issue: domain.Issue,
  step_id: String,
  workspace: workspace_run.PreparedStepWorkspace,
  orchestrator: domain.OrchestratorConfig,
) -> Result(Nil, String) {
  let was_trapping_exits = process_ext.trap_exits(True)
  let subject = process.new_subject()
  let pid =
    process.spawn(fn() {
      dependencies.after_step(issue, step_id, workspace, orchestrator)
      process.send(subject, Nil)
    })
  let monitor = process.monitor(pid)
  let selector =
    process.new_selector()
    |> process.select_map(subject, fn(_) { AfterStepCompleted })
    |> process.select_specific_monitor(monitor, AfterStepDown)
    |> process.select_trapped_exits(fn(_) { AfterStepLinkedExit })
  let result = receive_after_step_result(selector, monitor, step_id)
  let _ = process_ext.trap_exits(was_trapping_exits)
  result
}

fn receive_after_step_result(
  selector: process.Selector(AfterStepMessage),
  monitor: process.Monitor,
  step_id: String,
) -> Result(Nil, String) {
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
) -> Result(Nil, String) {
  case down {
    process.ProcessDown(_, _, reason) ->
      Error(after_step_down_reason(step_id, reason))
    process.PortDown(_, _, _) -> Error("after_step_monitor_down:" <> step_id)
  }
}

fn after_step_down_reason(
  step_id: String,
  reason: process.ExitReason,
) -> String {
  case reason {
    process.Normal -> "after_step_exited_without_result:" <> step_id
    process.Killed -> "after_step_killed:" <> step_id
    process.Abnormal(_) -> "after_step_crashed:" <> step_id
  }
}

fn run_step(
  step: workflow_dag.WorkflowStep,
  workspace: workspace_run.PreparedStepWorkspace,
  issue: domain.Issue,
  orchestrator: domain.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  dependencies: Dependencies,
  artifacts: Dict(String, step_artifact.StepArtifact),
) -> #(
  step_artifact.StepArtifact,
  domain.TokenTotals,
  Option(domain.Issue),
  Int,
) {
  case step.kind {
    workflow_dag.CommandStep(run, timeout_ms) -> {
      let timeout_ms =
        option_unwrap(timeout_ms, orchestrator.dag_hooks.timeout_ms)
      #(
        dependencies.command_step(
          step.id,
          run,
          workspace.path,
          timeout_ms,
          secrets,
          orchestrator.artifact_limits,
        ),
        domain.zero_token_totals(),
        None,
        0,
      )
    }
    workflow_dag.AgentStep(prompt_ref) -> {
      let prompt_template = case prompt_ref {
        workflow_dag.PromptInline(prompt) -> prompt
        workflow_dag.PromptFile(path) -> path
      }
      case
        template.render_with_locals(
          prompt_template,
          issue,
          None,
          step_artifact.to_template_locals(artifacts),
        )
      {
        Error(_) -> #(
          step_artifact.from_command_result(
            step.id,
            1,
            "",
            "template render failed",
            False,
            secrets,
            orchestrator.artifact_limits,
          ),
          domain.zero_token_totals(),
          None,
          0,
        )
        Ok(prompt) -> {
          let effective = effective_for_step(orchestrator, step)
          case
            dependencies.agent_step(
              issue,
              step.id,
              prompt,
              effective,
              tracker_client,
              workspace.path,
              fn(_) { Nil },
              fn(_) { Nil },
            )
          {
            Ok(success) -> #(
              step_artifact.from_agent_success(
                step.id,
                success,
                secrets,
                orchestrator.artifact_limits,
              ),
              success.tokens,
              success.final_issue,
              success.turns,
            )
            Error(failure) -> #(
              step_artifact.from_command_result(
                step.id,
                1,
                "",
                "agent step failed:" <> error.agent_code(failure.reason),
                False,
                secrets,
                orchestrator.artifact_limits,
              ),
              failure.tokens,
              failure.final_issue,
              0,
            )
          }
        }
      }
    }
  }
}

fn effective_for_step(
  orchestrator: domain.OrchestratorConfig,
  step: workflow_dag.WorkflowStep,
) -> domain.EffectiveConfig {
  let settings =
    model_config.resolve(orchestrator.model_settings, step.model_settings)
  let command =
    model_config.apply_to_command(orchestrator.effective.pi.command, settings)
  domain.EffectiveConfig(
    ..orchestrator.effective,
    pi: domain.PiConfig(..orchestrator.effective.pi, command: command),
  )
}

fn mark_all_running(
  state: workflow_scheduler.SchedulerState,
  starts: List(PreparedStart),
) -> workflow_scheduler.SchedulerState {
  case starts {
    [] -> state
    [PreparedStart(step: step, ..), ..rest] ->
      mark_all_running(workflow_scheduler.mark_running(state, step.id), rest)
  }
}

fn cleanup_if_needed(
  run_root: Option(String),
  orchestrator: domain.OrchestratorConfig,
  dependencies: Dependencies,
) -> Result(Nil, error.WorkspaceError) {
  case run_root {
    None -> Ok(Nil)
    Some(path) -> dependencies.cleanup_run(path, orchestrator)
  }
}

fn add_tokens(
  left: domain.TokenTotals,
  right: domain.TokenTotals,
) -> domain.TokenTotals {
  domain.TokenTotals(
    input: left.input + right.input,
    output: left.output + right.output,
    cache_read: left.cache_read + right.cache_read,
    cache_write: left.cache_write + right.cache_write,
    total: left.total + right.total,
  )
}

fn option_unwrap(value: Option(a), default: a) -> a {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn option_or(value: Option(a), fallback: Option(a)) -> Option(a) {
  case value {
    Some(_) -> value
    None -> fallback
  }
}

fn prepared_run_root(starts: List(PreparedStart)) -> Option(String) {
  case starts {
    [PreparedStart(workspace: workspace, ..), ..] -> Some(workspace.run_root)
    [] -> None
  }
}
