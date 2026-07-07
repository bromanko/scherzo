import gleam/erlang/process
import gleam/int
import gleam/option.{type Option, None, Some}
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/config/types as config_types
import scherzo/error
import scherzo/model_config
import scherzo/session/tokens as session_tokens
import scherzo/step_artifact
import scherzo/structured_output_tool_spec
import scherzo/tracker
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_attempt
import scherzo/workflow_checkpoint
import scherzo/workflow_dag
import scherzo/workflow_identity
import scherzo/workflow_outcome
import scherzo/workflow_recovery_checkpoint_guard
import scherzo/workflow_step_recovery
import scherzo/workspace_run

pub opaque type Dependencies(context) {
  Dependencies(
    checkpoint: workflow_checkpoint.Writer,
    agent_step: fn(
      tracker_issue.Issue,
      context,
      workflow_attempt.AgentPromptMode,
      workflow_attempt.StepAttemptContext,
      config_types.EffectiveConfig,
      tracker.Client,
      fn(agent_types.RunnerUpdate) -> Nil,
      fn(process.Subject(worker_command.Command)) -> Nil,
      fn(workflow_attempt.PiSessionObservation) -> Nil,
    ) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
    make_context: fn() ->
      Result(context, structured_output_tool_spec.ToolSpecError),
    make_attempt_context: fn(context, workflow_attempt.AgentPromptMode) ->
      workflow_attempt.StepAttemptContext,
  )
}

pub fn dependencies(
  checkpoint checkpoint: workflow_checkpoint.Writer,
  agent_step agent_step: fn(
    tracker_issue.Issue,
    context,
    workflow_attempt.AgentPromptMode,
    workflow_attempt.StepAttemptContext,
    config_types.EffectiveConfig,
    tracker.Client,
    fn(agent_types.RunnerUpdate) -> Nil,
    fn(process.Subject(worker_command.Command)) -> Nil,
    fn(workflow_attempt.PiSessionObservation) -> Nil,
  ) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
  make_context make_context: fn() ->
    Result(context, structured_output_tool_spec.ToolSpecError),
  make_attempt_context make_attempt_context: fn(
    context,
    workflow_attempt.AgentPromptMode,
  ) -> workflow_attempt.StepAttemptContext,
) -> Dependencies(context) {
  Dependencies(
    checkpoint: checkpoint,
    agent_step: agent_step,
    make_context: make_context,
    make_attempt_context: make_attempt_context,
  )
}

pub type AttemptOutcome {
  RecoveryRecheckRequested(
    tokens: session_tokens.TokenTotals,
    final_issue: Option(tracker_issue.Issue),
    turns: Int,
  )
  RecoveryStop(
    tokens: session_tokens.TokenTotals,
    final_issue: Option(tracker_issue.Issue),
    turns: Int,
    recovery_evidence: workflow_outcome.RecoveryEvidence,
  )
}

pub fn combine_evidence(
  current: workflow_outcome.RecoveryEvidence,
  next: workflow_outcome.RecoveryEvidence,
) -> workflow_outcome.RecoveryEvidence {
  case current, next {
    workflow_outcome.StepRecoveryRecheckRequested, _ ->
      workflow_outcome.StepRecoveryRecheckRequested
    _, workflow_outcome.StepRecoveryRecheckRequested ->
      workflow_outcome.StepRecoveryRecheckRequested
    workflow_outcome.StepRecoveryRan, _ -> workflow_outcome.StepRecoveryRan
    _, workflow_outcome.StepRecoveryRan -> workflow_outcome.StepRecoveryRan
    _, _ -> workflow_outcome.NoStepRecovery
  }
}

pub fn effective_for_failure(
  dag: workflow_dag.WorkflowDag,
  step: workflow_dag.WorkflowStep,
  failed_attempt_index: Int,
) -> Option(workflow_dag.EffectiveRecoveryConfig) {
  case step.on_failure == workflow_dag.ContinueWorkflow {
    True -> None
    False ->
      case workflow_dag.effective_recovery_config(dag, step) {
        Ok(Some(config)) if failed_attempt_index <= config.attempts ->
          Some(config)
        _ -> None
      }
  }
}

pub fn execute(
  step: workflow_dag.WorkflowStep,
  workspace: workspace_run.PreparedStepWorkspace,
  failed_artifact: step_artifact.StepArtifact,
  config: workflow_dag.EffectiveRecoveryConfig,
  issue: tracker_issue.Issue,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  dependencies: Dependencies(context),
) -> AttemptOutcome {
  let recovery_attempt_number = workspace.attempt_index
  let recovery_session_id =
    workflow_identity.step_session_id(
      workspace.run_id,
      step.id <> "-recovery-" <> int.to_string(recovery_attempt_number),
      workspace.attempt_index,
    )
  let prompt_ref = prompt_ref_text(config.prompt)
  let start =
    workflow_checkpoint.StepRecoveryStarted(
      run_id: workspace.run_id,
      workflow_id: workspace.workflow_id,
      step_id: step.id,
      failed_attempt_index: workspace.attempt_index,
      recovery_attempt_number: recovery_attempt_number,
      recovery_session_id: recovery_session_id,
      model: recovery_model_name(orchestrator, step, config.model),
      prompt_ref: prompt_ref,
    )
  case dependencies.checkpoint.step_recovery_started(start) {
    Error(error) -> {
      let Nil =
        note_ignored_checkpoint_error(workflow_checkpoint.describe_error(error))
      RecoveryStop(
        session_tokens.zero_token_totals(),
        None,
        0,
        workflow_outcome.NoStepRecovery,
      )
    }
    Ok(Nil) -> {
      let checkpoint_root = orchestrator.effective.workspace.root
      case
        workflow_recovery_checkpoint_guard.snapshot_for_run(
          checkpoint_root,
          workspace.run_id,
        )
      {
        Error(guard_error) -> {
          ignore_secondary_checkpoint_result(
            workflow_step_recovery.record_finished(
              dependencies.checkpoint,
              workspace,
              step.id,
              recovery_attempt_number,
              recovery_session_id,
              workflow_recovery_checkpoint_guard.recovery_artifact_restore_failed,
              "Protected checkpoint preflight failed",
              workflow_step_recovery.detail(
                workflow_recovery_checkpoint_guard.describe_error(guard_error),
                secrets,
              ),
              None,
            ),
          )
          RecoveryStop(
            session_tokens.zero_token_totals(),
            None,
            0,
            workflow_outcome.StepRecoveryRan,
          )
        }
        Ok(snapshot) ->
          case
            workflow_step_recovery.record_recovery_input(
              dependencies.checkpoint,
              workspace,
              step.id,
              recovery_attempt_number,
              failed_artifact,
              secrets,
            )
          {
            Error(write_error) -> {
              let reason =
                "recovery_input_artifact_write_failed:"
                <> workflow_checkpoint.describe_error(write_error)
                |> workflow_step_recovery.detail(secrets)
              ignore_secondary_checkpoint_result(
                workflow_step_recovery.record_finished(
                  dependencies.checkpoint,
                  workspace,
                  step.id,
                  recovery_attempt_number,
                  recovery_session_id,
                  "recovery_input_artifact_write_failed",
                  "Recovery input artifact write failed",
                  reason,
                  None,
                ),
              )
              RecoveryStop(
                session_tokens.zero_token_totals(),
                None,
                0,
                workflow_outcome.StepRecoveryRan,
              )
            }
            Ok(recovery_input) ->
              case dependencies.make_context() {
                Error(spec_error) -> {
                  let failure_reason =
                    workflow_step_recovery.tool_spec_unavailable_reason(
                      spec_error,
                      secrets,
                    )
                  ignore_secondary_checkpoint_result(
                    workflow_step_recovery.record_finished(
                      dependencies.checkpoint,
                      workspace,
                      step.id,
                      recovery_attempt_number,
                      recovery_session_id,
                      "tool_spec_unavailable",
                      "Recovery tool spec unavailable",
                      failure_reason,
                      None,
                    ),
                  )
                  RecoveryStop(
                    session_tokens.zero_token_totals(),
                    None,
                    0,
                    workflow_outcome.StepRecoveryRan,
                  )
                }
                Ok(context) -> {
                  let prompt =
                    workflow_step_recovery.prompt(prompt_ref, recovery_input)
                  let prompt_mode = workflow_attempt.StepRecoveryPrompt(prompt)
                  let effective =
                    effective_for_recovery(orchestrator, step, config.model)
                  let attempt_context =
                    dependencies.make_attempt_context(context, prompt_mode)
                  case
                    dependencies.agent_step(
                      issue,
                      context,
                      prompt_mode,
                      attempt_context,
                      effective,
                      tracker_client,
                      fn(_) { Nil },
                      fn(_) { Nil },
                      fn(observation) {
                        ignore_secondary_checkpoint_result(
                          dependencies.checkpoint.step_pi_session_recorded(
                            observation,
                          ),
                        )
                      },
                    )
                  {
                    Ok(success) ->
                      case
                        workflow_recovery_checkpoint_guard.restore_after_recovery(
                          checkpoint_root,
                          snapshot,
                        )
                      {
                        Ok(events) ->
                          apply_success(
                            step,
                            workspace,
                            recovery_attempt_number,
                            recovery_session_id,
                            success,
                            secrets,
                            dependencies.checkpoint,
                            guard_reason_suffix(events),
                          )
                        Error(guard_error) ->
                          stop_after_guard_failure(
                            step,
                            workspace,
                            recovery_attempt_number,
                            recovery_session_id,
                            success.tokens,
                            success.final_issue,
                            success.turns,
                            guard_error,
                            secrets,
                            dependencies.checkpoint,
                          )
                      }
                    Error(failure) ->
                      case
                        workflow_recovery_checkpoint_guard.restore_after_recovery(
                          checkpoint_root,
                          snapshot,
                        )
                      {
                        Ok(events) -> {
                          let failure_reason =
                            append_reason_suffix(
                              error.agent_artifact_detail(failure.reason)
                                |> workflow_step_recovery.detail(secrets),
                              guard_reason_suffix(events),
                            )
                          ignore_secondary_checkpoint_result(
                            workflow_step_recovery.record_finished(
                              dependencies.checkpoint,
                              workspace,
                              step.id,
                              recovery_attempt_number,
                              recovery_session_id,
                              "worker_failed",
                              "Recovery worker failed",
                              failure_reason,
                              None,
                            ),
                          )
                          RecoveryStop(
                            failure.tokens,
                            failure.final_issue,
                            0,
                            workflow_outcome.StepRecoveryRan,
                          )
                        }
                        Error(guard_error) ->
                          stop_after_guard_failure(
                            step,
                            workspace,
                            recovery_attempt_number,
                            recovery_session_id,
                            failure.tokens,
                            failure.final_issue,
                            0,
                            guard_error,
                            secrets,
                            dependencies.checkpoint,
                          )
                      }
                  }
                }
              }
          }
      }
    }
  }
}

fn apply_success(
  step: workflow_dag.WorkflowStep,
  workspace: workspace_run.PreparedStepWorkspace,
  recovery_attempt_number: Int,
  recovery_session_id: String,
  success: agent_types.WorkerSuccess,
  secrets: List(String),
  checkpoint: workflow_checkpoint.Writer,
  reason_suffix: String,
) -> AttemptOutcome {
  case workflow_step_recovery.decision(success) {
    Ok(workflow_step_recovery.Recheck(summary, reason)) ->
      case
        workflow_step_recovery.record_decision(
          checkpoint,
          step.id,
          workspace,
          recovery_attempt_number,
          recovery_session_id,
          "recheck",
          summary,
          append_reason_suffix(reason, reason_suffix),
          Some(workspace.attempt_index + 1),
          secrets,
        )
      {
        Ok(Nil) ->
          RecoveryRecheckRequested(
            success.tokens,
            success.final_issue,
            success.turns,
          )
        Error(record_error) -> {
          let Nil =
            note_ignored_checkpoint_error(describe_decision_record_error(
              record_error,
            ))
          RecoveryStop(
            success.tokens,
            success.final_issue,
            success.turns,
            workflow_outcome.StepRecoveryRan,
          )
        }
      }
    Ok(workflow_step_recovery.GaveUp(summary, reason)) -> {
      let Nil = case
        workflow_step_recovery.record_decision(
          checkpoint,
          step.id,
          workspace,
          recovery_attempt_number,
          recovery_session_id,
          "gave_up",
          summary,
          append_reason_suffix(reason, reason_suffix),
          None,
          secrets,
        )
      {
        Ok(Nil) -> Nil
        Error(record_error) ->
          note_ignored_checkpoint_error(describe_decision_record_error(
            record_error,
          ))
      }
      RecoveryStop(
        success.tokens,
        success.final_issue,
        success.turns,
        workflow_outcome.StepRecoveryRan,
      )
    }
    Error(protocol_error) -> {
      let protocol_reason =
        workflow_step_recovery.describe_error(protocol_error)
        <> ":"
        <> workflow_step_recovery.error_message(protocol_error)
      let protocol_reason =
        append_reason_suffix(
          workflow_step_recovery.detail(protocol_reason, secrets),
          reason_suffix,
        )
      ignore_secondary_checkpoint_result(workflow_step_recovery.record_finished(
        checkpoint,
        workspace,
        step.id,
        recovery_attempt_number,
        recovery_session_id,
        "invalid_output",
        "Recovery output was invalid",
        protocol_reason,
        None,
      ))
      RecoveryStop(
        success.tokens,
        success.final_issue,
        success.turns,
        workflow_outcome.StepRecoveryRan,
      )
    }
  }
}

fn describe_decision_record_error(
  error: workflow_step_recovery.DecisionRecordError,
) -> String {
  case error {
    workflow_step_recovery.RecoveryDecisionArtifactWriteFailed(checkpoint_error) ->
      "artifact_write_failed:"
      <> workflow_checkpoint.describe_error(checkpoint_error)
    workflow_step_recovery.RecoveryDecisionFinishedCheckpointFailed(
      checkpoint_error,
    ) ->
      "finished_checkpoint_failed:"
      <> workflow_checkpoint.describe_error(checkpoint_error)
  }
}

fn guard_reason_suffix(
  events: List(workflow_recovery_checkpoint_guard.GuardEvent),
) -> String {
  case events {
    [] -> ""
    _ -> workflow_recovery_checkpoint_guard.events_to_diagnostic(events)
  }
}

fn append_reason_suffix(reason: String, suffix: String) -> String {
  case suffix == "" {
    True -> reason
    False -> reason <> "; " <> suffix
  }
}

fn stop_after_guard_failure(
  step: workflow_dag.WorkflowStep,
  workspace: workspace_run.PreparedStepWorkspace,
  recovery_attempt_number: Int,
  recovery_session_id: String,
  tokens: session_tokens.TokenTotals,
  final_issue: Option(tracker_issue.Issue),
  turns: Int,
  guard_error: workflow_recovery_checkpoint_guard.GuardError,
  secrets: List(String),
  checkpoint: workflow_checkpoint.Writer,
) -> AttemptOutcome {
  ignore_secondary_checkpoint_result(workflow_step_recovery.record_finished(
    checkpoint,
    workspace,
    step.id,
    recovery_attempt_number,
    recovery_session_id,
    workflow_recovery_checkpoint_guard.recovery_artifact_restore_failed,
    "Protected checkpoint restoration failed",
    workflow_step_recovery.detail(
      workflow_recovery_checkpoint_guard.describe_error(guard_error),
      secrets,
    ),
    None,
  ))
  RecoveryStop(tokens, final_issue, turns, workflow_outcome.StepRecoveryRan)
}

fn prompt_ref_text(prompt_ref: workflow_dag.PromptRef) -> String {
  case prompt_ref {
    workflow_dag.PromptInline(prompt) -> prompt
    workflow_dag.PromptResolvedFile(_, prompt) -> prompt
    workflow_dag.PromptFile(path) -> path
  }
}

fn recovery_model_name(
  orchestrator: config_types.OrchestratorConfig,
  step: workflow_dag.WorkflowStep,
  override_model: Option(String),
) -> Option(String) {
  case override_model {
    Some(model) -> Some(model)
    None ->
      model_config.resolve(orchestrator.model_settings, step.model_settings).model
  }
}

fn effective_for_recovery(
  orchestrator: config_types.OrchestratorConfig,
  step: workflow_dag.WorkflowStep,
  override_model: Option(String),
) -> config_types.EffectiveConfig {
  let base =
    model_config.resolve(orchestrator.model_settings, step.model_settings)
  let settings =
    model_config.Settings(
      model: option.or(override_model, base.model),
      thinking: base.thinking,
    )
  let command =
    model_config.apply_to_command(orchestrator.effective.pi.command, settings)
  let argv_command = case orchestrator.effective.pi.argv_command {
    Some(argv) ->
      Some(
        config_types.PiArgvCommand(
          ..argv,
          args: model_config.apply_to_argv_args(argv.args, settings),
        ),
      )
    None -> None
  }
  config_types.EffectiveConfig(
    ..orchestrator.effective,
    pi: config_types.PiConfig(
      ..orchestrator.effective.pi,
      command: command,
      argv_command: argv_command,
    ),
  )
}

fn ignore_secondary_checkpoint_result(
  result: Result(Nil, workflow_checkpoint.CheckpointError),
) -> Nil {
  case result {
    Ok(Nil) -> Nil
    Error(error) ->
      note_ignored_checkpoint_error(workflow_checkpoint.describe_error(error))
  }
}

fn note_ignored_checkpoint_error(_message: String) -> Nil {
  Nil
}
