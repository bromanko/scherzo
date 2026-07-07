import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/agent/run_attempt
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/error
import scherzo/model_config
import scherzo/session/tokens as session_tokens
import scherzo/step_artifact
import scherzo/template
import scherzo/tracker
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_attempt
import scherzo/workflow_checkpoint
import scherzo/workflow_dag
import scherzo/workflow_run/step_artifacts
import scherzo/workflow_run/step_context.{type StepContext}
import scherzo/workflow_run/structured_output_step
import scherzo/workspace_profile
import scherzo/workspace_run

pub type Dependencies {
  Dependencies(
    command_step: fn(
      StepContext,
      String,
      Int,
      List(String),
      config_types.ArtifactLimits,
    ) -> step_artifact.StepArtifact,
    agent_step: fn(
      tracker_issue.Issue,
      StepContext,
      workflow_attempt.AgentPromptMode,
      workflow_attempt.StepAttemptContext,
      config_types.EffectiveConfig,
      tracker.Client,
      fn(agent_types.RunnerUpdate) -> Nil,
      fn(process.Subject(worker_command.Command)) -> Nil,
      fn(workflow_attempt.PiSessionObservation) -> Nil,
    ) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
    checkpoint: workflow_checkpoint.Writer,
  )
}

pub type StepResult {
  StepResult(
    artifact: step_artifact.StepArtifact,
    tokens: session_tokens.TokenTotals,
    final_issue: Option(tracker_issue.Issue),
    turns: Int,
  )
}

pub fn default_command_step(
  context: StepContext,
  command: String,
  timeout_ms: Int,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
) -> step_artifact.StepArtifact {
  command_step.run_with_env(
    context.step_id,
    command,
    context.workspace_path,
    timeout_ms,
    step_context.command_env(context),
    secrets,
    limits,
  )
}

pub fn default_agent_step(
  issue: tracker_issue.Issue,
  context: StepContext,
  prompt_mode: workflow_attempt.AgentPromptMode,
  attempt_context: workflow_attempt.StepAttemptContext,
  effective: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(agent_types.RunnerUpdate) -> Nil,
  command_ready: fn(process.Subject(worker_command.Command)) -> Nil,
  record_pi_session: fn(workflow_attempt.PiSessionObservation) -> Nil,
) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
  let command_subject = process.new_subject()
  let redaction_secrets =
    step_redaction_secrets(context, effective_config_secrets(effective))
  run_attempt.run_prompt_mode_in_workspace(
    issue,
    prompt_mode,
    attempt_context,
    config_types.with_pi_env(effective, step_context.command_env(context)),
    tracker_client,
    fn(_, update) {
      emit_update(agent_types.redact_runner_update(update, redaction_secrets))
    },
    command_subject,
    fn() { command_ready(command_subject) },
    context.workspace_path,
    record_pi_session,
  )
}

fn step_redaction_secrets(
  context: StepContext,
  secrets: List(String),
) -> List(String) {
  list.append(
    secrets,
    workspace_profile.driver_context_redaction_values(context.workspace_context),
  )
}

fn effective_config_secrets(
  config: config_types.EffectiveConfig,
) -> List(String) {
  case config.tracker.api_key {
    Some(value) -> [value]
    None -> []
  }
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

pub fn run(
  step: workflow_dag.WorkflowStep,
  workspace: workspace_run.PreparedStepWorkspace,
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  dependencies: Dependencies,
  artifacts: Dict(String, step_artifact.StepArtifact),
  pi_session_continuations: Dict(String, workflow_attempt.PiContinuation),
  profile: config_types.WorkspaceHookProfile,
) -> StepResult {
  let #(artifact, tokens, final_issue, turns) =
    run_step(
      step,
      workspace,
      issue,
      dag,
      orchestrator,
      tracker_client,
      secrets,
      dependencies,
      artifacts,
      pi_session_continuations,
      profile,
    )
  StepResult(
    artifact: artifact,
    tokens: tokens,
    final_issue: final_issue,
    turns: turns,
  )
}

fn run_step(
  step: workflow_dag.WorkflowStep,
  workspace: workspace_run.PreparedStepWorkspace,
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  dependencies: Dependencies,
  artifacts: Dict(String, step_artifact.StepArtifact),
  pi_session_continuations: Dict(String, workflow_attempt.PiContinuation),
  profile: config_types.WorkspaceHookProfile,
) -> #(
  step_artifact.StepArtifact,
  session_tokens.TokenTotals,
  Option(tracker_issue.Issue),
  Int,
) {
  let context =
    step_context.from_prepared(step, workspace, issue, orchestrator, profile)
  case step.kind {
    workflow_dag.CommandStep(run, timeout_ms) -> {
      let timeout_ms = case timeout_ms {
        Some(value) -> value
        None -> 60_000
      }
      #(
        dependencies.command_step(
          context,
          run,
          timeout_ms,
          secrets,
          orchestrator.artifact_limits,
        ),
        session_tokens.zero_token_totals(),
        None,
        0,
      )
    }
    workflow_dag.AgentStep(prompt_ref, structured_output_spec) ->
      run_agent_step(
        step,
        prompt_ref,
        structured_output_spec,
        context,
        issue,
        dag,
        orchestrator,
        tracker_client,
        secrets,
        dependencies,
        artifacts,
        pi_session_continuations,
      )
  }
}

fn run_agent_step(
  step: workflow_dag.WorkflowStep,
  prompt_ref: workflow_dag.PromptRef,
  structured_output_spec: Option(workflow_dag.StructuredOutputSpec),
  context: StepContext,
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  dependencies: Dependencies,
  artifacts: Dict(String, step_artifact.StepArtifact),
  pi_session_continuations: Dict(String, workflow_attempt.PiContinuation),
) -> #(
  step_artifact.StepArtifact,
  session_tokens.TokenTotals,
  Option(tracker_issue.Issue),
  Int,
) {
  case
    structured_output_step.prepare_tool_context(context, structured_output_spec)
  {
    Error(spec_error) -> #(
      step_artifact.from_command_result(
        step.id,
        1,
        "",
        "structured-output tool spec generation failed: "
          <> spec_error.code
          <> ":"
          <> spec_error.message,
        False,
        secrets,
        orchestrator.artifact_limits,
      ),
      session_tokens.zero_token_totals(),
      None,
      0,
    )
    Ok(context) ->
      case
        prompt_mode_for_step(
          step,
          prompt_ref,
          issue,
          artifacts,
          pi_session_continuations,
          context,
        )
      {
        Error(Nil) -> #(
          step_artifact.from_command_result(
            step.id,
            1,
            "",
            "template render failed",
            False,
            secrets,
            orchestrator.artifact_limits,
          ),
          session_tokens.zero_token_totals(),
          None,
          0,
        )
        Ok(prompt_mode) -> {
          let effective = effective_for_step(orchestrator, step)
          let continuation = case dict.get(pi_session_continuations, step.id) {
            Ok(value) -> Some(value)
            Error(Nil) -> None
          }
          case
            run_agent_invocation(
              issue,
              context,
              dag,
              orchestrator,
              prompt_mode,
              continuation,
              effective,
              tracker_client,
              dependencies,
            )
          {
            Ok(success) ->
              agent_success_result(
                step,
                context,
                success,
                structured_output_spec,
                issue,
                dag,
                orchestrator,
                tracker_client,
                secrets,
                dependencies,
                effective,
              )
            Error(failure) ->
              agent_failure_result(
                step,
                context,
                failure,
                structured_output_spec,
                issue,
                dag,
                orchestrator,
                tracker_client,
                secrets,
                dependencies,
                effective,
              )
          }
        }
      }
  }
}

fn run_agent_invocation(
  issue: tracker_issue.Issue,
  context: StepContext,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  prompt_mode: workflow_attempt.AgentPromptMode,
  continuation: Option(workflow_attempt.PiContinuation),
  effective: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  dependencies: Dependencies,
) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
  let attempt_context =
    workflow_attempt_context(
      context,
      dag,
      orchestrator,
      prompt_mode,
      continuation,
    )
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
        dependencies.checkpoint.step_pi_session_recorded(observation),
      )
    },
  )
}

fn agent_failure_result(
  step: workflow_dag.WorkflowStep,
  context: StepContext,
  failure: agent_types.WorkerFailure,
  structured_output_spec: Option(workflow_dag.StructuredOutputSpec),
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  dependencies: Dependencies,
  effective: config_types.EffectiveConfig,
) -> #(
  step_artifact.StepArtifact,
  session_tokens.TokenTotals,
  Option(tracker_issue.Issue),
  Int,
) {
  structured_output_step.finish_failure(
    step,
    context,
    failure,
    structured_output_spec,
    secrets,
    orchestrator.artifact_limits,
    dependencies.checkpoint,
    fn(prompt_mode, continuation) {
      run_agent_invocation(
        issue,
        context,
        dag,
        orchestrator,
        prompt_mode,
        continuation,
        effective,
        tracker_client,
        dependencies,
      )
    },
  )
  |> structured_output_agent_result_tuple
}

fn agent_success_result(
  step: workflow_dag.WorkflowStep,
  context: StepContext,
  success: agent_types.WorkerSuccess,
  structured_output_spec: Option(workflow_dag.StructuredOutputSpec),
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  dependencies: Dependencies,
  effective: config_types.EffectiveConfig,
) -> #(
  step_artifact.StepArtifact,
  session_tokens.TokenTotals,
  Option(tracker_issue.Issue),
  Int,
) {
  structured_output_step.finish_success(
    step,
    context,
    success,
    structured_output_spec,
    secrets,
    orchestrator.artifact_limits,
    dependencies.checkpoint,
    fn(prompt_mode, continuation) {
      run_agent_invocation(
        issue,
        context,
        dag,
        orchestrator,
        prompt_mode,
        continuation,
        effective,
        tracker_client,
        dependencies,
      )
    },
  )
  |> structured_output_agent_result_tuple
}

fn structured_output_agent_result_tuple(
  result: structured_output_step.AgentResult,
) -> #(
  step_artifact.StepArtifact,
  session_tokens.TokenTotals,
  Option(tracker_issue.Issue),
  Int,
) {
  let structured_output_step.AgentResult(
    artifact: artifact,
    tokens: tokens,
    final_issue: final_issue,
    turns: turns,
  ) = result
  #(artifact, tokens, final_issue, turns)
}

pub fn is_recovery_resume_validation_artifact(
  artifact: step_artifact.StepArtifact,
) -> Bool {
  step_artifacts.is_recovery_resume_validation_artifact(artifact)
}

pub fn agent_reason_for_artifact(
  artifact: step_artifact.StepArtifact,
) -> Option(error.AgentRunnerError) {
  step_artifacts.agent_reason_for_artifact(artifact)
}

fn prompt_mode_for_step(
  step: workflow_dag.WorkflowStep,
  prompt_ref: workflow_dag.PromptRef,
  issue: tracker_issue.Issue,
  artifacts: Dict(String, step_artifact.StepArtifact),
  pi_session_continuations: Dict(String, workflow_attempt.PiContinuation),
  context: StepContext,
) -> Result(workflow_attempt.AgentPromptMode, Nil) {
  case dict.get(pi_session_continuations, step.id) {
    Ok(continuation) ->
      Ok(workflow_attempt.RecoveryPrompt(continuation.recovery_prompt))
    Error(Nil) -> {
      let prompt_template = case prompt_ref {
        workflow_dag.PromptInline(prompt) -> prompt
        workflow_dag.PromptResolvedFile(_, prompt) -> prompt
        workflow_dag.PromptFile(path) -> path
      }
      let locals =
        list.append(
          step_artifact.to_template_locals(artifacts),
          workspace_profile.driver_context_template_locals(
            context.workspace_context,
          ),
        )
      case template.render_with_locals(prompt_template, issue, None, locals) {
        Ok(prompt) -> Ok(workflow_attempt.OriginalPrompt(prompt))
        Error(_) -> Error(Nil)
      }
    }
  }
}

pub fn workflow_attempt_context(
  context: StepContext,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  prompt_mode: workflow_attempt.AgentPromptMode,
  continuation: Option(workflow_attempt.PiContinuation),
) -> workflow_attempt.StepAttemptContext {
  let workflow_fingerprint =
    workflow_attempt.workflow_fingerprint(dag, orchestrator)
  workflow_attempt.StepAttemptContext(
    run_id: context.run_id,
    issue_id: context.issue_id,
    issue_identifier: context.issue_identifier,
    workflow_id: context.workflow_id,
    workflow_fingerprint: workflow_fingerprint,
    step_id: context.step_id,
    workspace_name: context.workspace_name,
    attempt_index: context.attempt_index,
    workspace_path: context.workspace_path,
    continuation_capable: case prompt_mode {
      workflow_attempt.RecoveryPrompt(_) -> True
      workflow_attempt.StepRecoveryPrompt(_) -> False
      workflow_attempt.OriginalPrompt(_)
      | workflow_attempt.StructuredOutputRetryPrompt(_) ->
        orchestrator.effective.pi.session_persistence.enabled
    },
    continuation_session_file: case continuation {
      Some(value) -> Some(value.session_file)
      None -> None
    },
  )
}

pub fn continuation_capable(
  step: workflow_dag.WorkflowStep,
  orchestrator: config_types.OrchestratorConfig,
) -> Bool {
  case step.kind {
    workflow_dag.AgentStep(_, _) ->
      orchestrator.effective.pi.session_persistence.enabled
    workflow_dag.CommandStep(_, _) -> False
  }
}

fn effective_for_step(
  orchestrator: config_types.OrchestratorConfig,
  step: workflow_dag.WorkflowStep,
) -> config_types.EffectiveConfig {
  let settings =
    model_config.resolve(orchestrator.model_settings, step.model_settings)
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
