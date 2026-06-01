import birl
import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/agent/run_attempt
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/artifact_publication_executor
import scherzo/artifact_publication_recording
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/error
import scherzo/log
import scherzo/model_config
import scherzo/orchestrator/schedule_core
import scherzo/process_ext
import scherzo/result_artifact
import scherzo/session/tokens as session_tokens
import scherzo/step_artifact
import scherzo/structured_output
import scherzo/structured_output_metadata
import scherzo/structured_output_source
import scherzo/structured_output_tool_spec
import scherzo/template
import scherzo/tracker
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_attempt
import scherzo/workflow_checkpoint
import scherzo/workflow_contract_manifest as contract_manifest
import scherzo/workflow_dag
import scherzo/workflow_identity
import scherzo/workflow_outcome
import scherzo/workflow_recovery_checkpoint_guard
import scherzo/workflow_run/contract_io
import scherzo/workflow_run/contract_io_error as contract_error
import scherzo/workflow_run/workstream_handoff
import scherzo/workflow_scheduler
import scherzo/workflow_step_recovery
import scherzo/workflow_structured_retry
import scherzo/workspace_profile
import scherzo/workspace_run

pub type PostSuccessCleanupWarning {
  PostSuccessCleanupWarning(code: String, message: String, run_root: String)
}

pub type WorkflowRunSuccess {
  WorkflowRunSuccess(
    worker_success: agent_types.WorkerSuccess,
    artifacts: Dict(String, step_artifact.StepArtifact),
    run_root: String,
    cleanup_warning: Option(PostSuccessCleanupWarning),
  )
}

pub type WorkflowRunFailure {
  WorkflowRunFailure(
    reason: String,
    agent_reason: Option(error.AgentRunnerError),
    artifacts: Dict(String, step_artifact.StepArtifact),
    run_root: Option(String),
    failed_step_id: Option(String),
  )
}

pub type StepContext {
  StepContext(
    workflow_id: String,
    run_id: String,
    run_root: String,
    workflow_bundle_dir: String,
    step_id: String,
    attempt_index: Int,
    workspace_name: String,
    workspace_path: String,
    workspace_context: workspace_profile.WorkspaceDriverContext,
    config_dir: String,
    issue_id: String,
    issue_identifier: String,
    run_kind: String,
    scheduled_job_id: String,
    schedule_due_at: String,
    schedule_started_at: String,
    run_attempt: Int,
    extra_pi_env: List(#(String, String)),
  )
}

pub type StepAttemptContext {
  StepAttemptContext(step_id: String, next_attempt: Int)
}

pub type ContractRunValues {
  ContractRunValues(
    inputs: Dict(String, contract_manifest.ManifestValue),
    context: Dict(String, contract_manifest.ManifestValue),
  )
}

pub type ScheduledInvocationContext {
  ScheduledInvocationContext(
    job_id: String,
    workflow_id: String,
    due_at: String,
    started_at: String,
    run_id: String,
    attempt: Int,
  )
}

pub type RunInvocation {
  RunInvocation(
    run_id: String,
    workflow_fingerprint: String,
    supplied_contract_values: ContractRunValues,
    scheduled_context: Option(ScheduledInvocationContext),
  )
}

pub type RecoveredRunContext {
  RecoveredRunContext(
    workflow_id: String,
    workflow_fingerprint: String,
    run_id: String,
    run_root: String,
    recovery_evidence: workflow_outcome.RecoveryEvidence,
    scheduler_statuses: Dict(String, workflow_scheduler.StepRuntime),
    artifacts: Dict(String, step_artifact.StepArtifact),
    prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
    step_attempts: Dict(String, Int),
    token_totals: session_tokens.TokenTotals,
    final_issue: Option(tracker_issue.Issue),
    turns: Int,
    warnings: List(String),
    pi_session_continuations: Dict(String, workflow_attempt.PiContinuation),
    contract_inputs_recorded: Option(workflow_checkpoint.ArtifactWritten),
    contract_outputs_recorded: Option(workflow_checkpoint.ArtifactWritten),
  )
}

pub type RunContext {
  FreshRun(RunInvocation)
  RecoveredRun(RecoveredRunContext)
}

pub type AttemptStart {
  AttemptStart(
    workflow_id: String,
    workflow_fingerprint: String,
    run_id: String,
    issue_id: String,
    step_id: String,
    attempt: Int,
    workspace_name: String,
    workspace_path: String,
    source_workspace_name: Option(String),
    source_workspace_path: Option(String),
  )
}

pub type AttemptFinish {
  AttemptFinish(
    run_id: String,
    step_id: String,
    attempt: Int,
    terminal_status: String,
    artifact: step_artifact.StepArtifact,
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
    after_step: fn(
      tracker_issue.Issue,
      String,
      workspace_run.PreparedStepWorkspace,
      config_types.OrchestratorConfig,
      config_types.WorkspaceHookProfile,
    ) -> Nil,
    cleanup_run: fn(
      String,
      config_types.OrchestratorConfig,
      config_types.WorkspaceHookProfile,
    ) -> Result(Nil, error.WorkspaceError),
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

pub type ResumeState {
  ResumeState(
    artifacts: Dict(String, step_artifact.StepArtifact),
    workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
    next_attempt_indexes: Dict(String, Int),
    run_root: Option(String),
    recovery_evidence: workflow_outcome.RecoveryEvidence,
    pi_session_continuations: Dict(String, workflow_attempt.PiContinuation),
    contract_inputs_recorded: Option(workflow_checkpoint.ArtifactWritten),
    contract_outputs_recorded: Option(workflow_checkpoint.ArtifactWritten),
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

type StepBatchOutcome {
  StepBatchCompleted(List(StepExecutionResult))
  StepBatchFatal(StepExecutionResult)
}

type WorkflowStartError {
  WorkflowRunRootFailed(error.WorkspaceError)
  WorkflowStartCheckpointFailed(workflow_checkpoint.CheckpointError)
}

type PublicationRecordingError {
  PublicationRecordingFailed(String)
}

type StepBatchError {
  StepWorkerDown(step_id: String, reason: process.ExitReason)
}

type StepBatchStartError {
  StepBatchStartError(reason: String, cleanup_allowed: Bool)
}

type AfterStepMessage {
  AfterStepCompleted
  AfterStepDown(process.Down)
  AfterStepLinkedExit
}

type AfterStepError {
  AfterStepExitedWithoutResult(step_id: String)
  AfterStepKilled(step_id: String)
  AfterStepCrashed(step_id: String)
  AfterStepMonitorDown(step_id: String)
}

type RecoveryAttemptOutcome {
  RecoveryRetryRequested(
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

fn combine_recovery_evidence(
  current: workflow_outcome.RecoveryEvidence,
  next: workflow_outcome.RecoveryEvidence,
) -> workflow_outcome.RecoveryEvidence {
  case current, next {
    workflow_outcome.StepRecoveryRetryRequested, _ ->
      workflow_outcome.StepRecoveryRetryRequested
    _, workflow_outcome.StepRecoveryRetryRequested ->
      workflow_outcome.StepRecoveryRetryRequested
    workflow_outcome.StepRecoveryRan, _ -> workflow_outcome.StepRecoveryRan
    _, workflow_outcome.StepRecoveryRan -> workflow_outcome.StepRecoveryRan
    _, _ -> workflow_outcome.NoStepRecovery
  }
}

type PrepareReadyFailure {
  PrepareReadyFailure(
    reason: String,
    agent_reason: Option(error.AgentRunnerError),
    run_root: Option(String),
    prepared_starts: List(PreparedStart),
  )
}

pub fn default_dependencies() -> Dependencies {
  Dependencies(
    prepare_step: workspace_run.prepare_step_attempt,
    prepare_recovered_step: workspace_run.prepare_recovered_step_attempt,
    after_step: workspace_run.after_step,
    cleanup_run: workspace_run.cleanup_run,
    command_step: fn(context, command, timeout_ms, secrets, limits) {
      command_step.run_with_env(
        context.step_id,
        command,
        context.workspace_path,
        timeout_ms,
        step_command_env(context),
        secrets,
        limits,
      )
    },
    agent_step: fn(
      issue,
      context,
      prompt_mode,
      attempt_context,
      effective,
      tracker_client,
      emit_update,
      command_ready,
      record_pi_session,
    ) {
      let command_subject = process.new_subject()
      let redaction_secrets =
        step_redaction_secrets(context, effective_config_secrets(effective))
      run_attempt.run_prompt_mode_in_workspace(
        issue,
        prompt_mode,
        attempt_context,
        config_types.with_pi_env(effective, step_command_env(context)),
        tracker_client,
        fn(_, update) {
          emit_update(agent_types.redact_runner_update(
            update,
            redaction_secrets,
          ))
        },
        command_subject,
        fn() { command_ready(command_subject) },
        context.workspace_path,
        record_pi_session,
      )
    },
    checkpoint: workflow_checkpoint.noop_writer(),
  )
}

fn profile_redaction_secrets(
  profile: config_types.WorkspaceHookProfile,
  secrets: List(String),
) -> List(String) {
  list.append(secrets, workspace_profile.profile_redaction_values(profile))
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

pub fn empty_contract_run_values() -> ContractRunValues {
  ContractRunValues(inputs: dict.new(), context: dict.new())
}

pub fn execute(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  run_id: String,
  dependencies: Dependencies,
) -> Result(WorkflowRunSuccess, WorkflowRunFailure) {
  execute_with_contract_values(
    issue,
    dag,
    orchestrator,
    tracker_client,
    secrets,
    run_id,
    empty_contract_run_values(),
    dependencies,
  )
}

pub fn execute_with_contract_values(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  run_id: String,
  supplied_contract_values: ContractRunValues,
  dependencies: Dependencies,
) -> Result(WorkflowRunSuccess, WorkflowRunFailure) {
  execute_with_context(
    issue,
    dag,
    orchestrator,
    tracker_client,
    secrets,
    FreshRun(RunInvocation(
      run_id: run_id,
      workflow_fingerprint: workflow_attempt.workflow_fingerprint(
        dag,
        orchestrator,
      ),
      supplied_contract_values: supplied_contract_values,
      scheduled_context: None,
    )),
    dependencies,
  )
}

pub fn execute_scheduled(
  scheduled: schedule_core.ScheduledRunContext,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  dependencies: Dependencies,
) -> Result(WorkflowRunSuccess, WorkflowRunFailure) {
  let issue = scheduled_placeholder_issue(scheduled)
  execute_with_context(
    issue,
    dag,
    orchestrator,
    tracker_client,
    secrets,
    FreshRun(RunInvocation(
      run_id: scheduled.run_id,
      workflow_fingerprint: workflow_attempt.workflow_fingerprint(
        dag,
        orchestrator,
      ),
      supplied_contract_values: empty_contract_run_values(),
      scheduled_context: Some(ScheduledInvocationContext(
        job_id: scheduled.job_id,
        workflow_id: scheduled.workflow_id,
        due_at: schedule_core.iso_utc(scheduled.due_at_ms),
        started_at: schedule_core.iso_utc(scheduled.started_at_ms),
        run_id: scheduled.run_id,
        attempt: scheduled.attempt,
      )),
    )),
    scheduled_dependencies(scheduled, dependencies),
  )
}

pub fn execute_with_resume(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  run_id: String,
  dependencies: Dependencies,
  resume: ResumeState,
) -> Result(WorkflowRunSuccess, WorkflowRunFailure) {
  let scheduler_state =
    scheduler_with_artifacts(workflow_scheduler.init(dag), resume.artifacts)
  let run_root = option.or(first_run_root(resume.workspaces), resume.run_root)
  let run_root_value = option.unwrap(run_root, "")
  execute_with_context(
    issue,
    dag,
    orchestrator,
    tracker_client,
    secrets,
    RecoveredRun(RecoveredRunContext(
      workflow_id: dag.id,
      workflow_fingerprint: workflow_attempt.workflow_fingerprint(
        dag,
        orchestrator,
      ),
      run_id: run_id,
      run_root: run_root_value,
      recovery_evidence: resume.recovery_evidence,
      scheduler_statuses: scheduler_state.statuses,
      artifacts: resume.artifacts,
      prepared_workspaces: resume.workspaces,
      step_attempts: resume.next_attempt_indexes,
      token_totals: session_tokens.zero_token_totals(),
      final_issue: None,
      turns: 0,
      warnings: [],
      pi_session_continuations: resume.pi_session_continuations,
      contract_inputs_recorded: resume.contract_inputs_recorded,
      contract_outputs_recorded: resume.contract_outputs_recorded,
    )),
    dependencies,
  )
}

fn scheduled_dependencies(
  scheduled: schedule_core.ScheduledRunContext,
  dependencies: Dependencies,
) -> Dependencies {
  Dependencies(
    ..dependencies,
    prepare_step: fn(
      _issue,
      _workflow_id,
      _run_id,
      step_id,
      _attempt_index,
      workspace_ref,
      orchestrator,
      profile,
      known,
    ) {
      workspace_run.prepare_scheduled_step_attempt(
        scheduled,
        step_id,
        workspace_ref,
        orchestrator,
        profile,
        known,
      )
    },
    prepare_recovered_step: fn(
      _issue,
      _workflow_id,
      _run_id,
      _expected_run_root,
      step_id,
      _attempt_index,
      workspace_ref,
      orchestrator,
      profile,
      known,
    ) {
      workspace_run.prepare_scheduled_step_attempt(
        scheduled,
        step_id,
        workspace_ref,
        orchestrator,
        profile,
        known,
      )
    },
    after_step: fn(_, step_id, prepared, orchestrator, profile) {
      workspace_run.scheduled_after_step(
        scheduled,
        step_id,
        prepared,
        orchestrator,
        profile,
      )
    },
    command_step: fn(context, command, timeout_ms, secrets, limits) {
      let template_context = scheduled_template_context(scheduled)
      let context = scheduled_step_context(context, scheduled)
      case template.render_scheduled(command, template_context) {
        Ok(rendered) ->
          dependencies.command_step(
            context,
            rendered,
            timeout_ms,
            secrets,
            limits,
          )
        Error(err) ->
          step_artifact.from_command_result(
            context.step_id,
            1,
            "",
            "template render failed:" <> error.template_code(err),
            False,
            secrets,
            limits,
          )
      }
    },
  )
}

fn scheduled_step_context(
  context: StepContext,
  scheduled: schedule_core.ScheduledRunContext,
) -> StepContext {
  StepContext(
    ..context,
    issue_id: "",
    issue_identifier: "",
    run_kind: "scheduled",
    scheduled_job_id: scheduled.job_id,
    schedule_due_at: schedule_core.iso_utc(scheduled.due_at_ms),
    schedule_started_at: schedule_core.iso_utc(scheduled.started_at_ms),
    run_attempt: scheduled.attempt,
  )
}

fn scheduled_template_context(
  scheduled: schedule_core.ScheduledRunContext,
) -> template.ScheduledTemplateContext {
  template.ScheduledTemplateContext(
    job_id: scheduled.job_id,
    workflow_id: scheduled.workflow_id,
    due_at: schedule_core.iso_utc(scheduled.due_at_ms),
    started_at: schedule_core.iso_utc(scheduled.started_at_ms),
    run_id: scheduled.run_id,
    attempt: scheduled.attempt,
  )
}

fn scheduled_placeholder_issue(
  scheduled: schedule_core.ScheduledRunContext,
) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "",
    identifier: scheduled.job_id,
    title: "Scheduled job " <> scheduled.job_id,
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked("scheduled"),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}

pub fn execute_with_context(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  context: RunContext,
  dependencies: Dependencies,
) -> Result(WorkflowRunSuccess, WorkflowRunFailure) {
  case workspace_profile.resolve(dag, orchestrator) {
    Error(err) ->
      Error(WorkflowRunFailure(
        reason: workspace_profile.error_label(err),
        agent_reason: None,
        artifacts: run_context_artifacts(context),
        run_root: run_context_run_root(context),
        failed_step_id: None,
      ))
    Ok(profile) -> {
      let secrets = profile_redaction_secrets(profile, secrets)
      case context {
        FreshRun(invocation) ->
          case
            ensure_workflow_started(
              issue,
              dag,
              orchestrator,
              invocation,
              dependencies,
            )
          {
            Error(error) ->
              Error(WorkflowRunFailure(
                reason: workflow_start_error_reason(error),
                agent_reason: None,
                artifacts: dict.new(),
                run_root: None,
                failed_step_id: None,
              ))
            Ok(Nil) ->
              case
                record_inputs_if_contracted(
                  issue,
                  dag,
                  orchestrator,
                  invocation,
                  dependencies,
                  profile,
                )
              {
                Error(error) -> {
                  let reason = contract_error.describe_error(error)
                  ignore_secondary_checkpoint_result(
                    dependencies.checkpoint.workflow_finished(
                      workflow_checkpoint.WorkflowFinished(
                        run_id: invocation.run_id,
                        workflow_id: dag.id,
                        issue_id: issue.id,
                        task_ref: task_ref(issue),
                        outcome: workflow_outcome.terminal_failed_fatal(
                          workflow_outcome.NoStepRecovery,
                        ),
                        token_total: 0,
                        turns: 0,
                      ),
                    ),
                  )
                  Error(WorkflowRunFailure(
                    reason: reason,
                    agent_reason: None,
                    artifacts: dict.new(),
                    run_root: None,
                    failed_step_id: None,
                  ))
                }
                Ok(Nil) ->
                  loop(
                    issue,
                    dag,
                    orchestrator,
                    tracker_client,
                    secrets,
                    invocation.run_id,
                    invocation.workflow_fingerprint,
                    None,
                    workflow_outcome.NoStepRecovery,
                    False,
                    dependencies,
                    workflow_scheduler.init(dag),
                    dict.new(),
                    dict.new(),
                    None,
                    dict.new(),
                    session_tokens.zero_token_totals(),
                    None,
                    0,
                    True,
                    dict.new(),
                    profile,
                  )
              }
          }
        RecoveredRun(recovered) ->
          case recovered.workflow_id != dag.id {
            True ->
              Error(WorkflowRunFailure(
                reason: "workflow_recovery_invalid:workflow_id_mismatch",
                agent_reason: None,
                artifacts: recovered.artifacts,
                run_root: Some(recovered.run_root),
                failed_step_id: None,
              ))
            False ->
              case
                ensure_recovered_workflow_started(
                  issue,
                  recovered,
                  dependencies,
                )
              {
                Error(error) ->
                  Error(WorkflowRunFailure(
                    reason: workflow_start_error_reason(error),
                    agent_reason: None,
                    artifacts: recovered.artifacts,
                    run_root: Some(recovered.run_root),
                    failed_step_id: None,
                  ))
                Ok(Nil) ->
                  case
                    record_recovered_inputs_if_contracted(
                      issue,
                      dag,
                      orchestrator,
                      recovered,
                      dependencies,
                      profile,
                    )
                  {
                    Error(error) -> {
                      let reason = contract_error.describe_error(error)
                      ignore_secondary_checkpoint_result(
                        dependencies.checkpoint.workflow_finished(
                          workflow_checkpoint.WorkflowFinished(
                            run_id: recovered.run_id,
                            workflow_id: dag.id,
                            issue_id: issue.id,
                            task_ref: task_ref(issue),
                            outcome: workflow_outcome.terminal_failed_fatal(
                              recovered.recovery_evidence,
                            ),
                            token_total: recovered.token_totals.total,
                            turns: recovered.turns,
                          ),
                        ),
                      )
                      Error(WorkflowRunFailure(
                        reason: reason,
                        agent_reason: None,
                        artifacts: recovered.artifacts,
                        run_root: Some(recovered.run_root),
                        failed_step_id: None,
                      ))
                    }
                    Ok(Nil) ->
                      case
                        workflow_scheduler.init_with_statuses(
                          dag,
                          recovered.scheduler_statuses,
                        )
                      {
                        Error(reason) ->
                          Error(WorkflowRunFailure(
                            reason: "workflow_recovery_invalid:" <> reason,
                            agent_reason: None,
                            artifacts: recovered.artifacts,
                            run_root: Some(recovered.run_root),
                            failed_step_id: None,
                          ))
                        Ok(scheduler_state) -> {
                          let cleanup_allowed =
                            workflow_scheduler.outcome(dag, scheduler_state)
                            != workflow_scheduler.WorkflowInProgress
                          loop(
                            issue,
                            dag,
                            orchestrator,
                            tracker_client,
                            secrets,
                            recovered.run_id,
                            recovered.workflow_fingerprint,
                            recovered.contract_outputs_recorded,
                            recovered.recovery_evidence,
                            True,
                            dependencies,
                            scheduler_state,
                            recovered.artifacts,
                            recovered.prepared_workspaces,
                            Some(recovered.run_root),
                            recovered.step_attempts,
                            recovered.token_totals,
                            recovered.final_issue,
                            recovered.turns,
                            cleanup_allowed,
                            recovered.pi_session_continuations,
                            profile,
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

fn run_context_artifacts(
  context: RunContext,
) -> Dict(String, step_artifact.StepArtifact) {
  case context {
    FreshRun(_) -> dict.new()
    RecoveredRun(recovered) -> recovered.artifacts
  }
}

fn run_context_run_root(context: RunContext) -> Option(String) {
  case context {
    FreshRun(_) -> None
    RecoveredRun(recovered) -> Some(recovered.run_root)
  }
}

fn workflow_start_error_reason(error: WorkflowStartError) -> String {
  case error {
    WorkflowRunRootFailed(error) -> error.workspace_code(error)
    WorkflowStartCheckpointFailed(error) ->
      "checkpoint_failed:" <> workflow_checkpoint.describe_error(error)
  }
}

fn ensure_workflow_started(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  invocation: RunInvocation,
  dependencies: Dependencies,
) -> Result(Nil, WorkflowStartError) {
  use run_root <- result.try(
    case invocation.scheduled_context {
      Some(scheduled) ->
        workspace_run.scheduled_run_root_for(
          scheduled.job_id,
          scheduled.workflow_id,
          scheduled.run_id,
          orchestrator,
        )
      None ->
        workspace_run.run_root_for(
          issue,
          dag.id,
          invocation.run_id,
          orchestrator,
        )
    }
    |> result.map_error(WorkflowRunRootFailed),
  )
  dependencies.checkpoint.workflow_started(workflow_checkpoint.WorkflowStarted(
    run_id: invocation.run_id,
    workflow_id: dag.id,
    workflow_fingerprint: invocation.workflow_fingerprint,
    issue_id: issue.id,
    issue_identifier: issue.identifier,
    task_ref: task_ref(issue),
    issue_fingerprint: workflow_attempt.issue_fingerprint(issue),
    observed_updated_at_ms: observed_updated_at_ms(issue),
    run_root: run_root,
  ))
  |> result.map_error(WorkflowStartCheckpointFailed)
}

fn ensure_recovered_workflow_started(
  issue: tracker_issue.Issue,
  recovered: RecoveredRunContext,
  dependencies: Dependencies,
) -> Result(Nil, WorkflowStartError) {
  dependencies.checkpoint.workflow_started(workflow_checkpoint.WorkflowStarted(
    run_id: recovered.run_id,
    workflow_id: recovered.workflow_id,
    workflow_fingerprint: recovered.workflow_fingerprint,
    issue_id: issue.id,
    issue_identifier: issue.identifier,
    task_ref: task_ref(issue),
    issue_fingerprint: workflow_attempt.issue_fingerprint(issue),
    observed_updated_at_ms: observed_updated_at_ms(issue),
    run_root: recovered.run_root,
  ))
  |> result.map_error(WorkflowStartCheckpointFailed)
}

fn record_recovered_inputs_if_contracted(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  recovered: RecoveredRunContext,
  dependencies: Dependencies,
  profile: config_types.WorkspaceHookProfile,
) -> Result(Nil, contract_error.ContractIoError) {
  contract_io.record_recovered_inputs_if_contracted(
    issue,
    dag,
    orchestrator,
    contract_io.RecoveredInvocation(
      run_id: recovered.run_id,
      workflow_fingerprint: recovered.workflow_fingerprint,
      steps_started: dict.size(recovered.artifacts) > 0
        || dict.size(recovered.prepared_workspaces) > 0
        || dict.size(recovered.step_attempts) > 0,
      contract_inputs_recorded: recovered.contract_inputs_recorded,
    ),
    dependencies.checkpoint,
    profile,
  )
}

fn record_inputs_if_contracted(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  invocation: RunInvocation,
  dependencies: Dependencies,
  profile: config_types.WorkspaceHookProfile,
) -> Result(Nil, contract_error.ContractIoError) {
  contract_io.record_inputs_if_contracted(
    issue,
    dag,
    orchestrator,
    contract_io.RunInvocation(
      run_id: invocation.run_id,
      workflow_fingerprint: invocation.workflow_fingerprint,
      supplied_contract_values: contract_io.ContractRunValues(
        inputs: invocation.supplied_contract_values.inputs,
        context: invocation.supplied_contract_values.context,
      ),
      scheduled_context: option.map(invocation.scheduled_context, fn(scheduled) {
        contract_io.ScheduledInvocationContext(
          job_id: scheduled.job_id,
          workflow_id: scheduled.workflow_id,
          due_at: scheduled.due_at,
          started_at: scheduled.started_at,
          run_id: scheduled.run_id,
          attempt: scheduled.attempt,
        )
      }),
    ),
    dependencies.checkpoint,
    profile,
  )
}

fn record_outputs_if_contracted(
  dag: workflow_dag.WorkflowDag,
  run_id: String,
  workflow_fingerprint: String,
  contract_outputs_recorded: Option(workflow_checkpoint.ArtifactWritten),
  dependencies: Dependencies,
  artifacts: Dict(String, step_artifact.StepArtifact),
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
) -> Result(contract_io.ContractOutputsResult, contract_error.ContractIoError) {
  contract_io.record_outputs_if_contracted(
    dag,
    run_id,
    workflow_fingerprint,
    contract_outputs_recorded,
    dependencies.checkpoint,
    artifacts,
    prepared_workspaces,
  )
}

fn emit_workstream_handoff_if_configured(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  run_id: String,
  workflow_fingerprint: String,
  outputs: contract_io.ContractOutputsResult,
  dependencies: Dependencies,
) -> Result(Nil, workstream_handoff.HandoffError) {
  workstream_handoff.emit_if_configured(
    issue,
    dag,
    run_id,
    workflow_fingerprint,
    outputs,
    dependencies.checkpoint,
  )
}

fn record_publications_if_configured(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  outputs: contract_io.ContractOutputsResult,
  run_id: String,
  dependencies: Dependencies,
) -> Result(
  artifact_publication_recording.PublicationRecordingResult,
  PublicationRecordingError,
) {
  case outputs.manifest {
    Some(output_manifest) ->
      artifact_publication_executor.execute_routes(
        dag.publication_routes,
        orchestrator.artifact_repositories,
        orchestrator.config_dir,
        output_manifest,
        issue,
        run_id,
        dependencies.checkpoint,
      )
      |> result.map_error(PublicationRecordingFailed)
    None ->
      Ok(
        artifact_publication_recording.PublicationRecordingResult(
          required_failures: [],
          optional_failures: [],
          attempts: [],
        ),
      )
  }
}

fn publication_recording_error_reason(
  error: PublicationRecordingError,
) -> String {
  case error {
    PublicationRecordingFailed(reason) -> reason
  }
}

pub fn failure_report(failure: WorkflowRunFailure) -> String {
  case failed_command_artifact(failure) {
    Some(artifact) ->
      case step_artifact.command_failure_summary(artifact) {
        Some(summary) ->
          workflow_command_failure_prefix(artifact)
          <> failure.reason
          <> "\n"
          <> summary
        None -> failure.reason
      }
    None -> failure.reason
  }
}

pub fn failed_command_failure(
  failure: WorkflowRunFailure,
) -> Option(#(String, String)) {
  case failed_command_artifact(failure) {
    Some(artifact) ->
      case artifact.failure_code {
        Some(code) -> Some(#(code, artifact.step_id))
        None -> None
      }
    None -> None
  }
}

fn failed_command_artifact(
  failure: WorkflowRunFailure,
) -> Option(step_artifact.StepArtifact) {
  case failure.failed_step_id {
    Some(step_id) ->
      case dict.get(failure.artifacts, step_id) {
        Ok(artifact) -> Some(artifact)
        Error(Nil) -> None
      }
    None -> None
  }
}

fn workflow_command_failure_prefix(
  artifact: step_artifact.StepArtifact,
) -> String {
  case artifact.failure_code {
    Some(code) -> "workflow_command_failed:" <> code <> "\n"
    None -> ""
  }
}

fn loop(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  run_id: String,
  workflow_fingerprint: String,
  contract_outputs_recorded: Option(workflow_checkpoint.ArtifactWritten),
  recovery_evidence: workflow_outcome.RecoveryEvidence,
  recovered_execution: Bool,
  dependencies: Dependencies,
  scheduler_state: workflow_scheduler.SchedulerState,
  artifacts: Dict(String, step_artifact.StepArtifact),
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
  run_root: Option(String),
  attempt_indexes: Dict(String, Int),
  tokens: session_tokens.TokenTotals,
  final_issue: Option(tracker_issue.Issue),
  turns: Int,
  cleanup_allowed: Bool,
  pi_session_continuations: Dict(String, workflow_attempt.PiContinuation),
  profile: config_types.WorkspaceHookProfile,
) -> Result(WorkflowRunSuccess, WorkflowRunFailure) {
  case workflow_scheduler.outcome(dag, scheduler_state) {
    workflow_scheduler.WorkflowSucceeded -> {
      let result =
        step_artifact.workflow_result_artifact(
          dag,
          artifacts,
          orchestrator.artifact_limits,
        )
      let final_issue = option.unwrap(final_issue, issue)
      let workspace_path = option.unwrap(run_root, "")
      case
        record_outputs_if_contracted(
          dag,
          run_id,
          workflow_fingerprint,
          contract_outputs_recorded,
          dependencies,
          artifacts,
          prepared_workspaces,
        )
      {
        Ok(outputs) if outputs.missing == [] ->
          case
            record_publications_if_configured(
              final_issue,
              dag,
              orchestrator,
              outputs,
              run_id,
              dependencies,
            )
          {
            Ok(publication_result) ->
              case publication_result.required_failures {
                [] ->
                  case
                    emit_workstream_handoff_if_configured(
                      issue,
                      dag,
                      run_id,
                      workflow_fingerprint,
                      outputs,
                      dependencies,
                    )
                  {
                    Ok(Nil) -> {
                      use Nil <- result_try_checkpoint(
                        dependencies.checkpoint.workflow_finished(
                          workflow_checkpoint.WorkflowFinished(
                            run_id: run_id,
                            workflow_id: dag.id,
                            issue_id: issue.id,
                            task_ref: task_ref(issue),
                            outcome: workflow_outcome.terminal_success(
                              recovery_evidence,
                            ),
                            token_total: tokens.total,
                            turns: turns,
                          ),
                        ),
                        artifacts,
                        run_root,
                        None,
                      )
                      let cleanup_result =
                        cleanup_if_allowed(
                          run_root,
                          orchestrator,
                          profile,
                          dependencies,
                          cleanup_allowed,
                        )
                      case cleanup_result {
                        Ok(Nil) -> {
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
                            cleanup_warning: None,
                          ))
                        }
                        Error(err) -> {
                          let cleanup_code = error.workspace_code(err)
                          let cleanup_reason =
                            "post_success_cleanup_failed:"
                            <> cleanup_code
                            <> "; run_root="
                            <> workspace_path
                          let warning_message = case
                            dependencies.checkpoint.workflow_diagnostic(
                              workflow_checkpoint.WorkflowDiagnostic(
                                run_id: run_id,
                                workflow_id: dag.id,
                                issue_id: issue.id,
                                reason: cleanup_reason,
                              ),
                            )
                          {
                            Ok(Nil) -> cleanup_reason
                            Error(checkpoint_error) ->
                              cleanup_reason
                              <> "; diagnostic_append_failed:"
                              <> workflow_checkpoint.describe_error(
                                checkpoint_error,
                              )
                          }
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
                            cleanup_warning: Some(PostSuccessCleanupWarning(
                              code: cleanup_code,
                              message: warning_message,
                              run_root: workspace_path,
                            )),
                          ))
                        }
                      }
                    }
                    Error(error) -> {
                      let reason = workstream_handoff.describe_error(error)
                      use Nil <- result_try_checkpoint(
                        dependencies.checkpoint.workflow_finished(
                          workflow_checkpoint.WorkflowFinished(
                            run_id: run_id,
                            workflow_id: dag.id,
                            issue_id: issue.id,
                            task_ref: task_ref(issue),
                            outcome: workflow_outcome.terminal_failed_fatal(
                              recovery_evidence,
                            ),
                            token_total: tokens.total,
                            turns: turns,
                          ),
                        ),
                        artifacts,
                        run_root,
                        None,
                      )
                      let cleanup_suffix =
                        cleanup_failure_suffix(cleanup_if_allowed(
                          run_root,
                          orchestrator,
                          profile,
                          dependencies,
                          cleanup_allowed,
                        ))
                      Error(WorkflowRunFailure(
                        reason: "workflow_workstream_handoff_failed:"
                          <> reason
                          <> cleanup_suffix,
                        agent_reason: None,
                        artifacts: artifacts,
                        run_root: run_root,
                        failed_step_id: None,
                      ))
                    }
                  }
                [failure, ..] -> {
                  use Nil <- result_try_checkpoint(
                    dependencies.checkpoint.workflow_finished(
                      workflow_checkpoint.WorkflowFinished(
                        run_id: run_id,
                        workflow_id: dag.id,
                        issue_id: issue.id,
                        task_ref: task_ref(issue),
                        outcome: workflow_outcome.terminal_failed_fatal(
                          recovery_evidence,
                        ),
                        token_total: tokens.total,
                        turns: turns,
                      ),
                    ),
                    artifacts,
                    run_root,
                    None,
                  )
                  let cleanup_suffix =
                    cleanup_failure_suffix(cleanup_if_allowed(
                      run_root,
                      orchestrator,
                      profile,
                      dependencies,
                      cleanup_allowed,
                    ))
                  Error(WorkflowRunFailure(
                    reason: "workflow_publication_required_failed:"
                      <> failure.publication_id
                      <> ":"
                      <> failure.code
                      <> cleanup_suffix,
                    agent_reason: None,
                    artifacts: artifacts,
                    run_root: run_root,
                    failed_step_id: None,
                  ))
                }
              }
            Error(error) -> {
              let reason = publication_recording_error_reason(error)
              use Nil <- result_try_checkpoint(
                dependencies.checkpoint.workflow_finished(
                  workflow_checkpoint.WorkflowFinished(
                    run_id: run_id,
                    workflow_id: dag.id,
                    issue_id: issue.id,
                    task_ref: task_ref(issue),
                    outcome: workflow_outcome.terminal_failed_fatal(
                      recovery_evidence,
                    ),
                    token_total: tokens.total,
                    turns: turns,
                  ),
                ),
                artifacts,
                run_root,
                None,
              )
              let cleanup_suffix =
                cleanup_failure_suffix(cleanup_if_allowed(
                  run_root,
                  orchestrator,
                  profile,
                  dependencies,
                  cleanup_allowed,
                ))
              Error(WorkflowRunFailure(
                reason: "workflow_publication_recording_failed:"
                  <> reason
                  <> cleanup_suffix,
                agent_reason: None,
                artifacts: artifacts,
                run_root: run_root,
                failed_step_id: None,
              ))
            }
          }
        Ok(outputs) -> {
          let missing = case outputs.missing {
            [missing, ..] -> missing
            [] -> "unknown"
          }
          use Nil <- result_try_checkpoint(
            dependencies.checkpoint.workflow_finished(
              workflow_checkpoint.WorkflowFinished(
                run_id: run_id,
                workflow_id: dag.id,
                issue_id: issue.id,
                task_ref: task_ref(issue),
                outcome: workflow_outcome.terminal_failed_fatal(
                  recovery_evidence,
                ),
                token_total: tokens.total,
                turns: turns,
              ),
            ),
            artifacts,
            run_root,
            None,
          )
          let cleanup_suffix =
            cleanup_failure_suffix(cleanup_if_allowed(
              run_root,
              orchestrator,
              profile,
              dependencies,
              cleanup_allowed,
            ))
          Error(WorkflowRunFailure(
            reason: "workflow_required_output_missing:"
              <> missing
              <> cleanup_suffix,
            agent_reason: None,
            artifacts: artifacts,
            run_root: run_root,
            failed_step_id: None,
          ))
        }
        Error(error) -> {
          let reason = contract_error.describe_error(error)
          use Nil <- result_try_checkpoint(
            dependencies.checkpoint.workflow_finished(
              workflow_checkpoint.WorkflowFinished(
                run_id: run_id,
                workflow_id: dag.id,
                issue_id: issue.id,
                task_ref: task_ref(issue),
                outcome: workflow_outcome.terminal_failed_fatal(
                  recovery_evidence,
                ),
                token_total: tokens.total,
                turns: turns,
              ),
            ),
            artifacts,
            run_root,
            None,
          )
          let cleanup_suffix =
            cleanup_failure_suffix(cleanup_if_allowed(
              run_root,
              orchestrator,
              profile,
              dependencies,
              cleanup_allowed,
            ))
          Error(WorkflowRunFailure(
            reason: "workflow_output_manifest_failed:"
              <> reason
              <> cleanup_suffix,
            agent_reason: None,
            artifacts: artifacts,
            run_root: run_root,
            failed_step_id: None,
          ))
        }
      }
    }
    workflow_scheduler.WorkflowFailed -> {
      let output_suffix = case
        record_outputs_if_contracted(
          dag,
          run_id,
          workflow_fingerprint,
          contract_outputs_recorded,
          dependencies,
          artifacts,
          prepared_workspaces,
        )
      {
        Ok(_) -> ""
        Error(error) ->
          "; workflow_output_manifest_failed:"
          <> contract_error.describe_error(error)
      }
      let cleanup_suffix =
        cleanup_failure_suffix(cleanup_if_allowed(
          run_root,
          orchestrator,
          profile,
          dependencies,
          cleanup_allowed,
        ))
      use Nil <- result_try_checkpoint(
        dependencies.checkpoint.workflow_finished(
          workflow_checkpoint.WorkflowFinished(
            run_id: run_id,
            workflow_id: dag.id,
            issue_id: issue.id,
            task_ref: task_ref(issue),
            outcome: workflow_outcome.terminal_failed_fatal(recovery_evidence),
            token_total: tokens.total,
            turns: turns,
          ),
        ),
        artifacts,
        run_root,
        None,
      )
      Error(WorkflowRunFailure(
        reason: "workflow_step_failed" <> output_suffix <> cleanup_suffix,
        agent_reason: None,
        artifacts: artifacts,
        run_root: run_root,
        failed_step_id: None,
      ))
    }
    workflow_scheduler.WorkflowInProgress -> {
      let ready = workflow_scheduler.ready_steps(dag, scheduler_state)
      case ready {
        [] -> {
          mark_workflow_failed_terminal(
            dependencies,
            recovery_evidence,
            run_id,
            dag.id,
            issue.id,
            task_ref(issue),
            tokens.total,
            turns,
            [],
          )
          let cleanup_suffix =
            cleanup_failure_suffix(cleanup_if_allowed(
              run_root,
              orchestrator,
              profile,
              dependencies,
              cleanup_allowed,
            ))
          Error(WorkflowRunFailure(
            reason: "workflow_deadlocked" <> cleanup_suffix,
            agent_reason: None,
            artifacts: artifacts,
            run_root: run_root,
            failed_step_id: None,
          ))
        }
        steps -> {
          let steps =
            select_workspace_serial_batch(
              steps,
              issue,
              dag.id,
              run_id,
              orchestrator,
              attempt_indexes,
              dict.new(),
              [],
            )
          case
            prepare_ready_steps(
              steps,
              issue,
              dag.id,
              run_id,
              orchestrator,
              dependencies,
              secrets,
              prepared_workspaces,
              run_root,
              recovered_execution,
              attempt_indexes,
              [],
              profile,
            )
          {
            Error(PrepareReadyFailure(
              reason,
              agent_reason,
              prepared_run_root,
              prepared_starts,
            )) -> {
              let failure_run_root = option.or(prepared_run_root, run_root)
              mark_workflow_failed_terminal(
                dependencies,
                recovery_evidence,
                run_id,
                dag.id,
                issue.id,
                task_ref(issue),
                tokens.total,
                turns,
                prepared_starts,
              )
              let cleanup_suffix =
                cleanup_failure_suffix(cleanup_if_allowed(
                  failure_run_root,
                  orchestrator,
                  profile,
                  dependencies,
                  cleanup_allowed,
                ))
              Error(WorkflowRunFailure(
                reason: reason <> cleanup_suffix,
                agent_reason: agent_reason,
                artifacts: artifacts,
                run_root: failure_run_root,
                failed_step_id: None,
              ))
            }
            Ok(prepared) -> {
              let #(
                prepared_starts,
                prepared_workspaces,
                run_root,
                attempt_indexes,
              ) = prepared
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
                workflow_fingerprint,
                contract_outputs_recorded,
                recovery_evidence,
                dependencies,
                scheduler_state,
                artifacts,
                prepared_workspaces,
                run_root,
                attempt_indexes,
                tokens,
                final_issue,
                turns,
                cleanup_allowed,
                recovered_execution,
                pi_session_continuations,
                profile,
              )
            }
          }
        }
      }
    }
  }
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
  let attempt_index =
    dict.get(attempt_indexes, step.id)
    |> result.unwrap(1)
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
  issue: tracker_issue.Issue,
  workflow_id: String,
  run_id: String,
  orchestrator: config_types.OrchestratorConfig,
  dependencies: Dependencies,
  secrets: List(String),
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
  current_run_root: Option(String),
  recovered_execution: Bool,
  attempt_indexes: Dict(String, Int),
  acc: List(PreparedStart),
  profile: config_types.WorkspaceHookProfile,
) -> Result(
  #(
    List(PreparedStart),
    Dict(String, workspace_run.PreparedStepWorkspace),
    Option(String),
    Dict(String, Int),
  ),
  PrepareReadyFailure,
) {
  case steps {
    [] -> {
      let run_root = option.or(prepared_run_root(acc), current_run_root)
      Ok(#(list.reverse(acc), prepared_workspaces, run_root, attempt_indexes))
    }
    [step, ..rest] -> {
      let attempt_index =
        dict.get(attempt_indexes, step.id)
        |> result.unwrap(1)
      let next_attempt_indexes =
        dict.insert(attempt_indexes, step.id, attempt_index + 1)
      case
        prepare_step_for_mode(
          dependencies,
          recovered_execution,
          current_run_root,
          issue,
          workflow_id,
          run_id,
          step.id,
          attempt_index,
          step.workspace,
          orchestrator,
          profile,
          prepared_workspaces,
        )
      {
        Error(workspace_run.WorkspaceFailure(err)) ->
          Error(PrepareReadyFailure(
            "workspace_failed:" <> error.workspace_code(err),
            None,
            option.or(prepared_run_root(acc), current_run_root),
            list.reverse(acc),
          ))
        Error(workspace_run.HookFailure(err)) ->
          Error(PrepareReadyFailure(
            hook_failure_report(err, secrets),
            Some(error.WorkflowHookFailed(err)),
            option.or(prepared_run_root(acc), current_run_root),
            list.reverse(acc),
          ))
        Ok(prepared) -> {
          case
            dependencies.checkpoint.step_prepared(
              run_id,
              workflow_id,
              step.id,
              prepared,
            )
          {
            Error(error) ->
              Error(PrepareReadyFailure(
                "checkpoint_failed:"
                  <> workflow_checkpoint.describe_error(error),
                None,
                prepared_run_root([
                  PreparedStart(step: step, workspace: prepared),
                  ..acc
                ]),
                list.reverse([
                  PreparedStart(step: step, workspace: prepared),
                  ..acc
                ]),
              ))
            Ok(Nil) -> {
              let prepared_workspaces =
                dict.insert(prepared_workspaces, step.workspace.name, prepared)
              prepare_ready_steps(
                rest,
                issue,
                workflow_id,
                run_id,
                orchestrator,
                dependencies,
                secrets,
                prepared_workspaces,
                current_run_root,
                recovered_execution,
                next_attempt_indexes,
                [PreparedStart(step: step, workspace: prepared), ..acc],
                profile,
              )
            }
          }
        }
      }
    }
  }
}

fn prepare_step_for_mode(
  dependencies: Dependencies,
  recovered_execution: Bool,
  current_run_root: Option(String),
  issue: tracker_issue.Issue,
  workflow_id: String,
  run_id: String,
  step_id: String,
  attempt_index: Int,
  workspace_ref: workflow_dag.WorkspaceRef,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
) -> Result(workspace_run.PreparedStepWorkspace, workspace_run.PrepareError) {
  case recovered_execution, current_run_root {
    True, Some(expected_run_root) ->
      dependencies.prepare_recovered_step(
        issue,
        workflow_id,
        run_id,
        expected_run_root,
        step_id,
        attempt_index,
        workspace_ref,
        orchestrator,
        profile,
        prepared_workspaces,
      )
    _, _ ->
      dependencies.prepare_step(
        issue,
        workflow_id,
        run_id,
        step_id,
        attempt_index,
        workspace_ref,
        orchestrator,
        profile,
        prepared_workspaces,
      )
  }
}

// The scheduler can return a ready batch larger than one when independent steps
// have different logical workspaces. The runner prepares the whole selected
// batch before starting any of those steps, then executes the batch concurrently
// and applies results back in DAG order so downstream prompts see deterministic
// artifact state.
fn execute_prepared_steps(
  starts: List(PreparedStart),
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  run_id: String,
  workflow_fingerprint: String,
  contract_outputs_recorded: Option(workflow_checkpoint.ArtifactWritten),
  recovery_evidence: workflow_outcome.RecoveryEvidence,
  dependencies: Dependencies,
  scheduler_state: workflow_scheduler.SchedulerState,
  artifacts: Dict(String, step_artifact.StepArtifact),
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
  run_root: Option(String),
  attempt_indexes: Dict(String, Int),
  tokens: session_tokens.TokenTotals,
  final_issue: Option(tracker_issue.Issue),
  turns: Int,
  cleanup_allowed: Bool,
  recovered_execution: Bool,
  pi_session_continuations: Dict(String, workflow_attempt.PiContinuation),
  profile: config_types.WorkspaceHookProfile,
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
        workflow_fingerprint,
        contract_outputs_recorded,
        recovery_evidence,
        recovered_execution,
        dependencies,
        scheduler_state,
        artifacts,
        prepared_workspaces,
        run_root,
        attempt_indexes,
        tokens,
        final_issue,
        turns,
        cleanup_allowed,
        pi_session_continuations,
        profile,
      )
    _ -> {
      case
        run_prepared_batch(
          starts,
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
      {
        Error(StepBatchStartError(reason, batch_cleanup_allowed)) -> {
          mark_workflow_failed_terminal(
            dependencies,
            recovery_evidence,
            run_id,
            dag.id,
            issue.id,
            task_ref(issue),
            tokens.total,
            turns,
            starts,
          )
          let cleanup_suffix =
            cleanup_failure_suffix(cleanup_if_allowed(
              run_root,
              orchestrator,
              profile,
              dependencies,
              cleanup_allowed || batch_cleanup_allowed,
            ))
          Error(WorkflowRunFailure(
            reason: reason <> cleanup_suffix,
            agent_reason: None,
            artifacts: artifacts,
            run_root: run_root,
            failed_step_id: None,
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
            workflow_fingerprint,
            contract_outputs_recorded,
            recovery_evidence,
            dependencies,
            scheduler_state,
            artifacts,
            prepared_workspaces,
            run_root,
            attempt_indexes,
            tokens,
            final_issue,
            turns,
            True,
            recovered_execution,
            pi_session_continuations,
            profile,
          )
        }
        Ok(StepBatchFatal(result)) ->
          finish_fatal_batch_result(
            starts,
            result,
            issue,
            dag,
            orchestrator,
            tracker_client,
            secrets,
            run_id,
            workflow_fingerprint,
            contract_outputs_recorded,
            recovery_evidence,
            dependencies,
            scheduler_state,
            artifacts,
            prepared_workspaces,
            run_root,
            attempt_indexes,
            tokens,
            final_issue,
            turns,
            True,
            recovered_execution,
            pi_session_continuations,
            profile,
          )
      }
    }
  }
}

fn run_prepared_batch(
  starts: List(PreparedStart),
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  dependencies: Dependencies,
  artifacts: Dict(String, step_artifact.StepArtifact),
  pi_session_continuations: Dict(String, workflow_attempt.PiContinuation),
  profile: config_types.WorkspaceHookProfile,
) -> Result(StepBatchOutcome, StepBatchStartError) {
  let was_trapping_exits = process_ext.trap_exits(True)
  let subject = process.new_subject()
  let spawned =
    spawn_prepared_steps(
      starts,
      subject,
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
        |> result.map_error(fn(error) {
          StepBatchStartError(step_batch_error_reason(error), True)
        })
      let _previous_trap_exits = process_ext.trap_exits(was_trapping_exits)
      result
    }
  }
}

fn spawn_prepared_steps(
  starts: List(PreparedStart),
  subject: process.Subject(StepExecutionResult),
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  dependencies: Dependencies,
  artifacts: Dict(String, step_artifact.StepArtifact),
  pi_session_continuations: Dict(String, workflow_attempt.PiContinuation),
  profile: config_types.WorkspaceHookProfile,
) -> Result(List(SpawnedStepWorker), StepBatchStartError) {
  spawn_prepared_steps_loop(
    starts,
    subject,
    issue,
    dag,
    orchestrator,
    tracker_client,
    secrets,
    dependencies,
    artifacts,
    pi_session_continuations,
    profile,
    [],
  )
}

fn spawn_prepared_steps_loop(
  starts: List(PreparedStart),
  subject: process.Subject(StepExecutionResult),
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  dependencies: Dependencies,
  artifacts: Dict(String, step_artifact.StepArtifact),
  pi_session_continuations: Dict(String, workflow_attempt.PiContinuation),
  profile: config_types.WorkspaceHookProfile,
  acc: List(SpawnedStepWorker),
) -> Result(List(SpawnedStepWorker), StepBatchStartError) {
  case starts {
    [] -> Ok(list.reverse(acc))
    [PreparedStart(step, workspace), ..rest] -> {
      let session_id =
        workflow_identity.step_session_id(
          workspace.run_id,
          step.id,
          workspace.attempt_index,
        )
      let start_result = case dict.get(pi_session_continuations, step.id) {
        Ok(continuation) ->
          dependencies.checkpoint.step_continuation_started(
            workspace.run_id,
            workspace.workflow_id,
            step.id,
            workspace.attempt_index,
            continuation.session_id,
          )
        Error(Nil) ->
          dependencies.checkpoint.step_started(
            workspace.run_id,
            workspace.workflow_id,
            step.id,
            workspace.attempt_index,
            session_id,
            None,
            step_is_continuation_capable(step, orchestrator),
          )
      }
      case start_result {
        Error(error) -> {
          terminate_step_workers(monitor_to_pid(acc, dict.new()))
          Error(StepBatchStartError(
            reason: "checkpoint_failed:"
              <> workflow_checkpoint.describe_error(error),
            cleanup_allowed: acc != [],
          ))
        }
        Ok(Nil) -> {
          let pid =
            process.spawn(fn() {
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
            dag,
            orchestrator,
            tracker_client,
            secrets,
            dependencies,
            artifacts,
            pi_session_continuations,
            profile,
            [
              SpawnedStepWorker(step_id: step.id, pid: pid, monitor: monitor),
              ..acc
            ],
          )
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

fn collect_step_results(
  remaining: Int,
  selector: process.Selector(StepBatchMessage),
  monitor_to_step: Dict(process.Monitor, String),
  step_to_monitor: Dict(String, process.Monitor),
  monitor_to_pid: Dict(process.Monitor, process.Pid),
  failure_policies: Dict(String, workflow_dag.FailurePolicy),
  acc: List(StepExecutionResult),
) -> Result(StepBatchOutcome, StepBatchError) {
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
) -> Result(StepBatchOutcome, StepBatchError) {
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
          Error(StepWorkerDown(step_id, reason))
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

fn step_batch_error_reason(error: StepBatchError) -> String {
  case error {
    StepWorkerDown(step_id, reason) -> step_worker_down_reason(step_id, reason)
  }
}

fn finish_fatal_batch_result(
  starts: List(PreparedStart),
  result: StepExecutionResult,
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  run_id: String,
  workflow_fingerprint: String,
  contract_outputs_recorded: Option(workflow_checkpoint.ArtifactWritten),
  recovery_evidence: workflow_outcome.RecoveryEvidence,
  dependencies: Dependencies,
  scheduler_state: workflow_scheduler.SchedulerState,
  artifacts: Dict(String, step_artifact.StepArtifact),
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
  run_root: Option(String),
  attempt_indexes: Dict(String, Int),
  tokens: session_tokens.TokenTotals,
  final_issue: Option(tracker_issue.Issue),
  turns: Int,
  cleanup_allowed: Bool,
  recovered_execution: Bool,
  pi_session_continuations: Dict(String, workflow_attempt.PiContinuation),
  profile: config_types.WorkspaceHookProfile,
) -> Result(WorkflowRunSuccess, WorkflowRunFailure) {
  let artifacts = dict.insert(artifacts, result.step_id, result.artifact)
  case prepared_start_by_step(starts, result.step_id) {
    Error(Nil) ->
      terminal_fatal_batch_failure(
        starts,
        result,
        issue,
        dag,
        run_id,
        workflow_fingerprint,
        contract_outputs_recorded,
        recovery_evidence,
        orchestrator,
        dependencies,
        artifacts,
        prepared_workspaces,
        run_root,
        tokens.total,
        turns,
        cleanup_allowed,
        profile,
        checkpoint_error: None,
      )
    Ok(PreparedStart(step, workspace)) -> {
      let finished =
        workflow_checkpoint.StepFinished(
          run_id: run_id,
          workflow_id: dag.id,
          step_id: step.id,
          attempt_index: workspace.attempt_index,
          outcome: workflow_outcome.failed_fatal,
          workspace_name: workspace.workspace_name,
          workspace_path: workspace.path,
          token_total: result.tokens.total,
          turns: result.turns,
        )
      case
        finalize_step_attempt(
          dependencies,
          issue,
          step.id,
          workspace,
          orchestrator,
          profile,
          finished,
          result.artifact,
        )
      {
        Error(error) ->
          terminal_fatal_batch_failure(
            starts,
            result,
            issue,
            dag,
            run_id,
            workflow_fingerprint,
            contract_outputs_recorded,
            recovery_evidence,
            orchestrator,
            dependencies,
            artifacts,
            prepared_workspaces,
            run_root,
            tokens.total,
            turns,
            cleanup_allowed,
            profile,
            checkpoint_error: Some(error),
          )
        Ok(_) ->
          case
            effective_recovery_for_failure(dag, step, workspace.attempt_index)
          {
            Some(config) ->
              case
                execute_step_recovery(
                  step,
                  workspace,
                  result.artifact,
                  config,
                  issue,
                  dag,
                  orchestrator,
                  tracker_client,
                  secrets,
                  dependencies,
                  profile,
                )
              {
                RecoveryRetryRequested(
                  recovery_tokens,
                  recovery_final_issue,
                  recovery_turns,
                ) -> {
                  let scheduler_state =
                    mark_batch_pending(scheduler_state, starts)
                  let tokens =
                    add_tokens(
                      add_tokens(tokens, result.tokens),
                      recovery_tokens,
                    )
                  let final_issue =
                    latest_final_issue(final_issue, recovery_final_issue)
                  loop(
                    issue,
                    dag,
                    orchestrator,
                    tracker_client,
                    secrets,
                    run_id,
                    workflow_fingerprint,
                    contract_outputs_recorded,
                    workflow_outcome.StepRecoveryRetryRequested,
                    recovered_execution,
                    dependencies,
                    scheduler_state,
                    artifacts,
                    prepared_workspaces,
                    run_root,
                    attempt_indexes,
                    tokens,
                    final_issue,
                    turns + result.turns + recovery_turns,
                    cleanup_allowed,
                    pi_session_continuations,
                    profile,
                  )
                }
                RecoveryStop(
                  recovery_tokens,
                  _,
                  recovery_turns,
                  stop_recovery_evidence,
                ) -> {
                  let recovery_evidence =
                    combine_recovery_evidence(
                      recovery_evidence,
                      stop_recovery_evidence,
                    )
                  terminal_fatal_batch_failure(
                    starts,
                    result,
                    issue,
                    dag,
                    run_id,
                    workflow_fingerprint,
                    contract_outputs_recorded,
                    recovery_evidence,
                    orchestrator,
                    dependencies,
                    artifacts,
                    prepared_workspaces,
                    run_root,
                    tokens.total + result.tokens.total + recovery_tokens.total,
                    turns + result.turns + recovery_turns,
                    cleanup_allowed,
                    profile,
                    checkpoint_error: None,
                  )
                }
              }
            None ->
              terminal_fatal_batch_failure(
                starts,
                result,
                issue,
                dag,
                run_id,
                workflow_fingerprint,
                contract_outputs_recorded,
                recovery_evidence,
                orchestrator,
                dependencies,
                artifacts,
                prepared_workspaces,
                run_root,
                tokens.total + result.tokens.total,
                turns + result.turns,
                cleanup_allowed,
                profile,
                checkpoint_error: None,
              )
          }
      }
    }
  }
}

fn terminal_fatal_batch_failure(
  starts: List(PreparedStart),
  result: StepExecutionResult,
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  run_id: String,
  workflow_fingerprint: String,
  contract_outputs_recorded: Option(workflow_checkpoint.ArtifactWritten),
  recovery_evidence: workflow_outcome.RecoveryEvidence,
  orchestrator: config_types.OrchestratorConfig,
  dependencies: Dependencies,
  artifacts: Dict(String, step_artifact.StepArtifact),
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
  run_root: Option(String),
  workflow_finished_token_total: Int,
  workflow_finished_turns: Int,
  cleanup_allowed: Bool,
  profile: config_types.WorkspaceHookProfile,
  checkpoint_error checkpoint_error: Option(workflow_checkpoint.CheckpointError),
) -> Result(WorkflowRunSuccess, WorkflowRunFailure) {
  let reason = case checkpoint_error {
    Some(error) ->
      "checkpoint_failed:" <> workflow_checkpoint.describe_error(error)
    None -> workflow_step_failed_reason(result)
  }
  let output_suffix = case checkpoint_error {
    Some(_) -> ""
    None ->
      case
        record_outputs_if_contracted(
          dag,
          run_id,
          workflow_fingerprint,
          contract_outputs_recorded,
          dependencies,
          artifacts,
          prepared_workspaces,
        )
      {
        Ok(_) -> ""
        Error(error) ->
          "; workflow_output_manifest_failed:"
          <> contract_error.describe_error(error)
      }
  }
  mark_prepared_attempts_interrupted(
    starts,
    dependencies,
    dag.id,
    "fatal_sibling_finished",
    Some(result.step_id),
  )
  ignore_secondary_checkpoint_result(
    dependencies.checkpoint.workflow_finished(
      workflow_checkpoint.WorkflowFinished(
        run_id: run_id,
        workflow_id: dag.id,
        issue_id: issue.id,
        task_ref: task_ref(issue),
        outcome: workflow_outcome.terminal_failed_fatal(recovery_evidence),
        token_total: workflow_finished_token_total,
        turns: workflow_finished_turns,
      ),
    ),
  )
  let cleanup_suffix =
    cleanup_failure_suffix(cleanup_if_allowed(
      run_root,
      orchestrator,
      profile,
      dependencies,
      cleanup_allowed,
    ))
  Error(WorkflowRunFailure(
    reason: reason <> output_suffix <> cleanup_suffix,
    agent_reason: agent_reason_for_artifact(result.artifact),
    artifacts: artifacts,
    run_root: run_root,
    failed_step_id: Some(result.step_id),
  ))
}

fn finalize_step_attempt(
  dependencies: Dependencies,
  issue: tracker_issue.Issue,
  step_id: String,
  workspace: workspace_run.PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
  finished: workflow_checkpoint.StepFinished,
  artifact: step_artifact.StepArtifact,
) -> Result(
  workflow_checkpoint.ArtifactWritten,
  workflow_checkpoint.CheckpointError,
) {
  use artifact_ref <- result.try(dependencies.checkpoint.write_step_artifact(
    finished,
    artifact,
  ))
  use Nil <- result.try(
    run_after_step(
      dependencies,
      issue,
      step_id,
      workspace,
      orchestrator,
      profile,
    )
    |> result.map_error(fn(error) {
      workflow_checkpoint.CheckpointAppendFailed(after_step_error_reason(error))
    }),
  )
  use Nil <- result.try(dependencies.checkpoint.step_finished(
    finished,
    artifact_ref,
  ))
  Ok(artifact_ref)
}

fn effective_recovery_for_failure(
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

fn execute_step_recovery(
  step: workflow_dag.WorkflowStep,
  workspace: workspace_run.PreparedStepWorkspace,
  failed_artifact: step_artifact.StepArtifact,
  config: workflow_dag.EffectiveRecoveryConfig,
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  dependencies: Dependencies,
  profile: config_types.WorkspaceHookProfile,
) -> RecoveryAttemptOutcome {
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
      let checkpoint_root = checkpoint_workspace_root(orchestrator)
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
            recovery_context(step_context(
              step,
              workspace,
              issue,
              orchestrator,
              profile,
            ))
          {
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
                workflow_step_recovery.prompt(
                  prompt_ref,
                  step.id,
                  workspace.attempt_index,
                  failed_artifact,
                )
              let prompt_mode = workflow_attempt.StepRecoveryPrompt(prompt)
              let effective =
                effective_for_recovery(orchestrator, step, config.model)
              let attempt_context =
                workflow_attempt_context(
                  context,
                  dag,
                  orchestrator,
                  prompt_mode,
                  None,
                )
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
                      apply_recovery_success(
                        step,
                        workspace,
                        recovery_attempt_number,
                        recovery_session_id,
                        success,
                        secrets,
                        dependencies,
                        guard_reason_suffix(events),
                      )
                    Error(guard_error) ->
                      stop_recovery_after_guard_failure(
                        step,
                        workspace,
                        recovery_attempt_number,
                        recovery_session_id,
                        success.tokens,
                        success.final_issue,
                        success.turns,
                        guard_error,
                        secrets,
                        dependencies,
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
                        append_recovery_reason_suffix(
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
                      stop_recovery_after_guard_failure(
                        step,
                        workspace,
                        recovery_attempt_number,
                        recovery_session_id,
                        failure.tokens,
                        failure.final_issue,
                        0,
                        guard_error,
                        secrets,
                        dependencies,
                      )
                  }
              }
            }
          }
      }
    }
  }
}

fn apply_recovery_success(
  step: workflow_dag.WorkflowStep,
  workspace: workspace_run.PreparedStepWorkspace,
  recovery_attempt_number: Int,
  recovery_session_id: String,
  success: agent_types.WorkerSuccess,
  secrets: List(String),
  dependencies: Dependencies,
  reason_suffix: String,
) -> RecoveryAttemptOutcome {
  case workflow_step_recovery.decision(success) {
    Ok(workflow_step_recovery.RetryRequested(summary, reason)) ->
      case
        workflow_step_recovery.record_decision(
          dependencies.checkpoint,
          step.id,
          workspace,
          recovery_attempt_number,
          recovery_session_id,
          "retry_requested",
          summary,
          append_recovery_reason_suffix(reason, reason_suffix),
          Some(workspace.attempt_index + 1),
          secrets,
        )
      {
        Ok(Nil) ->
          RecoveryRetryRequested(
            success.tokens,
            success.final_issue,
            success.turns,
          )
        Error(record_error) -> {
          let Nil =
            note_ignored_checkpoint_error(
              describe_recovery_decision_record_error(record_error),
            )
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
          dependencies.checkpoint,
          step.id,
          workspace,
          recovery_attempt_number,
          recovery_session_id,
          "gave_up",
          summary,
          append_recovery_reason_suffix(reason, reason_suffix),
          None,
          secrets,
        )
      {
        Ok(Nil) -> Nil
        Error(record_error) ->
          note_ignored_checkpoint_error(describe_recovery_decision_record_error(
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
        append_recovery_reason_suffix(
          workflow_step_recovery.detail(protocol_reason, secrets),
          reason_suffix,
        )
      ignore_secondary_checkpoint_result(workflow_step_recovery.record_finished(
        dependencies.checkpoint,
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

fn describe_recovery_decision_record_error(
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

fn checkpoint_workspace_root(
  orchestrator: config_types.OrchestratorConfig,
) -> String {
  orchestrator.effective.workspace.root
}

fn guard_reason_suffix(
  events: List(workflow_recovery_checkpoint_guard.GuardEvent),
) -> String {
  case events {
    [] -> ""
    _ -> workflow_recovery_checkpoint_guard.events_to_diagnostic(events)
  }
}

fn append_recovery_reason_suffix(reason: String, suffix: String) -> String {
  case suffix == "" {
    True -> reason
    False -> reason <> "; " <> suffix
  }
}

fn stop_recovery_after_guard_failure(
  step: workflow_dag.WorkflowStep,
  workspace: workspace_run.PreparedStepWorkspace,
  recovery_attempt_number: Int,
  recovery_session_id: String,
  tokens: session_tokens.TokenTotals,
  final_issue: Option(tracker_issue.Issue),
  turns: Int,
  guard_error: workflow_recovery_checkpoint_guard.GuardError,
  secrets: List(String),
  dependencies: Dependencies,
) -> RecoveryAttemptOutcome {
  ignore_secondary_checkpoint_result(workflow_step_recovery.record_finished(
    dependencies.checkpoint,
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

fn recovery_context(
  context: StepContext,
) -> Result(StepContext, structured_output_tool_spec.ToolSpecError) {
  workflow_step_recovery.tool_spec_env(
    context.workflow_id,
    context.run_id,
    context.step_id,
    context.attempt_index,
    structured_output.validator_repo_root(
      context.config_dir,
      context.workspace_path,
    ),
    context.run_root,
  )
  |> result.map(fn(env) { StepContext(..context, extra_pi_env: [env]) })
}

fn prompt_ref_text(prompt_ref: workflow_dag.PromptRef) -> String {
  case prompt_ref {
    workflow_dag.PromptInline(prompt) -> prompt
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

fn latest_final_issue(
  previous: Option(tracker_issue.Issue),
  next: Option(tracker_issue.Issue),
) -> Option(tracker_issue.Issue) {
  case next {
    Some(_) -> next
    None -> previous
  }
}

fn mark_batch_pending(
  state: workflow_scheduler.SchedulerState,
  starts: List(PreparedStart),
) -> workflow_scheduler.SchedulerState {
  case starts {
    [] -> state
    [PreparedStart(step: step, ..), ..rest] ->
      mark_batch_pending(workflow_scheduler.mark_pending(state, step.id), rest)
  }
}

fn workflow_step_failed_reason(result: StepExecutionResult) -> String {
  case result.artifact.failure_code {
    Some(code) ->
      case string.starts_with(code, "structured_output_") {
        True -> "workflow_step_failed:" <> code <> ":step=" <> result.step_id
        False -> "workflow_step_failed"
      }
    None -> "workflow_step_failed"
  }
}

fn mark_prepared_attempts_interrupted(
  starts: List(PreparedStart),
  dependencies: Dependencies,
  workflow_id: String,
  reason: String,
  skipped_step_id: Option(String),
) -> Nil {
  case starts {
    [] -> Nil
    [PreparedStart(step: step, workspace: workspace), ..rest] -> {
      case skipped_step_id != Some(step.id) {
        True ->
          ignore_secondary_checkpoint_result(
            dependencies.checkpoint.step_interrupted(
              workspace.run_id,
              workflow_id,
              step.id,
              workspace.attempt_index,
              reason,
            ),
          )
        False -> Nil
      }
      mark_prepared_attempts_interrupted(
        rest,
        dependencies,
        workflow_id,
        reason,
        skipped_step_id,
      )
    }
  }
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
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  run_id: String,
  workflow_fingerprint: String,
  contract_outputs_recorded: Option(workflow_checkpoint.ArtifactWritten),
  recovery_evidence: workflow_outcome.RecoveryEvidence,
  dependencies: Dependencies,
  scheduler_state: workflow_scheduler.SchedulerState,
  artifacts: Dict(String, step_artifact.StepArtifact),
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
  run_root: Option(String),
  attempt_indexes: Dict(String, Int),
  tokens: session_tokens.TokenTotals,
  final_issue: Option(tracker_issue.Issue),
  turns: Int,
  cleanup_allowed: Bool,
  recovered_execution: Bool,
  pi_session_continuations: Dict(String, workflow_attempt.PiContinuation),
  profile: config_types.WorkspaceHookProfile,
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
        workflow_fingerprint,
        contract_outputs_recorded,
        recovery_evidence,
        recovered_execution,
        dependencies,
        scheduler_state,
        artifacts,
        prepared_workspaces,
        run_root,
        attempt_indexes,
        tokens,
        final_issue,
        turns,
        cleanup_allowed,
        pi_session_continuations,
        profile,
      )
    [PreparedStart(step: step, workspace: workspace), ..rest] -> {
      case dict.get(result_by_step, step.id) {
        Error(Nil) -> {
          mark_workflow_failed_terminal(
            dependencies,
            recovery_evidence,
            run_id,
            dag.id,
            issue.id,
            task_ref(issue),
            tokens.total,
            turns,
            starts,
          )
          let cleanup_suffix =
            cleanup_failure_suffix(cleanup_if_allowed(
              run_root,
              orchestrator,
              profile,
              dependencies,
              cleanup_allowed,
            ))
          Error(WorkflowRunFailure(
            reason: "missing_prepared_step_result:" <> step.id <> cleanup_suffix,
            agent_reason: None,
            artifacts: artifacts,
            run_root: run_root,
            failed_step_id: Some(step.id),
          ))
        }
        Ok(result) -> {
          let outcome =
            workflow_checkpoint.step_outcome(
              result.artifact,
              on_failure: step.on_failure == workflow_dag.ContinueWorkflow,
            )
          let finished =
            workflow_checkpoint.StepFinished(
              run_id: run_id,
              workflow_id: dag.id,
              step_id: step.id,
              attempt_index: workspace.attempt_index,
              outcome: outcome,
              workspace_name: workspace.workspace_name,
              workspace_path: workspace.path,
              token_total: result.tokens.total,
              turns: result.turns,
            )
          case
            dependencies.checkpoint.write_step_artifact(
              finished,
              result.artifact,
            )
          {
            Error(error) -> {
              mark_workflow_failed_terminal(
                dependencies,
                recovery_evidence,
                run_id,
                dag.id,
                issue.id,
                task_ref(issue),
                tokens.total + result.tokens.total,
                turns + result.turns,
                starts,
              )
              let cleanup_suffix =
                cleanup_failure_suffix(cleanup_if_allowed(
                  run_root,
                  orchestrator,
                  profile,
                  dependencies,
                  cleanup_allowed,
                ))
              Error(WorkflowRunFailure(
                reason: "checkpoint_failed:"
                  <> workflow_checkpoint.describe_error(error)
                  <> cleanup_suffix,
                agent_reason: None,
                artifacts: artifacts,
                run_root: run_root,
                failed_step_id: Some(step.id),
              ))
            }
            Ok(artifact_ref) ->
              case
                run_after_step(
                  dependencies,
                  issue,
                  step.id,
                  workspace,
                  orchestrator,
                  profile,
                )
              {
                Error(error) -> {
                  let reason = after_step_error_reason(error)
                  mark_workflow_failed_terminal(
                    dependencies,
                    recovery_evidence,
                    run_id,
                    dag.id,
                    issue.id,
                    task_ref(issue),
                    tokens.total + result.tokens.total,
                    turns + result.turns,
                    starts,
                  )
                  let cleanup_suffix =
                    cleanup_failure_suffix(cleanup_if_allowed(
                      run_root,
                      orchestrator,
                      profile,
                      dependencies,
                      cleanup_allowed,
                    ))
                  Error(WorkflowRunFailure(
                    reason: reason <> cleanup_suffix,
                    agent_reason: None,
                    artifacts: artifacts,
                    run_root: run_root,
                    failed_step_id: Some(step.id),
                  ))
                }
                Ok(Nil) ->
                  case
                    dependencies.checkpoint.step_finished(
                      finished,
                      artifact_ref,
                    )
                  {
                    Error(error) -> {
                      mark_workflow_failed_terminal(
                        dependencies,
                        recovery_evidence,
                        run_id,
                        dag.id,
                        issue.id,
                        task_ref(issue),
                        tokens.total + result.tokens.total,
                        turns + result.turns,
                        starts,
                      )
                      let cleanup_suffix =
                        cleanup_failure_suffix(cleanup_if_allowed(
                          run_root,
                          orchestrator,
                          profile,
                          dependencies,
                          cleanup_allowed,
                        ))
                      Error(WorkflowRunFailure(
                        reason: "checkpoint_failed:"
                          <> workflow_checkpoint.describe_error(error)
                          <> cleanup_suffix,
                        agent_reason: None,
                        artifacts: artifacts,
                        run_root: run_root,
                        failed_step_id: Some(step.id),
                      ))
                    }
                    Ok(Nil) -> {
                      let artifacts =
                        dict.insert(artifacts, step.id, result.artifact)
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
                      apply_prepared_results(
                        rest,
                        result_by_step,
                        issue,
                        dag,
                        orchestrator,
                        tracker_client,
                        secrets,
                        run_id,
                        workflow_fingerprint,
                        contract_outputs_recorded,
                        recovery_evidence,
                        dependencies,
                        scheduler_state,
                        artifacts,
                        prepared_workspaces,
                        run_root,
                        attempt_indexes,
                        tokens,
                        final_issue,
                        turns + result.turns,
                        cleanup_allowed,
                        recovered_execution,
                        pi_session_continuations,
                        profile,
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

fn run_after_step(
  dependencies: Dependencies,
  issue: tracker_issue.Issue,
  step_id: String,
  workspace: workspace_run.PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
) -> Result(Nil, AfterStepError) {
  let was_trapping_exits = process_ext.trap_exits(True)
  let subject = process.new_subject()
  let pid =
    process.spawn(fn() {
      dependencies.after_step(issue, step_id, workspace, orchestrator, profile)
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
      Error(after_step_error_from_exit(step_id, reason))
    process.PortDown(_, _, _) -> Error(AfterStepMonitorDown(step_id))
  }
}

fn after_step_error_from_exit(
  step_id: String,
  reason: process.ExitReason,
) -> AfterStepError {
  case reason {
    process.Normal -> AfterStepExitedWithoutResult(step_id)
    process.Killed -> AfterStepKilled(step_id)
    process.Abnormal(_) -> AfterStepCrashed(step_id)
  }
}

fn after_step_error_reason(error: AfterStepError) -> String {
  case error {
    AfterStepExitedWithoutResult(step_id) ->
      "after_step_exited_without_result:" <> step_id
    AfterStepKilled(step_id) -> "after_step_killed:" <> step_id
    AfterStepCrashed(step_id) -> "after_step_crashed:" <> step_id
    AfterStepMonitorDown(step_id) -> "after_step_monitor_down:" <> step_id
  }
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
  let context = step_context(step, workspace, issue, orchestrator, profile)
  case step.kind {
    workflow_dag.CommandStep(run, timeout_ms) -> {
      let timeout_ms = option.unwrap(timeout_ms, 60_000)
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
  case prepare_structured_output_tool_context(context, structured_output_spec) {
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

fn prepare_structured_output_tool_context(
  context: StepContext,
  structured_output_spec: Option(workflow_dag.StructuredOutputSpec),
) -> Result(StepContext, structured_output_tool_spec.ToolSpecError) {
  case structured_output_spec {
    Some(spec) ->
      case structured_output_tool_spec.schema_path_for_source(spec.source) {
        Some(_) -> {
          use tool_spec <- result.try(
            structured_output_tool_spec.for_step(
              structured_output_tool_spec.BuildInput(
                workflow_id: context.workflow_id,
                run_id: context.run_id,
                step_id: context.step_id,
                attempt_index: context.attempt_index,
                repository_root: structured_output.validator_repo_root(
                  context.config_dir,
                  context.workspace_path,
                ),
                spec: spec,
              ),
            ),
          )
          use written <- result.try(structured_output_tool_spec.write(
            tool_spec,
            context.run_root,
          ))
          Ok(
            StepContext(..context, extra_pi_env: [
              structured_output_tool_spec.env_pair(written),
            ]),
          )
        }
        None -> Ok(context)
      }
    None -> Ok(context)
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
  let artifact =
    agent_failure_artifact(
      step.id,
      failure,
      secrets,
      orchestrator.artifact_limits,
    )
  case
    workflow_structured_retry.transient_agent_failure_diagnostic(
      structured_output_spec,
      failure,
      secrets,
    )
  {
    Some(#(spec, initial_diagnostic)) ->
      retry_structured_output(
        step,
        context,
        workflow_structured_retry.agent_failure_as_success(failure),
        workflow_structured_retry.agent_failure_artifact_with_structured_output(
          artifact,
          failure,
          spec,
          secrets,
        ),
        Some(spec),
        initial_diagnostic,
        issue,
        dag,
        orchestrator,
        tracker_client,
        secrets,
        dependencies,
        effective,
      )
    None -> #(artifact, failure.tokens, failure.final_issue, 0)
  }
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
  let artifact =
    agent_success_artifact(
      step,
      context,
      success,
      structured_output_spec,
      secrets,
      orchestrator.artifact_limits,
      dependencies.checkpoint,
    )
  case structured_output_retry_diagnostic(structured_output_spec, artifact) {
    None -> #(artifact, success.tokens, success.final_issue, success.turns)
    Some(initial_diagnostic) ->
      retry_structured_output(
        step,
        context,
        success,
        artifact,
        structured_output_spec,
        initial_diagnostic,
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

fn structured_output_retry_diagnostic(
  spec: Option(workflow_dag.StructuredOutputSpec),
  artifact: step_artifact.StepArtifact,
) -> Option(step_artifact.StructuredOutputRetryDiagnostic) {
  case spec {
    Some(spec) ->
      case
        spec.required
        && spec.validation_retries > 0
        && structured_output_artifact_retryable(artifact)
      {
        True -> structured_output_attempt_diagnostic(1, artifact)
        False -> None
      }
    None -> None
  }
}

fn structured_output_artifact_retryable(
  artifact: step_artifact.StepArtifact,
) -> Bool {
  case artifact.structured_output {
    Some(step_artifact.StructuredOutputError(_, _, _, Some(details), _)) ->
      details.retryable
    _ -> is_structured_output_validation_failure(artifact.failure_code)
  }
}

fn is_structured_output_validation_failure(code: Option(String)) -> Bool {
  case code {
    Some("structured_output_artifact_write_failed")
    | Some("structured_output_json_schema_config_error")
    | Some("structured_output_command_config_error")
    | Some("structured_output_command_timeout") -> False
    Some(value) -> string.starts_with(value, "structured_output_")
    None -> False
  }
}

fn structured_output_attempt_diagnostic(
  attempt: Int,
  artifact: step_artifact.StepArtifact,
) -> Option(step_artifact.StructuredOutputRetryDiagnostic) {
  case artifact.structured_output {
    Some(step_artifact.StructuredOutputValid(_)) ->
      Some(step_artifact.StructuredOutputRetryDiagnostic(
        attempt: attempt,
        status: "valid",
        failure_code: None,
        message: "required structured output validated",
      ))
    Some(step_artifact.StructuredOutputError(_, _, message, details, _)) ->
      Some(step_artifact.StructuredOutputRetryDiagnostic(
        attempt: attempt,
        status: "error",
        failure_code: artifact.failure_code,
        message: structured_output_retry_message(message, details),
      ))
    _ -> None
  }
}

fn structured_output_retry_message(
  message: String,
  details: Option(step_artifact.StructuredOutputErrorDetails),
) -> String {
  case details {
    Some(details) ->
      message <> "\nRetryable: " <> bool_string(details.retryable)
    None -> message
  }
}

fn bool_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn retry_structured_output(
  step: workflow_dag.WorkflowStep,
  context: StepContext,
  first_success: agent_types.WorkerSuccess,
  first_artifact: step_artifact.StepArtifact,
  structured_output_spec: Option(workflow_dag.StructuredOutputSpec),
  initial_diagnostic: step_artifact.StructuredOutputRetryDiagnostic,
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
  case structured_output_spec {
    None -> #(
      first_artifact,
      first_success.tokens,
      first_success.final_issue,
      first_success.turns,
    )
    Some(spec) -> {
      let retry_prompt =
        workflow_structured_retry.retry_prompt(
          step.id,
          context.run_root,
          context.workspace_path,
          spec,
          initial_diagnostic,
        )
      let retry_result =
        run_agent_invocation(
          issue,
          context,
          dag,
          orchestrator,
          workflow_attempt.StructuredOutputRetryPrompt(retry_prompt),
          None,
          effective,
          tracker_client,
          dependencies,
        )
      case retry_result {
        Ok(retry_success) ->
          finish_structured_output_retry_success(
            step,
            context,
            first_success,
            retry_success,
            structured_output_spec,
            initial_diagnostic,
            spec,
            orchestrator,
            secrets,
            dependencies,
          )
        Error(retry_failure) ->
          finish_structured_output_retry_failure(
            first_success,
            first_artifact,
            retry_failure,
            initial_diagnostic,
            spec,
            secrets,
          )
      }
    }
  }
}

fn finish_structured_output_retry_success(
  step: workflow_dag.WorkflowStep,
  context: StepContext,
  first_success: agent_types.WorkerSuccess,
  retry_success: agent_types.WorkerSuccess,
  structured_output_spec: Option(workflow_dag.StructuredOutputSpec),
  initial_diagnostic: step_artifact.StructuredOutputRetryDiagnostic,
  spec: workflow_dag.StructuredOutputSpec,
  orchestrator: config_types.OrchestratorConfig,
  secrets: List(String),
  dependencies: Dependencies,
) -> #(
  step_artifact.StepArtifact,
  session_tokens.TokenTotals,
  Option(tracker_issue.Issue),
  Int,
) {
  let retry_artifact =
    agent_success_artifact(
      step,
      context,
      retry_success,
      structured_output_spec,
      secrets,
      orchestrator.artifact_limits,
      dependencies.checkpoint,
    )
  let retry_diagnostic =
    structured_output_attempt_diagnostic(2, retry_artifact)
    |> option.unwrap(step_artifact.StructuredOutputRetryDiagnostic(
      attempt: 2,
      status: step_artifact.status_to_string(retry_artifact.status),
      failure_code: retry_artifact.failure_code,
      message: "structured output retry completed",
    ))
  let outcome = case step_artifact.succeeded(retry_artifact.status) {
    True -> "succeeded"
    False -> "failed"
  }
  let retry_info =
    workflow_structured_retry.retry_info(spec, outcome, [
      initial_diagnostic,
      retry_diagnostic,
    ])
  let artifact =
    step_artifact.with_structured_output_retry_info(retry_artifact, retry_info)
  #(
    artifact,
    add_tokens(first_success.tokens, retry_success.tokens),
    option.or(retry_success.final_issue, first_success.final_issue),
    first_success.turns + retry_success.turns,
  )
}

fn finish_structured_output_retry_failure(
  first_success: agent_types.WorkerSuccess,
  first_artifact: step_artifact.StepArtifact,
  retry_failure: agent_types.WorkerFailure,
  initial_diagnostic: step_artifact.StructuredOutputRetryDiagnostic,
  spec: workflow_dag.StructuredOutputSpec,
  secrets: List(String),
) -> #(
  step_artifact.StepArtifact,
  session_tokens.TokenTotals,
  Option(tracker_issue.Issue),
  Int,
) {
  let retry_diagnostic =
    step_artifact.StructuredOutputRetryDiagnostic(
      attempt: 2,
      status: "agent_failure",
      failure_code: Some(error.agent_code(retry_failure.reason)),
      message: workflow_structured_retry.failure_message(retry_failure, secrets),
    )
  let retry_info =
    workflow_structured_retry.retry_info(spec, "failed", [
      initial_diagnostic,
      retry_diagnostic,
    ])
  let artifact =
    step_artifact.with_structured_output_retry_info(first_artifact, retry_info)
  #(
    artifact,
    add_tokens(first_success.tokens, retry_failure.tokens),
    option.or(retry_failure.final_issue, first_success.final_issue),
    first_success.turns,
  )
}

fn agent_success_artifact(
  step: workflow_dag.WorkflowStep,
  context: StepContext,
  success: agent_types.WorkerSuccess,
  structured_output_spec: Option(workflow_dag.StructuredOutputSpec),
  secrets: List(String),
  limits: config_types.ArtifactLimits,
  checkpoint: workflow_checkpoint.Writer,
) -> step_artifact.StepArtifact {
  case structured_output_spec {
    None -> step_artifact.from_agent_success(step.id, success, secrets, limits)
    Some(spec) ->
      agent_success_with_structured_output(
        step,
        context,
        success,
        spec,
        secrets,
        limits,
        checkpoint,
      )
  }
}

fn agent_success_with_structured_output(
  step: workflow_dag.WorkflowStep,
  context: StepContext,
  success: agent_types.WorkerSuccess,
  spec: workflow_dag.StructuredOutputSpec,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
  checkpoint: workflow_checkpoint.Writer,
) -> step_artifact.StepArtifact {
  let base = step_artifact.from_agent_success(step.id, success, secrets, limits)
  let format = workflow_dag.structured_output_format_to_string(spec.format)
  let workflow_dag.StructuredObjectSchema(required_keys) = spec.schema
  case
    structured_output.validate_agent_result(
      spec,
      success.result,
      secrets,
      structured_output.default_validator_runner(
        structured_output.default_validator_context(
          context.config_dir,
          context.run_root,
          context.workflow_id,
          context.workflow_bundle_dir,
          context.run_id,
          step.id,
          context.attempt_index,
          context.workspace_path,
          spec.artifact_name,
          format,
          spec.source,
        ),
        secrets,
      ),
    )
  {
    Ok(structured_output.StructuredOutputAbsent) ->
      step_artifact.StepArtifact(
        ..base,
        structured_output: Some(step_artifact.StructuredOutputAbsent(
          spec.artifact_name,
          format,
          "not_applicable",
        )),
      )
    Ok(structured_output.StructuredOutputPresent(payload_json)) ->
      write_structured_output_artifact(
        step,
        context,
        success,
        spec,
        format,
        required_keys,
        payload_json,
        secrets,
        limits,
        checkpoint,
      )
    Error(error) -> {
      let failure_code = structured_output.error_code(error)
      step_artifact.from_agent_structured_output_error_with_details(
        step.id,
        success,
        secrets,
        limits,
        failure_code,
        structured_output.error_message_for_step(error, step.id),
        spec.artifact_name,
        format,
        Some(step_artifact.StructuredOutputErrorDetails(
          code: failure_code,
          retryable: structured_output.error_retryable(error),
          validator_name: structured_output.error_validator_name(error),
          validator_type: structured_output.error_validator_type(error),
          diagnostic_summary: structured_output.error_diagnostic_summary(error),
          stdout_truncated: structured_output.error_stdout_truncated(error),
          stderr_truncated: structured_output.error_stderr_truncated(error),
        )),
      )
    }
  }
}

fn write_structured_output_artifact(
  step: workflow_dag.WorkflowStep,
  context: StepContext,
  success: agent_types.WorkerSuccess,
  spec: workflow_dag.StructuredOutputSpec,
  format: String,
  required_keys: List(String),
  payload_json: String,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
  checkpoint: workflow_checkpoint.Writer,
) -> step_artifact.StepArtifact {
  let validation =
    structured_output_metadata.from_spec_with_receipt(
      spec,
      structured_output.validator_repo_root(
        context.config_dir,
        context.workspace_path,
      ),
      receipt_json_for_source(spec.source, success.result.tool_calls),
    )
  let write =
    workflow_checkpoint.StructuredOutputWrite(
      run_id: context.run_id,
      workflow_id: context.workflow_id,
      step_id: step.id,
      attempt_index: context.attempt_index,
      artifact_name: spec.artifact_name,
      format: format,
      schema_required_keys: required_keys,
      validation: validation,
      payload_json: payload_json,
    )
  case checkpoint.write_structured_output_artifact(write) {
    Ok(written) ->
      step_artifact.from_agent_success_with_valid_structured_output(
        step.id,
        success,
        secrets,
        limits,
        step_artifact.StructuredOutputMetadata(
          artifact_name: spec.artifact_name,
          format: format,
          ref: written.ref,
          path: written.path,
          uri: written.uri,
          display_path: written.display_path,
          local_path: written.local_path,
          sha256: written.sha256,
          bytes: written.bytes,
          schema_status: "valid",
          source_type: validation.source_type,
          source_tool_name: validation.source_tool_name,
          source_parameters_schema_path: validation.source_parameters_schema_path,
          source_parameters_schema_sha256: validation.source_parameters_schema_sha256,
          source_receipt_json: validation.source_receipt_json,
          baseline_required_keys: required_keys,
          validators: structured_output_metadata.validator_summaries(validation),
          retry: None,
        ),
      )
    Error(error) -> {
      let message =
        "step "
        <> step.id
        <> " structured output artifact write failed: "
        <> workflow_checkpoint.describe_error(error)
      step_artifact.from_agent_structured_output_error(
        step.id,
        success,
        secrets,
        limits,
        "structured_output_artifact_write_failed",
        message,
        spec.artifact_name,
        format,
      )
    }
  }
}

fn receipt_json_for_source(
  source: structured_output_source.StructuredOutputSource,
  tool_calls: List(result_artifact.ToolCallSubmission),
) -> Option(String) {
  case source {
    structured_output_source.PiToolCallSource(tool_name, _, _, _) ->
      receipt_json_for_tool(tool_calls, tool_name)
    structured_output_source.FinalResponseSource -> None
  }
}

fn receipt_json_for_tool(
  tool_calls: List(result_artifact.ToolCallSubmission),
  tool_name: String,
) -> Option(String) {
  case tool_calls {
    [] -> None
    [call, ..rest] ->
      case call.name == tool_name, call.receipt_json {
        True, Some(receipt_json) -> Some(receipt_json)
        _, _ -> receipt_json_for_tool(rest, tool_name)
      }
  }
}

fn agent_failure_artifact(
  step_id: String,
  failure: agent_types.WorkerFailure,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
) -> step_artifact.StepArtifact {
  let detail = error.agent_artifact_detail(failure.reason)
  let stderr = case is_recovery_resume_validation_failure(failure.reason) {
    True ->
      "SCHERZO_FAILURE_CODE="
      <> workflow_attempt.recovery_pi_resume_validation_failed
      <> "\n"
      <> detail
    False -> detail
  }
  let artifact =
    step_artifact.from_command_result(
      step_id,
      1,
      "",
      stderr,
      False,
      secrets,
      limits,
    )
  step_artifact.StepArtifact(
    ..artifact,
    summary_text: artifact.summary_text
      <> context_recovery_summary_suffix(failure.reason),
  )
}

fn context_recovery_summary_suffix(reason: error.AgentRunnerError) -> String {
  case reason {
    error.ContextRecoveryExhausted(
      recovery_method: recovery_method,
      context_artifact_ref: context_artifact_ref,
      result_artifact_ref: result_artifact_ref,
      ..,
    ) ->
      " context_recovery=failed recovery_exhausted=true recovery_method="
      <> recovery_method
      <> summary_ref("context_artifact", context_artifact_ref)
      <> summary_ref("result_artifact", result_artifact_ref)
    _ -> ""
  }
}

fn summary_ref(label: String, ref: Option(String)) -> String {
  case ref {
    Some(ref) -> " " <> label <> "=" <> ref
    None -> ""
  }
}

fn is_recovery_resume_validation_failure(
  reason: error.AgentRunnerError,
) -> Bool {
  case reason {
    error.PiFailed(error.PiProtocolError(message)) ->
      message == workflow_attempt.recovery_pi_resume_validation_failed
    _ -> False
  }
}

fn is_recovery_resume_validation_artifact(
  artifact: step_artifact.StepArtifact,
) -> Bool {
  artifact.failure_code
  == Some(workflow_attempt.recovery_pi_resume_validation_failed)
}

fn agent_reason_for_artifact(
  artifact: step_artifact.StepArtifact,
) -> Option(error.AgentRunnerError) {
  case is_recovery_resume_validation_artifact(artifact) {
    True ->
      Some(
        error.PiFailed(error.PiProtocolError(
          workflow_attempt.recovery_pi_resume_validation_failed,
        )),
      )
    False -> None
  }
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

fn workflow_attempt_context(
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

fn step_is_continuation_capable(
  step: workflow_dag.WorkflowStep,
  orchestrator: config_types.OrchestratorConfig,
) -> Bool {
  case step.kind {
    workflow_dag.AgentStep(_, _) ->
      orchestrator.effective.pi.session_persistence.enabled
    workflow_dag.CommandStep(_, _) -> False
  }
}

fn step_context(
  step: workflow_dag.WorkflowStep,
  workspace: workspace_run.PreparedStepWorkspace,
  issue: tracker_issue.Issue,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
) -> StepContext {
  StepContext(
    workflow_id: workspace.workflow_id,
    run_id: workspace.run_id,
    run_root: workspace.run_root,
    workflow_bundle_dir: workspace.workflow_bundle_dir,
    step_id: step.id,
    attempt_index: workspace.attempt_index,
    workspace_name: workspace.workspace_name,
    workspace_path: workspace.path,
    workspace_context: workspace_profile.driver_context(profile, orchestrator),
    config_dir: orchestrator.config_dir,
    issue_id: issue.id,
    issue_identifier: issue.identifier,
    run_kind: "issue",
    scheduled_job_id: "",
    schedule_due_at: "",
    schedule_started_at: "",
    run_attempt: 0,
    extra_pi_env: [],
  )
}

fn step_command_env(context: StepContext) -> List(#(String, String)) {
  let generated = [
    #("SCHERZO_CONFIG_DIR", context.config_dir),
    #("SCHERZO_WORKFLOW_ID", context.workflow_id),
    #("SCHERZO_WORKFLOW_BUNDLE_DIR", context.workflow_bundle_dir),
    #("SCHERZO_RUN_ID", context.run_id),
    #("SCHERZO_RUN_ROOT", context.run_root),
    #("SCHERZO_RUN_KIND", context.run_kind),
    #("SCHERZO_ISSUE_ID", context.issue_id),
    #("SCHERZO_ISSUE_IDENTIFIER", context.issue_identifier),
    #("SCHERZO_SCHEDULED_JOB_ID", context.scheduled_job_id),
    #("SCHERZO_SCHEDULE_DUE_AT", context.schedule_due_at),
    #("SCHERZO_SCHEDULE_STARTED_AT", context.schedule_started_at),
    #("SCHERZO_RUN_ATTEMPT", int.to_string(context.run_attempt)),
    #("SCHERZO_STEP_ID", context.step_id),
    #("SCHERZO_ATTEMPT_INDEX", int.to_string(context.attempt_index)),
    #(
      "SCHERZO_ATTEMPT_KEY",
      workflow_identity.attempt_key(
        context.run_id,
        context.step_id,
        context.attempt_index,
      ),
    ),
    #(
      "SCHERZO_HOOK_IDEMPOTENCY_KEY",
      workflow_identity.hook_idempotency_key(context.run_id, context.step_id),
    ),
    #("SCHERZO_WORKSPACE_NAME", context.workspace_name),
    #("SCHERZO_WORKSPACE_PATH", context.workspace_path),
  ]
  workspace_profile.driver_context_env_vars_with_generated(
    context.workspace_context,
    generated,
  )
  |> list.append(context.extra_pi_env)
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

fn cleanup_if_allowed(
  run_root: Option(String),
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
  dependencies: Dependencies,
  allowed: Bool,
) -> Result(Nil, error.WorkspaceError) {
  case allowed {
    True -> cleanup_if_needed(run_root, orchestrator, profile, dependencies)
    False -> Ok(Nil)
  }
}

fn cleanup_failure_suffix(
  cleanup_result: Result(Nil, error.WorkspaceError),
) -> String {
  case cleanup_result {
    Ok(Nil) -> ""
    Error(err) -> "; cleanup_failed:" <> error.workspace_code(err)
  }
}

fn cleanup_if_needed(
  run_root: Option(String),
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
  dependencies: Dependencies,
) -> Result(Nil, error.WorkspaceError) {
  case run_root {
    None -> Ok(Nil)
    Some(path) -> dependencies.cleanup_run(path, orchestrator, profile)
  }
}

fn observed_updated_at_ms(issue: tracker_issue.Issue) -> Int {
  case issue.updated_at {
    Some(time) -> birl.to_unix_milli(time)
    None -> 0
  }
}

fn task_ref(issue: tracker_issue.Issue) -> Option(workflow_checkpoint.TaskRef) {
  workflow_checkpoint.linear_task_ref_for_issue(
    issue.id,
    issue.identifier,
    issue.url,
  )
}

fn mark_workflow_failed_terminal(
  dependencies: Dependencies,
  recovery_evidence: workflow_outcome.RecoveryEvidence,
  run_id: String,
  workflow_id: String,
  issue_id: String,
  task_ref: Option(workflow_checkpoint.TaskRef),
  token_total: Int,
  turns: Int,
  active_attempts: List(PreparedStart),
) -> Nil {
  mark_prepared_attempts_interrupted(
    active_attempts,
    dependencies,
    workflow_id,
    "terminal_failure",
    None,
  )
  ignore_secondary_checkpoint_result(
    dependencies.checkpoint.workflow_finished(
      workflow_checkpoint.WorkflowFinished(
        run_id: run_id,
        workflow_id: workflow_id,
        issue_id: issue_id,
        task_ref: task_ref,
        outcome: workflow_outcome.terminal_failed_fatal(recovery_evidence),
        token_total: token_total,
        turns: turns,
      ),
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

fn add_tokens(
  left: session_tokens.TokenTotals,
  right: session_tokens.TokenTotals,
) -> session_tokens.TokenTotals {
  session_tokens.TokenTotals(
    input: left.input + right.input,
    output: left.output + right.output,
    cache_read: left.cache_read + right.cache_read,
    cache_write: left.cache_write + right.cache_write,
    total: left.total + right.total,
  )
}

fn scheduler_with_artifacts(
  state: workflow_scheduler.SchedulerState,
  artifacts: Dict(String, step_artifact.StepArtifact),
) -> workflow_scheduler.SchedulerState {
  artifacts
  |> dict.to_list
  |> list.fold(state, fn(state, entry) {
    let #(step_id, artifact) = entry
    workflow_scheduler.mark_finished(state, step_id, artifact)
  })
}

fn first_run_root(
  workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
) -> Option(String) {
  case dict.values(workspaces) {
    [workspace, ..] -> Some(workspace.run_root)
    [] -> None
  }
}

fn result_try_checkpoint(
  result: Result(Nil, workflow_checkpoint.CheckpointError),
  artifacts: Dict(String, step_artifact.StepArtifact),
  run_root: Option(String),
  failed_step_id: Option(String),
  next: fn(Nil) -> Result(WorkflowRunSuccess, WorkflowRunFailure),
) -> Result(WorkflowRunSuccess, WorkflowRunFailure) {
  case result {
    Ok(Nil) -> next(Nil)
    Error(error) ->
      Error(WorkflowRunFailure(
        reason: "checkpoint_failed:"
          <> workflow_checkpoint.describe_error(error),
        agent_reason: None,
        artifacts: artifacts,
        run_root: run_root,
        failed_step_id: failed_step_id,
      ))
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
