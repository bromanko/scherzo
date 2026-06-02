import birl
import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/config/types as config_types
import scherzo/error
import scherzo/orchestrator/schedule_core
import scherzo/process_ext
import scherzo/session/tokens as session_tokens
import scherzo/step_artifact
import scherzo/structured_output
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
import scherzo/workflow_run/contract_io
import scherzo/workflow_run/contract_io_error as contract_error
import scherzo/workflow_run/recovery_execution
import scherzo/workflow_run/step_context as step_context_internal
import scherzo/workflow_run/step_execution
import scherzo/workflow_run/terminal_policy
import scherzo/workflow_run/workspace_preparation.{
  type PreparedStart, PrepareReadyFailure, PreparedBatch, PreparedStart,
}
import scherzo/workflow_scheduler
import scherzo/workflow_step_recovery
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

pub fn default_dependencies() -> Dependencies {
  Dependencies(
    prepare_step: workspace_run.prepare_step_attempt,
    prepare_recovered_step: workspace_run.prepare_recovered_step_attempt,
    after_step: workspace_run.after_step,
    cleanup_run: workspace_run.cleanup_run,
    command_step: fn(context, command, timeout_ms, secrets, limits) {
      step_execution.default_command_step(
        internal_step_context(context),
        command,
        timeout_ms,
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
      step_execution.default_agent_step(
        issue,
        internal_step_context(context),
        prompt_mode,
        attempt_context,
        effective,
        tracker_client,
        emit_update,
        command_ready,
        record_pi_session,
      )
    },
    checkpoint: workflow_checkpoint.noop_writer(),
  )
}

fn step_execution_dependencies(
  dependencies: Dependencies,
) -> step_execution.Dependencies {
  step_execution.Dependencies(
    command_step: fn(context, command, timeout_ms, secrets, limits) {
      dependencies.command_step(
        external_step_context(context),
        command,
        timeout_ms,
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
      dependencies.agent_step(
        issue,
        external_step_context(context),
        prompt_mode,
        attempt_context,
        effective,
        tracker_client,
        emit_update,
        command_ready,
        record_pi_session,
      )
    },
    checkpoint: dependencies.checkpoint,
  )
}

fn internal_step_context(
  context: StepContext,
) -> step_context_internal.StepContext {
  step_context_internal.StepContext(
    workflow_id: context.workflow_id,
    run_id: context.run_id,
    run_root: context.run_root,
    workflow_bundle_dir: context.workflow_bundle_dir,
    step_id: context.step_id,
    attempt_index: context.attempt_index,
    workspace_name: context.workspace_name,
    workspace_path: context.workspace_path,
    workspace_context: context.workspace_context,
    config_dir: context.config_dir,
    issue_id: context.issue_id,
    issue_identifier: context.issue_identifier,
    run_kind: context.run_kind,
    scheduled_job_id: context.scheduled_job_id,
    schedule_due_at: context.schedule_due_at,
    schedule_started_at: context.schedule_started_at,
    run_attempt: context.run_attempt,
    extra_pi_env: context.extra_pi_env,
  )
}

fn external_step_context(
  context: step_context_internal.StepContext,
) -> StepContext {
  StepContext(
    workflow_id: context.workflow_id,
    run_id: context.run_id,
    run_root: context.run_root,
    workflow_bundle_dir: context.workflow_bundle_dir,
    step_id: context.step_id,
    attempt_index: context.attempt_index,
    workspace_name: context.workspace_name,
    workspace_path: context.workspace_path,
    workspace_context: context.workspace_context,
    config_dir: context.config_dir,
    issue_id: context.issue_id,
    issue_identifier: context.issue_identifier,
    run_kind: context.run_kind,
    scheduled_job_id: context.scheduled_job_id,
    schedule_due_at: context.schedule_due_at,
    schedule_started_at: context.schedule_started_at,
    run_attempt: context.run_attempt,
    extra_pi_env: context.extra_pi_env,
  )
}

fn profile_redaction_secrets(
  profile: config_types.WorkspaceHookProfile,
  secrets: List(String),
) -> List(String) {
  list.append(secrets, workspace_profile.profile_redaction_values(profile))
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

fn terminal_runtime(dependencies: Dependencies) -> terminal_policy.Runtime {
  terminal_policy.runtime(
    checkpoint: dependencies.checkpoint,
    cleanup_run: dependencies.cleanup_run,
  )
}

fn terminal_result_to_workflow_result(
  result: Result(terminal_policy.Success, terminal_policy.Failure),
) -> Result(WorkflowRunSuccess, WorkflowRunFailure) {
  case result {
    Ok(success) ->
      Ok(WorkflowRunSuccess(
        worker_success: success.worker_success,
        artifacts: success.artifacts,
        run_root: success.run_root,
        cleanup_warning: option.map(
          success.cleanup_warning,
          terminal_cleanup_warning,
        ),
      ))
    Error(failure) ->
      Error(WorkflowRunFailure(
        reason: failure.reason,
        agent_reason: failure.agent_reason,
        artifacts: failure.artifacts,
        run_root: failure.run_root,
        failed_step_id: failure.failed_step_id,
      ))
  }
}

fn terminal_cleanup_warning(
  warning: terminal_policy.PostSuccessCleanupWarning,
) -> PostSuccessCleanupWarning {
  PostSuccessCleanupWarning(
    code: warning.code,
    message: warning.message,
    run_root: warning.run_root,
  )
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

pub fn failure_report(failure: WorkflowRunFailure) -> String {
  terminal_policy.failure_report(
    failure.reason,
    failure.failed_step_id,
    failure.artifacts,
  )
}

pub fn failed_command_failure(
  failure: WorkflowRunFailure,
) -> Option(#(String, String)) {
  terminal_policy.failed_command_failure(
    failure.failed_step_id,
    failure.artifacts,
  )
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
      let final_issue = option.unwrap(final_issue, issue)
      terminal_policy.finish_success(terminal_policy.SuccessInput(
        issue: issue,
        final_issue: final_issue,
        dag: dag,
        orchestrator: orchestrator,
        run_id: run_id,
        workflow_fingerprint: workflow_fingerprint,
        contract_outputs_recorded: contract_outputs_recorded,
        recovery_evidence: recovery_evidence,
        runtime: terminal_runtime(dependencies),
        artifacts: artifacts,
        prepared_workspaces: prepared_workspaces,
        run_root: run_root,
        tokens: tokens,
        turns: turns,
        cleanup_allowed: cleanup_allowed,
        profile: profile,
      ))
      |> terminal_result_to_workflow_result
    }
    workflow_scheduler.WorkflowFailed ->
      terminal_policy.finish_scheduler_failure(terminal_policy.FailureInput(
        issue: issue,
        dag: dag,
        orchestrator: orchestrator,
        run_id: run_id,
        workflow_fingerprint: workflow_fingerprint,
        contract_outputs_recorded: contract_outputs_recorded,
        recovery_evidence: recovery_evidence,
        runtime: terminal_runtime(dependencies),
        artifacts: artifacts,
        prepared_workspaces: prepared_workspaces,
        run_root: run_root,
        token_total: tokens.total,
        turns: turns,
        cleanup_allowed: cleanup_allowed,
        profile: profile,
      ))
      |> terminal_result_to_workflow_result
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
          case
            workspace_preparation.prepare_ready_batch(
              steps,
              workspace_preparation.Context(
                issue: issue,
                workflow_id: dag.id,
                run_id: run_id,
                orchestrator: orchestrator,
                secrets: secrets,
                current_run_root: run_root,
                recovered_execution: recovered_execution,
                profile: profile,
              ),
              workspace_preparation.Dependencies(
                prepare_step: dependencies.prepare_step,
                prepare_recovered_step: dependencies.prepare_recovered_step,
                step_prepared: dependencies.checkpoint.step_prepared,
              ),
              prepared_workspaces,
              attempt_indexes,
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
            Ok(PreparedBatch(
              prepared_starts,
              prepared_workspaces,
              run_root,
              attempt_indexes,
            )) -> {
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
            step_execution.continuation_capable(step, orchestrator),
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
              let result =
                step_execution.run(
                  step,
                  workspace,
                  issue,
                  dag,
                  orchestrator,
                  tracker_client,
                  secrets,
                  step_execution_dependencies(dependencies),
                  artifacts,
                  pi_session_continuations,
                  profile,
                )
              process.send(
                subject,
                StepExecutionResult(
                  step_id: step.id,
                  artifact: result.artifact,
                  tokens: result.tokens,
                  final_issue: result.final_issue,
                  turns: result.turns,
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
  case step_execution.is_recovery_resume_validation_artifact(result.artifact) {
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
            recovery_execution.effective_for_failure(
              dag,
              step,
              workspace.attempt_index,
            )
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
                recovery_execution.RecoveryRetryRequested(
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
                recovery_execution.RecoveryStop(
                  recovery_tokens,
                  _,
                  recovery_turns,
                  stop_recovery_evidence,
                ) -> {
                  let recovery_evidence =
                    recovery_execution.combine_evidence(
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
  terminal_policy.finish_fatal_step_failure(
    terminal_policy.FatalStepFailureInput(
      issue: issue,
      dag: dag,
      orchestrator: orchestrator,
      run_id: run_id,
      workflow_fingerprint: workflow_fingerprint,
      contract_outputs_recorded: contract_outputs_recorded,
      recovery_evidence: recovery_evidence,
      runtime: terminal_runtime(dependencies),
      artifacts: artifacts,
      prepared_workspaces: prepared_workspaces,
      run_root: run_root,
      workflow_finished_token_total: workflow_finished_token_total,
      workflow_finished_turns: workflow_finished_turns,
      cleanup_allowed: cleanup_allowed,
      profile: profile,
      failed_step_id: result.step_id,
      failed_artifact: result.artifact,
      agent_reason: step_execution.agent_reason_for_artifact(result.artifact),
      checkpoint_error: checkpoint_error,
      interrupt_active_attempts: fn() {
        mark_prepared_attempts_interrupted(
          starts,
          dependencies,
          dag.id,
          "fatal_sibling_finished",
          Some(result.step_id),
        )
      },
    ),
  )
  |> terminal_result_to_workflow_result
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
) -> recovery_execution.AttemptOutcome {
  recovery_execution.execute(
    step,
    workspace,
    failed_artifact,
    config,
    issue,
    orchestrator,
    tracker_client,
    secrets,
    recovery_execution.dependencies(
      checkpoint: dependencies.checkpoint,
      agent_step: dependencies.agent_step,
      make_context: fn() {
        recovery_context(
          external_step_context(step_context_internal.from_prepared(
            step,
            workspace,
            issue,
            orchestrator,
            profile,
          )),
        )
      },
      make_attempt_context: fn(context, prompt_mode) {
        step_execution.workflow_attempt_context(
          internal_step_context(context),
          dag,
          orchestrator,
          prompt_mode,
          None,
        )
      },
    ),
  )
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
  terminal_policy.cleanup_if_allowed(
    run_root,
    orchestrator,
    profile,
    terminal_runtime(dependencies),
    allowed,
  )
}

fn cleanup_failure_suffix(
  cleanup_result: Result(Nil, error.WorkspaceError),
) -> String {
  terminal_policy.cleanup_failure_suffix(cleanup_result)
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
  terminal_policy.mark_workflow_failed_terminal(
    dependencies.checkpoint,
    recovery_evidence,
    run_id,
    workflow_id,
    issue_id,
    task_ref,
    token_total,
    turns,
    fn() {
      mark_prepared_attempts_interrupted(
        active_attempts,
        dependencies,
        workflow_id,
        "terminal_failure",
        None,
      )
    },
  )
}

fn ignore_secondary_checkpoint_result(
  result: Result(Nil, workflow_checkpoint.CheckpointError),
) -> Nil {
  terminal_policy.ignore_secondary_checkpoint_result(result)
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
