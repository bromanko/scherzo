import birl
import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/artifact_publication_executor
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_recording
import scherzo/artifact_publication_runtime
import scherzo/artifact_repository/command_runner
import scherzo/config/types as config_types
import scherzo/error
import scherzo/orchestrator/schedule_core
import scherzo/result_artifact
import scherzo/session/tokens as session_tokens
import scherzo/step_artifact
import scherzo/template
import scherzo/tracker
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_attempt
import scherzo/workflow_checkpoint
import scherzo/workflow_contract_manifest as contract_manifest
import scherzo/workflow_dag
import scherzo/workflow_identity
import scherzo/workflow_interface_snapshot
import scherzo/workflow_outcome
import scherzo/workflow_run/command_step_timeout_retry
import scherzo/workflow_run/contract_io
import scherzo/workflow_run/contract_io_error as contract_error
import scherzo/workflow_run/recovery_execution
import scherzo/workflow_run/step_context as step_context_internal
import scherzo/workflow_run/step_execution
import scherzo/workflow_run/step_worker_pool.{
  type PreparedStart, type StepBatchError, type StepBatchOutcome,
  type StepExecutionResult,
}
import scherzo/workflow_run/structured_output_step
import scherzo/workflow_run/terminal_policy
import scherzo/workflow_run/workspace_preparation.{
  PrepareReadyFailure, PreparedBatch,
}
import scherzo/workflow_run/workstream_handoff
import scherzo/workflow_scheduler
import scherzo/workspace
import scherzo/workspace_profile
import scherzo/workspace_run
import simplifile

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
    source: workspace.WorkspaceSource,
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

type AppliedPreparedResults {
  AppliedPreparedResults(
    scheduler_state: workflow_scheduler.SchedulerState,
    artifacts: Dict(String, step_artifact.StepArtifact),
    tokens: session_tokens.TokenTotals,
    final_issue: Option(tracker_issue.Issue),
    turns: Int,
  )
}

type WorkflowStartError {
  WorkflowRunRootFailed(error.WorkspaceError)
  WorkflowStartCheckpointFailed(workflow_checkpoint.CheckpointError)
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

pub fn execute_scheduled_with_resume(
  scheduled: schedule_core.ScheduledRunContext,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  tracker_client: tracker.Client,
  secrets: List(String),
  dependencies: Dependencies,
  resume: ResumeState,
) -> Result(WorkflowRunSuccess, WorkflowRunFailure) {
  execute_with_resume(
    scheduled_placeholder_issue(scheduled),
    dag,
    orchestrator,
    tracker_client,
    secrets,
    scheduled.run_id,
    scheduled_dependencies(scheduled, dependencies),
    resume,
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
      workflow_id: workflow_dag.id(dag),
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
                record_workflow_interface_snapshot(
                  dag,
                  invocation,
                  dependencies.checkpoint,
                )
              {
                Error(error) ->
                  Error(WorkflowRunFailure(
                    reason: "checkpoint_failed:"
                      <> workflow_checkpoint.describe_error(error),
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
                            workflow_id: workflow_dag.id(dag),
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
          }
        RecoveredRun(recovered) ->
          case recovered.workflow_id != workflow_dag.id(dag) {
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
                            workflow_id: workflow_dag.id(dag),
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
          workflow_dag.id(dag),
          invocation.run_id,
          orchestrator,
        )
    }
    |> result.map_error(WorkflowRunRootFailed),
  )
  dependencies.checkpoint.workflow_started(workflow_checkpoint.WorkflowStarted(
    run_id: invocation.run_id,
    workflow_id: workflow_dag.id(dag),
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

fn record_workflow_interface_snapshot(
  dag: workflow_dag.WorkflowDag,
  invocation: RunInvocation,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(Nil, workflow_checkpoint.CheckpointError) {
  let snapshot =
    workflow_interface_snapshot.from_dag(dag, invocation.workflow_fingerprint)
  let contents = workflow_interface_snapshot.to_string(snapshot)
  use written <- result.try(checkpoint.write_workflow_interface_snapshot(
    invocation.run_id,
    contents,
  ))
  checkpoint.workflow_interface_snapshot_recorded(
    workflow_checkpoint.WorkflowInterfaceSnapshotRecorded(
      run_id: invocation.run_id,
      workflow_id: workflow_dag.id(dag),
      workflow_fingerprint: invocation.workflow_fingerprint,
      artifact: written,
    ),
  )
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
) -> Result(contract_io.ContractOutputsResult, String) {
  contract_io.record_outputs_if_contracted(
    dag,
    run_id,
    workflow_fingerprint,
    contract_outputs_recorded,
    dependencies.checkpoint,
    artifacts,
    prepared_workspaces,
  )
  |> result.map_error(contract_error.describe_error)
}

fn emit_workstream_handoff_if_configured(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  run_id: String,
  workflow_fingerprint: String,
  outputs: contract_io.ContractOutputsResult,
  dependencies: Dependencies,
) -> Result(Nil, String) {
  workstream_handoff.emit_if_configured(
    issue,
    dag,
    run_id,
    workflow_fingerprint,
    outputs,
    dependencies.checkpoint,
  )
  |> result.map_error(workstream_handoff.describe_error)
}

fn record_publications_if_configured(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  outputs: contract_io.ContractOutputsResult,
  run_id: String,
  recovered_execution: Bool,
  dependencies: Dependencies,
  _run_root: Option(String),
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
  profile: config_types.WorkspaceHookProfile,
) -> Result(artifact_publication_recording.PublicationRecordingResult, String) {
  case outputs.manifest {
    Some(output_manifest) -> {
      let workflow_bundle_dir =
        workflow_identity.workflow_bundle_dir(
          orchestrator,
          workflow_dag.id(dag),
        )
      case recovered_execution {
        True ->
          artifact_publication_executor.execute_recovered_routes_with_runner_and_state_root_and_publication_driver(
            workflow_dag.publication_routes(dag),
            orchestrator.artifact_repositories,
            orchestrator.config_dir,
            workflow_bundle_dir,
            orchestrator.effective.workspace.root,
            output_manifest,
            issue,
            run_id,
            dependencies.checkpoint,
            command_runner.production(),
            artifact_publication_runtime.driver_for_run(
              issue,
              dag,
              orchestrator,
              prepared_workspaces,
              profile,
            ),
          )
        False ->
          artifact_publication_executor.execute_routes_with_runner_and_state_root_and_publication_driver(
            workflow_dag.publication_routes(dag),
            orchestrator.artifact_repositories,
            orchestrator.config_dir,
            workflow_bundle_dir,
            orchestrator.effective.workspace.root,
            output_manifest,
            issue,
            run_id,
            dependencies.checkpoint,
            command_runner.production(),
            artifact_publication_runtime.driver_for_run(
              issue,
              dag,
              orchestrator,
              prepared_workspaces,
              profile,
            ),
          )
      }
    }
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

fn result_with_publication_summary(
  result: result_artifact.ResultArtifact,
  publication_result: artifact_publication_recording.PublicationRecordingResult,
  checkpoint: workflow_checkpoint.Writer,
  limits: config_types.ArtifactLimits,
) -> result_artifact.ResultArtifact {
  let artifact_publication_recording.PublicationRecordingResult(attempts:, ..) =
    publication_result
  case publication_summary_text(attempts, checkpoint) {
    "" -> result
    summary ->
      result_artifact.append(
        result,
        result_artifact.from_final_response(
          Some(summary),
          False,
          "artifact_publication",
        ),
        limits.workflow_summary_max_chars,
      )
  }
}

fn publication_summary_text(
  attempts: List(artifact_publication_recording.PublicationAttemptSummary),
  checkpoint: workflow_checkpoint.Writer,
) -> String {
  let lines = publication_summary_lines(attempts, checkpoint, [])
  case lines {
    [] -> ""
    _ -> "## Publication\n" <> string.join(lines, with: "\n")
  }
}

fn publication_summary_lines(
  attempts: List(artifact_publication_recording.PublicationAttemptSummary),
  checkpoint: workflow_checkpoint.Writer,
  acc: List(String),
) -> List(String) {
  case attempts {
    [] -> list.reverse(acc)
    [attempt, ..rest] ->
      publication_summary_lines(rest, checkpoint, [
        publication_attempt_summary_line(attempt, checkpoint),
        ..acc
      ])
  }
}

fn publication_attempt_summary_line(
  attempt: artifact_publication_recording.PublicationAttemptSummary,
  checkpoint: workflow_checkpoint.Writer,
) -> String {
  case checkpoint.read_artifact(attempt.manifest_ref) {
    Ok(contents) ->
      case artifact_publication_manifest.decode_manifest_json(contents) {
        Ok(manifest) -> publication_manifest_summary_line(attempt, manifest)
        Error(_) ->
          "- "
          <> attempt.publication_id
          <> ": "
          <> attempt.status
          <> " (manifest: `"
          <> attempt.manifest_ref
          <> "`)"
      }
    Error(_) ->
      "- "
      <> attempt.publication_id
      <> ": "
      <> attempt.status
      <> " (manifest unavailable: `"
      <> attempt.manifest_ref
      <> "`)"
  }
}

fn publication_manifest_summary_line(
  attempt: artifact_publication_recording.PublicationAttemptSummary,
  manifest: artifact_publication_manifest.PublicationManifest,
) -> String {
  let status = artifact_publication_manifest.status_to_string(manifest.status)
  let branch = option.unwrap(manifest.branch, "")
  let pr_url = option.unwrap(manifest.pr_url, "")
  let commit_sha = option.unwrap(manifest.commit_sha, "")
  "- "
  <> attempt.publication_id
  <> ": "
  <> status
  <> optional_summary_field("PR_URL", pr_url)
  <> optional_summary_field("BRANCH", branch)
  <> optional_summary_field("COMMIT_SHA", commit_sha)
  <> " (manifest: `"
  <> attempt.manifest_ref
  <> "`)"
}

fn optional_summary_field(label: String, value: String) -> String {
  case string.trim(value) {
    "" -> ""
    trimmed -> " " <> label <> "=" <> trimmed
  }
}

fn clear_success_retention_marker(run_root: Option(String)) -> Nil {
  case run_root {
    None -> Nil
    Some(path) ->
      case simplifile.delete(workspace_run.cleanup_retention_marker(path)) {
        Ok(Nil) | Error(simplifile.Enoent) -> Nil
        Error(file_error) ->
          note_ignored_retention_marker_cleanup_error(simplifile.describe_error(
            file_error,
          ))
      }
  }
}

fn note_ignored_retention_marker_cleanup_error(_message: String) -> Nil {
  Nil
}

fn append_optional_publication_diagnostics(
  failures: List(artifact_publication_recording.PublicationFailure),
  run_id: String,
  workflow_id: String,
  issue_id: String,
  dependencies: Dependencies,
) -> Result(Nil, String) {
  case failures {
    [] -> Ok(Nil)
    [failure, ..rest] -> {
      use _ <- result.try(
        dependencies.checkpoint.workflow_diagnostic(
          workflow_checkpoint.WorkflowDiagnostic(
            run_id: run_id,
            workflow_id: workflow_id,
            issue_id: issue_id,
            reason: "workflow_publication_optional_failed:"
              <> failure.publication_id
              <> ":"
              <> failure.code
              <> ":"
              <> failure.message,
          ),
        )
        |> result.map_error(workflow_checkpoint.describe_error),
      )
      append_optional_publication_diagnostics(
        rest,
        run_id,
        workflow_id,
        issue_id,
        dependencies,
      )
    }
  }
}

fn record_required_publication_retention_diagnostic(
  retain_workspace: Bool,
  dag: workflow_dag.WorkflowDag,
  issue: tracker_issue.Issue,
  run_id: String,
  dependencies: Dependencies,
) -> Nil {
  case retain_workspace {
    True ->
      ignore_secondary_checkpoint_result(
        dependencies.checkpoint.workflow_diagnostic(
          workflow_checkpoint.WorkflowDiagnostic(
            run_id: run_id,
            workflow_id: workflow_dag.id(dag),
            issue_id: issue.id,
            reason: "workflow_publication_workspace_retained_for_commit_stack_publication_failure",
          ),
        ),
      )
    False -> Nil
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
        Some(code) ->
          Some(#(
            command_step_timeout_retry.report_failure_code(failure.reason, code),
            artifact.step_id,
          ))
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
              recovered_execution,
              dependencies,
              run_root,
              prepared_workspaces,
              profile,
            )
          {
            Ok(publication_result) ->
              case publication_result.required_failures {
                [] -> {
                  let Nil = clear_success_retention_marker(run_root)
                  case
                    append_optional_publication_diagnostics(
                      publication_result.optional_failures,
                      run_id,
                      workflow_dag.id(dag),
                      issue.id,
                      dependencies,
                    )
                  {
                    Ok(Nil) -> {
                      let result =
                        result_with_publication_summary(
                          result,
                          publication_result,
                          dependencies.checkpoint,
                          orchestrator.artifact_limits,
                        )
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
                                workflow_id: workflow_dag.id(dag),
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
                                    workflow_id: workflow_dag.id(dag),
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
                        Error(reason) -> {
                          use Nil <- result_try_checkpoint(
                            dependencies.checkpoint.workflow_finished(
                              workflow_checkpoint.WorkflowFinished(
                                run_id: run_id,
                                workflow_id: workflow_dag.id(dag),
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
                    }
                    Error(reason) -> {
                      use Nil <- result_try_checkpoint(
                        dependencies.checkpoint.workflow_finished(
                          workflow_checkpoint.WorkflowFinished(
                            run_id: run_id,
                            workflow_id: workflow_dag.id(dag),
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
                        reason: "workflow_publication_diagnostic_failed:"
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
                [failure, ..] -> {
                  let retain_workspace =
                    cleanup_allowed
                    && artifact_publication_runtime.failures_require_workspace_retention(
                      workflow_dag.publication_routes(dag),
                      publication_result.required_failures,
                    )
                  let Nil =
                    record_required_publication_retention_diagnostic(
                      retain_workspace,
                      dag,
                      issue,
                      run_id,
                      dependencies,
                    )
                  use Nil <- result_try_checkpoint(
                    dependencies.checkpoint.workflow_finished(
                      workflow_checkpoint.WorkflowFinished(
                        run_id: run_id,
                        workflow_id: workflow_dag.id(dag),
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
                  let publication_cleanup_allowed =
                    cleanup_allowed && !retain_workspace
                  let cleanup_suffix =
                    cleanup_failure_suffix(cleanup_if_allowed(
                      run_root,
                      orchestrator,
                      profile,
                      dependencies,
                      publication_cleanup_allowed,
                    ))
                  let retention_suffix = case retain_workspace {
                    True ->
                      artifact_publication_runtime.retention_reason_suffix(
                        workflow_dag.publication_routes(dag),
                        publication_result.required_failures,
                      )
                    False -> ""
                  }
                  Error(WorkflowRunFailure(
                    reason: "workflow_publication_required_failed:"
                      <> failure.publication_id
                      <> ":"
                      <> failure.code
                      <> retention_suffix
                      <> cleanup_suffix,
                    agent_reason: None,
                    artifacts: artifacts,
                    run_root: run_root,
                    failed_step_id: None,
                  ))
                }
              }
            Error(reason) -> {
              use Nil <- result_try_checkpoint(
                dependencies.checkpoint.workflow_finished(
                  workflow_checkpoint.WorkflowFinished(
                    run_id: run_id,
                    workflow_id: workflow_dag.id(dag),
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
                workflow_id: workflow_dag.id(dag),
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
        Error(reason) -> {
          use Nil <- result_try_checkpoint(
            dependencies.checkpoint.workflow_finished(
              workflow_checkpoint.WorkflowFinished(
                run_id: run_id,
                workflow_id: workflow_dag.id(dag),
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
        Error(error) -> "; workflow_output_manifest_failed:" <> error
      }
      use Nil <- result_try_checkpoint(
        dependencies.checkpoint.workflow_finished(
          workflow_checkpoint.WorkflowFinished(
            run_id: run_id,
            workflow_id: workflow_dag.id(dag),
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
      let cleanup_suffix =
        cleanup_failure_suffix(cleanup_if_allowed(
          run_root,
          orchestrator,
          profile,
          dependencies,
          cleanup_allowed,
        ))
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
            workflow_dag.id(dag),
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
                workflow_id: workflow_dag.id(dag),
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
              let prepared_starts = worker_prepared_starts(prepared_starts)
              mark_workflow_failed_terminal(
                dependencies,
                recovery_evidence,
                run_id,
                workflow_dag.id(dag),
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
              let prepared_starts = worker_prepared_starts(prepared_starts)
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

fn worker_prepared_starts(
  starts: List(workspace_preparation.PreparedStart),
) -> List(PreparedStart) {
  list.map(starts, fn(start) {
    let workspace_preparation.PreparedStart(step: step, workspace: workspace) =
      start
    step_worker_pool.prepared_start(step, workspace)
  })
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
        Error(batch_error) -> {
          let reason = step_worker_pool.describe_step_batch_error(batch_error)
          let batch_cleanup_allowed =
            step_worker_pool.step_batch_error_cleanup_allowed(batch_error)
          mark_workflow_failed_terminal(
            dependencies,
            recovery_evidence,
            run_id,
            workflow_dag.id(dag),
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
        Ok(outcome) ->
          step_worker_pool.fold_step_batch_outcome(
            outcome,
            fn(results) {
              let result_by_step =
                results
                |> list.map(fn(result) {
                  #(step_worker_pool.step_result_step_id(result), result)
                })
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
            },
            fn(result, sibling_results, interrupted_step_ids, drained) {
              finish_fatal_batch_result(
                starts,
                result,
                sibling_results,
                interrupted_step_ids,
                drained,
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
            },
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
) -> Result(StepBatchOutcome, StepBatchError) {
  step_worker_pool.run_prepared_batch(
    starts,
    prepared_batch_timeout_ms(starts, orchestrator),
    fn(timeout) {
      step_batch_timeout_artifact(
        timeout,
        starts,
        orchestrator,
        secrets,
        orchestrator.artifact_limits,
      )
    },
    fn(step_id) {
      case prepared_start_by_step(starts, step_id) {
        Ok(start) -> {
          let step = step_worker_pool.prepared_start_step(start)
          let workspace = step_worker_pool.prepared_start_workspace(start)
          recovery_execution.effective_for_failure(
            dag,
            step,
            workspace.attempt_index,
          )
          != None
        }
        Error(Nil) -> False
      }
    },
    fn(step, workspace) {
      start_prepared_step(
        step,
        workspace,
        orchestrator,
        dependencies,
        pi_session_continuations,
      )
    },
    fn(step, workspace) {
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
      #(result.artifact, result.tokens, result.final_issue, result.turns)
    },
  )
}

fn step_batch_timeout_artifact(
  timeout: step_worker_pool.StepBatchTimeoutContext,
  starts: List(PreparedStart),
  orchestrator: config_types.OrchestratorConfig,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
) -> step_artifact.StepArtifact {
  let configured_timeout_ms =
    configured_timeout_ms_for_timeout(timeout, starts, orchestrator)
  let command_deadline_ms =
    command_deadline_monotonic_ms(
      timeout.batch_started_monotonic_ms,
      configured_timeout_ms,
    )
  let stderr =
    step_batch_timeout_stderr(
      timeout,
      configured_timeout_ms,
      command_deadline_ms,
    )
  step_artifact.from_command_result_with_metadata(
    timeout.step_id,
    timeout.command,
    124,
    Some(timeout.duration_ms),
    None,
    "",
    stderr,
    True,
    secrets,
    limits,
    False,
    False,
  )
}

fn step_batch_timeout_stderr(
  timeout: step_worker_pool.StepBatchTimeoutContext,
  configured_timeout_ms: Option(Int),
  command_deadline_ms: Option(Int),
) -> String {
  "SCHERZO_FAILURE_CODE="
  <> step_worker_pool.step_batch_timeout_failure_code
  <> "\nstep batch deadline exceeded after "
  <> int.to_string(timeout.duration_ms)
  <> "ms\n"
  <> "timeout_kind: step_batch_watchdog\n"
  <> "duration_ms: "
  <> int.to_string(timeout.duration_ms)
  <> "\n"
  <> option_string_line("diagnostic_step_id", timeout.diagnostic_step_id)
  <> option_int_line("configured_timeout_ms", configured_timeout_ms)
  <> option_int_line("deadline_monotonic_ms", command_deadline_ms)
  <> "batch_started_monotonic_ms: "
  <> int.to_string(timeout.batch_started_monotonic_ms)
  <> "\n"
  <> "batch_deadline_monotonic_ms: "
  <> int.to_string(timeout.batch_deadline_monotonic_ms)
  <> "\n"
  <> "timeout_monotonic_ms: "
  <> int.to_string(timeout.timed_out_monotonic_ms)
  <> "\n"
}

fn configured_timeout_ms_for_timeout(
  timeout: step_worker_pool.StepBatchTimeoutContext,
  starts: List(PreparedStart),
  orchestrator: config_types.OrchestratorConfig,
) -> Option(Int) {
  case timeout.diagnostic_step_id {
    Some(step_id) -> configured_step_timeout_ms(step_id, starts, orchestrator)
    None -> None
  }
}

fn configured_step_timeout_ms(
  step_id: String,
  starts: List(PreparedStart),
  orchestrator: config_types.OrchestratorConfig,
) -> Option(Int) {
  case prepared_start_by_step(starts, step_id) {
    Error(Nil) -> None
    Ok(start) ->
      Some(step_watchdog_timeout_ms(
        step_worker_pool.prepared_start_step(start),
        orchestrator,
      ))
  }
}

fn command_deadline_monotonic_ms(
  base_monotonic_ms: Int,
  configured_timeout_ms: Option(Int),
) -> Option(Int) {
  case configured_timeout_ms {
    Some(configured_timeout_ms) ->
      Some(base_monotonic_ms + configured_timeout_ms)
    None -> None
  }
}

fn option_string_line(label: String, value: Option(String)) -> String {
  case value {
    Some(value) -> label <> ": " <> value <> "\n"
    None -> ""
  }
}

fn option_int_line(label: String, value: Option(Int)) -> String {
  case value {
    Some(value) -> label <> ": " <> int.to_string(value) <> "\n"
    None -> ""
  }
}

const command_step_default_timeout_ms = 60_000

const step_batch_watchdog_grace_ms = 5000

const agent_step_watchdog_margin_ms = 60_000

fn prepared_batch_timeout_ms(
  starts: List(PreparedStart),
  orchestrator: config_types.OrchestratorConfig,
) -> Int {
  max_prepared_step_timeout_ms(starts, orchestrator, 0)
  + step_batch_watchdog_grace_ms
}

fn max_prepared_step_timeout_ms(
  starts: List(PreparedStart),
  orchestrator: config_types.OrchestratorConfig,
  max_ms: Int,
) -> Int {
  case starts {
    [] -> max_ms
    [start, ..rest] -> {
      let step = step_worker_pool.prepared_start_step(start)
      max_prepared_step_timeout_ms(
        rest,
        orchestrator,
        max_int(max_ms, step_watchdog_timeout_ms(step, orchestrator)),
      )
    }
  }
}

fn step_watchdog_timeout_ms(
  step: workflow_dag.WorkflowStep,
  orchestrator: config_types.OrchestratorConfig,
) -> Int {
  case step.kind {
    workflow_dag.CommandStep(_, timeout_ms) ->
      max_int(
        0,
        optional_timeout_ms(timeout_ms, command_step_default_timeout_ms),
      )
    workflow_dag.AgentStep(..) -> agent_step_watchdog_timeout_ms(orchestrator)
  }
}

fn optional_timeout_ms(timeout_ms: Option(Int), default_ms: Int) -> Int {
  case timeout_ms {
    Some(value) -> value
    None -> default_ms
  }
}

fn agent_step_watchdog_timeout_ms(
  orchestrator: config_types.OrchestratorConfig,
) -> Int {
  let pi = orchestrator.effective.pi
  let agent = orchestrator.effective.agent
  let per_turn_ms =
    max_int(0, pi.turn_timeout_ms)
    + max_int(0, pi.read_timeout_ms)
    + max_int(0, pi.stall_timeout_ms)
    + max_int(0, pi.ui_request_timeout_ms)
  let turn_budget_ms = max_int(1, agent.max_turns) * per_turn_ms
  turn_budget_ms + agent_step_watchdog_margin_ms
}

fn max_int(left: Int, right: Int) -> Int {
  case left > right {
    True -> left
    False -> right
  }
}

fn start_prepared_step(
  step: workflow_dag.WorkflowStep,
  workspace: workspace_run.PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
  dependencies: Dependencies,
  pi_session_continuations: Dict(String, workflow_attempt.PiContinuation),
) -> Result(Nil, String) {
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
  start_result
  |> result.map_error(fn(error) {
    "checkpoint_failed:" <> workflow_checkpoint.describe_error(error)
  })
}

fn finish_fatal_batch_result(
  starts: List(PreparedStart),
  result: StepExecutionResult,
  sibling_results: List(StepExecutionResult),
  interrupted_step_ids: List(String),
  drained: Bool,
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
  let result_step_id = step_worker_pool.step_result_step_id(result)
  let result_artifact = step_worker_pool.step_result_artifact(result)
  let result_tokens = step_worker_pool.step_result_tokens(result)
  let result_turns = step_worker_pool.step_result_turns(result)
  let result_final_issue = step_worker_pool.step_result_final_issue(result)
  let sibling_result_by_step =
    sibling_results
    |> list.map(fn(result) {
      #(step_worker_pool.step_result_step_id(result), result)
    })
    |> dict.from_list
  let sibling_starts =
    prepared_starts_with_results(
      starts,
      sibling_result_by_step,
      result_step_id,
      [],
    )
  use applied <- result.try(apply_prepared_results_state(
    sibling_starts,
    sibling_result_by_step,
    issue,
    dag,
    orchestrator,
    run_id,
    recovery_evidence,
    dependencies,
    scheduler_state,
    artifacts,
    run_root,
    tokens,
    final_issue,
    turns,
    cleanup_allowed,
    profile,
  ))
  let AppliedPreparedResults(
    scheduler_state: sibling_scheduler_state,
    artifacts: sibling_artifacts,
    tokens: sibling_tokens,
    final_issue: sibling_final_issue,
    turns: sibling_turns,
  ) = applied
  let failure_artifacts =
    dict.insert(sibling_artifacts, result_step_id, result_artifact)
  case prepared_start_by_step(starts, result_step_id) {
    Error(Nil) ->
      terminal_fatal_batch_failure(
        starts,
        interrupted_step_ids,
        result,
        issue,
        dag,
        run_id,
        workflow_fingerprint,
        contract_outputs_recorded,
        recovery_evidence,
        orchestrator,
        dependencies,
        failure_artifacts,
        prepared_workspaces,
        run_root,
        sibling_tokens.total + result_tokens.total,
        sibling_turns + result_turns,
        cleanup_allowed,
        profile,
        checkpoint_error: None,
      )
    Ok(start) -> {
      let step = step_worker_pool.prepared_start_step(start)
      let workspace = step_worker_pool.prepared_start_workspace(start)
      let finished =
        workflow_checkpoint.StepFinished(
          run_id: run_id,
          workflow_id: workflow_dag.id(dag),
          step_id: step.id,
          attempt_index: workspace.attempt_index,
          outcome: workflow_outcome.failed_fatal,
          workspace_name: workspace.workspace_name,
          workspace_path: workspace.path,
          token_total: result_tokens.total,
          turns: result_turns,
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
          result_artifact,
        )
      {
        Error(error) ->
          terminal_fatal_batch_failure(
            starts,
            interrupted_step_ids,
            result,
            issue,
            dag,
            run_id,
            workflow_fingerprint,
            contract_outputs_recorded,
            recovery_evidence,
            orchestrator,
            dependencies,
            failure_artifacts,
            prepared_workspaces,
            run_root,
            sibling_tokens.total + result_tokens.total,
            sibling_turns + result_turns,
            cleanup_allowed,
            profile,
            checkpoint_error: Some(error),
          )
        Ok(_) -> {
          let batch_safe_to_retry = drained || list.length(starts) <= 1
          case
            command_step_batch_timeout_retry_attempt(
              step,
              workspace,
              result_artifact,
              batch_safe_to_retry,
              interrupted_step_ids,
            )
          {
            Some(retry_attempt_index) ->
              case
                record_command_step_timeout_retry_scheduled(
                  dependencies,
                  run_id,
                  workflow_dag.id(dag),
                  issue.id,
                  step.id,
                  workspace.attempt_index,
                  retry_attempt_index,
                )
              {
                Error(error) ->
                  terminal_fatal_batch_failure(
                    starts,
                    interrupted_step_ids,
                    result,
                    issue,
                    dag,
                    run_id,
                    workflow_fingerprint,
                    contract_outputs_recorded,
                    recovery_evidence,
                    orchestrator,
                    dependencies,
                    failure_artifacts,
                    prepared_workspaces,
                    run_root,
                    sibling_tokens.total + result_tokens.total,
                    sibling_turns + result_turns,
                    cleanup_allowed,
                    profile,
                    checkpoint_error: Some(error),
                  )
                Ok(Nil) -> {
                  let scheduler_state =
                    workflow_scheduler.mark_pending(
                      sibling_scheduler_state,
                      step.id,
                    )
                  let tokens = add_tokens(sibling_tokens, result_tokens)
                  let final_issue =
                    latest_final_issue(sibling_final_issue, result_final_issue)
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
                    dict.delete(failure_artifacts, step.id),
                    prepared_workspaces,
                    run_root,
                    attempt_indexes,
                    tokens,
                    final_issue,
                    sibling_turns + result_turns,
                    cleanup_allowed,
                    pi_session_continuations,
                    profile,
                  )
                }
              }
            None -> {
              let Nil =
                record_command_step_timeout_retry_exhausted_if_needed(
                  dependencies,
                  run_id,
                  workflow_dag.id(dag),
                  issue.id,
                  step,
                  workspace,
                  result_artifact,
                )
              case drained {
                True ->
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
                          result_artifact,
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
                        recovery_execution.RecoveryRecheckRequested(
                          recovery_tokens,
                          recovery_final_issue,
                          recovery_turns,
                        ) -> {
                          let scheduler_state =
                            workflow_scheduler.mark_pending(
                              sibling_scheduler_state,
                              step.id,
                            )
                          let tokens =
                            add_tokens(
                              add_tokens(sibling_tokens, result_tokens),
                              recovery_tokens,
                            )
                          let final_issue =
                            latest_final_issue(
                              latest_final_issue(
                                sibling_final_issue,
                                result_final_issue,
                              ),
                              recovery_final_issue,
                            )
                          let fresh_recheck_continuations =
                            dict.delete(pi_session_continuations, step.id)
                          loop(
                            issue,
                            dag,
                            orchestrator,
                            tracker_client,
                            secrets,
                            run_id,
                            workflow_fingerprint,
                            contract_outputs_recorded,
                            workflow_outcome.StepRecoveryRecheckRequested,
                            recovered_execution,
                            dependencies,
                            scheduler_state,
                            dict.delete(failure_artifacts, step.id),
                            prepared_workspaces,
                            run_root,
                            attempt_indexes,
                            tokens,
                            final_issue,
                            sibling_turns + result_turns + recovery_turns,
                            cleanup_allowed,
                            fresh_recheck_continuations,
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
                            interrupted_step_ids,
                            result,
                            issue,
                            dag,
                            run_id,
                            workflow_fingerprint,
                            contract_outputs_recorded,
                            recovery_evidence,
                            orchestrator,
                            dependencies,
                            failure_artifacts,
                            prepared_workspaces,
                            run_root,
                            sibling_tokens.total
                              + result_tokens.total
                              + recovery_tokens.total,
                            sibling_turns + result_turns + recovery_turns,
                            cleanup_allowed,
                            profile,
                            checkpoint_error: None,
                          )
                        }
                      }
                    None ->
                      terminal_fatal_batch_failure(
                        starts,
                        interrupted_step_ids,
                        result,
                        issue,
                        dag,
                        run_id,
                        workflow_fingerprint,
                        contract_outputs_recorded,
                        recovery_evidence,
                        orchestrator,
                        dependencies,
                        failure_artifacts,
                        prepared_workspaces,
                        run_root,
                        sibling_tokens.total + result_tokens.total,
                        sibling_turns + result_turns,
                        cleanup_allowed,
                        profile,
                        checkpoint_error: None,
                      )
                  }
                False ->
                  terminal_fatal_batch_failure(
                    starts,
                    interrupted_step_ids,
                    result,
                    issue,
                    dag,
                    run_id,
                    workflow_fingerprint,
                    contract_outputs_recorded,
                    recovery_evidence,
                    orchestrator,
                    dependencies,
                    failure_artifacts,
                    prepared_workspaces,
                    run_root,
                    sibling_tokens.total + result_tokens.total,
                    sibling_turns + result_turns,
                    cleanup_allowed,
                    profile,
                    checkpoint_error: None,
                  )
              }
            }
          }
        }
      }
    }
  }
}

fn command_step_batch_timeout_retry_attempt(
  step: workflow_dag.WorkflowStep,
  workspace: workspace_run.PreparedStepWorkspace,
  artifact: step_artifact.StepArtifact,
  batch_safe_to_retry: Bool,
  interrupted_step_ids: List(String),
) -> Option(Int) {
  command_step_timeout_retry.next_retry_attempt(
    is_command_step: is_command_step(step),
    artifact: artifact,
    batch_safe_to_retry: batch_safe_to_retry,
    interrupted_step_ids: interrupted_step_ids,
    attempt_index: workspace.attempt_index,
  )
}

fn record_command_step_timeout_retry_scheduled(
  dependencies: Dependencies,
  run_id: String,
  workflow_id: String,
  issue_id: String,
  step_id: String,
  failed_attempt_index: Int,
  retry_attempt_index: Int,
) -> Result(Nil, workflow_checkpoint.CheckpointError) {
  dependencies.checkpoint.workflow_diagnostic(
    workflow_checkpoint.WorkflowDiagnostic(
      run_id: run_id,
      workflow_id: workflow_id,
      issue_id: issue_id,
      reason: command_step_timeout_retry.retry_scheduled_diagnostic_reason(
        step_id,
        failed_attempt_index,
        retry_attempt_index,
      ),
    ),
  )
}

fn record_command_step_timeout_retry_exhausted_if_needed(
  dependencies: Dependencies,
  run_id: String,
  workflow_id: String,
  issue_id: String,
  step: workflow_dag.WorkflowStep,
  workspace: workspace_run.PreparedStepWorkspace,
  artifact: step_artifact.StepArtifact,
) -> Nil {
  case
    command_step_timeout_retry.retry_exhausted(
      is_command_step: is_command_step(step),
      artifact: artifact,
      attempt_index: workspace.attempt_index,
    )
  {
    True ->
      ignore_secondary_checkpoint_result(
        dependencies.checkpoint.workflow_diagnostic(
          workflow_checkpoint.WorkflowDiagnostic(
            run_id: run_id,
            workflow_id: workflow_id,
            issue_id: issue_id,
            reason: command_step_timeout_retry.retry_exhausted_diagnostic_reason(
              step.id,
              workspace.attempt_index,
            ),
          ),
        ),
      )
    False -> Nil
  }
}

fn is_command_step(step: workflow_dag.WorkflowStep) -> Bool {
  case step.kind {
    workflow_dag.CommandStep(..) -> True
    workflow_dag.AgentStep(..) -> False
  }
}

fn terminal_fatal_batch_failure(
  starts: List(PreparedStart),
  interrupted_step_ids: List(String),
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
  let result_step_id = step_worker_pool.step_result_step_id(result)
  let result_artifact = step_worker_pool.step_result_artifact(result)
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
      failed_step_id: result_step_id,
      failed_artifact: result_artifact,
      failed_step_reason_override: command_step_timeout_terminal_reason(
        starts,
        result_step_id,
        result_artifact,
      ),
      agent_reason: step_execution.agent_reason_for_artifact(result_artifact),
      checkpoint_error: checkpoint_error,
      interrupt_active_attempts: fn() {
        mark_selected_prepared_attempts_interrupted(
          starts,
          interrupted_step_ids,
          dependencies,
          workflow_dag.id(dag),
          "fatal_sibling_finished",
        )
      },
    ),
  )
  |> terminal_result_to_workflow_result
}

fn command_step_timeout_terminal_reason(
  starts: List(PreparedStart),
  step_id: String,
  artifact: step_artifact.StepArtifact,
) -> Option(String) {
  case prepared_start_for_step(starts, step_id) {
    Some(#(step, workspace)) ->
      case
        command_step_timeout_retry.retry_exhausted(
          is_command_step: is_command_step(step),
          artifact: artifact,
          attempt_index: workspace.attempt_index,
        )
      {
        True -> Some(command_step_timeout_retry.terminal_reason(step_id))
        False -> None
      }
    None -> None
  }
}

fn prepared_start_for_step(
  starts: List(PreparedStart),
  step_id: String,
) -> Option(#(workflow_dag.WorkflowStep, workspace_run.PreparedStepWorkspace)) {
  case starts {
    [] -> None
    [start, ..rest] -> {
      let step = step_worker_pool.prepared_start_step(start)
      case step.id == step_id {
        True -> Some(#(step, step_worker_pool.prepared_start_workspace(start)))
        False -> prepared_start_for_step(rest, step_id)
      }
    }
  }
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
    |> result.map_error(workflow_checkpoint.CheckpointAppendFailed),
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
      make_context: fn() {
        structured_output_step.recovery_context(
          step_context_internal.from_prepared(
            step,
            workspace,
            issue,
            orchestrator,
            profile,
          ),
        )
      },
      make_attempt_context: fn(context, prompt_mode) {
        step_execution.workflow_attempt_context(
          context,
          dag,
          orchestrator,
          prompt_mode,
          None,
        )
      },
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

fn mark_selected_prepared_attempts_interrupted(
  starts: List(PreparedStart),
  interrupted_step_ids: List(String),
  dependencies: Dependencies,
  workflow_id: String,
  reason: String,
) -> Nil {
  case starts {
    [] -> Nil
    [start, ..rest] -> {
      let step = step_worker_pool.prepared_start_step(start)
      let workspace = step_worker_pool.prepared_start_workspace(start)
      case list.contains(interrupted_step_ids, step.id) {
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
      mark_selected_prepared_attempts_interrupted(
        rest,
        interrupted_step_ids,
        dependencies,
        workflow_id,
        reason,
      )
    }
  }
}

fn prepared_start_ids(
  starts: List(PreparedStart),
  acc: List(String),
) -> List(String) {
  case starts {
    [] -> list.reverse(acc)
    [start, ..rest] ->
      prepared_start_ids(rest, [
        step_worker_pool.prepared_start_step_id(start),
        ..acc
      ])
  }
}

fn prepared_starts_with_results(
  starts: List(PreparedStart),
  result_by_step: Dict(String, StepExecutionResult),
  skipped_step_id: String,
  acc: List(PreparedStart),
) -> List(PreparedStart) {
  case starts {
    [] -> list.reverse(acc)
    [start, ..rest] -> {
      let step_id = step_worker_pool.prepared_start_step_id(start)
      case step_id != skipped_step_id && dict.has_key(result_by_step, step_id) {
        True ->
          prepared_starts_with_results(rest, result_by_step, skipped_step_id, [
            start,
            ..acc
          ])
        False ->
          prepared_starts_with_results(
            rest,
            result_by_step,
            skipped_step_id,
            acc,
          )
      }
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
      case step_worker_pool.prepared_start_step_id(start) == step_id {
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
  use applied <- result.try(apply_prepared_results_state(
    starts,
    result_by_step,
    issue,
    dag,
    orchestrator,
    run_id,
    recovery_evidence,
    dependencies,
    scheduler_state,
    artifacts,
    run_root,
    tokens,
    final_issue,
    turns,
    cleanup_allowed,
    profile,
  ))
  let AppliedPreparedResults(
    scheduler_state: scheduler_state,
    artifacts: artifacts,
    tokens: tokens,
    final_issue: final_issue,
    turns: turns,
  ) = applied
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
}

fn apply_prepared_results_state(
  starts: List(PreparedStart),
  result_by_step: Dict(String, StepExecutionResult),
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  run_id: String,
  recovery_evidence: workflow_outcome.RecoveryEvidence,
  dependencies: Dependencies,
  scheduler_state: workflow_scheduler.SchedulerState,
  artifacts: Dict(String, step_artifact.StepArtifact),
  run_root: Option(String),
  tokens: session_tokens.TokenTotals,
  final_issue: Option(tracker_issue.Issue),
  turns: Int,
  cleanup_allowed: Bool,
  profile: config_types.WorkspaceHookProfile,
) -> Result(AppliedPreparedResults, WorkflowRunFailure) {
  case starts {
    [] ->
      Ok(AppliedPreparedResults(
        scheduler_state: scheduler_state,
        artifacts: artifacts,
        tokens: tokens,
        final_issue: final_issue,
        turns: turns,
      ))
    [start, ..rest] -> {
      let step = step_worker_pool.prepared_start_step(start)
      let workspace = step_worker_pool.prepared_start_workspace(start)
      case dict.get(result_by_step, step.id) {
        Error(Nil) -> {
          mark_workflow_failed_terminal(
            dependencies,
            recovery_evidence,
            run_id,
            workflow_dag.id(dag),
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
          let result_artifact = step_worker_pool.step_result_artifact(result)
          let result_tokens = step_worker_pool.step_result_tokens(result)
          let result_turns = step_worker_pool.step_result_turns(result)
          let result_final_issue =
            step_worker_pool.step_result_final_issue(result)
          let outcome =
            workflow_checkpoint.step_outcome(
              result_artifact,
              on_failure: step.on_failure == workflow_dag.ContinueWorkflow,
            )
          let finished =
            workflow_checkpoint.StepFinished(
              run_id: run_id,
              workflow_id: workflow_dag.id(dag),
              step_id: step.id,
              attempt_index: workspace.attempt_index,
              outcome: outcome,
              workspace_name: workspace.workspace_name,
              workspace_path: workspace.path,
              token_total: result_tokens.total,
              turns: result_turns,
            )
          case
            dependencies.checkpoint.write_step_artifact(
              finished,
              result_artifact,
            )
          {
            Error(error) -> {
              mark_workflow_failed_terminal(
                dependencies,
                recovery_evidence,
                run_id,
                workflow_dag.id(dag),
                issue.id,
                task_ref(issue),
                tokens.total + result_tokens.total,
                turns + result_turns,
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
                  let reason = error
                  mark_workflow_failed_terminal(
                    dependencies,
                    recovery_evidence,
                    run_id,
                    workflow_dag.id(dag),
                    issue.id,
                    task_ref(issue),
                    tokens.total + result_tokens.total,
                    turns + result_turns,
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
                        workflow_dag.id(dag),
                        issue.id,
                        task_ref(issue),
                        tokens.total + result_tokens.total,
                        turns + result_turns,
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
                    Ok(Nil) ->
                      apply_prepared_results_state(
                        rest,
                        result_by_step,
                        issue,
                        dag,
                        orchestrator,
                        run_id,
                        recovery_evidence,
                        dependencies,
                        workflow_scheduler.mark_finished(
                          scheduler_state,
                          step.id,
                          result_artifact,
                        ),
                        dict.insert(artifacts, step.id, result_artifact),
                        run_root,
                        add_tokens(tokens, result_tokens),
                        latest_final_issue(final_issue, result_final_issue),
                        turns + result_turns,
                        cleanup_allowed,
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

fn run_after_step(
  dependencies: Dependencies,
  issue: tracker_issue.Issue,
  step_id: String,
  workspace: workspace_run.PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
) -> Result(Nil, String) {
  step_worker_pool.run_after_step(step_id, fn() {
    dependencies.after_step(issue, step_id, workspace, orchestrator, profile)
  })
  |> result.map_error(step_worker_pool.describe_after_step_error)
}

fn mark_all_running(
  state: workflow_scheduler.SchedulerState,
  starts: List(PreparedStart),
) -> workflow_scheduler.SchedulerState {
  case starts {
    [] -> state
    [start, ..rest] -> {
      let step_id = step_worker_pool.prepared_start_step_id(start)
      mark_all_running(workflow_scheduler.mark_running(state, step_id), rest)
    }
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
  mark_selected_prepared_attempts_interrupted(
    active_attempts,
    prepared_start_ids(active_attempts, []),
    dependencies,
    workflow_id,
    "terminal_failure",
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
