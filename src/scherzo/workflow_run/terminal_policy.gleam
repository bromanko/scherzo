import gleam/dict.{type Dict}
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/artifact_publication_executor
import scherzo/artifact_publication_recording
import scherzo/config/types as config_types
import scherzo/error
import scherzo/session/tokens as session_tokens
import scherzo/step_artifact
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_checkpoint
import scherzo/workflow_dag
import scherzo/workflow_outcome
import scherzo/workflow_run/contract_io
import scherzo/workflow_run/contract_io_error as contract_error
import scherzo/workflow_run/workstream_handoff
import scherzo/workspace_run

pub type PostSuccessCleanupWarning {
  PostSuccessCleanupWarning(code: String, message: String, run_root: String)
}

pub type Success {
  Success(
    worker_success: agent_types.WorkerSuccess,
    artifacts: Dict(String, step_artifact.StepArtifact),
    run_root: String,
    cleanup_warning: Option(PostSuccessCleanupWarning),
  )
}

pub type Failure {
  Failure(
    reason: String,
    agent_reason: Option(error.AgentRunnerError),
    artifacts: Dict(String, step_artifact.StepArtifact),
    run_root: Option(String),
    failed_step_id: Option(String),
  )
}

pub opaque type Runtime {
  Runtime(
    checkpoint: workflow_checkpoint.Writer,
    cleanup_run: fn(
      String,
      config_types.OrchestratorConfig,
      config_types.WorkspaceHookProfile,
    ) -> Result(Nil, error.WorkspaceError),
  )
}

pub fn runtime(
  checkpoint checkpoint: workflow_checkpoint.Writer,
  cleanup_run cleanup_run: fn(
    String,
    config_types.OrchestratorConfig,
    config_types.WorkspaceHookProfile,
  ) -> Result(Nil, error.WorkspaceError),
) -> Runtime {
  Runtime(checkpoint: checkpoint, cleanup_run: cleanup_run)
}

type PublicationPolicyError {
  PublicationPolicyError(reason: String)
}

type WorkstreamHandoffError {
  WorkstreamHandoffError(reason: workstream_handoff.HandoffError)
}

pub type SuccessInput {
  SuccessInput(
    issue: tracker_issue.Issue,
    final_issue: tracker_issue.Issue,
    dag: workflow_dag.WorkflowDag,
    orchestrator: config_types.OrchestratorConfig,
    run_id: String,
    workflow_fingerprint: String,
    contract_outputs_recorded: Option(workflow_checkpoint.ArtifactWritten),
    recovery_evidence: workflow_outcome.RecoveryEvidence,
    runtime: Runtime,
    artifacts: Dict(String, step_artifact.StepArtifact),
    prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
    run_root: Option(String),
    tokens: session_tokens.TokenTotals,
    turns: Int,
    cleanup_allowed: Bool,
    profile: config_types.WorkspaceHookProfile,
  )
}

pub type FailureInput {
  FailureInput(
    issue: tracker_issue.Issue,
    dag: workflow_dag.WorkflowDag,
    orchestrator: config_types.OrchestratorConfig,
    run_id: String,
    workflow_fingerprint: String,
    contract_outputs_recorded: Option(workflow_checkpoint.ArtifactWritten),
    recovery_evidence: workflow_outcome.RecoveryEvidence,
    runtime: Runtime,
    artifacts: Dict(String, step_artifact.StepArtifact),
    prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
    run_root: Option(String),
    token_total: Int,
    turns: Int,
    cleanup_allowed: Bool,
    profile: config_types.WorkspaceHookProfile,
  )
}

pub type FatalStepFailureInput {
  FatalStepFailureInput(
    issue: tracker_issue.Issue,
    dag: workflow_dag.WorkflowDag,
    orchestrator: config_types.OrchestratorConfig,
    run_id: String,
    workflow_fingerprint: String,
    contract_outputs_recorded: Option(workflow_checkpoint.ArtifactWritten),
    recovery_evidence: workflow_outcome.RecoveryEvidence,
    runtime: Runtime,
    artifacts: Dict(String, step_artifact.StepArtifact),
    prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
    run_root: Option(String),
    workflow_finished_token_total: Int,
    workflow_finished_turns: Int,
    cleanup_allowed: Bool,
    profile: config_types.WorkspaceHookProfile,
    failed_step_id: String,
    failed_artifact: step_artifact.StepArtifact,
    agent_reason: Option(error.AgentRunnerError),
    checkpoint_error: Option(workflow_checkpoint.CheckpointError),
    interrupt_active_attempts: fn() -> Nil,
  )
}

pub fn finish_success(input: SuccessInput) -> Result(Success, Failure) {
  let result =
    step_artifact.workflow_result_artifact(
      input.dag,
      input.artifacts,
      input.orchestrator.artifact_limits,
    )
  let workspace_path = case input.run_root {
    Some(path) -> path
    None -> ""
  }
  case
    contract_io.record_outputs_if_contracted(
      input.dag,
      input.run_id,
      input.workflow_fingerprint,
      input.contract_outputs_recorded,
      input.runtime.checkpoint,
      input.artifacts,
      input.prepared_workspaces,
    )
  {
    Ok(outputs) if outputs.missing == [] ->
      case
        record_publications_if_configured(
          input.final_issue,
          input.dag,
          input.orchestrator,
          outputs,
          input.run_id,
          input.runtime.checkpoint,
        )
      {
        Ok(publication_result) ->
          case publication_result.required_failures {
            [] ->
              case
                emit_workstream_handoff_if_configured(
                  input.issue,
                  input.dag,
                  input.run_id,
                  input.workflow_fingerprint,
                  outputs,
                  input.runtime.checkpoint,
                )
              {
                Ok(Nil) -> {
                  use Nil <- result_try_checkpoint(
                    input.runtime.checkpoint.workflow_finished(
                      workflow_checkpoint.WorkflowFinished(
                        run_id: input.run_id,
                        workflow_id: input.dag.id,
                        issue_id: input.issue.id,
                        task_ref: task_ref(input.issue),
                        outcome: workflow_outcome.terminal_success(
                          input.recovery_evidence,
                        ),
                        token_total: input.tokens.total,
                        turns: input.turns,
                      ),
                    ),
                    input.artifacts,
                    input.run_root,
                    None,
                  )
                  let cleanup_result =
                    cleanup_if_allowed(
                      input.run_root,
                      input.orchestrator,
                      input.profile,
                      input.runtime,
                      input.cleanup_allowed,
                    )
                  case cleanup_result {
                    Ok(Nil) ->
                      Ok(Success(
                        worker_success: agent_types.WorkerSuccess(
                          final_issue: Some(input.final_issue),
                          final_classification: agent_types.FinalTerminal,
                          workspace_path: workspace_path,
                          tokens: input.tokens,
                          turns: input.turns,
                          result: result,
                        ),
                        artifacts: input.artifacts,
                        run_root: workspace_path,
                        cleanup_warning: None,
                      ))
                    Error(err) -> {
                      let cleanup_code = error.workspace_code(err)
                      let cleanup_reason =
                        "post_success_cleanup_failed:"
                        <> cleanup_code
                        <> "; run_root="
                        <> workspace_path
                      let warning_message = case
                        input.runtime.checkpoint.workflow_diagnostic(
                          workflow_checkpoint.WorkflowDiagnostic(
                            run_id: input.run_id,
                            workflow_id: input.dag.id,
                            issue_id: input.issue.id,
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
                      Ok(Success(
                        worker_success: agent_types.WorkerSuccess(
                          final_issue: Some(input.final_issue),
                          final_classification: agent_types.FinalTerminal,
                          workspace_path: workspace_path,
                          tokens: input.tokens,
                          turns: input.turns,
                          result: result,
                        ),
                        artifacts: input.artifacts,
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
                Error(error) ->
                  terminal_success_blocker_failure(
                    input,
                    "workflow_workstream_handoff_failed:"
                      <> workstream_handoff.describe_error(error.reason),
                  )
              }
            [failure, ..] ->
              terminal_success_blocker_failure(
                input,
                "workflow_publication_required_failed:"
                  <> failure.publication_id
                  <> ":"
                  <> failure.code,
              )
          }
        Error(error) ->
          terminal_success_blocker_failure(
            input,
            "workflow_publication_recording_failed:" <> error.reason,
          )
      }
    Ok(outputs) -> {
      let missing = case outputs.missing {
        [missing, ..] -> missing
        [] -> "unknown"
      }
      terminal_success_blocker_failure(
        input,
        "workflow_required_output_missing:" <> missing,
      )
    }
    Error(error) ->
      terminal_success_blocker_failure(
        input,
        "workflow_output_manifest_failed:"
          <> contract_error.describe_error(error),
      )
  }
}

fn terminal_success_blocker_failure(
  input: SuccessInput,
  reason: String,
) -> Result(Success, Failure) {
  use Nil <- result_try_checkpoint(
    input.runtime.checkpoint.workflow_finished(
      workflow_checkpoint.WorkflowFinished(
        run_id: input.run_id,
        workflow_id: input.dag.id,
        issue_id: input.issue.id,
        task_ref: task_ref(input.issue),
        outcome: workflow_outcome.terminal_failed_fatal(input.recovery_evidence),
        token_total: input.tokens.total,
        turns: input.turns,
      ),
    ),
    input.artifacts,
    input.run_root,
    None,
  )
  let cleanup_suffix =
    cleanup_failure_suffix(cleanup_if_allowed(
      input.run_root,
      input.orchestrator,
      input.profile,
      input.runtime,
      input.cleanup_allowed,
    ))
  Error(Failure(
    reason: reason <> cleanup_suffix,
    agent_reason: None,
    artifacts: input.artifacts,
    run_root: input.run_root,
    failed_step_id: None,
  ))
}

pub fn finish_scheduler_failure(
  input: FailureInput,
) -> Result(Success, Failure) {
  let output_suffix = case
    contract_io.record_outputs_if_contracted(
      input.dag,
      input.run_id,
      input.workflow_fingerprint,
      input.contract_outputs_recorded,
      input.runtime.checkpoint,
      input.artifacts,
      input.prepared_workspaces,
    )
  {
    Ok(_) -> ""
    Error(error) ->
      "; workflow_output_manifest_failed:"
      <> contract_error.describe_error(error)
  }
  let cleanup_suffix =
    cleanup_failure_suffix(cleanup_if_allowed(
      input.run_root,
      input.orchestrator,
      input.profile,
      input.runtime,
      input.cleanup_allowed,
    ))
  use Nil <- result_try_checkpoint(
    input.runtime.checkpoint.workflow_finished(
      workflow_checkpoint.WorkflowFinished(
        run_id: input.run_id,
        workflow_id: input.dag.id,
        issue_id: input.issue.id,
        task_ref: task_ref(input.issue),
        outcome: workflow_outcome.terminal_failed_fatal(input.recovery_evidence),
        token_total: input.token_total,
        turns: input.turns,
      ),
    ),
    input.artifacts,
    input.run_root,
    None,
  )
  Error(Failure(
    reason: "workflow_step_failed" <> output_suffix <> cleanup_suffix,
    agent_reason: None,
    artifacts: input.artifacts,
    run_root: input.run_root,
    failed_step_id: None,
  ))
}

pub fn finish_fatal_step_failure(
  input: FatalStepFailureInput,
) -> Result(Success, Failure) {
  let reason = case input.checkpoint_error {
    Some(error) ->
      "checkpoint_failed:" <> workflow_checkpoint.describe_error(error)
    None -> step_failed_reason(input.failed_step_id, input.failed_artifact)
  }
  let output_suffix = case input.checkpoint_error {
    Some(_) -> ""
    None ->
      case
        contract_io.record_outputs_if_contracted(
          input.dag,
          input.run_id,
          input.workflow_fingerprint,
          input.contract_outputs_recorded,
          input.runtime.checkpoint,
          input.artifacts,
          input.prepared_workspaces,
        )
      {
        Ok(_) -> ""
        Error(error) ->
          "; workflow_output_manifest_failed:"
          <> contract_error.describe_error(error)
      }
  }
  input.interrupt_active_attempts()
  ignore_secondary_checkpoint_result(
    input.runtime.checkpoint.workflow_finished(
      workflow_checkpoint.WorkflowFinished(
        run_id: input.run_id,
        workflow_id: input.dag.id,
        issue_id: input.issue.id,
        task_ref: task_ref(input.issue),
        outcome: workflow_outcome.terminal_failed_fatal(input.recovery_evidence),
        token_total: input.workflow_finished_token_total,
        turns: input.workflow_finished_turns,
      ),
    ),
  )
  let cleanup_suffix =
    cleanup_failure_suffix(cleanup_if_allowed(
      input.run_root,
      input.orchestrator,
      input.profile,
      input.runtime,
      input.cleanup_allowed,
    ))
  Error(Failure(
    reason: reason <> output_suffix <> cleanup_suffix,
    agent_reason: input.agent_reason,
    artifacts: input.artifacts,
    run_root: input.run_root,
    failed_step_id: Some(input.failed_step_id),
  ))
}

pub fn mark_workflow_failed_terminal(
  checkpoint: workflow_checkpoint.Writer,
  recovery_evidence: workflow_outcome.RecoveryEvidence,
  run_id: String,
  workflow_id: String,
  issue_id: String,
  task_ref: Option(workflow_checkpoint.TaskRef),
  token_total: Int,
  turns: Int,
  interrupt_active_attempts: fn() -> Nil,
) -> Nil {
  interrupt_active_attempts()
  ignore_secondary_checkpoint_result(
    checkpoint.workflow_finished(workflow_checkpoint.WorkflowFinished(
      run_id: run_id,
      workflow_id: workflow_id,
      issue_id: issue_id,
      task_ref: task_ref,
      outcome: workflow_outcome.terminal_failed_fatal(recovery_evidence),
      token_total: token_total,
      turns: turns,
    )),
  )
}

pub fn failure_report(
  reason: String,
  failed_step_id: Option(String),
  artifacts: Dict(String, step_artifact.StepArtifact),
) -> String {
  case failed_command_artifact(failed_step_id, artifacts) {
    Some(artifact) ->
      case step_artifact.command_failure_summary(artifact) {
        Some(summary) ->
          workflow_command_failure_prefix(artifact) <> reason <> "\n" <> summary
        None -> reason
      }
    None -> reason
  }
}

pub fn failed_command_failure(
  failed_step_id: Option(String),
  artifacts: Dict(String, step_artifact.StepArtifact),
) -> Option(#(String, String)) {
  case failed_command_artifact(failed_step_id, artifacts) {
    Some(artifact) ->
      case artifact.failure_code {
        Some(code) -> Some(#(code, artifact.step_id))
        None -> None
      }
    None -> None
  }
}

fn failed_command_artifact(
  failed_step_id: Option(String),
  artifacts: Dict(String, step_artifact.StepArtifact),
) -> Option(step_artifact.StepArtifact) {
  case failed_step_id {
    Some(step_id) ->
      case dict.get(artifacts, step_id) {
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

fn step_failed_reason(
  step_id: String,
  artifact: step_artifact.StepArtifact,
) -> String {
  case artifact.failure_code {
    Some(code) ->
      case string.starts_with(code, "structured_output_") {
        True -> "workflow_step_failed:" <> code <> ":step=" <> step_id
        False -> "workflow_step_failed"
      }
    None -> "workflow_step_failed"
  }
}

pub fn cleanup_if_allowed(
  run_root: Option(String),
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
  runtime: Runtime,
  allowed: Bool,
) -> Result(Nil, error.WorkspaceError) {
  case allowed {
    True -> cleanup_if_needed(run_root, orchestrator, profile, runtime)
    False -> Ok(Nil)
  }
}

pub fn cleanup_failure_suffix(
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
  runtime: Runtime,
) -> Result(Nil, error.WorkspaceError) {
  case run_root {
    None -> Ok(Nil)
    Some(path) -> runtime.cleanup_run(path, orchestrator, profile)
  }
}

fn result_try_checkpoint(
  result: Result(Nil, workflow_checkpoint.CheckpointError),
  artifacts: Dict(String, step_artifact.StepArtifact),
  run_root: Option(String),
  failed_step_id: Option(String),
  next: fn(Nil) -> Result(Success, Failure),
) -> Result(Success, Failure) {
  case result {
    Ok(Nil) -> next(Nil)
    Error(error) ->
      Error(Failure(
        reason: "checkpoint_failed:"
          <> workflow_checkpoint.describe_error(error),
        agent_reason: None,
        artifacts: artifacts,
        run_root: run_root,
        failed_step_id: failed_step_id,
      ))
  }
}

pub fn ignore_secondary_checkpoint_result(
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

fn record_publications_if_configured(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  outputs: contract_io.ContractOutputsResult,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(
  artifact_publication_recording.PublicationRecordingResult,
  PublicationPolicyError,
) {
  case outputs.manifest {
    Some(output_manifest) ->
      case
        artifact_publication_executor.execute_routes(
          dag.publication_routes,
          orchestrator.artifact_repositories,
          orchestrator.config_dir,
          output_manifest,
          issue,
          run_id,
          checkpoint,
        )
      {
        Ok(result) -> Ok(result)
        Error(reason) -> Error(PublicationPolicyError(reason))
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

fn emit_workstream_handoff_if_configured(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  run_id: String,
  workflow_fingerprint: String,
  outputs: contract_io.ContractOutputsResult,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(Nil, WorkstreamHandoffError) {
  case
    workstream_handoff.emit_if_configured(
      issue,
      dag,
      run_id,
      workflow_fingerprint,
      outputs,
      checkpoint,
    )
  {
    Ok(Nil) -> Ok(Nil)
    Error(reason) -> Error(WorkstreamHandoffError(reason))
  }
}

fn task_ref(issue: tracker_issue.Issue) -> Option(workflow_checkpoint.TaskRef) {
  workflow_checkpoint.linear_task_ref_for_issue(
    issue.id,
    issue.identifier,
    issue.url,
  )
}
