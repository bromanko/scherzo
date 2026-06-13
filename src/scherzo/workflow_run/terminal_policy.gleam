import gleam/dict.{type Dict}
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/config/types as config_types
import scherzo/error
import scherzo/step_artifact
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_checkpoint
import scherzo/workflow_dag
import scherzo/workflow_outcome
import scherzo/workflow_run/contract_io
import scherzo/workflow_run/contract_io_error as contract_error
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

fn task_ref(issue: tracker_issue.Issue) -> Option(workflow_checkpoint.TaskRef) {
  workflow_checkpoint.linear_task_ref_for_issue(
    issue.id,
    issue.identifier,
    issue.url,
  )
}
