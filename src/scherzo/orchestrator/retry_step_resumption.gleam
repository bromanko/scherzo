import gleam/result
import scherzo/config/types as config_types
import scherzo/orchestrator/retry_issue_reactivation
import scherzo/retry_step_validation
import scherzo/runtime_bundle
import scherzo/state/recovery
import scherzo/tracker/adapter
import scherzo/workflow_dag
import scherzo/workflow_fingerprint
import scherzo/workspace_profile

pub type Validation {
  Validation(
    dag: workflow_dag.WorkflowDag,
    profile: config_types.WorkspaceHookProfile,
  )
}

pub fn reactivate_issue(
  tracker_adapter: adapter.TrackerAdapter,
  effective: config_types.EffectiveConfig,
  recovered: recovery.RecoveredWorkflowRun,
) -> Result(recovery.RecoveredWorkflowRun, #(String, String)) {
  case
    retry_issue_reactivation.for_operator_retry(
      tracker_adapter,
      effective,
      recovered.issue,
      "operator_retry_step",
    )
  {
    Ok(issue) -> Ok(recovery.RecoveredWorkflowRun(..recovered, issue: issue))
    Error(retry_issue_reactivation.ReactivationError(reason, message)) ->
      Error(#(reason, message))
  }
}

pub fn validate(
  bundle: runtime_bundle.RuntimeBundle,
  recovered: recovery.RecoveredWorkflowRun,
) -> Result(Validation, retry_step_validation.Failure) {
  case runtime_bundle.select_workflow(bundle, recovered.issue) {
    Error(runtime_bundle.BundleError(code, message)) ->
      Error(retry_step_validation.Failure(
        reason: "workflow_drift",
        message: "workflow unavailable: " <> code <> ":" <> message,
      ))
    Ok(#(workflow_id, dag)) -> {
      use current_fingerprint <- result.try(workflow_fingerprint_for_execution(
        dag,
        bundle.orchestrator,
      ))
      use _ <- result.try(retry_step_validation.validate_workflow_identity(
        recovered.workflow_id,
        workflow_id,
        recovered.workflow_fingerprint,
        current_fingerprint,
      ))
      case workspace_profile.resolve(dag, bundle.orchestrator) {
        Error(_) ->
          Error(retry_step_validation.Failure(
            reason: "workspace_recovery_failed",
            message: "workspace profile unavailable",
          ))
        Ok(profile) -> Ok(Validation(dag: dag, profile: profile))
      }
    }
  }
}

fn workflow_fingerprint_for_execution(
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(String, retry_step_validation.Failure) {
  case workflow_fingerprint.fingerprint_for_execution(dag, orchestrator) {
    Error(err) ->
      Error(retry_step_validation.Failure(
        reason: "workflow_drift",
        message: "workflow fingerprint failed: "
          <> fingerprint_error_message(err),
      ))
    Ok(fingerprint) -> Ok(fingerprint)
  }
}

fn fingerprint_error_message(
  err: workflow_fingerprint.FingerprintError,
) -> String {
  case err {
    workflow_fingerprint.PromptFileReadFailed(path) ->
      "prompt_file_read_failed:" <> path
    workflow_fingerprint.UnsupportedWorkflowShape(reason) ->
      "unsupported_workflow_shape:" <> reason
    workflow_fingerprint.WorkspaceProfileUnavailable(profile_name) ->
      "workspace_profile_unavailable:" <> profile_name
  }
}
