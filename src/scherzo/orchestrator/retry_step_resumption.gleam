import gleam/option.{None, Some}
import gleam/result
import scherzo/config/types as config_types
import scherzo/error
import scherzo/orchestrator/retry_issue_reactivation
import scherzo/retry_step_validation
import scherzo/runtime_bundle
import scherzo/state/recovery
import scherzo/tracker/adapter
import scherzo/workflow_dag
import scherzo/workflow_fingerprint
import scherzo/workspace_manifest
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
        Ok(profile) -> {
          use _ <- result.try(validate_retained_workspace_metadata(
            bundle,
            recovered,
            profile,
          ))
          Ok(Validation(dag: dag, profile: profile))
        }
      }
    }
  }
}

pub fn validate_operational_inputs(
  bundle: runtime_bundle.RuntimeBundle,
  recovered: recovery.RecoveredWorkflowRun,
) -> Result(Nil, retry_step_validation.Failure) {
  case runtime_bundle.select_workflow(bundle, recovered.issue) {
    Error(runtime_bundle.BundleError(code, message)) ->
      Error(retry_step_validation.Failure(
        reason: "workflow_drift",
        message: "workflow unavailable: " <> code <> ":" <> message,
      ))
    Ok(#(_, dag)) ->
      case workspace_profile.resolve(dag, bundle.orchestrator) {
        Error(_) ->
          Error(retry_step_validation.Failure(
            reason: "workspace_recovery_failed",
            message: "workspace profile unavailable",
          ))
        Ok(profile) ->
          validate_retained_workspace_metadata(bundle, recovered, profile)
      }
  }
}

fn validate_retained_workspace_metadata(
  bundle: runtime_bundle.RuntimeBundle,
  recovered: recovery.RecoveredWorkflowRun,
  profile: config_types.WorkspaceHookProfile,
) -> Result(Nil, retry_step_validation.Failure) {
  case profile.driver {
    None -> Ok(Nil)
    Some(_) -> {
      let context =
        workspace_profile.driver_context(profile, bundle.orchestrator)
      case
        workspace_manifest.cleanup_entries_for_run(
          recovered.run_root,
          recovered.run_id,
          recovered.workflow_id,
          profile.name,
          context.driver,
          config_types.workspace_capability_names(context.capabilities),
        )
      {
        Ok([_, ..]) -> Ok(Nil)
        Ok([]) ->
          Error(retained_workspace_failure(
            "managed workspace manifest is empty",
          ))
        Error(workspace_error) ->
          Error(
            retained_workspace_failure(workspace_error_message(workspace_error)),
          )
      }
    }
  }
}

fn retained_workspace_failure(detail: String) -> retry_step_validation.Failure {
  retry_step_validation.Failure(
    reason: "retained_recovery_unavailable",
    message: "workspace-driver metadata is missing or invalid ("
      <> detail
      <> ")",
  )
}

fn workspace_error_message(workspace_error: error.WorkspaceError) -> String {
  case workspace_error {
    error.WorkspaceOutsideRoot(path) -> "workspace outside root: " <> path
    error.WorkspaceIo(message) -> message
    error.PartialWorkspace(path) -> "partial workspace: " <> path
    error.UnsafeWorkspaceKey(key) -> "unsafe workspace key: " <> key
    error.WorkspaceCollision(message) -> message
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
