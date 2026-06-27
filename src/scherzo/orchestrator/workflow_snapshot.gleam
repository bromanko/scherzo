import gleam/dict.{type Dict}
import gleam/option.{type Option, None, Some}
import scherzo/config/types as config_types
import scherzo/error
import scherzo/runtime_bundle
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_dag
import scherzo/workflow_fingerprint
import scherzo/workspace_run

pub type Snapshot {
  Snapshot(
    workflow_id: String,
    dag: workflow_dag.WorkflowDag,
    orchestrator: config_types.OrchestratorConfig,
    fingerprint: String,
    run_root: String,
  )
}

pub type SnapshotError {
  SnapshotError(code: String, message: String)
}

pub fn error_fields(error: SnapshotError) -> #(String, String) {
  let SnapshotError(code, message) = error
  #(code, message)
}

pub fn worker_start_error_reason(error: SnapshotError) -> String {
  let SnapshotError(code, message) = error
  case code {
    "workflow_fingerprint_failed" -> code
    "workflow_run_root_failed" -> message
    _ -> code <> ":" <> message
  }
}

pub fn for_workflow_id(
  workflow_dags: Dict(String, workflow_dag.WorkflowDag),
  orchestrator: config_types.OrchestratorConfig,
  issue: tracker_issue.Issue,
  workflow_id: String,
  run_id: String,
) -> Result(Snapshot, SnapshotError) {
  case dict.get(workflow_dags, workflow_id) {
    Error(_) ->
      Error(SnapshotError(
        "unknown_workflow_label",
        "unknown workflow label: " <> workflow_id,
      ))
    Ok(dag) -> from_dag(workflow_id, dag, orchestrator, issue, run_id)
  }
}

pub fn for_worker_start(
  snapshot: Option(Snapshot),
  bundle: runtime_bundle.RuntimeBundle,
  issue: tracker_issue.Issue,
  workflow_id: String,
  run_id: String,
) -> Result(Snapshot, SnapshotError) {
  case snapshot {
    Some(snapshot) -> Ok(snapshot)
    None ->
      case runtime_bundle.workflow_by_id(bundle, workflow_id) {
        Error(runtime_bundle.BundleError(code, message)) ->
          Error(SnapshotError(code, message))
        Ok(#(workflow_id, dag)) ->
          from_dag(workflow_id, dag, bundle.orchestrator, issue, run_id)
      }
  }
}

fn from_dag(
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  issue: tracker_issue.Issue,
  run_id: String,
) -> Result(Snapshot, SnapshotError) {
  case workflow_fingerprint.fingerprint_for_execution(dag, orchestrator) {
    Error(_) ->
      Error(SnapshotError(
        "workflow_fingerprint_failed",
        "workflow fingerprint failed for workflow " <> workflow_id,
      ))
    Ok(fingerprint) ->
      case
        workspace_run.run_root_for(
          issue,
          workflow_dag.id(dag),
          run_id,
          orchestrator,
        )
      {
        Error(err) ->
          Error(SnapshotError(
            "workflow_run_root_failed",
            error.workspace_code(err),
          ))
        Ok(run_root) ->
          Ok(Snapshot(
            workflow_id: workflow_id,
            dag: dag,
            orchestrator: orchestrator,
            fingerprint: fingerprint,
            run_root: run_root,
          ))
      }
  }
}
