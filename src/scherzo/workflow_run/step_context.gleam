import gleam/int
import gleam/list
import scherzo/config/types as config_types
import scherzo/orchestrator/schedule_core
import scherzo/path
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_dag
import scherzo/workflow_identity
import scherzo/workspace_profile
import scherzo/workspace_run

pub type StepContext {
  StepContext(
    workflow_id: String,
    run_id: String,
    run_root: String,
    run_artifact_dir: String,
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

pub fn from_prepared(
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
    run_artifact_dir: run_artifact_dir(
      orchestrator.effective.workspace.root,
      workspace.run_id,
    ),
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

pub fn scheduled(
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

fn run_artifact_dir(workspace_root: String, run_id: String) -> String {
  path.join(
    path.join(path.join(workspace_root, ".scherzo-state"), "artifacts"),
    "runs/" <> run_id,
  )
}

pub fn command_env(context: StepContext) -> List(#(String, String)) {
  let generated = [
    #("SCHERZO_CONFIG_DIR", context.config_dir),
    #("SCHERZO_WORKFLOW_ID", context.workflow_id),
    #("SCHERZO_WORKFLOW_BUNDLE_DIR", context.workflow_bundle_dir),
    #("SCHERZO_RUN_ID", context.run_id),
    #("SCHERZO_RUN_ROOT", context.run_root),
    #("SCHERZO_RUN_ARTIFACT_DIR", context.run_artifact_dir),
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
